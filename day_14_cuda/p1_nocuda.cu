#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>

#define BUFFER_SIZE 2048
#define MAX_SHAPES 1024

enum TOKEN_TYPE {
  TT_INT = 0,
  TT_COMMA = 1,
  TT_ARROW = 2,
  TT_EOL = 3,
  TT_EOF = 4
};
const char* TOKEN_TYPE_STR[] = {
  "TT_INT",
  "TT_COMMA",
  "TT_ARROW",
  "TT_EOL",
  "TT_EOF"
};

struct token {
  TOKEN_TYPE token_type;
  int tt_int; // only defined when token_type == TT_INT
};

struct tokenizer {
  int fd;
  char buf[BUFFER_SIZE<<1];
  int buf_pos;
  int buf_len;
  token token;
};

struct node {
  int x;
  int y;
  struct node* next;
};

struct grid {
  int width;
  int height;
  unsigned char* data;
};
enum {
  G_EMPTY = 0,
  G_WALL = 1,
  G_SAND = 2,
};
enum {
  SAND_OUT_OF_BOUNDS = 0,
  SAND_FELL = 1,
  SAND_STOPPED = 2,
};

__global__ void cuda_hello() {
  printf("hello world from GPU!\n");
}


/*
==================== Tokenizer ======================
*/

void tokenizer_load_file(struct tokenizer *t, const char *filename) {
  t->fd = open(filename, O_RDONLY);
  for (int i = 0; i < BUFFER_SIZE; i++) {
    t->buf[i] = 0;
  }
  t->buf_pos = -1;
  t->buf_len = 0;
}

void _tokenizer_show_debug_info(struct tokenizer *t) {
  printf("fd=%d\tbuf_pos=%d\tbuf_len=%d\n", t->fd, t->buf_pos, t->buf_len);
}

void _tokenizer_prev_char(struct tokenizer *t) {
  t->buf_pos--;
  if (t->buf_pos < 0) {
    t->buf_pos = (BUFFER_SIZE<<1) - 1;
  }
  t->buf_pos = t->buf_pos % (BUFFER_SIZE<<1);
}

void _tokenizer_next_char(struct tokenizer *t) {
  t->buf_pos++;
  if (t->buf_pos == t->buf_len) {
    if ((BUFFER_SIZE<<1) - (t->buf_pos + BUFFER_SIZE) < 0) {
      t->buf_pos = 0;
    }
    t->buf_len = read(t->fd, t->buf + t->buf_pos, BUFFER_SIZE) + t->buf_pos;
    if (t->buf_len == t->buf_pos) {
      t->token.token_type = TT_EOF;
    }
  }
  t->buf_pos = t->buf_pos % (BUFFER_SIZE<<1);
}

void tokenizer_next(struct tokenizer *t) {
  _tokenizer_next_char(t);
  if (t->token.token_type == TT_EOF) {
    return;
  }
  char c = t->buf[t->buf_pos];

  // eat whitespace
  while (c == ' ' || c == '\t') {
    _tokenizer_next_char(t);
    if (t->token.token_type == TT_EOF) {
      return;
    }
    c = t->buf[t->buf_pos];
  }

  if (c == ',') {
    t->token.token_type = TT_COMMA;
    return;
  }
  if (c == '\n') {
    t->token.token_type = TT_EOL;
    return;
  }
  if (c == '-') {
    _tokenizer_next_char(t);
    c = t->buf[t->buf_pos];
    if (t->token.token_type == TT_EOF || c != '>') {
      _tokenizer_show_debug_info(t);
      printf("Expected '>' after '-'\n");
      exit(1);
    } else {
      t->token.token_type = TT_ARROW;
      return;
    }
  }

  if (c < '0' || c > '9') {
    printf("%d %d\n", t->buf_len, t->buf_pos);
    printf("Token char='%c' hex='0x%x' not recognized\n", c, c);
    exit(1);
  }
  int cur = 0;
  for (;;) {
    if (t->token.token_type == TT_EOF) {
      _tokenizer_prev_char(t);
      t->token.token_type = TT_INT;
      t->token.tt_int = cur;
      return;
    }
    if (c >= '0' && c <= '9') {
      cur = cur * 10 + (c - '0');
    } else {
      _tokenizer_prev_char(t);
      t->token.token_type = TT_INT;
      t->token.tt_int = cur;
      return;
    }
    _tokenizer_next_char(t);
    c = t->buf[t->buf_pos];
  }
}

/*
===================== Parser =======================
*/

void print_shapes(struct node* shape[]) {
  for (int i = 0; i < MAX_SHAPES; i++) {
    struct node* cur = shape[i];
    if (cur == NULL) {
      return;
    }
    cur = cur->next;
    printf("shape %d: ", i);
    while (cur != NULL) {
      printf("(%d, %d) ", cur->x, cur->y);
      cur = cur->next;
    }
    printf("\n");
  }
}

struct node* parse_shape(struct tokenizer *t) {
  struct node* head = (struct node*)malloc(sizeof(struct node));
  struct node* prev = head;
  struct node* cur;
  while (true) {
    cur = (struct node*)malloc(sizeof(struct node));
    cur->next = NULL;
    cur->x = 0;
    cur->y = 0;
    prev->next = cur;
    tokenizer_next(t);
    if (t->token.token_type != TT_INT) {
      printf("parser error: shape.x should be int, got %s\n", TOKEN_TYPE_STR[t->token.token_type]);
      exit(1);
    }
    cur->x = t->token.tt_int;
    tokenizer_next(t);
    if (t->token.token_type != TT_COMMA) {
      struct node* shapes[1];
      shapes[0] = head;
      print_shapes(shapes);
      printf("parser error: expected comma, got %s\n", TOKEN_TYPE_STR[t->token.token_type]);
      exit(1);
    }
    tokenizer_next(t);
    if (t->token.token_type != TT_INT) {
      printf("parser error: shape.y should be int\n");
      exit(1);
    }
    cur->y = t->token.tt_int;
    tokenizer_next(t);
    if (t->token.token_type == TT_EOL || t->token.token_type == TT_EOF) {
      cur->next = NULL;
      return head;
    }
    prev = cur;
  }
}

void parse_shapes(struct tokenizer *t, struct node* shapes[]) {
  for (int i = 0; i < MAX_SHAPES; i++) {
    shapes[i] = NULL;
  }
  for (int i = 0; i < MAX_SHAPES; i++) {
    shapes[i] = parse_shape(t);
    if (t->token.token_type == TT_EOF) {
      return;
    }
  }
}

/*
===================== Simulation =======================
*/

int shapes_max_x(struct node* shapes[]) {
  int max_x = 0;
  for (int i = 0; i < MAX_SHAPES; i++) {
    struct node* cur = shapes[i];
    if (cur == NULL) {
      break;
    }
    cur = cur->next;
    while (cur != NULL) {
      max_x = max(max_x, cur->x);
      cur = cur->next;
    }
  }
  return max_x;
}

int shapes_max_y(struct node* shapes[]) {
  int max_y = 0;
  for (int i = 0; i < MAX_SHAPES; i++) {
    struct node* cur = shapes[i];
    if (cur == NULL) {
      break;
    }
    cur = cur->next;
    while (cur != NULL) {
      max_y = max(max_y, cur->y);
      cur = cur->next;
    }
  }
  return max_y;
}

void grid_draw_shapes(struct grid* g, struct node* shapes[]) {
  for (int i = 0; i < MAX_SHAPES; i++) {
    struct node* cur = shapes[i];
    if (cur == NULL) {
      break;
    }
    cur = cur->next;
    struct node* prev = cur;
    if (cur->next == NULL) {
      printf("shape %d has only one point\n", i);
      exit(1);
    }
    cur = cur->next;
    while (cur != NULL) {
      if (cur->y == prev->y) {
        int from_x = min(prev->x, cur->x);
        int to_x = max(prev->x, cur->x);
        int y = prev->y;
        for (int x = from_x; x <= to_x; x++) {
          g->data[x + y * g->width] = G_WALL;
        }
      } else if (cur->x == prev->x) {
        int from_y = min(prev->y, cur->y);
        int to_y = max(prev->y, cur->y);
        int x = prev->x;
        for (int y = from_y; y <= to_y; y++) {
          g->data[x + y * g->width] = G_WALL;
        }
      } else {
        printf("Not a line\n");
        exit(1);
      }
      prev = cur;
      cur = cur->next;
    }
  }
}

struct grid* grid_create(struct node* shapes[]) {
  // Get dimensions
  int width = 1;
  int height = 1;
  int max_x = shapes_max_x(shapes);
  int max_y = shapes_max_y(shapes);
  while (width < max_x)
    width <<= 1;
  while (height < max_y)
    height  <<= 1;
  unsigned char *data = (unsigned char*)malloc(width * height * sizeof(unsigned char));
  struct grid* g = (struct grid*)malloc(sizeof(struct grid));
  g->width = width;
  g->height = height;
  g->data = data;
  grid_draw_shapes(g, shapes);
  return g;
}

void grid_print(struct grid* g) {
  printf("     ");
  for (int x = 494; x < 504; x++) {
    printf(" %3.d ", x);
  }
  printf("\n");
  for (int y = 0; y < 10; y++) {
    printf(" %3.d ", y);
    for (int x = 494; x < 504; x++) {
      switch(g->data[x + y * g->width]) {
        case G_EMPTY:
          printf("  .  ");
          break;
        case G_SAND:
          printf("  O  ");
          break;
        case G_WALL:
          printf("  #  ");
          break;
        default:
          printf("  ?  ");
          break;
      }
    }
    printf("\n");
  }
}

bool in_range(struct grid* g, int x, int y) {
  return x >= 0 && x < g->width && y >= 0 && y < g->height;
}

int simulate_step(struct grid* g) {
  int out = SAND_STOPPED;
  struct grid* g2 = (struct grid*)malloc(sizeof(struct grid));
  g2->data = (unsigned char*)malloc(g->width * g->height * sizeof(unsigned char));
  for (int x = 0; x < g->width; x++) {
    for (int y = 0; y < g->height; y++) {
      g2->data[x + y * g->width] = g->data[x + y * g->width];
    }
  }
  for (int x = 0; x < g->width; x++) {
    for (int y = 0; y < g->height; y++) {
      if (g->data[x + y * g->width] == G_SAND) {
        if (!in_range(g, x, y+1)) {
          g2->data[x + y * g->width] = G_EMPTY;
          return SAND_OUT_OF_BOUNDS;
        } else if (g->data[x + (y+1) * g->width] == G_EMPTY) {
          g2->data[x + y * g->width] = G_EMPTY;
          g2->data[x + (y+1) * g->width] = G_SAND;
          out = SAND_FELL;
        } else if (!in_range(g, x-1, y+1)) {
          return SAND_OUT_OF_BOUNDS;
        } else if (g->data[(x-1) + (y+1) * g->width] == G_EMPTY) {
          g2->data[x + y * g->width] = G_EMPTY;
          g2->data[(x-1) + (y+1) * g->width] = G_SAND;
          out = SAND_FELL;
        } else if (!in_range(g, x+1, y+1)) {
          return SAND_OUT_OF_BOUNDS;
        } else if (g->data[(x+1) + (y+1) * g->width] == G_EMPTY) {
          g2->data[x + y * g->width] = G_EMPTY;
          g2->data[(x+1) + (y+1) * g->width] = G_SAND;
          out = SAND_FELL;
        }
      }
    }
  }
  for (int x = 0; x < g->width; x++) {
    for (int y = 0; y < g->height; y++) {
      g->data[x + y * g->width] = g2->data[x + y * g->width];
    }
  }
  free(g2->data);
  free(g2);
  return out;
}

void simulate_v2(struct node* shapes[]) {
  struct grid* g = grid_create(shapes);
  int spawn = 500;
  while (true) {
    //grid_print(g);
    if (g->data[spawn] != G_EMPTY)  {
      goto DONE;
    }
    g->data[spawn] = G_SAND;
    for (int y = 0; y < g->height; y++) {
      for (int x = 0; x < g->width; x++) {
        if (g->data[x + y * g->width] != G_SAND) {
          continue;
        }
        if (!in_range(g, x, y+1)) {
          g->data[x + y * g->width] = G_EMPTY;
          goto DONE;
        } else if (g->data[x + (y+1) * g->width] == G_EMPTY) {
          g->data[x + y * g->width] = G_EMPTY;
          g->data[x + (y+1) * g->width] = G_SAND;
        } else if (!in_range(g, x-1, y+1)) {
          g->data[x + y * g->width] = G_EMPTY;
          goto DONE;
        } else if (g->data[(x-1) + (y+1) * g->width] == G_EMPTY) {
          g->data[x + y * g->width] = G_EMPTY;
          g->data[(x-1) + (y+1) * g->width] = G_SAND;
        } else if (!in_range(g, x+1, y+1)) {
          g->data[x + y * g->width] = G_EMPTY;
          goto DONE;
        } else if (g->data[(x+1) + (y+1) * g->width] == G_EMPTY) {
          g->data[x + y * g->width] = G_EMPTY;
          g->data[(x+1) + (y+1) * g->width] = G_SAND;
        }
      }
    }
  }
DONE:
  grid_print(g);
  int num_stopped = 0;
  for (int x = 0; x < g->width; x++) {
    for (int y = 0; y < g->height; y++) {
      if (g->data[x + y * g->width] == G_SAND) {
        num_stopped++;
      }
    }
  }
  printf("%d sand pieces\n", num_stopped);
}

void simulate(struct node* shapes[]) {
  struct grid* g = grid_create(shapes);

  int num_stopped = 0;
  int spawn = 500;
  while (true) {
    if (g->data[spawn] != G_EMPTY)  {
      break;
    }
    g->data[spawn] = G_SAND;
    while (true) {
      int res = simulate_step(g);
      if (res == SAND_OUT_OF_BOUNDS) {
        break;
      }
      if (res == SAND_STOPPED) {
        num_stopped++;
        break;
      }
    }
  }
  grid_print(g);
  printf("Stopped %d times\n", num_stopped);
}

int main() {
  tokenizer *t = (tokenizer *)malloc(sizeof(tokenizer));
  tokenizer_load_file(t, "input_small.txt");
  struct node* shapes[MAX_SHAPES];
  parse_shapes(t, shapes);
  // print_shapes(shapes);
  simulate_v2(shapes);
  cuda_hello<<<1,1>>>();
  cudaDeviceSynchronize();
  printf("hello world from CPU!\n");
  return 0;
}
