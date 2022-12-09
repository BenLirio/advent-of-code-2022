#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#define MAP_HEIGHT 10000
#define MAP_WIDTH 10000
struct pos {
  int x;
  int y;
};

unsigned char visited[MAP_HEIGHT][MAP_WIDTH];
struct pos h_pos;
struct pos t_pos;
int unique_positions = 0;

void update_visited() {
  int x = t_pos.x + MAP_WIDTH / 2;
  int y = t_pos.y + MAP_HEIGHT / 2;
  if (!visited[x][y]) {
    unique_positions++;
    visited[x][y] = 1;
  }
}

int abs(int x) {
  if (x < 0)
    return -x;
  else
    return x;
}
int max(int a, int b) {
  if (a > b)
    return a;
  else
    return b;
}
void follow() {
  int dx = abs(h_pos.x - t_pos.x);
  int dy = abs(h_pos.y - t_pos.y);
  if (max(dx, dy) < 2)
    return;
  if (dx == 1)
    t_pos.x = h_pos.x;
  if (dy == 1)
    t_pos.y = h_pos.y;
  if (dx == 2)
    t_pos.x = (h_pos.x + t_pos.x) / 2;
  if (dy == 2)
    t_pos.y = (h_pos.y + t_pos.y) / 2;
}
void move(struct pos d_pos) {
  h_pos.x += d_pos.x;
  h_pos.y += d_pos.y;
  follow();
  update_visited();
}

void show() {
  for (int y = 4; y >= 0; y--) {
    for (int x = 0; x < 6; x++) {
      if (h_pos.x == x && h_pos.y == y)
        printf("H");
      else if (t_pos.x == x && t_pos.y == y)
        printf("T");
      else if (visited[y][x])
        printf("#");
      else
        printf(".");
    }
    printf("\n");
  }
  getchar();
}

int main() {
  FILE* fp = fopen("input.txt", "r");

  char action;
  int value;
  // show();
  for (;;) {
    int res = fscanf(fp, "%c %d\n", &action, &value);
    if (res == EOF)
      break;
    for (int i = 0; i < value; i++) {
      struct pos d_pos;
      d_pos.x = 0;
      d_pos.y = 0;
      if (action == 'L')
        d_pos.x = -1;
      if (action == 'R')
        d_pos.x = 1;
      if (action == 'U')
        d_pos.y = 1;
      if (action == 'D')
        d_pos.y = -1;
      move(d_pos);
      // show();
    }
  }
  printf("unique_positions=%d\n", unique_positions);
}