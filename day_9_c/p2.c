#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#define MAP_HEIGHT 10000
#define MAP_WIDTH 10000
#define ROPE_LEN 10

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

struct pos {
  int x;
  int y;
};

unsigned char visited[MAP_HEIGHT][MAP_WIDTH];
struct pos poses[ROPE_LEN];
int unique_positions = 0;

void update_visited() {
  // Set origin to the center of the visited matrix
  int x = poses[ROPE_LEN-1].x + MAP_WIDTH / 2;
  int y = poses[ROPE_LEN-1].y + MAP_HEIGHT / 2;
  if (!visited[x][y]) {
    unique_positions++;
    visited[x][y] = 1;
  }
}

void follow() {
  for (int i = 1; i < ROPE_LEN; i++) {
    int dx = abs(poses[i-1].x - poses[i].x);
    int dy = abs(poses[i-1].y - poses[i].y);
    if (max(dx, dy) < 2)
      return;
    if (dx == 1)
      poses[i].x = poses[i-1].x;
    if (dy == 1)
      poses[i].y = poses[i-1].y;
    if (dx == 2)
      poses[i].x = (poses[i-1].x + poses[i].x) / 2;
    if (dy == 2)
      poses[i].y = (poses[i-1].y + poses[i].y) / 2;
  }
}

void move(struct pos d_pos) {
  poses[0].x += d_pos.x;
  poses[0].y += d_pos.y;
  follow();
  update_visited();
}

int main() {
  FILE* fp = fopen("input.txt", "r");

  char action;
  int value;
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
    }
  }
  printf("unique_positions=%d\n", unique_positions);
}