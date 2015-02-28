#include <stdio.h>

int inc (int x) { return x + 1; }

void encrypt (char *s) {
  char *p = s;
  for (; *p; p++) *p ^= 0x07;
}

typedef struct { int x, y; } point;

int sizeof_pt(void) { return sizeof(point); }

void init_pt (point *p, int x, int y) {
  p->x = x;
  p->y = y;
}

int get_x (point *p) { return p->x; }
int get_y (point *p) { return p->y; }

int sqdist (point *p) {
  return p->x * p->x  +  p->y * p->y;
}
