
#include <stdlib.h>

/*extern void* malloc(size_t sz);
extern void* calloc(size_t n, size_t s);*/

typedef struct Queue_t
{
   int sz;
   int* elements;
   int head;
   int tail;
} Queue;

Queue* new(int n)
{
   Queue* q = malloc(sizeof(Queue));
   q->sz = n+1;
   q->elements = calloc(q->sz, sizeof(int));
   q->head = 0;
   q->tail = 0;
   return q;
}

int put(Queue* q, int element)
{
   q->elements[q->tail] = element;
   q->tail = (q->tail + 1) % q->sz;
   return element;
}

int get(Queue* q)
{
   int element = q->elements[q->head];
   q->head = (q->head + 1) % q->sz;
   return element;
}

int space(Queue* q)
{
   int n_elems = (q->tail >= q->head) ? (q->tail - q->head) : ((q->tail + q->sz) - q->head);
   return (q->sz - n_elems) - 1;
}

