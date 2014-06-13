
#include <stdlib.h>
#include "q_api.h"

/*extern void* malloc(size_t sz);
extern void* calloc(size_t n, size_t s);*/

Queue* new(int n)
{
   Queue* q = malloc(sizeof(Queue));
   q->size = n+1;
   q->buffer = calloc(q->size, sizeof(int));
   q->head = 0;
   q->tail = 0;
   return q;
}

int put(Queue* q, int element)
{
   q->buffer[q->tail] = element;
   q->tail = (q->tail + 1) % q->size;
   return element;
}

int get(Queue* q)
{
   int elem = q->buffer[q->head];
   q->head = (q->head + 1) % q->size;
   return elem;
}

int space(Queue* q)
{
   return q->size - ((q->tail + q->size)- q->head) % q->size - 1;
}
