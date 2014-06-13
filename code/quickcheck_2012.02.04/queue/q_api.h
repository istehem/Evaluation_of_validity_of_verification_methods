
typedef struct
{
   int size;
   int head;
   int tail;
   int* buffer;
} Queue;

Queue* new(int);
int put(Queue*, int);
int get(Queue*);
int space(Queue* q);
