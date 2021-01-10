#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

typedef struct List list_t;

struct List
{
  void *head;
  list_t *tail;
};

list_t *makelist(void *x, list_t *xs)
{
  list_t *ans = (list_t *)malloc(sizeof(list_t));
  ans->head = x;
  ans->tail = xs;
  return ans;
}

list_t *map(void *(*f)(void *, void *), void *env, list_t *xs)
{
  if (xs == NULL)
    return NULL;
  return makelist(f(env, xs->head), map(f, env, xs->tail));
}

list_t *filter(bool (*f)(void *, void *), void *env, list_t *xs)
{
  if (xs == NULL)
    return NULL;
  if (f(env, xs->head))
    return makelist(xs->head, filter(f, env, xs->tail));
  return filter(f, env, xs->tail);
}

int length(list_t *xs)
{
  int ans = 0;
  while (xs != NULL)
  {
    ++ans;
    xs = xs->tail;
  }
  return ans;
}

void *doubleInt(void *ignore, void *i)
{
  return (void *)(((intptr_t)i) * 2);
}

// assumes list holds intptr_t fileds
list_t *doubleAll(list_t *xs)
{
  return map(doubleInt, NULL, xs);
}

// type casts to match what filter expects
bool isN(void *n, void *i)
{
  return ((intptr_t)n) == ((intptr_t)i);
}

int countNs(list_t *xs, intptr_t n)
{
  return length(filter(isN, (void *)n, xs));
}
