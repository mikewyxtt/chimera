#include "string_utils.h"

int strlen(char* str) {
    int length = 0;

    while(*str++) {
        length++;
    }

    return length;
}


void itoa (char *buf, int base, int d)
{
  char *p = buf;
  char *p1, *p2;
  unsigned long ud = d;
  int divisor = 10;
  
  /*  If %d is specified and D is minus, put ‘-’ in the head. */
  if (base == 'd' && d < 0)
    {
      *p++ = '-';
      buf++;
      ud = -d;
    }
  else if (base == 'x')
    divisor = 16;

  /*  Divide UD by DIVISOR until UD == 0. */
  do
    {
      int remainder = ud % divisor;
      
      *p++ = (remainder < 10) ? remainder + '0' : remainder + 'a' - 10;
    }
  while (ud /= divisor);

  /*  Terminate BUF. */
  *p = 0;
  
  /*  Reverse BUF. */
  p1 = buf;
  p2 = p - 1;
  while (p1 < p2)
    {
      char tmp = *p1;
      *p1 = *p2;
      *p2 = tmp;
      p1++;
      p2--;
    }
}

void memcpy(uintptr_t *destination, uintptr_t *source, uintptr_t count) {
    asm volatile ("cld\n\t"
                  "rep movsb\n\t"
                  : "=D" (destination), "=S" (source), "=c" (count)
                  : "0" (destination), "1" (source), "2" (count)
                  : "memory");
}


void memset(uintptr_t *destination, int value, uintptr_t count) {
    asm volatile ("cld\n\t"
                  "rep stosb"
                  :
                  : "D" (destination), "a" (value), "c" (count)
                  : "memory");
}

void memmove(uintptr_t *destination, uintptr_t *source, uintptr_t count) {
    uintptr_t *s = source;
    uintptr_t *d = destination;

    if (s < d && s + count > d) {
        for (uintptr_t i = count; i != 0; --i) {
          d[i - 1] = s[i - 1];
        }
    }
    else {
      for (uintptr_t i = 0; i < count; ++i) {
        d[i] = s[i];
      }
    }
}