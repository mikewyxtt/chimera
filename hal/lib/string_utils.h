#ifndef STRING_UTILS_H
#define STRING_UTILS_H
#include <stdint.h>

/* Calculates the length of a string */
int strlen(char* str);

/* Converts an integer to a string */
void itoa (char *buf, int base, int d);

/* Copy specified number of bytes from source address to destination address */
void memcpy(uintptr_t *destination, uintptr_t *source, uintptr_t count);

/* Copy specified number of bytes from source address to destination address */
void memset(uintptr_t *destination, int value, uintptr_t count);


void memmove(uintptr_t *destination, uintptr_t *source, uintptr_t count);

#endif