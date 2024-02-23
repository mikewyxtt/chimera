#include "io.h"

void HAL_IO_WriteByte(uint16_t port, uint8_t data) {
    asm volatile ("out %1, %0" : : "a" (data), "d" (port));
}
