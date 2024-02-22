#include "multiboot.h"
#include <stdint.h>

void log(char* msg);
void serial_putc(char c);
void outb(uint16_t port, uint8_t data);
int strlen(char* str);

void main(uint32_t magic, uint32_t multiboot_header_addr) {
    log("asdf");
    while(1);
}


void log(char* msg) {
    for(int i = 0; msg[i] != '\0'; i++) {
        serial_putc(msg[i]);
    }

    serial_putc('\n');
    serial_putc('\r');
}

void outb(uint16_t port, uint8_t data) {
    asm volatile ("out %1, %0" : : "a" (data), "d" (port));
}

void serial_putc(char c) {
    outb(0x3f8, c);
}

int strlen(char* str) {
    int length = 0;
    while(*str++) {
        length++;
    }
    return length;
}