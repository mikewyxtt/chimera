#ifndef BOOTINFO_H
#define BOOTINFO_H

#include <stdint.h>
#include <stdbool.h>


struct DriverInfo {
    /* Name of the driver */
    char name[25];

    /* Type of driver */
    char type[25];

    /* Physical start address of the driver */
    uintptr_t addr;

    /* Size of the driver in bytes */
    uint32_t size;
};

struct BootInfo {
    /* Early log buffer information */
    struct {
        /* Size of the buffer*/
        uint16_t size;

        /* Current index of log buffer*/
        uint16_t index;

        /* Index of buffer when it was last flushed */
        uint16_t last_flush_index;

        /* The early log buffer */
        char buffer[6144];
    } EarlyLogBuffer;

    /* Framebuffer information */
    struct {
        /* Framebuffer enabled? */
        bool enabled;

        /* Framebuffer physical address */
        uintptr_t addr;

        /* Framebuffer width in pixels */
        uint32_t width;

        /* Framebuffer height in pixels */
        uint32_t height;

        /* Framebuffer pitch */
        uint32_t pitch;

        /* Framebuffer depth (Bits per pixel) */
        uint32_t depth;
    } Framebuffer;

    /* Console Information */
    struct {
        int cursor_pos;
        int line;
        int max_chars;
        int max_line;

    } Console;

    /* Serial debugging information */
    struct {
        /* Serial enabled? */
        bool enabled;

        /* Serial port to output to */
        uint16_t port;
    } Serial;

    /* Address and size of critical servers */
    struct {
        /* Virtual File System Server */
        struct {
            uintptr_t addr;
            int size;
        } VFS;

        /* Memory Management Server */
        struct {
            uintptr_t addr;
            int size;
        } MM;

        /* Scheduler server */
        struct {
            uintptr_t addr;
            int size;
        } Sched;

        /* Processs management server */
        struct {
            uintptr_t addr;
            int size;
        } PM;
    } CriticalServers;

    /* List of drivers */
    struct {
      /* Array containing driver inforation */
      struct DriverInfo driverinfo[25];

      /* Number of drivers present */
      int count;
    } DriverList;

    /* Boot parameters passed in from bootloader */
    char params[100];
};

#endif