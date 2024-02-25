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

struct CriticalComponent {
    bool present;
    uintptr_t addr;
    int size;
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
        uint16_t width;

        /* Framebuffer height in pixels */
        uint16_t height;

        /* Framebuffer pitch */
        uint16_t pitch;

        /* Framebuffer depth (Bytes per pixel) */
        uint8_t depth;

        /* Size of the framebuffer in bytes */
        uint64_t size;
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
        struct CriticalComponent abc;
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
      uint32_t count;
    } DriverList;

    struct {
        uint64_t available_memory;
        struct {
            //
        } MemoryMap;
    } MemoryInfo;

    /* Boot parameters passed in from bootloader */
    char *params;
};

#endif