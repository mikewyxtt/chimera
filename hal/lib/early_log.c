#include <stdarg.h>
#include "early_log.h"
#include "bootinfo.h"
#include "io.h"
#include "string_utils.h"

/* Print a single char to the framebuffer and/or serial port */
static void putchar(struct BootInfo *bootinfo, int c);

void early_log(struct BootInfo *bootinfo, const char *format, ...) {
    va_list args;
    va_start(args, format);

    int c;
    char buf[20];

    while ((c = *format++) != 0)
        {
        if (c != '%')
            putchar (bootinfo, c);
        else
            {
            char *p, *p2;
            int pad0 = 0, pad = 0;
            
            c = *format++;
            if (c == '0')
                {
                pad0 = 1;
                c = *format++;
                }

            if (c >= '0' && c <= '9')
                {
                pad = c - '0';
                c = *format++;
                }

            switch (c)
                {
                case 'd':
                case 'u':
                case 'x':
                    itoa (buf, c, va_arg(args, int));
                    p = buf;
                    goto string;
                break;

                case 's':
                p = va_arg(args, char*);
                if (! p)
                    p = "(null)";

                string:
                for (p2 = p; *p2; p2++);
                for (; p2 < p + pad; p2++)
                    putchar (bootinfo, pad0 ? '0' : ' ');
                while (*p)
                    putchar (bootinfo, *p++);
                break;

                default:
                    putchar (bootinfo, va_arg(args, int));
                break;
                }
            }
        }
}

static void putchar(struct BootInfo *bootinfo, int c) {

    // Write the char to the framebuffer if we have one.
    if(bootinfo->Framebuffer.enabled) {
        // display to fb
    }


    // Write the char to the serial port if it's enabled.
    if(bootinfo->Serial.enabled) {
        HAL_IO_WriteByte(bootinfo->Serial.port, c);

        // If it's a newline we need to send the carriage return as well, serial is weird
        if (c == '\n') {
            HAL_IO_WriteByte(bootinfo->Serial.port, '\r');
        }
    }

    // Make sure we don't overrun the buffer
    if (bootinfo->EarlyLogBuffer.index < bootinfo->EarlyLogBuffer.size) {
        bootinfo->EarlyLogBuffer.index++;
    }

    else { 
        bootinfo->EarlyLogBuffer.index = 0;
    }

    // Update the last flush index, currently unused
    bootinfo->EarlyLogBuffer.last_flush_index = bootinfo->EarlyLogBuffer.index;
}
