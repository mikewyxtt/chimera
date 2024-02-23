#include <stdint.h>
#include <stdbool.h>
#include "multiboot.h"
#include "../../lib/early_log.h"
#include "../../lib/bootinfo.h"


void main(uint32_t magic, uint32_t multiboot_header_addr) {
    // Ensure we were loaded by a Multiboot 2 compliant bootloader
    if(magic != MULTIBOOT2_BOOTLOADER_MAGIC) {
        // do something? hang for now..
        while(1);
    }

    // Create and initialize BootInfo struct
    struct BootInfo bootinfo;

    bootinfo.Framebuffer.enabled = false;

    // Extract data from multiboot header
    struct multiboot_tag *tag;

    for (tag = (struct multiboot_tag *) (multiboot_header_addr + 8);
          tag->type != MULTIBOOT_TAG_TYPE_END;
          tag = (struct multiboot_tag *) ((multiboot_uint8_t *) tag + ((tag->size + 7) & ~7))) {
        

        switch (tag->type) {
        case MULTIBOOT_TAG_TYPE_FRAMEBUFFER:

            // clang errors out if we define the struct right after the case statement...
            while(0); 
            struct multiboot_tag_framebuffer *fbtag = (struct multiboot_tag_framebuffer *) tag;

            if (fbtag->common.framebuffer_type == 1) {

                // type of 1 means RGB, 0 means EGA text mode (unsupported)
                bootinfo.Framebuffer.enabled = true; 
            }

            bootinfo.Framebuffer.addr = fbtag->common.framebuffer_addr;
            bootinfo.Framebuffer.width = fbtag->common.framebuffer_width;
            bootinfo.Framebuffer.height = fbtag->common.framebuffer_height;
            bootinfo.Framebuffer.pitch = fbtag->common.framebuffer_pitch;
            bootinfo.Framebuffer.depth = fbtag->common.framebuffer_bpp / 8;
            break;
        
            default:
                break;
        }
    }

    // Set early log buffer values
    bootinfo.EarlyLogBuffer.size = sizeof(bootinfo.EarlyLogBuffer.buffer);
    bootinfo.EarlyLogBuffer.index = 0;
    bootinfo.EarlyLogBuffer.last_flush_index = 0;

    // Set serial port values
    #ifdef SERIAL_LOG
      bootinfo.Serial.enabled = true;
      bootinfo.Serial.port = 0x3f8;
    #endif

    // Set console values
    if (bootinfo.Framebuffer.enabled) {
        bootinfo.Console.cursor_pos = 0;
        bootinfo.Console.line = 0;
        bootinfo.Console.max_chars = bootinfo.Framebuffer.width / 8;
        bootinfo.Console.max_line = bootinfo.Framebuffer.height / 16;
    }

    early_log(&bootinfo, "Framebuffer Info:\n");
    early_log(&bootinfo, "\tEnabled: %d\n", bootinfo.Framebuffer.enabled);
    early_log(&bootinfo, "\tAddress: 0x%x\n", bootinfo.Framebuffer.addr);
    early_log(&bootinfo, "\tWidth: %d\n", bootinfo.Framebuffer.width);
    early_log(&bootinfo, "\tHeight: %d\n", bootinfo.Framebuffer.height);
    early_log(&bootinfo, "\tPitch: %d\n", bootinfo.Framebuffer.pitch);
    early_log(&bootinfo, "\tDepth: %d\n", bootinfo.Framebuffer.depth);

    early_log(&bootinfo, "Console Info:\n");
    early_log(&bootinfo, "\tMax chars: %d\n", bootinfo.Console.max_chars);
    early_log(&bootinfo, "\tMax lines: %d\n", bootinfo.Console.max_line);
    early_log(&bootinfo, "\tCursor position: %d\n", bootinfo.Console.cursor_pos);
    early_log(&bootinfo, "\tCursor line: %d\n", bootinfo.Console.line);
    
    // int font[15];
    // font[0] =  0b00000000;
    // font[1] =  0b00000000;
    // font[2] =  0b00000000;
    // font[3] =  0b00010000;
    // font[4] =  0b00111000;
    // font[5] =  0b01101100;
    // font[6] =  0b11000110;
    // font[7] =  0b11000110;
    // font[8] =  0b11111110;
    // font[9] =  0b11000110;
    // font[10] = 0b11000110;
    // font[11] = 0b11000110;
    // font[12] = 0b11000110;
    // font[13] = 0b00000000;
    // font[14] = 0b00000000;
    // font[15] = 0b00000000;


    while(1);

    
    early_log(&bootinfo, "Magic Number: 0x%x\n", magic);
    early_log(&bootinfo, "Multiboot header addr: 0x%x\n", &multiboot_header_addr);
    early_log(&bootinfo, "Log buffer size: %d\n", bootinfo.EarlyLogBuffer.size);
    early_log(&bootinfo, "Serial port enabled?: %d\n", bootinfo.Serial.enabled);
    early_log(&bootinfo, "Serial port: 0x%x\n", bootinfo.Serial.port);

    while(1);
}



