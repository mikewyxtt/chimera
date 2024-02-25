#include <stdint.h>
#include <stdbool.h>
#include "multiboot.h"
#include "../../lib/early_log.h"
#include "../../lib/string_utils.h"
#include "../../lib/bootinfo.h"

uint8_t smbios_major;
int smbios_minor;


/* Initialize the BootInfo table */
static void InitializeBootInfo(struct BootInfo *bootinfo, uintptr_t multiboot_header_addr);

/* Parse the Multiboot 2 Header to setup BootInfo table */
static void ParseMultibootHeader(struct BootInfo *bootinfo, uintptr_t multiboot_header_addr);


void main(uint32_t magic, uint32_t multiboot_header_addr) {
    // Ensure we were loaded by a Multiboot 2 compliant bootloader
    if(magic != MULTIBOOT2_BOOTLOADER_MAGIC) {
        // do something? hang for now..
        while(1);
    }

    // Create and initialize BootInfo struct
    struct BootInfo bootinfo;
    memset((uintptr_t*)&bootinfo, 0, sizeof(bootinfo));
    InitializeBootInfo(&bootinfo, multiboot_header_addr);
    
    early_log(&bootinfo, "Multiboot 2 Info:\n");
    early_log(&bootinfo, "\tMagic Number: 0x%x\n", magic);
    early_log(&bootinfo, "\tMultiboot header addr: 0x%x\n", multiboot_header_addr);

    early_log(&bootinfo, "Framebuffer Info:\n");
    early_log(&bootinfo, "\tEnabled: %d\n", bootinfo.Framebuffer.enabled);
    early_log(&bootinfo, "\tAddress: 0x%x\n", bootinfo.Framebuffer.addr);
    early_log(&bootinfo, "\tResolution: %dx%d\n", bootinfo.Framebuffer.width, bootinfo.Framebuffer.height);
    early_log(&bootinfo, "\tPitch: %d bytes\n", bootinfo.Framebuffer.pitch);
    early_log(&bootinfo, "\tDepth: %d bits\n", bootinfo.Framebuffer.depth * 8);
    early_log(&bootinfo, "\tSize: %d bytes\n", bootinfo.Framebuffer.size);

    early_log(&bootinfo, "Console Info:\n");
    early_log(&bootinfo, "\tMax chars: %d\n", bootinfo.Console.max_chars);
    early_log(&bootinfo, "\tMax lines: %d\n", bootinfo.Console.max_line);
    early_log(&bootinfo, "\tCursor position: %d\n", bootinfo.Console.cursor_pos);
    early_log(&bootinfo, "\tCursor line: %d\n", bootinfo.Console.line);

    early_log(&bootinfo, "Serial Port Info:\n");
    early_log(&bootinfo, "\tEnabled: %d\n", bootinfo.Serial.enabled);
    early_log(&bootinfo, "\tUsing Port: 0x%x\n", bootinfo.Serial.port);

    early_log(&bootinfo, "System Info:\n");
    early_log(&bootinfo, "\tAvailable Memory: %dMB\n", bootinfo.MemoryInfo.available_memory / 1024);
    
    early_log(&bootinfo, "Misc. Info:\n");
    early_log(&bootinfo, "\tLog buffer size: %d\n", bootinfo.EarlyLogBuffer.size);
    early_log(&bootinfo, "\tBoot paramaters: %s\n", bootinfo.params);

    while(1);
}


static void InitializeBootInfo(struct BootInfo *bootinfo, uintptr_t multiboot_header_addr) {
    // Set default values (NOTE: All values are set to 0 up until this point.)

    // Early Log Buffer
    bootinfo->EarlyLogBuffer.size = sizeof(bootinfo->EarlyLogBuffer.buffer);

    // Serial Logging
    #ifdef SERIAL_LOG
        bootinfo->Serial.enabled = true;
        bootinfo->Serial.port = 0x3f8;
    #endif

    // Required components list
    //bootinfo->CriticalServers.MM.

    // Parse Multiboot 2 header to fill in the BootInfo table
    ParseMultibootHeader(bootinfo, multiboot_header_addr);
}


static void ParseMultibootHeader(struct BootInfo *bootinfo, uintptr_t multiboot_header_addr) {
    /* We parse the header twice. First we get the boot args and list of components that will be loaded */
    struct multiboot_tag *tag;

    for (tag = (struct multiboot_tag *) (multiboot_header_addr + 8);
          tag->type != MULTIBOOT_TAG_TYPE_END;
          tag = (struct multiboot_tag *) ((multiboot_uint8_t *) tag + ((tag->size + 7) & ~7))) {
        

        switch (tag->type) {
        case MULTIBOOT_TAG_TYPE_FRAMEBUFFER:
            ; // Clang errors out if we define the struct right after the case statement...
            
            struct multiboot_tag_framebuffer *fbtag = (struct multiboot_tag_framebuffer *) tag;

            if (fbtag->common.framebuffer_type == 1) {
                // Type of 1 means RGB, 2 means EGA text mode (unsupported), 0 means indexed color (unsupported)
                bootinfo->Framebuffer.enabled = true;
                bootinfo->Framebuffer.addr = fbtag->common.framebuffer_addr;
                bootinfo->Framebuffer.width = fbtag->common.framebuffer_width;
                bootinfo->Framebuffer.height = fbtag->common.framebuffer_height;
                bootinfo->Framebuffer.pitch = fbtag->common.framebuffer_pitch;
                bootinfo->Framebuffer.depth = fbtag->common.framebuffer_bpp / 8;
                bootinfo->Framebuffer.size = bootinfo->Framebuffer.width * bootinfo->Framebuffer.height * bootinfo->Framebuffer.depth;

                // Since we have a framebuffer, initialize the console.
                bootinfo->Console.cursor_pos = 0;
                bootinfo->Console.line = 0;
                bootinfo->Console.max_chars = (bootinfo->Framebuffer.width / 8) - 1;
                bootinfo->Console.max_line = (bootinfo->Framebuffer.height / 16) - 1;
            }
            break;

        case MULTIBOOT_TAG_TYPE_CMDLINE:
            bootinfo->params = ((struct multiboot_tag_string*) tag)->string;
            break;

        case MULTIBOOT_TAG_TYPE_BASIC_MEMINFO:
            bootinfo->MemoryInfo.available_memory = ((struct multiboot_tag_basic_meminfo *) tag)->mem_upper;
            break;

        default:
            break;
        }
    }
}

