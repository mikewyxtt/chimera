#include <stdint.h>
#include <stdbool.h>
#include "multiboot.h"
#include "../../lib/early_log.h"
#include "../../lib/bootinfo.h"


void main(uint32_t magic, uint32_t *multiboot_header_addr) {
    // Ensure we were loaded by a Multiboot 2 compliant bootloader
    if(magic != MULTIBOOT2_BOOTLOADER_MAGIC) {
        // do something? hang for now..
        while(1);
    }

    // Create and initialize BootInfo struct
    struct BootInfo bootinfo;
    bootinfo.EarlyLogBuffer.size = sizeof(bootinfo.EarlyLogBuffer.buffer);
    bootinfo.Framebuffer.enabled = true;

    #ifdef SERIAL_LOG
        bootinfo.Serial.enabled = true;
        bootinfo.Serial.port = 0x3f8;
    #endif

    
    early_log(&bootinfo, "Test Number: 0x%x\n", 3);
    early_log(&bootinfo, "Magic Number: 0x%x\n", magic);
    early_log(&bootinfo, "Multiboot header addr: 0x%x\n", &multiboot_header_addr);

    while(1);
}


// void multiboot_find_tag(int tag_num, uint32_t multiboot_header_addr) {
//     while(1) {
        
//     }
// }




