/* Functions to facilitate logging early in the boot process */

#ifndef EARLY_LOG_H
#define EARLY_LOG_H

#include <stdint.h>
#include "bootinfo.h"

/* Enable serial debugging */
#define SERIAL_LOG

/* Early logging function, similar to printf. Allows all system components to print messages in a decentralized manner */
void early_log(struct BootInfo *bootinfo, const char *format, ...);


#endif
