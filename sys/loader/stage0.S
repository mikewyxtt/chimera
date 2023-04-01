/*
 *	[File]
 *	sys/loader/stage0.S
 * 
 *  [Description]
 *  Bootloader MBR code. Performs no checks at all, just jumps to the first sector of the disk it loaded from.
 *
 * Copyright (C) 2023 Michael Wyatt
 *
 * This file is part of Chimera.
 * Chimera is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation,
 * either version 3 of the License, or (at your option) any later version. Chimera is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY# without
 * even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. You should have received a
 * copy of the GNU General Public License along with Chimera. If not, see <https://www.gnu.org/licenses/>.
 * 
 */
.code16
.global entry

/*
 * Constants
 */
 .set MAGIC,        0xaa55                  # Magic number so BIOS knows we're bootable
 .set ORIGIN,       0x600                   # Location in memory where we will move ourself before passing control to stage1
 .set LOAD,         0x7c00                  # Where we get loaded into memory initially, and also where we will load stage1 into memory

/*
 * We are essentially replicating the FreeBSD bootloader, but only including the bare minimum to get into kernel space.
 * We will follow suit to them and relocate our stage0 code to 0x600 so we can load stage1 at 0x7C00. I believe this is done
 * for chainloading purposes; other OS MBR are going to be linked to run from 0x7c00. It is not necessary for our OS but in the
 * future when we expand the functionality of this loader, we won't have to change a bunch of stuff...
 */

entry:
    CLD                                     # Something to do with the way strings are handled
    XORW %AX, %AX
    MOVW %AX, %ES
    MOVW %AX, %DS
    MOVW %AX, %SS
    MOVW $LOAD, %SP

    MOVW %SP, %SI
    MOVW $entry, %DI
    MOVW $0x100, %CX
    REP
    MOVSW

    MOVW %DI, %BP
    MOVB $0x8, %CL
    REP
    STOSW
    INCB -0xE(%DI)

    JMP load_stage1-LOAD+ORIGIN                    # We can now jump to 0x600

load_stage1:
    MOV $0x80, %DL                          # Tell the BIOS we want to access drive 0
    MOVW $LOAD, %BX                         # Tell the BIOS where we want it to read the sector to. We want to load it to 0x7c00 so we can jump to it
    MOV $0x02, %AH                          # Tell the BIOS we want to READ from a disk
    MOVB $0x01, %AL                         # Tell the BIOS we want to read 1 sector from the disk
    INT $0x13                               # BIOS disk I/O interrupt

    JMP *%BX                                # JMP to stage1



/* Add magic number/boot signature for BIOS. Pad rest of the file with zeros. */
.fill 510-(.-entry), 1, 0
.word MAGIC
