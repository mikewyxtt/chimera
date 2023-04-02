/*
 *	[File]
 *	sys/loader/stage1.S
 * 
 *  [Description]
 *  Just after the MBR, this part of the bootloader preps the system a bit more before running the loader program itself
 *
 * Copyright (C) 2023 Michael Wyatt
 *
 * This file is part of Chimera.
 * Chimera is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation,
 * either version 3 of the License, or (at your option) any later version. Chimera is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
 * even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. You should have received a
 * copy of the GNU General Public License along with Chimera. If not, see <https://www.gnu.org/licenses/>.
 * 
 */

.code16 # use 16 bits
.global entry

/*
        CHANGE ENTRY TO STAGE1_ENTRY!!!!!!!!
*/

entry:
    # Set video mode to 80x25. This will also clear the screen
    MOV $0x00, %AH
    MOV $0X03, %AL
    INT $0x10

    JMP main

# Main bootloader function.
main:
    MOV $init_msg, %SI
    CALL println

    # Stop execution
    HLT

# Functions

/* [Function]
 *   println
 *
 * [Args]
 *   %SI: String to print
 *
 * [Description]
 *   Loads byte from %SI into %AL and increments %SI. Iterates through the string printing each char
 *   until %AL is '0'. Prints newline, moves cursor.
 */
println:
    LODSB

    CMP $0, %AL
    JE done

    CALL putc
    JMP println
    RET

/* [Function]
 *   putc
 *
 * [Args]
 *   %AL: Char to print
 *
 * [Description]
 *   Prints a character to the screen.
 */ 
putc:
    MOV $0xE, %AH
    INT $0x10
    RET


done:
    RET


/* Data section */
init_msg: .asciz "Chimera Bootloader stage1 v0.0\n"

/* Add magic number/boot signature for BIOS. Pad rest of the binary with zeros. */
.fill 510-(.-entry), 1, 0
.word 0xaa55
