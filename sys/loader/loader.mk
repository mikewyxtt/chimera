##
## 	[File]
##	sys/loader/loader.mk
##  
##  [Description]
##	Creates bootloader build target
## 
##  Copyright (C) 2023 Michael Wyatt
## 
##  This file is part of Chimera.
##  Chimera is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation,
##  either version 3 of the License, or (at your option) any later version. Chimera is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
##  even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. You should have received a
##  copy of the GNU General Public License along with Chimera. If not, see <https://www.gnu.org/licenses/>.
## 


# Assemble and link stage0 
sys/loader/stage0 : sys/loader/stage0.S
	$(AS) -c $< -o $@.o --target=i386-unknown-linux-gnu
	$(LD) -o $@ -e entry $@.o --oformat binary --section-start=.text=0x600

# Assemble and link stage1
sys/loader/stage1 : sys/loader/stage1.S
	$(AS) -c $< -o $@.o --target=i386-unknown-linux-gnu
	$(LD) -o $@ -e entry $@.o --oformat binary --section-start=.text=0x7C00

WORLD_TARGETS += 	sys/loader/stage0 \
					sys/loader/stage1

CLEAN_TARGETS += 	sys/loader/stage0.o \
					sys/loader/stage1.o