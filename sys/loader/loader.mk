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


# Assembles MBR
sys/loader/loader.o : sys/loader/loader.S
	$(AS) -c sys/loader/loader.S -o sys/loader/loader.o --target=i386-unknown-linux-gnu


# Links MBR into flat binary
sys/loader/loader.bin : sys/loader/loader.o
	$(LD) -o sys/loader/loader.bin -e init sys/loader/loader.o 	--oformat binary \
																	--section-start=.text=0x7c00

WORLD_TARGETS += sys/loader/loader.bin
CLEAN_TARGETS += sys/loader/loader.o