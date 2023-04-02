##
## 	[File]
##	Makefile
##  
##  [Description]
##	Simple top level, non-recursive makefile to build the entire system.
## 
##  Copyright (C) 2023 Michael Wyatt
## 
##  This file is part of Chimera.
##  Chimera is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation,
##  either version 3 of the License, or (at your option) any later version. Chimera is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
##  even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. You should have received a
##  copy of the GNU General Public License along with Chimera. If not, see <https://www.gnu.org/licenses/>.
##


### HOW TO USE THIS MAKEFILE ###
## 'make buildkernel'		Compiles the kernel by itself
## 'make buildworld:'		Compiles userspace apps/bootloader
## 'make all'				Compiles everything
## 'make release'			Compiles everything and creates a bootable image
###


# Include the tool definitions
include include/tools.mk

.DEFAULT_GOAL := all

# Uncommenting this line allows you to see every command run by the makefile
#VERBOSE := 


## Include kernel and application makefiles ##
include sys/loader/loader.mk
#include sys/sys.mk
include bin/bin.mk


### Build targets ###
.PHONY: buildkernel
buildkernel: $(KERNEL_TARGETS)
	@echo "buildkernel complete."

.PHONY: buildworld
buildworld: $(WORLD_TARGETS)
	@echo "buildworld complete."

.PHONY: all
all: $(KERNEL_TARGETS) $(WORLD_TARGETS)

.PHONY: release
release: $(KERNEL_TARGETS) $(WORLD_TARGETS)
	@echo "buildworld + buildkernel complete."
	@sh tools/mkimage.sh

.PHONY: clean
clean:
	@rm -Rf $(WORLD_TARGETS) $(KERNEL_TARGETS) $(CLEAN_TARGETS)
	@echo "Source tree cleaned successfully."
