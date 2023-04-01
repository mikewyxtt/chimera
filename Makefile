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

# Include the tool definitions
include include/tools.mk

# Uncommenting this line allows you to see every command run by the makefile
#VERBOSE := 


.PHONY: all
all:
	$(ECHO) "ERROR: 'make all' not supported.\n"
	$(ECHO) "usage:"
	$(ECHO) "'make world'\t" "Builds user software"
	$(ECHO) "'make kernel'\t" "Builds kernel"
	$(ECHO) "'make release'\t" "Builds the entire system, then creates images"


# Include kernel and application makefiles
include sys/sys.mk
include bin/bin.mk


### Base system targets ###
.PHONY: kernel
kernel: $(KERNEL_TARGETS)

.PHONY: world
world: $(WORLD_TARGETS)


### Misc targets ###
## Removes build files
clean:
	@echo "Cleaning source tree..."
	@rm -Rf $(WORLD_TARGETS)
	@echo "Source tree cleaned."

# Builds entire system then creates ISO image
.PHONY: release
release: $(kernel) $(mkimage)

# Creates ISO image
.PHONY: mkimage
mkimage:
	$(ECHO) "Creating ISO image..."


# Include rules to build the stuff above ^^
include include/rules.mk