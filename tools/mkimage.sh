##
## 	[File]
##	tools/mkimage.sh
##  
##  [Description]
##	Script that creates a bootable disk image
## 
##  Copyright (C) 2023 Michael Wyatt
## 
##  This file is part of Chimera.
##  Chimera is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation,
##  either version 3 of the License, or (at your option) any later version. Chimera is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
##  even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. You should have received a
##  copy of the GNU General Public License along with Chimera. If not, see <https://www.gnu.org/licenses/>.
##

echo "Creating release image..."


# Create a basic MBR disk image
hdiutil create -fs FAT32 -size 50m -volname Chimera -layout MBRSPUD chimera.img

# Write stage0, skip over partition table, write Magic number for last two bytes
dd if=stage0 of=chimera.dmg conv=notrunc bs=1 count=446
dd if=stage0 of=chimera.dmg conv=notrunc bs=1 count=2 skip=510 seek=510

# Write stage1 at the very beginning of the first partition
dd if=stage1 of=chimera.dmg conv=notrunc bs=1 count=512 seek=512

# Create dir for IMG files
mkdir -p IMG


# Cleanup
rm -Rf IMG floppy.img bootdisk.img