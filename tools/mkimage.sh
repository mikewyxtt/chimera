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

# Create dir for ISO files
mkdir -p ISO

# Make bootable 14.4MB floppy disk image(full size would be 2880 sectors but we use 2876 to account for 4 sector MBR)
dd if=mbr.bin of=bootdisk.img bs=512 count=1
dd if=/dev/zero of=bootdisk.img bs=512 count=2876 seek=1
cp bootdisk.img ISO

# Create the ISO, delete temporary files
mkisofs -o chimera.iso -V Chimera -b bootdisk.img ISO
rm -Rf ISO floppy.img bootdisk.img