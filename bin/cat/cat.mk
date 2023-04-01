##
## 	[File]
##	bin/cat/cat.mk
##  
##  [Description]
##	Adds cat to WORLD_TARGET list.
## 
##  Copyright (C) 2023 Michael Wyatt
## 
##  This file is part of Chimera.
##  Chimera is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation,
##  either version 3 of the License, or (at your option) any later version. Chimera is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
##  even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. You should have received a
##  copy of the GNU General Public License along with Chimera. If not, see <https://www.gnu.org/licenses/>.
## 

bin/cat/cat.jar : bin/cat/cat.kt
	$(KOTLINC) -d $@ -include-runtime  $<

WORLD_TARGETS += bin/cat/cat.jar
