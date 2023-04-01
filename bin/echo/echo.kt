/*
 *	[File]
 *	bin/echo/echo.kt
 * 
 *	[Description]
 *	Utility to print text to screen
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


fun main(args: Array<String>) {
	val space = ' '
	val newline = '\n'

    var n_flag = false;
    var i = 0

	/* 
 	 * Check for "-n" flag. If present, change n_flag to true and move iterator forward to prevent "-n" from printing to the screen. This is a 
 	 * quick and dirty way of doing this, but it works....
 	 */
	if (args.size >= 1) {
		if (args[0] == "-n") {
			n_flag = true
			i += 1
		}
        
		while (i < args.size) {
            print(args[i])
            print(space)
			i += 1
		}
	}

	// Print newline before exiting, unless -n flag was used
	if (!n_flag) {
		print(newline)
	}
}
