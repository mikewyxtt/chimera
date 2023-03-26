/* 
 * Echo.java
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

public class Echo {
    public static void main (String[] args) {
		boolean n_flag = false;

		char space = ' ';
		char newline = '\n';

		int i = 0;

		/* 
		 * Check for "-n" flag. If present, change n_flag to true and move iterator forward to prevent "-n" from printing to the screen. This is a 
		 * quick and dirty way of doing this, but it works....
		 */
		if (args.length != 0) {
			if (args[0].equals("-n")) {
				n_flag = true;
				i++;
			}

			for (;i < args.length; i++) {
				System.out.print(args[i]);
				System.out.print(space);
			}
		}
		
		// Print a newline before exiting, unless -n was used
		if (!n_flag) {
			System.out.print(newline);
		}
    }
}
