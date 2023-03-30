/* 
 * echo.rs
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

use std::env;

fn main() {
	let args: Vec<String> = env::args().collect();
	let mut n_flag = false;

	let space = ' ';
	let newline = '\n';

	let mut i = 1;

	/* 
 	 * Check for "-n" flag. If present, change n_flag to true and move iterator forward to prevent "-n" from printing to the screen. This is a 
 	 * quick and dirty way of doing this, but it works....
 	 */
	if !args.len().eq(&1) {
		if args[1].eq("-n") {
			n_flag = true;
			i+=1;
		}

		while i < args.len() {
			print!("{}{}", args[i], space);
			i+=1;
		}
	}

	// Print newline before exiting, unless -n flag was used
	if !n_flag {
		print!("{}", newline);
	}
}
