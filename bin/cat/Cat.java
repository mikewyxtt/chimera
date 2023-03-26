/*
 * Cat.java
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

import java.io.*;

public class Cat {
	public static void main(String[] args) {

		/* 
		 * For each file passed into the utility: create a new File object, open a connection to the file, then print the contents of the file
		 */
		for (String s: args) {
			try {
				File file = new File(s);
				FileInputStream filestream = new FileInputStream(file);     

				int r = 0;

				while((r = filestream.read()) != -1) {  
					System.out.print((char)r);  
				}

				filestream.close();
			}

			catch(Exception e) {   
				System.err.println("cat: " + s + ": No such file or directory");
			}
		}
	}

	/*
	 * Display proper usage to the user upon error
	 */
	void usage() {
		System.err.println("usage: cat [-u] [file ...]");
	}
}
