/*
 *	[File]
 *	bin/cat/cat.kt
 * 
 *	[Description]
 *	Utility to concatinate a file.
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

import java.io.*

fun main(args: Array<String>) {
    
    for(f in args.indices) {
        try {
            var file = fopen(args[f])
            var r = file.read()

            while(r != -1) {
                print(r.toChar())
                r = file.read()
            }
        }

        catch(e: Exception) {
//          println("cat: " + e + ": No such file or directory.")
            println(e)
        }
    }
 }

/*
 * Display proper usage to the user upon error
 */
fun usage() {
    println("usage: cat [-u] [file...]")
 }
/*
 * Test to see if we can implement the open() syscall in this bitch
 */
fun fopen(path: String): FileInputStream {
    val file: File = File(path)
    val fstream: FileInputStream = FileInputStream(file)
    return fstream
}