/*
 *	[File]
 *	bin/pwd/pwd.kt
 * 
 *	[Description]
 *	Utility to print $PWD to STDIO
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
    
    println(System.getenv("PWD"))
 }

/*
 * Display proper usage to the user upon error
 */
fun usage() {
    // in the future we will add the flags for PWD
 }
