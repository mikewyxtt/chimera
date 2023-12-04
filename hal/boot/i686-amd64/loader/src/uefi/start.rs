/**/

/* 
 * The UEFI firmware places 3 32 bit pointers on the stack before giving us control:
 *
 *                  -----------------------------------
 *                 |      Pointer      |    Location   |
 *                 |-----------------------------------|
 *                 | EFI_SYSTEM_TABLE  |   [ESP + 8]   |
 *                 |-----------------------------------|
 *                 |    EFI_HANDLE     |   [ESP + 4]   |
 *                 |-----------------------------------|
 *                 |  Return Address   |   [ESP + 0]   |
 *                  -----------------------------------
 *
 * The return address is irrelevant as we don't plan on returning to the UEFI boot manager and EFI_HANDLE is just a pointer to the executable file's file header. The only pointer we need
 * is the EFI_SYSTEM_TABLE. We will use the EFI_SYSTEM_TABLE pointer to locate all of the essential information/functions from the UEFI firmware. e.g. to print to the console, we use this
 * pointer to find the Boot Services Table, which we use to find the Console Support Protocol, which we then read to make a function pointer to EFI_SIMPLE_TEXT_OUTPUT_PROTOCOL.OutputString(),
 * which obviously prints a string if you call it with the proper arguments.
 *
 */

/* _start is the universal entry point for both the BIOS and UEFI variations of the loader. Here we simply need to store the pointer to the efi system table, initialize some things, and pass control to main().  */
pub fn _start() {
//    main();

    loop {}
}

