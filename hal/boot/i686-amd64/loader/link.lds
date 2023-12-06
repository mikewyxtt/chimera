/* Entry point symbol, aka the first function that is called once loaded. */
/*OUTPUT_FORMAT("i-pei-i386") */
ENTRY(_start)

SECTIONS 
{
	/* Starting adddress, we use 1MiB. */
	. = 0x100000;

	/* Code section */
	.text ALIGN(4096) : {
		*(.text)
	}

	/* Read-only data */
	.rodata ALIGN(4096) : {
		*(.rodata)
	}

	/* Read/Write data */
	.data ALIGN(4096) : {
		*(.data)
	}

	/* Uninitialized Read/Write data + Stack */
	.bss ALIGN(4096) : {
		*(COMMON)
		*(.bss)
	}
}

