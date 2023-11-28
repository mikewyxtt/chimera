Chimera is a UNIX-like operating system.

Notable design choices:
	- Modular design; similar to a microkernel but all servers and drivers operate inside a shared address space to avoid performance consequences associated with message passing. Rust's protection mechanisms are good enough to ensure stability. 
	- Networking Kernel, message passing utilizes packet based system similar to TCP/IP
	- Hardware Abstraction Layer to ensure easy portability
