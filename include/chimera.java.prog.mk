# Makefile include for Java programs

JAR_FLAGS += cvfe

RUST_CLASSES := $(patsubst %.java,%.class,$(RUST_SRCS))

# Java source -> class pattern rule
%.class : %.java
	@$(ECHO) "[RUSTC]\t" $< "->" $@
	$(VERBOSE) $(RUSTC) $(RUSTC_FLAGS) $<


.PHONY: all
all: $(PROG)

.PHONY: clean
clean:
	$(VERBOSE) $(RM) -f $(RUST_CLASSES) $(PROG)
