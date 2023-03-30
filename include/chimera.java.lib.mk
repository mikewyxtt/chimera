JAR_FLAGS += cvf

RUST_CLASSES := $(patsubst %.java,%.class,$(RUST_SRCS))

# Java source -> class pattern rule
%.class : %.java
	@$(ECHO) "[RUSTC]\t" $< "->" $@
	$(VERBOSE) $(RUSTC) $(RUSTC_FLAGS) $<


# Make a JAR
$(RUST_LIB): $(RUST_CLASSES)
#	@$(ECHO) "Making library JAR: " $(RUST_LIB) "\b..."
#	$(VERBOSE) $(JAR) $(JAR_FLAGS) $(RUST_LIB) $(RUST_CLASSES)

.PHONY: all
all: $(RUST_LIB)

.PHONY: clean
clean:
	$(VERBOSE) $(RM) -f $(RUST_CLASSES) $(RUST_LIB)
