# Makefile include for Java programs

JAR_FLAGS += cvfe

JAVA_CLASSES := $(patsubst %.java,%.class,$(JAVA_SRCS))

# Java source -> class pattern rule
%.class : %.java
	@$(ECHO) "[JAVAC]\t" $< "->" $@
	$(VERBOSE) $(JAVAC) $(JAVAC_FLAGS) $<


# Make a JAR
$(JAVA_PROG): $(JAVA_CLASSES)
	@$(ECHO) "Making JAR: " $(JAVA_PROG) "\b..."
	$(VERBOSE) $(JAR) $(JAR_FLAGS) $(JAVA_PROG) $(ENTRY_CLASS) $(JAVA_CLASSES)

.PHONY: all
all: $(JAVA_PROG)

.PHONY: clean
clean:
	$(VERBOSE) $(RM) -f $(JAVA_CLASSES) $(JAVA_PROG)
