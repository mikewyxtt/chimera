JAVA_CLASSES := $(patsubst %.java,%.class,$(JAVA_SRCS))

# Java source -> class pattern rule
%.class : %.java
	@$(ECHO) "[JAVAC]\t" $< "->" $@
	$(VERBOSE) $(JAVAC) $(JAVAC_FLAGS) $<


# Make a JAR
$(PROG): $(JAVA_CLASSES)
	@$(ECHO) "Making JAR: " $(PROG) "\b..."
	$(VERBOSE) $(JAR) $(JAR_FLAGS) $(PROG) $(ENTRY_CLASS) $(JAVA_CLASSES)

.PHONY: all
all: $(PROG)

.PHONY: clean
clean:
	$(VERBOSE) $(RM) -f $(JAVA_CLASSES) $(PROG)
