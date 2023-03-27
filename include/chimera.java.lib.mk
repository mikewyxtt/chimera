JAR_FLAGS += cvf

JAVA_CLASSES := $(patsubst %.java,%.class,$(JAVA_SRCS))

# Java source -> class pattern rule
%.class : %.java
	@$(ECHO) "[JAVAC]\t" $< "->" $@
	$(VERBOSE) $(JAVAC) $(JAVAC_FLAGS) $<


# Make a JAR
$(JAVA_LIB): $(JAVA_CLASSES)
	@$(ECHO) "Making library JAR: " $(JAVA_LIB) "\b..."
	$(VERBOSE) $(JAR) $(JAR_FLAGS) $(JAVA_LIB) $(JAVA_CLASSES)

.PHONY: all
all: $(JAVA_LIB)

.PHONY: clean
clean:
	$(VERBOSE) $(RM) -f $(JAVA_CLASSES) $(JAVA_LIB)
