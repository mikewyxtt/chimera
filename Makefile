SUBDIRS := bin/cat bin/echo

.PHONY: all clean

all:
	@echo "Building system..."
	@for dir in $(SUBDIRS); do \
		make -I $(PWD)/include -C $$dir all; \
	done

clean:
	@echo "Cleaning build files..."
	@for dir in $(SUBDIRS); do \
		make -I $(PWD)/include -C $$dir clean; \
	done
