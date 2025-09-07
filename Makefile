PREFIX         ?= /usr
GUILE_SITEDIR  ?= $(PREFIX)/share/guile/site/3.0
PROJECT        := icnu
SRC_DIR        := $(PROJECT)
DESTDIR        ?=

.PHONY: all build install uninstall clean test

all: build

# Compiles all source files into .go bytecode files.
build:
	@echo "Compiling all $(PROJECT) modules..."
	@guild compile -L . $(shell find $(SRC_DIR) -name '*.scm')

# Installs the .scm source files into the system's Guile site directory.
install: build
	@echo "Installing source files to $(DESTDIR)$(GUILE_SITEDIR)/"
	@install -d $(DESTDIR)$(GUILE_SITEDIR)
	@cp -a $(SRC_DIR) $(DESTDIR)$(GUILE_SITEDIR)/

# Runs all test suites.
test:
	@echo "Running tests..."
	@if [ -f tests/ic-lib-tests.scm ]; then \
		guile -L . tests/ic-lib-tests.scm || exit 1; \
	fi
	@for f in tests/*-tests.scm; do \
		if [ "$$f" != "tests/ic-lib-tests.scm" ]; then \
			echo "--- Running $$f ---"; \
			guile -L . $$f || exit 1; \
		fi \
	done

# Removes the installed files.
uninstall:
	@echo "Uninstalling from $(DESTDIR)$(GUILE_SITEDIR)/$(PROJECT)"
	@rm -rf $(DESTDIR)$(GUILE_SITEDIR)/$(PROJECT)

# Removes compiled bytecode files and Guile's cache.
clean:
	@echo "Cleaning compiled files and cache"
	@find . -type f -name '*.go' -delete
	@-rm -rf $(HOME)/.cache/guile/ccache
