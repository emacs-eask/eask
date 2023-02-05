EMACS ?= emacs
EASK ?= eask

.PHONY: clean checkdoc lint package install compile test

ci: clean package install compile

build-source:
	@echo "Building source..."
	$(EASK) load "scripts/generate-source.el" --allow-error

generate_badges_system:
	@echo "Generating system badges.."
	@$(EASK) load "scripts/generate-badges-system.el"

package:
	@echo "Packaging..."
	$(EASK) package

install:
	@echo "Installing..."
	$(EASK) install

compile:
	@echo "Compiling..."
	$(EASK) compile

test:
	@echo "Testing..."
	$(EASK) test ert ./test/*.el

checkdoc:
	@echo "Run checkdoc..."
	$(EASK) lint checkdoc

lint:
	@echo "Run package-lint..."
	$(EASK) lint package

clean:
	$(EASK) clean all
