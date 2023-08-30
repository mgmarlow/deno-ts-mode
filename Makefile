EMACS := emacs -Q -batch

LOADPATH := -L .

PACKAGE_FILES := $(filter-out $(wildcard *-test.el), $(wildcard *.el))

TEST_FILES := $(wildcard *-test.el)

COMPILED_FILES := $(wildcard *.elc)

.PHONY: compile test clean

test: compile
	$(EMACS) $(LOADPATH) -l ert -l $(TEST_FILES) -f ert-run-tests-batch-and-exit

compile: clean
	$(EMACS) $(LOADPATH) -f batch-byte-compile $(PACKAGE_FILES)

clean:
	rm -f $(COMPILED_FILES)
