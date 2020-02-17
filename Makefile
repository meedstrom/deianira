# https://nullprogram.com/blog/2020/01/22/ for a guide
.POSIX:
.SUFFIXES: .el .elc
EMACS	= emacs
EL   	= escape-modality.elc escape-modality-common.elc
escape-modality-enforce-tidy.elc escape-modality-x11.elc escape-modality-test.elc
ELC  	= $(EL:.el=.elc)

compile: $(ELC)

check: escape-modality-test.elc
	$(EMACS) -Q --batch -L . -l escape-modality-test.elc -f ert-run-tests-batch

clean:
	rm -f $(ELC)

# For bug reproduction in a relatively clean environment
run: $(ELC)
    $(EMACS) -Q -L . -l foo-c.elc -f foo-mode

escape-modality.elc: escape-modality-common.elc escape-modality-enforce-tidy.elc
escape-modality-enforce-tidy.elc: escape-modality-common.elc
escape-modality-x11.elc: escape-modality-common.elc

.el.elc:
	$(EMACS) -Q --batch -L . -f batch-byte-compile $<
