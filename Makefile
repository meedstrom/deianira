# https://nullprogram.com/blog/2020/01/22/ for a guide
.POSIX:
.SUFFIXES: .el .elc
EMACS	= emacs
EL   	= escape-modality.el escape-modality-common.el escape-modality-enforce-tidy.el escape-modality-x11.el escape-modality-test.el
ELC  	= $(EL:.el=.elc)

compile: $(ELC)

check: escape-modality-test.elc
	$(EMACS) -Q --batch -L . -l escape-modality-test.elc -f ert-run-tests-batch

clean:
	rm -f $(ELC)

run: $(ELC)
    $(EMACS) -Q -L . -l foo-c.elc -f foo-mode

escape-modality.elc: escape-modality-common.elc escape-modality-enforce-tidy.elc
escape-modality-enforce-tidy.elc: escape-modality-common.elc
escape-modality-x11.elc: escape-modality-common.elc
escape-modality-test.elc: $(ELC)

.el.elc:
	$(EMACS) -Q --batch -L . -f batch-byte-compile $<
