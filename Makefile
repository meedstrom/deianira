# https://nullprogram.com/blog/2020/01/22/ for a guide
.POSIX:
.SUFFIXES: .el .elc
EMACS	= emacs
EL   	= escape-modality.el
ELC  	= $(EL:.el=.elc)

compile: $(ELC)

check: escape-modality.elc
	$(EMACS) -Q --batch -L . -l escape-modality.elc -f ert-run-tests-batch

clean:
	rm -f $(ELC)

run: $(ELC)
    $(EMACS) -Q -L . -l foo-c.elc -f foo-mode

.el.elc:
	$(EMACS) -Q --batch -L . -f batch-byte-compile $<
