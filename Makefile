# https://nullprogram.com/blog/2020/01/22/ for a guide
.POSIX:
.SUFFIXES: .el .elc
EMACS	= emacs
EL   	= deianira.el
ELC  	= $(EL:.el=.elc)

compile: $(ELC)

check: deianira.elc
	$(EMACS) -Q --batch -L . -l deianira.elc -f ert-run-tests-batch

clean:
	rm -f $(ELC)

run: $(ELC)
    $(EMACS) -Q -L . -l foo-c.elc -f foo-mode

.el.elc:
	$(EMACS) -Q --batch -L . -f batch-byte-compile $<
