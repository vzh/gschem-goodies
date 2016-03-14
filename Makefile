userrcdir = ${HOME}/.gEDA
exampledir = examples
gschemrc = ${userrcdir}/gschemrc
scripts = moddate.scm keymap-translation.scm cache.scm

inputs := $(scripts:%=${exampledir}/%)
outputs := $(scripts:%=${userrcdir}/%)

.PHONY: all install uninstall

all:
	@echo "Type 'make install' or 'make uninstall'."
	@echo "Please see README for more information."


${outputs}: ${userrcdir}/%:${exampledir}/%
	install -m 600 $< $@
	if ! grep -q '^[ \t]*(load[ \t]*"'$*'"[ \t]*)' ${gschemrc}; then \
		echo '(load "'$*'")' >> ${gschemrc}; \
	fi

install: ${outputs}

uninstall:
	rm ${outputs}
	for i in ${scripts}; do \
		sed -i '/^[ \t]*(load[ \t]*"'$$i'"[ \t]*$)/ d' ${gschemrc}; \
	done
