userrcdir = ${HOME}/.gEDA
gschemrc = ${userrcdir}/gschemrc
scripts = moddate.scm keymap-translation.scm

.PHONY: all install uninstall

all:
	@echo "Type 'make install' or 'make uninstall'."
	@echo "Please see README for more information."
install:
	install -m 600 ${scripts} ${userrcdir}
	for i in ${scripts}; do \
		if ! grep -q '^[ \t]*(load[ \t]*"'$$i'"[ \t]*)' ${gschemrc}; then \
			echo '(load "'$$i'")' >> ${gschemrc}; \
		fi; \
	done

uninstall:
	for i in ${scripts}; do \
		rm ${userrcdir}/$$i; \
	done
	for i in ${scripts}; do \
		sed -i '/^[ \t]*(load[ \t]*"'$$i'"[ \t]*$)/ d' ${gschemrc}; \
	done
