#$ID:$
SHELL = /bin/sh

SUBS = src


all:
	- PWD=`pwd`;\
	for i in ${SUBS}; do \
		echo '<<<<' $$i '>>>>'; \
		cd ${PWD}/$$i; \
		make $@; \
	done

clean:
	- PWD=`pwd`;\
	for i in ${SUBS}; do \
		echo '<<<<' $$i '>>>>'; \
		cd ${PWD}/$$i; \
		make $@; \
	done
