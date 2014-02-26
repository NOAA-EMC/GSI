SHELL = /bin/bash

SUBS = pre fnl

all:
	- PWD=`pwd`;\
	for i in ${SUBS}; do \
		cd ${PWD}/$$i; \
		make $@; \
	done


clean:
	- PWD=`pwd`;\
	for i in ${SUBS}; do \
		cd ${PWD}/$$i; \
		make $@; \
	done
