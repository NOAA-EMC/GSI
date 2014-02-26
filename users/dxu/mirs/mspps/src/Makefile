SHELL = /bin/bash

SUBS = AMSUA AMSUA_1bs AMSUB_1bs MHS
 

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
