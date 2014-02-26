SHELL = /bin/sh

paths_path = ../setup
include $(paths_path)/paths


#SUBS = 1dvar crtm fwd lib testbed
SUBS = lib fwd 1dvar testbed lib_cpp

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
