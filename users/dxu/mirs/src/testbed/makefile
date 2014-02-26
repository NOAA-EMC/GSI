#$ID:$
SHELL = /bin/sh

SUBS =  biasGenerAndMonit \
	chopp \
	fm \
	mergeEDR \
	mergeNEDTofDiffInstr \
	nwp \
	rdr2tdr \
	retrHeritage \
	retrRegress \
	tdr2sdr \
        vipp \
        grid \
	mirs2nc
        #convExternalFormats \


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
