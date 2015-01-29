SHELL=	/bin/sh
FCOMP = ncepxlf95
SRC_DIR = ./

OBJS = consts.o \
       read_write_utils.o \
       soil_utils.o \
       interp_utils.o \
       interp_utils_nesdis.o \
       ll2xy_utils.o

include makefile.conf

${LIB}:	${OBJS}
	ar -rv -X64 ${LIB} ${OBJS}
	rm ${OBJS} 

consts.o: 
	${FCOMP} ${FFLAGS} -c ${SRC_DIR}/consts.f

read_write_utils.o: 
	${FCOMP} ${FFLAGS} -c ${SRC_DIR}/read_write_utils.f

soil_utils.o: consts.o
	${FCOMP} ${FFLAGS} -c ${SRC_DIR}/soil_utils.f

interp_utils.o: ll2xy_utils.o
	${FCOMP} ${FFLAGS} -c ${SRC_DIR}/interp_utils.f

interp_utils_nesdis.o: ll2xy_utils.o
	${FCOMP} ${FFLAGS} -c ${SRC_DIR}/interp_utils_nesdis.f

ll2xy_utils.o:
	${FCOMP} ${FFLAGS} -c ${SRC_DIR}/ll2xy_utils.f

clean:
	rm -f *.o ${MOD_DIR}/*.mod ${LIB}
