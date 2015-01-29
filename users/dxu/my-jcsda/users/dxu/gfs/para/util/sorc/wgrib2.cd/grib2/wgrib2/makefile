#
# this makefile is for gnu-make on a linux box
# wgrib2 rerequires grib2c (NCEP C grib2), jasper (jpeg), z and png libraries
#
#  REQUIRES GNU make
# mod 1/07 M. Schwarb
# mod 2/07 W. Ebisuzaki changes for *.dat files
# mod 8/07 W. Ebisuzaki cleanup
# mod 4/09 W. Ebisuzaki config.h, netcdf4
#

SHELL=/bin/sh


all:=$(patsubst %.c,%.o,$(wildcard *.c))
code:=$(filter-out fnlist.o,$(all))
o=$(wildcard *.o)
h:=grb2.h  wgrib2.h fnlist.h config.h
options=$(wildcard [A-Z]*.c)
CODE_TABLE_DAT=$(wildcard CodeTable_[0-9].[0-9]*.dat)
FLAGS=${DEFS} ${CPPFLAGS} ${CFLAGS}

wgrib2: $h ${all}
#	${CC} -o wgrib2 ${FLAGS} ${all} ${LDFLAGS}
	${CC} -o wgrib2 ${CFLAGS} ${all} ${LDFLAGS}
	rm Config.o

fast:	${code}
	touch fnlist.o fnlist.c fnlist.h
	${CC} -o wgrib2 ${FLAGS} ${all} ${LDFLAGS}
	rm Config.o

fnlist.c:	${options}
	./function.sh 

fnlist.h:	${options}
	./function.sh

Help.o:	Help.c wgrib2.h
	${CC} -c ${FLAGS} Help.c

CodeTable.o:	CodeTable.c ${CODE_TABLE_DAT}
	${CC} -c ${FLAGS} CodeTable.c

cname.o:	cname.c gribtab.dat
	${CC} -c ${FLAGS} cname.c

Sec1.o:	Sec1.c code_table0.dat ncep_tableC.dat
	${CC} -c ${FLAGS} Sec1.c

gribtab.o:	gribtab.c gribtab.dat misc_gribtab.dat
	${CC} -c ${FLAGS} gribtab.c

Mod_grib.o:	Mod_grib.c NCEP_local_levels_test.h
	${CC} -c ${CFLAGS} Mod_grib.c


.c.o:	$(*).c
	$(CC) -c ${FLAGS}  $*.c

clean:
	touch wgrib2
	rm ${o} wgrib2
