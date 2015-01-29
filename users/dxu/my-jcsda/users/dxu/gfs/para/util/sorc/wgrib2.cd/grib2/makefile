# REQUIRES GMAKE!!!!
#
# makefile for wgrib2
# 
# compiles every #@?! library needed by wgrib2
# then tries to compile wgrib2
#
# mod 1/07 M. Schwarb (libgrib2c name change)
# mode 4/09 W. Ebisuzaki (use config.h)
# mode 6/10 W. Ebisuzaki ipolates
#
# this version uses netcdf libraries -- compile C only version
#  get from UCAR if compile doesn't work
#
#
# on NCEP AIX
# export CC=/usr/vacpp/bin/xlC_r
# export CPP=/usr/bin/cpp
# for OS-X: uncomment line for makefile -f scripts/makefile.darwin
#
SHELL=/bin/sh

# netcdf3: write netcdf files with netcdf-3 library
# netcdf4: write netcdf files with netcdf-4 library
# regex: regular expression package used by (match,not), POSIX-2
# tigge: enable -tigge option for tigge names
# mysql: write to mysql files
# ipolates: fortran interpolation library
#
# the flags are stored in wgrib2/config.h
#
USE_NETCDF4=0
USE_NETCDF3=1
USE_REGEX=1
USE_TIGGE=1
USE_MYSQL=0
USE_IPOLATES=0

# wCPPFLAGS has the directory of the includes 
# wLDFLAGS has the directory/name of the library


wCPPFLAGS:=-O2
wLDFLAGS:=
cwd:=${CURDIR}

CONFIG_H=${cwd}/wgrib2/config.h
a:=$(shell echo "/* config.h */" > ${CONFIG_H})

ifeq ($(USE_REGEX),1)
   a:=$(shell echo "\#define USE_REGEX" >> ${CONFIG_H})
else
   a:=$(shell echo "//\#define USE_REGEX" >> ${CONFIG_H})
endif

ifeq ($(USE_TIGGE),1)
   a:=$(shell echo "\#define USE_TIGGE" >> ${CONFIG_H})
else
   a:=$(shell echo "//\#define USE_TIGGE" >> ${CONFIG_H})
endif

# grib2c library

g:=${cwd}/g2clib-1.1.8
glib:=$g/libgrib2c.a
wLDFLAGS+=-L$g -lgrib2c
wCPPFLAGS+=-I$g

# Jasper

j=${cwd}/jasper-1.900.1
jlib=$j/src/libjasper/.libs/libjasper.a
wLDFLAGS+=-L$j/src/libjasper/.libs -ljasper
wCPPFLAGS+=-I$j/src/libjasper/include


ifeq ($(USE_NETCDF3),1)
   n:=${cwd}/netcdf-3.6.2
   nlib:=$n/libsrc/.libs/libnetcdf.a
   wLDFLAGS+=-L$n/libsrc/.libs -lnetcdf
   wCPPFLAGS+=-I$n/libsrc
   a:=$(shell echo "\#define USE_NETCDF3" >> ${CONFIG_H})
else
   a:=$(shell echo "//\#define USE_NETCDF3" >> ${CONFIG_H})
endif

ifeq ($(USE_NETCDF4),1)
   n4:=${cwd}/netcdf-4.0.1
   n4lib:=${n4}/libsrc4/.libs/libnetcdf.a
   h5:=${cwd}/hdf5-1.8.2
   h5lib:=${h5}/src/.libs/libhdf5.a
#   wLDFLAGS+=-L${n4}/libsrc/.libs -lnetcdf
   wLDFLAGS+=-L${n4}/libsrc4/.libs -lnetcdf -L${h5}/hl/src/.libs -lhdf5_hl -L${h5}/src/.libs -lhdf5
   wCPPFLAGS+=-I${n4}/libsrc4
   a:=$(shell echo "\#define USE_NETCDF4" >> ${CONFIG_H})
else
   a:=$(shell echo "//\#define USE_NETCDF4" >> ${CONFIG_H})
endif

ifeq ($(USE_MYSQL),1)
   wCPPFLAGS+=`mysql_config --cflags`
   wLDFLAGS+=`mysql_config --libs`
   a:=$(shell echo "\#define USE_MYSQL" >> ${CONFIG_H})
else
   a:=$(shell echo "//\#define USE_MYSQL" >> ${CONFIG_H})
endif

ifeq ($(USE_IPOLATES),1)
   ip:=${cwd}/iplib
   iplib:=${ip}/libipolate.a
   wLDFLAGS+=-L${ip} -lipolate 

#  for compiling with fortran library
#  wLDFLAGS+= (libraries need by the fortran code)
#  wCPPFLAGS+= -D(FORTRAN Name)   see New_grid.c
#  F90=(command to compile fortran code)  see iplib/makefile

# for G95 - personal system
   wLDFLAGS+=-L/export/wesley/wd51we/g95-install/lib/gcc-lib/i686-pc-linux-gnu/4.0.3 -lf95
   wCPPFLAGS+=-DG95
   F90=g95

# for gfortran - ubuntu
#   wLDFLAGS+=-lgfortran
#   wCPPFLAGS+=-DGFORTRAN
#   F90=gfortran

# NCEP CCS:
#   wLDFLAGS+=-L/usr/lib - -lxlf90 
#   wCPPFLAGS+=-DXLF
#   F90=xlf

   a:=$(shell echo "\#define USE_IPOLATES" >> ${CONFIG_H})
else
   a:=$(shell echo "//\#define USE_IPOLATES" >> ${CONFIG_H})
endif

# png 

p=${cwd}/libpng-1.2.43
plib=$p/.libs/libpng.a
wLDFLAGS+=-L$p/.libs -lpng
wCPPFLAGS+=-I$p

# z

z=${cwd}/zlib-1.2.5
zlib=$z/libz.a
wLDFLAGS+=-L$z -lz
wCPPFLAGS+=-I$z

wLDFLAGS+=-lm
wCPPFLAGS+=-I/usr/include ${CPPFLAGS}

# -----------------------------------------------------

wLDFLAGS+=-lm
wCPPFLAGS+=-I/usr/include ${CPPFLAGS}

# -----------------------------------------------------

w=wgrib2
prog=$w/wgrib2


# check if make is GNU make else use gmake
make_is_gnu:=$(word 1,$(shell make -v))
ifeq ($(make_is_gnu),GNU)
   MAKE:=make
else
   MAKE:=gmake
endif


${prog}:        $w/*.c $w/*.h ${jlib} ${nlib} ${zlib} ${plib} ${h5lib} ${glib} ${n4lib} ${iplib}
	cd $w && export LDFLAGS="${wLDFLAGS}" && export CPPFLAGS="${wCPPFLAGS}" && ${MAKE}

fast:        $w/*.c $w/*.h ${jlib} ${nlib} ${zlib} ${plib} ${h5lib} ${glib} ${n4lib} ${iplib}
	cd $w && export LDFLAGS="${wLDFLAGS}" && export CPPFLAGS="${wCPPFLAGS}" && ${MAKE}


${jlib}:
	cp $j.tar.gz tmpj.tar.gz
	gunzip -n tmpj.tar.gz
	tar -xvf tmpj.tar
	rm tmpj.tar
	cd $j && ./configure --without-x --disable-libjpeg --disable-opengl && ${MAKE}

${plib}:	${zlib}
	cp $p.tar.gz tmpp.tar.gz
	gunzip -n tmpp.tar.gz
	tar -xvf tmpp.tar
	rm tmpp.tar
#       for OSX
#	export LDFLAGS="-L$z" && cd $p && export CPPFLAGS="${wCPPFLAGS}" && make -f scripts/makefile.darwin
#	for everybody else
	export LDFLAGS="-L$z" && cd $p && export CPPFLAGS="${wCPPFLAGS}" && ./configure --disable-shared && ${MAKE}

${zlib}:
	cp $z.tar.gz tmpz.tar.gz
	gunzip -n tmpz.tar.gz
	tar -xvf tmpz.tar
	rm tmpz.tar
	cd $z && ./configure && ${MAKE}


${glib}:	${jlib} ${plib} ${zlib}
	touch ${glib}
	rm ${glib}
	cd $g && export CPPFLAGS="${wCPPFLAGS}" && ${MAKE}


${nlib}:
	cp netcdf.tar.gz tmpn.tar.gz
	gunzip -n tmpn.tar.gz
	tar -xvf tmpn.tar
	rm tmpn.tar
	cd $n && ./configure --enable-c-only && ${MAKE} check

${n4lib}:	${zlib} ${h5lib}
	mkdir -p ${cwd}/zlib/include
	mkdir -p ${cwd}/zlib/lib
	cp ${z}/*.h ${cwd}/zlib/include/
	cp ${z}/*.a ${cwd}/zlib/lib/

	mkdir -p ${cwd}/hdf5/include
	cp  ${h5}/src/*.h ${h5}/hl/src/*.h ${cwd}/hdf5/include
	mkdir -p ${cwd}/hdf5/lib
	cp ${h5}/src/.libs/*a ${h5}/hl/src/.libs/*a ${cwd}/hdf5/lib/

	cp ${n4}.tar.gz tmpn.tar.gz
	gunzip -n tmpn.tar.gz
	tar -xvf tmpn.tar
	rm tmpn.tar
	cd ${n4} && ./configure --enable-netcdf-4 --enable-c-only --with-zlib=${cwd}/zlib --with-hdf5=${cwd}/hdf5 && ${MAKE}

#	cd ${n4} && ./configure --enable-c-only && ${MAKE} check

#	cd ${n4} && ./configure --enable-netcdf-4 --enable-c-only --with-zlib=${cwd}/zlib --with-hdf5=${cwd}/hdf5 && ${MAKE} check

#	cd ${n4} && ./configure --enable-c-only && ${MAKE} check

#	cd ${n4} && ./configure --enable-netcdf-4 --enable-c-only && ${MAKE} check


${h5lib}:
	cp ${h5}.tar.gz tmph5.tar.gz
	gunzip -n tmph5.tar.gz
	tar -xvf tmph5.tar
	rm tmph5.tar
	cd ${h5} && ./configure --disable-shared --disable-fortran --with-zlib=$z && ${MAKE} && ${MAKE}
#	cd ${h5} && ./configure --disable-shared --disable-fortran --with-zlib=$z && ${MAKE} && ${MAKE} check

${iplib}:
	cd ${ip} && export F90=${F90} && make

clean:
	cd $w && ${MAKE} clean
	cd $g && touch junk.a junk.o && rm *.o *.a
	rm -rf $n
	rm -rf $j
	rm -rf $p
	rm -rf $z
	rm -rf ${n4}
	rm -rf ${h5}

