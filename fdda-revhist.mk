#!/bin/gmake
SHELL	= sh
ME	= gsi GSI_GridComp

head: HEAD
#HEAD: GEOSdas-fdda-beta1
#HEAD: jg-gsi-[GEOSdas-fdda-beta1]-f1
#HEAD: jg-gsi-nceptb200804-asis
HEAD: jg-gsi-ncep200804-merged

jg-gsi-ncep200804-merged: jg-gsi-ncep200804 ry-gAdas-[rt-gAdas-beta11p2]-p1
	: $@: $^
	:	merged with GMAO location changes in beta11p2.
	:	. added m_dgeevx.F90 for a portable dgeev;
	:	. removed GMAO background bias correction;
	:	. removed NCEP gfsio_module dependency;
	:	. backtracked wrf_binary_interface.F90 and
	:	  wrf_netcdf_interface.F90 to beta11p2 because
	:	  of an unknown procedure;
	:	. Kept Makefile and Makefile.dependency asis for now;
	:	. etc.
	:

jg-gsi-ncep200804: ncep-gsi-2006_09 jg-gsi-nceptb200804-asis
	: $@: $^
	:	merged as is --for completeness at GMAO-- between the
	:	two given revisions.
	:

jg-gsi-nceptb200804-asis:
	: $@: $^
	:	This tag contains "Q4FY08.data_upgrade/sorc/gsi/" of the
	:	tar.gz file released by NCEP from their ftp site --for
	:	GMAO only--.  It includes the source code and make
	:	scripts from sub-directory src/, the NCEP shell program
	:	executing global GSI inbedded a setup namelist, and five
	:	globa_xxxx.txt input files.  Most files are included as-
	:	is, except two filename changes:
	:
	:		mv dprodx.f90    dprodx.F90,
	:		mv Makefile.conf Makefile.conf.AIX
	:

jg-gsi-[GEOSdas-fdda-beta1]-f1: GEOSdas-fdda-beta1 ry-gAdas-[rt-gAdas-beta11p2]-p1
	: $@: $^
	:	with fixes to include TLNMC tested in
	:	ry-gAdas-[rt-gAdas-beta11p2]-p1.
	:

GEOSdas-fdda-beta1: rt-gAdas-beta11p2
	: $@: $^
	:	first FDDA release for out of developer applications.
	:

ry-gAdas-[rt-gAdas-beta11p2]-p1: rt-gAdas-beta11p2
	: $@: $^
	:	with local changes to include TLNMC -- Tangent-Linear
	:	Normal Mode Constraint.
	:

rt-gAdas-beta11p2: ncep-gsi-2006_09
	: $@: $^
	:	GMAO local build
	:

ncep-gsi-2006_09: ncep-gsi-2006_06
	: $@: $^
	:	NCEP CVS release which is appeared to be almost the same
	:	as in as the tarball release on NCEP GSI website with minor
	:	differences in mpimod.F90.
	:

ncep-gsi-2006_06:
	: $@: $^
	:	NCEP CVS release.
	:
