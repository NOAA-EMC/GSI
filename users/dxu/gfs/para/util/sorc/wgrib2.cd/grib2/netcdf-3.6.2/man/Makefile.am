## This is a automake file, part of Unidata's netCDF package.

# Copyright 2005-2006, see the COPYRIGHT file for more information.
# This file builds the netcdf documentation.

# The "all" target does nothing in this directory. The "dist" target
# *does* build the documentation, because the distribution contains
# all of the built documentation.

# $Id: Makefile.am,v 1.38 2007/01/10 15:41:48 ed Exp $

# These are the source files for the netcdf manuals which go with the
# C library.
texi_files = netcdf.texi netcdf-install.texi netcdf-c.texi	\
netcdf-tutorial.texi

# This is the html version of the man page for the C library.
html_mans = netcdf-man-3.html

# If utilities are being built, add their man pages.
if BUILD_UTILITIES
html_mans += ncdump-man-1.html ncgen-man-1.html 
endif

# Add F77 manual if user is building f77 API.
if BUILD_F77
texi_files += netcdf-f77.texi 
html_mans += netcdf_f77-man.html
endif

# Add F90 manual if user is building f90 API.
if BUILD_F90
texi_files += netcdf-f90.texi 
html_mans += netcdf_f90-man.html
endif

# Add C++ manual if user is building C++ API.
if BUILD_CXX
texi_files += netcdf-cxx.texi
endif

# Get lists of the coresponding ps, info, and pdf files.
html_docs = ${texi_files:.texi=.html} 
html_dirs = ${texi_files:.texi=} 
ps_docs = ${texi_files:.texi=.ps} 
dvi_docs = ${texi_files:.texi=.dvi} 
pdf_docs = ${texi_files:.texi=.pdf}
info_docs= ${texi_files:.texi=.info}
txt_docs= ${texi_files:.texi=.txt}

# These files will be included with the dist.
EXTRA_DIST = netcdf.m4 $(pdf_docs) $(html_mans) $(txt_docs) $(dvi_docs)	\
$(ps_docs) $(info_docs) $(html_docs)

# If we have not used --enable-build-docs then none of the following
# will get into the make file. This should not be necessary, but is
# because of broken implementations of make on AIX and perhaps other
# platforms.

# Get texi2dvi to shut the hell up!
TEXI2DVI = texi2dvi -s

build_texi_files = $(texi_files)

# These are the texi sources for the manuals.
info_TEXINFOS = $(build_texi_files)

# These will be installed on the user machine. We'll only install one
# version of the html docs, the ones with one page per manual. The
# version with one page per node is just for the netCDF website, so we
# don't ship it or install it.
if INSTALL_DOCS
doc_DATA = $(pdf_docs) $(html_mans) $(txt_docs) $(ps_docs)	\
$(html_docs) $(info_docs)
endif

# These are lists of the source code which will be inserted into the
# netCDF Tutorial. Note that the tutorial always contains info about
# all 4 APIs, not just the ones that are installed.
c_examples = simple_xy_wr.c simple_xy_rd.c sfc_pres_temp_wr.c	\
sfc_pres_temp_rd.c pres_temp_4D_wr.c pres_temp_4D_rd.c
f77_examples = ${c_examples:.c=.f} 
f90_examples = ${c_examples:.c=.f90} 
cxx_examples = ${c_examples:.c=.cpp} 
cdl_examples = simple_xy.cdl sfc_pres_temp.cdl pres_temp_4D.cdl

# All the documents depend on defines.texi for some common
# definitions.
netcdf_TEXINFOS = defines.texi
netcdf_install_TEXINFOS = defines.texi
netcdf_c_TEXINFOS = defines.texi
netcdf_f77_TEXINFOS = defines.texi
netcdf_f90_TEXINFOS = defines.texi
netcdf_cxx_TEXINFOS = defines.texi
netcdf_tutorial_TEXINFOS = defines.texi ${c_examples} ${f77_examples}	\
${f90_examples} ${cxx_examples} ${cdl_examples}

# To build documentation you must have M4 in the path.
M4=m4

# Rule to generate the ASCII text copies of the documentation.
.texi.txt: 
	makeinfo -o $*.txt --no-headers $(srcdir)/$<

# Generate the HTML version of the netcdf-3 C man page.
netcdf-man-3.html: $(top_srcdir)/libsrc/netcdf.3
	groff -man -Thtml $? > $@

# Generate the HTML version of the netcdf-3 F77 man page.
netcdf_f77-man.html: $(top_srcdir)/fortran/netcdf_f77.3
	groff -man -Thtml $? > $@

# Generate the HTML version of the netcdf-3 F90 man page.
netcdf_f90-man.html: $(top_srcdir)/f90/netcdf_f90.3
	groff -man -Thtml $? > $@

# Generate the HTML version of the ncgen man page.
ncgen-man-1.html: $(top_srcdir)/ncgen/ncgen.1
	groff -man -Thtml $? > $@

# Generate the HTML version of the ncdump man page.
ncdump-man-1.html: $(top_srcdir)/ncdump/ncdump.1
	groff -man -Thtml $? > $@

# Slightly modified copies of all the example programs are created in
# this directory, so that they can be included in the tutorial. To
# work within texinfo, all } must be changed to @}, and { to
# @{. (Because otherwise teinfo will eat the curly brakets in the
# code.) This requires sed.

# Copy C examples to this directory.
${c_examples}:
	sed -e 's/{/@{/' -e 's/}/@}/' $(top_srcdir)/examples/C/$@ > $@

# Copy F77 examples to this directory.
${f77_examples}:
	sed -e 's/{/@{/' -e 's/}/@}/' $(top_srcdir)/examples/F77/$@ > $@

# Copy F90 examples to this directory.
${f90_examples}:
	sed -e 's/{/@{/' -e 's/}/@}/' $(top_srcdir)/examples/F90/$@ > $@

# Copy CXX examples to this directory.
${cxx_examples}:
	sed -e 's/{/@{/' -e 's/}/@}/' $(top_srcdir)/examples/CXX/$@ > $@

# Copy CDL examples to this directory.
${cdl_examples}:
	sed -e 's/{/@{/' -e 's/}/@}/' $(top_srcdir)/examples/CDL/$@ > $@

# Automake 1.9.6 changed the naming of html output directories. These
# makefile targets override automake and get the old naming scheme,
# which is really more useful for us. But if the automake team
# persists with these cahnges, then one day I'll change over too.
netcdf.html: netcdf.texi defines.texi
	$(MAKEINFO) --html $(AM_MAKEINFOHTMLFLAGS) $(MAKEINFOFLAGS) -I$(srcdir) netcdf.texi
	$(MAKEINFO) --no-split --html $(AM_MAKEINFOHTMLFLAGS) $(MAKEINFOFLAGS) -I$(srcdir) netcdf.texi

netcdf-install.html: netcdf-install.texi defines.texi
	$(MAKEINFO) --html $(AM_MAKEINFOHTMLFLAGS) $(MAKEINFOFLAGS) -I$(srcdir) netcdf-install.texi
	$(MAKEINFO) --no-split --html $(AM_MAKEINFOHTMLFLAGS) $(MAKEINFOFLAGS) -I$(srcdir) netcdf-install.texi

netcdf-c.html: netcdf-c.texi defines.texi
	$(MAKEINFO) --html $(AM_MAKEINFOHTMLFLAGS) $(MAKEINFOFLAGS) -I$(srcdir) netcdf-c.texi
	$(MAKEINFO) --no-split --html $(AM_MAKEINFOHTMLFLAGS) $(MAKEINFOFLAGS) -I$(srcdir) netcdf-c.texi

netcdf-f77.html: netcdf-f77.texi defines.texi
	$(MAKEINFO) --html $(AM_MAKEINFOHTMLFLAGS) $(MAKEINFOFLAGS) -I$(srcdir) netcdf-f77.texi
	$(MAKEINFO) --no-split --html $(AM_MAKEINFOHTMLFLAGS) $(MAKEINFOFLAGS) -I$(srcdir) netcdf-f77.texi

netcdf-cxx.html: netcdf-cxx.texi defines.texi
	$(MAKEINFO) --html $(AM_MAKEINFOHTMLFLAGS) $(MAKEINFOFLAGS) -I$(srcdir) netcdf-cxx.texi
	$(MAKEINFO) --no-split --html $(AM_MAKEINFOHTMLFLAGS) $(MAKEINFOFLAGS) -I$(srcdir) netcdf-cxx.texi

netcdf-f90.html: netcdf-f90.texi defines.texi
	$(MAKEINFO) --html $(AM_MAKEINFOHTMLFLAGS) $(MAKEINFOFLAGS) -I$(srcdir) netcdf-f90.texi
	$(MAKEINFO) --no-split --html $(AM_MAKEINFOHTMLFLAGS) $(MAKEINFOFLAGS) -I$(srcdir) netcdf-f90.texi

netcdf-tutorial.html: netcdf-tutorial.texi defines.texi
	$(MAKEINFO) --html $(AM_MAKEINFOHTMLFLAGS) $(MAKEINFOFLAGS) -I$(srcdir) netcdf-tutorial.texi
	$(MAKEINFO) --no-split --html $(AM_MAKEINFOHTMLFLAGS) $(MAKEINFOFLAGS) -I$(srcdir) netcdf-tutorial.texi

# This builds the docs from source, if necessary, and tars up
# everything needed for the website. Run this and copy the resulting
# tarball to the /contents/netcdf/docs directory to update the on-line
# docs.
web-tarball: $(pdf_docs) $(html_mans) $(txt_docs) $(dvi_docs) $(ps_docs) $(info_docs) $(html_docs)
	tar cf netcdf_info.tar *.info*
	gzip -f netcdf_info.tar
	zip netcdf_info *.info
	tar cf netcdf_pdf.tar *.pdf
	gzip -f netcdf_pdf.tar
	zip netcdf_pdf *.pdf
	tar cf netcdf_html.tar $(html_dirs)
	gzip -f netcdf_html.tar
	zip netcdf_html $(html_dirs)
	tar cf netcdf_txt.tar *.txt
	gzip -f netcdf_txt.tar
	zip netcdf_txt *.txt
	cp $(top_srcdir)/ncdump/ncdump.1 .
	cp $(top_srcdir)/ncgen/ncgen.1 .
	cp $(top_srcdir)/libsrc/netcdf.3 .
	cp $(top_srcdir)/fortran/netcdf_f77.3 .
	cp $(top_srcdir)/f90/netcdf_f90.3 .
	tar cf netcdf_man.tar netcdf.3 netcdf_f77.3 netcdf_f90.3 ncdump.1 ncgen.1
	gzip -f netcdf_man.tar
	zip netcdf_man netcdf.3 netcdf_f77.3 netcdf_f90.3 ncdump.1 ncgen.1
	tar cf netcdf_docs.tar $(html_dirs) *.pdf *.txt *.html *.tar.gz *.zip 
	gzip -f netcdf_docs.tar

# Clean up all the junk we created!
# MAINTAINERCLEANFILES = $(ps_docs) $(pdf_docs) $(txt_docs) $(info_docs)		\
# $(html_docs) $(man_MANS) ${c_examples} ${f77_examples} ${f90_examples}	\
# ${cxx_examples} ${cdl_examples} *.1 *.3 ncdump-man-1.html		\
# ncgen-man-1.html netcdf_f77-man.html netcdf-man-3.html			\
# netcdf_f90-man.html stamp-* version*


