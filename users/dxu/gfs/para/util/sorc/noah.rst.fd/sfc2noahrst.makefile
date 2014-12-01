FC	=xlf90_r
LIBDIR=../../../lib
INCS	=-I $(LIBDIR)/incmod/sfcio_4
#FOPTS	=-g -qnosave -qsmp=noauto -O0 -qarch=auto -qmaxmem=-1
FOPTS	=-qnosave -qsmp=noauto -O2 -qarch=auto -qmaxmem=-1
LOPTS	=
LIBS	=-L$(LIBDIR) -lw3_4 -lsp_4 -lsfcio_4 -lessl
sfc2noahrst:	sfc2noahrst.f
	xlf90_r $(INCS) $(FOPTS) $(LOPTS) sfc2noahrst.f $(LIBS) -o sfc2noahrst
