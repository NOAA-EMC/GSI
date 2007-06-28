FC	=xlf90_r
INCS	=-I /nwprod/lib/incmod/sfcio_4
FOPTS	=-qnosave -qsmp=noauto -O2 -qarch=auto -qmaxmem=-1
LOPTS	=
LIBS	=-L/nwprod/lib -lw3_4 -lsp_4 -lsfcio_4 -lessl
sfc2gg:	sfc2gg.f
	xlf90_r $(INCS) $(FOPTS) $(LOPTS) sfc2gg.f $(LIBS) -o sfc2gg
