# make for wgrib

wgrib:	wgrib.c
	/usr/vacpp/bin/xlC_r -O3 -qstrict -q64 -o wgrib -DDEF_T62_NCEP_TABLE=opn -DFAST_GRIBTAB -DP_TABLE_FIRST wgrib.c

