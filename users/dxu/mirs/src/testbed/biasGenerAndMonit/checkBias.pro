@../../../setup/paths_idl.pro
;====================================================================
; Name:		CheckBias
;
;
; Type:		IDL Subroutine
;
;
; Description:  Check bias abnormal
;
;
; Arguments:
;
;      Name		    Type	    Description
;      ---------------------------------------------------
;	- biasFile   	      I             Bias file
;	- modelErrFile	      I		    Model Error File
;	- biasTxt   	      O             abnormal info. of bias
;
;
; Subroutines needed:
;       - ReadBias
;
;
; History:
;       2008      Wanchun Chen			PSGS
;
;====================================================================
PRO checkBias, satId=satId, biasFileDynamic=biasFileDynamic, biasFileStatic=biasFileStatic, modelErrFile=modelErrFile, nedtFile=nedtFile, tunFile=tunFile, biasTxt=biasTxt
    
    ;satId='n18'
    ;biasFile='/net/orbit227l/disk1/pub/mirs_operational/data/SemiStaticData/biasCorrec/biasCorrec_n18_2009_10_17.dat_ecmw'
    ;modelErrFile='/net/orbit227l/disk1/pub/mirs_operational/data/SemiStaticData/biasCorrec/ModelErrFile_n18_2008_10_17.dat_ecmw'
    ;nedtFile='/net/orbit227l/disk1/pub/mirs_operational/data/TestbedData/nedt/n18_amsua_mhs/n18_amsua_mhs_nedt_2009_10_17_befFM.dat'
    ;TunFile='/net/orbit227l/home/pub/mirs_operational/data/StaticData/TuningData/TunParams_n18_amsua_mhs.in'
    ;biasTxt='biasTxt'
    
    ;satId='f16'
    ;biasFile='/net/orbit138l/disk2/pub/kgarrett/mirs_operational/data/SemiStaticData/biasCorrec/biasCorrec_f16_2008_12_03.dat_gdas'
    ;modelErrFile='/net/orbit138l/disk2/pub/kgarrett/mirs_operational/data/SemiStaticData/biasCorrec/ModelErrFile_f16_2008_12_03.dat_gdas'
    ;nedtFile='/net/orbit138l/disk2/pub/kgarrett/mirs_operational/data/TestbedData/nedt/f16_ssmis/f16_ssmis_nedt_2008_12_03_aftFM.dat'
    ;TunFile='/net/orbit227l/home/pub/mirs_operational/data/StaticData/TuningData/TunParams_f16_ssmis.in'
    ;biasTxt='biasTxt'

    ReadBias,biasFileDynamic,nchan,npos,cfreq,meanBias,Slope,Intercept,meanTbsimu,meanTbmeas
    readTuningFile,TunFile,nchan,ChanSel

    ReadBias,biasFileStatic,nchan0,npos0,cfreq0,meanBias0,Slope0,Intercept0,meanTbsimu0,meanTbmeas0
    
    deltaBias = (meanBias - meanBias0)
    
    readErr,nedtFile,cfreq,nedt,nchan,ComputedOrNotFlag
    readErr,modelErrFile,cfreq,errs,nchan,ComputedOrNotFlag

    openw, lun, biasTxt, /get_lun
    titleWrite=0
    for ichan=0, NCHAN-1 do begin
    for ipos =0, NPOS-1  do begin
       ;---Skip F18 channel 8 after channel anomaly
       if (ichan eq 7 and satId eq 'f18') then CONTINUE
       if (abs(deltaBias(ipos,ichan)) gt 5*nedt(ichan) and chansel(ichan) eq 1) then begin
         if titleWrite eq 0 then begin
	    printf, lun, 'The message is triggered when ABS(Delta Bias) > 5 * ABS(nedt)'
	    printf, lun, 'Chan  Pos  Dynamic Bias   Static Bias  DeltaBias  Model Error  Nedt Value'
	    titleWrite=1 
	 endif
            printf, lun,  ichan, ipos, meanBias(ipos,ichan), meanBias0(ipos,ichan),deltaBias(ipos,ichan),errs(ichan),nedt(ichan), $
	    	format='(i5, 1x, i2, f12.2, f12.2, f12.2, f12.2, f12.2)' 
       endif 
    
    endfor
    endfor
    
    close, lun
    free_lun, lun

END
