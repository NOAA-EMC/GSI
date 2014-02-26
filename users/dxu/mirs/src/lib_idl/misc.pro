$Id: misc.pro 3343 2013-10-09 17:26:34Z amims $
;---------------------------------------------------------------------------------------------------
; Summary of miscellaneous subroutines needed across the applications.
;
; S.A. Boukabara IMSG Inc. @ NOAA/NESDIS 2005-2006
;
;---------------------------------------------------------------------------------------------------


;===================================================================================================
; Name:		stratifPerfs
;
; Type:		IDL Subroutine
;  
; Description:
;    to stratify performances of any EDR by many parameters
;    (difference in time, distance, etc.)
;
; 
;
; Subroutines needed:
;	- plotStratifStat
;       - DiscreteStratif
;
; Record of revisions:
;        Date          Programmer      		Description of change
;    ============    ==============    =========================================
;     09/01/2007       Wanchun Chen     	Original code
;     
;===================================================================================================
PRO stratifPerfs,md_use,Ref,Retr,ind,nAlgos,label,SfcExt,Range1,Range2,Range3,thck,scalFactSiz,col,$
    psmAlt,linesty,LabelsRetr,difftime,diffdist,Ang,lat,lon,test1,stationID,uniqStatID,nStatIDs,$
    raobTyp,uniqRaobTyp,nRaobTyps,minlat,maxlat,minlon,maxlon,unit
   ;---vs diff time
   IF (MD_use eq 1) THEN BEGIN
       erase
       !p.multi=[3,1,3,0,0]
       what2show = 0 ;0->stratified bias, 1->stratified stdev, 2->nelemts
       plotStratifStat,difftime,Ref,Retr,ind,nAlgos,'Difference in Time (hours)','Difference in '+label+' (Ref-Algo)',$
         'Assessment of '+label+' Errors over:'+sfcExt,Range1,thck,what2show,5,scalFactSiz*0.8,col,-psmAlt,linesty,LabelsRetr
       what2show = 1 ;0->stratified bias, 1->stratified stdev, 2->nelemts
       plotStratifStat,difftime,Ref,Retr,ind,nAlgos,'Difference in Time (hours)','Stdev in '+label+' (Ref-Algo)',$
         'Assessment of '+label+' Errors over:'+sfcExt,Range2,thck,what2show,5,scalFactSiz*0.8,col,-psmAlt,linesty,LabelsRetr
       what2show = 2 ;0->stratified bias, 1->stratified stdev, 2->nelemts
       plotStratifStat,difftime,Ref,Retr,ind,nAlgos,'Difference in Time (hours)','Number of Points ',$
         'Assessment of '+label+' Errors over:'+sfcExt,Range3,thck,what2show,5,scalFactSiz*0.8,col,-psmAlt,linesty,LabelsRetr
   ENDIF
   ;---vs diff distance
   IF (MD_use eq 1) THEN BEGIN
       erase
       !p.multi=[3,1,3,0,0]
       what2show = 0 ;0->stratified bias, 1->stratified stdev, 2->nelemts
       plotStratifStat,diffdist,Ref,Retr,ind,nAlgos,'Difference in distance (kms)','Difference in '+label+' (Ref-Algo)',$
         'Assessment of '+label+' Errors over:'+sfcExt,Range1,thck,what2show,5,scalFactSiz*0.8,col,-psmAlt,linesty,LabelsRetr
       what2show = 1 ;0->stratified bias, 1->stratified stdev, 2->nelemts
       plotStratifStat,diffdist,Ref,Retr,ind,nAlgos,'Difference in distance (kms)','Stdev in '+label+' (Ref-Algo)',$
         'Assessment of '+label+' Errors over:'+sfcExt,Range2,thck,what2show,5,scalFactSiz*0.8,col,-psmAlt,linesty,LabelsRetr
       what2show = 2 ;0->stratified bias, 1->stratified stdev, 2->nelemts
       plotStratifStat,diffdist,Ref,Retr,ind,nAlgos,'Difference in distance (kms)','Number of Points ',$
         'Assessment of '+label+' Errors over:'+sfcExt,Range3,thck,what2show,5,scalFactSiz*0.8,col,-psmAlt,linesty,LabelsRetr
   ENDIF
   ;---vs angle 
   erase
   !p.multi=[3,1,3,0,0]
   what2show = 0  ;0->stratified bias, 1->stratified stdev, 2->nelemts
   plotStratifStat,Ang,Ref,Retr,ind,nAlgos,'Satellite Zen Angle (deg)','Difference in '+label+' (Ref-Algo)',$
     'Assessment of '+label+' Errors over:'+sfcExt,Range1,thck,what2show,30,scalFactSiz*0.8,col,-psmAlt,linesty,LabelsRetr
   what2show = 1  ;0->stratified bias, 1->stratified stdev, 2->nelemts
   plotStratifStat,Ang,Ref,Retr,ind,nAlgos,'Satellite Zen Angle (deg)','Stdev in '+label+' (Ref-Algo)',$
     'Assessment of '+label+' Errors over:'+sfcExt,Range2,thck,what2show,30,scalFactSiz*0.8,col,-psmAlt,linesty,LabelsRetr
   what2show = 2  ;0->stratified bias, 1->stratified stdev, 2->nelemts
   plotStratifStat,Ang,Ref,Retr,ind,nAlgos,'Satellite Zen Angle (deg)','Number of Points ',$
     'Assessment of '+label+' Errors over:'+sfcExt,Range3,thck,what2show,30,scalFactSiz*0.8,col,-psmAlt,linesty,LabelsRetr
   ;---vs latitude
   erase
   !p.multi=[3,1,3,0,0]
   what2show = 0  ;0->stratified bias, 1->stratified stdev, 2->nelemts
   plotStratifStat,lat,Ref,Retr,ind,nAlgos,'Raob Latitude','Difference in '+label+' (Ref-Algo)',$
     'Assessment of '+label+' Errors over:'+sfcExt,Range1,thck,what2show,10,scalFactSiz*0.8,col,-psmAlt,linesty,LabelsRetr
   what2show = 1  ;0->stratified bias, 1->stratified stdev, 2->nelemts
   plotStratifStat,lat,Ref,Retr,ind,nAlgos,'Raob Latitude','Stdev in '+label+' (Ref-Algo)',$
     'Assessment of '+label+' Errors over:'+sfcExt,Range2,thck,what2show,10,scalFactSiz*0.8,col,-psmAlt,linesty,LabelsRetr
   what2show = 2  ;0->stratified bias, 1->stratified stdev, 2->nelemts
   plotStratifStat,lat,Ref,Retr,ind,nAlgos,'Raob Latitude','Number of Points ',$
     'Assessment of '+label+' Errors over:'+sfcExt,Range3,thck,what2show,10,scalFactSiz*0.8,col,-psmAlt,linesty,LabelsRetr
   ;-----Evaluate as a functin of station ID
   IF (MD_use eq 1) THEN BEGIN
       DiscreteStratif,test1,stationID,uniqStatID,nStatIDs,Ref-Retr(1,*),lat,lon,stats_byStat,lat_byStation,lon_byStation
       erase
       !p.multi=1
       tit=label+' Diff (Ref-Retr) -Mean Bias- Over:'+sfcExt
       ind0=where(reform(stats_byStat(*,0)) gt -999)
       mapPlot,minlat,maxlat,minlon,maxlon,lat_byStation,lon_byStation,ind0,tit,Range1[0],Range1[1],reform(stats_byStat(*,0)),unit,1.2,8,0.5,0,'(f7.2)'
       erase
       !p.multi=1
       tit=label+' Diff (Ref-Retr) -Std Deviation- Over:'+sfcExt
       mapPlot,minlat,maxlat,minlon,maxlon,lat_byStation,lon_byStation,ind0,tit,Range2[0],Range2[1],reform(stats_byStat(*,1)),unit,1.2,8,0.5,0,'(f7.2)'
       erase
       !p.multi=[2,1,2,0,0]
       plot,uniqStatID(ind0),stats_byStat(ind0,0),psym=-8,xtitle='Station ID',ytitle='Mean '+label+' Diff (Ref-Retr)',title='Over:'+sfcExt
       ind0=where(reform(stats_byStat(*,1)) gt -999)
       plot,uniqStatID(ind0),stats_byStat(ind0,1),psym=-8,xtitle='Station ID',ytitle='Stdev '+label+' Diff (Ref-Retr)',title='Over:'+sfcExt
   ENDIF
   ;-----Evaluate as a functin of raob instrument type
   IF (MD_use eq 1) THEN BEGIN
       DiscreteStratif,test1,raobTyp,uniqRaobTyp,nRaobTyps,Ref-Retr(1,*),lat,lon,stats_byRaobTyp,lat_byRaobTyp,lon_byRaobTyp
       erase
       !p.multi=[2,1,2,0,0]
       ind0=where(reform(stats_byRaobTyp(*,0)) gt -999)
       plot,uniqStatID(ind0),stats_byRaobTyp(ind0,0),psym=-8,xtitle='Raob Instrument Type',ytitle='Mean '+label+' Diff (Ref-Retr)',$
         title='Over:'+sfcExt
       ind0=where(reform(stats_byRaobTyp(*,1)) gt -999)
       plot,uniqStatID(ind0),stats_byRaobTyp(ind0,1),psym=-8,xtitle='Raob Instrument Type',ytitle='Stdev '+label+' Diff (Ref-Retr)',$
         title='Over:'+sfcExt
   ENDIF
END


;===================================================================================================
; Name:		GetLayersIndex
;
; Type:		IDL Subroutine
;  
; Description:
;    To get layer indexes from NLAY=100 layers
;
; 
; Arguments:    
;     
;	Name	    	Type	    	Description
;     ---------------------------------------------------
;	satId   	i 		satellite ID -- 'n18', 'metopA', 'f16'   
;	yyyymmdd	i		yyyymmdd
;       layers		i		array of layers
;     	layers_index	o		array of layers indexes
;
;
; Subroutines needed:
;	- None
;
; Record of revisions:
;        Date          Programmer      		Description of change
;    ============    ==============    =========================================
;     09/01/2007       Wanchun Chen     	Original code
;     
;===================================================================================================
Pro getLayersIndex, gridDir, satId, yyyymmdd, layers, layers_index 

  ;***************************************************************
  ;   	read in pressure
  ;***************************************************************
  NLAY=100
  press=fltarr(NLAY)
  tmp=0.0
  openr, iu, gridDir + 'GRID_' + satId+'_'+yyyymmdd+'_press.txt', /get_lun

  for i=0, NLAY-1 do begin
    readf, iu, format='(f)', tmp
    press(i) = tmp
  endfor

  close,iu
  free_lun,iu,/force

  ;***************************************************************
  ;	pick out layers index
  ;***************************************************************
  n_layer=N_Elements(layers)
  layers_index = make_array(n_layer, value=0)
  diff = make_array(n_layer, value=1000)
  
  for jlay=0, n_layer-1 do begin
  for ilay=0, NLAY-1    do begin
      
      if ABS(press(ilay) - layers(jlay)) lt diff(jlay) then begin
      	diff(jlay) = ABS(press(ilay) - layers(jlay))
	layers_index(jlay) = ilay
      endif
    
  endfor
  endfor
  

End



;===================================================================================================
; Name:		Loadct_tpw
;
; Type:		IDL Subroutine
;  
; Description:
;    To load MSPPS TWP color table, thus no need to carry saved color tables
;
; 
; Arguments:    
;	
;	Name	    	Type	    	Description
;     ---------------------------------------------------
;	r   		o 		red elements
;	g   		o 		green elements
;	b   		o 		blue elements

;
; Subroutines needed:
;	- None
;
;
; Record of revisions:
;        Date          Programmer      		Description of change
;    ============    ==============    =========================================
;     02/17/2009       Wanchun Chen     	Original code
;     07/19/2012       Wanchun Chen     	Adjusted colors
;     
;===================================================================================================

Pro loadct_tpw, r, g, b

r = [ $
 243B, 243B, 243B, 242B, 241B, 240B, 240B, 232B, 221B, 210B, 198B, 187B, 176B, 165B, 153B, 142B,$
 131B, 131B, 120B, 108B,  97B,  86B,  75B,  63B,  52B,  41B,  30B,  18B,   7B,   0B,   0B,   0B,$
   0B,   0B,   0B,   0B,   0B,   0B,   0B,   0B,   0B,   0B,   0B,   0B,   0B,   0B,   0B,   0B,$
   0B,   0B,   0B,   0B,   0B,   0B,   0B,   0B,   0B,   0B,   0B,   0B,   0B,   0B,   0B,   0B,$
   0B,   0B,   0B,   0B,   0B,   0B,   0B,   0B,   0B,   0B,   0B,   7B,  19B,  31B,  43B,  55B,$
  66B,  66B,  78B,  90B, 102B, 114B, 126B, 137B, 149B, 161B, 173B, 185B, 196B, 208B, 220B, 232B,$
 244B, 244B, 251B, 251B, 251B, 251B, 250B, 250B, 250B, 250B, 250B, 249B, 249B, 249B, 249B, 248B,$
 248B, 248B, 248B, 248B, 247B, 247B, 247B, 247B, 247B, 241B, 235B, 230B, 224B, 219B, 213B, 207B,$
 202B, 202B, 196B, 191B, 185B, 180B, 174B, 168B, 163B, 157B, 152B, 146B, 141B, 135B, 129B, 131B,$
 137B, 137B, 143B, 149B, 155B, 161B, 167B, 172B, 178B, 184B, 190B, 196B, 202B, 208B, 213B, 219B,$
 225B, 225B, 231B, 237B, 243B, 249B, 252B, 251B, 250B, 249B, 248B, 247B, 246B, 245B, 244B, 243B,$
 242B, 242B, 241B, 240B, 239B, 238B, 237B, 236B, 235B, 234B, 233B, 232B, 231B, 231B, 231B, 232B,$
 232B, 232B, 233B, 233B, 234B, 234B, 235B, 235B, 236B, 236B, 237B, 237B, 238B, 238B, 238B, 239B,$
 239B, 239B, 240B, 240B, 241B, 241B, 242B, 242B, 242B, 243B, 243B, 244B, 244B, 245B, 245B, 245B,$
 246B, 246B, 246B, 247B, 247B, 248B, 248B, 248B, 249B, 249B, 250B, 250B, 250B, 250B, 250B, 250B,$
 250B, 250B, 250B, 250B, 250B, 250B, 250B, 250B, 250B, 250B, 250B, 250B, 250B, 250B, 250B, 250B ]

g = [ $
 228B, 228B, 227B, 225B, 223B, 222B, 220B, 217B, 214B, 211B, 208B, 205B, 202B, 199B, 196B, 193B,$
 190B, 190B, 187B, 184B, 181B, 178B, 175B, 172B, 169B, 166B, 163B, 160B, 157B, 155B, 155B, 155B,$
 155B, 155B, 156B, 156B, 156B, 156B, 156B, 157B, 157B, 157B, 157B, 158B, 158B, 158B, 158B, 159B,$
 159B, 159B, 159B, 159B, 160B, 164B, 168B, 172B, 177B, 181B, 185B, 190B, 194B, 198B, 203B, 207B,$
 211B, 211B, 216B, 220B, 224B, 229B, 233B, 237B, 241B, 246B, 250B, 251B, 251B, 250B, 249B, 249B,$
 248B, 248B, 248B, 247B, 247B, 246B, 246B, 245B, 244B, 244B, 243B, 243B, 242B, 242B, 241B, 240B,$
 240B, 240B, 240B, 240B, 240B, 240B, 240B, 240B, 240B, 240B, 240B, 240B, 240B, 240B, 240B, 240B,$
 240B, 240B, 240B, 240B, 240B, 240B, 240B, 240B, 240B, 234B, 228B, 223B, 217B, 212B, 206B, 201B,$
 195B, 195B, 190B, 184B, 179B, 173B, 168B, 162B, 157B, 151B, 145B, 140B, 134B, 129B, 123B, 122B,$
 123B, 123B, 124B, 125B, 126B, 127B, 128B, 129B, 130B, 131B, 132B, 132B, 133B, 134B, 135B, 136B,$
 137B, 137B, 138B, 139B, 140B, 141B, 140B, 137B, 133B, 129B, 126B, 122B, 118B, 115B, 111B, 107B,$
 104B, 104B, 100B,  96B,  93B,  89B,  85B,  82B,  78B,  74B,  71B,  67B,  64B,  61B,  58B,  56B,$
  53B,  53B,  51B,  48B,  46B,  43B,  41B,  38B,  36B,  33B,  31B,  28B,  26B,  23B,  20B,  18B,$
  15B,  15B,  13B,  10B,  10B,  10B,  10B,  10B,  11B,  11B,  11B,  11B,  12B,  12B,  12B,  12B,$
  12B,  12B,  13B,  13B,  13B,  13B,  14B,  14B,  14B,  14B,  15B,  15B,  15B,  15B,  15B,  15B,$
  15B,  15B,  15B,  15B,  15B,  15B,  15B,  15B,  15B,  15B,  15B,  15B,  15B,  15B,  15B,  15B ]

b = [ $
 221B, 221B, 219B, 217B, 214B, 212B, 210B, 203B, 193B, 183B, 173B, 164B, 154B, 144B, 134B, 124B,$
 114B, 114B, 105B,  95B,  85B,  75B,  65B,  55B,  45B,  36B,  26B,  16B,   6B,   0B,   0B,   0B,$
   0B,   0B,   0B,   0B,   0B,   0B,   0B,   0B,   0B,   0B,   0B,   0B,   0B,   0B,   0B,   0B,$
   0B,   0B,   0B,   0B,   0B,   0B,   0B,   0B,   0B,   0B,   0B,   0B,   0B,   0B,   0B,   0B,$
   0B,   0B,   0B,   0B,   0B,   0B,   0B,   0B,   0B,   0B,   0B,   0B,   0B,   0B,   0B,   0B,$
   0B,   0B,   0B,   1B,   1B,   1B,   1B,   1B,   1B,   1B,   2B,   2B,   2B,   2B,   2B,   2B,$
   2B,   2B,   2B,   2B,   2B,   2B,   2B,   2B,   2B,   1B,   1B,   1B,   1B,   1B,   1B,   1B,$
   0B,   0B,   0B,   0B,   0B,   0B,   0B,   0B,   0B,   0B,   0B,   0B,   0B,   0B,   1B,   1B,$
   1B,   1B,   1B,   1B,   2B,   2B,   2B,   2B,   2B,   3B,   3B,   3B,   3B,   3B,   3B,   3B,$
   3B,   3B,   3B,   3B,   3B,   2B,   2B,   2B,   2B,   2B,   2B,   1B,   1B,   1B,   1B,   1B,$
   0B,   0B,   0B,   0B,   0B,   0B,   0B,   0B,   0B,   0B,   0B,   0B,   0B,   0B,   0B,   0B,$
   0B,   0B,   1B,   1B,   1B,   1B,   1B,   1B,   1B,   1B,   1B,   1B,   2B,   2B,   2B,   2B,$
   2B,   2B,   2B,   2B,   2B,   2B,   2B,   2B,   3B,   3B,   3B,   3B,   3B,   3B,   3B,   3B,$
   3B,   3B,   3B,   3B,   3B,   3B,   3B,   3B,   3B,   2B,   2B,   2B,   2B,   2B,   2B,   1B,$
   1B,   1B,   1B,   1B,   1B,   0B,   0B,   0B,   0B,   0B,   0B,   0B,   0B,   0B,   0B,   0B,$
   0B,   0B,   0B,   0B,   0B,   0B,   0B,   0B,   0B,   0B,   0B,   0B,   0B,   0B,   0B,   0B ]

End



;===================================================================================================
; Name:		Loadct_sice
;
; Type:		IDL Subroutine
;  
; Description:
;    To load MSPPS Sea Ice color table, thus no need to carry saved color tables
;
; 
; Arguments:    
;	
;	Name	    	Type	    	Description
;     ---------------------------------------------------
;	r   		o 		red elements
;	g   		o 		green elements
;	b   		o 		blue elements

;
; Subroutines needed:
;	- None
;
;
; Record of revisions:
;        Date          Programmer      		Description of change
;    ============    ==============    =========================================
;     02/17/2009       Wanchun Chen     	Original code
;     
;===================================================================================================
Pro loadct_sice, r, g, b

r = [ $
255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,$
255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,207B,207B,207B,207B,207B,207B,$
207B,207B,207B,207B,207B,207B,207B,207B,207B,207B,207B,207B,207B,207B,207B,207B,$
207B,207B,207B,207B,207B,207B,207B,207B,207B,207B,207B,207B,207B,207B,207B,207B,$
207B,207B,207B,207B,207B,207B,207B,207B,207B,207B,207B,207B,207B,207B,207B,207B,$
207B,207B,207B,207B,207B,207B,207B,207B,207B,207B,207B,207B,207B,207B,207B,207B,$
203B,200B,197B,193B,190B,187B,184B,180B,177B,174B,170B,167B,164B,161B,157B,154B,$
151B,147B,144B,141B,138B,134B,131B,128B,124B,121B,118B,115B,111B,108B,105B,101B,$
98B,95B,91B,88B,85B,82B,78B,75B,72B,69B,65B,62B,59B,55B,52B,49B,$
46B,42B,39B,36B,32B,29B,26B,23B,19B,16B,13B,9B,6B,3B,0B,0B,$
0B,1B,1B,2B,2B,3B,3B,4B,4B,5B,5B,6B,6B,7B,7B,8B,$
8B,9B,9B,10B,11B,11B,12B,12B,13B,13B,14B,14B,15B,15B,16B,16B,$
17B,17B,18B,18B,19B,19B,20B,20B,21B,22B,22B,23B,23B,24B,24B,25B,$
25B,26B,26B,27B,27B,28B,28B,29B,29B,30B,30B,31B,31B,32B,33B,33B,$
31B,30B,29B,28B,27B,26B,25B,24B,23B,22B,21B,20B,19B,18B,17B,15B,$
14B,13B,12B,11B,10B,9B,8B,7B,6B,5B,4B,3B,2B,1B,0B,255B ]

g = [ $
255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,$
255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,236B,236B,236B,236B,236B,236B,$
236B,236B,236B,236B,236B,236B,236B,236B,236B,236B,236B,236B,236B,236B,236B,236B,$
236B,236B,236B,236B,236B,236B,236B,236B,236B,236B,236B,236B,236B,236B,236B,236B,$
236B,236B,236B,236B,236B,236B,236B,236B,236B,236B,236B,236B,236B,236B,236B,236B,$
236B,236B,236B,236B,236B,236B,236B,236B,236B,236B,236B,236B,236B,236B,236B,236B,$
236B,236B,236B,236B,237B,237B,237B,237B,238B,238B,238B,238B,238B,239B,239B,239B,$
239B,240B,240B,240B,240B,240B,241B,241B,241B,241B,242B,242B,242B,242B,242B,243B,$
243B,243B,243B,244B,244B,244B,244B,244B,245B,245B,245B,245B,246B,246B,246B,246B,$
246B,247B,247B,247B,247B,248B,248B,248B,248B,248B,249B,249B,249B,249B,250B,250B,$
246B,242B,238B,234B,230B,226B,222B,218B,214B,210B,206B,202B,198B,194B,190B,186B,$
182B,178B,174B,170B,166B,162B,158B,154B,150B,146B,142B,138B,134B,130B,126B,123B,$
119B,115B,111B,107B,103B,99B,95B,91B,87B,83B,79B,75B,71B,67B,63B,59B,$
55B,51B,47B,43B,39B,35B,31B,27B,23B,19B,15B,11B,7B,3B,0B,0B,$
0B,0B,1B,1B,1B,2B,2B,3B,3B,3B,4B,4B,5B,5B,5B,6B,$
6B,6B,7B,7B,8B,8B,8B,9B,9B,10B,10B,10B,11B,11B,12B,255B ]

b = [ $
255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,$
255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,$
255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,$
255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,$
255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,$
255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,$
254B,254B,254B,254B,254B,254B,254B,254B,254B,254B,254B,254B,253B,253B,253B,253B,$
253B,253B,253B,253B,253B,253B,253B,253B,253B,252B,252B,252B,252B,252B,252B,252B,$
252B,252B,252B,252B,252B,251B,251B,251B,251B,251B,251B,251B,251B,251B,251B,251B,$
251B,251B,250B,250B,250B,250B,250B,250B,250B,250B,250B,250B,250B,250B,250B,250B,$
250B,250B,250B,250B,250B,250B,250B,250B,250B,250B,250B,250B,250B,250B,250B,250B,$
250B,250B,250B,250B,250B,250B,250B,250B,250B,250B,250B,250B,250B,250B,250B,250B,$
250B,250B,250B,250B,250B,250B,250B,250B,250B,250B,250B,250B,250B,250B,250B,250B,$
250B,250B,250B,250B,250B,250B,250B,250B,250B,250B,250B,250B,250B,250B,250B,250B,$
246B,242B,239B,235B,232B,228B,224B,221B,217B,214B,210B,207B,203B,199B,196B,192B,$
189B,185B,181B,178B,174B,171B,167B,164B,160B,156B,153B,149B,146B,142B,139B,255B ]

end


;===================================================================================================
; Name:		Loadct_swe
;
; Type:		IDL Subroutine
;  
; Description:
;    To load MSPPS SWE color table, thus no need to carry saved color tables
;
; 
; Arguments:    
;	
;	Name	    	Type	    	Description
;     ---------------------------------------------------
;	r   		o 		red elements
;	g   		o 		green elements
;	b   		o 		blue elements

;
; Subroutines needed:
;	- None
;
;
; Record of revisions:
;        Date          Programmer      		Description of change
;    ============    ==============    =========================================
;     02/17/2009       Wanchun Chen     	Original code
;     
;===================================================================================================
Pro loadct_swe, r, g, b

r = [ $
255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,$
255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,0B,0B,0B,$
0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,$
0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,$
0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,$
0B,0B,0B,0B,0B,136B,136B,136B,136B,136B,136B,136B,136B,136B,136B,136B,$
136B,136B,136B,136B,136B,136B,136B,136B,136B,136B,136B,136B,136B,136B,136B,136B,$
136B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,$
0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,255B,255B,255B,$
255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,$
255B,255B,255B,255B,255B,255B,255B,255B,255B,137B,137B,137B,137B,137B,137B,137B,$
137B,137B,137B,137B,137B,137B,137B,137B,137B,137B,137B,137B,137B,137B,137B,137B,$
137B,137B,137B,137B,137B,160B,160B,160B,160B,160B,160B,160B,160B,160B,160B,160B,$
160B,160B,160B,160B,160B,160B,160B,160B,160B,160B,160B,160B,160B,160B,160B,160B,$
160B,160B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,$
255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B ]


g = [ $
255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,$
255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,228B,228B,228B,255B,255B,255B,$
255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,$
255B,255B,255B,255B,255B,255B,255B,0B,0B,0B,0B,0B,0B,0B,0B,0B,$
0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,$
0B,0B,0B,0B,0B,202B,202B,202B,202B,202B,202B,202B,202B,202B,202B,202B,$
202B,202B,202B,202B,202B,202B,202B,202B,202B,202B,202B,202B,202B,202B,202B,202B,$
202B,200B,200B,200B,200B,200B,200B,200B,200B,200B,200B,200B,200B,200B,200B,200B,$
200B,200B,200B,200B,200B,200B,200B,200B,200B,200B,200B,200B,200B,255B,255B,255B,$
255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,$
255B,255B,255B,255B,255B,255B,255B,255B,255B,150B,150B,150B,150B,150B,150B,150B,$
150B,150B,150B,150B,150B,150B,150B,150B,150B,150B,150B,150B,150B,150B,150B,150B,$
150B,150B,150B,150B,150B,90B,90B,90B,90B,90B,90B,90B,90B,90B,90B,90B,$
90B,90B,90B,90B,90B,90B,90B,90B,90B,90B,90B,90B,90B,90B,90B,90B,$
90B,90B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,$
0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,255B ]


b = [ $
255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,$
255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,200B,200B,200B,255B,255B,255B,$
255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,$
255B,255B,255B,255B,255B,255B,255B,240B,240B,240B,240B,240B,240B,240B,240B,240B,$
240B,240B,240B,240B,240B,240B,240B,240B,240B,240B,240B,240B,240B,240B,240B,240B,$
240B,240B,240B,240B,240B,240B,240B,240B,240B,240B,240B,240B,240B,240B,240B,240B,$
240B,240B,240B,240B,240B,240B,240B,240B,240B,240B,240B,240B,240B,240B,240B,240B,$
240B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,$
0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,$
0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,$
0B,0B,0B,0B,0B,0B,0B,0B,0B,26B,26B,26B,26B,26B,26B,26B,$
26B,26B,26B,26B,26B,26B,26B,26B,26B,26B,26B,26B,26B,26B,26B,26B,$
26B,26B,26B,26B,26B,8B,8B,8B,8B,8B,8B,8B,8B,8B,8B,8B,$
8B,8B,8B,8B,8B,8B,8B,8B,8B,8B,8B,8B,8B,8B,8B,8B,$
8B,8B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,$
0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,255B ]
	
end


;===================================================================================================
; Name:		Loadct_climate
;
; Type:		IDL Subroutine
;  
; Description:
;    To load MSPPS Climate products color table, thus no need to carry saved color tables
;
; 
; Arguments:    
;	
;	Name	    	Type	    	Description
;     ---------------------------------------------------
;	r   		o 		red elements
;	g   		o 		green elements
;	b   		o 		blue elements

;
; Subroutines needed:
;	- None
;
;
; Record of revisions:
;        Date          Programmer      		Description of change
;    ============    ==============    =========================================
;     02/17/2009       Wanchun Chen     	Original code
;     
;===================================================================================================
Pro loadct_climate, r, g, b

r = [ $
255B,255B,249B,244B,239B,234B,229B,224B,219B,214B,209B,204B,198B,193B,188B,183B,$
178B,173B,168B,163B,158B,153B,147B,142B,137B,132B,127B,122B,117B,112B,107B,102B,$
96B,91B,86B,81B,76B,71B,66B,61B,56B,51B,45B,40B,35B,30B,25B,20B,$
15B,10B,5B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,$
0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,$
0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,$
0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,1B,1B,1B,1B,$
2B,2B,2B,2B,3B,3B,3B,3B,4B,4B,4B,4B,5B,5B,5B,5B,$
6B,6B,6B,6B,6B,7B,7B,7B,7B,8B,8B,8B,8B,9B,9B,9B,$
9B,10B,10B,10B,10B,11B,11B,11B,11B,12B,12B,17B,23B,28B,34B,39B,$
45B,50B,56B,61B,67B,72B,78B,83B,89B,94B,100B,105B,111B,116B,122B,127B,$
133B,139B,144B,150B,155B,161B,166B,172B,177B,183B,188B,194B,199B,205B,210B,216B,$
221B,227B,232B,238B,243B,249B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,$
255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,$
255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,$
255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B ]

g = [ $
255B,228B,223B,218B,214B,209B,205B,200B,196B,191B,186B,182B,177B,173B,168B,164B,$
159B,155B,150B,145B,141B,136B,132B,127B,123B,118B,114B,109B,104B,100B,95B,91B,$
86B,82B,77B,72B,68B,63B,59B,54B,50B,45B,41B,36B,31B,27B,22B,18B,$
13B,9B,4B,0B,0B,4B,9B,14B,19B,23B,28B,33B,38B,43B,47B,52B,$
57B,62B,66B,71B,76B,81B,86B,90B,95B,100B,105B,109B,114B,119B,124B,129B,$
133B,138B,143B,148B,152B,157B,162B,167B,172B,176B,181B,186B,191B,195B,200B,205B,$
210B,215B,219B,224B,229B,234B,239B,239B,239B,239B,239B,239B,239B,239B,239B,240B,$
240B,240B,240B,240B,240B,240B,241B,241B,241B,241B,241B,241B,241B,242B,242B,242B,$
242B,242B,242B,242B,243B,243B,243B,243B,243B,243B,243B,244B,244B,244B,244B,244B,$
244B,244B,245B,245B,245B,245B,245B,245B,245B,246B,246B,246B,246B,246B,246B,247B,$
247B,247B,247B,247B,248B,248B,248B,248B,248B,249B,249B,249B,249B,249B,250B,250B,$
250B,250B,250B,251B,251B,251B,251B,251B,252B,252B,252B,252B,252B,253B,253B,253B,$
253B,253B,254B,254B,254B,254B,255B,255B,250B,245B,241B,236B,232B,227B,223B,218B,$
214B,209B,204B,200B,195B,191B,186B,182B,177B,173B,168B,163B,159B,154B,150B,145B,$
141B,136B,132B,127B,122B,118B,113B,109B,104B,100B,95B,91B,86B,81B,77B,72B,$
68B,63B,59B,54B,50B,45B,40B,36B,31B,27B,22B,18B,13B,9B,4B,0B ]

b = [ $
255B,200B,201B,202B,203B,204B,205B,206B,207B,208B,209B,211B,212B,213B,214B,215B,$
216B,217B,218B,219B,220B,222B,223B,224B,225B,226B,227B,228B,229B,230B,231B,233B,$
234B,235B,236B,237B,238B,239B,240B,241B,242B,244B,245B,246B,247B,248B,249B,250B,$
251B,252B,253B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,$
255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,$
255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,$
255B,255B,255B,255B,255B,255B,255B,255B,249B,244B,239B,234B,229B,224B,219B,214B,$
209B,204B,198B,193B,188B,183B,178B,173B,168B,163B,158B,153B,147B,142B,137B,132B,$
127B,122B,117B,112B,107B,102B,96B,91B,86B,81B,76B,71B,66B,61B,56B,51B,$
45B,40B,35B,30B,25B,20B,15B,10B,5B,0B,0B,0B,0B,0B,0B,0B,$
0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,$
0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,$
0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,$
0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,$
0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,$
0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B ]

End

;===================================================================================================
; Name:		Loadct_bias
;
; Type:		IDL Subroutine
;  
; Description:
;    To load bias color table. In MIRS, its index is 45 ( color_table_index = 45 )
; 
; Arguments:    
;	
;	Name	    	Type	    	Description
;     ---------------------------------------------------
;	r   		o 		red elements
;	g   		o 		green elements
;	b   		o 		blue elements

;
; Subroutines needed:
;	- None
;
;
; Record of revisions:
;        Date          Programmer      		Description of change
;    ============    ==============    =========================================
;     11/19/2009       Wanchun Chen     	Original code
;     
;===================================================================================================
Pro loadct_bias, r, g, b

r = [ $ 
 0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,$
 0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,$
 0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,$
 5B,10B,16B,21B,27B,32B,37B,43B,48B,54B,59B,65B,70B,75B,81B,86B,92B,97B,103B,108B,113B,119B,124B,130B,135B,141B,146B,$
 151B,157B,162B,168B,173B,179B,184B,189B,195B,200B,206B,211B,217B,222B,227B,233B,238B,244B,249B,255B,255B,255B,255B,255B,255B,255B,255B,$
 255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,$
 255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,$
 255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,$
 255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,250B,246B,241B,237B,233B,228B,224B,219B,215B,211B,206B,202B,197B,193B,189B,184B,$
 180B,175B,171B,167B,162B,158B,153B,149B,145B,140B,136B,131B,131B]

g = [ $
 0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,$
 0B,0B,0B,0B,0B,3B,7B,11B,15B,19B,23B,27B,31B,35B,39B,43B,47B,51B,55B,59B,63B,67B,71B,75B,79B,83B,87B,91B,95B,$
 99B,103B,107B,111B,115B,119B,123B,127B,131B,135B,139B,143B,147B,151B,155B,159B,163B,167B,171B,175B,179B,183B,187B,188B,189B,190B,191B,193B,194B,$
 195B,196B,198B,199B,200B,201B,203B,204B,205B,206B,207B,209B,210B,211B,212B,214B,215B,216B,217B,219B,220B,221B,222B,224B,225B,226B,227B,228B,230B,$
 231B,232B,233B,235B,236B,237B,238B,240B,241B,242B,243B,245B,244B,243B,242B,241B,240B,239B,238B,238B,237B,236B,235B,234B,233B,232B,231B,231B,230B,$
 229B,228B,227B,226B,225B,224B,224B,223B,222B,221B,220B,219B,218B,217B,217B,216B,215B,214B,213B,212B,211B,210B,210B,209B,208B,207B,206B,205B,204B,$
 203B,203B,199B,195B,191B,187B,183B,179B,175B,171B,167B,163B,159B,155B,151B,147B,143B,139B,135B,131B,127B,123B,119B,115B,111B,107B,103B,99B,95B,$
 91B,87B,83B,79B,75B,71B,67B,63B,59B,55B,51B,47B,43B,39B,35B,31B,27B,23B,19B,15B,11B,7B,3B,0B,0B,0B,0B,0B,0B,$
 0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B]

b = [ $
 131B,131B,135B,139B,143B,147B,151B,155B,159B,163B,167B,171B,175B,179B,183B,187B,191B,195B,199B,203B,207B,211B,215B,219B,223B,227B,231B,235B,239B,$
 243B,247B,251B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,$
 255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,255B,254B,254B,253B,253B,253B,252B,$
 252B,252B,251B,251B,251B,250B,250B,249B,249B,249B,248B,248B,248B,247B,247B,247B,246B,246B,245B,245B,245B,244B,244B,244B,243B,243B,243B,242B,242B,$
 241B,241B,241B,240B,240B,240B,239B,239B,239B,238B,238B,238B,233B,228B,223B,218B,213B,208B,203B,198B,193B,188B,183B,178B,173B,168B,163B,158B,153B,$
 148B,143B,138B,133B,128B,123B,119B,114B,109B,104B,99B,94B,89B,84B,79B,74B,69B,64B,59B,54B,49B,44B,39B,34B,29B,24B,19B,14B,9B,$
 4B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,$
 0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,$
 0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B,0B]

End



;===================================================================================================
; Name:		plot_grid
;
; Type:		IDL Subroutine
;  
; Description:
;    To plot gridded data into png files using z-buffer
;
; 
; Arguments:    
;     
;	Name	    		Type	    	Description
;     ---------------------------------------------------
;	tmp   			i 		img array      
;	sfcMask			i		land/sea mark (the same dimension as tmp)
;	map_name		i		map name
;	minvalue		i		low bound
;	maxvalue		i		high bound
;	title			i		title
;	sfcPick			i		pick between sea/land/all (0/1/2)
;	lndsea_tag 		i		land/sea Tag    
;	DIV 			i		Color bar division
;     	format			i		Color bar digit format, (I2), (f3.1), etc 
;	LatMin 			i		Minimum lat of output image
;	LatMax 			i		Maximum lat of output image 
;	LonMin 			i		Minimum lon of output image
;	LonMax 			i		Maximum lon of output image
;	color_table_index     	i		Optional argument of color table index
;
;					0 - 39:   the system predefined color table
;					41:       MSPPS TPW/CLW/RR like color table
;					42:	  MSPPS Sea Ice color table
;					43:	  MSPPS SWE color table
;					44:	  MSPPS Climate color table
;					45:	  MIRS bias color table
;     
; Subroutines needed:
;	- ColorBar
;
;
; Record of revisions:
;        Date          Programmer      		Description of change
;    ============    ==============    =========================================
;     09/01/2007       Wanchun Chen     	Original code
;     02/17/2009       Wanchun Chen     	Added optional color table index
;     06/28/2011       Wanchun Chen     	Added -888.0 to distinguish "no data" (-999.0) and "qc fail ( -99.0 )".
;     
;===================================================================================================
Pro plot_grid,tmp,sfcMask,map_name,minvalue,maxvalue,latmin,latmax,lonmin,lonmax,title,sfcPick,div,format,$
	      color_table_index=color_table_index

;---- scaling
image=bytscl(tmp, min=minvalue, max=maxvalue, top=239) + 1B

;---- device set up
set_plot,'Z'
device,set_resolution=[650, 500]
position=[0.05, 0.15, 0.95, 0.9]


;---- load color table 33 if no color table given
IF N_ELEMENTS(color_table_index) EQ 0 THEN BEGIN
  loadct, 33, /SILENT
  TVLCT, r, g, b, /get
ENDIF

;---- if have color_table_index, then use it
IF N_ELEMENTS(color_table_index) EQ 1 THEN BEGIN

  IF color_table_index GE 0 and color_table_index LE 40 THEN BEGIN
    loadct, color_table_index, /SILENT
    TVLCT, r, g, b, /get
  ENDIF

  IF color_table_index EQ 41 THEN BEGIN
    ; load MSPPS TPW colors
    loadct_tpw, r, g, b
    TVLCT, r, g, b
  ENDIF  

  IF color_table_index EQ 42 THEN BEGIN
    ; load MSPPS Climate colors
    loadct_sice, r, g, b 
    TVLCT, r, g, b
  ENDIF  
  
  IF color_table_index EQ 43 THEN BEGIN
    ; load MSPPS Climate colors
    loadct_swe, r, g, b 
    TVLCT, r, g, b
  ENDIF  
 
  IF color_table_index EQ 44 THEN BEGIN
    ; load MSPPS Climate colors
    loadct_climate, r, g, b 
    TVLCT, r, g, b
  ENDIF  
 
  ;---- more options go here
  
ENDIF


r(0)=255   & g(0)=255   & b(0)=255   	; Load White Color          - 0   for white background 
r(255)=0   & g(255)=0   & b(255)=0 	; Load Black Color          - 255 for black color
r(254)=255 & g(254)=255 & b(254)=255	; Load White Color          - 254 for Missing Data
r(253)=215 & g(253)=215 & b(253)=215	; Load Grey Color (20%)     - 253 for Land/Ocean Coverage
r(252)=98  & g(252)=98  & b(252)=98	; Load Grey Color (70%)     - 252 for non-convergent points(qc fail)
r(251)=176 & g(251)=196 & b(251)=222	; Load Lightsteelblue Color - 251 for non-reported points(snow/ice prevents)
drawColor = 255

;---- color adjustment
info=size(tmp)
NCOL=info[1]
NROW=info[2]

;---- a parameter to indicate whether we have -888.0 values or not
non_reported_existence=0

for icol=0, NCOL-1 do begin
for irow=0, NROW-1 do begin
  if ( minvalue gt 0 and tmp(icol,irow) ge 0 and tmp(icol,irow) lt minvalue ) then image(icol,irow) = 1
  if tmp(icol,irow) eq -99.0  then image(icol,irow) = 252
  if tmp(icol,irow) eq -999.0 then image(icol,irow) = 0
  if tmp(icol,irow) eq -888.0 then begin
     image(icol,irow) = 251
     non_reported_existence = 1
  endif   
  if ( sfcPick eq 0 and sfcMask(icol,irow) eq 1 ) then image(icol,irow)  = 253
  if ( sfcPick eq 1 and sfcMask(icol,irow) eq 0 ) then image(icol,irow)  = 253
endfor
endfor

;---- map setup
map_set, /cyl, limit=[latmin,lonmin,latmax,lonmax], position=position, $
	title=title, color=drawColor, charsize=0.8, /NOBORDER  
warp=map_image(image,xx,yy,xs,ys,compress=1)
TV,warp,xx,yy,xsize=xs,ysize=ys

;---- xticks step
lon_span = lonmax-lonmin
londel = 30
if lon_span le  30 then londel = 2
if lon_span gt  30 and lon_span le 60 then londel = 5
if lon_span gt  60 and lon_span le 120 then londel = 10
if lon_span gt 120 and lon_span le 240 then londel = 20
if lon_span gt 240 then londel = 30

;---- yticks step
lat_span = latmax-latmin
latdel = 15
if lat_span le  15 then latdel = 1
if lat_span gt  15 and lat_span le 60 then latdel = 5
if lat_span gt  60 and lat_span le 90 then latdel = 10
if lat_span gt  90 then latdel = 15
 
MAP_GRID, LatDel=latdel, LonDel=londel, color=drawColor
MAP_CONTINENTS, /CONT, /COUNTRIES, /USA, COLOR=drawColor, /HIRES

ticks_lon=FIX(lon_span/londel)
ticks_lat=FIX(lat_span/latdel)

longitude = lonmin + findgen(ticks_lon+1)*londel
latitude  = latmin + findgen(ticks_lat+1)*latdel

;---- plot X-Y axis
Plot, longitude, latitude, XStyle=1, YStyle=1, Xticks=ticks_lon, Yticks=ticks_lat, $
      POSITION=position, xrange=[lonmin,lonmax], yrange=[latmin,latmax], $
      Color=drawColor, Charsize=0.8, /NoData, /NoErase

;---- Plot color bar
position_bar=fltarr(4)
position_bar[0]=position[0]+0.325
position_bar[1]=position[1]-0.1
position_bar[2]=position[2]
position_bar[3]=position_bar[1]+0.03
ColorBar,Bottom=1,NColors=240,Color=drawColor,Position=position_bar,Format=format,$
	 Divisions=div,Minor=1,TickLen=0.0001,Range=[minvalue,maxvalue],CharSize=0.8

;---- Plot Missing lengend (white)
mis_position=fltarr(4)
mis_position[0]=position[0]-0.025
mis_position[1]=position[1]-0.1
mis_position[2]=position[0]+0.025
mis_position[3]=mis_position[1]+0.03
ColorBar,Bottom=0,NColors=1, Position=mis_position, Divisions=2, COLOR=drawColor,$
	 Minor=1,TickLen=0.0001,TickNames=[' ','NoData',' '],CharSize=0.8

;---- Plot QC fail lengend (gray)
qc_position=fltarr(4)
qc_position[0]=position[0]+0.05
qc_position[1]=position[1]-0.1
qc_position[2]=position[0]+0.10
qc_position[3]=qc_position[1]+0.03
ColorBar,Bottom=252,NColors=1, Position=qc_position, Divisions=2, COLOR=drawColor,$
	 Minor=1,TickLen=0.0001,TickNames=[' ','QC fail',' '],CharSize=0.8

;---- Plot land/sea legend, if sfcPick is 0-ocean or 1-land
sfc_position=fltarr(4)
sfc_position[0]=position[0]+0.125
sfc_position[1]=position[1]-0.1
sfc_position[2]=position[0]+0.175
sfc_position[3]=sfc_position[1]+0.03

if ( sfcPick eq 0 ) then maskNames = [' ','Land',' '] ; we mask land if only plot ocean part
if ( sfcPick eq 1 ) then maskNames = [' ','Sea', ' '] ; we mask sea  if only plot land part

if ( sfcPick eq 0 or sfcPick eq 1 ) then $
  ColorBar,Bottom=253,NColors=1, Position=sfc_position, Divisions=2, COLOR=drawColor,$
	   Minor=1,TickLen=0.0001,TickNames=maskNames,CharSize=0.8

;---- plot non-reported legend if -888.0 appears
non_position=fltarr(4)
non_position[0]=position[0]+0.20
non_position[1]=position[1]-0.1
non_position[2]=position[0]+0.25
non_position[3]=non_position[1]+0.03
if ( non_reported_existence eq 1 ) then $
  ColorBar,Bottom=251,NColors=1, Position=non_position, Divisions=2, COLOR=drawColor,$
	   Minor=1,TickLen=0.0001,TickNames=[' ','NoReport',' '],CharSize=0.8

;---- write out image file
write_png, map_name, TVRD(), r, g, b

device,/close

end


;===================================================================================================
; Name:		Plot_GridQCF
;
; Type:		IDL Subroutine
;  
; Description:
;    To plot gridded data into png files using z-buffer and using
;    high contrast between the discrete values of gridded data
;
; 
; Arguments:    
;     
;	Name	    	Type	    	Description
;     ---------------------------------------------------
;	tmp   		i 		img array      
;	sfcMask		i		land/sea mark (the same dimension as tmp)
;	map_name	i		map name
;	minvalue	i		low bound
;	maxvalue	i		high bound
;	title		i		title
;	sfcPick		i		pick between sea/land/all (0/1/2)
;	lndsea_tag 	i		land/sea Tag    
;	DIV 		i		Color bar division
;     	format		i		Color bar digit format, (I2), (f3.1), etc 
;	LatMin 		i		Minimum lat of output image
;	LatMax 		i		Maximum lat of output image 
;	LonMin 		i		Minimum lon of output image
;	LonMax 		i		Maximum lon of output image
;     
;
; Subroutines needed:
;	- ColorBar
;
; History:
;     06/06/2008       Flavio Iturbide-Sanchez, IMSG Inc @ NOAA/NESDIS/STAR     	
;     
;===================================================================================================
Pro plot_gridQCF,tmp,sfcMask,map_name,minvalue,maxvalue,latmin,latmax,lonmin,lonmax,title,sfcPick,div,format

;---- scaling
image=bytscl(tmp, min=minvalue, max=maxvalue, top=239) + 1B

;;---- device set up
set_plot,'Z'
device,set_resolution=[650, 500]
position=[0.05, 0.15, 0.95, 0.9]

;---- load color table
loadct, 39, /SILENT 	
TVLCT, r, g, b, /get
;0
for i=1,11 do begin
   r(i)=245 & g(i)=245 & b(i)=220	;beige 
endfor
;1:
for i=12,38 do begin
  r(i)=255 & g(i)=0 & b(i)=0		;red [14,16] 
endfor
;2:
for i=39,65  do begin
  r(i)=51 & g(i)=102 & b(i)=255	        ;light blue
endfor
;3:
for i=66,92 do begin
  r(i)=0 & g(i)=255 & b(i)=255		;aqua [0, 2]
endfor
;4:
for i=93,119 do begin
  r(i)=0 & g(i)=0 & b(i)=255		;blue [2,4]
endfor
;5:
for i=120,146 do begin
  r(i)=50 & g(i)=205 & b(i)=50		;light green [6,8]
endfor
;6:
for i=147,173 do begin
   r(i)=0 & g(i)=255 & b(i)=0		;bright green 
endfor
;7:
for i=174,200 do begin
   r(i)=0 & g(i)=102 & b(i)=0		;dark green
endfor
;8:
for i=201,227 do begin
  r(i)=255 & g(i)=205 & b(i)=0		;dark yellow
endfor
;9:
for i=228,239 do begin
  r(i)=255 & g(i)=90 & b(i)=0		;orange
endfor

r(0)=255   & g(0)=255   & b(0)=255   	; Load White Color - 0 for background 
r(255)=0   & g(255)=0   & b(255)=0 	; Load Black Color - 255
r(254)=255 & g(254)=255 & b(254)=255	; Load White Color - 254 for Missing Data
r(253)=215 & g(253)=215 & b(253)=215	; Load Grey Color (20%) - 253 for Land/Ocean Coverage
r(252)=98  & g(252)=98  & b(252)=98	; Load Grey Color (70%) - 252 for non-convergent points(qc fail)
drawColor = 255

;---- color adjustment
info=size(tmp)
NCOL=info[1]
NROW=info[2]

for icol=0, NCOL-1 do begin
for irow=0, NROW-1 do begin
  if ( minvalue gt 0 and tmp(icol,irow) ge 0 and tmp(icol,irow) lt minvalue ) then image(icol,irow) = 1
  if tmp(icol,irow) eq -99.0 then image(icol,irow)  = 252
  if tmp(icol,irow) eq -999.0 then image(icol,irow) = 0
  if ( sfcPick eq 0 and sfcMask(icol,irow) eq 1 ) then image(icol,irow)  = 253
  if ( sfcPick eq 1 and sfcMask(icol,irow) eq 0 ) then image(icol,irow)  = 253
endfor
endfor

;---- map setup
map_set, /cyl, limit=[latmin,lonmin,latmax,lonmax], position=position, $
	title=title, color=drawColor, charsize=0.8, /NOBORDER  
warp=map_image(image,xx,yy,xs,ys,compress=1)
TV,warp,xx,yy,xsize=xs,ysize=ys

MAP_GRID, LatDel=15, LonDel=30, color=drawColor
MAP_CONTINENTS, /CONT, /COUNTRIES, /USA, COLOR=drawColor, /HIRES

ticks_lon=FIX((lonmax-lonmin)/30)
ticks_lat=FIX((latmax-latmin)/15)

longitude = lonmin + findgen(ticks_lon+1)*30
latitude  = latmin + findgen(ticks_lat+1)*15

;---Labels
legen1='0:GOOD'
legen2='1:BAD'
legen3='PRECIP:'
legen4='2:LGHT'
legen5='3:MED'
legen6='4:HVY'
legen7='INVERS:'
legen8='5:TMP'
legen9='6:HUM'
legen10='7:TMP+HUM'
legen11='SATUR:'
legen12='8:CLD/PRECIP'
legen13='9:noCLD/noPRECIP'

;---- plot X-Y axis
Plot, longitude, latitude, XStyle=1, YStyle=1, Xticks=ticks_lon, Yticks=ticks_lat, $
      POSITION=position, xrange=[lonmin,lonmax], yrange=[latmin,latmax], $
      Color=drawColor, Charsize=0.8, /NoData, /NoErase
XYOUTS, -194,-124, legen1, charsize=0.79,charthick=1.0
XYOUTS, -169,-124, legen2, charsize=0.79,charthick=1.0,color=12
XYOUTS, -147,-124, legen3, charsize=0.79,charthick=1.0
XYOUTS, -120,-124, legen4, charsize=0.79,charthick=1.0,color=39
XYOUTS, -95, -124, legen5, charsize=0.79,charthick=1.0,color=66
XYOUTS, -73, -124, legen6, charsize=0.79,charthick=1.0,color=93
XYOUTS, -51, -124, legen7, charsize=0.79,charthick=1.0
XYOUTS, -25, -124, legen8, charsize=0.79,charthick=1.0,color=120
XYOUTS, -4,  -124, legen9, charsize=0.79,charthick=1.0,color=147
XYOUTS, 19,  -124, legen10,charsize=0.79,charthick=1.0,color=174
XYOUTS, 60,  -124, legen11,charsize=0.79,charthick=1.0
XYOUTS, 85,  -124, legen12,charsize=0.79,charthick=1.0,color=201
XYOUTS, 133, -124, legen13,charsize=0.79,charthick=1.0,color=228

;---- Plot color bar
position_bar=fltarr(4)
position_bar[0]=position[0]+0.3
position_bar[1]=position[1]-0.080
position_bar[2]=position[2]
position_bar[3]=position_bar[1]+0.03
ColorBar,Bottom=1,NColors=240,Color=drawColor,Position=position_bar,Format=format,$
	Divisions=div,Minor=1,TickLen=0.0001,Range=[minvalue,maxvalue],CharSize=0.8

;---- Plot Missing lengend (white)
mis_position=fltarr(4)
mis_position[0]=position[0]
mis_position[1]=position[1]-0.080
mis_position[2]=position[0]+0.05
mis_position[3]=mis_position[1]+0.03
ColorBar,Bottom=0,NColors=1, Position=mis_position, Divisions=2, COLOR=drawColor,$
	Minor=1,TickLen=0.0001,TickNames=[' ','NoData',' '],CharSize=0.8

;---- Plot land/sea legend, if sfcPick is 0-ocean or 1-land
sfc_position=fltarr(4)
sfc_position[0]=position[0]+0.15
sfc_position[1]=position[1]-0.080
sfc_position[2]=position[0]+0.2
sfc_position[3]=sfc_position[1]+0.03

if ( sfcPick eq 0 ) then maskNames = [' ','Land',' ']  ; we mask land if only plot ocean part
if ( sfcPick eq 1 ) then maskNames = [' ','Ocean',' '] ; we mask ocean if only plot land part

if ( sfcPick eq 0 or sfcPick eq 1 ) then $
    ColorBar,Bottom=253,NColors=1, Position=sfc_position, Divisions=2, COLOR=drawColor,$
	Minor=1,TickLen=0.0001,TickNames=maskNames,CharSize=0.8

;---- write out image file
write_png, map_name, TVRD(), r, g, b

end


;===================================================================================================
; Name:         Plot_Polar
;
; Type:         IDL Subroutine
;
; Description:
;    To plot snow parameters in Polar Stereograplic Projection
;
;
; Arguments:
;
;       Name            Type            Description
;     ---------------------------------------------------
;       tmp             i               vector of product data (e.g., sea ice, swe, surface type,etc)
;       sfcMask         i               vector of surface type ( ocean <= 1, land > 1)
;       map_name        i               map name
;       minvalue        i               low bound
;       maxvalue        i               high bound
;       title           i               title
;       sfcPick         i               pick between sea/land/all (0/1/2)
;       DIV             i               Color bar division
;       format          i               Color bar digit format, (I2), (f3.1), etc
;       LatMin          i               Minimum lat of output image
;       LatMax          i               Maximum lat of output image
;       LonMin          i               Minimum lon of output image
;       LonMax          i               Maximum lon of output image
;       centrlat        i               Central latitude of projection, e.g., 90.0 for Northern hemisphere
;       orientlon       i               Longitude orientation of projection, e.g., -80.0 for Northern Hemisphere
;       xlat            i               vector of latitude values 
;       xlon            i               vector of longitude values
;
;
; Subroutines needed:
;       - ColorBar
;
;
; Record of revisions:
;        Date          Programmer               Description of change
;    ============    ==============    =========================================
;     02/29/2008       Cezar Kongoli            Original code
;     02/17/2009       Wanchun Chen             add an optional argument symsize
;
;==============================================================================================
Pro plot_polar,tmp,sfcMask,map_name,minvalue,maxvalue,latmin,latmax,lonmin,lonmax,centrlat,orientlon,xlat,xlon,title, $ 
    sfcPick,div,format,symsize=symsize

;---- scaling
image=bytscl(tmp, min=minvalue, max=maxvalue, top=239) + 1B

;---- device set up
set_plot,'Z'
device,set_resolution=[650, 500]
position=[0.05, 0.15, 0.95, 0.9]

;---- load color table
loadct, 33, /SILENT
TVLCT, r, g, b, /get

r(0)=255   & g(0)=255   & b(0)=255   	; Load White Color - 0 for background 
r(255)=0   & g(255)=0   & b(255)=0 	; Load Black Color - 255
r(254)=255 & g(254)=255 & b(254)=255	; Load White Color - 254 for Missing Data
r(253)=215 & g(253)=215 & b(253)=215	; Load Grey Color (20%) - 253 for Land/Ocean Coverage
r(252)=98  & g(252)=98  & b(252)=98	; Load Grey Color (70%) - 252 for non-convergent points(qc fail)
drawColor = 255


;---- color adjustment
nrec=n_elements(tmp)

for iprof=0L, nrec-1 do begin
  if ( minvalue gt 0 and tmp(iprof) ge 0 and tmp(iprof) lt minvalue ) then image(iprof) = 1
  if tmp(iprof) eq -99.0 then image(iprof)  = 252
  if tmp(iprof) eq -999.0 then image(iprof) = 0
  if ( sfcPick eq 0 and (sfcMask(iprof) eq 2 or sfcMask(iprof) eq 3) ) then image(iprof)  = 253  ; pick ocean, filter out land(2) and snow(3)
  if ( sfcPick eq 1 and (sfcMask(iprof) eq 0 or sfcMask(iprof) eq 1) ) then image(iprof)  = 253  ; pick land, filter out ocean(0) and sea ice(1)
endfor

;---- defining user symbol for plotting values
csize = 16.0
scal = 0.8
a = findgen(csize+1) * (!PI*2.5/float(csize))
usersym,scal*cos(a)/2,scal*sin(a)/2,/fill

;---- defining latitude grid depending on projection
if (centrlat gt 0.0) then begin
  lats =[0,15,30,45,60,75,90]
  Projection = 'Northern Hemisphere'
endif
if (centrlat lt 0.0) then begin
  Projection = 'Southern Hemisphere'
  lats =[0,-15,-30,-45,-60,-75,-90]
endif
lons=[-180,-135,-90,-45,0,45,90,135,180]

;---- map setup
MAP_SET, centrlat, 0, orientlon, /Stereo, /GRID, /Isotropic, /Horizon, E_GRID={LABEL:2},position=position,title=title, $
        color=255, charsize=0.75,/NOBORDER, limit=[latmin,lonmin,latmax,lonmax]

;---- Plot values on map
if n_elements(symsize) eq 1 then symbsize=symsize
if n_elements(symsize) eq 0 then symbsize=0.8

FOR iprof=0L,nrec-1 DO BEGIN
    oplot,[xlon[iprof]],[xlat[iprof]],color=image[iprof],psym=8,symsize=symbsize,thick=0.5
ENDFOR

MAP_GRID, LABEL=1, LATS=lats, LATLAB=-25, LONS=lons,  LONLAB=5, GLINESTYLE=0, color=255
MAP_CONTINENTS, /CONTINENTS, /COUNTRIES, /USA, COLOR=255


;---- Plot color bar
position_bar=fltarr(4)
position_bar[0]=position[0]+0.3
position_bar[1]=position[1]-0.1
position_bar[2]=position[2]
position_bar[3]=position_bar[1]+0.03
ColorBar,Bottom=1,NColors=240,Color=drawColor,Position=position_bar,Format=format,$
	Divisions=div,Minor=1,TickLen=0.0001,Range=[minvalue,maxvalue],CharSize=0.8

;---- Plot Missing lengend (white)
mis_position=fltarr(4)
mis_position[0]=position[0]
mis_position[1]=position[1]-0.1
mis_position[2]=position[0]+0.05
mis_position[3]=mis_position[1]+0.03
ColorBar,Bottom=0,NColors=1, Position=mis_position, Divisions=2, COLOR=drawColor,$
	Minor=1,TickLen=0.0001,TickNames=[' ','NoData',' '],CharSize=0.8

;---- Plot QC fail lengend (gray)
qc_position=fltarr(4)
qc_position[0]=position[0]+0.075
qc_position[1]=position[1]-0.1
qc_position[2]=position[0]+0.125
qc_position[3]=qc_position[1]+0.03
ColorBar,Bottom=252,NColors=1, Position=qc_position, Divisions=2, COLOR=drawColor,$
	Minor=1,TickLen=0.0001,TickNames=[' ','QC fail',' '],CharSize=0.8

;---- Plot land/sea legend, if sfcPick is 0-ocean or 1-land
sfc_position=fltarr(4)
sfc_position[0]=position[0]+0.15
sfc_position[1]=position[1]-0.1
sfc_position[2]=position[0]+0.2
sfc_position[3]=sfc_position[1]+0.03

if ( sfcPick eq 0 ) then maskNames = [' ','Land',' ']  ; we mask land if only plot ocean part
if ( sfcPick eq 1 ) then maskNames = [' ','Ocean',' '] ; we mask ocean if only plot land part

if ( sfcPick eq 0 or sfcPick eq 1 ) then $
    ColorBar,Bottom=253,NColors=1, Position=sfc_position, Divisions=2, COLOR=drawColor,$
	Minor=1,TickLen=0.0001,TickNames=maskNames,CharSize=0.8

;---- write out image file
write_png, map_name, TVRD(), r, g, b

end


;===================================================================================================
; Name:         Plot_Snow_Polar
;
; Type:         IDL Subroutine
;
; Description:
;    To plot snow parameters in Polar Stereograplic Projection
;
;
; Arguments:
;
;       Name            Type            Description
;     ---------------------------------------------------
;       tmp             i               vector of product data (e.g., sea ice, swe, surface type,etc)
;       sfcMask         i               vector of surface type ( ocean <= 1, land > 1)
;       map_name        i               map name
;       minvalue        i               low bound
;       maxvalue        i               high bound
;       title           i               title
;       sfcPick         i               pick between sea/land/all (0/1/2)
;       DIV             i               Color bar division
;       format          i               Color bar digit format, (I2), (f3.1), etc
;       LatMin          i               Minimum lat of output image
;       LatMax          i               Maximum lat of output image
;       LonMin          i               Minimum lon of output image
;       LonMax          i               Maximum lon of output image
;       centrlat        i               Central latitude of projection, e.g., 90.0 for Northern hemisphere
;       orientlon       i               Longitude orientation of projection, e.g., -80.0 for Northern Hemisphere
;       xlat            i               vector of latitude values 
;       xlon            i               vector of longitude values
;
;
; Subroutines needed:
;       - ColorBar
;
;
; Record of revisions:
;        Date          Programmer               Description of change
;    ============    ==============    =========================================
;     02/29/2008       Cezar Kongoli            Original code
;     10/29/2008       Wanchun Chen             Modified for Snow to be the same as SSD snow & ice chart
;
;==============================================================================================
Pro plot_snow_polar,tmp,sfcMask,map_name,minvalue,maxvalue,latmin,latmax,lonmin,lonmax,centrlat,$
    orientlon,xlat,xlon,title,sfcPick,div,format

;---- device set up
set_plot,'Z'
device,set_resolution=[650, 500]
position=[0.05, 0.10, 0.95, 0.9]

;---- load color table
loadct, 39, /silent
TVLCT, r, g, b, /get

r(0)=0 & g(0)=0 & b(0)=0

for i=1,240 do begin
  r(i)=255 & g(i)=255 & b(i)=255
endfor

for i=241,251 do begin
  r(i)=0 & g(i)=0 & b(i)=255
endfor

r(252)=50   & g(252)=205  & b(252)=50
r(253)=255  & g(253)=255  & b(253)=0 	; yellow
r(254)=0    & g(254)=0    & b(254)=255
r(255)=255  & g(255)=255  & b(255)=255

drawColor = 255

;---- color adjustment
nrec=n_elements(tmp)
;---- scaling
image=bytscl(tmp, min=minvalue, max=maxvalue, top=250)
image(*) = 0B

for iprof=0L, nrec-1 do begin
  if ( tmp(iprof) ge 1 ) then image(iprof)  = 1
  if ( tmp(iprof) lt 1 ) then begin
    if ( sfcMask(iprof) eq 0 ) then image(iprof)  = 254  ; ocean(0)  - blue
    if ( sfcMask(iprof) eq 1 ) then image(iprof)  = 253  ; seaice(1) - yellow
    if ( sfcMask(iprof) eq 2 ) then image(iprof)  = 252  ; land(2)   - green
  endif
endfor

; defining user symbol for plotting values
csize = 16.0
scal = 0.8
a = findgen(csize+1) * (!PI*2.5/float(csize))
usersym,scal*cos(a)/2,scal*sin(a)/2,/fill

; defining latitude grid depending on projection
if (centrlat gt 0.0) then lats =[0,15,30,45,60,75,90]
if (centrlat lt 0.0) then lats =[0,-15,-30,-45,-60,-75,-90]
lons=[-180,-135,-90,-45,0,45,90,135,180]

;---- map setup
MAP_SET, centrlat, 0, orientlon, /Stereo, /GRID, /Isotropic, /Horizon, E_GRID={LABEL:2}, position=position, title=title, $
         color=255, charsize=0.75,/NOBORDER, limit=[latmin,lonmin,latmax,lonmax]

; Plot values on map
nc=250
FOR iprof=0L,nrec-1 DO BEGIN
      oplot,[xlon[iprof]],[xlat[iprof]],color=image[iprof],psym=8,symsize=0.8,thick=0.5
ENDFOR

MAP_GRID, LABEL=1, LATS=lats, LATLAB=-25, LONS=lons,  LONLAB=5, GLINESTYLE=0, color=255
MAP_CONTINENTS, /CONTINENTS, /COUNTRIES, /USA, COLOR=0

;*********************************************************
; Draw color bars
;*********************************************************
position_bar=fltarr(4)

;---- snow bar
position_bar[0]=0.02
position_bar[2]=0.10

position_bar[1]=0.06
position_bar[3]=0.08

xstart = position_bar(0) * !D.X_VSIZE
ystart = position_bar(1) * !D.Y_VSIZE
xsize = (position_bar(2) - position_bar(0)) * !D.X_VSIZE
ysize = (position_bar(3) - position_bar(1)) * !D.Y_VSIZE

bar = REPLICATE(240B, 256) # REPLICATE(1B, 10)
bar = CONGRID(bar, CEIL(xsize), CEIL(ysize), /INTERP)
TV, bar, xstart, ystart

;---- ice bar 
position_bar[1]=0.02
position_bar[3]=0.04

xstart = position_bar(0) * !D.X_VSIZE
ystart = position_bar(1) * !D.Y_VSIZE
xsize = (position_bar(2) - position_bar(0)) * !D.X_VSIZE
ysize = (position_bar(3) - position_bar(1)) * !D.Y_VSIZE

bar = REPLICATE(253B, 256) # REPLICATE(1B, 10)
bar = CONGRID(bar, CEIL(xsize), CEIL(ysize), /INTERP)
TV, bar, xstart, ystart

xyouts, 0.11, 0.063, 'snow',	charsize=0.8,charthick=0.8, color=255, /normal
xyouts, 0.11, 0.023, 'ice',     charsize=0.8,charthick=0.8, color=255, /normal

;---- write out image file
write_png, map_name, TVRD(), r, g, b
device,/close
end


;===================================================================================================
; Name:		Plot_Vert
;
; Type:		IDL Subroutine
;  
; Description:
;    To plot gridded data vertical distribution
;
; 
; Arguments:    
;     
;	Name	    	Type	    	Description
;     ---------------------------------------------------
;	tmp   		i 		img array      
;	xval		i		x-axis
;	yval		i		y-axix
;	xrange		i		x range
;	yrange		i		y range
;	title		i		title
;	xtitle		i		xtitle
;	ytitle		i		ytitle
;	map_name	i		image file name
;	minvalue	i		min values
;	maxvalue	i		max values
;	Xticks 		i		# of x ticks
;	div 		i		division in color bar
;     
;
; Subroutines needed:
;	- ColorBar
;
;
; Record of revisions:
;        Date          Programmer      		Description of change
;    ============    ==============    =========================================
;     09/01/2007       Wanchun Chen     	Original code
;     04/08/2009       Wanchun Chen     	Modified to adjust vertical pressure
;     11/13/2009       Wanchun Chen     	remove qc fail part below psfc
;     
;===================================================================================================
Pro plot_vert,tmp,xval,yval,xrange,yrange,xtitle,ytitle,title,map_name,minvalue,maxvalue,Xticks,$
    div,fmt,Yticks,YtickName,x,psfc,color_table,isMirs

;---- device set up  - Z-buffer
set_plot,'Z'
;device,set_resolution=[650, 325]
;position=[0.1, 0.175, 0.95, 0.925]
device,set_resolution=[550, 325]
position=[0.1, 0.175, 0.975, 0.925]

;---- load color table
if color_table ne 41 then begin
  loadct, 33, /silent
  TVLCT, r, g, b, /get
endif

if color_table eq 41 then begin
  loadct_tpw, r, g, b
  TVLCT, r, g, b
endif

;---- to check if -99.0 values show up to decided whether to plot qc fail bar or not
tmpSaved = tmp
ss = where( FIX(tmp) eq -99, cnt_qcfail)

r(0)=255 & g(0)=255 & b(0)=255
r(255)=0 & g(255)=0 & b(255)=0
r(254)=255 & g(254)=255 & b(254)=255	; Load White Color - 254 for Missing Data
r(253)=215 & g(253)=215 & b(253)=215	; Load Grey Color (20%) - 253 for Land/Ocean Coverage
r(252)=98  & g(252)=98  & b(252)=98	; Load Grey Color (70%) - 252 for non-convergent points(qc fail)

drawColor = 255

;---- color adjustment
info=size(tmp)
XDIM=info[1]
YDIM=info[2]

PLAYERS = [  $
      0.0110,$
      0.0270,$
      0.0570,$
      0.1070,$
      0.1810,$
      0.2850,$
      0.4250,$
      0.6100,$
      0.8450,$
      1.1360,$
      1.4920,$
      1.9200,$
      2.4270,$
      3.0200,$
      3.7080,$
      4.4980,$
      5.3990,$
      6.4170,$
      7.5610,$
      8.8390,$
     10.2580,$
     11.8260,$
     13.5520,$
     15.4440,$
     17.5080,$
     19.7530,$
     22.1880,$
     24.8180,$
     27.6520,$
     30.6970,$
     33.9630,$
     37.4540,$
     41.1780,$
     45.1440,$
     49.3580,$
     53.8270,$
     58.5570,$
     63.5570,$
     68.8330,$
     74.3900,$
     80.2360,$
     86.3760,$
     92.8170,$
     99.5650,$
    106.6270,$
    114.0070,$
    121.7120,$
    129.7460,$
    138.1150,$
    146.8260,$
    155.8810,$
    165.2870,$
    175.0480,$
    185.1690,$
    195.6550,$
    206.5080,$
    217.7340,$
    229.3370,$
    241.3210,$
    253.6890,$
    266.4440,$
    279.5910,$
    293.1310,$
    307.0680,$
    321.4060,$
    336.1460,$
    351.2920,$
    366.8450,$
    382.8090,$
    399.1840,$
    415.9720,$
    433.1760,$
    450.7970,$
    468.8360,$
    487.2960,$
    506.1750,$
    525.4760,$
    545.1990,$
    565.3460,$
    585.9160,$
    606.9090,$
    628.3260,$
    650.1660,$
    672.4300,$
    695.1160,$
    718.2250,$
    741.7570,$
    765.7090,$
    790.0800,$
    814.8710,$
    840.0790,$
    865.7040,$
    891.7430,$
    918.1950,$
    945.0570,$
    972.3290,$
   1000.0080,$
   1028.0900,$
   1056.5740,$
   1085.4580 ]

;--------------------------------------------------------------------------
; The relationship of layers and pressure layers are not linear.
; We need to adjust this according to surface pressure to reflect
; this in yaxis, which is pressure, sort of convert from layers into
; pressure(shrink in vertical direction to make it above surface line) 
;--------------------------------------------------------------------------
PRESSURE_SPAN = ABS(yrange(0)-yrange(1))

layers_psfc = intarr(XDIM)
layers_psfc(*) = -999
for icol=0, XDIM-1 do begin
  
  if psfc(icol) gt 0 then begin
      layers_psfc(icol)= FIX( YDIM*( 1-(psfc(icol)-yrange(1))/PRESSURE_SPAN ) )
      if layers_psfc(icol) lt 0    then  layers_psfc(icol) = 0
      if layers_psfc(icol) ge YDIM then  layers_psfc(icol) = YDIM-1
  endif
  
  ;---- find the lowest -99.0 value level as surface level when qc fails
  if fix(psfc(icol)) eq -99 then begin
    ss = where(tmp(icol,*) eq -99, cnt)
    if cnt ge 1 then begin
      ilay=99-ss(0)
      pressure=PLAYERS(ilay)
      layers_psfc(icol)= FIX( YDIM*( 1-(pressure-yrange(1))/PRESSURE_SPAN ) )
      if layers_psfc(icol) lt 0 then layers_psfc(icol) = 0
      if layers_psfc(icol) ge YDIM then layers_psfc(icol) = YDIM-1
    endif
  endif
  
endfor

;---- the lowest level to have valid values(non-missing values)
layers_valid = intarr(XDIM)
layers_valid(*) = -999
for icol = 0, XDIM-1 do begin
for ilay = YDIM-1, 0, -1 do begin  
  if tmp(icol,ilay) gt -999 then layers_valid(icol) = ilay
endfor
endfor

dummy=tmp
tmp(*,*)=-999.0
for icol = 0, XDIM-1 do begin
  if layers_psfc(icol) ge 0 and layers_valid(icol) ge 0 and layers_psfc(icol) ge layers_valid(icol) then begin
     ratio     = ( (YDIM-layers_valid(icol)) * 1.0 ) / ( (YDIM-layers_psfc(icol)) * 1.0 )
     intercept = layers_valid(icol) - ratio*layers_psfc(icol)
     for ilay_high = layers_psfc(icol), YDIM-1 do begin
       ilay_low = FIX( ratio * ilay_high + intercept )
       if ilay_low ge YDIM  then ilay_low = YDIM-1 
       if ilay_low lt 0     then ilay_low = 0
       tmp(icol,ilay_high) = dummy(icol,ilay_low)
     endfor
  endif
  if FIX(psfc(icol)) eq -99 and layers_psfc(icol) ge 0 and layers_psfc(icol) le (YDIM-1) then $
     tmp(icol,layers_psfc(icol):(YDIM-1)) = -99.0
endfor

;---- scaling
image=bytscl(tmp, min=minvalue, max=maxvalue, top=240)

if isMirs eq 4 then begin
   tmp = tmpSaved
   ss = where( FIX(tmp) eq -99, cnt)
   if cnt ge 1 then tmp(ss) = -999.0
   image=bytscl(tmp, min=minvalue, max=maxvalue, top=240)
endif

;---- adjust colors for some special values in the tmp
for irow=0, YDIM-1 do begin 
for icol=0, XDIM-1 do begin
  if tmp(icol,irow) eq -99.0  then image(icol,irow) = 252
  if tmp(icol,irow) eq -999.0 then image(icol,irow) = 254
endfor
endfor

;---- coordinates
xsize=(position[2] - position[0]) * !D.X_VSIZE
ysize=(position[3] - position[1]) * !D.Y_VSIZE
xstart=position[0] * !D.X_VSIZE
ystart=position[1] * !D.Y_VSIZE

TV, Congrid(image,xsize,ysize), xstart, ystart

;---- Plot X-Y axises
Plot, xval, yval, xrange=xrange, yrange=yrange, xtitle=xtitle, ytitle=ytitle, title=title,    $
      POSITION=position, Color=drawColor, Charsize=0.75, XStyle=1, YStyle=1, /NoData, /NoErase,$
      Xticks=Xticks, Yticks=Yticks, YtickName=YtickName

ss = where( psfc lt 0.0, cnt) & if( cnt gt 0 ) then psfc(ss) = !VALUES.F_NAN        
Oplot, x, psfc, color=255, thick=2

;---- Plot a color bar
position_bar=fltarr(4)
position_bar[0]=position[0]+0.3
position_bar[1]=position[1]-0.125
position_bar[2]=position[2]-0.1
position_bar[3]=position_bar[1]+0.03
ColorBar,Bottom=1,NColors=240,Color=drawColor,Position=position_bar,Format=fmt,$
	Divisions=div,Minor=1,TickLen=0.0001,Range=[minvalue,maxvalue],CharSize=0.75

;---- Plot Missing lengend (white)
position_bar[0]=position[0]
position_bar[1]=position[1]-0.125
position_bar[2]=position[0]+0.05
position_bar[3]=position_bar[1]+0.03
ColorBar,Bottom=0,NColors=1, Position=position_bar, Divisions=2, COLOR=drawColor,$
	Minor=1,TickLen=0.0001,TickNames=[' ','NoData',' '],CharSize=0.75

;---- Plot QC fail lengend (dark gray)
position_bar[0]=position[0]+0.075
position_bar[1]=position[1]-0.125
position_bar[2]=position[0]+0.125
position_bar[3]=position_bar[1]+0.03
if cnt_qcfail ge 1 then $
    ColorBar,Bottom=252,NColors=1, Position=position_bar, Divisions=2, COLOR=drawColor,$
	Minor=1,TickLen=0.0001,TickNames=[' ','QC fail',' '],CharSize=0.75

;---- surface pressure (black)
position_bar[0]=position[0]+0.175
position_bar[1]=position[1]-0.125
position_bar[2]=position[0]+0.225
position_bar[3]=position_bar[1]+0.03
ColorBar,Bottom=255,NColors=1, Position=position_bar, Divisions=2, COLOR=drawColor,$
	Minor=1,TickLen=0.0001,TickNames=[' ','sfc pressure',' '],CharSize=0.75

;---- write out image file
write_png, map_name, TVRD(), r, g, b
device, /close

End


;===================================================================================================
; Name:		Plot_Vert2
;
; Type:		IDL Subroutine
;  
; Description:
;    To plot gridded data vertical distribution of rain and graupel together
;    puls 4 curves: rain top, graupel bottom, freezing level and sfc pressure.
;
; 
; Arguments:    
;     
;	Name	    	Type	    	Description
;     ---------------------------------------------------
;	tmp   		i 		img array      
;	xval		i		x-axis
;	yval		i		y-axix
;	xrange		i		x range
;	yrange		i		y range
;	title		i		title
;	xtitle		i		xtitle
;	ytitle		i		ytitle
;	map_name	i		image file name
;	minvalue	i		min values
;	maxvalue	i		max values
;	Xticks 		i		# of x ticks
;	div 		i		division in color bar
;	fmt 		i		formt in color bar
;     
;
; Subroutines needed:
;	- ColorBar
;
;
; Record of revisions:
;        Date          Programmer      		Description of change
;    ============    ==============    =========================================
;     04/02/2009       Wanchun Chen     	Original code
;     04/08/2009       Wanchun Chen     	Modified to adjust vertical pressure
;     
;===================================================================================================
Pro plot_vert2, tmp,xval,yval,xrange,yrange,xtitle,ytitle,title,map_name,minvalue,maxvalue,Xticks,$
	        div,fmt,Yticks,YtickName,x,psfc,pfreezing,praintop,picebottom,color_table,isMirs

;---- device set up  - Z-buffer
set_plot,'Z'
;device,set_resolution=[650, 325]
;position=[0.1, 0.175, 0.95, 0.925]
device,set_resolution=[550, 325]
position=[0.1, 0.175, 0.975, 0.925]

;---- load color table
if color_table eq 41 then begin
  loadct_tpw, r, g, b
  TVLCT, r, g, b
endif

if color_table eq 45 then begin
  loadct_bias, r, g, b
  TVLCT, r, g, b
endif

if color_table ne 41 and color_table ne 45 then begin
  ;loadct, 33
  loadct, color_table, /silent
  TVLCT, r, g, b, /get
endif

;---- check to see if any -99 values there to determine if we need to plot qc fail bar
tmpSaved = tmp
ss = where( FIX(tmp) eq -99, cnt_qcfail)

PLAYERS = [  $
      0.0110,$
      0.0270,$
      0.0570,$
      0.1070,$
      0.1810,$
      0.2850,$
      0.4250,$
      0.6100,$
      0.8450,$
      1.1360,$
      1.4920,$
      1.9200,$
      2.4270,$
      3.0200,$
      3.7080,$
      4.4980,$
      5.3990,$
      6.4170,$
      7.5610,$
      8.8390,$
     10.2580,$
     11.8260,$
     13.5520,$
     15.4440,$
     17.5080,$
     19.7530,$
     22.1880,$
     24.8180,$
     27.6520,$
     30.6970,$
     33.9630,$
     37.4540,$
     41.1780,$
     45.1440,$
     49.3580,$
     53.8270,$
     58.5570,$
     63.5570,$
     68.8330,$
     74.3900,$
     80.2360,$
     86.3760,$
     92.8170,$
     99.5650,$
    106.6270,$
    114.0070,$
    121.7120,$
    129.7460,$
    138.1150,$
    146.8260,$
    155.8810,$
    165.2870,$
    175.0480,$
    185.1690,$
    195.6550,$
    206.5080,$
    217.7340,$
    229.3370,$
    241.3210,$
    253.6890,$
    266.4440,$
    279.5910,$
    293.1310,$
    307.0680,$
    321.4060,$
    336.1460,$
    351.2920,$
    366.8450,$
    382.8090,$
    399.1840,$
    415.9720,$
    433.1760,$
    450.7970,$
    468.8360,$
    487.2960,$
    506.1750,$
    525.4760,$
    545.1990,$
    565.3460,$
    585.9160,$
    606.9090,$
    628.3260,$
    650.1660,$
    672.4300,$
    695.1160,$
    718.2250,$
    741.7570,$
    765.7090,$
    790.0800,$
    814.8710,$
    840.0790,$
    865.7040,$
    891.7430,$
    918.1950,$
    945.0570,$
    972.3290,$
   1000.0080,$
   1028.0900,$
   1056.5740,$
   1085.4580 ]


r(0)=255 & g(0)=255 & b(0)=255
r(255)=0 & g(255)=0 & b(255)=0
r(254)=255 & g(254)=255 & b(254)=255	; Load White Color - 254 for Missing Data
r(253)=215 & g(253)=215 & b(253)=215	; Load Grey Color (20%) - 253 for Land/Ocean Coverage
r(252)=98  & g(252)=98  & b(252)=98	; Load Grey Color (70%) - 252 for non-convergent points(qc fail)

r(246)=255 & g(246)=0   & b(246)=255

if color_table ne 41 then begin
  r(247)=0   & g(247)=255 & b(247)=0
endif
if color_table eq 41 then begin
  r(247)=0   & g(247)=0 & b(247)=255    ; -66.0, rain top, color index 247 - blue or green, depending on color table used
endif

r(248)=127 & g(248)=90  & b(248)=88     ; -77.0, ice bottom, color index 248 - brown

drawColor = 255

;---- color adjustment
info=size(tmp)
XDIM=info[1]
YDIM=info[2]

;--------------------------------------------------------------------------
; The relationship of layers and pressure layers are not linear.
; We need to adjust this according to surface pressure to reflect
; this in yaxis, which is pressure, sort of convert from layers into
; pressure(shrink in vertical direction to make it above surface line) 
;--------------------------------------------------------------------------

PRESSURE_SPAN = ABS(yrange(0)-yrange(1))

layers_psfc = intarr(XDIM)
layers_psfc(*) = -999

for icol=0, XDIM-1 do begin
  
  if psfc(icol) gt 0 then begin
      layers_psfc(icol)= FIX( YDIM*( 1-(psfc(icol)-yrange(1))/PRESSURE_SPAN ) )
      if layers_psfc(icol) lt 0    then  layers_psfc(icol) = 0
      if layers_psfc(icol) ge YDIM then  layers_psfc(icol) = YDIM-1
  endif

  ;---- if qc failed, we need check -999 level to determine psfc
  ;---- above -999 levels, should be -99.0 levels 
  if fix(psfc(icol)) eq -99 then begin
    ss = where(tmp(icol,*) eq -999, cnt)
    if cnt ge 1 then begin
      ilay=100-ss(cnt-1)-1
      if ilay lt 0 then ilay = 0
      if ilay ge YDIM then ilay = YDIM-1
      pressure=PLAYERS(ilay)
      layers_psfc(icol) = FIX( YDIM*( 1-(pressure-yrange(1))/PRESSURE_SPAN ) )
      if layers_psfc(icol) lt 0 then layers_psfc(icol) = 0
      if layers_psfc(icol) ge YDIM then layers_psfc(icol) = YDIM-1
    endif
  endif
  
endfor


layers_valid = intarr(XDIM)
layers_valid(*) = -999
for icol = 0, XDIM-1 do begin
for ilay = YDIM-1, 0, -1 do begin  
  if tmp(icol,ilay) gt -99.0 then layers_valid(icol) = ilay
endfor
endfor

dummy=tmp
tmp(*,*)=-999.0

for icol = 0, XDIM-1 do begin

  if layers_psfc(icol) ge 0 and layers_valid(icol) ge 0 and layers_psfc(icol) gt layers_valid(icol) then begin
     
     ratio     = ( (YDIM-layers_valid(icol)) * 1.0 ) / ( (YDIM-layers_psfc(icol)) * 1.0 )
     intercept = layers_valid(icol) - ratio*layers_psfc(icol)
     
     for ilay_high = layers_psfc(icol), YDIM-1 do begin
       
       ilay_low = FIX( ratio * ilay_high + intercept )
       
       if ilay_low ge YDIM  then ilay_low = YDIM-1 
       if ilay_low lt 0     then ilay_low = 0
     	
       tmp(icol,ilay_high) = dummy(icol,ilay_low)

     endfor

  endif
 
  if FIX(psfc(icol)) eq -99 then tmp(icol,layers_psfc(icol):(YDIM-1)) = -99.0

endfor


;---- scaling
if isMirs eq 4 then begin
  tmp = tmpSaved
  ss = where( FIX(tmp) eq -99, cnt)
  if cnt ge 1 then tmp(ss) = -999.0
endif
  
image=bytscl(tmp, min=minvalue, max=maxvalue, top=240)

;---- adjust colors for some special values in the tmp
for icol=0, XDIM-1 do begin
for irow=0, YDIM-1 do begin 
  if tmp(icol,irow) eq -66.0  then image(icol,irow) = 247  ; rain top -66
  if tmp(icol,irow) eq -77.0  then image(icol,irow) = 248  ; ice bottom -77
  if tmp(icol,irow) eq -99.0  then image(icol,irow) = 252
  if tmp(icol,irow) eq -999.0 then image(icol,irow) = 254
endfor
endfor

;---- coordinates
xsize=(position[2] - position[0]) * !D.X_VSIZE
ysize=(position[3] - position[1]) * !D.Y_VSIZE
xstart=position[0] * !D.X_VSIZE
ystart=position[1] * !D.Y_VSIZE

TV, Congrid(image,xsize,ysize), xstart, ystart

;---- Plot X-Y axises
Plot, xval, yval, xrange=xrange, yrange=yrange, xtitle=xtitle, ytitle=ytitle, title=title,    $
      POSITION=position, Color=drawColor, Charsize=0.75, XStyle=1, YStyle=1, /NoData, /NoErase,$
      Xticks=Xticks,Yticks=Yticks, YtickName=YtickName      

ss = where( psfc       lt 0.0, cnt) & if( cnt gt 0 ) then psfc(ss)       = !VALUES.F_NAN        
ss = where( pfreezing  lt 0.0, cnt) & if( cnt gt 0 ) then pfreezing(ss)  = !VALUES.F_NAN        
;ss = where( praintop   lt 0.0, cnt) & if( cnt gt 0 ) then praintop(ss)   = !VALUES.F_NAN        
;ss = where( picebottom lt 0.0, cnt) & if( cnt gt 0 ) then picebottom(ss) = !VALUES.F_NAN        

Oplot, x, psfc,       color=255, thick=2
Oplot, x, pfreezing,  color=246, thick=1
;Oplot, x, praintop,   color=247, thick=2
;Oplot, x, picebottom, color=248, thick=2

;---- Plot a color bar
position_bar=fltarr(4)

;---- Plot Missing lengend (white)
position_bar[0]=0.01
position_bar[1]=position[1]-0.125
position_bar[2]=0.06
position_bar[3]=position_bar[1]+0.03
ColorBar,Bottom=0,NColors=1, Position=position_bar, Divisions=2, COLOR=drawColor,$
	Minor=1,TickLen=0.0001,TickNames=[' ','NoData',' '],CharSize=0.75

;---- Plot QC fail lengend (gray)
position_bar[0]=0.07
position_bar[1]=position[1]-0.125
position_bar[2]=0.12
position_bar[3]=position_bar[1]+0.03
if cnt_qcfail ge 1 then $
  ColorBar,Bottom=252,NColors=1, Position=position_bar, Divisions=2, COLOR=drawColor,$
	Minor=1,TickLen=0.0001,TickNames=[' ','QcFail',' '],CharSize=0.75

position_bar[0]=0.17
position_bar[1]=position[1]-0.125
position_bar[2]=0.53
position_bar[3]=position_bar[1]+0.03
ColorBar,Bottom=1,NColors=240,Color=drawColor,Position=position_bar,Format=fmt,$
	Divisions=div,Minor=1,TickLen=0.0001,Range=[minvalue,maxvalue],CharSize=0.75


;---- freezing level (pink)
position_bar[0]=0.56
position_bar[1]=position[1]-0.125
position_bar[2]=0.66
position_bar[3]=position_bar[1]+0.03
ColorBar,Bottom=246,NColors=1, Position=position_bar, Divisions=2, COLOR=drawColor,$
	Minor=1,TickLen=0.0001,TickNames=[' ','freezing',' '],CharSize=0.75

;---- rain top level (lime)
position_bar[0]=0.67
position_bar[1]=position[1]-0.125
position_bar[2]=0.77
position_bar[3]=position_bar[1]+0.03
ColorBar,Bottom=247,NColors=1, Position=position_bar, Divisions=2, COLOR=drawColor,$
	Minor=1,TickLen=0.0001,TickNames=[' ','top rain',' '],CharSize=0.75

;---- bottom level (brown)
position_bar[0]=0.78
position_bar[1]=position[1]-0.125
position_bar[2]=0.88
position_bar[3]=position_bar[1]+0.03
ColorBar,Bottom=248,NColors=1, Position=position_bar, Divisions=2, COLOR=drawColor,$
	Minor=1,TickLen=0.0001,TickNames=[' ','bot. ice',' '],CharSize=0.75

;---- surface pressure (black)
position_bar[0]=0.89
position_bar[1]=position[1]-0.125
position_bar[2]=0.99
position_bar[3]=position_bar[1]+0.03
ColorBar,Bottom=255,NColors=1, Position=position_bar, Divisions=2, COLOR=drawColor,$
	Minor=1,TickLen=0.0001,TickNames=[' ','sfc pres.',' '],CharSize=0.75

;---- write out image file
write_png, map_name, TVRD(), r, g, b
device, /close

End



;===================================================================================================
; Name:		Plot_Bias
;
; Type:		IDL Subroutine
;  
; Description:
;    To plot Bias ( many lines ), like plot_linem, m - multiples
;
; 
; Arguments:    
;     
;	Name	    	Type	    	Description
;     ---------------------------------------------------
;	x   		i 		x values   
;	bias		i		y valyes
;	yval		i		y-axix
;	title		i		title
;	xtitle		i		xtitle
;	ytitle		i		ytitle
;	png_file	i		image file name
;	npos 		i		# of position scan - # of lines to plot
;     
;
; Subroutines needed:
;	- ColorBar
;
;
; Record of revisions:
;        Date          Programmer      		Description of change
;    ============    ==============    	========================================
;     09/01/2007       Wanchun Chen     	Original code
;     
;===================================================================================================
Pro Plot_bias, x, bias,title,xtitle,ytitle,png_file,yrange,npos,xticks,isize

set_plot, 'z'

LoadCT, 39, /silent
charsz = 1
if ( ISIZE EQ 0 ) THEN charsz = 0.60

if ( ISIZE EQ 0 ) THEN device,set_resolution=[325, 250]
if ( ISIZE EQ 1 ) THEN device,set_resolution=[650, 500]
if ( ISIZE EQ 2 ) THEN device,set_resolution=[3000,500]

position = [0.1, 0.2, 0.95, 0.95]
if ( ISIZE EQ 0 ) THEN  position = [0.15,  0.25, 0.95, 0.925]
if ( ISIZE EQ 1 ) THEN  position = [0.10,  0.25, 0.95, 0.95]
if ( ISIZE EQ 2 ) THEN  position = [0.025, 0.25, 0.95, 0.95]

dummy=LABEL_DATE(DATE_FORMAT=['%M%D!C%Y'])

step = 240.0/(npos*1.0)

plot, x, bias[0,*], CHARSIZE=charsz, TITLE=title, YTITLE=ytitle,     $
  XTICKUNITS = ['Day'], XTICKFORMAT='LABEL_DATE', xstyle=1,ystyle=1, $
  COLOR=1, BACKGROUND=255, YRANGE=yrange, /NODATA, position=position,$
  xticks=xticks

for ipos=0, npos-1 do begin
    oplot, x, bias[*,ipos], COLOR=FIX(ipos*step), PSYM=0, SYMSIZE=charsz
endfor

;---- Draw a color bar
position_bar=fltarr(4)
position_bar[0]=position[0]+0.1
position_bar[1]=position[1]-0.175
position_bar[2]=position[2]-0.1
position_bar[3]=position_bar[1]+0.03
ColorBar,NColors=npos*step,Color=1,Position=position_bar, FORMAT='(I2)', $
	 Divisions=6,Minor=1,TickLen=0.0001,Range=[1,npos],CharSize=charsz

xyouts, position_bar[0]+0.25, position_bar[3]+0.02, 'Scan Position', $
        COLOR=1, CHARSIZE=charsz, CHARTHICK=1.0, /normal 

;---- write out image file
thisImage = TVRD()
TVLCT, r, g, b, /Get
Write_PNG, png_file, thisImage, r, g, b

device,/close

end



;===================================================================================================
; Name:		Plot_BiasMeanStdv
;
; Type:		IDL Subroutine
;  
; Description:
;    To plot Bias ( many lines ), like plot_linem, m - multiples
;
; 
; Arguments:    
;     
;	Name	    	Type	    	Description
;     ---------------------------------------------------
;	x   		i 		x values   
;	bias		i		y valyes
;	yval		i		y-axix
;	title		i		title
;	xtitle		i		xtitle
;	ytitle		i		ytitle
;	map_name	i		image file name
;	nlay 		i		# of layers
;	xticks 		i		# of ticks
;	isMeanOrNot 	i		1 - plot a 0 line, 0 - no
;     
;
; Subroutines needed:
;	- ColorBar
;
;
; Record of revisions:
;        Date          Programmer      		Description of change
;    ============    ==============    	========================================
;     01/15/2008       Wanchun Chen     	Original code
;     
;===================================================================================================
PRO PLOT_BiasMeanStdv, xval, bias, xtitle, ytitle, title, yrange, map_name, nlay, xticks, isMeanOrNot, ISIZE, range, div

SET_PLOT,'Z'
LoadCT, 39, /silent

DEVICE, SET_RESOLUTION=[650, 500]
if ISIZE EQ 0 THEN DEVICE, SET_RESOLUTION=[325, 250]
if ISIZE EQ 1 THEN DEVICE, SET_RESOLUTION=[650, 500]
if ISIZE EQ 2 THEN DEVICE, SET_RESOLUTION=[3000,500]

charsiz = 0.6
if ISIZE EQ 0 THEN charsiz = 0.6
if ISIZE EQ 1 THEN charsiz = 1.0 
if ISIZE EQ 2 THEN charsiz = 1.0 

symsize = 0.8

xmin=min(xval)
xmax=max(xval)

dummy=LABEL_DATE(DATE_FORMAT=['%M%D!C%Y'])
position = [0.15, 0.3, 0.95, 0.9]
if ISIZE EQ 0 THEN position = [0.15, 0.30, 0.95, 0.9]
if ISIZE EQ 1 THEN position = [0.15, 0.35, 0.95, 0.9]
if ISIZE EQ 2 THEN position = [0.15, 0.35, 0.95, 0.9]

PLOT, xval,bias[*,0],title=title,color=1,background=255,ytitle=ytitle,yrange=yrange, $ 
    MIN_VALUE=-98,MAX_VALUE=98,XTICKUNITS=['Day'], XTICKFORMAT='LABEL_DATE',         $
    xstyle=1,ystyle=1,xticks=xticks,CHARSIZE=charsiz,/nodata,position=position

for ilay=0, NLAY-1 do begin
  oplot, xval, bias[*,ilay], MIN_VALUE=-98, MAX_VALUE=98, COLOR=ilay*23, PSYM=0, SYMSIZE=symsize
endfor

if isMeanOrNot eq 1 then OPLOT, [xmin,xmax],  [0, 0], color=1, LINESTYLE=2, THICK=4

;---- Draw a color bar
if ISIZE EQ 0 THEN xyouts, 0.48, 0.13, 'Layer (mb)', COLOR=1, CHARSIZE=charsiz, CHARTHICK=1.0, /normal 
if ISIZE EQ 1 THEN xyouts, 0.48, 0.18, 'Layer (mb)', COLOR=1, CHARSIZE=charsiz, CHARTHICK=1.0, /normal 
if ISIZE EQ 2 THEN xyouts, 0.48, 0.18, 'Layer (mb)', COLOR=1, CHARSIZE=charsiz, CHARTHICK=1.0, /normal 


;---- Draw a color bar
position_bar=fltarr(4)
position_bar[0]=position[0]
position_bar[1]=position[1]-0.225
position_bar[2]=position[2]
position_bar[3]=position_bar[1]+0.03
ColorBar,NColors=nlay*23,Color=1,Position=position_bar, FORMAT='(I4)', $
	 Divisions=div,Minor=1,TickLen=0.0001,Range=range,CharSize=charsiz

;---- write out image file
thisImage = TVRD()
TVLCT, r, g, b, /Get
Write_PNG, map_name, thisImage, r, g, b

END

;===================================================================================================
; Name:		Plot_tsScores
;
; Type:		IDL Subroutine
;  
; Description:
;    To plot time series of skill scores ( including many lines ), like plot_linem, m - multiples
;
; 
; Arguments:    
;     
;	Name	    	Type	    	Description
;     ---------------------------------------------------
;	x   		i 		x values   
;	bias		i		y values
;	yval		i		y-axix
;	title		i		title
;	xtitle		i		xtitle
;	ytitle		i		ytitle
;	map_name	i		image file name
;	nseries 	i		# of time series (lines)
;	xticks 		i		# of ticks
;	isMeanOrNot 	i		1 - plot a 0 line, 0 - no
;     
;
; Subroutines needed:
;	- ColorBar
;
;
; Record of revisions:
;        Date          Programmer      		Description of change
;    ============    ==============    	========================================
;     01/15/2008       Wanchun Chen     	Original code
;     
;===================================================================================================
PRO PLOT_tsScores, xval, bias, xtitle, ytitle, title, yrange, map_name, nseries, xticks, isMeanOrNot, ISIZE, range, div, ticknames, psym

; don't plot if only 1 time level in series
if(n_elements(bias[*,0]) le 1)then begin
  print,'plot_tsScores: time series length le 1; skipping figure generation'
  return
endif

SET_PLOT,'Z'
LoadCT, 39, /silent

;DEVICE, SET_RESOLUTION=[650, 500]
if ISIZE EQ 0 THEN DEVICE, SET_RESOLUTION=[325, 250]
;if ISIZE EQ 0 THEN DEVICE, SET_RESOLUTION=[640, 480]
;if ISIZE EQ 0 THEN DEVICE, SET_RESOLUTION=[650, 500]
;if ISIZE EQ 1 THEN DEVICE, SET_RESOLUTION=[3000, 500]
if ISIZE EQ 1 THEN DEVICE, SET_RESOLUTION=[650, 500]

csize         = 16
a             = findgen(csize+1) *(!pi*2./float(csize))
usersym,cos(a)/2,sin(a)/2,/fill
; for resolution [325, 250]
;symsize    = 0.8
;charsiz = 0.6
; for resolution [650, 500]
;symsize    = 1.5
;charsiz = 1.2

xmin=min(xval[where(xval gt 0.)])
xmax=max(xval[where(xval gt 0.)])

;dummy=LABEL_DATE(DATE_FORMAT=['%M','%Y'])
dummy=LABEL_DATE(DATE_FORMAT=['%M%D!C%Y'])

sz=size(xval)

xmaxInd=n_elements(xval[*,0])-1
ilayMax=0
if(sz[0] eq 1)then begin
    ind=where(xval[*,0] eq xmax,count)
    if(count gt 0)then xmaxInd=ind[0]
endif else begin
    xmaxInd=-1
    for ilay=0, NSERIES-1 do begin
        ind=where(xval[*,ilay] eq xmax,count)
        if(count gt 0 and ind[0] gt xmaxInd)then begin
            xmaxInd=ind[0]
            ilayMax=ilay
        endif
    endfor
endelse
;print,'ilaymax,xmaxInd=',ilaymax,xmaxInd

if ISIZE EQ 0 THEN BEGIN
  symsize    = 0.8
  charsiz = 0.6
  position = [0.15, 0.2, 0.95, 0.9]
;  xticks=fix(n_elements(bias[*,0])/120)
;  xticks=fix(n_elements(bias[*,0])/14)
;  xtickinterval=0.5
;  xtickinterval=1
  if xmaxInd le 190 then xtickInterval=0.5
  if xmaxInd gt 190 and xmaxInd le 365 then xtickInterval=1
  if xmaxInd gt 365 and xmaxInd le 730 then xtickInterval=2
  if xmaxInd gt 730 and xmaxInd le 1095 then xtickInterval=3
  if xmaxInd gt 1095 and xmaxInd le 1460 then xtickInterval=6
  if xmaxInd gt 1460 and xmaxInd le 1825 then xtickInterval=6
  if xmaxInd gt 1825 then xtickInterval=6
  xminor=2
  xticks=6
ENDIF
if ISIZE EQ 1 THEN BEGIN
  symsize    = 1.0
  charsiz = 0.75
  position = [0.15, 0.2, 0.95, 0.9]
;  xticks=fix(n_elements(bias[*,0])/30)
;  xticks=fix(n_elements(bias[*,0])/14)
;  xtickinterval=0.5
;  xtickinterval=1
  if xmaxInd le 190 then xtickInterval=1
  if xmaxInd gt 190 and xmaxInd le 365 then xtickInterval=1
  if xmaxInd gt 365 and xmaxInd le 730 then xtickInterval=1
  if xmaxInd gt 730 and xmaxInd le 1095 then xtickInterval=2
  if xmaxInd gt 1095 and xmaxInd le 1460 then xtickInterval=3
  if xmaxInd gt 1460 and xmaxInd le 1825 then xtickInterval=6
  if xmaxInd gt 1825 then xtickInterval=6
  xminor=2
  xticks=6
ENDIF

if(nseries eq 1)then begin
  nadd=1
  multf=50.
  position = [0.15, 0.2, 0.95, 0.9]
endif else begin
  nadd=0
  multf=fix(255./(nseries-1))-1
  position = [0.2, 0.35, 0.95, 0.9]
endelse



PLOT, xval[0:xmaxInd,ilayMax], bias[*,0], title=title, $
    color=1,background=255,ytitle=ytitle, yrange=yrange,$ 
    MIN_VALUE=-98, MAX_VALUE=98,$
    XTICKUNITS = ['Day'], XTICKFORMAT='LABEL_DATE', $
    xstyle=1,ystyle=1,xticks=xticks, xthick=2, ythick=2, CHARSIZE=charsiz, xminor=xminor, /nodata, position=position

;find first index of time array with missing values (if any), and plot
;only prior points where time is not missing

for ilay=0, NSERIES-1 do begin
    if(sz[0] eq 1)then begin
        xvalInd=0
        ntValid=n_elements(xval)
    endif
    if(sz[0] gt 1)then begin
        xvalInd=ilay
        ntValid=n_elements(xval[*,xvalInd])
    endif
    ind=where(xval[*,xvalInd] gt 0.,count)
    if(count gt 0)then begin
        ntValid=ind[n_elements(ind)-1]
    endif
;    print,'ntValid=',ntValid

    oplot, xval[0:ntValid,xvalInd], bias[*,ilay], MIN_VALUE=-98, MAX_VALUE=98, COLOR=(ilay+nadd)*multf, PSYM=psym, SYMSIZE=symsize
endfor

if isMeanOrNot eq 1 then OPLOT, [xmin,xmax],  [0, 0], color=1, LINESTYLE=2, THICK=4

;---- Draw a color bar
if(nseries gt 1) then begin
  xyouts, 0.00, 0.26, 'Date:', 	  COLOR=1, CHARSIZE=charsiz, CHARTHICK=1.0, /normal 
;  xyouts, 0.00, 0.13, 'Score:', COLOR=1, CHARSIZE=charsiz, CHARTHICK=1.0, /normal 
  xyouts, 0.00, 0.13, 'Sensor:', COLOR=1, CHARSIZE=charsiz, CHARTHICK=1.0, /normal 

;---- Draw a color bar
  position_bar=fltarr(4)
  position_bar[0]=position[0]
  position_bar[1]=position[1]-0.225
  position_bar[2]=position[2]
  position_bar[3]=position_bar[1]+0.03
  ColorBar,NColors=(nseries-1)*multf,Color=1,Position=position_bar, FORMAT='(a3)', $
	Divisions=div,Minor=1,TickLen=0.0001,Range=range,CharSize=charsiz,Ticknames=ticknames

endif

;---- write out image file
thisImage = TVRD()
TVLCT, r, g, b, /Get
Write_PNG, map_name, thisImage, r, g, b

END

;===================================================================================================
; convert calendar date into julian date
; name: cal2jday
;
;	year: 	I
;	month: 	I
;	day:    I
; 	jday:   O
;
; authour: Wanchun Chen
;
;===================================================================================================
PRO cal2jday, year, month, day, jday

    JulianDate1=[ 0,  31,  60,  91,  121,  152,  182,  213,  244,  274,  305,  335 ]
    JulianDate2=[ 0,  31,  59,  90,  120,  151,  181,  212,  243,  273,  304,  334 ]
    
    div4   = year MOD 4
    div100 = year MOD 100
    div400 = year MOD 400
    
    leap = 0
    
    if (div4 eq 0 and div100 ne 0) or div400 eq 0 then leap = 1
    
    if ( leap eq 1 ) then begin
        im = month-1
	jday = JulianDate1(im) + day 
    endif
    
    if ( leap eq 0 ) then begin
        im = month-1
	jday = JulianDate2(im) + day 
    endif
    
    ;print, jday    
    
END



;===================================================================================================
; Name:		PLOT_ASYM ( Deprecated )
;
;
; Type:		IDL Subroutine
;
;
; Description: 	Plots the asymmetry between GDAS and MIRS retrievals.
;		Deprecated, replaced with Plot_line or plot_line2
; Arguments:
;
;      Name		    	Type	    	Description
;      ---------------------------------------------------
;	- bin_box		I		x-direction values
;	- mirs	               	I   		Y values     
;	- prod			I		product name
;	- filePrex		I		file name prefix
;	- date			I		date
;	- cend			I		cending (as, ds)
;	- title			I		title
;	- ytitle		I		y title
;	- yrange		I		y range
;	- figsDir		I		image directory
;
; Subroutines needed:
;       - None
;
;
; History:
;       09-22-2007      Wanchun Chen
;
;
;===================================================================================================
PRO PLOT_ASYM, bin_box, mirs, prod, YRange, Ytitle, filePrex, lndsea, date, cend, figsDir
   set_plot,'Z'
   device,set_resolution=[650, 500]
   r=bytarr(256) & g=bytarr(256) & b=bytarr(256)
   r(*)=255 & g(*)=255 & b(*)=255
   r(143)=0 & g(143)=255 & b(143)=0
   r(243)=255 & g(243)=0 & b(243)=0
   r(63)=0 & g(63)=0 & b(63)=255
   r(2)=255 & g(2)=127 & b(1)=0
   r(1)=0 & g(1)=0 & b(1)=0
   TVLCT, r, g, b
   color1=63 & color2=243 & color5=1 &color3=2
   !P.REGION = [0.05, 0.05, 0.95, 0.95]
   symsize = 0.8
   dumx=[1,2] & dumy=[1,2]
   xmin=-60  & xmax= 60
   cending = 'Asc'
   if ( cend eq 'ds' ) then cending = 'Des'
   title = 'MIRS Retrieval - NWP Analysis (GDAS) ' + date + ' ' + cending
   PLOT, dumx, dumy, CHARSIZE=1.0, TITLE=title,$
     COLOR=1, BACKGROUND=255, XRANGE = [xmin, xmax], YRANGE=YRange, 	$
     ytitle=YTitle, xtitle='Local Zenith Angle (degree)', /NODATA
   OPLOT, bin_box, mirs, COLOR=color1, PSYM=-5, SYMSIZE=symsize
   yyyymmdd = strmid(date,0,4) + strmid(date,5,2) + strmid(date,8,2)
   map_name = figsDir + filePrex + yyyymmdd + '_' + prod + '_' + lndsea + '_' + cend + '.png'
   write_png, map_name, TVRD(), r, g, b
   device,/close
END

;===============================================================
; Name:		PLOT_HIST_4BIAS
;
;
; Type:		IDL Subroutine
;
;
; Description:  Plots histograms to assess MIRS Tb Correction
;
; History:
;       03-20-2009      Kevin Garrett, IMSG Inc @ NOAA/NESDIS/STAR
;
;===============================================================

PRO PLOT_HIST_4BIAS,x1,x2,x3,nbin,tit,xtit,ytit,M1,M2,M3,xmin,xmax,str1,str2,str3,chsz,map_name

  set_plot,'Z'
  device,set_resolution=[650, 500]
  r=bytarr(256) & g=bytarr(256) & b=bytarr(256)
  r(*)=255 & g(*)=255 & b(*)=255
   
  r(1)=0   & g(1)=0   & b(1)=0
  r(2)=255 & g(2)=127 & b(1)=0
  
  r(10)=0 & g(10)=0    & b(10)=0   ;black
  r(20)=0   & g(20)=0    & b(20)=255 ;blue
  r(30)=255   & g(30)=0    & b(30)=0   ;red
  r(40)=255  & g(40)=255  & b(40)=255 ; white
   
  TVLCT, r, g, b
  color1=10 & color2=20 &color3=30 &color4=40

  res1=histogram(x1,nbins=nbin,locations=loc1)
  res1=res1/float(max(res1))*100.
  ind=where(res1 ge 80.)
  M1=mean(loc1(ind))
  ymin1=min(res1)
  ymax1=max(res1)

  res2=histogram(x2,nbins=nbin,locations=loc2)
  res2=res2/float(max(res2))*100.
  ind=where(res2 ge 80.)
  M2=mean(loc2(ind))
  ymin2=min(res2)
  ymax2=max(res2)

  res3=histogram(x3,nbins=nbin,locations=loc3)
  res3=res3/float(max(res3))*100.
  ind=where(res3 ge 80.)
  M3=mean(loc3(ind))
  ymin3=min(res3)
  ymax3=max(res3)

  dumx=[1,2] & dumy=[1,2]
  PLOT, dumx, dumy, CHARSIZE=chsz, TITLE=tit,$
    COLOR=1, BACKGROUND=255, XRANGE = [xmin, xmax], YRANGE=[ymin1,ymax1], 	$
    ytitle=ytit, xtitle=xtit, xstyle=1,ystyle=1,/NODATA

  oplot,loc1,res1,color=color3,linestyle=0,thick=3
  plots,[M1,M1],[ymin1,ymax1],color=color3,linestyle=0,thick=1
  
  oplot,loc2,res2,color=color3,linestyle=2,thick=3
  plots,[M2,M2],[ymin2,ymax2],color=color3,linestyle=2,thick=1
  
  oplot,loc3,res3,color=color1,linestyle=2,thick=3
  plots,[M3,M3],[ymin3,ymax3],color=color1,linestyle=2,thick=1

  plots,[0,0],[0,100],thick=3,color=color1

  xline1=0.18 & yline1=0.01
  xline2=0.43 & yline2=0.01
  xline3=0.70 & yline3=0.01

 
  xyouts, xline1, yline1, "___ " + str1, color=color3, CHARSIZE=0.9, CHARTHICK=0.8, /normal
  xyouts, xline2, yline2, "_ _ " + str2, color=color3, CHARSIZE=0.9, CHARTHICK=0.8, /normal
  xyouts, xline3, yline3, "_ _ " + str3, color=color1, CHARSIZE=0.9, CHARTHICK=0.8, /normal

  write_png, map_name, TVRD(), r, g, b
  device,/close

END


;===============================================================
; Name:		PLOT_HIST_4BIAS_BLANK
;
;
; Type:		IDL Subroutine
;
;
; Description:  Plots a dummy blank image
;
; History:
;       03-20-2009      Kevin Garrett, IMSG Inc @ NOAA/NESDIS/STAR
;       11-20-2010      Wanchun Chen,  DELL Inc @ NOAA/NESDIS/STAR
;
;===============================================================

PRO PLOT_HIST_4BIAS_BLANK,x1,x2,x3,nbin,tit,xtit,ytit,M1,M2,M3,xmin,xmax,str1,str2,str3,chsz,map_name

  set_plot,'Z'
  device,set_resolution=[650, 500]
  r=bytarr(256) & g=bytarr(256) & b=bytarr(256)
  r(*)=255 & g(*)=255 & b(*)=255
   
  r(1)=0   & g(1)=0   & b(1)=0
  r(2)=255 & g(2)=127 & b(1)=0
  
  r(10)=0   & g(10)=0    & b(10)=0   ; black
  r(20)=0   & g(20)=0    & b(20)=255 ; blue
  r(30)=255 & g(30)=0    & b(30)=0   ; red
  r(40)=255 & g(40)=255  & b(40)=255 ; white
   
  TVLCT, r, g, b
  color1=10 & color2=20 &color3=30 &color4=40

  dumx=[1,2] & dumy=[1,2]
  PLOT, dumx, dumy, CHARSIZE=chsz, TITLE=tit,$
    COLOR=1, BACKGROUND=255, XRANGE = [xmin, xmax], YRANGE=[-10,10], 	$
    ytitle=ytit, xtitle=xtit, xstyle=1,ystyle=1,/NODATA
  
  xline1=0.18 & yline1=0.01
  xline2=0.43 & yline2=0.01
  xline3=0.70 & yline3=0.01

  xyouts, xline2, 0.4, 'NO DATA', color=color1, CHARSIZE=3, /normal

  xyouts, xline1, yline1, "___ " + str1, color=color3, CHARSIZE=0.9, CHARTHICK=0.8, /normal
  xyouts, xline2, yline2, "_ _ " + str2, color=color3, CHARSIZE=0.9, CHARTHICK=0.8, /normal
  xyouts, xline3, yline3, "_ _ " + str3, color=color1, CHARSIZE=0.9, CHARTHICK=0.8, /normal

  write_png, map_name, TVRD(), r, g, b
  device,/close

END


;===================================================================================================
; Name:		PLOT_LINE_4BIAS
;
;
; Type:		IDL Subroutine
;
;
; Description:  Plots 3 lines
;
; Arguments:
;
;      Name		    	Type	    	Description
;      ---------------------------------------------------
;	- xval			I		x-direction values
;	- y1val	               	I   		y-direction values 1         
;	- y2val	               	I   		y-direction values 2        
;       - y3val	               	I   		y-direction values 3   
;       - y4val	               	I   		y-direction values 4 
;	- title			I		title
;	- xtitle		I		x title
;	- ytitle		I		y title
;	- xrange		I		x range
;	- yrange		I		y range
;	- map_name		I		map name
;	- str1			I		line 1 legend
;	- str2			I		line 2 legend
;	- str3			I		line 3 legend
;       - str4                  I               line 4 legend
;
; Subroutines needed:
;       - None
;
;
; History:
;       04-24-2008     Kevin Garrett
;
;
;===================================================================================================
PRO PLOT_LINES_4BIAS, xval, y1val, y2val, y3val, y4val, y5val, xtitle, ytitle, title, xrange, yrange, map_name, $
	str1, str2, str3, str4, str5, lines2plot
   set_plot,'Z'
   device,set_resolution=[650, 500]
   r=bytarr(256) & g=bytarr(256) & b=bytarr(256)
   r(*)=255 & g(*)=255 & b(*)=255
   
   r(1)=0   & g(1)=0   & b(1)=0
   r(2)=255 & g(2)=127 & b(1)=0
   
   r(50)=255 & g(50)=0    & b(50)=0   ;red
   r(60)=0   & g(60)=0    & b(60)=255 ;blue
   r(70)=47 & g(70)=200  & b(70)=47   ;dark green
   r(80)=0   & g(80)=255  & b(80)=0   ;green
   r(90)=0   & g(90)=0    & b(90)=0   ;black

   r(100)=255  & g(100)=255  & b(100)=255
   
   TVLCT, r, g, b
   color1=50 & color2=60 &color3=70 &color4=80 &color5=90
   
   y1idx = where(y1val gt -9999)
   y2idx = where(y2val gt -9999)
   y3idx = where(y3val gt -9999)
   y4idx = where(y4val gt -9999)
   y5idx = where(y5val gt -9999)

   !P.REGION = [0.01, 0.01, 0.95, 0.95]
   symsize = 0.8
   dumx=[1,2] & dumy=[1,2]
   xmin=xrange(0)  & xmax=xrange(1)
   PLOT, dumx, dumy, XTITLE=xtitle, YTITLE=ytitle, TITLE=title,XRANGE=xrange, YRANGE=YRange, $
    	COLOR=1, BACKGROUND=255, CHARSIZE=0.8, /NODATA, XSTYLE=1, YSTYLE=1, yticks=yticks;, ytickname=ytickname, 
   
   if ( N_elements(y1idx) gt 1 and lines2plot(0) eq 1) then OPLOT, xval(y1idx), y1val(y1idx), COLOR=color1, LINESTYLE=0, THICK=3, SYMSIZE=symsize
   if ( N_elements(y1idx) le 1 and lines2plot(0) eq 1) then OPLOT, xval, y1val, COLOR=100, LINESTYLE=0, THICK=3, SYMSIZE=symsize
   
   if ( N_elements(y2idx) gt 1 and lines2plot(1) eq 1) then OPLOT, xval(y2idx), y2val(y2idx), COLOR=color1, LINESTYLE=2, THICK=3, SYMSIZE=symsize
   if ( N_elements(y2idx) le 1 and lines2plot(1) eq 1) then OPLOT, xval, y2val, COLOR=100, LINESTYLE=2, THICK=3, SYMSIZE=symsize
    
   if ( N_elements(y3idx) gt 1 and lines2plot(2) eq 1) then OPLOT, xval(y3idx), y3val(y3idx), COLOR=color1, LINESTYLE=1, THICK=3, SYMSIZE=symsize
   if ( N_elements(y3idx) le 1 and lines2plot(2) eq 1) then OPLOT, xval, y3val, COLOR=100, LINESTYLE=1, THICK=3, SYMSIZE=symsize

   if ( N_elements(y4idx) gt 1 and lines2plot(3) eq 1) then OPLOT, xval(y4idx), y4val(y4idx), COLOR=color5, LINESTYLE=0, THICK=3, SYMSIZE=symsize
   if ( N_elements(y4idx) le 1 and lines2plot(3) eq 1) then OPLOT, xval, y4val, COLOR=100, LINESTYLE=0, THICK=3, SYMSIZE=symsize
   
   if ( N_elements(y5idx) gt 1 and lines2plot(4) eq 1) then OPLOT, xval(y5idx), y5val(y5idx), COLOR=color5, LINESTYLE=2, THICK=3, SYMSIZE=symsize
   if ( N_elements(y5idx) le 1 and lines2plot(4) eq 1) then OPLOT, xval, y5val, COLOR=100, LINESTYLE=2, THICK=3, SYMSIZE=symsize
   

   xline1=0.18 & yline1=0.01
   xline2=0.30 & yline2=0.01
   xline3=0.44 & yline3=0.01
   xline4=0.67 & yline4=0.01
   xline5=0.79 & yline5=0.01

   xyouts, 0.08, yline1, 'Residual:', color=color5, CHARSIZE=1.0, CHARTHICK=0.8, /normal
   xyouts, 0.61, yline1, 'Bias:', color=color5, CHARSIZE=1.0, CHARTHICK=0.8, /normal

   xyouts, xline1, yline1, "___ " + str1, color=color1, CHARSIZE=0.9, CHARTHICK=0.8, /normal
   xyouts, xline2, yline2, "_ _ " + str2, color=color1, CHARSIZE=0.9, CHARTHICK=0.8, /normal
   xyouts, xline3, yline3, ".... " + str3, color=color1, CHARSIZE=0.9, CHARTHICK=0.8, /normal
   xyouts, xline4, yline4, "___ " + str4, color=color5, CHARSIZE=0.9, CHARTHICK=0.8, /normal
   xyouts, xline5, yline5, "_ _ " + str5, color=color5, CHARSIZE=0.9, CHARTHICK=0.8, /normal

   write_png, map_name, TVRD(), r, g, b
   device,/close
END

;===================================================================================================
; Name:		PLOT_LINE
;
;
; Type:		IDL Subroutine
;
;
; Description:  Plots 1 line
;
; Arguments:
;
;      Name		    	Type	    	Description
;      ---------------------------------------------------
;	- xval			I		x-direction values
;	- yval	               	I   		y-direction values         
;	- title			I		title
;	- xtitle		I		x title
;	- ytitle		I		y title
;	- xrange		I		x range
;	- yrange		I		y range
;	- map_name		I		map name
;
; Subroutines needed:
;       - None
;
;
; History:
;       09-22-2007      Wanchun Chen
;
;
;===================================================================================================
PRO plot_line, xval, yval, xtitle, ytitle, title, xrange, yrange, map_name, stdv=stdv
   set_plot,'Z'
   device,set_resolution=[650, 500]
   r=bytarr(256) & g=bytarr(256) & b=bytarr(256)
   r(*)=255 & g(*)=255 & b(*)=255
   r(143)=0 & g(143)=255 & b(143)=0
   r(243)=255 & g(243)=0 & b(243)=0
   r(63)=0 & g(63)=0 & b(63)=255
   r(2)=255 & g(2)=127 & b(1)=0
   r(1)=0 & g(1)=0 & b(1)=0
   TVLCT, r, g, b
   color1=63 & color2=243 &color3=2 & color5=1
   !P.REGION = [0.05, 0.05, 0.95, 0.95]
   symsize = 0.8
   dumx=[1,2] & dumy=[1,2]
   ss = where(FIX(yval) eq -999, count)  &  if( count GT 0 ) then yval(ss) = !VALUES.F_NAN
   
   len = ( yrange(1) - yrange(0) ) * 0.05
   xmin=xrange(0)  & xmax=xrange(1)
   PLOT, dumx, dumy, XTITLE=xtitle, YTITLE=ytitle, TITLE=title,XRANGE=xrange, YRANGE=YRange, $
    	COLOR=1, BACKGROUND=255, CHARSIZE=0.8, /NODATA, YSTYLE=1, XSTYLE=1
   OPLOT, xval, yval, COLOR=color1;, PSYM=-5, SYMSIZE=symsize
   
   N = N_ELEMENTS(stdv)
   IF N GT 1 THEN BEGIN
       for i = 0, N-1 do begin
           xi = xval(i)
	   ymin = yval(i) - stdv(i)
	   ymax = yval(i) + stdv(i)
	   oplot, [xi,xi], [ymin, ymax], linestyle=0, color=1
       endfor
   ENDIF
   
   write_png, map_name, TVRD(), r, g, b
   device,/close
END



;===================================================================================================
; Name:		PLOT_LINE2
;
;
; Type:		IDL Subroutine
;
;
; Description:  Plots 2 lines
;
; Arguments:
;
;      Name		    	Type	    	Description
;      ---------------------------------------------------
;	- xval			I		x-direction values
;	- y1val	               	I   		y-direction values 1         
;	- y2val	               	I   		y-direction values 2        
;	- title			I		title
;	- xtitle		I		x title
;	- ytitle		I		y title
;	- xrange		I		x range
;	- yrange		I		y range
;	- map_name		I		map name
;	- str1			I		line 1 legend
;	- str2			I		line 2 legend
;
; Subroutines needed:
;       - None
;
;
; History:
;       09-22-2007      Wanchun Chen
;
;
;===================================================================================================
PRO PLOT_LINE2, xval, y1val, y2val, xtitle, ytitle, title, xrange, yrange, map_name, str1, str2, stdv1=stdv1, stdv2=stdv2
   set_plot,'Z'
   device,set_resolution=[650, 500]
   r=bytarr(256) & g=bytarr(256) & b=bytarr(256)
   r(*)=255 & g(*)=255 & b(*)=255
   
   r(1)=0 & g(1)=0 & b(1)=0
   r(2)=255 & g(2)=127 & b(1)=0
   
   r(50)=255 & g(50)=0   & b(50)=0
   r(60)=0   & g(60)=0   & b(60)=255
   r(70)=0   & g(70)=255 & b(70)=0
   r(80)=255 & g(80)=255 & b(80)=0
   
   TVLCT, r, g, b
   color1=50 & color2=60 &color3=70 & color4=80
   
   !P.REGION = [0.05, 0.05, 0.95, 0.95]
   symsize = 0.8
   dumx=[1,2] & dumy=[1,2]
   xmin=xrange(0)  & xmax=xrange(1)
   
   ss = where(FIX(y1val) eq -999, count)  &  if( count GT 0 ) then y1val(ss) = !VALUES.F_NAN 
   ss = where(FIX(y2val) eq -999, count)  &  if( count GT 0 ) then y2val(ss) = !VALUES.F_NAN 
   
   PLOT, dumx, dumy, XTITLE=xtitle, YTITLE=ytitle, TITLE=title,XRANGE=xrange, YRANGE=YRange, $
    	COLOR=1, BACKGROUND=255, CHARSIZE=0.8, /NODATA, YSTYLE=1, XSTYLE=1
   OPLOT, xval, y1val, COLOR=color1 ; , PSYM=-5, SYMSIZE=symsize
   OPLOT, xval, y2val, COLOR=color2 ; , PSYM=-5, SYMSIZE=symsize
   
   xline1=0.30 & yline1=0.02
   xline2=0.60 & yline2=0.02

   xyouts, xline1, yline1, "___ " + str1, color=color1, CHARSIZE=1.5, CHARTHICK=0.8, /normal
   xyouts, xline2, yline2, "___ " + str2, color=color2, CHARSIZE=1.5, CHARTHICK=0.8, /normal
   
   len = ( yrange(1) - yrange(0) ) * 0.05
   xlen = xrange(1) - xrange(0)
   
   N = N_ELEMENTS(stdv1)
   IF N GT 1 THEN BEGIN
       for i = 0, N-1 do begin
           xi = xval(i)
	   ymin = y1val(i) - stdv1(i)
	   ymax = y1val(i) + stdv1(i)
	   oplot, [xi,xi], [ymin, ymax], linestyle=0, color=color1
       endfor
   ENDIF
   
   N = N_ELEMENTS(stdv2)
   IF N GT 1 THEN BEGIN
       for i = 0, N-1 do begin
           ;---- for scan angle, xlen is 120, we shift 1; for scan position, xlen is 29, we shift 0.24
           xi = xval(i) + 1.0*xlen/120.0  
	   ymin = y2val(i) - stdv2(i)
	   ymax = y2val(i) + stdv2(i)
	   oplot, [xi,xi], [ymin, ymax], linestyle=0, color=color2
       endfor
   ENDIF

   write_png, map_name, TVRD(), r, g, b
   device,/close
END

;===================================================================================================
; Name:		PLOT_LINE3
;
;
; Type:		IDL Subroutine
;
;
; Description:  Plots 3 lines
;
; Arguments:
;
;      Name		    	Type	    	Description
;      ---------------------------------------------------
;	- xval			I		x-direction values
;	- y1val	               	I   		y-direction values 1         
;	- y2val	               	I   		y-direction values 2        
;	- y3val	               	I   		y-direction values 3            
;	- title			I		title
;	- xtitle		I		x title
;	- ytitle		I		y title
;	- xrange		I		x range
;	- yrange		I		y range
;	- map_name		I		map name
;	- str1			I		line 1 legend
;	- str2			I		line 2 legend
;	- str3			I		line 3 legend
;
; Subroutines needed:
;       - None
;
;
; History:
;       04-24-2008     Kevin Garrett
;
;
;===================================================================================================
PRO PLOT_LINE3, x1val, x2val, x3val, yval, xtitle, ytitle, title, xrange, yrange, map_name, $
	str1, str2, str3, isMeanOrNot, ytickname, yticks, TQ
   set_plot,'Z'
   device,set_resolution=[650, 500]
   r=bytarr(256) & g=bytarr(256) & b=bytarr(256)
   r(*)=255 & g(*)=255 & b(*)=255
   
   r(1)=0   & g(1)=0   & b(1)=0
   r(2)=255 & g(2)=127 & b(1)=0
   
   r(50)=255 & g(50)=0    & b(50)=0
   r(60)=0   & g(60)=0    & b(60)=255
   r(70)=0   & g(70)=255  & b(70)=0
   
   r(100)=255  & g(100)=255  & b(100)=255
   
   TVLCT, r, g, b
   color1=50 & color2=60 &color3=70
   
   x1idx = where(x1val gt -9999)
   x2idx = where(x2val gt -9999)
   x3idx = where(x3val gt -9999)

   !P.REGION = [0.05, 0.05, 0.95, 0.95]
   symsize = 0.8
   dumx=[1,2] & dumy=[1,2]
   xmin=xrange(0)  & xmax=xrange(1)
   PLOT, dumx, dumy, XTITLE=xtitle, YTITLE=ytitle, TITLE=title,XRANGE=xrange, YRANGE=YRange, $
    	COLOR=1, BACKGROUND=255, CHARSIZE=0.8, /NODATA, /YLOG, XSTYLE=1, YSTYLE=1, yticks=yticks;, ytickname=ytickname, 
   
   if ( N_elements(x1idx) gt 1 ) then OPLOT, x1val(x1idx), yval(x1idx), COLOR=color1, PSYM=-5, SYMSIZE=symsize
   if ( N_elements(x1idx) le 1 ) then OPLOT, x1val, yval, COLOR=100, PSYM=-5, SYMSIZE=symsize
   
   if ( N_elements(x2idx) gt 1 ) then OPLOT, x2val(x2idx), yval(x2idx), COLOR=color2, PSYM=-5, SYMSIZE=symsize
   if ( N_elements(x2idx) le 1 ) then OPLOT, x2val, yval, COLOR=100, PSYM=-5, SYMSIZE=symsize
    
   if ( N_elements(x3idx) gt 1 ) then OPLOT, x3val(x3idx), yval(x3idx), COLOR=color3, PSYM=-5, SYMSIZE=symsize
   if ( N_elements(x3idx) le 1 ) then OPLOT, x3val, yval, COLOR=100, PSYM=-5, SYMSIZE=symsize
   
   if isMeanOrNot eq 1 then OPLOT, [0,0], YRange, color=1, LINESTYLE=2

   xline1=0.25 & yline1=0.02
   xline2=0.45 & yline2=0.02
   xline3=0.65 & yline3=0.02

   xyouts, xline1, yline1, "__ " + str1, color=color1, CHARSIZE=1.0, CHARTHICK=0.8, /normal
   xyouts, xline2, yline2, "__ " + str2, color=color2, CHARSIZE=1.0, CHARTHICK=0.8, /normal
   xyouts, xline3, yline3, "__ " + str3, color=color3, CHARSIZE=1.0, CHARTHICK=0.8, /normal
   
   ;---- plot IORD-II requirements(TQ=0 -- T, TQ=1 --> Q )
   linesty=1
   colr=1
   thck=2
   
   if TQ eq 0 and isMeanOrNot eq 0 then begin
    	plots,[2.5,2.5],[950,700],linestyle=linesty,color=colr,thick=thck
    	plots,[1.5,2.5],[700,700],linestyle=linesty,color=colr,thick=thck
    	plots,[1.5,1.5],[700,100],linestyle=linesty,color=colr,thick=thck
    	;plots,[1.5,1.5],[300,30], linestyle=linesty,color=colr,thick=thck
    	;plots,[1.5,1.5],[30,10],  linestyle=linesty,color=colr,thick=thck
   endif
  
   if TQ eq 1 and isMeanOrNot eq 0 then begin
    	plots,[20,20],[950,600],linestyle=linesty,color=colr,thick=thck
    	plots,[20,40],[600,600],linestyle=linesty,color=colr,thick=thck
    	plots,[40,40],[600,200],linestyle=linesty,color=colr,thick=thck
    	;plots,[40,40],[400,100],linestyle=linesty,color=colr,thick=thck
   endif

   write_png, map_name, TVRD(), r, g, b
   device,/close
END


;===================================================================================================
; Name:		PLOT_LINE4
;
;
; Type:		IDL Subroutine
;
;
; Description:  Plots 4 lines
;
; Arguments:
;
;      Name		    	Type	    	Description
;      ---------------------------------------------------
;	- xval			I		x-direction values
;	- y1val	               	I   		y-direction values 1         
;	- y2val	               	I   		y-direction values 2        
;	- y3val	               	I   		y-direction values 3         
;	- y4val	               	I   		y-direction values 4        
;	- title			I		title
;	- xtitle		I		x title
;	- ytitle		I		y title
;	- xrange		I		x range
;	- yrange		I		y range
;	- map_name		I		map name
;	- str1			I		line 1 legend
;	- str2			I		line 2 legend
;	- str3			I		line 3 legend
;	- str4			I		line 4 legend
;
; Subroutines needed:
;       - None
;
;
; History:
;       09-22-2007      Wanchun Chen
;
;
;===================================================================================================
PRO PLOT_LINE4, x1val, x2val, x3val, x4val, yval, xtitle, ytitle, title, xrange, yrange, map_name, $
	str1, str2, str3, str4, isMeanOrNot, ytickname, yticks, TQ
   set_plot,'Z'
   device,set_resolution=[650, 500]
   r=bytarr(256) & g=bytarr(256) & b=bytarr(256)
   r(*)=255 & g(*)=255 & b(*)=255
   
   r(1)=0   & g(1)=0   & b(1)=0
   r(2)=255 & g(2)=127 & b(1)=0
   
   r(50)=255 & g(50)=0    & b(50)=0
   r(60)=0   & g(60)=0    & b(60)=255
   r(70)=0   & g(70)=255  & b(70)=0
   r(80)=0   & g(80)=255  & b(80)=255
   
   r(100)=255  & g(100)=255  & b(100)=255
   
   TVLCT, r, g, b
   color1=50 & color2=60 &color3=70 & color4=80
   
   x1idx = where(x1val gt -9999)
   x2idx = where(x2val gt -9999)
   x3idx = where(x3val gt -9999)
   x4idx = where(x4val gt -9999)

   !P.REGION = [0.05, 0.05, 0.95, 0.95]
   symsize = 0.8
   dumx=[1,2] & dumy=[1,2]
   xmin=xrange(0)  & xmax=xrange(1)
   PLOT, dumx, dumy, XTITLE=xtitle, YTITLE=ytitle, TITLE=title,XRANGE=xrange, YRANGE=YRange, $
    	COLOR=1, BACKGROUND=255, CHARSIZE=0.8, /NODATA, /YLOG, XSTYLE=1, YSTYLE=1, yticks=yticks;, ytickname=ytickname, 
   
   if ( N_elements(x1idx) gt 1 ) then OPLOT, x1val(x1idx), yval(x1idx), COLOR=color1, PSYM=-5, SYMSIZE=symsize
   if ( N_elements(x1idx) le 1 ) then OPLOT, x1val, yval, COLOR=100, PSYM=-5, SYMSIZE=symsize
   
   if ( N_elements(x2idx) gt 1 ) then OPLOT, x2val(x2idx), yval(x2idx), COLOR=color2, PSYM=-5, SYMSIZE=symsize
   if ( N_elements(x2idx) le 1 ) then OPLOT, x2val, yval, COLOR=100, PSYM=-5, SYMSIZE=symsize
    
   if ( N_elements(x3idx) gt 1 ) then OPLOT, x3val(x3idx), yval(x3idx), COLOR=color3, PSYM=-5, SYMSIZE=symsize
   if ( N_elements(x3idx) le 1 ) then OPLOT, x3val, yval, COLOR=100, PSYM=-5, SYMSIZE=symsize
   
   if ( N_elements(x4idx) gt 1 ) then OPLOT, x4val(x4idx), yval(x4idx), COLOR=color4, PSYM=-5, SYMSIZE=symsize
   if ( N_elements(x4idx) le 1 ) then OPLOT, x4val, yval, COLOR=100, PSYM=-5, SYMSIZE=symsize
   
   if isMeanOrNot eq 1 then OPLOT, [0,0], YRange, color=1, LINESTYLE=2
   
   xline1=0.20 & yline1=0.02
   xline2=0.40 & yline2=0.02
   xline3=0.60 & yline3=0.02
   xline4=0.80 & yline4=0.02

   xyouts, xline1, yline1, "__ " + str1, color=color1, CHARSIZE=1.0, CHARTHICK=0.8, /normal
   xyouts, xline2, yline2, "__ " + str2, color=color2, CHARSIZE=1.0, CHARTHICK=0.8, /normal
   xyouts, xline3, yline3, "__ " + str3, color=color3, CHARSIZE=1.0, CHARTHICK=0.8, /normal
   xyouts, xline4, yline4, "__ " + str4, color=color4, CHARSIZE=1.0, CHARTHICK=0.8, /normal
   
   ;---- plot IORD-II requirements(TQ=0 -- T, TQ=1 --> Q )
   linesty=1
   colr=1
   thck=2
   
   if TQ eq 0 and isMeanOrNot eq 0 then begin
    	plots,[2.5,2.5],[950,700],linestyle=linesty,color=colr,thick=thck
    	plots,[1.5,2.5],[700,700],linestyle=linesty,color=colr,thick=thck
    	plots,[1.5,1.5],[700,100],linestyle=linesty,color=colr,thick=thck
    	;plots,[1.5,1.5],[300,30], linestyle=linesty,color=colr,thick=thck
    	;plots,[1.5,1.5],[30,10],  linestyle=linesty,color=colr,thick=thck
   endif
  
   if TQ eq 1 and isMeanOrNot eq 0 then begin
    	plots,[20,20],[950,600],linestyle=linesty,color=colr,thick=thck
    	plots,[20,40],[600,600],linestyle=linesty,color=colr,thick=thck
    	plots,[40,40],[600,200],linestyle=linesty,color=colr,thick=thck
    	;plots,[40,40],[400,100],linestyle=linesty,color=colr,thick=thck
   endif

   write_png, map_name, TVRD(), r, g, b
   device,/close
END

;===================================================================================================
; Name:		PLOT_LINE5
;
;
; Type:		IDL Subroutine
;
;
; Description:  Plots 5 lines
;
; Arguments:
;
;      Name		    	Type	    	Description
;      ---------------------------------------------------
;	- xval			I		x-direction values
;	- y1val	               	I   		y-direction values 1         
;	- y2val	               	I   		y-direction values 2        
;	- y3val	               	I   		y-direction values 3         
;	- y4val	               	I   		y-direction values 4        
;	- y5val	               	I   		y-direction values 5      
;	- title			I		title
;	- xtitle		I		x title
;	- ytitle		I		y title
;	- xrange		I		x range
;	- yrange		I		y range
;	- map_name		I		map name
;	- str1			I		line 1 legend
;	- str2			I		line 2 legend
;	- str3			I		line 3 legend
;	- str4			I		line 4 legend
;	- str5			I		line 5 legend
;
; Subroutines needed:
;       - None
;
;
; History:
;       09-22-2007      Wanchun Chen
;       03-22-2008      Kevin Garrett: Modified plot_line4 to plot one
;       more parameter
;
;
;===================================================================================================
PRO PLOT_LINE5, x1val, x2val, x3val, x4val, x5val, yval, xtitle, ytitle, title, xrange, yrange, map_name, $
	str1, str2, str3, str4, str5, isMeanOrNot, ytickname, yticks, TQ
   set_plot,'Z'
   device,set_resolution=[650, 500]
   r=bytarr(256) & g=bytarr(256) & b=bytarr(256)
   r(*)=255 & g(*)=255 & b(*)=255
   
   r(1)=0   & g(1)=0   & b(1)=0
   r(2)=255 & g(2)=127 & b(1)=0
   
   r(50)=255 & g(50)=0    & b(50)=0
   r(60)=0   & g(60)=0    & b(60)=255
   r(70)=0   & g(70)=255  & b(70)=0
   r(80)=0   & g(80)=255  & b(80)=255
   r(90)=0   & g(90)=0    & b(90)=0
   
   r(100)=255  & g(100)=255  & b(100)=255
   
   TVLCT, r, g, b
   color1=50 & color2=60 &color3=70 & color4=80 & color5=90
   
   x1idx = where(x1val gt -9999)
   x2idx = where(x2val gt -9999)
   x3idx = where(x3val gt -9999)
   x4idx = where(x4val gt -9999)
   x5idx = where(x5val gt -9999)

   !P.REGION = [0.05, 0.05, 0.95, 0.95]
   symsize = 0.8
   dumx=[1,2] & dumy=[1,2]
   xmin=xrange(0)  & xmax=xrange(1)
   PLOT, dumx, dumy, XTITLE=xtitle, YTITLE=ytitle, TITLE=title,XRANGE=xrange, YRANGE=YRange, $
    	COLOR=1, BACKGROUND=255, CHARSIZE=0.8, /NODATA, /YLOG, XSTYLE=1, YSTYLE=1, yticks=yticks;, ytickname=ytickname, 
   
   if ( N_elements(x1idx) gt 1 ) then OPLOT, x1val(x1idx), yval(x1idx), COLOR=color1, PSYM=-5, SYMSIZE=symsize
   if ( N_elements(x1idx) le 1 ) then OPLOT, x1val, yval, COLOR=100, PSYM=-5, SYMSIZE=symsize
   
   if ( N_elements(x2idx) gt 1 ) then OPLOT, x2val(x2idx), yval(x2idx), COLOR=color2, PSYM=-5, SYMSIZE=symsize
   if ( N_elements(x2idx) le 1 ) then OPLOT, x2val, yval, COLOR=100, PSYM=-5, SYMSIZE=symsize
    
   if ( N_elements(x3idx) gt 1 ) then OPLOT, x3val(x3idx), yval(x3idx), COLOR=color3, PSYM=-5, SYMSIZE=symsize
   if ( N_elements(x3idx) le 1 ) then OPLOT, x3val, yval, COLOR=100, PSYM=-5, SYMSIZE=symsize
   
   if ( N_elements(x4idx) gt 1 ) then OPLOT, x4val(x4idx), yval(x4idx), COLOR=color4, PSYM=-5, SYMSIZE=symsize
   if ( N_elements(x4idx) le 1 ) then OPLOT, x4val, yval, COLOR=100, PSYM=-5, SYMSIZE=symsize

   if ( N_elements(x5idx) gt 1 ) then OPLOT, x5val(x5idx), yval(x5idx), COLOR=color5, PSYM=-5, SYMSIZE=symsize
   if ( N_elements(x5idx) le 1 ) then OPLOT, x5val, yval, COLOR=100, PSYM=-5, SYMSIZE=symsize
   
   if isMeanOrNot eq 1 then OPLOT, [0,0], YRange, color=1, LINESTYLE=2
   
   xline1=0.05 & yline1=0.02
   xline2=0.25 & yline2=0.02
   xline3=0.43 & yline3=0.02
   xline4=0.60 & yline4=0.02
   xline5=0.85 & yline5=0.02

   xyouts, xline1, yline1, "__ " + str1, color=color1, CHARSIZE=1.0, CHARTHICK=0.8, /normal
   xyouts, xline2, yline2, "__ " + str2, color=color2, CHARSIZE=1.0, CHARTHICK=0.8, /normal
   xyouts, xline3, yline3, "__ " + str3, color=color3, CHARSIZE=1.0, CHARTHICK=0.8, /normal
   xyouts, xline4, yline4, "__ " + str4, color=color4, CHARSIZE=1.0, CHARTHICK=0.8, /normal
   xyouts, xline5, yline5, "__ " + str5, color=color5, CHARSIZE=1.0, CHARTHICK=0.8, /normal

   ;---- plot IORD-II requirements(TQ=0 -- T, TQ=1 --> Q )
   linesty=1
   colr=1
   thck=2
   
   if TQ eq 0 and isMeanOrNot eq 0 then begin
    	plots,[2.5,2.5],[950,700],linestyle=linesty,color=colr,thick=thck
    	plots,[1.5,2.5],[700,700],linestyle=linesty,color=colr,thick=thck
    	plots,[1.5,1.5],[700,100],linestyle=linesty,color=colr,thick=thck
    	;plots,[1.5,1.5],[300,30], linestyle=linesty,color=colr,thick=thck
    	;plots,[1.5,1.5],[30,10],  linestyle=linesty,color=colr,thick=thck
   endif
  
   if TQ eq 1 and isMeanOrNot eq 0 then begin
    	plots,[20,20],[950,600],linestyle=linesty,color=colr,thick=thck
    	plots,[20,40],[600,600],linestyle=linesty,color=colr,thick=thck
    	plots,[40,40],[600,200],linestyle=linesty,color=colr,thick=thck
    	;plots,[40,40],[400,100],linestyle=linesty,color=colr,thick=thck
   endif

   write_png, map_name, TVRD(), r, g, b
   device,/close
END


;===============================================================
; Name:		AddClrReqQ, AddCldReqQ,AddClrReqT,AddCldReqT  
;
;
; Type:		IDL Subroutine
;
;
; Description:  The following series of subroutines aims
;               at overplotting the NPOESS IORD requirements
;               for sounding temperature and humidity. The
;               requirements are different for cloudy (cld)
;               and clear (clr) conditions.
;
;
; Arguments:
;
;      Name		    Type	    Description
;      ---------------------------------------------------
;	- linesty            I            Line style index to be used
;	- col                I            Color index to be used
;	- thck               I            Thickness to be used.
;
;
; Subroutines needed:
;       - None
;
;
; History:
;       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
;
;===============================================================

PRO AddClrReqQ,linesty,col,thck
    plots,[20,20],[1000,600],linestyle=linesty,color=col,thick=thck
    plots,[20,35],[600,600],linestyle=linesty,color=col,thick=thck
    plots,[35,35],[600,300],linestyle=linesty,color=col,thick=thck
    plots,[35,35],[300,100],linestyle=linesty,color=col,thick=thck
END


PRO AddCldReqQ,linesty,col,thck
    plots,[20,20],[1000,600],linestyle=linesty,color=col,thick=thck
    plots,[20,40],[600,600],linestyle=linesty,color=col,thick=thck
    plots,[40,40],[600,400],linestyle=linesty,color=col,thick=thck
    plots,[40,40],[400,100],linestyle=linesty,color=col,thick=thck
END


PRO AddClrReqT,linesty,col,thck
    plots,[1.6,1.6],[1000,300],linestyle=linesty,color=col,thick=thck,/data
    plots,[1.5,1.6],[300,300],linestyle=linesty,color=col,thick=thck,/data
    plots,[1.5,1.5],[300,30],linestyle=linesty,color=col,thick=thck,/data
    plots,[1.5,1.5],[30,10],linestyle=linesty,color=col,thick=thck,/data
END


PRO AddCldReqT,linesty,col,thck
    plots,[2.5,2.5],[1000,700],linestyle=linesty,color=col,thick=thck
    plots,[1.5,2.5],[700,700],linestyle=linesty,color=col,thick=thck
    plots,[1.5,1.5],[700,300],linestyle=linesty,color=col,thick=thck
    plots,[1.5,1.5],[300,100],linestyle=linesty,color=col,thick=thck
    ;plots,[1.5,1.5],[100,30],linestyle=linesty,color=col,thick=thck
    ;plots,[1.5,1.5],[30,10],linestyle=linesty,color=col,thick=thck
END


;===============================================================
; Name:		performVertAvg
;
;
; Type:		IDL Subroutine
;
;
; Description:  Performs vertical averaging of temperature and 
;               humidity, according to NPOESS IORD requirements.
;               Cloudy and Clear averaging are both generated by
;               this subroutine.
;
; Arguments:
;
;      Name		    Type    Description
;      ---------------------------------------------------
;	- Nlev               I      Number of levels
;	- Nlay               I      Number of layers
;	- PresLevVec         I      Level-based press grid
;	- PresLayVec         I      Layer-based press grid
;	- TempLayVec         I      Layer-based temperature profile
;	- H2O                I      Water vapor profile
;	- SfcP               I      Surface pressure
;	- Zsfc               I      Altitude at the surface
;	- Tavg_clr           O      Vert-averaged temperature (clr requirements)
;	- Tavg_cld           O      Vert-averaged temperature (cld requirements)
;	- Qavg_clr           O      Vert-averaged humidity (clr requirements)
;	- Qavg_cld           O      Vert-averaged humidity (clr requirements)
;
;
; Subroutines needed:
;       - None
;
;
; History:
;       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
;
;===============================================================

PRO performVertAvg,Nlev,Nlay,PresLevVec,PresLayVec,TempLayVec,H2O,SfcP,Zsfc,Tavg_clr,Tavg_cld,Qavg_clr,Qavg_cld
    vcs_clr_sfcTo_300mb = 1000.     ; in meters
    vcs_clr_300To_30mb  = 3000. ; in meters
    vcs_clr_30To_1mb    = 5000. ; in meters
    vcs_clr_1To_0_01mb  = 5000. ; in meters
    vcs_cld_sfcTo_700mb = 1000. ; in meters
    vcs_cld_700To_300mb = 1000. ; in meters
    vcs_cld_300To_30mb  = 3000. ; in meters
    vcs_cld_30To_1mb    = 5000. ; in meters
    vcs_cld_1To_0_01mb  = 5000. ; in meters
    Tavg_clr=make_array(nLay,/float,value=0.)
    Tavg_cld=make_array(nLay,/float,value=0.)
    Qavg_clr=make_array(nLay,/float,value=0.)
    Qavg_cld=make_array(nLay,/float,value=0.)
    compute_height,Nlev,Nlay,PresLevVec,TempLayVec,H2O,SfcP,Z,Zavg,Zsfc,K
    nLayEff=K
    for iLay=0,Nlay-1 do begin
        Zalt                          = Zavg(iLay)
        Pavg                          = PresLayVec(iLay)
        IF (Pavg ge 300)                 THEN vcs_clr = vcs_clr_sfcTo_300mb
        IF (Pavg lt 300 and Pavg ge 30)  THEN vcs_clr = vcs_clr_300To_30mb
        IF (Pavg lt 30  and Pavg ge 1)   THEN vcs_clr = vcs_clr_30To_1mb
        IF (Pavg lt 1 )                  THEN vcs_clr = vcs_clr_1To_0_01mb
        IF (Pavg ge 700)                 THEN vcs_cld = vcs_cld_sfcTo_700mb
        IF (Pavg lt 700 and Pavg ge 300) THEN vcs_cld = vcs_cld_700To_300mb
        IF (Pavg lt 300 and Pavg ge 30)  THEN vcs_cld = vcs_cld_300To_30mb
        IF (Pavg lt 30  and Pavg ge 1 )  THEN vcs_cld = vcs_cld_30To_1mb
        IF (Pavg lt 1 )                  THEN vcs_cld = vcs_cld_1To_0_01mb
        indZ_clr                      = where(Zavg ge (Zalt-(vcs_clr/2.)) and $
                                              Zavg le (Zalt+(vcs_clr/2.)) and $
                                              PresLayVec le SfcP          and $
                                              TempLayVec lt 700           and $
                                              TempLayVec gt 0,icount_clr)
        indZ_cld                      = where(Zavg ge (Zalt-(vcs_cld/2.)) and $
                                              Zavg le (Zalt+(vcs_cld/2.)) and $
                                              PresLayVec le SfcP          and $
                                              TempLayVec lt 700           and $
                                              TempLayVec gt 0,icount_cld)
        IF (icount_clr ne 0) THEN BEGIN
            Tavg_clr(iLay)                = mean(TempLayVec(indZ_clr))
            Qavg_clr(iLay)                = mean(H2O(indZ_clr))
        ENDIF
        IF (icount_clr eq 0) THEN BEGIN
            Tavg_clr(iLay)                = TempLayVec(iLay)
            Qavg_clr(iLay)                = H2O(iLay)
        ENDIF
        IF (icount_cld ne 0) THEN BEGIN
            Tavg_cld(iLay)                = mean(TempLayVec(indZ_cld))
            Qavg_cld(iLay)                = mean(H2O(indZ_cld))
        ENDIF
        IF (icount_cld eq 0) THEN BEGIN
            Tavg_cld(iLay)                = TempLayVec(iLay)
            Qavg_cld(iLay)                = H2O(iLay)
        ENDIF
    endfor
END

;===============================================================
; Name:		compute_height
;
;
; Type:		IDL Subroutine
;
;
; Description:  Computes the height profile from the temperature 
;               and water vapor profiles.
;
; Arguments:
;
;      Name	  Type	    Description
;      ---------------------------------------------------
;	- Nlev     I       Number of levels          
;	- Nlay     I       Number of layers
;	- Plev     I       Level-based pressure grid
;	- Tlay     I       Layer temperature profile
;	- H2O      I       Humidity profile
;	- Ps       I       Surface pressure
;	- Z        I       Height depth profile (for each layer)
;	- Zavg     O       Height profile (layer level)
;	- Zsfc     I       Altitude at surface
;	- K        O       Effective levels number
;
;
; Subroutines needed:
;       - None
;
;
; History:
;       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
;
;===============================================================

PRO compute_height,Nlev,Nlay,Plev,Tlay,H2O,Ps,Z,Zavg,Zsfc,K
    ZZ     = make_array(nLev,/float,value=0.)
    Z      = make_array(nLev,/float,value=0.)
    Zavg   = make_array(nLay,/float,value=0.)
    zz(0)  = Zsfc
    FOR i=0,Nlev-2 DO BEGIN
       if(Plev(i+1) le Ps) then begin
           PP=(Plev(i+1)+Plev(i))/2.0
           TT=TLay(i)
           dP=Plev(i+1)-Plev(i)
           dz=dP/PP*287.0*TT/9.8
           ZZ(i+1)=ZZ(i)+dz
       endif else begin 
           PP=(Ps+Plev(i))/2.0
           TT=Tlay(i)
           dP=Ps-Plev(i)
           dz=dP/PP*287.0*TT/9.8
           ZZ(i+1)=ZZ(i)+dz
           BREAK
       endelse
   endfor
   K=i+1
   if(K ge Nlev) then K=Nlev-1
   FOR i=0,K-1 DO BEGIN
       Z(i)=ZZ(K)-ZZ(i)
   endfor
   ;---compute mid-layer altitude
   FOR i=0,K-2 DO BEGIN
       Zavg(i)=(Z(i)+Z(i+1))/2.
   ENDFOR
   return
end 



;===============================================================
; Name:		compJulDay
;
;
; Type:		IDL Subroutine
;
; 
; Description:  Computes the Julian day from calendar date
;
;
; Arguments:
;
;      Name		    Type	    Description
;      ---------------------------------------------------
;	- year               I             Year
;	- month              I             Month
;	- day                I             Day
;	- jul_day            O             Julian day
;
;
; Subroutines needed:
;       - None
;
;
; History:
;       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
;
;===============================================================

PRO compJulDay,year,month,day,jul_day
    day1=[31,28,31,30,31,30,31,31,30,31,30,31]
    day2=[31,29,31,30,31,30,31,31,30,31,30,31]
    leap=0
    if ( ( year mod 4 eq 0 and year mod 100 ne 0 ) or ( year mod 400 eq 0 ) ) then leap = 1
    sum1=0
    FOR i=1,month-1 DO BEGIN
        if( leap eq 1) then begin
            sum1=sum1+day2[i-1]
        endif 
        if( leap eq 0) then begin
            sum1=sum1+day1[i-1]
        endif
    ENDFOR
    jul_day=sum1+day
END 

;===============================================================
; Name:		compCalDay
;
;
; Type:		IDL Subroutine
;
; 
; Description:  Computes the calendar date from the Julian day
;               Returns the date as a string 'YYYYMMDD'
;
;
; Arguments:
;
;      Name		    Type	    Description
;      ---------------------------------------------------
;	- jul_day            I             Julian day
;       - year               I             Year
;	- date               O             Calendar date
;
;
; Subroutines needed:
;       - None
;
;
; History:
;       08-29-2007      Kevin Garrett, IMSG Inc @ NOAA/NESDIS/ORA
;
;===============================================================

PRO compCalDay,jul_day,year,date

     cal_day = 0

     ;---Arrays for the months of the year 
     months       = ['01','02','03','04','05','06', $
                     '07','08','09','10','11','12']
     ;---Julian day of the first of every month - 1
     days_nonleap = [0,31,59,90,120,151,181,212,    $
                     243,273,304,334]
     days_leap    = [0,31,60,91,121,152,182,213,    $
                     244,274,305,335]
     ;---Determine if the year is a leap year
     if ( ( year mod 4 eq 0 and year mod 100 ne 0 ) or ( year mod 400 eq 0 ) ) then begin
         leap = 1 
     endif else begin
         leap = 0
     endelse
     ;---Calculate the calendar day
     if (leap eq 1) then begin 
         diff = jul_day - days_leap
         index   = max(where(diff ge 0)) 
         cal_day = diff[index]
     endif else begin
         diff    = jul_day - days_nonleap
         index   = max(where(diff ge 0)) 
         cal_day = diff[index]
     endelse
     if (cal_day eq 0) then begin
         cal_day = diff[index - 1]
         month   = months[index -1]
     endif else begin 
         month   = months[index]
     endelse
     ;---Return the string variable date
     cal_day = strcompress(string(cal_day), /remove_all)
     len = strlen(cal_day)
     if (len eq 1) then begin
         date = strcompress(string(year) + string(month) + '0' + cal_day,/remove_all)
     endif else begin
         date = strcompress(string(year) + string(month) + cal_day,/remove_all)
     endelse
END

;===============================================================
; Name:		VELOVECT_sid
;
;
; Type:		IDL Subroutine
;
;
; Description:  Displays the velocity field (wind vectors).
;
;
;
; History:
;       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
;
;===============================================================

PRO VELOVECT_sid,U,V,X,Y,X00,X11,Y00,Y11,nX,nY, Length = length, Dots = dots,  $
        CLIP=clip, NOCLIP=noclip, OVERPLOT=overplot, _REF_EXTRA=extra
        COMPILE_OPT strictarr
        on_error,2              ;Return to caller if an error occurs
        mag = sqrt(u^2.+v^2.)   ;magnitude.
        x0 = X00                ;get scaling
        x1 = X11
        y0 = Y00
        y1 = Y11
        x_step=(x1-x0)/(nX) ; Convert to float. Integer math
        y_step=(y1-y0)/(nY) ; could result in divide by 0
        maxmag=max([max(abs(u/x_step)),max(abs(v/y_step))])
        sina = length * (u/maxmag)
        cosa = length * (v/maxmag)
        if n_elements(title) le 0 then title = ''
        ;--------------  plot to get axes  ---------------
        if n_elements(noclip) eq 0 then noclip = 1
        x_b0=x0-x_step
        x_b1=x1+x_step
        y_b0=y0-y_step
        y_b1=y1+y_step
        if (not keyword_set(overplot)) then begin
            if n_elements(position) eq 0 then begin
                plot,[x_b0,x_b1],[y_b1,y_b0],/nodata,/xst,/yst,_EXTRA = extra
            endif else begin
                plot,[x_b0,x_b1],[y_b1,y_b0],/nodata,/xst,/yst, _EXTRA = extra
            endelse
        endif
        if n_elements(clip) eq 0 then clip = [!x.crange[0],!y.crange[0],!x.crange[1],!y.crange[1]]
        r = .3                  ;len of arrow head
        angle = 22.5 * !dtor    ;Angle of arrowhead
        st = r * sin(angle)     ;sin 22.5 degs * length of head
        ct = r * cos(angle)
        x0 = x
        dx = sina
        x1 = x0 + dx
        y0 = y
        dy = cosa
        y1 = y0 + dy
        xd=x_step
        yd=y_step
        plots,[x0,x1,x1-(ct*dx/xd-st*dy/yd)*xd, x1,x1-(ct*dx/xd+st*dy/yd)*xd], $
          [y0,y1,y1-(ct*dy/yd+st*dx/xd)*yd, y1,y1-(ct*dy/yd-st*dx/xd)*yd], $
          clip=clip,noclip=noclip,_EXTRA=extra
end





;===============================================================
; Name:		defineColors
;
;
; Type:		IDL Subroutine
;
;
; Description:  Defines the r,g,b, white, black colors
;
;
; Arguments:
;
;      Name		    Type	    Description
;      ---------------------------------------------------
;	- colors             O              Colors structure
;       - nc                 O              Number of effective colors 
;
;
; Subroutines needed:
;       - None
;
;
; History:
;       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
;
;===============================================================

PRO defineColors,colors,nc
  colors={red:0,green:0,blue:0,white:0,black:0}
  rc_red= [255, 0, 0, 255, 0]
  rc_green = [0, 255, 0, 255, 0]
  rc_blue=[0,0,255,255,0]
  nc=!d.table_size-n_elements(rc_red)
  tvlct,r,g,b,/get
  r=congrid(r,nc)
  g=congrid(g,nc)
  b=congrid(b,nc)
  tvlct,r,g,b
  tvlct,rc_red,rc_green,rc_blue,nc
  for i=0,n_tags(colors)-1 do begin
      colors.(i)=nc+i
  endfor
END


;===============================================================
; Name:		captureInAnyFormat
;
;
; Type:		IDL Subroutine
;
;
; Description:  Captures the content of a window and put it in a file
;               using different formats.
;
;
; Arguments:
;
;      Name	    Type       Description
;      ---------------------------------------------------
;	- wid        I         Window ID (which we want to capture)
;	- file       I         File name (where we want to put the plot)
;	- format     I         'bmp','gif','jpeg','png','ppm','srf','tiff'
;
;
; Subroutines needed:
;       - None
;
;
; History:
;       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
;
;===============================================================

PRO captureInAnyFormat,wid,file,format
  wset,wid
  IF (!d.window eq -1) then begin
      print,'Error in wid'
      return
  ENDIF
  widget_control,/hourglass
  tvlct,r,g,b,/get
  device,get_visual_name=ThisvisualClass
  IF (ThisvisualClass eq 'PseudoColor') then a=tvrd()
  IF (ThisvisualClass ne 'PseudoColor') then a=tvrd(true=3)
  IF (ThisvisualClass eq 'PseudoColor') then write_image,file,format,a,r,g,b,/append
  IF (ThisvisualClass ne 'PseudoColor') then begin
      aa=transpose(a,[2,0,1])
      write_image,file,format,aa,/append
  ENDIF
  return
END

;===============================================================
; Name:		sectionDisplay
;
;
; Type:		IDL Subroutine
;
;
; Description:  Displays a vertical cross-section of a 3D field.
;
;
; History:
;       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
;
;===============================================================

PRO sectionDisplay,array,tit,leg,fmt,nc,x,y,ArrMin,ArrMax,xtit,ytit,wid,xsize,ysize,chsz,xlines,ylines
  ;!p.font=1
  wset=wid
  ;xcols=400L
  ;yrows=300L
  xcols=xsize
  yrows=ysize
  ncols=nc-2
  beta=congrid(array,xcols,yrows,cubic=-1,/interp)
  image = Bytscl(beta,top=ncols-1,max=ArrMax,min=ArrMin)+1B
  Image = (Image > 1B ) < ncols
  Index =where(beta eq 0)
  IF (Index(0) ne -1) THEN Image(index)=0B
  ny=n_elements(y)
  nx=n_elements(x)
  indxLevlsY=indgen(ny)
  indxLevlsX=indgen(nx)
  yy=congrid(indxLevlsY,yrows,cubic=-1,/interp,/center,/MINUS_ONE)
  xx=congrid(indxLevlsX,xcols,cubic=-1,/interp,/center,/MINUS_ONE)
  ;TV,image,100,180
  TV,image,xsize/8,ysize/4
  plot,xx,yy,ytick_get=ytick,xtick_get=xtick,yrange=[min(yy),max(yy)],xrange=[min(xx),max(xx)],/nodata,$
    position=[xsize/8,ysize/4,xsize/8+xsize,ysize/4+ysize],/noerase,charsize=1.2*chsz,thick=5
  yticknew=string(y(fix(ytick)),'(f6.1)')
  xticknew=string(x(fix(xtick)),'(f6.1)')
  plot,xx,yy,ytickname=yticknew,xtickname=xticknew,yrange=[min(yy),max(yy)],xrange=[min(xx),max(xx)],/nodata,$
    position=[xsize/8,ysize/4,xsize/8+xsize,ysize/4+ysize],$
    xtitle=xtit,ytitle=ytit,/noerase,title=tit,color=255,/device,charsize=1.2*chsz,thick=5
  ;----plot lines
  nXlines    = n_elements(xlines)
  nYlines    = n_elements(ylines)
  indxLinesY = indgen(ny)
  indxLinesX = indgen(nx)
  yy         = congrid(indxLinesY,yrows,cubic=-1,/interp,/center,/MINUS_ONE)
  xx         = congrid(indxLinesX,xcols,cubic=-1,/interp,/center,/MINUS_ONE)
  FOR iline=0,nXlines-1 DO BEGIN
      plots,[xlines[iline],xlines[iline]],[min(yy),max(yy)]
  ENDFOR
  FOR iline=0,nYlines-1 DO BEGIN
      plots,[min(xx),max(xx)],[ylines[iline],ylines[iline]]
  ENDFOR
  ;---color bar
  Bar =Byte(indgen(ncols))
  Bar = (Bar > 1B) <ncols
  Bar =[ [Bar],[Bar] ]
  lengthBar=25
  Bar = congrid(Bar, xsize, lengthBar)
  TV, Bar,xsize/8,ysize/12
  x0=Indgen(xcols)*ArrMax/xcols
  y0=Indgen(yrows)*20./yrows
  plot,x0,y0,/nodata,position=[xsize/8,ysize/12,xsize/8+xsize,ysize/12+lengthBar],xrange=[ArrMin,ArrMax],xtitle=leg,xticklen=0.2,$
    xtickformat=Fmt,/noerase,color=255,/device,charsize=1.2*chsz,yticks=1,ystyle=1,xstyle=1,ytickname=['',''],$
    yrange=[min(y0),max(y0)],ytickformat='(a1)',thick=9
  return
end


;===============================================================
; Name:		MAP_ARRAY
;
;
; Type:		IDL Subroutine
;
;
; Description:  Displays an array content on a map.
;
;
; History:
;       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
;
;===============================================================

PRO MAP_ARRAY, VAR, X,Y,Xtit,Ytit,MAP_NAME, MINVALUE, MAXVALUE, TITLE, DIV, FORMAT, resol, position
   image=bytscl(var, min=minvalue, max=maxvalue, top=nc)
   NC=!D.table_size-1
   TVLCT,r,g,b,/GET
   set_plot,'Z'
   device,set_resolution=resol
   DrawColor=NC+1
   map_set, /cyl, limit=[-90,-180, 90, 180],position=position,title=title,color=DrawColor, charsize=0.8,/NOBORDER
   warp=map_image(image,xx,yy,xs,ys,/bilin,compress=1)
   TV,warp,xx,yy,xsize=xs,ysize=ys
   Plot, X,Y, /NoData, XStyle=1, YStyle=1, Xticks=12, Yticks=6,POSITION=position, /NoErase, $
     xtitle=xtit, ytitle=ytit,Color=drawColor, Charsize=0.8,xrange=[-180,180],yrange=[-90,90]
   MAP_GRID, color=DrawColor
   MAP_CONTINENTS, /CONT, /COUNTRIES, /USA, COLOR=drawColor
   ;--Draw a color bar
   position_bar=fltarr(4)
   position_bar[0]=position[0]+0.1
   position_bar[1]=position[1]-0.1
   position_bar[2]=position[2]-0.1
   position_bar[3]=position_bar[1]+0.03
   ColorBar2, NColors=NC, DIVISION=DIV, FORMAT=FORMAT,MIN=minvalue,MAX=maxvalue,POSITION=position_bar,$
          CHARSIZE=0.8,COLOR=drawColor
   ;---Output in png format
   write_png, map_name, TVRD(), r, g, b
end


PRO DISPLAY_ARRAY, VAR, X,Y,Xtit,Ytit,MAP_NAME, MINVALUE, MAXVALUE, TITLE, DIV, FORMAT, resol, position
   image=bytscl(var, min=minvalue, max=maxvalue, top=nc)
   NC=!D.table_size-1
   TVLCT,r,g,b,/GET
   set_plot,'Z'
   device,set_resolution=resol
   DrawColor=NC+1
   TV,image
   ;Plot, X,Y, /NoData, XStyle=1, YStyle=1, Xticks=12, Yticks=6,POSITION=position, /NoErase, $
   ;  xtitle=xtit, ytitle=ytit,Color=drawColor, Charsize=0.8,xrange=[-180,180],yrange=[-90,90]
   ;--Draw a color bar
   position_bar=fltarr(4)
   position_bar[0]=position[0]+0.1
   position_bar[1]=position[1]-0.1
   position_bar[2]=position[2]-0.1
   position_bar[3]=position_bar[1]+0.03
   ColorBar2, NColors=NC, DIVISION=DIV, FORMAT=FORMAT,MIN=minvalue,MAX=maxvalue,POSITION=position_bar,$
          CHARSIZE=0.8,COLOR=drawColor
   ;---Output in png format
   write_png, map_name, TVRD(), r, g, b
end



;===============================================================
; Name:		consts
;
;
; Type:		IDL Subroutine
;
;
; Description:  Function to define constants in a structure to be
;               then used accross all applications.
;;
;
; History:
;       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
;
;===============================================================

PRO consts, Consts
   MW_O3     = 47.99820D0
   MW_DRYAIR = 28.9648D0
   Consts={                                        $
          ;---Weights of molecular species
          MW_H2O: 18.01528D0,                      $
          MW_O3: MW_O3,                            $
          MW_DRYAIR: MW_DRYAIR,                    $
          ;---Standard gravity->Symbol:g,  Units:m/s^2,  
          STANDARD_GRAVITY: 9.80665D0,             $
          ;---Avogadro constant->Symbol:N(A),  Units:mole^-1,  Rel.Uncert.(ppm): 0.079
          AVOGADRO_CONSTANT: 6.02214199D+23,       $
          mr2ppmv_o3: 1000.0*MW_DRYAIR / MW_O3,    $
          ;---surface types
          OC_TYP: 0,                               $
          SEAICE_TYP: 1,                           $             
          LD_TYP: 2,                               $            
          SNOW_TYP: 3,                             $           
          DESERT_TYP: 4,                           $
          COAST_TYP: 6,                            $
	  ;---Integer satllite ID
	  INT_SATID_N18: 1,                        $			
	  INT_SATID_METOPA: 2,                     $
	  INT_SATID_F16: 3,                        $
	  INT_SATID_N19: 4,                        $
	  INT_SATID_F18: 5,                        $
	  INT_SATID_NPP: 6,                        $
	  INT_SATID_AQUA: 7,                       $
          INT_SATID_FY3RI: 8,                      $
          INT_SATID_TRMM: 9,                       $
          INT_SATID_GPM: 10,                       $
          INT_SATID_MTMA: 12,                      $
          INT_SATID_MTSA: 13,                      $
	  INT_SATID_METOPB: 14,                    $
	  INT_SATID_GCOMW1: 15,                    $
	  INT_SATID_F17: 18,                       $
	  ;---String satllite ID
	  STR_SATID_N18: 'n18',                    $
	  STR_SATID_METOPA: 'metopA',              $
	  STR_SATID_F16: 'f16',                    $
	  STR_SATID_N19: 'n19',                    $
	  STR_SATID_F18: 'f18',                    $
	  STR_SATID_NPP: 'npp',                    $
	  STR_SATID_AQUA: 'aqua',                  $
          STR_SATID_FY3RI: 'fy3ri',                $
          STR_SATID_TRMM: 'trmm',                  $
          STR_SATID_GPM: 'gpm',                    $
          STR_SATID_MTMA: 'mtma',                  $
          STR_SATID_MTSA: 'mtsa',                  $
	  STR_SATID_METOPB: 'metopB',              $
	  STR_SATID_GCOMW1: 'gcomw1',              $
	  STR_SATID_F17: 'f17'                     $
         }
END 




;===============================================================
; Name:		Transf2Log
;
;
; Type:		IDL Subroutine
;
;
; Description:  Transforms into logarithm space (of an array)
;
;
;
; History:
;       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
;
;===============================================================

PRO OptionalTransfIntoLogArr,iMode,nProfiles,nLay,xLay
  IF (iMode ne 0) THEN BEGIN
      Transf2Log,nprofiles,nlay,xLay(*,*),xLayLog,iMode
      xLay(*,*)=xLayLog(*,*)
  ENDIF
END
 
PRO Transf2Log,nprofiles,nlay,xlay,xLayLog,iMode
  xLayLog=xLay
  FOR iprof=0L,nprofiles-1 DO BEGIN
      FOR ilay=0,nLay-1 DO BEGIN
          IF (xlay(iprof,ilay) ge 0.) THEN BEGIN
              IF (iMode eq 1) THEN BEGIN
                  xlayLog(iprof,ilay)=alog(max([xlay(iprof,ilay),0.00001]))
              ENDIF
              IF (iMode eq 2) THEN BEGIN
                  xlayLog(iprof,ilay)=alog(-alog(max([xlay(iprof,ilay),0.00001])))
              ENDIF
          ENDIF
      ENDFOR
  ENDFOR
END

;===============================================================
; Name:		Transf2LogVec
;
;
; Type:		IDL Subroutine
;
;
; Description:  Transforms into logarithm space (of a vector)
;
;
; History:
;       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
;
;===============================================================

PRO OptionalTransfIntoLogVec,iMode,nProfiles,xLay
  IF (iMode ne 0) THEN BEGIN
      Transf2LogVec,nprofiles,xLay(*),xLayLog,iMode
      xLay(*)=xLayLog(*)
  ENDIF
END

 
PRO Transf2LogVec,nprofiles,xlay,xLayLog,iMode
  xLayLog=xLay
  FOR iprof=0L,nprofiles-1 DO BEGIN
      IF (iMode eq 1) THEN BEGIN
          xlayLog(iprof)=alog(max([xlay(iprof),0.00001]))
      ENDIF
      IF (iMode eq 2) THEN BEGIN
          xlayLog(iprof)=alog(-alog(max([xlay(iprof),0.00001])))
      ENDIF
  ENDFOR
END


;===============================================================
; Name:		ComputeIndicesAtm
;
;
; Type:		IDL Subroutine
;
;
; Description:  Computes the indexes of ATmospheric part
;
;
; Subroutines needed:
;       - None
;
;
; History:
;       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
;
;===============================================================

PRO ComputeIndicesAtm,iTemp,iWvap,iOzon,iClw,iRain,iSnow,iIce,iGrpl,iTskinAtm,iSfcpAtm,NEIGENV_temp,$
  NEIGENV_wvap,NEIGENV_ozon,NEIGENV_clw,NEIGENV_rain,NEIGENV_snow,NEIGENV_ice,NEIGENV_grpl,NEIGENV_tskinAtm
  iTemp     = 0
  iWvap     = iTemp+NEIGENV_temp
  iOzon     = iWvap+NEIGENV_wvap
  iClw      = iOzon+NEIGENV_ozon
  iRain     = iClw +NEIGENV_clw
  iSnow     = iRain+NEIGENV_rain
  iIce      = iSnow+NEIGENV_snow
  iGrpl     = iIce +NEIGENV_ice
  iTskinAtm = iGrpl+NEIGENV_grpl
  iSfcpAtm  = iTskinAtm+NEIGENV_tskinAtm
END

;===============================================================
; Name:		ComputeIndicesSfc
;
;
; Type:		IDL Subroutine
;
;
; Description:  Computes indexes of the surface vector
;
;
; Subroutines needed:
;       - None
;
;
; History:
;       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
;
;===============================================================

PRO ComputeIndicesSfc,iEmis,iRefl,iWindSp,iTskin,iDeltat,iSfcPres,NEIGENV_emiss,$
  NEIGENV_refl,NEIGENV_windsp,NEIGENV_tskin,NEIGENV_deltat,NEIGENV_SfcPres
  iEmis    = 0
  iRefl    = iEmis+ NEIGENV_emiss
  iWindSp  = iRefl + NEIGENV_refl
  iTskin   = iWindSp + NEIGENV_windsp
  iDeltat  = iTskin + NEIGENV_tskin 
  iSfcPres = iDeltat + NEIGENV_deltat
END

;===============================================================
; Name:		compStats
;
;
; Type:		IDL Subroutine
;
;
; Description:  Determines a number of statistics given two vectors
;
; History:
;       09-06-2007      Kevin Garrett, IMSG Inc @ NOAA/NESDIS/ORA
;
;===============================================================

PRO compStats,x,y,stats
  nelts=n_elements(x)
  corrfac=correlate(x,y)
  bias=moment(x-y)
  res=poly_fit(x,y,1,/double)
  rms=sqrt(mean((x-y)^2))
  stats=fltarr(9)
  stats(0)=corrfac
  stats(1)=bias[0]
  stats(2)=stdev(x-y)
  stats(3)=nelts
  stats(4)=res(1)
  stats(5)=res(0)
  stats(6)=rms
  stats(7)=mean(x)
  stats(8)=mean(y)
END


;===============================================================
; Name:		plotHist
;
;
; Type:		IDL Subroutine
;
;
; Description:  Plots a histogram.
;
; History:
;       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
;
;===============================================================

PRO plotHist,x,nbin,tit,xtit,ytit,M,iOplot,xmin,xmax,res,xlog,ylog,chsz
  res=histogram(x,nbins=nbin,locations=loc)
  res=res/float(max(res))*100.
  ymin=min(res)
  ymax=max(res)
  IF (iOplot eq 0) THEN BEGIN
      IF (xlog eq 0 and ylog eq 0) THEN plot,loc,res,xtitle=xtit,ytitle=ytit,$
        title=tit,yrange=[ymin,ymax],xrange=[xmin,xmax],xstyle=1,ystyle=1,charsize=chsz
      IF (xlog eq 0 and ylog eq 1) THEN plot,loc,res,xtitle=xtit,ytitle=ytit,$
        title=tit,yrange=[ymin,ymax],xrange=[xmin,xmax],xstyle=1,ystyle=1,/ylog,charsize=chsz
      IF (xlog eq 1 and ylog eq 0) THEN plot,loc,res,xtitle=xtit,ytitle=ytit,$
        title=tit,yrange=[ymin,ymax],xrange=[xmin,xmax],xstyle=1,ystyle=1,/xlog,charsize=chsz
      IF (xlog eq 1 and ylog eq 1) THEN plot,loc,res,xtitle=xtit,ytitle=ytit,$
        title=tit,yrange=[ymin,ymax],xrange=[xmin,xmax],xstyle=1,ystyle=1,/xlog,/ylog,charsize=chsz
      plots,[M,M],[ymin,ymax]
  ENDIF
  IF (iOplot eq 1) THEN BEGIN
      oplot,loc,res,color=240,linestyle=2
      plots,[M,M],[ymin,ymax],color=240,linestyle=2
  ENDIF
END

;===============================================================
; Name:		plotVec
;
;
; Type:		IDL Subroutine
;
;
; Description:  A more general form of Plot specifically for 
;               vertical profiles. Where we can choose to
;               plot in linear or log scales,etc  
;
; Subroutines needed:
;       - None
;
;
; History:
;       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
;
;===============================================================

PRO plotVec,X,pres_lay,X0,xrangDiff1,xrangDiff2,Pmult1,Pmult2,xtit,ytit,tit,iLinOrLog
  !p.multi=Pmult1
  thck=4
  IF (iLinOrLog eq 1) THEN $
    plot,X,pres_lay,yrange=[1000,0.01],/ylog,xtitle=xtit,ytitle=ytit,title=tit,xrange=xrangDiff1,$
    xthick=thck,ythick=thck
  IF (iLinOrLog eq 0) THEN $
    plot,X,pres_lay,yrange=[1000,0.01],xtitle=xtit,ytitle=ytit,title=tit,xrange=xrangDiff1,$
    xthick=thck,ythick=thck
  oplot,X0,pres_lay,linestyle=2
  !p.multi=Pmult2
  IF (iLinOrLog eq 1) THEN $ 
    plot,X-X0,pres_lay,yrange=[1000,0.01],/ylog,xrange=xrangDiff2,$
    xtitle='Difference Original-Reconstructed',$
    xthick=thck,ythick=thck
  IF (iLinOrLog eq 0) THEN $
    plot,X-X0,pres_lay,yrange=[1000,0.01],xrange=xrangDiff2,$
    xtitle='Difference Original-Reconstructed',$
    xthick=thck,ythick=thck
END

;===============================================================
; Name:		plot_legend
;
;
; Type:		IDL Subroutine
;
;
; Description:  Plots the legend on a plot.
;
;
; History:
;       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
;
;===============================================================

PRO plot_legend,iPlotCorner,nlegds,linesty,psm,col,comm,chsz
                                ;iPlotCorner = ;0->upper right,
                                ;1->upper left, 2->lower right,
                                ;3->lower left
  thck=4
  IF (iPlotCorner eq 0) THEN begin
      xx=0.75
      yy=0.85
  ENDIF
  IF (iPlotCorner eq 1) THEN begin
      xx=0.15
      yy=0.9
  ENDIF
  IF (iPlotCorner eq 2) THEN begin
      xx=0.8
      yy=0.15
  ENDIF
  IF (iPlotCorner eq 3) THEN begin
      xx=0.15
      yy=0.15
  ENDIF   
  FOR ileg=0,nlegds-1 do begin
      plots,[xx,xx+0.05],[yy-0.05*ileg,yy-0.05*ileg],linestyle=linesty[ileg],psym=-psm[ileg],/normal ,color=col[ileg],thick=thck
      xyouts,xx+0.051,yy-0.05*ileg,charsize=chsz,comm[ileg],/normal
  ENDFOR
END

;===============================================================
; Name:		PlotScatt
;
;
; Type:		IDL Subroutine
;
;
; Description:  Plots a scatterplot with the addition (or not) of
;               most common statistics.
;
; History:
;       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
;
;===============================================================

PRO PlotScatt,X,Y,xtit,ytit,tit,iAddStats,xmin,xmax,ymin,ymax,iScaleFact,thck
  plot,x,y,xtitle=xtit,ytitle=ytit,title=tit,xrange=[xmin,xmax],$
    yrange=[ymin,ymax],xstyle=1,ystyle=1,psym=1,charsize=1.*iScaleFact,symsize=0.6*iScaleFact,thick=thck,xthick=thck ,ythick=thck 
  dy=ymax-ymin
  if (stdev(x) ne 0. and stdev(y) ne 0.) THEN BEGIN
      IF (iAddStats eq 1) THEN BEGIN
          plots,[xmin,xmax],[ymin,ymax],color=240
          res   = moment(X-Y)
          DX    = (xmax-xmin)/30.
          DY    = (ymax-ymin)/30.
          ;poly  = poly_fit(X,Y,1,/double,yfit=yfit,Yerror=yErr)
          rms   = sqrt(mean((X-Y)^2))
          ;oplot,X,yfit,color=120,sym=3
          xyouts,xmin+DX*2.,ymax-DY*1.0,'Number Pts:'+string(n_elements(X),'(i8)'),charsize=0.6*iScaleFact,$
             charthick=0.8*iScaleFact
          xyouts,xmin+DX*2.,ymax-DY*2.5,'Mean (X-Y):'+string(res(0),'(f9.3)'),     charsize=0.6*iScaleFact,$
            charthick=0.8*iScaleFact
          xyouts,xmin+DX*2.,ymax-DY*4.0,'RMS:'+string(rms,'(f9.3)'),               charsize=0.6*iScaleFact,$
            charthick=0.8*iScaleFact
          xyouts,xmin+DX*2.,ymax-DY*5.5,'Variance:'+string(res(1),'(f9.3)'),       charsize=0.6*iScaleFact,$
            charthick=0.8*iScaleFact
          xyouts,xmin+DX*2.,ymax-DY*7.0,'Std Dev:'+string(stdev(X-Y),'(f9.3)'),    charsize=0.6*iScaleFact,$
            charthick=0.8*iScaleFact
          xyouts,xmin+DX*2.,ymax-DY*8.5,'Corr Fact:'+string(correlate(x,y),'(f9.3)'),charsize=0.6*iScaleFact,$
            charthick=0.8*iScaleFact
          ;xyouts,xmin+DX*2.,ymax-DY*6.,'Intercept:'+string(poly(0),'(f9.3)'),     charsize=0.5*iScaleFact,$
          ;  charthick=0.8*iScaleFact
          ;xyouts,xmin+DX*2.,ymax-DY*7.,'Slope:'+string(poly(1),'(f9.3)'),         charsize=0.5*iScaleFact,$
          ;  charthick=0.8*iScaleFact
          ;xyouts,xmin+DX*2.,ymax-DY*8.,'Fitting Err:'+string(yErr,'(f9.3)'),      charsize=0.5*iScaleFact,$
          ;  charthick=0.8*iScaleFact
      ENDIF
  ENDIF
  RETURN
END


;===============================================================
; Name:		PlotScatt_png
;
;
; Type:		IDL Subroutine
;
;
; Description:  Plots a scatterplot with the addition (or not) of
;               most common statistics.
;
; History:
;       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
;
;===============================================================
PRO PlotScatt_png,X,Y,xtit,ytit,tit,iAddStats,xmin,xmax,ymin,ymax,iScaleFact,thck,imgFile

 set_plot, 'z'
 loadct, 33, /silent
 TVLCT, r, g, b, /get

 r(0)=255   & g(0)=255   & b(0)=255    ; Load White Color - 0 for background
 r(255)=0   & g(255)=0   & b(255)=0    ; Load Black Color - 255

 plot,x,y,xtitle=xtit,ytitle=ytit,title=tit,xrange=[xmin,xmax],$
   yrange=[ymin,ymax],xstyle=1,ystyle=1,psym=1,charsize=1.*iScaleFact,symsize=0.6*iScaleFact,thick=thck,xthick=thck ,ythick=thck
 dy=ymax-ymin
 if (stdev(x) ne 0. and stdev(y) ne 0.) THEN BEGIN
     IF (iAddStats eq 1) THEN BEGIN
         plots,[xmin,xmax],[ymin,ymax],color=240
         res   = moment(X-Y)
         DX    = (xmax-xmin)/30.
         DY    = (ymax-ymin)/30.
         ;poly  = poly_fit(X,Y,1,/double,yfit=yfit,Yerror=yErr)
         rms   = sqrt(mean((X-Y)^2))
         ;oplot,X,yfit,color=120,sym=3
         xyouts,xmin+DX*2.,ymax-DY*1.0,'Number Pts:'+string(n_elements(X),'(i8)'),charsize=0.6*iScaleFact,$
            charthick=0.8*iScaleFact
         xyouts,xmin+DX*2.,ymax-DY*2.5,'Mean (X-Y):'+string(res(0),'(f9.3)'),     charsize=0.6*iScaleFact,$
           charthick=0.8*iScaleFact
         xyouts,xmin+DX*2.,ymax-DY*4.0,'RMS:'+string(rms,'(f9.3)'),               charsize=0.6*iScaleFact,$
           charthick=0.8*iScaleFact
         xyouts,xmin+DX*2.,ymax-DY*5.5,'Variance:'+string(res(1),'(f9.3)'),       charsize=0.6*iScaleFact,$
           charthick=0.8*iScaleFact
         xyouts,xmin+DX*2.,ymax-DY*7.0,'Std Dev:'+string(stdev(X-Y),'(f9.3)'),    charsize=0.6*iScaleFact,$
           charthick=0.8*iScaleFact
         xyouts,xmin+DX*2.,ymax-DY*8.5,'Corr Fact:'+string(correlate(x,y),'(f9.3)'),charsize=0.6*iScaleFact,$
           charthick=0.8*iScaleFact
         ;xyouts,xmin+DX*2.,ymax-DY*6.,'Intercept:'+string(poly(0),'(f9.3)'),     charsize=0.5*iScaleFact,$
         ;  charthick=0.8*iScaleFact
         ;xyouts,xmin+DX*2.,ymax-DY*7.,'Slope:'+string(poly(1),'(f9.3)'),         charsize=0.5*iScaleFact,$
         ;  charthick=0.8*iScaleFact
         ;xyouts,xmin+DX*2.,ymax-DY*8.,'Fitting Err:'+string(yErr,'(f9.3)'),      charsize=0.5*iScaleFact,$
         ;  charthick=0.8*iScaleFact
     ENDIF
 ENDIF

 write_png, imgFile, tvrd(), r, g, b

 RETURN
END


;===============================================================
; Name:		TransfIntoScan
;
;
; Type:		IDL Subroutine
;
;
; Description:  Transforms a vector into an array. Used to 
;               get a scanline-based array of Tbs from a simple
;               vector of individual TBs.
;
;
; History:
;       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
;
;===============================================================

PRO TransfIntoScan,tb,tbScan,npos,nscanl,scanPosTot,PosScan,nFovs,nList
  ntot          = 0
  nscanLperFile = lonarr(nList)
  FOR ifile=0,nList-1 DO Begin
      nscanLperFile(ifile)=nFovs(ifile)/npos
      IF (nscanLperFile(ifile)*npos ne nFovs(ifile)) then print,'Warning: #elts of file#',ifile,' not multiple of ',npos,nFovs(ifile)
      ntot=ntot+nscanLperFile(ifile)*npos
  ENDFOR
  scanPosTot       = tb
  scanPosTot(*,*)  = -99.
  nscanl      = ntot/npos
  ;---sanity checks
  IF (nscanl*npos ne ntot) then begin
      print,'Error: dimension not multiple of ',npos,ntot,nscanl
      stop
  endif
  IF (nscanl ne total(nscanLperFile(0:nList-1))) then begin
      print,'Error:  ',nscanl,sum(nscanLperFile(0:nList-1))
      stop
  endif
  typ=size(tb,/type)
  if (typ eq 4) then tbScan=fltarr(nscanl,npos)
  if (typ eq 2) then tbScan=intarr(nscanl,npos)  
  if (typ eq 3) then tbScan=lonarr(nscanl,npos)
  if (typ eq 7) then tbScan=strarr(nscanl,npos)
  iscanTot=-1L
  FOR ifile=0,nList-1 DO Begin
      itot=-1L
      FOR iscanL=0L,nscanLperFile(ifile)-1 DO BEGIN
          iscanTot=iscanTot+1
          FOR j=0L,npos-1 do begin
              itot                   = itot+1
              tbScan(iscanTot,j)     = tb(itot,ifile)
              scanPosTot(itot,ifile) = float(j+1)
              ;---sanity check
              IF (scanPosTot(itot,ifile) ne PosScan(itot,ifile)) then begin
                  print , 'Error:Inconsistent scan position (provided and computed)'
                  print, PosScan(itot,ifile),scanPosTot(itot,ifile)
                  stop
              ENDIF
          ENDFOR
      ENDFOR
  ENDFOR
END


;===============================================================
; Name:		plot3dIn2d
;
;
; Type:		IDL Subroutine
;
;
; Description:  Plots a 3D representation using a 2D plot. The
;               3rd dimension will be in a form of countours 
;               overplotted on the 2D plot.
;
;
; History:
;       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
;
;===============================================================

PRO plot3dIn2d,minX,maxX,minY,maxY,minZ,maxZ,X,Y,Z,nnx,nny,xtit,ytit,ztit,tit,chsz,iplotStats,stats,unit,fmt,scal
  nx = n_elements(x)
  ny = n_elements(y)
  nz = n_elements(z)
  if (nx ne ny) then stop,'Error: # of elts in x and y not identical'
  if (nx ne nz) then stop,'Error: # of elts in x and z not identical'
  nelts=nx
  xmin=min(x)
  xmax=max(x)
  dx=(xmax-xmin)/nnx
  ymin=min(y)
  ymax=max(y)
  dy=(ymax-ymin)/nny
  Z0=make_array(nnx,nny,/float,value=-999)
  x0=fltarr(nnx)
  y0=fltarr(nny)
  for ix=0,nnx-1 do begin
      xbound1=xmin+ix*dx
      xbound2=xmin+ix*dx+dx
      indx=where(x ge xbound1 and x le xbound2, icountx)
      if (icountx eq 0) then x0(ix)=(xbound1+xbound2)/2.
      if (icountx ne 0) then x0(ix)=mean(x(indx))
      for iy=0,nny-1 do begin
          ybound1=ymin+iy*dy
          ybound2=ymin+iy*dy+dy
          indy=where(y ge ybound1 and y le ybound2,icounty)
          if (icounty eq 0) then y0(iy) = (ybound1+ybound2)/2.
          if (icounty ne 0) then y0(iy) =  mean(y(indy))
          indxy=where(x ge xbound1 and x le xbound2 and $
                     y ge ybound1 and y le ybound2 ,icount)
          ;z0(ix,iy) = float(icount)
          if (icount ne 0) then z0(ix,iy) = mean(z(indxy))
      endfor
  endfor
  minZ=min(Z0(where(z0 ge 0)))
  maxZ=max(Z0(where(z0 ge 0)))
  z0=((z0-minZ)/(maxZ-minZ))*100.
  for ix=0,nnx-1 do begin
      for iy=0,nny-1 do begin
          IF (z0(ix,iy) gt 100) THEN z0(ix,iy)=100
          IF (z0(ix,iy) lt 0)   THEN z0(ix,iy)=0
      endfor
  endfor
  levs=findgen(120)/1.
  cols=levs*2
  contour,z0,x0,y0,levels=levs,c_colors=cols,xrange=[minX,maxX],xstyle=1,ystyle=1,$
    yrange=[minY,maxY],xtitle=xtit,ytitle=ytit,title=tit,charsize=chsz,position=[0.1,0.2,0.9,0.8]
  nc=!D.table_size
  nc=nc-2
  colorbar,ncolors=nc,/horizontal,range=[minZ,maxZ],title=unit,$
    format=fmt,charsize=scal*1.3,font=1,position=[0.10,0.05,0.9,0.1]
  c9=correlate(x,y)
  c10=moment(x-y)
  res=poly_fit(x,y,1,/double)
  rms=sqrt(mean((x-y)^2))
  stats=fltarr(7)
  stats(0)=c9
  stats(1)=c10[0]
  stats(2)=stdev(x-y)
  stats(3)=nelts
  stats(4)=res(1)
  stats(5)=res(0)
  stats(6)=rms
  if (iplotStats eq 1) then begin
      plots,[minX,maxX],[minY,maxY]
      oplot,x0,res(1)*x0+res(0),color=240
      comm9='Correlation Factor:'+string(c9)
      comm10=strmid(strcompress('Mean Bias: '+string(c10[0])),0,20)
      comm11='Std Dev:'+string(stdev(x-y))
      comm12='Number of Elts:'+string(nelts)
      comm13='Slope:'+string(res(1))
      comm14='Intercept:'+string(res(0))
      comm15='RMS:'+string(rms)
      xs=minX+(maxX-minX)/10.
      r0=minY
      r=maxY
      xyouts,xs,r0+(0.92*(r-r0)),comm9,charsize=chsz
      xyouts,xs,r0+(0.88*(r-r0)),comm10,charsize=chsz
      xyouts,xs,r0+(0.84*(r-r0)),comm11,charsize=chsz
      xyouts,xs,r0+(0.80*(r-r0)),comm12,charsize=chsz
      xyouts,xs,r0+(0.76*(r-r0)),comm13,charsize=chsz
      xyouts,xs,r0+(0.72*(r-r0)),comm14,charsize=chsz
      xyouts,xs,r0+(0.68*(r-r0)),comm15,charsize=chsz
  endif
END


;===============================================================
; Name:		ChoppVec
;
;
; Type:		IDL Subroutine
;
;
; Description:  Plots the X vx Y in a contour form, where the 
;               red color represents the highest density of points.
;
;
; Subroutines needed:
;       - None
;
;
; History:
;       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
;
;===============================================================

PRO ChoppVec,minX,maxX,minY,maxY,X,Y,nnx,nny,xtit,ytit,tit,chsz,iplotStats,stats
  nx = n_elements(x)
  ny = n_elements(y)
  if (nx ne ny) then stop,'Error: # of elts in x and y not identical'
  nelts=nx
  xmin=min(x)
  xmax=max(x)
  dx=(xmax-xmin)/nnx
  ymin=min(y)
  ymax=max(y)
  dy=(ymax-ymin)/nny
  Z0=fltarr(nnx,nny)
  x0=fltarr(nnx)
  y0=fltarr(nny)
  for ix=0,nnx-1 do begin
      xbound1=xmin+ix*dx
      xbound2=xmin+ix*dx+dx
      indx=where(x ge xbound1 and x le xbound2, icountx)
      if (icountx eq 0) then x0(ix)=(xbound1+xbound2)/2.
      if (icountx ne 0) then x0(ix)=mean(x(indx))
      for iy=0,nny-1 do begin
          ybound1=ymin+iy*dy
          ybound2=ymin+iy*dy+dy
          indy=where(y ge ybound1 and y le ybound2,icounty)
          if (icounty eq 0) then y0(iy) = (ybound1+ybound2)/2.
          if (icounty ne 0) then y0(iy) =  mean(y(indy))
          indxy=where(x ge xbound1 and x le xbound2 and $
                     y ge ybound1 and y le ybound2,icount)
          z0(ix,iy) = float(icount)
      endfor
  endfor
  z0=(z0/max(z0))*100.
  levs=findgen(100)/1.
  cols=levs*2.3
  contour,z0,x0,y0,levels=levs,c_colors=cols,xrange=[minX,maxX],xstyle=1,ystyle=1,$
    yrange=[minY,maxY],xtitle=xtit,ytitle=ytit,title=tit,charsize=chsz
  c9=correlate(x,y)
  c10=moment(x-y)
  res=poly_fit(x,y,1,/double)
  rms=sqrt(mean((x-y)^2))
  stats=fltarr(9)
  stats(0)=c9
  stats(1)=c10[0]
  stats(2)=stdev(x-y)
  stats(3)=nelts
  stats(4)=res(1)
  stats(5)=res(0)
  stats(6)=rms
  stats(7)=mean(x)
  stats(8)=mean(y)
  if (iplotStats eq 1) then begin
      plots,[minX,maxX],[minY,maxY]
      oplot,x0,res(1)*x0+res(0),color=240
      comm9='Correlation Factor:'+string(c9)
      comm10='Mean Bias:'+string(c10[0])
      comm11='Std Dev:'+string(stdev(x-y))
      comm12='Number of Elts:'+string(nelts)
      comm13='Slope:'+string(res(1))
      comm14='Intercept:'+string(res(0))
      comm15='RMS:'+string(rms)
      xs=minX+(maxX-minX)/10.
      r0=minY
      r=maxY
      xyouts,xs,r0+(0.92*(r-r0)),comm9,charsize=chsz*0.5
      xyouts,xs,r0+(0.86*(r-r0)),comm10,charsize=chsz*0.5
      xyouts,xs,r0+(0.80*(r-r0)),comm11,charsize=chsz*0.5
      xyouts,xs,r0+(0.74*(r-r0)),comm12,charsize=chsz*0.5
      xyouts,xs,r0+(0.68*(r-r0)),comm13,charsize=chsz*0.5
      xyouts,xs,r0+(0.62*(r-r0)),comm14,charsize=chsz*0.5
      xyouts,xs,r0+(0.56*(r-r0)),comm15,charsize=chsz*0.5
  endif
END


;===============================================================
; Name:		ChoppVec_png
;
;
; Type:		IDL Subroutine
;
;
; Description:  Plots the X vx Y in a contour form, where the 
;               red color represents the highest density of points.
;		PNG version
;
;
; Subroutines needed:
;       - None
;
;
; History:
;       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
;
;===============================================================

PRO ChoppVec_png,minX,maxX,minY,maxY,X,Y,nnx,nny,xtit,ytit,tit,chsz,iplotStats,stats, img_name

  set_plot,'Z'
  device,set_resolution=[650, 500]
  position=[0.05, 0.15, 0.95, 0.9]

  ;---- load color table
  loadct, 39, /silent
  TVLCT, rr, gg, bb, /get
  rr(0)=255   & gg(0)=255   & bb(0)=255   	; Load White Color - 0 for background 
  rr(255)=0   & gg(255)=0   & bb(255)=0 	; Load Black Color - 255

  nx = n_elements(x)
  ny = n_elements(y)
  if (nx ne ny) then stop,'Error: # of elts in x and y not identical'
  nelts=nx
  xmin=min(x)
  xmax=max(x)
  dx=(xmax-xmin)/nnx
  ymin=min(y)
  ymax=max(y)
  dy=(ymax-ymin)/nny
  Z0=fltarr(nnx,nny)
  x0=fltarr(nnx)
  y0=fltarr(nny)
  for ix=0,nnx-1 do begin
      xbound1=xmin+ix*dx
      xbound2=xmin+ix*dx+dx
      indx=where(x ge xbound1 and x le xbound2, icountx)
      if (icountx eq 0) then x0(ix)=(xbound1+xbound2)/2.
      if (icountx ne 0) then x0(ix)=mean(x(indx))
      for iy=0,nny-1 do begin
          ybound1=ymin+iy*dy
          ybound2=ymin+iy*dy+dy
          indy=where(y ge ybound1 and y le ybound2,icounty)
          if (icounty eq 0) then y0(iy) = (ybound1+ybound2)/2.
          if (icounty ne 0) then y0(iy) =  mean(y(indy))
          indxy=where(x ge xbound1 and x le xbound2 and $
                     y ge ybound1 and y le ybound2,icount)
          z0(ix,iy) = float(icount)
      endfor
  endfor
  z0=(z0/max(z0))*100.
  levs=findgen(100)/1.
  cols=levs*2.3
  contour,z0,x0,y0,levels=levs,c_colors=cols,xrange=[minX,maxX],xstyle=1,ystyle=1,$
    yrange=[minY,maxY],xtitle=xtit,ytitle=ytit,title=tit,charsize=chsz
  c9=correlate(x,y)
  c10=moment(x-y)
  res=poly_fit(x,y,1,/double,status=pfit_stat)
  rms=sqrt(mean((x-y)^2))
  stats=fltarr(9)
  stats(0)=c9
  stats(1)=c10[0]
  stats(2)=stdev(x-y)
  stats(3)=nelts
  if(pfit_stat eq 0)then begin
      stats(4)=res(1)
      stats(5)=res(0)
  endif
;  stats(4)=res(1)
;  stats(5)=res(0)
  stats(6)=rms
  stats(7)=mean(x)
  stats(8)=mean(y)
  if (iplotStats eq 1) then begin
      plots,[minX,maxX],[minY,maxY]
      if(pfit_stat eq 0)then oplot,x0,res(1)*x0+res(0),color=240
      comm9='Correlation Factor:'+string(c9)
      comm10='Mean Bias:'+string(c10[0])
      comm11='Std Dev:'+string(stdev(x-y))
      comm12='Number of Elts:'+string(nelts)
      if(pfit_stat eq 0)then begin
          comm13='Slope:'+string(res(1))
          comm14='Intercept:'+string(res(0))
      endif else begin
          comm13='Slope: N/A'
          comm14='Intercept: N/A'
      endelse
;      comm13='Slope:'+string(res(1))
;      comm14='Intercept:'+string(res(0))
      comm15='RMS:'+string(rms)
      xs=minX+(maxX-minX)/10.
      r0=minY
      r=maxY
      xyouts,xs,r0+(0.92*(r-r0)),comm9,charsize=chsz*0.5
      xyouts,xs,r0+(0.86*(r-r0)),comm10,charsize=chsz*0.5
      xyouts,xs,r0+(0.80*(r-r0)),comm11,charsize=chsz*0.5
      xyouts,xs,r0+(0.74*(r-r0)),comm12,charsize=chsz*0.5
      xyouts,xs,r0+(0.68*(r-r0)),comm13,charsize=chsz*0.5
      xyouts,xs,r0+(0.62*(r-r0)),comm14,charsize=chsz*0.5
      xyouts,xs,r0+(0.56*(r-r0)),comm15,charsize=chsz*0.5
  endif
  
  ;---- write out image file
  write_png, img_name, TVRD(), rr, gg, bb

END


;===============================================================
; Name:		ChoppVec_png_dummy
;
; Type:		IDL Subroutine
;
; Description:  Plot a dummy blank image
;               
; Subroutines needed:
;       - None
;
; History:
;       08-10-2011      Wanchun Chen
;
;===============================================================

Pro ChoppVec_png_dummy,minX,maxX,minY,maxY,xtit,ytit,tit,chsz,img_name

  set_plot,'Z'
  device,set_resolution=[650, 500]
  position=[0.05, 0.15, 0.95, 0.9]

  ;---- load color table
  loadct, 39, /silent
  TVLCT, rr, gg, bb, /get
  rr(0)=255   & gg(0)=255   & bb(0)=255   	; Load White Color - 0 for background 
  rr(255)=0   & gg(255)=0   & bb(255)=0 	; Load Black Color - 255

  plot, [minx, maxx], [miny, maxy], xrange=[minx, maxx], yrange=[miny, maxy], position=position,$
        /nodata, Xticks=2, Yticks=2, charsize=chz, xTitle=xtit, yTitle=ytit, title=tit
  xyouts,0.35,0.50, 'NO DATA', /normal, charsize=3.0, charthick=2.0, color=255
  write_png, img_name, TVRD(), rr, gg, bb
  device,/close
 
End


;===============================================================
; Name:		SlicePlot
;
;
; Type:		IDL Subroutine
;
;
; Description:  Plots the X vs Y in a way that slices the X into 
;               pieces and plots the average Y for that slice bin
;               along with the standard deviation (if requested).
;
;               If iOverplot <0 then just computes statistics (no plot)
; History:
;       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
;
;===============================================================

PRO SlicePlot,minX,maxX,minY,maxY,X,Y,nnx,xtit,ytit,tit,chsz,iOverplot,x0,y0,iAddStdDev
  nx = n_elements(x)
  ny = n_elements(y)
  if (nx ne ny) then stop,'Error: # of elts in x and y not identical'
  nelts=nx
  xmin=minX
  xmax=maxX
  dx=(xmax-xmin)/nnx
  x0=make_array(nnx,value=-9999,/float)
  y0=make_array(nnx,8,value=-9999,/float)
  for ix=0,nnx-1 do begin
      xbound1=xmin+ix*dx
      xbound2=xmin+ix*dx+dx
      indx=where(x ge xbound1 and x le xbound2, icountx)
      ;if (icountx eq 0) then begin
      ;    x0(ix)=(xbound1+xbound2)/2.
      ;    y0(ix,0)=0.
      ;    y0(ix,1)=0.
      ;    y0(ix,2)=0
      ;    y0(ix,3)=0
      ;    y0(ix,4)=0
      ;    y0(ix,5)=0
      ;    y0(ix,6)=0
      ;    y0(ix,7)=0
      ;endif
      if (icountx ge 2) then begin
          x0(ix)   = mean(x(indx))
          y0(ix,0) = mean(y(indx))
          y0(ix,1) = stdev(y(indx))
          y0(ix,2) = n_elements(y(indx))
          y0(ix,3) = max(y(indx))
          y0(ix,4) = min(y(indx))
          ;res      = poly_fit(X(indx),Y(indx),1,/double)
          ;y0(ix,5) = res[1] ;slope
          ;y0(ix,6) = res[0] ;intercept
          y0(ix,7) = correlate(x(indx),y(indx))
      endif
  endfor
  IF (iOverplot eq 0) THEN BEGIN
      plot,x0,y0(*,0),xrange=[xmin,xmax],xstyle=1,ystyle=1,$
        yrange=[minY,maxY],xtitle=xtit,ytitle=ytit,title=tit,charsize=chsz
      IF (iAddStdDev eq 1) THEN oploterr,x0,y0(*,0),y0(*,1)
  ENDIF
  IF (iOverplot gt 0) THEN BEGIN
      oplot,x0,y0(*,0),psym=-iOverPlot,color=min([240,iOverplot*120])
      IF (iAddStdDev eq 1) THEN oploterr,x0,y0(*,0),y0(*,1)
  ENDIF
END

;----Compute FOV size of AMSUA: Wanchun Chen. (original C-code from Huan Meng). 2006
PRO FOV_A, fov_size_A
  NUMSPOT_A=30
  PI=3.141593
  SCAN_ANG_A=3.3
  REARTH=6371.0
  RSAT=833.0
  i=0
  angle=0.0
  angle1=0.0
  ang=fltarr(NUMSPOT_A+1)
  for i=0, NUMSPOT_A do begin
    angle = PI * SCAN_ANG_A * (i - NUMSPOT_A/2.0) / 180.0
    angle1 = (REARTH + RSAT) * sin(angle) / REARTH
    angle1 = atan(angle1 / sqrt(1 - angle1 * angle1))
    ang(i) = (angle1 * 180.0 / PI) - SCAN_ANG_A * (i - NUMSPOT_A/2.0)
  endfor
  for i=0, NUMSPOT_A-1 do begin
     fov_size_A(i) = abs(ang(i+1) - ang(i)) 
  endfor
END


;----Compute FOV size of AMSUB: Wanchun Chen. (original C-code from Huan Meng). 2006
PRO FOV_B, fov_size_B
  NUMSPOT_B=90
  PI=3.141593
  SCAN_ANG_B=1.1
  REARTH=6371.0
  RSAT=833.0
  i=0
  angle=0.0
  angle1=0.0
  ang=fltarr(NUMSPOT_B+1)
  for i=0, NUMSPOT_B do begin
    angle = PI * SCAN_ANG_B * (i - NUMSPOT_B/2.0) / 180.0
    angle1 = (REARTH + RSAT) * sin(angle) / REARTH
    angle1 = atan(angle1 / sqrt(1 - angle1 * angle1))
    ang(i) = (angle1 * 180.0 / PI) - SCAN_ANG_B * (i - NUMSPOT_B/2.0)
  endfor
  for i=0, NUMSPOT_B-1 do begin
     fov_size_B(i) = abs(ang(i+1) - ang(i))
  endfor
END


;---Wanchun Chen (averaging of AMSU FOvs)
PRO AVE_MIRS, VAR, NUM, NCOL, NROW, NLAY
  for k=0, NLAY-1 do begin
  for j=0, NROW-1 do begin
  for i=0, NCOL-1 do begin
    if ( NUM(i,j,k) gt 0 ) then VAR(i,j,k)=VAR(i,j,k)/NUM(i,j,k) $
    else  VAR(i,j,k)=-999.0
  endfor
  endfor
  endfor
  return
END

;===============================================================
; Name:		DiscreteStratif
;
;
; Type:		IDL Subroutine
;
;
; Description:  Performs stratification of vector x2process based
;               on vector stationID whose unique discrete elements 
;               are contained in uniqStatID (of size nStatIDs).
;
; History:
;       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
;
;===============================================================
PRO DiscreteStratif,LogConditions,stationID,uniqStatID,nStatIDs,x2process,lat,lon,stats_byStation,lat_byStation,lon_byStation
   stats_byStation  =make_array(nStatIDs,6,value=-999,/float)
   lat_byStation    =make_array(nStatIDs,value=-999,/float)
   lon_byStation    =make_array(nStatIDs,value=-999,/float)
   FOR istat=0,nStatIDs-1 DO BEGIN
       test1  = LogConditions and stationID eq uniqStatID(istat)
       ind    = where(test1 ,ncount) 
       IF (ncount ge 2) THEN BEGIN
           x2plot = x2process(ind)
           stats_byStation(istat,0) = mean(x2plot)
           stats_byStation(istat,1) = stdev(x2plot)
           stats_byStation(istat,2) = sqrt(mean(x2plot^2))
           stats_byStation(istat,3) = ncount
           lat_byStation(istat)     = mean(lat(ind))
           lon_byStation(istat)     = mean(lon(ind))
       ENDIF
   ENDFOR
END


;===============================================================
; Name:		plotStratifStat
;
;
; Type:		IDL Subroutine
;
;
; Description:  Plots stratified statistics
;
; History:
;       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
;
;===============================================================
PRO plotStratifStat,XX,Yref,Yretr,ind,nAlgos,xtit,ytit,tit,yrang,thck,what2show,nnx,scalFactSiz,col,psm,linesty,LabelsRetr
   X=XX(ind)
   Y=Yref(ind)-YRetr(0,ind)
   SlicePlot,min(X),max(X),min(Y),max(Y),X,Y,nnx,xtit,ytit,tit,scalFactSiz,-99,x0,y0,0
   ind0=where(x0 gt -999,ncount)
   IF (ncount gt 1) THEN BEGIN
       plot,x0(ind0),y0(ind0,what2show),ystyle=1,yrange=yrang,thick=thck,$
         xtitle=xtit,ytitle=ytit,title=tit,charsize=scalFactSiz
   ENDIF
   FOR ialgo = 1,nAlgos-1 DO BEGIN
       Y=YRef(ind)-YRetr(ialgo,ind)
       SlicePlot,min(X),max(X),min(Y),max(Y),X,Y,nnx,xtit,ytit,tit,scalFactSiz,-99,x0,y0,0
       ind0=where(x0 gt -999,ncount)
       IF (ncount gt 1) THEN BEGIN
           oplot,x0(ind0),y0(ind0,what2show),color=col(ialgo),psym=psm(ialgo),linestyle=linesty(ialgo)
       ENDIF
   ENDFOR
   plot_legend,0,nAlgos,linesty,-psm,col,LabelsRetr,scalFactSiz*0.7
END

;===============================================================
; Name:		PlotCompChVec
;
;
; Type:		IDL Subroutine
;
;
; Description:  Plots comparison of 2-D arrays of channel measurements
;               either 1 to 1 (scale=1) or on separate scales 
;               (scale/=1) such as TBD vs TB
;
; Subroutines needed:
;       - None
;
;
; History:
;       09-06-2007      Kevin Garrett, IMSG Inc @ NOAA/NESDIS/ORA
;
;===============================================================
PRO PlotCompChVec,x,y,xtit,ytit,pmulti,chs,psym,scale
    erase
    !p.multi=pmulti
    FOR ich = 0,n_elements(chs)-1 DO BEGIN
        tit=strcompress('Ch.' + string(ich+1) + string(chs[ich]) + ' GHz')
        IF (scale eq 1) THEN BEGIN
            xmin = min([min(x(*,ich)),min(y(*,ich))])
            xmax = max([max(x(*,ich)),max(y(*,ich))])
            ymin = min([min(x(*,ich)),min(y(*,ich))])
            ymax = max([max(x(*,ich)),max(y(*,ich))])
        ENDIF ELSE BEGIN
            xmin = min(x(*,ich))
            xmax = max(x(*,ich))
            ymin = min(y(*,ich))
            ymax = max(y(*,ich))
        ENDELSE
        ymin = [-100,-100,-60,-10,-20,-15,-10,-8,-5,-5,-3,-10,-15,-20,-100,-80,-80,-40,-40,-80]
        ymax = [150,150,90,50,20,15,10,8,5,5,3,10,15,20,100,100,60,20,20,20]

        plot,x(*,ich),y(*,ich),xtitle=xtit,ytitle=ytit,psym=3,color=0,background=255, $
	     xrange=[xmin,xmax],yrange=[ymin(ich),ymax(ich)],title=tit, $
	     xstyle=1,ystyle=1,charsize=1.,xthick=3.0,ythick=3.0,thick=1.0,charthick=3.0
        IF (scale eq 1) THEN BEGIN
            plots,[xmin,xmax],[ymin,ymax]
        ENDIF
    ENDFOR
END

;===============================================================
; Name:		PlotCompChVec2Par
;
;
; Type:		IDL Subroutine
;
;
; Description:  Plots comparison of a 2-D array of channel measurements
;               to a 1-D parameter
;               
;
; Subroutines needed:
;       - None
;
;
; History:
;       09-06-2007      Kevin Garrett, IMSG Inc @ NOAA/NESDIS/ORA
;
;===============================================================
PRO PlotCompChVec2Par,x,y,xtit,ytit,pmulti,chs,psym
    erase
    !p.multi=pmulti
    FOR ich = 0,n_elements(chs)-1 DO BEGIN
        tit=strcompress('Ch.' + string(ich+1) + string(chs[ich]) + ' GHz')
        xmin = min(x)
        xmax = max(x)
        ymin = min(y(*,ich))
        ymax = max(y(*,ich))
        plot,x,y(*,ich),xtitle=xtit,ytitle=ytit,psym=3,color=0,background=255, $
	     xrange=[xmin,xmax],yrange=[ymin,ymax],title=tit, $
	     xstyle=1,ystyle=1,charsize=1.,xthick=3.0,ythick=3.0,thick=1.0,charthick=3.0
    ENDFOR
END


;===============================================================
; Name:		PlotPerfEval
;
;
; Type:		IDL Subroutine
;
;
; Description:  Calls a variety to plotting routines to compare
;               a vector, y1, to another, x1 on 1 to 1 scale;
;               and compares another vector, y2, to parameters
;               x1,x2 and x3.
;               
;
; Subroutines needed:
;       - PlotScatt
;       - ChoppVec
;       - SlicePlot
;
;
; History:
;       09-06-2007      Kevin Garrett, IMSG Inc @ NOAA/NESDIS/ORA
;
;===============================================================
PRO PlotPerfEval,x1,x2,x3,y1,y2,xtit1,xtit2,xtit3,ytit1,ytit2,tit,psym,chsz
    erase
    !p.multi=[6,2,3,0,0]
    min=min([min(y1),min(x1)])
    max=max([max(y1),max(x1)])
    x1min=min(x1)
    x1max=max(x1)
    x2min=min(x2)
    x2max=max(x2)
    x3min=min(x3)
    x3max=max(x3)
    y2min=min(y2)
    y2max=max(y2)
    PlotScatt,x1,y1,xtit1,ytit1,tit,1,min,max,min,max,1.2,3
    plot,x1,y2,xtitle=xtit1,ytitle=ytit2,psym=psym
    ChoppVec,min,max,min,max,x1,y1,30,30,xtit1,ytit1,tit,1.2,1,stats
    SlicePlot,x1min,x1max,y2min,y2max,x1,y2,6,xtit1,ytit2,tit,1.2,0,sx0,sy0,0
    plot,x2,y2,xtitle=xtit2,ytitle=ytit2,psym=psym,xrange=[x2min,x2max],yrange=[y2min,y2max]
    plot,x3,y2,xtitle=xtit3,ytitle=ytit2,psym=psym,xrange=[x3min,x3max],yrange=[y2min,y2max]
END


;===============================================================
; Name:		PlotTrend
;
;
; Type:		IDL Subroutine
;
;
; Description:  Uses SlicePlot to plot trend of a vector
;               and overplots other vectors for comparison
;               Also plots the stdev of each vector and
;               the number of points used to sum in SlicePlot 
;               
;               Can only be used to plot 1 or compare 2,3 or 4 vectors
;
; Subroutines needed:
;       - plot_legend
;       - SlicePlot
;
;
; History:
;       09-06-2007      Kevin Garrett, IMSG Inc @ NOAA/NESDIS/ORA
;
;===============================================================
PRO PlotTrend,x1,y1,y2,y3,y4,nnx,xtit,ytit1,ytit2,tit,legend_text
    erase
    !p.multi=[3,1,3,0,0]
    lines       = [0,0,0,0]
    symbols     = [0,0,0,0]
    legend_col  = [0,75,150,225]
    xmin=min(x1)
    xmax=max(x1)
    IF (n_elements(y2) eq 1 and n_elements(y3) eq 1 and n_elements(y4) eq 1) THEN BEGIN
        ymin=min(y1)
        ymax=max(y1)
        SlicePlot,xmin,xmax,ymin,ymax,x1,y1,nnx,xtit,ytit1,tit,1.2,0,sx0,sy0,0
        plot_legend,2,1,lines,symbols,legend_col,legend_text,1.2
        plot, sx0,sy0(*,1),xtitle=xtit,ytitle=ytit2
        plot, sx0,sy0(*,2),xtitle=xtit,ytitle='Number of points'
    ENDIF
    IF (n_elements(y2) gt 1 and n_elements(y3) eq 1 and n_elements(y4) eq 1) THEN BEGIN
        ymin=min([min(y1),min(y2)])
        ymax=max([max(y1),max(y2)])
        SlicePlot,xmin,xmax,ymin,ymax,x1,y1,nnx,xtit,ytit1,tit,1.2,0,sx0,sy0,0
        SlicePlot,xmin,xmax,ymin,ymax,x1,y2,nnx,xtit,ytit1,tit,1.2,1,sx1,sy1,0
        plot_legend,2,2,lines,symbols,legend_col,legend_text,1.2
        plot, sx0,sy0(*,1),xtitle=xtit,ytitle=ytit2
        oplot,sx1,sy1(*,1),color=min([240,1*75])
        plot, sx0,sy0(*,2),xtitle=xtit,ytitle='Number of points'
    ENDIF
    IF (n_elements(y3) gt 1 and n_elements(y4) eq 1) THEN BEGIN
        ymin=min([min(y1),min(y2),min(y3)])
        ymax=max([max(y1),max(y2),max(y3)])
        SlicePlot,xmin,xmax,ymin,ymax,x1,y1,nnx,xtit,ytit1,tit,1.2,0,sx0,sy0,0
        SlicePlot,xmin,xmax,ymin,ymax,x1,y2,nnx,xtit,ytit1,tit,1.2,1,sx1,sy1,0
        SlicePlot,xmin,xmax,ymin,ymax,x1,y3,nnx,xtit,ytit1,tit,1.2,2,sx2,sy2,0
        plot_legend,2,3,lines,symbols,legend_col,legend_text,1.2
        plot, sx0,sy0(*,1),xtitle=xtit,ytitle=ytit2
        oplot,sx1,sy1(*,1),color=min([240,1*75])
        oplot,sx2,sy2(*,1),color=min([240,2*75])
        plot, sx0,sy0(*,2),xtitle=xtit,ytitle='Number of points'
    ENDIF 
    IF (n_elements(y4) gt 1) THEN BEGIN
        ymin=min([min(y1),min(y2),min(y3),min(y4)])
        ymax=max([max(y1),max(y2),max(y3),max(y4)])
        SlicePlot,xmin,xmax,ymin,ymax,x1,y1,nnx,xtit,ytit1,tit,1.2,0,sx0,sy0,0
        SlicePlot,xmin,xmax,ymin,ymax,x1,y2,nnx,xtit,ytit1,tit,1.2,1,sx1,sy1,0
        SlicePlot,xmin,xmax,ymin,ymax,x1,y3,nnx,xtit,ytit1,tit,1.2,2,sx2,sy2,0
        SlicePlot,xmin,xmax,ymin,ymax,x1,y4,nnx,xtit,ytit1,tit,1.2,3,sx3,sy3,0
        plot_legend,2,4,lines,symbols,legend_col,legend_text,1.2
        plot, sx0,sy0(*,1),xtitle=xtit,ytitle=ytit2
        oplot,sx1,sy1(*,1),color=min([240,1*75])
        oplot,sx2,sy2(*,1),color=min([240,2*75])
        oplot,sx3,sy3(*,1),color=min([240,3*75])
        plot, sx0,sy0(*,2),xtitle=xtit,ytitle='Number of points'
    ENDIF
END


;===============================================================
; Name:		PlotProf
;
;
; Type:		IDL Subroutine
;
;
; Description:  Plots an atmospheric profile. If the profile
;               contains invalid layers/levels, they should
;               be set to fill values of -32768
;
;               Set xbnds=[0,0] in call to let PlotProf 
;               determine xmin,xmax
;
;               May also be used to overplot profiles
;               
; Subroutines needed:
;               none
;
; History:
;       09-06-2007      Kevin Garrett, IMSG Inc @ NOAA/NESDIS/ORA
;
;===============================================================
PRO PlotProf,x1,y1,xbnds,ymin,ymax,tit,xtit,ytit,ioplot,col,linsty,chsz,thck
    IF (ioplot eq 0) THEN BEGIN
        IF (xbnds(0) eq 0 and xbnds(1) eq 0) THEN BEGIN
            ind=where(abs(x1-(-32768)) ge 0.002)
            xrange=[min(x1(ind)),max(x1(ind))]
            plot,x1(ind),y1(ind),/ylog,xrange=xrange,yrange=[ymax,ymin],title=tit,xtitle=xtit,ytitle=ytit,color=col, $
                 linestyle=linsty,charsize=chsz,thick=thck
        ENDIF ELSE BEGIN
            ind=where(abs(x1-(-32768)) ge 0.002)
            plot,x1(ind),y1(ind),/ylog,xrange=xbnds,yrange=[ymax,ymin],title=tit,xtitle=xtit,ytitle=ytit,color=col, $
                 linestyle=linsty,charsize=chsz,thick=thck
        ENDELSE
    ENDIF ELSE BEGIN
        ind=where(abs(x1-(-32768)) ge 0.002)
        oplot,x1(ind),y1(ind),color=col,linestyle=linsty,thick=thck
    ENDELSE
END


;===============================================================
; Name:		PlotProfStats
;
;
; Type:		IDL Subroutine
;
;
; Description:  Plots atmospheric profile statistics. If the profile
;               contains invalid layers/levels, they should
;               be set to fill values
;
;               Set Stats2Plot elements to 1 for desired statistic
;               stat2plot = [corr,bias,stdv,nelts,slp,intcpt,rms]
;
;               May plot up to 4 profiles (x1,x2,x3,x4) to compare 
;               statistics (be sure to set pmulti and iProfs to match)
;
;               Will call PlotProf and plot all on same scale
;               Capable to overplot profiles
;               
; Subroutines needed:
;               PlotProf
;
; History:
;       09-06-2007      Kevin Garrett, IMSG Inc @ NOAA/NESDIS/ORA
;
;===============================================================
PRO PlotProfStats,x1,x2,x3,x4,y1,ymin,ymax,tit,xtit,ytit,stats2plot,iProfs,ioplot, $
                  lincol,linsty,chsz,thck
    stat=['Corr ','Bias ','Stdev ','Nelts ','Slope ','Intrcpt ','RMS ']
    IF (ioplot eq 1) THEN BEGIN
        linsty = linsty
        lincol = lincol
    ENDIF ELSE BEGIN
        linsty = [0,0,0,0]
        lincol = [0,0,0,0]
    ENDELSE
    FOR istat=0,6 DO BEGIN
        IF (stats2plot(istat) eq 1) THEN BEGIN
            erase
            !p.multi=[2,2,1,0,0]
            ;---Set xmin,xmax to same scale
            ind=where(abs(x1(istat,*)-(-32768)) ge 0.002)
            IF (istat ge 3) THEN BEGIN
                IF (iProfs eq 1) THEN xbnds = [min(x1(istat,ind)),max(x1(istat,ind))]        ELSE $
                  IF (iProfs eq 2) THEN xbnds = [min([min(x1(istat,ind)),min(x2(istat,ind))]),      $
                                                 max([max(x1(istat,ind)),max(x2(istat,ind))])] ELSE $ 
                  IF (iProfs eq 3) THEN xbnds = [min([min(x1(istat,ind)),min(x2(istat,ind)),min(x3(istat,ind))]),      $
                                                 max([max(x1(istat,ind)),max(x2(istat,ind)),max(x3(istat,ind))])] ELSE $
                  IF (iProfs eq 4) THEN xbnds = [min([min(x1(istat,ind)),min(x2(istat,ind)), $
                                                      min(x3(istat,ind)),min(x4(istat,ind))]),    $
                                                 max([max(x1(istat,ind)),max(x2(istat,ind)), $
                                                      max(x3(istat,ind)),max(x4(istat,ind))])]
            ENDIF
            IF (istat eq 1) THEN xbnds = [-5,5] ELSE $
            IF (istat eq 2) THEN xbnds = [0,6]
            ;---Plot
            IF (iProfs ge 1) THEN BEGIN
                PlotProf,x1(istat,ind),y1(ind),xbnds,ymin,ymax,tit,stat(istat)+xtit,ytit,0,lincol(1),linsty(1),chsz,thck
            ENDIF
            IF (iProfs ge 2) THEN BEGIN
                PlotProf,x2(istat,ind),y1(ind),xbnds,ymin,ymax,tit,stat(istat)+xtit,ytit,ioplot,lincol(2),linsty(2),chsz,thck
            ENDIF
            IF (iProfs ge 3) THEN BEGIN
                PlotProf,x3(istat,ind),y1(ind),xbnds,ymin,ymax,tit,stat(istat)+xtit,ytit,ioplot,lincol(3),linsty(3),chsz,thck
            ENDIF
            IF (iProfs eq 4) THEN BEGIN
                PlotProf,x4(istat,ind),y1(ind),xbnds,ymin,ymax,tit,stat(istat)+xtit,ytit,ioplot,lincol(4),linsty(4),chsz,thck
            ENDIF
        ENDIF
    ENDFOR
END


;===============================================================
; Name:		Plot_scatter
;
; Type:		IDL Subroutine
;
; Description:  Plots atmospheric profile statistics. If the profile
;               contains invalid layers/levels, they should
;               be set to fill values
;
;               Set Stats2Plot elements to 1 for desired statistic
;               stat2plot = [corr,bias,stdv,nelts,slp,intcpt,rms]
;
;               May plot up to 4 profiles (x1,x2,x3,x4) to compare 
;               statistics (be sure to set pmulti and iProfs to match)
;
;               Will call PlotProf and plot all on same scale
;               Capable to overplot profiles
;               
; Subroutines needed:
;               None
;
; History:
;       05-30-2008      Wanchun Chen
;
;===============================================================
Pro  plot_scatter, minx, maxx, miny, maxy, xval, yval, xtitle, ytitle, title, imgFile, nticks=nticks
  set_plot,'Z'
  device,set_resolution=[650, 500]
  position=[0.1, 0.1, 0.95, 0.95]
  
  if n_elements(nticks) eq 0 then nticks = 7
  
  TVLCT, r, g, b, /get
  r(0)=255 & g(0)=255 & b(0)=255
  r(255)=0 & g(255)=0 & b(255)=0
  r(7)=0   & g(7)=0   & b(7)=255
  r(8)=255 & g(8)=0   & b(8)=0
  
  plot, [minx, maxx], [miny, maxy], xrange=[minx, maxx], yrange=[miny, maxy], position=position,$
	/nodata, Xticks=nticks, Yticks=nticks, charsize=0.75, xTitle=xtitle, yTitle=ytitle, title=title,$
	xstyle=1,ystyle=1
  oplot, xval,yval, psym=2, symsize=0.25, color=7
  oplot, [minx, maxx], [miny, maxy], linestyle=0, color=8
  corr = correlate(xval,yval)
  nelts = n_elements(xval)
  ;momt=moment(xval-yval)
  ;bias=momt(0)
  bias=mean(xval-yval)
  std=stdev(xval-yval)
  xyouts,0.125,0.90, 'corr='+STRTRIM(String(corr),2),/normal
  xyouts,0.125,0.87, 'nPts='+STRTRIM(String(nelts),2),/normal
  xyouts,0.125,0.84, 'bias='+STRTRIM(String(bias),2),/normal
  xyouts,0.125,0.81, 'stdv='+STRTRIM(String(std),2),/normal
  write_png, imgFile, TVRD(), r, g, b
  device,/close
End


;===============================================================
; Name:		Plot_scatter_dummy
;
; Type:		IDL Subroutine
;
; Description:  Plots a dummy blank scatter plot
;               
; Subroutines needed:
;               None
;
; History:
;       02-18-2011      Wanchun Chen
;
;===============================================================
Pro  plot_scatter_dummy, minx, maxx, miny, maxy, xtitle, ytitle, title, imgFile, nticks=nticks
  set_plot,'Z'
  device,set_resolution=[650, 500]
  position=[0.1, 0.1, 0.95, 0.95]
  if n_elements(nticks) eq 0 then nticks = 7

  TVLCT, r, g, b, /get
  r(0)=255 & g(0)=255 & b(0)=255
  r(255)=0 & g(255)=0 & b(255)=0
  
  plot, [minx, maxx], [miny, maxy], xrange=[minx, maxx], yrange=[miny, maxy], position=position,$
	/nodata, Xticks=nticks, Yticks=nticks, charsize=0.75, xTitle=xtitle, yTitle=ytitle, title=title,$
	xstyle=1,ystyle=1
  xyouts,0.125,0.90, 'corr=0',/normal
  xyouts,0.125,0.87, 'nPts=0',/normal
  xyouts,0.125,0.84, 'bias=0',/normal
  xyouts,0.125,0.81, 'stdv=0',/normal
  write_png, imgFile, TVRD(), r, g, b
  device,/close
End


;===============================================================
; Name:		PlotHistGeo
;
;
; Type:		IDL Subroutine
;
;
; Description:  Plots a histogram.
;
; History:
;       05-30-2008      Favio Iturbide-Sanchez, IMSG Inc @ NOAA/NESDIS/STAR
;       12-08-2008      Kevin Garrett - added geo3, label, xrange,
;                                       binsize; and made more generic
;
;===============================================================

PRO PlotHistGeo,geo1,geo2,geo3,geo1label,geo2label,geo3label,xrange,binsize,xtitle,ytitle,title,imgFile

;---- Device Set Up
set_plot,'Z'
device,set_resolution=[650, 500]
position=[0.05, 0.15, 0.95, 0.9]
;---- Load Color Table
loadct, 33, /silent
TVLCT, r, g, b, /get
r(0)=255   & g(0)=255   & b(0)=255   	; Load White Color - 0 for background 
r(255)=0   & g(255)=0   & b(255)=0 	; Load Black Color - 255
r(254)=255 & g(254)=255 & b(254)=255	; Load White Color - 254 for Missing Data
r(253)=215 & g(253)=215 & b(253)=215	; Load Grey Color (20%) - 253 for Land/Ocean Coverage
r(252)=98  & g(252)=98  & b(252)=98	; Load Grey Color (70%) - 252 for non-convergent points(qc fail)
drawColor = 255

hist_geo1=HISTOGRAM(float(geo1),binsize=binsize,location=loc1)
hist_geo1=100*hist_geo1/float(N_ELEMENTS(geo1))
IF (geo2[0] ge 0) THEN BEGIN 
    hist_geo2=HISTOGRAM(float(geo2),binsize=binsize,locations=loc2)
    hist_geo2=100*hist_geo2/float(N_ELEMENTS(geo2))
ENDIF
IF (geo3[0] ge 0) THEN BEGIN
    hist_geo3=HISTOGRAM(float(geo3),binsize=binsize,locations=loc3)
    hist_geo3=100*hist_geo3/float(N_ELEMENTS(geo3))
ENDIF

scal1 = xrange(1)-(xrange(1)*0.4)
scal2 = xrange(1)/10.

yrange=[0.01,100.0]
dumx=[1,2] & dumy=[1,2]
PLOT, dumx, dumy, XTITLE=xtitle, YTITLE=ytitle,XRANGE=xrange, YRANGE=yrange,TITLE=title, $
  CHARSIZE=0.95,/NODATA

OPLOT, loc1, hist_geo1, PSYM = 10,color=50,linestyle=5,thick=3
IF (geo2[0] ge 0) THEN OPLOT, loc2, hist_geo2, PSYM = 10,color=210,linestyle=3,thick=3
IF (geo3[0] ge 0) THEN OPLOT, loc3, hist_geo3, PSYM = 10,color=150,linestyle=2,thick=3
oplot, [scal1,scal1+scal2], [80,80],color=50,linestyle=5,thick=3.0
XYOUTS, scal1+2*scal2,80.0,geo1label, charsize=1.2,charthick=1.0

IF (geo2[0] ge 0) THEN oplot, [scal1,scal1+scal2], [76,76], color=210,linestyle=3,thick=3.0
IF (geo2[0] ge 0) THEN XYOUTS, scal1+2*scal2,76.0,geo2label, charsize=1.2,charthick=1.0
IF (geo3[0] ge 0) THEN oplot, [scal1,scal1+scal2], [72,72], color=150,linestyle=2,thick=3.0
IF (geo3[0] ge 0) THEN XYOUTS, scal1+2*scal2,72.0,geo3label, charsize=1.2,charthick=1.0
;---- Write Out Image File
write_png,imgFile, TVRD(), r, g, b
END

;===============================================================
; Name:		PlotHistGeo1
;
;
; Type:		IDL Subroutine
;
;
; Description:  Plots a histogram. (based on PlotHistGeo)
;
; History:
;       05-30-2008      Favio Iturbide-Sanchez, IMSG Inc @ NOAA/NESDIS/STAR
;       12-08-2008      Kevin Garrett - added geo3, label, xrange,
;                                       binsize; and made more generic
;       04-07-2009      C. Grassotti - added binmin and nbins argument
;                                      for bins
;
;===============================================================

PRO PlotHistGeo1,geo1,geo2,geo3,geo1label,geo2label,geo3label,xrange,binmin,binsize,nbins,xtitle,ytitle,title,imgFile

;---- Device Set Up
set_plot,'Z'
device,set_resolution=[650, 500]
position=[0.05, 0.15, 0.95, 0.9]
;---- Load Color Table
loadct, 33, /silent
TVLCT, r, g, b, /get
r(0)=255   & g(0)=255   & b(0)=255   	; Load White Color - 0 for background 
r(255)=0   & g(255)=0   & b(255)=0 	; Load Black Color - 255
r(254)=255 & g(254)=255 & b(254)=255	; Load White Color - 254 for Missing Data
r(253)=215 & g(253)=215 & b(253)=215	; Load Grey Color (20%) - 253 for Land/Ocean Coverage
r(252)=98  & g(252)=98  & b(252)=98	; Load Grey Color (70%) - 252 for non-convergent points(qc fail)
drawColor = 255

hist_geo1=HISTOGRAM(float(geo1),min=binmin,binsize=binsize,nbins=nbins,location=loc1)
hist_geo1=100*hist_geo1/float(N_ELEMENTS(geo1))
loc1=loc1+0.5*binsize
;print,'loc1=',loc1
;print,'hist_geo1=',hist_geo1
IF (geo2[0] ge 0) THEN BEGIN 
    hist_geo2=HISTOGRAM(float(geo2),min=binmin,binsize=binsize,nbins=nbins,locations=loc2)
    hist_geo2=100*hist_geo2/float(N_ELEMENTS(geo2))
    loc2=loc2+0.5*binsize
;    print,'loc2=',loc2
;    print,'hist_geo2=',hist_geo2
ENDIF
IF (geo3[0] ge 0) THEN BEGIN
    hist_geo3=HISTOGRAM(float(geo3),min=binmin,binsize=binsize,nbins=nbins,locations=loc3)
    hist_geo3=100*hist_geo3/float(N_ELEMENTS(geo3))
    loc3=loc3+0.5*binsize
;    print,'loc3=',loc3
;    print,'hist_geo3=',hist_geo3
ENDIF

scal1 = xrange(1)-(xrange(1)*0.4)
scal2 = xrange(1)/10.

yrange=[0.01,100.0]
dumx=[1,2] & dumy=[1,2]
PLOT, dumx, dumy, XTITLE=xtitle, YTITLE=ytitle,XRANGE=xrange, YRANGE=yrange,TITLE=title, $
  CHARSIZE=0.95,/NODATA

OPLOT, loc1, hist_geo1, PSYM = 10,color=50,linestyle=5,thick=3
IF (geo2[0] ge 0) THEN OPLOT, loc2, hist_geo2, PSYM = 10,color=210,linestyle=3,thick=3
IF (geo3[0] ge 0) THEN OPLOT, loc3, hist_geo3, PSYM = 10,color=150,linestyle=2,thick=3
oplot, [scal1,scal1+scal2], [80,80],color=50,linestyle=5,thick=3.0
XYOUTS, scal1+2*scal2,80.0,geo1label, charsize=1.2,charthick=1.0

IF (geo2[0] ge 0) THEN oplot, [scal1,scal1+scal2], [76,76], color=210,linestyle=3,thick=3.0
IF (geo2[0] ge 0) THEN XYOUTS, scal1+2*scal2,76.0,geo2label, charsize=1.2,charthick=1.0
IF (geo3[0] ge 0) THEN oplot, [scal1,scal1+scal2], [72,72], color=150,linestyle=2,thick=3.0
IF (geo3[0] ge 0) THEN XYOUTS, scal1+2*scal2,72.0,geo3label, charsize=1.2,charthick=1.0
;---- Write Out Image File
write_png,imgFile, TVRD(), r, g, b
END

;===============================================================
; Name:		PlotHistGeoLog
;
;
; Type:		IDL Subroutine
;
;
; Description:  Plots a histogram.
;
; History:
;       08-08-2008      Favio Iturbide-Sanchez, IMSG Inc @ NOAA/NESDIS/STAR
;
;===============================================================

PRO PlotHistGeoLog,geo1,geo2,geo1label,geo2label,xtitle,ytitle,title,imgFile

;---- Device Set Up
set_plot,'Z'
device,set_resolution=[650, 500]
position=[0.05, 0.15, 0.95, 0.9]
;---- Load Color Table
loadct, 33, /silent
TVLCT, r, g, b, /get
r(0)=255   & g(0)=255   & b(0)=255   	; Load White Color - 0 for background 
r(255)=0   & g(255)=0   & b(255)=0 	; Load Black Color - 255
r(254)=255 & g(254)=255 & b(254)=255	; Load White Color - 254 for Missing Data
r(253)=215 & g(253)=215 & b(253)=215	; Load Grey Color (20%) - 253 for Land/Ocean Coverage
r(252)=98  & g(252)=98  & b(252)=98	; Load Grey Color (70%) - 252 for non-convergent points(qc fail)
drawColor = 255

binsize=0.005
hist_mirs=HISTOGRAM(float(geo1),binsize=binsize,location=loc1)
hist_nwp=HISTOGRAM(float(geo2),binsize=binsize,locations=loc2)
hist_mirs=100*hist_mirs/float(N_ELEMENTS(geo1))
hist_nwp=100*hist_nwp/float(N_ELEMENTS(geo2))

xrange=[1e-3,1.0]
yrange=[1e-2,100.0]
dumx=[1,2] & dumy=[1,2]
PLOT, dumx, dumy, XTITLE=xtitle, YTITLE=ytitle,XRANGE=xrange, YRANGE=yrange,TITLE=title, $
  CHARSIZE=1.1,/XLOG,/YLOG,/NODATA
OPLOT, loc1, hist_mirs, PSYM = 10,color=50,linestyle=3,thick=2
OPLOT, loc2, hist_nwp, PSYM = 10,color=210,linestyle=3,thick=2
OPLOT, [0.03,0.07], [27.5,27.5],color=50,linestyle=3,thick=3.0
XYOUTS, 0.08,25.0,geo1label, charsize=1.2,charthick=1.0
OPLOT, [0.03,0.07], [39.5,39.5], color=210,linestyle=3,thick=3.0
XYOUTS, 0.08,36.0,geo2label, charsize=1.2,charthick=1.0
;---- Write Out Image File
write_png,imgFile, TVRD(), r, g, b
END

;===============================================================
; Name:		PlotHist1Par
;
;
; Type:		IDL Subroutine
;
;
; Description:  Plots a histogram.
;
; History:
;       08-08-2008      Favio Iturbide-Sanchez, IMSG Inc @ NOAA/NESDIS/STAR
;
;===============================================================

PRO PlotHist1Parm,parm1,binsize,xmin,xmax,ymin,xtitle,ytitle,title,imgFile

;---- Device Set Up
set_plot,'Z'
device,set_resolution=[650, 500]
position=[0.05, 0.15, 0.95, 0.9]
;---- Load Color Table
loadct, 33, /silent
TVLCT, r, g, b, /get
r(0)=255   & g(0)=255   & b(0)=255   	; Load White Color - 0 for background 
r(255)=0   & g(255)=0   & b(255)=0 	; Load Black Color - 255
r(254)=255 & g(254)=255 & b(254)=255	; Load White Color - 254 for Missing Data
r(253)=215 & g(253)=215 & b(253)=215	; Load Grey Color (20%) - 253 for Land/Ocean Coverage
r(252)=98  & g(252)=98  & b(252)=98	; Load Grey Color (70%) - 252 for non-convergent points(qc fail)
drawColor = 255

hist_parm1=HISTOGRAM(float(parm1),binsize=binsize,location=loc1)

y_max=max(hist_parm1)*1.5
xrange=[xmin,xmax]
yrange=[ymin,y_max]
dumx=[1,2] & dumy=[1,2]
PLOT, dumx, dumy, XTITLE=xtitle, YTITLE=ytitle,XRANGE=xrange, YRANGE=yrange,TITLE=title, $
  CHARSIZE=1.0,/NODATA
OPLOT, loc1, hist_parm1, PSYM = 10,color=50,linestyle=0,thick=2
;---- Write Out Image File
write_png,imgFile, TVRD(), r, g, b
END

;===============================================================
; Name:		PlotGeoVs
;
;
; Type:		IDL Subroutine
;
;
; Description:  Plots two variables using discrete points
;               
; Subroutines needed:
;               None
;
; History:
;       08-25-2008      Favio Iturbide-Sanchez, IMSG Inc @ NOAA/NESDIS/STAR
;
;===============================================================
PRO  PlotGeoVs, minx, maxx, miny, maxy, x_ticks, y_ticks, xval, yval, xtitle, ytitle, title, imgFile

  set_plot,'Z'
  device,set_resolution=[650, 500]

  TVLCT, r, g, b, /get
  r(0)=255 & g(0)=255 & b(0)=255
  r(255)=0 & g(255)=0 & b(255)=0

  r(7)=0 & g(7)=0 & b(7)=255
  r(8)=255 & g(8)=0 & b(8)=0
  
  plot,  [minx, maxx], [miny, maxy], xrange=[minx, maxx], yrange=[miny, maxy], $
	  /nodata, Xticks=x_ticks, Yticks=y_ticks, charsize=1.0, $
	  xTitle=xtitle, yTitle=ytitle, title=title
  oplot, xval,yval, psym=2, symsize=0.25, color=7
  write_png, imgFile, TVRD(), r, g, b									     

END

;===============================================================
; Name:		PlotHistXYLog
;
;
; Type:		IDL Subroutine
;
;
; Description:  Plots a histogram.
;
; History:
;       10-13-2010      Favio Iturbide-Sanchez, IMSG Inc @ NOAA/NESDIS/STAR
;
;===============================================================
PRO PlotHistXYLog,geo1,geo2,binsize,xmin,xmax,ymin,ymax,xminline,xmaxline,y1line,y2line, $
                  x1lab,y1lab,x2lab,y2lab,geo1label,geo2label,xtitle,ytitle,title,imgFile

;---- Device Set Up
set_plot,'Z'
device,set_resolution=[650, 500]
position=[0.05, 0.15, 0.95, 0.9]
;---- Load Color Table
loadct, 33, /silent
TVLCT, r, g, b, /get
r(0)=255   & g(0)=255   & b(0)=255   	; Load White Color - 0 for background 
r(255)=0   & g(255)=0   & b(255)=0 	; Load Black Color - 255
r(254)=255 & g(254)=255 & b(254)=255	; Load White Color - 254 for Missing Data
r(253)=215 & g(253)=215 & b(253)=215	; Load Grey Color (20%) - 253 for Land/Ocean Coverage
r(252)=98  & g(252)=98  & b(252)=98	; Load Grey Color (70%) - 252 for non-convergent points(qc fail)
drawColor = 255

hist_mirs=HISTOGRAM(float(geo1),min=0.0,binsize=binsize,location=loc1)
hist_nwp=HISTOGRAM(float(geo2),min=0.0,binsize=binsize,locations=loc2)
hist_mirs=100*hist_mirs/float(N_ELEMENTS(geo1))
hist_nwp=100*hist_nwp/float(N_ELEMENTS(geo2))

xrange=[xmin,xmax]
yrange=[ymin,ymax]
dumx=[1,2] & dumy=[1,2]
PLOT, dumx, dumy, XTITLE=xtitle, YTITLE=ytitle,XRANGE=xrange, YRANGE=yrange,TITLE=title, $
  CHARSIZE=1.0,/XLOG,/YLOG,XSTYLE=1,YSTYLE=1,/NODATA
OPLOT, loc1, hist_mirs, PSYM = 10,color=50,linestyle=3,thick=2
OPLOT, loc2, hist_nwp, PSYM = 10,color=210,linestyle=3,thick=2
OPLOT, [xminline,xmaxline], [y1line,y1line],color=50,linestyle=3,thick=3.0
XYOUTS, x1lab,y1lab,geo1label, charsize=1.0,charthick=1.0
OPLOT, [xminline,xmaxline], [y2line,y2line], color=210,linestyle=3,thick=3.0
XYOUTS, x2lab,y2lab,geo2label, charsize=1.0,charthick=1.0
;---- Write Out Image File
write_png,imgFile, TVRD(), r, g, b
END


;===============================================================
; Name:		Undefine
;
;
; Type:		IDL Subroutine
;
;
; Description:  undefine a variable to release resource
;               
; Subroutines needed:
;               None
;
; History:
;       10-07-2008      Wanchun Chen PSGS Inc @ NOAA/NESDIS/STAR 
;
;===============================================================
PRO UNDEFINE, varname  
   tempvar = SIZE(TEMPORARY(varname))
END



;===============================================================
; Name:		BTEST
;
;
; Type:		IDL Subroutine
;
;
; Description:  to mimic fortran 90 BTEST
;               
; Subroutines needed:
;               None
;
; History:
;       10-07-2008      Wanchun Chen PSGS Inc @ NOAA/NESDIS/STAR 
;
;===============================================================
FUNCTION BTEST, varname, pos
  
  mask   = 1
  vartmp = varname
  
  for i = 0, pos-1 do begin
    vartmp = vartmp / 2
  endfor
  
  return, ( vartmp and mask )
  
end

;===============================================================
; Name:		Plotscatter
;
;
; Type:		IDL Subroutine
;
;
; Description:  Plots atmospheric profile statistics. 
;               
; Subroutines needed:
;               None
;
; History:
;    09-03-2008      Favio Iturbide-Sanchez, IMSG Inc @ NOAA/NESDIS/STAR      
;
;===============================================================
PRO  Plotscatter, minx,maxx,miny,maxy,x_ticks,y_ticks,xlabout,ylabout,xval,yval,xtitle,ytitle,title,imgFile

  set_plot,'Z'
  device,set_resolution=[650, 500]

  TVLCT, r, g, b, /get
  r(0)=255 & g(0)=255 & b(0)=255
  r(255)=0 & g(255)=0 & b(255)=0

  r(7)=0 & g(7)=0 & b(7)=255
  r(8)=255 & g(8)=0 & b(8)=0
  
  plot,  [minx, maxx], [miny, maxy], xrange=[minx, maxx], yrange=[miny, maxy], $
	  /nodata, Xticks=x_ticks, Yticks=y_ticks, charsize=0.98, $
	  xTitle=xtitle, yTitle=ytitle, title=title
  oplot, xval,yval, psym=2, symsize=0.25, color=7
  oplot, [minx, maxx], [miny, maxy], linestyle=0, color=8
  corr = correlate(xval,yval)
  nelts = n_elements(xval)
  momt=moment(xval-yval)
  bias=momt(0)
  std=stdev(xval-yval)
  xyouts,xlabout,ylabout, 'corr='+STRTRIM(String(corr),2),/normal
  xyouts,xlabout,ylabout-0.03, 'nPts='+STRTRIM(String(nelts),2),/normal
  xyouts,xlabout,ylabout-0.06, 'bias='+STRTRIM(String(bias),2),/normal
  xyouts,xlabout,ylabout-0.09, 'stdv='+STRTRIM(String(std),2),/normal
  write_png, imgFile, TVRD(), r, g, b									     

END

;===============================================================
; Name:		PlotRRLat
;
;
; Type:		IDL Subroutine
;
;
; Description:  Plots the Daily Average Rain Rate as a Function of Latitude. 
;               
; Subroutines needed:
;               None
;
; History:
;    10-20-2008      Favio Iturbide-Sanchez, IMSG Inc @ NOAA/NESDIS/STAR      
;
;===============================================================
PRO  PlotRRLat, minx,maxx,miny,maxy,x_ticks,y_ticks,xlabout,ylabout,xval,yval,max_xval,max_yval, $
                xtitle,ytitle,title,imgFile

  set_plot,'Z'
  device,set_resolution=[650, 500]

  TVLCT, r, g, b, /get
  r(0)=255 & g(0)=255 & b(0)=255
  r(255)=0 & g(255)=0 & b(255)=0

  r(7)=0 & g(7)=0 & b(7)=255
  r(8)=255 & g(8)=0 & b(8)=0
  
  plot,[minx, maxx], [miny, maxy], xrange=[minx, maxx], yrange=[miny, maxy], $
	  /nodata, Xticks=x_ticks, Yticks=y_ticks, charsize=0.98, $
	  xTitle=xtitle, yTitle=ytitle, title=title,YTICKLAYOUT=0,YTICKINTERVAL=15
  oplot, xval,yval, linestyle=0,color=7,thick=1.0
  xyouts,0.58,0.91, 'Max. Rainfall (mm/day)='+STRTRIM(String(max_xval),2),/normal
  xyouts,0.58,0.88, 'Lat='+STRTRIM(String(max_yval),2),/normal
  write_png, imgFile, TVRD(), r, g, b									     

END



;===============================================================
; Name:		Plot2RRLat
;
;
; Type:		IDL Subroutine
;
;
; Description:  Plots the Daily Average Rain Rate as a Function of
;               Latitude of two Precipitation Estimates
;               
; Subroutines needed:
;               None
;
; History:
;    12-03-2008      Favio Iturbide-Sanchez, IMSG Inc @ NOAA/NESDIS/STAR      
;
;===============================================================
PRO  Plot2RRLat, minx,maxx,miny,maxy,x_ticks,y_ticks,labout1,labout2, $
                 x1val,y1val,x2val,y2val,max_x1val,max_y1val,max_x2val, $
                 max_y2val,xtitle,ytitle,title,imgFile

  set_plot,'Z'
  device,set_resolution=[650, 500]

  TVLCT, r, g, b, /get
  r(0)=255 & g(0)=255 & b(0)=255
  r(255)=0 & g(255)=0 & b(255)=0

  r(7)=0 & g(7)=0 & b(7)=255
  r(8)=255 & g(8)=0 & b(8)=0
  
  plot,[minx, maxx], [miny, maxy], xrange=[minx, maxx], yrange=[miny, maxy], $
	  /nodata, Xticks=x_ticks, Yticks=y_ticks, charsize=0.98, $
	  xTitle=xtitle, yTitle=ytitle, title=title,YTICKLAYOUT=0,YTICKINTERVAL=15
  oplot, x1val,y1val, linestyle=0,color=7,thick=1.0
  oplot, x2val,y2val, linestyle=3,color=8,thick=1.0
  oplot, [9.0,9.9], [81.0,81.0],color=7,linestyle=0,thick=3.0
  oplot, [9.0,9.9], [73.0,73.0],color=8,linestyle=3,thick=3.0
  xyouts,0.83,0.90, labout1, charsize=1.2,charthick=1.0,/normal
  xyouts,0.83,0.86, labout2, charsize=1.2,charthick=1.0,/normal
  xyouts,0.56,0.22, 'Max. RR MIRS (mm/day)='+STRTRIM(String(max_x1val),2),/normal
  xyouts,0.56,0.19, 'Lat='+STRTRIM(String(max_y1val),2),/normal
  xyouts,0.56,0.15, 'Max. RR MSPPS (mm/day)='+STRTRIM(String(max_x2val),2),/normal
  xyouts,0.56,0.12, 'Lat='+STRTRIM(String(max_y2val),2),/normal
  write_png, imgFile, TVRD(), r, g, b									     

END


;===============================================================
; Name:		Plot3HistGeoLog
;
;
; Type:		IDL Subroutine
;
;
; Description:  Plots a histogram.
;
; History:
;       12-08-2008      Favio Iturbide-Sanchez, IMSG Inc @ NOAA/NESDIS/STAR
;
;===============================================================

PRO Plot3HistGeoLog,geo1,geo2,geo3,geo1label,geo2label,geo3label,xtitle,ytitle,title,imgFile

;---- Device Set Up
set_plot,'Z'
device,set_resolution=[650, 500]
position=[0.05, 0.15, 0.95, 0.9]
;---- Load Color Table
loadct, 33, /silent
TVLCT, r, g, b, /get
r(0)=255   & g(0)=255   & b(0)=255   	; Load White Color - 0 for background 
r(255)=0   & g(255)=0   & b(255)=0 	; Load Black Color - 255
r(254)=255 & g(254)=255 & b(254)=255	; Load White Color - 254 for Missing Data
r(253)=215 & g(253)=215 & b(253)=215	; Load Grey Color (20%) - 253 for Land/Ocean Coverage
r(252)=98  & g(252)=98  & b(252)=98	; Load Grey Color (70%) - 252 for non-convergent points(qc fail)
drawColor = 255

binsize=0.2
hist_mirs=HISTOGRAM(float(geo1),binsize=binsize,location=loc1)
hist_nwp2=HISTOGRAM(float(geo2),binsize=binsize,locations=loc2)
hist_nwp3=HISTOGRAM(float(geo3),binsize=binsize,locations=loc3)
hist_mirs=100*hist_mirs/float(N_ELEMENTS(geo1))
hist_nwp2=100*hist_nwp2/float(N_ELEMENTS(geo2))
hist_nwp3=100*hist_nwp3/float(N_ELEMENTS(geo3))

xrange=[1e-3,10.0]
yrange=[1e-2,100.0]
dumx=[1,2] & dumy=[1,2]
PLOT, dumx, dumy, XTITLE=xtitle, YTITLE=ytitle,XRANGE=xrange, YRANGE=yrange,TITLE=title, $
  CHARSIZE=0.95,/YLOG,/NODATA
OPLOT, loc1, hist_mirs, PSYM = 10,color=50,linestyle=3,thick=3
OPLOT, loc2, hist_nwp2, PSYM = 10,color=210,linestyle=3,thick=3
OPLOT, loc3, hist_nwp3, PSYM = 10,color=138,linestyle=3,thick=3
OPLOT, [7.0,8.0], [65.0,65.0],color=50,linestyle=3,thick=3.0
XYOUTS, 0.83,0.90,geo1label, charsize=1.2,charthick=1.0,/normal
OPLOT, [7.0,8.0], [45.0,45.0], color=210,linestyle=3,thick=3.0
XYOUTS, 0.83,0.86,geo2label, charsize=1.2,charthick=1.0,/normal
OPLOT, [7.0,8.0], [30.0,30.0], color=138,linestyle=3,thick=3.0
XYOUTS, 0.83,0.83,geo3label, charsize=1.2,charthick=1.0,/normal
;---- Write Out Image File
write_png,imgFile, TVRD(), r, g, b
END


;===============================================================
; Name:		Plot3RRLat
;
;
; Type:		IDL Subroutine
;
;
; Description:  Plots the Daily Average Rain Rate as a Function of
;               Latitude of three Precipitation Estimates
;               
; Subroutines needed:
;               None
;
; History:
;    12-09-2008      Favio Iturbide-Sanchez, IMSG Inc @ NOAA/NESDIS/STAR      
;
;===============================================================
PRO  Plot3RRLat, minx,maxx,miny,maxy,x_ticks,y_ticks,labout1,labout2,labout3, $
                 x1val,y1val,x2val,y2val,x3val,y3val,xtitle,ytitle,title,imgFile

  set_plot,'Z'
  device,set_resolution=[650, 500]

  loadct, 33, /silent
  TVLCT, r, g, b, /get
  r(0)=255   & g(0)=255   & b(0)=255 ; Load White Color - 0 for background 
  r(255)=0   & g(255)=0   & b(255)=0 ; Load Black Color - 255
  r(254)=255 & g(254)=255 & b(254)=255 ; Load White Color - 254 for Missing Data
  r(253)=215 & g(253)=215 & b(253)=215 ; Load Grey Color (20%) - 253 for Land/Ocean Coverage
  r(252)=98  & g(252)=98  & b(252)=98 ; Load Grey Color (70%) - 252 for non-convergent points(qc fail)
  drawColor = 255

  
  plot,[minx, maxx], [miny, maxy], xrange=[minx, maxx], yrange=[miny, maxy], $
	  /nodata, Xticks=x_ticks, Yticks=y_ticks, charsize=0.98, $
	  xTitle=xtitle, yTitle=ytitle, title=title,YTICKLAYOUT=0,YTICKINTERVAL=15
  oplot, x1val,y1val, linestyle=3,color=50,thick=3.0
  oplot, x2val,y2val, linestyle=3,color=210,thick=3.0
  oplot, x3val,y3val, linestyle=3,color=138,thick=3.0
  oplot, [0.90,0.99], [81.0,81.0],color=50,linestyle=3,thick=3.0
  oplot, [0.90,0.99], [73.0,73.0],color=210,linestyle=3,thick=3.0
  oplot, [0.90,0.99], [65.0,65.0],color=138,linestyle=3,thick=3.0
  xyouts,0.83,0.90, labout1, charsize=1.2,charthick=1.0,/normal
  xyouts,0.83,0.86, labout2, charsize=1.2,charthick=1.0,/normal
  xyouts,0.83,0.82, labout3, charsize=1.2,charthick=1.0,/normal
  write_png, imgFile, TVRD(), r, g, b									     

END


;======================================================================================
; Name:		Plot_climate
;
; Type:		IDL Subroutine
;
; Description:  Plots the Climate products. If have a color table provided,
;               then use it; otherwise, use system color table 33 as other
;               mirs images generated from gridded level 3 data.
;               
; Subroutines needed:
;               colorbar
;
; History:
;    02-11-2009      Wanchun Chen, PSGS @ NOAA/NESDIS/STAR      
;
;======================================================================================
Pro plot_climate,arr,title,xtitle,ytitle,png_file,latmin,latmax,lonmin,lonmax,   $
    latitude,longitude,position,xsize,ysize,xstart,ystart,minval,maxval,div,fmt, $
    color_table_index=color_table_index

  set_plot, 'Z'

  IF N_ELEMENTS(COLOR_TABLE_INDEX) EQ 1 THEN BEGIN
  
    loadct_climate, r, g, b
    TVLCT, r, g, b
  
  ENDIF ELSE BEGIN
  
    loadct, 33, /silent
    TVLCT, r, g, b, /get

  ENDELSE


  char_size=0.75

  r(0)=255   & g(0)=255   & b(0)=255   	; Load White Color - 0 for background 
  r(255)=0   & g(255)=0   & b(255)=0    ; Load Black Color - 255
  r(254)=255 & g(254)=255 & b(254)=255	; Load White Color - 254 for Missing Data
  r(253)=215 & g(253)=215 & b(253)=215	; Load Grey Color (20%) - 253 for Land/Ocean Coverage
  r(252)=98  & g(252)=98  & b(252)=98	; Load Grey Color (70%) - 252 for non-convergent points(qc fail)
  drawColor = 255

  ;---- scaling
  image=bytscl(arr, min=minval, max=maxval, top=239) + 1B

  ;---- color adjustment
  info=size(arr)
  NCOL=info[1]
  NROW=info[2]

  for icol=0, NCOL-1 do begin
  for irow=0, NROW-1 do begin
    if ( minval gt 0 and arr(icol,irow) ge 0 and arr(icol,irow) lt minval ) then image(icol,irow) = 1
    if arr(icol,irow) eq  -99.0 then image(icol,irow) = 252
    if arr(icol,irow) eq -999.0 then image(icol,irow) = 0
  endfor
  endfor

  ;---- map setup
  map_set, /cyl, limit=[latmin,lonmin,latmax,lonmax], position=position, $
	  title=title, color=drawColor, charsize=char_size, /NOBORDER  
  warp=map_image(image,xx,yy,xs,ys,compress=1)
  TV,warp,xx,yy,xsize=xs,ysize=ys

  MAP_GRID, LatDel=15, LonDel=30, color=drawColor
  MAP_CONTINENTS, /CONT, /COUNTRIES, /USA, COLOR=drawColor

  ticks_lon=FIX((lonmax-lonmin)/30)
  ticks_lat=FIX((latmax-latmin)/15)

  longitude = lonmin + findgen(ticks_lon+1)*30
  latitude  = latmin + findgen(ticks_lat+1)*15

  ;---- plot X-Y axis
  Plot, longitude, latitude, XStyle=1, YStyle=1, Xticks=ticks_lon, Yticks=ticks_lat, $
	POSITION=position, xrange=[lonmin,lonmax], yrange=[latmin,latmax], $
	Color=drawColor, Charsize=char_size, /NoData, /NoErase

  ;---- Plot color bar
  position_bar=fltarr(4)
  position_bar[0]=position[0]+0.15
  position_bar[1]=position[1]-0.1
  position_bar[2]=position[2]-0.15
  position_bar[3]=position_bar[1]+0.03
  ColorBar,Bottom=1,NColors=240,Color=drawColor,Position=position_bar,Format=fmt,$
	  Divisions=div,Minor=1,TickLen=0.0001,Range=[minval,maxval],CharSize=0.75

  ;---- Plot Missing lengend (white)
  mis_position=fltarr(4)
  mis_position[0]=position[0]+0.075
  mis_position[1]=position[1]-0.1
  mis_position[2]=mis_position[0]+0.05
  mis_position[3]=mis_position[1]+0.03
  ;ColorBar,Bottom=0,NColors=1, Position=mis_position, Divisions=2, COLOR=drawColor,$
  ;	  Minor=1,TickLen=0.0001,TickNames=[' ','NoData',' '],CharSize=0.75

  ;---- Plot QC fail lengend (gray)
  qc_position=fltarr(4)
  qc_position[0]=position[0]+0.075
  qc_position[1]=position[1]-0.1
  qc_position[2]=position[0]+0.125
  qc_position[3]=qc_position[1]+0.03
  ;ColorBar,Bottom=252,NColors=1, Position=qc_position, Divisions=2, COLOR=drawColor,$
  ;	Minor=1,TickLen=0.0001,TickNames=[' ','QC fail',' '],CharSize=0.75

  ;---- write out image file
  write_png, png_file, TVRD(), r, g, b


END


;===============================================================
; Name:		Plot_sfc_grid
;
; Type:		IDL Subroutine
;
; Description:  Plots the 4 surface types defined in mirs:
;
;		0 : ocean   - blue
;		1 : sea ice - yellow
;               2 : land    - green
;		3 : snow    - white
;
; Subroutines needed:
;               none
;
; History:
;    02-11-2009      Wanchun Chen, PSGS @ NOAA/NESDIS/STAR      
;
;===============================================================
Pro plot_sfc_grid,tmp,png_file,minvalue,maxvalue,latmin,latmax,lonmin,lonmax,title

;---- device set up
set_plot,'Z'
device,set_resolution=[650, 500]
position=[0.05, 0.15, 0.95, 0.9]

;---- load color table
loadct, 33, /SILENT
TVLCT, r, g, b, /get

drawColor = 255

for i = 0, 249 do begin
    r(i)=255 & g(i)=255 & b(i)=255 
endfor

r(250)=0    & g(250)=0   & b(250)=0     ; black  - 250
r(251)=98   & g(251)=98  & b(251)=98	; gray   - 251
r(252)=50   & g(252)=205 & b(252)=50    ; green  - 252
r(253)=255  & g(253)=255 & b(253)=0 	; yellow - 253
r(254)=0    & g(254)=0   & b(254)=255   ; blue   - 254
r(255)=0    & g(255)=0   & b(255)=0     ; Black  - 255

;---- color adjustment
info=size(tmp)
NCOL=info[1]
NROW=info[2]

image = bytarr(NCOL,NROW)

for icol=0, NCOL-1 do begin
for irow=0, NROW-1 do begin
  if FIX(tmp(icol,irow)) eq -999 then image(icol,irow) = 250 ; missing - black
  if FIX(tmp(icol,irow)) eq -99  then image(icol,irow) = 251 ; qc fail - gray
  if FIX(tmp(icol,irow)) eq 0    then image(icol,irow) = 254 ; ocean - blue 
  if FIX(tmp(icol,irow)) eq 1    then image(icol,irow) = 253 ; ice  - yellow
  if FIX(tmp(icol,irow)) eq 2    then image(icol,irow) = 252 ; land - green
  if FIX(tmp(icol,irow)) eq 3    then image(icol,irow) = 249 ; snow - white 
endfor
endfor


;---- map setup
map_set, /cyl, limit=[latmin,lonmin,latmax,lonmax], position=position, $
	title=title, color=drawColor, charsize=0.8, /NOBORDER  
warp=map_image(image,xx,yy,xs,ys,compress=1)
TV,warp,xx,yy,xsize=xs,ysize=ys

MAP_GRID, LatDel=15, LonDel=30, color=drawColor
MAP_CONTINENTS, /CONT, /COUNTRIES, /USA, COLOR=drawColor ;, /HIRES

ticks_lon=FIX((lonmax-lonmin)/30)
ticks_lat=FIX((latmax-latmin)/15)

longitude = lonmin + findgen(ticks_lon+1)*30
latitude  = latmin + findgen(ticks_lat+1)*15

;---- plot X-Y axis
Plot, longitude, latitude, XStyle=1, YStyle=1, Xticks=ticks_lon, Yticks=ticks_lat, $
      POSITION=position, xrange=[lonmin,lonmax], yrange=[latmin,latmax], $
      Color=drawColor, Charsize=0.8, /NoData, /NoErase


;---- draw color bars
 
bar_position=fltarr(4)

bar_position[1] = 0.05
bar_position[3] = 0.08

;---- Plot Missing lengend (black) - 250
bar_position[0]=0.05
bar_position[2]=0.20
ColorBar,Bottom=250,NColors=1, Position=bar_position, Divisions=2, COLOR=255,$
         Minor=1,TickLen=0.0001,TickNames=[' ','no data',' '],CharSize=0.8

;---- Plot QC fail lengend (gray) - 251
bar_position[0]=0.20
bar_position[2]=0.35
ColorBar,Bottom=251,NColors=1, Position=bar_position, Divisions=2, COLOR=255,$
	 Minor=1,TickLen=0.0001,TickNames=[' ','qc fail',' '],CharSize=0.8

;---- Plot Ocean lengend (blue) - 254
bar_position[0]=0.35
bar_position[2]=0.50
ColorBar,Bottom=254,NColors=1, Position=bar_position, Divisions=2, COLOR=255,$
        Minor=1,TickLen=0.0001,TickNames=[' ','ocean',' '],CharSize=0.8

;---- Plot Ice lengend (yellow) - 253
bar_position[0]=0.50
bar_position[2]=0.65
ColorBar,Bottom=253,NColors=1, Position=bar_position, Divisions=2, COLOR=255,$
        Minor=1,TickLen=0.0001,TickNames=[' ','ice',' '],CharSize=0.8

;---- Plot land lengend (green) - 252
bar_position[0]=0.65
bar_position[2]=0.80
ColorBar,Bottom=252,NColors=1, Position=bar_position, Divisions=2, COLOR=255,$
        Minor=1,TickLen=0.0001,TickNames=[' ','land',' '],CharSize=0.8

;---- Plot snow lengend (white) - 249
bar_position[0]=0.80
bar_position[2]=0.95
ColorBar,Bottom=249,NColors=1, Position=bar_position, Divisions=2, COLOR=255,$
        Minor=1,TickLen=0.0001,TickNames=[' ','snow',' '],CharSize=0.8

;---- write out image file
write_png, png_file, TVRD(), r, g, b

device,/close
end


;===============================================================
; Name:		Plot_sfc_polar
;
; Type:		IDL Subroutine
;
; Description:  Plots the 4 surface types in polar plots
;
;		0 : ocean   - blue
;		1 : sea ice - yellow
;               2 : land    - green
;		3 : snow    - white
;
; Subroutines needed:
;               none
;
; History:
;    02-11-2009      Wanchun Chen, PSGS @ NOAA/NESDIS/STAR      
;
;===============================================================
Pro plot_sfc_polar,tmp,png_file,minvalue,maxvalue,latmin,latmax,lonmin,lonmax,$
    centrlat,orientlon,xlat,xlon,title,symsize

;---- device set up
set_plot,'Z'
device,set_resolution=[650, 500]
position=[0.05, 0.10, 0.90, 0.95]

;---- load color table
loadct, 39, /SILENT
TVLCT, r, g, b, /get

for i=0,249 do begin
  r(i)=255 & g(i)=255 & b(i)=255
endfor

r(250)=0    & g(250)=0   & b(250)=0     ; black
r(251)=98   & g(251)=98  & b(251)=98	; gray
r(252)=50   & g(252)=205 & b(252)=50    ; green
r(253)=255  & g(253)=255 & b(253)=0 	; yellow
r(254)=0    & g(254)=0   & b(254)=255   ; blue
r(255)=0    & g(255)=0   & b(255)=0     ; black

drawColor  = 255
blackColor = 250

;---- color adjustment
nrec=n_elements(tmp)
;---- scaling
image=bytscl(tmp, min=minvalue, max=maxvalue, top=250)
image(*) = 250B

for iprof=0L, nrec-1 do begin
  if FIX(tmp(iprof)) eq -999 then image(iprof)  = 250 ; missing - black
  if FIX(tmp(iprof)) eq -99  then image(iprof)  = 251 ; qc fail - gray
  if FIX(tmp(iprof)) eq 0    then image(iprof)  = 254 ; ocean - blue
  if FIX(tmp(iprof)) eq 1    then image(iprof)  = 253 ; sea ice - yellow
  if FIX(tmp(iprof)) eq 2    then image(iprof)  = 252 ; land - green
  if FIX(tmp(iprof)) eq 3    then image(iprof)  = 249 ; snow - white
endfor

; defining user symbol for plotting values
csize = 16.0
scal  = 0.8
a = findgen(csize+1) * (!PI*2.5/float(csize))
usersym,scal*cos(a)/2,scal*sin(a)/2,/fill

; defining latitude grid depending on projection
if (centrlat gt 0.0) then lats =[0,15,30,45,60,75,90]
if (centrlat lt 0.0) then lats =[0,-15,-30,-45,-60,-75,-90]
lons=[-180,-135,-90,-45,0,45,90,135,180]

;---- map setup
MAP_SET, centrlat, 0, orientlon, /Stereo, /GRID, /Isotropic, /Horizon, $
	 E_GRID={LABEL:2},position=position,title=title,color=blackColor,$
	 charsize=0.75,/NOBORDER,limit=[latmin,lonmin,latmax,lonmax]


; Plot values on map
nc=250
FOR iprof=0L,nrec-1 DO BEGIN
    oplot,[xlon[iprof]],[xlat[iprof]],color=image[iprof],psym=8,symsize=symsize,thick=0.5
ENDFOR

MAP_GRID, LABEL=1, LATS=lats, LATLAB=-25, LONS=lons,  LONLAB=5, GLINESTYLE=0, color=blackColor
MAP_CONTINENTS, /CONTINENTS, /COUNTRIES, /USA, COLOR=blackColor

;*********************************************************
; Draw color bars
;*********************************************************
bar_position=fltarr(4)
adjust = -0.025
bar_position[1] = 0.05
bar_position[3] = 0.08

;---- Plot Missing lengend (black) - 250
bar_position[0]=0.20+adjust
bar_position[2]=0.30+adjust
ColorBar,Bottom=250,NColors=1, Position=bar_position, Divisions=2, COLOR=255,$
         Minor=1,TickLen=0.0001,TickNames=[' ','no data',' '],CharSize=0.8

;---- Plot QC fail lengend (gray) - 251
bar_position[0]=0.30+adjust
bar_position[2]=0.40+adjust
ColorBar,Bottom=251,NColors=1, Position=bar_position, Divisions=2, COLOR=255,$
	 Minor=1,TickLen=0.0001,TickNames=[' ','qc fail',' '],CharSize=0.8

;---- Plot Ocean lengend (blue) - 254
bar_position[0]=0.40+adjust
bar_position[2]=0.50+adjust
ColorBar,Bottom=254,NColors=1, Position=bar_position, Divisions=2, COLOR=255,$
        Minor=1,TickLen=0.0001,TickNames=[' ','ocean',' '],CharSize=0.8

;---- Plot Ice lengend (yellow) - 253
bar_position[0]=0.50+adjust
bar_position[2]=0.60+adjust
ColorBar,Bottom=253,NColors=1, Position=bar_position, Divisions=2, COLOR=255,$
        Minor=1,TickLen=0.0001,TickNames=[' ','ice',' '],CharSize=0.8

;---- Plot land lengend (green) - 252
bar_position[0]=0.60+adjust
bar_position[2]=0.70+adjust
ColorBar,Bottom=252,NColors=1, Position=bar_position, Divisions=2, COLOR=255,$
        Minor=1,TickLen=0.0001,TickNames=[' ','land',' '],CharSize=0.8

;---- Plot snow lengend (white) - 249
bar_position[0]=0.70+adjust
bar_position[2]=0.80+adjust
ColorBar,Bottom=249,NColors=1, Position=bar_position, Divisions=2, COLOR=255,$
        Minor=1,TickLen=0.0001,TickNames=[' ','snow',' '],CharSize=0.8

;---- write out image file
write_png, png_file, TVRD(), r, g, b

end



;===================================================================================================
;  Name: biasAsym2D.pro
;
;  Purpose:
;    To compute and plot bias and bias asymmetry of 2-D data sets.(tpw,tskin,clw,rr,etc)
;    The input is MIRS 3rd level gridded data set.
;
;  Dependence:
;    plot_grid.pro
;    plot_line.pro
;    plot_line2.pro
;
;  Record of revisions:
;        Date          Programmer     	Description of change
;    ============    ==============    =====================================
;     02/18/09         Wanchun Chen     Original Code
;
;===================================================================================================
Pro biasAsym2D, prodId,prodTxt,unit,prod1,prod2,satId1,satId2,yyyymmdd,cend,$
    angleArr,sfcArr1,sfcArr2,sfcMask,minvalue,maxvalue,minvalue_bias,maxvalue_bias,div,fmt,$
    NCOL,NROW,NBIN,LWP_CUT,RWP_CUT,figsDir,prefix_bias,prefix_asym,version,$
    latmin,latmax,lonmin,lonmax,timediff,lwp1,lwp2,rwp1,rwp2
  
MISSING_FLT = -999.0
MISSING_INT = -999
CONT_MIN    = 200L

BIN_BOX  = 5 * indgen(NBIN+1) - 60
BIN_BOX2 = 5 * indgen(NBIN)   - 57.5
if satId1 eq 'f16' or satId1 eq 'f17' or satId1 eq 'f18' then begin
  BIN_BOX  = indgen(NBIN+1)+1
  BIN_BOX2 = indgen(NBIN)+1
endif

time_delta = 1.0 ; 1.0 means 24 hour difference
    
;---- bias computation ----
bias=fltarr(NCOL,NROW) & bias(*,*) = MISSING_FLT

if prodId eq 'scanday' then begin
  for irow = 0,NROW-1 do begin 
  for icol = 0,NCOL-1 do begin
      if ( prod1(icol,irow) ge 0 and prod2(icol,irow) ge 0 ) then $
        bias(icol,irow) = ( prod1(icol,irow) - prod2(icol,irow) ) * 24.0
  endfor
  endfor
endif

if prodId eq 'angle' then begin
  for irow = 0,NROW-1 do begin 
  for icol = 0,NCOL-1 do begin
      if ( ABS(prod1(icol,irow)) lt 90 and ABS(prod2(icol,irow)) lt 90 ) $
        then bias(icol,irow) = prod1(icol,irow) - prod2(icol,irow)
  endfor
  endfor
endif

if prodId eq 'scanpos' then begin
  for irow = 0,NROW-1 do begin 
  for icol = 0,NCOL-1 do begin
      if ( prod1(icol,irow) ge  1 and prod2(icol,irow) ge  1 and $
           prod1(icol,irow) le 30 and prod2(icol,irow) le 30 ) then $
           bias(icol,irow) = prod1(icol,irow) - prod2(icol,irow)
  endfor
  endfor
endif

;---- over sea and over land use a different filter ( lwp-sea/ice, rwp-land/snow ) ----
if prodId eq 'gs'  or prodId eq 'sice' or prodId eq 'sicefy' or prodId eq 'sicemy' or $
   prodId eq 'swe' or prodId eq 'tpw'  or prodId eq 'tskin' then begin
  for irow = 0,NROW-1 do begin 
  for icol = 0,NCOL-1 do begin
   
    if ( prod1(icol,irow) ge 0.0 and prod2(icol,irow) ge 0.0 and $
         prod1(icol,irow) le 2000.0 and prod2(icol,irow) le 2000.0 and $
         timediff(icol,irow) le time_delta and $
         sfcArr1(icol,irow) le 1 and sfcArr2(icol,irow) le 1 and $
         lwp1(icol,irow) le LWP_CUT and  lwp2(icol,irow) le LWP_CUT ) then $
         bias(icol,irow) = prod1(icol,irow) - prod2(icol,irow)
   
    if ( prod1(icol,irow) ge 0.0 and prod2(icol,irow) ge 0.0 and $
         prod1(icol,irow) le 2000.0 and prod2(icol,irow) le 2000.0 and $
         timediff(icol,irow) le time_delta and $
         sfcArr1(icol,irow) ge 2 and sfcArr2(icol,irow) ge 2 and $
         rwp1(icol,irow) le RWP_CUT and  rwp2(icol,irow) le RWP_CUT ) then $
         bias(icol,irow) = prod1(icol,irow) - prod2(icol,irow)

  endfor
  endfor
endif

if prodId eq 'clw' or prodId eq 'iwp' or prodId eq 'lwp'  or prodId eq 'psfc' or $
   prodId eq 'rr'  or prodId eq 'rwp' or prodId eq 'snow' then begin
  
  for irow = 0,NROW-1 do begin 
  for icol = 0,NCOL-1 do begin
    if ( prod1(icol,irow) ge 0.0 and prod2(icol,irow) ge 0.0 and $
         prod1(icol,irow) le 2000.0 and prod2(icol,irow) le 2000.0 and $
         timediff(icol,irow) le time_delta ) then $
         bias(icol,irow) = prod1(icol,irow) - prod2(icol,irow)
  endfor
  endfor
  
endif


if cend eq 'as' then cendTxt = ' Asc '
if cend eq 'ds' then cendTxt = ' Des '

date=strmid(yyyymmdd,0,4) + '-' + strmid(yyyymmdd,4,2) + '-' + strmid(yyyymmdd,6,2)

;---- bias grid ----
sfcIds=['sea', 'lnd', 'all']
sfcTxts=['Sea','Land', 'All']
nsfc = N_ELEMENTS(sfcIds)

for sfcPick = 0, 2 do begin
  sfcId = sfcIds[sfcPick]
  sfcTxt = sfcTxts[sfcPick]
  png_file = figsDir+prefix_bias+yyyymmdd +'_'+prodId+'_'+sfcId+'_'+cend+'.png'
  title = 'MIRS '+strupcase(satId1)+' - '+strupcase(satId2) +' '+prodTxt+$
          unit+' '+date+cendTxt+'(V'+version +')'
  plot_grid,bias,sfcMask,png_file,minvalue_bias,maxvalue_bias,latmin,latmax,lonmin,lonmax,title,sfcPick,div,fmt
endfor

;---- mean bias ( scan angle or scan position ) dependency ----
sfcIds=['sea', 'ice', 'lnd',  'snw', 'all']
sfcTxts=['Sea','Ice', 'Land', 'Snw', 'All']
nsfc = N_ELEMENTS(sfcIds)

asyms = fltarr(NBIN,nsfc) & asyms(*,*) = MISSING_FLT
stdvs = fltarr(NBIN,nsfc) & stdvs(*,*) = MISSING_FLT

for ibin = 0, NBIN - 1 do begin
  
  for isfc = 0, nsfc-1 do begin

    if isfc lt nsfc-1 then begin
      ss = where( angleArr ge BIN_BOX(ibin) and angleArr lt BIN_BOX(ibin+1) and bias gt -999.0 and sfcArr1 eq isfc and sfcArr2 eq isfc, cnt)
    endif else begin
      ss = where( angleArr ge BIN_BOX(ibin) and angleArr lt BIN_BOX(ibin+1) and bias gt -999.0, cnt)
    endelse

    if cnt gt CONT_MIN then begin
      asyms(ibin,isfc) = MEAN(bias(ss))
      stdvs(ibin,isfc) = STDDEV(bias(ss))
    endif 
     
  endfor 

endfor

;---- plot angle asymmetry ---- 
xrange=[-60,60]
xtitle='Local Zenith Angle (degree)'
if satId1 eq 'f16' or satId1 eq 'f17' or satId1 eq 'f18'  then begin
  xrange=[0,31]
  xtitle='Scan Position'
endif

yrange=[minvalue_bias,maxvalue_bias]

;-- all
lndsea='all'
ytitle=prodTxt + ' Bias ' + unit + ' All'
title = 'MIRS ' + strupcase(satId1) + ' - ' + strupcase(satId2) + prodTxt + $
        date + cendTxt + ' (V' + version +')'
png_file=figsDir+prefix_asym+yyyymmdd+'_'+prodId+'_'+lndsea+'_'+cend+'.png'
plot_line,BIN_BOX2,asyms(*,4),xtitle,ytitle,title,xrange,yrange,png_file,stdv=stdvs(*,4)

;-- sea and ice
lndsea='sea'
ytitle=prodTxt + ' Bias ' + unit + ' Over Sea'
title = 'MIRS ' + strupcase(satId1) + ' - ' + strupcase(satId2) + prodTxt + $
        date + cendTxt + ' (V' + version +')'
png_file=figsDir+prefix_asym+yyyymmdd+'_'+prodId+'_'+lndsea+'_'+cend+'.png'
plot_line2, BIN_BOX2, asyms(*,0), asyms(*,1), xtitle, ytitle, title, xrange, yrange, $
            png_file, "Sea", "Sea Ice", stdv1=stdvs(*,0), stdv2=stdvs(*,1)

;-- land and snow
lndsea='lnd'
ytitle=prodTxt + ' Bias ' + unit + ' Over Land'
title = 'MIRS ' + strupcase(satId1) + ' - ' + strupcase(satId2) + prodTxt + $
        date + cendTxt + ' (V' + version +')'
png_file=figsDir+prefix_asym+yyyymmdd+'_'+prodId+'_'+lndsea+'_'+cend+'.png'
plot_line2, BIN_BOX2, asyms(*,2), asyms(*,3), xtitle, ytitle, title, xrange, yrange, $
            png_file, "Land", "Snow", stdv1=stdvs(*,2), stdv2=stdvs(*,3)

End


;===================================================================================================
;  Name: biasAsym3D.pro
;
;  Purpose:
;    To compute and plot bias and bias asymmetry of 3-D data sets.(em,temp,wv,ym,ymCorr)
;    The input is MIRS 3rd level gridded data set.
;
;  Dependence:
;    plot_grid.pro
;    plot_line.pro
;    plot_line2.pro
;
;  Record of revisions:
;        Date          Programmer     	Description of change
;    ============    ==============    =======================
;     02/18/09         Wanchun Chen     Original Code
;
;===================================================================================================
Pro biasAsym3D, prodId,prodTxt,unit,prod1,prod2,satId1,satId2,yyyymmdd,cend,$
    angleArr,sfcArr1,sfcArr2,sfcMask,minvalues,maxvalues,div,fmt,$
    NCOL,NROW,NBIN,LWP_CUT,RWP_CUT,figsDir,prefix_bias,prefix_asym,version,$
    latmin,latmax,lonmin,lonmax,timediff,lwp1,lwp2,rwp1,rwp2,NLAY,titles
    
MISSING_FLT = -999.0
MISSING_INT = -999
CONT_MIN    = 200L

BIN_BOX  = 5 * indgen(NBIN+1) - 60
BIN_BOX2 = 5 * indgen(NBIN)   - 57.5
if satId1 eq 'f16' or satId1 eq 'f17' or satId1 eq 'f18' then begin
  BIN_BOX  = indgen(NBIN+1) + 1
  BIN_BOX2 = indgen(NBIN)+1
endif

time_delta = 1.0 ; 1.0 means 24 hours difference

;---- bias computation ----
bias=fltarr(NCOL,NROW,NLAY) & bias(*,*,*) = MISSING_FLT

if prodId eq 'temp' or prodId eq 'wv' then begin
  
  for ilay = 0,NLAY - 1 do begin
  for irow = 0,NROW - 1 do begin 
  for icol = 0,NCOL - 1 do begin

      if ( prod1(icol,irow,ilay) ge 0.0 and prod2(icol,irow,ilay) ge 0.0 and $
           sfcArr1(icol,irow) le 1 and sfcArr2(icol,irow) le 1 and $ 
           timediff(icol,irow) le time_delta and $
           lwp1(icol,irow) le LWP_CUT and lwp2(icol,irow) le LWP_CUT ) then $
      	bias(icol,irow,ilay) = prod1(icol,irow,ilay) - prod2(icol,irow,ilay)
		
      if ( prod1(icol,irow,ilay) ge 0.0 and prod2(icol,irow,ilay) ge 0.0 and $
           sfcArr1(icol,irow) ge 2 and sfcArr2(icol,irow) ge 2 and $ 
           timediff(icol,irow) le time_delta  and $
           rwp1(icol,irow) le RWP_CUT and rwp2(icol,irow) le RWP_CUT ) then $
      	bias(icol,irow,ilay) = prod1(icol,irow,ilay) - prod2(icol,irow,ilay)

  endfor
  endfor
  endfor

endif else begin

  for ilay = 0,NLAY - 1 do begin
  for irow = 0,NROW - 1 do begin 
  for icol = 0,NCOL - 1 do begin
      if ( prod1(icol,irow,ilay) ge 0.0 and prod2(icol,irow,ilay) ge 0.0 and $
           timediff(icol,irow) le time_delta ) then $
      bias(icol,irow,ilay) = prod1(icol,irow,ilay) - prod2(icol,irow,ilay)
  endfor
  endfor
  endfor

endelse


if cend eq 'as' then cendTxt = ' Asc '
if cend eq 'ds' then cendTxt = ' Des '

date=strmid(yyyymmdd,0,4) + '-' + strmid(yyyymmdd,4,2) + '-' + strmid(yyyymmdd,6,2)

;---- bias plot ----
sfcIds=['sea','lnd', 'all']
sfcTxts=['Sea','Land', 'All']

for sfcPick = 0, 2 do begin

  sfcId = sfcIds[sfcPick]
  sfcTxt = sfcTxts[sfcPick]

  for ilay = 0, NLAY - 1 do begin

    tmp=bias(*,*,ilay)
    minvalue=minvalues(ilay)
    maxvalue=maxvalues(ilay)
    
    png_file = figsDir + prefix_bias + yyyymmdd +'_' + prodId + $
               '_' + titles[ilay] + '_'+sfcId+'_' + cend + '.png'
    title = 'MIRS ' + strupcase(satId1) + ' - ' + strupcase(satId2) + $
            prodTxt + unit + ' @ ' + titles[ilay] + ' ' + $
            date + cendTxt + '(V' + version +')'
    plot_grid, tmp,sfcMask,png_file,minvalue,maxvalue,latmin,latmax,lonmin,lonmax,title,sfcPick,div,fmt   
  
  endfor
    
endfor
  
;---- mean bias angle dependency ----
sfcIds=['sea', 'ice', 'lnd',  'snw', 'all']
sfcTxts=['Sea','Ice', 'Land', 'Snw', 'All']
nsfc = N_ELEMENTS(sfcIds)

asyms = fltarr(NBIN,NLAY,NSFC) & asyms(*,*,*) = MISSING_FLT
stdvs = fltarr(NBIN,NLAY,NSFC) & stdvs(*,*,*) = MISSING_FLT

for ilay = 0, NLAY - 1 do begin 
for ibin = 0, NBIN - 1 do begin
for isfc = 0, NSFC - 1 do begin

    if isfc lt nsfc-1 then begin
      ss = where( angleArr ge BIN_BOX(ibin) and angleArr lt BIN_BOX(ibin+1) and bias gt MISSING_FLT and sfcArr1 eq isfc and sfcArr2 eq isfc, cnt)
    endif else begin
      ss = where( angleArr ge BIN_BOX(ibin) and angleArr lt BIN_BOX(ibin+1) and bias gt MISSING_FLT, cnt)
    endelse

    if cnt gt CONT_MIN then begin
      asyms(ibin,ilay,isfc) = MEAN(bias(ss))
      stdvs(ibin,ilay,isfc) = STDDEV(bias(ss))
    endif 
     
endfor 
endfor
endfor

;---- plot ( scan angle or scan position ) asymmetry ---- 
xrange=[-60,60]
xtitle='Local Zenith Angle (degree)'
if satId1 eq 'f16' or satId1 eq 'f17' or satId1 eq 'f18' then begin
  xrange=[0,31]
  xtitle='Scan Position'
endif

for ilay = 0, NLAY - 1 do begin

    yrange=[minvalues(ilay),maxvalues(ilay)]
    
    ;---all
    lndsea = 'all'
    ytitle = prodTxt +' Bias ' + unit + ' All'
    title = 'MIRS ' + strupcase(satId1) + ' - ' + strupcase(satId2) + $
            prodTxt + unit + ' @ ' + titles(ilay) + ' ' + $
            date + cendTxt + '(V' + version +')'
    png_file = figsDir+prefix_asym+yyyymmdd+'_'+prodId+'_'+$
               titles(ilay)+'_'+lndsea+'_'+cend+'.png'
    plot_line, BIN_BOX2, asyms(*,ilay,4), xtitle, ytitle, title, $
               xrange, yrange, png_file, stdv=stdvs(*,ilay,4)

    ;-- sea and ice
    lndsea = 'sea'
    ytitle = prodTxt +' Bias ' + unit + ' Over Sea'
    title = 'MIRS ' + strupcase(satId1) + ' - ' + strupcase(satId2) + $
            prodTxt + unit + ' @ ' + titles(ilay) + ' ' + $
            date + cendTxt + '(V' + version +')'
    png_file = figsDir+prefix_asym+yyyymmdd+'_'+prodId+'_'+$
               titles(ilay)+'_'+lndsea+'_'+cend+'.png'
    plot_line2, BIN_BOX2, asyms(*,ilay,0), asyms(*,ilay,1), $
                xtitle, ytitle, title, xrange, yrange, png_file, "Sea", $
                "Sea Ice", stdv1=stdvs(*,ilay,0), stdv2=stdvs(*,ilay,1)

    ;-- land and snow
    lndsea = 'lnd'
    ytitle = prodTxt +' Bias ' + unit + ' Over Land'
    title = 'MIRS ' + strupcase(satId1) + ' - ' + strupcase(satId2) + $
            prodTxt + unit + ' @ ' + titles(ilay) + ' ' + $
            date + cendTxt + '(V' + version +')'
    png_file = figsDir+prefix_asym+yyyymmdd+'_'+prodId+'_'+$
               titles(ilay)+'_'+lndsea+'_'+cend+'.png'
    plot_line2, BIN_BOX2, asyms(*,ilay,2), asyms(*,ilay,3), $
                xtitle, ytitle, title, xrange, yrange, png_file, "Land", $
                "Snow", stdv1=stdvs(*,ilay,2), stdv2=stdvs(*,ilay,3)
endfor

End


;===================================================================================================
;  Name: vertical_cross
;
;  Purpose:
;    To plot vertical cross section of hydrological products
;
;  Dependence:
;    plot_vert.pro
;
;  Record of revisions:
;        Date          Programmer     	Description of change
;    ============    ==============    ======================
;     04/06/09         Wanchun Chen     Original Code
;     11/13/09         Wanchun Chen     removed qc fail part below psfc
;
;===================================================================================================
Pro vertical_cross, NCOL,NROW,NLAY, gridfactor, gridDataDir, figsDir, $
    satId, yyyymmdd, date, prod_name, cend, psfc, div, fmt, minval, maxval, isMirs

  if prod_name eq 'temp'     then prod_title = ' Temperature (K) '
  if prod_name eq 'wv'       then prod_title = ' Water Vapor (g/kg) '
  if prod_name eq 'clwp'     then prod_title = ' CLW (mm) '
  if prod_name eq 'rainp'    then prod_title = ' Rain (mm) '
  if prod_name eq 'graupelp' then prod_title = ' Graupel (mm) '
  
  if cend eq 'as' then cendTxt = ' Asc '
  if cend eq 'ds' then cendTxt = ' Des '
  
  if isMirs eq 0 or isMirs eq 4 then begin
	nwpId=''
	nwpTitle=''
  endif
  
  if isMirs eq 1 then begin
	nwpId='_gdas'
	nwpTitle='GDAS Collo. '
  endif
  
  if isMirs eq 2 then begin
	nwpId='_ecmwf'
	nwpTitle='ECMWF Collo. '
  endif
  
  if isMirs eq 3 then begin
	nwpId='_gfs'
	nwpTitle='GFS Collo. '
  endif
  
  prefix='mirs_adv_poes_'+satId+nwpId+'_vert_glb'
  prefix_us='mirs_adv_poes_'+satId+nwpId+'_vert_us'

  if ( satId eq 'f16' or satId eq 'f17' or satId eq 'f18' ) then begin
    prefix='mirs_adv_dmsp_'+satId+nwpId+'_vert_glb'
    prefix_us='mirs_adv_dmsp_'+satId+nwpId+'_vert_us'
  endif
  if ( satId eq 'trmm' or satId eq 'gpm' ) then begin
    prefix='mirs_adv_eos_'+satId+nwpId+'_vert_glb'
    prefix_us='mirs_adv_eos_'+satId+nwpId+'_vert_us'
  endif
  if ( satId eq 'aqua' or satId eq 'fy3ri' ) then begin
    prefix='mirs_adv_eos_'+satId+nwpId+'_vert_glb'
    prefix_us='mirs_adv_eos_'+satId+nwpId+'_vert_us'
  endif
  if ( satId eq 'npp' ) then begin
    prefix='mirs_adv_npoess_'+satId+nwpId+'_vert_glb'
    prefix_us='mirs_adv_npoess_'+satId+nwpId+'_vert_us'
  endif
  if ( satId eq 'trmm2a12' ) then begin
    prefix='mirs_'+satId+'_vert_glb'
    prefix_us='mirs_'+satId+'_vert_us'
  endif
  
  titleSatId='MIRS ' + strupcase(satId)
  if ( satId eq 'aqua'      ) then titleSatId='MIRS ' + 'AMSR-E'
  if ( satId eq 'fy3ri'     ) then titleSatId='MIRS ' + 'FY3/MWRI'
  if ( satId eq 'trmm2a12'  ) then titleSatId='TRMM_2A12'
 
  ;***************************************************************
  ;   read in data
  ;***************************************************************
  fileGrid=gridDataDir+'GRID_'+satId+nwpId+'_'+yyyymmdd+'_'+prod_name+'_'+cend+'.dat'
  if FILE_TEST(fileGrid) eq 0 then begin
    print, 'Not exist:'+fileGrid
    RETURN
  endif
  
  prod=fltarr(NCOL,NROW,NLAY)
  tmp=fltarr(NCOL,NROW)
  openr,lun,fileGrid,/get_lun,/Swap_Endian
  for ilay=0, NLAY-1 do begin
    readu, lun, tmp
    prod(*,*,ilay) = tmp(*,*)
  endfor
  free_lun,lun

  ;***************************************************************
  ;   reverse z-direction
  ;***************************************************************
  dummy=prod
  for ilay=0, NLAY-1 do begin
    prod(*,*,ilay) = dummy(*,*,NLAY-1-ilay)  
  endfor
  UNDEFINE, dummy
   
  ytitle='Pressure (MB)'
  degree_sign=String(37B)
  
  yval=[1085,100]
  yrange=[1085,100]
  yticks=8
  ytickname=['1085','890','718','565','430','320','230','155','100']
  color_table=41
  ;---- only need those layers below 100mb(including 100mb)
  index_start = 43  ; averaged for many days, we found 100mb --> 43(index)

  if prod_name eq 'temp' then begin
     yval=[1085.0, 0.01]  
     yrange=[1085, 0.01]
     yticks=10
     ytickname=['1085', '815', '585', '400', '250', '150', '75', '30', '10', '1', '0.01']
     color_table=33
     index_start = 0
     ;if isMirs eq 1 then index_start = 21  ; gdas temp have values since 10mb(layer index 21 downward)
  endif

  NLAY_PROD = NLAY - index_start
  
  ;---- Globe --------------------------------------------------------------------------------------
  step=40
  if gridfactor eq 2 then step=20
  
  ;---- lat fixed ----
  xval=[-180,-150,-120,-90,-60,-30,0,30,60,90,120,150,180]
  xrange=[-180,180]
  xtitle='Longitude'
  xticks=N_elements(xval)-1
  lons = -180.0 + findgen(NCOL)/gridfactor

  for ilat=0, NROW-1, step do begin
    latval = ( ilat * 1.0 / gridfactor ) - 90.0
    latstr = strtrim(string(fix(latval)),2)
    title = nwpTitle + titleSatId + prod_title + ' on latitude ' + latstr + '!9' + degree_sign + '!X' + cendTxt + date
    map_name = figsDir + prefix + '_' + prod_name + '_' + yyyymmdd + '_lat_' + latstr + '_' + cend + '.png'
    sb=fltarr(360*gridfactor,NLAY_PROD)
    for ilay = 0, NLAY_PROD - 1 do begin
      sb(*,ilay) = prod(*,ilat,ilay)
    endfor
    plot_vert, sb, xval, yval, xrange, yrange, xtitle, ytitle, title, map_name, minval, maxval,$
               xticks, div, fmt, yticks, ytickname, lons, psfc(*,ilat), color_table, isMirs
  endfor

  ilat=NROW
  latval = ( ilat * 1.0 / gridfactor ) - 90.0
  latstr = strtrim(string(fix(latval)),2)
  title = nwpTitle + titleSatId + prod_title + ' on latitude ' + latstr + '!9' + degree_sign + '!X' + cendTxt + date
  map_name = figsDir + prefix + '_' + prod_name + '_' + yyyymmdd + '_lat_' + latstr + '_' + cend + '.png'
  sb=fltarr(360*gridfactor,NLAY_PROD)
  for ilay = 0, NLAY_PROD - 1 do begin
    sb(*,ilay) = prod(*,ilat-1,ilay)
  endfor
  plot_vert, sb, xval, yval, xrange, yrange, xtitle, ytitle, title, map_name, minval, maxval,$
             xticks, div, fmt, yticks, ytickname, lons, psfc(*,ilat-1), color_table, isMirs

  ;---- lon fixed ----
  xval=[-90,-60,-30,0,30,60,90]
  xrange=[-90,90]
  xtitle='Latitude'
  xticks=N_elements(xval)-1
  lats = -90 + findgen(NROW)/gridfactor

  for ilon=0, NCOL-1, step  do begin
    lonval = ( ilon * 1.0 / gridfactor ) - 180.0
    lonstr = strtrim(string(fix(lonval)),2)
    title = nwpTitle + titleSatId + prod_title + ' on longitude ' + lonstr + '!9' + degree_sign + '!X' + cendTxt + date
    map_name = figsDir + prefix + '_' + prod_name + '_' + yyyymmdd + '_lon_' + lonstr + '_' + cend + '.png'
    sb=fltarr(180*gridfactor,NLAY_PROD)
    for ilay = 0, NLAY_PROD - 1 do begin
      sb(*,ilay) = prod(ilon,*,ilay)
    endfor
    plot_vert, sb, xval, yval, xrange, yrange, xtitle, ytitle, title, map_name, minval, maxval,$
               xticks, div, fmt, yticks, ytickname, lats, psfc(ilon,*), color_table, isMirs
  endfor

  ilon=NCOL
  lonval = ( ilon * 1.0 / gridfactor ) - 180.0
  lonstr = strtrim(string(fix(lonval)),2)
  title = nwpTitle + titleSatId + prod_title + ' on longitude ' + lonstr + '!9' + degree_sign + '!X' + cendTxt + date
  map_name = figsDir + prefix + '_' + prod_name + '_' + yyyymmdd + '_lon_' + lonstr + '_' + cend + '.png'
  sb=fltarr(180*gridfactor,NLAY_PROD)
  for ilay = 0, NLAY_PROD - 1 do begin
    sb(*,ilay) = prod(ilon-1,*,ilay)
  endfor
  plot_vert, sb, xval, yval, xrange, yrange, xtitle, ytitle, title, map_name, minval, maxval,$
             xticks, div, fmt, yticks, ytickname, lats, psfc(ilon-1,*), color_table, isMirs


  ;---- USA ----------------------------------------------------------------------------------------
  step=4
  if gridfactor eq 2 then step=2

  lat_min=20.0
  lat_max=55.0
  lon_min=-130.0
  lon_max=-60.0

  ilat_bot=(lat_min+90)*gridfactor
  ilat_top=(lat_max+90)*gridfactor

  ilon_left=(lon_min+180)*gridfactor
  ilon_right=(lon_max+180)*gridfactor

  ;---- lat fixed ----
  xval = fix(lon_min) + indgen(8)*10
  xrange=[lon_min,lon_max]
  xtitle='Longitude'
  xticks=N_elements(xval)-1
  lons = lon_min + findgen(ilon_right-ilon_left)/gridfactor

  for ilat=ilat_bot,  ilat_top, step do begin
    latval = ( ilat * 1.0 / gridfactor ) - 90.0
    latstr = strtrim(string(fix(latval)),2)
    title = nwpTitle + titleSatId + prod_title + ' on latitude ' + latstr + '!9' + degree_sign + '!X' + cendTxt + date
    map_name = figsDir + prefix_us + '_' + prod_name + '_' + yyyymmdd + '_lat_' + latstr + '_' + cend + '.png'
    sb=fltarr((ilon_right-ilon_left),NLAY_PROD)
    for ilay=0, NLAY_PROD - 1 do begin
    for ilon=0, ilon_right-ilon_left-1 do begin
      sb(ilon,ilay) = prod(ilon+ilon_left,ilat,ilay)
    endfor
    endfor
    plot_vert, sb, xval, yval, xrange, yrange, xtitle, ytitle, title, map_name, minval,maxval,$
	       xticks, div, fmt, yticks, ytickname, lons, psfc(ilon_left:ilon_right-1,ilat), color_table, isMirs
  endfor

  ;---- lon fixed ----------
  xval=fix(lat_min) + indgen(8)*5
  xrange=[lat_min,lat_max]
  xtitle='Latitude'
  xticks=N_elements(xval)-1
  lats = lat_min + findgen(ilat_top-ilat_bot)/gridfactor

  for ilon=ilon_left, ilon_right, step  do begin
    lonval = ( ilon * 1.0 / gridfactor ) - 180.0
    lonstr = strtrim(string(fix(lonval)),2)
    title = nwpTitle + titleSatId + prod_title + ' on longitude ' + lonstr + '!9' + degree_sign + '!X' + cendTxt + date
    map_name = figsDir + prefix_us + '_' + prod_name + '_' + yyyymmdd + '_lon_' + lonstr + '_' + cend + '.png'
    sb=fltarr((ilat_top-ilat_bot),NLAY_PROD)
    for ilay=0, NLAY_PROD - 1 do begin
    for ilat=0, ilat_top-ilat_bot-1 do begin
      sb(ilat,ilay) = prod(ilon,ilat+ilat_bot,ilay)
    endfor
    endfor
    plot_vert, sb, xval, yval, xrange, yrange, xtitle, ytitle, title, map_name, minval,maxval,$
	       xticks, div, fmt, yticks, ytickname, lats, psfc(ilon,ilat_bot:ilat_top-1), color_table, isMirs
  endfor
      

  UNDEFINE, prod

End



;===================================================================================================
; Name:		plot_vert_diff
;
; Type:		IDL Subroutine
;  
; Description:
;    To plot gridded data bias vertical distribution
;
; 
; Arguments:    
;     
;	Name	    	Type	    	Description
;     ---------------------------------------------------
;	tmp   		i 		img array      
;	xval		i		x-axis
;	yval		i		y-axix
;	xrange		i		x range
;	yrange		i		y range
;	title		i		title
;	xtitle		i		xtitle
;	ytitle		i		ytitle
;	map_name	i		image file name
;	minvalue	i		min values
;	maxvalue	i		max values
;	Xticks 		i		# of x ticks
;	div 		i		division in color bar
;	psfc 		i		surface pressure
;	color_table 	i		color table used ( >41, MIRS defined its own )
;     
;
; Subroutines needed:
;	- ColorBar
;
;
; Record of revisions:
;        Date          Programmer      		Description of change
;    ============    ==============    =========================================
;     11/17/2009       Wanchun Chen     	Original code
;     
;===================================================================================================
Pro plot_vert_diff,tmp,xval,yval,xrange,yrange,xtitle,ytitle,title,map_name,minvalue,maxvalue,$
    Xticks,div,fmt,Yticks,YtickName,x,psfc,color_table

;---- device set up  - Z-buffer
set_plot,'Z'
;device,set_resolution=[650, 325]
;position=[0.1, 0.175, 0.95, 0.925]
device,set_resolution=[550, 325]
position=[0.1, 0.175, 0.975, 0.925]

;---- load color table
if color_table ne 41 then begin
  loadct, 33, /silent
  TVLCT, r, g, b, /get
endif

if color_table eq 41 then begin
  loadct_tpw, r, g, b
  TVLCT, r, g, b
endif

;---- color adjustment
r(0)=255 & g(0)=255 & b(0)=255
r(255)=0 & g(255)=0 & b(255)=0
r(254)=255 & g(254)=255 & b(254)=255	; Load White Color - 254 for Missing Data
r(253)=215 & g(253)=215 & b(253)=215	; Load Grey Color (20%) - 253 for Land/Ocean Coverage
r(252)=98  & g(252)=98  & b(252)=98	; Load Grey Color (70%) - 252 for non-convergent points(qc fail)

drawColor = 255

info=size(tmp)
XDIM=info[1]
YDIM=info[2]

;--------------------------------------------------------------------------
; The relationship of layers and pressure layers are not linear.
; We need to adjust this according to surface pressure to reflect
; this in yaxis, which is pressure, sort of convert from layers into
; pressure(shrink in vertical direction to make it above surface line) 
;--------------------------------------------------------------------------

PRESSURE_SPAN = ABS(yrange(0)-yrange(1))

layers_psfc = intarr(XDIM)
layers_psfc(*) = -999

for icol=0, XDIM-1 do begin
  if psfc(icol) gt 0 then begin
      layers_psfc(icol)= FIX( YDIM*( 1-(psfc(icol)-yrange(1))/PRESSURE_SPAN ) )
      if layers_psfc(icol) lt 0    then  layers_psfc(icol) = 0
      if layers_psfc(icol) ge YDIM then  layers_psfc(icol) = YDIM-1
  endif
endfor

layers_valid = intarr(XDIM)
layers_valid(*) = -999

for icol = 0, XDIM-1 do begin
for ilay = YDIM-1, 0, -1 do begin
  if tmp(icol,ilay) gt -99.0 then layers_valid(icol) = ilay
endfor
endfor

dummy=tmp
tmp(*,*)=-999.0

for icol = 0, XDIM-1 do begin

  if layers_psfc(icol) ge 0 and layers_valid(icol) ge 0 and layers_psfc(icol) ge layers_valid(icol) then begin
     
     ratio     = ( (YDIM-layers_valid(icol)) * 1.0 ) / ( (YDIM-layers_psfc(icol)) * 1.0 )
     intercept = layers_valid(icol) - ratio*layers_psfc(icol)
     
     for ilay_high = layers_psfc(icol), YDIM-1 do begin
       
       ilay_low = FIX( ratio * ilay_high + intercept )
       
       if ilay_low ge YDIM  then ilay_low = YDIM-1 
       if ilay_low lt 0     then ilay_low = 0
     	
       tmp(icol,ilay_high) = dummy(icol,ilay_low)

     endfor

  endif

endfor

;---- scaling
image=bytscl(tmp, min=minvalue, max=maxvalue, top=240)

;---- adjust colors for some special values in the tmp
for irow=0, YDIM-1 do begin 
for icol=0, XDIM-1 do begin
  if tmp(icol,irow) eq -999.0 then image(icol,irow) = 254
endfor
endfor

;---- coordinates
xsize=(position[2] - position[0]) * !D.X_VSIZE
ysize=(position[3] - position[1]) * !D.Y_VSIZE
xstart=position[0] * !D.X_VSIZE
ystart=position[1] * !D.Y_VSIZE


TV, Congrid(image,xsize,ysize), xstart, ystart

;---- Plot X-Y axises
Plot, xval, yval, xrange=xrange, yrange=yrange, xtitle=xtitle, ytitle=ytitle, title=title,    $
      POSITION=position, Color=drawColor, Charsize=0.75, XStyle=1, YStyle=1, /NoData, /NoErase,$
      Xticks=Xticks, Yticks=Yticks, YtickName=YtickName

ss = where( psfc lt 0.0, cnt) & if( cnt gt 0 ) then psfc(ss) = !VALUES.F_NAN        
Oplot, x, psfc, color=255, thick=2

;---- Plot a color bar
position_bar=fltarr(4)
position_bar[0]=position[0]+0.3
position_bar[1]=position[1]-0.125
position_bar[2]=position[2]-0.1
position_bar[3]=position_bar[1]+0.03
ColorBar,Bottom=1,NColors=240,Color=drawColor,Position=position_bar,Format=fmt,$
	Divisions=div,Minor=1,TickLen=0.0001,Range=[minvalue,maxvalue],CharSize=0.75

;---- Plot Missing lengend (white)
position_bar[0]=position[0]
position_bar[1]=position[1]-0.125
position_bar[2]=position[0]+0.05
position_bar[3]=position_bar[1]+0.03
ColorBar,Bottom=0,NColors=1, Position=position_bar, Divisions=2, COLOR=drawColor,$
	Minor=1,TickLen=0.0001,TickNames=[' ','NoData',' '],CharSize=0.75

;---- surface pressure (black)
position_bar[0]=position[0]+0.175
position_bar[1]=position[1]-0.125
position_bar[2]=position[0]+0.225
position_bar[3]=position_bar[1]+0.03
ColorBar,Bottom=255,NColors=1, Position=position_bar, Divisions=2, COLOR=drawColor,$
	Minor=1,TickLen=0.0001,TickNames=[' ','sfc pressure',' '],CharSize=0.75

;---- write out image file
write_png, map_name, TVRD(), r, g, b
device, /close

End


;===================================================================================================
;  Name: vertical_cross_diff
;
;  Purpose:
;    To plot vertical cross section of profile difference
;  
;  Dependence:
;    plot_vert_diff.pro
;
;  Record of revisions:
;        Date          Programmer     	Description of change
;    ============    ==============    =====================================
;     11/17/09         Wanchun Chen     Original Code
;
;===================================================================================================
Pro vertical_cross_diff, NCOL,NROW,NLAY,gridfactor,satId1,satId2,gridDir1,gridDir2,figsDir,$
    yyyymmdd, date, prod_id, cend, psfc, div, fmt, minval, maxval

  if prod_id eq 'temp'     then prod_title = ' Temperature (K) '
  if prod_id eq 'wv'       then prod_title = ' Water Vapor (g/kg) '
  if prod_id eq 'clwp'     then prod_title = ' CLW (mm) '
  if prod_id eq 'rainp'    then prod_title = ' Rain (mm) '
  if prod_id eq 'graupelp' then prod_title = ' Graupel (mm) '
  
  if cend eq 'as' then cendtxt = ' Asc '
  if cend eq 'ds' then cendtxt = ' Des '
  
  refId = satId2
  refTitle = STRUPCASE(satId2)
  
  if satId2 eq 'gdas' or satId2 eq 'ecmwf' or satId2 eq 'gfs' then begin
    refId = satId1 + '_' + satId2
    refTitle = 'Collo. ' + STRUPCASE(satId2)
  endif
  
  prefix='mirs_adv_poes_'+satId1+'_'+satId2+'_diff_vert_glb'
  prefix_us='mirs_adv_poes_'+satId1+'_'+satId2+'_diff_vert_us'
  
  if ( satId1 eq 'f16' or satId1 eq 'f17' or satId1 eq 'f18' ) then begin
    prefix='mirs_adv_dmsp_'+satId1+'_'+satId2+'_diff_vert_glb'
    prefix_us='mirs_adv_dmsp_'+satId1+'_'+satId2+'_diff_vert_us'
  endif
  if ( satId1 eq 'trmm' or satId1 eq 'gpm' ) then begin
    prefix='mirs_adv_eos_'+satId1+'_'+satId2+'_diff_vert_glb'
    prefix_us='mirs_adv_eos_'+satId1+'_'+satId2+'_diff_vert_us'
  endif
  if ( satId1 eq 'aqua' or satId1 eq 'fy3ri' ) then begin
    prefix='mirs_adv_eos_'+satId1+'_'+satId2+'_diff_vert_glb'
    prefix_us='mirs_adv_eos_'+satId1+'_'+satId2+'_diff_vert_us'
  endif
  if ( satId1 eq 'npp' ) then begin
    prefix='mirs_adv_npoess_'+satId1+'_'+satId2+'_vert_glb'
    prefix_us='mirs_adv_npoess_'+satId1+'_'+satId2+'_vert_us'
  endif
  
  titleSatId=strupcase(satId1)
  if ( satId1 eq 'aqua'  ) then titleSatId='AMSR-E'
  if ( satId1 eq 'fy3ri' ) then titleSatId='FY3/MWRI'
  if ( satId1 eq 'npp'   ) then titleSatId='NPP/ATMS'

  ;***************************************************************
  ;   read in data set 1 and 2
  ;***************************************************************
  tmp = fltarr(NCOL,NROW)
  
  file1 = gridDir1+'GRID_'+satId1+'_'+yyyymmdd+'_'+prod_id+'_'+cend+'.dat'
  if FILE_TEST(file1) eq 0 then begin
    print, 'Not exist:'+file1
    RETURN
  endif
  
  prod1 = fltarr(NCOL,NROW,NLAY)
  openr, lun, file1,/get_lun,/Swap_Endian
  for ilay=0, NLAY-1 do begin
    readu, lun, tmp
    prod1(*,*,ilay) = tmp(*,*)
  endfor
  free_lun,lun

  file2 = gridDir2+'GRID_'+refId+'_'+yyyymmdd+'_'+prod_id+'_'+cend+'.dat'
  if FILE_TEST(file2) eq 0 then begin
    print, 'Not exist:'+file2
    RETURN
  endif
  
  prod2 = fltarr(NCOL,NROW,NLAY)
  openr, lun, file2,/get_lun,/Swap_Endian
  for ilay=0, NLAY-1 do begin
    readu, lun, tmp
    prod2(*,*,ilay) = tmp(*,*)
  endfor
  free_lun,lun

  ;---- array to hold difference, give them initial values of -999.0 ( MIRS default missing value )
  prod = fltarr(NCOL,NROW,NLAY) & prod(*,*,*) = -999.0
  
  for ilay=0, NLAY-1 do begin
  for irow=0, NROW-1 do begin
  for icol=0, NCOL-1 do begin
    if prod1(icol,irow,ilay) gt -99.0 and prod2(icol,irow,ilay) gt -99.0 then $
       prod(icol,irow,ilay) = prod1(icol,irow,ilay) - prod2(icol,irow,ilay)
  endfor
  endfor
  endfor
  
  UNDEFINE, prod1
  UNDEFINE, prod2

  ;***************************************************************
  ;   reverse z-direction
  ;***************************************************************
  dummy=prod
  for ilay=0, NLAY-1 do begin
    prod(*,*,ilay) = dummy(*,*,NLAY-1-ilay)  
  endfor
  UNDEFINE, dummy
   
  ytitle='Pressure (MB)'
  degree_sign=String(37B)
  
  ;---- hydrological fields have meaningful values starting from 100mb downward
  yval=[1085,100]
  yrange=[1085,100]
  yticks=8
  ytickname=['1085','890','718','565','430','320','230','150','100']
  
  ;---- we use rain rate (tpw) color tabel for hydrological fields
  color_table=41
  
  ;---- only need those layers below 100mb(including 100mb)
  index_start = 43  ; averaged for many days, we found 100mb --> 43(index)
  ;---- temp profile use a different color table ( IDL color table 33 )
  if prod_id eq 'temp' then begin
     yval=[1085.0, 0.01]  
     yrange=[1085, 0.01]
     yticks=10
     ytickname=['1085', '815', '585', '400', '250', '150', '75', '30', '10', '1', '0.01']
     index_start = 0
     color_table=33
  endif
  
  ;---- number of effective layers where we have valid values
  NLAY_PROD = NLAY - index_start
  
  ;---- Globe --------------------------------------------------------------------------------------
  step=40
  if gridfactor eq 2 then step=20

  ;---- lat fixed ----
  xval=[-180,-150,-120,-90,-60,-30,0,30,60,90,120,150,180]
  xrange=[-180,180]
  xtitle='Longitude'
  xticks=N_elements(xval)-1
  lons = -180.0 + findgen(NCOL)/gridfactor

  for ilat=0, NROW-1, step do begin
    latval = ( ilat * 1.0 / gridfactor ) - 90.0
    latstr = strtrim(string(fix(latval)),2)
    title = titleSatId + ' - ' + refTitle + prod_title + ' on latitude ' + latstr + '!9' + degree_sign + '!X' + cendTxt + date
    map_name = figsDir + prefix + '_' + prod_id + '_' + yyyymmdd + '_lat_' + latstr + '_' + cend + '.png'
    sb=fltarr(360*gridfactor,NLAY_PROD)
    for ilay = 0, NLAY_PROD - 1 do begin
      sb(*,ilay) = prod(*,ilat,ilay)
    endfor
    plot_vert_diff, sb, xval, yval, xrange, yrange, xtitle, ytitle, title, map_name, minval, maxval,$
               xticks, div, fmt, yticks, ytickname, lons, psfc(*,ilat), color_table
  endfor

  ilat=NROW
  latval = ( ilat * 1.0 / gridfactor ) - 90.0
  ;latstr = strtrim(string(fix(latval)),2)
  latstr = strtrim(string(round(latval)),2)
  title = titleSatId + ' - ' + refTitle + prod_title + ' on latitude ' + latstr + '!9' + degree_sign + '!X' + cendTxt + date
  map_name = figsDir + prefix + '_' + prod_id + '_' + yyyymmdd + '_lat_' + latstr + '_' + cend + '.png'
  sb=fltarr(360*gridfactor,NLAY_PROD)
  for ilay = 0, NLAY_PROD - 1 do begin
    sb(*,ilay) = prod(*,ilat-1,ilay)
  endfor
  plot_vert_diff, sb, xval, yval, xrange, yrange, xtitle, ytitle, title, map_name, minval, maxval,$
             xticks, div, fmt, yticks, ytickname, lons, psfc(*,ilat-1), color_table

  ;---- lon fixed ----
  xval=[-90,-60,-30,0,30,60,90]
  xrange=[-90,90]
  xtitle='Latitude'
  xticks=N_elements(xval)-1
  lats = -90 + findgen(NROW)/gridfactor

  for ilon=0, NCOL-1, step  do begin
    lonval = ( ilon * 1.0 / gridfactor ) - 180.0
    lonstr = strtrim(string(fix(lonval)),2)
    title = titleSatId + ' - ' + refTitle + prod_title + ' on longitude ' + lonstr + '!9' + degree_sign + '!X' + cendTxt + date
    map_name = figsDir + prefix + '_' + prod_id + '_' + yyyymmdd + '_lon_' + lonstr + '_' + cend + '.png'
    sb=fltarr(180*gridfactor,NLAY_PROD)
    for ilay = 0, NLAY_PROD - 1 do begin
      sb(*,ilay) = prod(ilon,*,ilay)
    endfor
    plot_vert_diff, sb, xval, yval, xrange, yrange, xtitle, ytitle, title, map_name, minval, maxval,$
               xticks, div, fmt, yticks, ytickname, lats, psfc(ilon,*), color_table
  endfor

  ilon=NCOL
  lonval = ( ilon * 1.0 / gridfactor ) - 180.0
  ;lonstr = strtrim(string(fix(lonval)),2)
  lonstr = strtrim(string(round(lonval)),2)
  title = titleSatId + ' - ' + refTitle + prod_title + ' on longitude ' + lonstr + '!9' + degree_sign + '!X' + cendTxt + date
  map_name = figsDir + prefix + '_' + prod_id + '_' + yyyymmdd + '_lon_' + lonstr + '_' + cend + '.png'
  sb=fltarr(180*gridfactor,NLAY_PROD)
  for ilay = 0, NLAY_PROD - 1 do begin
    sb(*,ilay) = prod(ilon-1,*,ilay)
  endfor
  plot_vert_diff, sb, xval, yval, xrange, yrange, xtitle, ytitle, title, map_name, minval, maxval,$
             xticks, div, fmt, yticks, ytickname, lats, psfc(ilon-1,*), color_table


  ;---- USA ----------------------------------------------------------------------------------------
  step=4
  if gridfactor eq 2 then step=2

  lat_min=20.0
  lat_max=55.0
  lon_min=-130.0
  lon_max=-60.0

  ilat_bot=(lat_min+90)*gridfactor
  ilat_top=(lat_max+90)*gridfactor

  ilon_left=(lon_min+180)*gridfactor
  ilon_right=(lon_max+180)*gridfactor

  ;---- lat fixed ----
  xval = fix(lon_min) + indgen(8)*10
  xrange=[lon_min,lon_max]
  xtitle='Longitude'
  xticks=N_elements(xval)-1
  lons = lon_min + findgen(ilon_right-ilon_left)/gridfactor

  for ilat=ilat_bot,  ilat_top, step do begin
    latval = ( ilat * 1.0 / gridfactor ) - 90.0
    latstr = strtrim(string(fix(latval)),2)
    title = titleSatId + ' - ' + refTitle + prod_title + ' on latitude ' + latstr + '!9' + degree_sign + '!X' + cendTxt + date
    map_name = figsDir + prefix_us + '_' + prod_id + '_' + yyyymmdd + '_lat_' + latstr + '_' + cend + '.png'
    sb=fltarr((ilon_right-ilon_left),NLAY_PROD)
    for ilay=0, NLAY_PROD - 1 do begin
    for ilon=0, ilon_right-ilon_left-1 do begin
      sb(ilon,ilay) = prod(ilon+ilon_left,ilat,ilay)
    endfor
    endfor
    plot_vert_diff, sb, xval, yval, xrange, yrange, xtitle, ytitle, title, map_name, minval,maxval,$
	       xticks, div, fmt, yticks, ytickname, lons, psfc(ilon_left:ilon_right-1,ilat), color_table
  endfor

  ;---- lon fixed ----------
  xval=fix(lat_min) + indgen(8)*5
  xrange=[lat_min,lat_max]
  xtitle='Latitude'
  xticks=N_elements(xval)-1
  lats = lat_min + findgen(ilat_top-ilat_bot)/gridfactor

  for ilon=ilon_left, ilon_right, step  do begin
    lonval = ( ilon * 1.0 / gridfactor ) - 180.0
    lonstr = strtrim(string(fix(lonval)),2)
    title = titleSatId + ' - ' + refTitle + prod_title + ' on longitude ' + lonstr + '!9' + degree_sign + '!X' + cendTxt + date
    map_name = figsDir + prefix_us + '_' + prod_id + '_' + yyyymmdd + '_lon_' + lonstr + '_' + cend + '.png'
    sb=fltarr((ilat_top-ilat_bot),NLAY_PROD)
    for ilay=0, NLAY_PROD - 1 do begin
    for ilat=0, ilat_top-ilat_bot-1 do begin
      sb(ilat,ilay) = prod(ilon,ilat+ilat_bot,ilay)
    endfor
    endfor
    plot_vert_diff, sb, xval, yval, xrange, yrange, xtitle, ytitle, title, map_name, minval,maxval,$
	       xticks, div, fmt, yticks, ytickname, lats, psfc(ilon,ilat_bot:ilat_top-1), color_table 
  endfor
      
  UNDEFINE, prod

End

;===============================================================
; Name:		DensityScatter
;
;
; Type:		IDL Subroutine
;
;
; Description:  Grids X and Y data in scatter plot form, where the 
;               red color represents the highest density of points.
;		Output in encapsulated PS or PNG, scale is log 10.
;
;
; Subroutines needed:
;       - None
;
;
; History:
;       07-28-2009      Kevin Garrett, IMSG Inc @ NOAA/NESDIS/STAR
;
;===============================================================

PRO DensityScatter,minX,maxX,minY,maxY,X,Y,xtit,ytit,tit,chsz,iplotStats,stats,img_name,gridScale,symbol_size,ps_out

  ;set_plot,'x'
  xz=18 & yz=18
  winsize=[!D.X_SIZE,!D.Y_SIZE]
  ;window,/free,/pixmap,retain=2,xsize=winsize[0],ysize=winsize[1]
  if (ps_out eq 0) then begin
      set_plot,'Z'
      device,set_resolution=[650, 500]
  endif
  if (ps_out eq 1) then begin
      close,/all
      erase
      !p.multi=1
      !p.font=-1
      set_plot,'ps'
      device,filename=img_name+'.ps',/color,bits_per_pixel=8,xsize=xz,ysize=yz,xoffset=1,yoffset=1,/encapsulated,$
        /portrait,font_size=16,/bold,/courier
  endif
  position=[1.8/xz,3.6/yz,17.1/xz,17.1/yz]
;  position=[1.6/xz,1.8/yz,12.8/xz,16.2/yz]
  ;---- load color table
  loadct,39,/silent
  TVLCT, rr, gg, bb, /get
  rr(0)=255   & gg(0)=255   & bb(0)=255   	; Load White Color - 0 for background 
  rr(255)=0   & gg(255)=0   & bb(255)=0 	; Load Black Color - 255
  nc=!D.table_size
  nc=nc-2
  ;---- set up grids from scale factor
  gridX=findgen(((maxX-minX)/gridScale)+(1))
  gridY=findgen(((maxY-minY)/gridScale)+(1))
  gridX(*)=0
  gridY(*)=0

  nx=n_elements(gridX)
  ny=n_elements(gridY)
  for ix=0,nx-1 do begin
      gridX(ix)=(ix*gridScale)+minX
  endfor
  for iy=0,ny-1 do begin
      gridY(iy)=(iy*gridScale)+minY
  endfor
  ;---- loop over grids and count number of points in each cell
  gridDensity=make_array(nx,ny,/float,value=0)
  colorarray=make_array(nx,ny,/int,value=0)
  for ix=0,nx-2 do begin
      idx=where(X gt gridX(ix) and X le gridX(ix+1),cnt1)
      for iy=0,ny-2 do begin
          if (cnt1 gt 0) then begin
              idy=where(y(idx) gt gridY(iy) and y(idx) le gridY(iy+1),cnt2)
              gridDensity(ix,iy)=cnt2
          endif
      endfor
  endfor

  ;---- plot
  if (ps_out eq 0) then plot,gridX,gridY,/nodata,title=tit,xtitle=xtit,ytitle=ytit,yrange=[minY,maxY],position=position,xrange=[minX,maxX],thick=3
  if (ps_out eq 1) then plot,gridX,gridY,/nodata,title=tit,xtitle=xtit,ytitle=ytit,yrange=[minY,maxY],position=position,xrange=[minX,maxX],thick=3,font=0
  if (max(gridDensity) le 1000) then maxGridDensity=1000
  if (max(gridDensity) gt 1000 and max(gridDensity) le 10000) then maxGridDensity=10000
  if (max(gridDensity) gt 10000 and max(gridDensity) le 100000) then maxGridDensity=100000
  if (max(gridDensity) gt 100000) then maxGridDensity=100000
  for ix=0,nx-2 do begin
      for iy=0,ny-2 do begin
          gridPnt=gridDensity(ix,iy)
          if (gridPnt eq 0) then CONTINUE
          indcol=0L
          xind=(float(alog10(gridPnt)-0.0000001)/float(alog10(maxGridDensity)-0.0000001))*nc
          indcol=long(xind) > 1L < nc
          colorarray(ix,iy)=indcol-1
          usersym,[0,0,gridScale,gridScale,0],[0,gridScale,gridScale,0,0],/fill
          oplot,[gridX(ix)],[gridY(iy)],psym=8,color=indcol-1,symsize=symbol_size
      endfor
  endfor

  ;---- compute statistics
  nelts=n_elements(x)
  c9=correlate(x,y)
  c10=moment(x-y)
  res=poly_fit(x,y,1,/double,status=pfit_stat)
  rms=sqrt(mean((x-y)^2))
  stats=fltarr(9)
  stats(0)=c9
  stats(1)=c10[0]
  stats(2)=stdev(x-y)
  stats(3)=nelts
  if(pfit_stat eq 0)then begin
      stats(4)=res(1)
      stats(5)=res(0)
  endif
  stats(6)=rms
  stats(7)=mean(x)
  stats(8)=mean(y)
  if (iplotStats eq 1) then begin
      plots,[minX,maxX],[minY,maxY]
      if (pfit_stat eq 0) then oplot,gridX,res(1)*gridX+res(0),color=240
      comm9=strmid(strcompress('Correlation: '+string(c9)),0,19)
      comm10=strmid(strcompress('Bias: '+string(c10[0])),0,20)
      comm11=strmid(strcompress('Std Dev: '+string(stdev(x-y))),0,15)
      comm12=strmid(strcompress('Points: '+string(nelts)),0,15)
      if(pfit_stat eq 0)then begin
          comm13=strmid(strcompress('Slope: '+string(res(1))),0,13)
          comm14=strmid(strcompress('Intercept: '+string(res(0))),0,18)
      endif else begin
          comm13='Slope: N/A'
          comm14='Intercept: N/A'
      endelse
      comm15=strmid(strcompress('RMS: '+string(rms)),0,11)
      xs=minX+(maxX-minX)/18.
      r0=minY
      r=maxY
      xyouts,xs,r0+(0.94*(r-r0)),comm9,charsize=chsz*0.7
      xyouts,xs,r0+(0.90*(r-r0)),comm10,charsize=chsz*0.7
      xyouts,xs,r0+(0.86*(r-r0)),comm11,charsize=chsz*0.7
      xyouts,xs,r0+(0.82*(r-r0)),comm12,charsize=chsz*0.7
      xyouts,xs,r0+(0.78*(r-r0)),comm13,charsize=chsz*0.7
      xyouts,xs,r0+(0.74*(r-r0)),comm14,charsize=chsz*0.7
      xyouts,xs,r0+(0.70*(r-r0)),comm15,charsize=chsz*0.7
  endif
  ;---- create colorbar
  position_bar=fltarr(4)
  position_bar[0]=position[0]+0.05
  position_bar[1]=position[1]-0.15
  position_bar[2]=position[2]-0.05
  position_bar[3]=position_bar[1]+0.03

  colorbar3,/xlog,xticks=0,ncolors=nc-2,/horizontal,range=[1,maxGridDensity],title='Density of Points', $
    charsize=chsz*0.7,position=position_bar

  plot,gridX,gridY,/nodata,yrange=[minY,maxY],xrange=[minY,maxY],position=position,/noerase,font=0
  ;---- write out image file
  if (ps_out eq 0) then write_png, img_name, TVRD(), rr, gg, bb
  device,/close
END


;===============================================================
; Name:		DensityScatter_2
;
;
; Type:		IDL Subroutine
;
;
; Description:  Creates and plots data in scatter plot with
;               statistics, but does not create PNG or PS 
;               device (those must be defined outside of the
;               program).  This is a plotting mechanism only
;
;
; Subroutines needed:
;       - None
;
;
; History:
;       05-15-2013      Kevin Garrett, RTi @ NOAA/NESDIS/STAR
;
;===============================================================

PRO DensityScatter_2,minX,maxX,minY,maxY,X,Y,xtit,ytit,tit,chsz,iplotStats,stats,gridScale,symbol_size
  xz=18 & yz=18
  position=[1.8/xz,3.6/yz,17.1/xz,17.1/yz]
  ;---- load color table
  loadct,39,/silent
  TVLCT, rr, gg, bb, /get
  rr(0)=255   & gg(0)=255   & bb(0)=255   	; Load White Color - 0 for background 
  rr(255)=0   & gg(255)=0   & bb(255)=0 	; Load Black Color - 255
  nc=!D.table_size
  nc=nc-2
  ;---- set up grids from scale factor
  gridX=findgen(((maxX-minX)/gridScale)+(1))
  gridY=findgen(((maxY-minY)/gridScale)+(1))
  gridX(*)=0
  gridY(*)=0

  nx=n_elements(gridX)
  ny=n_elements(gridY)
  for ix=0,nx-1 do begin
      gridX(ix)=(ix*gridScale)+minX
  endfor
  for iy=0,ny-1 do begin
      gridY(iy)=(iy*gridScale)+minY
  endfor
  ;---- loop over grids and count number of points in each cell
  gridDensity=make_array(nx,ny,/float,value=0)
  colorarray=make_array(nx,ny,/int,value=0)
  for ix=0,nx-2 do begin
      idx=where(X gt gridX(ix) and X le gridX(ix+1),cnt1)
      for iy=0,ny-2 do begin
          if (cnt1 gt 0) then begin
              idy=where(y(idx) gt gridY(iy) and y(idx) le gridY(iy+1),cnt2)
              gridDensity(ix,iy)=cnt2
          endif
      endfor
  endfor

  ;---- plot
  plot,gridX,gridY,/nodata,title=tit,xtitle=xtit,ytitle=ytit,yrange=[minY,maxY],xstyle=1,ystyle=1, $
    position=position,xrange=[minX,maxX],thick=3,font=1
  if (max(gridDensity) le 1000)                                 then maxGridDensity=1000
  if (max(gridDensity) gt 1000 and max(gridDensity) le 10000)   then maxGridDensity=10000
  if (max(gridDensity) gt 10000 and max(gridDensity) le 100000) then maxGridDensity=100000
  if (max(gridDensity) gt 100000)                               then maxGridDensity=100000
  for ix=0,nx-2 do begin
      for iy=0,ny-2 do begin
          gridPnt=gridDensity(ix,iy)
          if (gridPnt eq 0) then CONTINUE
          indcol=0L
          xind=(float(alog10(gridPnt)-0.0000001)/float(alog10(maxGridDensity)-0.0000001))*nc
          indcol=long(xind) > 1L < nc
          colorarray(ix,iy)=indcol-1
          usersym,[0,0,gridScale,gridScale,0],[0,gridScale,gridScale,0,0],/fill
          oplot,[gridX(ix)],[gridY(iy)],psym=8,color=indcol-1,symsize=symbol_size
      endfor
  endfor

  ;---- compute statistics
  nelts=n_elements(x)
  c9=correlate(x,y)
  c10=moment(x-y)
  res=poly_fit(x,y,1,/double,status=pfit_stat)
  rms=sqrt(mean((x-y)^2))
  stats=fltarr(9)
  stats(0)=c9
  stats(1)=c10[0]
  stats(2)=stdev(x-y)
  stats(3)=nelts
  if(pfit_stat eq 0)then begin
      stats(4)=res(1)
      stats(5)=res(0)
  endif
  stats(6)=rms
  stats(7)=mean(x)
  stats(8)=mean(y)
  if (iplotStats eq 1) then begin
      plots,[minX,maxX],[minY,maxY]
      if (pfit_stat eq 0) then oplot,gridX,res(1)*gridX+res(0),color=240
      comm9=strmid(strcompress('Correlation: '+string(c9)),0,19)
      comm10=strmid(strcompress('Bias: '+string(c10[0])),0,20)
      comm11=strmid(strcompress('Std Dev: '+string(stdev(x-y))),0,15)
      comm12=strmid(strcompress('Points: '+string(nelts)),0,15)
      if(pfit_stat eq 0)then begin
          comm13=strmid(strcompress('Slope: '+string(res(1))),0,13)
          comm14=strmid(strcompress('Intercept: '+string(res(0))),0,18)
      endif else begin
          comm13='Slope: N/A'
          comm14='Intercept: N/A'
      endelse
      comm15=strmid(strcompress('RMS: '+string(rms)),0,11)
      xs=minX+(maxX-minX)/18.
      r0=minY
      r=maxY
      xyouts,xs,r0+(0.94*(r-r0)),comm9,charsize=chsz*0.7
      xyouts,xs,r0+(0.90*(r-r0)),comm10,charsize=chsz*0.7
      xyouts,xs,r0+(0.86*(r-r0)),comm11,charsize=chsz*0.7
      xyouts,xs,r0+(0.82*(r-r0)),comm12,charsize=chsz*0.7
      xyouts,xs,r0+(0.78*(r-r0)),comm13,charsize=chsz*0.7
      xyouts,xs,r0+(0.74*(r-r0)),comm14,charsize=chsz*0.7
      xyouts,xs,r0+(0.70*(r-r0)),comm15,charsize=chsz*0.7
  endif
  ;---- create colorbar
  position_bar=fltarr(4)
  position_bar[0]=position[0]+0.05
  position_bar[1]=position[1]-0.18
  position_bar[2]=position[2]-0.05
  position_bar[3]=position_bar[1]+0.03

  colorbar3,/xlog,xticks=0,ncolors=nc-2,/horizontal,range=[1,maxGridDensity],title='Density of Points', $
    charsize=chsz*0.7,position=position_bar

END

;===================================================================================================
; Name:		plot_hist_line2
;
; Type:		IDL Subroutine
;  
; Description:
;    To plot histogram 2 lines plot
;
; 
; Arguments:    
;     
;
; Subroutines needed:
;	- None
;
;
; Record of revisions:
;        Date          Programmer      		Description of change
;    ============    ==============    =========================================
;     02/06/2012       Wanchun Chen     	Original code
;     
;===================================================================================================

Pro plot_hist_line2, x, y1, y2, title,xtitle,ytitle,png_file,xrange,yrange,leg1,leg2,mean1,mean2
;
set_plot, 'z'
;
r=bytarr(256) & g=bytarr(256) & b=bytarr(256)
r(*)=255 & g(*)=255 & b(*)=255
;
r(1)=255 & g(1)=0    & b(1)=0
r(2)=0   & g(2)=255  & b(2)=0
r(3)=0   & g(3)=0    & b(3)=255
r(4)=0   & g(4)=255  & b(4)=255
r(5)=0   & g(5)=0    & b(5)=0   
;
TVLCT, r, g, b
;
color1=1 & color2=2 & color3=3 & color4=4 & color5=5
;
colors=[color1,color2,color3,color4,color5]
;
white=255
black=5
red=1
blue=3
;
position=[0.1, 0.15, 0.95, 0.925]
charsz = 0.8
thck = 1.0
;
plot, x, y1, CHARSIZE=charsz, TITLE=title, XTITLE=xtitle, YTITLE=ytitle, color=black, xstyle=1, ystyle=1, $
             BACKGROUND=white, XRANGE=xrange, YRANGE=yrange, /NODATA, position=position
; in case you need hitogram step by step one, use psym=10 option
;oplot, x, y1, color=red,  psym=10, SYMSIZE=1.5, thick=thck
;oplot, x, y2, color=blue, psym=10, SYMSIZE=1.5, thick=thck
oplot, x, y1, color=red,  thick=thck
oplot, x, y2, color=blue, thick=thck

ymax1 = max(y1)
ymax2 = max(y2)
ss1 = where( y1 eq ymax1 )
ss2 = where( y2 eq ymax2 )

xx1 = x[ss1[0]]
xx2 = x[ss2[0]]

NN=N_ELEMENTS(x)

ind1=0
mimi=ABS(mean1-x(0))
for i=1,NN-1 do begin
  if ABS(x(i)-mean1) lt mimi then begin
    ind1 = i
    mimi = ABS(x(i)-mean1)
  endif
endfor

ind2=0
mimi=ABS(mean2-x(0))
for i=1,NN-1 do begin
  if ABS(x(i)-mean2) lt mimi then begin
    ind2 = i
    mimi = ABS(x(i)-mean2)
  endif
endfor


; 3 vertical lines
plotS, [0,0],        yrange, linestyle=0, color=black, thick=thck

;plotS, [xx1,xx1], [0,ymax1], linestyle=0, color=red,   thick=thck
;plotS, [xx2,xx2], [0,ymax2], linestyle=0, color=blue,  thick=thck

;plotS, [xx1_mean,xx1_mean], [0,yy1_mean], linestyle=0, color=red,   thick=thck
;plotS, [xx2_mean,xx2_mean], [0,yy2_mean], linestyle=0, color=blue,  thick=thck

;plotS, [mean1,mean1], yrange, linestyle=0, color=red,   thick=thck
;plotS, [mean2,mean2], yrange, linestyle=0, color=blue,  thick=thck

plotS, [mean1,mean1], [0,y1(ind1)], linestyle=0, color=red,   thick=thck
plotS, [mean2,mean2], [0,y2(ind2)], linestyle=0, color=blue,  thick=thck


; legends
xline1=0.2 & yline1=0.02
xline2=0.6 & yline2=0.02
xyouts, xline1, yline1, "____ " + leg1, color=red,  CHARSIZE=1.0, CHARTHICK=thck, /normal
xyouts, xline2, yline2, "____ " + leg2, color=blue, CHARSIZE=1.0, CHARTHICK=thck, /normal

thisImage = TVRD()
TVLCT, r, g, b, /Get
Write_PNG, png_file, thisImage, r, g, b
;
End 


;===================================================================================================
;+
; NAME:
;        STRREPLACE
;
; PURPOSE:
;        The STRREPLACE procedure replaces the contents of one string
;        with another.  The first occurrence of the search substring, Find
;        within the source string, String is replaced by the string,
;        Replacement.
;
; CATEGORY:
;        String Processing.
;
; CALLING SEQUENCE:
;
;        STRREPLACE, String, Find, Replacement
;
; INPUTS:
;        String:   The string to have substring(s) replaced.  If String is
;                  an array, Find is replaced by Replacement in the first
;                  occurrence of Find of every element of the array.
;
;        Find:     The scalar substring to be replaced. If this argument is
;                  not a string, it is converted using IDL's default
;                  formatting rules.
;
;        Replacement:   A scalar string to replace the Find substring. If
;                  this argument is not a string, it is converted using IDL's
;                  default formattting rules.
;
; EXAMPLE:
;
;        If the variable A contains the string "IBM is fun", the
;        substring "IBM" can be replaced with the string "Microsoft"
;        by entering:
;
;        STRREPLACE, A, 'IBM', 'Microsoft'
;
; MODIFICATION HISTORY:
;        Written by:    Han Wen, June 1995.
;-
;===================================================================================================

pro STRREPLACE, Strings, Find1, Replacement1

;   Check integrity of input parameter

         NP        = N_PARAMS()
         if (NP ne 3) then message,'Must be called with 3 parameters, '+$
                   'Strings, Find, Replacement'

         sz        = SIZE(Strings)
         ns        = n_elements(sz)
         if (sz(ns-2) ne 7) then message,'Parameter must be of string type.'

         Find      = STRING(Find1)
         pos       = STRPOS(Strings,Find)
         here      = WHERE(pos ne -1, nreplace)

         if (nreplace eq 0) then return

         Replacement=STRING(Replacement1)
         Flen      = strlen(Find)
         for i=0,nreplace-1 do begin

              j         = here(i)
              prefix    = STRMID(Strings(j),0,pos(j))
              suffix    = STRMID(Strings(j),pos(j)+Flen,$
                                       strlen(Strings(j))-(pos(j)+Flen))
              Strings(j) = prefix + replacement + suffix
         endfor
End

