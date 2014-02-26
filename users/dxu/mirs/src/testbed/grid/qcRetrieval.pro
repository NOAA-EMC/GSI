@../../../setup/paths_idl.pro
;===============================================================================
;
;  Purpose:
;    Read from DEP files and output retrieval qc stats orbit by orbit.
;    Also output those abnormal stats to another file. This abnormal file's 
;    size as a criteria to trigger email alert.
;  
;  Sub used
;	- BTEST
;	- ReadDep
;
;  Record of revisions:
;        Date          Programmer       Description of change
;    ------------    --------------    ----------------------
;     08/18/2010       Wanchun Chen     Original Code
;
;===============================================================================

Pro qcRetrieval,namelist=namelist

depList='/disk1/pub/mirs_operational/data/InputsData/f16_depFiles_2011-02-10.list'
pathMon='/disk1/pub/mirs_operational/data/TestbedData/PerfsMonitoring/f16_ssmis/orbitmon/'
file_abnormal='/disk1/pub/mirs_operational/data/ControlData/qcRetrieval_abnormal_f16_2011_02_10'

fromNameList=1
if ( fromNameList eq 1 ) then begin
  openr,iu,namelist,/get_lun 
  readf,iu,format='(a)', depList         ; DEP file list
  readf,iu,format='(a)', pathMon         ; path to put qc retrieval stats file
  readf,iu,format='(a)', file_abnormal   ; file to save those abnormal stats
  free_lun,iu
endif

print, 'qcRetrieval.pro'

;---- open abnormal file for possible write out ----
openw, lun, file_abnormal, /get_lun

openr, iu_dep, depList, /get_lun

file_dep=''
file_mon=''
prefix_mon='QC_MON'

WHILE ( NOT EOF(iu_dep) ) DO BEGIN

  readf, iu_dep, file_dep, format='(a)'
  print, ''
  print, 'dep='+file_dep
  
  ;---- QC Mon file name generation ----
  basefile = FILE_BASENAME(file_dep)
  filelength = strlen(basefile)
  file_mon=pathMon+prefix_mon+strmid(basefile,6,filelength-6)
  print, 'mon='+file_mon
  
  ;---- read DEP file ----
  ReadDEP, file_dep, nprofiles, Dep
  
  ;help, Dep.qc
  tmp = Dep.qc(0,*)
  
  ss = where(tmp eq 0, cnt0)
  ss = where(tmp eq 1, cnt1)
  ss = where(tmp eq 2, cnt2)
  
  nPrfValid = 0.0
  cntChiSq = 0.0
  for iprf=0L,Dep.nProfsProcessed-1 do begin
    result = BTEST(Dep.qc(3,iprf),0) 
    if result eq 0 then begin
      nPrfValid = nPrfValid + 1.0
      if Dep.ChiSq(iprf) ge 0 and Dep.ChiSq(iprf) le 1 then cntChiSq = cntChiSq + 1.0
    endif
  endfor
  ;print, 'nPrfValid=',nPrfValid
  ;print, 'cntChiSq=',cntChiSq
  
  per0 = float(cnt0)/Dep.nProfsProcessed
  per1 = float(cnt1)/Dep.nProfsProcessed
  per2 = float(cnt2)/Dep.nProfsProcessed
  
  if per0 gt 1 then per0 = 0.
  if per1 gt 1 then per1 = 0.
  if per2 gt 1 then per2 = 0.
  
  if nPrfValid eq 0 then perc = 1
  if nPrfValid gt 0 then perc = cntChiSq/nPrfValid
  
  year = Dep.scanYear(0)
  day = Dep.scanDay(0) + Dep.scanUTC(0)/86400.0
  
  ;---- output stats ----
  openw, iu_mon, file_mon, /get_lun
  printf, iu_mon, year,day,per0,per1,per2,perc, format='(i4,1x,f9.5,1x,f7.4,1x,f7.4,1x,f7.4,1x,f7.4)'
  free_lun,iu_mon
  
  ;---- output abnormal stats if any ----
  if per0 lt 0.3 or per2 gt 0.1 or perc lt 0.7 then begin
    printf, lun, ''
    printf, lun, file_dep
    printf, lun, 'QC(0)=', 100*per0, '%', format='(A6,F5.2,A1)'
    printf, lun, 'QC(1)=', 100*per1, '%', format='(A6,F5.2,A1)' 
    printf, lun, 'QC(2)=', 100*per2, '%', format='(A6,F5.2,A1)' 
    printf, lun, 'Convergence Rate=', 100*perc, '%', format='(A17,F5.2,A1)'
  endif
  
  undefine, Dep
  
ENDWHILE
  
free_lun,iu_dep

free_lun,lun


End
