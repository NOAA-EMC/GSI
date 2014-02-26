;$Id: checkNEDT.pro 723 2007-11-21 18:34:32Z wchen $
@../../../setup/paths_idl.pro
;====================================================================
; Name:		CheckNEDT
;
;
; Type:		IDL Subroutine
;
;
; Description:  Check if NEDT exceedes specification.
;
;
; Arguments:
;
;      Name		    Type	    Description
;      ---------------------------------------------------
;	- nedtFile   	      I             NEDT file
;	- TunFile   	      I             Tuning file
;	- nedtTxt   	      O             abnormal info. of NEDT
;
;
; Subroutines needed:
;       - readErr
;	- readTuningFile
;
; History:
;       - 11/21/2007      Wanchun Chen	    Original coder
;
;====================================================================

PRO checkNEDT, nedtFile=nedtFile, nedtTxt=nedtTxt, TunFile=TunFile

NEDT_LIMIT=[0.27,0.27,0.36,0.225,0.225, 0.225,0.225,0.225,0.225,0.36, 0.36,0.54,0.72,1.08,0.45, 0.9,0.9,0.9,0.9,0.9]

;nedtFile='/net/orbit227l/disk1/pub/mirs_operational/data/TestbedData/nedt/n19_amsua_mhs/n19_amsua_mhs_nedt_2010_06_01_befFM.dat'
;nedtTxt='/net/orbit227l/home/pub/mirs_operational/src/testbed/nedtMonitoring/checkNEDT.txt'
;TunFile='/net/orbit227l/home/pub/mirs_operational/data/StaticData/TuningData/TunParams_n19_amsua_mhs.in'

readErr,nedtFile,cfreq,nedt,nchan,ComputedOrNotFlag
readTuningFile,TunFile,nchan,ChanSel

;---- derieve satid from nedtFile ----
nedtFile_base = FILE_BASENAME(nedtFile)
pos_underscore = STRPOS(nedtFile_base, '_')
satid = STRMID(nedtFile_base,0,pos_underscore)

if satid eq 'n18' then $
  NEDT_LIMIT = [0.235,0.182,0.277,0.168,0.190,0.144,0.168,0.195,0.165,0.204,$
	        0.226,0.309,0.423,0.678,0.134,0.244,0.382,0.559,0.434,0.355]

if satid eq 'n19' then $
  NEDT_LIMIT = [0.192,0.166,0.209,0.148,0.160,0.143,0.166,0.171,0.165,0.206,$
	        0.221,0.310,0.424,0.678,0.135,0.252,0.400,0.582,0.455,0.354]

if satid eq 'metopA' then $
  NEDT_LIMIT = [0.205,0.220,0.255,0.143,0.168,0.133,0.441,0.160,0.173,0.213,$
                0.246,0.329,0.451,0.740,0.111,0.228,0.369,0.507,0.406,0.359]

if satid eq 'metopB' then $
  NEDT_LIMIT = [0.205,0.220,0.255,0.143,0.168,0.133,0.441,0.160,0.173,0.213,$
                0.246,0.329,0.451,0.740,0.111,0.228,0.369,0.507,0.406,0.359]

if satid eq 'npp' then $
  NEDT_LIMIT = [0.9,0.9,1.2,0.75,0.75,0.75,0.75,0.75,0.75,0.75,1.2,$
                1.2,1.5,2.4,3.60,0.50,0.60,0.80,0.80,0.80,0.80,0.9]

nchan=20
if satid eq 'npp' then nchan=22


openw, lun, nedtTxt, /get_lun
for i=0, nchan-1 do begin
  if ( nedt(i) gt 1.1*NEDT_LIMIT(i) and ChanSel(i) eq 1 )  then begin
      printf, lun, strcompress('Channel ' + string(i+1) + ' NEDT: ' + string(nedt(i)) + ' is > ' + string(NEDT_LIMIT(i)) + ' (spec)')
      printf, lun, 'NEDT exceeds specification and channel is turned on.'
      printf, lun, ''
  endif
  if ( nedt(i) gt 1.1*NEDT_LIMIT(i) and ChanSel(i) eq 0 )  then begin
      printf, lun, strcompress('Channel ' + string(i+1) + ' NEDT: ' + string(nedt(i)) + ' is > ' + string(NEDT_LIMIT(i)) + ' (spec)')
      printf, lun, '* NEDT exceeds specification and channel is turned off.'
      printf, lun, ''
  endif
  if ( ComputedOrNotFlag(i) gt 0 and ChanSel(i) eq 1 ) then begin
      printf, lun, strcompress('Channel ' + string(i+1) + ' NEDT: not computed.')
      printf, lun, ''
  endif
  if ( nedt(i) le 1.1*NEDT_LIMIT(i) and ChanSel(i) eq 0 )  then begin
      printf, lun, strcompress('Channel ' + string(i+1) + ' NEDT: ' + string(nedt(i)) + ' is < ' + string(NEDT_LIMIT(i)*1.1) + ' (110% spec)')
      printf, lun, '* NEDT within specification but channel is turned off.'
      printf, lun, ''
  endif
endfor
free_lun, lun

END
