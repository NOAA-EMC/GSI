!===============================================================
! Name:      mergeNedt_npp_atms
!
!
! Type:         Main Program
!
!
! Description:
!
!
! History:
!       11-22-2011      Wanchun Chen      Original Code
!
!===============================================================

Program mergeNedt_npp_atms
  Implicit none
  
  INTEGER, PARAMETER         :: NCHAN = 22
  CHARACTER(LEN=250)         :: fileIn=''
  INTEGER                    :: nfile=0, ichan=1
  
  REAL,DIMENSION(NCHAN)      :: freqs, rms, nedts, rm1, nedt1
  CHARACTER(LEN=250)         :: line1,line2,line3,line4
  
  !---Namelist Data
  CHARACTER(LEN=250)         :: listFile=''
  CHARACTER(LEN=250)         :: NEDTFile=''
  NAMELIST /MergeNEDTCntrl/ListFile,NEDTfile

  !---Read Control parameters
  READ(*,NML=MergeNEDTCntrl)
  
  rms = 0.0
  nedts = 0.0
  
  OPEN(unit=19,file=ListFile)
  nfile=0
  do while ( 1 .eq. 1 )
    READ(19, '(A)', end=99) fileIn
    !write(*,'(A)' ) TRIM(fileIn)
    
    open(29, file=TRIM(fileIn))
    read(29, '(A)' ) line1
    read(29, '(A)' ) line2
    read(29, '(10(F10.3))' ) freqs
    read(29, '(A)' ) line3
    read(29, '(10(F10.3))' ) rm1
    read(29, '(A)' ) line4
    read(29, '(10(F10.3))' ) nedt1
    close(29)
    
    do ichan=1,NCHAN
      if( rm1(ichan)   .gt. 0.0 ) rms(ichan) = rms(ichan) + rm1(ichan)
      if( nedt1(ichan) .gt. 0.0 ) nedts(ichan) = nedts(ichan) + nedt1(ichan)
    enddo
    
    nfile = nfile + 1
  enddo
  
99 Continue

  close(19)
  
  write(*,*) 'nfile=', nfile
  
  if( nfile .ge. 1 ) then
    rms(:) = rms(:)/nfile
    nedts(:) = nedts(:)/nfile
  else
    rms(:) = -999.0
    nedts(:) = -999.0
  endif
  
  !---- output 
  open(39,file=NEDTfile)
  write(39,'(A)') TRIM(line1) 
  write(39,'(A)') TRIM(line2) 
  write(39,'(10(F10.3))') freqs
  write(39,'(A)') TRIM(line3)
  write(39,'(10(F10.3))' )  rms
  write(39,'(A)') TRIM(line4) 
  write(39,'(10(F10.3))' )  nedts
  close(39)

End Program mergeNedt_npp_atms
