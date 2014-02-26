!***************************************************************************************************
!  TO compute Geophysical Performance Bias/Asymmetry and Radiometric Bias/Asymmetry.
!  
!  Weather condition filtering ( use clw and rr as filtering factor ) only apply to
!
!               tpw, tskin, wspd, temp, wc, em  
!  
!
!  Input:       Daily Gridded 3rd level data
!  Output:      Gridded Bias/Asymmetry data.
!
!  01/27/2007   Wanchun Chen      Original Coder
!  05/01/2012   Wanchun Chen      Added CLW and RR as filter factor to 6 parameters
!
!***************************************************************************************************

Program gridBias
  USE Consts
  USE misc
  implicit none
  !---intrinsic functions used
  intrinsic :: TRIM, ABS
  
  integer, parameter  :: lenf=256
  integer, parameter  :: CHISQ_CUT=5
  real,    parameter  :: CLW_CUT=0.05
  real,    parameter  :: RAIN_CUT=0.001
  real,    parameter  :: MISSING=-999.0
  real,    parameter  :: FAILING=-99.0
  integer, parameter  :: NPROD1=1
  integer, parameter  :: NPROD2=7
  integer, parameter  :: NPROD3=5
  integer, parameter  :: NPROD4=1
  integer, parameter  :: NPROD5=1
  integer, parameter  :: NPROD6=1
  integer, parameter  :: NCEND=2
  integer, parameter  :: NSFC=5
  integer, parameter  :: NCOND=4
  
  character(LEN=lenf) :: fileDepen,fileChisq,fileSfc,fileClw,fileRr,fileMirs,fileNwp,fileRad,fileBias,fileAsym

  character(LEN=2),dimension(NPROD1)  :: prods1 = (/ 'rr' /)
  character(LEN=5),dimension(NPROD2)  :: prods2 = (/ 'tpw  ', 'tskin', 'clw  ', 'iwp  ', 'swe  ', 'psfc ', 'wspd ' /)
  character(LEN=8),dimension(NPROD3)  :: prods3 = (/ 'temp    ', 'wv      ','clwp    ', 'rainp   ', 'graupelp' /)
  character(LEN=2),dimension(NPROD4)  :: prods4 = (/ 'em' /)
  character(LEN=3),dimension(NPROD5)  :: prods5 = (/ 'tbl' /)  ! corrected clear-sky measured TB from gridded scene
  character(LEN=3),dimension(NPROD6)  :: prods6 = (/ 'lwp' /)
  character(LEN=2),dimension(NCEND)   :: cends  = (/ 'as', 'ds' /)
  
  integer                        :: iprod
  character(LEN=8)               :: prodId
  integer                        :: icend
  character(LEN=8)               :: nwpString=DEFAULT_VALUE_STR4 
  character(LEN=7)               :: dependString='angle'
  
  !-- namelist data
  character(LEN=8)               :: satId=DEFAULT_VALUE_STR4
  character(LEN=lenf)            :: yyyymmdd=DEFAULT_VALUE_STR4
  integer                        :: gridfactor=DEFAULT_VALUE_INT
  character(LEN=lenf)            :: gridPath=DEFAULT_VALUE_STR4
  real                           :: latmin=DEFAULT_VALUE_REAL
  real                           :: latmax=DEFAULT_VALUE_REAL
  real                           :: lonmin=DEFAULT_VALUE_REAL
  real                           :: lonmax=DEFAULT_VALUE_REAL
  integer                        :: NLAY=DEFAULT_VALUE_INT
  integer                        :: NCHAN=DEFAULT_VALUE_INT
  integer                        :: nwpData=DEFAULT_VALUE_INT ! 1-GDAS, 2-ECMWF, 3-GFS
  integer                        :: isatId=DEFAULT_VALUE_INT  ! 1-N18,2-MetopA,3-F16,4-N19,6-NPP,9-TRMM,10-GPM,14:MetopB
  
  NAMELIST /GridBiasNameList/satId,yyyymmdd,gridfactor,gridPath,latmin,latmax,lonmin,lonmax,NLAY,NCHAN,nwpData,isatId

  READ(*,NML=GridBiasNameList)
  
  !---- nwp String
  if( nwpData .eq. 1 ) then
    nwpString = 'gdas'
  else if( nwpData .eq. 2 ) then
    nwpString = 'ecmwf'
  else if( nwpData .eq. 3 ) then
    nwpString = 'gfs'
  endif
  
  !---- use angle or scan position to compute bias asymmetry, POSE/NPP use angle, the rest use scan position
  if ( isatId .eq. sensor_id_n18 .or. isatId .eq. sensor_id_metopA .or. &
       isatId .eq. sensor_id_n19 .or. isatId .eq. sensor_id_metopB .or. &
       isatId .eq. sensor_id_npp .or. isatId .eq. sensor_id_mtsa ) then
    dependString='angle'
  else
    dependString='scanpos'
  endif
  
  !*************************************************************
  !  Geophysical Performace Radiance
  !*************************************************************
  if( nwpData .eq. 3 ) then
    
    Prods1Loop: do iprod = 1, NPROD1
    prodId = prods1(iprod)
    do icend = 1, NCEND  
      fileDepen = 'GRID_'//trim(satId)//'_'//trim(yyyymmdd)//'_'//TRIM(dependString)//'_'//cends(icend)//'.dat'
      fileChisq = 'GRID_'//trim(satId)//'_'//trim(yyyymmdd)//'_chisq_'//cends(icend)//'.dat'
      fileSfc   = 'GRID_'//trim(satId)//'_'//trim(yyyymmdd)//'_sfc2_'//cends(icend)//'.dat'
      fileClw   = 'GRID_'//trim(satId)//'_'//trim(yyyymmdd)//'_clw_'//cends(icend)//'.dat'
      fileRr    = 'GRID_'//trim(satId)//'_'//trim(yyyymmdd)//'_rr_'//cends(icend)//'.dat'
      fileMirs  = 'GRID_'//trim(satId)//'_'//trim(yyyymmdd)//'_'//trim(prods1(iprod))//'_'//cends(icend)//'.dat'
      fileNwp   = 'GRID_'//trim(satId)//'_'//trim(nwpString)//'_'//trim(yyyymmdd)//'_'//&
                  trim(prods1(iprod))//'_'//cends(icend)//'.dat'
      fileBias  = 'GRID_'//trim(satId)//'_'//trim(nwpString)//'_bias_'//trim(yyyymmdd)//&
                  '_'//trim(prods1(iprod))//'_'//cends(icend)//'.dat'
      fileAsym  = 'GRID_'//trim(satId)//'_'//trim(nwpString)//'_asym_'//trim(yyyymmdd)//&
                  '_'//trim(prods1(iprod))//'_'//cends(icend)//'.dat'
      write(*,'(A)')'fileBias='//trim(gridPath)//trim(fileBias) 
      write(*,'(A)')'fileAsym='//trim(gridPath)//trim(fileAsym) 
      call computeBias2(fileDepen,fileChisq,fileSfc,fileClw,fileRr,fileMirs,fileNwp,fileBias,fileAsym,&
           gridfactor,gridPath,latmin,latmax,lonmin,lonmax,isatId,prodId)
    enddo
    enddo Prods1Loop
 
  endif

  !-- 2-dimension array (grid bias prods2 list of parameters)
  Prods2Loop: do iprod = 1, NPROD2
  prodId = prods2(iprod)
  do icend = 1, NCEND  
    
    if ( ( strcmp(trim(prods2(iprod)),'wspd') .eq. 0 ) .and. &
         .not. ( isatId .eq. sensor_id_f16 .or. &
                 isatId .eq. sensor_id_f17 .or. &
                 isatId .eq. sensor_id_f18 .or. &
                 isatId .eq. sensor_id_trmm .or. &
                 isatId .eq. sensor_id_mtma) ) CYCLE Prods2Loop
    
    fileDepen = 'GRID_'//trim(satId)//'_'//trim(yyyymmdd)//'_'//TRIM(dependString)//'_'//cends(icend)//'.dat'
    fileChisq = 'GRID_'//trim(satId)//'_'//trim(yyyymmdd)//'_chisq_'//cends(icend)//'.dat'
    fileSfc   = 'GRID_'//trim(satId)//'_'//trim(yyyymmdd)//'_sfc2_'//cends(icend)//'.dat'      
    fileClw   = 'GRID_'//trim(satId)//'_'//trim(yyyymmdd)//'_clw_'//cends(icend)//'.dat'
    fileRr    = 'GRID_'//trim(satId)//'_'//trim(yyyymmdd)//'_rr_'//cends(icend)//'.dat'
    fileMirs  = 'GRID_'//trim(satId)//'_'//trim(yyyymmdd)//'_'//trim(prods2(iprod))//'_'//cends(icend)//'.dat'
    fileNwp   = 'GRID_'//trim(satId)//'_'//trim(nwpString)//'_'//trim(yyyymmdd)//'_'//&
                trim(prods2(iprod))//'_'//cends(icend)//'.dat'
    fileBias  = 'GRID_'//trim(satId)//'_'//trim(nwpString)//'_bias_'//trim(yyyymmdd)//&
                '_'//trim(prods2(iprod))//'_'//cends(icend)//'.dat'
    fileAsym  = 'GRID_'//trim(satId)//'_'//trim(nwpString)//'_asym_'//trim(yyyymmdd)//&
                '_'//trim(prods2(iprod))//'_'//cends(icend)//'.dat'
    write(*,'(A)')'fileBias='//trim(gridPath)//trim(fileBias) 
    write(*,'(A)')'fileAsym='//trim(gridPath)//trim(fileAsym) 
    call computeBias2(fileDepen,fileChisq,fileSfc,fileClw,fileRr,fileMirs,fileNwp,fileBias,fileAsym,&
         gridfactor,gridPath,latmin,latmax,lonmin,lonmax,isatId,prodId)
  enddo
  enddo Prods2Loop


  !-- (NCOL,NROW,NLAY) prods3
  do iprod = 1, NPROD3
  prodId = prods3(iprod)
  do icend = 1, NCEND

    fileDepen = 'GRID_'//trim(satId)//'_'//trim(yyyymmdd)//'_'//TRIM(dependString)//'_'//cends(icend)//'.dat'
    fileChisq = 'GRID_'//trim(satId)//'_'//trim(yyyymmdd)//'_chisq_'//cends(icend)//'.dat'
    fileSfc   = 'GRID_'//trim(satId)//'_'//trim(yyyymmdd)//'_sfc2_'//cends(icend)//'.dat'
    fileClw   = 'GRID_'//trim(satId)//'_'//trim(yyyymmdd)//'_clw_'//cends(icend)//'.dat'
    fileRr    = 'GRID_'//trim(satId)//'_'//trim(yyyymmdd)//'_rr_'//cends(icend)//'.dat'
    fileMirs  = 'GRID_'//trim(satId)//'_'//trim(yyyymmdd)//'_'//trim(prods3(iprod))//'_'//cends(icend)//'.dat'
    fileNwp   = 'GRID_'//trim(satId)//'_'//trim(nwpString)//'_'//trim(yyyymmdd)//'_'//&
                trim(prods3(iprod))//'_'//cends(icend)//'.dat'
    fileBias  = 'GRID_'//trim(satId)//'_'//trim(nwpString)//'_bias_'//trim(yyyymmdd)//&
                '_'//trim(prods3(iprod))//'_'//cends(icend)//'.dat'
    fileAsym  = 'GRID_'//trim(satId)//'_'//trim(nwpString)//'_asym_'//trim(yyyymmdd)//&
                '_'//trim(prods3(iprod))//'_'//cends(icend)//'.dat'
    write(*,'(A)')'fileBias='//trim(gridPath)//trim(fileBias) 
    write(*,'(A)')'fileAsym='//trim(gridPath)//trim(fileAsym) 
    call computeBias3(fileDepen,fileChisq,fileSfc,fileClw,fileRr,fileMirs,fileNwp,fileBias,fileAsym,&
         gridfactor,gridPath,latmin,latmax,lonmin,lonmax,NLAY,isatId,prodId)
  enddo
  enddo

  !-- (NCOL,NROW,NCHAN) prods4
  do iprod = 1, NPROD4
  prodId = prods4(iprod)
  do icend = 1, NCEND

    fileDepen = 'GRID_'//trim(satId)//'_'//trim(yyyymmdd)//'_'//TRIM(dependString)//'_'//cends(icend)//'.dat'
    fileChisq = 'GRID_'//trim(satId)//'_'//trim(yyyymmdd)//'_chisq_'//cends(icend)//'.dat'
    fileSfc   = 'GRID_'//trim(satId)//'_'//trim(yyyymmdd)//'_sfc2_'//cends(icend)//'.dat'
    fileClw   = 'GRID_'//trim(satId)//'_'//trim(yyyymmdd)//'_clw_'//cends(icend)//'.dat'
    fileRr    = 'GRID_'//trim(satId)//'_'//trim(yyyymmdd)//'_rr_'//cends(icend)//'.dat'
    fileMirs  = 'GRID_'//trim(satId)//'_'//trim(yyyymmdd)//'_'//trim(prods4(iprod))//'_'//cends(icend)//'.dat'
    fileNwp   = 'GRID_'//trim(satId)//'_'//trim(nwpString)//'_'//trim(yyyymmdd)//'_'//&
                trim(prods4(iprod))//'_'//cends(icend)//'.dat'
    fileBias  = 'GRID_'//trim(satId)//'_'//trim(nwpString)//'_bias_'//trim(yyyymmdd)//&
                '_'//trim(prods4(iprod))//'_'//cends(icend)//'.dat'
    fileAsym  = 'GRID_'//trim(satId)//'_'//trim(nwpString)//'_asym_'//trim(yyyymmdd)//&
                '_'//trim(prods4(iprod))//'_'//cends(icend)//'.dat'
    write(*,'(A)')'fileBias='//trim(gridPath)//trim(fileBias) 
    write(*,'(A)')'fileAsym='//trim(gridPath)//trim(fileAsym) 
    call computeBias3(fileDepen,fileChisq,fileSfc,fileClw,fileRr,fileMirs,fileNwp,fileBias,fileAsym,&
         gridfactor,gridPath,latmin,latmax,lonmin,lonmax,NCHAN,isatId,prodId)
  enddo
  enddo
   
  !**************************************************************************
  !  Clear sky Corrected measured TB(tbl) - TB from FWD radiance file (tb)
  !**************************************************************************
  !-- (NCOL,NROW,NCHAN) prods5
  do iprod = 1, NPROD5
  prodId = prods5(iprod)
  do icend = 1, NCEND

    fileDepen = 'GRID_'//trim(satId)//'_'//trim(yyyymmdd)//'_'//TRIM(dependString)//'_'//cends(icend)//'.dat'
    fileChisq = 'GRID_'//trim(satId)//'_'//trim(yyyymmdd)//'_chisq_'//cends(icend)//'.dat'
    fileSfc   = 'GRID_'//trim(satId)//'_'//trim(yyyymmdd)//'_sfc2_'//cends(icend)//'.dat'
    fileClw   = 'GRID_'//trim(satId)//'_'//trim(yyyymmdd)//'_clw_'//cends(icend)//'.dat'
    fileRr    = 'GRID_'//trim(satId)//'_'//trim(yyyymmdd)//'_rr_'//cends(icend)//'.dat'
    fileMirs  = 'GRID_'//trim(satId)//'_'//trim(yyyymmdd)//'_'//trim(prods5(iprod))//'_'//cends(icend)//'.dat' 
    fileRad   = 'GRID_'//trim(satId)//'_'//trim(nwpString)//'_'//trim(yyyymmdd)//'_tb_'//cends(icend)//'.dat'
    fileBias  = 'GRID_'//trim(satId)//'_'//trim(nwpString)//'_bias_'//trim(yyyymmdd)//&
                '_'//trim(prods5(iprod))//'_'//cends(icend)//'.dat'
    fileAsym  = 'GRID_'//trim(satId)//'_'//trim(nwpString)//'_asym_'//trim(yyyymmdd)//&
                '_'//trim(prods5(iprod))//'_'//cends(icend)//'.dat'
    write(*,'(A)')'fileBias='//trim(gridPath)//trim(fileBias) 
    write(*,'(A)')'fileAsym='//trim(gridPath)//trim(fileAsym) 
    call computeBias3(fileDepen,fileChisq,fileSfc,fileClw,fileRr,fileMirs,fileRad,fileBias,fileAsym,&
         gridfactor,gridPath,latmin,latmax,lonmin,lonmax,NCHAN,isatId,prodId)
  enddo
  enddo

  !*************************************************************
  !  Corrected TB residual ( both are in gridded EDR files )
  !  tbc - tbf 
  !  TB residual is defined as Scene%YmCorr(tbc) - Scene%YFwd(tbf)
  !*************************************************************
  !-- (NCOL,NROW,NCHAN) 
  prodId = 'tbc'
  do icend = 1, NCEND

    fileDepen = 'GRID_'//trim(satId)//'_'//trim(yyyymmdd)//'_'//TRIM(dependString)//'_'//cends(icend)//'.dat'
    fileChisq = 'GRID_'//trim(satId)//'_'//trim(yyyymmdd)//'_chisq_'//cends(icend)//'.dat'
    fileSfc   = 'GRID_'//trim(satId)//'_'//trim(yyyymmdd)//'_sfc2_'//cends(icend)//'.dat'
    fileClw   = 'GRID_'//trim(satId)//'_'//trim(yyyymmdd)//'_clw_'//cends(icend)//'.dat'
    fileRr    = 'GRID_'//trim(satId)//'_'//trim(yyyymmdd)//'_rr_'//cends(icend)//'.dat'
    fileMirs  = 'GRID_'//trim(satId)//'_'//trim(yyyymmdd)//'_tbc_'//cends(icend)//'.dat' 
    fileRad   = 'GRID_'//trim(satId)//'_'//trim(yyyymmdd)//'_tbf_'//cends(icend)//'.dat'
    fileBias  = 'GRID_'//trim(satId)//'_mirs_'//trim(nwpString)//'_bias_'//trim(yyyymmdd)//&
                '_tbc_'//cends(icend)//'.dat'
    fileAsym  = 'GRID_'//trim(satId)//'_mirs_'//trim(nwpString)//'_asym_'//trim(yyyymmdd)//&
                '_tbc_'//cends(icend)//'.dat'
    write(*,'(A)')'fileBias='//trim(gridPath)//trim(fileBias)
    write(*,'(A)')'fileAsym='//trim(gridPath)//trim(fileAsym) 
    call computeBias3(fileDepen,fileChisq,fileSfc,fileClw,fileRr,fileMirs,fileRad,fileBias,fileAsym,&
         gridfactor,gridPath,latmin,latmax,lonmin,lonmax,NCHAN,isatId,prodId)
  enddo

  !*************************************************************
  ! 2-dimension array (grid bias of MIRS LWP and NWP CLW)
  !*************************************************************
  do iprod = 1, NPROD6
  prodId = prods6(iprod)
  do icend = 1, NCEND  

    fileDepen = 'GRID_'//trim(satId)//'_'//trim(yyyymmdd)//'_'//TRIM(dependString)//'_'//cends(icend)//'.dat'
    fileChisq = 'GRID_'//trim(satId)//'_'//trim(yyyymmdd)//'_chisq_'//cends(icend)//'.dat'
    fileSfc   = 'GRID_'//trim(satId)//'_'//trim(yyyymmdd)//'_sfc2_'//cends(icend)//'.dat'      
    fileClw   = 'GRID_'//trim(satId)//'_'//trim(yyyymmdd)//'_clw_'//cends(icend)//'.dat'
    fileRr    = 'GRID_'//trim(satId)//'_'//trim(yyyymmdd)//'_rr_'//cends(icend)//'.dat'
    fileMirs  = 'GRID_'//trim(satId)//'_'//trim(yyyymmdd)//'_'//trim(prods6(iprod))//'_'//cends(icend)//'.dat'
    fileNwp   = 'GRID_'//trim(satId)//'_'//trim(nwpString)//'_'//trim(yyyymmdd)//'_'//&
                trim(prods2(3))//'_'//cends(icend)//'.dat'
    fileBias  = 'GRID_'//trim(satId)//'_'//trim(nwpString)//'_bias_'//trim(yyyymmdd)//&
                '_'//trim(prods6(iprod))//'_'//cends(icend)//'.dat' 
    fileAsym  = 'GRID_'//trim(satId)//'_'//trim(nwpString)//'_asym_'//trim(yyyymmdd)//&
                '_'//trim(prods6(iprod))//'_'//cends(icend)//'.dat'     
    write(*,'(A)')'fileBias='//trim(gridPath)//trim(fileBias) 
    write(*,'(A)')'fileAsym='//trim(gridPath)//trim(fileAsym)
    call computeBias2(fileDepen,fileChisq,fileSfc,fileClw,fileRr,fileMirs,fileNwp,fileBias,fileAsym,&
         gridfactor,gridPath,latmin,latmax,lonmin,lonmax,isatId,prodId)
  enddo
  enddo


  contains

  
  !*************************************************************************************************
  ! subroutine to compute 2-D bias and bias assymmetry
  !*************************************************************************************************
  subroutine computeBias2(fileDepen,fileChisq,fileSfc,fileClw,fileRr,fileMirs,fileNwp,fileBias,fileAsym,&
             gridfactor,gridPath,latmin,latmax,lonmin,lonmax,isatId,prodId)

  integer             :: NBIN
  character(LEN=lenf) :: fileDepen,fileChisq,fileSfc,fileClw,fileRr,fileMirs,fileNwp,fileBias,fileAsym,gridPath
  real                :: latmin,latmax,lonmin,lonmax,ang1,clw1,rr1
  integer             :: gridfactor, NCOL, NROW, irow, icol, ibin, isfc, icond, isatId
  character(LEN=8)    :: prodId
  integer             :: sfc1, cond1, bias1
  logical             :: cond_logic = .false.
  logical             :: sfc_logic = .false.
  logical             :: bin_logic = .false.
  logical             :: all_logic = .false.
  
  integer, dimension(:), allocatable    :: bin_box
  real,dimension(:,:), allocatable      :: angle,chisq,sfc,clw,rr
  
  real,dimension(:,:), allocatable      :: mirs,nwp,bias
  real, dimension(:,:,:), allocatable   :: asym
  real, dimension(:,:,:), allocatable   :: cont

  NCOL = 360*gridfactor
  NROW = 180*gridfactor

  NBIN=30
  if (isatId .eq. sensor_id_npp   ) NBIN=35
  if (isatId .eq. sensor_id_trmm  ) NBIN=26
  if (isatId .eq. sensor_id_gpm   ) NBIN=26
  if (isatId .eq. sensor_id_amsre ) NBIN=191
  if (isatId .eq. sensor_id_gcomw1) NBIN=243
! MT MADRAS proxy data currently based on CR TRMM; will need updating
!  if (isatId .eq. sensor_id_mtma  ) NBIN=60
  if (isatId .eq. sensor_id_mtma  ) NBIN=27
! MT SAPHIR proxy data currently based on LR N18; will need updating
  if (isatId .eq. sensor_id_mtsa  ) NBIN=35
!  if (isatId .eq. sensor_id_mtsa  ) NBIN=26

  allocate ( angle( 1:NCOL, 1:NROW) )
  allocate ( chisq( 1:NCOL, 1:NROW) )
  allocate ( sfc( 1:NCOL, 1:NROW) )
  allocate ( clw( 1:NCOL, 1:NROW) )
  allocate ( rr( 1:NCOL, 1:NROW) )
  allocate ( mirs( 1:NCOL, 1:NROW) )
  allocate ( nwp( 1:NCOL, 1:NROW) )
  allocate ( bias( 1:NCOL, 1:NROW) )
  allocate ( asym(NBIN,NSFC,NCOND) )
  allocate ( cont(NBIN,NSFC,NCOND) )
  allocate ( bin_box(NBIN+1) )
 
  open(22,file=trim(gridPath)//trim(fileDepen),form='unformatted',access='direct',recl=4*NCOL*NROW) 
  read(22,rec=1)angle
  close(22)

  open(22,file=trim(gridPath)//trim(fileChisq),form='unformatted',access='direct',recl=4*NCOL*NROW) 
  read(22,rec=1)chisq
  close(22)

  open(22,file=trim(gridPath)//trim(fileSfc),form='unformatted',access='direct',recl=4*NCOL*NROW) 
  read(22,rec=1)sfc
  close(22)

  open(22,file=trim(gridPath)//trim(fileClw),form='unformatted',access='direct',recl=4*NCOL*NROW) 
  read(22,rec=1)clw
  close(22)

  open(22,file=trim(gridPath)//trim(fileRr),form='unformatted',access='direct',recl=4*NCOL*NROW) 
  read(22,rec=1)rr
  close(22)

  open(22,file=trim(gridPath)//trim(fileMirs),form='unformatted',access='direct',recl=4*NCOL*NROW) 
  read(22,rec=1)mirs
  close(22)

  open(22,file=trim(gridPath)//trim(fileNwp), form='unformatted',access='direct',recl=4*NCOL*NROW) 
  read(22,rec=1)nwp
  close(22)

  !---- Bias, OK here, we can use gridded CLW and RR if we want to further filter it when plotting
  do irow = 1,NROW 
  do icol = 1,NCOL 
      if ( mirs(icol,irow) .ge. 0 .and. nwp(icol,irow) .ge. 0 .and. chisq(icol,irow) .le. CHISQ_CUT) then
          bias(icol,irow) = mirs(icol,irow) - nwp(icol,irow)
      else if ( mirs(icol,irow) .ge. 0 .and. nwp(icol,irow) .ge. 0 .and. chisq(icol,irow) .gt. CHISQ_CUT) then
          bias(icol,irow) = FAILING
      else 
          bias(icol,irow) = MISSING
      endif

  enddo
  enddo

  open(22,file=trim(gridPath)//trim(fileBias),form='unformatted',access='direct',recl=4*NCOL*NROW)
  write(22,rec=1)bias
  close(22)

  !---- Bias Assymmetry
  if ( isatId .eq. sensor_id_n18 .or. isatId .eq. sensor_id_metopA .or. &
       isatId .eq. sensor_id_n19 .or. isatId .eq. sensor_id_metopB ) then
    do ibin = 1, NBIN+1
        bin_box(ibin) = 4 * (ibin-1) - 60
    enddo
  else if( isatId .eq. sensor_id_npp ) then
    do ibin = 1, NBIN+1
        bin_box(ibin) = 4 * (ibin-1) - 70
    enddo
  else if ( isatId .eq. sensor_id_f16   .or. isatId .eq. sensor_id_f18 .or. &
            isatId .eq. sensor_id_f17   .or. &
            isatId .eq. sensor_id_trmm  .or. isatId .eq. sensor_id_gpm .or. &
            isatId .eq. sensor_id_amsre .or. isatId .eq. sensor_id_gcomw1 ) then
    do ibin = 1, NBIN+1
        bin_box(ibin) = ibin-1
    enddo
! MT MADRAS proxy data currently based on CR TRMM; will need updating
  else if ( isatId .eq. sensor_id_mtma ) then
    do ibin = 1, NBIN+1
        bin_box(ibin) = (ibin-1)
    enddo
! MT SAPHIR proxy data currently based on LR N18; will need updating
  else if ( isatId .eq. sensor_id_mtsa ) then
    do ibin = 1, NBIN+1
!        bin_box(ibin) = 4 * (ibin-1) - 70
        bin_box(ibin) = 4 * (ibin-1) - 52
    enddo
  else if ( isatId .eq. sensor_id_fy3ri ) then
    do ibin = 1, NBIN+1
        bin_box(ibin) = 4 * (ibin-1)
    enddo
  endif

  asym = 0.0
  cont = 0.0

  do irow = 1, NROW 
  do icol = 1, NCOL

    ang1 = angle(icol,irow)
    sfc1 = INT( sfc(icol,irow) ) + 1 !!!! NB
    clw1 = clw(icol,irow)
    rr1  = rr(icol,irow)
    bias1 = INT( bias(icol,irow) )

    if( rr1 .ge. RAIN_CUT ) then
      cond1 = 1
    else if( rr1 .lt. RAIN_CUT .and. clw1 .ge. CLW_CUT ) then
      cond1 = 2
    else if( rr1 .lt. RAIN_CUT .and. clw1 .lt. CLW_CUT ) then
      cond1 = 3  
    else
      cond1 = -999
    endif  

    do icond = 1, NCOND

      if( strcmp(prodId, 'tpw')   .eq. 0 .or. &
          strcmp(prodId, 'tskin') .eq. 0 .or. &
          strcmp(prodId, 'psfc')  .eq. 0 .or. &
          strcmp(prodId, 'wspd')  .eq. 0 .or. &
          strcmp(prodId, 'temp')  .eq. 0 .or. &
          strcmp(prodId, 'wv')    .eq. 0 .or. &
          strcmp(prodId, 'em')    .eq. 0 ) then

          cond_logic = icond .eq. cond1

      else
        cond_logic = .true.
      endif

      if( icond .eq. NCOND ) cond_logic = .true.

      do isfc = 1, NSFC

        if( isfc .lt. NSFC ) then
          sfc_logic = sfc1 .eq. isfc
        else
          sfc_logic = .true.
        endif

        do ibin = 1, NBIN

          bin_logic = ang1 .ge. BIN_BOX(ibin) .and. ang1 .lt. BIN_BOX(ibin+1) .and. bias1 .ne. MISSING .and. bias1 .ne. FAILING

          all_logic = cond_logic .and. sfc_logic .and. bin_logic

          if( all_logic ) then
             asym(ibin,isfc,icond) = asym(ibin,isfc,icond) + bias(icol,irow)
             cont(ibin,isfc,icond) = cont(ibin,isfc,icond) + 1
          endif      

        enddo

      enddo

    enddo

  enddo
  enddo
  
  !---- average
  do icond = 1, NCOND
  do isfc = 1, NSFC
  do ibin = 1, NBIN
    if( cont(ibin,isfc,icond) .ge. 1 ) then
      asym(ibin,isfc,icond) = asym(ibin,isfc,icond) / cont(ibin,isfc,icond)
    else
      asym(ibin,isfc,icond) = MISSING
    endif
  enddo
  enddo
  enddo

  open(22,file=trim(gridPath)//trim(fileAsym),form='unformatted',access='direct',recl=4*NBIN*NSFC*NCOND)
  write(22,rec=1)asym(1:NBIN,1:NSFC,1:NCOND)
  close(22)

  deallocate( angle )
  deallocate( chisq )
  deallocate( sfc  )
  deallocate( clw )
  deallocate( rr  )
  deallocate( mirs )
  deallocate( nwp  )
  deallocate( bias )
  deallocate( asym )
  deallocate( cont )
  deallocate( bin_box )

  end subroutine computeBias2


  !*************************************************************************************************
  ! subroutine to compute 3-d bias and bias assymmetry
  !*************************************************************************************************
  subroutine computeBias3(fileDepen,fileChisq,fileSfc,fileClw,fileRr,fileMirs,fileNwp,fileBias,fileAsym,&
             gridfactor,gridPath,latmin,latmax,lonmin,lonmax,NLAY,isatId,prodId)

  integer             :: NBIN
  character(LEN=lenf) :: fileDepen,fileChisq,fileSfc,fileClw,fileRr,fileMirs,fileNwp,fileBias,fileAsym,gridPath
  real                :: latmin,latmax,lonmin,lonmax,ang1,clw1,rr1
  integer             :: gridfactor, NCOL, NROW, NLAY, irow, icol, ilay, ibin, isfc, icond, isatId
  character(LEN=8)    :: prodId
  integer             :: sfc1, cond1, bias1, irec
  logical             :: cond_logic = .false.
  logical             :: sfc_logic = .false.
  logical             :: bin_logic = .false.
  logical             :: all_logic = .false.
  
  integer, dimension(:), allocatable    :: bin_box
  real,dimension(:,:), allocatable      :: angle,chisq,sfc,clw,rr
  
  real,dimension(:,:,:), allocatable    :: mirs,nwp,bias
  real, dimension(:,:,:,:), allocatable :: asym
  real, dimension(:,:,:,:), allocatable :: cont

  integer :: ncount

  NCOL = 360*gridfactor
  NROW = 180*gridfactor

  NBIN=30
  if (isatId .eq. sensor_id_npp   ) NBIN=35
  if (isatId .eq. sensor_id_trmm  ) NBIN=26
  if (isatId .eq. sensor_id_gpm   ) NBIN=26
  if (isatId .eq. sensor_id_amsre ) NBIN=191
  if (isatId .eq. sensor_id_gcomw1) NBIN=243
! MT MADRAS proxy data currently based on CR TRMM; will need updating
!  if (isatId .eq. sensor_id_mtma  ) NBIN=60
  if (isatId .eq. sensor_id_mtma  ) NBIN=27
! MT SAPHIR proxy data currently based on LR N18; will need updating
!  if (isatId .eq. sensor_id_mtsa  ) NBIN=35
  if (isatId .eq. sensor_id_mtsa  ) NBIN=26

  allocate ( angle( 1:NCOL, 1:NROW) )
  allocate ( chisq( 1:NCOL, 1:NROW) )
  allocate ( sfc( 1:NCOL, 1:NROW) )
  allocate ( clw( 1:NCOL, 1:NROW) )
  allocate ( rr( 1:NCOL, 1:NROW) )
  allocate ( mirs( 1:NCOL, 1:NROW, 1:NLAY) )
  allocate ( nwp(  1:NCOL, 1:NROW, 1:NLAY) )
  allocate ( bias( 1:NCOL, 1:NROW, 1:NLAY) )
  allocate ( asym(NBIN,NSFC,NCOND,NLAY) )
  allocate ( cont(NBIN,NSFC,NCOND,NLAY) )
  allocate ( bin_box(NBIN+1) )

  open(22,file=trim(gridPath)//trim(fileDepen),form='unformatted',access='direct',recl=4*NCOL*NROW) 
  read(22,rec=1)angle
  close(22)

  open(22,file=trim(gridPath)//trim(fileChisq),form='unformatted',access='direct',recl=4*NCOL*NROW) 
  read(22,rec=1)chisq
  close(22)

  open(22,file=trim(gridPath)//trim(fileSfc),form='unformatted',access='direct',recl=4*NCOL*NROW) 
  read(22,rec=1)sfc
  close(22)
  
  ncount=count(sfc .eq. 0)
  ncount=count(sfc .eq. 1)
  ncount=count(sfc .eq. 2)
  ncount=count(sfc .eq. 3)

  open(22,file=trim(gridPath)//trim(fileClw),form='unformatted',access='direct',recl=4*NCOL*NROW) 
  read(22,rec=1)clw
  close(22)

  open(22,file=trim(gridPath)//trim(fileRr),form='unformatted',access='direct',recl=4*NCOL*NROW) 
  read(22,rec=1)rr
  close(22)

  open(22,file=trim(gridPath)//trim(fileMirs),form='unformatted',access='direct',recl=4*NCOL*NROW) 
  do ilay=1,NLAY
      read(22,rec=ilay)mirs(1:NCOL,1:NROW,ilay)
  enddo
  close(22)

  open(22,file=trim(gridPath)//trim(fileNwp), form='unformatted',access='direct',recl=4*NCOL*NROW) 
  do ilay=1,NLAY
      read(22,rec=ilay)nwp(1:NCOL,1:NROW,ilay)
  enddo
  close(22)


  !---- Bias
  do ilay = 1,NLAY
  do irow = 1,NROW 
  do icol = 1,NCOL 
      if ( mirs(icol,irow,ilay) .ge. 0 .and. nwp(icol,irow,ilay) .ge. 0 .and. chisq(icol,irow) .le. CHISQ_CUT) then
          bias(icol,irow,ilay) = mirs(icol,irow,ilay) - nwp(icol,irow,ilay)
      else if ( mirs(icol,irow,ilay) .ge. 0 .and. nwp(icol,irow,ilay) .ge. 0 .and. chisq(icol,irow) .gt. CHISQ_CUT) then
          bias(icol,irow,ilay) = FAILING
      else
          bias(icol,irow,ilay) = MISSING
      endif
  enddo
  enddo
  enddo

  open(22,file=trim(gridPath)//trim(fileBias),form='unformatted',access='direct',recl=4*NCOL*NROW)
  do ilay = 1, NLAY
      write(22,rec=ilay)bias(1:NCOL,1:NROW,ilay)
  enddo
  close(22)


  !---- Bias Assymmetry
  if ( isatId .eq. sensor_id_n18 .or. isatId .eq. sensor_id_metopA .or. &
       isatId .eq. sensor_id_n19 .or. isatId .eq. sensor_id_metopB ) then
    do ibin = 1, NBIN+1
        bin_box(ibin) = 4 * (ibin-1) - 60
    enddo
  else if( isatId .eq. sensor_id_npp ) then
    do ibin = 1, NBIN+1
        bin_box(ibin) = 4 * (ibin-1) - 70
    enddo
  else if ( isatId .eq. sensor_id_f16   .or. isatId .eq. sensor_id_f18 .or. &
            isatId .eq. sensor_id_f17   .or. &
            isatId .eq. sensor_id_trmm  .or. isatId .eq. sensor_id_gpm .or. &
            isatId .eq. sensor_id_amsre .or. isatId .eq. sensor_id_gcomw1 ) then
    do ibin = 1, NBIN+1
        bin_box(ibin) = ibin-1
    enddo
! MT MADRAS proxy data currently based on CR TRMM; will need updating
  else if ( isatId .eq. sensor_id_mtma ) then
    do ibin = 1, NBIN+1
        bin_box(ibin) = ibin-1
    enddo
! MT SAPHIR proxy data currently based on LR N18; will need updating
  else if ( isatId .eq. sensor_id_mtsa ) then
    do ibin = 1, NBIN+1
        bin_box(ibin) = 4 * (ibin-1) - 70
    enddo
  else if ( isatId .eq. sensor_id_fy3ri ) then
    do ibin = 1, NBIN+1
        bin_box(ibin) = 4 * (ibin-1)
    enddo
  endif

  asym = 0.0
  cont = 0.0

  do ilay = 1, NLAY
  do irow = 1, NROW 
  do icol = 1, NCOL

    ang1 = angle(icol,irow)
    sfc1 = INT( sfc(icol,irow) ) + 1  !!!! NB
    clw1 = clw(icol,irow)
    rr1  = rr(icol,irow)
    bias1 = INT( bias(icol,irow,ilay) )

    if( rr1 .ge. RAIN_CUT ) then
      cond1 = 1
    else if( rr1 .lt. RAIN_CUT .and. clw1 .ge. CLW_CUT ) then
      cond1 = 2
    else if( rr1 .lt. RAIN_CUT .and. clw1 .lt. CLW_CUT ) then
      cond1 = 3  
    else
      cond1 = -999
    endif  

    do icond = 1, NCOND

      if( strcmp(prodId, 'tpw')   .eq. 0 .or. &
          strcmp(prodId, 'tskin') .eq. 0 .or. &
          strcmp(prodId, 'psfc')  .eq. 0 .or. &
          strcmp(prodId, 'wspd')  .eq. 0 .or. &
          strcmp(prodId, 'temp')  .eq. 0 .or. &
          strcmp(prodId, 'wv')    .eq. 0 .or. &
          strcmp(prodId, 'em')    .eq. 0 ) then

          cond_logic = icond .eq. cond1

      else
        cond_logic = .true.
      endif

      if( icond .eq. NCOND ) cond_logic = .true.
    
      do isfc = 1, NSFC
    
        if( isfc .lt. NSFC ) then
          sfc_logic = sfc1 .eq. isfc
        else
          sfc_logic = .true.
        endif

        do ibin = 1, NBIN

          bin_logic = ang1 .ge. BIN_BOX(ibin) .and. ang1 .lt. BIN_BOX(ibin+1) .and. bias1 .ne. MISSING .and. bias1 .ne. FAILING

          all_logic = cond_logic .and. sfc_logic .and. bin_logic

          if( all_logic ) then
            asym(ibin,isfc,icond,ilay) = asym(ibin,isfc,icond,ilay) + bias(icol,irow,ilay)
            cont(ibin,isfc,icond,ilay) = cont(ibin,isfc,icond,ilay) + 1
          endif      

        enddo

      enddo

    enddo

  enddo
  enddo
  enddo

  !---- average
  do ilay = 1, NLAY
  do icond = 1, NCOND
  do isfc = 1, NSFC
  do ibin = 1, NBIN
    if( cont(ibin,isfc,icond,ilay) .ge. 1 ) then
      asym(ibin,isfc,icond,ilay) = asym(ibin,isfc,icond,ilay) / cont(ibin,isfc,icond,ilay)
    else
      asym(ibin,isfc,icond,ilay) = MISSING
    endif
  enddo
  
  enddo
  enddo
  enddo

  open(22,file=trim(gridPath)//trim(fileAsym),form='unformatted',access='direct',recl=4*NBIN*NSFC*NCOND*NLAY)
  write(22,rec=1)asym(1:NBIN,1:NSFC,1:NCOND,1:NLAY)
  close(22)

  deallocate( angle )
  deallocate( chisq )
  deallocate( sfc  )
  deallocate( clw  )
  deallocate( rr  )
  deallocate( mirs )
  deallocate( nwp  )
  deallocate( bias )
  deallocate( asym )
  deallocate( cont )
  deallocate( bin_box )

  end subroutine computeBias3

end program gridBias

