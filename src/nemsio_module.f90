!----------------------------------------------------------------------------
module nemsio_module
!$$$ module document block
!
! module:   nemsio_module      API for NEMS input/output 
!
! Abstract: This module handles NEMS input/output
!
! Program history log
!    2006-11-10    Jun Wang  for gfsio
!    2008-02-29    Jun Wang
!    2008-11-04    Jun Wang - Changes to NFRAME; users can output fewer
!                             metadata records and update data fields in
!                             existing files.
!    2009-03-31    Jun Wang - use new bacio to handle >2gb file, changed
!                             record name to 16 characters
!    2009-04-28    Jun Wang - allow user to update meta data for date, forecast time
!
! Public Variables
! Public Defined Types
!   nemsio_gfile
!     private
!        gtype:   character(nemsio_charkind8)  NEMSIO file identifier
!        gdatatype:character(nemsio_charkind8) data format
!        modelname:character(nemsio_charkind8) modelname
!        version: integer(nemsio_intkind)   verion number
!        nmeta:   integer(nemsio_intkind)   number of metadata rec
!        lmeta:   integer(nemsio_intkind)   length of metadata rec 2 for model paramodels
!        nrec:    integer(nemsio_intkind)   number of data rec
!        idate(1:7):integer(nemsio_intkind) initial date (yyyy/mm/dd/hh/mm/ssn/ssd)
!        nfday:   integer(nemsio_intkind)   forecast day
!        nfhour:  integer(nemsio_intkind)   forecast hour
!        nfminute:integer(nemsio_intkind)   forecast minutes
!        nfsecondn:integer(nemsio_intkind)  numerator of forecast second fraction
!        nfsecondd:integer(nemsio_intkind)  denominator of forecast second fraction
!        dimy:    integer(nemsio_intkind)   dimension in latitude
!        dimx:    integer(nemsio_intkind)   dimension in Longitude
!        dimz:    integer(nemsio_intkind)   number of levels
!        nframe:  integer(nemsio_intkind)   dimension of halo
!        nsoil:    integer(nemsio_intkind)  number of soil layers
!        ntrac:    integer(nemsio_intkind)  number of tracers
!        jcap:    integer(nemsio_intkind)   spectral truncation
!        ncldt:   integer(nemsio_intkind)   number of cloud types
!        idsl:    integer(nemsio_intkind)   semi-lagrangian id
!        idvc:    integer(nemsio_intkind)   vertical coordinate id
!        idvm:    integer(nemsio_intkind)   mass variable id
!        idrt:    integer(nemsio_intkind)   grid identifier
!                 (idrt=4 for gaussian grid,
!                  idrt=0 for equally-spaced grid including poles,
!                  idrt=256 for equally-spaced grid excluding poles)
!        rlon_min:real(nemsio_realkind)     minimal longtitude of regional domain (global:set to 0)
!        rlon_max:real(nemsio_realkind)     maximal longtitude of regional domain (global:set to 360.)
!        rlat_min:real(nemsio_realkind)     minimal longtitude of regional domain (global:set to -90)
!        rlat_max:real(nemsio_realkind)     maximal longtitude of regional domain (global:set to 90)
!        extrameta:logical(nemsio_logickind)extra meta data flag 
!        nmetavari:integer(nemsio_intkind)  number of extra meta data integer variables
!        nmetavarr:integer(nemsio_intkind)  number of extra meta data real variables
!        nmetavarl:integer(nemsio_intkind)  number of extra meta data logical variables
!        nmetavarc:integer(nemsio_intkind)  number of extra meta data character variables
!        nmetaaryi:integer(nemsio_intkind)  number of extra meta data integer arrays
!        nmetaaryr:integer(nemsio_intkind)  number of extra meta data real arrays
!        nmetaaryl:integer(nemsio_intkind)  number of extra meta data logical arrays
!        nmetaaryc:integer(nemsio_intkind)  number of extra meta data character arrays
!
!        recname: character(nemsio_charkind),allocatable    recname(:)
!        reclevtyp: character(nemsio_charkind),allocatable    reclevtyp(:)
!        reclev:  integer(nemsio_intkind),allocatable       reclev(:)
!        vcoord:  real(nemsio_realkind),allocatable         vcoord(:,:,:)
!        lat:  real(nemsio_realkind),allocatable         lat(:) lat for mess point
!        lon:  real(nemsio_realkind),allocatable         lon(:) lon for mess point
!        gvlat1d: real(nemsio_realkind),allocatable         gvlat1d(:) lat for wind point
!        gvlon1d: real(nemsio_realkind),allocatable         gvlon1d(:) lon for wind point
!        Cpi:     real(nemsio_realkind),allocatable         cpi(:)
!        Ri:      real(nemsio_realkind),allocatable         ri(:)
!
!        variname:character(nemsio_charkind)  names of extra meta data integer variables
!        varrname:character(nemsio_charkind)  names of extra meta data real variables
!        varlname:character(nemsio_charkind)  names of extra meta data logical variables
!        varcname:character(nemsio_charkind)  names of extra meta data character variables
!        varival: integer(nemsio_intkind)     values of extra meta data integer variables
!        varrval: real(nemsio_realkind)       values of extra meta data integer variables
!        varlval: logical(nemsio_logickind)   values of extra meta data integer variables
!        varcval: character(nemsio_charkind)  values of extra meta data integer variables
!        aryiname:character(nemsio_charkind)  names of extra meta data integer arrays
!        aryrname:character(nemsio_charkind)  names of extra meta data real arrays
!        arylname:character(nemsio_charkind)  names of extra meta data logical arrays
!        arycname:character(nemsio_charkind)  names of extra meta data character arrays
!        aryilen: integer(nemsio_intkind)     lengths of extra meta data integer arrays
!        aryilen: integer(nemsio_intkind)     number of extra meta data integer arrays
!        aryilen: integer(nemsio_intkind)     number of extra meta data integer arrays

!!--- file handler
!        gfname:  character(255)  file name
!        gaction: character(nemsio_charkind)  read/write
!        flunit:  integer(nemsio_intkind)  unit number  
!
! Public method
!   nemsio_init
!   nemsio_finalize
!   nemsio_open
!   nemsio_writerec
!   nemsio_readirec
!   nemsio_writerecv
!   nemsio_readirecv
!   nemsio_writerecw34
!   nemsio_readirecw34
!   nemsio_writerecvw34
!   nemsio_readirecvw34
!   nemsio_close
!   nemsio_getfilehead
!   nemsio_getrechead
! Possible return code
!          0   Successful call
!         -1   Open or close I/O error
!         -2   array size
!         -3   Meta data I/O error (possible EOF)
!         -4   GETGB/PUTGB error
!         -5   Search record or set GRIB message info error
!         -6   allocate/deallocate error
!         -7   set grib table
!         -8   file meta data initialization (default:1152*576)
!         -9   NOT nemsio type file
!         -10  get/close file unit
!         -11  read/write bin data
!         -12  read/write NMM B grid lat lon
!         -13  read/write NMM sfc var
!         -15  read/write gsi 
!         -17  get var from file header
!
!$$$ end module document block
!
  use kinds, only: r_single,r_kind,i_kind,i_llong
  use constants, only: izero,ione,zero,quarter,half,one,two,four
  implicit none
  private
!------------------------------------------------------------------------------
! private variables and type needed by nemsio_gfile
  integer(i_kind),parameter:: nemsio_lmeta1=48,nemsio_lmeta3=32
  integer(i_kind),parameter:: nemsio_charkind=16,nemsio_charkind8=8
  integer(i_kind),parameter:: nemsio_logickind=4
  integer(i_kind),parameter     :: nemsio_intfill=-9999_i_kind
  integer(i_llong),parameter    :: nemsio_intfill8=-9999_i_llong
  logical(nemsio_logickind),parameter:: nemsio_logicfill=.false.
  real(i_kind),parameter     :: nemsio_kpds_intfill=-1_i_kind
  real(r_single),parameter   :: nemsio_realfill=-9999._r_single
!for grib
  real(r_single),parameter   :: nemsio_undef_grb=9.E20_r_single
!
!------------------------------------------------------------------------------
!---  public types
  type,public :: nemsio_gfile
    private
    character(nemsio_charkind8) :: gtype=' '
    integer(i_kind)             :: version=nemsio_intfill
    character(nemsio_charkind8) :: gdatatype=' '
    character(nemsio_charkind8) :: modelname=' '
    integer(i_kind)             :: nmeta=nemsio_intfill
    integer(i_kind)             :: lmeta=nemsio_intfill
    integer(i_kind)             :: nrec=nemsio_intfill
!
    integer(i_kind):: idate(7)=nemsio_intfill
    integer(i_kind):: nfday=nemsio_intfill
    integer(i_kind):: nfhour=nemsio_intfill
    integer(i_kind):: nfminute=nemsio_intfill
    integer(i_kind):: nfsecondn=nemsio_intfill
    integer(i_kind):: nfsecondd=nemsio_intfill
!    integer(i_kind):: ifdate(7)=nemsio_intfill
!
    integer(i_kind):: dimx=nemsio_intfill
    integer(i_kind):: dimy=nemsio_intfill
    integer(i_kind):: dimz=nemsio_intfill
    integer(i_kind):: nframe=nemsio_intfill
    integer(i_kind):: nsoil=nemsio_intfill
    integer(i_kind):: ntrac=nemsio_intfill
!
    integer(i_kind) :: jcap=nemsio_intfill
    integer(i_kind) :: ncldt=nemsio_intfill
    integer(i_kind) :: idvc=nemsio_intfill
    integer(i_kind) :: idsl=nemsio_intfill
    integer(i_kind) :: idvm=nemsio_intfill
    integer(i_kind) :: idrt=nemsio_intfill
    real(r_single) :: rlon_min=nemsio_realfill
    real(r_single) :: rlon_max=nemsio_realfill
    real(r_single) :: rlat_min=nemsio_realfill
    real(r_single) :: rlat_max=nemsio_realfill
    logical(nemsio_logickind) :: extrameta=nemsio_logicfill
!
    integer(i_kind):: nmetavari=nemsio_intfill
    integer(i_kind):: nmetavarr=nemsio_intfill
    integer(i_kind):: nmetavarl=nemsio_intfill
    integer(i_kind):: nmetavarc=nemsio_intfill
    integer(i_kind):: nmetaaryi=nemsio_intfill
    integer(i_kind):: nmetaaryr=nemsio_intfill
    integer(i_kind):: nmetaaryl=nemsio_intfill
    integer(i_kind):: nmetaaryc=nemsio_intfill
!
    character(nemsio_charkind),allocatable :: recname(:)
    character(nemsio_charkind),allocatable :: reclevtyp(:)
    integer(i_kind),allocatable    :: reclev(:)
!
    real(r_single),allocatable      :: vcoord(:,:,:)
    real(r_single),allocatable      :: lat(:)
    real(r_single),allocatable      :: lon(:)
    real(r_single),allocatable      :: dx(:)
    real(r_single),allocatable      :: dy(:)
!
    real(r_single),allocatable      :: Cpi(:)
    real(r_single),allocatable      :: Ri(:)
!
    character(nemsio_charkind),allocatable :: variname(:)
    integer(i_kind),allocatable    :: varival(:)
    character(nemsio_charkind),allocatable :: varrname(:)
    real(r_single),allocatable      :: varrval(:)
    character(nemsio_charkind),allocatable :: varlname(:)
    logical(nemsio_logickind),allocatable  :: varlval(:)
    character(nemsio_charkind),allocatable :: varcname(:)
    character(nemsio_charkind),allocatable :: varcval(:)
!
    character(nemsio_charkind),allocatable :: aryiname(:)
    integer(i_kind),allocatable    :: aryilen(:)
    integer(i_kind),allocatable    :: aryival(:,:)
    character(nemsio_charkind),allocatable :: aryrname(:)
    integer(i_kind),allocatable    :: aryrlen(:)
    real(r_single),allocatable      :: aryrval(:,:)
    character(nemsio_charkind),allocatable :: arylname(:)
    integer(i_kind),allocatable    :: aryllen(:)
    logical(nemsio_logickind),allocatable  :: arylval(:,:)
    character(nemsio_charkind),allocatable :: arycname(:)
    integer(i_kind),allocatable    :: aryclen(:)
    character(nemsio_charkind),allocatable :: arycval(:,:)
!  
    character(255) :: gfname
    character(nemsio_charkind8) :: gaction
    integer(i_llong)    :: tlmeta=nemsio_intfill
    integer(i_kind)    :: fieldsize=nemsio_intfill
    integer(i_kind)    :: flunit=nemsio_intfill
    integer(i_kind)    :: headvarinum=nemsio_intfill
    integer(i_kind)    :: headvarrnum=nemsio_intfill
    integer(i_kind)    :: headvarcnum=nemsio_intfill
    integer(i_kind)    :: headvarlnum=nemsio_intfill
    integer(i_kind)    :: headaryinum=nemsio_intfill
    integer(i_kind)    :: headaryrnum=nemsio_intfill
    integer(i_kind)    :: headarycnum=nemsio_intfill
    character(nemsio_charkind),allocatable :: headvarcname(:)
    character(nemsio_charkind),allocatable :: headvariname(:)
    character(nemsio_charkind),allocatable :: headvarrname(:)
    character(nemsio_charkind),allocatable :: headvarlname(:)
    character(nemsio_charkind),allocatable :: headaryiname(:)
    character(nemsio_charkind),allocatable :: headaryrname(:)
    character(nemsio_charkind),allocatable :: headarycname(:)
    integer(i_kind),allocatable    :: headvarival(:)
    real(r_single),allocatable      :: headvarrval(:)
    character(nemsio_charkind),allocatable :: headvarcval(:)
    logical(nemsio_logickind),allocatable  :: headvarlval(:)
    integer(i_kind),allocatable    :: headaryival(:,:)
    real(r_single),allocatable      :: headaryrval(:,:)
    character(nemsio_charkind),allocatable :: headarycval(:,:)
    character,allocatable       :: cbuf(:)
    integer(i_kind)     :: mbuf=0,nlen,nnum,mnum
    integer(i_llong)    :: tlmetalat=nemsio_intfill
    integer(i_llong)    :: tlmetalon=nemsio_intfill
    integer(i_llong)    :: tlmetadx=nemsio_intfill
    integer(i_llong)    :: tlmetady=nemsio_intfill
    integer(i_llong)    :: tlmetavarival=nemsio_intfill
    integer(i_llong)    :: tlmetaaryival=nemsio_intfill
  end type nemsio_gfile
!
!------------------------------------------------------------------------------
!--- private types
  type :: nemsio_meta1
    sequence
     character(nemsio_charkind8) :: gtype
     character(nemsio_charkind8) :: modelname
     character(nemsio_charkind8) :: gdatatype
     integer(i_kind) :: version,nmeta,lmeta
     integer(i_kind) :: reserve(3)
  end type nemsio_meta1
!
  type :: nemsio_meta2
    sequence
    integer(i_kind) :: nrec 
    integer(i_kind) :: idate(1:7),nfday,nfhour,nfminute,nfsecondn, &
                               nfsecondd,dimx,dimy,dimz,nframe,nsoil,ntrac,&
                               jcap,ncldt,idvc,idsl,idvm,idrt
    real(r_single)   :: rlon_min,rlon_max,rlat_min,rlat_max 
    logical(nemsio_logickind) :: extrameta
  end type nemsio_meta2
!
  type :: nemsio_meta3
    integer(i_kind) :: nmetavari,nmetavarr,nmetavarl,nmetavarc, &
                               nmetaaryi,nmetaaryr,nmetaaryl,nmetaaryc
  end type nemsio_meta3
!
  type  :: nemsio_grbmeta
    integer(i_kind)   :: jf=nemsio_intfill
    integer(i_kind)   :: j=nemsio_kpds_intfill
    integer(i_kind)   :: jpds(200)=nemsio_kpds_intfill
    integer(i_kind)   :: jgds(200)=nemsio_kpds_intfill
    logical*1,allocatable     :: lbms(:)
  end type nemsio_grbmeta
!
  type :: nemsio_grbtbl_item
    character(nemsio_charkind)     :: shortname=' '
    character(nemsio_charkind*2)   :: leveltype=' '
    integer(i_kind)    :: precision,g1lev,g1param,g1level 
  end type nemsio_grbtbl_item
!
  type :: nemsio_grbtbl
    integer(i_kind)                :: iptv
    type(nemsio_grbtbl_item)       :: item(255)
  end type nemsio_grbtbl 
!
  type(nemsio_grbtbl),save  :: gribtable(10)
!
!----- interface
  interface nemsio_getheadvar
    module procedure nemsio_getfheadvari
    module procedure nemsio_getfheadvarr
    module procedure nemsio_getfheadvarl
    module procedure nemsio_getfheadvarc
    module procedure nemsio_getfheadaryi
    module procedure nemsio_getfheadaryr
    module procedure nemsio_getfheadaryl
    module procedure nemsio_getfheadaryc
  end interface nemsio_getheadvar
!
  interface nemsio_setheadvar
    module procedure nemsio_setfheadvari
    module procedure nemsio_setfheadaryi
  end interface nemsio_setheadvar
!
  interface nemsio_readrec
    module procedure nemsio_readrec4
    module procedure nemsio_readrec8
  end interface nemsio_readrec
!
  interface nemsio_readrecv
    module procedure nemsio_readrecv4
    module procedure nemsio_readrecv8
  end interface nemsio_readrecv
!
  interface nemsio_writerec
    module procedure nemsio_writerec4
    module procedure nemsio_writerec8
  end interface nemsio_writerec
!
  interface nemsio_writerecv
    module procedure nemsio_writerecv4
    module procedure nemsio_writerecv8
  end interface nemsio_writerecv
!
  interface splat
    module procedure nemsio_splat4
    module procedure nemsio_splat8
  end interface splat
!
  interface nemsio_readrecbin4
    module procedure nemsio_readrecbin4d4
    module procedure nemsio_readrecbin4d8
  end interface nemsio_readrecbin4
!
  interface nemsio_readrecbin8
    module procedure nemsio_readrecbin8d4
    module procedure nemsio_readrecbin8d8
  end interface nemsio_readrecbin8
!
  interface nemsio_readrecvbin4
    module procedure nemsio_readrecvbin4d4
    module procedure nemsio_readrecvbin4d8
  end interface nemsio_readrecvbin4
!
  interface nemsio_readrecvbin8
    module procedure nemsio_readrecvbin8d4
    module procedure nemsio_readrecvbin8d8
  end interface nemsio_readrecvbin8
!
  interface nemsio_writerecbin4
    module procedure nemsio_writerecbin4d4
    module procedure nemsio_writerecbin4d8
  end interface nemsio_writerecbin4
!
  interface nemsio_writerecbin8
    module procedure nemsio_writerecbin8d4
    module procedure nemsio_writerecbin8d8
  end interface nemsio_writerecbin8
!
  interface nemsio_writerecvbin4
    module procedure nemsio_writerecvbin4d4
    module procedure nemsio_writerecvbin4d8
  end interface nemsio_writerecvbin4
!
  interface nemsio_writerecvbin8
    module procedure nemsio_writerecvbin8d4
    module procedure nemsio_writerecvbin8d8
  end interface nemsio_writerecvbin8
!
!--- file unit for putgb/getgb ----
  integer(i_kind),save   :: fileunit(600:699)=0
!------------------------------------------------------------------------------
!public mehtods
  public nemsio_undef_grb
  public nemsio_charkind,nemsio_charkind8,nemsio_logickind
  public nemsio_init,nemsio_finalize,nemsio_open,nemsio_close
  public nemsio_readrec,nemsio_writerec,nemsio_readrecv,nemsio_writerecv
  public nemsio_readrecw34,nemsio_writerecw34,nemsio_readrecvw34,nemsio_writerecvw34
  public nemsio_getfilehead,nemsio_getheadvar,nemsio_getrechead
  public nemsio_setfilehead,nemsio_setheadvar
!
contains
!-------------------------------------------------------------------------------
  subroutine nemsio_init(iret)
!$$$  subprogram documentation block
!                .      .    .                                      .
! subprogram:    nemsio_init
!   prgmmr:
!
! abstract: set grib table
!
! program history log:
!   2009-08-31  lueken - added subprogram doc block
!
!   input argument list:
!
!   output argument list:
!    iret
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! initialization
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    implicit none
    integer(i_kind),optional,intent(out):: iret
    integer(i_kind) :: ios

    call nemsio_setgrbtbl(ios)
    if ( present(iret)) iret=ios
    if ( ios.ne.izero) then
       if (present(iret)) return
       call nemsio_stop
    endif
!
  end subroutine nemsio_init
!------------------------------------------------------------------------------
  subroutine nemsio_finalize()
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    nemsio_finalize
!   prgmmr:
!
! abstract: finalization
!
! program history log:
!   2009-08-31  lueken - added subprogram doc block
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
    implicit none

  end subroutine nemsio_finalize
!------------------------------------------------------------------------------
  subroutine nemsio_open(gfile,gfname,gaction,iret,gdatatype,version, &
      nmeta,lmeta,modelname,nrec,idate,nfday,nfhour,                  &
      nfminute,nfsecondn,nfsecondd,                                   &
      dimx,dimy,dimz,nframe,nsoil,ntrac,jcap,ncldt,idvc,idsl,idvm,idrt,     &
      rlon_min,rlon_max,rlat_min,rlat_max,extrameta,           &
      nmetavari,nmetavarr,nmetavarl,                                        &
      nmetaaryi,nmetaaryr,nmetaaryl,                                        &
      recname,reclevtyp,reclev,vcoord,lat,lon,dx,dy,cpi,ri,                 &
      variname,varival,varrname,varrval,varlname,varlval,varcname,varcval,  &
      aryiname,aryilen,aryival,aryrname,aryrlen,aryrval,                    &
      arylname,aryllen,arylval,arycname,aryclen,arycval  )
!$$$  subprogram documentation block
!                .      .    .                                      .
! subprogram:    nemsio_open
!   prgmmr:
!
! abstract: open nemsio file, and read/write the meta data
!
! program history log:
!   2009-08-31  lueken - added subprogram doc block
!
!   input argument list:
!    gfile
!    gfname
!    gaction
!    gdatatype,modelname
!    version,nmeta,lmeta,nrec
!    idate(7),nfday,nfhour
!    nfminute, nfsecondn,nfsecondd
!    dimx,dimy,dimz,nframe
!    nsoil,ntrac
!    jcap,ncldt,idvc,idsl
!    idvm,idrt
!    rlat_min,rlat_max
!    rlon_min,rlon_max
!    extrameta
!    nmetavari,nmetavarr
!    nmetavarl,nmetaaryi,nmetaaryr,nmetaaryl
!    recname,reclevtyp
!    reclev
!    vcoord
!    lat,lon
!    dx,dy
!    Cpi,Ri
!    variname,varrname
!    varlname,varcname,aryiname,aryrname,arylname,arycname
!    aryilen,aryrlen
!    aryllen,aryclen
!    varival,aryival
!    varrval,aryrval
!    varlval,arylval
!    varcval,arycval
!
!   output argument list:
!    gfile
!    iret
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    implicit none
    type(nemsio_gfile),intent(inout)     :: gfile
    character*(*),intent(in)            :: gfname
    character*(*),intent(in)            :: gaction
!-------------------------------------------------------------------------------
! optional variables
!-------------------------------------------------------------------------------
    integer(i_kind),optional,intent(out) :: iret
    character*(*),optional,intent(in)    :: gdatatype,modelname
    integer(i_kind),optional,intent(in)  :: version,nmeta,lmeta,nrec
    integer(i_kind),optional,intent(in)  :: idate(7),nfday,nfhour,    &
            nfminute, nfsecondn,nfsecondd
    integer(i_kind),optional,intent(in)  :: dimx,dimy,dimz,nframe,    &
            nsoil,ntrac
    integer(i_kind),optional,intent(in)  :: jcap,ncldt,idvc,idsl,     &
            idvm,idrt
    real(r_single),optional,intent(in)    :: rlat_min,rlat_max,   &
             rlon_min,rlon_max
    logical(nemsio_logickind),optional,intent(in):: extrameta
    integer(i_kind),optional,intent(in)  :: nmetavari,nmetavarr, &   
            nmetavarl,nmetaaryi,nmetaaryr,nmetaaryl
!
    character*(*),optional,intent(in)            :: recname(:),reclevtyp(:)
    integer(i_kind),optional,intent(in)  :: reclev(:)
    real(r_single),optional,intent(in)    :: vcoord(:,:,:)
    real(r_single),optional,intent(in)    :: lat(:),lon(:)
    real(r_single),optional,intent(in)    :: dx(:),dy(:)
    real(r_single),optional,intent(in)    :: Cpi(:),Ri(:)
!
    character*(*),optional,intent(in)            :: variname(:),varrname(:),&
          varlname(:),varcname(:),aryiname(:),aryrname(:),arylname(:),arycname(:)
    integer(i_kind),optional,intent(in)  :: aryilen(:),aryrlen(:),  &
          aryllen(:),aryclen(:)
    integer(i_kind),optional,intent(in)  :: varival(:),aryival(:,:)
    real(r_single),optional,intent(in)    :: varrval(:),aryrval(:,:)
    logical(nemsio_logickind),optional,intent(in):: varlval(:),arylval(:,:)
    character(*),optional,intent(in)             :: varcval(:),arycval(:,:)
!
    integer(i_kind)      :: ios
!------------------------------------------------------------
! assign a unit number 
!------------------------------------------------------------
    if (present(iret)) iret=-ione
    call nemsio_getlu(gfile,gfname,gaction,ios)
    if ( ios.ne.izero ) then
       if ( present(iret))  then
         iret=ios
         return
       else
         call nemsio_stop
       endif
    endif
!------------------------------------------------------------
! open and read meta data for READ
!------------------------------------------------------------
!    print *,'in rcreate, gfname=',gfname,'gaction=',lowercase(gaction)
    if ( equal_str_nocase(trim(gaction),'read') .or. equal_str_nocase(trim(gaction),'rdwr')) then
      if ( equal_str_nocase(trim(gaction),'read') )then
       call baopenr(gfile%flunit,gfname,ios)
       if ( ios.ne.izero) then
        if ( present(iret))  then
          return
        else
          call nemsio_stop
        endif
       endif
      else
       call baopen(gfile%flunit,gfname,ios)
       if ( ios.ne.izero) then
        if ( present(iret))  then
          return
        else
          call nemsio_stop
        endif
       endif
     endif
!     print *,'open read file=',gfname
!
! read  meta data for gfile
!
       call nemsio_rcreate(gfile,ios)
       if ( ios.ne.izero) then
        if ( present(iret))  then
          iret=ios
          return
        else
          call nemsio_stop
        endif
       endif
!
!set grib index buf 
!
      if(gfile%gdatatype=='grib') then
       gfile%mbuf=256*1024
       gfile%nnum=izero
       gfile%nlen=izero
       gfile%mnum=-ione
       if(allocated(gfile%cbuf)) deallocate(gfile%cbuf)
       allocate(gfile%cbuf(gfile%mbuf))
      endif
!------------------------------------------------------------
! open and write meta data for WRITE
!------------------------------------------------------------
    elseif ( equal_str_nocase(trim(gaction),'write') ) then
      call baopenwt(gfile%flunit,gfname,ios)
      if ( ios.ne.izero) then
       if ( present(iret))  then
         return
       else
         call nemsio_stop
       endif
      endif
      call nemsio_wcreate(gfile,ios,gdatatype=gdatatype, &
        version=version, nmeta=nmeta,lmeta=lmeta,modelname=modelname,  &
        nrec=nrec,idate=idate,nfday=nfday,nfhour=nfhour,nfminute=nfminute,&
        nfsecondn=nfsecondn, nfsecondd=nfsecondd, &
        dimx=dimx,dimy=dimy,dimz=dimz,nframe=nframe,nsoil=nsoil,   &
        ntrac=ntrac,jcap=jcap,ncldt=ncldt,idvc=idvc,idsl=idsl,    &
        idvm=idvm,idrt=idrt,                          &
        rlon_min=rlon_min,rlon_max=rlon_max,rlat_min=rlat_min, &
        rlat_max=rlat_max,extrameta=extrameta, &
        nmetavari=nmetavari,nmetavarr=nmetavarr,  &
        nmetavarl=nmetavarl,nmetaaryi=nmetaaryi,nmetaaryr=nmetaaryr,&
        nmetaaryl=nmetaaryl,recname=recname,reclevtyp=reclevtyp,    &
        reclev=reclev,vcoord=vcoord,lat=lat,lon=lon,dx=dx,dy=dy,    &
        cpi=cpi,ri=ri,variname=variname,varival=varival,varrname=varrname,&
        varrval=varrval,varlname=varlname,varlval=varlval, &
        varcname=varcname,varcval=varcval, &
        aryiname=aryiname,aryilen=aryilen,aryival=aryival, &
        aryrname=aryrname,aryrlen=aryrlen,aryrval=aryrval, &
        arylname=arylname,aryllen=aryllen,arylval=arylval, &
        arycname=arycname,aryclen=aryclen,arycval=arycval  )
      if ( ios.ne.izero) then
       if ( present(iret))  then
         iret=ios
         return
       else
         call nemsio_stop
       endif
     endif
!------------------------------------------------------------
! if gaction is wrong
!------------------------------------------------------------
    else
       if ( present(iret))  then
         return
       else
         call nemsio_stop
       endif
    endif
!------------------------------------------------------------
! set default header
!------------------------------------------------------------
    if(.not.allocated(gfile%headvariname).or. &
       .not.allocated(gfile%headvarrname).or. &
       .not.allocated(gfile%headvarcname).or. &
       .not.allocated(gfile%headvarlname).or. &
       .not.allocated(gfile%headaryiname).or. &
       .not.allocated(gfile%headaryrname) ) then

      call nemsio_setfhead(gfile,ios)
      if ( present(iret)) iret=ios
      if ( ios.ne.izero) then
        if (present(iret)) return
        call nemsio_stop
      endif
    endif

    iret=izero
  end subroutine nemsio_open
!------------------------------------------------------------------------------
  subroutine nemsio_close(gfile,iret)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    nemsio_close
!   prgmmr:
!
! abstract: close gfile including closing the file, returning unit number, 
!           setting file meta data empty
!
! program history log:
!   2009-08-31  lueken - added subprogram doc block
!
!   input argument list:
!    gfile
!
!   output argument list:
!    gfile
!    iret
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block


    implicit none
    type(nemsio_gfile),intent(inout)     :: gfile
    integer(i_kind),optional,intent(out)  :: iret
    integer(i_kind)      :: ios
!------------------------------------------------------------
! close the file
!------------------------------------------------------------
    if ( present(iret) ) iret=-ione
    call baclose(gfile%flunit,ios)
    if ( ios.ne.izero) then
       if ( present(iret))  then
         return
       else
         call nemsio_stop
       endif
    endif
!------------------------------------------------------------
! free the file unit
!------------------------------------------------------------
    call nemsio_clslu(gfile,ios)
    if ( ios.ne.izero) then
       if ( present(iret))  then
         iret=ios
         return
       else
         call nemsio_stop
       endif
    endif
!------------------------------------------------------------
! empty gfile meta data
!------------------------------------------------------------
    call nemsio_axmeta(gfile,ios)
    if ( ios.ne.izero) then
       if ( present(iret))  then
         iret=ios
         return
       else
         call nemsio_stop
       endif
    endif
    if ( present(iret)) iret=izero
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  end subroutine nemsio_close
!------------------------------------------------------------------------------

  subroutine nemsio_rcreate(gfile,iret)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    nemsio_rcreate
!   prgmmr:
!
! abstract: read nemsio meta data
!
! program history log:
!   2009-08-31  lueken - added subprogram doc block
!
!   input argument list:
!    gfile
!
!   output argument list:
!    gfile
!    iret
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    implicit none
    type(nemsio_gfile),intent(inout)     :: gfile
    integer(i_kind),intent(out)  :: iret
!local variables
    integer(i_kind)      :: ios,nmeta
    integer(i_llong)     :: iskip,iread,nread
    type(nemsio_meta1)           :: meta1
    type(nemsio_meta2)           :: meta2
    type(nemsio_meta3)           :: meta3
    integer(i_kind)              :: i
    character(nemsio_charkind8),allocatable :: char8var(:)
!------------------------------------------------------------
! read first meta data record
!------------------------------------------------------------
    iret=-3
    iskip=izero
    iread=nemsio_lmeta1
    call bafrreadl(gfile%flunit,iskip,iread,nread,meta1)
!    print *,'in rcreate, iread=',iread,'nread=',nread
    if(nread.lt.iread) return
    gfile%tlmeta=nread
!    print *,'tlmeta=',gfile%tlmeta
    gfile%gtype=meta1%gtype
    gfile%version=meta1%version
    gfile%nmeta=meta1%nmeta
    gfile%lmeta=meta1%lmeta
    gfile%gdatatype=meta1%gdatatype
    gfile%modelname=meta1%modelname
    if ( trim(gfile%gdatatype).ne."bin4" .and. trim(gfile%gdatatype).ne."bin8" &
         .and. trim(gfile%gdatatype).ne."grib" ) then
      gfile%gdatatype="grib"
    endif
    if ( gfile%gtype(1:6) .ne. 'NEMSIO' ) then
      iret=-9
      return
    endif
    if ( gfile%nmeta .ne. 12 ) then
      print*,'WARNING: Not standard meta data, may not be ingested into GSI!!!'
!      iret=-9
!      return
    endif
!------------------------------------------------------------
! read second meta data record
!------------------------------------------------------------
    iskip=iskip+nread
    iread=gfile%lmeta
    call bafrreadl(gfile%flunit,iskip,iread,nread,meta2)
    if(nread.lt.iread) return
    gfile%tlmeta=gfile%tlmeta+nread
!    print *,'tlmeta2 =',gfile%tlmeta,'iskip=',iskip,'iread=',iread,'nread=',nread
    gfile%nrec=meta2%nrec
    gfile%idate(1:7)=meta2%idate(1:7)
    gfile%nfday=meta2%nfday
    gfile%nfhour=meta2%nfhour
    gfile%nfminute=meta2%nfminute
    gfile%nfsecondn=meta2%nfsecondn
    gfile%nfsecondd=meta2%nfsecondd
    gfile%dimx=meta2%dimx
    gfile%dimy=meta2%dimy
    gfile%dimz=meta2%dimz
    gfile%nframe=meta2%nframe
    gfile%nsoil=meta2%nsoil
    gfile%ntrac=meta2%ntrac
    gfile%jcap=meta2%jcap
    gfile%ncldt=meta2%ncldt
    gfile%idvc=meta2%idvc
    gfile%idsl=meta2%idsl
    gfile%idvm=meta2%idvm
    gfile%idrt=meta2%idrt
    gfile%rlon_min=meta2%rlon_min
    gfile%rlon_max=meta2%rlon_max
    gfile%rlat_min=meta2%rlat_min
    gfile%rlat_max=meta2%rlat_max
    gfile%extrameta=meta2%extrameta
    gfile%fieldsize=(gfile%dimx+2*gfile%nframe)*(gfile%dimy+2*gfile%nframe)

    nmeta=gfile%nmeta-2
!------------------------------------------------------------
! set up gfile required meata arrays
!------------------------------------------------------------
    call nemsio_almeta(gfile,ios)
    if ( ios .ne. izero ) then
      iret=ios
      return
    endif
!------------------------------------------------------------
! read gfile meta data array (meta rec 3:13)
!------------------------------------------------------------
!meta3:recname
    if ( gfile%nmeta.lt.3 ) then
      print *,'WARNING: no names,level type and  &
     &   levs for the fields in the meta data in this nemsio file'
    endif
    if(gfile%nmeta-2>izero) then
      iskip=iskip+nread
      iread=len(gfile%recname)*size(gfile%recname)
      call bafrreadl(gfile%flunit,iskip,iread,nread,gfile%recname)
      if(nread.lt.iread)  then
         iread=nemsio_charkind8*size(gfile%recname)
         allocate(char8var(size(gfile%recname)))
         call bafrreadl(gfile%flunit,iskip,iread,nread,char8var)
         gfile%recname=char8var
         deallocate(char8var)
         if (nread.lt.iread) return
      endif
      nmeta=nmeta-ione
      gfile%tlmeta=gfile%tlmeta+nread
!      print *,'tlmetarecname =',gfile%tlmeta,'nread=',nread
    endif

    if (gfile%nmeta-3>izero ) then
!meta4:reclevtyp
      iskip=iskip+nread
      iread=len(gfile%reclevtyp)*size(gfile%reclevtyp)
      call bafrreadl(gfile%flunit,iskip,iread,nread,gfile%reclevtyp)
      if(nread.lt.iread) return
      nmeta=nmeta-ione
      gfile%tlmeta=gfile%tlmeta+nread
!      print *,'tlmetareclwvtyp =',gfile%tlmeta,'nread=',nread
    endif

    if (gfile%nmeta-4 >izero ) then
!meta5:reclev
      iskip=iskip+nread
      iread=kind(gfile%reclev)*size(gfile%reclev)
      call bafrreadl(gfile%flunit,iskip,iread,nread,gfile%reclev)
      if(nread.lt.iread) return
      nmeta=nmeta-ione
      gfile%tlmeta=gfile%tlmeta+nread
!      print *,'tlmetareclev =',gfile%tlmeta,'nread=',nread
    endif

    if (gfile%nmeta-5 >izero ) then
!meta6:vcoord
      iskip=iskip+nread
      iread=kind(gfile%vcoord)*size(gfile%vcoord)
      call bafrreadl(gfile%flunit,iskip,iread,nread,gfile%vcoord)
      if(nread.lt.iread) return
      nmeta=nmeta-ione
      gfile%tlmeta=gfile%tlmeta+nread
!      print *,'tlmetavcoord =',gfile%tlmeta,'nread=',nread
    endif

    if ( gfile%nmeta-6>izero ) then
!meta7:lat
      iskip=iskip+nread
      iread=kind(gfile%lat)*size(gfile%lat)
      call bafrreadl(gfile%flunit,iskip,iread,nread,gfile%lat)
      if(nread.lt.iread) return
      nmeta=nmeta-ione
      gfile%tlmetalat=gfile%tlmeta
      gfile%tlmeta=gfile%tlmeta+nread
!      print *,'tlmetareclat =',gfile%tlmeta,'nread=',nread,   &
!         maxval(gfile%lat),minval(gfile%lat)
    endif

    if ( gfile%nmeta-7>izero ) then
!meta8:lon
      iskip=iskip+nread
      iread=kind(gfile%lon)*size(gfile%lon)
      call bafrreadl(gfile%flunit,iskip,iread,nread,gfile%lon)
      if(nread.lt.iread) return
      nmeta=nmeta-ione
      gfile%tlmetalon=gfile%tlmeta
      gfile%tlmeta=gfile%tlmeta+nread
!      print *,'tlmetareclon =',gfile%tlmeta,'nread=',nread,  &
!         maxval(gfile%lat),minval(gfile%lat)
    endif

    if ( gfile%nmeta-8>izero ) then
!meta9:dx
      iskip=iskip+nread
      iread=kind(gfile%dx)*size(gfile%dx)
      call bafrreadl(gfile%flunit,iskip,iread,nread,gfile%dx)
      if(nread.lt.iread) return
      nmeta=nmeta-ione
      gfile%tlmetadx=gfile%tlmeta
      gfile%tlmeta=gfile%tlmeta+nread
!      print *,'tlmetarecdx =',gfile%tlmeta,'nread=',nread,  &
!        maxval(gfile%dx),minval(gfile%dx)
    endif

    if ( gfile%nmeta-9>izero ) then
!meta10:dy
      iskip=iskip+nread
      iread=kind(gfile%dy)*size(gfile%dy)
      call bafrreadl(gfile%flunit,iskip,iread,nread,gfile%dy)
      if(nread.lt.iread) return
      nmeta=nmeta-ione
      gfile%tlmetady=gfile%tlmeta
      gfile%tlmeta=gfile%tlmeta+nread
!      print *,'tlmetarecdy =',gfile%tlmeta,'nread=',nread, &
!         maxval(gfile%dy),maxval(gfile%dy)
    endif

     if ( gfile%nmeta-10>izero ) then
!meta11:cpi
      iskip=iskip+nread
      iread=kind(gfile%cpi)*size(gfile%Cpi)
      call bafrreadl(gfile%flunit,iskip,iread,nread,gfile%Cpi)
      if(nread.lt.iread) return
      nmeta=nmeta-ione
      gfile%tlmeta=gfile%tlmeta+nread
!      print *,'tlmetacpi =',gfile%tlmeta,'nread=',nread
    endif

    if ( gfile%nmeta-11>izero ) then
!Ri
      iskip=iskip+nread
      iread=kind(gfile%ri)*size(gfile%Ri)
      call bafrreadl(gfile%flunit,iskip,iread,nread,gfile%Ri)
      if(nread.lt.iread) return
      nmeta=nmeta-ione
      gfile%tlmeta=gfile%tlmeta+nread
!      print *,'tlmetri =',gfile%tlmeta,'nread=',nread
    endif
!
    if ( gfile%nmeta-12>izero ) then
      print *,'nmeta=',nmeta,' WARNING:there are more meta to be read!'
    endif
     
    if(gfile%extrameta) then
!------------------------------------------------------------
! read out extra meta data
!------------------------------------------------------------
    iskip=iskip+nread
    iread=nemsio_lmeta3
    call bafrreadl(gfile%flunit,iskip,iread,nread,meta3)
    if(nread.lt.iread) return
    gfile%tlmeta=gfile%tlmeta+nread
    gfile%nmetavari=meta3%nmetavari
    gfile%nmetavarr=meta3%nmetavarr
    gfile%nmetavarl=meta3%nmetavarl
    gfile%nmetavarc=meta3%nmetavarc
    gfile%nmetaaryi=meta3%nmetaaryi
    gfile%nmetaaryr=meta3%nmetaaryr
    gfile%nmetaaryl=meta3%nmetaaryl
    gfile%nmetaaryc=meta3%nmetaaryc
!      print *,'tlmeta3 =',gfile%tlmeta,'nread=',nread, &
!     'nmetavari=',gfile%nmetavari,'nvarr=',gfile%nmetavarr, &
!     'varl=',gfile%nmetavarl,'varc=',gfile%nmetavarc, &
!     gfile%nmetaaryi,gfile%nmetaaryr,gfile%nmetaaryl,gfile%nmetaaryc
   
    call nemsio_alextrameta(gfile,ios)
    if ( ios .ne. izero ) then
      iret=ios
      return
    endif
!    print *,'after nemsio_alextrameta'

!meta var integer
    if (gfile%nmetavari.gt.izero) then
      iskip=iskip+nread
      iread=len(gfile%variname)*gfile%nmetavari
      call bafrreadl(gfile%flunit,iskip,iread,nread,gfile%variname)
!      print *,'after get varint name,iskip=',iskip,'iread=',iread,'nread=',nread
      if(nread.lt.iread)  then
         iread=nemsio_charkind8*gfile%nmetavari
         allocate(char8var(gfile%nmetavari))
         call bafrreadl(gfile%flunit,iskip,iread,nread,char8var)
         gfile%variname=char8var
         deallocate(char8var)
!      print *,'after get varint name8,iskip=',iskip,'iread=',iread,'nread=',nread
         if (nread.lt.iread) return
      endif
      gfile%tlmeta=gfile%tlmeta+nread
!      print *,'tlmetavari =',gfile%tlmeta,'nread=',nread,'iread=',iread,gfile%nmetavari
      iskip=iskip+nread
      iread=i_kind*gfile%nmetavari
      call bafrreadl(gfile%flunit,iskip,iread,nread,gfile%varival)
      if(nread.lt.iread) return
      gfile%tlmetavarival=gfile%tlmeta
      gfile%tlmeta=gfile%tlmeta+nread 
!      print *,'tlmetavarival =',gfile%tlmetavarival,gfile%tlmeta,'nread=',nread
    endif
!meta var real
    if (gfile%nmetavarr.gt.izero) then
      iskip=iskip+nread
      iread=len(gfile%varrname)*gfile%nmetavarr
      call bafrreadl(gfile%flunit,iskip,iread,nread,gfile%varrname)
      if(nread.lt.iread)  then
         iread=nemsio_charkind8*gfile%nmetavarr
         allocate(char8var(gfile%nmetavarr))
         call bafrreadl(gfile%flunit,iskip,iread,nread,char8var)
         gfile%varrname=char8var
         deallocate(char8var)
         if (nread.lt.iread) return
      endif
      gfile%tlmeta=gfile%tlmeta+nread
!      print *,'tlmetavarr =',gfile%tlmeta,'nread=',nread,gfile%nmetavarr
      iskip=iskip+nread
      iread=kind(gfile%varrval)*gfile%nmetavarr
      call bafrreadl(gfile%flunit,iskip,iread,nread,gfile%varrval)
      if(nread.lt.iread) return
      gfile%tlmeta=gfile%tlmeta+nread
!      print *,'tlmetavarrval =',gfile%tlmeta,'nread=',nread
    endif
!meta var logical
    if (gfile%nmetavarl.gt.izero) then
      iskip=iskip+nread
      iread=len(gfile%varlname)*gfile%nmetavarl
      call bafrreadl(gfile%flunit,iskip,iread,nread,gfile%varlname)
      if(nread.lt.iread)  then
         iread=nemsio_charkind8*gfile%nmetavarl
         allocate(char8var(gfile%nmetavarl))
         call bafrreadl(gfile%flunit,iskip,iread,nread,char8var)
         gfile%varlname=char8var
         deallocate(char8var)
         if (nread.lt.iread) return
      endif
      gfile%tlmeta=gfile%tlmeta+nread
!      print *,'tlmetavarl =',gfile%tlmeta,'nread=',nread,gfile%nmetavarl
      iskip=iskip+nread
      iread=nemsio_logickind*gfile%nmetavarl
      call bafrreadl(gfile%flunit,iskip,iread,nread,gfile%varlval)
      if(nread.lt.iread) return
      gfile%tlmeta=gfile%tlmeta+nread
!      print *,'tlmetavarlval =',gfile%tlmeta,'nread=',nread
    endif
!meta var string
    if (gfile%nmetavarc.gt.izero) then
      iskip=iskip+nread
      iread=len(gfile%varcname)*gfile%nmetavarc
      call bafrreadl(gfile%flunit,iskip,iread,nread,gfile%varcname)
      if(nread.lt.iread)  then
         iread=nemsio_charkind8*gfile%nmetavarc
         allocate(char8var(gfile%nmetavarc))
         call bafrreadl(gfile%flunit,iskip,iread,nread,char8var)
         gfile%varcname=char8var
         deallocate(char8var)
         if (nread.lt.iread) return
      endif
      gfile%tlmeta=gfile%tlmeta+nread
!      print *,'tlmetavarc =',gfile%tlmeta,'nread=',nread,gfile%nmetavarc
      iskip=iskip+nread
      iread=len(gfile%varcval)*gfile%nmetavarc
      call bafrreadl(gfile%flunit,iskip,iread,nread,gfile%varcval)
      if(nread.lt.iread) return
      gfile%tlmeta=gfile%tlmeta+nread
!      print *,'tlmetavarcval =',gfile%tlmeta,'nread=',nread
    endif
!meta arr integer
    if (gfile%nmetaaryi.gt.izero) then
      iskip=iskip+nread
      iread=len(gfile%aryiname)*gfile%nmetaaryi
      call bafrreadl(gfile%flunit,iskip,iread,nread,gfile%aryiname)
      if(nread.lt.iread)  then
         iread=nemsio_charkind8*gfile%nmetaaryi
         allocate(char8var(gfile%nmetaaryi))
         call bafrreadl(gfile%flunit,iskip,iread,nread,char8var)
         gfile%aryiname=char8var
         deallocate(char8var)
         if (nread.lt.iread) return
      endif
      gfile%tlmeta=gfile%tlmeta+nread
!      print *,'tlmetaaryinam =',gfile%tlmeta,'nread=',nread
      iskip=iskip+nread
      iread=kind(gfile%nmetaaryi)*gfile%nmetaaryi
      call bafrreadl(gfile%flunit,iskip,iread,nread,gfile%aryilen)
      if(nread.lt.iread) return
      gfile%tlmeta=gfile%tlmeta+nread
      gfile%tlmetaaryival=gfile%tlmeta
!      print *,'tlmetaaryilen =',gfile%tlmeta,'nread=',nread
      allocate(gfile%aryival(maxval(gfile%aryilen),gfile%nmetaaryi))
      do i=1,gfile%nmetaaryi
        iskip=iskip+nread
        iread=kind(gfile%aryival)*gfile%aryilen(i)
        call bafrreadl(gfile%flunit,iskip,iread,nread,gfile%aryival(:,i))
        if(nread.lt.iread) return
        gfile%tlmeta=gfile%tlmeta+nread

!      print *,'tlmetaaryival =',gfile%tlmeta,'nread=',nread
      enddo
    endif
!meta arr real
    if (gfile%nmetaaryr.gt.izero) then
      iskip=iskip+nread
      iread=len(gfile%aryrname)*gfile%nmetaaryr
      call bafrreadl(gfile%flunit,iskip,iread,nread,gfile%aryrname)
      if(nread.lt.iread)  then
         iread=nemsio_charkind8*gfile%nmetaaryr
         allocate(char8var(gfile%nmetaaryr))
         call bafrreadl(gfile%flunit,iskip,iread,nread,char8var)
         gfile%aryrname=char8var
         deallocate(char8var)
         if (nread.lt.iread) return
      endif
      gfile%tlmeta=gfile%tlmeta+nread
!      print *,'tlmetaaryrnam =',gfile%tlmeta,'nread=',nread
      iskip=iskip+nread
      iread=kind(gfile%aryrlen)*gfile%nmetaaryr
      call bafrreadl(gfile%flunit,iskip,iread,nread,gfile%aryrlen)
      if(nread.lt.iread) return
      gfile%tlmeta=gfile%tlmeta+nread
      print *,'tlmetaaryrlen =',gfile%tlmeta,'nread=',nread
      allocate(gfile%aryrval(maxval(gfile%aryrlen),gfile%nmetaaryr) )
      do i=1,gfile%nmetaaryr
        iskip=iskip+nread
        iread=kind(gfile%aryrval)*gfile%aryrlen(i)
        call bafrreadl(gfile%flunit,iskip,iread,nread,gfile%aryrval(:,i))
        if(nread.lt.iread) return
        gfile%tlmeta=gfile%tlmeta+nread
!      print *,'tlmetaaryrval =',gfile%tlmeta,'nread=',nread
      enddo
    endif
!meta arr logical
    if (gfile%nmetaaryl.gt.izero) then
      iskip=iskip+nread
      iread=len(gfile%arylname)*gfile%nmetaaryl
      call bafrreadl(gfile%flunit,iskip,iread,nread,gfile%arylname)
      if(nread.lt.iread)  then
         iread=nemsio_charkind8*gfile%nmetaaryl
         allocate(char8var(gfile%nmetaaryl))
         call bafrreadl(gfile%flunit,iskip,iread,nread,char8var)
         gfile%arylname=char8var
         deallocate(char8var)
         if (nread.lt.iread) return
      endif
      gfile%tlmeta=gfile%tlmeta+nread
      iskip=iskip+nread
      iread=kind(gfile%aryllen)*gfile%nmetaaryl
      call bafrreadl(gfile%flunit,iskip,iread,nread,gfile%aryllen)
      if(nread.lt.iread) return
      gfile%tlmeta=gfile%tlmeta+nread
      allocate(gfile%arylval(maxval(gfile%aryllen),gfile%nmetaaryl) )
      do i=1,gfile%nmetaaryl
        iskip=iskip+nread
        iread=kind(gfile%arylval)*gfile%aryllen(i)
        call bafrreadl(gfile%flunit,iskip,iread,nread,gfile%arylval(:,i))
        if(nread.lt.iread) return
        gfile%tlmeta=gfile%tlmeta+nread
      enddo
    endif
!meta arr char
    if (gfile%nmetaaryc.gt.izero) then
      iskip=iskip+nread
      iread=len(gfile%arycname)*gfile%nmetaaryc
      call bafrreadl(gfile%flunit,iskip,iread,nread,gfile%arycname)
      if(nread.lt.iread)  then
         iread=nemsio_charkind8*gfile%nmetaaryc
         allocate(char8var(gfile%nmetaaryc))
         call bafrreadl(gfile%flunit,iskip,iread,nread,char8var)
         gfile%arycname=char8var
         deallocate(char8var)
         if (nread.lt.iread) return
      endif
      gfile%tlmeta=gfile%tlmeta+nread
      iskip=iskip+nread
      iread=kind(gfile%aryclen)*gfile%nmetaaryc
      call bafrreadl(gfile%flunit,iskip,iread,nread,gfile%aryclen)
      if(nread.lt.iread) return
      gfile%tlmeta=gfile%tlmeta+nread
      allocate(gfile%arycval(maxval(gfile%aryclen),gfile%nmetaaryc) )
      do i=1,gfile%nmetaaryc
        iskip=iskip+nread
        iread=len(gfile%arycval)*gfile%aryclen(i)
        call bafrreadl(gfile%flunit,iskip,iread,nread,gfile%arycval(:,i))
        if(nread.lt.iread) return
        gfile%tlmeta=gfile%tlmeta+nread
      enddo
    endif
!
!end if extrameta
   endif
!
   print *,'end of rcreate!'
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
   iret=izero
  end subroutine nemsio_rcreate
!------------------------------------------------------------------------------

  subroutine nemsio_wcreate(gfile,iret,gdatatype,version,  &
      nmeta,lmeta,modelname,nrec,idate,nfday,              &
      nfhour,nfminute,nfsecondn,nfsecondd,                 &
      dimx,dimy,dimz,nframe,nsoil,ntrac,jcap,ncldt,idvc,idsl,idvm,idrt,     &
      rlon_min,rlon_max,rlat_min,rlat_max,extrameta,                        &
      nmetavari,nmetavarr,nmetavarl,nmetavarc,                              &
      nmetaaryi,nmetaaryr,nmetaaryl,nmetaaryc,                              &
      recname,reclevtyp,reclev,vcoord,lat,lon,dx,dy,cpi,ri,                 &
      variname,varival,varrname,varrval,varlname,varlval,varcname,varcval,  &
      aryiname,aryilen,aryival,aryrname,aryrlen,aryrval,                    &
      arylname,aryllen,arylval,arycname,aryclen,arycval  )
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    nemsio_wcreate
!   prgmmr:
!
! abstract: write nemsio meta data
!
! program history log:
!   2009-08-31  lueken - added subprogram doc block
!
!   input argument list:
!    gfile
!    gdatatype,modelname
!    version,nmeta,lmeta,nrec
!    idate(7),nfday,nfhour
!    nfminute,nfsecondn,nfsecondd
!    dimx,dimy,dimz,nframe
!    nsoil,ntrac
!    jcap,ncldt,idvc,idsl
!    idvm,idrt
!    rlat_min,rlat_max
!    rlon_min,rlon_max
!    extrameta
!    nmetavari,nmetavarr
!    metavarl,nmetavarc,nmetaaryi,nmetaaryr,nmetaaryl,nmetaaryc
!    recname,reclevtyp
!    reclev
!    vcoord
!    lat,lon
!    dx,dy
!    Cpi,Ri
!    variname,varrname
!    varlname,varcname,aryiname,aryrname,arylname,arycname
!    aryilen,aryrlen
!    aryllen,aryclen
!    varival,aryival
!    varrval,aryrval
!    varlval,arylval
!    varcval,arycval
!    
!   output argument list:
!    gfile
!    iret
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    implicit none
    type(nemsio_gfile),intent(inout)            :: gfile
    integer(i_kind),intent(out)                 :: iret
!optional variables
    character*(*),optional,intent(in)           :: gdatatype,modelname
    integer(i_kind),optional,intent(in)         :: version,nmeta,lmeta,nrec
    integer(i_kind),optional,intent(in)         :: idate(7),nfday,nfhour,  &
            nfminute,nfsecondn,nfsecondd
    integer(i_kind),optional,intent(in)         :: dimx,dimy,dimz,nframe,    &
            nsoil,ntrac
    integer(i_kind),optional,intent(in)         :: jcap,ncldt,idvc,idsl,     &
            idvm,idrt
    real(r_single),optional,intent(in)          :: rlat_min,rlat_max,   &
             rlon_min,rlon_max
    logical(nemsio_logickind),optional,intent(in):: extrameta
    integer(i_kind),optional,intent(in)  :: nmetavari,nmetavarr, &
            nmetavarl,nmetavarc,nmetaaryi,nmetaaryr,nmetaaryl,nmetaaryc
!
    character*(*),optional,intent(in)            :: recname(:),reclevtyp(:)
    integer(i_kind),optional,intent(in)  :: reclev(:)
    real(r_single),optional,intent(in)    :: vcoord(:,:,:)
    real(r_single),optional,intent(in)    :: lat(:),lon(:)
    real(r_single),optional,intent(in)    :: dx(:),dy(:)
    real(r_single),optional,intent(in)    :: Cpi(:),Ri(:)
!
    character*(*),optional,intent(in)            :: variname(:),varrname(:),&
          varlname(:),varcname(:),aryiname(:),aryrname(:),arylname(:),arycname(:)
    integer(i_kind),optional,intent(in)  :: aryilen(:),aryrlen(:),  &
          aryllen(:),aryclen(:)
    integer(i_kind),optional,intent(in)  :: varival(:),aryival(:,:)
    real(r_single),optional,intent(in)    :: varrval(:),aryrval(:,:)
    logical(nemsio_logickind),optional,intent(in):: varlval(:),arylval(:,:)
    character(*),optional,intent(in)             :: varcval(:),arycval(:,:)
!
!---  local variables
!
    integer(i_llong) :: iskip,iwrite,nwrite
    type(nemsio_meta1)      :: meta1
    type(nemsio_meta2)      :: meta2
    type(nemsio_meta3)      :: meta3
    integer(i_kind)         :: i,ios,nummeta
    logical :: linit
!------------------------------------------------------------
! set gfile meta data to operational model (default) if it's empty
!------------------------------------------------------------
    iret=-3
    gfile%gtype="NEMSIO"
    if(present(gdatatype)) then
      if ( trim(gdatatype).ne.'grib'.and.trim(gdatatype).ne.'bin4'.and. &
           trim(gdatatype).ne.'bin8' ) return
      gfile%gdatatype=gdatatype
    else
      gfile%gdatatype='grib'
    endif
    if(present(modelname)) then 
      gfile%modelname=modelname
    else
      gfile%modelname="GFS"
    endif
!
!    print *,'NEMSIO file,datatype,model is ',gfile%gtype, &
!        gfile%gdatatype,gfile%modelname,idate(1:7)
    if(present(version)) gfile%version=version
    if(present(dimx)) gfile%dimx=dimx
    if(present(dimy)) gfile%dimy=dimy
    if(present(dimz)) gfile%dimz=dimz
    if(present(nrec)) gfile%nrec=nrec
    if(present(nmeta)) gfile%nmeta=nmeta
    if(gfile%nmeta==nemsio_intfill) gfile%nmeta=12
    if(present(lmeta)) gfile%lmeta=lmeta
    if(gfile%lmeta==nemsio_intfill)   &
      gfile%lmeta=25*i_kind+4*r_single+nemsio_logickind
!    print *,'in wcreate, dim=',gfile%dimx,gfile%dimy,gfile%dimz,'nmeta=',gfile%nmeta,gfile%lmeta
    if(present(nsoil)) gfile%nsoil=nsoil
    if(gfile%nsoil.eq.nemsio_intfill) gfile%nsoil=4
    if(present(nframe)) gfile%nframe=nframe
    if(gfile%nframe.eq.nemsio_intfill) gfile%nframe=izero
    if(gfile%modelname=='GFS')gfile%nframe=izero
    if(present(idate)) gfile%idate=idate
    if ( gfile%idate(1) .lt. 50) then
        gfile%idate(1)=2000+gfile%idate(1)
    else if (gfile%idate(1) .lt. 100) then
        gfile%idate(1)=1999+gfile%idate(1)
    endif
    if ( gfile%idate(1).eq.nemsio_intfill) then
      print *,'idate=',gfile%idate,' WRONG: please provide idate(1:7)(yyyy/mm/dd/hh/min/secn/secd)!!!'
      call nemsio_stop()
    endif
!
    linit= gfile%dimx .eq. nemsio_intfill .or. gfile%dimy .eq. nemsio_intfill &
      .or. gfile%dimz .eq. nemsio_intfill .or. gfile%nrec .eq. nemsio_intfill &
      .or. gfile%nmeta .eq. 12
!    
    if ( gfile%gtype(1:6).eq."NEMSIO" .and. linit ) then
      call nemsio_gfinit(gfile,ios,recname=recname,reclevtyp=reclevtyp,reclev=reclev)
      if (ios .ne.izero ) then
        iret=ios
        return
      endif
    endif
    print*,'after gfinit,gtype=',gfile%gtype,'nmeta=',gfile%nmeta,'linit=',linit
!
!------------------------------------------------------------
! set up basic gfile meta data variables from outsides to 
! define meta data array
!------------------------------------------------------------
    if(present(nfday)) gfile%nfday=nfday
    if(present(nfhour)) gfile%nfhour=nfhour
    if(present(nfminute)) gfile%nfminute=nfminute
    if(present(nfsecondn)) gfile%nfsecondn=nfsecondn
    if(present(nfsecondd)) gfile%nfsecondd=nfsecondd
    if(present(ntrac)) gfile%ntrac=ntrac
    if(gfile%ntrac.eq.nemsio_intfill) gfile%ntrac=izero
    if(present(ncldt)) gfile%ncldt=ncldt
    if(present(jcap)) gfile%jcap=jcap
    if(present(idvc)) gfile%idvc=idvc
    if(present(idsl)) gfile%idsl=idsl
    if(present(idvm)) gfile%idvm=idvm
    if(present(idrt)) gfile%idrt=idrt
    if(present(rlon_min)) gfile%rlon_min=rlon_min
    if(present(rlon_max)) gfile%rlon_max=rlon_max
    if(present(rlat_min)) gfile%rlat_min=rlat_min
    if(present(rlat_max)) gfile%rlat_max=rlat_max
    if(present(extrameta)) gfile%extrameta=extrameta
    if(gfile%fieldsize.eq.nemsio_intfill) &
       gfile%fieldsize=(gfile%dimx+2*gfile%nframe)*(gfile%dimy+2*gfile%nframe)
!
    if( gfile%extrameta )then
      if(present(nmetavari).and.nmetavari.gt.izero.and.present(variname) &
         .and.size(variname).eq.nmetavari .and. &
         present(varival).and.size(varival).eq.nmetavari) then
           gfile%nmetavari=nmetavari
           if(allocated(gfile%variname)) deallocate(gfile%variname)
           if(allocated(gfile%varival)) deallocate(gfile%varival)
           allocate(gfile%variname(nmetavari),gfile%varival(nmetavari))
           gfile%variname=variname
           gfile%varival=varival
      endif
      if(present(nmetavarr).and.nmetavarr.gt.izero.and.present(varrname) &
         .and.size(varrname).eq.nmetavarr .and. &
         present(varrval).and.size(varrval).eq.nmetavarr) then
           gfile%nmetavarr=nmetavarr
           if(allocated(gfile%varrname)) deallocate(gfile%varrname)
           if(allocated(gfile%varrval)) deallocate(gfile%varrval)
           allocate(gfile%varrname(nmetavarr),gfile%varrval(nmetavarr))
           gfile%varrname=varrname
           gfile%varrval=varrval
      endif
      if(present(nmetavarl).and.nmetavarl.gt.izero.and.present(varlname) &
         .and.size(varlname).eq.nmetavarl .and. &
         present(varlval).and.size(varlval).eq.nmetavarl) then
           gfile%nmetavarl=nmetavarl
           if(allocated(gfile%varlname)) deallocate(gfile%varlname)
           if(allocated(gfile%varlval)) deallocate(gfile%varlval)
           allocate(gfile%varlname(nmetavarl),gfile%varlval(nmetavarl))
           gfile%varlname=varlname
           gfile%varlval=varlval
      endif
      if(present(nmetavarc).and.nmetavarc.gt.izero.and.present(varcname) &
         .and.size(varcname).eq.nmetavarc .and. &
         present(varcval).and.size(varcval).eq.nmetavarc) then
           gfile%nmetavarc=nmetavarc
           if(allocated(gfile%varcname)) deallocate(gfile%varcname)
           if(allocated(gfile%varcval)) deallocate(gfile%varcval)
           allocate(gfile%varcname(nmetavarc),gfile%varcval(nmetavarc))
           gfile%varcname=varcname
           gfile%varcval=varcval
      endif
      if(present(nmetaaryi).and.nmetaaryi.gt.izero.and.present(aryiname) &
         .and.size(aryiname).eq.nmetaaryi .and. &
         present(aryilen).and.size(aryilen).eq.nmetaaryi) then
           gfile%nmetaaryi=nmetaaryi
           if(allocated(gfile%aryiname)) deallocate(gfile%aryiname)
           if(allocated(gfile%aryilen)) deallocate(gfile%aryilen)
           allocate(gfile%aryiname(nmetaaryi),gfile%aryilen(nmetaaryi))
           gfile%aryiname=aryiname
           gfile%aryilen=aryilen
           if(present(aryival).and.size(aryival).eq.nmetaaryi*maxval(gfile%aryilen) ) then
             if(allocated(gfile%aryival)) deallocate(gfile%aryival)
             allocate(gfile%aryival(maxval(gfile%aryilen),nmetaaryi))
             gfile%aryival=aryival
           endif
      endif
      if(present(nmetaaryr).and.nmetaaryr.gt.izero.and.present(aryrname) &
         .and.size(aryrname).eq.nmetaaryr .and. &
         present(aryrlen).and.size(aryrlen).eq.nmetaaryr) then
           gfile%nmetaaryr=nmetaaryr
           if(allocated(gfile%aryrname)) deallocate(gfile%aryrname)
           if(allocated(gfile%aryrlen)) deallocate(gfile%aryrlen)
           allocate(gfile%aryrname(nmetaaryr),gfile%aryrlen(nmetaaryr))
           gfile%aryrname=aryrname
           gfile%aryrlen=aryrlen
!           print *,'in wcreate,gfile%aryrname=',gfile%aryrname
!           print *,'in wcreate,gfile%aryrlen=',gfile%aryrlen
           if(present(aryrval).and.size(aryrval).eq.nmetaaryr*maxval(gfile%aryrlen)) then
             if(allocated(gfile%aryrval)) deallocate(gfile%aryrval)
             allocate(gfile%aryrval(maxval(gfile%aryrlen),nmetaaryr))
             gfile%aryrval=aryrval
           endif
      endif
      if(present(nmetaaryl).and.nmetaaryl.gt.izero.and.present(arylname) &
          .and.size(arylname).eq.nmetaaryl .and. &
           present(aryllen).and.size(aryllen).eq.nmetaaryl) then
           gfile%nmetaaryl=nmetaaryl
           if(allocated(gfile%arylname)) deallocate(gfile%arylname)
           if(allocated(gfile%aryllen)) deallocate(gfile%aryllen)
           allocate(gfile%arylname(nmetaaryl),gfile%aryllen(nmetaaryl))
           gfile%arylname=arylname
           gfile%aryllen=aryllen
           if(present(arylval).and.size(arylval).eq.nmetaaryl*maxval(gfile%aryllen)) then
             if(allocated(gfile%arylval)) deallocate(gfile%arylval)
             allocate(gfile%arylval(maxval(gfile%aryllen),nmetaaryl))
             gfile%arylval=arylval
           endif
      endif
      if(present(nmetaaryc).and.nmetaaryc.gt.izero.and.present(arycname) &
          .and.size(arycname).eq.nmetaaryc .and. &
          present(aryclen).and.size(aryclen).eq.nmetaaryc) then
           gfile%nmetaaryc=nmetaaryc
           if(allocated(gfile%arycname)) deallocate(gfile%arycname)
           if(allocated(gfile%aryclen)) deallocate(gfile%aryclen)
           allocate(gfile%arycname(nmetaaryc),gfile%aryclen(nmetaaryc))
           gfile%arycname=arycname
           gfile%aryclen=aryclen
           if(present(arycval).and.size(arycval).eq.nmetaaryc*maxval(gfile%aryclen)) then
             if(allocated(gfile%arycval)) deallocate(gfile%arycval)
             allocate(gfile%arycval(maxval(gfile%aryclen),nmetaaryc))
             gfile%arycval=arycval
           endif
      endif
      if (gfile%nmetavari+gfile%nmetavarr+gfile%nmetavarl+gfile%nmetavarc+ &
           gfile%nmetaaryi+gfile%nmetaaryr+gfile%nmetaaryl+gfile%nmetaaryc &
           .lt.8*nemsio_intfill )then
           print *,'WRONG: gfile%extrameta is not compatiable with input extra meta!'
           return
      endif
    endif 
!------------------------------------------------------------
! check gfile meta data array size
!------------------------------------------------------------
    call nemsio_chkgfary(gfile,ios)
    if (ios.ne. izero) then
      iret=ios
      return
    endif
!------------------------------------------------------------
! continue to set gfile meta data variables tnd arrays
!------------------------------------------------------------
!set gfile data type to bin/grb, default set to grb
!recname
    if(present(recname) ) then
!       print*,'gfile%nrec=',gfile%nrec,'size(recname)=',size(recname)
       if (gfile%nrec.eq.size(recname)) then
         gfile%recname=recname
       else
         print *,'WRONG: the size of recname is not equal to the total number of the fields in the file!'
         return
       endif
    endif
!reclevtyp
    if(present(reclevtyp)) then
       if (gfile%nrec.eq.size(reclevtyp)) then
         gfile%reclevtyp=reclevtyp
       else
         print *,'WRONG: the size of reclevtyp is not equal to the total number of the fields in the file!'
         return
       endif
    endif
!reclev
    if(present(reclev) ) then
       if (gfile%nrec.eq.size(reclev)) then
         gfile%reclev=reclev
       else
         print *,'WRONG: the size of reclev is not equal to the total number of the fields in the file!'
         return
       endif
    endif
!
!vcoord vcoord(levs+1
    if(present(vcoord) ) then
       if ((gfile%dimz+1)*3*2.eq.size(vcoord)) then
         gfile%vcoord=vcoord
       else
         print *,'WRONG: the size of vcoord is not (lm+1,3,2) !'
         return
       endif
    endif
!lat
    if(present(lat) ) then
!       write(0,*)'gfile%fieldsize=',gfile%fieldsize,'size(lat)=',size(lat)
       if (gfile%fieldsize.eq.size(lat)) then
         if(.not.(all(lat==zero))) gfile%lat=lat
       else
         print *,'WRONG: the input size(lat) ',size(lat),' is not equal to: ',gfile%fieldsize
         return
       endif
    endif
    gfile%rlat_max=maxval(gfile%lat)
    gfile%rlat_min=minval(gfile%lat)
!lon
    if(present(lon) ) then
       if (gfile%fieldsize.eq.size(lon)) then
         if(.not.(all(lon==zero)) ) gfile%lon=lon
       else
         print *,'WRONG: the input size(lon) ',size(lon),' is not equal to: ',gfile%fieldsize
         return
       endif
    endif
    gfile%rlon_max=maxval(gfile%lon)
    gfile%rlon_min=minval(gfile%lon)
!dx
    if(present(dx) ) then
!       write(0,*)'gfile%fieldsize=',gfile%fieldsize,'size(dx)=',size(dx)
       if (gfile%fieldsize.eq.size(dx)) then
         if(.not.(all(dx==zero)) ) gfile%dx=dx
       else
         print *,'WRONG: the input size(dx) ',size(dx),' is not equal to: ',gfile%fieldsize
         return
       endif
    endif
!dy
    if(present(dy) ) then
       if (gfile%fieldsize.eq.size(dy)) then
         if(.not.(all(dy==zero)) ) gfile%dy=dy
       else
         print *,'WRONG: the input size(dy) ',size(dy),' is not equal to: ',gfile%fieldsize
         return
       endif
    endif
!Cpi
    if( present(Cpi) ) then
       if (gfile%ntrac+1.eq.size(gfile%Cpi)) then
         if(.not.(all(cpi==zero))) gfile%Cpi = Cpi
       else
         print *,'WRONG: the input size(cpi) ',size(cpi),' is not equal to: ',gfile%ntrac+1
         return
       endif
!       print *,'in wcreate,cpi=',maxval(cpi),minval(cpi),maxval(gfile%Cpi),minval(cpi),&
!        'size(cpi)=',size(cpi),size(gfile%Cpi),'ntrac=',gfile%ntrac

    endif
!Ri
    if( present(Ri) ) then
       if (gfile%ntrac+1.eq.size(gfile%Ri)) then
         if(.not.(all(ri==zero))) gfile%Ri = Ri
       else
         print *,'WRONG: the input size(ri) ',size(ri),' is not equal to: ',gfile%ntrac+1
         return
       endif
    endif
!       print *,'in wcreate,ri=',maxval(ri),minval(ri),maxval(gfile%ri),minval(ri),&
!        'size(ri)=',size(ri),size(gfile%ri),'ntrac=',gfile%ntrac
!
!------------------------------------------------------------
! write out first meta data record
!------------------------------------------------------------
    meta1%gtype=gfile%gtype
    meta1%gdatatype=gfile%gdatatype
    meta1%modelname=gfile%modelname
    meta1%version=gfile%version
    meta1%nmeta=gfile%nmeta
    meta1%lmeta=gfile%lmeta
    meta1%reserve=izero
    iskip=izero
    iwrite=nemsio_lmeta1
    call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,meta1)
    if(nwrite.lt.iwrite) return
    gfile%tlmeta=nwrite
!    print *,'tlmet1 =',gfile%tlmeta,'nwrite=',nwrite,meta1%gdatatype
!------------------------------------------------------------
! write out second meta data record
!------------------------------------------------------------
    meta2%nrec=gfile%nrec
    meta2%idate(1:7)=gfile%idate(1:7)
    meta2%nfday=gfile%nfday
    meta2%nfhour=gfile%nfhour
    meta2%nfminute=gfile%nfminute
    meta2%nfsecondn=gfile%nfsecondn
    meta2%nfsecondd=gfile%nfsecondd
    meta2%dimx=gfile%dimx
    meta2%dimy=gfile%dimy
    meta2%dimz=gfile%dimz
    meta2%nframe=gfile%nframe
    meta2%nsoil=gfile%nsoil
    meta2%ntrac=gfile%ntrac
    meta2%jcap=gfile%jcap
    meta2%ncldt=gfile%ncldt
    meta2%idvc=gfile%idvc
    meta2%idsl=gfile%idsl
    meta2%idvm=gfile%idvm
    meta2%idrt=gfile%idrt
    meta2%rlon_min=gfile%rlon_min
    meta2%rlon_max=gfile%rlon_max
    meta2%rlat_min=gfile%rlat_min
    meta2%rlat_max=gfile%rlat_max
    meta2%extrameta=gfile%extrameta
    iskip=iskip+nwrite
    iwrite=gfile%lmeta
    call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,meta2)
    if(nwrite.lt.iwrite) return
    gfile%tlmeta=gfile%tlmeta+nwrite
!------------------------------------------------------------
! write out 3rd-13th meta data record (arrays)
!------------------------------------------------------------
!recname
    if ( gfile%nmeta-2>izero ) then
      iskip=iskip+nwrite
      iwrite=len(gfile%recname)*size(gfile%recname)
      call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,gfile%recname)
      if(nwrite.lt.iwrite) return
      gfile%tlmeta=gfile%tlmeta+nwrite
!      print *,'tlmetrecname =',gfile%tlmeta,'nwrite=',nwrite
    endif

!reclevtyp
    if ( gfile%nmeta-3>izero ) then
      iskip=iskip+nwrite
      iwrite=len(gfile%reclevtyp)*size(gfile%reclevtyp)
      call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,gfile%reclevtyp)
      if(nwrite.lt.iwrite) return
      gfile%tlmeta=gfile%tlmeta+nwrite
!      print *,'tlmetreclevty=',gfile%tlmeta,'nwrite=',nwrite
    endif

!reclev
    if ( gfile%nmeta-4>izero ) then
      iskip=iskip+nwrite
      iwrite=kind(gfile%reclev)*size(gfile%reclev)
      call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,gfile%reclev)
      if(nwrite.lt.iwrite) return
      gfile%tlmeta=gfile%tlmeta+nwrite
!      print *,'tlmetreclev=',gfile%tlmeta,'nwrite=',nwrite
    endif
!vcoord
    nummeta=gfile%nmeta-5
    if ( nummeta.gt.izero ) then
      iskip=iskip+nwrite
      iwrite=kind(gfile%vcoord)*size(gfile%vcoord)
      call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,gfile%vcoord)
      if(nwrite.lt.iwrite) return
      gfile%tlmeta=gfile%tlmeta+nwrite
!      print *,'tlmetavcoord=',gfile%tlmeta,'nwrite=',nwrite,'nummeta=', &
!        nummeta,'gfile%nmeta=',gfile%nmeta
      nummeta=nummeta-ione
    endif
!lat
    if ( nummeta.gt.izero ) then
      iskip=iskip+nwrite
      iwrite=kind(gfile%lat)*size(gfile%lat)
      call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,gfile%lat)
      if(nwrite.lt.iwrite) return
      gfile%tlmetalat=gfile%tlmeta
      gfile%tlmeta=gfile%tlmeta+nwrite
!      print *,'tlmetreclat=',gfile%tlmeta,'nwrite=',nwrite,  &
!       maxval(gfile%lat),minval(gfile%lat)
      nummeta=nummeta-ione
    endif
!lon
    if ( nummeta.gt.izero ) then
      iskip=iskip+nwrite
      iwrite=kind(gfile%lon)*size(gfile%lon)
      call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,gfile%lon)
      if(nwrite.lt.iwrite) return
      gfile%tlmetalon=gfile%tlmeta
      gfile%tlmeta=gfile%tlmeta+nwrite
!      print *,'tlmetreclon=',gfile%tlmeta,'nwrite=',nwrite,'nummeta=',nummeta, &
!         maxval(gfile%lon),minval(gfile%lon)
      nummeta=nummeta-ione
    endif
!dx
    if ( nummeta.gt.izero ) then
      if(all(gfile%dx==zero)) gfile%dx=nemsio_realfill
      iskip=iskip+nwrite
      iwrite=kind(gfile%dx)*size(gfile%dx)
      call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,gfile%dx)
!      print *,'tlmetrecdx=',gfile%tlmeta,'iwrite=',iwrite,'nwrite=',nwrite, &
!         maxval(gfile%dx),minval(gfile%dx)
      if(nwrite.lt.iwrite) return
      gfile%tlmetadx=gfile%tlmeta
      gfile%tlmeta=gfile%tlmeta+nwrite
!      print *,'tlmetrecdx=',gfile%tlmeta,'nwrite=',nwrite,  &
!        maxval(gfile%dx),minval(gfile%dx),maxval(gfile%dy),maxval(gfile%dy)
      nummeta=nummeta-ione
    endif
!dy
    if ( nummeta.gt.izero ) then
      if(all(gfile%dy==zero)) gfile%dy=nemsio_realfill
      iskip=iskip+nwrite
      iwrite=kind(gfile%dy)*size(gfile%dy)
      call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,gfile%dy)
      if(nwrite.lt.iwrite) return
      gfile%tlmetady=gfile%tlmeta
      gfile%tlmeta=gfile%tlmeta+nwrite
!      print *,'tlmetrecdy=',gfile%tlmeta,'nwrite=',nwrite
      nummeta=nummeta-ione
    endif
!Cpi
    if ( nummeta.gt.izero ) then
      if(all(gfile%cpi==zero)) gfile%cpi=nemsio_realfill
      iskip=iskip+nwrite
      iwrite=kind(gfile%Cpi)*size(gfile%Cpi)
      call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,gfile%Cpi)
      if(nwrite.lt.iwrite) return
      gfile%tlmeta=gfile%tlmeta+nwrite
!      print *,'tlmetreccpi=',gfile%tlmeta,'nwrite=',nwrite,  &
!        'cpi=',maxval(gfile%cpi),minval(gfile%cpi)
      nummeta=nummeta-ione
    endif
!Ri
    if ( nummeta.gt.izero ) then
      if(all(gfile%ri==zero)) gfile%ri=nemsio_realfill
      iskip=iskip+nwrite
      iwrite=kind(gfile%Ri)*size(gfile%Ri)
      call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,gfile%Ri)
      if(nwrite.lt.iwrite) return
      gfile%tlmeta=gfile%tlmeta+nwrite
!      print *,'tlmetrecri=',gfile%tlmeta,'nwrite=',nwrite,   &
!        'ri=',maxval(gfile%ri),minval(gfile%ri)
      nummeta=nummeta-ione
    endif
!------------------------------------------------------------
! write out extra meta data record 
!------------------------------------------------------------
    if(gfile%extrameta) then
      meta3%nmetavari=gfile%nmetavari
      meta3%nmetavarr=gfile%nmetavarr
      meta3%nmetavarl=gfile%nmetavarl
      meta3%nmetavarc=gfile%nmetavarc
      meta3%nmetaaryi=gfile%nmetaaryi
      meta3%nmetaaryr=gfile%nmetaaryr
      meta3%nmetaaryl=gfile%nmetaaryl
      meta3%nmetaaryc=gfile%nmetaaryc
      iskip=iskip+nwrite
      iwrite=nemsio_lmeta3
      call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,meta3)
      if(nwrite.lt.iwrite) return
      gfile%tlmeta=gfile%tlmeta+nwrite
!      print *,'tlmetameta3=',gfile%tlmeta
!
!-- write meta var integer
      if (gfile%nmetavari.gt.izero) then
        iskip=iskip+nwrite
        iwrite=len(gfile%variname)*gfile%nmetavari
        call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,gfile%variname)
        if(nwrite.lt.iwrite) return
        gfile%tlmeta=gfile%tlmeta+nwrite
        iskip=iskip+nwrite
        iwrite=kind(gfile%varival)*gfile%nmetavari
        call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,gfile%varival)
        if(nwrite.lt.iwrite) return
        gfile%tlmetavarival=gfile%tlmeta
        gfile%tlmeta=gfile%tlmeta+nwrite
!      print *,'rlmetavari=',gfile%tlmeta
      endif
      if (gfile%nmetavarr.gt.izero) then
        iskip=iskip+nwrite
        iwrite=len(gfile%varrname)*gfile%nmetavarr
        call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,gfile%varrname)
        if(nwrite.lt.iwrite) return
        gfile%tlmeta=gfile%tlmeta+nwrite
        iskip=iskip+nwrite
        iwrite=kind(gfile%varrval)*gfile%nmetavarr
        call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,gfile%varrval)
        if(nwrite.lt.iwrite) return
        gfile%tlmeta=gfile%tlmeta+nwrite
      endif
      if (gfile%nmetavarl.gt.izero) then
        iskip=iskip+nwrite
        iwrite=len(gfile%varlname)*gfile%nmetavarl
        call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,gfile%varlname)
        if(nwrite.lt.iwrite) return
        gfile%tlmeta=gfile%tlmeta+nwrite
        iskip=iskip+nwrite
        iwrite=kind(gfile%varlval)*gfile%nmetavarl
        call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,gfile%varlval)
        if(nwrite.lt.iwrite) return
        gfile%tlmeta=gfile%tlmeta+nwrite
      endif
      if (gfile%nmetavarc.gt.izero) then
        iskip=iskip+nwrite
        iwrite=len(gfile%varcname)*gfile%nmetavarc
        call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,gfile%varcname)
        if(nwrite.lt.iwrite) return
        gfile%tlmeta=gfile%tlmeta+nwrite
        iskip=iskip+nwrite
        iwrite=len(gfile%varcval)*gfile%nmetavarc
        call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,gfile%varcval)
        if(nwrite.lt.iwrite) return
        gfile%tlmeta=gfile%tlmeta+nwrite
      endif
!meta arr integer
      if (gfile%nmetaaryi.gt.izero) then
        iskip=iskip+nwrite
        iwrite=len(gfile%aryiname)*gfile%nmetaaryi
        call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,gfile%aryiname)
        if(nwrite.lt.iwrite) return
        gfile%tlmeta=gfile%tlmeta+nwrite
        iskip=iskip+nwrite
        iwrite=kind(gfile%aryilen)*gfile%nmetaaryi
        call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,gfile%aryilen)
        if(nwrite.lt.iwrite) return
        gfile%tlmeta=gfile%tlmeta+nwrite
        gfile%tlmetaaryival=gfile%tlmeta
        do i=1,gfile%nmetaaryi
          iskip=iskip+nwrite
          iwrite=kind(gfile%aryival)*gfile%aryilen(i)
          call bafrwritel(gfile%flunit,iskip,iwrite,nwrite, &
                         gfile%aryival(1:gfile%aryilen(i),i))
          if(nwrite.lt.iwrite) return
          gfile%tlmeta=gfile%tlmeta+nwrite
!          print *,'tlmetaryint=',i,gfile%tlmeta,'nwrite=',nwrite
        enddo
!          print *,'after tlmetaryi ',gfile%nmetaaryr,gfile%nmetaaryl,gfile%nmetaaryc
      endif
!meta arr real
      if (gfile%nmetaaryr.gt.izero) then
!          print *,'before tlmetaryr'
        iskip=iskip+nwrite
        iwrite=len(gfile%aryrname)*gfile%nmetaaryr
        call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,gfile%aryrname)
        if(nwrite.lt.iwrite) return
        gfile%tlmeta=gfile%tlmeta+nwrite
!          print *,'before tlmetaryr 1'
        iskip=iskip+nwrite
        iwrite=kind(gfile%aryrlen)*gfile%nmetaaryr
        call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,gfile%aryrlen)
        if(nwrite.lt.iwrite) return
        gfile%tlmeta=gfile%tlmeta+nwrite
!          print *,'before tlmetaryr 2'
        do i=1,gfile%nmetaaryr
          iskip=iskip+nwrite
          iwrite=kind(gfile%aryrval)*gfile%aryrlen(i)
          call bafrwritel(gfile%flunit,iskip,iwrite,nwrite, &
                         gfile%aryrval(1:gfile%aryrlen(i),i))
          if(nwrite.lt.iwrite) return
          gfile%tlmeta=gfile%tlmeta+nwrite
!          print *,'tlmetaryreal=',i,gfile%tlmeta,'nwrite=',nwrite
        enddo
      endif
!meta arr logical
      if (gfile%nmetaaryl.gt.izero) then
        iskip=iskip+nwrite
        iwrite=len(gfile%arylname)*gfile%nmetaaryl
        call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,gfile%arylname)
        if(nwrite.lt.iwrite) return
        gfile%tlmeta=gfile%tlmeta+nwrite
        iskip=iskip+nwrite
        iwrite=kind(gfile%aryllen)*gfile%nmetaaryl
        call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,gfile%aryllen)
        if(nwrite.lt.iwrite) return
        gfile%tlmeta=gfile%tlmeta+nwrite
        do i=1,gfile%nmetaaryl
          iskip=iskip+nwrite
          iwrite=kind(gfile%arylval)*gfile%aryllen(i)
          call bafrwritel(gfile%flunit,iskip,iwrite,nwrite, &
                         gfile%arylval(1:gfile%aryllen(i),i))
          if(nwrite.lt.iwrite) return
          gfile%tlmeta=gfile%tlmeta+nwrite
!          print *,'tlmetarylogic=',i,gfile%tlmeta,'nwrite=',nwrite
        enddo
      endif
!meta arr logical
      if (gfile%nmetaaryc.gt.0) then
        iskip=iskip+nwrite
        iwrite=len(gfile%arycname)*gfile%nmetaaryc
        call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,gfile%arycname)
        if(nwrite.lt.iwrite) return
        gfile%tlmeta=gfile%tlmeta+nwrite
        iskip=iskip+nwrite
        iwrite=kind(gfile%aryclen)*gfile%nmetaaryc
        call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,gfile%aryclen)
        if(nwrite.lt.iwrite) return
        gfile%tlmeta=gfile%tlmeta+nwrite
        do i=1,gfile%nmetaaryc
          iskip=iskip+nwrite
          iwrite=len(gfile%arycval)*gfile%aryclen(i)
          call bafrwritel(gfile%flunit,iskip,iwrite,nwrite, &
                         gfile%arycval(1:gfile%aryclen(i),i))
          if(nwrite.lt.iwrite) return
          gfile%tlmeta=gfile%tlmeta+nwrite
!          print *,'tlmetarycogic=',i,gfile%tlmeta,'nwrite=',nwrite
        enddo
      endif
    endif
!    print *,'end of wcreate,nsoil=',gfile%nsoil,'nrec=',gfile%nrec,'tlmeta=',gfile%tlmeta

    iret=0
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine nemsio_wcreate
!------------------------------------------------------------------------------

  subroutine nemsio_setfheadvari(gfile,varname,varval,iret)
!$$$  subprogram documentation block
!                .      .    .                                        .
! subprogram:    nemsio_setfheadvari
!   prgmmr:
!
! abstract: reset meta data integer value from file header, ONLY for time
!
! program history log:
!   2009-08-31  lueken - added subprogram doc block
!
!   input argument list:
!    gfile
!    varname
!    varval
!
!   output argument list:
!    gfile
!    iret
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block


    implicit none
    type(nemsio_gfile),intent(inout)              :: gfile
    character(len=*),  intent(in)                 :: varname
    integer(i_kind),intent(in)            :: varval
    integer(i_kind),optional,intent(out)  :: iret
    integer(i_kind)                       :: i,lhead
    integer(i_llong) :: iskip,iwrite,nwrite
    type(nemsio_meta2)      :: meta2
!---
    if(present(iret) ) iret=-17
    lhead=izero
!--- only allow change time in second meta data
    if (equal_str_nocase(trim(varname),'nfday')) then
      gfile%nfday=varval
      gfile%headvarival(5)=varval
      lhead=ione
    else if (equal_str_nocase(trim(varname),'nfhour')) then
      gfile%nfhour=varval
      gfile%headvarival(6)=varval
      lhead=ione
    else if (equal_str_nocase(trim(varname),'nfminute')) then
      gfile%nfminute=varval
      gfile%headvarival(7)=varval
      lhead=ione
    else if (equal_str_nocase(trim(varname),'nfsecondd')) then
      gfile%nfsecondd=varval
      gfile%headvarival(8)=varval
      lhead=ione
    else if (equal_str_nocase(trim(varname),'nfsecondn')) then
      gfile%nfsecondn=varval
      gfile%headvarival(9)=varval
      lhead=ione
    endif
    if(lhead==ione) then
!      print *,'in setfhearvari,2,nfday=',gfile%nfday,'nfhour=',gfile%nfhour, &
!       'nfminute=',gfile%nfminute,'secd=',gfile%nfsecondd,'nsecn=',gfile%nfsecondn
!------------------------------------------------------------
! write out second meta data record
!------------------------------------------------------------
      meta2%nrec=gfile%nrec
      meta2%idate(1:7)=gfile%idate(1:7)
      meta2%nfday=gfile%nfday
      meta2%nfhour=gfile%nfhour
      meta2%nfminute=gfile%nfminute
      meta2%nfsecondn=gfile%nfsecondn
      meta2%nfsecondd=gfile%nfsecondd
      meta2%dimx=gfile%dimx
      meta2%dimy=gfile%dimy
      meta2%dimz=gfile%dimz
      meta2%nframe=gfile%nframe
      meta2%nsoil=gfile%nsoil
      meta2%ntrac=gfile%ntrac
      meta2%jcap=gfile%jcap
      meta2%ncldt=gfile%ncldt
      meta2%idvc=gfile%idvc
      meta2%idsl=gfile%idsl
      meta2%idvm=gfile%idvm
      meta2%idrt=gfile%idrt
      meta2%rlon_min=gfile%rlon_min
      meta2%rlon_max=gfile%rlon_max
      meta2%rlat_min=gfile%rlat_min
      meta2%rlat_max=gfile%rlat_max
      meta2%extrameta=gfile%extrameta
      iskip=nemsio_lmeta1+8
      iwrite=gfile%lmeta
      call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,meta2)
      if(nwrite.lt.iwrite) return
      if(present(iret)) iret=izero
      return
    endif     
!---
    if(gfile%nmetavari.gt.izero) then
      do i=1,gfile%nmetavari
        if(equal_str_nocase(trim(varname),trim(gfile%variname(i))) ) then
           gfile%varival(i)=varval
           iskip=gfile%tlmetavarival
           iwrite=kind(gfile%varival)*gfile%nmetavari
           call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,gfile%varival)
           if(nwrite.lt.iwrite) return 
           if(present(iret)) iret=izero
           return
        endif
      enddo
    endif
!---
    if(.not.present(iret)) call nemsio_stop
    return
  end subroutine nemsio_setfheadvari
!------------------------------------------------------------------------------

  subroutine nemsio_setfheadaryi(gfile,varname,varval,iret)
!$$$  subprogram documentation block
!                .      .    .                                      .
! subprogram:    nemsio_setfheadaryi
!   prgmmr:
!
! abstract: reset meta data integer value from file header, ONLY for time
!
! program history log:
!   2009-08-31  lueken - added subprogram doc block
!
!   input argument list:
!    gfile
!    varname
!    varval
!
!   output argument list:
!    gfile
!    iret
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    implicit none
    type(nemsio_gfile),intent(inout)              :: gfile
    character(len=*),  intent(in)                 :: varname
    integer(i_kind),intent(in)            :: varval(:)
    integer(i_kind),optional,intent(out)  :: iret
    integer(i_kind):: i,lhead
    integer(i_llong) :: iskip,iwrite,nwrite
    type(nemsio_meta2)      :: meta2
!---
    if(present(iret) ) iret=-17
!--- only allow to change time in second meta data
    if (equal_str_nocase(trim(varname),'idate')) then
      if(size(gfile%idate)==size(varval)) then
      gfile%idate(:)=varval(:)
      gfile%headaryival(:,1)=varval(:)
!------------------------------------------------------------
! write out second meta data record
!------------------------------------------------------------
      meta2%nrec=gfile%nrec
      meta2%idate(1:7)=gfile%idate(1:7)
      meta2%nfday=gfile%nfday
      meta2%nfhour=gfile%nfhour
      meta2%nfminute=gfile%nfminute
      meta2%nfsecondn=gfile%nfsecondn
      meta2%nfsecondd=gfile%nfsecondd
      meta2%dimx=gfile%dimx
      meta2%dimy=gfile%dimy
      meta2%dimz=gfile%dimz
      meta2%nframe=gfile%nframe
      meta2%nsoil=gfile%nsoil
      meta2%ntrac=gfile%ntrac
      meta2%jcap=gfile%jcap
      meta2%ncldt=gfile%ncldt
      meta2%idvc=gfile%idvc
      meta2%idsl=gfile%idsl
      meta2%idvm=gfile%idvm
      meta2%idrt=gfile%idrt
      meta2%rlon_min=gfile%rlon_min
      meta2%rlon_max=gfile%rlon_max
      meta2%rlat_min=gfile%rlat_min
      meta2%rlat_max=gfile%rlat_max
      meta2%extrameta=gfile%extrameta
      iskip=nemsio_lmeta1+8
      iwrite=gfile%lmeta
      call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,meta2)
      if(nwrite.lt.iwrite) return
      if(present(iret)) iret=izero
      return
     endif
    endif     
!---
    if(gfile%nmetaaryi.gt.izero) then
      do i=1,gfile%nmetaaryi
        if(equal_str_nocase(trim(varname),trim(gfile%aryiname(i))) ) then
           if(gfile%aryilen(i)==size(varval)) then
             gfile%aryival(1:gfile%aryilen(i),i)=varval(1:size(varval)) 
             lhead=ione
             exit
           endif
        endif
      enddo
      if(lhead==ione) then
        iskip=gfile%tlmetaaryival
        nwrite=izero
        do i=1,gfile%nmetaaryi
          iskip=iskip+nwrite
          iwrite=kind(gfile%aryival)*gfile%aryilen(i)
          call bafrwritel(gfile%flunit,iskip,iwrite,nwrite, &
                         gfile%aryival(1:gfile%aryilen(i),i))
          if(nwrite.lt.iwrite) return
        enddo
        if(present(iret)) iret=izero
        return
      endif
    endif
!---
    if(.not.present(iret)) call nemsio_stop
    return
  end subroutine nemsio_setfheadaryi
!------------------------------------------------------------------------------

  subroutine nemsio_setfilehead(gfile,iret,lat,lon,dx,dy)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    nemsio_setfilehead
!   prgmmr:
!
! abstract: reset some nemsio meta data information from outside
!
! program history log:
!   2009-08-31  lueken - added subprogram doc block
!
!   input argument list:
!    gfile
!    lat,lon
!    dx,dy
!
!   output argument list:
!    gfile
!    iret
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    implicit none
    type(nemsio_gfile),intent(inout)             :: gfile
    integer(i_kind),optional,intent(out) :: iret
    real(r_single),optional,intent(in)    :: lat(:),lon(:)
    real(r_single),optional,intent(in)    :: dx(:),dy(:)
!
!--- local vars
    integer(i_llong) :: iskip,iwrite,nwrite
!
!---
    if (present(iret)) iret=-3
!
!--- check the size first, then set the value
!--- lat
    if(present(lat) ) then
       if (size(lat).ne.gfile%fieldsize) then
         if ( present(iret))  return
         call nemsio_stop
       else
         gfile%lat=lat
         gfile%headaryrval(:,2)=gfile%lat
         if(equal_str_nocase(trim(gfile%gaction),'write') .and. &
             gfile%tlmetalat/=nemsio_intfill8) then
            iskip=gfile%tlmetalat
            iwrite=kind(gfile%lat)*size(gfile%lat)
            call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,gfile%lat)
            if(nwrite.lt.iwrite) return
         endif
       endif
    endif
!--- lon
    if(present(lon) ) then
       if (size(lon).ne.gfile%fieldsize) then
         if ( present(iret)) return
         call nemsio_stop
       else
         gfile%lon=lon
         gfile%headaryrval(:,3)=gfile%lon
         if(equal_str_nocase(trim(gfile%gaction),'write').and.   &
             gfile%tlmetalon/=nemsio_intfill8) then
            iskip=gfile%tlmetalon
            iwrite=kind(gfile%lon)*size(gfile%lon)
            call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,gfile%lon)
            if(nwrite.lt.iwrite) return
         endif
       endif
    endif
!--- dx
    if(present(dx) ) then
       print *,'getfilehead, size(dx)=',size(dx),gfile%fieldsize,  &
          maxval(gfile%dx),minval(gfile%dx)
       if (size(dx).ne.gfile%fieldsize) then
         if ( present(iret))  return
         call nemsio_stop
       else
         gfile%dx=dx
         gfile%headaryrval(:,4)=gfile%dx
         if(equal_str_nocase(trim(gfile%gaction),'write').and.   &
             gfile%tlmetadx/=nemsio_intfill8) then
            iskip=gfile%tlmetadx
            iwrite=kind(gfile%dx)*size(gfile%dx)
            call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,gfile%dx)
            if(nwrite.lt.iwrite) return
         endif
       endif
    endif
!--- dy
    if(present(dy) ) then
!       print *,'getfilehead, size(dy)=',size(dy),gfile%fieldsize,  &
!          maxval(gfile%dy),minval(gfile%dy)
       if (size(dy).ne.gfile%fieldsize) then
         if ( present(iret)) return
         call nemsio_stop
       else
         gfile%dy=dy
         gfile%headaryrval(:,5)=gfile%dy
         if(equal_str_nocase(trim(gfile%gaction),'write').and.   &
             gfile%tlmetady/=nemsio_intfill8) then
            iskip=gfile%tlmetady
            iwrite=kind(gfile%dy)*size(gfile%dy)
            call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,gfile%dy)
            if(nwrite.lt.iwrite) return
         endif
       endif
    endif
!
    iret=izero
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine nemsio_setfilehead
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
  subroutine nemsio_getfilehead(gfile,iret,gtype,gdatatype,gfname,gaction, &
      modelname,version,nmeta,lmeta,nrec,idate,nfday,nfhour,nfminute, &
      nfsecondn,nfsecondd,dimx,dimy,dimz,nframe,nsoil,ntrac,ncldt,jcap,&
      idvc,idsl,idvm,idrt, rlon_min,rlon_max,rlat_min,rlat_max,tlmeta, &
      extrameta,nmetavari,nmetavarr,nmetavarl,nmetavarc,nmetaaryi,nmetaaryr, &
      nmetaaryl,nmetaaryc,    &
      recname,reclevtyp,reclev,vcoord,lon,lat,dx,dy,cpi,ri, &
      variname,varival,varrname,varrval,varlname,varlval,varcname,varcval, &
      aryiname,aryilen,aryival,aryrname,aryrlen,aryrval,    &
      arylname,aryllen,arylval,arycname,aryclen,arycval    )
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    nemsio_getfilehead
!   prgmmr:
!
! abstract: get nemsio meta data information from outside
!
! program history log:
!   2009-08-31  lueken - added subprogram doc block
!
!   input argument list:
!    gfile
!
!   output argument list:
!    iret
!    gtype,gdatatype,gfname
!    gaction,modelname
!    version,nmeta,lmeta
!    nrec,idate(7),nfday,nfhour
!    nfminute,nfsecondn,nfsecondd
!    dimx,dimy,dimz,nframe
!    nsoil,ntrac
!    ncldt,jcap,idvc,idsl,idvm,idrt
!    rlon_min,rlon_max
!    rlat_min,rlat_max
!    tlmeta
!    extrameta
!    nmetavari,nmetavarr
!    nmetavarl,nmetavarc,nmetaaryi
!    nmetaaryr,nmetaaryl,nmetaaryc
!    recname
!    reclevtyp
!    reclev
!    vcoord
!    lat,lon
!    dx,dy
!    Cpi,Ri
!    variname,varrname
!    varlname,varcname
!    aryiname,aryrname
!    arylname,arycname
!    aryilen,aryrlen
!    aryllen,aryclen
!    varival,aryival
!    varrval,aryrval
!    varlval,arylval
!    varcval,arycval
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block



    implicit none
    type(nemsio_gfile),intent(in)                :: gfile
    integer(i_kind),optional,intent(out) :: iret
    character*(*),optional,intent(out)           :: gtype,gdatatype,gfname, &
                                                    gaction,modelname
    integer(i_kind),optional,intent(out) :: version,nmeta,lmeta
    integer(i_kind),optional,intent(out) :: nrec,idate(7),nfday,nfhour, &
                                                    nfminute,nfsecondn,nfsecondd
    integer(i_kind),optional,intent(out) :: dimx,dimy,dimz,nframe, &
                                                    nsoil,ntrac
    integer(i_kind),optional,intent(out) :: ncldt,jcap,idvc,idsl,idvm,idrt
    real(r_single),optional,intent(out)   :: rlon_min,rlon_max,rlat_min, &
                                                    rlat_max
    integer(i_kind),optional,intent(out)  :: tlmeta
    logical(nemsio_logickind),optional,intent(out):: extrameta
    integer(i_kind),optional,intent(out)  :: nmetavari,nmetavarr, &
                                                    nmetavarl,nmetavarc,nmetaaryi, &
                                                    nmetaaryr,nmetaaryl,nmetaaryc
    character(*),optional,intent(out)            :: recname(:)
    character(*),optional,intent(out)            :: reclevtyp(:)
    integer(i_kind),optional,intent(out) :: reclev(:)
    real(r_single),optional,intent(out)   :: vcoord(:,:,:)
    real(r_single),optional,intent(out)   :: lat(:),lon(:)
    real(r_single),optional,intent(out)   :: dx(:),dy(:)
    real(r_single),optional,intent(out)   :: Cpi(:),Ri(:)
    character(*),optional,intent(out)            :: variname(:),varrname(:)
    character(*),optional,intent(out)            :: varlname(:),varcname(:)
    character(*),optional,intent(out)            :: aryiname(:),aryrname(:)
    character(*),optional,intent(out)            :: arylname(:),arycname(:)
    integer(i_kind),optional,intent(out) :: aryilen(:),aryrlen(:)
    integer(i_kind),optional,intent(out) :: aryllen(:),aryclen(:)
    integer(i_kind),optional,intent(out) :: varival(:),aryival(:,:)
    real(r_single),optional,intent(out)   :: varrval(:),aryrval(:,:)
    logical(nemsio_logickind),optional,intent(out):: varlval(:),arylval(:,:)
    character(*),optional,intent(out)             :: varcval(:),arycval(:,:)
!
!------------------------------------------------------------
    if (present(iret)) iret=-3
    if(present(gtype)) gtype=gfile%gtype
    if(present(gdatatype)) gdatatype=gfile%gdatatype
    if(present(gfname)) gfname=trim(gfile%gfname)
    if(present(gaction)) gaction=gfile%gaction
    if(present(modelname)) modelname=gfile%modelname
    if(present(version)) version=gfile%version
    if(present(nmeta)) nmeta=gfile%nmeta
    if(present(lmeta)) lmeta=gfile%lmeta
    if(present(nrec)) nrec=gfile%nrec
    if(present(nfday)) nfday=gfile%nfday
    if(present(nfhour)) nfhour=gfile%nfhour
    if(present(nfminute)) nfminute=gfile%nfminute
    if(present(nfsecondn)) nfsecondn=gfile%nfsecondn
    if(present(nfsecondd)) nfsecondd=gfile%nfsecondd
    if(present(idate)) idate=gfile%idate
    if(present(dimx)) dimx=gfile%dimx
    if(present(dimy)) dimy=gfile%dimy
    if(present(dimz)) dimz=gfile%dimz
    if(present(nframe)) nframe=gfile%nframe
    if(present(nsoil)) nsoil=gfile%nsoil
    if(present(ntrac)) ntrac=gfile%ntrac
    if(present(jcap)) jcap=gfile%jcap
    if(present(ncldt)) ncldt=gfile%ncldt
    if(present(idvc)) idvc=gfile%idvc
    if(present(idsl)) idsl=gfile%idsl
    if(present(idvm)) idvm=gfile%idvm
    if(present(idrt)) idrt=gfile%idrt
    if(present(rlon_min)) rlon_min=gfile%rlon_min
    if(present(rlon_max)) rlon_max=gfile%rlon_max
    if(present(rlat_min)) rlat_min=gfile%rlat_min
    if(present(rlat_max)) rlat_max=gfile%rlat_max
    if(present(tlmeta)) tlmeta=gfile%tlmeta
    if(present(extrameta)) extrameta=gfile%extrameta
!
!    print *,'in getfilehead, 1extrameta=',gfile%extrameta,        &
!     'nrec=',gfile%nrec,'size(recname)=',size(recname),           &
!     size(reclevtyp),size(reclev)
!--- rec
    if(present(recname) ) then
       if (gfile%nrec.ne.size(recname)) then
         if ( present(iret)) return
         call nemsio_stop
       else
         recname=gfile%recname
       endif
    endif
    if(present(reclevtyp)) then
       if (gfile%nrec.ne.size(reclevtyp)) then
         if ( present(iret)) return
         call nemsio_stop
       else
         reclevtyp=gfile%reclevtyp
       endif
    endif
    if(present(reclev) ) then
       if (gfile%nrec.ne.size(reclev)) then
         if ( present(iret)) return
         call nemsio_stop
       else
         reclev=gfile%reclev
       endif
    endif
!--- vcoord
    if(present(vcoord)) then
       if (size(vcoord) .ne. (gfile%dimz+ione)*2*3 ) then
         if ( present(iret))  return
         call nemsio_stop
       else
         vcoord=gfile%vcoord
       endif
    endif
!--- lat
    if(present(lat) ) then
       if (size(lat).ne.gfile%fieldsize) then
         print *,'WRONG: size(lat)=',size(lat),' is not equal to ',gfile%fieldsize
         if ( present(iret))  return
         call nemsio_stop
       else
         lat=gfile%lat
       endif
    endif
!--- lon
    if(present(lon) ) then
       if (size(lon).ne.gfile%fieldsize) then
         print *,'WRONG: size(lon)=',size(lon),' is not equal to ',gfile%fieldsize
         if ( present(iret)) return
         call nemsio_stop
       else
         lon=gfile%lon
       endif
    endif
!--- dx
    if(present(dx) ) then
       print *,'getfilehead, size(dx)=',size(dx),gfile%fieldsize,  &
          maxval(gfile%dx),minval(gfile%dx)
       if (size(dx).ne.gfile%fieldsize) then
         print *,'WRONG: size(dX)=',size(dx),' is not equal to ',gfile%fieldsize
         if ( present(iret))  return
         call nemsio_stop
       else
         dx=gfile%dx
       endif
    endif
!--- dy
    if(present(dy) ) then
!       print *,'getfilehead, size(dy)=',size(dy),gfile%fieldsize,  &
!          maxval(gfile%dy),minval(gfile%dy)
       if (size(dy).ne.gfile%fieldsize) then
         print *,'WRONG: size(dy)=',size(dy),' is not equal to ',gfile%fieldsize
         if ( present(iret)) return
         call nemsio_stop
       else
         dy=gfile%dy
       endif
    endif
!--- Cpi
    if(present(Cpi) ) then
       if (gfile%ntrac+ione.ne.size(Cpi)) then
         if ( present(iret)) return
         call nemsio_stop
       else
         Cpi=gfile%Cpi
       endif
    endif
!--- Ri
    if(present(Ri) ) then 
       if (gfile%ntrac+ione.ne.size(Ri)) then
         if ( present(iret)) return
         call nemsio_stop
       else
         Ri=gfile%Ri
       endif
    endif
!------------------------------------------------------------------------------
!*** for extra meta field
!------------------------------------------------------------------------------
!extrameta
    if(present(extrameta) ) extrameta=gfile%extrameta
    if(gfile%extrameta) then
      if (present(nmetavari) ) nmetavari=gfile%nmetavari
      if (present(nmetavarr) ) nmetavarr=gfile%nmetavarr
      if (present(nmetavarl) ) nmetavarl=gfile%nmetavarl
      if (present(nmetavarc) ) nmetavarc=gfile%nmetavarc
      if (present(nmetaaryi) ) nmetaaryi=gfile%nmetaaryi
      if (present(nmetaaryr) ) nmetaaryr=gfile%nmetaaryr
      if (present(nmetaaryl) ) nmetaaryl=gfile%nmetaaryl
      if (present(nmetaaryc) ) nmetaaryc=gfile%nmetaaryc
      if ( gfile%nmetavari.gt.izero ) then
         if (present(variname).and.size(variname).eq.gfile%nmetavari) &
             variname=gfile%variname
         if (present(varival).and.size(varival).eq.gfile%nmetavari) &
             varival=gfile%varival
      endif
      if ( gfile%nmetavarr.gt.izero ) then
         if (present(varrname).and.size(varrname).eq.gfile%nmetavarr) &
             varrname=gfile%varrname
         if (present(varrval).and.size(varrval).eq.gfile%nmetavarr) &
             varrval=gfile%varrval
      endif
      if ( gfile%nmetavarl.gt.izero ) then
         if (present(varlname).and.size(varlname).eq.gfile%nmetavarl) &
             varlname=gfile%varlname
         if (present(varlval).and.size(varlval).eq.gfile%nmetavarl) &
             varlval=gfile%varlval
      endif
      if ( gfile%nmetavarc.gt.izero ) then
         if (present(varcname).and.size(varcname).eq.gfile%nmetavarc) &
             varcname=gfile%varcname
         if (present(varcval).and.size(varcval).eq.gfile%nmetavarc) &
             varcval=gfile%varcval
      endif
      if ( gfile%nmetaaryi.gt.izero ) then
         if (present(aryiname).and.size(aryiname).eq.gfile%nmetaaryi) &
             aryiname=gfile%aryiname
         if (present(aryilen).and.size(aryilen).eq.gfile%nmetaaryi) &
             aryilen=gfile%aryilen
         if (present(aryival).and.size(aryival).eq.gfile%nmetaaryi*maxval(gfile%aryilen) ) &
             aryival=gfile%aryival
      endif
      if ( gfile%nmetaaryr.gt.izero ) then
         if (present(aryrname).and.size(aryrname).eq.gfile%nmetaaryr) &
             aryrname=gfile%aryrname
         if (present(aryrlen).and.size(aryrlen).eq.gfile%nmetaaryr) &
             aryrlen=gfile%aryrlen
         if (present(aryrval).and.size(aryrval).eq.gfile%nmetaaryr*maxval(gfile%aryrlen) ) &
             aryrval=gfile%aryrval
      endif
      if ( gfile%nmetaaryl.gt.izero ) then
         if (present(arylname).and.size(arylname).eq.gfile%nmetaaryl) &
             arylname=gfile%arylname
         if (present(aryllen).and.size(aryllen).eq.gfile%nmetaaryl) &
             aryllen=gfile%aryllen
         if (present(arylval).and.size(arylval).eq.gfile%nmetaaryl*maxval(gfile%aryllen) ) &
             arylval=gfile%arylval
      endif
      if ( gfile%nmetaaryc.gt.izero ) then
         if (present(arycname).and.size(arycname).eq.gfile%nmetaaryc) &
             arycname=gfile%arycname
         if (present(aryclen).and.size(aryclen).eq.gfile%nmetaaryc) &
             aryclen=gfile%aryclen
         if (present(arycval).and.size(arycval).eq.gfile%nmetaaryc*maxval(gfile%aryclen) ) &
             arycval=gfile%arycval
      endif
    endif

!    print *,'after getfilehead'
    if ( present(iret)) iret=izero
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine nemsio_getfilehead
!------------------------------------------------------------------------------

   subroutine nemsio_getfheadvari(gfile,varname,varval,iret)
!$$$  subprogram documentation block
!                .      .    .                                        .
! subprogram:    nemsio_getfheadvari
!   prgmmr:
!
! abstract: get meta data var value from file header
!
! program history log:
!   2009-08-31  lueken - added subprogram doc block
!
!   input argument list:
!    gfile
!    varname
!
!   output argument list:
!    varval
!    iret
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block


    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    character(len=*),  intent(in)                 :: varname
    integer(i_kind),intent(out)           :: varval
    integer(i_kind),optional,intent(out)  :: iret
    integer(i_kind), automatic :: i
!---
    if(present(iret) ) iret=-17
    do i=1,gfile%headvarinum
      if(equal_str_nocase(trim(varname),trim(gfile%headvariname(i))) ) then
           varval=gfile%headvarival(i)
           if(present(iret) ) iret=izero
           return
      endif
    enddo
!---
    if(gfile%nmetavari.gt.izero) then
      do i=1,gfile%nmetavari
        if(equal_str_nocase(trim(varname),trim(gfile%variname(i))) ) then
           varval=gfile%varival(i)
           if(present(iret) ) iret=izero
           return
        endif
      enddo
    endif
!---    
    if(.not.present(iret) ) call nemsio_stop
    return
  end subroutine nemsio_getfheadvari
!------------------------------------------------------------------------------

   subroutine nemsio_getfheadvarr(gfile,varname,varval,iret)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    nemsio_getfheadvarr
!   prgmmr:
!
! abstract: get meta data var value from file header
!
! program history log:
!   2009-08-31  lueken - added subprogram doc block
!
!   input argument list:
!    gfile
!    varname
!
!   output argument list:
!    varval
!    iret
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    character(len=*),  intent(in)                 :: varname
    real(r_single),intent(out)             :: varval
    integer(i_kind),optional,intent(out)  :: iret
    integer(i_kind), automatic :: i
!---
    if(present(iret) ) iret=-17
    do i=1,gfile%headvarrnum
      if(equal_str_nocase(trim(varname),trim(gfile%headvarrname(i))) ) then
           varval=gfile%headvarrval(i)
           if(present(iret) ) iret=izero
           return
      endif
    enddo
!---
    if(gfile%nmetavarr.gt.izero) then
      do i=1,gfile%nmetavarr
        if(equal_str_nocase(trim(varname),trim(gfile%varrname(i))) ) then
           varval=gfile%varrval(i)
           if(present(iret) ) iret=izero
           return
        endif
      enddo
    endif

    if(.not.present(iret) ) call nemsio_stop
    return
  end subroutine nemsio_getfheadvarr
!------------------------------------------------------------------------------

   subroutine nemsio_getfheadvarl(gfile,varname,varval,iret)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    nemsio_getfheadvarl
!   prgmmr:
!
! abstract: get meta data var value from file header
!
! program history log:
!   2009-08-31  lueken - added subprogram doc block
!
!   input argument list:
!    gfile
!    varname
!
!   output argument list:
!    varval
!    iret
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    character(*),  intent(in)                     :: varname
    logical(nemsio_logickind),intent(out)         :: varval
    integer(i_kind),optional,intent(out)  :: iret
    integer(i_kind)                       :: i
!---
    if(present(iret) ) iret=-17
    if(gfile%nmetavarl.gt.izero) then
      do i=1,gfile%nmetavarl
        if(equal_str_nocase(trim(varname),trim(gfile%varlname(i))) ) then
           varval=gfile%varlval(i)
           if(present(iret) ) iret=izero
           return
        endif
      enddo
    endif
!---
    if(.not.present(iret) ) call nemsio_stop
    return
  end subroutine nemsio_getfheadvarl
!------------------------------------------------------------------------------

   subroutine nemsio_getfheadvarc(gfile,varname,varval,iret)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    nemsio_getfheadvarc
!   prgmmr:
!
! abstract: get meta data var value from file header
!
! program history log:
!   2009-08-31  lueken - added subprogram doc block
!
!   input argument list:
!    gfile
!    varname
!
!   output argument list:
!    varval
!    iret
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    character(*),  intent(in)                     :: varname
    character(*),intent(out)                      :: varval
    integer(i_kind),optional,intent(out)          :: iret
    integer(i_kind), automatic :: i
!---
    if(present(iret) ) iret=-17
    do i=1,gfile%headvarcnum
      if(equal_str_nocase(trim(varname),trim(gfile%headvarcname(i))) ) then
           varval=gfile%headvarcval(i)
           if(present(iret) ) iret=izero
           return
      endif
    enddo
!---
    if(gfile%nmetavarc.gt.izero) then
      do i=1,gfile%nmetavarc
        if(equal_str_nocase(trim(varname),trim(gfile%varcname(i))) ) then
           varval=gfile%varcval(i)
           if(present(iret) ) iret=izero
           return
        endif
      enddo
    endif
!---
    if(.not.present(iret) ) call nemsio_stop
    return
  end subroutine nemsio_getfheadvarc
!------------------------------------------------------------------------------

  subroutine nemsio_getfheadaryi(gfile,varname,varval,iret)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    nemsio_getfheadaryi
!   prgmmr:
!
! abstract: get meta data var value from file header
!
! program history log:
!   2009-08-31  lueken - added subprogram doc block
!
!   input argument list:
!    gfile
!    varname
!
!   output argument list:
!    varval
!    iret
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    character(*),  intent(in)                     :: varname
    integer(i_kind),intent(out)                   :: varval(:)
    integer(i_kind),optional,intent(out)          :: iret
    integer(i_kind), automatic :: i,ierr
!---
    if(present(iret) ) iret=-17
    do i=1,gfile%headaryinum
      if(equal_str_nocase(trim(varname),trim(gfile%headaryiname(i))) ) then
           varval(:)=gfile%headaryival(1:gfile%aryilen(i),i)
           if(present(iret) ) iret=izero
           return
      endif
    enddo
!---
    if(gfile%nmetaaryi.gt.izero) then
      do i=1,gfile%nmetaaryi
        if(equal_str_nocase(trim(varname),trim(gfile%aryiname(i))) ) then
           varval(:)=gfile%aryival(1:gfile%aryilen(i),i)
           if(present(iret) ) iret=izero
           ierr=izero
           return
        endif
      enddo
    endif
!---    
    if(.not.present(iret) ) call nemsio_stop
    return
  end subroutine nemsio_getfheadaryi
!------------------------------------------------------------------------------

   subroutine nemsio_getfheadaryr(gfile,varname,varval,iret)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    nemsio_getfheadaryr
!   prgmmr:
!
! abstract: get meta data var value from file header
!
! program history log:
!   2009-08-31  lueken - added subprogram doc block
!
!   input argument list:
!    gfile
!    varname
!
!   output argument list:
!    varval
!    iret
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    character(*),  intent(in)                     :: varname
    real(r_single),intent(out)                    :: varval(:)
    integer(i_kind),optional,intent(out)          :: iret
    integer(i_kind)                               :: i,ierr
!---
    if(present(iret) ) iret=-17
    if(gfile%headaryrnum>izero) then
     do i=1,gfile%headaryrnum
      if(equal_str_nocase(trim(varname),trim(gfile%headaryrname(i))) ) then
           varval(:)=gfile%headaryrval(1:gfile%aryrlen(i),i)
           if(present(iret) ) iret=izero
           return
      endif
     enddo
    endif
!---
    if(gfile%nmetaaryr.gt.izero) then
      do i=1,gfile%nmetaaryr
        if(equal_str_nocase(trim(varname),trim(gfile%aryrname(i)))) then
           varval(:)=gfile%aryrval(1:gfile%aryrlen(i),i)
           if(present(iret) ) iret=izero
           ierr=izero
           return
        endif
      enddo
    endif
!---
    if(.not.present(iret) ) call nemsio_stop
    return
  end subroutine nemsio_getfheadaryr
!------------------------------------------------------------------------------

   subroutine nemsio_getfheadaryl(gfile,varname,varval,iret)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    nemsio_getfheadaryl
!   prgmmr:
!
! abstract: get meta data var value from file header
!
! program history log:
!   2009-08-31  lueken - added subprogram doc block
!
!   input argument list:
!    gfile
!    varname
!
!   output argument list:
!    varval
!    iret
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    character(*),  intent(in)                     :: varname
    logical(nemsio_logickind),intent(out)         :: varval(:)
    integer(i_kind),optional,intent(out)          :: iret
    integer(i_kind)                               :: i,ierr
!---
    if(present(iret) ) iret=-17
    if(gfile%nmetaaryl.gt.izero) then
      do i=1,gfile%nmetaaryl
        if(equal_str_nocase(trim(varname),trim(gfile%arylname(i)))) then
           varval(:)=gfile%arylval(1:gfile%aryllen(i),i)
           if(present(iret) ) iret=izero
           ierr=izero
           return
        endif
      enddo
    endif
!---
    if(.not.present(iret) ) call nemsio_stop
    return
  end subroutine nemsio_getfheadaryl
!------------------------------------------------------------------------------

   subroutine nemsio_getfheadaryc(gfile,varname,varval,iret)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    nemsio_getfheadaryc
!   prgmmr:
!
! abstract: get meta data var value from file header
!
! program history log:
!   2009-08-31  lueken - added subprogram doc block
!
!   input argument list:
!    gfile
!    varname
!
!   output argument list:
!    varval
!    iret
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    character(len=*),  intent(in)                 :: varname
    character(*),intent(out)                      :: varval(:)
    integer(i_kind),optional,intent(out)          :: iret
    integer(i_kind)                               :: i,ierr
!---
    if(present(iret) ) iret=-17
    if(gfile%headarycnum>izero) then
     do i=1,gfile%headarycnum
      if(equal_str_nocase(trim(varname),trim(gfile%headarycname(i))) ) then
           varval(:)=gfile%headarycval(1:gfile%aryclen(i),i)
           if(present(iret) ) iret=izero
           return
      endif
     enddo
    endif
!---
    if(gfile%nmetaaryc.gt.izero) then
      do i=1,gfile%nmetaaryc
        if(equal_str_nocase(trim(varname),trim(gfile%arycname(i)))) then
           varval(:)=gfile%arycval(1:gfile%aryclen(i),i)
           if(present(iret) ) iret=izero
           ierr=izero
           return
        endif
      enddo
    endif
!---
    if(.not.present(iret) ) call nemsio_stop
    return
  end subroutine nemsio_getfheadaryc
!------------------------------------------------------------------------------

  subroutine nemsio_readrec4(gfile,jrec,data,gdatatype,nframe,iret)
!$$$  subprogram documentation block
!                .      .    .                                        .
! subprogram:    nemsio_readrec4
!   prgmmr:
!
! abstract: read nemsio data by record number into a 2D 32 bits array
!
! program history log:
!   2009-08-31  lueken - added subprogram doc block
!
!   input argument list:
!    gfile
!    jrec
!    data
!    gdatatype
!    nframe
!
!   output argument list:
!    data
!    iret
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    integer(i_kind),intent(in)                    :: jrec
    real(r_single),intent(inout)                  :: data(:)
    integer(i_kind),optional,intent(out)          :: iret
    character(*), optional, intent(in)            :: gdatatype
    integer(i_kind),optional,intent(in)           :: nframe
    real(r_single),allocatable                    :: datatmp(:)
    integer(i_kind)                               :: i,j
!------------------------------------------------------------
! read 4 byte rec
!------------------------------------------------------------
   iret=-11
!---     
   if ( present(gdatatype) ) then
      if (trim(gdatatype).ne.trim(gfile%gdatatype) ) then
      print *,'WRONG: data type not consistant in fileheader and read request' 
      call nemsio_stop
     endif
   endif 
!---
   allocate(datatmp(gfile%fieldsize) )
   if ( gfile%gdatatype .eq. 'bin4') then 
      call nemsio_readrecbin4d4(gfile,jrec,datatmp,iret)
      if ( iret .ne.izero ) return
   else if ( gfile%gdatatype .eq. 'bin8') then
      call nemsio_readrecbin8d4(gfile,jrec,datatmp,iret)
      if ( iret .ne.izero ) return
   else
     call nemsio_readrecgrb4(gfile,jrec,datatmp,iret)
     if ( iret .ne.izero ) return
   endif
!---
   if ( present(nframe) ) then
     if(nframe.le.gfile%nframe ) then
      do j=1,gfile%dimy+2*gfile%nframe-2*nframe
       do i=1,gfile%dimx+2*gfile%nframe -2*nframe
        data(i+(j-ione)*(gfile%dimx+2*gfile%nframe-2*nframe))=datatmp(i+nframe        &
          +(j-ione+nframe)*(gfile%dimx+2*gfile%nframe))
       enddo
      enddo
     else
       print *,"WARNING: nframe is larger than the nframe in the file!"
       call nemsio_stop
     endif
   else
     data=datatmp
   endif
   deallocate(datatmp)
!---
   iret=0
   return
  end subroutine nemsio_readrec4
!------------------------------------------------------------------------------

  subroutine nemsio_readrec8(gfile,jrec,data,gdatatype,nframe,iret)
!$$$  subprogram documentation block
!                .      .    .                                        .
! subprogram:    nemsio_readrec8
!   prgmmr:
!
! abstract: read nemsio data (bin) by record number into a 2D 32 bits array
!
! program history log:
!   2009-08-31  lueken - added subprogram doc block
!
!   input argument list:
!    gfile
!    jrec
!    data
!    gdatatype
!    nframe
!
!   output argument list:
!    data
!    iret
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    integer(i_kind),intent(in)                    :: jrec
    real(r_kind),intent(inout)                    :: data(:)
    integer(i_kind),optional,intent(out)          :: iret
    character(*), optional, intent(in)            :: gdatatype
    integer(i_kind),optional,intent(in)           :: nframe
    real(r_kind),allocatable                      :: datatmp(:)
    integer(i_kind)                               :: i,j
!------------------------------------------------------------
! read 4 byte rec
!------------------------------------------------------------
   iret=-11
   if ( present(gdatatype) ) then
      if (trim(gdatatype).ne.trim(gfile%gdatatype) ) then
      print *,'WRONG: data type not consistant in fileheader and read request' 
      call nemsio_stop
     endif
   endif 

   allocate(datatmp(gfile%fieldsize))
   if ( gfile%gdatatype .eq. 'bin4') then
     call nemsio_readrecbin4d8(gfile,jrec,datatmp,iret)
     if ( iret .ne.izero ) return
   else if ( gfile%gdatatype .eq. 'bin8') then
     call nemsio_readrecbin8d8(gfile,jrec,datatmp,iret)
     if ( iret .ne.izero ) return
   else
     call nemsio_readrecgrb8(gfile,jrec,datatmp,iret)
     if ( iret .ne.izero ) return
   endif
!---
   if ( present(nframe) ) then
     if(nframe.le.gfile%nframe ) then
      do j=1,gfile%dimy+2*gfile%nframe-2*nframe
       do i=1,gfile%dimx+2*gfile%nframe -2*nframe
        data(i+(j-ione)*(gfile%dimx+2*gfile%nframe-2*nframe))=datatmp(i+nframe        &
          +(j-ione+nframe)*(gfile%dimx+2*gfile%nframe))
       enddo
      enddo
     else
       print *,"WARNING: nframe is larger than the nframe in the file!"
       call nemsio_stop
     endif
   else
     data=datatmp
   endif
   deallocate(datatmp)

!
   iret=izero
   return
  end subroutine nemsio_readrec8
!------------------------------------------------------------------------------

  subroutine nemsio_readrecv4(gfile,name,levtyp,lev,data,gdatatype,nframe,iret)
!$$$  subprogram documentation block
!                .      .    .                                        .
! subprogram:    nemsio_readrecv4
!   prgmmr:
!
! abstract: read nemsio data by record number into a 2D 32 bits array
!
! program history log:
!   2009-08-31  lueken - added subprogram doc block
!
!   input argument list:
!    gfile
!    name
!    levtyp
!    lev
!    data
!    gdatatype
!    nframe
!
!   output argument list:
!    data
!    iret
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block 

    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    character(*),intent(in)                       :: name
    character(*),intent(in),optional              :: levtyp
    integer(i_kind),optional,intent(in)           :: lev
    real(r_single),intent(inout)                  :: data(:)
    integer(i_kind),optional,intent(out)          :: iret
    character(*), optional, intent(in)            :: gdatatype
    integer(i_kind),optional,intent(in)           :: nframe
    real(r_single),allocatable                    :: datatmp(:)
    integer(i_kind)                               :: i,j
!------------------------------------------------------------
! read 4 byte rec
!------------------------------------------------------------
   iret=-11
   if ( present(gdatatype) ) then
      if (trim(gdatatype).ne.trim(gfile%gdatatype) ) then
      print *,'WRONG: data type not consistant in fileheader and read request' 
      call nemsio_stop
     endif
   endif 

   allocate(datatmp(gfile%fieldsize) )
   if ( gfile%gdatatype .eq. 'bin4') then
     call nemsio_readrecvbin4(gfile,name,levtyp,lev,datatmp,iret)
     if ( iret .ne.izero ) return
   else if ( gfile%gdatatype .eq. 'bin8') then
     call nemsio_readrecvbin8(gfile,name,levtyp,lev,datatmp,iret)
     if ( iret .ne.izero ) return
   else
     call nemsio_readrecvgrb4(gfile,name,levtyp,lev,datatmp,iret)
     if ( iret .ne.izero ) return
   endif
!---
   if ( present(nframe) ) then
     if(nframe.le.gfile%nframe ) then
      do j=1,gfile%dimy+2*gfile%nframe-2*nframe
       do i=1,gfile%dimx+2*gfile%nframe -2*nframe
        data(i+(j-ione)*(gfile%dimx+2*gfile%nframe-2*nframe))=datatmp(i+nframe        &
          +(j-ione+nframe)*(gfile%dimx+2*gfile%nframe))
       enddo
      enddo
     else
       print *,"WARNING: nframe is larger than the nframe in the file!"
       call nemsio_stop
     endif
   else
     data=datatmp
   endif
   deallocate(datatmp)
!---
   iret=izero
   return
  end subroutine nemsio_readrecv4
!------------------------------------------------------------------------------

  subroutine nemsio_readrecv8(gfile,name,levtyp,lev,data,gdatatype,nframe,iret)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    nemsio_readrecv8
!   prgmmr:
!
! abstract: read nemsio data by record number into a 2D 32 bits array
!
! program history log:
!   2009-08-31  lueken - added subprogram doc block
!
!   input argument list:
!    gfile
!    name
!    levtyp
!    lev
!    data
!    gdatatype
!    nframe
!
!   output argument list:
!    data
!    iret
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    character(*),intent(in)                       :: name
    character(*),intent(in),optional              :: levtyp
    integer(i_kind),optional,intent(in)           :: lev
    real(r_kind),intent(inout)                    :: data(:)
    integer(i_kind),optional,intent(out)          :: iret
    character(*), optional, intent(in)            :: gdatatype
    integer(i_kind),optional,intent(in)           :: nframe
    real(r_kind),allocatable                      :: datatmp(:)
    integer(i_kind)                               :: i,j
!------------------------------------------------------------
! read 8 byte rec
!------------------------------------------------------------
   iret=-11
   if ( present(gdatatype) ) then
      if (trim(gdatatype).ne.trim(gfile%gdatatype) ) then
      print *,'WRONG: data type not consistant in fileheader and read request' 
      call nemsio_stop
     endif
   endif 

   allocate(datatmp(gfile%fieldsize) )
   if ( gfile%gdatatype .eq. 'bin4') then
     call nemsio_readrecvbin4(gfile,name,levtyp,lev,datatmp,iret)
     if ( iret .ne.izero ) return
   else if ( gfile%gdatatype .eq. 'bin8') then
     call nemsio_readrecvbin8(gfile,name,levtyp,lev,datatmp,iret)
     if ( iret .ne.izero ) return
   else
     call nemsio_readrecvgrb8(gfile,name,levtyp,lev,datatmp,iret)
     if ( iret .ne.izero ) return
   endif
!---
   if ( present(nframe) ) then
     if(nframe.le.gfile%nframe ) then
      do j=1,gfile%dimy+2*gfile%nframe-2*nframe
       do i=1,gfile%dimx+2*gfile%nframe -2*nframe
        data(i+(j-ione)*(gfile%dimx+2*gfile%nframe-2*nframe))=datatmp(i+nframe        &
          +(j-ione+nframe)*(gfile%dimx+2*gfile%nframe))
       enddo
      enddo
     else
       print *,"WARNING: nframe is larger than the nframe in the file!"
       call nemsio_stop
     endif
   else
     data=datatmp
   endif
   deallocate(datatmp)
!
   iret=izero
   return
  end subroutine nemsio_readrecv8
!------------------------------------------------------------------------------

!*****************   read bin data set :  ********************************

!------------------------------------------------------------------------------

  subroutine nemsio_readrecbin4d4(gfile,jrec,data,iret)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    nemsio_readrecbin4d4
!   prgmmr:
!
! abstract: read nemsio data (bin) by record number into a 2D 32 bits array
!
! program history log:
!   2009-08-31  lueken - added subprogram doc block
!
!   input argument list:
!    gfile
!    jrec
!    data
!
!   output argument list:
!    data
!    iret
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    integer(i_kind),intent(in)                    :: jrec
    real(r_single),intent(inout)                  :: data(:)
    integer(i_kind),optional,intent(out)          :: iret
    integer(i_llong) :: iskip,iread,nread

    iret=-11
    iskip=gfile%tlmeta+int(jrec-ione,i_llong)*int(kind(data)*gfile%fieldsize+8,i_llong)
    iread=int(r_single,i_llong)*int(size(data),i_llong)
    call bafrreadl(gfile%flunit,iskip,iread,nread,data)
    if(nread.lt.iread) return
    iret=izero

    return
  end subroutine nemsio_readrecbin4d4
!------------------------------------------------------------------------------

  subroutine nemsio_readrecbin4d8(gfile,jrec,data,iret)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    nemsio_readrecbin4d8
!   prgmmr:
!
! abstract: read nemsio data (bin) by record number into a 2D 32 bits array
!
! program history log:
!   2009-08-31  lueken - added subprogram doc block
!
!   input argument list:
!    gfile
!    jrec
!    data
!
!   output argument list:
!    data
!    iret
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    integer(i_kind),intent(in)                    :: jrec
    real(r_kind),intent(out)                      :: data(:)
    integer(i_kind),optional,intent(out)          :: iret
    real(r_single),allocatable                    :: data4(:)
    integer(i_llong) :: iskip,iread,nread

    iret=-11
    allocate(data4(size(data)) )
    iskip=gfile%tlmeta+int(jrec-ione,i_llong)*int(kind(data4)*gfile%fieldsize+8,i_llong)
    iread=int(r_single,i_llong)*int(size(data4),i_llong)
    call bafrreadl(gfile%flunit,iskip,iread,nread,data4)
    if(nread.lt.iread) return
    data=data4
    iret=izero

    return
  end subroutine nemsio_readrecbin4d8
!------------------------------------------------------------------------------

  subroutine nemsio_readrecvbin4d4(gfile,name,levtyp,lev,data,iret)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    nemsio_readrecvbin4d4
!   prgmmr:
!
! abstract: read nemsio data (bin) by record number into a 2D 32 bits array
!
! program history log:
!   2009-08-31  lueken - added subprogram doc block
!
!   input argument list:
!    gfile
!    name
!    levtyp
!    lev
!
!   output argument list:
!    data
!    iret
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    character(*),intent(in)                       :: name
    character(*),intent(in),optional              :: levtyp
    integer(i_kind),optional,intent(in)           :: lev
    real(r_single),intent(out)                    :: data(:)
    integer(i_kind),optional,intent(out)          :: iret
    integer(i_llong) :: iskip,iread,nread
    integer(i_kind)  :: jrec, ierr

    iret=-12
    call nemsio_searchrecv(gfile,jrec,name,levtyp,lev,ierr)
    if ( ierr .ne. izero)  return
    iskip=gfile%tlmeta+int(jrec-ione,i_llong)*int(r_single*gfile%fieldsize+8,i_llong)
    iread=int(kind(data),i_llong)*int(size(data),i_llong)
    call bafrreadl(gfile%flunit,iskip,iread,nread,data)
    if(nread.lt.iread) return
    iret=izero

    return
  end subroutine nemsio_readrecvbin4d4
!------------------------------------------------------------------------------

  subroutine nemsio_readrecvbin4d8(gfile,name,levtyp,lev,data,iret)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    nemsio_readrecvbin4d8
!   prgmmr:
!
! abstract: read nemsio data (bin) by record number into a 2D 32 bits array
!
! program history log:
!   2009-08-31  lueken - added subprogram doc block
!
!   input argument list:
!    gfile
!    name
!    levtyp
!    lev
!
!   output argument list:
!    data
!    iret
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    character(*),intent(in)                       :: name
    character(*),intent(in),optional              :: levtyp
    integer(i_kind),optional,intent(in)           :: lev
    real(r_kind),intent(out)                      :: data(:)
    integer(i_kind),optional,intent(out)          :: iret
    real(r_single),allocatable                    :: data4(:)
    integer(i_llong) :: iskip,iread,nread
    integer(i_kind) :: jrec, ierr

    iret=-11
    allocate(data4(size(data)) )
    call nemsio_searchrecv(gfile,jrec,name,levtyp,lev,ierr)
    if ( ierr .ne. izero) return
    iskip=gfile%tlmeta+int(jrec-ione,i_llong)*int(r_single*gfile%fieldsize+8,i_llong)
    iread=int(kind(data4),i_llong)*int(size(data4),i_llong)
    call bafrreadl(gfile%flunit,iskip,iread,nread,data4)
    if(nread.lt.iread) return
    data=data4
    iret=izero

    return
  end subroutine nemsio_readrecvbin4d8
!------------------------------------------------------------------------------

  subroutine nemsio_readrecbin8d4(gfile,jrec,data,iret)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    nemsio_readrecbin8d4
!   prgmmr:
!
! abstract: read nemsio data (bin) by record number into a 2D 32 bits array
!
! program history log:
!   2009-08-31  lueken - added subprogram doc block
!
!   input argument list:
!    gfile
!    jrec
!
!   output argument list:
!    data
!    iret
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    integer(i_kind),intent(in)                    :: jrec
    real(r_single),intent(out)                    :: data(:)
    integer(i_kind),optional,intent(out)          :: iret
    real(r_kind),allocatable                      :: data8(:)
    integer(i_llong) :: iskip,iread,nread

    iret=-11
    allocate(data8(size(data)) )
    iskip=gfile%tlmeta+int(jrec-1,i_llong)*int(r_kind*gfile%fieldsize+8,i_llong)
    iread=int(r_kind,i_llong)*int(size(data8),i_llong)
    call bafrreadl(gfile%flunit,iskip,iread,nread,data8)
    if(nread.lt.iread) return
    data=data8
    iret=izero

    return
  end subroutine nemsio_readrecbin8d4
!------------------------------------------------------------------------------

  subroutine nemsio_readrecbin8d8(gfile,jrec,data,iret)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    nemsio_readrecbin8d8
!   prgmmr:
!
! abstract: read nemsio data (bin) by record number into a 2D 32 bits array
!
! program history log:
!   2009-08-31  lueken - added subprogram doc block
!
!   input argument list:
!    gfile
!    jrec
!
!   output argument list:
!    data
!    iret
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    integer(i_kind),intent(in)                    :: jrec
    real(r_kind),intent(out)                      :: data(:)
    integer(i_kind),optional,intent(out)          :: iret
    integer(i_llong) :: iskip,iread,nread

    iret=-11
    iskip=gfile%tlmeta+int(jrec-ione,i_llong)*int(r_kind*gfile%fieldsize+8,i_llong)
    iread=int(r_kind,i_llong)*int(size(data),i_llong)
    call bafrreadl(gfile%flunit,iskip,iread,nread,data)
    if(nread.lt.iread) return
    iret=izero

    return
  end subroutine nemsio_readrecbin8d8
!------------------------------------------------------------------------------

  subroutine nemsio_readrecvbin8d4(gfile,name,levtyp,lev,data,iret)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    nemsio_readrecvbin8d4
!   prgmmr:
!
! abstract: read nemsio data (bin) by record number into a 2D 32 bits array
!
! program history log:
!   2009-08-31  lueken - added subprogram doc block
!
!   input argument list:
!    gfile
!    name
!    levtyp
!    lev
!
!   output argument list:
!    data
!    iret
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    character(*),intent(in)                       :: name
    character(*),intent(in),optional              :: levtyp
    integer(i_kind),optional,intent(in)           :: lev
    real(r_single),intent(out)                    :: data(:)
    integer(i_kind),optional,intent(out)          :: iret
    real(r_kind),allocatable                      :: data8(:)
    integer(i_llong) :: iskip,iread,nread
    integer(i_kind) :: jrec, ierr

    iret=-11
    allocate(data8(size(data)) )
    call nemsio_searchrecv(gfile,jrec,name,levtyp,lev,ierr)
    if ( ierr .ne. izero) return
!    print *,'name=',name,'levtyp=',levtyp,'lev=',lev,'jrec=',jrec
    iskip=gfile%tlmeta+int(jrec-ione,i_llong)*int(r_kind*gfile%fieldsize+8,i_llong)
    iread=int(r_kind,i_llong)*int(size(data8),i_llong)
    call bafrreadl(gfile%flunit,iskip,iread,nread,data8)
    if(nread.lt.iread) return
    data=data8
    iret=izero

    return
  end subroutine nemsio_readrecvbin8d4
!------------------------------------------------------------------------------

  subroutine nemsio_readrecvbin8d8(gfile,name,levtyp,lev,data,iret)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    nemsio_readrecvbin8d8
!   prgmmr:
!
! abstract: read nemsio data (bin) by record number into a 2D 32 bits array
!
! program history log:
!   2009-08-31  lueken - added subprogram doc block
!
!   input argument list:
!    gfile
!    name
!    levtyp
!    lev
!
!   output argument list:
!    data
!    iret
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    character(*),intent(in)                       :: name
    character(*),intent(in),optional              :: levtyp
    integer(i_kind),optional,intent(in)           :: lev
    real(r_kind),intent(out)                      :: data(:)
    integer(i_kind),optional,intent(out)          :: iret
    integer(i_llong) :: iskip,iread,nread
    integer(i_kind) :: jrec, ierr

    iret=-11
    call nemsio_searchrecv(gfile,jrec,name,levtyp,lev,ierr)
    if ( ierr .ne. izero) return
    iskip=gfile%tlmeta+int(jrec-ione,i_llong)*int(r_kind*gfile%fieldsize+8,i_llong)
    iread=int(r_kind,i_llong)*int(size(data),i_llong)
    call bafrreadl(gfile%flunit,iskip,iread,nread,data)
    if(nread.lt.iread) return
    iret=izero

    return
  end subroutine nemsio_readrecvbin8d8
!------------------------------------------------------------------------------

  subroutine nemsio_searchrecv(gfile,jrec,name,levtyp,lev,iret)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    nemsio_searchrecv
!   prgmmr:
!
! abstract: search rec number giving rec name, levtyp and lev
!
! program history log:
!   2009-08-31  lueken - added subprogram doc block
!
!   input argument list:
!    gfile
!    name
!    levtyp
!    lev
!
!   output argument list:
!    jrec
!    iret
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    integer(i_kind),intent(out)                   :: jrec
    character(*),intent(in)                       :: name
    character(*),intent(in),optional              :: levtyp
    integer(i_kind),optional,intent(in)           :: lev
    integer(i_kind),optional,intent(out)          :: iret
    integer(i_kind)                               :: i, nlen,nlen1

    iret=-11
    nlen=min(len(name),len(gfile%recname))
    nlen1=min(len(levtyp),len(gfile%reclevtyp))
!
    jrec=izero
!    print *,'in search rec, recname length=',gfile%recname,'nrec=',gfile%nrec
    if(size(gfile%recname)/=gfile%nrec) return
    if(.not.present(levtyp)) then
!      print *,'in search rec, name=',lowercase(name)(1:nlen)
      do i=1,gfile%nrec
        if ( equal_str_nocase(trim(name),trim(gfile%recname(i))) ) then
           jrec=i
           exit
        endif
      enddo
    else if (size(gfile%reclevtyp).eq.gfile%nrec) then
      if(.not.present(lev)) then
       do i=1,gfile%nrec
        if ( equal_str_nocase(trim(name),trim(gfile%recname(i))) .and. &
           equal_str_nocase(trim(levtyp),trim(gfile%reclevtyp(i))) ) then
           jrec=i
           exit
        endif
       enddo
      else if(size(gfile%reclev).eq.gfile%nrec) then
       do i=1,gfile%nrec
        if ( equal_str_nocase(trim(name),trim(gfile%recname(i))) .and. &
           equal_str_nocase(trim(levtyp),trim(gfile%reclevtyp(i)) ) .and. &
           lev==gfile%reclev(i) ) then
           jrec=i
           exit
        endif
       enddo
      endif
    endif
    if ( jrec .ne.izero ) iret=izero
!
    return
  end subroutine nemsio_searchrecv
!------------------------------------------------------------------------------
!
!*****************   read grb data set :  *************************************
!
!------------------------------------------------------------------------------

  subroutine nemsio_readrecw34(gfile,jrec,data,iret)
!$$$  subprogram documentation block
!                .      .    .                                        .
! subprogram:    nemsio_readrecw34
!   prgmmr:
!
! abstract: read nemsio data by record number into a 2D 32 bits array, 
!           using w3_4 library to compile
!
! program history log:
!   2009-08-31  lueken - added subprogram doc block
!
!   input argument list:
!    gfile
!    jrec
!
!   output argument list:
!    data
!    iret
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    integer(i_kind),intent(in)                    :: jrec
    real(r_single),intent(out)                    :: data(:)
    integer(i_kind),optional,intent(out)          :: iret
    type(nemsio_grbmeta)         :: grbmeta
    integer(i_kind)             :: luidx
    integer(i_kind)             :: kf,k,kpds(200),kgds(200)
    logical*1,allocatable       :: lbms(:)
    integer(i_kind)             :: N=nemsio_kpds_intfill
    integer(i_kind)             :: ios,w34
!------------------------------------------------------------
! set up grib meta 
!------------------------------------------------------------
    luidx=izero
    if ( present(iret)) iret=-4
    w34=ione
    call nemsio_setrqst(gfile,grbmeta,ios,jrec=jrec,w34=w34)
    if (ios.ne.izero) then
       if ( present(iret))  then
         iret=ios
         return
       else
         call nemsio_stop
       endif
    endif
    allocate(lbms(grbmeta%jf))
    N=izero
!------------------------------------------------------------
! get data from getgb
!------------------------------------------------------------
!    print *,'in nemsio, before getgbm,mbuf=',gfile%mbuf,&
!     'nlen=',gfile%nlen,'nnum=',gfile%nnum,'mnum=',gfile%mnum, &
!     'jf=',grbmeta%jf,'jpds=',grbmeta%jpds(1:20),'jgds=', &
!     grbmeta%jgds(1:20) 
    call getgbm(gfile%flunit,luidx,grbmeta%jf,N,grbmeta%jpds,grbmeta%jgds,&
      gfile%mbuf,gfile%cbuf,gfile%nlen,gfile%nnum,gfile%mnum, &
      kf,k,kpds,kgds,lbms,data,ios)
    deallocate(lbms,grbmeta%lbms)
    if(ios.ne.izero) then
       if ( present(iret))  then
         print *,'getgb_ios=',ios
         return
       else
         call nemsio_stop
       endif
    endif
    if (present(iret)) iret=izero
  end subroutine nemsio_readrecw34
!------------------------------------------------------------------------------

  subroutine nemsio_readrecgrb4(gfile,jrec,data,iret)
!$$$  subprogram documentation block
!                .      .    .                                        .
! subprogram:    nemsio_readrecgrb4
!   prgmmr:
!
! abstract: read nemsio data by record number into a 2D 32 bits array,
!           using w3_d library to compile
!
! program history log:
!   2009-08-31  lueken - added subprogram doc block
!
!   input argument list:
!    gfile
!    jrec
!
!   output argument list:
!    data
!    iret
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    integer(i_kind),intent(in)                    :: jrec
    real(r_single),intent(out)                    :: data(:)
    integer(i_kind),optional,intent(out)          :: iret
    real(r_kind),allocatable                      :: data8(:)
    type(nemsio_grbmeta)         :: grbmeta
    integer(i_kind)             :: luidx
    integer(i_kind)             :: kf,k,kpds(200),kgds(200)
    logical*1,allocatable       :: lbms(:)
    integer(i_kind)             :: N=nemsio_kpds_intfill
    integer(i_kind)             :: ios
!------------------------------------------------------------
! set up grib meta 
!------------------------------------------------------------
    luidx=izero
    if ( present(iret)) iret=-4
    allocate(data8(size(data)) )
    call nemsio_setrqst(gfile,grbmeta,ios,jrec=jrec)
    if (ios.ne.izero) then
       if ( present(iret))  then
         iret=ios
         return
       else
         call nemsio_stop
       endif
    endif
!------------------------------------------------------------
! get data from getgb _w3d
!------------------------------------------------------------
    data8=data
    allocate(lbms(grbmeta%jf))
    N=izero
!    print *,'getrecgrb4,in nemsio, before getgbm,mbuf=',gfile%mbuf,&
!     'nlen=',gfile%nlen,'nnum=',gfile%nnum,'mnum=',gfile%mnum, &
!     'jf=',grbmeta%jf,'jpds=',grbmeta%jpds(1:20),'jgds=', &
!     grbmeta%jgds(1:20) 
    call getgbm(gfile%flunit,luidx,grbmeta%jf,N,grbmeta%jpds,grbmeta%jgds,&
      gfile%mbuf,gfile%cbuf,gfile%nlen,gfile%nnum,gfile%mnum, &
      kf,k,kpds,kgds,lbms,data8,ios)
    data=data8
    deallocate(lbms,grbmeta%lbms)
    if(ios.ne.izero) then
       if ( present(iret))  then
         print *,'getgb_ios=',ios
         return
       else
         call nemsio_stop
       endif
    endif
    if (present(iret)) iret=izero
  end subroutine nemsio_readrecgrb4
!------------------------------------------------------------------------------

  subroutine nemsio_readrecgrb8(gfile,jrec,data,iret)
!$$$  subprogram documentation block
!                .      .    .                                        .
! subprogram:    nemsio_readrecgrb8
!   prgmmr:
!
! abstract: read nemsio data by record number into a 2D 64 bits array, 
!           using w3_d library to compile
!
! program history log:
!   2009-08-31  lueken - added subprogram doc block
!
!   input argument list:
!    gfile
!    jrec
!
!   output argument list:
!    data
!    iret
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    integer(i_kind),intent(in)                    :: jrec
    real(r_kind),intent(out)                      :: data(:)
    integer(i_kind),optional,intent(out)          :: iret
    type(nemsio_grbmeta)         :: grbmeta
    integer(i_kind)             :: luidx
    integer(i_kind)             :: kf,k,kpds(200),kgds(200)
    logical*1,allocatable       :: lbms(:)
    integer(i_kind)             :: N=nemsio_kpds_intfill
    integer(i_kind)             :: ios
!------------------------------------------------------------
! set up grib meta 
!------------------------------------------------------------
    luidx=izero
    if ( present(iret)) iret=-4
    call nemsio_setrqst(gfile,grbmeta,ios,jrec=jrec)
    if (ios.ne.izero) then
       if ( present(iret))  then
         iret=ios
         return
       else
         call nemsio_stop
       endif
    endif
!------------------------------------------------------------
! get data from getgb _w3d
!------------------------------------------------------------
    allocate(lbms(grbmeta%jf))
    N=izero
    call getgbm(gfile%flunit,luidx,grbmeta%jf,N,grbmeta%jpds,grbmeta%jgds,&
      gfile%mbuf,gfile%cbuf,gfile%nlen,gfile%nnum,gfile%mnum, &
      kf,k,kpds,kgds,lbms,data,ios)
    deallocate(lbms,grbmeta%lbms)
    if(ios.ne.izero) then
       if ( present(iret))  then
         print *,'getgb_ios=',ios
         return
       else
         call nemsio_stop
       endif
    endif
    if (present(iret)) iret=izero
  end subroutine nemsio_readrecgrb8
!------------------------------------------------------------------------------

  subroutine nemsio_readrecvw34(gfile,vname,vlevtyp,vlev,data,iret)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    nemsio_readrecvw34
!   prgmmr:
!
! abstract: read nemsio data by field name into 32 bits array, 
!           using w3_4 library to compile
!
! program history log:
!   2009-08-31  lueken - added subprogram doc block
!
!   input argument list:
!    gfile
!    vname,vlevtyp
!    vlev
!
!   output argument list:
!    data
!    iret
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    character*(*),intent(in)                      :: vname,vlevtyp
    integer(i_kind),intent(in)                    :: vlev
    real(r_single),intent(out)                    :: data(:)
    integer(i_kind),optional,intent(out)          :: iret
    type(nemsio_grbmeta)         :: grbmeta
    integer(i_kind)             :: luidx
    integer(i_kind)             :: kf,k,kpds(200),kgds(200)
    logical*1,allocatable       :: lbms(:)
    integer(i_kind)             :: N=nemsio_kpds_intfill
    integer(i_kind)             :: ios,w34
!------------------------------------------------------------
! set up grib meta 
!------------------------------------------------------------
    luidx=izero
    if ( present(iret)) iret=-4
    w34=ione
    call nemsio_setrqst(gfile,grbmeta,ios,vname=vname, &
      vlevtyp=vlevtyp, vlev=vlev ,w34=w34)
    if (ios.ne.izero) then
       if ( present(iret))  then
         iret=ios
         return
       else
         call nemsio_stop
       endif
    endif
!------------------------------------------------------------
! get data from getgb _w34
!------------------------------------------------------------
    allocate(lbms(grbmeta%jf))
    N=izero
    call getgbm(gfile%flunit,luidx,grbmeta%jf,N,grbmeta%jpds,grbmeta%jgds,&
      gfile%mbuf,gfile%cbuf,gfile%nlen,gfile%nnum,gfile%mnum, &
      kf,k,kpds,kgds,lbms,data,ios)
    deallocate(lbms,grbmeta%lbms)
    if(ios.ne.izero) then
       if ( present(iret))  then
          print *,'getgb_ios=',ios
         return
       else
         call nemsio_stop
       endif
    endif
    if ( present(iret)) iret=izero
  end subroutine nemsio_readrecvw34
!------------------------------------------------------------------------------

  subroutine nemsio_readrecvgrb4(gfile,vname,vlevtyp,vlev,data,iret)
!$$$  subprogram documentation block
!                .      .    .                                        .
! subprogram:    nemsio_readrecvgrb4
!   prgmmr:
!
! abstract: read nemsio data by field name into a 2D 32bits array, 
!           using w3_d library to compile
!
! program history log:
!   2009-08-31  lueken - added subprogram doc block
!
!   input argument list:
!    gfile
!    vname,vlevtype
!    vlev
!
!   output argument list:
!    data
!    iret
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    character*(*),intent(in)                      :: vname,vlevtyp
    integer(i_kind),intent(in)                    :: vlev
    real(r_single),intent(out)                    :: data(:)
    integer(i_kind),optional,intent(out)          :: iret
    real(r_kind),allocatable                      :: data8(:)
    type(nemsio_grbmeta)         :: grbmeta
    integer(i_kind)             :: luidx
    integer(i_kind)             :: kf,k,kpds(200),kgds(200)
    logical*1,allocatable       :: lbms(:)
    integer(i_kind)             :: N=nemsio_kpds_intfill
    integer(i_kind)             :: ios
!------------------------------------------------------------
! set up grib meta 
!------------------------------------------------------------
    luidx=izero
    if ( present(iret)) iret=-4
    allocate(data8(size(data)) )
    call nemsio_setrqst(gfile,grbmeta,ios,vname=vname, &
      vlevtyp=vlevtyp, vlev=vlev )
    if (ios.ne.izero) then
       if ( present(iret))  then
         iret=ios
         return
       else
         call nemsio_stop
       endif
    endif
!------------------------------------------------------------
! get data from getgb _w3d
!------------------------------------------------------------
    data8=data
    allocate(lbms(grbmeta%jf))
    N=izero
    call getgbm(gfile%flunit,luidx,grbmeta%jf,N,grbmeta%jpds,grbmeta%jgds,&
      gfile%mbuf,gfile%cbuf,gfile%nlen,gfile%nnum,gfile%mnum, &
      kf,k,kpds,kgds,lbms,data8,ios)
    data=data8
    deallocate(lbms,grbmeta%lbms)
    if(ios.ne.izero) then
       if ( present(iret))  then
          print *,'getgb_ios=',ios
         return
       else
         call nemsio_stop
       endif
    endif
    if ( present(iret)) iret=izero
  end subroutine nemsio_readrecvgrb4
!------------------------------------------------------------------------------

  subroutine nemsio_readrecvgrb8(gfile,vname,vlevtyp,vlev,data,iret)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    nemsio_readrecvgrb8
!   prgmmr:
!
! abstract: read nemsio data by field name into a 2D 64bits array, 
!           using w3_d library to compile
!
! program history log:
!   2009-08-31  lueken - added subprogram doc block
!
!   input argument list:
!    gfile
!    vname,vlevtyp
!    vlev
!
!   output argument list:
!    data
!    iret
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    character*(*),intent(in)                      :: vname,vlevtyp
    integer(i_kind),intent(in)                    :: vlev
    real(r_kind),intent(out)                      :: data(:)
    integer(i_kind),optional,intent(out)          :: iret
    type(nemsio_grbmeta)         :: grbmeta
    integer(i_kind)             :: luidx
    integer(i_kind)             :: kf,k,kpds(200),kgds(200)
    logical*1,allocatable       :: lbms(:)
    integer(i_kind)             :: N=nemsio_kpds_intfill
    integer(i_kind)             :: ios
!------------------------------------------------------------
! set up grib meta 
!------------------------------------------------------------
    luidx=izero
    if ( present(iret)) iret=-4
    call nemsio_setrqst(gfile,grbmeta,ios,vname=vname, &
      vlevtyp=vlevtyp, vlev=vlev )
    if (ios.ne.izero) then
       if ( present(iret))  then
         iret=ios
         return
       else
         call nemsio_stop
       endif
    endif
!------------------------------------------------------------
! get data from getgb _w3d
!------------------------------------------------------------
    allocate(lbms(grbmeta%jf))
    N=izero
    call getgbm(gfile%flunit,luidx,grbmeta%jf,N,grbmeta%jpds,grbmeta%jgds,&
      gfile%mbuf,gfile%cbuf,gfile%nlen,gfile%nnum,gfile%mnum, &
      kf,k,kpds,kgds,lbms,data,ios)
    deallocate(lbms,grbmeta%lbms)
    if(ios.ne.izero) then
       if ( present(iret))  then
          print *,'getgb_ios=',ios
         return
       else
         call nemsio_stop
       endif
    endif
    if ( present(iret)) iret=izero
  end subroutine nemsio_readrecvgrb8
!------------------------------------------------------------------------------

!*****************   write data set :  ********************************

!------------------------------------------------------------------------------

  subroutine nemsio_writerec4(gfile,jrec,data,gdatatype,iret,itr,zhour)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    nemsio_writerec4
!   prgmmr:
!
! abstract: write nemsio a 2D 32 bits array data into bin file using record number
!
! program history log:
!   2009-08-31  lueken - added subprogram doc block
!
!   input argument list:
!    gfile
!    jrec
!    data
!    itr
!    zhour
!    gdatatype
!
!   output argument list:
!    iret
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    integer(i_kind),intent(in)                    :: jrec
    real(r_single),intent(in)                     :: data(:)
    integer(i_kind),optional,intent(out)          :: iret
    integer(i_kind),optional,intent(in)           :: itr 
    real(r_single),optional,intent(in)            :: zhour
    character(*), optional, intent(in)            :: gdatatype
!
!------------------------------------------------------------
! read 4 byte rec
!------------------------------------------------------------
   if ( present(gdatatype) ) then
      if (trim(gdatatype).ne.trim(gfile%gdatatype) ) then
      print *,'WRONG: data type not consistant in fileheader and read request' 
      call nemsio_stop
     endif
   endif 

   if ( gfile%gdatatype .eq. 'bin4') then
     call nemsio_writerecbin4d4(gfile,jrec,data,iret)
     if ( iret .ne.izero ) return
   else if ( gfile%gdatatype .eq. 'bin8') then
     call nemsio_writerecbin8d4(gfile,jrec,data,iret)
     if ( iret .ne.izero ) return
   else
     call nemsio_writerecgrb4(gfile,jrec,data,iret,itr=itr,zhour=zhour)
     if ( iret .ne.izero ) return
   endif
   iret=izero
!
   return
  end subroutine nemsio_writerec4
!------------------------------------------------------------------------------

  subroutine nemsio_writerec8(gfile,jrec,data,gdatatype,iret,itr,zhour)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    nemsio_writerec8
!   prgmmr:
!
! abstract: write nemsio a 2D 64 bits array data into bin file using record number
!
! program history log:
!   2009-08-31  lueken - added subprogram doc block
!
!   input argument list:
!    gfile
!    jrec
!    data
!    itr
!    zhour
!    gdatatype
!
!   output argument list:
!    iret
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    integer(i_kind),intent(in)                    :: jrec
    real(r_kind),intent(in)                       :: data(:)
    integer(i_kind),optional,intent(out)          :: iret
    integer(i_kind),optional,intent(in)           :: itr 
    real(r_single),optional,intent(in)            :: zhour
    character(*), optional, intent(in)            :: gdatatype
!------------------------------------------------------------
! read 4 byte rec
!------------------------------------------------------------
   if ( present(gdatatype) ) then
      if (trim(gdatatype).ne.trim(gfile%gdatatype) ) then
      print *,'WRONG: data type not consistant in fileheader and read request' 
      call nemsio_stop
     endif
   endif 

   if ( gfile%gdatatype .eq. 'bin4') then
     call nemsio_writerecbin4d8(gfile,jrec,data,iret)
     if ( iret .ne.izero ) return
   else if ( gfile%gdatatype .eq. 'bin8') then
     call nemsio_writerecbin8d8(gfile,jrec,data,iret)
     if ( iret .ne.izero ) return
   else
     call nemsio_writerecgrb8(gfile,jrec,data,iret,itr=itr,zhour=zhour)
     if ( iret .ne.izero ) return
   endif
   iret=izero
!
   return
  end subroutine nemsio_writerec8
!------------------------------------------------------------------------------

  subroutine nemsio_writerecv4(gfile,name,levtyp,lev,data,gtype,gdatatype,iret, &
             itr,zhour)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    nemsio_writerecv4
!   prgmmr:
!
! abstract: write nemsio a 2D 32 bits array data into bin file using record number
!
! program history log:
!   2009-08-31  lueken - added subprogram doc block
!
!   input argument list:
!    gfile
!    name
!    levtyp
!    lev
!    data
!    itr
!    zhour
!    gtype
!    gdatatype
!
!   output argument list:
!    iret
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    character(*),intent(in)                       :: name
    character(*),optional,intent(in)              :: levtyp
    integer(i_kind),optional,intent(in)           :: lev
    real(r_single),intent(in)                     :: data(:)
    integer(i_kind),optional,intent(out)          :: iret
    integer(i_kind),optional,intent(in)           :: itr 
    real(r_single),optional,intent(in)            :: zhour
    character(*), optional, intent(in)            :: gtype
    character(*), optional, intent(in)            :: gdatatype
!------------------------------------------------------------
! read 4 byte rec
!------------------------------------------------------------
   if (present(gtype) .and. gfile%gtype.ne.trim(gtype) ) then
     print *,'ERROR: the NEMSIO model type is ',gfile%gtype, 'input is',gtype
     call nemsio_stop
   endif
   if ( present(gdatatype) ) then
      if (trim(gdatatype).ne.trim(gfile%gdatatype) ) then
      print *,'WRONG: data type not consistant in fileheader and read request' 
      call nemsio_stop
     endif
   endif 

   if ( gfile%gdatatype .eq. 'bin4') then
!     print *,'call nemsio_writerecvbin4d4'
     call nemsio_writerecvbin4d4(gfile,name,levtyp,lev,data,iret)
     if ( iret .ne.izero ) return
   else if ( gfile%gdatatype .eq. 'bin8') then
     call nemsio_writerecvbin8d4(gfile,name,levtyp,lev,data,iret)
     if ( iret .ne.izero ) return
   else
!     print *,'call nemsio_writerecvgrb4'
     call nemsio_writerecvgrb4(gfile,name,levtyp,lev,data,iret,itr=itr,        &
          zhour=zhour)
     if ( iret .ne.izero ) return
   endif
   iret=izero
!
   return
  end subroutine nemsio_writerecv4
!------------------------------------------------------------------------------

  subroutine nemsio_writerecv8(gfile,name,levtyp,lev,data,gtype,gdatatype,iret, &
             itr,zhour)
!$$$  subprogram documentation block
!                .      .    .                                      .
! subprogram:    nemsio_writerecv8
!   prgmmr:
!
! abstract: write nemsio a 2D 32 bits array data into bin file using record number
!
! program history log:
!   2009-08-31  lueken - added subprogram doc block
!
!   input argument list:
!    gfile
!    name
!    levtyp
!    lev
!    data
!    itr
!    zhour
!    gtype
!    gdatatype
!
!   output argument list:
!    iret
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    character(*),intent(in)                       :: name
    character(*),optional,intent(in)              :: levtyp
    integer(i_kind),optional,intent(in)           :: lev
    real(r_kind),intent(in)                       :: data(:)
    integer(i_kind),optional,intent(out)          :: iret
    integer(i_kind),optional,intent(in)           :: itr 
    real(r_single),optional,intent(in)            :: zhour
    character(*), optional, intent(in)            :: gtype
    character(*), optional, intent(in)            :: gdatatype
!------------------------------------------------------------
! read 4 byte rec
!------------------------------------------------------------
   if (present(gtype) .and. gfile%gtype.ne.trim(gtype) ) then
     print *,'ERROR: the NEMSIO model type is ',gfile%gtype, 'input is',gtype
     call nemsio_stop
   endif
   if ( present(gdatatype) ) then
      if (trim(gdatatype).ne.trim(gfile%gdatatype) ) then
      print *,'WRONG: data type not consistant in fileheader and read request' 
      call nemsio_stop
     endif
   endif 

   if ( gfile%gdatatype .eq. 'bin4') then
     call nemsio_writerecvbin4d8(gfile,name,levtyp,lev,data,iret)
     if ( iret .ne.izero ) return
   else if ( gfile%gdatatype .eq. 'bin8') then
     call nemsio_writerecvbin8d8(gfile,name,levtyp,lev,data,iret)
     if ( iret .ne.izero ) return
   else
     call nemsio_writerecvgrb8(gfile,name,levtyp,lev,data,iret,itr=itr,    &
          zhour=zhour)
     if ( iret .ne.izero ) return
   endif
   iret=izero
!
   return
  end subroutine nemsio_writerecv8
!------------------------------------------------------------------------------

!*****************   write out bin data set :  ********************************

!------------------------------------------------------------------------------

  subroutine nemsio_writerecbin4d4(gfile,jrec,data,iret)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    nemsio_writerecbin4d4
!   prgmmr:
!
! abstract: read nemsio data (bin) by record number into a 2D 32 bits array
!
! program history log:
!   2009-08-31  lueken - added subprogram doc block
!
!   input argument list:
!    gfile
!    jrec
!    data
!
!   output argument list:
!    iret
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    integer(i_kind),intent(in)                    :: jrec
    real(r_single),intent(in)                     :: data(:)
    integer(i_kind),optional,intent(out)          :: iret
    integer(i_llong) :: iskip,iwrite,nwrite

!
    real(r_kind) timef,stime
!
    iret=-11
    if(size(data)/=gfile%fieldsize) then
      print *,'WRONG: input data size ',size(data),' is not match the data domain ', &
        gfile%fieldsize,'please check dimension and nframe'
      return
    endif
    iskip=gfile%tlmeta+int(jrec-ione,i_llong)*int(r_single*gfile%fieldsize+8,i_llong)
    iwrite=int(r_single,i_llong)*int(size(data),i_llong)
    call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,data)
!     write(0,'(a15,I13.0)')'writerec iskip=',iskip 
!     print *,'in nemsio_writerecbin4d4,iwrite=',iwrite,'nwrite=',nwrite
    if(nwrite.lt.iwrite) return
    iret=izero

    return
  end subroutine nemsio_writerecbin4d4
!------------------------------------------------------------------------------

  subroutine nemsio_writerecbin4d8(gfile,jrec,data,iret)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    nemsio_writerecbin4d8
!   prgmmr:
!
! abstract: read nemsio data (bin) by record number into a 2D 32 bits array
!
! program history log:
!   2009-08-31  lueken - added subprogram doc block
!
!   input argument list:
!    gfile
!    jrec
!    data
!
!   output argument list:
!    iret
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    integer(i_kind),intent(in)                    :: jrec
    real(r_kind),intent(in)                       :: data(:)
    integer(i_kind),optional,intent(out)          :: iret
    real(r_single),allocatable                    :: data4(:)
    integer(i_llong) :: iskip,iwrite,nwrite

    iret=-11
    if(size(data)/=gfile%fieldsize) then
      print *,'WRONG: input data size ',size(data),' is not match the data domain ', &
        gfile%fieldsize,'please check dimension and nframe'
      return
    endif
    allocate(data4(size(data)) )
    data4=data
    iskip=gfile%tlmeta+int(jrec-ione,i_llong)*int(r_single*gfile%fieldsize+8,i_llong)
    iwrite=int(r_single,i_llong)*int(size(data4),i_llong)
    call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,data4)
    if(nwrite.lt.iwrite) return
    iret=izero

    return
  end subroutine nemsio_writerecbin4d8
!------------------------------------------------------------------------------

 subroutine nemsio_writerecvbin4d4(gfile,name,levtyp,lev,data,iret)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    nemsio_writerecvbin4d4
!   prgmmr:
!
! abstract: read nemsio data (bin) by record number into a 2D 32 bits array
!
! program history log:
!   2009-08-31  lueken - added subprogram doc block
!
!   input argument list:
!    gfile
!    name
!    levtyp
!    lev
!    data
!
!   output argument list:
!    iret
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    character(*),intent(in)                       :: name
    character(*),optional,intent(in)              :: levtyp
    integer(i_kind),optional,intent(in)           :: lev
    real(r_single),intent(in)                     :: data(:)
    integer(i_kind),optional,intent(out)          :: iret
    integer(i_kind)  :: jrec, ierr
    integer(i_llong) :: iskip,iwrite,nwrite

    iret=-11
    call nemsio_searchrecv(gfile,jrec,name,levtyp,lev,ierr)
    if ( ierr .ne. izero) return
    if(size(data)/=gfile%fieldsize) then
      print *,'WRONG: input data size ',size(data),' is not match the data domain ', &
        gfile%fieldsize,'please check dimension and nframe'
      return
    endif
    iskip=gfile%tlmeta+int(jrec-ione,i_llong)*int(r_single*gfile%fieldsize+8,i_llong)
    iwrite=int(r_single,i_llong)*int(size(data),i_llong)
    call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,data)
    if(nwrite.lt.iwrite) return
    iret=izero

    return
  end subroutine nemsio_writerecvbin4d4
!------------------------------------------------------------------------------

 subroutine nemsio_writerecvbin4d8(gfile,name,levtyp,lev,data,iret)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    nemsio_writerecvbin4d8
!   prgmmr:
!
! abstract: read nemsio data (bin) by record number into a 2D 32 bits array
!
! program history log:
!   2009-08-31  lueken - added subprogram doc block
!
!   input argument list:
!    gfile
!    name
!    levtyp
!    lev
!    data
!
!   output argument list:
!    iret
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    character(*),intent(in)                       :: name
    character(*),optional,intent(in)              :: levtyp
    integer(i_kind),optional,intent(in)           :: lev
    real(r_kind),intent(in)                       :: data(:)
    integer(i_kind),optional,intent(out)          :: iret
    real(r_single),allocatable                    :: data4(:)
    integer(i_kind)  :: jrec, ierr
    integer(i_llong) :: iskip,iwrite,nwrite

    iret=-11
    allocate(data4(size(data)) )
    data4=data
    call nemsio_searchrecv(gfile,jrec,name,levtyp,lev,ierr)
    if ( ierr .ne. izero) return
    if(size(data)/=gfile%fieldsize) then
      print *,'WRONG: input data size ',size(data),' is not match the data domain ', &
        gfile%fieldsize,'please check dimension and nframe'
      return
    endif
    iskip=gfile%tlmeta+int(jrec-ione,i_llong)*int(r_single*gfile%fieldsize+8,i_llong)
    iwrite=int(r_single,i_llong)*int(size(data4),i_llong)
    call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,data4)
    if(nwrite.lt.iwrite) return
    iret=izero

    return
  end subroutine nemsio_writerecvbin4d8
!------------------------------------------------------------------------------

  subroutine nemsio_writerecbin8d4(gfile,jrec,data,iret)
!$$$  subprogram documentation block
!                .      .    .                                      .
! subprogram:    nemsio_writerecbin8d4
!   prgmmr:
!
! abstract: read nemsio data (bin) by record number into a 2D 32 bits array
!
! program history log:
!   2009-08-31  lueken - added subprogram doc block
!
!   input argument list:
!    gfile
!    jrec
!    data
!
!   output argument list:
!    iret
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    integer(i_kind),intent(in)                    :: jrec
    real(r_single),intent(in)                     :: data(:)
    integer(i_kind),optional,intent(out)          :: iret
    real(r_kind),allocatable                      :: data8(:)
    integer(i_llong) :: iskip,iwrite,nwrite

    iret=-11
    if(size(data)/=gfile%fieldsize) then
      print *,'WRONG: input data size ',size(data),' is not match the data domain ', &
        gfile%fieldsize,'please check dimension and nframe'
      return
    endif
    allocate(data8(size(data)) )
    data8=data
    iskip=gfile%tlmeta+int(jrec-ione,i_llong)*int(r_kind*gfile%fieldsize+8,i_llong)
    iwrite=int(r_kind,i_llong)*int(size(data8),i_llong)
    call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,data8)
    if(nwrite.lt.iwrite) return
    iret=izero

    return
  end subroutine nemsio_writerecbin8d4
!------------------------------------------------------------------------------

  subroutine nemsio_writerecbin8d8(gfile,jrec,data,iret)
!$$$  subprogram documentation block
!                .      .    .                                        .
! subprogram:    nemsio_writerecbin8d4
!   prgmmr:
!
! abstract: read nemsio data (bin) by record number into a 2D 32 bits array
!
! program history log:
!   2009-08-31  lueken - added documentation block
!
!   input argument list:
!    gfile
!    jrec
!    data
!
!   output argument list:
!    iret
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block


    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    integer(i_kind),intent(in)                    :: jrec
    real(r_kind),intent(in)                       :: data(:)
    integer(i_kind),optional,intent(out)          :: iret
    integer(i_llong) :: iskip,iwrite,nwrite

    iret=-11
    if(size(data)/=gfile%fieldsize) then
      print *,'WRONG: input data size ',size(data),' is not match the data domain ', &
        gfile%fieldsize,'please check dimension and nframe'
      return
    endif
    iskip=gfile%tlmeta+int(jrec-ione,i_llong)*int(r_kind*gfile%fieldsize+8,i_llong)
    iwrite=int(r_kind,i_llong)*int(size(data),i_llong)
    call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,data)
    if(nwrite.lt.iwrite) return
    iret=izero

    return
  end subroutine nemsio_writerecbin8d8
!------------------------------------------------------------------------------

  subroutine nemsio_writerecvbin8d4(gfile,name,levtyp,lev,data,iret)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    nemsio_writerecvbin8d4
!   prgmmr:
!
! abstract: read nemsio data (bin) by record number into a 2D 32 bits array
!
! program history log:
!   2009-08-31  lueken - added subprogram doc block
!
!   input argument list:
!    gfile
!    name
!    levtyp
!    lev
!    data
!
!   output argument list:
!    iret
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    character(*),intent(in)                       :: name
    character(*),optional,intent(in)              :: levtyp
    integer(i_kind),optional,intent(in)           :: lev
    real(r_single),intent(in)                     :: data(:)
    integer(i_kind),optional,intent(out)          :: iret
    real(r_kind),allocatable                      :: data8(:)
    integer(i_kind)  :: jrec, ierr
    integer(i_llong) :: iskip,iwrite,nwrite

    iret=-11
    if(size(data)/=gfile%fieldsize) then
      print *,'WRONG: input data size ',size(data),' is not match the data domain ', &
        gfile%fieldsize,'please check dimension and nframe'
      return
    endif
    allocate(data8(size(data)) )
    data8=data
    call nemsio_searchrecv(gfile,jrec,name,levtyp,lev,ierr)
    if ( ierr .ne. izero) return
    iskip=gfile%tlmeta+int(jrec-ione,i_llong)*int(r_kind*gfile%fieldsize+8,i_llong)
    iwrite=int(r_kind,i_llong)*int(size(data8),i_llong)
    call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,data8)
    if(nwrite.lt.iwrite) return
    iret=izero

    return
  end subroutine nemsio_writerecvbin8d4

!------------------------------------------------------------------------------

  subroutine nemsio_writerecvbin8d8(gfile,name,levtyp,lev,data,iret)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    nemsio_writerecvbin8d8
!   prgmmr:
!
! abstract: read nemsio data (bin) by record number into a 2D 32 bits array
!
! program history log:
!   2009-08-31  lueken - added subprogram doc block
!
!   input argument list:
!    gfile
!    name
!    levtyp
!    lev
!    data
!
!   output argument list:
!    iret
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block


    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    character(*),intent(in)                       :: name
    character(*),optional,intent(in)              :: levtyp
    integer(i_kind),optional,intent(in)           :: lev
    real(r_kind),intent(in)                       :: data(:)
    integer(i_kind),optional,intent(out)          :: iret
    integer(i_kind)  :: jrec, ierr
    integer(i_llong) :: iskip,iwrite,nwrite

    iret=-11
    if(size(data)/=gfile%fieldsize) then
      print *,'WRONG: input data size ',size(data),' is not match the data domain ', &
        gfile%fieldsize,'please check dimension and nframe'
      return
    endif
    call nemsio_searchrecv(gfile,jrec,name,levtyp,lev,ierr)
    if ( ierr .ne. izero) return
    iskip=gfile%tlmeta+int(jrec-ione,i_llong)*int(r_kind*gfile%fieldsize+8,i_llong)
    iwrite=int(r_kind,i_llong)*int(size(data),i_llong)
    call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,data)
    if(nwrite.lt.iwrite) return
    iret=izero

    return
  end subroutine nemsio_writerecvbin8d8
!------------------------------------------------------------------------------
!
!*****************   write out grb data set :  ********************************
!
!------------------------------------------------------------------------------

  subroutine nemsio_writerecw34(gfile,jrec,data,iret,idrt,itr,zhour)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    nemsio_writerecw34
!   prgmmr:
!
! abstract: read nemsio data by record number into a 2D 32bits array, 
!           using w3_4 library to compile
!
! program history log:
!   2009-08-31  lueken - added subprogram doc block
!
!   input argument list:
!    gfile
!    jrec
!    data
!    idrt
!    itr
!    zhour
!
!   output argument list:
!    iret
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    implicit none
    type(nemsio_gfile),intent(in)               :: gfile
    integer(i_kind),intent(in)                  :: jrec
    real(r_single),intent(in)                   :: data(:)
    integer(i_kind),optional,intent(out)        :: iret
    integer(i_kind),optional,intent(in)         :: idrt
    integer(i_kind),optional,intent(in)         :: itr
    real(r_single),optional,intent(in)          :: zhour
    type(nemsio_grbmeta)           :: grbmeta
    integer(i_kind)                :: i
    integer(i_kind)                :: ios,w34,ibms
!---
    real(r_single)      :: mymax
!------------------------------------------------------------
! set up grib meta 
!------------------------------------------------------------
    if(present(iret)) iret=-4
    w34=ione
!------------------------------------------------------------
! set up grib meta lbms
!------------------------------------------------------------
    ibms=izero
    if(any(abs(data)>=nemsio_undef_grb)) ibms=ione
!
    if(present(idrt)) then
      call nemsio_setrqst(gfile,grbmeta,ios,jrec=jrec,w34=w34, &
           idrt=idrt,itr=itr,zhour=zhour,ibms=ibms)
    else
      call nemsio_setrqst(gfile,grbmeta,ios,jrec=jrec,w34=w34, &
           itr=itr,zhour=zhour,ibms=ibms)
    endif
    if (ios.ne.izero) then
       if ( present(iret))  then
         iret=ios
         return
       else
         call nemsio_stop
       endif
    endif
!
    grbmeta%lbms=.true.
    where(abs(data)>=nemsio_undef_grb) grbmeta%lbms=.false.
    mymax=minval(data)
    do i=1,gfile%fieldsize
     if(abs(data(i))<nemsio_undef_grb) then
       if(data(i) .gt.mymax) mymax=data(i)
     endif
    enddo
!
!------------------------------------------------------------
! check precision -- for pressure now
!------------------------------------------------------------
    if ( grbmeta%jpds(5).eq.ione .and. grbmeta%jpds(6).eq.109 ) then
     grbmeta%jpds(22)=min(int(5-log10(mymax)),2)
    endif
!------------------------------------------------------------
! get data from putgb _w34
!------------------------------------------------------------
    call putgb(gfile%flunit,grbmeta%jf,grbmeta%jpds,grbmeta%jgds, &
      grbmeta%lbms,data,ios)
    deallocate(grbmeta%lbms)
    if(ios.ne.izero) then
       if ( present(iret))  then
          print *,'putgb_ios=',ios
         iret=ios
         return
       else
         call nemsio_stop
       endif
    endif
    if(present(iret)) iret=izero
  end subroutine nemsio_writerecw34
!------------------------------------------------------------------------------

  subroutine nemsio_writerecgrb4(gfile,jrec,data,iret,idrt,itr,zhour)
!$$$  subprogram documentation block
!                .      .    .                                         .
! subprogram:    nemsio_writerecgrb4
!   prgmmr:
!
! abstract: read nemsio data by record number into a 2D 32bits array, 
!           using w3_d library to compile
! program history log:
!   2009-08-31  lueken -added subprogram doc block
!
!   input argument list:
!    gfile
!    jrec
!    data
!    idrt
!    itr
!    zhour
!
!   output argument list:
!    iret
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    implicit none
    type(nemsio_gfile),intent(in)               :: gfile
    integer(i_kind),intent(in)                  :: jrec
    real(r_single),intent(in)                   :: data(:)
    integer(i_kind),optional,intent(out)        :: iret
    integer(i_kind),optional,intent(in)         :: idrt
    integer(i_kind),optional,intent(in)         :: itr
    real(r_single),optional,intent(in)          :: zhour
    real(r_kind),allocatable                    :: data8(:)
    type(nemsio_grbmeta)           :: grbmeta
    integer(i_kind)                :: nc,i,nc1
    integer(i_kind)                :: ios,ibms
    real(r_single)                 :: mymax
!------------------------------------------------------------
! set up grib meta 
!------------------------------------------------------------
    if(present(iret)) iret=-4
!------------------------------------------------------------
! set up grib meta ibms
!------------------------------------------------------------
    ibms=izero
!
    allocate(data8(size(data)) )
    data8=data
    if(any(abs(data8)>=nemsio_undef_grb))  ibms=izero
!
!------------------------------------------------------------
! set up grib meta data
!------------------------------------------------------------
    if(present(idrt)) then
      call nemsio_setrqst(gfile,grbmeta,ios,jrec=jrec,idrt=idrt, &
           itr=itr,zhour=zhour,ibms=ibms)
    else
      call nemsio_setrqst(gfile,grbmeta,ios,jrec=jrec, &
           itr=itr,zhour=zhour,ibms=ibms)
    endif
    if (ios.ne.izero) then
       if ( present(iret))  then
         iret=ios
         return
       else
         call nemsio_stop
       endif
    endif
!
!------------------------------------------------------------
! set up lbms 
!------------------------------------------------------------
    grbmeta%lbms=.true.
    where(abs(data8)>=nemsio_undef_grb) grbmeta%lbms=.false.
    mymax=minval(data8)
    do i=1,gfile%fieldsize
     if(abs(data8(i))<nemsio_undef_grb) then
        if(data8(i) .gt.mymax) mymax=data8(i)
     endif
    enddo
!     write(0,*)'in writerecgrb4,max=',mymax,'nc=',nc,'nc1=',nc1,'imb=',ibms, &
!      'size(data)=',size(data),'size(lbms)=',size(grbmeta%lbms), &
!      grbmeta%lbms(1:15),data8(1:15)
!
!------------------------------------------------------------
! check precision -- for pressure now
!------------------------------------------------------------
    if ( grbmeta%jpds(5).eq.ione .and. grbmeta%jpds(6).eq.109 ) then
     grbmeta%jpds(22)=min(int(5-log10(mymax)),2)
    endif
!------------------------------------------------------------
! get data from putgb _w3d
!------------------------------------------------------------
!    allocate(data8(size(data)) )
!    data8=data
!    write(0,*)'in writerecgrb4,before putgb=',grbmeta%lbms(1:15)
    call putgb(gfile%flunit,grbmeta%jf,grbmeta%jpds,grbmeta%jgds, &
      grbmeta%lbms,data8,ios)
    deallocate(grbmeta%lbms)
    if(ios.ne.izero) then
       if ( present(iret))  then
          print *,'putgb_ios=',ios
         iret=ios
         return
       else
         call nemsio_stop
       endif
    endif
    if(present(iret)) iret=izero
  end subroutine nemsio_writerecgrb4
!------------------------------------------------------------------------------

  subroutine nemsio_writerecgrb8(gfile,jrec,data8,iret,idrt,itr,zhour)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    nemsio_writerecgrb8
!   prgmmr:
!
! abstract: read nemsio data by record number into a 2D 64bits array, 
!           using w3_d library to compile
!
! program history log:
!   2009-08-31  lueken - added subprogram doc block
!
!   input argument list:
!    gfile
!    jrec
!    data8
!    idrt
!    itr
!    zhour
!
!   output argument list:
!    iret
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    implicit none
    type(nemsio_gfile),intent(in)               :: gfile
    integer(i_kind),intent(in)                  :: jrec
    real(r_kind),intent(in)                     :: data8(:)
    integer(i_kind),optional,intent(out)        :: iret
    integer(i_kind),optional,intent(in)         :: idrt
    integer(i_kind),optional,intent(in)         :: itr
    real(r_single),optional,intent(in)          :: zhour
    type(nemsio_grbmeta)         :: grbmeta
    integer(i_kind)              :: i
    integer(i_kind)              :: ios,ibms
!---
    real(r_single)               :: mymax
!------------------------------------------------------------
! set up grib meta 
!------------------------------------------------------------
    if(present(iret)) iret=-4
!------------------------------------------------------------
! set up grib meta lbms
!------------------------------------------------------------
    ibms=izero
    if(any(abs(data8)>=nemsio_undef_grb))  ibms=ione
!
    if(present(idrt)) then
      call nemsio_setrqst(gfile,grbmeta,ios,jrec=jrec,idrt=idrt, &
           itr=itr,zhour=zhour,ibms=ibms)
    else
      call nemsio_setrqst(gfile,grbmeta,ios,jrec=jrec, &
           itr=itr,zhour=zhour,ibms=ibms)
    endif
    if (ios.ne.izero) then
       if ( present(iret))  then
         iret=ios
         return
       else
         call nemsio_stop
       endif
    endif
!
    grbmeta%lbms=.true.
    where(abs(data8)>=nemsio_undef_grb) grbmeta%lbms=.false.
    mymax=minval(data8)
    do i=1,gfile%fieldsize
     if(abs(data8(i))<nemsio_undef_grb) then
        if(data8(i) .gt.mymax) mymax=data8(i)
     endif
    enddo
!
!------------------------------------------------------------
! check precision -- for pressure now
!------------------------------------------------------------
    if ( grbmeta%jpds(5).eq.ione .and. grbmeta%jpds(6).eq.109 ) then
     grbmeta%jpds(22)=min(int(5-log10(mymax)),2)
    endif
!------------------------------------------------------------
! get data from putgb _w3d
!------------------------------------------------------------
    call putgb(gfile%flunit,grbmeta%jf,grbmeta%jpds,grbmeta%jgds, &
      grbmeta%lbms,data8,ios)
    deallocate(grbmeta%lbms)
    if(ios.ne.izero) then
       if ( present(iret))  then
          print *,'putgb_ios=',ios
         iret=ios
         return
       else
         call nemsio_stop
       endif
    endif
    if(present(iret)) iret=izero
  end subroutine nemsio_writerecgrb8
!------------------------------------------------------------------------------

  subroutine nemsio_writerecvw34(gfile,vname,vlevtyp,vlev,data,iret,idrt, &
             itr,zhour)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    nemsio_writerecvw34
!   prgmmr:
!
! abstract: read nemsio data by field name into a 2D 32bits array, 
!           using w3_4 library to compile
!
! program history log:
!   2009-08-31  lueken - added subprogram doc block
!
!   input argument list:
!    gfile
!    vname,vlevtyp
!    vlev
!    data
!    idrt
!    itr
!    zhour
!
!   output argument list:
!    iret
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    implicit none
    type(nemsio_gfile),intent(in)               :: gfile
    character*(*),intent(in)                    :: vname,vlevtyp
    integer(i_kind),intent(in)                  :: vlev
    real(r_single),intent(in)                   :: data(:)
    integer(i_kind),optional,intent(out)        :: iret
    integer(i_kind),optional,intent(in)         :: idrt
    integer(i_kind),optional,intent(in)         :: itr
    real(r_single),optional,intent(in)          :: zhour
    type(nemsio_grbmeta)         :: grbmeta
    integer(i_kind)              :: i
    integer(i_kind)              :: ios,w34,ibms
    real(r_single)               :: mymax
!------------------------------------------------------------
! set up grib meta 
!------------------------------------------------------------
    if(present(iret)) iret=-4
!------------------------------------------------------------
! set up grib meta lbms
!------------------------------------------------------------
    ibms=izero
    if(any(abs(data)>=nemsio_undef_grb))  ibms=ione
!
    w34=ione
    if(present(idrt)) then
      call nemsio_setrqst(gfile,grbmeta,ios,vname=vname, &
        vlevtyp=vlevtyp, vlev=vlev, w34=w34, idrt=idrt,  &
        itr=itr,zhour=zhour,ibms=ibms)
    else
      call nemsio_setrqst(gfile,grbmeta,ios,vname=vname, &
        vlevtyp=vlevtyp, vlev=vlev, w34=w34,itr=itr,     &
        zhour=zhour,ibms=ibms)
    endif
    if (ios.ne.izero) then
       if ( present(iret))  then
         iret=ios
         return
       else
         call nemsio_stop
       endif
    endif
!
    grbmeta%lbms=.true.
    where(abs(data)>=nemsio_undef_grb) grbmeta%lbms=.false.
    mymax=minval(data)
    do i=1,gfile%fieldsize
     if(abs(data(i))<nemsio_undef_grb) then
        if(data(i) .gt.mymax) mymax=data(i)
     endif
    enddo
!
!------------------------------------------------------------
! check precision -- for pressure now
!------------------------------------------------------------
    if ( grbmeta%jpds(5).eq.ione .and. grbmeta%jpds(6).eq.109 ) then
     grbmeta%jpds(22)=min(int(5-log10(mymax)),2)
    endif
!------------------------------------------------------------
! get data from putgb _w34
!------------------------------------------------------------
    call putgb(gfile%flunit,grbmeta%jf,grbmeta%jpds,grbmeta%jgds, &
      grbmeta%lbms,data,ios)
    deallocate(grbmeta%lbms)
    if(ios.ne.izero) then
       if ( present(iret))  then
          print *,'putgb_ios=',ios
         iret=ios
         return
       else
         call nemsio_stop
       endif
    endif
    if(present(iret)) iret=izero
  end subroutine nemsio_writerecvw34
!------------------------------------------------------------------------------

  subroutine nemsio_writerecvgrb4(gfile,vname,vlevtyp,vlev,data,iret,idrt, &
             itr,zhour)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    nemsio_writerecvgrb4
!   prgmmr:
!
! abstract: read nemsio data by field name into a 2D 32bits array, 
!           using w3_d library to compile
!
! program history log:
!   2009-08-31  lueken - added documentation block
!
!   input argument list:
!    gfile
!    vname,vlevtyp
!    vlev
!    data
!    idrt
!    itr
!    zhour
!
!   output argument list:
!    iret
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    implicit none
    type(nemsio_gfile),intent(in)               :: gfile
    character*(*),intent(in)                    :: vname,vlevtyp
    integer(i_kind),intent(in)                  :: vlev
    real(r_single),intent(in)                   :: data(:)
    integer(i_kind),optional,intent(out)        :: iret
    integer(i_kind),optional,intent(in)         :: idrt
    integer(i_kind),optional,intent(in)         :: itr
    real(r_single),optional,intent(in)          :: zhour
    real(r_kind),allocatable                    :: data8(:)
    type(nemsio_grbmeta)         :: grbmeta
    integer(i_kind)              :: i
    integer(i_kind)              :: ios,ibms
    real(r_single)               :: mymax
!------------------------------------------------------------
! set up grib meta 
!------------------------------------------------------------
    if(present(iret)) iret=-4
!------------------------------------------------------------
! set up grib meta lbms
!------------------------------------------------------------
    ibms=izero
    if(any(abs(data)>=nemsio_undef_grb))  ibms=ione
!
    if(present(idrt)) then
      call nemsio_setrqst(gfile,grbmeta,ios,vname=vname, &
        vlevtyp=vlevtyp, vlev=vlev, idrt=idrt,itr=itr,   &
        zhour=zhour,ibms=ibms)
    else
      call nemsio_setrqst(gfile,grbmeta,ios,vname=vname, &
        vlevtyp=vlevtyp, vlev=vlev,itr=itr,zhour=zhour,ibms=ibms)
    endif
    if (ios.ne.izero) then
       if ( present(iret))  then
         iret=ios
         return
       else
         call nemsio_stop
       endif
    endif
!
    grbmeta%lbms=.true.
    where(abs(data)>=nemsio_undef_grb) grbmeta%lbms=.false.
    mymax=minval(data)
    do i=1,gfile%fieldsize
     if(abs(data(i))<nemsio_undef_grb) then
       if(data(i) .gt.mymax) mymax=data(i)
     endif
    enddo
!
!------------------------------------------------------------
! check precision -- for pressure now
!------------------------------------------------------------
    if ( grbmeta%jpds(5).eq.ione .and. grbmeta%jpds(6).eq.109 ) then
     grbmeta%jpds(22)=min(int(5-log10(mymax)),2)
    endif
!------------------------------------------------------------
! get data from putgb _w3d
!------------------------------------------------------------
    allocate(data8(size(data)) )
    daTa8=data
    call putgb(gfile%flunit,grbmeta%jf,grbmeta%jpds,grbmeta%jgds, &
      grbmeta%lbms,data8,ios)
    deallocate(grbmeta%lbms)
    if(ios.ne.izero) then
       if ( present(iret))  then
          print *,'putgb_ios=',ios
         iret=ios
         return
       else
         call nemsio_stop
       endif
    endif
    if(present(iret)) iret=izero
  end subroutine nemsio_writerecvgrb4
!------------------------------------------------------------------------------

  subroutine nemsio_writerecvgrb8(gfile,vname,vlevtyp,vlev,data8,iret,idrt,itr, &
       zhour)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    nemsio_writerecvgrb8
!   prgmmr:
!
! abstract: read nemsio data by field name into a 2D 64bits array, 
!           using w3_d library to compile
!
! program history log:
!   2009-08-31  lueken - added subprogram doc block
!
!   input argument list:
!    gfile
!    vname,vlevtyp
!    vlev
!    data8
!    idrt
!    itr
!    zhour
!
!   output argument list:
!    iret
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block


    implicit none
    type(nemsio_gfile),intent(in)               :: gfile
    character*(*),intent(in)                    :: vname,vlevtyp
    integer(i_kind),intent(in)                  :: vlev
    real(r_kind),intent(in)                     :: data8(:)
    integer(i_kind),optional,intent(out)        :: iret
    integer(i_kind),optional,intent(in)         :: idrt
    integer(i_kind),optional,intent(in)         :: itr
    real(r_single),optional,intent(in)          :: zhour
    type(nemsio_grbmeta)         :: grbmeta
    integer(i_kind)              :: i
    integer(i_kind)              :: ios,ibms
    real(r_single)               :: mymax
!------------------------------------------------------------
! set up grib meta 
!------------------------------------------------------------
    if(present(iret)) iret=-4
!------------------------------------------------------------
! set up grib meta lbms
!------------------------------------------------------------
    ibms=izero
    if(any(abs(data8)>=nemsio_undef_grb))  ibms=ione
!
    if(present(idrt)) then
      call nemsio_setrqst(gfile,grbmeta,ios,vname=vname, &
        vlevtyp=vlevtyp, vlev=vlev, idrt=idrt,itr=itr,   &
        zhour=zhour,ibms=ibms)
    else
      call nemsio_setrqst(gfile,grbmeta,ios,vname=vname, &
        vlevtyp=vlevtyp, vlev=vlev,itr=itr,zhour=zhour,ibms=ibms)
    endif
    if (ios.ne.izero) then
       if ( present(iret))  then
         iret=ios
         return
       else
         call nemsio_stop
       endif
    endif
!
    grbmeta%lbms=.true.
    where(abs(data8)>=nemsio_undef_grb) grbmeta%lbms=.false.
    mymax=minval(data8)
    do i=1,gfile%fieldsize
     if(abs(data8(i))<nemsio_undef_grb) then
        if(data8(i) .gt.mymax) mymax=data8(i)
     endif
    enddo
!
!------------------------------------------------------------
! check precision -- for pressure now
!------------------------------------------------------------
    if ( grbmeta%jpds(5).eq.ione .and. grbmeta%jpds(6).eq.109 ) then
     grbmeta%jpds(22)=min(int(5-log10(mymax)),2)
    endif
!------------------------------------------------------------
! get data from putgb _w3d
!------------------------------------------------------------
    call putgb(gfile%flunit,grbmeta%jf,grbmeta%jpds,grbmeta%jgds, &
      grbmeta%lbms,data8,ios)
    deallocate(grbmeta%lbms)
    if(ios.ne.izero) then
       if ( present(iret))  then
          print *,'putgb_ios=',ios
         iret=ios
         return
       else
         call nemsio_stop
       endif
    endif
    if(present(iret)) iret=izero
  end subroutine nemsio_writerecvgrb8
!----------------------------------------------------------------------------

  subroutine nemsio_setrqst(gfile,grbmeta,iret,jrec,vname,vlevtyp,vlev,w34,idrt, &
                            itr,zhour,ibms)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    nemsio_setrqst
!   prgmmr:
!
! abstract: if given record number, find record name, lev typ, and levs or
!           record name,lev type and lev can be got from argument list.
!           with record name,lev typ and level, set up grib meta, jpds and
!           jgds
!
! program history log:
!   2009-08-31  lueken - added subprogram doc block
!
!   input argument list:
!    gfile
!    jrec
!    vname,vlevtyp
!    vlev
!    w34
!    idrt
!    itr
!    zhour
!    ibms
!
!   output argument list:
!    grbmeta
!    iret
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    implicit none
    type(nemsio_gfile),intent(in)                :: gfile
    type(nemsio_grbmeta),intent(out)             :: grbmeta
    integer(i_kind),optional,intent(in)          :: jrec
    character(*),optional,intent(in)             :: vname,vlevtyp
    integer(i_kind),optional,intent(in)          :: vlev
    integer(i_kind),intent(out)                  :: iret
    integer(i_kind),optional,intent(in)          :: w34
    integer(i_kind),optional,intent(in)          :: idrt
    integer(i_kind),optional,intent(in)          :: itr
    real(r_single),optional,intent(in)           :: zhour
    integer(i_kind),optional,intent(in)          :: ibms
    character(255)  :: name,levtyp
    integer(i_kind) :: icen,igrid,iptv,jbms,jftu,jp1,jp2,jtr,jna,jnm,ios
    integer(i_kind) :: lev,ktbl,krec,idrt_in
!------------------------------------------------------------
! with record number, find record name, level type and level
!------------------------------------------------------------
    iret=-5
    if ( present(jrec)) then
      if ( jrec.gt.izero .and. jrec.le.gfile%nrec) then
        name=gfile%recname(jrec)
        levtyp=gfile%reclevtyp(jrec)
        lev=gfile%reclev(jrec)
      else
        return
      endif
    elseif ( present(vname) .and. present(vlevtyp) .and. present(vlev)) then
      name=trim(vname)
      levtyp=trim(vlevtyp)
      lev=vlev
    else 
       return
    endif
!------------------------------------------------------------
! find index in grib table according to recname and reclevtyp
!------------------------------------------------------------
    call nemsio_grbtbl_search(trim(name),trim(levtyp),ktbl,krec,ios)
    if(ios.ne.izero) return
!*** lev: for special layer
!    if ( gribtable(ktbl)%item(krec)%leveltype .eq.'sfc' ) then
    if ( trim(gribtable(ktbl)%item(krec)%leveltype) .ne.'layer' .and. &
         trim(gribtable(ktbl)%item(krec)%leveltype) .ne.'mid layer' ) then
        lev=izero
    endif
!    print *,'in searchrst,jrec=',jrec,'name=',trim(name),'levtyp=',trim(levtyp),&
!      'lev=',lev,'gribtb levtype=',gribtable(ktbl)%item(krec)%leveltype
!------------------------------------------------------------
! for read, just need to set up jpds(05-07)
!------------------------------------------------------------
!--- read:set jpds5,6,7
!    if ( lowercase(gfile%gaction)(1:4).eq."read") then
    if ( equal_str_nocase(trim(gfile%gaction),"read") ) then
      grbmeta%jpds(05)=gribtable(ktbl)%item(krec)%g1param
      grbmeta%jpds(06)=gribtable(ktbl)%item(krec)%g1level
      grbmeta%jpds(07)=lev
      if ( grbmeta%jpds(06).eq.110 ) then
        grbmeta%jpds(07)=256*(lev-ione)+lev
      endif
      if (gribtable(ktbl)%item(krec)%g1lev.ne.izero) then
        grbmeta%jpds(07)=gribtable(ktbl)%item(krec)%g1lev
      endif
    else
!------------------------------------------------------------
! for write, need to set up jgds(1:25), jpds(01-20)
!------------------------------------------------------------
      if (present(idrt)) then
        idrt_in = idrt
      else
!*** gfile idrt
        idrt_in=gfile%idrt
      endif
!*** for itr
      jftu=ione
      jtr=10
      jp1=gfile%nfhour
      jp2=izero
      if(present(itr) ) then
        jtr=itr
        if(itr==3.or.itr==2.or.itr==4) then   !avg
           if(present(zhour)) then
             jp1=nint(zhour)
             jp2=gfile%nfhour
           else
             print *,'ERROR in nemsio gribfile,itr=',itr,'need to set zhour'
           endif
        endif
      endif 
      jbms=izero
      if(present(ibms)) jbms=ibms
!
      icen=7
!
      if ( present(w34) ) then
        call nemsio_makglgds(gfile,idrt_in,igrid,grbmeta%jgds,ios,w34)
      else
        call nemsio_makglgds(gfile,idrt_in,igrid,grbmeta%jgds,ios)
!        write(0,*)'after nemsio_makglgds,idrt=',idrt_in,'ios=',ios,'igrid=',igrid, &
!          'jbms=',jbms
      endif
      if(ios.ne.izero) return
      iptv=gribtable(ktbl)%iptv
      jna=izero
      jnm=izero
      call nemsio_makglpds(gfile,iptv,icen,igrid,jbms,&
           jftu,jp1,jp2,jtr,jna,jnm,ktbl,krec,lev,grbmeta%jpds,ios)
!        write(0,*)'after nemsio_makglpds,jpds=',grbmeta%jpds(1:25),'ios=',ios,  &
!           'lev=',lev
      if(ios.ne.izero) return
    endif
!------------------------------------------------------------
! set up grib meta lbms
!------------------------------------------------------------
    grbmeta%jf=gfile%fieldsize
    allocate(grbmeta%lbms(grbmeta%jf))
    iret=izero
  end subroutine nemsio_setrqst    
!------------------------------------------------------------------------------

  subroutine nemsio_getrechead(gfile,jrec,name,levtyp,lev,iret)
!$$$  subprogram documentation block
!                .      .    .                                      .
! subprogram:    nemsio_getrechead
!   prgmmr:
!
! abstract: given record number, return users record name, lev typ, and levs
!
! program history log:
!   2009-08-31  lueken - added subprogram doc block
!
!   input argument list:
!    gfile
!    jrec
!    name
!    levtyp
!  
!   output argument list:
!    name
!    levtyp
!    lev
!    iret
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
    implicit none
    type(nemsio_gfile),intent(in)                :: gfile
    integer(i_kind),intent(in)                   :: jrec
    character(*),intent(inout)                   :: name
    character(*),optional,intent(inout)          :: levtyp
    integer(i_kind),optional,intent(out)         :: lev
    integer(i_kind),optional,intent(out)         :: iret
! - - - - - - - - - - - - - -  - - - - - - - -  - - - - - - - - - - - - - - - -
    if( present(iret)) iret=-5
    if ( jrec.gt.izero .or. jrec.le.gfile%nrec) then
      if(gfile%nmeta>2) then
        name=gfile%recname(jrec)
      else
        print *,'WRONG: recname is not specified in meta data!'
        return
      endif
      if(present(levtyp).and.gfile%nmeta>3) then
        levtyp=gfile%reclevtyp(jrec)
      endif
      if(present(lev).and.gfile%nmeta>4) then
        lev=gfile%reclev(jrec)
      endif
      if(present(iret)) iret=izero
!      print *,'in getrechead, nrec=',gfile%nrec,'name=',name,'levtyp=',levtyp,'lev=',lev
      return
    else
      if ( present(iret))  then
       print *,'WRONG: jrec is either less than 1 or greater than gfile%nrec'
       return
      else
        call nemsio_stop
      endif
    endif
  end subroutine nemsio_getrechead
!------------------------------------------------------------------------------

  subroutine nemsio_makglgds(gfile,idrt,igrid,kgds,iret,w34)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    nemsio_makglgds
!   prgmmr:
!
! abstract: set up gds for grib meta
!
! program history log:
!   2009-08-31  lueken - added subprogram doc block
!
!   input argument list:
!    gfile
!    idrt
!    w34
!
!   output argument list:
!    iret
!    igrid,kgds
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    implicit none
    type(nemsio_gfile),intent(in)      :: gfile
    integer(i_kind),intent(out)        :: iret
    integer(i_kind),intent(in)         :: idrt
    integer(i_kind),optional,intent(in):: w34
    integer(i_kind),intent(out)        :: igrid,kgds(200)
    real(r_kind)   :: slat8(gfile%dimy)
    real(r_single) :: slat4(gfile%dimy)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    iret=-5
    igrid=255
    if(idrt.eq.izero.and.gfile%dimx.eq.144.and.gfile%dimy.eq.73) igrid=2
    if(idrt.eq.izero.and.gfile%dimx.eq.360.and.gfile%dimy.eq.181) igrid=3
    if(idrt.eq.izero.and.gfile%dimx.eq.720.and.gfile%dimy.eq.361) igrid=4
    if(idrt.eq.4.and.gfile%dimx.eq.192.and.gfile%dimy.eq.94) igrid=98
    if(idrt.eq.4.and.gfile%dimx.eq.384.and.gfile%dimy.eq.192) igrid=126
    if(idrt.eq.4.and.gfile%dimx.eq.512.and.gfile%dimy.eq.256) igrid=170
    if(idrt.eq.4.and.gfile%dimx.eq.768.and.gfile%dimy.eq.384) igrid=127
!    write(0,*)'in nemsio_makdglgds,idrt=',idrt,'dimx=',gfile%dimx,'dimy=',gfile%dimy
    kgds(1)=modulo(idrt,256)
    kgds(2)=gfile%dimx
    kgds(3)=gfile%dimy
    select case(idrt)
    case(0)
      kgds(4)=90000
    case(4)
!------------------------------------------------------------
! call different split for w3_4 lib and w3_d lib
!------------------------------------------------------------
      if (present (w34)) then
        call splat(idrt,gfile%dimy,slat4)
        kgds(4)=nint(180000./acos(-one)*asin(slat4(ione)))
      else
        call splat(idrt,gfile%dimy,slat8)
        kgds(4)=nint(180000./acos(-one)*asin(slat8(ione)))
      endif
    case(256)
      kgds(4)=90000-nint(half*180000./gfile%dimy)
    end select
    kgds(5)=izero
    kgds(6)=128
    kgds(7)=-kgds(4)
    kgds(8)=-nint(360000./gfile%dimx)
    kgds(9)=-kgds(8)
    select case(idrt)
    case(0)
      kgds(10)=nint(180000./(gfile%dimy-ione))
    case(4)
      kgds(10)=gfile%dimy/2
    case(256)
      kgds(10)=nint(180000./gfile%dimy)
    end select
    kgds(11)=izero
    kgds(12)=izero
    kgds(13:18)=-ione
    kgds(19)=izero
    kgds(20)=255
    kgds(21:)=-ione
    iret=izero
  end subroutine nemsio_makglgds
!------------------------------------------------------------------------------

  subroutine nemsio_makglpds(gfile,iptv,icen,igrid,ibms,&
                    iftu,ip1,ip2,itr,ina,inm,ktbl,krec,lev,kpds,iret)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    nemsio_makglpds
!   prgmmr:
!
! abstract: set up gps for grib meta
!
! program history log:
!   2009-08-31  lueken - added subprogram doc block
!
!   input argument list:
!    gfile
!    iptv,icen,igrid,ibms
!    iftu,ip1,ip2,itr,ina,inm,ktbl,krec,lev
!
!   output argument list:
!    kpds
!    iret
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    implicit none
    type(nemsio_gfile),intent(in)  :: gfile
    integer(i_kind),intent(in)     :: iptv,icen,igrid,ibms
    integer(i_kind),intent(in)     :: iftu,ip1,ip2,itr,ina,inm,ktbl,krec,lev
   integer(i_kind),intent(out)     :: kpds(200)
   integer(i_kind),intent(out)     :: iret
   integer(i_kind) :: igen,icen2
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    iret=-5
!
!get igen icen2 
    call nemsio_getheadvar(gfile,'igen',igen,iret)
    if (iret.ne.izero ) then
      if(trim(gfile%modelname)=='GFS') igen=82
    else
      print *,'ERROR: please specify model generating flag'
      return
    endif
    call nemsio_getheadvar(gfile,'icen2',icen2,iret)
    if (iret.ne.izero ) then
      if(trim(gfile%modelname).eq.'GFS') then
        icen2=izero
      else
        print *,'ERROR: please specify subcenter id,modelname=',gfile%modelname
        return
      endif
    endif
!
    kpds(01)=icen
    kpds(02)=igen
    kpds(03)=igrid
    kpds(04)=128+64*ibms
    kpds(05)=gribtable(ktbl)%item(krec)%g1param
    kpds(06)=gribtable(ktbl)%item(krec)%g1level
    kpds(07)=lev
    if(gribtable(ktbl)%item(krec)%g1lev/=izero)then
      kpds(07)=gribtable(ktbl)%item(krec)%g1lev
    endif
!*** deal with dpres 
    if ( kpds(06).eq.110 ) then
    kpds(07)=256*(lev-ione)+lev
    endif
!***
    kpds(08)=mod(gfile%idate(1)-ione,100)+ione
    kpds(09)=gfile%idate(2)
    kpds(10)=gfile%idate(3)
    kpds(11)=gfile%idate(4)
    kpds(12)=izero
    kpds(13)=iftu
    kpds(14)=ip1
    kpds(15)=ip2
    kpds(16)=itr
    kpds(17)=ina
    kpds(18)=ione
    kpds(19)=iptv
    kpds(20)=inm
    kpds(21)=(gfile%idate(1)-ione)/100+ione
    kpds(22)=gribtable(ktbl)%item(krec)%precision
    kpds(23)=icen2
    kpds(24)=izero
    kpds(25)=izero
    kpds(26:)=-ione
    iret=izero
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine nemsio_makglpds
!------------------------------------------------------------------------------

  subroutine nemsio_grbtbl_search(vname,vlevtyp,ktbl,krec,iret)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    nemsio_grbtbl_search
!   prgmmr:
!
! abstract: given record name, levtyp and index number in grib table
!
! program history log:
!   2009-08-31  lueken - added subprogram doc block
!
!   input argument list:
!    vname,vlevtyp
!
!   output argument list:
!    ktbl,krec
!    iret
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    implicit none
    character(*),intent(in)       :: vname,vlevtyp
    integer(i_kind),intent(out)   :: ktbl,krec
    integer(i_kind),intent(out)   :: iret
    integer(i_kind)               :: i,j,nlen,nlen1
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    iret=-5
    nlen=len(trim(vname))
    nlen1=len(trim(vlevtyp))
    ktbl=izero
    krec=izero
!    write(0,*)'vname=',vname,'vlevtyp=',vlevtyp,'nlen=',nlen,'nlen1=',nlen1
    do j=1,size(gribtable)
    do i=1,size(gribtable(j)%item)
      if(equal_str_nocase(trim(vname),trim(gribtable(j)%item(i)%shortname)) .and. &
        equal_str_nocase(trim(vlevtyp),trim(gribtable(j)%item(i)%leveltype)) )then
        ktbl=j
        krec=i
        iret=izero
        exit
      endif
    enddo 
    enddo 
!    write(0,*)'in grbtbl_search,krec=',krec,'ktbl=',ktbl
  end subroutine nemsio_grbtbl_search
!------------------------------------------------------------------------------

  subroutine nemsio_chkgfary(gfile,iret)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    nemsio_chkgfary
!   pgrmmr:
!
! abstract: check if arrays in gfile is allocated and with right size
!
! program history log:
!   2009-08-31  lueken - added subprogram doc block
!
!   input argument list:
!    gfile
!
!   output argument list:
!    gfile
!    iret
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    implicit none
    type(nemsio_gfile),intent(inout)         :: gfile
    integer(i_kind),intent(out)              :: iret
    integer(i_kind)   :: ios
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    iret=-2
    if ( gfile%dimx .eq. nemsio_intfill .or. gfile%dimy .eq. nemsio_intfill &
        .or. gfile%dimz .eq. nemsio_intfill .or. gfile%nrec .eq. nemsio_intfill ) then
        print *,'WRONG: dimx,dimy,dimz and nrec  must be defined!'
        return
    endif
    if(gfile%nmeta>5) then
      if (.not. allocated(gfile%vcoord) .or. size(gfile%vcoord).ne. &
       (gfile%dimz+1)*3*2 ) then
       call nemsio_almeta1(gfile,ios)
       if (ios .ne. izero) return
      endif
    endif
    if(gfile%nmeta>6) then
    if (.not.allocated(gfile%lat) .or. size(gfile%lat).ne.gfile%fieldsize .or.&
        .not.allocated(gfile%lon) .or. size(gfile%lon).ne.gfile%fieldsize .or.&
        .not.allocated(gfile%dx) .or. size(gfile%dx).ne.gfile%fieldsize .or.&
        .not.allocated(gfile%dy) .or. size(gfile%dy).ne.gfile%fieldsize) then
        call nemsio_almeta2(gfile,ios)
        if (ios .ne. izero) return
    endif
    endif
    if(gfile%nmeta>10) then
      if(gfile%ntrac==nemsio_intfill) then
        print *,'WRONG: ntrac is not defined!'
        return
      endif
      if (.not.allocated(gfile%Cpi) .or. size(gfile%Cpi).ne.gfile%ntrac+ione .or. &
        .not.allocated(gfile%Ri) .or. size(gfile%Ri).ne.gfile%ntrac+ione ) then
        call nemsio_almeta3(gfile,ios)
        if (ios .ne. izero) return
      endif
    endif

    if(gfile%nmeta>2) then
    if (allocated(gfile%recname) .and. size(gfile%recname).eq.gfile%nrec)&
    then
        if (allocated(gfile%reclevtyp) .and. size(gfile%reclevtyp) &
        .eq.gfile%nrec) then
           if (allocated(gfile%reclev) .and. size(gfile%reclev).eq. &
             gfile%nrec) then
               iret=izero
               return
           endif
         endif
   endif
   call  nemsio_almeta4(gfile,ios)
   if (ios .ne. izero) return
   endif
   iret=izero
  end subroutine nemsio_chkgfary
!------------------------------------------------------------------------------

  subroutine nemsio_almeta(gfile,iret)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    nemsio_almeta
!   prgmmr:
!
! abstract: allocate all the arrays in gfile
!
! program history log:
!   2009-08-31  lueken - added subprogram doc block
!
!   input argument list:
!    gfile
!
!   output argument list:
!    gfile
!    iret
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
    implicit none
    type(nemsio_gfile),intent(inout)  :: gfile 
    integer(i_kind),intent(out)       :: iret
    integer(i_kind) ::dimvcoord1
    integer(i_kind) ::dimrecname,dimreclevtyp,dimreclev
    integer(i_kind) ::dimfield
    integer(i_kind) ::dimcpr
    integer(i_kind) ::iret1,iret2,iret3,iret4
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    iret=izero
    dimvcoord1=gfile%dimz+ione
    dimrecname=gfile%nrec
    dimreclevtyp=gfile%nrec
    dimreclev=gfile%nrec
    dimfield=gfile%fieldsize
    dimcpr=gfile%ntrac+ione
    if(allocated(gfile%recname)) deallocate(gfile%recname)
    if(allocated(gfile%reclevtyp)) deallocate(gfile%reclevtyp)
    if(allocated(gfile%reclev)) deallocate(gfile%reclev)
    if(allocated(gfile%vcoord)) deallocate(gfile%vcoord)
    if(allocated(gfile%lat)) deallocate(gfile%lat)
    if(allocated(gfile%lon)) deallocate(gfile%lon)
    if(allocated(gfile%dx)) deallocate(gfile%dx)
    if(allocated(gfile%dy)) deallocate(gfile%dy)
    if(allocated(gfile%Cpi)) deallocate(gfile%Cpi)
    if(allocated(gfile%Ri)) deallocate(gfile%Ri)
    if(gfile%nmeta>2)then
      allocate(gfile%recname(dimrecname),  gfile%reclevtyp(dimreclevtyp), &
             gfile%reclev(dimreclev), &
             stat=iret1)
      if(iret1.eq.izero) then
      gfile%reclev=nemsio_intfill
      gfile%recname=' '
      gfile%reclevtyp=' '
      endif
      iret=iret+abs(iret1)
    endif
    if(gfile%nmeta>5)then
      allocate(gfile%vcoord(dimvcoord1,3,2) ,stat=iret2) 
      if(iret3.eq.izero) then
      gfile%vcoord=nemsio_realfill
      endif
      iret=iret+abs(iret2)
    endif
    if(gfile%nmeta>6)then
      allocate(gfile%lat(dimfield), gfile%lon(dimfield), &
             gfile%dx(dimfield), gfile%dy(dimfield) ,stat=iret3)
      if(iret3.eq.izero) then
      gfile%lat=nemsio_realfill
      gfile%lon=nemsio_realfill
      gfile%dx=nemsio_realfill
      gfile%dy=nemsio_realfill
      endif
      iret=iret+abs(iret3)
    endif
    if(gfile%nmeta>10)then
      allocate(gfile%Cpi(dimcpr), gfile%Ri(dimcpr), stat=iret4)
      if(iret4.eq.izero) then
      gfile%Cpi=nemsio_realfill
      gfile%Ri=nemsio_realfill
      endif
      iret=iret+abs(iret4)
    endif

!    print *,'iret1=',iret1,'iret2=',iret2,'dimx=',gfile%dimx,'dimy=',gfile%dimy,'nframe=',gfile%nframe
    if(iret.ne.izero) iret=-6
  end subroutine nemsio_almeta
!------------------------------------------------------------------------------

  subroutine nemsio_alextrameta(gfile,iret)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    nemsio_alextrameta
!   prgmmr:
!
! abstract: allocate all the arrays in gfile
!
! program history log:
!   2009-08-31  lueken - added subprogram doc block
!
!   input argument list:
!    gfile
!
!   output argument list:
!    gfile
!    iret
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    implicit none
    type(nemsio_gfile),intent(inout)  :: gfile
    integer(i_kind),intent(out)       :: iret
    integer(i_kind) ::iret1
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    iret=-6
    if(gfile%extrameta) then
!      print *,'nmetavari=',gfile%nmetavari,'nmetavarr=',gfile%nmetavarr, &
!              'nmetavarl=',gfile%nmetavarl,'nmetavarc=',gfile%nmetavarc, &
!              'nmetaaryi=',gfile%nmetaaryi,'nmetaaryr=',gfile%nmetaaryi, &
!              'nmetaaryl=',gfile%nmetaaryl,'nmetaaryc=',gfile%nmetaaryc
      if(gfile%nmetavari.gt.izero) then
         if(allocated(gfile%variname)) deallocate(gfile%variname)
         if(allocated(gfile%varival)) deallocate(gfile%varival)
         allocate(gfile%variname(gfile%nmetavari), &
                  gfile%varival(gfile%nmetavari), stat=iret1 )
         if(iret1.ne.izero) return
      endif
      if(gfile%nmetavarr.gt.izero) then
         if(allocated(gfile%varrname)) deallocate(gfile%varrname)
         if(allocated(gfile%varrval)) deallocate(gfile%varrval)
         allocate(gfile%varrname(gfile%nmetavarr), &
                  gfile%varrval(gfile%nmetavarr), stat=iret1 )
         if(iret1.ne.izero) return
      endif
      if(gfile%nmetavarl.gt.izero) then
         if(allocated(gfile%varlname)) deallocate(gfile%varlname)
         if(allocated(gfile%varlval)) deallocate(gfile%varlval)
         allocate(gfile%varlname(gfile%nmetavarl), &
                  gfile%varlval(gfile%nmetavarl), stat=iret1 )
         if(iret1.ne.izero) return
      endif
      if(gfile%nmetavarc.gt.izero) then
         if(allocated(gfile%varcname)) deallocate(gfile%varcname)
         if(allocated(gfile%varcval)) deallocate(gfile%varcval)
         allocate(gfile%varcname(gfile%nmetavarc), &
                  gfile%varcval(gfile%nmetavarc), stat=iret1 )
         if(iret1.ne.izero) return
      endif
      if(gfile%nmetaaryi.gt.izero) then
         if(allocated(gfile%aryiname)) deallocate(gfile%aryiname)
         if(allocated(gfile%aryilen)) deallocate(gfile%aryilen)
         if(allocated(gfile%aryival)) deallocate(gfile%aryival)
         allocate(gfile%aryiname(gfile%nmetaaryi), &
                  gfile%aryilen(gfile%nmetaaryi), stat=iret1 )
         if(iret1.ne.izero) return
      endif
      if(gfile%nmetaaryr.gt.izero) then
         if(allocated(gfile%aryrname)) deallocate(gfile%aryrname)
         if(allocated(gfile%aryrlen)) deallocate(gfile%aryrlen)
         if(allocated(gfile%aryrval)) deallocate(gfile%aryrval)
         allocate(gfile%aryrname(gfile%nmetaaryr), &
                  gfile%aryrlen(gfile%nmetaaryr), stat=iret1 )
         if(iret1.ne.izero) return
      endif
      if(gfile%nmetaaryl.gt.izero) then
         if(allocated(gfile%arylname)) deallocate(gfile%arylname)
         if(allocated(gfile%aryllen)) deallocate(gfile%aryllen)
         if(allocated(gfile%arylval)) deallocate(gfile%arylval)
         allocate(gfile%arylname(gfile%nmetaaryl), &
                  gfile%aryllen(gfile%nmetaaryl), stat=iret1 )
         if(iret1.ne.izero) return
      endif
      if(gfile%nmetaaryc.gt.izero) then
         if(allocated(gfile%arycname)) deallocate(gfile%arycname)
         if(allocated(gfile%aryclen)) deallocate(gfile%aryclen)
         if(allocated(gfile%arycval)) deallocate(gfile%arycval)
         allocate(gfile%arycname(gfile%nmetaaryc), &
                  gfile%aryclen(gfile%nmetaaryc), stat=iret1 )
         if(iret1.ne.izero) return
      endif
    endif

    iret=izero
!    print *,'end of alextrameta'
  end subroutine nemsio_alextrameta
!------------------------------------------------------------------------------

  subroutine nemsio_almeta1(gfile,iret)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    nemsio_almeta1
!   prgmmr:
!
! abstract: allocate vcoord in gfile
!
! program history log:
!   2009-08-31  lueken - added subprogram doc block
!
!   input argument list:
!    gfile
!
!   output argument list:
!    gfile
!    iret
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    implicit none
    type(nemsio_gfile),intent(inout)  :: gfile
    integer(i_kind),intent(out)       :: iret
    integer(i_kind) :: dimvcoord1
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    dimvcoord1=gfile%dimz+ione
    if(allocated(gfile%vcoord)) deallocate(gfile%vcoord)
    allocate(gfile%vcoord(dimvcoord1,3,2), stat=iret)
    if(iret.eq.izero) then
      gfile%vcoord=nemsio_realfill
    endif
    if(iret.ne.izero) iret=-6
  end subroutine nemsio_almeta1
!------------------------------------------------------------------------------

  subroutine nemsio_almeta2(gfile,iret)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    nemsio_almeta2
!   prgmmr:
!
! abstract: allocate lat1d in gfile
!
! program history log:
!   2009-08-31  lueken - added subprogram doc block
!
!   input argument list:
!    gfile
!
!   output argument list:
!    gfile
!    iret
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    implicit none
    type(nemsio_gfile),intent(inout)  :: gfile
    integer(i_kind),intent(out)       :: iret
    integer(i_kind) :: dimlat
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    dimlat=gfile%fieldsize
    if(allocated(gfile%lat)) deallocate(gfile%lat)
    if(allocated(gfile%lon)) deallocate(gfile%lon)
    if(allocated(gfile%dx)) deallocate(gfile%dx)
    if(allocated(gfile%dy)) deallocate(gfile%dy)
    allocate(gfile%lat(dimlat),gfile%lon(dimlat), &
             gfile%dx(dimlat),gfile%dy(dimlat), stat=iret)
    if(iret.eq.izero) then
      gfile%lat=nemsio_realfill
      gfile%lon=nemsio_realfill
      gfile%dx=nemsio_realfill
      gfile%dy=nemsio_realfill
    endif
    if(iret.ne.izero) iret=-6
  end subroutine nemsio_almeta2
!------------------------------------------------------------------------------

  subroutine nemsio_almeta3(gfile,iret)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    nemsio_almeta3
!   prgmmr:
!
! abstract: allocate lon1d in gfile
!
! program history log:
!   2009-08-31  lueken - added subprogram doc block
!
!   input argument list:
!    gfile
!
!   output argument list:
!    gfile
!    iret
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    implicit none
    type(nemsio_gfile),intent(inout)  :: gfile
    integer(i_kind),intent(out)       :: iret
    integer(i_kind) :: dim1d
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    dim1d=gfile%ntrac+1
    if(allocated(gfile%Cpi)) deallocate(gfile%Cpi)
    if(allocated(gfile%Ri)) deallocate(gfile%Ri)
    allocate(gfile%Cpi(dim1d),gfile%Ri(dim1d),stat=iret)
    if(iret.eq.izero) then
       gfile%Cpi=nemsio_realfill
       gfile%Ri=nemsio_realfill
    endif
    if(iret.ne.izero) iret=-6
  end subroutine nemsio_almeta3
!------------------------------------------------------------------------------

  subroutine nemsio_almeta4(gfile,iret)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    nemsio_almeta4
!   prgmmr:
!
! abstract: allocate recnam, reclvevtyp, and reclev in gfile
!
! program history log:
!   2009-08-31  lueken - added subprogram doc block
!
!   input argument list:
!    gfile
!
!   output argument list:
!    gfile
!    iret
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    implicit none
    type(nemsio_gfile),intent(inout)  :: gfile
    integer(i_kind),intent(out)       :: iret
    integer(i_kind) :: dimrecname,dimreclevtyp,dimreclev
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if(gfile%nrec<izero) then
      print *,'WRONG: Please set nrec, it is ',gfile%nrec,' now!'
      iret=-6
      return
    endif
    dimrecname=gfile%nrec
    dimreclevtyp=gfile%nrec
    dimreclev=gfile%nrec
    if(allocated(gfile%recname)) deallocate(gfile%recname)
    if(allocated(gfile%reclevtyp)) deallocate(gfile%reclevtyp)
    if(allocated(gfile%reclev)) deallocate(gfile%reclev)
    allocate(gfile%recname(dimrecname),  gfile%reclevtyp(dimreclevtyp), &
             gfile%reclev(dimreclev), stat=iret)
!    print *,'allocate recname,iert=',iret,'dim=',gfile%nrec
    if(iret.eq.izero) then
      gfile%reclev=nemsio_intfill
      gfile%recname=' '
      gfile%reclevtyp=' '
    endif
    if(iret.ne.izero) iret=-6
  end subroutine nemsio_almeta4
!------------------------------------------------------------------------------

  subroutine nemsio_axmeta(gfile,iret)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    nemsio_axmeta
!   prgmmr:
!
! abstract: empty gfile variables and decallocate arrays in gfile
!
! program history log:
!   2009-08-31  lueken - added subprogram doc block
!
!   input argument list:
!    gfile
!
!   output argument list:
!    gfile
!    iret
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    implicit none
    type(nemsio_gfile),intent(inout)      :: gfile
    integer(i_kind),intent(out)           :: iret
    integer(i_kind)                       :: ierr
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    iret=-6
    gfile%gtype=' '
    gfile%gdatatype=' '
    gfile%modelname=' '
    gfile%version=nemsio_intfill
    gfile%nmeta=nemsio_intfill
    gfile%lmeta=nemsio_intfill
    gfile%nrec=nemsio_intfill
    gfile%idate(1:7)=nemsio_intfill
    gfile%nfday=nemsio_intfill
    gfile%nfhour=nemsio_intfill
    gfile%nfminute=nemsio_intfill
    gfile%nfsecondn=nemsio_intfill
    gfile%nfsecondd=nemsio_intfill
    gfile%dimx=nemsio_intfill
    gfile%dimy=nemsio_intfill
    gfile%dimz=nemsio_intfill
    gfile%nframe=nemsio_intfill
    gfile%nsoil=nemsio_intfill
    gfile%ntrac=nemsio_intfill
    gfile%jcap=nemsio_intfill
    gfile%ncldt=nemsio_intfill
    gfile%idvc=nemsio_intfill
    gfile%idsl=nemsio_intfill
    gfile%idvm=nemsio_intfill
    gfile%idrt=nemsio_intfill
    gfile%rlon_min=nemsio_realfill
    gfile%rlon_max=nemsio_realfill
    gfile%rlat_min=nemsio_realfill
    gfile%rlat_max=nemsio_realfill
    gfile%extrameta=nemsio_logicfill
    gfile%nmetavari=nemsio_intfill
    gfile%nmetavarr=nemsio_intfill
    gfile%nmetavarl=nemsio_intfill
    gfile%nmetavarc=nemsio_intfill
    gfile%nmetaaryi=nemsio_intfill
    gfile%nmetaaryr=nemsio_intfill
    gfile%nmetaaryl=nemsio_intfill
    gfile%nmetaaryc=nemsio_intfill
    gfile%tlmeta=nemsio_intfill
    gfile%tlmetalat=nemsio_intfill
    gfile%tlmetalon=nemsio_intfill
    gfile%tlmetadx=nemsio_intfill
    gfile%tlmetady=nemsio_intfill

    if(gfile%nmeta>2) then
    if(allocated(gfile%recname)) deallocate(gfile%recname,stat=ierr)
    if(allocated(gfile%reclevtyp)) deallocate(gfile%reclevtyp,stat=ierr)
    if(allocated(gfile%reclev)) deallocate(gfile%reclev,stat=ierr)
    endif

    if(gfile%nmeta>5) then
    if(allocated(gfile%vcoord)) deallocate(gfile%vcoord,stat=ierr)
    endif

    if(gfile%nmeta>6) then
    if(allocated(gfile%lat)) deallocate(gfile%lat,stat=ierr)
    if(allocated(gfile%lon)) deallocate(gfile%lon,stat=ierr)
    if(allocated(gfile%dx)) deallocate(gfile%dx,stat=ierr)
    if(allocated(gfile%dy)) deallocate(gfile%dy,stat=ierr)
    endif

    if(gfile%nmeta>10) then
    if(allocated(gfile%Cpi)) deallocate(gfile%Cpi,stat=ierr)
    if(allocated(gfile%Ri)) deallocate(gfile%Ri,stat=ierr)
    endif
!
    gfile%mbuf=izero
    gfile%nnum=izero
    gfile%nlen=izero
    gfile%mnum=izero
    if(allocated(gfile%cbuf)) deallocate(gfile%cbuf)
    if(allocated(gfile%headvariname)) deallocate(gfile%headvariname,stat=ierr)
    if(allocated(gfile%headvarrname)) deallocate(gfile%headvarrname,stat=ierr)
    if(allocated(gfile%headvarlname)) deallocate(gfile%headvarlname,stat=ierr)
    if(allocated(gfile%headvarcname)) deallocate(gfile%headvarcname,stat=ierr)
    if(allocated(gfile%headvarival)) deallocate(gfile%headvarival,stat=ierr)
    if(allocated(gfile%headvarrval)) deallocate(gfile%headvarrval,stat=ierr)
    if(allocated(gfile%headvarlval)) deallocate(gfile%headvarlval,stat=ierr)
    if(allocated(gfile%headvarcval)) deallocate(gfile%headvarcval,stat=ierr)
    if(allocated(gfile%headaryiname)) deallocate(gfile%headaryiname,stat=ierr)
    if(allocated(gfile%headaryrname)) deallocate(gfile%headaryrname,stat=ierr)
    if(allocated(gfile%headarycname)) deallocate(gfile%headarycname,stat=ierr)
    if(allocated(gfile%headaryival)) deallocate(gfile%headaryival,stat=ierr)
    if(allocated(gfile%headaryrval)) deallocate(gfile%headaryrval,stat=ierr)
    if(allocated(gfile%headarycval)) deallocate(gfile%headarycval,stat=ierr)
!
    iret=izero
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine nemsio_axmeta
!------------------------------------------------------------------------------

  subroutine nemsio_setfhead(gfile,iret)
!$$$  subprogram documentation block
!                .      .    .                                        .
! subprogram:    nemsio_setfhead
!   prgmmr:
!
! abstract: required file header (default)
!
! program history log:
!   2009-08-31  lueken - added subprogram doc block
!
!   input argument list:
!    gfile
!
!   output argument list:
!    gfile
!    iret
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    implicit none
    type(nemsio_gfile),intent(inout)     :: gfile
    integer(i_kind),intent(out)          :: iret
    integer(i_kind)                      :: i,j,k
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    iret=-17
    gfile%headvarinum=29
    gfile%headvarrnum=4
    gfile%headvarlnum=ione
    gfile%headvarcnum=3
!
    if(gfile%nmeta>4) then
      gfile%headaryinum=2
    else
      gfile%headaryinum=ione
    endif
!
    if(gfile%nmeta>11) then
      gfile%headaryrnum=7
    elseif(gfile%nmeta>10) then
      gfile%headaryrnum=6
    elseif(gfile%nmeta>9) then
      gfile%headaryrnum=5
    elseif(gfile%nmeta>8) then
      gfile%headaryrnum=4
    elseif(gfile%nmeta>7) then
      gfile%headaryrnum=3
    elseif(gfile%nmeta>6) then
      gfile%headaryrnum=2
    elseif(gfile%nmeta>5) then
      gfile%headaryrnum=ione
    endif
!
    if(gfile%nmeta>3) then
      gfile%headarycnum=2
    elseif(gfile%nmeta>2) then
      gfile%headarycnum=ione
    else
      gfile%headarycnum=izero
    endif
!
!    print*,'in setfhead,before headvariname,headvarival'
    allocate(gfile%headvariname(gfile%headvarinum),gfile%headvarival(gfile%headvarinum) )
    gfile%headvariname(1)='version'
    gfile%headvarival(1)=gfile%version
    gfile%headvariname(2)='nmeta'
    gfile%headvarival(2)=gfile%nmeta
    gfile%headvariname(3)='lmeta'
    gfile%headvarival(3)=gfile%lmeta
    gfile%headvariname(4)='nrec'
    gfile%headvarival(4)=gfile%nrec
    gfile%headvariname(5)='nfday'
    gfile%headvarival(5)=gfile%nfday
    gfile%headvariname(6)='nfhour'
    gfile%headvarival(6)=gfile%nfhour
    gfile%headvariname(7)='nfminute'
    gfile%headvarival(7)=gfile%nfminute
    gfile%headvariname(8)='nfsecondn'
    gfile%headvarival(8)=gfile%nfsecondn
    gfile%headvariname(9)='nfsecondd'
    gfile%headvarival(9)=gfile%nfsecondd
    gfile%headvariname(10)='dimx'
    gfile%headvarival(10)=gfile%dimx
    gfile%headvariname(11)='dimy'
    gfile%headvarival(11)=gfile%dimy
    gfile%headvariname(12)='dimz'
    gfile%headvarival(12)=gfile%dimz
    gfile%headvariname(13)='nframe'
    gfile%headvarival(13)=gfile%nframe
    gfile%headvariname(14)='nsoil'
    gfile%headvarival(14)=gfile%nsoil
    gfile%headvariname(15)='ntrac'
    gfile%headvarival(15)=gfile%ntrac
    gfile%headvariname(16)='jcap'
    gfile%headvarival(16)=gfile%jcap
    gfile%headvariname(17)='ncldt'
    gfile%headvarival(17)=gfile%ncldt
    gfile%headvariname(18)='idvc'
    gfile%headvarival(18)=gfile%idvc
    gfile%headvariname(19)='idsl'
    gfile%headvarival(19)=gfile%idsl
    gfile%headvariname(20)='idvm'
    gfile%headvarival(20)=gfile%idvm
    gfile%headvariname(21)='idrt'
    gfile%headvarival(21)=gfile%idrt
    gfile%headvariname(22)='nmetavari'
    gfile%headvarival(22)=gfile%nmetavari
    gfile%headvariname(23)='nmetavarr'
    gfile%headvarival(23)=gfile%nmetavarr
    gfile%headvariname(24)='nmetavarl'
    gfile%headvarival(24)=gfile%nmetavarl
    gfile%headvariname(25)='nmetavarc'
    gfile%headvarival(25)=gfile%nmetavarc
    gfile%headvariname(26)='nmetaaryi'
    gfile%headvarival(26)=gfile%nmetaaryi
    gfile%headvariname(27)='nmetaaryr'
    gfile%headvarival(27)=gfile%nmetaaryr
    gfile%headvariname(28)='nmetaaryl'
    gfile%headvarival(28)=gfile%nmetaaryl
    gfile%headvariname(29)='nmetaaryc'
    gfile%headvarival(29)=gfile%nmetaaryc
!
    allocate(gfile%headvarrname(gfile%headvarrnum),gfile%headvarrval(gfile%headvarrnum) )
    gfile%headvarrname(1)='rlon_min'
    gfile%headvarrval(1)=gfile%rlon_min
    gfile%headvarrname(2)='rlon_max'
    gfile%headvarrval(2)=gfile%rlon_max
    gfile%headvarrname(3)='rlat_min'
    gfile%headvarrval(3)=gfile%rlat_min
    gfile%headvarrname(4)='rlat_min'
    gfile%headvarrval(4)=gfile%rlat_min
!
    allocate(gfile%headvarcname(gfile%headvarcnum),gfile%headvarcval(gfile%headvarcnum) )
    gfile%headvarcname(1)='gtype'
    gfile%headvarcval(1)=gfile%gtype
    gfile%headvarcname(2)='modelname'
    gfile%headvarcval(2)=gfile%modelname
    gfile%headvarcname(3)='gdatatype'
    gfile%headvarcval(3)=gfile%gdatatype
!head logic var
    allocate(gfile%headvarlname(gfile%headvarlnum),gfile%headvarlval(gfile%headvarlnum) )
    gfile%headvarlname(1)='extrameta'
    gfile%headvarlval(1)=gfile%extrameta
!
!--- gfile%head int ary
!    print *,'before setfhead, headaryi,nrec=',gfile%nrec,gfile%headaryinum
    allocate(gfile%headaryiname(gfile%headaryinum) )
    allocate(gfile%headaryival(max(size(gfile%reclev),7),gfile%headaryinum))
    gfile%headaryiname(1)='idate'
    gfile%headaryival(1:7,1)=gfile%idate(1:7)
    if(gfile%headaryinum>1) then
       gfile%headaryiname(2)='reclev'
       gfile%headaryival(:,2)=gfile%reclev(:)
    endif
!
!--- gfile%head real ary
    if(gfile%headaryrnum>izero) then
      if(.not.allocated(gfile%headaryrname)) allocate(gfile%headaryrname(gfile%headaryrnum) )
      if(.not.allocated(gfile%headaryrval)) &
        allocate(gfile%headaryrval(max(gfile%fieldsize,(gfile%dimz+ione)*6),gfile%headaryrnum))
      gfile%headaryrname(1)='vcoord'
      do j=1,2
      do i=1,3
       do k=1,gfile%dimz+ione
        gfile%headaryrval(k+((j-ione)*3+i-ione)*(gfile%dimz+ione),1)=gfile%vcoord(k,i,j)
       enddo
      enddo
      enddo
      if(gfile%headaryrnum>ione) then
        gfile%headaryrname(2)='lat'
        gfile%headaryrval(:,2)=gfile%lat
      endif
      if(gfile%headaryrnum>2) then
        gfile%headaryrname(3)='lon'
        gfile%headaryrval(:,3)=gfile%lon
      endif
      if(gfile%headaryrnum>3) then
        gfile%headaryrname(4)='dx'
        gfile%headaryrval(:,4)=gfile%dx
      endif
      if(gfile%headaryrnum>4) then
        gfile%headaryrname(5)='dy'
        gfile%headaryrval(:,5)=gfile%dy
      endif
      if(gfile%headaryrnum>5) then
        gfile%headaryrname(6)='cpi'
        gfile%headaryrval(1:size(gfile%cpi),6)=gfile%cpi
      endif
      if(gfile%headaryrnum>6) then
        gfile%headaryrname(7)='ri'
        gfile%headaryrval(1:size(gfile%ri),7)=gfile%ri
      endif
    endif
!
!--- gfile%head char var
!    print *,'before setfhead, headaryc,nrec=',gfile%nrec,gfile%headarycnum
    if(gfile%headarycnum >izero) then
      allocate(gfile%headarycname(gfile%headarycnum) )
      allocate(gfile%headarycval(size(gfile%recname),gfile%headarycnum))
      gfile%headarycname(1)='recname'
      gfile%headarycval(:,1)=gfile%recname
      if(gfile%headarycnum >ione) then
        gfile%headarycname(2)='reclevtyp'
        gfile%headarycval(:,2)=gfile%reclevtyp
      endif
    endif
!
    iret=izero
  end subroutine nemsio_setfhead
!------------------------------------------------------------------------------

  subroutine nemsio_setgrbtbl(iret)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    nemsio_setgrbtbl
!   prgmmr:
!
! abstract: set up grib table
!
! program history log:
!   2009-08-31  lueken - added subprogram doc block
!
!   input argument list:
!
!   output argument list:
!    iret
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    implicit none
    integer(i_kind),intent(out)  :: iret
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    iret=-7
    gribtable(1)%iptv=2
    gribtable(1)%item(1)=nemsio_grbtbl_item('hgt','sfc',1,0,7,1)
    gribtable(1)%item(2)=nemsio_grbtbl_item('pres','sfc',0,0,1,1)
    gribtable(1)%item(3)=nemsio_grbtbl_item('pres','mid layer',0,0,1,109)
    gribtable(1)%item(4)=nemsio_grbtbl_item('dpres','mid layer',2,0,1,110)
    gribtable(1)%item(5)=nemsio_grbtbl_item('tmp','mid layer',2,0,11,109)
    gribtable(1)%item(6)=nemsio_grbtbl_item('ugrd','mid layer',2,0,33,109)
    gribtable(1)%item(7)=nemsio_grbtbl_item('vgrd','mid layer',2,0,34,109)
    gribtable(1)%item(8)=nemsio_grbtbl_item('spfh','mid layer',7,0,51,109)
    gribtable(1)%item(9)=nemsio_grbtbl_item('o3mr','mid layer',9,0,154,109)
    gribtable(1)%item(10)=nemsio_grbtbl_item('clwmr','mid layer',7,0,153,109)
!
    gribtable(1)%item(11)=nemsio_grbtbl_item('vvel','mid layer',9,0,39,109)
    gribtable(1)%item(12)=nemsio_grbtbl_item('tmp','sfc',3,0,11,1)
    gribtable(1)%item(13)=nemsio_grbtbl_item('soilw','0-10 cm down',4,10,144,112)
    gribtable(1)%item(14)=nemsio_grbtbl_item('soilw','10-40 cm down',4,2600,144,112)
    gribtable(1)%item(15)=nemsio_grbtbl_item('soilw','40-100 cm down',4,10340,144,112)
    gribtable(1)%item(16)=nemsio_grbtbl_item('soilw','100-200 cm down',4,25800,144,112)
    gribtable(1)%item(17)=nemsio_grbtbl_item('tmp','0-10 cm down',3,10,11,112)
    gribtable(1)%item(18)=nemsio_grbtbl_item('tmp','10-40 cm down',3,2600,11,112)
    gribtable(1)%item(19)=nemsio_grbtbl_item('tmp','40-100 cm down',3,10340,11,112)
    gribtable(1)%item(20)=nemsio_grbtbl_item('tmp','100-200 cm down',3,25800,11,112)
!
    gribtable(1)%item(21)=nemsio_grbtbl_item('weasd','sfc',5,0,65,1)
    gribtable(1)%item(22)=nemsio_grbtbl_item('tg3','sfc',2,0,11,111)
    gribtable(1)%item(23)=nemsio_grbtbl_item('sfcr','sfc',4,0,83,1)
    gribtable(1)%item(24)=nemsio_grbtbl_item('tcdc','high cld lay',0,0,71,234)
    gribtable(1)%item(25)=nemsio_grbtbl_item('pres','high cld top',-1,0,1,233)
    gribtable(1)%item(26)=nemsio_grbtbl_item('pres','high cld bot',-1,0,1,232)
    gribtable(1)%item(27)=nemsio_grbtbl_item('tmp','high cld top',3,0,11,233)
    gribtable(1)%item(28)=nemsio_grbtbl_item('tcdc','mid cld lay',0,0,71,224)
    gribtable(1)%item(29)=nemsio_grbtbl_item('pres','mid cld top',-1,0,1,223)
    gribtable(1)%item(30)=nemsio_grbtbl_item('pres','mid cld bot',-1,0,1,222)
!
    gribtable(1)%item(31)=nemsio_grbtbl_item('tmp','mid cld top',3,0,11,223)
    gribtable(1)%item(32)=nemsio_grbtbl_item('tcdc','low cld lay',0,0,71,214)
    gribtable(1)%item(33)=nemsio_grbtbl_item('pres','low cld top',-1,0,1,213)
    gribtable(1)%item(34)=nemsio_grbtbl_item('pres','low cld bot',-1,0,1,212)
    gribtable(1)%item(35)=nemsio_grbtbl_item('tmp','low cld top',3,0,11,213)
    gribtable(1)%item(36)=nemsio_grbtbl_item('tcdc','atmos col',0,0,71,200)   !orog???
    gribtable(1)%item(37)=nemsio_grbtbl_item('tcdc','convect-cld laye',3,0,71,244)   !orog???
    gribtable(1)%item(38)=nemsio_grbtbl_item('pres','convect-cld bot',-1,0,1,242)
    gribtable(1)%item(39)=nemsio_grbtbl_item('pres','convect-cld top',-1,0,1,243)
    gribtable(1)%item(40)=nemsio_grbtbl_item('tcdc','bndary-layer cld',3,0,71,211)   !orog???
!
    gribtable(1)%item(41)=nemsio_grbtbl_item('alvsf','sfc',3,0,176,1)
    gribtable(1)%item(42)=nemsio_grbtbl_item('alvwf','sfc',3,0,177,1)
    gribtable(1)%item(43)=nemsio_grbtbl_item('alnsf','sfc',3,0,178,1)
    gribtable(1)%item(44)=nemsio_grbtbl_item('alnwf','sfc',3,0,179,1)
    gribtable(1)%item(45)=nemsio_grbtbl_item('land','sfc',0,0,81,1)
    gribtable(1)%item(46)=nemsio_grbtbl_item('veg','sfc',2,0,87,1)
    gribtable(1)%item(47)=nemsio_grbtbl_item('cnwat','sfc',5,0,223,1)
    gribtable(1)%item(48)=nemsio_grbtbl_item('f10m','10 m above gnd',5,10,180,105)
    gribtable(1)%item(49)=nemsio_grbtbl_item('ugrd','10 m above gnd',2,10,33,105)
    gribtable(1)%item(50)=nemsio_grbtbl_item('vgrd','10 m above gnd',2,10,34,105)
!
    gribtable(1)%item(51)=nemsio_grbtbl_item('tmp','2 m above gnd',3,2,11,105)
    gribtable(1)%item(52)=nemsio_grbtbl_item('spfh','2 m above gnd',6,2,51,105)
    gribtable(1)%item(53)=nemsio_grbtbl_item('vtype','sfc',1,0,225,1)
    gribtable(1)%item(54)=nemsio_grbtbl_item('facsf','sfc',3,0,207,1)
    gribtable(1)%item(55)=nemsio_grbtbl_item('facsf','sfc',3,0,208,1)
    gribtable(1)%item(56)=nemsio_grbtbl_item('fricv','sfc',3,0,253,1)
    gribtable(1)%item(57)=nemsio_grbtbl_item('ffmm','sfc',3,0,253,1)   !???
    gribtable(1)%item(58)=nemsio_grbtbl_item('ffhh','sfc',3,0,253,1)   !???
    gribtable(1)%item(59)=nemsio_grbtbl_item('icetk','sfc',2,0,92,1) 
    gribtable(1)%item(60)=nemsio_grbtbl_item('icec','sfc',3,0,91,1) 
!
    gribtable(1)%item(61)=nemsio_grbtbl_item('tisfc','sfc',2,0,171,1) 
    gribtable(1)%item(62)=nemsio_grbtbl_item('tprcp','sfc',2,0,171,1)  !tprc ???
    gribtable(1)%item(63)=nemsio_grbtbl_item('crain','sfc',0,0,140,1)  !srflag ???
    gribtable(1)%item(64)=nemsio_grbtbl_item('snod','sfc',6,0,66,1)  
    gribtable(1)%item(65)=nemsio_grbtbl_item('slc','soil layer',3,130,160,112)
    gribtable(1)%item(66)=nemsio_grbtbl_item('shdmin','sfc',3,0,189,1)
    gribtable(1)%item(67)=nemsio_grbtbl_item('shdmax','sfc',3,0,190,1)
    gribtable(1)%item(68)=nemsio_grbtbl_item('sotyp','sfc',1,0,224,1)
    gribtable(1)%item(69)=nemsio_grbtbl_item('salbd','sfc',1,0,194,1)
!jw    gribtable(1)%item(49)=nemsio_grbtbl_item('orog','sfc',1,0,194,1)   !orog???
!flx
    gribtable(1)%item(70)=nemsio_grbtbl_item('uflx','sfc',3,0,124,1)   
!
    gribtable(1)%item(71)=nemsio_grbtbl_item('vflx','sfc',3,0,125,1)   
    gribtable(1)%item(72)=nemsio_grbtbl_item('shtfl','sfc',0,0,122,1)   
    gribtable(1)%item(73)=nemsio_grbtbl_item('lhtfl','sfc',0,0,121,1)   
    gribtable(1)%item(74)=nemsio_grbtbl_item('dlwrf','sfc',0,0,205,1)   
    gribtable(1)%item(75)=nemsio_grbtbl_item('ulwrf','sfc',0,0,212,1)   
    gribtable(1)%item(76)=nemsio_grbtbl_item('ulwrf','nom. top',0,0,212,8)   
    gribtable(1)%item(77)=nemsio_grbtbl_item('uswrf','nom. top',0,0,211,8)  
    gribtable(1)%item(78)=nemsio_grbtbl_item('uswrf','sfc',0,0,211,1)  
    gribtable(1)%item(79)=nemsio_grbtbl_item('dswrf','sfc',0,0,204,1)   
    gribtable(1)%item(80)=nemsio_grbtbl_item('prate','sfc',6,0,59,1)   

    gribtable(1)%item(81)=nemsio_grbtbl_item('soilm','0-200 cm down',4,200,86,112)   
    gribtable(1)%item(82)=nemsio_grbtbl_item('vgtyp','sfc',1,0,225,1)   
    gribtable(1)%item(83)=nemsio_grbtbl_item('cprat','sfc',6,0,214,1)   
    gribtable(1)%item(84)=nemsio_grbtbl_item('gflux','sfc',0,0,155,1)  
    gribtable(1)%item(85)=nemsio_grbtbl_item('tmax','2 m above gnd',1,2,15,105)   
    gribtable(1)%item(86)=nemsio_grbtbl_item('tmin','2 m above gnd',1,2,16,105)  
    gribtable(1)%item(87)=nemsio_grbtbl_item('watr','sfc',5,0,90,1)   
    gribtable(1)%item(88)=nemsio_grbtbl_item('pevpr','sfc',0,0,145,1)   
    gribtable(1)%item(89)=nemsio_grbtbl_item('cwork','atmos col',0,0,146,200)   
    gribtable(1)%item(90)=nemsio_grbtbl_item('u-gwd','sfc',3,0,147,1)   
!
    gribtable(1)%item(91)=nemsio_grbtbl_item('v-gwd','sfc',3,0,148,1)  
    gribtable(1)%item(92)=nemsio_grbtbl_item('hpbl','sfc',0,0,221,1)  
    gribtable(1)%item(93)=nemsio_grbtbl_item('pwat','atmos col',1,0,54,200)   
    gribtable(1)%item(94)=nemsio_grbtbl_item('albdo','sfc',1,0,84,1)   
    gribtable(1)%item(95)=nemsio_grbtbl_item('cnwat','sfc',5,0,223,1)   
    gribtable(1)%item(96)=nemsio_grbtbl_item('sfexc','sfc',4,0,208,1)   
    gribtable(1)%item(97)=nemsio_grbtbl_item('pevpr','sfc',0,0,145,1)  
    gribtable(1)%item(98)=nemsio_grbtbl_item('dlwrf','sfc',0,0,205,1)   
    gribtable(1)%item(99)=nemsio_grbtbl_item('ulwrf','sfc',0,0,212,1)   
    gribtable(1)%item(100)=nemsio_grbtbl_item('uswrf','sfc',0,0,211,1)  
!
    gribtable(1)%item(101)=nemsio_grbtbl_item('dswrf','sfc',0,0,204,1)   
    gribtable(1)%item(102)=nemsio_grbtbl_item('ssrun','sfc',5,0,235,1)   
    gribtable(1)%item(103)=nemsio_grbtbl_item('tmp','hybrid lev 1',3,1,11,109)   
    gribtable(1)%item(104)=nemsio_grbtbl_item('spfh','hybrid lev 1',6,1,51,109)   
    gribtable(1)%item(105)=nemsio_grbtbl_item('ugrd','hybrid lev 1',2,1,33,109)  
    gribtable(1)%item(106)=nemsio_grbtbl_item('vgrd','hybrid lev 1',2,1,34,109)  
    gribtable(1)%item(107)=nemsio_grbtbl_item('hgt','hybrid lev 1',2,1,7,109)   
    gribtable(1)%item(108)=nemsio_grbtbl_item('evbs','sfc',0,0,199,1)   
    gribtable(1)%item(109)=nemsio_grbtbl_item('evcw','sfc',0,0,200,1)   
    gribtable(1)%item(110)=nemsio_grbtbl_item('trans','sfc',0,0,210,1)   
    gribtable(1)%item(111)=nemsio_grbtbl_item('snowc','sfc',3,0,238,1)   
!    
!    gribtable(1)%item(50)=nemsio_grbtbl_item('nlat','sfc',2,0,176,1)
!    gribtable(1)%item(51)=nemsio_grbtbl_item('elon','sfc',2,0,177,1)
!    gribtable(1)%item(52)=nemsio_grbtbl_item('nlonb','sfc',2,0,177,1)   !vlat ???
!    gribtable(1)%item(53)=nemsio_grbtbl_item('elonb','sfc',2,0,177,1)   !vlon ???
!    gribtable(1)%item(54)=nemsio_grbtbl_item('wtend','sfc',6,0,236,1)   !wtend precision
!    gribtable(1)%item(55)=nemsio_grbtbl_item('omgalf','sfc',6,0,154,1)   !wtend precision
!    gribtable(1)%item(56)=nemsio_grbtbl_item('omgalf','sfc',6,0,154,1)   !wtend precision
!
!*** table 129
    gribtable(2)%iptv=129
    gribtable(2)%item(1)=nemsio_grbtbl_item('duvb','sfc',2,0,200,1)   
    gribtable(2)%item(2)=nemsio_grbtbl_item('cduvb','sfc',2,0,201,1)   
!
!*** table 130
    gribtable(3)%iptv=130
    gribtable(3)%item(1)=nemsio_grbtbl_item('sltyp','sfc',0,0,222,1)
    gribtable(3)%item(2)=nemsio_grbtbl_item('sbsno','sfc',0,0,198,1)   
    gribtable(3)%item(3)=nemsio_grbtbl_item('soill','0-10 cm down',4,10,160,112)
    gribtable(3)%item(4)=nemsio_grbtbl_item('soill','10-40 cm down',4,2600,160,112)
    gribtable(3)%item(5)=nemsio_grbtbl_item('soill','40-100 cm down',4,10340,160,112)
    gribtable(3)%item(6)=nemsio_grbtbl_item('soill','100-200 cm down',4,25800,160,112)
    gribtable(3)%item(7)=nemsio_grbtbl_item('acond','sfc',4,0,179,1)   
!
    iret=izero
  end subroutine nemsio_setgrbtbl
!------------------------------------------------------------------------------

  subroutine nemsio_gfinit(gfile,iret,recname,reclevtyp,reclev)
!$$$  subprogram documentation block
!                .      .    .                                        .
! subprogram:    nemsio_gfinit
!   prgmmr:
!
! abstract: set gfile variables to operational model output
!
! program history log:
!   2009-08-31  lueken - added subprogram doc block
!
!   input argument list:
!    gfile
!    recname
!    reclevtyp
!    reclev
!
!   output argument list:
!    gfile
!    iret
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    implicit none
    type(nemsio_gfile),intent(inout)     :: gfile
    integer(i_kind),intent(out)          :: iret
    character(*),optional,intent(in)     :: recname(:)
    character(*),optional,intent(in)     :: reclevtyp(:)
    integer(i_kind),optional,intent(in)  :: reclev(:)
    integer(i_kind)                      :: i,j,rec
    real(r_kind),allocatable             :: slat(:)
    real(r_kind),allocatable             :: dx(:)
    real(r_kind)                         :: radi
    logical(nemsio_logickind)            :: linit
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! set operational format
!
    iret=-8
    gfile%version=200809
    gfile%nfday=izero
    gfile%nfhour=izero
    gfile%nfminute=izero
    gfile%nfsecondn=izero
    gfile%nfsecondd=100.
    gfile%extrameta=.false.
    gfile%nmetavari=izero
    gfile%nmetavarr=izero
    gfile%nmetavarl=izero
    gfile%nmetavarc=izero
    gfile%nmetaaryi=izero
    gfile%nmetaaryr=izero
    gfile%nmetaaryl=izero
    gfile%nmetaaryc=izero
!    write(0,*)'in gfinit, modelname=',gfile%modelname

    if ( gfile%modelname .eq. 'GFS') then
      if(gfile%dimy.eq.nemsio_intfill) gfile%dimy=576
      if(gfile%dimx.eq.nemsio_intfill) gfile%dimx=1152
      if(gfile%dimz.eq.nemsio_intfill) gfile%dimz=64
      if(gfile%nframe.eq.nemsio_intfill) gfile%nframe=izero
      if(gfile%ntrac.eq.nemsio_intfill) gfile%ntrac=3
      if(gfile%nrec.eq.nemsio_intfill)gfile%nrec=2+9*gfile%dimz+35+3*gfile%nsoil
      gfile%ncldt=ione
      gfile%idsl=izero
      gfile%idvm=izero
      gfile%idrt=4
      linit=gfile%dimy==576.and.gfile%dimx==1152.and.gfile%dimz==64
      gfile%extrameta=.True.
      gfile%nmetavari=5
      if(linit) then
        gfile%jcap=382
        gfile%idvc=2
        gfile%nmetavari=15
        gfile%nmetavarr=ione
        gfile%nmetaaryi=ione
      endif
    else if (gfile%modelname .eq. 'NMMB' ) then
      if(gfile%dimx.eq.nemsio_intfill) gfile%dimx=257
      if(gfile%dimy.eq.nemsio_intfill) gfile%dimy=181
      if(gfile%dimz.eq.nemsio_intfill) gfile%dimz=35
      if(gfile%nframe.eq.nemsio_intfill) gfile%nframe=ione
      if(gfile%ntrac.eq.nemsio_intfill) gfile%ntrac=4
      if(gfile%nrec.eq.nemsio_intfill)     & 
        gfile%nrec=86+20*gfile%dimz+(gfile%dimz+1)+3*gfile%nsoil+4
      linit=gfile%dimx==257.and.gfile%dimy==181.and.gfile%dimz==35
      if(linit) then
        gfile%extrameta=.True.
        gfile%nmetavari=9
        gfile%nmetavarr=12
        gfile%nmetavarl=2
        gfile%nmetaaryr=7
        gfile%rlon_min=-178.5937347
        gfile%rlon_max=178.5937347
        gfile%rlat_min=-89.49999237
        gfile%rlat_max=89.49999237
      endif
!    print *,'in gfinit, nrec=',gfile%nrec
    else if (gfile%modelname.eq. "GSI" ) then
      if(gfile%dimx.eq.nemsio_intfill) gfile%dimx=1152
      if(gfile%dimy.eq.nemsio_intfill) gfile%dimy=576
      if(gfile%dimz.eq.nemsio_intfill) gfile%dimz=64
      if(gfile%nrec.eq.nemsio_intfill)     & 
        gfile%nrec=10+3*gfile%dimz+gfile%ntrac*gfile%dimz
      linit=gfile%dimx==1152.and.gfile%dimy==576.and.gfile%dimz==64
      if(linit) then
        gfile%jcap=382
        gfile%idvc=2
        gfile%ncldt=ione
        gfile%idsl=izero
        gfile%idvm=izero
        gfile%idrt=4
        gfile%extrameta=.True.
        gfile%nmetaaryc=ione
      endif
    endif
    if(gfile%dimx.eq.nemsio_intfill.or.gfile%dimy.eq.nemsio_intfill.or. &
       gfile%dimz.eq.nemsio_intfill.or.gfile%idate(1).eq.nemsio_intfill) then
       print *,'WRONG: please provide dimensions!'
       call nemsio_stop
    endif
    if(gfile%nframe.eq.nemsio_intfill) gfile%nframe=izero
    gfile%fieldsize=(gfile%dimx+2*gfile%nframe)*(gfile%dimy+2*gfile%nframe)
    if(gfile%nrec.eq.nemsio_intfill) gfile%nrec=12+(3+gfile%ntrac)*gfile%dimz
!
!    print *,'gfinit, after set up dimension,',gfile%nrec,gfile%ntrac,gfile%fieldsize,&
!       gfile%dimz
    call nemsio_almeta(gfile,iret)
    if ( iret.ne.izero ) return
    call nemsio_alextrameta(gfile,iret)
    if ( iret.ne.izero ) return
!    print *,'gfinit, after set up allocate array size dx',size(gfile%dx),size(gfile%cpi), &
!      size(gfile%variname), size(gfile%varrname),size(gfile%varlname),size(gfile%aryrname),&
!      gfile%nmetavari,gfile%nmetavarr,gfile%nmetavarl,gfile%nmetaaryr,gfile%nmetaaryi
!
!
    if ( gfile%modelname .eq. 'GFS' ) then
      gfile%variname=(/'itrun  ','iorder ','irealf ','igen   ','icen2  '/)
      gfile%varival=(/1,2,1,82,0/)
      if(linit) then
      gfile%variname=(/'itrun  ','iorder ','irealf ','igen   ','latf   ','lonf   ','latr   ','lonr   ', &
                      'icen2  ','idpp   ','idvt   ','idrun  ','idusr  ','ixgr   ','nvcoord'/)
      gfile%varival=(/1,2,1,82,576,1152,576,1152,0,21,0,0,0,0,2/)
      gfile%varrname=(/'pdryini'/)
      gfile%varrval=(/98.29073/)
      gfile%aryiname(1)='iens'
      gfile%aryilen(1)=2
      allocate(gfile%aryival(maxval(gfile%aryilen),gfile%nmetaaryi))
      gfile%aryival(:,1)=(/0,0/)
!      print *,'before gfile vcoord',size(gfile%vcoord,1),size(gfile%vcoord,2),size(gfile%vcoord,3)

      if(gfile%dimz==64) then
      gfile%vcoord(1:gfile%dimz+ione,1,1)=(/2*0.0000000,0.57499999,5.7410002,21.516001,55.712002, &
      116.89900,214.01500,356.22299,552.71997,812.48901,1143.9880,1554.7889, &
      2051.1499,2637.5530,3316.2170,4086.6140,4945.0288,5884.2061,6893.1172, &
      7956.9082,9057.0508,10171.712,11276.348,12344.490,13348.671,14261.435, &
      15056.342,15708.893,16197.315,16503.145,16611.604,16511.736,16197.967, &
      15683.489,14993.074,14154.316,13197.065,12152.937,11054.853,9936.6143, &
      8832.5371,7777.1499,6804.8740,5937.0498,5167.1460,4485.4932,3883.0520, &
      3351.4600,2883.0381,2470.7881,2108.3660,1790.0510,1510.7111,1265.7520, &
      1051.0800,863.05798,698.45697,554.42401,428.43399,318.26599,221.95799, &
      137.78999,64.247002,0.0000000 /)
      gfile%vcoord(1:gfile%dimz+ione,2,1)=(/1.0000000,0.99467117,0.98862660,0.98174226,0.97386760, &
      0.96482760,0.95443410,0.94249105,0.92879730,0.91315103,0.89535499, &
      0.87522358,0.85259068,0.82731885,0.79930973,0.76851469,0.73494524, &
      0.69868290,0.65988702,0.61879963,0.57574666,0.53113484,0.48544332, &
      0.43921080,0.39301825,0.34746850,0.30316412,0.26068544,0.22057019, &
      0.18329623,0.14926878,0.11881219,0.92166908E-01,0.69474578E-01,0.50646842E-01, &
      0.35441618E-01, 0.23555880E-01,0.14637120E-01,0.82940198E-02,0.41067102E-02, &
      0.16359100E-02,0.43106001E-03,0.36969999E-04,0.0000000*22 /) 
      gfile%vcoord(1:gfile%dimz+ione,3,1)=zero
      gfile%vcoord(1:gfile%dimz+ione,1,2)=zero
      gfile%vcoord(1:gfile%dimz+ione,2,2)=zero
      gfile%vcoord(1:gfile%dimz+ione,3,2)=zero
      endif

     if(.not.present(recname).or..not.present(reclevtyp).or..not.present(reclev) )then
     if(size(gfile%recname).eq.2+9*gfile%dimz+35+3*gfile%nsoil) then
     rec=ione
     gfile%recname(rec)='hgt'
     gfile%recname(rec+ione)='pres'
     gfile%recname(rec+2:rec+gfile%dimz+ione)='pres'
     gfile%recname(rec+gfile%dimz+2:rec+2*gfile%dimz+ione)='dpres'
     gfile%recname(rec+2*gfile%dimz+2:rec+3*gfile%dimz+ione)='tmp'
     gfile%recname(rec+3*gfile%dimz+2:rec+4*gfile%dimz+ione)='ugrd'
     gfile%recname(rec+4*gfile%dimz+2:rec+5*gfile%dimz+ione)='vgrd'
     gfile%recname(rec+5*gfile%dimz+2:rec+6*gfile%dimz+ione)='spfh'
     gfile%recname(rec+6*gfile%dimz+2:rec+7*gfile%dimz+ione)='o3mr'
     gfile%recname(rec+7*gfile%dimz+2:rec+8*gfile%dimz+ione)='clwmr'
     gfile%recname(rec+8*gfile%dimz+2:rec+9*gfile%dimz+ione)='vvel'
     rec=rec+9*gfile%dimz+ione
     gfile%recname(rec+ione:rec+35)=(/'slmsk ','orog  ','tsea  ','sheleg','tg3   ','zorl  ', &
       'cv    ','cvb   ','cvt   ',  &
       'alvsf ','alvwf ','alnsf ','alnwf ','vfrac ','canopy','f10m  ','t2m   ',    &
       'q2m   ','vtype ','stype ','facsf ','facwf ','uustar','ffmm  ','ffhh  ',     &
       'hice  ','fice  ','tisfc ','tprcp ','srflag','snwdph','shdmin','shdmax',  &
       'slope ','snoalb' /)
     gfile%recname(rec+36:rec+35+gfile%nsoil)='stc'
     gfile%recname(rec+36+gfile%nsoil:rec+35+2*gfile%nsoil)='smc'
     gfile%recname(rec+36+2*gfile%nsoil:rec+35+3*gfile%nsoil)='slc'
     endif

     if(size(gfile%reclevtyp).eq.2+9*gfile%dimz+35+3*gfile%nsoil) then
     rec=ione
     gfile%reclevtyp='sfc'
     gfile%reclevtyp(rec+2:rec+gfile%dimz+ione)='mid layer'
     gfile%reclevtyp(rec+gfile%dimz+2:rec+2*gfile%dimz+ione)='mid layer'
     gfile%reclevtyp(rec+2*gfile%dimz+2:rec+3*gfile%dimz+ione)='mid layer'
     gfile%reclevtyp(rec+3*gfile%dimz+2:rec+4*gfile%dimz+ione)='mid layer'
     gfile%reclevtyp(rec+4*gfile%dimz+2:rec+5*gfile%dimz+ione)='mid layer'
     gfile%reclevtyp(rec+5*gfile%dimz+2:rec+6*gfile%dimz+ione)='mid layer'
     gfile%reclevtyp(rec+6*gfile%dimz+2:rec+7*gfile%dimz+ione)='mid layer'
     gfile%reclevtyp(rec+7*gfile%dimz+2:rec+8*gfile%dimz+ione)='mid layer'
     gfile%reclevtyp(rec+8*gfile%dimz+2:rec+9*gfile%dimz+ione)='mid layer'
     rec=rec+9*gfile%dimz+36
     gfile%reclevtyp(rec+ione:rec+3*gfile%nsoil)='soil layer'
     endif
!
     if(size(gfile%reclev).eq.2+9*gfile%dimz+35+3*gfile%nsoil) then
     gfile%reclev=ione
     rec=2
     do j=3,11
     do i=1,gfile%dimz
       gfile%reclev(rec+(j-3)*gfile%dimz+i)=i
     enddo
     enddo
     rec=rec+9*gfile%dimz+35
     do j=1,3
     do i=1,gfile%nsoil
       gfile%reclev(rec+(j-ione)*gfile%nsoil+i)=i
     enddo
     enddo
     endif
!
     endif
    endif 
!
!lat:
     allocate(slat(gfile%dimy))
     call splat(gfile%idrt,gfile%dimy,slat)
     radi=180.0/(four*atan(one))
     do  i=1,gfile%dimy
       gfile%lat((i-ione)*gfile%dimx+ione:i*gfile%dimx) = asin(slat(i)) * radi
     enddo
     deallocate(slat)
!lon:
     do i=1,gfile%dimx
       gfile%lon(i) = 360./gfile%dimx*(i-ione)
     enddo
     do j=2,gfile%dimy
       gfile%lon((j-ione)*gfile%dimx+ione:j*gfile%dimx) = gfile%lon(1:gfile%dimx)
     enddo
!     write(0,*)'in gfinit, lat=',maxval(gfile%lat(1:gfile%fieldsize)),  &
!       minval(gfile%lat(1:gfile%fieldsize)),'lon=',&
!       maxval(gfile%lon(1:gfile%fieldsize)), &
!       minval(gfile%lon(1:gfile%fieldsize))
   else if ( gfile%modelname .eq. "NMMB" .and. linit) then
      gfile%variname=(/'mp_phys ','sfsfcphy','nphs    ','nclod   ', &
        'nheat   ','nprec   ','nrdlw   ','nrdsw   ','nsrfc   ' /)
      gfile%varival=(/5,99,2,60,60,60,60,60,60/)
      gfile%varrname=(/'pdtop ','dt    ','pt    ','tlm0d ','tph0d ','tstart', &
        'aphtim','ardlw ','ardsw ','asrfc ','avcnvc','avrain' /)
      gfile%varrval=(/26887.10156,180.,1000.,0.,0.,0.,-1000000.0, &
        -1000000.0,-1000000.0,-1000000.0,0.,0./)
      gfile%varlname=(/'run   ','global'/)
      gfile%varlval=(/.true.,.false. /)
      gfile%aryrname=(/'dsg1  ','dsg2  ','sgml1 ','sgml2 ','sg1   ','sg2   ','sldpth'/)
      gfile%aryrlen=(/gfile%dimz,gfile%dimz,gfile%dimz,gfile%dimz, &
        gfile%dimz+ione,gfile%dimz+ione,gfile%nsoil /)
      allocate(gfile%aryrval(maxval(gfile%aryrlen),gfile%nmetaaryr))
      if(size(gfile%aryrval,1).eq.36) then
      gfile%aryrval(1:35,1)=(/0.8208955079E-01,0.8582090586E-01,0.8582088351E-01,  &
             0.8582088351E-01,0.8582091331E-01,0.8582085371E-01,  &
             0.9328359365E-01,0.9701490402E-01,0.9701496363E-01,  &
             0.9701490402E-01,0.1044776440,0.0000000000E+00,0.0000000000E+00,  &
             0.0000000000E+00,0.0000000000E+00,0.0000000000E+00,0.0000000000E+00,  &
             0.0000000000E+00,0.0000000000E+00,0.0000000000E+00,0.0000000000E+00,  &
             0.0000000000E+00,0.0000000000E+00,0.0000000000E+00,0.0000000000E+00,  &
             0.0000000000E+00,0.0000000000E+00,0.0000000000E+00,0.0000000000E+00,  &
             0.0000000000E+00,0.0000000000E+00,0.0000000000E+00,0.0000000000E+00,  &
             0.0000000000E+00,0.0000000000E+00  /)
       gfile%aryrval(1:35,2)=(/0.0000000000E+00,0.0000000000E+00,0.0000000000E+00,0.0000000000E+00, &
             0.0000000000E+00,0.0000000000E+00,0.0000000000E+00,0.0000000000E+00, &
             0.0000000000E+00,0.0000000000E+00,0.0000000000E+00,0.4098360986E-01, &
             0.4371585697E-01,0.4781420529E-01,0.4918029904E-01,0.5054645240E-01, &
             0.5327869952E-01,0.5464482307E-01,0.5464476347E-01,0.5464485288E-01, &
             0.5464485288E-01,0.5464470387E-01,0.5191260576E-01,0.5054640770E-01, &
             0.4918038845E-01,0.4508191347E-01,0.4371589422E-01,0.3961753845E-01, &
             0.3551906347E-01,0.3005468845E-01,0.2732235193E-01,0.2459019423E-01, &
             0.1912564039E-01,0.1639348269E-01,0.8196711540E-02 /)
       gfile%aryrval(1:35,3)=(/0.4104477540E-01,0.1250000000,0.2108208984,0.2966417670,0.3824626803, &
              0.4682835639,0.5578358173,0.6529850364,0.7500000000,0.8470149040, &
              0.9477611780,1.000000000,1.000000000,1.000000000,1.000000000, &
              1.000000000,1.000000000,1.000000000,1.000000000,1.000000000,  &
              1.000000000,1.000000000,1.000000000,1.000000000,1.000000000,  &
              1.000000000,1.000000000,1.000000000,1.000000000,1.000000000,  &
              1.000000000,1.000000000,1.000000000,1.000000000,1.000000000 /)
       gfile%aryrval(1:35,4)=(/0.0000000000E+00,0.0000000000E+00,0.0000000000E+00,0.0000000000E+00, &
              0.0000000000E+00,0.0000000000E+00,0.0000000000E+00,0.0000000000E+00,  &
              0.0000000000E+00,0.0000000000E+00,0.0000000000E+00,0.2049180493E-01,  &
              0.6284153461E-01,0.1086065695,0.1571038216,0.2069672048,0.2588797808, &
              0.3128415346,0.3674863279,0.4221311212,0.4767760038,0.5314207673,     &
              0.5846993923,0.6359289289,0.6857923269,0.7329235077,0.7773224115,     &
              0.8189890981,0.8565573692,0.8893442750,0.9180327654,0.9439890385,     &
              0.9658470154,0.9836065769,0.9959016442/)
        gfile%aryrval(1:36,5)=(/0.0000000000E+00,0.8208955079E-01,0.1679104567,0.2537313402,  &
             0.3395522237,0.4253731370,0.5111939907,0.6044775844,0.7014924884,  &
             0.7985074520,0.8955223560,1.000000000,1.000000000,1.000000000,  &
             1.000000000,1.000000000,1.000000000,1.000000000,1.000000000,  &
             1.000000000,1.000000000,1.000000000,1.000000000,1.000000000,  &
             1.000000000,1.000000000,1.000000000,1.000000000,1.000000000,  &
             1.000000000,1.000000000,1.000000000,1.000000000,1.000000000,  &
             1.000000000,1.000000000 /)
        gfile%aryrval(1:36,6)=(/0.0000000000E+00,0.0000000000E+00,0.0000000000E+00,0.0000000000E+00, &
             0.0000000000E+00,0.0000000000E+00,0.0000000000E+00,0.0000000000E+00, &
             0.0000000000E+00,0.0000000000E+00,0.0000000000E+00,0.0000000000E+00, &
             0.4098360986E-01,0.8469946682E-01,0.1325136721,0.1816939712,  &
             0.2322404236,0.2855191231,0.3401639462,0.3948087096,0.4494535625, &
             0.5040984154,0.5587431192,0.6106557250,0.6612021327,0.7103825212, &
             0.7554644346,0.7991803288,0.8387978673,0.8743169308,0.9043716192, &
             0.9316939712,0.9562841654,0.9754098058,0.9918032885,1.000000000 /)
        gfile%aryrval(1,7)=0.1000000015
        gfile%aryrval(2,7)=0.3000000119
        gfile%aryrval(3,7)=0.6000000238
        gfile%aryrval(4,7)=1.000000000
        endif
!
        gfile%dy=111282.1953
        allocate(dx(gfile%dimy+2*gfile%nframe))
        dx=zero
        if(size(dx).eq.183) then
        dx(1:183)=(/2731.143066,0.0000000000E+00,2731.143066,5461.452148,8190.078125,10916.22852, &
             13639.05469,16357.72461,19071.41211,21779.29102,24480.51758,27174.30469,29859.81250, &
             32536.22656,35202.73047,37858.51172,40502.74219,43134.64844,45753.42188,48358.25781, &
             50948.35938,53522.93750,56081.21094,58622.40625,61145.74609,63650.46094,66135.78906, &
             68600.96094,71045.23438,73467.88281,75868.14844,78245.29688,80598.62500,82927.38281, &
             85230.89062,87508.42969,89759.32031,91982.86719,94178.39062,96345.23438,98482.71875, &
             100590.2188,102667.0625,104712.6406,106726.3281,108707.5000,110655.5625,112569.9062, &
             114449.9844,116295.1719,118104.9531,119878.7500,121616.0312,123316.2656,124978.9453, &
             126603.5469,128189.5938,129736.5781,131244.0625,132711.5625,134138.6250,135524.8438, &
             136869.7500,138173.0000,139434.1406,140652.8125,141828.6406,142961.2812,144050.3438, &
             145095.5469,146096.5625,147053.0625,147964.7656,148831.4062,149652.6875,150428.4062, &
             151158.2969,151842.1562,152479.7500,153070.9062,153615.4219,154113.1562,154563.9375, &
             154967.6406,155324.1406,155633.3281,155895.1094,156109.3906,156276.1250,156395.2656, &
             156466.7656,156490.5938,156466.7656,156395.2656,156276.1250,156109.3906,155895.1094, &
             155633.3281,155324.1406,154967.6406,154563.9375,154113.1562,153615.4219,153070.9062, &
             152479.7500,151842.1562,151158.2969,150428.4062,149652.6875,148831.4062,147964.7656, &
             147053.0625,146096.5625,145095.5469,144050.3438,142961.2812,141828.6406,140652.8125, &
             139434.1406,138173.0000,136869.7500,135524.8438,134138.6250,132711.5625,131244.0625, &
             129736.5781,128189.5938,126603.5469,124978.9453,123316.2656,121616.0312,119878.7500, &
             118104.9531,116295.1719,114449.9844,112569.9062,110655.5625,108707.5000,106726.3281, &
             104712.6406,102667.0625,100590.2188,98482.71875,96345.23438,94178.39062,91982.86719, &
             89759.32031,87508.42969,85230.89062,82927.38281,80598.62500,78245.29688,75868.14844, &
             73467.88281,71045.23438,68600.96094,66135.78906,63650.46094,61145.74609,58622.40625, &
             56081.21094,53522.93750,50948.35938,48358.25781,45753.42188,43134.64844,40502.74219, &
             37858.51172,35202.73047,32536.22656,29859.81250,27174.30469,24480.51758,21779.29102, &
             19071.41211,16357.72461,13639.05469,10916.22852,8190.078125,5461.452148,2731.143066, &
             0.0000000000E+00,2731.143066 /)
!       print *,'size(dx)=',size(dx),'jm+2=',gfile%dimy+2,'size(gfile%dx)=',size(gfile%dx), &
!          maxval(gfile%dx),minval(gfile%dx),maxval(gfile%dy),maxval(gfile%dy),'nframe=', &
!          gfile%nframe,'dimy=',gfile%dimy
       if(allocated(gfile%dx).and.size(gfile%dx)==183*(gfile%dimx+2*gfile%nframe)) then
        do i=1,gfile%dimy+2*gfile%nframe
         gfile%dx((i-ione)*(gfile%dimx+2*gfile%nframe)+ione:i*(gfile%dimx+2*gfile%nframe))=dx(i)
        enddo
       endif
      endif
      deallocate(dx)

     if(.not.present(recname).or..not.present(reclevtyp).or..not.present(reclev) )then
     if(size(gfile%recname)==86+20*gfile%dimz+(gfile%dimz+1)+3*gfile%nsoil+4) then
     rec=ione
     gfile%recname(1)='hgt'
     gfile%recname(2)='glat'
     gfile%recname(3)='glon'
     gfile%recname(4)='dpres'
     gfile%recname(5)='vlat'
     gfile%recname(6)='vlon'
     gfile%recname(7)='acfrcv'
     gfile%recname(8)='acfrst'
     gfile%recname(9)='acprec'
     gfile%recname(10)='acsnom'
     gfile%recname(11)='acsnow'
     gfile%recname(12)='akhs_out'
     gfile%recname(13)='akms_out'
     gfile%recname(14)='albase'
     gfile%recname(15)='albedo'
     gfile%recname(16)='alwin'
     gfile%recname(17)='alwout'
     gfile%recname(18)='alwtoa'
     gfile%recname(19)='aswin'
     gfile%recname(20)='aswout'
     gfile%recname(21)='aswtoa'
     gfile%recname(22)='bgroff'
     gfile%recname(23)='cfrach'
     gfile%recname(24)='cfracl'
     gfile%recname(25)='cfracm'
     gfile%recname(26)='cldefi'
     gfile%recname(27)='cmc'
     gfile%recname(28)='cnvbot'
     gfile%recname(29)='cnvtop'
     gfile%recname(30)='cprate'
     gfile%recname(31)='cuppt'
     gfile%recname(32)='cuprec'
     gfile%recname(33)='czen'
     gfile%recname(34)='czmean'
     gfile%recname(35)='epsr'
     gfile%recname(36)='grnflx'
     gfile%recname(37)='hbotd'
     gfile%recname(38)='hbots'
     gfile%recname(39)='htopd'
     gfile%recname(40)='htops'
     gfile%recname(41)='mxsnal'
     gfile%recname(42)='pblh'
     gfile%recname(43)='potevp'
     gfile%recname(44)='prec'
     gfile%recname(45)='pshltr'
     gfile%recname(46)='q10'
     gfile%recname(47)='qsh'
     gfile%recname(48)='qshltr'
     gfile%recname(49)='qwbs'
     gfile%recname(50)='qz0'
     gfile%recname(51)='radot'
     gfile%recname(52)='rlwin'
     gfile%recname(53)='rlwtoa'
     gfile%recname(54)='rswin'
     gfile%recname(55)='rswinc'
     gfile%recname(56)='rswout'
     gfile%recname(57)='sfcevp'
     gfile%recname(58)='sfcexc'
     gfile%recname(59)='sfclhx'
     gfile%recname(60)='sfcshx'
     gfile%recname(61)='si'
     gfile%recname(62)='sice'
     gfile%recname(63)='sigt4'
     gfile%recname(64)='sm'
     gfile%recname(65)='smstav'
     gfile%recname(66)='smstot'
     gfile%recname(67)='sno'
     gfile%recname(68)='snopcx'
     gfile%recname(69)='soiltb'
     gfile%recname(70)='sr'
     gfile%recname(71)='ssroff'
     gfile%recname(72)='tsea'
     gfile%recname(73)='subshx'
     gfile%recname(74)='tg'
     gfile%recname(75)='th10'
     gfile%recname(76)='ths'
     gfile%recname(77)='thz0'
     gfile%recname(78)='tshltr'
     gfile%recname(79)='twbs'
     gfile%recname(80)='u10'
     gfile%recname(81)='uustar'
     gfile%recname(82)='uz0'
     gfile%recname(83)='v10'
     gfile%recname(84)='vfrac'
     gfile%recname(85)='vz0'
     gfile%recname(86)='zorl'

     rec=86
     gfile%recname(rec+ione:rec+gfile%dimz)='vvel'
     gfile%recname(rec+gfile%dimz+ione:rec+2*gfile%dimz)='dwdt'
     gfile%recname(rec+2*gfile%dimz+ione:rec+3*gfile%dimz+ione)='pres'
     gfile%recname(rec+3*gfile%dimz+2:rec+4*gfile%dimz+ione)='omgalf'
     gfile%recname(rec+4*gfile%dimz+2:rec+5*gfile%dimz+ione)='rrw'
     gfile%recname(rec+5*gfile%dimz+2:rec+6*gfile%dimz+ione)='cldfra'
     gfile%recname(rec+6*gfile%dimz+2:rec+7*gfile%dimz+ione)='clwmr'
     gfile%recname(rec+7*gfile%dimz+2:rec+8*gfile%dimz+ione)='exch_h'
     gfile%recname(rec+8*gfile%dimz+2:rec+9*gfile%dimz+ione)='spfh'
     gfile%recname(rec+9*gfile%dimz+2:rec+10*gfile%dimz+ione)='q2'
     gfile%recname(rec+10*gfile%dimz+2:rec+11*gfile%dimz+ione)='rlwtt'
     gfile%recname(rec+11*gfile%dimz+2:rec+12*gfile%dimz+ione)='rswtt'
     gfile%recname(rec+12*gfile%dimz+2:rec+13*gfile%dimz+ione)='tmp'
     gfile%recname(rec+13*gfile%dimz+2:rec+14*gfile%dimz+ione)='tcucn'
     gfile%recname(rec+14*gfile%dimz+2:rec+15*gfile%dimz+ione)='train'
     gfile%recname(rec+15*gfile%dimz+2:rec+16*gfile%dimz+ione)='ugrd'
     gfile%recname(rec+16*gfile%dimz+2:rec+17*gfile%dimz+ione)='vgrd'
     gfile%recname(rec+17*gfile%dimz+2:rec+18*gfile%dimz+ione)='xlen_mix'
     gfile%recname(rec+18*gfile%dimz+2:rec+19*gfile%dimz+ione)='f_ice'
     gfile%recname(rec+19*gfile%dimz+2:rec+20*gfile%dimz+ione)='f_rimef'
     gfile%recname(rec+20*gfile%dimz+2:rec+21*gfile%dimz+ione)='f_rain'
     gfile%recname(rec+21*gfile%dimz+2:rec+21*gfile%dimz+gfile%nsoil+ione)='sh2o'
     gfile%recname(rec+21*gfile%dimz+gfile%nsoil+2:rec+21*gfile%dimz+2*gfile%nsoil+ione)='smc'
     gfile%recname(rec+21*gfile%dimz+2*gfile%nsoil+2:rec+21*gfile%dimz+3*gfile%nsoil+ione)='stc'
     gfile%recname(rec+21*gfile%dimz+3*gfile%nsoil+2)='sltyp'
     gfile%recname(rec+21*gfile%dimz+3*gfile%nsoil+3)='vgtyp'
     gfile%recname(rec+21*gfile%dimz+3*gfile%nsoil+4)='cfrcv'
     gfile%recname(rec+21*gfile%dimz+3*gfile%nsoil+5)='cfrst'
     endif

!define rec layer type
     if(size(gfile%reclevtyp)==86+20*gfile%dimz+(gfile%dimz+ione)+3*gfile%nsoil+4) then
     gfile%reclevtyp='sfc'
     gfile%reclevtyp(4)='hybrid sig lev'
     gfile%reclevtyp(46)='10 m above gnd'
     gfile%reclevtyp(75)='10 m above gnd'
     gfile%reclevtyp(80)='10 m above gnd'
     gfile%reclevtyp(83)='10 m above gnd'
     rec=86
     gfile%reclevtyp(rec+ione:rec+gfile%dimz)='mid layer'
     gfile%reclevtyp(rec+gfile%dimz+ione:rec+2*gfile%dimz)='mid layer'
     gfile%reclevtyp(rec+2*gfile%dimz+ione:rec+3*gfile%dimz+ione)='layer'
     gfile%reclevtyp(rec+3*gfile%dimz+2:rec+4*gfile%dimz+ione)='mid layer'
     gfile%reclevtyp(rec+4*gfile%dimz+2:rec+5*gfile%dimz+ione)='mid layer'
     gfile%reclevtyp(rec+5*gfile%dimz+2:rec+6*gfile%dimz+ione)='mid layer'
     gfile%reclevtyp(rec+6*gfile%dimz+2:rec+7*gfile%dimz+ione)='mid layer'
     gfile%reclevtyp(rec+7*gfile%dimz+2:rec+8*gfile%dimz+ione)='mid layer'
     gfile%reclevtyp(rec+8*gfile%dimz+2:rec+9*gfile%dimz+ione)='mid layer'
     gfile%reclevtyp(rec+9*gfile%dimz+2:rec+10*gfile%dimz+ione)='mid layer'
     gfile%reclevtyp(rec+10*gfile%dimz+2:rec+11*gfile%dimz+ione)='mid layer'
     gfile%reclevtyp(rec+11*gfile%dimz+2:rec+12*gfile%dimz+ione)='mid layer'
     gfile%reclevtyp(rec+12*gfile%dimz+2:rec+13*gfile%dimz+ione)='mid layer'
     gfile%reclevtyp(rec+13*gfile%dimz+2:rec+14*gfile%dimz+ione)='mid layer'
     gfile%reclevtyp(rec+14*gfile%dimz+2:rec+15*gfile%dimz+ione)='mid layer'
     gfile%reclevtyp(rec+15*gfile%dimz+2:rec+16*gfile%dimz+ione)='mid layer'
     gfile%reclevtyp(rec+16*gfile%dimz+2:rec+17*gfile%dimz+ione)='mid layer'
     gfile%reclevtyp(rec+17*gfile%dimz+2:rec+18*gfile%dimz+ione)='mid layer'
     gfile%reclevtyp(rec+18*gfile%dimz+2:rec+19*gfile%dimz+ione)='mid layer'
     gfile%reclevtyp(rec+19*gfile%dimz+2:rec+20*gfile%dimz+ione)='mid layer'
     gfile%reclevtyp(rec+20*gfile%dimz+2:rec+21*gfile%dimz+ione)='mid layer'
     gfile%reclevtyp(rec+21*gfile%dimz+2)='0-10 cm down'
     gfile%reclevtyp(rec+21*gfile%dimz+3)='10-40 cm down'
     gfile%reclevtyp(rec+21*gfile%dimz+4)='40-100 cm down'
     gfile%reclevtyp(rec+21*gfile%dimz+5)='100-200 cm down'
     gfile%reclevtyp(rec+21*gfile%dimz+gfile%nsoil+2)='0-10 cm down'
     gfile%reclevtyp(rec+21*gfile%dimz+gfile%nsoil+3)='10-40 cm down'
     gfile%reclevtyp(rec+21*gfile%dimz+gfile%nsoil+4)='40-100 cm down'
     gfile%reclevtyp(rec+21*gfile%dimz+gfile%nsoil+5)='100-200 cm down'
     gfile%reclevtyp(rec+21*gfile%dimz+2*gfile%nsoil+2)='0-10 cm down'
     gfile%reclevtyp(rec+21*gfile%dimz+2*gfile%nsoil+3)='10-40 cm down'
     gfile%reclevtyp(rec+21*gfile%dimz+2*gfile%nsoil+4)='40-100 cm down'
     gfile%reclevtyp(rec+21*gfile%dimz+2*gfile%nsoil+5)='100-200 cm down'
     endif
!
!reclev
     if(size(gfile%reclev)==86+20*gfile%dimz+(gfile%dimz+ione)+3*gfile%nsoil+4) then
     gfile%reclev=ione
     rec=86
     do j=1,3
      do i=1,gfile%dimz
       gfile%reclev(rec+(j-ione)*gfile%dimz+i)=i
      enddo
     enddo
     gfile%reclev(rec+3*gfile%dimz+ione)=gfile%dimz+1
     do j=4,21
      do i=1,gfile%dimz
       gfile%reclev(rec+(j-ione)*gfile%dimz+ione+i)=i
      enddo
     enddo
     rec=rec+21*gfile%dimz+ione
     do j=22,24
      do i=1,gfile%nsoil
       gfile%reclev(rec+(j-22)*gfile%nsoil+i)=i
      enddo
     enddo
     endif
!
    endif
   else if ( gfile%modelname.eq. "GSI" .and.linit) then
!
    gfile%arycname(1)='recunit'
    gfile%aryclen(1)=gfile%nrec
    allocate(gfile%arycval(maxval(gfile%aryclen),gfile%nmetaaryc))
    gfile%arycval(1,1)='pgm'
    gfile%arycval(2,1)='nondim'
    gfile%arycval(3:gfile%dimz+2,1)='K'
    gfile%arycval(gfile%dimz+3:3*gfile%dimz+2,1)='m/s   '
    gfile%arycval(3*gfile%dimz+3:6*gfile%dimz+2,1)='kg/kg '
    gfile%arycval(6*gfile%dimz+3,1)='%'
    gfile%arycval(6*gfile%dimz+4,1)='K'
    gfile%arycval(6*gfile%dimz+5,1)='kg/m2  '
    gfile%arycval(6*gfile%dimz+6,1)='integer'
    gfile%arycval(6*gfile%dimz+7,1)='%      '
    gfile%arycval(6*gfile%dimz+8,1)='integer'
    gfile%arycval(6*gfile%dimz+9,1)='integer'
    gfile%arycval(6*gfile%dimz+10,1)='m     '
    gfile%arycval(6*gfile%dimz+11,1)='K     '
    gfile%arycval(6*gfile%dimz+12,1)='%     '

    if(.not.present(recname).or..not.present(reclevtyp).or..not.present(reclev) )then
!
     if(size(gfile%recname)==10+3*gfile%dimz+gfile%ntrac*gfile%dimz .and.   &
        size(gfile%reclevtyp)==10+3*gfile%dimz+gfile%ntrac*gfile%dimz .and. &
        size(gfile%reclev)==10+3*gfile%dimz+gfile%ntrac*gfile%dimz )then
      gfile%reclevtyp='sfc'
      gfile%reclev=ione
      gfile%recname(1)='hgt'
      gfile%recname(2)='pres'
      rec=2
      gfile%recname(rec+ione:rec+gfile%dimz)='tmp'
      gfile%reclevtyp(rec+ione:rec+gfile%dimz)='mid layer'
      gfile%recname(rec+gfile%dimz+ione:rec+2*gfile%dimz)='ugrd'
      gfile%reclevtyp(rec+gfile%dimz+ione:rec+2*gfile%dimz)='mid layer'
      gfile%recname(rec+2*gfile%dimz+ione:rec+3*gfile%dimz)='vgrd'
      gfile%reclevtyp(rec+2*gfile%dimz+ione:rec+3*gfile%dimz)='mid layer'
      do  i=1,3
       do j=1,gfile%dimz
         gfile%reclev(rec+(i-ione)*gfile%dimz+j)=j
       enddo
      enddo
      do i=1,gfile%ntrac
       if ( i.eq.ione) gfile%recname(rec+(2+i)*gfile%dimz+ione:rec+(3+i)*gfile%dimz)='spfh'
       if ( i.eq.ione) gfile%reclevtyp(rec+(2+i)*gfile%dimz+ione:rec+(3+i)*gfile%dimz)='mid layer'
       if ( i.eq.2) gfile%recname(rec+(2+i)*gfile%dimz+ione:rec+(3+i)*gfile%dimz)='o3mr'
       if ( i.eq.2) gfile%reclevtyp(rec+(2+i)*gfile%dimz+ione:rec+(3+i)*gfile%dimz)='mid layer'
       if ( i.eq.3) gfile%recname(rec+(2+i)*gfile%dimz+ione:rec+(3+i)*gfile%dimz)='clwmr'
       if ( i.eq.3) gfile%reclevtyp(rec+(2+i)*gfile%dimz+ione:rec+(3+i)*gfile%dimz)='mid layer'
       do j=1,gfile%dimz
         gfile%reclev(rec+(2+i)*gfile%dimz+j)=j
       enddo
      enddo
      rec=rec+3*gfile%dimz+gfile%ntrac*gfile%dimz
      gfile%recname(rec+ione)='f10m'
      gfile%recname(rec+2)='tsea'
      gfile%recname(rec+3)='sheleg'
      gfile%recname(rec+4)='vtype'
      gfile%recname(rec+5)='vfrac'
      gfile%recname(rec+6)='stype'
      gfile%recname(rec+7)='slmsk'
      gfile%recname(rec+8)='zorl'
      gfile%recname(rec+9)='stc'
      gfile%recname(rec+10)='smc'
      gfile%reclevtyp(rec+9:rec+10)='soil layer'
     endif
!
    endif
   endif
!jw   print *,' end of gfinit'
!
   iret=izero
  end subroutine nemsio_gfinit
!------------------------------------------------------------------------------

  subroutine nemsio_stop()
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    nemsio_stop
!   prgmmr:
!
! abstract: stop
!
! program history log:
!   2009-08-31  lueken - added subprogram doc block
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    implicit none
     stop
  end subroutine nemsio_stop
!------------------------------------------------------------------------------
!  temporary subroutines for basio file unit
    subroutine nemsio_getlu(gfile,gfname,gaction,iret)
!$$$  subprogram documentation block
!                .      .    .                                        .
! subprogram:    nemsio_getlu
!   prgmmr:
!
! abstract: set unit number to the first number available between 600-699
!           according to unit number array fileunit
!
! program history log:
!   2009-08-31  lueken - added subprogram doc block
!
!   input argument list:
!    gfile
!    gfname,gaction
!
!   output argument list:
!    gfile
!    iret
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

      implicit none
     type(nemsio_gfile),intent (inout) :: gfile
     character*(*),intent(in)          :: gfname,gaction
     integer(i_kind),intent(out) :: iret
     integer(i_kind)             :: i
     iret=-10
     gfile%gfname=gfname
     gfile%gaction=gaction
     do i=600,699
       if ( fileunit(i) .eq. izero ) then 
         gfile%flunit=i
         fileunit(i)=i
         iret=izero
         exit
       endif
     enddo
    end subroutine nemsio_getlu
!------------------------------------------------------------------------------
!  temporary subroutines for free unit number 
    subroutine nemsio_clslu(gfile,iret)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    nemsio_clslu
!   prgmmr:
!
! abstract: free unit number array index corresponding to unit number
!
! program history log:
!   2009-08-31  lueken - added subprogram doc block
!
!   input argument list:
!    gfile
!
!   output argument list:
!    gfile
!    iret
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

      implicit none
     type(nemsio_gfile),intent (inout) :: gfile
     integer(i_kind), intent(out)      :: iret
     iret=-10
     if ( fileunit(gfile%flunit) .ne. izero ) then
       fileunit(gfile%flunit)=izero
       gfile%flunit=izero
       iret=izero
     endif
    end subroutine nemsio_clslu
!------------------------------------------------------------------------------

      SUBROUTINE nemsio_splat4(IDRT,JMAX,ASLAT)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    nemsio_splat4
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-31  lueken - added subprogram doc block
!
!   input argument list:
!    idrt,jmax
!
!   output argument list:
!    ASLAT
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

      implicit none
      integer(i_kind),intent(in) :: idrt,jmax
      real(r_single),intent(out) :: ASLAT(JMAX)
      INTEGER(i_kind),PARAMETER:: KD=SELECTED_REAL_KIND(15,45)
      REAL(KIND=KD):: PK(JMAX/2),PKM1(JMAX/2),PKM2(JMAX/2)
      REAL(KIND=KD):: ASLATD(JMAX/2),SP,SPMAX,EPS=10.*EPSILON(SP)
      integer(i_kind),PARAMETER:: JZ=50
      REAL(r_kind) BZ(JZ)
      DATA BZ        / 2.4048255577,  5.5200781103, &
       8.6537279129, 11.7915344391, 14.9309177086, 18.0710639679, &
      21.2116366299, 24.3524715308, 27.4934791320, 30.6346064684, &
      33.7758202136, 36.9170983537, 40.0584257646, 43.1997917132, &
      46.3411883717, 49.4826098974, 52.6240518411, 55.7655107550, &
      58.9069839261, 62.0484691902, 65.1899648002, 68.3314693299, &
      71.4729816036, 74.6145006437, 77.7560256304, 80.8975558711, &
      84.0390907769, 87.1806298436, 90.3221726372, 93.4637187819, &
      96.6052679510, 99.7468198587, 102.888374254, 106.029930916, &
      109.171489649, 112.313050280, 115.454612653, 118.596176630, &
      121.737742088, 124.879308913, 128.020877005, 131.162446275, &
      134.304016638, 137.445588020, 140.587160352, 143.728733573, &
      146.870307625, 150.011882457, 153.153458019, 156.295034268 /
      REAL(r_kind):: DLT
      INTEGER(i_kind):: JHE,JHO
      real(r_kind),PARAMETER :: PI=3.14159265358979,C=(one-(two/PI)**2)*quarter
      real(r_kind) r
      integer(i_kind) jh,n,j
!C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!C  GAUSSIAN LATITUDES
!      print *,'nemsio_module,in SPLAT4',IDRT,JMAX
      IF(IDRT.EQ.4) THEN
        JH=JMAX/2
        JHE=(JMAX+ione)/2
        R=one/SQRT((JMAX+half)**2+C)
        DO J=1,MIN(JH,JZ)
          ASLATD(J)=COS(BZ(J)*R)
        ENDDO
        DO J=JZ+ione,JH
          ASLATD(J)=COS((BZ(JZ)+(J-JZ)*PI)*R)
        ENDDO
        SPMAX=one
        DO WHILE(SPMAX.GT.EPS)
          SPMAX=zero
          DO J=1,JH
            PKM1(J)=one
            PK(J)=ASLATD(J)
          ENDDO
          DO N=2,JMAX
            DO J=1,JH
              PKM2(J)=PKM1(J)
              PKM1(J)=PK(J)
              PK(J)=((2*N-ione)*ASLATD(J)*PKM1(J)-(N-ione)*PKM2(J))/N
            ENDDO
          ENDDO
          DO J=1,JH
            SP=PK(J)*(one-ASLATD(J)**2)/(JMAX*(PKM1(J)-ASLATD(J)*PK(J)))
            ASLATD(J)=ASLATD(J)-SP
            SPMAX=MAX(SPMAX,ABS(SP))
          ENDDO
        ENDDO
!CDIR$ IVDEP
        DO J=1,JH
          ASLAT(J)=ASLATD(J)
          ASLAT(JMAX+ione-J)=-ASLAT(J)
        ENDDO
        IF(JHE.GT.JH) THEN
          ASLAT(JHE)=zero
        ENDIF
!C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!C  EQUALLY-SPACED LATITUDES INCLUDING POLES
      ELSEIF(IDRT.EQ.izero) THEN
        JH=JMAX/2
        JHE=(JMAX+ione)/2
        JHO=JHE-ione
        DLT=PI/(JMAX-ione)
        ASLAT(1)=one
        DO J=2,JH
          ASLAT(J)=COS((J-ione)*DLT)
        ENDDO
!CDIR$ IVDEP
        DO J=1,JH
          ASLAT(JMAX+ione-J)=-ASLAT(J)
        ENDDO
        IF(JHE.GT.JH) THEN
          ASLAT(JHE)=zero
        ENDIF
!C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!C  EQUALLY-SPACED LATITUDES EXCLUDING POLES
      ELSEIF(IDRT.EQ.256) THEN
        JH=JMAX/2
        JHE=(JMAX+ione)/2
        JHO=JHE
        DLT=PI/JMAX
        ASLAT(1)=one
        DO J=1,JH
          ASLAT(J)=COS((J-half)*DLT)
        ENDDO
!CDIR$ IVDEP
        DO J=1,JH
          ASLAT(JMAX+ione-J)=-ASLAT(J)
        ENDDO
        IF(JHE.GT.JH) THEN
          ASLAT(JHE)=zero
        ENDIF
      ENDIF
!C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     end subroutine nemsio_splat4
!----------------------------------------------------------------------

      SUBROUTINE nemsio_splat8(IDRT,JMAX,ASLAT)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    nemsio_splat8
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-31  lueken - added subprogram doc block
!
!   input argument list:
!    idrt,jmax
!
!   output argument list:
!    ASLAT
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

      implicit none
      integer(i_kind),intent(in) :: idrt,jmax
      real(r_kind),intent(out) :: ASLAT(JMAX)
      INTEGER(i_kind),PARAMETER:: KD=SELECTED_REAL_KIND(15,45)
      REAL(KIND=KD):: PK(JMAX/2),PKM1(JMAX/2),PKM2(JMAX/2)
      REAL(KIND=KD):: ASLATD(JMAX/2),SP,SPMAX,EPS=10.*EPSILON(SP)
      integer(i_kind),PARAMETER:: JZ=50
      REAL(r_kind) BZ(JZ)
      DATA BZ        / 2.4048255577,  5.5200781103, &
       8.6537279129, 11.7915344391, 14.9309177086, 18.0710639679, &
      21.2116366299, 24.3524715308, 27.4934791320, 30.6346064684, &
      33.7758202136, 36.9170983537, 40.0584257646, 43.1997917132, &
      46.3411883717, 49.4826098974, 52.6240518411, 55.7655107550, &
      58.9069839261, 62.0484691902, 65.1899648002, 68.3314693299, &
      71.4729816036, 74.6145006437, 77.7560256304, 80.8975558711, &
      84.0390907769, 87.1806298436, 90.3221726372, 93.4637187819, &
      96.6052679510, 99.7468198587, 102.888374254, 106.029930916, &
      109.171489649, 112.313050280, 115.454612653, 118.596176630, &
      121.737742088, 124.879308913, 128.020877005, 131.162446275, &
      134.304016638, 137.445588020, 140.587160352, 143.728733573, &
      146.870307625, 150.011882457, 153.153458019, 156.295034268 /
      REAL(r_kind):: DLT
      INTEGER(i_kind):: JHE,JHO
      real(r_kind),PARAMETER :: PI=3.14159265358979,C=(one-(two/PI)**2)*quarter
      real(r_kind) r
      integer(i_kind) jh,n,j
!C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!C  GAUSSIAN LATITUDES
      IF(IDRT.EQ.4) THEN
        JH=JMAX/2
        JHE=(JMAX+ione)/2
        R=one/SQRT((JMAX+half)**2+C)
        DO J=1,MIN(JH,JZ)
          ASLATD(J)=COS(BZ(J)*R)
        ENDDO
        DO J=JZ+ione,JH
          ASLATD(J)=COS((BZ(JZ)+(J-JZ)*PI)*R)
        ENDDO
        SPMAX=one
        DO WHILE(SPMAX.GT.EPS)
          SPMAX=zero
          DO J=1,JH
            PKM1(J)=one
            PK(J)=ASLATD(J)
          ENDDO
          DO N=2,JMAX
            DO J=1,JH
              PKM2(J)=PKM1(J)
              PKM1(J)=PK(J)
              PK(J)=((2*N-ione)*ASLATD(J)*PKM1(J)-(N-ione)*PKM2(J))/N
            ENDDO
          ENDDO
          DO J=1,JH
            SP=PK(J)*(one-ASLATD(J)**2)/(JMAX*(PKM1(J)-ASLATD(J)*PK(J)))
            ASLATD(J)=ASLATD(J)-SP
            SPMAX=MAX(SPMAX,ABS(SP))
          ENDDO
        ENDDO
!CDIR$ IVDEP
        DO J=1,JH
          ASLAT(J)=ASLATD(J)
          ASLAT(JMAX+ione-J)=-ASLAT(J)
        ENDDO
        IF(JHE.GT.JH) THEN
          ASLAT(JHE)=zero
        ENDIF
!C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!C  EQUALLY-SPACED LATITUDES INCLUDING POLES
      ELSEIF(IDRT.EQ.izero) THEN
        JH=JMAX/2
        JHE=(JMAX+ione)/2
        JHO=JHE-ione
        DLT=PI/(JMAX-ione)
        ASLAT(1)=one
        DO J=2,J
          ASLAT(J)=COS((J-ione)*DLT)
        ENDDO
!CDIR$ IVDEP
        DO J=1,JH
          ASLAT(JMAX+ione-J)=-ASLAT(J)
        ENDDO
        IF(JHE.GT.JH) THEN
          ASLAT(JHE)=zero
        ENDIF
!C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!C  EQUALLY-SPACED LATITUDES EXCLUDING POLES
      ELSEIF(IDRT.EQ.256) THEN
        JH=JMAX/2
        JHE=(JMAX+ione)/2
        JHO=JHE
        DLT=PI/JMAX
        ASLAT(1)=one
        DO J=1,JH
          ASLAT(J)=COS((J-half)*DLT)
        ENDDO
!DIR$ IVDEP
        DO J=1,JH
          ASLAT(JMAX+ione-J)=-ASLAT(J)
        ENDDO
        IF(JHE.GT.JH) THEN
          ASLAT(JHE)=zero
        ENDIF
      ENDIF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     end subroutine nemsio_splat8
!------------------------------------------------------------------------------
!
     elemental function equal_str_nocase(str1,str2)
!$$$  subprogram documentation block
!                .      .    .                                        .
! subprogram:    equal_str_nocase
!   prgmmr:
!
! abstract: convert a word to lower case
!
! program history log:
!   2009-08-31  lueken - added subprogram doc block
!
!   input argument list:
!    str1
!    str2
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

      implicit none
      logical              :: equal_str_nocase
      Character (len=*) , intent(in) :: str1
      Character (len=*) , intent(in) :: str2
      integer(i_kind)                :: i,ic1,ic2,nlen
      nlen = len(str2)
!
      if(len(str1)/=nlen)  then
        equal_str_nocase=.false.
        return
      endif
      equal_str_nocase=.false.
      do i=1,nlen
        ic1 = ichar(str1(i:i))
        if (ic1 >= 65 .and. ic1 < 91) ic1 = ic1+32
        ic2 = ichar(str2(i:i))
        if (ic2 >= 65 .and. ic2 < 91) ic2 = ic2+32
        if(ic1/=ic2) then
           equal_str_nocase=.false.
           return
        endif
      end do
      equal_str_nocase=.true.
!
!-----------------------------------------------------------------------
!
      end function equal_str_nocase
!
!-----------------------------------------------------------------------
! 
     elemental function lowercase(word)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    lowercase
!   prgmmr:
!
! abstract: convert a word to lower case
!
! program history log:
!   2009-08-31  lueken - added subprogram doc block
!
!   input argument list:
!    word
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

      implicit none
      Character (len=32)              :: lowercase
      Character (len=*) , intent(in)  :: word
      integer(i_kind)                 :: i,ic,nlen
      nlen = len(word)
      if(nlen >32) then
        nlen=32
      endif
      lowercase(1:nlen)=word(1:nlen)
      do i=1,nlen
        ic = ichar(word(i:i))
        if (ic >= 65 .and. ic < 91) lowercase(i:i) = char(ic+32)
      end do
      if(nlen<32) lowercase(nlen+ione:)=' '
!
!-----------------------------------------------------------------------
!
      end function lowercase
!
!----------------------------------------------------------------------
  end module nemsio_module

!<><><>    add baciof.f here (from /climate/save/wx20wa/gfsio/bacio/sorc)
!-----------------------------------------------------------------------
      MODULE BACIO_MODULE
!$$$  F90-MODULE DOCUMENTATION BLOCK
!
! F90-MODULE: BACIO_MODULE   BYTE-ADDRESSABLE I/O MODULE
!   PRGMMR: IREDELL          ORG: NP23        DATE: 98-06-04
!
! ABSTRACT: MODULE TO SHARE FILE DESCRIPTORS
!   IN THE BYTE-ADDRESSABLE I/O PACKAGE.
!
! PROGRAM HISTORY LOG:
!   98-06-04  IREDELL
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!
!$$$
      use kinds, only: i_kind
      use constants, only: ione
      implicit none

      INTEGER(i_kind),EXTERNAL:: BACIO,BACIOL
      INTEGER(i_kind),DIMENSION(999),SAVE:: FD=999*0
      INTEGER(i_kind),DIMENSION(20),SAVE:: BAOPTS=0
  !   INCLUDE 'baciof.h'
      !><><< insert bacio.f manually here (from /climate/save/wx20wa/gfsio/bacio/sorc)
!     Include file to define variables for Fortran to C interface(s)
!     Robert Grumbine 16 March 1998
      INTEGER(i_kind),PARAMETER:: BACIO_OPENR=ione   ! Open file for read only
      INTEGER(i_kind),PARAMETER:: BACIO_OPENW=2      ! Open file for write only
      INTEGER(i_kind),PARAMETER:: BACIO_OPENRW=4     ! Open file for read or write
      INTEGER(i_kind),PARAMETER:: BACIO_CLOSE=8      ! Close file
      INTEGER(i_kind),PARAMETER:: BACIO_READ=16      ! Read from the file
      INTEGER(i_kind),PARAMETER:: BACIO_WRITE=32     ! Write to the file
      INTEGER(i_kind),PARAMETER:: BACIO_NOSEEK=64    ! Start I/O from previous spot
      INTEGER(i_kind),PARAMETER:: BACIO_OPENWT=128   ! Open for write only with truncation
      INTEGER(i_kind),PARAMETER:: BACIO_OPENWA=256   ! Open for write only with append
      END
!-----------------------------------------------------------------------

      SUBROUTINE BASETO(NOPT,VOPT)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM: BASETO         BYTE-ADDRESSABLE SET OPTIONS
!   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 1998-06-04
!
! ABSTRACT: SET OPTIONS FOR BYTE-ADDRESSABLE I/O.
!   ALL OPTIONS DEFAULT TO 0.
!   OPTION 1: BLOCKED READING OPTION
!             IF THE OPTION VALUE IS 1, THEN THE READING IS BLOCKED
!             INTO FOUR 4096-BYTE BUFFERS.  THIS MAY BE EFFICIENT IF
!             THE READS WILL BE REQUESTED IN MUCH SMALLER CHUNKS.
!             OTHERWISE, EACH CALL TO BAREAD INITIATES A PHYSICAL READ.
!
! PROGRAM HISTORY LOG:
!   1998-06-04  IREDELL
!
! USAGE:    CALL BASETO(NOPT,VOPT)
!   INPUT ARGUMENTS:
!     NOPT         INTEGER OPTION NUMBER
!     VOPT         INTEGER OPTION VALUE
!
! MODULES USED:
!   BACIO_MODULE   BYTE-ADDRESSABLE I/O FORTRAN INTERFACE
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!
!$$$
      use kinds, only: i_kind
      use constants, only: ione
      USE BACIO_MODULE
      implicit none

      INTEGER(i_kind), intent(in) :: NOPT,VOPT
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IF(NOPT.GE.ione.AND.NOPT.LE.20) BAOPTS(NOPT)=VOPT
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
!-----------------------------------------------------------------------
      SUBROUTINE BAOPEN(LU,CFN,IRET)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM: BAOPEN         BYTE-ADDRESSABLE OPEN
!   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 1998-06-04
!
! ABSTRACT: OPEN A BYTE-ADDRESSABLE FILE.
!
! PROGRAM HISTORY LOG:
!   1998-06-04  IREDELL
!
! USAGE:    CALL BAOPEN(LU,CFN,IRET)
!   INPUT ARGUMENTS:
!     LU           INTEGER UNIT TO OPEN
!     CFN          CHARACTER FILENAME TO OPEN
!                  (CONSISTING OF NONBLANK PRINTABLE CHARACTERS)
!   OUTPUT ARGUMENTS:
!     IRET         INTEGER RETURN CODE
!
! MODULES USED:
!   BACIO_MODULE   BYTE-ADDRESSABLE I/O FORTRAN INTERFACE
!
! SUBPROGRAMS CALLED:
!   BACIO          BYTE-ADDRESSABLE I/O C PACKAGE
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!
!$$$

      use kinds, only: i_kind,i_llong
      use constants, only: ione
      USE BACIO_MODULE
      implicit none
      integer(i_kind), intent(in) :: LU
      CHARACTER, intent(in) :: CFN*(*)
      integer(i_kind), intent(out) :: IRET
      CHARACTER(80) CMSG
      integer(i_llong) IB,JB,NB,KA
      CHARACTER A
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IF(LU.LT.001.OR.LU.GT.999) THEN
        IRET=6
        RETURN
      ENDIF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IRET=BACIOL(BACIO_OPENRW,IB,JB,ione,NB,KA,FD(LU),CFN,A)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
!-----------------------------------------------------------------------

      SUBROUTINE BAOPENR(LU,CFN,IRET)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM: BAOPENR        BYTE-ADDRESSABLE OPEN
!   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 1998-06-04
!
! ABSTRACT: OPEN A BYTE-ADDRESSABLE FILE FOR READ ONLY.
!
! PROGRAM HISTORY LOG:
!   1998-06-04  IREDELL
!
! USAGE:    CALL BAOPENR(LU,CFN,IRET)
!   INPUT ARGUMENTS:
!     LU           INTEGER UNIT TO OPEN
!     CFN          CHARACTER FILENAME TO OPEN
!                  (CONSISTING OF NONBLANK PRINTABLE CHARACTERS)
!   OUTPUT ARGUMENTS:
!     IRET         INTEGER RETURN CODE
!
! MODULES USED:
!   BACIO_MODULE   BYTE-ADDRESSABLE I/O FORTRAN INTERFACE
!
! SUBPROGRAMS CALLED:
!   BACIO          BYTE-ADDRESSABLE I/O C PACKAGE
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!
!$$$
      use kinds, only: i_kind,i_llong
      use constants, only: ione
      USE BACIO_MODULE
      implicit none
      CHARACTER, intent(in) :: CFN*(*)
      INTEGER(i_kind), intent(in) :: LU
      INTEGER(i_kind), intent(out) :: iret
      integer(i_llong) IB,JB,NB,KA
      CHARACTER A
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IF(LU.LT.001.OR.LU.GT.999) THEN
      print *,'in baopenr, lu=',lu
        IRET=6
        RETURN
      ENDIF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      print *,'in baopenr, before call baciol'
      IRET=BACIOL(BACIO_OPENR,IB,JB,ione,NB,KA,FD(LU),CFN,A)
      print *,'in baopenr, iret=',iret
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
!-----------------------------------------------------------------------

      SUBROUTINE BAOPENW(LU,CFN,IRET)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM: BAOPENW        BYTE-ADDRESSABLE OPEN
!   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 1998-06-04
!
! ABSTRACT: OPEN A BYTE-ADDRESSABLE FILE FOR WRITE ONLY.
!
! PROGRAM HISTORY LOG:
!   1998-06-04  IREDELL
!
! USAGE:    CALL BAOPENW(LU,CFN,IRET)
!   INPUT ARGUMENTS:
!     LU           INTEGER UNIT TO OPEN
!     CFN          CHARACTER FILENAME TO OPEN
!                  (CONSISTING OF NONBLANK PRINTABLE CHARACTERS)
!   OUTPUT ARGUMENTS:
!     IRET         INTEGER RETURN CODE
!
! MODULES USED:
!   BACIO_MODULE   BYTE-ADDRESSABLE I/O FORTRAN INTERFACE
!
! SUBPROGRAMS CALLED:
!   BACIO          BYTE-ADDRESSABLE I/O C PACKAGE
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!
!$$$
      use kinds, only: i_kind,i_llong
      use constants, only: ione
      USE BACIO_MODULE
      implicit none
      CHARACTER, intent(in) :: CFN*(*)
      INTEGER(i_kind), intent(in) :: LU
      INTEGER(i_kind), intent(out) :: iret
      integer(i_llong) IB,JB,NB,KA
      CHARACTER A
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IF(LU.LT.001.OR.LU.GT.999) THEN
        IRET=6
        RETURN
      ENDIF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IRET=BACIOL(BACIO_OPENW,IB,JB,ione,NB,KA,FD(LU),CFN,A)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
!-----------------------------------------------------------------------

      SUBROUTINE BAOPENWT(LU,CFN,IRET)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM: BAOPENWT       BYTE-ADDRESSABLE OPEN
!   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 1998-06-04
!
! ABSTRACT: OPEN A BYTE-ADDRESSABLE FILE FOR WRITE ONLY WITH TRUNCATION.
!
! PROGRAM HISTORY LOG:
!   1998-06-04  IREDELL
!
! USAGE:    CALL BAOPENWT(LU,CFN,IRET)
!   INPUT ARGUMENTS:
!     LU           INTEGER UNIT TO OPEN
!     CFN          CHARACTER FILENAME TO OPEN
!                  (CONSISTING OF NONBLANK PRINTABLE CHARACTERS)
!   OUTPUT ARGUMENTS:
!     IRET         INTEGER RETURN CODE
!
! MODULES USED:
!   BACIO_MODULE   BYTE-ADDRESSABLE I/O FORTRAN INTERFACE
!
! SUBPROGRAMS CALLED:
!   BACIO          BYTE-ADDRESSABLE I/O C PACKAGE
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!
!$$$
      use kinds, only: i_kind,i_llong
      use constants, only: ione
      USE BACIO_MODULE
      implicit none
      CHARACTER, intent(in) :: CFN*(*)
      INTEGER(i_kind), intent(in) :: LU
      INTEGER(i_kind), intent(out) :: iret
      integer(i_llong) IB,JB,NB,KA
      CHARACTER A
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IF(LU.LT.001.OR.LU.GT.999) THEN
        IRET=6
        RETURN
      ENDIF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IRET=BACIOL(BACIO_OPENWT,IB,JB,ione,NB,KA,FD(LU),CFN,A)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
!-----------------------------------------------------------------------

      SUBROUTINE BAOPENWA(LU,CFN,IRET)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM: BAOPENWA       BYTE-ADDRESSABLE OPEN
!   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 1998-06-04
!
! ABSTRACT: OPEN A BYTE-ADDRESSABLE FILE FOR WRITE ONLY WITH APPEND.
!
! PROGRAM HISTORY LOG:
!   1998-06-04  IREDELL
!
! USAGE:    CALL BAOPENWA(LU,CFN,IRET)
!   INPUT ARGUMENTS:
!     LU           INTEGER UNIT TO OPEN
!     CFN          CHARACTER FILENAME TO OPEN
!                  (CONSISTING OF NONBLANK PRINTABLE CHARACTERS)
!   OUTPUT ARGUMENTS:
!     IRET         INTEGER RETURN CODE
!
! MODULES USED:
!   BACIO_MODULE   BYTE-ADDRESSABLE I/O FORTRAN INTERFACE
!
! SUBPROGRAMS CALLED:
!   BACIO          BYTE-ADDRESSABLE I/O C PACKAGE
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!
!$$$
      use kinds, only: i_kind,i_llong
      use constants, only: ione
      USE BACIO_MODULE
      implicit none
      CHARACTER, intent(in) :: CFN*(*)
      INTEGER(i_kind), intent(in) :: LU
      INTEGER(i_kind), intent(out) :: iret
      integer(i_llong) IB,JB,NB,KA
      CHARACTER A
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IF(LU.LT.001.OR.LU.GT.999) THEN
        IRET=6
        RETURN
      ENDIF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IRET=BACIOL(BACIO_OPENWA,IB,JB,ione,NB,KA,FD(LU),CFN,A)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
!-----------------------------------------------------------------------

      SUBROUTINE BACLOSE(LU,IRET)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM: BACLOSE        BYTE-ADDRESSABLE CLOSE
!   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 1998-06-04
!
! ABSTRACT: CLOSE A BYTE-ADDRESSABLE FILE.
!
! PROGRAM HISTORY LOG:
!   1998-06-04  IREDELL
!
! USAGE:    CALL BACLOSE(LU,IRET)
!   INPUT ARGUMENTS:
!     LU           INTEGER UNIT TO CLOSE
!   OUTPUT ARGUMENTS:
!     IRET         INTEGER RETURN CODE
!
! MODULES USED:
!   BACIO_MODULE   BYTE-ADDRESSABLE I/O FORTRAN INTERFACE
!
! SUBPROGRAMS CALLED:
!   BACIO          BYTE-ADDRESSABLE I/O C PACKAGE
!
! REMARKS:  A BAOPEN MUST HAVE ALREADY BEEN CALLED.
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!
!$$$
      use kinds, only: i_kind,i_llong
      use constants, only: izero,ione
      USE BACIO_MODULE
      implicit none
      INTEGER(i_kind), intent(in) :: LU
      INTEGER(i_kind), intent(out) :: iret
      integer(i_llong) IB,JB,NB,KA
      CHARACTER A
      CHARACTER CFN
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IF(LU.LT.001.OR.LU.GT.999) THEN
        IRET=6
        RETURN
      ENDIF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IRET=BACIOL(BACIO_CLOSE,IB,JB,ione,NB,KA,FD(LU),CFN,A)
      IF(IRET.EQ.izero) FD(LU)=izero
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
!-----------------------------------------------------------------------

      SUBROUTINE BAREAD(LU,IB,NB,KA,A)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    baread
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-09-01  lueken - added subprogram doc block
!
!   input argument list:
!    LU,IB,NB
!
!   output argument list:
!    KA
!    A
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

        use kinds, only: i_kind,i_llong
        use constants, only: izero
        IMPLICIT NONE
        INTEGER(i_kind),INTENT(IN) :: LU,IB,NB
        INTEGER(i_kind),INTENT(OUT) :: KA
        CHARACTER,INTENT(OUT) :: A(NB)
        INTEGER(i_llong) :: LONG_IB,LONG_NB,LONG_KA
!
        if(IB<izero .or. NB<izero ) THEN
          print *,'WRONG: in BAFRREAD starting postion IB or read '//    &
         'data size NB < 0, STOP! '//                                    &
         'Consider using bafreadl and long integer'
          KA=izero
          return
        ENDIF
        LONG_IB=IB
        LONG_NB=NB
        CALL BAREADL(LU,LONG_IB,LONG_NB,LONG_KA,A)
        KA=LONG_KA

      END SUBROUTINE BAREAD
!-----------------------------------------------------------------------

      SUBROUTINE BAREADL(LU,IB,NB,KA,A)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM: BAREAD         BYTE-ADDRESSABLE READ
!   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 1998-06-04
!
! ABSTRACT: READ A GIVEN NUMBER OF BYTES FROM AN UNBLOCKED FILE,
!   SKIPPING A GIVEN NUMBER OF BYTES.
!   THE PHYSICAL I/O IS BLOCKED INTO FOUR 4096-BYTE BUFFERS
!   IF THE BYTE-ADDRESSABLE OPTION 1 HAS BEEN SET TO 1 BY BASETO.
!   THIS BUFFERED READING IS INCOMPATIBLE WITH NO-SEEK READING.
!
! PROGRAM HISTORY LOG:
!   1998-06-04  IREDELL
!
! USAGE:    CALL BAREAD(LU,IB,NB,KA,A)
!   INPUT ARGUMENTS:
!     LU           INTEGER UNIT TO READ
!     IB           INTEGER NUMBER OF BYTES TO SKIP
!                  (IF IB<0, THEN THE FILE IS ACCESSED WITH NO SEEKING)
!     NB           INTEGER NUMBER OF BYTES TO READ
!   OUTPUT ARGUMENTS:
!     KA           INTEGER NUMBER OF BYTES ACTUALLY READ
!     A            CHARACTER*1 (NB) DATA READ
!
! MODULES USED:
!   BACIO_MODULE   BYTE-ADDRESSABLE I/O FORTRAN INTERFACE
!
! SUBPROGRAMS CALLED:
!   BACIO          BYTE-ADDRESSABLE I/O C PACKAGE
!
! REMARKS:  A BAOPEN MUST HAVE ALREADY BEEN CALLED.
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!
!$$$
      use kinds, only: i_kind,i_llong
      use constants, only: izero,ione
      USE BACIO_MODULE
!    
      IMPLICIT NONE
      INTEGER(i_kind),intent(in)  :: LU
      INTEGER(i_llong),intent(in)  :: IB,NB
      INTEGER(i_llong),intent(out) :: KA
      CHARACTER,intent(out)       :: A(NB)
      CHARACTER CFN
      integer(i_llong),PARAMETER :: NY=4096,MY=4
      INTEGER(i_llong) NS(MY),NN(MY)
      INTEGER(i_llong) JB,LONG_0,KY,I,K,IY,JY,LUX
      INTEGER(i_kind) IRET
!      INTEGER LU,IB,NB,KA
      CHARACTER Y(NY,MY)
      DATA LUX/0/
      SAVE JY,NS,NN,Y,LUX
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IF(FD(LU).LE.izero) THEN
        KA=izero
        RETURN
      ENDIF
      IF(IB.LT.izero.AND.BAOPTS(1).EQ.ione) THEN
        KA=izero
        RETURN
      ENDIF
      IF(NB.LE.izero) THEN
        KA=izero
        RETURN
      ENDIF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      LONG_0=izero                                                         !jw
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  UNBUFFERED I/O
      IF(BAOPTS(1).NE.ione) THEN
        IF(IB.GE.izero) THEN
          IRET=BACIOL(BACIO_READ,IB,JB,ione,NB,KA,FD(LU),CFN,A)
        ELSE
!          IRET=BACIOL(BACIO_READ+BACIO_NOSEEK,izero,JB,ione,NB,KA,FD(LU),CFN,A)
          IRET=BACIOL(BACIO_READ+BACIO_NOSEEK,LONG_0,JB,ione,NB,KA,        &
                      FD(LU),CFN,A)
        ENDIF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  BUFFERED I/O
!  GET DATA FROM PREVIOUS CALL IF POSSIBLE
      ELSE
        KA=izero
        IF(LUX.NE.LU) THEN
          JY=izero
          NS=izero
          NN=izero
        ELSE
          DO I=1,MY
            IY=MOD(JY+I-ione,MY)+ione
            KY=IB+KA-NS(IY)
            IF(KA.LT.NB.AND.KY.GE.LONG_0.AND.KY.LT.NN(IY)) THEN
              K=MIN(NB-KA,NN(IY)-KY)
              A(KA+ione:KA+K)=Y(KY+ione:KY+K,IY)
              KA=KA+K
            ENDIF
          ENDDO
        ENDIF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  SET POSITION AND READ BUFFER AND GET DATA
        IF(KA.LT.NB) THEN
          LUX=ABS(LU)
          JY=MOD(JY,MY)+ione
          NS(JY)=IB+KA
          IRET=BACIOL(BACIO_READ,NS(JY),JB,ione,NY,NN(JY),  &
                     FD(LUX),CFN,Y(1,JY))
          IF(NN(JY).GT.izero) THEN
            K=MIN(NB-KA,NN(JY))
            A(KA+ione:KA+K)=Y(1:K,JY)
            KA=KA+K
          ENDIF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  CONTINUE TO READ BUFFER AND GET DATA
          DO WHILE(NN(JY).EQ.NY.AND.KA.LT.NB)
            JY=MOD(JY,MY)+ione
            NS(JY)=NS(JY)+NN(JY)
            IRET=BACIOL(BACIO_READ+BACIO_NOSEEK,NS(JY),JB,ione,NY,NN(JY),  &
                       FD(LUX),CFN,Y(1,JY))
            IF(NN(JY).GT.izero) THEN
              K=MIN(NB-KA,NN(JY))
              A(KA+ione:KA+K)=Y(1:K,JY)
              KA=KA+K
            ENDIF
          ENDDO
        ENDIF
      ENDIF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END SUBROUTINE BAREADL
!-----------------------------------------------------------------------

      SUBROUTINE BAWRITE(LU,IB,NB,KA,A)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    bawrite
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-09-01  lueken - added subprogram doc block
!
!   input argument list:
!    LU,IB,NB
!    A
!
!   output argument list:
!    KA
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
        use kinds, only: i_kind,i_llong
        use constants, only: izero
        IMPLICIT NONE
        INTEGER(i_kind),INTENT(IN) :: LU,IB,NB
        INTEGER(i_kind),INTENT(OUT) :: KA
        CHARACTER,INTENT(IN) :: A(NB)
        INTEGER(i_llong) :: LONG_IB,LONG_NB,LONG_KA
!
        if(IB<izero .or. NB<izero ) THEN
          print *,'WRONG: in BAFRWRITEstarting postion IB or read '//    &
         'data size NB <0, STOP! ' //                                    &
         'Consider using bafrrwritel and long integer'
          KA=izero
          return
        ENDIF
!
        LONG_IB=IB
        LONG_NB=NB
        CALL BAWRITEL(LU,LONG_IB,LONG_NB,LONG_KA,A)
        KA=LONG_KA                                                     !jun

      END SUBROUTINE BAWRITE
!-----------------------------------------------------------------------

      SUBROUTINE BAWRITEL(LU,IB,NB,KA,A)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM: BAWRITE        BYTE-ADDRESSABLE WRITE
!   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 1998-06-04
!
! ABSTRACT: WRITE A GIVEN NUMBER OF BYTES TO AN UNBLOCKED FILE,
!   SKIPPING A GIVEN NUMBER OF BYTES.
!
! PROGRAM HISTORY LOG:
!   1998-06-04  IREDELL
!
! USAGE:    CALL BAWRITE(LU,IB,NB,KA,A)
!   INPUT ARGUMENTS:
!     LU           INTEGER UNIT TO WRITE
!     IB           INTEGER NUMBER OF BYTES TO SKIP
!                  (IF IB<0, THEN THE FILE IS ACCESSED WITH NO SEEKING)
!     NB           INTEGER NUMBER OF BYTES TO WRITE
!     A            CHARACTER*1 (NB) DATA TO WRITE
!   OUTPUT ARGUMENTS:
!     KA           INTEGER NUMBER OF BYTES ACTUALLY WRITTEN
!
! MODULES USED:
!   BACIO_MODULE   BYTE-ADDRESSABLE I/O FORTRAN INTERFACE
!
! SUBPROGRAMS CALLED:
!   BACIO          BYTE-ADDRESSABLE I/O C PACKAGE
!
! REMARKS:  A BAOPEN MUST HAVE ALREADY BEEN CALLED.
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!
!$$$
      use kinds, only: i_kind,i_llong
      use constants, only: izero,ione
      USE BACIO_MODULE
!
      IMPLICIT NONE
!
      INTEGER(i_kind),intent(in) :: LU
      INTEGER(i_llong),intent(in) :: IB,NB
      INTEGER(i_llong),intent(out):: KA
      CHARACTER,intent(in) ::  A(NB)
!
      CHARACTER CFN
      INTEGER(i_llong) :: JB,LONG_0
      INTEGER(i_kind) :: IRET
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IF(FD(LU).LE.izero) THEN
        KA=izero
        RETURN
      ENDIF
      IF(NB.LE.izero) THEN
        KA=izero
        RETURN
      ENDIF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      LONG_0=izero
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IF(IB.GE.izero) THEN
        IRET=BACIOL(BACIO_WRITE,IB,JB,ione,NB,KA,FD(LU),CFN,A)
      ELSE
!        IRET=BACIOL(BACIO_WRITE+BACIO_NOSEEK,izero,JB,ione,NB,KA,FD(LU),CFN,A)
        IRET=BACIOL(BACIO_WRITE+BACIO_NOSEEK,LONG_0,JB,ione,NB,KA,         &
                    FD(LU),CFN,A)
      ENDIF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END SUBROUTINE  BAWRITEL
!-----------------------------------------------------------------------

      SUBROUTINE WRYTE(LU,NB,A)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    wryte
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-09-01  lueken - added subprogram doc block
!
!   input argument list:
!    LU
!    NB
!    A
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
      use kinds, only: i_kind,i_llong
      use constants, only: izero
      USE BACIO_MODULE
!
      IMPLICIT NONE
!
      INTEGER(i_kind),intent(in) :: LU
      INTEGER(i_kind),intent(in) :: NB
      CHARACTER,intent(in) ::  A(NB)
      INTEGER(i_llong) :: LONG_NB
!
      IF(NB<izero) THEN
       PRINT *,'WRONG: NB: the number of bytes to write  <0, STOP!'
       RETURN
      ENDIF
      LONG_NB=NB
      print *,'in wryte,nb=',nb
      CALL WRYTEL(LU,LONG_NB,A)
!
      END SUBROUTINE WRYTE
!-----------------------------------------------------------------------

      SUBROUTINE WRYTEL(LU,NB,A)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM: WRYTE          WRITE DATA OUT BY BYTES
!   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 1998-06-04
!
! ABSTRACT: WRITE A GIVEN NUMBER OF BYTES TO AN UNBLOCKED FILE.
!
! PROGRAM HISTORY LOG:
!   92-10-31  IREDELL
!   95-10-31  IREDELL     WORKSTATION VERSION
!   1998-06-04  IREDELL   BACIO VERSION
!
! USAGE:    CALL WRYTE(LU,NB,A)
!   INPUT ARGUMENTS:
!     LU           INTEGER UNIT TO WHICH TO WRITE
!     NB           INTEGER NUMBER OF BYTES TO WRITE
!     A            CHARACTER*1 (NB) DATA TO WRITE
!
! MODULES USED:
!   BACIO_MODULE   BYTE-ADDRESSABLE I/O FORTRAN INTERFACE
!
! SUBPROGRAMS CALLED:
!   BACIO          BYTE-ADDRESSABLE I/O C PACKAGE
!
! REMARKS:  A BAOPEN MUST HAVE ALREADY BEEN CALLED.
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!
!$$$
      use kinds, only: i_kind,i_llong
      use constants, only: izero,ione
      USE BACIO_MODULE
!
      IMPLICIT NONE
      INTEGER(i_kind),intent(in) :: LU
      INTEGER(i_llong),intent(in) :: NB
      CHARACTER,INTENT(in)       :: A(NB)
      INTEGER(i_llong) :: LONG_0,JB,KA
      INTEGER(i_kind) :: IRET
      CHARACTER CFN
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IF(FD(LU).LE.izero) THEN
        RETURN
      ENDIF
      IF(NB.LE.izero) THEN
        RETURN
      ENDIF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      LONG_0=izero
      IRET=BACIOL(BACIO_WRITE+BACIO_NOSEEK,LONG_0,JB,ione,NB,KA,           &
                  FD(LU),CFN,A)
      print *,'in wrytel,nb=',nb,'jb=',jb,'ka=',ka,'iret=',iret
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END

!<><><>    add bafrio.f here (from /climate/save/wx20wa/gfsio/bacio/sorc)
!-----------------------------------------------------------------------

      SUBROUTINE BAFRINDEX(LU,IB,LX,IX)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    bafrindex
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-09-01  lueken - added subprogram doc block
!
!   input argument list:
!    LU,IB
!    LX
!
!   output argument list:
!    LX
!    IX
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
      use kinds, only: i_kind,i_llong
      IMPLICIT NONE
      INTEGER(i_kind),INTENT(IN):: LU,IB
      INTEGER(i_kind),INTENT(INOUT):: LX
      INTEGER(i_kind),INTENT(OUT):: IX
      integer(i_llong) :: LONG_IB,LONG_LX ,LONG_IX
!
      LONG_IB=IB
      LONG_LX=LX
      call BAFRINDEXL(LU,LONG_IB,LONG_LX,LONG_IX)
      LX=LONG_LX
      IX=LONG_IX

      return
      end SUBROUTINE BAFRINDEX
!-----------------------------------------------------------------------

      SUBROUTINE BAFRINDEXL(LU,IB,LX,IX)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM: BAFRINDEX      BYTE-ADDRESSABLE FORTRAN RECORD INDEX
!   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 1999-01-21
!
! ABSTRACT: THIS SUBPROGRAM EITHER READS AN UNFORMATTED FORTRAN RECORD
!   AND RETURN ITS LENGTH AND START BYTE OF THE NEXT FORTRAN RECORD;
!   OR GIVEN THE RECORD LENGTH, WITHOUT I/O IT DETERMINES THE START BYTE
!   OF THE NEXT FORTRAN RECORD.
!
! PROGRAM HISTORY LOG:
!   1999-01-21  IREDELL
!
! USAGE:    CALL BAFRINDEX(LU,IB,LX,IX)
!   INPUT ARGUMENTS:
!     LU           INTEGER LOGICAL UNIT TO READ
!                  IF LU<=0, THEN DETERMINE IX FROM LX
!     IB           INTEGER FORTRAN RECORD START BYTE
!                  (FOR THE FIRST FORTRAN RECORD, IB SHOULD BE 0)
!     LX           INTEGER RECORD LENGTH IN BYTES IF LU<=0
!
!   OUTPUT ARGUMENTS:
!     LX           INTEGER RECORD LENGTH IN BYTES IF LU>0,
!                  OR LX=-1 FOR I/O ERROR (PROBABLE END OF FILE),
!                  OR LX=-2 FOR I/O ERROR (INVALID FORTRAN RECORD)
!     IX           INTEGER START BYTE FOR THE NEXT FORTRAN RECORD
!                  (COMPUTED ONLY IF LX>=0)
!
! SUBPROGRAMS CALLED:
!   BAREAD         BYTE-ADDRESSABLE READ
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!
!$$$
      use kinds, only: i_kind,i_llong
      use constants, only: izero,ione
      IMPLICIT NONE
      INTEGER(i_kind),INTENT(IN):: LU
      INTEGER(i_llong),INTENT(IN):: IB
      INTEGER(i_llong),INTENT(INOUT):: LX
      INTEGER(i_llong),INTENT(OUT):: IX
      INTEGER(i_llong),PARAMETER:: LBCW=4
      INTEGER(i_kind):: BCW1,BCW2
      INTEGER(i_llong):: KR
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  COMPARE FIRST BLOCK CONTROL WORD AND TRAILING BLOCK CONTROL WORD
      IF(LU.GT.izero) THEN
        CALL BAREADL(LU,IB,LBCW,KR,BCW1)
        IF(KR.NE.LBCW) THEN
          LX=-ione
        ELSE
          CALL BAREADL(LU,IB+LBCW+BCW1,LBCW,KR,BCW2)
          IF(KR.NE.LBCW.OR.BCW1.NE.BCW2) THEN
            LX=-2
          ELSE
            LX=BCW1
          ENDIF
        ENDIF
      ENDIF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  COMPUTE START BYTE FOR THE NEXT FORTRAN RECORD
      IF(LX.GE.izero) IX=IB+LBCW+LX+LBCW
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END SUBROUTINE BAFRINDEXL
!-----------------------------------------------------------------------

      SUBROUTINE BAFRREAD(LU,IB,NB,KA,A)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    bafrread
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-09-01  lueken - added subprogram doc block
!
!   input argument list:
!    LU,IB,NB
!
!   output argument list:
!    KA
!    A
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
      use kinds, only: i_kind,i_llong
      use constants, only: izero
      IMPLICIT NONE
      INTEGER(i_kind),INTENT(IN):: LU,IB,NB
      INTEGER(i_kind),INTENT(OUT):: KA
      CHARACTER,INTENT(OUT):: A(NB)
      INTEGER(i_llong) :: LONG_IB,LONG_NB,LONG_KA
!
        if(IB<izero .or. NB<izero ) THEN
          print *,'WRONG: in BAFRREAD starting postion IB or read '//    &
       'data size NB < 0, STOP! Consider use bafreadl and long integer'
          KA=izero
          return
        ENDIF
        LONG_IB=IB
        LONG_NB=NB
        CALL BAFRREADL(LU,LONG_IB,LONG_NB,LONG_KA,A)
        KA=LONG_KA
      END SUBROUTINE BAFRREAD
!-----------------------------------------------------------------------

      SUBROUTINE BAFRREADL(LU,IB,NB,KA,A)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM: BAFRREAD       BYTE-ADDRESSABLE FORTRAN RECORD READ
!   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 1999-01-21
!
! ABSTRACT: THIS SUBPROGRAM READS AN UNFORMATTED FORTRAN RECORD
!
! PROGRAM HISTORY LOG:
!   1999-01-21  IREDELL
!
! USAGE:    CALL BAFRREAD(LU,IB,NB,KA,A)
!   INPUT ARGUMENTS:
!     LU           INTEGER LOGICAL UNIT TO READ
!     IB           INTEGER FORTRAN RECORD START BYTE
!                  (FOR THE FIRST FORTRAN RECORD, IB SHOULD BE 0)
!     NB           INTEGER NUMBER OF BYTES TO READ
!
!   OUTPUT ARGUMENTS:
!     KA           INTEGER NUMBER OF BYTES IN FORTRAN RECORD
!                  (IN WHICH CASE THE NEXT FORTRAN RECORD
!                  SHOULD HAVE A START BYTE OF IB+KA),
!                  OR KA=-1 FOR I/O ERROR (PROBABLE END OF FILE),
!                  OR KA=-2 FOR I/O ERROR (INVALID FORTRAN RECORD),
!                  OR KA=-3 FOR I/O ERROR (REQUEST LONGER THAN RECORD)
!     A            CHARACTER*1 (NB) DATA READ
!
! SUBPROGRAMS CALLED:
!   BAFRINDEX      BYTE-ADDRESSABLE FORTRAN RECORD INDEX
!   BAREAD         BYTE-ADDRESSABLE READ
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!
!$$$
      use kinds, only: i_kind,i_llong
      use constants, only: izero,ione
      IMPLICIT NONE
      INTEGER(i_kind),INTENT(IN):: LU
      INTEGER(i_llong),INTENT(IN):: IB,NB
      INTEGER(i_llong),INTENT(OUT):: KA
      CHARACTER,INTENT(OUT):: A(NB)
      INTEGER(i_llong),PARAMETER:: LBCW=4
      INTEGER(i_llong):: LX,IX
      INTEGER(i_llong):: KR
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  VALIDATE FORTRAN RECORD
      CALL BAFRINDEXL(LU,IB,LX,IX)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  READ IF VALID
      IF(LX.LT.izero) THEN
        KA=LX
      ELSEIF(LX.LT.NB) THEN
        KA=-3
      ELSE
        CALL BAREADL(LU,IB+LBCW,NB,KR,A)
        IF(KR.NE.NB) THEN
          KA=-ione
        ELSE
          KA=LBCW+LX+LBCW
        ENDIF
      ENDIF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END SUBROUTINE BAFRREADL
!-----------------------------------------------------------------------

      SUBROUTINE BAFRWRITE(LU,IB,NB,KA,A)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    bafrwrite
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-09-01  lueken - added subprogram doc block
!
!   input argument list:
!    LU,IB,NB
!
!   output argument list:
!    KA
!    A
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
      use kinds, only: i_kind,i_llong
      use constants, only: izero
      IMPLICIT NONE
      INTEGER(i_kind),INTENT(IN):: LU,IB,NB
      INTEGER(i_kind),INTENT(OUT):: KA
      CHARACTER,INTENT(OUT):: A(NB)
      INTEGER(i_llong) :: LONG_IB,LONG_NB,LONG_KA
!
        if(IB<izero .or. NB<izero ) THEN
          print *,'WRONG: in BAFRREAD starting postion IB or read '//    &
         'data size NB <0, STOP! ' //                                    &
         'Consider use bafrrwritel and long integer'
          KA=izero
          return
        ENDIF
        LONG_IB=IB
        LONG_NB=NB
        CALL BAFRWRITEL(LU,LONG_IB,LONG_NB,LONG_KA,A)
        KA=LONG_KA
!
      END SUBROUTINE BAFRWRITE
!-----------------------------------------------------------------------

      SUBROUTINE BAFRWRITEL(LU,IB,NB,KA,A)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM: BAFRWRITE      BYTE-ADDRESSABLE FORTRAN RECORD WRITE
!   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 1999-01-21
!
! ABSTRACT: THIS SUBPROGRAM WRITES AN UNFORMATTED FORTRAN RECORD
!
! PROGRAM HISTORY LOG:
!   1999-01-21  IREDELL
!
! USAGE:    CALL BAFRWRITE(LU,IB,NB,KA,A)
!   INPUT ARGUMENTS:
!     LU           INTEGER LOGICAL UNIT TO WRITE
!     IB           INTEGER FORTRAN RECORD START BYTE
!                  (FOR THE FIRST FORTRAN RECORD, IB SHOULD BE 0)
!     NB           INTEGER NUMBER OF BYTES TO WRITE
!     A            CHARACTER*1 (NB) DATA TO WRITE
!
!   OUTPUT ARGUMENTS:
!     KA           INTEGER NUMBER OF BYTES IN FORTRAN RECORD
!                  (IN WHICH CASE THE NEXT FORTRAN RECORD
!                  SHOULD HAVE A START BYTE OF IB+KA),
!                  OR KA=-1 FOR I/O ERROR
!
! SUBPROGRAMS CALLED:
!   BAWRITE        BYTE-ADDRESSABLE WRITE
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!
!$$$
      use kinds, only: i_kind,i_llong
      use constants, only: ione
      IMPLICIT NONE
      INTEGER(i_kind),INTENT(IN):: LU
      INTEGER(i_llong),INTENT(IN):: IB,NB
      INTEGER(i_llong),INTENT(OUT):: KA
      CHARACTER,INTENT(IN):: A(NB)
      INTEGER(i_llong),PARAMETER:: LBCW=4
      INTEGER(i_kind):: BCW
      INTEGER(i_llong):: KR
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  WRITE DATA BRACKETED BY BLOCK CONTROL WORDS
      BCW=NB
      CALL BAWRITEL(LU,IB,LBCW,KR,BCW)
      IF(KR.NE.LBCW) THEN
        KA=-ione
      ELSE
        CALL BAWRITEL(LU,IB+LBCW,NB,KR,A)
        IF(KR.NE.NB) THEN
          KA=-ione
        ELSE
          CALL BAWRITEL(LU,IB+LBCW+BCW,LBCW,KR,BCW)
          IF(KR.NE.LBCW) THEN
            KA=-ione
          ELSE
            KA=LBCW+BCW+LBCW
          ENDIF
        ENDIF
      ENDIF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END SUBROUTINE  BAFRWRITEL
