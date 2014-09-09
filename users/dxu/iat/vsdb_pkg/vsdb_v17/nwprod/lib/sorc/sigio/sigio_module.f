!-------------------------------------------------------------------------------
module sigio_module
!$$$  Module Documentation Block
!
! Module:    sigio_module    API for global spectral sigma file I/O
!   Prgmmr: iredell          Org: w/nx23     date: 1999-01-18
!
! Abstract: This module provides an Application Program Interface
!   for performing I/O on the sigma restart file of the global spectral model.
!   Functions include opening, reading, writing, and closing as well as
!   allocating and deallocating data buffers used in the transfers.
!   The I/O performed here is sequential.
!   The transfers are limited to header records or data records.
!   
! Program History Log:
!   1999-01-18  Mark Iredell
!
! Public Variables:
!   sigio_lhead1      Integer parameter length of first header record (=32)
!   sigio_charkind    Integer parameter kind or length of passed characters (=8)
!   sigio_intkind     Integer parameter kind or length of passed integers (=4)
!   sigio_realkind    Integer parameter kind or length of passed reals (=4)
!   sigio_dblekind    Integer parameter kind or length of passed longreals (=8)
!   sigio_realfill    Real(sigio_realkind) parameter fill value (=-9999.)
!   sigio_dblefill    Real(sigio_dblekind) parameter fill value (=-9999.)
!
! Public Defined Types:
!   sigio_head        Sigma file header information
!     clabsig           Character(sigio_lhead1) ON85 label
!                       (obsolescent)
!     fhour             Real(sigio_realkind) forecast hour
!     idate             Integer(sigio_intkind)(4) initial date
!                       (hour, month, day, 4-digit year)
!     si                Real(sigio_realkind)(101) sigma interfaces
!                       (obsolescent)
!     sl                Real(sigio_realkind)(100) sigma levels
!                       (obsolescent)
!     ak                Real(sigio_realkind)(101) hybrid interface a
!                       (obsolescent)
!     bk                Real(sigio_realkind)(101) hybrid interface b
!                       (obsolescent)
!     jcap              Integer(sigio_intkind) spectral truncation
!     levs              Integer(sigio_intkind) number of levels
!     itrun             Integer(sigio_intkind) truncation flag
!                       (=1 for triangular)
!     iorder            Integer(sigio_intkind) coefficient order flag
!                       (=2 for ibm order)
!     irealf            Integer(sigio_intkind) floating point flag
!                       (=1 for 4-byte ieee, =2 for 8-byte ieee)
!     igen              Integer(sigio_intkind) model generating flag
!     latf              Integer(sigio_intkind) latitudes in dynamics
!                       (=(jcap+1)*3/2)
!     lonf              Integer(sigio_intkind) longitudes in dynamics
!                       (>=(jcap+1)*3 appropriate for fft)
!     latb              Integer(sigio_intkind) latitudes in physics
!     lonb              Integer(sigio_intkind) longitudes in physics
!     latr              Integer(sigio_intkind) latitudes in radiation
!     lonr              Integer(sigio_intkind) longitudes in radiation
!     ntrac             Integer(sigio_intkind) number of tracers
!     icen2             Integer(sigio_intkind) subcenter id
!     iens              Integer(sigio_intkind)(2) ensemble ids
!     idpp              Integer(sigio_intkind) processing id
!     idsl              Integer(sigio_intkind) semi-lagrangian id
!     idvc              Integer(sigio_intkind) vertical coordinate id
!                       (=1 for sigma, =2 for ec-hybrid, =3 for ncep hybrid)
!     idvm              Integer(sigio_intkind) mass variable id
!     idvt              Integer(sigio_intkind) tracer variable id
!     idrun             Integer(sigio_intkind) run id
!     idusr             Integer(sigio_intkind) user-defined id
!     pdryini           Real(sigio_realkind) global mean dry air pressure (kPa)
!                       (obsolescent)
!     ncldt             Integer(sigio_intkind) number of cloud types
!     ixgr              Integer(sigio_intkind) extra grid field id
!                       (=0 for none, =1 for zhao1, =2 for zhao2,
!                        =3 for ferrier)
!     ivs               Integer(sigio_intkind) version number
!     nvcoord           Integer(sigio_intkind) number of vcoord profiles
!  The following variables should be allocated with sigio_alhead:
!     vcoord            Real(sigio_realkind)((levs+1),nvcoord) vcoord profiles
!     cfvars            Character(8)(5+ntrac) field variable names
!  The following variables should not be modified by the user:
!     nxgr              Integer(sigio_intkind) number of extra grid fields
!     nxss              Integer(sigio_intkind) number of extra scalars
!     nhead             Integer(sigio_intkind) number of header records
!     ndata             Integer(sigio_intkind) number of data records
!     lhead             Integer(sigio_intkind)(nhead) header record lengths
!     ldata             Integer(sigio_intkind)(ndata) data record lengths
!
!   sigio_data        Sigma file data fields
!     hs                Real(sigio_realkind)(:) pointer to spectral
!                       coefficients of surface height in m
!     ps                Real(sigio_realkind)(:) pointer to spectral
!                       coefficients of log of surface pressure over 1 kPa
!     t                 Real(sigio_realkind)(:,:) pointer to spectral
!                       coefficients of virtual temperature by level in K
!     d                 Real(sigio_realkind)(:,:) pointer to spectral
!                       coefficients of divergence by level in 1/second
!     z                 Real(sigio_realkind)(:,:) pointer to spectral
!                       coefficients of vorticity by level in 1/second
!     q                 Real(sigio_realkind)(:,:,:) pointer to spectral
!                       coefficients of tracers by level and tracer number
!                       in specific densities
!     xgr               Real(sigio_realkind)(:,:,:) pointer to extra grid fields
!                       by longitude, latitude and number of extra grid fields
!     xss               Real(sigio_realkind)(:) pointer to scalar array
!                       
!   sigio_dbta        Sigma file longreal data fields
!     hs                Real(sigio_dblekind)(:) pointer to spectral
!                       coefficients of surface height in m
!     ps                Real(sigio_dblekind)(:) pointer to spectral
!                       coefficients of log of surface pressure over 1 kPa
!     t                 Real(sigio_dblekind)(:,:) pointer to spectral
!                       coefficients of virtual temperature by level in K
!     d                 Real(sigio_dblekind)(:,:) pointer to spectral
!                       coefficients of divergence by level in 1/second
!     z                 Real(sigio_dblekind)(:,:) pointer to spectral
!                       coefficients of vorticity by level in 1/second
!     q                 Real(sigio_dblekind)(:,:,:) pointer to spectral
!                       coefficients of tracers by level and tracer number
!                       in specific densities
!     xgr               Real(sigio_dblekind)(:,:,:) pointer to extra grid fields
!                       by longitude, latitude and number of extra grid fields
!     xss               Real(sigio_dblekind)(:) pointer to scalar array
!                       
! Public Subprograms:
!   sigio_sropen      Open sigma file for sequential reading
!     lu                Integer(sigio_intkind) input logical unit
!     cfname            Character(*) input filename
!     iret              Integer(sigio_intkind) output return code
!
!   sigio_swopen      Open sigma file for sequential writing
!     lu                Integer(sigio_intkind) input logical unit
!     cfname            Character(*) input filename
!     iret              Integer(sigio_intkind) output return code
!
!   sigio_sclose      Close sigma file for sequential I/O
!     lu                Integer(sigio_intkind) input logical unit
!     iret              Integer(sigio_intkind) output return code
!
!   sigio_srhead      Read header information with sequential I/O
!     lu                Integer(sigio_intkind) input logical unit
!     head              Type(sigio_head) output header information
!     iret              Integer(sigio_intkind) output return code
!
!   sigio_swhead      Write header information with sequential I/O
!     lu                Integer(sigio_intkind) input logical unit
!     head              Type(sigio_head) input header information
!     iret              Integer(sigio_intkind) output return code
!
!   sigio_alhead      Allocate head allocatables
!     head              Type(sigio_head) input/output header information
!     iret              Integer(sigio_intkind) output return code
!     levs              Integer(sigio_intkind) optional number of levels
!     nvcoord           Integer(sigio_intkind) optional number of vcoords
!     ntrac             Integer(sigio_intkind) optional number of tracers
!
!   sigio_aldata      Allocate data fields
!     head              Type(sigio_head) input header information
!     data              Type(sigio_data) output data fields
!     iret              Integer(sigio_intkind) output return code
!
!   sigio_axdata      Deallocate data fields
!     data              Type(sigio_data) output data fields
!     iret              Integer(sigio_intkind) output return code
!
!   sigio_srdata      Read data fields with sequential I/O
!     lu                Integer(sigio_intkind) input logical unit
!     head              Type(sigio_head) input header information
!     data              Type(sigio_data) output data fields
!     iret              Integer(sigio_intkind) output return code
!
!   sigio_swdata      Write data fields with sequential I/O
!     lu                Integer(sigio_intkind) input logical unit
!     head              Type(sigio_head) input header information
!     data              Type(sigio_data) input data fields
!     iret              Integer(sigio_intkind) output return code
!
!   sigio_aldbta      Allocate longreal data fields
!     head              Type(sigio_head) input header information
!     dbta              Type(sigio_dbta) output longreal data fields
!     iret              Integer(sigio_intkind) output return code
!
!   sigio_axdbta      Deallocate longreal data fields
!     dbta              Type(sigio_dbta) output longreal data fields
!     iret              Integer(sigio_intkind) output return code
!
!   sigio_srdbta      Read longreal data fields with sequential I/O
!     lu                Integer(sigio_intkind) input logical unit
!     head              Type(sigio_head) input header information
!     dbta              Type(sigio_dbta) output longreal data fields
!     iret              Integer(sigio_intkind) output return code
!
!   sigio_swdbta      Write longreal data fields with sequential I/O
!     lu                Integer(sigio_intkind) input logical unit
!     head              Type(sigio_head) input header information
!     dbta              Type(sigio_dbta) input longreal data fields
!     iret              Integer(sigio_intkind) output return code
!
!   sigio_srohdc      Open, read header & data and close with sequential I/O
!     lu                Integer(sigio_intkind) input logical unit
!     cfname            Character(*) input filename
!     head              Type(sigio_head) output header information
!     data              Type(sigio_data) or type(sigio_dbta) output data fields
!     iret              Integer(sigio_intkind) output return code
!
!   sigio_swohdc      Open, write header & data and close with sequential I/O
!     lu                Integer(sigio_intkind) input logical unit
!     cfname            Character(*) input filename
!     head              Type(sigio_head) input header information
!     data              Type(sigio_data) or type(sigio_dbta) output data fields
!     iret              Integer(sigio_intkind) output return code
!
!   sigio_modpr        Compute model pressures
!     im                Integer(sigio_intkind) input number of points
!     ix                Integer(sigio_intkind) input first dimension
!     km                Integer(sigio_intkind) input number of levels
!     nvcoord           Integer(sigio_intkind) input number of vertical coords
!     idvc              Integer(sigio_intkind) input vertical coordinate id
!                       (1 for sigma and 2 for hybrid)
!     idsl              Integer(sigio_intkind) input type of sigma structure
!                       (1 for phillips or 2 for mean)
!     vcoord            Real(sigio_realkind)(km+1,nvcoord) input vertical coords
!                       for idvc=1, nvcoord=1: sigma interface
!                       for idvc=2, nvcoord=2: hybrid interface a and b
!     iret              Integer(sigio_intkind) output return code
!     ps                Real(sigio_realkind)(ix) input optional surface pressure (Pa)
!     tv                Real(sigio_realkind)(ix,km) input optional virtual temperature (K)
!     pd                Real(sigio_realkind)(ix,km) output optional delta pressure (Pa)
!     pm                Real(sigio_realkind)(ix,km) output optional layer pressure (Pa)
!
!   sigio_modprd      Compute model pressures - double precision
!     im                Integer(sigio_intkind) input number of points
!     ix                Integer(sigio_intkind) input first dimension
!     km                Integer(sigio_intkind) input number of levels
!     nvcoord           Integer(sigio_intkind) input number of vertical coords
!     idvc              Integer(sigio_intkind) input vertical coordinate id
!                       (1 for sigma and 2 for hybrid)
!     idsl              Integer(sigio_intkind) input type of sigma structure
!                       (1 for phillips or 2 for mean)
!     vcoord            Real(sigio_realkind)(km+1,nvcoord) input vertical coords
!                       for idvc=1, nvcoord=1: sigma interface
!                       for idvc=2, nvcoord=2: hybrid interface a and b
!     iret              Integer(sigio_intkind) output return code
!     ps                Real(sigio_dblekind)(ix) input optional surface pressure (Pa)
!     tv                Real(sigio_dblekind)(ix,km) input optional virtual temperature (K)
!     pd                Real(sigio_dblekind)(ix,km) output optional delta pressure (Pa)
!     pm                Real(sigio_dblekind)(ix,km) output optional layer pressure (Pa)
!
!   sigio_adhead        Set private data in header
!     head              Type(sigio_head) input/output header information
!
!   sigio_cnvpsv        Convert from model mass variable to pressure (Pa)
!                       when cnflg > 0, or the opposite when cnflag <= 0.
!                       Values of IDVM determines variable. 
!     im                Integer(sigio_intkind) input number of points
!     idvm              Integer(sigio_intkind) mass variable id
!     ps                Real(sigio_realkind)(im) inout mass variable or ps (Pa)
!     dp                Real(sigio_realkind)(im) output dp(out)/dp(in)
!     cnflg             Integer(sigio_intkind) input conversion flag.
!                       when >0, conversion is to surface pressure (Pa) and
!                       when <= 0, the conversion is to model mass variable.
!
!   sigio_cnvtdv        Convert from Virtual Temperature (Tv) or Enthalpy (h)
!                       to sensible Temperature (when cnflag > 0) and the opposite
!                       when cnflag <= 0. Values of IDVM determines Tv or h
!     im                Integer(sigio_intkind) input number of points
!     ix                Integer(sigio_intkind) input first dimension
!     km                Integer(sigio_intkind) input number of levels
!     idvc              Integer(sigio_intkind) input vertical coordinate id
!                       (1 for sigma, 2 for hybrid, and 3 for general hybrid)
!     idvm              Integer(sigio_intkind) mass variable id
!                       (32 for enthalpy)
!     ntrac             Integer(sigio_intkind) input number of tracers
!     iret              Integer(sigio_intkind) output return code
!     t                 Real(sigio_realkind)(ix,km) input Tv, h or T
!     q                 Real(sigio_realkind)(ix,km,ntrac) input tracers
!     cpi               Real(sigio_realkind)(ntrac) input specific heat at constant
!                       pressure for tracers
!    cnflg              Integer(sigio_intkind) input conversion flag. when >0
!                       conversion is to dry temperature from Tv or h and when
!                       <= 0, the conversion is from dry temperature to Tv or h.
!
! Remarks:
!   (1) The sigma file format follows:
!       For ivs=198410:
!         ON85 label (32 bytes)
!         Header information record containing
!           real forecast hour, initial date, sigma interfaces, sigma levels,
!           padding to allow for 100 levels, and finally 44 identifier words
!           containing JCAP, LEVS, NTRAC, IREALF, etc. (250 4-byte words)
!           (word size in the remaining records depends on the value of IREALF)
!         Orography (NC words, where NC=(JCAP+1)*(JCAP+2))
!         Log surface pressure (NC words)
!         Temperature (LEVS records of NC words)
!         Divergence & Vorticity interleaved (2*LEVS records of NC words)
!         Tracers (LEVS*NTRAC records of NC words)
!         Extra grid fields (NXGR records of LONB*LATB words)
!       For ivs=200509:
!         Label containing
!           'GFS ','SIG ',ivs,nhead,ndata,reserved(3) (8 4-byte words)
!         Header records
!           lhead(nhead),ldata(ndata) (nhead+ndata 4-byte words)
!           fhour, idate(4), jcap, levs, itrun, iorder, irealf, igen,
!             latf, lonf, latb, lonb, latr, lonr, ntrac, nvcoord, 
!             icen2, iens(2), idpp, idsl, idvc, idvm, idvt, idrun, idusr,
!             pdryini, ncldt, ixgr, reserved(18) (50 4-byte words)
!           vcoord((levs+1)*nvcoord 4-byte words)
!           cfvars(5+ntrac 8-byte character words)
!         Data records (word size depends on irealf)
!           orography (nc words, where nc=(jcap+1)*(jcap+2))
!           log surface pressure (nc words)
!           temperature (levs records of nc words)
!           divergence (levs records of nc words)
!           vorticity (levs records of nc words)
!           tracers (levs*ntrac records of nc words)
!           scalars (nxss words)
!           extra grid fields (nxgr records of lonb*latb words)
!           extra scalars (nxss words)
!
!   (2) Possible return codes:
!          0   Successful call
!         -1   Open or close I/O error
!         -2   Header record I/O error (possible EOF)
!         -3   Allocation or deallocation error
!         -4   Data record I/O error
!         -5   Insufficient data dimensions allocated
!
! Examples:
!   (1) Read the entire sigma file 'sigf24' and
!       print out the global mean temperature profile.
!
!     use sigio_module
!     type(sigio_head):: head
!     type(sigio_data):: data
!     call sigio_srohdc(11,'sigf24',head,data,iret)
!     print '(f8.2)',data%t(1,head%levs:1:-1)/sqrt(2.)
!     end
!
! Attributes:
!   Language: Fortran 90
!
!$$$
  implicit none
  private
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Public Variables
  integer,parameter,public:: sigio_lhead1=32
  integer,parameter,public:: sigio_intkind=4,sigio_realkind=4,sigio_dblekind=8
  integer,parameter,public:: sigio_charkind=8
  real(sigio_intkind),parameter,public:: sigio_intfill=-9999_sigio_intkind
  real(sigio_realkind),parameter,public:: sigio_realfill=-9999._sigio_realkind
  real(sigio_dblekind),parameter,public:: sigio_dblefill=-9999._sigio_dblekind
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Public Types
  type,public:: sigio_head
    character(sigio_lhead1):: clabsig=' '
    real(sigio_realkind):: fhour=sigio_realfill
    integer(sigio_intkind):: idate(4)=sigio_intfill
    real(sigio_realkind):: si(101)=sigio_realfill
    real(sigio_realkind):: sl(100)=sigio_realfill
    real(sigio_realkind):: ak(101)=sigio_realfill
    real(sigio_realkind):: bk(101)=sigio_realfill
    integer(sigio_intkind):: jcap=sigio_intfill
    integer(sigio_intkind):: levs=sigio_intfill
    integer(sigio_intkind):: itrun=sigio_intfill
    integer(sigio_intkind):: iorder=sigio_intfill
    integer(sigio_intkind):: irealf=sigio_intfill
    integer(sigio_intkind):: igen=sigio_intfill
    integer(sigio_intkind):: latf=sigio_intfill
    integer(sigio_intkind):: lonf=sigio_intfill
    integer(sigio_intkind):: latb=sigio_intfill
    integer(sigio_intkind):: lonb=sigio_intfill
    integer(sigio_intkind):: latr=sigio_intfill
    integer(sigio_intkind):: lonr=sigio_intfill
    integer(sigio_intkind):: ntrac=sigio_intfill
    integer(sigio_intkind):: icen2=sigio_intfill
    integer(sigio_intkind):: iens(2)=sigio_intfill
    integer(sigio_intkind):: idpp=sigio_intfill
    integer(sigio_intkind):: idsl=sigio_intfill
    integer(sigio_intkind):: idvc=sigio_intfill
    integer(sigio_intkind):: idvm=sigio_intfill
    integer(sigio_intkind):: idvt=sigio_intfill
    integer(sigio_intkind):: idrun=sigio_intfill
    integer(sigio_intkind):: idusr=sigio_intfill
    real(sigio_realkind):: pdryini=sigio_realfill
    integer(sigio_intkind):: ncldt=sigio_intfill
    integer(sigio_intkind):: ixgr=sigio_intfill
    integer(sigio_intkind):: ivs=sigio_intfill
    integer(sigio_intkind):: nvcoord=sigio_intfill
    real(sigio_realkind),allocatable:: vcoord(:,:)
    character(sigio_charkind),allocatable:: cfvars(:)
    integer(sigio_intkind):: nxgr=sigio_intfill
    integer(sigio_intkind):: nxss=sigio_intfill
    integer(sigio_intkind):: nhead=sigio_intfill
    integer(sigio_intkind):: ndata=sigio_intfill
    integer(sigio_intkind),allocatable:: lhead(:)
    integer(sigio_intkind),allocatable:: ldata(:)
    real(sigio_realkind), allocatable :: cpi(:), ri(:)
!   real(sigio_realkind):: cpi(100)=sigio_realfill
!   real(sigio_realkind):: ri(100)=sigio_realfill
  end type
  type,public:: sigio_data
    real(sigio_realkind),pointer:: hs(:)=>null()
    real(sigio_realkind),pointer:: ps(:)=>null()
    real(sigio_realkind),pointer:: t(:,:)=>null()
    real(sigio_realkind),pointer:: d(:,:)=>null()
    real(sigio_realkind),pointer:: z(:,:)=>null()
    real(sigio_realkind),pointer:: q(:,:,:)=>null()
    real(sigio_realkind),pointer:: xgr(:,:,:)=>null()
    real(sigio_realkind),pointer:: xss(:)=>null()
  end type
  type,public:: sigio_dbta
    real(sigio_dblekind),pointer:: hs(:)=>null()
    real(sigio_dblekind),pointer:: ps(:)=>null()
    real(sigio_dblekind),pointer:: t(:,:)=>null()
    real(sigio_dblekind),pointer:: d(:,:)=>null()
    real(sigio_dblekind),pointer:: z(:,:)=>null()
    real(sigio_dblekind),pointer:: q(:,:,:)=>null()
    real(sigio_dblekind),pointer:: xgr(:,:,:)=>null()
    real(sigio_dblekind),pointer:: xss(:)=>null()
  end type
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Public Subprograms
  public sigio_sropen,sigio_swopen,sigio_sclose,sigio_srhead,sigio_swhead  
  public sigio_alhead,sigio_aldata,sigio_axdata,sigio_srdata,sigio_swdata  
  public sigio_aldbta,sigio_axdbta,sigio_srdbta,sigio_swdbta  
  public sigio_srohdc,sigio_swohdc  
! public sigio_modpr,sigio_cnvtdv,sigio_cnvpsv
  public sigio_modpr,sigio_cnvtdv,sigio_cnvpsv
  public sigio_modprd,sigio_cnvtdvd,sigio_cnvpsvd
  interface sigio_srohdc
    module procedure sigio_srohdca,sigio_srohdcb
  end interface
  interface sigio_swohdc
    module procedure sigio_swohdca,sigio_swohdcb
  end interface
! interface sigio_modpr
!   module procedure sigio_modprs, sigio_modprd
! end interface
! interface sigio_cnvtdv
!   module procedure sigio_cnvtdvs, sigio_cnvtdvd
! end interface
! interface sigio_cnvps
!   module procedure sigio_cnvpsvs, sigio_cnvpsvd
! end interface
  public sigio_adhead
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Private Variables
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Private Types
  type sigio_head2
    sequence
    real(sigio_realkind):: fhour
    integer(sigio_intkind):: idate(4)
    real(sigio_realkind):: sisl(2*100+1)
    real(sigio_realkind):: ext(44)
  end type
contains
!-------------------------------------------------------------------------------
  subroutine sigio_sropen(lu,cfname,iret)
    implicit none
    integer(sigio_intkind),intent(in):: lu
    character*(*),intent(in):: cfname
    integer(sigio_intkind),intent(out):: iret
    integer ios
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    open(lu,file=cfname,form='unformatted',&
         status='old',action='read',iostat=ios)
    iret=ios
    if(iret.ne.0) iret=-1
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine sigio_swopen(lu,cfname,iret)
    implicit none
    integer(sigio_intkind),intent(in):: lu
    character*(*),intent(in):: cfname
    integer(sigio_intkind),intent(out):: iret
    integer ios
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    open(lu,file=cfname,form='unformatted',&
         status='unknown',action='readwrite',iostat=ios)
    iret=ios
    if(iret.ne.0) iret=-1
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine sigio_sclose(lu,iret)
    implicit none
    integer(sigio_intkind),intent(in):: lu
    integer(sigio_intkind),intent(out):: iret
    integer ios
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    close(lu,iostat=ios)
    iret=ios
    if(iret.ne.0) iret=-1
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine sigio_srhead(lu,head,iret)
    implicit none
    integer(sigio_intkind),intent(in):: lu
    type(sigio_head),intent(inout):: head
    integer(sigio_intkind),intent(out):: iret
    type(sigio_head2):: head2
    character(4):: cgfs,csig
    integer(sigio_intkind):: nhead,ndata,nresv(3)
    integer:: ios
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    iret=-2
    rewind lu
    read(lu,iostat=ios) head%clabsig
    if(ios.ne.0) return
    if(head%clabsig(1:8).eq.'GFS SIG ') then  ! modern sigma file
      rewind lu
      read(lu,iostat=ios) cgfs,csig,head%ivs,nhead,ndata,nresv
      if(ios.ne.0) return
      if(head%ivs.eq.200509) then
        read(lu,iostat=ios)
        if(ios.ne.0) return
        read(lu,iostat=ios) head%fhour,head%idate,head%jcap,head%levs,&
          head%itrun,head%iorder,head%irealf,head%igen,head%latf,head%lonf,&
          head%latb,head%lonb,head%latr,head%lonr,head%ntrac,head%nvcoord,&
          head%icen2,head%iens,head%idpp,head%idsl,head%idvc,head%idvm,&
          head%idvt,head%idrun,head%idusr,head%pdryini,head%ncldt,head%ixgr
        if(ios.ne.0) return
        call sigio_alhead(head,iret)
        read(lu,iostat=ios) head%vcoord
        if(ios.ne.0) return
        read(lu,iostat=ios) head%cfvars
        if(ios.ne.0) return
        if (mod(head%idvm/10,10) == 3)then
          read(lu,iostat=ios) head%cpi
          if(ios.ne.0) return
          read(lu,iostat=ios) head%ri
          if(ios.ne.0) return
        endif
        head%clabsig=' '
        head%si=sigio_realfill
        head%sl=sigio_realfill
        head%ak=sigio_realfill
        head%bk=sigio_realfill
        if(head%levs.lt.100.and.(head%idvc.eq.2.or.&
         (head%idvc.eq.3.and.all(head%vcoord(:,3).eq.0)))) then
          head%ak(1:head%levs+1)=head%vcoord(1:head%levs+1,1)
          head%bk(1:head%levs+1)=head%vcoord(1:head%levs+1,2)
        endif
!       head%pdryini=sigio_realfill
      else
        return
      endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    else
      read(lu,iostat=ios) head2%fhour,head2%idate,head2%sisl,head2%ext
      if(ios.ne.0) return
      head%fhour=head2%fhour
      head%idate=head2%idate
      head%jcap=head2%ext(1)
      head%levs=head2%ext(2)
      head%itrun=head2%ext(3)
      head%iorder=head2%ext(4)
      head%irealf=head2%ext(5)
      head%igen=head2%ext(6)
      head%lonf=head2%ext(7)
      head%latf=head2%ext(8)
      head%lonb=head2%ext(9)
      head%latb=head2%ext(10)
      head%lonr=head2%ext(11)
      head%latr=head2%ext(12)
      head%ntrac=max(head2%ext(13),1.)
      head%icen2=head2%ext(14)
      head%iens=head2%ext(15:16)
      head%idpp=head2%ext(17)
      head%idsl=head2%ext(18)
      head%idvc=head2%ext(19)
      head%idvm=head2%ext(20)
      head%idvt=head2%ext(21)
      head%idrun=head2%ext(22)
      head%idusr=head2%ext(23)
      head%pdryini=head2%ext(24)
      head%ncldt=head2%ext(25)
      head%ixgr=head2%ext(26)
      head%si=sigio_realfill
      head%sl=sigio_realfill
      head%ak=sigio_realfill
      head%bk=sigio_realfill
      if(head%idvc.eq.0.or.head%idvc.eq.1) then
        head%si(1:head%levs+1)=head2%sisl(1:head%levs+1)
        head%sl(1:head%levs)=head2%sisl(head%levs+2:2*head%levs+1)
        head%nvcoord=1
        call sigio_alhead(head,iret)
        head%vcoord(1:head%levs+1,1)=head2%sisl(1:head%levs+1)
      elseif(head%idvc.eq.2) then
        head%ak(1:head%levs+1)=head2%sisl(1:head%levs+1)
        head%bk(1:head%levs+1)=head2%sisl(head%levs+2:2*head%levs+2)
        head%nvcoord=2
        call sigio_alhead(head,iret)
        head%vcoord(1:head%levs+1,1)=head2%sisl(1:head%levs+1)
        head%vcoord(1:head%levs+1,2)=head2%sisl(head%levs+2:2*head%levs+2)
      elseif(head%idvc.eq.3) then
        head%nvcoord=2
        call sigio_alhead(head,iret)
        head%vcoord(1:head%levs+1,1)=head2%sisl(1:head%levs+1)
        head%vcoord(1:head%levs+1,2)=head2%sisl(head%levs+2:2*head%levs+2)
      endif
      head%ivs=198410
    endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call sigio_adhead(head)
    iret=0
  end subroutine
!-------------------------------------------------------------------------------
  subroutine sigio_swhead(lu,head,iret)
    implicit none
    integer(sigio_intkind),intent(in):: lu
    type(sigio_head),intent(inout):: head
    integer(sigio_intkind),intent(out):: iret
    integer(sigio_intkind) lhead,ldata
    type(sigio_head2):: head2
    integer:: ios
    integer i
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    iret=-2
    call sigio_adhead(head)
    rewind lu
    if(head%ivs.ge.200509) then
      write(lu,iostat=ios) 'GFS SIG ',head%ivs,head%nhead,head%ndata,0,0,0
      if(ios.ne.0) return
      write(lu,iostat=ios) head%lhead,head%ldata
      if(ios.ne.0) return
      write(lu,iostat=ios) head%fhour,head%idate,head%jcap,head%levs,&
        head%itrun,head%iorder,head%irealf,head%igen,head%latf,head%lonf,&
        head%latb,head%lonb,head%latr,head%lonr,head%ntrac,head%nvcoord,&
        head%icen2,head%iens,head%idpp,head%idsl,head%idvc,head%idvm,&
        head%idvt,head%idrun,head%idusr,head%pdryini,head%ncldt,head%ixgr,&
        (0,i=1,18)
      if(ios.ne.0) return
      if(size(head%vcoord).ne.(head%levs+1)*head%nvcoord) return
      write(lu,iostat=ios) head%vcoord
      if(ios.ne.0) return
      if(size(head%cfvars).ne.5+head%ntrac) return
      write(lu,iostat=ios) head%cfvars
      if(ios.ne.0) return
      if (mod(head%idvm/10,10) == 3) then
        write(lu,iostat=ios) head%cpi
        if(ios.ne.0) return
        write(lu,iostat=ios) head%ri
        if(ios.ne.0) return
      endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    else
      head2%fhour=head%fhour 
      head2%idate=head%idate 
      head2%sisl=0
      if(head%idvc.eq.0.or.head%idvc.eq.1) then
        if(head%nvcoord.eq.1.and.head%vcoord(1,1).eq.1.) then
          head2%sisl(1:head%levs+1)=head%vcoord(1:head%levs+1,1)
          call sigio_modpr(1,1,head%levs,head%nvcoord,head%idvc,head%idsl,&
                           head%vcoord,iret,ps=(/1./),&
                           pm=head2%sisl(head%levs+2:2*head%levs+1))
        else
          head2%sisl(1:head%levs+1)=head%si(1:head%levs+1)
          head2%sisl(head%levs+2:2*head%levs+1)=head%sl(1:head%levs)
        endif
      elseif(head%idvc.eq.2) then
        if(head%nvcoord.eq.2.and.head%vcoord(1,2).eq.1.) then
          head2%sisl(1:head%levs+1)=head%vcoord(1:head%levs+1,1)
          head2%sisl(head%levs+2:2*head%levs+2)=head%vcoord(1:head%levs+1,2)
        else
          head2%sisl(1:head%levs+1)=head%ak(1:head%levs+1)
          head2%sisl(head%levs+2:2*head%levs+2)=head%bk(1:head%levs+1)
        endif
      elseif(head%idvc.eq.3) then
        if(head%nvcoord.eq.2.and.head%vcoord(1,2).eq.1.) then
          head2%sisl(1:head%levs+1)=head%vcoord(1:head%levs+1,1)
          head2%sisl(head%levs+2:2*head%levs+2)=head%vcoord(1:head%levs+1,2)
        endif
      endif
      head2%ext(1)=head%jcap
      head2%ext(2)=head%levs
      head2%ext(3)=head%itrun
      head2%ext(4)=head%iorder
      head2%ext(5)=head%irealf
      head2%ext(6)=head%igen
      head2%ext(7)=head%lonf
      head2%ext(8)=head%latf
      head2%ext(9)=head%lonb
      head2%ext(10)=head%latb
      head2%ext(11)=head%lonr
      head2%ext(12)=head%latr
      head2%ext(13)=head%ntrac
      head2%ext(14)=head%icen2
      head2%ext(15:16)=head%iens
      head2%ext(17)=head%idpp
      head2%ext(18)=head%idsl
      head2%ext(19)=head%idvc
      head2%ext(20)=head%idvm
      head2%ext(21)=head%idvt
      head2%ext(22)=head%idrun
      head2%ext(23)=head%idusr
      head2%ext(24)=head%pdryini
      head2%ext(25)=head%ncldt
      head2%ext(26)=head%ixgr
      head2%ext(27:44)=0
      write(lu,iostat=ios) head%clabsig
      if(ios.ne.0) return
      write(lu,iostat=ios) head2%fhour,head2%idate,head2%sisl,head2%ext
      if(ios.ne.0) return
    endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    iret=0
  end subroutine
!-------------------------------------------------------------------------------
  subroutine sigio_alhead(head,iret,levs,nvcoord,ntrac,idvm)
    implicit none
    type(sigio_head),intent(inout):: head
    integer(sigio_intkind),intent(out):: iret
    integer(sigio_intkind),optional,intent(in):: levs,nvcoord,ntrac,idvm
    integer dim1v,dim2v,dim1c,thermodyn_id
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if(present(levs)) then
      dim1v=levs+1
    else
      dim1v=head%levs+1
    endif
    if(present(nvcoord)) then
      dim2v=nvcoord
    else
      dim2v=head%nvcoord
    endif
    if(present(ntrac)) then
      dim1c=5+ntrac
    else
      dim1c=5+head%ntrac
    endif
    if(allocated(head%vcoord)) deallocate(head%vcoord)
    if(allocated(head%cfvars)) deallocate(head%cfvars)
    allocate(head%vcoord(dim1v,dim2v),head%cfvars(dim1c),stat=iret)
    if(present(idvm)) then
      thermodyn_id=mod(idvm/10,10)
    else
      thermodyn_id=mod(head%idvm/10,10)
    endif
    if (thermodyn_id == 3) then
      if(allocated(head%cpi)) deallocate(head%cpi)
      if(allocated(head%ri)) deallocate(head%ri)
      allocate(head%cpi(dim1c-4),head%ri(dim1c-4),stat=iret)
    endif
    if(iret.eq.0) then
      head%vcoord=sigio_realfill
      head%cfvars=' '
    endif
    if(iret.ne.0) iret=-3
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine sigio_aldata(head,data,iret)
    implicit none
    type(sigio_head),intent(in):: head
    type(sigio_data),intent(inout):: data
    integer(sigio_intkind),intent(out):: iret
    integer nc,dim1,dim2,dim3q,dim1x,dim2x,dim3x
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call sigio_axdata(data,iret)
    nc=(head%jcap+1)*(head%jcap+2)
    dim1=nc
    dim2=head%levs
    dim3q=head%ntrac
    dim1x=head%lonb
    dim2x=head%latb
    dim3x=head%nxgr
    allocate(data%hs(dim1),data%ps(dim1),&
             data%t(dim1,dim2),data%d(dim1,dim2),data%z(dim1,dim2),&
             data%q(dim1,dim2,dim3q),&
             data%xgr(dim1x,dim2x,dim3x),data%xss(head%nxss),stat=iret)
    if(iret.ne.0) iret=-3
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine sigio_axdata(data,iret)
    implicit none
    type(sigio_data),intent(inout):: data
    integer(sigio_intkind),intent(out):: iret
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    deallocate(data%hs,data%ps,data%t,data%d,data%z,data%q,data%xgr,stat=iret)
    nullify(data%hs,data%ps,data%t,data%d,data%z,data%q,data%xgr)
    if(iret.ne.0) iret=-3
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine sigio_srdata(lu,head,data,iret)
    implicit none
    integer(sigio_intkind),intent(in):: lu
    type(sigio_head),intent(in):: head
    type(sigio_data),intent(inout):: data
    integer(sigio_intkind),intent(out):: iret
    type(sigio_dbta):: dbta
    integer:: nc,mdim1,mdim2,mdim3q,mdim1x,mdim2x,mdim3x,k,n,ios
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    nc=(head%jcap+1)*(head%jcap+2)
    mdim1=min(size(data%hs,1),size(data%ps,1),&
              size(data%t,1),size(data%d,1),size(data%z,1),&
              size(data%q,1))
    mdim2=min(size(data%t,2),size(data%d,2),size(data%z,2),&
              size(data%q,2))
    mdim3q=size(data%q,3)
    iret=-5
    if(mdim1.lt.nc.or.&
       mdim2.lt.head%levs.or.&
       mdim3q.lt.head%ntrac) return
    if(head%nxgr.gt.0) then
      mdim1x=size(data%xgr,1)
      mdim2x=size(data%xgr,2)
      mdim3x=size(data%xgr,3)
      if(mdim1x.lt.head%lonb.or.&
         mdim2x.lt.head%latb.or.&
         mdim3x.lt.head%nxgr) return
      if(size(data%xss).lt.head%nxss) return
    endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if(head%irealf.ne.2) then
      iret=-4
      read(lu,iostat=ios) data%hs(:nc)
      if(ios.ne.0) return
      read(lu,iostat=ios) data%ps(:nc)
      if(ios.ne.0) return
      do k=1,head%levs
        read(lu,iostat=ios) data%t(:nc,k)
        if(ios.ne.0) return
      enddo
      do k=1,head%levs
        read(lu,iostat=ios) data%d(:nc,k)
        if(ios.ne.0) return
        read(lu,iostat=ios) data%z(:nc,k)
        if(ios.ne.0) return
      enddo
      do n=1,head%ntrac
        do k=1,head%levs
          read(lu,iostat=ios) data%q(:nc,k,n)
          if(ios.ne.0) return
        enddo
      enddo
      do n=1,head%nxgr
        read(lu,iostat=ios) data%xgr(:head%lonb,:head%latb,n)
        if(ios.ne.0) return
      enddo
      if(head%nxss.gt.0) then
        read(lu,iostat=ios) data%xss(:head%nxss)
        if(ios.ne.0) return
      endif
      iret=0
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    else
      call sigio_aldbta(head,dbta,iret)
      if(iret.ne.0) return
      call sigio_srdbta(lu,head,dbta,iret)
      if(iret.ne.0) return
      data%hs(:nc)=dbta%hs(:nc)
      data%ps(:nc)=dbta%ps(:nc)
      data%t(:nc,:head%levs)=dbta%t(:nc,:head%levs)
      data%d(:nc,:head%levs)=dbta%d(:nc,:head%levs)
      data%z(:nc,:head%levs)=dbta%z(:nc,:head%levs)
      data%q(:nc,:head%levs,:head%ntrac)=dbta%q(:nc,:head%levs,:head%ntrac)
      data%xgr(:head%lonb,:head%latb,:head%nxgr)=&
       dbta%xgr(:head%lonb,:head%latb,:head%nxgr)
      data%xss(:head%nxss)=dbta%xss(:head%nxss)
      call sigio_axdbta(dbta,iret)
    endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine sigio_swdata(lu,head,data,iret)
    implicit none
    integer(sigio_intkind),intent(in):: lu
    type(sigio_head),intent(in):: head
    type(sigio_data),intent(in):: data
    integer(sigio_intkind),intent(out):: iret
    type(sigio_dbta):: dbta
    integer:: nc,mdim1,mdim2,mdim3q,mdim1x,mdim2x,mdim3x,k,n,ios
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    nc=(head%jcap+1)*(head%jcap+2)
    mdim1=min(size(data%hs,1),size(data%ps,1),&
              size(data%t,1),size(data%d,1),size(data%z,1),&
              size(data%q,1))
    mdim2=min(size(data%t,2),size(data%d,2),size(data%z,2),&
              size(data%q,2))
    mdim3q=size(data%q,3)
    iret=-5
    if(mdim1.lt.nc.or.&
       mdim2.lt.head%levs.or.&
       mdim3q.lt.head%ntrac) return
    if(head%nxgr.gt.0) then
      mdim1x=size(data%xgr,1)
      mdim2x=size(data%xgr,2)
      mdim3x=size(data%xgr,3)
      if(mdim1x.lt.head%lonb.or.&
         mdim2x.lt.head%latb.or.&
         mdim3x.lt.head%nxgr) return
      if(size(data%xss).lt.head%nxss) return
    endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if(head%irealf.ne.2) then
      iret=-4
      write(lu,iostat=ios) data%hs(:nc)
      if(ios.ne.0) return
      write(lu,iostat=ios) data%ps(:nc)
      if(ios.ne.0) return
      do k=1,head%levs
        write(lu,iostat=ios) data%t(:nc,k)
        if(ios.ne.0) return
      enddo
      do k=1,head%levs
        write(lu,iostat=ios) data%d(:nc,k)
        if(ios.ne.0) return
        write(lu,iostat=ios) data%z(:nc,k)
        if(ios.ne.0) return
      enddo
      do n=1,head%ntrac
        do k=1,head%levs
          write(lu,iostat=ios) data%q(:nc,k,n)
          if(ios.ne.0) return
        enddo
      enddo
      do n=1,head%nxgr
        write(lu,iostat=ios) data%xgr(:head%lonb,:head%latb,n)
        if(ios.ne.0) return
      enddo
      if(head%nxss.gt.0) then
        write(lu,iostat=ios) data%xss(:head%nxss)
        if(ios.ne.0) return
      endif
      iret=0
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    else
      call sigio_aldbta(head,dbta,iret)
      if(iret.ne.0) return
      dbta%hs(:nc)=data%hs(:nc)
      dbta%ps(:nc)=data%ps(:nc)
      dbta%t(:nc,:head%levs)=data%t(:nc,:head%levs)
      dbta%d(:nc,:head%levs)=data%d(:nc,:head%levs)
      dbta%z(:nc,:head%levs)=data%z(:nc,:head%levs)
      dbta%q(:nc,:head%levs,:head%ntrac)=data%q(:nc,:head%levs,:head%ntrac)
      dbta%xgr(:head%lonb,:head%latb,:head%nxgr)=&
       data%xgr(:head%lonb,:head%latb,:head%nxgr)
      dbta%xss(:head%nxss)=data%xss(:head%nxss)
      call sigio_swdbta(lu,head,dbta,iret)
      if(iret.ne.0) return
      call sigio_axdbta(dbta,iret)
    endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine sigio_srohdca(lu,cfname,head,data,iret)
    implicit none
    integer(sigio_intkind),intent(in):: lu
    character*(*),intent(in):: cfname
    type(sigio_head),intent(inout):: head
    type(sigio_data),intent(inout):: data
    integer(sigio_intkind),intent(out):: iret
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call sigio_sropen(lu,cfname,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call sigio_srhead(lu,head,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call sigio_aldata(head,data,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call sigio_srdata(lu,head,data,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call sigio_sclose(lu,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine sigio_swohdca(lu,cfname,head,data,iret)
    implicit none
    integer(sigio_intkind),intent(in):: lu
    character*(*),intent(in):: cfname
    type(sigio_head),intent(inout):: head
    type(sigio_data),intent(in):: data
    integer(sigio_intkind),intent(out):: iret
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call sigio_swopen(lu,cfname,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call sigio_swhead(lu,head,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call sigio_swdata(lu,head,data,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call sigio_sclose(lu,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine sigio_aldbta(head,dbta,iret)
    implicit none
    type(sigio_head),intent(in):: head
    type(sigio_dbta),intent(inout):: dbta
    integer(sigio_intkind),intent(out):: iret
    integer nc,dim1,dim2,dim3q,dim1x,dim2x,dim3x
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call sigio_axdbta(dbta,iret)
    nc=(head%jcap+1)*(head%jcap+2)
    dim1=nc
    dim2=head%levs
    dim3q=head%ntrac
    dim1x=head%lonb
    dim2x=head%latb
    dim3x=head%nxgr
    allocate(dbta%hs(dim1),dbta%ps(dim1),&
             dbta%t(dim1,dim2),dbta%d(dim1,dim2),dbta%z(dim1,dim2),&
             dbta%q(dim1,dim2,dim3q),&
             dbta%xgr(dim1x,dim2x,dim3x),dbta%xss(head%nxss),stat=iret)
    if(iret.ne.0) iret=-3
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine sigio_axdbta(dbta,iret)
    implicit none
    type(sigio_dbta),intent(inout):: dbta
    integer(sigio_intkind),intent(out):: iret
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    deallocate(dbta%hs,dbta%ps,dbta%t,dbta%d,dbta%z,dbta%q,dbta%xgr,stat=iret)
    nullify(dbta%hs,dbta%ps,dbta%t,dbta%d,dbta%z,dbta%q,dbta%xgr)
    if(iret.ne.0) iret=-3
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine sigio_srdbta(lu,head,dbta,iret)
    implicit none
    integer(sigio_intkind),intent(in):: lu
    type(sigio_head),intent(in):: head
    type(sigio_dbta),intent(inout):: dbta
    integer(sigio_intkind),intent(out):: iret
    type(sigio_data):: data
    integer:: nc,mdim1,mdim2,mdim3q,mdim1x,mdim2x,mdim3x,k,n,ios
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    nc=(head%jcap+1)*(head%jcap+2)
    mdim1=min(size(dbta%hs,1),size(dbta%ps,1),&
              size(dbta%t,1),size(dbta%d,1),size(dbta%z,1),&
              size(dbta%q,1))
    mdim2=min(size(dbta%t,2),size(dbta%d,2),size(dbta%z,2),&
              size(dbta%q,2))
    mdim3q=size(dbta%q,3)
    iret=-5
    if(mdim1.lt.nc.or.&
       mdim2.lt.head%levs.or.&
       mdim3q.lt.head%ntrac) return
    if(head%nxgr.gt.0) then
      mdim1x=size(dbta%xgr,1)
      mdim2x=size(dbta%xgr,2)
      mdim3x=size(dbta%xgr,3)
      if(mdim1x.lt.head%lonb.or.&
         mdim2x.lt.head%latb.or.&
         mdim3x.lt.head%nxgr) return
      if(size(dbta%xss).lt.head%nxss) return
    endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if(head%irealf.ne.2) then
      call sigio_aldata(head,data,iret)
      if(iret.ne.0) return
      call sigio_srdata(lu,head,data,iret)
      if(iret.ne.0) return
      dbta%hs(:nc)=data%hs(:nc)
      dbta%ps(:nc)=data%ps(:nc)
      dbta%t(:nc,:head%levs)=data%t(:nc,:head%levs)
      dbta%d(:nc,:head%levs)=data%d(:nc,:head%levs)
      dbta%z(:nc,:head%levs)=data%z(:nc,:head%levs)
      dbta%q(:nc,:head%levs,:head%ntrac)=data%q(:nc,:head%levs,:head%ntrac)
      dbta%xgr(:head%lonb,:head%latb,:head%nxgr)=&
       data%xgr(:head%lonb,:head%latb,:head%nxgr)
      dbta%xss(:head%nxss)=data%xss(:head%nxss)
      call sigio_axdata(data,iret)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    else
      iret=-4
      read(lu,iostat=ios) dbta%hs(:nc)
      if(ios.ne.0) return
      read(lu,iostat=ios) dbta%ps(:nc)
      if(ios.ne.0) return
      do k=1,head%levs
        read(lu,iostat=ios) dbta%t(:nc,k)
        if(ios.ne.0) return
      enddo
      do k=1,head%levs
        read(lu,iostat=ios) dbta%d(:nc,k)
        if(ios.ne.0) return
        read(lu,iostat=ios) dbta%z(:nc,k)
        if(ios.ne.0) return
      enddo
      do n=1,head%ntrac
        do k=1,head%levs
          read(lu,iostat=ios) dbta%q(:nc,k,n)
          if(ios.ne.0) return
        enddo
      enddo
      do n=1,head%nxgr
        read(lu,iostat=ios) dbta%xgr(:head%lonb,:head%latb,n)
        if(ios.ne.0) return
      enddo
      if(head%nxss.gt.0) then
        read(lu,iostat=ios) dbta%xss(:head%nxss)
        if(ios.ne.0) return
      endif
      iret=0
    endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine sigio_swdbta(lu,head,dbta,iret)
    implicit none
    integer(sigio_intkind),intent(in):: lu
    type(sigio_head),intent(in):: head
    type(sigio_dbta),intent(in):: dbta
    integer(sigio_intkind),intent(out):: iret
    type(sigio_data):: data
    integer:: nc,mdim1,mdim2,mdim3q,mdim1x,mdim2x,mdim3x,k,n,ios
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    nc=(head%jcap+1)*(head%jcap+2)
    mdim1=min(size(dbta%hs,1),size(dbta%ps,1),&
              size(dbta%t,1),size(dbta%d,1),size(dbta%z,1),&
              size(dbta%q,1))
    mdim2=min(size(dbta%t,2),size(dbta%d,2),size(dbta%z,2),&
              size(dbta%q,2))
    mdim3q=size(dbta%q,3)
    iret=-5
    if(mdim1.lt.nc.or.&
       mdim2.lt.head%levs.or.&
       mdim3q.lt.head%ntrac) return
    if(head%nxgr.gt.0) then
      mdim1x=size(dbta%xgr,1)
      mdim2x=size(dbta%xgr,2)
      mdim3x=size(dbta%xgr,3)
      if(mdim1x.lt.head%lonb.or.&
         mdim2x.lt.head%latb.or.&
         mdim3x.lt.head%nxgr) return
      if(size(dbta%xss).lt.head%nxss) return
    endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if(head%irealf.ne.2) then
      call sigio_aldata(head,data,iret)
      if(iret.ne.0) return
      data%hs(:nc)=dbta%hs(:nc)
      data%ps(:nc)=dbta%ps(:nc)
      data%t(:nc,:head%levs)=dbta%t(:nc,:head%levs)
      data%d(:nc,:head%levs)=dbta%d(:nc,:head%levs)
      data%z(:nc,:head%levs)=dbta%z(:nc,:head%levs)
      data%q(:nc,:head%levs,:head%ntrac)=dbta%q(:nc,:head%levs,:head%ntrac)
      data%xgr(:head%lonb,:head%latb,:head%nxgr)=&
       dbta%xgr(:head%lonb,:head%latb,:head%nxgr)
      data%xss(:head%nxss)=dbta%xss(:head%nxss)
      call sigio_swdata(lu,head,data,iret)
      if(iret.ne.0) return
      call sigio_axdata(data,iret)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    else
      iret=-4
      write(lu,iostat=ios) dbta%hs(:nc)
      if(ios.ne.0) return
      write(lu,iostat=ios) dbta%ps(:nc)
      if(ios.ne.0) return
      do k=1,head%levs
        write(lu,iostat=ios) dbta%t(:nc,k)
        if(ios.ne.0) return
      enddo
      do k=1,head%levs
        write(lu,iostat=ios) dbta%d(:nc,k)
        if(ios.ne.0) return
        write(lu,iostat=ios) dbta%z(:nc,k)
        if(ios.ne.0) return
      enddo
      do n=1,head%ntrac
        do k=1,head%levs
          write(lu,iostat=ios) dbta%q(:nc,k,n)
          if(ios.ne.0) return
        enddo
      enddo
      do n=1,head%nxgr
        write(lu,iostat=ios) dbta%xgr(:head%lonb,:head%latb,n)
        if(ios.ne.0) return
      enddo
      if(head%nxss.gt.0) then
        write(lu,iostat=ios) dbta%xss(:head%nxss)
        if(ios.ne.0) return
      endif
      iret=0
    endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine sigio_srohdcb(lu,cfname,head,dbta,iret)
    implicit none
    integer(sigio_intkind),intent(in):: lu
    character*(*),intent(in):: cfname
    type(sigio_head),intent(inout):: head
    type(sigio_dbta),intent(inout):: dbta
    integer(sigio_intkind),intent(out):: iret
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call sigio_sropen(lu,cfname,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call sigio_srhead(lu,head,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call sigio_aldbta(head,dbta,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call sigio_srdbta(lu,head,dbta,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call sigio_sclose(lu,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine sigio_swohdcb(lu,cfname,head,dbta,iret)
    implicit none
    integer(sigio_intkind),intent(in):: lu
    character*(*),intent(in):: cfname
    type(sigio_head),intent(inout):: head
    type(sigio_dbta),intent(in):: dbta
    integer(sigio_intkind),intent(out):: iret
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call sigio_swopen(lu,cfname,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call sigio_swhead(lu,head,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call sigio_swdbta(lu,head,dbta,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call sigio_sclose(lu,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine sigio_modpr(im,ix,km,nvcoord,idvc,idsl,vcoord,iret,&
                         ps,t,pd,dpddps,dpddt,pm,dpmdps,dpmdt)
    implicit none
    integer,intent(in):: im,ix,km,nvcoord,idvc,idsl
    real,intent(in):: vcoord(km+1,nvcoord)
    integer,intent(out):: iret
    real,intent(in),optional:: ps(ix),t(ix,km)
    real,intent(out),optional:: pd(ix,km),pm(ix,km)
    real,intent(out),optional:: dpddps(ix,km),dpddt(ix,km)
    real,intent(out),optional:: dpmdps(ix,km),dpmdt(ix,km)
    real(sigio_dblekind),parameter:: rocp=287.05/1004.6,rocpr=1/rocp
    real(sigio_dblekind),parameter:: t00=300.
    integer id1,id2
    real(sigio_dblekind) pid(im),dpiddps(im),dpiddt(im),tid(im),pidk(im)
    real(sigio_dblekind) piu,dpiudps,dpiudt,tiu,piuk
    real(sigio_dblekind) pmm,dpmdpid,dpmdpiu
    real(sigio_dblekind) pmk
    integer i,k
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if((idvc.eq.0.or.idvc.eq.1).and.nvcoord.eq.1.and.present(ps)) then
      id1=11
    elseif(idvc.eq.2.and.nvcoord.eq.2.and.present(ps)) then
      id1=22
    elseif(idvc.eq.3.and.nvcoord.eq.3.and.all(vcoord(:,3).eq.0).and.present(ps)) then
      id1=22
    elseif(idvc.eq.3.and.nvcoord.eq.2.and.present(ps).and.present(t)) then
      id1=32
    elseif(idvc.eq.3.and.nvcoord.eq.3.and.present(ps).and.present(t)) then
      id1=33
    else
      id1=0
    endif
    if(idsl.eq.0.or.idsl.eq.1) then
      id2=1
    elseif(idsl.eq.2) then
      id2=2
    else
      id2=0
    endif
    iret=0
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if(id1.gt.0.and.id2.gt.0) then
!$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(i)
      do i=1,im
        pid(i)=ps(i)
        dpiddps(i)=1
        dpiddt(i)=0
        tid(i)=0
        if(id2.eq.1) pidk(i)=pid(i)**rocp
      enddo
!$OMP END PARALLEL DO

!!$OMP PARALLEL DO DEFAULT(SHARED) &
!!$OMP& PRIVATE(i,k,piu,dpiudps,dpiudt,tiu,piuk,pmk,pmm,dpmdpid,dpmdpiu) &
!!$OMP& PRIVATE(pid,dpiddps,dpiddt,tid,pidk)

      do k=1,km
!$OMP PARALLEL DO DEFAULT(SHARED) &
!$OMP& PRIVATE(i,piu,dpiudps,dpiudt,tiu,piuk,pmk,pmm,dpmdpid,dpmdpiu)
        do i=1,im
          select case(id1)
          case(11)
            piu=vcoord(k+1,1)*ps(i)
            dpiudps=vcoord(k+1,1)
            dpiudt=0
          case(22)
            piu=vcoord(k+1,1)+vcoord(k+1,2)*ps(i)
            dpiudps=vcoord(k+1,2)
            dpiudt=0
          case(32)
            tiu=(t(i,k)+t(i,min(k+1,km)))/2
            piu=vcoord(k+1,2)*ps(i)+vcoord(k+1,1)*(tiu/t00)**rocpr
            dpiudps=vcoord(k+1,2)
            dpiudt=vcoord(k+1,1)*(tiu/t00)**rocpr*rocpr/tiu
            if(k.lt.km) dpiudt=dpiudt/2
          case(33)
            tiu=(t(i,k)+t(i,min(k+1,km)))/2
            piu=vcoord(k+1,1)+vcoord(k+1,2)*ps(i)+vcoord(k+1,3)*(tiu/t00)**rocpr
            dpiudps=vcoord(k+1,2)
            dpiudt=vcoord(k+1,3)*(tiu/t00)**rocpr*rocpr/tiu
            if(k.lt.km) dpiudt=dpiudt/2
          end select
          if(present(pd)) pd(i,k)=pid(i)-piu
          if(present(dpddps)) dpddps(i,k)=dpiddps(i)-dpiudps
          if(present(dpddt)) dpddt(i,k)=dpiddt(i)-dpiudt
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
          select case(id2)
          case(1)
            piuk=piu**rocp
            pmk=(pid(i)*pidk(i)-piu*piuk)/((rocp+1)*(pid(i)-piu))
            pmm=pmk**rocpr
            dpmdpid=rocpr*pmm/(pid(i)-piu)*(pidk(i)/pmk-1)
            dpmdpiu=rocpr*pmm/(pid(i)-piu)*(1-piuk/pmk)
          case(2)
            pmm=(pid(i)+piu)/2
            dpmdpid=0.5
            dpmdpiu=0.5
          end select
          if(present(pm)) pm(i,k)=pmm
          if(present(dpmdps)) dpmdps(i,k)=dpmdpid*dpiddps(i)+dpmdpiu*dpiudps
          if(present(dpmdt)) dpmdt(i,k)=dpmdpid*dpiddt(i)+dpmdpiu*dpiudt
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
          pid(i)=piu
          dpiddps(i)=dpiudps
          dpiddt(i)=dpiudt
          tid(i)=tiu
          if(id2.eq.1) pidk(i)=piuk
        enddo
!$OMP END PARALLEL DO
      enddo
!!$OMP END PARALLEL DO
    else
      if(id1.le.0) iret=iret+1
      if(id2.le.0) iret=iret+2
    endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine sigio_modprd(im,ix,km,nvcoord,idvc,idsl,vcoord,iret,&
                          ps,t,pd,dpddps,dpddt,pm,dpmdps,dpmdt)
    implicit none
    integer,intent(in):: im,ix,km,nvcoord,idvc,idsl
    real(sigio_dblekind),intent(in):: vcoord(km+1,nvcoord)
    integer,intent(out):: iret
    real(sigio_dblekind),intent(in),optional:: ps(ix),t(ix,km)
    real(sigio_dblekind),intent(out),optional:: pd(ix,km),pm(ix,km)
    real(sigio_dblekind),intent(out),optional:: dpddps(ix,km),dpddt(ix,km)
    real(sigio_dblekind),intent(out),optional:: dpmdps(ix,km),dpmdt(ix,km)
    real(sigio_dblekind),parameter:: rocp=287.05/1004.6,rocpr=1/rocp
    real(sigio_dblekind),parameter:: t00=300.
    integer id1,id2
    real(sigio_dblekind) pid(im),dpiddps(im),dpiddt(im),tid(im),pidk(im)
    real(sigio_dblekind) piu,dpiudps,dpiudt,tiu,piuk
    real(sigio_dblekind) pmm,dpmdpid,dpmdpiu
    real(sigio_dblekind) pmk
    integer i,k
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if((idvc.eq.0.or.idvc.eq.1).and.nvcoord.eq.1.and.present(ps)) then
      id1=11
    elseif(idvc.eq.2.and.nvcoord.eq.2.and.present(ps)) then
      id1=22
    elseif(idvc.eq.3.and.nvcoord.eq.3.and.all(vcoord(:,3).eq.0).and.present(ps)) then
      id1=22
    elseif(idvc.eq.3.and.nvcoord.eq.2.and.present(ps).and.present(t)) then
      id1=32
    elseif(idvc.eq.3.and.nvcoord.eq.3.and.present(ps).and.present(t)) then
      id1=33
    else
      id1=0
    endif
    if(idsl.eq.0.or.idsl.eq.1) then
      id2=1
    elseif(idsl.eq.2) then
      id2=2
    else
      id2=0
    endif
    iret=0
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if(id1.gt.0.and.id2.gt.0) then
!$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(i)
      do i=1,im
        pid(i)=ps(i)
        dpiddps(i)=1
        dpiddt(i)=0
        tid(i)=0
        if(id2.eq.1) pidk(i)=pid(i)**rocp
      enddo
!$OMP END PARALLEL DO

!!$OMP PARALLEL DO DEFAULT(SHARED) &
!!$OMP& PRIVATE(i,k,piu,dpiudps,dpiudt,tiu,piuk,pmk,pmm,dpmdpid,dpmdpiu) &
!!$OMP& PRIVATE(pid,dpiddps,dpiddt,tid,pidk)

      do k=1,km
!$OMP PARALLEL DO DEFAULT(SHARED) &
!$OMP& PRIVATE(i,piu,dpiudps,dpiudt,tiu,piuk,pmk,pmm,dpmdpid,dpmdpiu)
        do i=1,im
          select case(id1)
          case(11)
            piu=vcoord(k+1,1)*ps(i)
            dpiudps=vcoord(k+1,1)
            dpiudt=0
          case(22)
            piu=vcoord(k+1,1)+vcoord(k+1,2)*ps(i)
            dpiudps=vcoord(k+1,2)
            dpiudt=0
          case(32)
            tiu=(t(i,k)+t(i,min(k+1,km)))/2
            piu=vcoord(k+1,2)*ps(i)+vcoord(k+1,1)*(tiu/t00)**rocpr
            dpiudps=vcoord(k+1,2)
            dpiudt=vcoord(k+1,1)*(tiu/t00)**rocpr*rocpr/tiu
            if(k.lt.km) dpiudt=dpiudt/2
          case(33)
            tiu=(t(i,k)+t(i,min(k+1,km)))/2
            piu=vcoord(k+1,1)+vcoord(k+1,2)*ps(i)+vcoord(k+1,3)*(tiu/t00)**rocpr
            dpiudps=vcoord(k+1,2)
            dpiudt=vcoord(k+1,3)*(tiu/t00)**rocpr*rocpr/tiu
            if(k.lt.km) dpiudt=dpiudt/2
          end select
          if(present(pd)) pd(i,k)=pid(i)-piu
          if(present(dpddps)) dpddps(i,k)=dpiddps(i)-dpiudps
          if(present(dpddt)) dpddt(i,k)=dpiddt(i)-dpiudt
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
          select case(id2)
          case(1)
            piuk=piu**rocp
            pmk=(pid(i)*pidk(i)-piu*piuk)/((rocp+1)*(pid(i)-piu))
            pmm=pmk**rocpr
            dpmdpid=rocpr*pmm/(pid(i)-piu)*(pidk(i)/pmk-1)
            dpmdpiu=rocpr*pmm/(pid(i)-piu)*(1-piuk/pmk)
          case(2)
            pmm=(pid(i)+piu)/2
            dpmdpid=0.5
            dpmdpiu=0.5
          end select
          if(present(pm)) pm(i,k)=pmm
          if(present(dpmdps)) dpmdps(i,k)=dpmdpid*dpiddps(i)+dpmdpiu*dpiudps
          if(present(dpmdt)) dpmdt(i,k)=dpmdpid*dpiddt(i)+dpmdpiu*dpiudt
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
          pid(i)=piu
          dpiddps(i)=dpiudps
          dpiddt(i)=dpiudt
          tid(i)=tiu
          if(id2.eq.1) pidk(i)=piuk
        enddo
!$OMP END PARALLEL DO
      enddo
!!$OMP END PARALLEL DO
    else
      if(id1.le.0) iret=iret+1
      if(id2.le.0) iret=iret+2
    endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine sigio_adhead(head)
    implicit none
    type(sigio_head),intent(inout):: head
    integer jxss,nspec
    head%nxgr=0
    head%nxss=0
    if(head%ixgr.eq.1) then
      head%nxgr=2*head%levs+1
      head%nxss=0
    elseif(head%ixgr.eq.2) then
      head%nxgr=4*head%levs+3
      head%nxss=0
    elseif(head%ixgr.eq.3) then
      head%nxgr=3*head%levs+1
      head%nxss=0
    elseif(head%ixgr.eq.4) then
      head%nxgr=4*head%levs+3
      head%nxss=1
    elseif(head%ixgr.eq.5) then
      head%nxgr=3*head%levs+1
      head%nxss=1
    endif
    nspec=2+(3+head%ntrac)*head%levs
    if(head%ivs.eq.200509) then
      jxss=0
      if(head%nxss.gt.0) jxss=1
      head%nhead=5
      if (mod(head%idvm/10,10) == 3) then
        head%nhead=7
      endif
      head%ndata=nspec+head%nxgr+jxss
      if(allocated(head%lhead)) deallocate(head%lhead)
      if(allocated(head%ldata)) deallocate(head%ldata)
      allocate(head%lhead(head%nhead))
      allocate(head%ldata(head%ndata))
      if (mod(head%idvm/10,10) .ne. 3) then
        head%lhead=(/sigio_lhead1,4*(head%nhead+head%ndata),4*50,&
                   4*((head%levs+1)*head%nvcoord),8*(5+head%ntrac)/)
!     if (mod(head%idvm/10,10) == 3) then
      else
        head%lhead=(/sigio_lhead1,4*(head%nhead+head%ndata),4*50,   &
                   4*((head%levs+1)*head%nvcoord),8*(5+head%ntrac), &
                   4*(head%ntrac+1), 4*(head%ntrac+1)/)
!                  400, 400/)
      endif
      head%ldata(1:nspec)=4*head%irealf*(head%jcap+1)*(head%jcap+2)
      head%ldata(nspec+1:nspec+head%nxgr)=4*head%irealf*head%lonb*head%latb
      head%ldata(nspec+head%nxgr+1:head%ndata)=4*head%irealf*head%nxss
    else
      head%nhead=2
      head%ndata=nspec+head%nxgr
      if(allocated(head%lhead)) deallocate(head%lhead)
      if(allocated(head%ldata)) deallocate(head%ldata)
      allocate(head%lhead(head%nhead))
      allocate(head%ldata(head%ndata))
      head%lhead=(/sigio_lhead1,4*250/)
      head%ldata(1:nspec)=4*head%irealf*(head%jcap+1)*(head%jcap+2)
      head%ldata(nspec+1:nspec+head%nxgr)=4*head%irealf*head%lonb*head%latb
    endif
  end subroutine
!-------------------------------------------------------------------------------
  subroutine sigio_cnvpsv(im,idvm,ps,dp,cnflg)
    implicit none
    integer,intent(in)       :: im,idvm,cnflg
    real,intent(inout)       :: ps(im)
    real,intent(out)         :: dp(im)
    integer                  :: surfpress_id
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    surfpress_id = mod(idvm,10)
    if (cnflg > 0) then
      if (surfpress_id == 2) then
        dp=1.e3
        ps=ps*1.e3
      else
        dp=exp(ps)*1.e3
        ps=exp(ps)*1.e3
      endif
    else
      if (surfpress_id == 2) then
        dp=1/1.e3
        ps=ps/1.e3
      else
        dp=1/ps
        ps=log(ps/1.e3)
      endif
    endif
  end subroutine sigio_cnvpsv
!-------------------------------------------------------------------------------
  subroutine sigio_cnvtdv(im,ix,km,idvc,idvm,ntrac,iret,t,q,cpi,cnflg)
    implicit none
    integer,intent(in):: im,ix,km,idvc,idvm,ntrac,cnflg
    integer,intent(out):: iret
    real,intent(in)          :: q(ix,km,ntrac), cpi(0:ntrac)
    real,intent(inout)       :: t(ix,km)
    integer                  :: thermodyn_id, n
    real                     :: xcp(ix,km), sumq(ix,km)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    thermodyn_id = mod(IDVM/10,10)
!
    if (thermodyn_id == 3 .and. idvc == 3) then
      xcp(1:im,:)  = 0.0
      sumq(1:im,:) = 0.0
      do n=1,NTRAC
        if( cpi(n) .ne. 0.0) then
           xcp(1:im,:)  = xcp(1:im,:)  + q(1:im,:,n) * cpi(n)
           sumq(1:im,:) = sumq(1:im,:) + q(1:im,:,n)
        endif
      enddo
      xcp(1:im,:)  = (1.-sumq(1:im,:))*cpi(0) + xcp(1:im,:)   ! Mean Cp
!
    else
      xcp(1:im,:) = (1.+(461.50/287.05-1)*Q(1:im,:,1))        ! Virt factor
    endif
    if (cnflg > 0) then
      t(1:im,:) = t(1:im,:) / xcp(1:im,:)
    else
      t(1:im,:) = t(1:im,:) * xcp(1:im,:)
    endif
    iret = 0
!
    return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine sigio_cnvtdv
!-------------------------------------------------------------------------------
  subroutine sigio_cnvpsvd(im,idvm,ps,dp,cnflg)
    implicit none
    integer,intent(in)                       :: im,idvm,cnflg
    real(sigio_dblekind),intent(inout)       :: ps(im)
    real(sigio_dblekind),intent(out)         :: dp(im)
    integer                                  :: surfpress_id
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    surfpress_id = mod(idvm,10)
    if (cnflg > 0) then
      if (surfpress_id == 2) then
        dp=1.e3
        ps=ps*1.e3
      else
        dp=exp(ps)*1.e3
        ps=exp(ps)*1.e3
      endif
    else
      if (surfpress_id == 2) then
        dp=1/1.e3
        ps=ps/1.e3
      else
        dp=1/ps
        ps=log(ps/1.e3)
      endif
    endif
  end subroutine sigio_cnvpsvd
!-------------------------------------------------------------------------------
  subroutine sigio_cnvtdvd(im,ix,km,idvc,idvm,ntrac,iret,t,q,cpi,cnflg)
    implicit none
    integer,intent(in):: im,ix,km,idvc,idvm,ntrac,cnflg
    integer,intent(out):: iret
    real(sigio_realkind),intent(in)      :: cpi(0:ntrac)
    real(sigio_dblekind),intent(in)      :: q(ix,km,ntrac)
!   real(sigio_dblekind),intent(in)      :: q(ix,km,ntrac), cpi(0:ntrac)
    real(sigio_dblekind),intent(inout)   :: t(ix,km)
    integer                              :: thermodyn_id, n
    real(sigio_dblekind)                 :: xcp(ix,km), sumq(ix,km)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    thermodyn_id = mod(IDVM/10,10)
!
    if (thermodyn_id == 3 .and. idvc == 3) then
      xcp(1:im,:)  = 0.0
      sumq(1:im,:) = 0.0
      do n=1,NTRAC
        if( cpi(n) .ne. 0.0) then
           xcp(1:im,:)  = xcp(1:im,:)  + q(1:im,:,n) * cpi(n)
           sumq(1:im,:) = sumq(1:im,:) + q(1:im,:,n)
        endif
      enddo
      xcp(1:im,:)  = (1.-sumq(1:im,:))*cpi(0) + xcp(1:im,:)   ! Mean Cp
!
    else
      xcp(1:im,:) = (1.+(461.50/287.05-1)*Q(1:im,:,1))        ! Virt factor
    endif
    if (cnflg > 0) then
      t(1:im,:) = t(1:im,:) / xcp(1:im,:)
    else
      t(1:im,:) = t(1:im,:) * xcp(1:im,:)
    endif
    iret = 0
!
    return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine sigio_cnvtdvd
!-------------------------------------------------------------------------------
end module
