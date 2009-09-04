module specmod
!$$$   module documentation block
!                .      .    .                                       .
! module:    specmod
!   prgmmr: treadon          org: np23                date: 2003-11-24
!
! abstract: module containing spectral related variables
!
! program history log:   
!   2003-11-24  treadon
!   2004-04-28  d. kokron, updated SGI's fft to use scsl
!   2004-05-18  kleist, documentation
!   2004-08-27  treadon - add/initialize variables/arrays needed by 
!                         splib routines for grid <---> spectral 
!                         transforms
!   2007-04-26  yang    - based on idrt value xxxx descriptionxxx
!
! remarks: variable definitions below
!   def jcap         - spectral (assumed triangular) truncation
!   def nc           - (N+1)*(N+2); N=truncation
!   def nc1          - 2*(N+1); N=truncation
!   def ncd2         - [(N+1)*(N+2)]/2; N=truncation
!   def jnpe         - (N+2)/2; N=truncation
!   def factsml      - factor to ensure proper scalar coefficients are zero
!   def factvml      - factor to ensure proper vector coefficients are zero
!   def iromb        - integer spectral domain shape
!                      (0 for triangular, 1 for rhomboidal)
!   def idrt         - integer grid identifier
!                      (idrt=4 for gaussian grid,
!                       idrt=0 for equally-spaced grid including poles,
!                       idrt=256 for equally-spaced grid excluding poles)
!   def imax         - integer even number of longitudes for transform
!   def jmax         - integer number of latitudes for transform
!   def ijmax        - integer imax*jmax
!   def jn           - integer skip number between n.h. latitudes from north
!   def js           - integer skip number between s.h. latitudes from south
!   def kw           - integer skip number between wave fields
!   def jb           - integer latitude index (from pole) to begin transform
!   def je           - integer latitude index (from pole) to end transform
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$   end documentation block
  use kinds, only: r_kind,r_double,i_kind
  implicit none

  integer(i_kind),save :: jcap,nc,ncd2
  integer(i_kind),save :: iromb,idrt,imax,jmax,ijmax,jn,js,kw,jb,je,ioffset
  logical,allocatable,dimension(:),save :: factsml,factvml
  real(r_kind),allocatable,dimension(:),save :: eps,epstop,enn1,elonn1,eon,eontop
  real(r_kind),allocatable,dimension(:),save :: clat,slat,wlat
  real(r_kind),allocatable,dimension(:,:),save :: pln,plntop
  real(r_double),allocatable,dimension(:),save :: afft
  integer(i_kind) jcap_b,nc_b,ncd2_b
  integer(i_kind) iromb_b,idrt_b
  logical,allocatable,dimension(:):: factsml_b,factvml_b
  real(r_kind),allocatable,dimension(:):: eps_b,epstop_b,enn1_b,elonn1_b,eon_b,eontop_b
  real(r_kind),allocatable,dimension(:,:):: pln_b,plntop_b


contains
  
  subroutine init_spec
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_spec
!   prgmmr: treadon          org: np23                date: 2003-11-24
!
! abstract: initialize spectral variables to defaults
!
! program history log:
!   2003-11-24  treadon
!   2004-05-18  kleist, new variables and documentation
!   2004-08-27  treadon - move nlath to berror.f90
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    implicit none

    jcap=62
    jcap_b=62

    return
  end subroutine init_spec

  subroutine init_spec_vars(nlat,nlon,eqspace)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_spec_vars
!   prgmmr: treadon          org: np23                date: 2003-11-24
!
! abstract: initialize spectral variables
!
! program history log:
!   2003-11-24  treadon
!   2004-05-18  kleist, new variables and documentation
!   2004-08-27  treadon - add call to sptranf0 and associated arrays, 
!                         remove del21 and other unused arrays/variables
!   2006-04-06  middlecoff - remove jc=ncpus() since not used
!   2008-04-11  safford    - rm unused vars
!
!   input argument list:
!     nlat    - number of Gaussian latitudes
!     nlon    - number of longitudes
!     eqspace
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    use constants, only: izero
    implicit none

!   Declare passed variables
    integer(i_kind),intent(in):: nlat,nlon
    logical,optional,intent(in) :: eqspace

!   Declare local variables    
    integer(i_kind) ii1,l,m

    integer(i_kind) :: ldafft

!   Set constants
    nc=(jcap+1)*(jcap+2)
    ncd2=nc/2
    nc_b=(jcap_b+1)*(jcap_b+2)
    ncd2_b=nc_b/2


!   Allocate more arrays related to transforms
    allocate(factsml(nc),factvml(nc))
    allocate(factsml_b(nc_b),factvml_b(nc_b))


!   Set up factsml and factvml
    factsml=.false.
    factvml=.false.
    ii1=izero
    do l=izero,jcap
       do m=izero,jcap-l
          ii1=ii1+2
          if(l == izero)factsml(ii1)=.true.
          if(l == izero)factvml(ii1)=.true.
       end do
    end do
    factvml(1)=.true.
    factsml_b=.false.
    factvml_b=.false.
    ii1=izero
    do l=izero,jcap_b
       do m=izero,jcap_b-l
          ii1=ii1+2
          if(l == izero)factsml_b(ii1)=.true.
          if(l == izero)factvml_b(ii1)=.true.
       end do
    end do
    factvml_b(1)=.true.


!   Set other constants used in transforms
    iromb=0
    idrt=4
    if(present(eqspace)) then
      if(eqspace) idrt=256
    endif
    imax=nlon
    jmax=nlat-2
    ijmax=imax*jmax
    ioffset=imax*(jmax-1)
    jn=imax
    js=-jn
    kw=2*ncd2
    jb=1
    je=(jmax+1)/2

!   Allocate arrays
    allocate( eps(ncd2) )
    allocate( epstop(jcap+1) )
    allocate( enn1(ncd2) )
    allocate( elonn1(ncd2) )
    allocate( eon(ncd2) )
    allocate( eontop(jcap+1) )
    ldafft=50000+4*imax ! ldafft=256+imax would be sufficient at GMAO.
    allocate( afft(ldafft))
    allocate( clat(jb:je) )
    allocate( slat(jb:je) ) 
    allocate( wlat(jb:je) ) 
    allocate( pln(ncd2,jb:je) )
    allocate( plntop(jcap+1,jb:je) )

!   Initialize arrays used in transforms
    call sptranf0(iromb,jcap,idrt,imax,jmax,jb,je, &
       eps,epstop,enn1,elonn1,eon,eontop, &
       afft,clat,slat,wlat,pln,plntop)
    iromb_b=0
    idrt_b=4
    allocate( eps_b(ncd2_b) )
    allocate( epstop_b(jcap_b+1) )
    allocate( enn1_b(ncd2_b) )
    allocate( elonn1_b(ncd2_b) )
    allocate( eon_b(ncd2_b) )
    allocate( eontop_b(jcap_b+1) )
    allocate( pln_b(ncd2_b,jb:je) )
    allocate( plntop_b(jcap_b+1,jb:je) )

!   Initialize arrays used in transforms for background spectral truncation
    call sptranf0(iromb_b,jcap_b,idrt_b,imax,jmax,jb,je, &
       eps_b,epstop_b,enn1_b,elonn1_b,eon_b,eontop_b, &
       afft,clat,slat,wlat,pln_b,plntop_b)

    return
  end subroutine init_spec_vars

  subroutine destroy_spec_vars
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    destroy_spec_vars
!   prgmmr: treadon          org: np23                date: 2003-11-24
!
! abstract: deallocate memory from spectral variables
!
! program history log:
!   2003-11-24  treadon
!   2004-05-18  kleist, new variables and documentation
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    implicit none

    deallocate(factsml,factvml,factsml_b,factvml_b)
    deallocate(eps,epstop,enn1,elonn1,eon,eontop,afft,&
       clat,slat,wlat,pln,plntop)
    deallocate(eps_b,epstop_b,enn1_b,elonn1_b,eon_b, &
       eontop_b,pln_b,plntop_b)

    return
  end subroutine destroy_spec_vars

end module specmod
