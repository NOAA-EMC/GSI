module general_specmod
!$$$   module documentation block
!                .      .    .                                       .
! module:    general_specmod
!   prgmmr: treadon          org: np23                date: 2003-11-24
!
! abstract: copy of specmod, introducing structure variable spec_vars, so
!             spectral code can be used for arbitrary resolutions.
!
! program history log:   
!   2003-11-24  treadon
!   2004-04-28  d. kokron, updated SGI's fft to use scsl
!   2004-05-18  kleist, documentation
!   2004-08-27  treadon - add/initialize variables/arrays needed by 
!                         splib routines for grid <---> spectral 
!                         transforms
!   2007-04-26  yang    - based on idrt value xxxx descriptionxxx
!   2010-02-18  parrish - copy specmod to general_specmod and add structure variable spec_vars.
!                           remove all *_b variables, since now not necessary to have two 
!                           resolutions.  any number of resolutions can be now contained in
!                           type(spec_vars) variables passed in through init_spec_vars.  also
!                           remove init_spec, since not really necessary.
!
! subroutines included:
!   sub general_init_spec_vars
!   sub general_destroy_spec_vars
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

! set default to private
  private
! set subroutines to public
  public :: general_init_spec_vars
  public :: general_destroy_spec_vars
! set passed variables to public
  public :: spec_vars

  type spec_vars

     integer(i_kind) jcap
     integer(i_kind) nc
     integer(i_kind) ncd2
     integer(i_kind) iromb
     integer(i_kind) idrt
     integer(i_kind) imax
     integer(i_kind) jmax
     integer(i_kind) ijmax
     integer(i_kind) jn
     integer(i_kind) js
     integer(i_kind) kw
     integer(i_kind) jb
     integer(i_kind) je
     integer(i_kind) ioffset
     logical,       pointer :: factsml(:)   => NULL()
     logical,       pointer :: factvml(:)   => NULL()
     real(r_kind),  pointer :: eps(:)       => NULL()
     real(r_kind),  pointer :: epstop(:)    => NULL()
     real(r_kind),  pointer :: enn1(:)      => NULL()
     real(r_kind),  pointer :: elonn1(:)    => NULL()
     real(r_kind),  pointer :: eon(:)       => NULL()
     real(r_kind),  pointer :: eontop(:)    => NULL()
     real(r_kind),  pointer :: clat(:)      => NULL()
     real(r_kind),  pointer :: slat(:)      => NULL()
     real(r_kind),  pointer :: wlat(:)      => NULL()
     real(r_kind),  pointer :: pln(:,:)     => NULL()
     real(r_kind),  pointer :: plntop(:,:)  => NULL()
     real(r_kind),  pointer :: test_mask(:) => NULL()
     real(r_kind),  pointer :: rlats(:)     => NULL()
     real(r_kind),  pointer :: slats(:)     => NULL()
     real(r_kind),  pointer :: clats(:)     => NULL()
     real(r_kind),  pointer :: rlons(:)     => NULL()
     real(r_kind),  pointer :: slons(:)     => NULL()
     real(r_kind),  pointer :: clons(:)     => NULL()
     real(r_double),pointer :: afft(:)      => NULL()
     logical:: lallocated = .false.

  end type spec_vars

contains

  subroutine general_init_spec_vars(sp,jcap,jcap_test,nlat_a,nlon_a,eqspace)
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
!   2010-02-18  parrish - substantial changes to simplify and introduce input/output variable
!                            type(spec_vars) sp
!   2010-04-01  treadon - remove mpimod and rad2deg constants (not used)
!
!   input argument list:
!     sp     - type(spec_vars) variable 
!     jcap   - target resolution
!     jcap_test - test resolution,  used to construct mask which will zero out coefs
!                  with total wavenumber n in range jcap_test < n <= jcap
!     nlat_a - number of latitudes on target grid
!     nlon_a - number of longitudes on target grid
!     eqspace - optional flag for equal spaced grid instead of gaussian grid
!
!   output argument list:
!     sp     - type(spec_vars) variable, ready to use for target spectral resolution/grid.
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    use constants, only: izero,ione,zero,half,one,two,pi
    implicit none

!   Declare passed variables
    type(spec_vars) ,intent(inout) :: sp
    integer(i_kind) ,intent(in   ) :: jcap,jcap_test,nlat_a,nlon_a
    logical,optional,intent(in   ) :: eqspace

!   Declare local variables    
    integer(i_kind) i,ii1,j,l,m
    integer(i_kind) :: ldafft
    real(r_kind) :: dlon_a,half_pi,two_pi


!   Set constants used in transforms for analysis grid
    sp%jcap=jcap
    sp%nc=(jcap+ione)*(jcap+2_i_kind)
    sp%ncd2=sp%nc/2
    sp%iromb=izero
    sp%idrt=4_i_kind
    if(present(eqspace)) then
       if(eqspace) sp%idrt=256_i_kind
    endif
    sp%imax=nlon_a
    sp%jmax=nlat_a-2_i_kind
    sp%ijmax=sp%imax*sp%jmax
    sp%ioffset=sp%imax*(sp%jmax-ione)
    sp%jn=sp%imax
    sp%js=-sp%jn
    sp%kw=2*sp%ncd2
    sp%jb=ione
    sp%je=(sp%jmax+ione)/2



!   Allocate and initialize fact arrays
    if(sp%lallocated) then
       deallocate(sp%factsml,sp%factvml,sp%eps,sp%epstop,sp%enn1,sp%elonn1,sp%eon,sp%eontop)
       deallocate(sp%clat,sp%slat,sp%wlat,sp%pln,sp%plntop,sp%test_mask,sp%afft)
       deallocate(sp%rlats,sp%clats,sp%slats)
       deallocate(sp%rlons,sp%clons,sp%slons)
    end if
    allocate(sp%factsml(sp%nc),sp%factvml(sp%nc),sp%test_mask(sp%nc))
    sp%factsml=.false.
    sp%factvml=.false.
    sp%test_mask=one
    ii1=izero
    do l=izero,sp%jcap
       do m=izero,sp%jcap-l
          ii1=ii1+2_i_kind
          if(l == izero)sp%factsml(ii1)=.true.
          if(l == izero)sp%factvml(ii1)=.true.
          if(l+m.gt.jcap_test) then
             sp%test_mask(ii1-ione)=zero
             sp%test_mask(ii1)=zero
          end if
       end do
    end do
    sp%factvml(1)=.true.

!   Allocate and initialize arrays used in transforms
    allocate( sp%eps(sp%ncd2) )
    allocate( sp%epstop(sp%jcap+ione) )
    allocate( sp%enn1(sp%ncd2) )
    allocate( sp%elonn1(sp%ncd2) )
    allocate( sp%eon(sp%ncd2) )
    allocate( sp%eontop(sp%jcap+ione) )
    ldafft=50000_i_kind+4*sp%imax ! ldafft=256+imax would be sufficient at GMAO.
    allocate( sp%afft(ldafft))
    allocate( sp%clat(sp%jb:sp%je) )
    allocate( sp%slat(sp%jb:sp%je) ) 
    allocate( sp%wlat(sp%jb:sp%je) ) 
    allocate( sp%pln(sp%ncd2,sp%jb:sp%je) )
    allocate( sp%plntop(sp%jcap+ione,sp%jb:sp%je) )
    call sptranf0(sp%iromb,sp%jcap,sp%idrt,sp%imax,sp%jmax,sp%jb,sp%je, &
       sp%eps,sp%epstop,sp%enn1,sp%elonn1,sp%eon,sp%eontop, &
       sp%afft,sp%clat,sp%slat,sp%wlat,sp%pln,sp%plntop)
!     obtain rlats and rlons
    half_pi=half*pi
    two_pi=two*pi
    allocate(sp%rlats(nlat_a),sp%rlons(nlon_a))
    allocate(sp%clats(nlat_a),sp%clons(nlon_a))
    allocate(sp%slats(nlat_a),sp%slons(nlon_a))
    sp%rlats(1)=-half_pi
    sp%clats(1)=zero
    sp%slats(1)=-one
    sp%rlats(nlat_a)=half_pi
    sp%clats(nlat_a)=zero
    sp%slats(nlat_a)=one
    do i=1,(nlat_a-2_i_kind)/2_i_kind
       sp%rlats(nlat_a-i)= asin(sp%slat(i))
       sp%clats(nlat_a-i)=      sp%clat(i)
       sp%slats(nlat_a-i)=      sp%slat(i)
       sp%rlats(ione+i  )=-asin(sp%slat(i))
       sp%clats(ione+i  )=      sp%clat(i)
       sp%slats(ione+i  )=     -sp%slat(i)
    end do
    dlon_a=two_pi/nlon_a
    do j=1,nlon_a
       sp%rlons(j)=(j-one)*dlon_a
       sp%clons(j)=cos(sp%rlons(j))
       sp%slons(j)=sin(sp%rlons(j))
    end do
    sp%lallocated=.true.

    return
  end subroutine general_init_spec_vars

  subroutine general_destroy_spec_vars(sp)
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
!   2010-02-18  parrish, copy destroy_spec_vars to general_destroy_spec_vars
!
!   input argument list:
!     sp      -  type(spec_vars) variable, which is no longer required
!
!   output argument list:
!     sp      -  arrays in sp deleted
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    implicit none
    type(spec_vars),intent(inout) :: sp

    if(sp%lallocated) then
       deallocate(sp%factsml,sp%factvml)
       deallocate(sp%eps,sp%epstop,sp%enn1,sp%elonn1,sp%eon,sp%eontop,sp%afft,&
          sp%clat,sp%slat,sp%wlat,sp%pln,sp%plntop)
       sp%lallocated=.false.
    end if

    return
  end subroutine general_destroy_spec_vars

end module general_specmod
