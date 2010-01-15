module specmod_special
!!!!!!!!!!!!!!!!!!!!!NOTE:  this is a partial copy of specmod.f90, for local use only for isotropic
!!!!!!!!!!!!!!!!!!!!!!!!!    localization of global hybrid ensemble control variable a_en.
!!!!!!!!!!!!!!!!!!!!!!!!!    this copy was made so the spectral resolution could be adjusted
!!!!!!!!!!!!!!!!!!!!!!!!!!   based on the localization correlation length.  Since this length
!!!!!!!!!!!!!!!!!!!!!!!!!!!  is expected to be large compared to the actual model background
!!!!!!!!!!!!!!!!!!!!!!!!!!   error, lower spectral resolution can be used, saving on time
!!!!!!!!!!!!!!!!!!!!!!!!!!   spent applying localization to a_en.
!$$$   module documentation block
!                .      .    .                                       .
! module:    specmod_special   partial copy of module specmod from specmod.f90
!   prgmmr: parrish          org: np23                date: 2009-12-17
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

! set default to private
  private
! set subroutines to public
  public :: init_spec_vars_special
  public :: destroy_spec_vars_special
! set passed variables to public
  public :: nc,ijmax,iromb,jcap,idrt,imax,jmax,factsml,wlat,jb,je,slat, &
            jn,js,ioffset,eps,epstop,enn1,elonn1,eon,eontop,afft,clat,pln,plntop

  integer(i_kind),save :: jcap,nc,ncd2
  integer(i_kind),save :: iromb,idrt,imax,jmax,ijmax,jn,js,kw,jb,je,ioffset
  logical,allocatable,dimension(:),save :: factsml,factvml
  real(r_kind),allocatable,dimension(:),save :: eps,epstop,enn1,elonn1,eon,eontop
  real(r_kind),allocatable,dimension(:),save :: clat,slat,wlat
  real(r_kind),allocatable,dimension(:,:),save :: pln,plntop
  real(r_double),allocatable,dimension(:),save :: afft

contains

  subroutine init_spec_vars_special(nlat,nlon,eqspace)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    copy of init_spec_vars
!   prgmmr: parrish          org: np23                date: 2009-12-17
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
    use constants, only: izero,ione
    implicit none

!   Declare passed variables
    integer(i_kind) ,intent(in   ) :: nlat,nlon
    logical,optional,intent(in   ) :: eqspace

!   Declare local variables    
    integer(i_kind) ii1,l,m

    integer(i_kind) :: ldafft

!   Set constants
    nc=(jcap+ione)*(jcap+2_i_kind)
    ncd2=nc/2

!   Allocate more arrays related to transforms
    allocate(factsml(nc),factvml(nc))

!   Set up factsml and factvml
    factsml=.false.
    factvml=.false.
    ii1=izero
    do l=izero,jcap
       do m=izero,jcap-l
          ii1=ii1+2_i_kind
          if(l == izero)factsml(ii1)=.true.
          if(l == izero)factvml(ii1)=.true.
       end do
    end do
    factvml(1)=.true.

!   Set other constants used in transforms
    iromb=izero
    idrt=4_i_kind
    if(present(eqspace)) then
       if(eqspace) idrt=256_i_kind
    endif
    imax=nlon
    jmax=nlat-2_i_kind
    ijmax=imax*jmax
    ioffset=imax*(jmax-ione)
    jn=imax
    js=-jn
    kw=2*ncd2
    jb=ione
    je=(jmax+ione)/2

!   Allocate arrays
    allocate( eps(ncd2) )
    allocate( epstop(jcap+ione) )
    allocate( enn1(ncd2) )
    allocate( elonn1(ncd2) )
    allocate( eon(ncd2) )
    allocate( eontop(jcap+ione) )
    ldafft=50000_i_kind+4*imax ! ldafft=256+imax would be sufficient at GMAO.
    allocate( afft(ldafft))
    allocate( clat(jb:je) )
    allocate( slat(jb:je) ) 
    allocate( wlat(jb:je) ) 
    allocate( pln(ncd2,jb:je) )
    allocate( plntop(jcap+ione,jb:je) )

!   Initialize arrays used in transforms
    call sptranf0(iromb,jcap,idrt,imax,jmax,jb,je, &
       eps,epstop,enn1,elonn1,eon,eontop, &
       afft,clat,slat,wlat,pln,plntop)

    return
  end subroutine init_spec_vars_special

  subroutine destroy_spec_vars_special
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    copy of destroy_spec_vars
!   prgmmr: parrish          org: np23                date: 2009-12-17
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

    deallocate(factsml,factvml)
    deallocate(eps,epstop,enn1,elonn1,eon,eontop,afft,&
       clat,slat,wlat,pln,plntop)

    return
  end subroutine destroy_spec_vars_special

end module specmod_special

module transform_special
!!!!!!!!!!!!!!!!!!!!!NOTE:  this is a partial copy of transform.f90, for local use only for isotropic
!!!!!!!!!!!!!!!!!!!!!!!!!    localization of global hybrid ensemble control variable a_en.
!!!!!!!!!!!!!!!!!!!!!!!!!    this copy was made so the spectral resolution could be adjusted
!!!!!!!!!!!!!!!!!!!!!!!!!!   based on the localization correlation length.  Since this length
!!!!!!!!!!!!!!!!!!!!!!!!!!!  is expected to be large compared to the actual model background
!!!!!!!!!!!!!!!!!!!!!!!!!!   error, lower spectral resolution can be used, saving on time
!!!!!!!!!!!!!!!!!!!!!!!!!!   spent applying localization to a_en.
!$$$   module documentation block
!                .      .    .                                       .
! module:    transform_special   partial copy of transform.f90
!   prgmmr: parrish          org: np23                date: 2009-12-17
!
! abstract: module containing spectral related variables
!
! program history log:   
!   2003-11-24  iredell
!
! remarks: variable definitions below
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$   end documentation block

  use kinds, only: r_kind,i_kind
  implicit none

! set default to private
  private
! set subroutines to public
  public :: sptez_s_special

contains

subroutine sptez_s_special(wave,grid,idir)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  sptez_s_special  copy of sptez_s
!   prgmmr: parrish          org: np23                date: 2009-12-27
!
! absract: this subprogram performs a spherical transform
!           between spectral coefficients of a scalar quantity
!           and a field on a global cylindrical grid.
!           the wave-space can be either triangular or rhomboidal.
!           the grid-space can be either an equally-spaced grid
!           (with or without pole points) or a gaussian grid.
!           the wave field is in sequential 'ibm order'.
!           the grid field is indexed east to west, then north to south.
!           for more flexibility and efficiency, call sptran.
!           subprogram can be called from a multiprocessing environment.
!
!           This routine differs from splib routine sptez in that
!              1) the calling list only contains the in/out arrays and 
!                 flag for the direction in which to transform
!              2) it calls a version of sptranf that does not invoke 
!                 initialization routines on each entry
!              3) some generality built into the splib version is
!                 removed in the code below
!
! program history log:
!   1996-02-29  iredell
!   2004-08-23  treadon - adapt splib routine sptez for gsi use
!   2007-04-25  errico  - replace use of duplicate arguments in sptranf_s
!   input arguments:
!     wave     - real (2*mx) wave field if idir>0
!                where mx=(jcap+1)*((iromb+1)*jcap+2)/2
!     grid     - real (imax,jmax) grid field (e->w,n->s) if idir<0
!     idir     - integer transform flag
!                (idir>0 for wave to grid, idir<0 for grid to wave)
!
!   output arguments:
!     wave     - real (2*mx) wave field if idir<0
!                where mx=(maxwv+1)*((iromb+1)*maxwv+2)/2
!     grid     - real (imax,jmax) grid field (e->w,n->s) if idir>0
!
! subprograms called:
!   sptranf_s  -  perform a scalar spherical transform
!
! remarks: minimum grid dimensions for unaliased transforms to spectral:
!   dimension                    linear              quadratic
!   -----------------------      ---------           -------------
!   imax                         2*maxwv+2           3*maxwv/2*2+2
!   jmax (idrt=4,iromb=0)        1*maxwv+1           3*maxwv/2+1
!   jmax (idrt=4,iromb=1)        2*maxwv+1           5*maxwv/2+1
!   jmax (idrt=0,iromb=0)        2*maxwv+3           3*maxwv/2*2+3
!   jmax (idrt=0,iromb=1)        4*maxwv+3           5*maxwv/2*2+3
!   jmax (idrt=256,iromb=0)      2*maxwv+1           3*maxwv/2*2+1
!   jmax (idrt=256,iromb=1)      4*maxwv+1           5*maxwv/2*2+1
!   -----------------------      ---------           -------------
!
! attributes:
!   language: fortran 77
!
!$$$
  use kinds, only: r_kind,i_kind
  use specmod_special, only: nc,ijmax
  use constants, only: izero,zero
  implicit none

! Declare passed variables
  integer(i_kind)              ,intent(in   ) :: idir
  real(r_kind),dimension(nc)   ,intent(inout) :: wave
  real(r_kind),dimension(ijmax),intent(inout) :: grid

! Declare local variables
  integer(i_kind) i

! Zero appropriate output array based on direction of transform
  if (idir<izero) then
     do i=1,nc
        wave(i)=zero
     end do
  elseif (idir>izero) then
     do i=1,ijmax
        grid(i)=zero
     end do
  endif

! Call spectral <--> grid transform
  call sptranf_s_special(wave,grid,idir)

  return
end subroutine sptez_s_special

subroutine sptranf_s_special(wave,grid,idir)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  sptranf_s     perform a scalar spherical transform
!   prgmmr: iredell          org: np23                date: 1996-02-29
!
! abstract: this subprogram performs a spherical transform
!           between spectral coefficients of scalar quantities
!           and fields on a global cylindrical grid.
!           the wave-space can be either triangular or rhomboidal.
!           the grid-space can be either an equally-spaced grid
!           (with or without pole points) or a gaussian grid.
!           the wave and grid fields may have general indexing,
!           but each wave field is in sequential 'ibm order',
!           i.e. with zonal wavenumber as the slower index.
!           transforms are done in latitude pairs for efficiency;
!           thus grid arrays for each hemisphere must be passed.
!           if so requested, just a subset of the latitude pairs
!           may be transformed in each invocation of the subprogram.
!           the transforms are all multiprocessed over latitude except
!           the transform from fourier to spectral is multiprocessed
!           over zonal wavenumber to ensure reproducibility.
!           transform several fields at a time to improve vectorization.
!           subprogram can be called from a multiprocessing environment.
!
!           This routine differs from splib routine sptranf in that
!           it does not call sptranf0 (an initialization routine).
!
! program history log:
!   1996-02-29  iredell
!   1998-12-15  iredell  generic fft used
!   2004-08-23  treadon - adapt splib routine sptranf for gsi use
!   2006-05-03  treadon - remove jc from specmod list since not used
!   2007-04-25  errico  - replace use of duplicate arguments
!   2008-04-03  safford - rm unused vars and uses
!
!   input arguments:
!     wave     - real (*) wave fields if idir>0
!     grid     - real (*) grid fields (starting at jb) if idir<0
!     idir     - integer transform flag
!                (idir>0 for wave to grid, idir<0 for grid to wave)
!
!   output arguments:
!     wave     - real (*) wave fields if idir<0
!     grid     - real (*) grid fields (starting at jb) if idir>0
!
! subprograms called:
!   sptranf1     sptranf spectral transform
!   
! remarks: 
!   This routine assumes that splib routine sptranf0 has been 
!   previously called.  sptranf0 initializes arrays needed in
!   the transforms.
!
!   minimum grid dimensions for unaliased transforms to spectral:
!   dimension                    linear              quadratic
!   -----------------------      ---------           -------------
!   imax                         2*maxwv+2           3*maxwv/2*2+2
!   jmax (idrt=4,iromb=0)        1*maxwv+1           3*maxwv/2+1
!   jmax (idrt=4,iromb=1)        2*maxwv+1           5*maxwv/2+1
!   jmax (idrt=0,iromb=0)        2*maxwv+3           3*maxwv/2*2+3
!   jmax (idrt=0,iromb=1)        4*maxwv+3           5*maxwv/2*2+3
!   jmax (idrt=256,iromb=0)      2*maxwv+1           3*maxwv/2*2+1
!   jmax (idrt=256,iromb=1)      4*maxwv+1           5*maxwv/2*2+1
!   -----------------------      ---------           -------------
!
! attributes:
!   language: fortran 77
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: izero,ione,zero
  use specmod_special, only: iromb,jcap,idrt,imax,jmax,ijmax,&
       jn,js,jb,je,nc,ioffset,&
       eps,epstop,enn1,elonn1,eon,eontop,&
       afft,clat,slat,wlat,pln,plntop
  implicit none

! Declare passed variables
  integer(i_kind)              ,intent(in   ) :: idir
  real(r_kind),dimension(nc)   ,intent(inout) :: wave
  real(r_kind),dimension(ijmax),intent(inout) :: grid

! Declare local variables
  integer(i_kind) i,j,jj,ijn,ijs,mp
  real(r_kind),dimension(2*(jcap+ione)):: wtop
  real(r_kind),dimension(imax,2):: g
  integer(i_kind) :: ldafft
  ldafft=256_i_kind+imax

! Initialize local variables
  mp=izero

  do i=1,2*(jcap+ione)
     wtop(i)=zero
  end do

! Transform wave to grid
  if(idir >  izero) then
!!$omp parallel do private(j,i,jj,ijn,ijs,g)
     do j=jb,je
        call sptranf1(iromb,jcap,idrt,imax,jmax,j,j, &
             eps,epstop,enn1,elonn1,eon,eontop, &
             afft(1:ldafft),clat(j),slat(j),wlat(j), &
             pln(1,j),plntop(1,j),mp, &
             wave,wtop,g,idir)
        do i=1,imax
           jj  = j-jb
           ijn = i + jj*jn
           ijs = i + jj*js + ioffset
           grid(ijn)=g(i,1)
           grid(ijs)=g(i,2)
        enddo
     enddo
!!$omp end parallel do

! Transform grid to wave
  else
!!$omp parallel do private(j,i,jj,ijn,ijs,g)
     do j=jb,je
        if(wlat(j) >  zero) then
           do i=1,imax
              jj  = j-jb
              ijn = i + jj*jn
              ijs = i + jj*js + ioffset
              g(i,1)=grid(ijn)
              g(i,2)=grid(ijs)
              
           enddo
           call sptranf1(iromb,jcap,idrt,imax,jmax,j,j, &
                eps,epstop,enn1,elonn1,eon,eontop, &
                afft(1:ldafft),clat(j),slat(j),wlat(j), &
                pln(1,j),plntop(1,j),mp, &
                wave,wtop,g,idir)
        endif
     enddo
!!$omp end parallel do
  endif
end subroutine sptranf_s_special

end module transform_special

module spectral_transforms_special
!!!!!!!!!!!!!!!!!!!!!NOTE:  this is a partial copy of spectral_transforms.f90, for local use only for
!!!!!!!!!!!!!!!!!!!!!!!!!    isotropic localization of global hybrid ensemble control variable a_en.
!!!!!!!!!!!!!!!!!!!!!!!!!    this copy was made so the spectral resolution could be adjusted
!!!!!!!!!!!!!!!!!!!!!!!!!!   based on the localization correlation length.  Since this length
!!!!!!!!!!!!!!!!!!!!!!!!!!!  is expected to be large compared to the actual model background
!!!!!!!!!!!!!!!!!!!!!!!!!!   error, lower spectral resolution can be used, saving on time
!!!!!!!!!!!!!!!!!!!!!!!!!!   spent applying localization to a_en.
!$$$   module documentation block
!                .      .    .                                       .
! module:    spectral_transforms_special   partial copy of spectral_transforms.f90
!   prgmmr: parrish          org: np23                date: 2009-12-17
!
! abstract: module containing spectral related variables
!
! program history log:   
!   2003-11-24  iredell
!
! remarks: variable definitions below
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$   end documentation block

  use kinds, only: r_kind,i_kind
  implicit none

! set default to private
  private
! set subroutines to public
  public :: s2g0_special
  public :: s2g0_ad_special
  public :: g2s0_special
  public :: g2s0_ad_special

contains

subroutine g2s0_special(spectral_out,grid_in)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    g2s0_special     copy of g2s0
!   prgmmr: parrish          org: np23                date: 2009-12-17
!
! abstract: transform scalar from gaussian grid to spherical harmonic coefficients.
!           This works for equally spaced grid also
!
! program history log:
!   2006-07-15  kleist
!
!   input argument list:
!     grid_in  - input grid field on gaussian grid
!
!   output argument list:
!     spectral_out - output spherical harmonic coefficients
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use specmod_special, only: nc,factsml
  use kinds, only: r_kind,i_kind
  use constants, only: ione,zero
  use gridmod, only: nlat,nlon
  use transform_special, only: sptez_s_special
  implicit none

  real(r_kind),intent(  out) :: spectral_out(nc)
  real(r_kind),intent(in   ) :: grid_in(nlat,nlon)

  real(r_kind) work(nlon,nlat-2_i_kind),spec_work(nc)
  integer(i_kind) i,j,jj

!  Transfer contents of input grid to local work array
!  Reverse ordering in j direction from n-->s to s-->n
  do j=2,nlat-ione
     jj=nlat-j
     do i=1,nlon
        work(i,jj)=grid_in(j,i)
     end do
  end do
  call sptez_s_special(spec_work,work,-ione)

  do i=1,nc
     spectral_out(i)=spec_work(i)
     if(factsml(i))spectral_out(i)=zero
  end do
 
  return
end subroutine g2s0_special

subroutine g2s0_ad_special(spectral_in,grid_out)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    g2s0_ad_special     copy of g2s0_ad
!   prgmmr: parrish          org: np23                date: 2009-12-17
!
! abstract: adjoint of g2s0
!
! program history log:
!   2006-07-15  kleist
!   2007-05-15  errico  - Correct for proper use if grid includes equator 
!   2008-04-11  safford - rm unused var
!
!   input argument list:
!     spectral_in  - input spherical harmonic coefficients
!
!   output argument list:
!     grid_out - output grid field on gaussian grid
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use specmod_special, only: jcap,nc,factsml,wlat,jb,je
  use kinds, only: r_kind,i_kind
  use constants, only: izero,ione,zero,half,two
  use gridmod, only: nlat,nlon
  use transform_special, only: sptez_s_special
  implicit none

  real(r_kind),intent(in   ) :: spectral_in(nc)
  real(r_kind),intent(  out) :: grid_out(nlat,nlon)

  real(r_kind) work(nlon,nlat-2_i_kind),spec_work(nc)
  integer(i_kind) i,j,jj

  do i=1,nc
     spec_work(i)=spectral_in(i)/float(nlon)
     if(factsml(i))spec_work(i)=zero
  end do
  do i=2*jcap+3_i_kind,nc
     spec_work(i)=half*spec_work(i)
  end do
 
  call sptez_s_special(spec_work,work,ione)

!
! If nlat odd, then j=je is the equator.  The factor of 2 is because, 
! je is referenced only once, not twice as in the spectral transform 
! routines where half of the equator is considered in each hemisphere,
! separately. 
  do j=jb,je-mod(nlat,2_i_kind)
     do i=1,nlon
        work(i,j)=work(i,j)*wlat(j)
        work(i,nlat-ione-j)=work(i,nlat-ione-j)*wlat(j)
     end do
  end do
  
  if (mod(nlat,2_i_kind)  /=  izero) then
     do i=1,nlon
        work(i,je)=work(i,je)*two*wlat(je)
     end do
  endif

!  Transfer contents of output grid to local work array
!  Reverse ordering in j direction from n-->s to s-->n
  do j=2,nlat-ione
     jj=nlat-j
     do i=1,nlon
        grid_out(j,i)=work(i,jj)
     end do
  end do

!  Load zero into pole points
  do i=1,nlon
     grid_out(1,i)   =zero
     grid_out(nlat,i)=zero
  end do

  return
end subroutine g2s0_ad_special

subroutine s2g0_special(spectral_in,grid_out)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    s2g0_special        copy of s2g0
!   prgmmr: parrish          org: np23                date: 2009-12-17
!
! abstract: inverse of g2s0
!
! program history log:
!   2006-07-15  kleist
!   2007-05-15  errico - add call to spectra_pole_scalar
!   2008-04-11  safford - rm unused uses
!
!   input argument list:
!     spectral_in  - input spherical harmonic coefficients
!
!   output argument list:
!     grid_out - output grid field on gaussian grid
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use specmod_special, only: nc,factsml
  use kinds, only: r_kind,i_kind
  use constants, only: ione,zero,one,two
  use gridmod, only: nlat,nlon
  use transform_special, only: sptez_s_special
  implicit none

  real(r_kind),intent(in   ) :: spectral_in(nc)
  real(r_kind),intent(  out) :: grid_out(nlat,nlon)

  real(r_kind) work(nlon,nlat-2_i_kind),spec_work(nc)
  integer(i_kind) i,j,jj

  do i=1,nc
     spec_work(i)=spectral_in(i)
     if(factsml(i))spec_work(i)=zero
  end do
 
  call sptez_s_special(spec_work,work,ione)

!  Reverse ordering in j direction from n-->s to s-->n
!  And account for work array excluding pole points
  do j=2,nlat-ione
     jj=nlat-j
     do i=1,nlon
        grid_out(j,i)=work(i,jj)
     end do
  end do

!  fill in pole points using spectral coefficients
!  (replace earlier algorithm that assumed zero gradient next to pole)
  call spectra_pole_scalar_special (grid_out,spec_work)

  return
end subroutine s2g0_special

subroutine s2g0_ad_special(spectral_out,grid_in)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    s2g0_ad_special        copy of s2g0_ad
!   prgmmr: parrish          org: np23                date: 2009-12-17
!
! abstract: adjoint of s2g0
!
! program history log:
!   2006-07-15  kleist
!   2007-04-22  errico    correction for proper treatment of equator
!                         also add call to spectra_pole_scalar_ad 
!
!   input argument list:
!     grid_in  - input spherical harmonic coefficients
!
!   output argument list:
!     spectral_out - output grid field on gaussian grid
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use specmod_special, only: jcap,nc,factsml,wlat,jb,je
  use kinds, only: r_kind,i_kind
  use constants, only: izero,ione,zero,one,two
  use gridmod, only: nlat,nlon
  use transform_special, only: sptez_s_special
  implicit none

  real(r_kind),intent(  out) :: spectral_out(nc)
  real(r_kind),intent(in   ) :: grid_in(nlat,nlon)

  real(r_kind) work(nlon,nlat-2_i_kind),spec_work(nc)
  integer(i_kind) i,j,jj

!  Reverse ordering in j direction from n-->s to s-->n
!  And account for work array excluding pole points
  do j=2,nlat-ione
     jj=nlat-j
     do i=1,nlon
        work(i,jj)=grid_in(j,i)
     end do
  end do

  do j=jb,je-mod(nlat,2_i_kind)
     do i=1,nlon
        work(i,j)=work(i,j)/wlat(j)
        work(i,nlat-ione-j)=work(i,nlat-ione-j)/wlat(j)
     end do
  end do

  if (mod(nlat,2_i_kind)  /=  izero) then
     do i=1,nlon
        work(i,je)=work(i,je)/(two*wlat(je))
     end do
  endif

  call sptez_s_special(spec_work,work,-ione)

  do i=1,nc
     spec_work(i)=spec_work(i)*float(nlon)
  end do
  do i=2*jcap+3_i_kind,nc
     spec_work(i)=two*spec_work(i)
  end do

  call spectra_pole_scalar_ad_special (grid_in,spec_work)

  do i=1,nc
     spectral_out(i)=spec_work(i)
     if(factsml(i))spectral_out(i)=zero
  end do

  return
end subroutine s2g0_ad_special

subroutine spectra_pole_scalar_special (field,coefs)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    spectra_pole_scalar_special  copy of spectra_pole_scalar
!   prgmmr: parrish          org: np23                date: 2009-12-17
!
! abstract: fill pole values for scalar field using spectral coefficients
!
! program history log:
!   2007-05-15  errico
!   2009-11-24  parrish -- change values of starting assoc. legendre polynomial alp(0),alp(1)
!                           to be consistent with what is computed in sptranf0.
!                          alp(0) = sqrt(half), instead of alp(0)=one
!                    and   alp(1) = sqrt(three)*alp(0), instead of sqrt(three)
!
!   input argument list:
!     coefs  - spherical harmonic coefficients of scalar field
!
!   output argument list:
!     field - scalar field (modified at poles only)
!
! attributes:
!   language: f90
!
!$$$
  use specmod_special, only: nc,jcap
  use kinds, only: r_kind,i_kind
  use constants, only: ione,zero,half,three
  use gridmod, only: nlat,nlon
  
  implicit none      

  real(r_kind), intent(in   ) :: coefs(nc)        ! all spectral coefs
  real(r_kind), intent(inout) :: field(nlat,nlon) ! field, including pole    
! 
! Local variables

  integer(i_kind) :: n           ! order of assoc. legendre polynomial 
  integer(i_kind) :: n1          ! offset for real zonal wavenumber m=0 coefs
  integer(i_kind) :: j           ! longitude index      
  real(r_kind) :: alp0(0:jcap)   ! Assoc Legendre Poly for m=0 at the North Pole
  real(r_kind) :: epsi0(0:jcap)  ! epsilon factor for m=0
  real(r_kind) :: fnum, fden
  real(r_kind)  :: afac           ! alp for S. pole 
  real(r_kind) :: fpole_n, fpole_s    ! value of scalar field at n and s pole 
!
!  The spectral coefs are assumed to be ordered
!      alternating real, imaginary
!      all m=0 first, followed by m=1, etc.
!      ordered in ascending values of n-m
!      the first index is 1, correspond to the real part of the global mean.
!      triangular truncation assumed
!      These conditions determine the value of n1.
!
!  Compute epsilon for m=0.
  epsi0(0)=zero  
  do n=1,jcap
     fnum=real(n**2)
     fden=real(4*n**2-ione)
     epsi0(n)=dsqrt(fnum/fden)
  enddo
!
!  Compute Legendre polynomials for m=0 at North Pole
  alp0(0)=dsqrt(half)
  alp0(1)=dsqrt(three)*alp0(0)
  do n=2,jcap
     alp0(n)=(alp0(n-ione)-epsi0(n-ione)*alp0(n-2_i_kind))/epsi0(n)
  enddo
!
!  Compute projection of wavenumber 0 (only real values for this
  fpole_n=zero
  fpole_s=zero
  n1=ione
  do n=0,jcap 
     if (mod(n,2_i_kind) == ione) then
        afac=-alp0(n)
     else 
        afac= alp0(n)
     endif  
     fpole_n=fpole_n+alp0(n)*coefs(2*n+n1)
     fpole_s=fpole_s+   afac*coefs(2*n+n1)
  enddo
!
! set field for all "longitudes" at the pole to the same value
  do j=1,nlon
     field(   1,j)=fpole_s  
     field(nlat,j)=fpole_n
  enddo

end subroutine spectra_pole_scalar_special 
!
!  x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x
!
subroutine spectra_pole_scalar_ad_special (field,coefs)
 
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    spectra_pole_scalar_ad_special  copy of spectra_pole_scalar_ad
!   prgmmr: parrish          org: np23                date: 2009-12-17
!
! abstract: adjoint of spectra_pole_scalar
!
! program history log:
!   2007-05-15  errico
!   2009-11-24  parrish -- change values of starting assoc. legendre polynomial alp(0),alp(1)
!                           to be consistent with what is computed in sptranf0.
!                          alp(0) = sqrt(half), instead of alp(0)=one
!                    and   alp(1) = sqrt(three)*alp(0), instead of sqrt(three)
!
!   input argument list:
!     field -  adjoint (dual) of field (only poles used here)
!     coefs  - adjoint (dual) of spherical harmonic coefficients
!
!   output argument list:
!     coefs  - incremented adjoint (dual) of spherical harmonic coefficients
!
! attributes:
!   language: f90
!
!$$$
  use specmod_special, only: nc,jcap
  use kinds, only: r_kind,i_kind
  use constants, only: ione,zero,half,three
  use gridmod, only: nlat,nlon
  
  implicit none      

  real(r_kind), intent(inout) :: coefs(nc)  ! adjoint of all spectral coefs
  real(r_kind), intent(in   ) :: field(nlat,nlon) ! adjoint field, including pole    
! 
!  Local variables

  integer(i_kind) :: n           ! order of assoc. legendre polynomial 
  integer(i_kind) :: n1          ! offset for real zonal wavenumber m=0 coefs
  integer(i_kind) :: j           ! longitude index      
  real(r_kind) :: alp0(0:jcap)   ! Assoc Legendre Poly for m=0 at the North Pole
  real(r_kind) :: epsi0(0:jcap)  ! epsilon factor for m=0
  real(r_kind) :: fnum, fden
  real(r_kind)  :: afac           ! alp for S. pole
  real(r_kind) :: fpole_n, fpole_s    ! value of scalar field at n and s pole 


!
!  The spectral coefs are assumed to be ordered
!      alternating real, imaginary
!      all m=0 first, followed by m=1, etc.
!      ordered in ascending values of n-m
!      the first index is 1
!      triangular truncation assumed
!      These conditions determine the value of n1.
!
!  Compute epsilon for m=0.
  epsi0(0)=zero  
  do n=1,jcap
     fnum=real(n**2, r_kind)
     fden=real(4*n**2-ione, r_kind)
     epsi0(n)=dsqrt(fnum/fden)
  enddo
!
!  Compute Legendre polynomials for m=0 at North Pole
  alp0(0)=dsqrt(half)
  alp0(1)=dsqrt(three)*alp0(0)
  do n=2,jcap
     alp0(n)=(alp0(n-ione)-epsi0(n-ione)*alp0(n-2_i_kind))/epsi0(n)
  enddo
!
!  Compute projection of wavenumber 0 (only real values for this)
  fpole_n=zero
  fpole_s=zero
  do j=1,nlon
     fpole_n=fpole_n+field(nlat,j)
     fpole_s=fpole_s+field(   1,j)
  enddo
       
  n1=ione
  do n=0,jcap 
     if (mod(n,2_i_kind) == ione) then
        afac=-alp0(n)
     else 
        afac= alp0(n)
     endif
     coefs(2*n+n1)=coefs(2*n+n1)+afac*fpole_s+alp0(n)*fpole_n  
  enddo
!
end subroutine spectra_pole_scalar_ad_special 

end module spectral_transforms_special


module hybrid_ensemble_isotropic_global
!$$$   module documentation block
!                .      .    .                                       .
! module:    hybrid_ensemble_isotropic_global
!   prgmmr: parrish          org: np22                date: 2009-10-15
!
! abstract: contains routines for localization of the hybrid ensemble
!            control variable a_en.  this application is for
!            localization with a global model with homogeneous scale in horizontal
!            and vertical scale a function of vertical only.
!
! program history log:
!   2009-10-15  parrish, initial documentation.
!
! subroutines included:
!   sub xxxx                     - what xxxx does
!
! Variable Definitions:
!   def yyyy      - what yyyy is
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind
  implicit none

! set default to private
  private
! set subroutines to public
  public :: init_sf_xy
  public :: sf_xy

  real(r_kind),allocatable,dimension(:)::spectral_filter

contains

subroutine init_sf_xy(jcap_in)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_sf_xy
!   prgmmr: parrish          org: np23                date: 2009-12-17
!
! abstract: initialize horizontal spectral localization sf_xy
!
! program history log:
!   2009-12-17  parrish
!
!   input argument list:
!     jcap_in - maximum spectral truncation allowed
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$

  use gridmod, only: nlat,nlon
  use kinds, only: r_kind,i_kind,r_single
  use hybrid_ensemble_parameters,only : s_ens_h
  use specmod_special, only: jcap,nc,init_spec_vars_special,slat,destroy_spec_vars_special
  use spectral_transforms_special,only: g2s0_special,s2g0_special
  use constants, only: izero,ione,zero,half,one,two,three,rearth,pi
  use mpimod, only: mype
  implicit none

  integer(i_kind),intent(in   ) :: jcap_in

  integer(i_kind) i,ii,j,jmin,l,mm,n,nn,irn0,jcapmax
  real(r_kind),allocatable::g(:),gsave(:),errmax(:)
  real(r_kind) errmin,factor,x,y,f0new
  real(r_kind) rkm(nlat),f(nlat,nlon),f0(nlat,nlon)
  real(r_single) out1(nlon,nlat)
  real(r_single),allocatable::pn0_npole(:)
  integer(i_kind),dimension(0:40):: iblend
  integer(i_kind) is_ens_h,jcapest
  real(r_kind) s_ens_h_min
  

!    make sure s_ens_h is within allowable range  ( pi*rearth*.001/jcap_in <= s_ens_h <= 5500 )

  s_ens_h_min=pi*rearth*.001_r_kind/jcap_in
  if(s_ens_h <  s_ens_h_min) then
     if(mype == izero) write(6,*)' s_ens_h = ',s_ens_h,' km--too small, min value = ',s_ens_h_min,' km.'
     if(mype == izero) write(6,*)' s_ens_h reset to min value'
     s_ens_h=s_ens_h_min
  else if(s_ens_h >  5500._r_kind) then
     if(mype == izero) write(6,*)' s_ens_h = ',s_ens_h,' km--too large, max value = 5500 km.'
     if(mype == izero) write(6,*)' s_ens_h reset to max value'
     s_ens_h=5500._r_kind
  end if
    
    
  jcap=nint(1.2_r_kind*pi*rearth*.001_r_kind/s_ens_h)
  jcap=min(jcap,jcap_in)

!  set up spectral variables for jcap

  call init_spec_vars_special(nlat,nlon)

!    the following code is used to compute the desired spectrum to get a 
!     gaussian localization of desired length-scale.
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   how spectrum is obtained:
!
!     Correlation matrix:  C = Y*D*Ytrans
!
!       where Y is matrix of spherical harmonics, evaluated on gaussian grid, and D is a diagonal matrix
!
!     To obtain D, exploit fact that for D a function only of total wave-number n, then C is homogeneous
!       and isotropic on the sphere.
!
!     So look at the special case of a test point centered on the north pole.  The correlation function
!       is then only a function of latitude, call it c(phi), where c(pi/2) = 1.
!
!     Now we have C = P*D*Ptrans, where we have reduced the problem to 1 dimension, latitude, and in 
!       spectral space, total wave number n.  P is the zonal component only of Y.
!
!     Next, form the product
!                             C*e1 =P*D*Ptrans*e1,
!
!            where e1 is a vector of all 0, except for 1 at the north pole.
!
!     Then have P*D*Ptrans*e1 = sum(n) p(n,j)*d(n)*p(n,1) = c(j,1)
!
!        where j=1 corresponds to north pole point in this formulation.
!
!     Now if we have available C(j,1), a gaussian of desired length-scale evaluated (note, doesn't have to
!            be gaussian!) on the gaussian grid, then applying the inverse transform subroutine g2s0 to 
!     C yields the product
!
!             Chat(n) = d(n)*p(n,1)
!
!     So finally the desired spectrum is 
!     
!               d(n) = chat(n)/p(n,1)
!
!     To create the full spectral transform of the desired correlation, d(n) is copied to all non-zero 
!      zonal wave numbers on lines of constant total wave number n, multiplied by 1/2 to account for
!      two degrees of freedom for non-zero zonal wave numbers.
!
!     Note that while creating this routine, a bug was discovered in s2g0 routine for evaluation of pole 
!       value.  There was a missing factor of 1/sqrt(2) apparently.
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!                 create reference gaussian centered on north pole with desired length-scale.

!     compute latitudes in km from north pole
  rkm(nlat)=zero
  rkm(1)=two*asin(one)*rearth*.001_r_kind
  do i=1,(nlat-2_i_kind)/2
     rkm(nlat-i)=(asin(one)-asin(slat(i)))*rearth*.001_r_kind
     rkm(ione+i)=(asin(one)+asin(slat(i)))*rearth*.001_r_kind
  end do
  if(mype == izero) write(0,*)' lat,max(dlat)=',rkm(ione+(nlat-2_i_kind)/2),-rkm(nlat-(nlat-2_i_kind)/2)+rkm(ione+(nlat-2_i_kind)/2),' km'

  do i=1,nlat
     f0(i,1)=exp(-half*(rkm(i)/s_ens_h)**2)
  end do
  
  do j=2,nlon
     do i=1,nlat
        f0(i,j)=f0(i,1)
     end do
  end do

  allocate(g(nc),gsave(nc))
  call g2s0_special(g,f0)

  call s2g0_special(g,f)

!    adjust so value at np = 1
  f=f/f(nlat,1)
  call g2s0_special(g,f)
  call s2g0_special(g,f)
  if(mype == izero) write(0,*)' jcap,s_ens_h,max diff(f0-f)=',jcap,s_ens_h,maxval(abs(f0-f))

!            correct spectrum by dividing by pn0_npole
  gsave=g

!    obtain pn0_npole
  allocate(pn0_npole(0:jcap))
  do n=0,jcap
     g=zero
     g(2*n+ione)=one
     call s2g0_special(g,f)
     pn0_npole(n)=f(nlat,1)
  end do

  g=zero
  do n=0,jcap
     g(2*n+ione)=gsave(2*n+ione)/pn0_npole(n)
  end do

!    obtain spectral_filter

  allocate(spectral_filter(nc))
  ii=izero
  do l=0,jcap
     factor=one
     if(l >  izero) factor=half
     do n=l,jcap
        nn=nn+2_i_kind
        ii=ii+ione
        spectral_filter(ii)=factor*g(2*n+ione)
        ii=ii+ione
        if(l == izero) then
           spectral_filter(ii)=zero
        else
           spectral_filter(ii)=factor*g(2*n+ione)
        end if
     end do
  end do

  deallocate(g,gsave,pn0_npole)

  f=zero
  f(nlat/4,nlon/2)=one
  call sf_xy(f,1)
  do j=1,nlon
     do i=1,nlat
        out1(j,i)=f(i,j)
     end do
  end do
  if(mype == izero) call outgrads1(out1,nlon,nlat,'out1')

end subroutine init_sf_xy

subroutine sf_xy(f,nlevs)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    sf_xy       spectral isotropic localization for global domain
!
!   prgrmmr:  parrish        org: np22                              date: 2009-09-30
!
! abstract:  use spherical harmonic transform to do horizontal isotropic localization of hybrid
!             ensemble control variable a_en.
!
! program history log:
!   2009-10-16  parrish  initial documentation
!
!   input argument list:
!     f        - input field to be filtered
!     nlevs    -
!
!   output argument list:
!     f        - filtered output
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind
  use gridmod, only: nlat,nlon
  use spectral_transforms_special, only: s2g0_special,s2g0_ad_special
  use specmod_special, only: nc
  implicit none

  integer(i_kind),intent(in   ) :: nlevs
  real(r_kind)   ,intent(inout) :: f(nlat*nlon,nlevs)

  real(r_kind) g(nc)
  integer(i_kind) k

  do k=1,nlevs
     call s2g0_ad_special(g,f(:,k))
     g=g*spectral_filter
     call s2g0_special(g,f(:,k))
  end do

end subroutine sf_xy


end module hybrid_ensemble_isotropic_global
