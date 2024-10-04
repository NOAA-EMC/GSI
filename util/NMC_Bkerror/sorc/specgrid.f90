module specgrid
  use kinds, only: r_kind,r_double,i_kind
  implicit none

  integer jcap,jcapin,jcapsmooth,nc,ncin,ncd2,ncd2in
  integer iromb,idrt,imax,jmax,ijmax,jn,js,kw,jb,je,jc,ioffset
  integer imaxin,jmaxin,ijmaxin,jnin,jsin,kwin,jbin,jein,ioffsetin
  real(r_kind),allocatable,dimension(:):: factsml,factvml
  real(r_kind),allocatable,dimension(:):: eps,epstop,enn1,elonn1,eon,eontop
  real(r_kind),allocatable,dimension(:):: clat,slat,wlat
  real(r_kind),allocatable,dimension(:):: epsin,epstopin,enn1in,elonn1in,eonin,eontopin
  real(r_kind),allocatable,dimension(:):: clatin,slatin,wlatin
  real(r_kind),allocatable,dimension(:,:):: pln,plntop
  real(r_kind),allocatable,dimension(:,:):: plnin,plntopin
  real(r_double),allocatable,dimension(:):: afft_save,afft_savein

contains

  subroutine init_spec_vars(nlat,nlon,nsig)
    implicit none

    integer,intent(in):: nlat,nlon,nsig
    integer ii,ii1,l,m,ncpus
    real(r_kind) zero1

!   Set constants
    nc=(jcap+1)*(jcap+2)
    ncin=(jcapin+1)*(jcapin+2)
    ncd2=nc/2

!   Allocate more arrays related to transforms
    allocate(factsml(nc),factvml(nc))
!   Set up factsml and factvml
    ii=-1; ii1=0
    do l=0,jcap
       zero1=float(min(1,l))
       do m=0,jcap-l
          ii=ii+2; ii1=ii1+2
          factsml(ii)=1.; factsml(ii1)=zero1
          factvml(ii)=1.; factvml(ii1)=zero1
       end do
    end do

    factvml(1)=0.

!   Set other constants used in transforms
    idrt=4
    imax=nlon
    jmax=nlat-2
    ijmax=imax*jmax
    ioffset=imax*(jmax-1)
    jn=imax
    js=-jn
    kw=2*ncd2
    jb=1
    je=(jmax+1)/2
    jc=ncpus()

!   Allocate arrays
    allocate( eps(ncd2) )
    allocate( epstop(jcap+1) )
    allocate( enn1(ncd2) )
    allocate( elonn1(ncd2) )
    allocate( eon(ncd2) )
    allocate( eontop(jcap+1) )
    allocate( afft_save(50000+4*imax) )
    allocate( clat(jb:je) )
    allocate( slat(jb:je) )
    allocate( wlat(jb:je) )
    allocate( pln(ncd2,jb:je) )
    allocate( plntop(jcap+1,jb:je) )

    eps(:) = 0
    epstop(:)=0
    enn1(:) = 0
    elonn1(:) = 0
    eon(:) = 0
    eontop(:) = 0
    afft_save(:) = 0
    clat(:) = 0
    slat(:) = 0
    wlat(:) = 0
    pln(:,:) = 0
    plntop(:,:) = 0

!   Initialize arrays used in transforms
    call sptranf0(iromb,jcap,idrt,imax,jmax,jb,je, &
       eps,epstop,enn1,elonn1,eon,eontop, &
       afft_save,clat,slat,wlat,pln,plntop)

    return
  end subroutine init_spec_vars

  subroutine sptez_s(wave,grid,idir)
    use kinds, only: r_kind
    implicit none

! Declare passed variables
    integer,intent(in):: idir
    real(r_kind),dimension(nc),intent(inout):: wave
    real(r_kind),dimension(ijmax),intent(inout):: grid

! Declare local variables
    integer i

! Zero appropriate output array based on direction of transform
    if (idir<0) then
      do i=1,nc
        wave(i)=0.
      end do
    elseif (idir>0) then
      do i=1,ijmax
        grid(i)=0.
      end do
    endif

! Call spectral <--> grid transform
    call sptranf_s(wave,grid,grid,idir)

    return
  end subroutine sptez_s

  subroutine destroy_spec_vars
    deallocate(factsml,factvml)
    deallocate(eps,epstop,enn1,elonn1,eon,eontop,afft_save,&
       clat,slat,wlat,pln,plntop)
    return
  end subroutine destroy_spec_vars


  subroutine sptranf_s(wave,gridn,grids,idir)
    use kinds, only: r_kind
    implicit none

! Declare passed variables
    integer,intent(in):: idir
    real(r_kind),dimension(nc),intent(inout):: wave
    real(r_kind),dimension(ijmax),intent(inout):: gridn
    real(r_kind),dimension(ijmax),intent(inout):: grids

! Declare local variables
    integer i,j,jj,ij,ijn,ijs,mp
    real(r_kind),dimension(2*(jcap+1)):: wtop
    real(r_kind),dimension(imax,2):: g
    real(r_double),dimension(50000+4*imax):: afft

! this is needed for thread safety.
  afft = afft_save

! Initialize local variables
    mp=0
    
    do i=1,2*(jcap+1)
      wtop(i)=0.
    end do

! Transform wave to grid
    if(idir.gt.0) then
      do j=jb,je
        call sptranf1(iromb,jcap,idrt,imax,jmax,j,j, &
             eps,epstop,enn1,elonn1,eon,eontop, &
             afft,clat(j),slat(j),wlat(j), &
             pln(1,j),plntop(1,j),mp, &
             wave,wtop,g,idir)
        do i=1,imax
          jj  = j-jb
          ijn = i + jj*jn
          ijs = i + jj*js + ioffset
          gridn(ijn)=g(i,1)
          grids(ijs)=g(i,2)
        enddo
      enddo
! Transform grid to wave
    else
      do j=jb,je
        if(wlat(j).gt.0.) then
          do i=1,imax
            jj  = j-jb
            ijn = i + jj*jn
            ijs = i + jj*js + ioffset
            g(i,1)=gridn(ijn)
            g(i,2)=grids(ijs)
          enddo
          call sptranf1(iromb,jcap,idrt,imax,jmax,j,j, &
                eps,epstop,enn1,elonn1,eon,eontop, &
                afft,clat(j),slat(j),wlat(j), &
                pln(1,j),plntop(1,j),mp, &
                wave,wtop,g,idir)
        endif
      enddo
    endif
    return
  end subroutine sptranf_s

  subroutine fill_ns(grid_in,grid_out)
    use variables, only: nlat,nlon,zero,ltosi,ltosj,iglobal
    use kinds, only: i_kind,r_kind
    implicit none

    real(r_kind),dimension(nlon,nlat-2),intent(in):: grid_in  ! input grid
    real(r_kind),dimension(iglobal),intent(out):: grid_out  ! output grid

!  Declare local variables
    integer(i_kind) i,j,k,jj,nlatm2
    real(r_kind) rnlon,sumn,sums
    real(r_kind),dimension(nlat,nlon):: gridtmp

!  Transfer contents of input grid to local work array
!  Reverse ordering in j direction from n-->s to s-->n
    do j=2,nlat-1
      jj=nlat-j
      do i=1,nlon
        gridtmp(j,i)=grid_in(i,jj)
      end do
    end do

!  Compute mean along southern and northern latitudes
    sumn=zero
    sums=zero
    nlatm2=nlat-2
    do i=1,nlon
      sumn=sumn+grid_in(i,1)
      sums=sums+grid_in(i,nlatm2)
    end do
    rnlon=1./float(nlon)
    sumn=sumn*rnlon
    sums=sums*rnlon

!  Load means into local work array
    do i=1,nlon
      gridtmp(1,i)   =sums
      gridtmp(nlat,i)=sumn
    end do

!  Transfer local work array to output grid
   do k=1,iglobal
      i=ltosi(k)
      j=ltosj(k)
      grid_out(k)=gridtmp(i,j)
   end do


    return
  end subroutine fill_ns

  subroutine load_grid(grid_in,grid_out)
    use variables, only: nlat,nlon
    use kinds, only: r_kind,i_kind
    implicit none

    real(r_kind),dimension(nlat,nlon),intent(in):: grid_in        ! input grid
    real(r_kind),dimension(nlon,nlat-2),intent(out):: grid_out    ! output grid

    integer(i_kind) i,j,k,nlatm1,jj,j2

!  Transfer contents of local array to output array.
    nlatm1=nlat-1
    do j=2,nlatm1
      jj=nlat-j+1
      j2=j-1
      do i=1,nlon
        grid_out(i,j2)=grid_in(jj,i)
      end do
    end do

    return
 end subroutine load_grid

  subroutine unload_grid(grid_in,grid_out)
    use variables, only: nlat,nlon
    use kinds, only: r_kind,i_kind
    implicit none

    real(r_kind),dimension(nlon,nlat-2),intent(in):: grid_in        ! input grid
    real(r_kind),dimension(nlat,nlon),intent(out):: grid_out    ! output grid

    integer(i_kind) i,j,k,nlatm1,jj,j2

!  Transfer contents of local array to output array.
    do j=2,nlat-1
      jj=nlat-j
      do i=1,nlon
        grid_out(j,i)=grid_in(i,jj)
      end do
    end do

! for now attempt something stupid at pole
    do j=1,nlon
      grid_out(1,j)=grid_out(2,j)
      grid_out(nlat,j)=grid_out(nlat-1,j)
    end do

    return
 end subroutine unload_grid


 subroutine jcaptrans(z,fact,z4)
   use kinds, only: r_kind,r_single
   implicit none
   integer j,iiin,iiout,l,m
   real(r_kind),dimension(nc):: z,fact
   real(r_kind),dimension(ncin):: z4
   !real(r_single),dimension(ncin):: z4
   do j=1,nc
     z(j)=0.0
   end do
   iiin=1
   iiout=1
   do l=0,min(jcap,jcapin)
      do m=0,min(jcap,jcapin)-l
        if(m < jcapsmooth .and. l < jcapsmooth)then
         z(iiout+2*m)  =fact(iiout+2*m)*z4(iiin+2*m)
         z(iiout+2*m+1)=fact(iiout+2*m+1)*z4(iiin+2*m+1)
        else
         z(iiout+2*m)  =0.0
         z(iiout+2*m+1)=0.0
        end if
      end do
      iiin=iiin+2*(jcapin-l+1)
      iiout=iiout+2*(jcap-l+1)
   end do
   return
 end subroutine jcaptrans

subroutine sptezv_s(waved,wavez,gridu,gridv,idir)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  sptez_v       perform a simple vector spherical transform
!   prgmmr: iredell          org: np23                date: 1996-02-29
!
! abstract: this subprogram performs a spherical transform
!           between spectral coefficients of divergence and curl
!           and a vector field on a global cylindrical grid.
!           the wave-space is triangular.
!           the grid-space can be either an equally-spaced grid
!           (with or without pole points) or a gaussian grid.
!           the wave field is in sequential 'ibm order'.
!           the grid fiels is indexed east to west, then north to south.
!           for more flexibility and efficiency, call sptran.
!           subprogram can be called from a multiprocessing environment.
!
!           This routine differs from splib routine sptezv in that
!              1) the calling list only contains the in/out arrays and
!                 flag for the direction in which to transform
!              2) it calls a version of sptranfv that does not invoke
!                 initialization routines on each entry
!              3) some generality built into the splib version is
!                 removed in the code below
!
! program history log:
!   1996-02-29  iredell
!   2004-08-23  treadon - adapt splib routine sptezv for gsi use
!   2008-02-01  whitaker - modifications for use in ensemble kalman filter.
!
!   input arguments:
!     waved    - real (2*mx) wave divergence field if idir>0
!                where mx=(maxwv+1)*(maxwv+2)/2
!     wavez    - real (2*mx) wave vorticity field if idir>0
!                where mx=(maxwv+1)*(maxwv+2)/2
!     gridu    - real (imax,jmax) grid u-wind (e->w,n->s) if idir<0
!     gridv    - real (imax,jmax) grid v-wind (e->w,n->s) if idir<0
!     idir     - integer transform flag
!                (idir>0 for wave to grid, idir<0 for grid to wave)
!
!   output arguments:
!     waved    - real (2*mx) wave divergence field if idir<0
!                where mx=(maxwv+1)*(maxwv+2)/2
!     wavez    - real (2*mx) wave vorticity field if idir>0
!                where mx=(maxwv+1)*(maxwv+2)/2
!     gridu    - real (imax,jmax) grid u-wind (e->w,n->s) if idir>0
!     gridv    - real (imax,jmax) grid v-wind (e->w,n->s) if idir>0
!
! subprograms called:
!   sptranf_v  - perform a vector spherical transform
!
! remarks: minimum grid dimensions for unaliased transforms to spectral:
!   dimension                    linear              quadratic
!   -----------------------      ---------           -------------
!   imax                         2*maxwv+2           3*maxwv/2*2+2
!   jmax (idrt=4)                1*maxwv+1           3*maxwv/2+1
!   jmax (idrt=0)                2*maxwv+3           3*maxwv/2*2+3
!   jmax (idrt=256)              2*maxwv+1           3*maxwv/2*2+1
!   -----------------------      ---------           -------------
!
! attributes:
!   language: fortran 77
!
!$$$
  implicit none

! Declare passed variables
  integer(i_kind),intent(in):: idir
  real(r_kind),dimension(nc),intent(inout):: waved,wavez
  real(r_kind),dimension(ijmax),intent(inout):: gridu,gridv

! Declare local variables
  integer(i_kind) i

! Zero appropriate output array based on direction of transform
  if (idir < 0) then
     do i=1,nc
        waved(i)=0._r_kind
        wavez(i)=0._r_kind
     end do
  elseif (idir > 0) then
     do i=1,ijmax
        gridu(i)=0._r_kind
        gridv(i)=0._r_kind
     end do
  endif

! Call spectral <--> grid transform
  call sptranf_v(waved,wavez,gridu,gridu,gridv,gridv,idir)

end subroutine sptezv_s

subroutine sptranf_v(waved,wavez,gridun,gridus,gridvn,gridvs,idir)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  sptranf_v     perform a vecor spherical transform
!   prgmmr: iredell          org: np23                date: 1996-02-29
!
! abstract: this subprogram performs a spherical transform
!           between spectral coefficients of divergences and curls
!           and vector fields on a global cylindrical grid.
!           the wave-space is triangular.
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
!           This routine differs from splib routine sptranfv in that
!           it does not call sptranf0 (an initialization routine).
!
!
! program history log:
!   1996-02-29  iredell
!   1998-12-15  iredell  generic fft used
!   2004-08-23  treadon - adapt splib routine sptranfv for gsi use
!   2006-05-03  treadon - remove jc from specmod list since not used
!   2006-07-07  kleist - correct bug in indexing of j=1,2*ncd2 loop
!   2008-02-01  whitaker - modifications for use in ensemble kalman filter.
!
!   input arguments:
!     waved    - real (*) wave divergence fields if idir>0
!     wavez    - real (*) wave vorticity fields if idir>0
!     gridun   - real (*) n.h. grid u-winds (starting at jb) if idir<0
!     gridus   - real (*) s.h. grid u-winds (starting at jb) if idir<0
!     gridvn   - real (*) n.h. grid v-winds (starting at jb) if idir<0
!     gridvs   - real (*) s.h. grid v-winds (starting at jb) if idir<0
!     idir     - integer transform flag
!                (idir>0 for wave to grid, idir<0 for grid to wave)
!
!   output arguments:
!     waved    - real (*) wave divergence fields if idir<0
!                [waved=(d(gridu)/dlam+d(clat*gridv)/dphi)/(clat*rerth)]
!     wavez    - real (*) wave vorticity fields if idir<0
!                [wavez=(d(gridv)/dlam-d(clat*gridu)/dphi)/(clat*rerth)]
!     gridun   - real (*) n.h. grid u-winds (starting at jb) if idir>0
!     gridus   - real (*) s.h. grid u-winds (starting at jb) if idir>0
!     gridvn   - real (*) n.h. grid v-winds (starting at jb) if idir>0
!     gridvs   - real (*) s.h. grid v-winds (starting at jb) if idir>0
!
! subprograms called:
!   sptranf1     sptranf spectral transform
!   spdz2uv      compute winds from divergence and vorticity
!   spuv2dz      compute divergence and vorticity from winds
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
!   jmax (idrt=4)                1*maxwv+1           3*maxwv/2+1
!   jmax (idrt=0)                2*maxwv+3           3*maxwv/2*2+3
!   jmax (idrt=256)              2*maxwv+1           3*maxwv/2*2+1
!   -----------------------      ---------           -------------
!
! attributes:
!   language: fortran 77
!
!$$$
  implicit none

! Declare passed variables
  integer(i_kind),intent(in):: idir
  real(r_kind),dimension(nc):: waved,wavez
  real(r_kind),dimension(ijmax):: gridun,gridus,gridvn,gridvs

! Declare local variables
  integer(i_kind) i,j,jj,ijn,ijs
  integer(i_kind),dimension(2):: mp
  real(r_kind),dimension(ncd2*2,2):: w
  real(r_kind),dimension(2*(jcap+1),2):: wtop
  real(r_kind),dimension(imax,2,2):: g
  real(8),dimension(50000+4*imax):: afft

! Set parameters
  mp=1
! this is needed for thread safety.
  afft = afft_save

! Transform wave to grid
  if(idir > 0) then
     call spdz2uv(iromb,jcap,enn1,elonn1,eon,eontop, &
          waved,wavez, &
          w(1,1),w(1,2),wtop(1,1),wtop(1,2))
     do j=jb,je
        call sptranf1(iromb,jcap,idrt,imax,jmax,j,j, &
             eps,epstop,enn1,elonn1,eon,eontop, &
             afft,clat(j),slat(j),wlat(j), &
             pln(1,j),plntop(1,j),mp, &
             w(1,1),wtop(1,1),g(1,1,1),idir)
        call sptranf1(iromb,jcap,idrt,imax,jmax,j,j, &
             eps,epstop,enn1,elonn1,eon,eontop, &
             afft,clat(j),slat(j),wlat(j), &
             pln(1,j),plntop(1,j),mp, &
             w(1,2),wtop(1,2),g(1,1,2),idir)
        do i=1,imax
           jj   = j-jb
           ijn = i + jj*jn
           ijs = i + jj*js + ioffset
           gridun(ijn)=g(i,1,1)
           gridus(ijs)=g(i,2,1)
           gridvn(ijn)=g(i,1,2)
           gridvs(ijs)=g(i,2,2)

        enddo
     enddo

!  Transform grid to wave
  else
     w=0
     wtop=0
     do j=jb,je
        if(wlat(j) > 0._r_kind) then
           do i=1,imax
              jj   = j-jb
              ijn = i + jj*jn
              ijs = i + jj*js + ioffset

              g(i,1,1)=gridun(ijn)/clat(j)**2
              g(i,2,1)=gridus(ijs)/clat(j)**2
              g(i,1,2)=gridvn(ijn)/clat(j)**2
              g(i,2,2)=gridvs(ijs)/clat(j)**2
           enddo
           call sptranf1(iromb,jcap,idrt,imax,jmax,j,j, &
                eps,epstop,enn1,elonn1,eon,eontop, &
                afft,clat(j),slat(j),wlat(j), &
                pln(1,j),plntop(1,j),mp, &
                w(1,1),wtop(1,1),g(1,1,1),idir)
           call sptranf1(iromb,jcap,idrt,imax,jmax,j,j, &
                eps,epstop,enn1,elonn1,eon,eontop, &
                afft,clat(j),slat(j),wlat(j), &
                pln(1,j),plntop(1,j),mp, &
                w(1,2),wtop(1,2),g(1,1,2),idir)
        endif
     enddo
     call spuv2dz(iromb,jcap,enn1,elonn1,eon,eontop, &
          w(1,1),w(1,2),wtop(1,1),wtop(1,2), &
          waved(1),wavez(1))
  endif

 end subroutine sptranf_v
  subroutine init_spec_varsin(nlat,nlon,nsig)
    implicit none

    integer,intent(in):: nlat,nlon,nsig
    integer ii,ii1,l,m,ncpus
    real(r_kind) zero1

!   Set constants
!    nc=(jcap+1)*(jcap+2)
!    ncin=(jcapin+1)*(jcapin+2)
!    ncd2=nc/2
     ncd2in=ncin/2

!   Allocate more arrays related to transforms
!    allocate(factsml(nc),factvml(nc))
!   Set up factsml and factvml
!    ii=-1; ii1=0
!    do l=0,jcap
!       zero1=float(min(1,l))
!       do m=0,jcap-l
!          ii=ii+2; ii1=ii1+2
!          factsml(ii)=1.; factsml(ii1)=zero1
!          factvml(ii)=1.; factvml(ii1)=zero1
!       end do
!    end do

!    factvml(1)=0.

!   Set other constants used in transforms
!    idrt=4
    imaxin=nlon
    jmaxin=nlat-2
    ijmaxin=imaxin*jmaxin
    ioffsetin=imaxin*(jmaxin-1)
    jnin=imaxin
    jsin=-jnin
    kwin=2*ncd2in
    jbin=1
    jein=(jmaxin+1)/2
!    jc=ncpus()

!   Allocate arrays
    allocate( epsin(ncd2in) )
    allocate( epstopin(jcapin+1) )
    allocate( enn1in(ncd2in) )
    allocate( elonn1in(ncd2in) )
    allocate( eonin(ncd2in) )
    allocate( eontopin(jcapin+1) )
    allocate( afft_savein(50000+4*imaxin) )
    allocate( clatin(jbin:jein) )
    allocate( slatin(jbin:jein) )
    allocate( wlatin(jbin:jein) )
    allocate( plnin(ncd2in,jbin:jein) )
    allocate( plntopin(jcapin+1,jbin:jein) )

    epsin(:) = 0
    epstopin(:)=0
    enn1in(:) = 0
    elonn1in(:) = 0
    eonin(:) = 0
    eontopin(:) = 0
    afft_savein(:) = 0
    clatin(:) = 0
    slatin(:) = 0
    wlatin(:) = 0
    plnin(:,:) = 0
    plntopin(:,:) = 0

!   Initialize arrays used in transforms
    call sptranf0(iromb,jcapin,idrt,imaxin,jmaxin,jbin,jein, &
       epsin,epstopin,enn1in,elonn1in,eonin,eontopin, &
       afft_savein,clatin,slatin,wlatin,plnin,plntopin)

    return
  end subroutine init_spec_varsin

  subroutine sptez_sin(wave,grid,idir)
    use kinds, only: r_kind
    implicit none

! Declare passed variables
    integer,intent(in):: idir
    real(r_kind),dimension(ncin),intent(inout):: wave
    real(r_kind),dimension(ijmaxin),intent(inout):: grid

! Declare local variables
    integer i

! Zero appropriate output array based on direction of transform
    if (idir<0) then
      do i=1,ncin
        wave(i)=0.
      end do
    elseif (idir>0) then
      do i=1,ijmaxin
        grid(i)=0.
      end do
    endif

! Call spectral <--> grid transform
    call sptranf_sin(wave,grid,grid,idir)

    return
  end subroutine sptez_sin

  subroutine destroy_spec_varsin
!    deallocate(factsml,factvml)
    deallocate(epsin,epstopin,enn1in,elonn1in,eonin,eontopin,&
       afft_savein,clatin,slatin,wlatin,plnin,plntopin)
    return
  end subroutine destroy_spec_varsin


  subroutine sptranf_sin(wave,gridn,grids,idir)
    use kinds, only: r_kind
    implicit none

! Declare passed variables
    integer,intent(in):: idir
    real(r_kind),dimension(ncin),intent(inout):: wave
    real(r_kind),dimension(ijmaxin),intent(inout):: gridn
    real(r_kind),dimension(ijmaxin),intent(inout):: grids

! Declare local variables
    integer i,j,jj,ij,ijn,ijs,mp
    real(r_kind),dimension(2*(jcapin+1)):: wtop
    real(r_kind),dimension(imaxin,2):: g
    real(r_double),dimension(50000+4*imaxin):: afft

! this is needed for thread safety.
  afft = afft_savein

! Initialize local variables
    mp=0
    
    do i=1,2*(jcapin+1)
      wtop(i)=0.
    end do

! Transform wave to grid
    if(idir.gt.0) then
      do j=jbin,jein
        call sptranf1(iromb,jcapin,idrt,imaxin,jmaxin,j,j, &
             epsin,epstopin,enn1in,elonn1in,eonin,eontopin, &
             afft,clatin(j),slatin(j),wlatin(j), &
             plnin(1,j),plntopin(1,j),mp, &
             wave,wtop,g,idir)
        do i=1,imaxin
          jj  = j-jbin
          ijn = i + jj*jnin
          ijs = i + jj*jsin + ioffsetin
          gridn(ijn)=g(i,1)
          grids(ijs)=g(i,2)
        enddo
      enddo
! Transform grid to wave
    else
      do j=jbin,jein
        if(wlatin(j).gt.0.) then
          do i=1,imaxin
            jj  = j-jbin
            ijn = i + jj*jnin
            ijs = i + jj*jsin + ioffsetin
            g(i,1)=gridn(ijn)
            g(i,2)=grids(ijs)
          enddo
          call sptranf1(iromb,jcapin,idrt,imaxin,jmaxin,j,j, &
                epsin,epstopin,enn1in,elonn1in,eonin,eontopin, &
                afft,clatin(j),slatin(j),wlatin(j), &
                plnin(1,j),plntopin(1,j),mp, &
                wave,wtop,g,idir)
        endif
      enddo
    endif
    return
  end subroutine sptranf_sin

subroutine sptezv_sin(waved,wavez,gridu,gridv,idir)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  sptez_v       perform a simple vector spherical transform
!   prgmmr: iredell          org: np23                date: 1996-02-29
!
! abstract: this subprogram performs a spherical transform
!           between spectral coefficients of divergence and curl
!           and a vector field on a global cylindrical grid.
!           the wave-space is triangular.
!           the grid-space can be either an equally-spaced grid
!           (with or without pole points) or a gaussian grid.
!           the wave field is in sequential 'ibm order'.
!           the grid fiels is indexed east to west, then north to south.
!           for more flexibility and efficiency, call sptran.
!           subprogram can be called from a multiprocessing environment.
!
!           This routine differs from splib routine sptezv in that
!              1) the calling list only contains the in/out arrays and
!                 flag for the direction in which to transform
!              2) it calls a version of sptranfv that does not invoke
!                 initialization routines on each entry
!              3) some generality built into the splib version is
!                 removed in the code below
!
! program history log:
!   1996-02-29  iredell
!   2004-08-23  treadon - adapt splib routine sptezv for gsi use
!   2008-02-01  whitaker - modifications for use in ensemble kalman filter.
!
!   input arguments:
!     waved    - real (2*mx) wave divergence field if idir>0
!                where mx=(maxwv+1)*(maxwv+2)/2
!     wavez    - real (2*mx) wave vorticity field if idir>0
!                where mx=(maxwv+1)*(maxwv+2)/2
!     gridu    - real (imax,jmax) grid u-wind (e->w,n->s) if idir<0
!     gridv    - real (imax,jmax) grid v-wind (e->w,n->s) if idir<0
!     idir     - integer transform flag
!                (idir>0 for wave to grid, idir<0 for grid to wave)
!
!   output arguments:
!     waved    - real (2*mx) wave divergence field if idir<0
!                where mx=(maxwv+1)*(maxwv+2)/2
!     wavez    - real (2*mx) wave vorticity field if idir>0
!                where mx=(maxwv+1)*(maxwv+2)/2
!     gridu    - real (imax,jmax) grid u-wind (e->w,n->s) if idir>0
!     gridv    - real (imax,jmax) grid v-wind (e->w,n->s) if idir>0
!
! subprograms called:
!   sptranf_v  - perform a vector spherical transform
!
! remarks: minimum grid dimensions for unaliased transforms to spectral:
!   dimension                    linear              quadratic
!   -----------------------      ---------           -------------
!   imax                         2*maxwv+2           3*maxwv/2*2+2
!   jmax (idrt=4)                1*maxwv+1           3*maxwv/2+1
!   jmax (idrt=0)                2*maxwv+3           3*maxwv/2*2+3
!   jmax (idrt=256)              2*maxwv+1           3*maxwv/2*2+1
!   -----------------------      ---------           -------------
!
! attributes:
!   language: fortran 77
!
!$$$
  implicit none

! Declare passed variables
  integer(i_kind),intent(in):: idir
  real(r_kind),dimension(ncin),intent(inout):: waved,wavez
  real(r_kind),dimension(ijmaxin),intent(inout):: gridu,gridv

! Declare local variables
  integer(i_kind) i

! Zero appropriate output array based on direction of transform
  if (idir < 0) then
     do i=1,ncin
        waved(i)=0._r_kind
        wavez(i)=0._r_kind
     end do
  elseif (idir > 0) then
     do i=1,ijmaxin
        gridu(i)=0._r_kind
        gridv(i)=0._r_kind
     end do
  endif

! Call spectral <--> grid transform
  call sptranf_vin(waved,wavez,gridu,gridu,gridv,gridv,idir)

end subroutine sptezv_sin

subroutine sptranf_vin(waved,wavez,gridun,gridus,gridvn,gridvs,idir)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  sptranf_v     perform a vecor spherical transform
!   prgmmr: iredell          org: np23                date: 1996-02-29
!
! abstract: this subprogram performs a spherical transform
!           between spectral coefficients of divergences and curls
!           and vector fields on a global cylindrical grid.
!           the wave-space is triangular.
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
!           This routine differs from splib routine sptranfv in that
!           it does not call sptranf0 (an initialization routine).
!
!
! program history log:
!   1996-02-29  iredell
!   1998-12-15  iredell  generic fft used
!   2004-08-23  treadon - adapt splib routine sptranfv for gsi use
!   2006-05-03  treadon - remove jc from specmod list since not used
!   2006-07-07  kleist - correct bug in indexing of j=1,2*ncd2 loop
!   2008-02-01  whitaker - modifications for use in ensemble kalman filter.
!
!   input arguments:
!     waved    - real (*) wave divergence fields if idir>0
!     wavez    - real (*) wave vorticity fields if idir>0
!     gridun   - real (*) n.h. grid u-winds (starting at jb) if idir<0
!     gridus   - real (*) s.h. grid u-winds (starting at jb) if idir<0
!     gridvn   - real (*) n.h. grid v-winds (starting at jb) if idir<0
!     gridvs   - real (*) s.h. grid v-winds (starting at jb) if idir<0
!     idir     - integer transform flag
!                (idir>0 for wave to grid, idir<0 for grid to wave)
!
!   output arguments:
!     waved    - real (*) wave divergence fields if idir<0
!                [waved=(d(gridu)/dlam+d(clat*gridv)/dphi)/(clat*rerth)]
!     wavez    - real (*) wave vorticity fields if idir<0
!                [wavez=(d(gridv)/dlam-d(clat*gridu)/dphi)/(clat*rerth)]
!     gridun   - real (*) n.h. grid u-winds (starting at jb) if idir>0
!     gridus   - real (*) s.h. grid u-winds (starting at jb) if idir>0
!     gridvn   - real (*) n.h. grid v-winds (starting at jb) if idir>0
!     gridvs   - real (*) s.h. grid v-winds (starting at jb) if idir>0
!
! subprograms called:
!   sptranf1     sptranf spectral transform
!   spdz2uv      compute winds from divergence and vorticity
!   spuv2dz      compute divergence and vorticity from winds
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
!   jmax (idrt=4)                1*maxwv+1           3*maxwv/2+1
!   jmax (idrt=0)                2*maxwv+3           3*maxwv/2*2+3
!   jmax (idrt=256)              2*maxwv+1           3*maxwv/2*2+1
!   -----------------------      ---------           -------------
!
! attributes:
!   language: fortran 77
!
!$$$
  implicit none

! Declare passed variables
  integer(i_kind),intent(in):: idir
  real(r_kind),dimension(ncin):: waved,wavez
  real(r_kind),dimension(ijmaxin):: gridun,gridus,gridvn,gridvs

! Declare local variables
  integer(i_kind) i,j,jj,ijn,ijs
  integer(i_kind),dimension(2):: mp
  real(r_kind),dimension(ncd2in*2,2):: w
  real(r_kind),dimension(2*(jcapin+1),2):: wtop
  real(r_kind),dimension(imaxin,2,2):: g
  real(8),dimension(50000+4*imaxin):: afft

! Set parameters
  mp=1
! this is needed for thread safety.
  afft = afft_savein

! Transform wave to grid
  if(idir > 0) then
     call spdz2uv(iromb,jcapin,enn1in,elonn1in,eonin,eontopin, &
          waved,wavez, &
          w(1,1),w(1,2),wtop(1,1),wtop(1,2))
     do j=jbin,jein
        call sptranf1(iromb,jcapin,idrt,imaxin,jmaxin,j,j, &
             epsin,epstopin,enn1in,elonn1in,eonin,eontopin, &
             afft,clatin(j),slatin(j),wlatin(j), &
             plnin(1,j),plntopin(1,j),mp, &
             w(1,1),wtop(1,1),g(1,1,1),idir)
        call sptranf1(iromb,jcapin,idrt,imaxin,jmaxin,j,j, &
             epsin,epstopin,enn1in,elonn1in,eonin,eontopin, &
             afft,clatin(j),slatin(j),wlatin(j), &
             plnin(1,j),plntopin(1,j),mp, &
             w(1,2),wtop(1,2),g(1,1,2),idir)
        do i=1,imaxin
           jj   = j-jbin
           ijn = i + jj*jnin
           ijs = i + jj*jsin + ioffsetin
           gridun(ijn)=g(i,1,1)
           gridus(ijs)=g(i,2,1)
           gridvn(ijn)=g(i,1,2)
           gridvs(ijs)=g(i,2,2)

        enddo
     enddo

!  Transform grid to wave
  else
     w=0
     wtop=0
     do j=jbin,jein
        if(wlatin(j) > 0._r_kind) then
           do i=1,imaxin
              jj   = j-jbin
              ijn = i + jj*jnin
              ijs = i + jj*jsin + ioffsetin

              g(i,1,1)=gridun(ijn)/clatin(j)**2
              g(i,2,1)=gridus(ijs)/clatin(j)**2
              g(i,1,2)=gridvn(ijn)/clatin(j)**2
              g(i,2,2)=gridvs(ijs)/clatin(j)**2
           enddo
           call sptranf1(iromb,jcapin,idrt,imaxin,jmaxin,j,j, &
                epsin,epstopin,enn1in,elonn1in,eonin,eontopin, &
                afft,clatin(j),slatin(j),wlatin(j), &
                plnin(1,j),plntopin(1,j),mp, &
                w(1,1),wtop(1,1),g(1,1,1),idir)
           call sptranf1(iromb,jcapin,idrt,imaxin,jmaxin,j,j, &
                epsin,epstopin,enn1in,elonn1in,eonin,eontopin, &
                afft,clatin(j),slatin(j),wlatin(j), &
                plnin(1,j),plntopin(1,j),mp, &
                w(1,2),wtop(1,2),g(1,1,2),idir)
        endif
     enddo
     call spuv2dz(iromb,jcapin,enn1in,elonn1in,eonin,eontopin, &
          w(1,1),w(1,2),wtop(1,1),wtop(1,2), &
          waved(1),wavez(1))
  endif

 end subroutine sptranf_vin
 
end module specgrid

