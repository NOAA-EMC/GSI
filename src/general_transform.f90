subroutine general_sptez_s(sp,wave,grid,idir)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  sptez_s       perform a simple scalar spherical transform
!   prgmmr: iredell          org: np23                date: 1996-02-29
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
!   2010-02-18  parrish - copy to general_sptez_s, and pass specmod vars through
!                          input variable sp of type(spec_vars)
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
  use constants, only: izero,zero
  use general_specmod, only: spec_vars
  implicit none

! Declare passed variables
  type(spec_vars)              ,intent(in   ) :: sp
  integer(i_kind)              ,intent(in   ) :: idir
  real(r_kind),dimension(sp%nc)   ,intent(inout) :: wave
  real(r_kind),dimension(sp%ijmax),intent(inout) :: grid

! Declare local variables
  integer(i_kind) i

! Zero appropriate output array based on direction of transform
  if (idir<izero) then
     do i=1,sp%nc
        wave(i)=zero
     end do
  elseif (idir>izero) then
     do i=1,sp%ijmax
        grid(i)=zero
     end do
  endif

! Call spectral <--> grid transform
  call general_sptranf_s(sp,wave,grid,idir)

  return
end subroutine general_sptez_s

subroutine general_sptranf_s(sp,wave,grid,idir)
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
!   2010-02-18  parrish - copy to general_sptranf_s, and pass specmod vars through
!                          input variable sp of type(spec_vars)
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
  use general_specmod, only: spec_vars
  implicit none

! Declare passed variables
  type(spec_vars)              ,intent(in   ) :: sp
  integer(i_kind)              ,intent(in   ) :: idir
  real(r_kind),dimension(sp%nc)   ,intent(inout) :: wave
  real(r_kind),dimension(sp%ijmax),intent(inout) :: grid

! Declare local variables
  integer(i_kind) i,j,jj,ijn,ijs,mp
  real(r_kind),dimension(2*(sp%jcap+ione)):: wtop
  real(r_kind),dimension(sp%imax,2_i_kind):: g
  integer(i_kind) :: ldafft
  ldafft=256_i_kind+sp%imax

! Initialize local variables
  mp=izero

  do i=1,2*(sp%jcap+ione)
     wtop(i)=zero
  end do

! Transform wave to grid
  if(idir>izero) then
!$omp parallel do private(j,i,jj,ijn,ijs,g)
     do j=sp%jb,sp%je
        call sptranf1(sp%iromb,sp%jcap,sp%idrt,sp%imax,sp%jmax,j,j, &
             sp%eps,sp%epstop,sp%enn1,sp%elonn1,sp%eon,sp%eontop, &
             sp%afft(1:ldafft),sp%clat(j),sp%slat(j),sp%wlat(j), &
             sp%pln(1,j),sp%plntop(1,j),mp, &
             wave,wtop,g,idir)
        do i=1,sp%imax
           jj  = j-sp%jb
           ijn = i + jj*sp%jn
           ijs = i + jj*sp%js + sp%ioffset
           grid(ijn)=g(i,1)
           grid(ijs)=g(i,2)
        enddo
     enddo
!$omp end parallel do

! Transform grid to wave
  else
!!threading bug in this block
!!$omp parallel do private(j,i,jj,ijn,ijs,g)
     do j=sp%jb,sp%je
        if(sp%wlat(j)>zero) then
           do i=1,sp%imax
              jj  = j-sp%jb
              ijn = i + jj*sp%jn
              ijs = i + jj*sp%js + sp%ioffset
              g(i,1)=grid(ijn)
              g(i,2)=grid(ijs)
              
           enddo
           call sptranf1(sp%iromb,sp%jcap,sp%idrt,sp%imax,sp%jmax,j,j, &
                sp%eps,sp%epstop,sp%enn1,sp%elonn1,sp%eon,sp%eontop, &
                sp%afft(1:ldafft),sp%clat(j),sp%slat(j),sp%wlat(j), &
                sp%pln(1,j),sp%plntop(1,j),mp, &
                wave,wtop,g,idir)
        endif
     enddo
!!$omp end parallel do
  endif
end subroutine general_sptranf_s


subroutine general_sptez_v(sp,waved,wavez,gridu,gridv,idir)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  sptez_v       perform a simple vector spherical transform
!   prgmmr: iredell          org: np23                date: 1996-02-29
!
! abstract: this subprogram performs a spherical transform
!           between spectral coefficients of divergence and curl
!           and a vector field on a global cylindrical grid.
!           the wave-space can be either triangular or rhomboidal.
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
!   2007-04-25  errico  - replace use of duplicate arguments in sptranf_v
!   2008-04-03  safford - rm unused vars 
!   2010-02-18  parrish - copy to general_sptez_v, and pass specmod vars through
!                          input variable sp of type(spec_vars)
!
!   input arguments:
!     waved    - real (2*mx) wave divergence field if idir>0
!                where mx=(maxwv+1)*((iromb+1)*maxwv+2)/2
!     wavez    - real (2*mx) wave vorticity field if idir>0
!                where mx=(maxwv+1)*((iromb+1)*maxwv+2)/2
!     gridu    - real (imax,jmax) grid u-wind (e->w,n->s) if idir<0
!     gridv    - real (imax,jmax) grid v-wind (e->w,n->s) if idir<0
!     idir     - integer transform flag
!                (idir>0 for wave to grid, idir<0 for grid to wave)
!
!   output arguments:
!     waved    - real (2*mx) wave divergence field if idir<0
!                where mx=(maxwv+1)*((iromb+1)*maxwv+2)/2
!     wavez    - real (2*mx) wave vorticity field if idir>0
!                where mx=(maxwv+1)*((iromb+1)*maxwv+2)/2
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
  use general_specmod, only: spec_vars
  use constants, only: izero,zero
  implicit none

! Declare passed variables
  type(spec_vars)              ,intent(in   ) :: sp
  integer(i_kind)              ,intent(in   ) :: idir
  real(r_kind),dimension(sp%nc)   ,intent(inout) :: waved,wavez
  real(r_kind),dimension(sp%ijmax),intent(inout) :: gridu,gridv

! Declare local variables
  integer(i_kind) i

! Zero appropriate output array based on direction of transform
  if (idir<izero) then
     do i=1,sp%nc
        waved(i)=zero
        wavez(i)=zero
     end do
  elseif (idir>izero) then
     do i=1,sp%ijmax
        gridu(i)=zero
        gridv(i)=zero
     end do
  endif

! Call spectral <--> grid transform
  call general_sptranf_v(sp,waved,wavez,gridu,gridv,idir)

end subroutine general_sptez_v

subroutine general_sptranf_v(sp,waved,wavez,gridu,gridv,idir)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  sptranf_v     perform a vecor spherical transform
!   prgmmr: iredell          org: np23                date: 1996-02-29
!
! abstract: this subprogram performs a spherical transform
!           between spectral coefficients of divergences and curls
!           and vector fields on a global cylindrical grid.
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
!           This routine differs from splib routine sptranfv in that
!           it does not call sptranf0 (an initialization routine).
!
! program history log:
!   1996-02-29  iredell
!   1998-12-15  iredell  generic fft used
!   2004-08-23  treadon - adapt splib routine sptranfv for gsi use
!   2006-05-03  treadon - remove jc from specmod list since not used
!   2006-07-07  kleist - correct bug in indexing of j=1,2*ncd2 loop
!   2007-04-25  errico  - replace use of duplicate arguments
!   2010-02-18  parrish - copy to general_sptranf_v, and pass specmod vars through
!                          input variable sp of type(spec_vars)
!
!   input arguments:
!     waved    - real (*) wave divergence fields if idir>0
!     wavez    - real (*) wave vorticity fields if idir>0
!     gridu    - real (*) grid u-winds (starting at jb) if idir<0
!     gridv    - real (*) grid v-winds (starting at jb) if idir<0
!     idir     - integer transform flag
!                (idir>0 for wave to grid, idir<0 for grid to wave)
!
!   output arguments:
!     waved    - real (*) wave divergence fields if idir<0
!                [waved=(d(gridu)/dlam+d(clat*gridv)/dphi)/(clat*rerth)]
!     wavez    - real (*) wave vorticity fields if idir<0
!                [wavez=(d(gridv)/dlam-d(clat*gridu)/dphi)/(clat*rerth)]
!     gridu    - real (*) grid u-winds (starting at jb) if idir>0
!     gridv    - real (*) grid v-winds (starting at jb) if idir>0
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
  use general_specmod, only: spec_vars
  implicit none

! Declare passed variables
  type(spec_vars)              ,intent(in   ) :: sp
  integer(i_kind)              ,intent(in   ) :: idir
  real(r_kind),dimension(sp%nc)   ,intent(inout) :: waved,wavez
  real(r_kind),dimension(sp%ijmax),intent(inout) :: gridu,gridv


! Declare local variables
  integer(i_kind) i,j,jj,ijn,ijs
  integer(i_kind),dimension(2):: mp
  real(r_kind),dimension(sp%ncd2*2,2):: w
  real(r_kind),dimension(2*(sp%jcap+ione),2):: wtop
  real(r_kind),dimension(sp%imax,2,2):: g
  real(r_kind),dimension(sp%ncd2*2,2):: winc
  integer(i_kind) :: ldafft
  ldafft=256_i_kind+sp%imax

! Set parameters
  mp=ione

! Transform wave to grid
  if(idir>izero) then
     call spdz2uv(sp%iromb,sp%jcap,sp%enn1,sp%elonn1,sp%eon,sp%eontop, &
          waved,wavez, &
          w(1,1),w(1,2),wtop(1,1),wtop(1,2))
     do j=sp%jb,sp%je
        call sptranf1(sp%iromb,sp%jcap,sp%idrt,sp%imax,sp%jmax,j,j, &
             sp%eps,sp%epstop,sp%enn1,sp%elonn1,sp%eon,sp%eontop, &
             sp%afft(1:ldafft),sp%clat(j),sp%slat(j),sp%wlat(j), &
             sp%pln(1,j),sp%plntop(1,j),mp, &
             w(1,1),wtop(1,1),g(1,1,1),idir)
        call sptranf1(sp%iromb,sp%jcap,sp%idrt,sp%imax,sp%jmax,j,j, &
             sp%eps,sp%epstop,sp%enn1,sp%elonn1,sp%eon,sp%eontop, &
             sp%afft(1:ldafft),sp%clat(j),sp%slat(j),sp%wlat(j), &
             sp%pln(1,j),sp%plntop(1,j),mp, &
             w(1,2),wtop(1,2),g(1,1,2),idir)
        do i=1,sp%imax
           jj   = j-sp%jb
           ijn = i + jj*sp%jn
           ijs = i + jj*sp%js + sp%ioffset
           gridu(ijn)=g(i,1,1)
           gridu(ijs)=g(i,2,1)
           gridv(ijn)=g(i,1,2)
           gridv(ijs)=g(i,2,2)
           
        enddo
     enddo

!  Transform grid to wave
  else
     w=zero
     wtop=zero
     do j=sp%jb,sp%je
        if(sp%wlat(j)>zero) then
           do i=1,sp%imax
              jj   = j-sp%jb
              ijn = i + jj*sp%jn
              ijs = i + jj*sp%js + sp%ioffset

              g(i,1,1)=gridu(ijn)/sp%clat(j)**2
              g(i,2,1)=gridu(ijs)/sp%clat(j)**2
              g(i,1,2)=gridv(ijn)/sp%clat(j)**2
              g(i,2,2)=gridv(ijs)/sp%clat(j)**2
           enddo
           call sptranf1(sp%iromb,sp%jcap,sp%idrt,sp%imax,sp%jmax,j,j, &
                sp%eps,sp%epstop,sp%enn1,sp%elonn1,sp%eon,sp%eontop, &
                sp%afft(1:ldafft),sp%clat(j),sp%slat(j),sp%wlat(j), &
                sp%pln(1,j),sp%plntop(1,j),mp, &
                w(1,1),wtop(1,1),g(1,1,1),idir)
           call sptranf1(sp%iromb,sp%jcap,sp%idrt,sp%imax,sp%jmax,j,j, &
                sp%eps,sp%epstop,sp%enn1,sp%elonn1,sp%eon,sp%eontop, &
                sp%afft(1:ldafft),sp%clat(j),sp%slat(j),sp%wlat(j), &
                sp%pln(1,j),sp%plntop(1,j),mp, &
                w(1,2),wtop(1,2),g(1,1,2),idir)
        endif
     enddo
     call spuv2dz(sp%iromb,sp%jcap,sp%enn1,sp%elonn1,sp%eon,sp%eontop, &
          w(1,1),w(1,2),wtop(1,1),wtop(1,2), &
          winc(1,1),winc(1,2))
     
     do j=1,2*sp%ncd2
        waved(j)=waved(j)+winc(j,1)
        wavez(j)=wavez(j)+winc(j,2)
     end do
  endif

end subroutine general_sptranf_v
