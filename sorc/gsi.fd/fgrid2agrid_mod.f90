module fgrid2agrid_mod
!$$$   module documentation block
!                .      .    .                                       .
! module:    fgrid2agrid_mod module to move between analysis and filter grid
!   prgmmr: parrish          org: np22                date: 2005-06-06
!
! abstract: When using the anisotropic recursive filter in regional mode,
!             it is desirable to have a different grid to apply the filter.
!             In 3D applications, it is often necessary to run the anisotropic
!             filter on a coarser grid to keep run time and memory usage 
!             reasonable.  For 2D applications, related to the high resolution
!             NDFD surface analysis, it may be necessary to run the filter
!             on a finer grid to sufficiently resolve terrain features that
!             are incorporated in the anisotropic filter.  This module 
!             includes the setup, interpolation and adjoint routines that
!             are necessary for general interpolation between the analysis
!             grid and the desired filter grid.
!
! program history log:
!   2005-06-06  parrish
!
! subroutines included:
!   sub init_fgrid2agrid         - initialize interpolation variables and constants to defaults
!   sub create_fgrid2agrid       - compute all necessary interpolation info for desired grid ratio
!   sub get_3ops                 - called by create_fgrid2agrid--compute interpolation operators
!   sub destroy_fgrid2agrid      - free space used by interpolation constants
!   sub fgrid2agrid              - interpolate from filter grid to analysis grid
!   sub tfgrid2agrid             - adjoint of fgrid2agrid
!   sub agrid2fgrid              - interpolate from analysis grid to filter grid
!
! Variable Definitions:
!   def nlatf          - number of lats on filter grid
!   def nlonf          - number of lons on filter grid
!   def nord_f2a       - order of interpolation
!   def identity       - if true, then filter grid is same as analysis grid, 
!                         so no interpolation required
!   def grid_ratio_lon - ratio of filter grid to analysis grid in longitude direction
!   def grid_ratio_lat - ratio of filter grid to analysis grid in latitude direction
!   def f2a_lon        - structure variable containing interpolation info in longitude
!                         direction between filter and analysis grids
!   def f2a_lat        - structure variable containing interpolation info in latitude
!                         direction between filter and analysis grids
!   
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind
  use gridmod, only: nlat,nlon
  implicit none

  type fgrid2agrid_cons

    sequence

    integer(i_kind) nfgrid
    integer(i_kind) nagrid
    integer(i_kind) mfgrid
    integer(i_kind) magrid
    real(r_kind) grid_ratio
    integer(i_kind),pointer::iwin(:,:)
    integer(i_kind),pointer::nwin(:)
    integer(i_kind),pointer::itwin(:,:)
    integer(i_kind),pointer::ntwin(:)
    integer(i_kind),pointer::iswin(:,:)
    integer(i_kind),pointer::nswin(:)
    real(r_kind),pointer::win(:,:)
    real(r_kind),pointer::twin(:,:)
    real(r_kind),pointer::swin(:,:)

  end type fgrid2agrid_cons

  integer(i_kind) nlatf,nlonf,nord_f2a
  logical identity
  real(r_kind) grid_ratio_lon,grid_ratio_lat
  type(fgrid2agrid_cons) f2a_lon,f2a_lat

  contains

  subroutine init_fgrid2agrid
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_fgrid2agrid  initialize interpolation variables and constants to defaults
!   prgmmr: parrish          org: np22                date: 2005-06-06
!
! abstract: initialize structure variable designed to contain interpolation details, and also
!            various other constants to defaults.
!
! program history log:
!   2005-06-06  parrish
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
    use constants, only: one

!    initialize fgrid2agrid interpolation structure variables and other constants to defaults

    nlatf=1
    nlonf=1
    nord_f2a=4
    identity=.true.
    grid_ratio_lon=one
    grid_ratio_lat=one
    identity=.false.
    f2a_lon%nfgrid=1
    f2a_lon%nagrid=1
    f2a_lon%mfgrid=1
    f2a_lon%magrid=1
    f2a_lon%grid_ratio=one
    allocate(f2a_lon%iwin(2,2))
    allocate(f2a_lon%nwin(2))
    allocate(f2a_lon%itwin(2,2))
    allocate(f2a_lon%ntwin(2))
    allocate(f2a_lon%iswin(2,2))
    allocate(f2a_lon%nswin(2))
    allocate(f2a_lon%win(2,2))
    allocate(f2a_lon%twin(2,2))
    allocate(f2a_lon%swin(2,2))
    f2a_lat%nfgrid=1
    f2a_lat%nagrid=1
    f2a_lat%mfgrid=1
    f2a_lat%magrid=1
    f2a_lat%grid_ratio=one
    allocate(f2a_lat%iwin(2,2))
    allocate(f2a_lat%nwin(2))
    allocate(f2a_lat%itwin(2,2))
    allocate(f2a_lat%ntwin(2))
    allocate(f2a_lat%iswin(2,2))
    allocate(f2a_lat%nswin(2))
    allocate(f2a_lat%win(2,2))
    allocate(f2a_lat%twin(2,2))
    allocate(f2a_lat%swin(2,2))

  end subroutine init_fgrid2agrid

  subroutine create_fgrid2agrid(grid_ratio)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    create_fgrid2agrid  create interpolation variables
!   prgmmr: parrish          org: np22                date: 2005-06-06
!
! abstract: fill up structure variable with interpolation information for specified grid ratio.
!
!
! program history log:
!   2005-06-06  parrish
!
!   input argument list:
!     grid_ratio - ratio of filter grid resolution to analysis grid resolution
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

!    grid_ratio is desired ratio of the filter grid to analysis grid in analysis grid units.
!    compute nlatf, nlonf to get actual ratio to be the closest from below if grid_ratio > 1
!       grid_ratio_lat <= grid_ratio, grid_ratio_lon <= grid_ratio
!
!    but if grid_ratio < 1 then get actual ratio to be the closest from above, namely
!       grid_ratio_lat >= grid_ratio, grid_ratio_lon >= grid_ratio

!   if grid_ratio >= .999 and grid_ratio <= 1.001, then assume filter and analysis grid
!       are the same.

    use constants, only: one
    real(r_kind),intent(in):: grid_ratio

    if(grid_ratio.le.1.001_r_kind.and.grid_ratio.ge..999_r_kind) then
      nlatf=nlat
      nlonf=nlon 
      identity=.true.
      grid_ratio_lon=one
      grid_ratio_lat=one
      return
    else

      identity=.false.

      call get_3ops(f2a_lon,grid_ratio,grid_ratio_lon,nlon,nlonf,nord_f2a)
      call get_3ops(f2a_lat,grid_ratio,grid_ratio_lat,nlat,nlatf,nord_f2a)
     
    end if

  end subroutine create_fgrid2agrid

  subroutine get_3ops(f2a,grid_ratio_in,grid_ratio_out,ngrida,ngridf,iord)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    get_3ops          compute interpolation operators
!   prgmmr: parrish          org: np22                date: 2005-06-06
!
! abstract: obtain one-dimensional interpolation operators.
!
!
! program history log:
!   2005-06-06  parrish
!   2006-02-28  parrish - correct upper limit for j loop for adjoint of 
!                         smoothing interpolation
!
!   input argument list:
!     f2a_lat       - structure variable with previous/default interpolation information
!     grid_ratio_in - desired ratio of filter to analysis grid resolutions.
!     ngrida        - number of grid points on analysis grid for direction being considered.
!     iord          - order of interpolation
!
!   output argument list:
!     grid_ratio_out - actual ratio used (nearest value to grid_ratio_in which allows
!                       grid domain to be same for both filter and analysis grids)
!     ngridf        - number of grid points on filter grid for direction being considered.
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

!     obtain 3 types of interpolation/smoothing operators:

!         they are:

!     case 1:  grid_ratio_in > 1  :   filter grid coarse compared to analysis grid

!             win, iwin:   interpolation weights, addresses for filter grid --> analysis grid
!            twin, itwin:  adjoint of interpolation weights, addresses, analysis grid --> filter grid
!            swin, iswin:  smoothing interpolation weights, addresses for analysis grid --> filter grid
!
!     case 2:  grid_ratio_in < 1  :   filter grid finer than analysis grid
!
!        win, iwin:   smoothing interpolation weights, addresses for filter grid --> analysis grid
!        twin, itwin: adjoint of smoothing interpolation wgts, addresses for analysis grid --> filter grid
!        swin, iswin: interpolation weights, addresses for analysis grid --> filter grid
!
!
    use constants, only: zero,one

    type(fgrid2agrid_cons),intent(inout):: f2a
    real(r_kind),intent(in):: grid_ratio_in
    real(r_kind),intent(out):: grid_ratio_out
    integer(i_kind),intent(in):: iord,ngrida
    integer(i_kind),intent(out):: ngridf

    integer(i_kind) i,ii,ipmaxmax,ipminmin,j,jord,k,lbig,n,nc,nf,ntwinmax
    real(r_kind) rnc,rnf,dgrid
    integer(i_kind) ixi(0:iord)
    real(r_kind),allocatable::tl(:,:,:),alocal(:),blocal(:),grid(:),gridc(:),wgts(:,:)
    integer(i_kind),allocatable::iwgts(:,:),iflag(:)
    real(r_kind),allocatable::win(:,:),twin(:,:),swin(:,:),tswin(:,:)
    integer(i_kind),allocatable::iwin(:,:),nwin(:),itwin(:,:),ntwin(:)
    integer(i_kind),allocatable::iswin(:,:),nswin(:),itswin(:,:),ntswin(:)
    real(r_kind),allocatable::workc(:),hbig(:,:)
    integer(i_kind),allocatable::ipmax(:),ipmin(:)
!
!    actually, there are 4 types of operations, only 3 of which get saved.  We start by getting
!     basic interpolation

!--------------------------------------------------------
!-----first decide dimensions of fine grid and coarse grid
!--------------------------------------------------------

    if(grid_ratio_in.lt.one) then

      nc=ngrida
      rnf=one+(nc-one)/grid_ratio_in
      nf=rnf

    else

      nf=ngrida
      rnc=one+(nf-one)/grid_ratio_in
      nc=ceiling(rnc)

    end if

!--------------------------------------------------------
!-----define coarse grid coordinates for fine grid (coarse grid in fine grid units)
!--------------------------------------------------------

    dgrid=(nc-one)/(nf-one)   !  fine grid spacing in coarse grid units

    allocate(grid(nf))
    do i=1,nf
     grid(i)=one+dgrid*(i-one)
    end do
    grid(1)=one+nc*epsilon(one)     !  this guarantees that we interpolate
    grid(nf)=nc-nc*epsilon(one)     !   from coarse to fine

!--------------------------------------------------------
!-----get interpolation weights, indices for interpolation from coarse grid to fine grid
!--------------------------------------------------------

    allocate(gridc(nc))
    do i=1,nc
      gridc(i)=i
    end do
    allocate(tl(iord+1,iord+1,2*nc),alocal(2*nc),blocal(2*nc))
    allocate(wgts(nf,iord+1),iwgts(nf,iord+1),iflag(nf))
    allocate(win(iord+1,nf),iwin(iord+1,nf),nwin(nf))
    win=zero
    iwin=0
    nwin=0
    do jord=1,iord
     lbig=jord+1
     call simpin1_init(ixi,tl,alocal,blocal,jord,lbig,gridc,nc)
     call simpin1(wgts,wgts,wgts,iwgts,iflag,grid,nf,jord,lbig,gridc,nc,1,0,0,ixi,tl,alocal,blocal)
     do i=1,nf
      if(iflag(i).eq.1) then
       nwin(i)=lbig
       do k=1,lbig
        win(k,i)=wgts(i,k)
        iwin(k,i)=iwgts(i,k)
       end do
      end if
     end do
    end do
    if(minval(nwin).eq.0) then
         write(6,*)'GET_3OPS: ***ERROR*** while getting coarse to fine ', &
              'interpolation operator'
         call stop2(33)
    end if

!--------------------------------------------------------
!   next get adjoint weights and addresses by brute force 
!--------------------------------------------------------

    allocate(workc(nc),hbig(nf,nc))
    allocate(ipmax(nf),ipmin(nf))
    ipminmin=nf+1
    ipmaxmax=0
    do j=1,nc
      do k=1,nc
        workc(k)=zero
      end do
      workc(j)=one
      do i=1,nf
        hbig(i,j)=zero
        do n=1,nwin(i)
          hbig(i,j)=hbig(i,j)+win(n,i)*workc(iwin(n,i))
        end do
      end do
      do i=1,nf
        if(hbig(i,j).ne.zero) then
          ipmin(j)=i
          ipminmin=min(ipmin(j),ipminmin)
          exit
        end if
      end do
      do i=nf,1,-1
        if(hbig(i,j).ne.zero) then
          ipmax(j)=i
          ipmaxmax=max(ipmax(j),ipmaxmax)
          exit
        end if
      end do
    end do
    ntwinmax=ipmaxmax-ipminmin+1
    allocate(twin(ntwinmax,nc),itwin(ntwinmax,nc),ntwin(nc))
    itwin=0
    twin=zero
    do j=1,nc
     ntwin(j)=ipmax(j)-ipmin(j)+1
     ii=0
     do i=ipmin(j),ipmax(j)
      ii=ii+1
      itwin(ii,j)=i
      twin(ii,j)=hbig(i,j)
     end do
    end do

!--------------------------------------------------------
!   next get smoothing interpolation from fine to coarse
!--------------------------------------------------------

    allocate(swin(ntwinmax,nc),iswin(ntwinmax,nc),nswin(nc))
    swin=zero
    iswin=0
    nswin=0
    do j=1,nc
     workc(j)=zero
     do i=1,ntwin(j)
      workc(j)=workc(j)+twin(i,j)
     end do
     workc(j)=one/workc(j)
     nswin(j)=ntwin(j)
     do i=1,ntwin(j)
      swin(i,j)=workc(j)*twin(i,j)
      iswin(i,j)=itwin(i,j)
     end do
    end do

!--------------------------------------------------------
!   finally get adjoint of smoothing interpolation (coarse to fine)
!--------------------------------------------------------
    
    allocate(tswin(iord+1,nf),itswin(iord+1,nf),ntswin(nf))
    do i=1,nf
     ntswin(i)=nwin(i)
     do j=1,nwin(i)
      itswin(j,i)=iwin(j,i)
      tswin(j,i)=workc(iwin(j,i))*win(j,i)
     end do
    end do

!--------------------------------------------------------
!   now assign appropriate operators depending on value of grid_ratio
!--------------------------------------------------------

    if(grid_ratio_in.gt.one) then

      f2a%nfgrid=nc
      f2a%nagrid=nf
      ngridf=nc
      f2a%mfgrid=iord+1
      f2a%magrid=ntwinmax
      grid_ratio_out=(nf-one)/(nc-one)
      f2a%grid_ratio=grid_ratio_out
      deallocate(f2a%iwin,f2a%nwin,f2a%win)
      allocate(f2a%iwin(iord+1,nf))
      allocate(f2a%nwin(nf))
      allocate(f2a%win(iord+1,nf))
      f2a%iwin=0
      f2a%nwin=0
      f2a%win=zero
      do i=1,nf
        f2a%nwin(i)=nwin(i)
        do k=1,nwin(i)
          f2a%iwin(k,i)=iwin(k,i)
          f2a%win(k,i)=win(k,i)
        end do
      end do
      deallocate(f2a%itwin,f2a%ntwin,f2a%twin)
      deallocate(f2a%iswin,f2a%nswin,f2a%swin)
      allocate(f2a%itwin(ntwinmax,nc))
      allocate(f2a%iswin(ntwinmax,nc))
      allocate(f2a%ntwin(nc))
      allocate(f2a%twin(ntwinmax,nc))
      allocate(f2a%swin(ntwinmax,nc))
      allocate(f2a%nswin(nc))
      f2a%itwin=0
      f2a%ntwin=0
      f2a%twin=zero
      f2a%iswin=0
      f2a%nswin=0
      f2a%swin=zero
      do j=1,nc
        f2a%ntwin(j)=ntwin(j)
        f2a%nswin(j)=nswin(j)
        do k=1,ntwin(j)
          f2a%itwin(k,j)=itwin(k,j)
          f2a%iswin(k,j)=iswin(k,j)
          f2a%twin(k,j)=twin(k,j)
          f2a%swin(k,j)=swin(k,j)
        end do
      end do

    else

      f2a%nfgrid=nf
      f2a%nagrid=nc
      ngridf=nf
      f2a%mfgrid=ntwinmax
      f2a%magrid=iord+1
      grid_ratio_out=(nc-one)/(nf-one)
      f2a%grid_ratio=grid_ratio_out
      deallocate(f2a%iwin,f2a%nwin,f2a%win)
      allocate(f2a%iwin(ntwinmax,nc))
      allocate(f2a%nwin(nc))
      allocate(f2a%win(ntwinmax,nc))
      f2a%iwin=0
      f2a%nwin=0
      f2a%win=zero
      do j=1,nc
        f2a%nwin(j)=nswin(j)
        do k=1,nswin(j)
          f2a%iwin(k,j)=iswin(k,j)
          f2a%win(k,j)=swin(k,j)
        end do
      end do
      deallocate(f2a%itwin,f2a%ntwin,f2a%twin)
      deallocate(f2a%iswin,f2a%nswin,f2a%swin)
      allocate(f2a%itwin(iord+1,nf))
      allocate(f2a%iswin(iord+1,nf))
      allocate(f2a%ntwin(nf))
      allocate(f2a%nswin(nf))
      allocate(f2a%twin(iord+1,nf))
      allocate(f2a%swin(iord+1,nf))
      f2a%itwin=0
      f2a%ntwin=0
      f2a%twin=zero
      f2a%iswin=0
      f2a%nswin=0
      f2a%swin=zero
      do i=1,nf
        f2a%ntwin(i)=ntswin(i)
        f2a%nswin(i)=nwin(i)
        do k=1,ntswin(i)
          f2a%itwin(k,i)=itswin(k,i)
          f2a%iswin(k,i)=iwin(k,i)
          f2a%twin(k,i)=tswin(k,i)
          f2a%swin(k,i)=win(k,i)
        end do
      end do

    end if

    deallocate(tl,alocal,blocal,wgts,iwgts,iflag,grid,gridc)
    deallocate(hbig,win,iwin,nwin,workc,ipmax,ipmin)
    deallocate(twin,itwin,ntwin,swin,iswin,nswin,tswin,itswin,ntswin)

  end subroutine get_3ops

  subroutine destroy_fgrid2agrid
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    destroy_fgrid2agrid   release space used by fgrid2agrid
!   prgmmr: parrish          org: np22                date: 2005-06-06
!
! abstract: release space used by fgrid2agrid.
!
!
! program history log:
!   2005-06-06  parrish
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

! free space used by interpolation structures

    deallocate(f2a_lon%iwin,f2a_lon%nwin,f2a_lon%itwin)
    deallocate(f2a_lon%ntwin,f2a_lon%iswin,f2a_lon%nswin)
    deallocate(f2a_lon%win,f2a_lon%twin,f2a_lon%swin)
    deallocate(f2a_lat%iwin,f2a_lat%nwin,f2a_lat%itwin)
    deallocate(f2a_lat%ntwin,f2a_lat%iswin,f2a_lat%nswin)
    deallocate(f2a_lat%win,f2a_lat%twin,f2a_lat%swin)
    call init_fgrid2agrid

  end subroutine destroy_fgrid2agrid

  subroutine fgrid2agrid(f,a)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    fgrid2agrid   interpolate from filter to analysis grid
!   prgmmr: parrish          org: np22                date: 2005-06-06
!
! abstract: interpolate from filter to analysis grid
!
!
! program history log:
!   2005-06-06  parrish
!
!   input argument list:
!     f             - filter grid
!
!   output argument list:
!     a              - analysis grid
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
    real(r_kind),intent(in):: f(nlatf,nlonf)
    real(r_kind),intent(out):: a(nlat,nlon)

    integer i,j,j1,k
    real(r_kind) w1,w(nlat,nlonf)

    if(identity) then
      do j=1,nlonf
       do i=1,nlatf
        a(i,j)=f(i,j)
       end do
      end do
    else
      do j=1,nlonf
        do i=1,nlat
          w(i,j)=f2a_lat%win(1,i)*f(f2a_lat%iwin(1,i),j)
          do k=2,f2a_lat%nwin(i)
            w(i,j)=w(i,j)+f2a_lat%win(k,i)*f(f2a_lat%iwin(k,i),j)
          end do
        end do
      end do
      do j=1,nlon
        j1=f2a_lon%iwin(1,j)
        w1=f2a_lon%win(1,j)
        do i=1,nlat
          a(i,j)=w1*w(i,j1)
        end do
        do k=2,f2a_lon%nwin(j)
         j1=f2a_lon%iwin(k,j)
         w1=f2a_lon%win(k,j)
         do i=1,nlat
           a(i,j)=a(i,j)+w1*w(i,j1)
         end do
        end do
      end do
    end if

  end subroutine fgrid2agrid

  subroutine tfgrid2agrid(a,f)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    tfgrid2agrid  adjoint of fgrid2agrid
!   prgmmr: parrish          org: np22                date: 2005-06-06
!
! abstract: adjoint of fgrid2agrid
!
!
! program history log:
!   2005-06-06  parrish
!
!   input argument list:
!     a              - analysis grid
!
!   output argument list:
!     f             - filter grid
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

    use constants, only: zero
    real(r_kind),intent(out):: f(nlatf,nlonf)
    real(r_kind),intent(in):: a(nlat,nlon)

    integer i,j,j1,k
    real(r_kind) w1,w(nlat,nlonf)

    if(identity) then
      do j=1,nlonf
        do i=1,nlatf
          f(i,j)=a(i,j)
        end do
      end do
    else
      do j=1,nlonf
        j1=f2a_lon%itwin(1,j)
        w1=f2a_lon%twin(1,j)
        do i=1,nlat
          w(i,j)=w1*a(i,j1)
        end do
        do k=2,f2a_lon%ntwin(j)
         j1=f2a_lon%itwin(k,j)
         w1=f2a_lon%twin(k,j)
         do i=1,nlat
           w(i,j)=w(i,j)+w1*a(i,j1)
         end do
        end do
      end do
      do j=1,nlonf
        do i=1,nlatf
          f(i,j)=f2a_lat%twin(1,i)*w(f2a_lat%itwin(1,i),j)
          do k=2,f2a_lat%ntwin(i)
            f(i,j)=f(i,j)+f2a_lat%twin(k,i)*w(f2a_lat%itwin(k,i),j)
          end do
        end do
      end do
    end if

  end subroutine tfgrid2agrid

  subroutine agrid2fgrid(a,f)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    agrid2fgrid  interpolate from agrid to fgrid
!   prgmmr: parrish          org: np22                date: 2005-06-06
!
! abstract: interpolate from agrid to fgrid
!
!
! program history log:
!   2005-06-06  parrish
!
!   input argument list:
!     a              - analysis grid
!
!   output argument list:
!     f             - filter grid
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

    use constants, only: zero
    real(r_kind),intent(out):: f(nlatf,nlonf)
    real(r_kind),intent(in):: a(nlat,nlon)

    integer i,j,j1,k
    real(r_kind) w1,w(nlat,nlonf)

    if(identity) then
      do j=1,nlonf
        do i=1,nlatf
          f(i,j)=a(i,j)
        end do
      end do
    else
      do j=1,nlonf
        j1=f2a_lon%iswin(1,j)
        w1=f2a_lon%swin(1,j)
        do i=1,nlat
          w(i,j)=w1*a(i,j1)
        end do
        do k=2,f2a_lon%nswin(j)
         j1=f2a_lon%iswin(k,j)
         w1=f2a_lon%swin(k,j)
         do i=1,nlat
           w(i,j)=w(i,j)+w1*a(i,j1)
         end do
        end do
      end do
      do j=1,nlonf
        do i=1,nlatf
          f(i,j)=f2a_lat%swin(1,i)*w(f2a_lat%iswin(1,i),j)
          do k=2,f2a_lat%nswin(i)
            f(i,j)=f(i,j)+f2a_lat%swin(k,i)*w(f2a_lat%iswin(k,i),j)
          end do
        end do
      end do
    end if

  end subroutine agrid2fgrid

end module fgrid2agrid_mod
