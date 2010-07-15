!#define _DEBUG_
module hybrid_ensemble_isotropic_regional
!$$$   module documentation block
!                .      .    .                                       .
! module:    hybrid_ensemble_isotropic_regional
!   prgmmr: parrish          org: np22                date: 2009-09-28
!
! abstract: contains routines for localization of the hybrid ensemble
!            control variable a_en.  this application is for 
!            localization with a regional model with homogeneous scale in horizontal
!            and vertical scale a function of vertical only.
!
! program history log:
!   2009-09-28  parrish, initial documentation.
!   2010-02-26  parrish, remove redundant special spectral and sub2grid/grid2sub code.  replaced
!                 by general purpose code as part of adding dual resolution option.
!   2010-03-17  zhu  - use vlevs from gridmod
!   2010-04-06  parrish - fix dimension error in ensemble_forward_model_ad_dual_res and
!                           add array deallocation in ensemble_forward_model_ad_dual_res and
!                            ensemble_forward_model_dual_res
!   2010-05-14  parrish - repair code to get dual resolution option working again.  remove
!                           error stops when dual resolution invoked.
!   2010-05-20  todling - renamed all cstate to bundle to avoid confusion; the
!                         bundles here are not necessarily idendical to the control vector,
!                         but rather what this module take the ensemble to be composed of
!
! subroutines included:
!   sub init_rf_z                         - initialize localization recursive filter (z direction)
!   sub init_rf_x                         - initialize localization recursive filter (x direction)
!   sub init_rf_y                         - initialize localization recursive filter (y direction)
!   sub new_factorization_rf_z            - localization recursive filter (z direction)
!   sub new_factorization_rf_x            - localization recursive filter (x direction)
!   sub new_factorization_rf_y            - localization recursive filter (y direction)
!   sub normal_new_factorization_rf_z     - normalize localization recursive filter (z direction)
!   sub normal_new_factorization_rf_x     - normalize localization recursive filter (x direction)
!   sub normal_new_factorization_rf_y     - normalize localization recursive filter (y direction)
!   sub create_ensemble                   - allocate space for ensemble perturbations
!   sub load_ensemble                     - read/generate ensemble perturbations
!   sub rescale_ensemble_rh_perturbations - for internal generated perturbations only, normalize by ges q
!   sub ensemble_forward_model            - add ensemble contribution to analysis increment
!   sub ensemble_forward_model_ad         - adjoint of ensemble_forward_model
!   sub ensemble_forward_model_dual_res   - dual resolution version of ensemble_forward_model
!   sub ensemble_forward_model_ad_dual_res- adjoint of ensemble_forward_model_dual_res
!
! Variable Definitions:
!   def yyyy      - what yyyy is
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind,r_single
  use mpimod, only: mype
  use gsi_bundlemod, only: gsi_bundlecreate
  use gsi_bundlemod, only: gsi_grid
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use gsi_bundlemod, only: gsi_bundledestroy
  use gsi_bundlemod, only: gsi_gridcreate
  implicit none

! set default to private
  private
! set subroutines to public
  public :: init_rf_z
  public :: init_rf_x
  public :: init_rf_y
  public :: new_factorization_rf_z
  public :: new_factorization_rf_x
  public :: new_factorization_rf_y
  public :: normal_new_factorization_rf_z
  public :: normal_new_factorization_rf_x
  public :: normal_new_factorization_rf_y
  public :: create_ensemble
  public :: load_ensemble
  public :: rescale_ensemble_rh_perturbations
  public :: ensemble_forward_model
  public :: ensemble_forward_model_dual_res
  public :: ensemble_forward_model_ad
  public :: ensemble_forward_model_ad_dual_res
  public :: beta12mult
! set passed variables to public
  public :: st_en,vp_en,t_en,rh_en,oz_en,cw_en,p_en,sst_en

  character(len=*),parameter::myname='hybrid_ensemble_isotropic_regional'
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!      following variables are filter parameters for isotropic
!       homogeneous localization of hybrid control variable a_en

  real(r_kind),allocatable:: fmatz(:,:,:)
  real(r_kind),allocatable:: fmat0z(:,:)
  real(r_kind),allocatable:: fmatx(:,:,:,:)
  real(r_kind),allocatable:: fmat0x(:,:,:)
  real(r_kind),allocatable:: fmaty(:,:,:)
  real(r_kind),allocatable:: fmat0y(:,:)
  real(r_kind),allocatable:: znorm_new(:)
  real(r_kind),allocatable:: xnorm_new(:,:)
  real(r_kind),allocatable:: ynorm_new(:)

!    following is for storage of ensemble perturbations:

!   def st_en               - array of stream function ensemble perturbations
!   def vp_en               - array of potential function ensemble perturbations
!   def t_en                - array of virtual temp ensemble perturbations
!   def rh_en               - array of relative humidity ensemble perturbations
!   def oz_en               - array of ozone ensemble perturbations
!   def cw_en               - array of ozone ensemble perturbations
!   def p_en                - array of surface pressure ensemble perturbations
!   def sst_en              - array of skin temperature ensemble perturbations

  real(r_single),dimension(:,:),allocatable:: st_en,vp_en,t_en,rh_en,oz_en,cw_en,p_en,sst_en

!    following is for special subdomain to slab variables used when internally generating ensemble members

  integer(i_kind) nval2f,nscl
  integer(i_kind) nh_0,nh_1,nv_0,nv_1
  integer(i_kind),allocatable,dimension(:):: nsend_sd2h,ndsend_sd2h,nrecv_sd2h,ndrecv_sd2h
  integer(i_kind),allocatable,dimension(:):: i_recv,k_recv

  logical,parameter:: debug=.false.

! The following needs to come from a file outside the source code (Todling)
! NOTE: These names are convention they cannot be changed
  integer(i_kind),parameter::ne3d=6
  character(len=3),parameter::evars3d(ne3d)=(/'sf ','vp ','t  ','q  ','oz ','cw '/)
  integer(i_kind),parameter::ne2d=2
  character(len=3),parameter::evars2d(ne2d)=(/'ps ','sst'/)
  integer(i_kind),parameter::nevs=ne3d+ne2d
  character(len=3),parameter::evars(nevs)=(/'sf ','vp ','t  ','q  ','oz ','cw ','ps ','sst'/)


contains

subroutine init_rf_z(z_len)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_rf_z    initialize vertical recursive filter
!   prgmmr: parrish          org: np22                date: 2009-12-16
!
! abstract: initialize vertical recursive filter for hybrid ensemble control variable a_en
!            call this one first, then init_rf_x, init_rf_y
!
! program history log:
!   2009-12-16  parrish
!   2010-04-06  parrish - add 2nd option for units of vertical localization:
!                             if z_len < 0, then abs(z_len) is vertical localization length scale in
!                             units of ln(p).  otherwise, when z_len > 0, localization is in vertical
!                             grid units.  The ln(p) distance measurement is approximate, based on a
!                             fixed surface pressure of 1000mb.  This is because at the point where this
!                             is currently called, the background 3d pressure field is not yet available.
!                             A later version will correct this.
!                             For the current s_ens_v > 0, the measure is vertical grid units.  
!                             s_ens_v = 20 and s_ens_v = -0.44 are roughly comparable, and
!                             connection of .44 is .44 = (sqrt(.15)/sqrt(2))*1.6, where 1.6 is the value used
!                             by Jeff Whitaker for his distance in which the Gaspari-Cohn function 1st = 0.
!
!   input argument list:
!     z_len    - filter length scale in grid units
!
!   output argument list:
!
! remarks:  see modules used
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use gridmod, only: nsig,ak5,bk5
  use constants, only: half,one,rd_over_cp,zero

  real(r_kind),intent(in   ) :: z_len

  integer(i_kind) k,km,k0m,kp,k0p
  real(r_kind) aspect(nsig),p_interface(nsig+1),lnp_layer(nsig)
  real(r_kind) p_layer,dlnp,kap1,kapr

    kap1=rd_over_cp+one
    kapr=one/rd_over_cp

!    use new factorization:
  allocate(fmatz(2,nsig,2),fmat0z(nsig,2))

!   for z_len < zero, use abs val z_len and assume localization scale is in units of ln(p)
  if(z_len > zero) then

!  z_len is in grid units
     do k=1,nsig
        aspect(k)=z_len**2
     end do

  else

!  abs(z_len) is in units of ln(p)

!              put in approximate vertical scale which depends on ln(p)
!             to do this with minimal change, use psfc=1000, and construct a pressure profile
!                 from the vertical coordinate definition for the GFS model.
     do k=1,nsig+1
        p_interface(k)=ak5(k)+(bk5(k)*100._r_kind)
     end do
     do k=1,nsig
        p_layer=((p_interface(k)**kap1-p_interface(k+1)**kap1)/&
                           (kap1*(p_interface(k)-p_interface(k+1))))**kapr
               if(mype==0) write(6,*)' k,p_layer=',k,p_layer
        lnp_layer(k)=log(p_layer)
     end do
     do k=1,nsig
        kp=min(k+1,nsig)
        k0p=kp-1
        km=max(k-1,1)
        k0m=km+1
        dlnp=half*((lnp_layer(k0p)-lnp_layer(kp))+(lnp_layer(km)-lnp_layer(k0m)))
        aspect(k)=(z_len/dlnp)**2
        if(mype == 0) write(6,'(" k, vertical localization in grid units for ln(p) scaling =",i4,f10.2)') &
                                        k,sqrt(aspect(k))
     end do

  end if
  call get_new_alpha_beta(aspect,nsig,fmatz,fmat0z)

end subroutine init_rf_z

subroutine init_rf_x(x_len)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_rf_x    initialize x direction recursive filter
!   prgmmr: parrish          org: np22                date: 2009-12-16
!
! abstract: initialize longitude recursive filters for hybrid ensemble control variable a_en
!
! program history log:
!   2009-12-16  parrish
!
!   input argument list:
!     x_len -- filter length scale in grid units
!
!   output argument list:
!
! remarks:  see modules used
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use gridmod, only: nlat,nlon,region_dy,region_dx
  use constants, only: half

  real(r_kind),intent(in   ) :: x_len

  integer(i_kind) i,j,k,l,m
  real(r_kind) aspect(nlon)
  real(r_kind) fmatc(2,nlon,2),fmat0c(nlon,2)

!    use new factorization:
  if(allocated(fmatx)) deallocate(fmatx)
  if(allocated(fmat0x)) deallocate(fmat0x)
  allocate(fmatx(nlat,2,nlon,2),fmat0x(nlat,nlon,2))
  do i=1,nlat
     do j=1,nlon
        aspect(j)=(x_len*region_dy(nlat/2,nlon/2)/region_dx(i,j))**2 ! only works for rotated lat-lon grids
     end do
     call get_new_alpha_beta(aspect,nlon,fmatc,fmat0c)
     do k=1,2
        do j=1,nlon
           do l=1,2
              fmatx(i,l,j,k)=fmatc(l,j,k)
           end do
           fmat0x(i,j,k)=fmat0c(j,k)
        end do
     end do
  end do

end subroutine init_rf_x

subroutine init_rf_y(y_len)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_rf_y    initialize y direction recursive filter
!   prgmmr: parrish          org: np22                date: 2009-12-16
!
! abstract: initialize latitude recursive filters for hybrid ensemble control variable a_en
!
! program history log:
!   2009-12-16  parrish
!
!   input argument list:
!     y_len -- filter length scale in grid units
!
!   output argument list:
!
! remarks:  see modules used
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use gridmod, only: nlat
  use constants, only: half

  real(r_kind),intent(in   ) :: y_len

  real(r_kind) aspect(nlat)
  integer(i_kind) i,m

!    use new factorization:
  if(allocated(fmaty)) deallocate(fmaty)
  if(allocated(fmat0y)) deallocate(fmat0y)
  allocate(fmaty(2,nlat,2),fmat0y(nlat,2))
  do i=1,nlat
     aspect(i)=y_len**2
  end do
  call get_new_alpha_beta(aspect,nlat,fmaty,fmat0y)

end subroutine init_rf_y

subroutine new_factorization_rf_z(f,iadvance,iback)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    new_factorization_rf_z
!
!   prgrmmr:  parrish        org: np22                              date: 2009-09-28
!
! abstract:  apply new factorization Purser 1-d high-order filter in z (vertical) dimension.
!
! program history log:
!   2009-09-28  parrish  initial documentation
!   2010-02-20  parrish  modifications for dual resolution
!
!   input argument list:
!     f        - input field to be filtered
!     iadvance - =1  for forward operator, =2 for adjoint operator
!     iback    - =2  for forward operator, =1 for adjoint operator
!
!   output argument list:
!     f        - filtered output
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

  use hybrid_ensemble_parameters, only: grd_ens
  implicit none

  integer(i_kind),intent(in   ) :: iadvance,iback
  real(r_kind)   ,intent(inout) :: f(grd_ens%latlon11,grd_ens%nsig)

  integer(i_kind) i,j,k,l,nxy,nz

  nxy=grd_ens%latlon11 ; nz=grd_ens%nsig
  if(iadvance == 1) then
     do k=1,nz
        do i=1,nxy
           f(i,k)=znorm_new(k)*f(i,k)
        end do
     end do
  end if
  do k=1,nz
     do l=1,min(2,k-1)
        do i=1,nxy
           f(i,k)=f(i,k)-fmatz(l,k,iadvance)*f(i,k-l)
        end do
     end do
     do i=1,nxy
        f(i,k)=fmat0z(k,iadvance)*f(i,k)
     end do
  end do
  do k=nz,1,-1
     do l=1,min(2,nz-k)
        do i=1,nxy
           f(i,k)=f(i,k)-fmatz(l,k+l,iback)*f(i,k+l)
        end do
     end do
     do i=1,nxy
        f(i,k)=fmat0z(k,iback)*f(i,k)
     end do
  end do
  if(iadvance == 2) then
     do k=1,nz
        do i=1,nxy
           f(i,k)=znorm_new(k)*f(i,k)
        end do
     end do
  end if

end subroutine new_factorization_rf_z

subroutine new_factorization_rf_x(f,iadvance,iback,nlevs)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    new_factorization_rf_x
!
!   prgrmmr:  parrish        org: np22                              date: 2009-09-28
!
! abstract:  apply new factorization Purser 1-d high-order filter in x (longitude) direction.
!
! program history log:
!   2009-09-28  parrish  initial documentation
!   2010-02-20  parrish  modifications for dual resolution
!   2010-03-11  parrish  adjust for possibility that nlevs=0
!
!   input argument list:
!     f        - input field to be filtered
!     iadvance - =1  for forward operator, =2 for adjoint operator
!     iback    - =2  for forward operator, =1 for adjoint operator
!
!   output argument list:
!     f        - filtered output
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
  use hybrid_ensemble_parameters, only: grd_ens
  implicit none

  integer(i_kind),intent(in   ) :: iadvance,iback,nlevs
  real(r_kind)   ,intent(inout) :: f(grd_ens%nlat,grd_ens%nlon,max(nlevs,1))

  integer(i_kind) i,j,k,l,ny,nx,nz

  ny=grd_ens%nlat ; nx=grd_ens%nlon ; nz=nlevs
  do k=1,nz

     if(iadvance == 1) then
        do j=1,nx
           do i=1,ny
              f(i,j,k)=xnorm_new(i,j)*f(i,j,k)
           end do
        end do
     end if

     do j=1,nx
        do l=1,min(2,j-1)
           do i=1,ny
              f(i,j,k)=f(i,j,k)-fmatx(i,l,j,iadvance)*f(i,j-l,k)
           end do
        end do
        do i=1,ny
           f(i,j,k)=fmat0x(i,j,iadvance)*f(i,j,k)
        end do
     end do

     do j=nx,1,-1
        do l=1,min(2,nx-j)
           do i=1,ny
              f(i,j,k)=f(i,j,k)-fmatx(i,l,j+l,iback)*f(i,j+l,k)
           end do
        end do
        do i=1,ny
           f(i,j,k)=fmat0x(i,j,iback)*f(i,j,k)
        end do
     end do

     if(iadvance == 2) then
        do j=1,nx
           do i=1,ny
              f(i,j,k)=xnorm_new(i,j)*f(i,j,k)
           end do
        end do
     end if

  end do

end subroutine new_factorization_rf_x

subroutine new_factorization_rf_y(f,iadvance,iback,nlevs)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    new_factorization_rf_y
!
!   prgrmmr:  parrish        org: np22                              date: 2009-09-28
!
! abstract:  apply new factorization Purser 1-d high-order filter in y (latitude) direction.
!
! program history log:
!   2009-09-28  parrish  initial documentation
!   2010-02-20  parrish  modifications for dual resolution
!   2010-03-11  parrish  adjust for possibility that nlevs=0
!
!   input argument list:
!     f        - input field to be filtered
!     iadvance - =1  for forward operator, =2 for adjoint operator
!     iback    - =2  for forward operator, =1 for adjoint operator
!
!   output argument list:
!     f        - filtered output
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
  use gridmod, only: nlon
  use hybrid_ensemble_parameters, only: grd_ens
                      !                  use mpimod, only: mype
  implicit none

  integer(i_kind),intent(in   ) :: iadvance,iback,nlevs
  real(r_kind)   ,intent(inout) :: f(grd_ens%nlat,grd_ens%nlon*max(nlevs,1))

  integer(i_kind) i,k,l,nx,ny,nz

  nx=grd_ens%nlon ; ny=grd_ens%nlat ; nz=nlevs
  do k=1,nx*nz

     if(iadvance == 1) then
        do i=1,ny
           f(i,k)=ynorm_new(i)*f(i,k)
        end do
     end if

     do i=1,ny
        do l=1,min(2,i-1)
           f(i,k)=f(i,k)-fmaty(l,i,iadvance)*f(i-l,k)
        end do
        f(i,k)=fmat0y(i,iadvance)*f(i,k)
     end do

     do i=ny,1,-1
        do l=1,min(2,ny-i)
           f(i,k)=f(i,k)-fmaty(l,i+l,iback)*f(i+l,k)
        end do
        f(i,k)=fmat0y(i,iback)*f(i,k)
     end do

     if(iadvance == 2) then
        do i=1,ny
           f(i,k)=ynorm_new(i)*f(i,k)
        end do
     end if

  end do

end subroutine new_factorization_rf_y

subroutine normal_new_factorization_rf_z
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    normal_new_factorization_rf_z get normalization factor in z direction
!
!   prgrmmr:  parrish        org: np22                              date: 2009-09-28
!
! abstract:  compute normalization factor in z (vertical dimension)
!
! program history log:
!   2009-09-28  parrish  initial documentation
!   2010-02-20  parrish  modifications for dual resolution
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind
  use hybrid_ensemble_parameters, only: grd_ens
  use constants, only: zero,one
  implicit none

  integer(i_kind) k,kcount,lcount,iadvance,iback
  real(r_kind) f(grd_ens%latlon11,grd_ens%nsig),diag(grd_ens%nsig)

  if(allocated(znorm_new)) deallocate(znorm_new)
  allocate(znorm_new(grd_ens%nsig))

  znorm_new=one
  kcount=0
  lcount=0
  do
     f=zero
     do k=1,min(grd_ens%latlon11,grd_ens%nsig)
        kcount=kcount+1
        f(k,kcount)=one
        if(kcount == grd_ens%nsig) exit
     end do
     iadvance=1 ; iback=2
     call new_factorization_rf_z(f,iadvance,iback)
     iadvance=2 ; iback=1
     call new_factorization_rf_z(f,iadvance,iback)
     do k=1,min(grd_ens%latlon11,grd_ens%nsig)
        lcount=lcount+1
        diag(lcount)=sqrt(one/f(k,lcount))
        if(lcount == grd_ens%nsig) exit
     end do
     if(lcount == grd_ens%nsig) exit
  end do
  do k=1,grd_ens%nsig
     znorm_new(k)=diag(k)
  end do
!              check result:
  kcount=0
  lcount=0
  do
     f=zero
     do k=1,min(grd_ens%latlon11,grd_ens%nsig)
        kcount=kcount+1
        f(k,kcount)=one
        if(kcount == grd_ens%nsig) exit
     end do
     iadvance=1 ; iback=2
     call new_factorization_rf_z(f,iadvance,iback)
     iadvance=2 ; iback=1
     call new_factorization_rf_z(f,iadvance,iback)
     do k=1,min(grd_ens%latlon11,grd_ens%nsig)
        lcount=lcount+1
        diag(lcount)=f(k,lcount)
        if(lcount == grd_ens%nsig) exit
     end do
     if(lcount == grd_ens%nsig) exit
  end do
  write(6,*)' in normal_new_factorization_rf_z, min,max(diag)=',minval(diag),maxval(diag)

end subroutine normal_new_factorization_rf_z

subroutine normal_new_factorization_rf_x
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    normal_new_factorization_rf_x get normalization factor in x direction
!
!   prgrmmr:  parrish        org: np22                              date: 2009-09-28
!
! abstract:  compute normalization factor in longitude direction
!
! program history log:
!   2009-09-28  parrish  initial documentation
!   2010-02-20  parrish  modifications for dual resolution
!   2010-03-11  parrish  correct error that can lead to infinite loop, and introduce grd_ens%kend_alloc
!                         in dimension statements
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind
  use gridmod, only: nlon
  use hybrid_ensemble_parameters, only: grd_ens
  use constants, only: zero,one

  integer(i_kind) i,j,k,kcount,lcount,iadvance,iback
  real(r_kind) f(grd_ens%nlat,nlon,grd_ens%kend_alloc+1-grd_ens%kbegin_loc),diag(grd_ens%nlat,nlon)

!                       possible to have kend_loc - kbegin_loc-1 for processors not involved
!                          which results in infinite loops

  if(grd_ens%kend_loc < grd_ens%kbegin_loc) return

  if(allocated(xnorm_new)) deallocate(xnorm_new)
  allocate(xnorm_new(grd_ens%nlat,nlon))
  xnorm_new=one

  kcount=0
  lcount=0
  do
     f=zero
     do k=1,min(nlon,grd_ens%kend_loc+1-grd_ens%kbegin_loc)
        kcount=kcount+1
        do i=1,grd_ens%nlat
           f(i,kcount,k)=one
        end do
        if(kcount == nlon) exit
     end do
     iadvance=1 ; iback=2
     call new_factorization_rf_x(f,iadvance,iback,grd_ens%kend_loc+1-grd_ens%kbegin_loc)
     iadvance=2 ; iback=1
     call new_factorization_rf_x(f,iadvance,iback,grd_ens%kend_loc+1-grd_ens%kbegin_loc)
     do k=1,min(nlon,grd_ens%kend_loc+1-grd_ens%kbegin_loc)
        lcount=lcount+1
        do i=1,grd_ens%nlat
           diag(i,lcount)=sqrt(one/f(i,lcount,k))
        end do
        if(lcount == nlon) exit
     end do
     if(lcount == nlon) exit
  end do
  do j=1,nlon
     do i=1,grd_ens%nlat
        xnorm_new(i,j)=diag(i,j)
     end do
  end do

!           check accuracy of xnorm
  kcount=0
  lcount=0
  do
     f=zero
     do k=1,min(nlon,grd_ens%kend_loc+1-grd_ens%kbegin_loc)
        kcount=kcount+1
        do i=1,grd_ens%nlat
           f(i,kcount,k)=one
        end do
        if(kcount == nlon) exit
     end do
     iadvance=1 ; iback=2
     call new_factorization_rf_x(f,iadvance,iback,grd_ens%kend_loc+1-grd_ens%kbegin_loc)
     iadvance=2 ; iback=1
     call new_factorization_rf_x(f,iadvance,iback,grd_ens%kend_loc+1-grd_ens%kbegin_loc)
     do k=1,min(nlon,grd_ens%kend_loc+1-grd_ens%kbegin_loc)
        lcount=lcount+1
        do i=1,grd_ens%nlat
           diag(i,lcount)=f(i,lcount,k)
        end do
        if(lcount == nlon) exit
     end do
     if(lcount == nlon) exit
  end do
  write(6,*)' in normal_new_factorization_rf_x, min,max(diag)=',minval(diag),maxval(diag)

end subroutine normal_new_factorization_rf_x

subroutine normal_new_factorization_rf_y
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    normal_new_factorization_rf_y  get normalization factor in y direction
!
!   prgrmmr:  parrish        org: np22                              date: 2009-09-28
!
! abstract:  compute normalization factor in latitude direction
!
! program history log:
!   2009-09-28  parrish  initial documentation
!   2010-02-20  parrish  modifications for dual resolution
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind
  use gridmod, only: nlat,nlon
  use hybrid_ensemble_parameters, only: grd_ens
  use constants, only: zero,one
  use mpimod, only: mype
  implicit none

  integer(i_kind) k,kcount,lcount,iadvance,iback
  real(r_kind) f(nlat,nlon*(grd_ens%kend_alloc+1-grd_ens%kbegin_loc)),diag(nlat)

!                       possible to have kend_loc - kbegin_loc-1 for processors not involved
!                          which results in infinite loops

  if(grd_ens%kend_loc < grd_ens%kbegin_loc) return

  if(allocated(ynorm_new)) deallocate(ynorm_new)
  allocate(ynorm_new(nlat))
  ynorm_new=one

  kcount=0
  lcount=0
  do
     f=zero
     do k=1,min(nlon*(grd_ens%kend_loc+1-grd_ens%kbegin_loc),nlat)
        kcount=kcount+1
        f(kcount,k)=one
        if(kcount == nlat) exit
     end do
     iadvance=1 ; iback=2
     call new_factorization_rf_y(f,iadvance,iback,grd_ens%kend_loc+1-grd_ens%kbegin_loc)
     iadvance=2 ; iback=1
     call new_factorization_rf_y(f,iadvance,iback,grd_ens%kend_loc+1-grd_ens%kbegin_loc)
     do k=1,min(nlon*(grd_ens%kend_loc+1-grd_ens%kbegin_loc),nlat)
        lcount=lcount+1
        diag(lcount)=sqrt(one/f(lcount,k))
        if(lcount == nlat) exit
     end do
     if(lcount == nlat) exit
  end do
  do k=1,nlat
     ynorm_new(k)=diag(k)
  end do

!               check that ynorm is corect
  kcount=0
  lcount=0
  do
     f=zero
     do k=1,min(nlon*(grd_ens%kend_loc+1-grd_ens%kbegin_loc),nlat)
        kcount=kcount+1
        f(kcount,k)=one
        if(kcount == nlat) exit
     end do
     iadvance=1 ; iback=2
     call new_factorization_rf_y(f,iadvance,iback,grd_ens%kend_loc+1-grd_ens%kbegin_loc)
     iadvance=2 ; iback=1
     call new_factorization_rf_y(f,iadvance,iback,grd_ens%kend_loc+1-grd_ens%kbegin_loc)
     do k=1,min(nlon*(grd_ens%kend_loc+1-grd_ens%kbegin_loc),nlat)
        lcount=lcount+1
        diag(lcount)=f(lcount,k)
        if(lcount == nlat) exit
     end do
     if(lcount == nlat) exit
  end do
  write(6,*)' in normal_new_factorization_rf_y, min,max(diag)=',minval(diag),maxval(diag)

end subroutine normal_new_factorization_rf_y

  subroutine create_ensemble
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    create_ensemble        allocate space for ensembles
!   prgmmr: parrish          org: np22                date: 2009-06-16
!
! abstract: allocate space for ensemble perturbations used with the 
!             hybrid ensemble option.
!
! program history log:
!   2009-06-16  parrish
!   2010-02-20  parrish  modifications for dual resolution
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
    use hybrid_ensemble_parameters, only: n_ens,grd_ens

    implicit none


    allocate(st_en(grd_ens%latlon1n,n_ens), vp_en(grd_ens%latlon1n,n_ens), &
              t_en(grd_ens%latlon1n,n_ens), rh_en(grd_ens%latlon1n,n_ens), &
             oz_en(grd_ens%latlon1n,n_ens), cw_en(grd_ens%latlon1n,n_ens), &
              p_en(grd_ens%latlon11,n_ens),sst_en(grd_ens%latlon11,n_ens))
    if(debug) write(6,*)' in create_ensemble, grd_ens%latlon11,grd_ens%latlon1n,n_ens=', &
                                    grd_ens%latlon11,grd_ens%latlon1n,n_ens
    if(debug) write(6,*)' in create_ensemble, total bytes allocated=',4*(6*grd_ens%latlon1n+2*grd_ens%latlon11)*n_ens

  end subroutine create_ensemble

  subroutine load_ensemble
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    load_ensemble    read/generate ensemble perturbations
!   prgmmr: parrish          org: np22                date: 2009-09-11
!
! abstract: read or generate (if generate_ens=.true.) ensemble
!             perturbations used for hybrid ensemble option.
!
! program history log:
!   2009-09-11  parrish
!   2010-02-20  parrish  modifications for dual resolution
!   2010-03-15  zhu      make changes using cstate
!   2010-03-28  todling  update to use gsi_bundle
!   2010-06-03  parrish  multiple fixes for dual resolution
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
    use gridmod, only: regional
    use constants, only: zero,one
    use hybrid_ensemble_parameters, only: n_ens,generate_ens,grd_ens,grd_anl
    use mpimod, only: mype,ierror
    implicit none

!   real(r_kind),dimension(grd_ens%latlon1n)::st,vp,t,rh,oz,cw
!   real(r_kind),dimension(grd_ens%latlon11)::p,sst
    real(r_kind),dimension(grd_ens%latlon1n)::stbar,vpbar,tbar,rhbar,ozbar,cwbar
    real(r_kind),dimension(grd_ens%latlon11)::pbar,sstbar
    type(gsi_bundle):: bundle_anl,bundle_ens
    type(gsi_grid)  :: grid_anl,grid_ens
    integer(i_kind) i,j,k,n,ii
    integer(i_kind) istatus,ier
    real(r_kind),allocatable:: seed(:,:)
    real(r_kind) sig_norm,bar_norm
    character(len=*),parameter::myname_=trim(myname)//'*load_ensemble'
    character(50) title
    real(r_kind),pointer,dimension(:,:)  :: p,sst
    real(r_kind),pointer,dimension(:,:,:):: t,q,cw,oz,st,vp


    sig_norm=sqrt(one/max(one,n_ens-one))
    bar_norm=one/n_ens
    if(n_ens == 1) bar_norm=zero


    if(generate_ens) then

!                        initialize subdomain to slab routine special_sd2h
       call special_sd2h0
       allocate(seed(nval2f,nscl))
       seed=-one
       stbar=zero ; vpbar=zero ; tbar=zero ; rhbar=zero
       ozbar=zero ; cwbar=zero ; pbar=zero ; sstbar=zero

!      create simple regular grid
        call gsi_gridcreate(grid_anl,grd_ens%lat2,grd_ens%lon2,grd_ens%nsig)
        call gsi_gridcreate(grid_ens,grd_ens%lat2,grd_ens%lon2,grd_ens%nsig)
       do n=1,n_ens
          ! create internal structures w/ the same vars as those in the CV
          ! one at analysis resolution ...
          call gsi_bundlecreate (bundle_anl,grid_anl,'ensemble work',istatus, &
                                 names2d=evars2d,names3d=evars3d)
          if(istatus/=0) then
             write(6,*)trim(myname_),': trouble creating work anl bundle'
             call stop2(999)
          endif

          ! another at ensemble (reduced) resolution ...
          call gsi_bundlecreate (bundle_ens,grid_ens,'ensemble work ens',istatus, &
                                 names2d=evars2d,names3d=evars3d)
          if(istatus/=0) then
             write(6,*)trim(myname_),': trouble creating work ens bundle'
             call stop2(999)
          endif

#ifdef _DEBUG_
          call generate_one_ensemble_perturbation0(bundle_ens,seed)
#else
          call generate_one_ensemble_perturbation(bundle_anl,bundle_ens,seed)
#endif

          ! do some cleanning
          call gsi_bundledestroy(bundle_anl,istatus)
          if(istatus/=0) then
             write(6,*)trim(myname_),': trouble destroying work anl bundle'
             call stop2(999)
          endif

          ! Get required pointers
          ier=0
          call gsi_bundlegetpointer (bundle_ens, evars(1),st, istatus);ier=ier+istatus
          call gsi_bundlegetpointer (bundle_ens, evars(2),vp, istatus);ier=ier+istatus
          call gsi_bundlegetpointer (bundle_ens, evars(3), t, istatus);ier=ier+istatus
          call gsi_bundlegetpointer (bundle_ens, evars(4), q, istatus);ier=ier+istatus
          call gsi_bundlegetpointer (bundle_ens, evars(5),oz, istatus);ier=ier+istatus
          call gsi_bundlegetpointer (bundle_ens, evars(6),cw, istatus);ier=ier+istatus
          call gsi_bundlegetpointer (bundle_ens, evars(7), p, istatus);ier=ier+istatus
          call gsi_bundlegetpointer (bundle_ens, evars(8),sst,istatus);ier=ier+istatus
          if(ier/=0) then
             write(6,*)myname_,': cannot find pointers'
             call stop2(999)
          endif

          ii=0
          do k=1,grd_ens%nsig
             do j=1,grd_ens%lon2
                do i=1,grd_ens%lat2
                   ii=ii+1
                   st_en(ii,n)=st(i,j,k)
                   vp_en(ii,n)=vp(i,j,k)
                   t_en (ii,n)=t (i,j,k)
                   rh_en(ii,n)=q (i,j,k)
                   oz_en(ii,n)=oz(i,j,k)
                   cw_en(ii,n)=cw(i,j,k)
                   stbar(ii)=stbar(ii)+st(i,j,k)
                   vpbar(ii)=vpbar(ii)+vp(i,j,k)
                   tbar (ii)= tbar(ii)+t (i,j,k)
                   rhbar(ii)=rhbar(ii)+q (i,j,k)
                   ozbar(ii)=ozbar(ii)+oz(i,j,k)
                   cwbar(ii)=cwbar(ii)+cw(i,j,k)
                end do
             end do
          end do
          ii=0
          do j=1,grd_ens%lon2
             do i=1,grd_ens%lat2
                ii=ii+1
                p_en  (ii,n)=p(i,j)
                sst_en(ii,n)=sst(i,j)
                pbar  (ii)  =pbar  (ii)+ p  (i,j)
                sstbar(ii)  =sstbar(ii)+ sst(i,j)
             end do
          end do
          call gsi_bundledestroy(bundle_ens,istatus)
          if(istatus/=0) then
             write(6,*)trim(myname_),': trouble destroying work ens bundle'
             call stop2(999)
          endif
       end do
!                          remove mean, which is locally significantly non-zero, due to sample size.
!                           with real ensembles, the mean of the actual sample will be removed.
       do n=1,n_ens
          do i=1,grd_ens%latlon1n
             st_en(i,n)=(st_en(i,n)-stbar(i)*bar_norm)*sig_norm
             vp_en(i,n)=(vp_en(i,n)-vpbar(i)*bar_norm)*sig_norm
             t_en(i,n) =( t_en(i,n)- tbar(i)*bar_norm)*sig_norm
             rh_en(i,n)=(rh_en(i,n)-rhbar(i)*bar_norm)*sig_norm
             oz_en(i,n)=(oz_en(i,n)-ozbar(i)*bar_norm)*sig_norm
             cw_en(i,n)=(cw_en(i,n)-cwbar(i)*bar_norm)*sig_norm
          end do
          do i=1,grd_ens%latlon11
             p_en(i,n)=(p_en(i,n)- pbar(i)*bar_norm)*sig_norm
             sst_en(i,n)=(sst_en(i,n)-sstbar(i)*bar_norm)*sig_norm
          end do
       end do
    
    else
!            read in ensembles
      if (.not.regional) then
        call get_gefs_ensperts_dualres
      else
        if (mype==0) write(6,*) 'READING OF ACTUAL ENSEMBLE PERTS NOT AVAILABLE FOR NON-GFS RUN, PROGRAM STOP!'
        call stop2(999)
      end if

    end if

  end subroutine load_ensemble
     
  subroutine generate_one_ensemble_perturbation(bundle_anl,bundle_ens,seed)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    generate_one_ensemble_perturbation
!
!   prgrmmr:  parrish        org: np22                              date: 2009-09-28
!
! abstract:  compute normalization factor in latitude direction
!
! program history log:
!   2009-09-28  parrish  initial documentation
!   2010-02-20  parrish  modifications for dual resolution
!   2010-03-14  zhu     - make changes using cstate
!   2010-03-15  derber  - fix qvar3d for ensemble
!   2010-04-28  todling - update to use gsi_bundle
!   2010-05-18  parrish  reactivate dual resolution
!   2010-06-04  parrish  multiple fixes for dual resolution
!
!   input argument list:
!     seed     - old random number seeds (used for bit reproducibility of
!                 generated random ensemble perturbations on different 
!                 numbers of processors)
!
!   output argument list:
!     seed     - new random number seeds
!     st       - stream function part of generated ensemble perturbation
!     vp       - velocity potential part of generated ensemble perturbation
!     t        - virtual temperature part of generated ensemble perturbation
!     rh       - relative humidity part of generated ensemble perturbation
!     oz       - ozone part of generated ensemble perturbation
!     cw       - cloud water part of generated ensemble perturbation
!     p        - surface pressure part of generated ensemble perturbation
!     sst      - skin temperature part of generated ensemble perturbation
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

    use kinds, only: r_kind,i_kind,i_llong
    use gridmod, only: vlevs,nnnn1o,regional
    use mpimod, only: mype,mpi_rtype,mpi_comm_world,ierror,nvar_pe
    use hybrid_ensemble_parameters, only: uv_hyb_ens,grd_ens,grd_anl,p_e2a
    use general_sub2grid_mod, only: general_suba2sube_r_double
    use constants, only: zero,one
    implicit none

    character(len=*),parameter::myname_=myname//'*generate_one_ensemble_perturbation'
    real(r_kind)    ,intent(inout) :: seed(nval2f,nscl)
    type(gsi_bundle),intent(inout) :: bundle_anl,bundle_ens

    real(r_kind),dimension(nval2f,nnnn1o,nscl):: z
    real(r_kind) vert1(vlevs)
    integer(i_llong) iseed
    integer(i_kind) nvert,i,j,ii,is,naux,k
    integer(i_kind) ist,ivp,it,irh,ioz,icw,ip,isst
    integer(i_kind) ipc(2),istatus
    real(r_kind) aux
    real(r_kind),dimension(nh_0:nh_1,vlevs,nscl):: zsub
    real(r_kind),dimension(:,:,:),allocatable:: ua,va
    real(r_kind),pointer,dimension(:,:,:):: st,vp

    naux=0
    nvert=vlevs
    if(maxval(seed) <  zero) then

!       create initial seed for random numbers for each horizontal location.
  
       if(mype == 0) then
          call random_number(seed)
          do is=1,nscl
             do i=1,nval2f
                iseed=1+nint(seed(i,is)*2147483000._r_kind)
                seed(i,is)=iseed
             end do
          end do
       end if
       call mpi_bcast(seed,nval2f*nscl,mpi_rtype,0,mpi_comm_world,ierror)

    end if

    do is=1,nscl
       do i=nh_0,nh_1
#ifdef ibm_sp
          call dnrand(seed(i,is),nvert,vert1,aux,naux)
#else
! Generate a Vector of Normally Distributed Random Numbers (function from Lapack lib)
          call dlarnv(3,seed(i,is),nvert,vert1)
#endif
          do k=1,nvert
             zsub(i,k,is)=vert1(k)
          end do
       end do
    end do
    call special_sd2h(zsub,z)

!     if this is a global run, then need to fix tropical belt part of z so periodic overlap is correct
    if(.not.regional) call fix_belt(z)

    call ckgcov(z,bundle_anl,nnnn1o)

!     if uv_hyb_ens=.true., then convert st,vp to u,v
    if(uv_hyb_ens) then
       allocate(ua(grd_anl%lat2,grd_anl%lon2,grd_anl%nsig))
       allocate(va(grd_anl%lat2,grd_anl%lon2,grd_anl%nsig))
       call gsi_bundlegetpointer (bundle_anl, evars(1),st, istatus)
       call gsi_bundlegetpointer (bundle_anl, evars(2),vp, istatus)
       if(istatus/=0) then
          write(6,*) myname_,': error getting sf/vp pointers, aborting ...'
          call stop2(999)
       endif
       call getuv(ua,va,st,vp,0)
       st=ua
       vp=va
       deallocate(va)
       deallocate(ua)
    end if
    if(grd_anl%latlon11 == grd_ens%latlon11) then
       bundle_ens%values=bundle_anl%values
    else
       call general_suba2sube_r_double(grd_anl,grd_ens,p_e2a,bundle_anl%values,bundle_ens%values,regional)
    end if


  end subroutine generate_one_ensemble_perturbation

  subroutine generate_one_ensemble_perturbation0(bundle,seed)  ! for debug only
! RTodling: this is for debug only - something's not correct in ckgcov (not
! possible that it works in the context above ...
  use mpimod,only:mype
  use constants,only:two,one
  implicit none 
  type(gsi_bundle),intent(inout):: bundle
  real(r_kind)    ,intent(inout) :: seed(nval2f,nscl)
  real(r_kind),allocatable,dimension(:)   :: zz
  integer(i_kind) jj,ii,iseed
  integer(i_kind),allocatable:: nseed(:)

print*, 'Hack random pert'
iseed=12345
call random_seed(size=jj)
allocate(nseed(jj))
nseed(1:jj)=iseed
! The following because we don't want all procs to get
! exactly the same sequence (which would be repeated in
! the then not so random vector) but it makes the test
! not reproducible if the number of procs is changed.
nseed(1)=iseed+mype
call random_seed(put=nseed)
deallocate(nseed)
allocate(zz(bundle%ndim))
call random_number(zz)
do ii=1,bundle%ndim
   bundle%values(ii) = two*zz(ii)-one
enddo
deallocate(zz)

  end subroutine generate_one_ensemble_perturbation0

  subroutine fix_belt(z)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    fix_belt
!
!   prgrmmr:  parrish        org: np22                              date: 2009-09-28
!
! abstract: when generating random vector representation of control variable z for
!            global case, need to make adjustment to tropical belt part to
!            properly account for periodicity in overlap zone
!
! program history log:
!   2009-09-28  parrish  initial documentation
!   2010-02-20  parrish  modifications for dual resolution
!
!   input argument list:
!     z        - field to be adjusted
!
!   output argument list:
!     z        - adjusted field
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block


    use kinds, only: r_kind,i_kind
    use gridmod, only: nnnn1o
    use berror, only: nx,ny,nf
    use hybrid_ensemble_parameters, only: grd_ens
    implicit none

    real(r_kind),intent(inout) :: z(nval2f,nnnn1o,nscl)

    real(r_kind) zloc1(ny,nx)
    integer(i_kind) i,ii,j,jj,k

    do j=1,nscl
       do k=1,nnnn1o
          i=0
          do jj=1,nx
             do ii=1,ny
                i=i+1
                zloc1(ii,jj)=z(i,k,j)
             end do
          end do
          do jj=grd_ens%nlon+1,nx
             do ii=1,ny
                zloc1(ii,jj)=zloc1(ii,jj-grd_ens%nlon)
             end do
          end do
          i=0
          do jj=1,nx
             do ii=1,ny
                i=i+1
                z(i,k,j)=zloc1(ii,jj)
             end do
          end do
       end do
    end do
    
  end subroutine fix_belt

  subroutine rescale_ensemble_rh_perturbations
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    rescale_ensemble_rh_perturbations
!   prgmmr: parrish          org: np22                date: 2009-10-15
!
! abstract: rescale internally generated ensemble rh perturbations
!
! program history log:
!   2009-10-15  parrish
!   2010-02-20  parrish  modifications for dual resolution
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
    use kinds, only: r_kind,i_kind
    use gridmod, only: regional
    use hybrid_ensemble_parameters, only: n_ens,grd_ens,grd_anl,grd_a1,grd_e1,p_e2a
    use general_sub2grid_mod, only: general_suba2sube_r_double
    use berror, only: qvar3d
    implicit none

    integer(i_kind) i,ii,j,k,n
    real(r_kind) qvar3d_ens(grd_ens%lat2,grd_ens%lon2,grd_ens%nsig)

    if(grd_anl%latlon11 == grd_ens%latlon11) then
       qvar3d_ens=qvar3d
    else
       call general_suba2sube_r_double(grd_a1,grd_e1,p_e2a,qvar3d,qvar3d_ens,regional)
    end if
    do n=1,n_ens
       ii=0
       do k=1,grd_ens%nsig
          do j=1,grd_ens%lon2
             do i=1,grd_ens%lat2
                ii=ii+1
                rh_en(ii,n)=qvar3d_ens(i,j,k)*rh_en(ii,n)
             end do
          end do
       end do
    end do

  end subroutine rescale_ensemble_rh_perturbations

  subroutine destroy_ensemble
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    destroy_ensemble       deallocate space for ensembles
!   prgmmr: parrish          org: np22                date: 2009-06-16
!
! abstract: deallocate space for ensemble perturbations used with the 
!             hybrid ensemble option.
!
! program history log:
!   2009-06-16  parrish
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
    use hybrid_ensemble_parameters, only: l_hyb_ens
    implicit none

    if(l_hyb_ens) then
       deallocate(st_en,vp_en,t_en,rh_en,oz_en,cw_en,p_en,sst_en)
    end if

  end subroutine destroy_ensemble

  subroutine ensemble_forward_model(cvec,a_en)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    ensemble_forward_model  add ensemble part to anl vars
!   prgmmr: parrish          org: np22                date: 2009-06-16
!
! abstract: For the hybrid ensemble method, add ensemble contribution
!             to standard analysis control variables.  (This follows,
!             method outlined in Wang et al, MWR, 2008).

! program history log:
!   2009-09-11  parrish
!   2010-02-20  parrish  modifications for dual resolution
!   2010-03-23  zhu - use cstate
!   2010-05-07  parrish - remove error stop for dual resolution
!   2010-04-28  todling - update to use gsi_bundle
!
!   input argument list:
!     st       - stream function input control variable
!     vp       - velocity potential input control variable
!     t        - virtual temperatuare input control variable
!     rh       - relative humidity input control variable
!     oz       - ozone input control variable
!     cw       - cloud water input control variable
!     p        - surface pressure input control variable
!     sst      - skin temperature input control variable
!     a_en     - hybrid ensemble amplitude control variable
!
!   output argument list:
!     st       - stream function output control variable
!     vp       - velocity potential output control variable
!     t        - virtual temperatuare output control variable
!     rh       - relative humidity output control variable
!     oz       - ozone output control variable
!     cw       - cloud water output control variable
!     p        - surface pressure output control variable
!     sst      - skin temperature output control variable
!
! remarks:  
!    need to reconcile grid in gsi_bundle w/ grid_ens/grid_anl
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

    use hybrid_ensemble_parameters, only: n_ens,grd_ens,grd_anl
    use constants, only: zero

    implicit none
    type(gsi_bundle),intent(inout) :: cvec
    type(gsi_bundle),intent(in)    :: a_en(:)

    character(len=*),parameter::myname_=trim(myname)//'*ensemble_forward_model'
    logical nogood
    integer(i_kind) i,j,k,n,ii
    integer(i_kind) ipc(nevs),ipe(1),istatus

!   Check resolution consistency between static and ensemble components
    nogood=.not.(cvec%grid%im==a_en(1)%grid%im.and.&
                 cvec%grid%jm==a_en(1)%grid%jm.and.&
                 cvec%grid%km==a_en(1)%grid%km)
    if (nogood) then
       write(6,*) myname_,': static&ensemble vectors have inconsistent dims'
       call stop2(999)
    endif

!   Request ensemble-corresponding fields from control vector
    call gsi_bundlegetpointer (cvec,evars,ipc,istatus)
    if(istatus/=0) then
      write(6,*) myname_,': cannot find pointers'
      call stop2(999)
    endif
 
    ipe(1)=1
    do n=1,n_ens
       ii=0
       do k=1,grd_ens%nsig
          do j=1,grd_ens%lon2
             do i=1,grd_ens%lat2
                ii=ii+1
                cvec%r3(ipc(1))%q(i,j,k)=cvec%r3(ipc(1))%q(i,j,k)+a_en(n)%r3(ipe(1))%q(i,j,k)*st_en(ii,n)
                cvec%r3(ipc(2))%q(i,j,k)=cvec%r3(ipc(2))%q(i,j,k)+a_en(n)%r3(ipe(1))%q(i,j,k)*vp_en(ii,n)
                cvec%r3(ipc(3))%q(i,j,k)=cvec%r3(ipc(3))%q(i,j,k)+a_en(n)%r3(ipe(1))%q(i,j,k)* t_en(ii,n)
                cvec%r3(ipc(4))%q(i,j,k)=cvec%r3(ipc(4))%q(i,j,k)+a_en(n)%r3(ipe(1))%q(i,j,k)*rh_en(ii,n)
                cvec%r3(ipc(5))%q(i,j,k)=cvec%r3(ipc(5))%q(i,j,k)+a_en(n)%r3(ipe(1))%q(i,j,k)*oz_en(ii,n)
                cvec%r3(ipc(6))%q(i,j,k)=cvec%r3(ipc(6))%q(i,j,k)+a_en(n)%r3(ipe(1))%q(i,j,k)*cw_en(ii,n)
             end do
          end do
       end do
       ii=0
       do j=1,grd_ens%lon2
          do i=1,grd_ens%lat2
             ii=ii+1
             cvec%r2(ipc(7))%q(i,j)=cvec%r2(ipc(7))%q(i,j)+a_en(n)%r3(ipe(1))%q(i,j,1)*  p_en(ii,n)
             cvec%r2(ipc(8))%q(i,j)=cvec%r2(ipc(8))%q(i,j)+a_en(n)%r3(ipe(1))%q(i,j,1)*sst_en(ii,n)
          end do
       end do
    end do

  end subroutine ensemble_forward_model

  subroutine ensemble_forward_model_dual_res(cvec,a_en)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    ensemble_forward_model_dual_res  use for dualres option
!   prgmmr: parrish          org: np22                date: 2010-02-20
!
! abstract: Copy of ensemble_forward_model for use with dual resolution.

! program history log:
!   2010-02-20  parrish
!   2010-03-23  zhu - use cstate
!   2010-04-06  parrish - add deallocate(sube_vars)
!   2010-04-28  todling - update to use gsi_bundle
!   2010-05-07  parrish - remove error stop for dual resolution, and add cstate again
!                         to analysis variables after interpolation from
!                         ensemble grid.
!                         the ensemble part is still not updated for
!                         generalized control variable
!   2010-05-18  todling - revisited bundle usage in light of Dave's change (2010-05-07)
!
!   input argument list:
!     st       - stream function input control variable
!     vp       - velocity potential input control variable
!     t        - virtual temperatuare input control variable
!     rh       - relative humidity input control variable
!     oz       - ozone input control variable
!     cw       - cloud water input control variable
!     p        - surface pressure input control variable
!     sst      - skin temperature input control variable
!     a_en     - hybrid ensemble amplitude control variable
!
!   output argument list:
!     st       - stream function output control variable
!     vp       - velocity potential output control variable
!     t        - virtual temperatuare output control variable
!     rh       - relative humidity output control variable
!     oz       - ozone output control variable
!     cw       - cloud water output control variable
!     p        - surface pressure output control variable
!     sst      - skin temperature output control variable
!
! remarks:  
!    need to reconcile grid in gsi_bundle w/ grid_ens/grid_anl
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

    use hybrid_ensemble_parameters, only: n_ens,grd_ens,grd_anl,p_e2a,uv_hyb_ens
    use general_sub2grid_mod, only: general_sube2suba_r_double  
    use gridmod,only: regional
    use constants, only: zero

    implicit none
    type(gsi_bundle),intent(inout) :: cvec
    type(gsi_bundle),intent(in)    :: a_en(:)

    character(len=*),parameter::myname_=trim(myname)//'*ensemble_forward_model_dual_res'
    integer(i_kind) i,ii,jj,j,k,n,iv,im,jm,km
    real(r_kind),allocatable:: sube_vars(:),suba_vars(:)
    integer(i_kind) ipc(nevs),ipe(1),istatus

!   Request ensemble-corresponding fields from control vector
    call gsi_bundlegetpointer (cvec,evars,ipc,istatus)
    if(istatus/=0) then
      write(6,*) myname_,': cannot find pointers'
      call stop2(999)
    endif

    ipe(1)=1
    allocate(sube_vars(grd_ens%latlon11*grd_ens%num_fields))
    sube_vars=zero
    im=a_en(1)%grid%im
    jm=a_en(1)%grid%jm
    km=a_en(1)%grid%km
    do n=1,n_ens
       ii=0
       jj=0
       do k=1,km
          do j=1,jm
             do i=1,im
                ii=ii+1; jj=jj+1
                sube_vars(ii)=sube_vars(ii)+a_en(n)%r3(ipe(1))%q(i,j,k)*st_en(jj,n)
             end do
          end do
       end do
       jj=0
       do k=1,km
          do j=1,jm
             do i=1,im
                ii=ii+1; jj=jj+1
                sube_vars(ii)=sube_vars(ii)+a_en(n)%r3(ipe(1))%q(i,j,k)*vp_en(jj,n)
             end do
          end do
       end do
       jj=0
       do k=1,km
          do j=1,jm
             do i=1,im
                ii=ii+1; jj=jj+1
                sube_vars(ii)=sube_vars(ii)+a_en(n)%r3(ipe(1))%q(i,j,k)*t_en(jj,n)
             end do
          end do
       end do
       jj=0
       do k=1,km
          do j=1,jm
             do i=1,im
                ii=ii+1; jj=jj+1
                sube_vars(ii)=sube_vars(ii)+a_en(n)%r3(ipe(1))%q(i,j,k)*rh_en(jj,n)
             end do
          end do
       end do
       jj=0
       do k=1,km
          do j=1,jm
             do i=1,im
                ii=ii+1; jj=jj+1
                sube_vars(ii)=sube_vars(ii)+a_en(n)%r3(ipe(1))%q(i,j,k)*oz_en(jj,n)
             end do
          end do
       end do
       jj=0
       do k=1,km
          do j=1,jm
             do i=1,im
                ii=ii+1; jj=jj+1
                sube_vars(ii)=sube_vars(ii)+a_en(n)%r3(ipe(1))%q(i,j,k)*cw_en(jj,n)
             end do
          end do
       end do
       jj=0
       do j=1,jm
          do i=1,im
             ii=ii+1; jj=jj+1
             sube_vars(ii)=sube_vars(ii)+a_en(n)%r3(ipe(1))%q(i,j,1)*p_en(jj,n)
          end do
       end do
       jj=0
       do j=1,jm
          do i=1,im
             ii=ii+1; jj=jj+1
             sube_vars(ii)=sube_vars(ii)+a_en(n)%r3(ipe(1))%q(i,j,1)*sst_en(jj,n)
          end do
       end do
    end do
    allocate(suba_vars(grd_anl%latlon11*grd_anl%num_fields))
    call general_sube2suba_r_double(grd_ens,grd_anl,p_e2a,sube_vars,suba_vars,regional)
    deallocate(sube_vars)
    im=cvec%grid%im
    jm=cvec%grid%jm
    km=cvec%grid%km
! since suba_vars is ordered in the same way as sube_vars 
! (from choice of pointers made locally above)
! we can now simple loop over all locally defined 3d and 2d variables 
    ii=0
    do iv=1,ne3d
       do k=1,km
          do j=1,jm
             do i=1,im
                ii=ii+1
                cvec%r3(ipc(iv))%q(i,j,k)=cvec%r3(ipc(iv))%q(i,j,k)+suba_vars(ii)
             end do
          end do
       end do
    end do
    do iv=ne3d+1,nevs  ! as organized above, all 2d-variables are at the bottom
       do j=1,jm
          do i=1,im
             ii=ii+1
             cvec%r2(ipc(iv))%q(i,j)=cvec%r2(ipc(iv))%q(i,j)+suba_vars(ii)
          end do
       end do
    end do

    deallocate(suba_vars)

  end subroutine ensemble_forward_model_dual_res

  subroutine ensemble_forward_model_ad(cvec,a_en)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    ensemble_forward_model  add ensemble part to anl vars
!   prgmmr: parrish          org: np22                date: 2009-06-16
!
! abstract: For the hybrid ensemble method, add ensemble contribution
!             to standard analysis control variables.  (This follows,
!             method outlined in Wang et al, MWR, 2008).

! program history log:
!   2009-09-11  parrish
!   2010-02-20  parrish - adapt for dual resolution
!   2010-03-23  zhu - use cstate
!   2010-04-28  todling - update to use gsi_bundle
!
!   input argument list:
!     st       - stream function input control variable
!     vp       - velocity potential input control variable
!     t        - virtual temperatuare input control variable
!     rh       - relative humidity input control variable
!     oz       - ozone input control variable
!     cw       - cloud water input control variable
!     p        - surface pressure input control variable
!     sst      - skin temperature input control variable
!     a_en     - hybrid ensemble amplitude control variable
!
!   output argument list:
!     st       - stream function output control variable
!     vp       - velocity potential output control variable
!     t        - virtual temperatuare output control variable
!     rh       - relative humidity output control variable
!     oz       - ozone output control variable
!     cw       - cloud water output control variable
!     p        - surface pressure output control variable
!     sst      - skin temperature output control variable
!
! remarks:  
!    need to reconcile grid in gsi_bundle w/ grid_ens/grid_anl
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

    use hybrid_ensemble_parameters, only: n_ens,grd_ens,grd_anl
    implicit none

    type(gsi_bundle),intent(inout) :: cvec
    type(gsi_bundle),intent(inout) :: a_en(:)

    character(len=*),parameter::myname_=trim(myname)//'*ensemble_forward_model_ad'
    logical nogood
    integer(i_kind) i,j,k,n,ii
    integer(i_kind) ipc(nevs),ipe(1),istatus

!   Check resolution consistency between static and ensemble components
    nogood=.not.(cvec%grid%im==a_en(1)%grid%im.and.&
                 cvec%grid%jm==a_en(1)%grid%jm.and.&
                 cvec%grid%km==a_en(1)%grid%km)
    if (nogood) then
       write(6,*) myname_,': static/ensemble vectors have inconsistent dims'
       call stop2(999)
    endif

!   Request ensemble-corresponding fields from control vector
    call gsi_bundlegetpointer (cvec,evars,ipc,istatus)
    if(istatus/=0) then
      write(6,*) myname_,': cannot find pointers'
      call stop2(999)
    endif
    ipe(1)=1

    do n=1,n_ens
       ii=0
       do k=1,grd_ens%nsig
          do j=1,grd_ens%lon2
             do i=1,grd_ens%lat2
                ii=ii+1
                a_en(n)%r3(ipe(1))%q(i,j,k)=a_en(n)%r3(ipe(1))%q(i,j,k)+cvec%r3(ipc(1))%q(i,j,k)*st_en(ii,n)
                a_en(n)%r3(ipe(1))%q(i,j,k)=a_en(n)%r3(ipe(1))%q(i,j,k)+cvec%r3(ipc(2))%q(i,j,k)*vp_en(ii,n)
                a_en(n)%r3(ipe(1))%q(i,j,k)=a_en(n)%r3(ipe(1))%q(i,j,k)+cvec%r3(ipc(3))%q(i,j,k)* t_en(ii,n)
                a_en(n)%r3(ipe(1))%q(i,j,k)=a_en(n)%r3(ipe(1))%q(i,j,k)+cvec%r3(ipc(4))%q(i,j,k)*rh_en(ii,n)
                a_en(n)%r3(ipe(1))%q(i,j,k)=a_en(n)%r3(ipe(1))%q(i,j,k)+cvec%r3(ipc(5))%q(i,j,k)*oz_en(ii,n)
                a_en(n)%r3(ipe(1))%q(i,j,k)=a_en(n)%r3(ipe(1))%q(i,j,k)+cvec%r3(ipc(6))%q(i,j,k)*cw_en(ii,n)
             end do
          end do
       end do
       ii=0
       do j=1,grd_ens%lon2
          do i=1,grd_ens%lat2
             ii=ii+1
             a_en(n)%r3(ipe(1))%q(i,j,1)=a_en(n)%r3(ipe(1))%q(i,j,1)+cvec%r2(ipc(7))%q(i,j)*p_en(ii,n)
             a_en(n)%r3(ipe(1))%q(i,j,1)=a_en(n)%r3(ipe(1))%q(i,j,1)+cvec%r2(ipc(8))%q(i,j)*sst_en(ii,n)
          end do
       end do
    end do

  end subroutine ensemble_forward_model_ad

  subroutine ensemble_forward_model_ad_dual_res(cvec,a_en)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    ensemble_forward_model_ad_dual_res  use for dualres option
!   prgmmr: parrish          org: np22                date: 2010-02-20
!
! abstract: Copy of ensemble_forward_model_ad for use with dual resolution.

! program history log:
!   2010-02-20  parrish
!   2010-03-23  zhu - use cstate
!   2010-04-06  parrish - correct dimensions of st,vp,t,rh,oz,cw,p,sst. add deallocate(suba_vars)
!   2010-04-28  todling - update to use gsi_bundle
!   2010-05-07  parrish - remove error stop for dual resolution, and add cstate again
!                           to analysis variables before adjoint interpolation
!                           to ensemble grid.
!                           the ensemble part is still not updated for
!                           generalized control variable
!   2010-05-18  todling - revisited bundle usage in light of Dave's change (2010-05-07)
!
!   input argument list:
!     st       - stream function input control variable
!     vp       - velocity potential input control variable
!     t        - virtual temperatuare input control variable
!     rh       - relative humidity input control variable
!     oz       - ozone input control variable
!     cw       - cloud water input control variable
!     p        - surface pressure input control variable
!     sst      - skin temperature input control variable
!     a_en     - hybrid ensemble amplitude control variable
!
!   output argument list:
!     st       - stream function output control variable
!     vp       - velocity potential output control variable
!     t        - virtual temperatuare output control variable
!     rh       - relative humidity output control variable
!     oz       - ozone output control variable
!     cw       - cloud water output control variable
!     p        - surface pressure output control variable
!     sst      - skin temperature output control variable
!
! remarks:  
!    need to reconcile grid in gsi_bundle w/ grid_ens/grid_anl
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

    use hybrid_ensemble_parameters, only: n_ens,grd_ens,grd_anl,p_e2a,uv_hyb_ens
    use general_sub2grid_mod, only: general_sube2suba_r_double_ad
    use gridmod,only: regional
    use constants, only: zero
    implicit none

    type(gsi_bundle),intent(inout) :: cvec
    type(gsi_bundle),intent(inout) :: a_en(:)

    character(len=*),parameter::myname_=trim(myname)//'*ensemble_forward_model_ad_dual_res'
    integer(i_kind) i,ii,jj,j,k,n,iv,im,jm,km
    real(r_kind),allocatable:: sube_vars(:),suba_vars(:)
    integer(i_kind) ipc(nevs),ipe(1),istatus

!   Request ensemble-corresponding fields from control vector
    call gsi_bundlegetpointer (cvec,evars,ipc,istatus)
    if(istatus/=0) then
      write(6,*) myname_,': cannot find pointers'
      call stop2(999)
    endif
    ipe(1)=1

    allocate(suba_vars(grd_anl%latlon11*grd_anl%num_fields))

    im=cvec%grid%im
    jm=cvec%grid%jm
    km=cvec%grid%km
    ii=0
    do iv=1,ne3d
       do k=1,km
          do j=1,jm
             do i=1,im
                ii=ii+1
                suba_vars(ii)=cvec%r3(ipc(iv))%q(i,j,k)
             end do
          end do
       end do
    end do
    do iv=ne3d+1,nevs  ! as organized above, all 2d-variables are at the bottom
       do j=1,jm
          do i=1,im
             ii=ii+1
             suba_vars(ii)=cvec%r2(ipc(iv))%q(i,j)
          end do
       end do
    end do

    allocate(sube_vars(grd_ens%latlon11*grd_ens%num_fields))
    call general_sube2suba_r_double_ad(grd_ens,grd_anl,p_e2a,sube_vars,suba_vars,regional)
    deallocate(suba_vars)
    im=a_en(1)%grid%im
    jm=a_en(1)%grid%jm
    km=a_en(1)%grid%km
    do n=1,n_ens
       ii=0
       jj=0
       do k=1,km
          do j=1,jm
             do i=1,im
                ii=ii+1; jj=jj+1
                a_en(n)%r3(ipe(1))%q(i,j,k)=a_en(n)%r3(ipe(1))%q(i,j,k)+sube_vars(ii)*st_en(jj,n)
             end do
          end do
       end do
       jj=0
       do k=1,km
          do j=1,jm
             do i=1,im
                ii=ii+1; jj=jj+1
                a_en(n)%r3(ipe(1))%q(i,j,k)=a_en(n)%r3(ipe(1))%q(i,j,k)+sube_vars(ii)*vp_en(jj,n)
             end do
          end do
       end do
       jj=0
       do k=1,km
          do j=1,jm
             do i=1,im
             ii=ii+1; jj=jj+1
             a_en(n)%r3(ipe(1))%q(i,j,k)=a_en(n)%r3(ipe(1))%q(i,j,k)+sube_vars(ii)*t_en(jj,n)
             end do
          end do
       end do
       jj=0
       do k=1,km
          do j=1,jm
             do i=1,im
             ii=ii+1; jj=jj+1
             a_en(n)%r3(ipe(1))%q(i,j,k)=a_en(n)%r3(ipe(1))%q(i,j,k)+sube_vars(ii)*rh_en(jj,n)
             end do
          end do
       end do
       jj=0
       do k=1,km
          do j=1,jm
             do i=1,im
             ii=ii+1; jj=jj+1
             a_en(n)%r3(ipe(1))%q(i,j,k)=a_en(n)%r3(ipe(1))%q(i,j,k)+sube_vars(ii)*oz_en(jj,n)
             end do
          end do
       end do
       jj=0
       do k=1,km
          do j=1,jm
             do i=1,im
             ii=ii+1; jj=jj+1
             a_en(n)%r3(ipe(1))%q(i,j,k)=a_en(n)%r3(ipe(1))%q(i,j,k)+sube_vars(ii)*cw_en(jj,n)
             end do
          end do
       end do
       jj=0
       do j=1,jm
          do i=1,im
            ii=ii+1; jj=jj+1
            a_en(n)%r3(ipe(1))%q(i,j,1)=a_en(n)%r3(ipe(1))%q(i,j,1)+sube_vars(ii)*p_en(jj,n)
         end do
       end do
       jj=0
       do j=1,jm
          do i=1,im
             ii=ii+1; jj=jj+1
             a_en(n)%r3(ipe(1))%q(i,j,1)=a_en(n)%r3(ipe(1))%q(i,j,1)+sube_vars(ii)*sst_en(jj,n)
          end do
       end do
    end do
    deallocate(sube_vars)


  end subroutine ensemble_forward_model_ad_dual_res

  subroutine special_sd2h0
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    special_sd2h0  initialize subroutine special_sd2h
!   prgmmr: parrish          org: np22                date: 2009-06-16
!
! abstract: initialize subroutine special_sd2h (subdomain to slab for  
!             variable a_en).
!
! program history log:
!   2009-06-16  parrish
!   2010-02-10  parrish, correct allocate error on ndrecv_sd2h, found by Arthur Mizzi.
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

    use kinds, only: r_kind,i_kind
    use mpimod, only: npe,mype,mpi_comm_world,ierror,mpi_rtype
    use gridmod, only: nlat,nlon,nsig,nnnn1o,regional,vlevs
    use berror, only: nx,ny,nf
    implicit none

    integer(i_kind),dimension(0:npe-1):: nh_0_all,nh_1_all,nv_0_all,nv_1_all
    integer(i_kind) nvert,nh_tot,nh_this,nn,nv_tot,nv_this,kchk,n,kk,i,k
    real(r_kind),allocatable:: zsub(:,:),z(:)

!    set nval2f (loosely paraphrased from jfunc.f90)

    nscl=3          !  hard-wired here, later generalize when generalizing control variables
    if(regional) then
       nval2f=nlat*nlon
    else
       nval2f=ny*nx + 2*(2*nf+1)*(2*nf+1)
    end if


    allocate(nsend_sd2h(0:npe-1),ndsend_sd2h(0:npe),nrecv_sd2h(0:npe-1),ndrecv_sd2h(0:npe))
    allocate(i_recv(nval2f*nnnn1o),k_recv(nval2f*nnnn1o))
    nvert=vlevs

!  compute nv_0,nv_1

    nv_tot=nvert
    nv_this=nv_tot/npe
    if(mod(nv_tot,npe)/=0) nv_this=nv_this+1
    if(mod(nv_tot,npe)==0) then
       kchk=npe
    else
       kchk=mod(nv_tot,npe)
    end if

    nv_0_all=-1
    nv_1_all=-2
    nn=0
    do n=1,npe
       if(n<=kchk) then
          kk=nv_this
       else
          kk=nv_this-1
       end if
       if(kk>0) then
          nv_0_all(n-1)=nn+1
          nv_1_all(n-1)=nn+kk
       end if
       nn=nn+kk
    end do
    nv_0=nv_0_all(mype)
    nv_1=nv_1_all(mype)

!     compute nh_0, nh_1

    nh_tot=nval2f
    nh_this=nh_tot/npe
    if(mod(nh_tot,npe)/=0) nh_this=nh_this+1
    if(mod(nh_tot,npe)==0) then
       kchk=npe
    else
       kchk=mod(nh_tot,npe)
    end if

    nh_0_all=-1
    nh_1_all=-2
    nn=0
    do n=1,npe
       if(n<=kchk) then
          kk=nh_this
       else
          kk=nh_this-1
       end if
       if(kk>0) then
          nh_0_all(n-1)=nn+1
          nh_1_all(n-1)=nn+kk
       end if
       nn=nn+kk
    end do
    nh_0=nh_0_all(mype)
    nh_1=nh_1_all(mype)

!   compute nsend_sd2h,ndsend_sd2h,nrecv_sd2h,ndrecv_sd2h

    ndsend_sd2h(0)=0
    ndrecv_sd2h(0)=0
    do n=0,npe-1
       nsend_sd2h(n)=max(0,(nv_1_all(n)-nv_0_all(n)+1)*(nh_1-nh_0+1))
       ndsend_sd2h(n+1)=ndsend_sd2h(n)+nsend_sd2h(n)
       nrecv_sd2h(n)=max(0,(nv_1-nv_0+1)*(nh_1_all(n)-nh_0_all(n)+1))
       ndrecv_sd2h(n+1)=ndrecv_sd2h(n)+nrecv_sd2h(n)
    end do
    allocate(zsub(nh_0:nh_1,nvert),z(nval2f*(nv_1-nv_0+1)))
    do k=1,nvert
       do i=nh_0,nh_1
          zsub(i,k)=i
       end do
    end do
    call mpi_alltoallv(zsub,nsend_sd2h,ndsend_sd2h,mpi_rtype,&
                       z,nrecv_sd2h,ndrecv_sd2h,mpi_rtype,mpi_comm_world,ierror)
    do i=1,nval2f*(nv_1-nv_0+1)
       i_recv(i)=nint(z(i))
    end do

    do k=1,nvert
       do i=nh_0,nh_1
          zsub(i,k)=k
       end do
    end do
    call mpi_alltoallv(zsub,nsend_sd2h,ndsend_sd2h,mpi_rtype,&
                       z,nrecv_sd2h,ndrecv_sd2h,mpi_rtype,mpi_comm_world,ierror)
    do i=1,nval2f*(nv_1-nv_0+1)
       k_recv(i)=nint(z(i))
    end do

    deallocate(zsub,z)

  end subroutine special_sd2h0

  subroutine special_sd2h(zsub,z)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    special_sd2h  subdomain to slab for variable a_en
!   prgmmr: parrish          org: np22                date: 2009-06-16
!
! abstract: subdomain to slab for variable a_en.
!
! program history log:
!   2009-06-16  parrish
!
!   input argument list:
!     zsub     - input array on "subdomains"
!
!   output argument list:
!     z        - output array on slabs (form expected for input argument to ckgcov)
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use kinds, only: r_kind,i_kind
  use gridmod, only: nnnn1o,nsig,vlevs
  use constants, only: zero
  use mpimod, only: mype,mpi_rtype,ierror,mpi_comm_world
  implicit none

  real(r_kind),dimension(nh_0:nh_1,vlevs,nscl),intent(in   ) :: zsub
  real(r_kind),dimension(nval2f,nv_0:nv_1,nscl)         ,intent(  out) :: z

  real(r_kind) zsub1(nh_0:nh_1,vlevs),work(nval2f*(nv_1-nv_0+1))
  integer(i_kind) i,ii,is,k
! integer(i_kind) ibadp,ibadm,kbadp,kbadm
! logical good

!      1 <= nh_0 <= nh_1 <= nval2f

!      1 <= nv_0 <= nv_1 <= vlevs

  z=zero
  do is=1,nscl
     do k=1,vlevs
        do i=nh_0,nh_1
           zsub1(i,k)=zsub(i,k,is)
        end do
     end do
     call mpi_alltoallv(zsub1,nsend_sd2h,ndsend_sd2h,mpi_rtype,&
                        work,nrecv_sd2h,ndrecv_sd2h,mpi_rtype,mpi_comm_world,ierror)
     do ii=1,nval2f*(nv_1-nv_0+1)
        i=i_recv(ii) ; k=k_recv(ii)
        z(i,k,is)=work(ii)
     end do
  end do
  
end subroutine special_sd2h

subroutine beta12mult(grady)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    beta12mult  multiply grady by beta1inv and beta2inv        
!   prgmmr: parrish          org: np22                date: 2009-09-17
!
! abstract: when the hybrid ensemble option is turned on (l_hyb_ens=.true.)
!            the gradient vector grady contains two parts: the first is
!            the gradient with respect to the control variable associated
!            with the static background error covariance; the second is the
!            gradient with respect to the new ensemble control vector a_en.
!            the first is multiplied by beta1_inv, and the second by beta2_inv
!            where beta1_inv+beta2_inv=1.
!            adjusting beta1_inv between 0 and 1 allows tuning for optimal
!            blend between information provided by static background B and
!            ensemble based background.  beta1_inv=1 gives full weight to B
!            and betainv=0 gives full weight to ensemble.
!
! program history log:
!   2009-10-12  parrish  initial documentation
!   2010-03-29  kleist   comment out beta1_inv for SST
!   2010-04-28  todling  update to use gsi_bundle
!
!   input argument list:
!     grady    - input field  grady_x1 : grady_a_en
!
!   output
!     grady    - beta1_inv*grady_x1 : beta2_inv*grady_a_en
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  use kinds, only: r_kind,i_kind
  use gsi_4dvar, only: nsubwin
  use hybrid_ensemble_parameters, only: beta1_inv,n_ens
  use constants, only:  one
  use control_vectors
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use timermod, only: timer_ini,timer_fnl
  implicit none

! Declare passed variables
  type(control_vector),intent(inout) :: grady

! Declare local variables
  character(len=*),parameter::myname_=myname//'*beta12mult'
  integer(i_kind) ii,jj,nn
  real(r_kind) beta2_inv
  integer(i_kind) ipc(nevs),istatus

! Initialize timer
  call timer_ini('beta12mult')

  beta2_inv=one-beta1_inv

! Request CV pointers to vars pertinent to ensemble
  call gsi_bundlegetpointer ( grady%step(1), evars, ipc, istatus )
  if(istatus/=0) then
     write(6,*) myname_,': cannot proceed, CV does not contain ens-required field'
     call stop2(999)
  endif
  do ii=1,nsubwin

!    multiply by beta1_inv first:
     do jj=1,ne3d
        grady%step(ii)%r3(ipc(jj))%q =beta1_inv*grady%step(ii)%r3(ipc(jj))%q
     enddo
! Default to static B estimate for SST
!    grady%step(ii)%r2(ipc(8))%q=beta1_inv*grady%step(ii)%r2(ipc(8))%q ! <---- sst
     grady%step(ii)%r2(ipc(7))%q=beta1_inv*grady%step(ii)%r2(ipc(7))%q

!    next multiply by beta2inv:
     do nn=1,n_ens
        grady%aens(ii,nn)%r3(1)%q =beta2_inv*grady%aens(ii,nn)%r3(1)%q
     enddo
     
 
  end do

  call timer_fnl('beta12mult')

  return
end subroutine beta12mult

end module hybrid_ensemble_isotropic_regional

subroutine get_new_alpha_beta(aspect,ng,fmat_out,fmat0_out)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    get_new_alpha_beta
!
!   prgrmmr:
!
! abstract:  compute various constants for new factorization Purser 1-d high-order filter.
!            adapted as simplification from new_alpha_betaa4 in raflib.f90 for use with
!            simple homogeneous isotropic localization filter.
!
! program history log:
!   2009-09-28  parrish  initial documentation
!
!   input argument list:
!     aspect   - squared correlation scale, in grid units squared
!     ng       - length of string
!     m        - filter order
!
!   output argument list:
!     fmat_out,fmat0_out - filter parameters for special factorization
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind
  use constants, only: one
  use raflib, only: stringop
  implicit none

  integer(i_kind)            , intent(in   ) :: ng
  real(r_kind), dimension(ng), intent(in   ) :: aspect
  real(r_kind)               , intent(  out) :: fmat_out(2,ng,2),fmat0_out(ng,2)

  integer(i_kind) i,j
  real(r_kind) sig(0:ng-1),fmat(0:ng-1,-2:0,2)

  do i=1,ng
     sig(i-1)=sqrt(aspect(i))
  end do
  call stringop(ng-1,sig,fmat)

  do i=1,ng
     fmat_out(2,i,1)=fmat(i-1,-2,1)
     fmat_out(1,i,1)=fmat(i-1,-1,1)
     fmat0_out(i,1)=one/fmat(i-1,0,1)
     fmat_out(2,i,2)=fmat(i-1,-2,2)
     fmat_out(1,i,2)=fmat(i-1,-1,2)
     fmat0_out(i,2)=one/fmat(i-1,0,2)
  end do

end subroutine get_new_alpha_beta

subroutine bkerror_a_en(gradx,grady)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    bkerror_a_en  copy of bkerror for hybrid ensemble          
!   prgmmr: parrish          org: np22                date: 2009-09-17
!
! abstract: copy of bkerror for applying localization recursive filter
!            to hybrid ensemble control variable a_en.
!
! program history log:
!   2009-09-17  parrish  initial creation of code from a copy of bkerror
!   2010-05-20  todling  update to use bundle
!
!   input argument list:
!     gradx    - input field  
!
!   output
!     grady    - background structure * gradx 
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  use kinds, only: r_kind,i_kind
  use gsi_4dvar, only: nsubwin, lsqrtb
  use mpimod, only: mype ! _RT DEBUG
  use constants, only:  zero
  use control_vectors
  use timermod, only: timer_ini,timer_fnl
  use hybrid_ensemble_parameters, only: n_ens
  use gsi_bundlemod,only: gsi_bundlegetpointer
  implicit none

! Declare passed variables
  type(control_vector),intent(inout) :: gradx
  type(control_vector),intent(inout) :: grady

! Declare local variables
  integer(i_kind) ii,nn,ip,istatus

  if (lsqrtb) then
     write(6,*)'bkerror_a_en: not for use with lsqrtb'
     call stop2(317)
  end if

! Initialize timer
  call timer_ini('bkerror_a_en')

! Put things in grady first since operations change input variables
  call gsi_bundlegetpointer ( grady%aens(1,1),'a_en',ip,istatus)
  if(istatus/=0) then
     write(6,*)'bkerror_a_en: trouble getting pointer to ensemble CV'
     call stop2(317)
  endif
  do ii=1,nsubwin
     do nn=1,n_ens
        grady%aens(ii,nn)%r3(ip)%q=gradx%aens(ii,nn)%r3(ip)%q
     enddo
  end do

! Apply variances, as well as vertical & horizontal parts of background error
  do ii=1,nsubwin
     call bkgcov_a_en_new_factorization(grady%aens(ii,1:n_ens))
  end do

! Finalize timer
  call timer_fnl('bkerror_a_en')

  return
end subroutine bkerror_a_en

subroutine bkgcov_a_en_new_factorization(a_en)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    bkgcov_a_en copy of bkgcov for hybrid ens var a_en 
!   prgmmr: parrish        org: np22                date: 2009-09-17
!
! abstract: copy of bkgcov to apply localization with recursive filters
!            to hybrid ensemble control variable a_en.
!
! program history log:
!   2009-09-17  parrish
!   2010-02-20  parrish, adapt for dual resolution
!   2010-05-21  todling, update to use bundle
!
!   input argument list:
!     a_en     - control variable for ensemble contribution to background error
!     nlevs    - number of vertical levels for smoothing
!
!   output argument list:
!                 all after smoothing, combining scales
!     a_en     - control variable for ensemble contribution to background error
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: zero
  use gridmod, only: regional
  use hybrid_ensemble_parameters, only: n_ens,grd_loc
  use hybrid_ensemble_isotropic_regional, only: &
         new_factorization_rf_z,new_factorization_rf_x,new_factorization_rf_y
  use hybrid_ensemble_isotropic_global, only: sf_xy
  use general_sub2grid_mod, only: general_sub2grid_r_double,general_grid2sub_r_double
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer

  implicit none

! Passed Variables
! real(r_kind),dimension(grd_loc%latlon1n,n_ens),intent(inout) :: a_en
  type(gsi_bundle),intent(inout) :: a_en(n_ens)

! Local Variables
  integer(i_kind) i,ii,j,k,n,iflg,iadvance,iback,is,ie,ipnt,istatus
  real(r_kind) hwork(grd_loc%inner_vars,grd_loc%nlat,grd_loc%nlon,grd_loc%kbegin_loc:grd_loc%kend_alloc)
  real(r_kind),allocatable,dimension(:):: a_en_work

  iflg=1

  call gsi_bundlegetpointer(a_en(1),'a_en',ipnt,istatus)
  if(istatus/=0) then
     write(6,*)'bkgcov_a_en_new_factorization: trouble getting pointer to ensemble CV'
     call stop2(999)
  endif

! Apply vertical smoother on each ensemble member
  do k=1,n_ens

     iadvance=1 ; iback=2
     call new_factorization_rf_z(a_en(k)%r3(ipnt)%q,iadvance,iback)
 
  end do

! To avoid my having to touch the general sub2grid and grid2sub,
! get copy for ensemble components to work array
  allocate(a_en_work(n_ens*a_en(1)%ndim),stat=istatus)
  if(istatus/=0) then
     write(6,*)'bkgcov_a_en_new_factorization: trouble in alloc(a_en_work)'
     call stop2(999)
  endif
  ii=0
  do k=1,n_ens
     is=ii+1
     ie=is+a_en(1)%ndim
     a_en_work(is:ie)=a_en(k)%values(1:a_en(k)%ndim)
     ii=ii+a_en(1)%ndim
  enddo

! Convert from subdomain to full horizontal field distributed among processors
  call general_sub2grid_r_double(grd_loc,a_en_work,hwork)

! Apply horizontal smoother for number of horizontal scales
  if(regional) then
     iadvance=1 ; iback=2
     call new_factorization_rf_x(hwork,iadvance,iback,grd_loc%kend_loc+1-grd_loc%kbegin_loc)
     call new_factorization_rf_y(hwork,iadvance,iback,grd_loc%kend_loc+1-grd_loc%kbegin_loc)
     iadvance=2 ; iback=1
     call new_factorization_rf_y(hwork,iadvance,iback,grd_loc%kend_loc+1-grd_loc%kbegin_loc)
     call new_factorization_rf_x(hwork,iadvance,iback,grd_loc%kend_loc+1-grd_loc%kbegin_loc)
  else
     call sf_xy(hwork,grd_loc%kend_loc+1-grd_loc%kbegin_loc)
  end if

! Put back onto subdomains
  call general_grid2sub_r_double(grd_loc,hwork,a_en_work)

! Retrieve ensemble components from long vector
  ii=0
  do k=1,n_ens
     is=ii+1
     ie=is+a_en(1)%ndim
     a_en(k)%values(1:a_en(k)%ndim)=a_en_work(is:ie)
     ii=ii+a_en(1)%ndim
  enddo
  deallocate(a_en_work)

! Apply vertical smoother on each ensemble member
  do k=1,n_ens

     iadvance=2 ; iback=1
     call new_factorization_rf_z(a_en(k)%r3(ipnt)%q,iadvance,iback)

  end do

  return
end subroutine bkgcov_a_en_new_factorization

! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

subroutine hybrid_ensemble_setup
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    hybrid_ensemble_setup  initialize everything for hybrid ensemble
!   prgmmr: parrish          org: np22                date: 2009-09-17
!
! abstract: put everything for initializing hybrid ensemble in one subroutine.
!
! program history log:
!   2009-09-17  parrish
!   2010-02-20  parrish, adapt for dual resolution
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


  use kinds, only: r_kind,i_kind
  use hybrid_ensemble_parameters, only: aniso_a_en,generate_ens,n_ens,&
                      beta1_inv,s_ens_h,s_ens_v,nlon_ens,nlat_ens,jcap_ens,jcap_ens_test,&
                      grd_ens,grd_loc,grd_a1,grd_e1,grd_anl,sp_ens,p_e2a,dual_res,uv_hyb_ens
  use gridmod,only: regional,nsig,nlon,nlat,rlats,rlons
  use hybrid_ensemble_isotropic_regional, only: create_ensemble,load_ensemble, &
         init_rf_x,init_rf_y,init_rf_z,&
         normal_new_factorization_rf_z,normal_new_factorization_rf_x,normal_new_factorization_rf_y
  use general_sub2grid_mod, only: general_sub2grid_create_info
  use general_specmod, only: general_init_spec_vars
  use egrid2agrid_mod,only: g_create_egrid2agrid
  use mpimod, only: mype,ierror,npe
  use hybrid_ensemble_isotropic_global, only: init_sf_xy

  implicit none

  real(r_kind) s_ens_h_gu_x,s_ens_h_gu_y
  integer(i_kind) inner_vars,num_fields
  integer(i_kind) nord_e2a
  logical,allocatable::vector(:)

             nord_e2a=4       !   soon, move this to hybrid_ensemble_parameters

  if(aniso_a_en) then
     if(mype == 0) write(6,*)' anisotropic option not available yet for hybrid ensemble localization'
     if(mype >  -10) then
        call mpi_finalize(ierror)
        stop
     end if
  end if

  dual_res = (nlon /= nlon_ens .or. nlat /= nlat_ens)

!     2.  create grid info for ensemble, including stuff for ensemble general_sub2grid, general_grid2sub
  num_fields=nsig*n_ens
  inner_vars=1
  allocate(vector(num_fields))
  vector=.false.
  call general_sub2grid_create_info(grd_loc,inner_vars,nlat_ens,nlon_ens,nsig,num_fields,regional,vector)
  num_fields=6*nsig+2
  deallocate(vector)
  allocate(vector(num_fields))
  vector=.false.
  vector(1:2*nsig)=uv_hyb_ens     !  assume here that 1st two 3d variables are either u,v or psi,chi
  call general_sub2grid_create_info(grd_ens,inner_vars,nlat_ens,nlon_ens,nsig,num_fields,regional,vector)
  call general_sub2grid_create_info(grd_anl,inner_vars,nlat,nlon,nsig,num_fields,regional,vector)
  deallocate(vector)
  num_fields=nsig
  allocate(vector(num_fields))
  vector=.false.
  call general_sub2grid_create_info(grd_e1,inner_vars,nlat_ens,nlon_ens,nsig,num_fields,regional,vector)
  call general_sub2grid_create_info(grd_a1,inner_vars,nlat,nlon,nsig,num_fields,regional,vector)
  deallocate(vector)
  if(jcap_ens /= 0) call general_init_spec_vars(sp_ens,jcap_ens,jcap_ens_test,grd_ens%nlat,grd_ens%nlon)
  if(regional) then
     write(6,*)' not ready for dual-res regional hybrid yet, program stops'
     call mpi_finalize(ierror)
  else
     call g_create_egrid2agrid(nlat,rlats,nlon,rlons,grd_ens%nlat,sp_ens%rlats,grd_ens%nlon,sp_ens%rlons, &
                               nord_e2a,p_e2a)
  end if
    

!     3.  set up localization filters

  call init_rf_z(s_ens_v)
  call normal_new_factorization_rf_z

  if(regional) then
!     convert s_ens_h from km to grid units.
     call convert_km_to_grid_units(s_ens_h,s_ens_h_gu_x,s_ens_h_gu_y)
     call init_rf_x(s_ens_h_gu_x)
     call normal_new_factorization_rf_x
     call init_rf_y(s_ens_h_gu_y)
     call normal_new_factorization_rf_y
 
  else
     call init_sf_xy(jcap_ens)
  end if

end subroutine hybrid_ensemble_setup

subroutine convert_km_to_grid_units(s_ens_h,s_ens_h_gu_x,s_ens_h_gu_y)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    convert_km_to_grid_units
!   prgmmr: parrish          org: np22                date: 2009-09-17
!
! abstract: change horizontal localization length scales from km to grid units.
!
! program history log:
!   2009-09-17  parrish
!
!   input argument list:
!     s_ens_h  - input localization length scale in km
!
!   output argument list:
!     s_ens_h_gu_x - output x localization length in grid units
!     s_ens_h_gu_y - output y localization length in grid units
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use kinds, only: r_kind,i_kind
  use gridmod, only: region_dx,region_dy
  implicit none

  real(r_kind),intent(in   ) ::s_ens_h
  real(r_kind),intent(  out) ::s_ens_h_gu_x,s_ens_h_gu_y

  write(6,*)' in convert_km_to_grid_units, min, max region_dx*.001=',&
                    .001_r_kind*minval(region_dx),.001_r_kind*maxval(region_dx)
  s_ens_h_gu_x=s_ens_h/(.001_r_kind*maxval(region_dx))
  write(6,*)' in convert_km_to_grid_units, min, max region_dy*.001=',&
                    .001_r_kind*minval(region_dy),.001_r_kind*maxval(region_dy)
  s_ens_h_gu_y=s_ens_h/(.001_r_kind*maxval(region_dy))

  write(6,*)' in convert_km_to_grid_units, s_ens_h,s_ens_h_gu_x,y=',s_ens_h,s_ens_h_gu_x,s_ens_h_gu_y

end subroutine convert_km_to_grid_units

subroutine grads1(f,nvert,mype,fname)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    grads1      generate grads output file for f on subdomains
!   prgmmr: parrish          org: np22                date: 2009-09-17
!
! abstract: create grads output file for variable f on subdomains.  used
!            to visualize the field f using grads software.
!
! program history log:
!   2009-09-17  parrish
!
!   input argument list:
!     f     - field to generate grads output file
!     nvert - number of vertical levels in f
!     mype  - local processor
!     fname - character string used to identify field f in grads output file
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use kinds, only: r_single,r_kind,i_kind
  use constants, only: one
  use gridmod, only: nlat,nlon,lon2,lat2
  implicit none

  integer(i_kind),intent(in   ) :: nvert,mype
  character(*)   ,intent(in   ) :: fname
  real(r_kind)   ,intent(in   ) :: f(lat2,lon2,nvert)

  real(r_kind),dimension(nlat,nlon)::work
  real(r_single) outfield(nlon,nlat)

  character(50) dsname,title,filename
! data dsname/'test.dat'/
  data title/'inmi'/
  character(112) datdes(50000)
  character(1) blank
  data blank/' '/
  data undef/-9.99e33_r_single/

  integer(i_kind) i,k,kend,kstart,next,np,ioutdes,ioutdat
  integer(i_kind) last,j,koutmax
  real(r_single) undef
  real(r_single) startp,pinc

  if(mype == 0) then
     np=nvert
     startp=1._r_single
     pinc=1._r_single
     ioutdes=98550
     ioutdat=98551
     write(filename,'(a,".des")')trim(fname)
     write(dsname,'(a,".dat")')trim(fname)
     open(unit=ioutdes,file=trim(filename),form='formatted')
     open(unit=ioutdat,file=trim(dsname),form='unformatted')
     rewind ioutdes
     rewind ioutdat
     do i=1,50000
        write(datdes(i),'(112a1)')(blank,k=1,112)
     end do
     write(datdes(1),'("DSET ",a50)')dsname
     write(datdes(2),'("options big_endian sequential")')
     write(datdes(3),'("TITLE ",a50)')title
     write(datdes(4),'("UNDEF ",e11.2)')undef
     next=5
     write(datdes(next),'("XDEF ",i5," LINEAR ",f7.2,f7.2)')nlon,startp,pinc
     next=next+1
     write(datdes(next),'("YDEF ",i5," LINEAR ",f7.2,f7.2)')nlat,startp,pinc
     next=next+1
     write(datdes(next),'("ZDEF ",i5," LINEAR ",f7.2,f7.2)')np,startp,pinc
     next=next+1
     koutmax=1
     write(datdes(next),'("TDEF ",i5," LINEAR 0Z23may1992 24hr")')koutmax
     next=next+1
     write(datdes(next),'("VARS 1")')
     next=next+1
     write(datdes(next),'("f   ",i5," 99 f   ")')nvert
     next=next+1
     write(datdes(next),'("ENDVARS")')
     last=next
     write(ioutdes,'(a112)')(datdes(i),i=1,last)
 
  end if

  do k=1,nvert
     call sub2grid_1(f(1,1,k),work,0,mype)
     if(mype == 0) then
        do j=1,nlon ; do i=1,nlat
           outfield(j,i)=work(i,j)
        end do ; end do
        write(ioutdat)outfield
     end if
  end do

  if(mype == 0) then
     close(ioutdes)
     close(ioutdat)
  end if

end subroutine grads1

subroutine sub2grid_1(sub,grid,gridpe,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    grads1      generate grads output file for f on subdomains
!   prgmmr: parrish          org: np22                date: 2009-09-17
!
! abstract: straightforward, but inefficient code to convert a single variable
!            on subdomains to complete slab on one processor.
!
! program history log:
!   2009-09-17  parrish
!   2010-04-01  treadon - move strip to gridmod
!
!   input argument list:
!     sub   - field on subdomains
!     gridpe- processor that contains full slab representation of sub
!     mype  - local processor
!
!   output argument list:
!     grid  - field on complete slab
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$


  use kinds, only: r_kind,i_kind
  use constants, only: zero
  use gridmod, only: nlat,nlon,lat2,lon2,lat1,lon1,&
         ltosi,ltosj,iglobal,ijn,displs_g,itotsub,strip
  use mpimod, only: mpi_comm_world,ierror,mpi_rtype
  implicit none

  integer(i_kind)                  ,intent(in   ) :: gridpe,mype
  real(r_kind),dimension(lat2,lon2),intent(in   ) :: sub
  real(r_kind),dimension(nlat,nlon),intent(  out) :: grid

  real(r_kind),dimension(lat1*lon1):: zsm
  real(r_kind),dimension(itotsub):: work1
  integer(i_kind) mm1,i,j,k

  mm1=mype+1

  do j=1,lon1*lat1
     zsm(j)=zero
  end do
  call strip(sub,zsm,1)
  call mpi_gatherv(zsm,ijn(mm1),mpi_rtype, &
                 work1,ijn,displs_g,mpi_rtype, &
                 gridpe,mpi_comm_world,ierror)
  if(mype == gridpe) then
     do k=1,iglobal
        i=ltosi(k) ; j=ltosj(k)
        grid(i,j)=work1(k)
     end do
  end if

end subroutine sub2grid_1

subroutine grads1_ens(f,nvert,mype,fname)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    grads1      generate grads output file for f on subdomains
!   prgmmr: parrish          org: np22                date: 2009-09-17
!
! abstract: create grads output file for variable f on subdomains.  used
!            to visualize the field f using grads software.
!
! program history log:
!   2009-09-17  parrish
!
!   input argument list:
!     f     - field to generate grads output file
!     nvert - number of vertical levels in f
!     mype  - local processor
!     fname - character string used to identify field f in grads output file
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use kinds, only: r_single,r_kind,i_kind
  use constants, only: one
  use gridmod, only: nlat,nlon,lon2,lat2
  use hybrid_ensemble_parameters, only: grd_ens
  implicit none

  integer(i_kind),intent(in   ) :: nvert,mype
  character(*)   ,intent(in   ) :: fname
  real(r_kind)   ,intent(in   ) :: f(lat2,lon2,nvert)

  real(r_kind),dimension(nlat,nlon)::work
  real(r_single) outfield(nlon,nlat)

  character(50) dsname,title,filename
! data dsname/'test.dat'/
  data title/'inmi'/
  character(112) datdes(50000)
  character(1) blank
  data blank/' '/
  data undef/-9.99e33_r_single/

  integer(i_kind) i,k,kend,kstart,next,np,ioutdes,ioutdat
  integer(i_kind) last,j,koutmax
  real(r_single) undef
  real(r_single) startp,pinc

  if(mype == 0) then
     np=nvert
     startp=1._r_single
     pinc=1._r_single
     ioutdes=98550
     ioutdat=98551
     write(filename,'(a,".des")')trim(fname)
     write(dsname,'(a,".dat")')trim(fname)
     open(unit=ioutdes,file=trim(filename),form='formatted')
     open(unit=ioutdat,file=trim(dsname),form='unformatted')
     rewind ioutdes
     rewind ioutdat
     do i=1,50000
        write(datdes(i),'(112a1)')(blank,k=1,112)
     end do
     write(datdes(1),'("DSET ",a50)')dsname
     write(datdes(2),'("options big_endian sequential")')
     write(datdes(3),'("TITLE ",a50)')title
     write(datdes(4),'("UNDEF ",e11.2)')undef
     next=5
     write(datdes(next),'("XDEF ",i5," LINEAR ",f7.2,f7.2)')nlon,startp,pinc
     next=next+1
     write(datdes(next),'("YDEF ",i5," LINEAR ",f7.2,f7.2)')nlat,startp,pinc
     next=next+1
     write(datdes(next),'("ZDEF ",i5," LINEAR ",f7.2,f7.2)')np,startp,pinc
     next=next+1
     koutmax=1
     write(datdes(next),'("TDEF ",i5," LINEAR 0Z23may1992 24hr")')koutmax
     next=next+1
     write(datdes(next),'("VARS 1")')
     next=next+1
     write(datdes(next),'("f   ",i5," 99 f   ")')nvert
     next=next+1
     write(datdes(next),'("ENDVARS")')
     last=next
     write(ioutdes,'(a112)')(datdes(i),i=1,last)
 
  end if

  do k=1,nvert
     call sub2grid_1(f(1,1,k),work,0,mype)
     if(mype == 0) then
        do j=1,nlon ; do i=1,nlat
           outfield(j,i)=work(i,j)
        end do ; end do
        write(ioutdat)outfield
     end if
  end do

  if(mype == 0) then
     close(ioutdes)
     close(ioutdat)
  end if

end subroutine grads1_ens

subroutine sub2grid_1_ens(sub,grid,gridpe,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    grads1      generate grads output file for f on subdomains
!   prgmmr: parrish          org: np22                date: 2009-09-17
!
! abstract: straightforward, but inefficient code to convert a single variable
!            on subdomains to complete slab on one processor.
!
! program history log:
!   2009-09-17  parrish
!
!   input argument list:
!     sub   - field on subdomains
!     gridpe- processor that contains full slab representation of sub
!     mype  - local processor
!
!   output argument list:
!     grid  - field on complete slab
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$


  use kinds, only: r_kind,i_kind
  use constants, only: zero
  use hybrid_ensemble_parameters, only: grd_ens
  use mpimod, only: mpi_comm_world,ierror,mpi_rtype
  implicit none

  integer(i_kind)                  ,intent(in   ) :: gridpe,mype
  real(r_kind),dimension(grd_ens%lat2,grd_ens%lon2),intent(in   ) :: sub
  real(r_kind),dimension(grd_ens%nlat,grd_ens%nlon),intent(  out) :: grid

  real(r_kind),dimension(grd_ens%lat1,grd_ens%lon1):: zsm
  real(r_kind),dimension(grd_ens%itotsub):: work1
  integer(i_kind) mm1,i,i0,j,j0,k

  mm1=mype+1

  do j=2,grd_ens%lon2-1
     j0=j-1
     do i=2,grd_ens%lat2-1
        i0=i-1
        zsm(i0,j0)=sub(i,j)
     end do
  end do
  call mpi_gatherv(zsm,grd_ens%ijn(mm1),mpi_rtype, &
                 work1,grd_ens%ijn,grd_ens%displs_g,mpi_rtype, &
                 gridpe,mpi_comm_world,ierror)
  if(mype == gridpe) then
     do k=1,grd_ens%iglobal
        i=grd_ens%ltosi(k) ; j=grd_ens%ltosj(k)
        grid(i,j)=work1(k)
     end do
  end if

end subroutine sub2grid_1_ens
