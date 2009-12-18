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

  use kinds, only: r_kind,i_kind,r_single
  implicit none

! set default to private
  private
! set subroutines to public
  public :: init_grid_vars_a_en
  public :: init_mpi_vars_a_en
  public :: init_commvars_a_en
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
  public :: ensemble_forward_model_ad
! set passed variables to public
  public :: nnnn1o_a_en,nsig1o_a_en,irdsp_s_a_en,ircnt_s_a_en,iscnt_s_a_en,isdsp_s_a_en, &
            irdsp_g_a_en,ircnt_g_a_en,iscnt_g_a_en,isdsp_g_a_en

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!      the following variables are patterned after nsig1o and nnnn1o in gridmod, but here used 
!       for subdomain to horizontal slab representation of hybrid control variable a_en

  integer(i_kind) nsig1o_a_en       ! max no. of levels distributed on each processor
  integer(i_kind) nnnn1o_a_en       ! actual of levels distributed on current processor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!      the next set of variables are variations on ones in mpimod, but here used 
!       for subdomain to horizontal slab representation of hybrid control variable a_en

! communication arrays...set up in init_mpi_vars_a_en

  integer(i_kind),allocatable,dimension(:):: levs_id_a_en ! vert lev id for each level
                                             !  of the nsig1o_a_en slabs (zero if
                                             !  empty, else can vary between 1-->nsig)

  integer(i_kind),allocatable,dimension(:):: nvar_id_a_en ! identify ensemble number for each level
  integer(i_kind),allocatable,dimension(:,:):: nvar_pe_a_en ! pe where each var is kept
!

! Allocated in init_mpi_vars, defined by init_comm_vars_a_en

                                             ! comm. array, displacement ...
  integer(i_kind),allocatable,dimension(:):: isdsp_g_a_en !  for send to nsig1o_a_en slabs
  integer(i_kind),allocatable,dimension(:):: irdsp_g_a_en !  for receive from nsig1o_a_en slabs
  integer(i_kind),allocatable,dimension(:):: isdsp_s_a_en !  for send from nsig1o_a_en slabs
  integer(i_kind),allocatable,dimension(:):: irdsp_s_a_en !  for receive from nsig1o_a_en slabs

                                             ! comm. array, count ...
  integer(i_kind),allocatable,dimension(:):: iscnt_g_a_en !  for send to nsig1o_a_en slabs
  integer(i_kind),allocatable,dimension(:):: ircnt_g_a_en !  for receive from nsig1o_a_en slabs
  integer(i_kind),allocatable,dimension(:):: iscnt_s_a_en !  for send from nsig1o_a_en slabs
  integer(i_kind),allocatable,dimension(:):: ircnt_s_a_en !  for receive from nsig1o_a_en slabs


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!      the remainder of variables for this module are filter parameters for isotropic
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

contains

  subroutine init_grid_vars_a_en(npe)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_grid_vars_a_en copy of init_grid_vars for a_en
!   prgmmr: parrish          org: np22                date: 2009-09-17
!
! abstract: copy of init_grid_vars for use with communication for
!           moving between subdomain and horizontal slab storage for
!           hybrid ensemble control variable a_en.
!
! program history log:
!   2009-09-17  parrish
!
!   input argument list:
!     npe      - input number of processors
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

    use hybrid_ensemble_parameters, only: n_ens
    use gridmod, only: nsig
    implicit none

   integer(i_kind),intent(in)::npe    ! number of mpi tasks

    integer(i_kind) vlevs,k

! Initialize nsig1o_a_en to distribute levs/variables
! as evenly as possible over the tasks
    vlevs=nsig*n_ens
    nsig1o_a_en=vlevs/npe
    if(mod(vlevs,npe)/=0) nsig1o_a_en=nsig1o_a_en+1
    nnnn1o_a_en=nsig1o_a_en                  ! temporarily set the number of levels to nsig1o_a_en
     write(6,*)' in init_grid_vars_a_en, npe,vlevs,nsig1o_a_en,nnnn1o_a_en=', &
                                         npe,vlevs,nsig1o_a_en,nnnn1o_a_en

    return
  end subroutine init_grid_vars_a_en

  subroutine init_mpi_vars_a_en
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_mpi_vars_a_en copy of init_mpi_vars for a_en
!   prgmmr: parrish          org: np22                date: 2009-09-17
!
! abstract: copy of init_mpi_vars for use with communication for
!           moving between subdomain and horizontal slab storage for
!           hybrid ensemble control variable a_en.
!
! program history log:
!   2009-09-17  parrish
!
!   input argument list:
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


    use constants, only: izero,ione
    use gridmod, only: nsig
    use hybrid_ensemble_parameters, only: n_ens
    use mpimod, only: npe,mype
    implicit none

! !INPUT PARAMETERS:

    integer(i_kind) k,kk,kthis,n,nn,mm1
    integer(i_kind) varcnt,kchk

    allocate(levs_id_a_en(nsig1o_a_en),nvar_id_a_en(nsig1o_a_en))
    allocate(nvar_pe_a_en(n_ens*nsig,2))
    allocate(iscnt_g_a_en(npe),isdsp_g_a_en(npe),ircnt_g_a_en(npe),&
       irdsp_g_a_en(npe),iscnt_s_a_en(npe),isdsp_s_a_en(npe),ircnt_s_a_en(npe),&
       irdsp_s_a_en(npe))

    mm1=mype+1

! Initialize slab/subdomain communicators, redefined in
! init_commvars
    do n=1,npe
      iscnt_g_a_en(n)   = izero
      isdsp_g_a_en(n)   = izero
      ircnt_g_a_en(n)   = izero
      irdsp_g_a_en(n)   = izero
      iscnt_s_a_en(n)   = izero
      isdsp_s_a_en(n)   = izero
      ircnt_s_a_en(n)   = izero
      irdsp_s_a_en(n)   = izero

    end do

! Distribute variables as evenly as possible over the tasks

! Need to use a variable to know which tasks have a full nsig1o_a_en 
! array, and which one have the last level irrelevant
    if (mod(n_ens*nsig,npe)==izero) then
      kchk=npe
    else
      kchk=mod(nsig*n_ens,npe)
    end if

    levs_id_a_en=izero
    nvar_id_a_en=izero
    nvar_pe_a_en=-999

! Define which variable/level each task has for the
! global slabs (levs_id_a_en,nvar_id_a_en)
    varcnt=izero
    kthis=izero
    nn=1
    do n=1,npe
      if(n <= kchk) then
        kk=nsig1o_a_en
      else
        kk=nsig1o_a_en-1
      end if
      do k=1,kk
        varcnt=varcnt+1
        kthis=kthis+1
        if(kthis >  nsig) then
          kthis=1
          nn=nn+1
        end if
        nvar_pe_a_en(varcnt,1)=n-1
        nvar_pe_a_en(varcnt,2)=k
        if (n==mm1) then
          levs_id_a_en(k)=kthis
          nvar_id_a_en(k)=nn
        end if
      end do ! enddo over levs
    end do ! enddo over npe
!?????????????????????????????????following is for initial debug
    if(mype == 0) write(6,*)' in init_mpi_vars_a_en, kthis,varcnt=',kthis,varcnt
    if(mype == 0) write(6,*)' in init_mpi_vars_a_en, nsig,n_ens*nsig=',nsig,n_ens*nsig
    if(mype == 0) write(6,*)' in init_mpi_vars_a_en, nn,n_ens=',nn,n_ens
    if(kthis /= nsig.or.varcnt /= n_ens*nsig.or.nn /= n_ens) then
      if(mype == 0) write(6,*)' problem with init_mpi_vars_a_en, above lines should be equal'
      if(mype >  -1000) then
        call mpi_finalize(k)
        stop
      end if
    end if
!?????????????????????????????????preceding is for initial debug

    nnnn1o_a_en=0
    do k=1,nsig1o_a_en
       if (levs_id_a_en(k)/=0) nnnn1o_a_en=nnnn1o_a_en+1
    end do


    return
  end subroutine init_mpi_vars_a_en

  subroutine destroy_mpi_vars_a_en
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    destroy_mpi_vars_a_en copy destroy_mpi_vars for a_en
!                                     interpolation
!   prgmmr: parrish          org: np22                date: 2009-09-17
!
! abstract: copy of destroy_mpi_vars--removes arrays associated with
!            hybrid ensemble control variable a_en.
!
! program history log:
!   2009-09-17  parrish
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

   implicit none

    deallocate(levs_id_a_en)
    deallocate(nvar_pe_a_en)
    deallocate(iscnt_g_a_en,isdsp_g_a_en,ircnt_g_a_en,&
       irdsp_g_a_en,iscnt_s_a_en,isdsp_s_a_en,ircnt_s_a_en,&
       irdsp_s_a_en)
    return
  end subroutine destroy_mpi_vars_a_en

subroutine init_commvars_a_en
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_commvars_a_en copy of init_commvars for a_en
!   prgmmr: parrish          org: np22                date: 2009-09-23
!
! abstract: copy of init_commvars adapted for use in communicatoins
!           between tasks for hybrid ensemble control variable a_en. 
!
! program history log:
!   2009-09-23  parrish
!
!   input argument list:
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
  use kinds, only: i_kind
  use gridmod, only: nsig,ijn_s,ijn
  use mpimod, only: mype,npe
  use hybrid_ensemble_parameters, only: n_ens

  use constants, only: izero
  implicit none

  integer(i_kind) ns,mm1
  integer(i_kind) i,j,n,kchk
  
  mm1=mype+1

! vertical column / horizontal slice communicator arrays
  isdsp_g_a_en(1)=izero
  irdsp_g_a_en(1)=izero
  isdsp_s_a_en(1)=izero
  irdsp_s_a_en(1)=izero

  if (mod(n_ens*nsig,npe)==izero) then
    kchk=npe
  else
    kchk=mod(n_ens*nsig,npe)
  end if

  do n=1,npe
    if (n <= kchk) then
      iscnt_g_a_en(n)=ijn(mm1)*nsig1o_a_en
      ircnt_s_a_en(n)=ijn_s(mm1)*nsig1o_a_en
    else
      iscnt_g_a_en(n)=ijn(mm1)*(nsig1o_a_en-1)
      ircnt_s_a_en(n)=ijn_s(mm1)*(nsig1o_a_en-1)
    end if

    if (mm1 <= kchk) then
      ircnt_g_a_en(n)=ijn(n)*nsig1o_a_en
      iscnt_s_a_en(n)=ijn_s(n)*nsig1o_a_en
    else
      ircnt_g_a_en(n)=ijn(n)*(nsig1o_a_en-1)
      iscnt_s_a_en(n)=ijn_s(n)*(nsig1o_a_en-1)
    end if

    if (n/=1) then
      isdsp_g_a_en(n)=isdsp_g_a_en(n-1)+iscnt_g_a_en(n-1)
      irdsp_g_a_en(n)=irdsp_g_a_en(n-1)+ijn(n-1)*nsig1o_a_en
      isdsp_s_a_en(n)=isdsp_s_a_en(n-1)+ijn_s(n-1)*nsig1o_a_en
      irdsp_s_a_en(n)=irdsp_s_a_en(n-1)+ircnt_s_a_en(n-1)
    end if
  end do

  return
end subroutine init_commvars_a_en

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

  use gridmod, only: nsig
  use constants, only: half

  real(r_kind),intent(in):: z_len

  integer(i_kind) k
  real(r_kind) aspect(nsig)

!    use new factorization:
  allocate(fmatz(2,nsig,2),fmat0z(nsig,2))
  do k=1,nsig
    aspect(k)=z_len**2
  end do
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

  real(r_kind),intent(in):: x_len

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

  real(r_kind),intent(in):: y_len

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

  use gridmod,only: latlon11,nsig
  implicit none

  integer(i_kind),intent(in):: iadvance,iback
  real(r_kind),intent(inout):: f(latlon11,nsig)

  integer(i_kind) i,j,k,l,nxy,nz

  nxy=latlon11 ; nz=nsig
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

subroutine new_factorization_rf_x(f,iadvance,iback)
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

  use gridmod, only: nlat,nlon
  implicit none

  integer(i_kind),intent(in):: iadvance,iback
  real(r_kind),intent(inout):: f(nlat,nlon,nnnn1o_a_en)

  integer(i_kind) i,j,k,l,ny,nx,nz

  ny=nlat ; nx=nlon ; nz=nnnn1o_a_en
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

subroutine new_factorization_rf_y(f,iadvance,iback)
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

  use gridmod, only: nlat,nlon
                      !                  use mpimod, only: mype
  implicit none

  integer(i_kind),intent(in):: iadvance,iback
  real(r_kind),intent(inout):: f(nlat,nlon*nnnn1o_a_en)

  integer(i_kind) i,k,l,nx,ny,nz

  nx=nlon ; ny=nlat ; nz=nnnn1o_a_en
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
  use gridmod, only: latlon11,nsig
  use constants, only: zero,one,izero
  implicit none

  integer(i_kind) k,kcount,lcount,iadvance,iback
  real(r_kind) f(latlon11,nsig),diag(nsig)

  if(allocated(znorm_new)) deallocate(znorm_new)
  allocate(znorm_new(nsig))

  znorm_new=one
  kcount=izero
  lcount=izero
  do
    f=zero
    do k=1,min(latlon11,nsig)
      kcount=kcount+1
      f(k,kcount)=one
      if(kcount == nsig) exit
    end do
    iadvance=1 ; iback=2
    call new_factorization_rf_z(f,iadvance,iback)
    iadvance=2 ; iback=1
    call new_factorization_rf_z(f,iadvance,iback)
    do k=1,min(latlon11,nsig)
      lcount=lcount+1
      diag(lcount)=sqrt(one/f(k,lcount))
      if(lcount == nsig) exit
    end do
    if(lcount == nsig) exit
  end do
  do k=1,nsig
    znorm_new(k)=diag(k)
  end do
!              check result:
  kcount=izero
  lcount=izero
  do
    f=zero
    do k=1,min(latlon11,nsig)
      kcount=kcount+1
      f(k,kcount)=one
      if(kcount == nsig) exit
    end do
    iadvance=1 ; iback=2
    call new_factorization_rf_z(f,iadvance,iback)
    iadvance=2 ; iback=1
    call new_factorization_rf_z(f,iadvance,iback)
    do k=1,min(latlon11,nsig)
      lcount=lcount+1
      diag(lcount)=f(k,lcount)
      if(lcount == nsig) exit
    end do
    if(lcount == nsig) exit
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
  use constants, only: zero,one,izero

  integer(i_kind) i,j,k,kcount,lcount,iadvance,iback
  real(r_kind) f(nlat,nlon,nnnn1o_a_en),diag(nlat,nlon)

  if(allocated(xnorm_new)) deallocate(xnorm_new)
  allocate(xnorm_new(nlat,nlon))
  xnorm_new=one

  kcount=izero
  lcount=izero
  do
    f=zero
    do k=1,min(nlon,nnnn1o_a_en)
      kcount=kcount+1
      do i=1,nlat
        f(i,kcount,k)=one
      end do
      if(kcount == nlon) exit
    end do
    iadvance=1 ; iback=2
    call new_factorization_rf_x(f,iadvance,iback)
    iadvance=2 ; iback=1
    call new_factorization_rf_x(f,iadvance,iback)
    do k=1,min(nlon,nnnn1o_a_en)
      lcount=lcount+1
      do i=1,nlat
        diag(i,lcount)=sqrt(one/f(i,lcount,k))
      end do
      if(lcount == nlon) exit
    end do
    if(lcount == nlon) exit
  end do
  do j=1,nlon
    do i=1,nlat
      xnorm_new(i,j)=diag(i,j)
    end do
  end do

!           check accuracy of xnorm
  kcount=izero
  lcount=izero
  do
    f=zero
    do k=1,min(nlon,nnnn1o_a_en)
      kcount=kcount+1
      do i=1,nlat
        f(i,kcount,k)=one
      end do
      if(kcount == nlon) exit
    end do
    iadvance=1 ; iback=2
    call new_factorization_rf_x(f,iadvance,iback)
    iadvance=2 ; iback=1
    call new_factorization_rf_x(f,iadvance,iback)
    do k=1,min(nlon,nnnn1o_a_en)
      lcount=lcount+1
      do i=1,nlat
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
  use constants, only: zero,one,izero
                                  use mpimod, only: mype
  implicit none

  integer(i_kind) k,kcount,lcount,iadvance,iback
  real(r_kind) f(nlat,nlon*nnnn1o_a_en),diag(nlat)

  if(allocated(ynorm_new)) deallocate(ynorm_new)
  allocate(ynorm_new(nlat))
  ynorm_new=one

  kcount=izero
  lcount=izero
  do
    f=zero
    do k=1,min(nlon*nnnn1o_a_en,nlat)
      kcount=kcount+1
      f(kcount,k)=one
      if(kcount == nlat) exit
    end do
    iadvance=1 ; iback=2
    call new_factorization_rf_y(f,iadvance,iback)
    iadvance=2 ; iback=1
    call new_factorization_rf_y(f,iadvance,iback)
    do k=1,min(nlon*nnnn1o_a_en,nlat)
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
  kcount=izero
  lcount=izero
  do
    f=zero
    do k=1,min(nlon*nnnn1o_a_en,nlat)
      kcount=kcount+1
      f(kcount,k)=one
      if(kcount == nlat) exit
    end do
    iadvance=1 ; iback=2
    call new_factorization_rf_y(f,iadvance,iback)
    iadvance=2 ; iback=1
    call new_factorization_rf_y(f,iadvance,iback)
    do k=1,min(nlon*nnnn1o_a_en,nlat)
      lcount=lcount+1
      diag(lcount)=f(lcount,k)
      if(lcount == nlat) exit
    end do
    if(lcount == nlat) exit
  end do
  write(6,*)' in normal_new_factorizaiton_rf_y, min,max(diag)=',minval(diag),maxval(diag)

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
    use gridmod, only: latlon1n,latlon11
    use hybrid_ensemble_parameters, only: n_ens

    implicit none


    allocate(st_en(latlon1n,n_ens), vp_en(latlon1n,n_ens), &
              t_en(latlon1n,n_ens), rh_en(latlon1n,n_ens), &
             oz_en(latlon1n,n_ens), cw_en(latlon1n,n_ens), &
              p_en(latlon11,n_ens),sst_en(latlon11,n_ens))
                 write(6,*)' in create_ensemble, latlon11,latlon1n,n_ens=',latlon11,latlon1n,n_ens
                 write(6,*)' in create_ensemble, total bytes allocated=',4*(6*latlon1n+2*latlon11)*n_ens

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
    use gridmod, only: latlon11,latlon1n,nnnn1o,nlat,nlon
                                    use gridmod, only: nsig
    use constants, only: zero,one
    use hybrid_ensemble_parameters, only: n_ens,generate_ens
                                          use mpimod, only: mype,ierror
    implicit none

    real(r_kind),dimension(latlon1n)::st,vp,t,rh,oz,cw
    real(r_kind),dimension(latlon11)::p,sst
    real(r_kind),dimension(latlon1n)::stbar,vpbar,tbar,rhbar,ozbar,cwbar
    real(r_kind),dimension(latlon11)::pbar,sstbar
    integer(i_kind) i,n
    real(r_kind),allocatable:: seed(:,:)
    real(r_kind) sig_norm,bar_norm
                                      character(50) title

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
      do n=1,n_ens
        call generate_one_ensemble_perturbation(st,vp,t,rh,oz,cw,p,sst,seed)
        do i=1,latlon1n
          st_en(i,n)=st(i)
          vp_en(i,n)=vp(i)
           t_en(i,n)= t(i)
          rh_en(i,n)=rh(i)
          oz_en(i,n)=oz(i)
          cw_en(i,n)=cw(i)
          stbar(i)=stbar(i)+st(i)
          vpbar(i)=vpbar(i)+vp(i)
           tbar(i)= tbar(i)+ t(i)
          rhbar(i)=rhbar(i)+rh(i)
          ozbar(i)=ozbar(i)+oz(i)
          cwbar(i)=cwbar(i)+cw(i)
        end do
        do i=1,latlon11
          p_en(i,n)=p(i)
          sst_en(i,n)=sst(i)
          pbar(i)=pbar(i)+ p(i)
          sstbar(i)=sstbar(i)+ sst(i)
        end do
      end do
!                          remove mean, which is locally significantly non-zero, due to sample size.
!                           with real ensembles, the mean of the actual sample will be removed.
      do n=1,n_ens
        do i=1,latlon1n
          st_en(i,n)=(st_en(i,n)-stbar(i)*bar_norm)*sig_norm
          vp_en(i,n)=(vp_en(i,n)-vpbar(i)*bar_norm)*sig_norm
           t_en(i,n)=( t_en(i,n)- tbar(i)*bar_norm)*sig_norm
          rh_en(i,n)=(rh_en(i,n)-rhbar(i)*bar_norm)*sig_norm
          oz_en(i,n)=(oz_en(i,n)-ozbar(i)*bar_norm)*sig_norm
          cw_en(i,n)=(cw_en(i,n)-cwbar(i)*bar_norm)*sig_norm
        end do
        do i=1,latlon11
          p_en(i,n)=(p_en(i,n)- pbar(i)*bar_norm)*sig_norm
          sst_en(i,n)=(sst_en(i,n)-sstbar(i)*bar_norm)*sig_norm
        end do
      end do
    
    else
!            read in ensembles
     write(6,*)' ENSEMBLE INPUT FOR HYBRID ENSEMBLE 3DVAR NOT WRITTEN YET.  PROGRAM STOPS.'
     if(1 /= 0) stop
    end if

  end subroutine load_ensemble
     
  subroutine generate_one_ensemble_perturbation(st,vp,t,rh,oz,cw,p,sst,seed)
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
    use gridmod, only: nlat,nlon,istart,jstart,lat2,lon2,nsig,nnnn1o,regional
    use mpimod, only: mype,mpi_rtype,mpi_comm_world,ierror,nvar_pe
    use berror, only: qvar3d
    use hybrid_ensemble_parameters, only: uv_hyb_ens
    use constants, only: izero,zero,one

    real(r_kind),intent(inout)::seed(nval2f,nscl)
    real(r_kind),intent(out),dimension(lat2,lon2,nsig):: st,vp,t,rh,oz,cw
    real(r_kind),intent(out),dimension(lat2,lon2):: p,sst

    real(r_kind),dimension(nval2f,nnnn1o,nscl):: z
    real(r_kind) vert1(6*nsig+4)
    integer(i_llong) iseed
    integer(i_kind) nvert,i,is,naux,k
    real(r_kind) aux
    real(r_kind),dimension(nh_0:nh_1,6*nsig+4,nscl):: zsub
    real(r_kind),dimension(lat2,lon2,nsig):: u,v

    naux=izero
    nvert=(6*nsig+4)
    if(maxval(seed) <  zero) then

!       create initial seed for random numbers for each horizontal location.
  
      if(mype == izero) then
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
        call dnrand(seed(i,is),nvert,vert1,aux,naux)
        do k=1,nvert
          zsub(i,k,is)=vert1(k)
        end do
      end do
    end do
    call special_sd2h(zsub,z)

!     if this is a global run, then need to fix tropical belt part of z so periodic overlap is correct
    if(.not.regional) call fix_belt(z)

!     set qvar3d=1 here to get non-zero rh ensemble member.  later, after setuprhs on first outer loop,
!           rescale by proper qvar3d.  will take some thinking about how to do this for 
!            input ensemble members as opposed to inernally generated from sqrt(B) ensemble members.
    qvar3d=one
    call ckgcov(z,st,vp,t,p,rh,oz,sst,cw,nnnn1o)
!      reset qvar3d to zero--will get recomputed in compute_derived
    qvar3d=zero

!     if uv_hyb_ens=.true., then convert st,vp to u,v
    if(uv_hyb_ens) then
      call getuv(u,v,st,vp,izero)
      st=u
      vp=v
    end if

  end subroutine generate_one_ensemble_perturbation

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
    use gridmod, only: nlon,nnnn1o
    use berror, only: nx,ny,nf
    use constants, only: izero,ione
    implicit none

    real(r_kind),intent(inout):: z(nval2f,nnnn1o,nscl)

    real(r_kind) zloc1(ny,nx)
    integer(i_kind) i,ii,j,jj,k

    do j=1,nscl
      do k=1,nnnn1o
        i=izero
        do jj=1,nx
          do ii=1,ny
            i=i+ione
            zloc1(ii,jj)=z(i,k,j)
          end do
        end do
        do jj=nlon+1,nx
          do ii=1,ny
            zloc1(ii,jj)=zloc1(ii,jj-nlon)
          end do
        end do
        i=izero
        do jj=1,nx
          do ii=1,ny
            i=i+ione
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
    use gridmod, only: lat2,lon2,nsig
    use hybrid_ensemble_parameters, only: n_ens
    use berror, only: qvar3d
    use constants, only: izero
    implicit none

    integer(i_kind) i,ii,j,k,n

    do n=1,n_ens
      ii=izero
      do k=1,nsig
        do j=1,lon2
          do i=1,lat2
            ii=ii+1
            rh_en(ii,n)=qvar3d(i,j,k)*rh_en(ii,n)
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

  subroutine ensemble_forward_model(st,vp,t,rh,oz,cw,p,sst,a_en)
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
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

    use gridmod,only:       latlon1n,latlon11
    use hybrid_ensemble_parameters, only: n_ens
    use constants, only: zero

    real(r_kind),dimension(latlon1n),intent(inout):: st,vp,t,rh,oz,cw
    real(r_kind),dimension(latlon11),intent(inout):: p,sst
    real(r_kind),dimension(latlon1n,n_ens),intent(in):: a_en

    integer(i_kind) i,k

    do k=1,n_ens
      do i=1,latlon1n
        st(i)=st(i)+a_en(i,k)*st_en(i,k)
        vp(i)=vp(i)+a_en(i,k)*vp_en(i,k)
         t(i)= t(i)+a_en(i,k)* t_en(i,k)
        rh(i)=rh(i)+a_en(i,k)*rh_en(i,k)
        oz(i)=oz(i)+a_en(i,k)*oz_en(i,k)
        cw(i)=cw(i)+a_en(i,k)*cw_en(i,k)
      end do
      do i=1,latlon11
        p(i)  =p(i)  +a_en(i,k)*p_en(i,k)
        sst(i)=sst(i)+a_en(i,k)*sst_en(i,k)
      end do
    end do

  end subroutine ensemble_forward_model

  subroutine ensemble_forward_model_ad(st,vp,t,rh,oz,cw,p,sst,a_en)
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
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

    use gridmod,only:       latlon1n,latlon11
    use hybrid_ensemble_parameters, only: n_ens

    real(r_kind),dimension(latlon1n),intent(in):: st,vp,t,rh,oz,cw
    real(r_kind),dimension(latlon11),intent(in):: p,sst
    real(r_kind),dimension(latlon1n,n_ens),intent(inout):: a_en

    integer(i_kind) i,k


    do k=1,n_ens
      do i=1,latlon1n
        a_en(i,k)=a_en(i,k)+st(i)*st_en(i,k)
        a_en(i,k)=a_en(i,k)+vp(i)*vp_en(i,k)
        a_en(i,k)=a_en(i,k)+ t(i)* t_en(i,k)
        a_en(i,k)=a_en(i,k)+rh(i)*rh_en(i,k)
        a_en(i,k)=a_en(i,k)+oz(i)*oz_en(i,k)
        a_en(i,k)=a_en(i,k)+cw(i)*cw_en(i,k)
      end do
      do i=1,latlon11
        a_en(i,k)=a_en(i,k)+p(i)*p_en(i,k)
        a_en(i,k)=a_en(i,k)+sst(i)*sst_en(i,k)
      end do
    end do

  end subroutine ensemble_forward_model_ad

  subroutine special_sd2h0
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    special_sd2h0  initialize subroutine special_sd2h
!   prgmmr: parrish          org: np22                date: 2009-06-16
!
! abstract: initialize suboutine special_sd2h (subdomain to slab for  
!             variable a_en).
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

    use kinds, only: r_kind,i_kind
    use mpimod, only: npe,mype,mpi_comm_world,ierror,mpi_rtype
    use gridmod, only: nlat,nlon,nsig,nnnn1o,regional
    use berror, only: nx,ny,nf
    use constants, only: izero,ione
    implicit none

    integer(i_kind),dimension(0:npe-1):: nh_0_all,nh_1_all,nv_0_all,nv_1_all
    integer(i_kind) nvert,nh_tot,nh_this,nn,nv_tot,nv_this,kchk,n,kk,i,k
    real(r_kind),allocatable:: zsub(:,:),z(:)

!    set nval2f (loosely paraphrased from jfunc.f90)

    nscl=3_i_kind          !  hard-wired here, later generalize when generalizing control variables
    if(regional) then
      nval2f=nlat*nlon
    else
      nval2f=ny*nx + 2_i_kind*(2_i_kind*nf+ione)*(2_i_kind*nf+ione)
    end if


    allocate(nsend_sd2h(0:npe-1),ndsend_sd2h(0:npe),nrecv_sd2h(0:npe-1),ndrecv_sd2h(0:npe-1))
    allocate(i_recv(nval2f*nnnn1o),k_recv(nval2f*nnnn1o))
    nvert=6_i_kind*nsig+4_i_kind

!  compute nv_0,nv_1

    nv_tot=nvert
    nv_this=nv_tot/npe
    if(mod(nv_tot,npe)/=izero) nv_this=nv_this+ione
    if(mod(nv_tot,npe)==izero) then
      kchk=npe
    else
      kchk=mod(nv_tot,npe)
    end if

    nv_0_all=-ione
    nv_1_all=-2_i_kind
    nn=izero
    do n=ione,npe
      if(n<=kchk) then
        kk=nv_this
      else
        kk=nv_this-ione
      end if
      if(kk>izero) then
        nv_0_all(n-ione)=nn+ione
        nv_1_all(n-ione)=nn+kk
      end if
      nn=nn+kk
    end do
    nv_0=nv_0_all(mype)
    nv_1=nv_1_all(mype)
                     !   write(0,*)' nv_0,nv_1,nv_1-nv_0+1,nnnn1o=',nv_0,nv_1,nv_1-nv_0+1,nnnn1o
                     !   if(mype==izero) then
                     !     do n=0,npe-1
                     !       write(0,*)' n,nv_0_all(n),nv_1_all(n),size=',n,nv_0_all(n),nv_1_all(n),&
                     !                                                      nv_1_all(n)-nv_0_all(n)+1
                     !     end do
                     !   end if

!     compute nh_0, nh_1

    nh_tot=nval2f
    nh_this=nh_tot/npe
    if(mod(nh_tot,npe)/=izero) nh_this=nh_this+ione
    if(mod(nh_tot,npe)==izero) then
      kchk=npe
    else
      kchk=mod(nh_tot,npe)
    end if

    nh_0_all=-ione
    nh_1_all=-2_i_kind
    nn=izero
    do n=ione,npe
      if(n<=kchk) then
        kk=nh_this
      else
        kk=nh_this-ione
      end if
      if(kk>izero) then
        nh_0_all(n-1)=nn+ione
        nh_1_all(n-1)=nn+kk
      end if
      nn=nn+kk
    end do
    nh_0=nh_0_all(mype)
    nh_1=nh_1_all(mype)

!   compute nsend_sd2h,ndsend_sd2h,nrecv_sd2h,ndrecv_sd2h

    ndsend_sd2h(0)=izero
    ndrecv_sd2h(0)=izero
    do n=izero,npe-ione
      nsend_sd2h(n)=max(izero,(nv_1_all(n)-nv_0_all(n)+ione)*(nh_1-nh_0+ione))
      ndsend_sd2h(n+1)=ndsend_sd2h(n)+nsend_sd2h(n)
      nrecv_sd2h(n)=max(izero,(nv_1-nv_0+ione)*(nh_1_all(n)-nh_0_all(n)+ione))
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
!     st       - stream function input control variable
!
!   output argument list:
!     st       - stream function output control variable
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$



!     zsub:   input array on "subdomains"
!     z:      output array on slabs (form expected for input argument to ckgcov).

  use kinds, only: r_kind,i_kind
  use gridmod, only: nnnn1o,nsig
  use constants, only: izero
                                            use constants, only: zero
  use mpimod, only: mype,mpi_rtype,ierror,mpi_comm_world
  implicit none

  real(r_kind),dimension(nh_0:nh_1,6*nsig+4,nscl),intent(in)::zsub
  real(r_kind),dimension(nval2f,nv_0:nv_1,nscl),intent(out)::z

  real(r_kind) zsub1(nh_0:nh_1,6*nsig+4),work(nval2f*(nv_1-nv_0+1))
  integer(i_kind) i,ii,is,k
                       !integer(i_kind) ibadp,ibadm,kbadp,kbadm
                       !  logical good

!      1 <= nh_0 <= nh_1 <= nval2f

!      1 <= nv_0 <= nv_1 <= 6*nsig+4

   z=zero
   do is=1,nscl
    do k=1,6*nsig+4
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

  integer(i_kind), intent(in):: ng
  real(r_kind), dimension(ng), intent(in):: aspect
  real(r_kind),intent(out):: fmat_out(2,ng,2),fmat0_out(ng,2)

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
  use gridmod, only: nlat,nlon,periodic
  use hybrid_ensemble_isotropic_regional, only: nnnn1o_a_en
  use constants, only:  zero
  use control_vectors
  use timermod, only: timer_ini,timer_fnl
                                             use mpimod, only: mype            ! debug
  implicit none

! Declare passed variables
  type(control_vector),intent(inout):: gradx
  type(control_vector),intent(inout):: grady

! Declare local variables
  integer(i_kind) ii,iflg
  real(r_kind),dimension(nlat,nlon,nnnn1o_a_en):: work

  if (lsqrtb) then
    write(6,*)'bkerror_a_en: not for use with lsqrtb'
    call stop2(317)
  end if

! Initialize timer
  call timer_ini('bkerror_a_en')

! If dealing with periodic (sub)domain, gather full domain grids,
! account for periodicity, and redistribute to subdomains.  This
! only needs to be done when running with a single mpi task and
! then only for array gradx.
  if (periodic) then
    iflg=2
    do ii=1,nsubwin
      call sub2grid_a_en(work,gradx%step(ii)%a_en,iflg)
      call grid2sub_a_en(work,gradx%step(ii)%a_en)
    end do
  endif

! Put things in grady first since operations change input variables
  do ii=1,nsubwin
    grady%step(ii)%a_en=gradx%step(ii)%a_en
  end do

! Loop on control steps
  do ii=1,nsubwin

!   Apply variances, as well as vertical & horizontal parts of background error
    call bkgcov_a_en_new_factorization(grady%step(ii)%a_en,nnnn1o_a_en)

  end do

! Finalize timer
  call timer_fnl('bkerror_a_en')

  return
end subroutine bkerror_a_en

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
  use hybrid_ensemble_parameters, only: beta1_inv
  use constants, only:  one
  use control_vectors
  use timermod, only: timer_ini,timer_fnl
  implicit none

! Declare passed variables
  type(control_vector),intent(inout):: grady

! Declare local variables
  integer(i_kind) ii
  real(r_kind) beta2_inv

! Initialize timer
  call timer_ini('beta12mult')

  beta2_inv=one-beta1_inv

  do ii=1,nsubwin

!    multiply by beta1_inv first:
    grady%step(ii)%st(:) =beta1_inv*grady%step(ii)%st(:)
    grady%step(ii)%vp(:) =beta1_inv*grady%step(ii)%vp(:)
    grady%step(ii)%t(:)  =beta1_inv*grady%step(ii)%t(:)
    grady%step(ii)%p(:)  =beta1_inv*grady%step(ii)%p(:)
    grady%step(ii)%rh(:) =beta1_inv*grady%step(ii)%rh(:)
    grady%step(ii)%oz(:) =beta1_inv*grady%step(ii)%oz(:)
    grady%step(ii)%sst(:)=beta1_inv*grady%step(ii)%sst(:)
    grady%step(ii)%cw(:) =beta1_inv*grady%step(ii)%cw(:)

!    next multiply by beta2inv:
    grady%step(ii)%a_en(:) =beta2_inv*grady%step(ii)%a_en(:)

  end do


  call timer_fnl('beta12mult')

  return
end subroutine beta12mult

subroutine bkgcov_a_en_new_factorization(a_en,nlevs)
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
  use gridmod, only: latlon1n,nlat,nlon,regional
  use hybrid_ensemble_parameters, only: n_ens
  use hybrid_ensemble_isotropic_regional, only: nnnn1o_a_en,&
         new_factorization_rf_z,new_factorization_rf_x,new_factorization_rf_y
  use hybrid_ensemble_isotropic_global, only: sf_xy

  implicit none

! Passed Variables
  integer(i_kind),intent(in):: nlevs
  real(r_kind),dimension(latlon1n,n_ens),intent(inout):: a_en

! Local Variables
  integer(i_kind) i,j,k,iflg,iadvance,iback
  real(r_kind),dimension(nlat,nlon,nnnn1o_a_en):: hwork

  iflg=1

  do k=1,n_ens

! Apply vertical smoother
   iadvance=1 ; iback=2
   call new_factorization_rf_z(a_en(:,k),iadvance,iback)

  end do

! Convert from subdomain to full horizontal field distributed among processors
  call sub2grid_a_en(hwork,a_en,iflg)

! Apply horizontal smoother for number of horizontal scales
  if(regional) then
    iadvance=1 ; iback=2
    call new_factorization_rf_x(hwork,iadvance,iback)
    call new_factorization_rf_y(hwork,iadvance,iback)
    iadvance=2 ; iback=1
    call new_factorization_rf_y(hwork,iadvance,iback)
    call new_factorization_rf_x(hwork,iadvance,iback)
  else
    call sf_xy(hwork,nnnn1o_a_en)
  end if

! Put back onto subdomains
  call grid2sub_a_en(hwork,a_en)

  do k=1,n_ens

! Apply vertical smoother
   iadvance=2 ; iback=1
   call new_factorization_rf_z(a_en(:,k),iadvance,iback)

  end do

  return
end subroutine bkgcov_a_en_new_factorization

subroutine grid2sub_a_en(workout,a_en)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    grid2sub_a_en copy of grid2sub for hyb ens var a_en
!   prgmmr: parrish          org: np22                date: 2009-09-17
!
! abstract: copy of subroutine grid2sub for application to hybrid
!            ensemble control variable a_en
!
! program history log:
!   2009-09-17  create code from copy of grid2sub.f90
!
!   input argument list:
!     workout  - input grid values on full grid after horizontal part of cov.
!
!   output argument list:
!     a_en     - grid values on subdomains
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use mpimod, only: ierror,mpi_comm_world,mpi_rtype,reorder2
  use gridmod, only: itotsub,nsig,ltosj_s,ltosi_s,lat2,lon2,nlat,nlon,&
       latlon1n,latlon11
  use hybrid_ensemble_isotropic_regional, only: nnnn1o_a_en,nsig1o_a_en, &
                           irdsp_s_a_en,ircnt_s_a_en,iscnt_s_a_en,isdsp_s_a_en
  use constants, only: zero
  use hybrid_ensemble_parameters, only: n_ens
  implicit none

! Declare passed variables
  real(r_kind),dimension(nlat,nlon,nnnn1o_a_en),intent(in):: workout
  real(r_kind),dimension(lat2,lon2,nsig*n_ens),intent(out):: a_en

! Declare local variables
  integer(i_kind) k,l,ni1,ni2
  real(r_kind),dimension(itotsub,nsig1o_a_en):: work1


! Transfer input array to local work array
  do k=1,nnnn1o_a_en
     do l=1,itotsub
        ni1=ltosi_s(l); ni2=ltosj_s(l)
        work1(l,k)=workout(ni1,ni2,k)
     end do
  end do

! reoder work1 global slab array for communications
  call reorder2(work1,nsig1o_a_en,nnnn1o_a_en)

! send global slabs to subdomains
  call mpi_alltoallv(work1(1,1),iscnt_s_a_en,isdsp_s_a_en,&
       mpi_rtype,a_en,ircnt_s_a_en,irdsp_s_a_en,&
       mpi_rtype,mpi_comm_world,ierror)

 return
end subroutine grid2sub_a_en

subroutine sub2grid_a_en(workin,a_en,iflg)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    sub2grid_a_en copy of sub2grid for control var a_en
!   prgmmr: parrish          org: np22                date: 2009-09-17
!
! abstract: copy of sub2grid for use with hybrid ensemble control
!            variable a_en.
!
! program history log:
!   2009-09-17  parrish
!
!   input argument list:
!     a_en     - a_en grid values                 
!     iflg     = 1=not periodic, 2=periodic
!
!   output argument list:
!     workin   - output grid values on full grid after vertical operations
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use mpimod, only: ierror,mpi_comm_world,mpi_rtype,strip,strip_periodic,reorder
  
  use gridmod, only: itotsub,lat1,lon1,lat2,lon2,iglobal,&
       nlat,nlon,nsig,ltosi,ltosj
  use hybrid_ensemble_isotropic_regional, only: nnnn1o_a_en,nsig1o_a_en,&
                         irdsp_g_a_en,ircnt_g_a_en,iscnt_g_a_en,isdsp_g_a_en
  use constants, only: zero
  use hybrid_ensemble_parameters, only: n_ens
  implicit none

! Declare passed variables
  integer(i_kind),intent(in):: iflg
  real(r_kind),dimension(lat2,lon2,n_ens*nsig),intent(in):: a_en
  real(r_kind),dimension(nlat,nlon,nnnn1o_a_en),intent(out):: workin

! Declare local variables
  integer(i_kind) j,k,l,ni1,ni2
  real(r_kind),dimension(lat1*lon1*n_ens*nsig):: xhatsm
  real(r_kind),dimension(max(iglobal,itotsub),nsig1o_a_en):: work1  !  contain nsig1o slab of any variables

! strip off boundary points and load vector for communication
  if (iflg==1) then
     call strip(a_en,xhatsm,n_ens*nsig)
  elseif (iflg==2) then
     call strip_periodic(a_en,xhatsm,n_ens*nsig)
  else
     write(6,*)'SUB2GRID_a_en:  ***ERROR*** iflg=',iflg,' is an illegal value'
  endif


! zero out work arrays
  do k=1,nsig1o_a_en
    do j=1,itotsub
      work1(j,k)=zero
    end do
  end do
! send subdomain vector to global slabs
  call mpi_alltoallv(xhatsm(1),iscnt_g_a_en,isdsp_g_a_en,&
       mpi_rtype,work1(1,1),ircnt_g_a_en,irdsp_g_a_en,mpi_rtype,&
       mpi_comm_world,ierror)

! reorder work1 array post communication
  call reorder(work1,nsig1o_a_en,nnnn1o_a_en)
  do k=1,nnnn1o_a_en
   do l=1,iglobal
      ni1=ltosi(l); ni2=ltosj(l)
      workin(ni1,ni2,k)=work1(l,k)
   end do
  end do

  return
end subroutine sub2grid_a_en

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
                      beta1_inv,s_ens_h,s_ens_v
  use gridmod,only: regional,nsig
  use hybrid_ensemble_isotropic_regional, only: nsig1o_a_en,init_grid_vars_a_en,init_mpi_vars_a_en,&
                 init_commvars_a_en,create_ensemble,load_ensemble,init_rf_x,init_rf_y,init_rf_z,&
         normal_new_factorization_rf_z,normal_new_factorization_rf_x,normal_new_factorization_rf_y
  use mpimod, only: mype,ierror,npe
  use specmod, only: jcap
  use hybrid_ensemble_isotropic_global, only: init_sf_xy

  implicit none

  real(r_kind) s_ens_h_gu_x,s_ens_h_gu_y

  if(aniso_a_en) then
     if(mype == 0) write(6,*)' anisotropic option not available yet for hybrid ensemble localization'
     if(mype >  -10) then
       call mpi_finalize(ierror)
       stop
     end if
  end if


!     2.  create grid info for ensemble, including stuff for ensemble sub2grid, grid2sub

  call init_grid_vars_a_en(npe)
  call init_mpi_vars_a_en
  call init_commvars_a_en


!     3.  read/generate ensemble perturbations

  call create_ensemble
  call load_ensemble


!     4.  set up localization filters

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
    call init_sf_xy(jcap)
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

  real(r_kind),intent(in)::s_ens_h
  real(r_kind),intent(out)::s_ens_h_gu_x,s_ens_h_gu_y

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

  use kinds, only: r_kind,i_kind
  use gridmod, only: nlat,nlon,lon2,lat2
  implicit none

  integer(i_kind) nvert,mype
  character(*) fname
  real(r_kind)   f(lat2,lon2,nvert)

  real(r_kind),dimension(nlat,nlon)::work
  real(4) outfield(nlon,nlat)

  character(50) dsname,title,filename
! data dsname/'test.dat'/
  data title/'inmi'/
  character(112) datdes(50000)
  character(1) blank
  data blank/' '/
  data undef/-9.99e33/

  integer(i_kind) i,k,kend,kstart,next,np,ioutdes,ioutdat
  integer(i_kind) last,j,koutmax
  real(4) undef
  real(4) startp,pinc

  if(mype == 0) then
    np=nvert
    startp=1.
    pinc=1.
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
         ltosi,ltosj,iglobal,ijn,displs_g,itotsub
  use mpimod, only: mpi_comm_world,ierror,mpi_rtype,strip
  implicit none

  integer(i_kind), intent(in)::gridpe,mype
  real(r_kind),dimension(lat2,lon2),intent(in):: sub
  real(r_kind),dimension(nlat,nlon),intent(out)::grid

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
