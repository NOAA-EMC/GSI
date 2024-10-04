module ncep_pertmod

use gsi_4dvar, only: idmodel
implicit none

private

public :: ncep_4dmodel_tl
public :: ncep_4dmodel_ad

interface ncep_4dmodel_tl
          module procedure n4dmodel_tl_
end interface
interface ncep_4dmodel_ad
          module procedure n4dmodel_ad_
end interface

character(len=*),parameter::myname='ncep_pertmod'
logical::verbose_=.true.

contains

subroutine n4dmodel_tl_ (p_xi,yy,nymdi,nhmsi,ndt,rc)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    ncep_4dmodel_tl      tlm entry for for 4dvar
!   prgmmr: rancic                          date: 2011-01-21
!
! abstract: 
!
! program history log:
!   2011-01-21  rancic - update to use gsi_bundle
!   2011-05-21  todling - add calls to getprs,t2tsen; encapsulate in module;
!                         revisit trajectory counters
!
!   input argument list:
!     xx      - model variables 
!
!   output argument list:      
!     xx       - model variables 
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: i_kind,r_kind
  use mpimod, only: mype
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use gsi_bundlemod, only: assignment(=)
  use guess_grids, only: ges_z,ntguessig
  use tends4pertmod, only: istep4pert
  use mpeu_util, only: die
  implicit none
  
! Declare passed variables  
  type(gsi_bundle),               pointer:: p_xi ! input gsi_bundle, maybe null()
  type(gsi_bundle), intent(inout),target :: yy   ! output gsi_bundle
  integer(i_kind),intent(in)     :: nymdi ! Date of initial perturbation, as in YYYYMMDD
  integer(i_kind),intent(in)     :: nhmsi ! Time of initial perturbation, as in HHMMSS
  integer(i_kind),intent(in)     :: ndt   ! number of time steps to integrate
  integer(i_kind),intent(out)    :: rc    ! error return code
! Declare local variables
  character(len=*),parameter::myname_=myname//'n4dmodel_tl_'
  integer(i_kind)                :: indx_min   ! Lower index for TLM integration 
  integer(i_kind)                :: indx_max   ! Upper index for TLM integration 
  integer(i_kind) ier,istatus
  real(r_kind),pointer,dimension(:,:,:) :: u
  real(r_kind),pointer,dimension(:,:,:) :: v
  real(r_kind),pointer,dimension(:,:,:) :: tv
  real(r_kind),pointer,dimension(:,:,:) :: q
  real(r_kind),pointer,dimension(:,:,:) :: oz
  real(r_kind),pointer,dimension(:,:,:) :: cw
  real(r_kind),pointer,dimension(:,:,:) :: p3d
  real(r_kind),pointer,dimension(:,:,:) :: tsen
  real(r_kind),pointer,dimension(:,:)   :: ps

  integer(i_kind) it

!******************************************************************************

!   Get required pointers

    rc=0
    if(idmodel) return

    ier=0
    call gsi_bundlegetpointer(yy,'u',   u,  istatus);ier=istatus+ier
    call gsi_bundlegetpointer(yy,'v',   v,  istatus);ier=istatus+ier
    call gsi_bundlegetpointer(yy,'tv',  tv, istatus);ier=istatus+ier
    call gsi_bundlegetpointer(yy,'q',   q,  istatus);ier=istatus+ier
    call gsi_bundlegetpointer(yy,'oz',  oz, istatus);ier=istatus+ier
    call gsi_bundlegetpointer(yy,'cw',  cw, istatus);ier=istatus+ier
    call gsi_bundlegetpointer(yy,'ps',  ps, istatus);ier=istatus+ier

    if(ier/=0) then
       call die(myname_,'invalid pointer(1)',ier)
    endif

    it=ntguessig
    istep4pert=istep4pert+1
    indx_min=istep4pert
    indx_max=istep4pert+ndt-1
   if(verbose_.and.mype==0) print *, 'TL istep4pert, ndt, indx_min, indx_max = ', istep4pert, ndt, indx_min, indx_max

    call gsi_model_tl(u,v,tv,q,oz,cw,ps,ges_z(1,1,it),mype,indx_min,indx_max)

    call gsi_bundlegetpointer(yy,'tsen', tsen, istatus);ier=istatus+ier
    call gsi_bundlegetpointer(yy,'p3d',   p3d, istatus);ier=istatus+ier
    if(ier/=0) then
       call die(myname_,'invalid pointer(2)',ier)
    endif

    call getprs_tl (ps,tv,p3d)
    call tv_to_tsen(tv,q,tsen)

    istep4pert=istep4pert+ndt-1

  return
end subroutine n4dmodel_tl_

subroutine n4dmodel_ad_(xx,p_yi,nymdi,nhmsi,ndt,rc)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    ncep_4dmodel_tl      tlm entry for for 4dvar
!   prgmmr: rancic                          date: 2011-01-21
!
! abstract: 
!
! program history log:
!   2011-01-21  rancic - update to use gsi_bundle
!   2011-05-21  todling - add calls to getprs,t2tsen; encapsulate in module;
!                         revisit trajectory counters
!
!   input argument list:
!     xx      - model variables 
!
!   output argument list:      
!     xx       - model variables 
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: i_kind,r_kind
  use mpimod, only: mype
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use gsi_bundlemod, only: assignment(=)
  use guess_grids, only: ges_z,ntguessig
  use tends4pertmod, only: istep4pert,itime_max
  use mpeu_util, only: die
  implicit none
  
! Declare passed variables  
  type(gsi_bundle), intent(inout),target ::   xx ! output gsi_bundle
  type(gsi_bundle),               pointer:: p_yi ! input gsi_bundle, maybe null()
  integer(i_kind),intent(in)     :: nymdi ! Date of initial perturbation, as in YYYYMMDD
  integer(i_kind),intent(in)     :: nhmsi ! Time of initial perturbation, as in HHMMSS
  integer(i_kind),intent(in)     :: ndt   ! number of time steps to integrate
  integer(i_kind),intent(out)    :: rc    ! error return code
! Declare local variables
  character(len=*),parameter::myname_=myname//'n4dmodel_ad_'
  integer(i_kind)                :: indx_min   ! Lower index for TLM integration 
  integer(i_kind)                :: indx_max   ! Upper index for TLM integration 
  integer(i_kind) ier,istatus
  real(r_kind),pointer,dimension(:,:,:) :: u
  real(r_kind),pointer,dimension(:,:,:) :: v
  real(r_kind),pointer,dimension(:,:,:) :: tv
  real(r_kind),pointer,dimension(:,:,:) :: q
  real(r_kind),pointer,dimension(:,:,:) :: oz
  real(r_kind),pointer,dimension(:,:,:) :: cw
  real(r_kind),pointer,dimension(:,:,:) :: p3d
  real(r_kind),pointer,dimension(:,:,:) :: tsen
  real(r_kind),pointer,dimension(:,:)   :: ps

  integer(i_kind) it


!******************************************************************************

!   Get required pointers

    rc=0

    if(idmodel) return

    ier=0
    call gsi_bundlegetpointer(xx,'u',   u,   istatus);ier=istatus+ier
    call gsi_bundlegetpointer(xx,'v',   v,   istatus);ier=istatus+ier
    call gsi_bundlegetpointer(xx,'tv',  tv,  istatus);ier=istatus+ier
    call gsi_bundlegetpointer(xx,'q',   q,   istatus);ier=istatus+ier
    call gsi_bundlegetpointer(xx,'oz',  oz,  istatus);ier=istatus+ier
    call gsi_bundlegetpointer(xx,'cw',  cw,  istatus);ier=istatus+ier
    call gsi_bundlegetpointer(xx,'ps',  ps,  istatus);ier=istatus+ier
    call gsi_bundlegetpointer(xx,'tsen',tsen,istatus);ier=istatus+ier
    call gsi_bundlegetpointer(xx,'p3d', p3d, istatus);ier=istatus+ier

    if(ier/=0) then
       call die(myname_,'invalid pointer',ier)
    endif

    it=ntguessig
    indx_max=itime_max-istep4pert
    indx_min=indx_max-ndt+1
    if(verbose_.and.mype==0) print *, 'AD istep4pert, ndt, indx_min, indx_max = ', istep4pert, ndt, indx_min, indx_max

    call tv_to_tsen_ad(tv,q,tsen)
    call getprs_ad    (ps,tv,p3d)

    call gsi_model_ad(u,v,tv,q,oz,cw,ps,ges_z(1,1,it),mype,indx_min,indx_max)

    istep4pert=istep4pert+ndt

    rc=0

  return
  end subroutine n4dmodel_ad_

end module ncep_pertmod
