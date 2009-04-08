!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!    &    NASA/GMAO, Global Modeling and Assimilation Office             !
!-------------------------------------------------------------------------
!BOP
!
! !MODULE:  m_gsiBiases --- Module defining bias prediction scheme
!
! !INTERFACE:
!

module m_gsiBiases

! !USES:

  use kinds,only : r_kind
  use kinds,only : i_kind
  use jfunc, only : biascor, bcoption, diurnalbc

  implicit none

! !DESCRIPTION: module defining bias prediction scheme
!
! !REVISION HISTORY:
!
!   2006-10-01  guo     - initial code
!   2006-12-03  todling - add bias_correct interface; add create/destroy
!   2006-12-04  todling - documentation/prologue
!   2007-04-10  todling - bug fix in alloc/dealloc (init/clean)
!   2009-01-20  todling - protect against re-initialization try
!
! !AUTHOR:
!   guo           org: gmao                date: 2006-10-01
!
!EOP
!-------------------------------------------------------------------------

  private
  public :: bias_hour
  public :: nbc

  public :: correct_bias
  public :: update_bias
  public :: compress_bias
  public :: create_bias_grids
  public :: destroy_bias_grids

  public :: bias_ps
  public :: bias_tskin
  public :: bias_vor
  public :: bias_div
  public :: bias_cwmr
  public :: bias_q
  public :: bias_oz
  public :: bias_tv
  public :: bias_u
  public :: bias_v

  integer(i_kind),save :: bias_hour = -1
  integer(i_kind),save :: nbc       = -1

  interface compress_bias; module procedure &
    comp2d_,comp3d_; end interface
  interface update_bias 
    module procedure update2d_,update3d_,update3d3d_,updateall_,update_st
  end interface
  interface correct_bias; module procedure correct_; end interface
  interface create_bias_grids ; module procedure init_   ; end interface
  interface destroy_bias_grids; module procedure clean_  ; end interface

  real(r_kind),allocatable,dimension(:,:,:)   :: bias_ps
  real(r_kind),allocatable,dimension(:,:,:)   :: bias_tskin
  real(r_kind),allocatable,dimension(:,:,:,:) :: bias_vor
  real(r_kind),allocatable,dimension(:,:,:,:) :: bias_div
  real(r_kind),allocatable,dimension(:,:,:,:) :: bias_cwmr
  real(r_kind),allocatable,dimension(:,:,:,:) :: bias_q
  real(r_kind),allocatable,dimension(:,:,:,:) :: bias_oz
  real(r_kind),allocatable,dimension(:,:,:,:) :: bias_tv
  real(r_kind),allocatable,dimension(:,:,:,:) :: bias_u
  real(r_kind),allocatable,dimension(:,:,:,:) :: bias_v

  logical,save:: initialized_=.false.

contains

subroutine init_()

  use gridmod, only: lat2,lon2,nsig
  use constants, only: zero
  implicit none

  integer(i_kind):: istatus
  integer(i_kind):: i,j,k,n

  if (initialized_) return 
  if ( biascor < zero ) return
  nbc = 1
  if (nint(diurnalbc)==1) nbc=3

  allocate(bias_ps(lat2,lon2,nbc),bias_tskin(lat2,lon2,nbc),&
           bias_vor(lat2,lon2,nsig,nbc),&
           bias_div(lat2,lon2,nsig,nbc),bias_cwmr(lat2,lon2,nsig,nbc),&
           bias_oz(lat2,lon2,nsig,nbc),bias_q(lat2,lon2,nsig,nbc),&
           bias_tv(lat2,lon2,nsig,nbc),bias_u(lat2,lon2,nsig,nbc),&
           bias_v(lat2,lon2,nsig,nbc),stat=istatus)
       if (istatus/=0) then
              write(6,*)'CREATE_BIAS_GRIDS:  allocate error5, istatus=',&
              istatus
       endif

       do n=1,nbc
       do j=1,lon2
          do i=1,lat2
             bias_ps   (i,j,n)=zero
             bias_tskin(i,j,n)=zero
          end do
       end do
       end do
       do n=1,nbc
          do k=1,nsig
             do j=1,lon2
                do i=1,lat2
                   bias_vor (i,j,k,n)=zero
                   bias_div (i,j,k,n)=zero
                   bias_tv  (i,j,k,n)=zero
                   bias_q   (i,j,k,n)=zero
                   bias_oz  (i,j,k,n)=zero
                   bias_cwmr(i,j,k,n)=zero
                   bias_u   (i,j,k,n)=zero
                   bias_v   (i,j,k,n)=zero
                end do
             end do
          end do
       end do

initialized_ = .true.

end subroutine init_

subroutine clean_()
   integer(i_kind):: istatus
   if (.not.initialized_) return 
   if ( nbc < 0 ) return
   deallocate(bias_ps,bias_tskin,bias_vor,bias_div,&
              bias_tv,bias_q,bias_oz,bias_cwmr,bias_u,bias_v,stat=istatus)
   write(6,*)'CREATE_BIAS_GRIDS:  deallocate error5, istatus=',istatus
end subroutine clean_

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!    &    NASA/GMAO, Global Modeling and Assimilation Office             !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: comp2d_ --- 2d-interface to bias compression
!
! !INTERFACE:
!

subroutine comp2d_(bias2d_,bias,hour)

! !USES:

   implicit none

! !INPUT PARAMETERS:

   real   (r_kind),dimension(:,:,:),intent(in) :: bias
   integer(i_kind),intent(in) :: hour

! !INPUT/OUTPUT PARAMETERS:

   real   (r_kind),dimension(:,:),intent(inout) :: bias2d_

! !DESCRIPTION: 2d-interface to bias prediction model
!
! !REVISION HISTORY:
!
!   2006-10-01  guo     - initial code
!   2006-12-04  todling - documentation/prologue
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP; Altix Linux
!
! !AUTHOR:
!   guo          org: gmao     date: 2006-10-01
!
!EOP
!-------------------------------------------------------------------------

   real(r_kind) :: twopi
   real(r_kind) :: coshr,sinhr

   twopi=8._r_kind*atan(1._r_kind)
   coshr=1._r_kind*cos(twopi*hour/24._r_kind)*diurnalbc
   sinhr=1._r_kind*sin(twopi*hour/24._r_kind)*diurnalbc

   bias2d_(:,:) = bias(:,:,1) + &
 		  bias(:,:,2)*coshr + &
	 	  bias(:,:,3)*sinhr
end subroutine comp2d_

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!    &    NASA/GMAO, Global Modeling and Assimilation Office             !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: comp3d_ --- 3d-interface to bias compression
!
! !INTERFACE:
!

subroutine comp3d_(bias3d_,bias,hour)

! USES:

   implicit none

! !INPUT PARAMETERS:

   real   (r_kind),dimension(:,:,:,:),intent(in) :: bias
   integer(i_kind),intent(in) :: hour

! !INPUT/OUTPUT PARAMETERS:

   real   (r_kind),dimension(:,:,:),intent(inout) :: bias3d_

! !DESCRIPTION: 3d-interface to bias prediction model
!
! !REVISION HISTORY:
!
!   2006-10-01  guo     - initial code
!   2006-12-04  todling - documentation/prologue
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP; Altix Linux
!
! !AUTHOR:
!   guo          org: gmao     date: 2006-10-01
!
!EOP
!-------------------------------------------------------------------------

   real(r_kind) :: twopi
   real(r_kind) :: coshr,sinhr

   twopi=8._r_kind*atan(1._r_kind)
   coshr=1._r_kind*cos(twopi*hour/24._r_kind)*diurnalbc
   sinhr=1._r_kind*sin(twopi*hour/24._r_kind)*diurnalbc

   bias3d_(:,:,:) = bias(:,:,:,1) + &
	 	    bias(:,:,:,2)*coshr + &
 		    bias(:,:,:,3)*sinhr
end subroutine comp3d_

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!    &    NASA/GMAO, Global Modeling and Assimilation Office             !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: update2d_ --- 2d-interface to bias prediction model
!
! !INTERFACE:
!

subroutine update2d_(bias,lat2,lon2,xhat,hour)

! USES:

  implicit none

! !INPUT PARAMETERS:

  integer(i_kind),intent(in) :: lat2,lon2
  real   (r_kind),dimension(lat2*lon2),intent(in) :: xhat
  integer(i_kind),optional,intent(in) :: hour

! !INPUT/OUTPUT PARAMETERS:

  real   (r_kind),dimension(:,:,:),intent(inout) :: bias

! !DESCRIPTION: 2d-interface to bias prediction model
!
! !REVISION HISTORY:
!
!   2006-10-01  guo     - initial code
!   2006-12-04  todling - documentation/prologue
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP; Altix Linux
!
! !AUTHOR:
!   guo          org: gmao     date: 2006-10-01
!
!EOP
!-------------------------------------------------------------------------

! local variables

  real   (r_kind) :: dumpcor_
  real   (r_kind) :: twopi,coshr,sinhr
  integer(i_kind) :: i,ib,ie

  dumpcor_=1._r_kind
  if (bcoption == 1) dumpcor_=0.98
  if (bcoption == 2) dumpcor_=1.0-0.5*biascor

  if(present(hour)) then
    twopi=8._r_kind*atan(1._r_kind)
    coshr=cos(twopi*hour/24._r_kind)*2._r_kind*biascor
    sinhr=sin(twopi*hour/24._r_kind)*2._r_kind*biascor

    ib=0
    do i=1,lon2
      ie=ib+lat2
      bias(:,i,1)=dumpcor_*bias(:,i,1) + biascor*xhat(ib+1:ie)
      bias(:,i,2)=dumpcor_*bias(:,i,2) +   coshr*xhat(ib+1:ie)
      bias(:,i,3)=dumpcor_*bias(:,i,3) +   sinhr*xhat(ib+1:ie)
      ib=ie
    end do

  else
    ib=0
    do i=1,lon2
      ie=ib+lat2
      bias(:,i,1)=dumpcor_*bias(:,i,1) + biascor*xhat(ib+1:ie)
      ib=ie
    end do
  endif
end subroutine update2d_

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!    &    NASA/GMAO, Global Modeling and Assimilation Office             !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: update3d_ --- 3d-interface to bias prediction model
!
! !INTERFACE:
!

subroutine update3d_(bias,lat2,lon2,nsig,xhat,hour)

! !USES:

  implicit none

! !INPUT PARAMETERS:

  integer(i_kind),intent(in) :: lat2,lon2,nsig
  real   (r_kind),dimension(lat2*lon2*nsig),intent(in) :: xhat
  integer(i_kind),optional,intent(in) :: hour

! !INPUT/OUTPUT PARAMETERS:

  real   (r_kind),dimension(:,:,:,:),intent(inout) :: bias

! !DESCRIPTION: 3d-interface to bias prediction model, w/ xhat as
!               a single-dimension array.
!
! !REVISION HISTORY:
!
!   2006-10-01  guo     - initial code
!   2006-12-04  todling - documentation/prologue
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP; Altix Linux
!
! !AUTHOR:
!   guo          org: gmao     date: 2006-10-01
!
!EOP
!-------------------------------------------------------------------------

! local variables

  real   (r_kind) :: dumpcor_
  real   (r_kind) :: twopi,coshr,sinhr
  integer(i_kind) :: k,i,ib,ie

  dumpcor_=1._r_kind
  if (bcoption == 1) dumpcor_=0.98
  if (bcoption == 2) dumpcor_=1.0-0.5*biascor

  if(present(hour)) then
    twopi=8._r_kind*atan(1._r_kind)
    coshr=cos(twopi*hour/24._r_kind)*2._r_kind*biascor
    sinhr=sin(twopi*hour/24._r_kind)*2._r_kind*biascor

    ib=0
    do k=1,nsig
    do i=1,lon2
      ie=ib+lat2
      bias(:,i,k,1)=dumpcor_*bias(:,i,k,1) + biascor*xhat(ib+1:ie)
      bias(:,i,k,2)=dumpcor_*bias(:,i,k,2) +   coshr*xhat(ib+1:ie)
      bias(:,i,k,3)=dumpcor_*bias(:,i,k,3) +   sinhr*xhat(ib+1:ie)
      ib=ie
    end do
    end do

  else
    ib=0
    do k=1,nsig
    do i=1,lon2
      ie=ib+lat2
      bias(:,i,k,1)=dumpcor_*bias(:,i,k,1) + biascor*xhat(ib+1:ie)
      ib=ie
    end do
    end do
  endif
end subroutine update3d_

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!    &    NASA/GMAO, Global Modeling and Assimilation Office             !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: update3d3d_ --- 3d-interface to bias prediction model
!
! !INTERFACE:
!

subroutine update3d3d_(bias,lat2,lon2,nsig,xhat,hour)

! !USES:

  implicit none

! !INPUT PARAMETERS:

  integer(i_kind),intent(in) :: lat2,lon2,nsig
  real   (r_kind),dimension(:,:,:),intent(in) :: xhat
  integer(i_kind),optional,intent(in) :: hour

! !INPUT/OUTPUT PARAMETERS:

  real   (r_kind),dimension(:,:,:,:),intent(inout) :: bias

! !DESCRIPTION: 3d-interface to bias prediction model
!
! !REVISION HISTORY:
!
!   2006-10-01  guo     - initial code
!   2006-12-04  todling - documentation/prologue
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP; Altix Linux
!
! !AUTHOR:
!   guo          org: gmao     date: 2006-10-01
!
!EOP
!-------------------------------------------------------------------------

! local variables

  real   (r_kind) :: dumpcor_
  real   (r_kind) :: twopi,coshr,sinhr
  integer(i_kind) :: k,i,ib,ie
                                                                                                                                                             
  dumpcor_=1._r_kind
  if (bcoption == 1) dumpcor_=0.98
  if (bcoption == 2) dumpcor_=1.0-0.5*biascor
                                                                                                                                                             
  if(present(hour)) then
    twopi=8._r_kind*atan(1._r_kind)
    coshr=cos(twopi*hour/24._r_kind)*2._r_kind*biascor
    sinhr=sin(twopi*hour/24._r_kind)*2._r_kind*biascor
                                                                                                                                                             
      bias(:,:,:,1)=dumpcor_*bias(:,:,:,1) + biascor*xhat(:,:,:)
      bias(:,:,:,2)=dumpcor_*bias(:,:,:,2) +   coshr*xhat(:,:,:)
      bias(:,:,:,3)=dumpcor_*bias(:,:,:,3) +   sinhr*xhat(:,:,:)
                                                                                                                                                             
  else
      bias(:,:,:,1)=dumpcor_*bias(:,:,:,1) + biascor*xhat(:,:,:)
  endif
end subroutine update3d3d_

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!    &    NASA/GMAO, Global Modeling and Assimilation Office             !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: updateall_ --- update bias in all analysis fields
!
! !INTERFACE:
!

subroutine updateall_ (xhat,xhatuv,xhat_div,xhat_vor,xhat_q,hour)

! !USES:

  use gridmod, only: lat2,lon2,nsig,latlon11,latlon1n

  implicit none

! !INPUT PARAMETERS:

  real(r_kind),dimension(:),intent(in):: xhat
  real(r_kind),dimension(:),intent(in):: xhatuv
  real(r_kind),dimension(:,:,:),intent(in):: xhat_vor,xhat_div,xhat_q
  integer(i_kind),optional,intent(in) :: hour

! !OUTPUT PARAMETERS:

! !DESCRIPTION: 3d-interface to update bias of all fields
!
! !REVISION HISTORY:
!
!   2006-12-04  todling - initial code
!
! !TO DO:
!
!   1. check dims of input xhat arrays
!       real(r_kind),dimension(nclen),intent(inout):: xhat
!       real(r_kind),dimension(2*latlon1n),intent(in):: xhatuv
!       real(r_kind),dimension(lat2,lon2,nsig):: xhat_vor,xhat_div,xhat_q
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP; Altix Linux
!
! !AUTHOR:
!   todling          org: gmao     date: 2006-12-04
!
!EOP
!-------------------------------------------------------------------------

  integer(i_kind) :: l2,l3
  integer(i_kind) :: ncw,nt,np,noz,nsst,nq,nu,nv,nst,nvp
 
  l2=lat2*lon2-1
  l3=lat2*lon2*nsig-1

  nst=1                                  ! streamfunction
  nvp=nst+latlon1n                       ! velocity potential
  nt=nvp +latlon1n                       ! t
  nq=nt  +latlon1n                       ! q
  noz=nq +latlon1n                       ! oz
  ncw=noz+latlon1n                       ! cloud water
  np=ncw +latlon1n                       ! surface pressure
  nsst=np+latlon11                       ! skin temperature

! Define pointers for isolated u,v on subdomains work vector
  nu=1                                   ! zonal wind
  nv=nu+latlon1n                         ! meridional wind

  call update3d_  (bias_u    ,lat2,lon2,nsig,xhatuv(nu:nu+l3)  ,hour)
  call update3d_  (bias_v    ,lat2,lon2,nsig,xhatuv(nv:nv+l3)  ,hour)
  call update3d_  (bias_tv   ,lat2,lon2,nsig,xhat(nt:nt+l3)    ,hour)
  call update3d_  (bias_q    ,lat2,lon2,nsig,xhat_q            ,hour)
  call update3d_  (bias_oz   ,lat2,lon2,nsig,xhat(noz:noz+l3)  ,hour)
  call update3d_  (bias_cwmr ,lat2,lon2,nsig,xhat(ncw:ncw+l3)  ,hour)
  call update3d3d_(bias_vor  ,lat2,lon2,nsig,xhat_div          ,hour)
  call update3d3d_(bias_div  ,lat2,lon2,nsig,xhat_vor          ,hour)
  call update2d_  (bias_ps   ,lat2,lon2     ,xhat(np:np+l2)    ,hour)
  call update2d_  (bias_tskin,lat2,lon2     ,xhat(nsst:nsst+l2),hour)

end subroutine updateall_

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!    &    NASA/GMAO, Global Modeling and Assimilation Office             !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: update_st   --- update bias in all analysis fields
!
! !INTERFACE:
!

subroutine update_st(xhat,xhat_div,xhat_vor,hour)

  use gridmod, only: lat2,lon2,nsig
  use state_vectors

  implicit none

  type(state_vector),intent(in):: xhat
  real(r_kind),dimension(:,:,:),intent(in):: xhat_vor,xhat_div
  integer(i_kind),optional,intent(in) :: hour

! !DESCRIPTION: state vector interface to update bias of all fields
!
! !REVISION HISTORY:
!
!   2007-04-13  tremolet - initial code
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP; Altix Linux
!
!EOP
!-------------------------------------------------------------------------
 
  call update3d_  (bias_u    ,lat2,lon2,nsig,xhat%u  ,hour)
  call update3d_  (bias_v    ,lat2,lon2,nsig,xhat%v  ,hour)
  call update3d_  (bias_tv   ,lat2,lon2,nsig,xhat%t  ,hour)
  call update3d_  (bias_q    ,lat2,lon2,nsig,xhat%q  ,hour)
  call update3d_  (bias_oz   ,lat2,lon2,nsig,xhat%oz ,hour)
  call update3d_  (bias_cwmr ,lat2,lon2,nsig,xhat%cw ,hour)
  call update3d3d_(bias_vor  ,lat2,lon2,nsig,xhat_div,hour)
  call update3d3d_(bias_div  ,lat2,lon2,nsig,xhat_vor,hour)
  call update2d_  (bias_ps   ,lat2,lon2     ,xhat%p  ,hour)
  call update2d_  (bias_tskin,lat2,lon2     ,xhat%sst,hour)

end subroutine update_st

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!    &    NASA/GMAO, Global Modeling and Assimilation Office             !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: updateall_ --- update bias in all analysis fields
!
! !INTERFACE:
!

subroutine correct_()

! !USES:

  use gridmod, only: lat2,lon2,nsig,latlon1n
  use guess_grids, only: nfldsig,ntguessig
  use guess_grids, only: ges_z,ges_ps,ges_u,ges_v,ges_vor,ges_div,&
                         ges_tv,ges_q,ges_oz,ges_cwmr,sfct
  use constants, only: zero,tiny_r_kind

  implicit none

! !INPUT PARAMETERS:
! !OUTPUT PARAMETERS:
! !DESCRIPTION: correct background field with bias estimate
!
! !REVISION HISTORY:
!
!   2006-12-04  todling - initial code
!
! !TO DO:
!
!   1. check dims of input xhat arrays
!       real(r_kind),dimension(nclen),intent(inout):: xhat
!       real(r_kind),dimension(2*latlon1n),intent(in):: xhatuv
!       real(r_kind),dimension(lat2,lon2,nsig):: xhat_vor,xhat_div,xhat_q
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP; Altix Linux
!
! !AUTHOR:
!   todling          org: gmao     date: 2006-12-04
!
!EOP
!-------------------------------------------------------------------------
! local variables

  real(r_kind),allocatable,dimension(:,:)  :: b_ps
  real(r_kind),allocatable,dimension(:,:)  :: b_tskin
  real(r_kind),allocatable,dimension(:,:,:):: b_vor
  real(r_kind),allocatable,dimension(:,:,:):: b_div
  real(r_kind),allocatable,dimension(:,:,:):: b_cwmr
  real(r_kind),allocatable,dimension(:,:,:):: b_q
  real(r_kind),allocatable,dimension(:,:,:):: b_oz
  real(r_kind),allocatable,dimension(:,:,:):: b_tv
  real(r_kind),allocatable,dimension(:,:,:):: b_u
  real(r_kind),allocatable,dimension(:,:,:):: b_v

  integer(i_kind),allocatable,dimension(:) :: hours
  integer(i_kind) :: i,j,k,it

! Get memory for bias-related arrays

  allocate(hours(nfldsig))
  allocate(b_ps(lat2,lon2),b_tskin(lat2,lon2),b_vor(lat2,lon2,nsig),&
           b_div(lat2,lon2,nsig),b_cwmr(lat2,lon2,nsig),&
           b_oz(lat2,lon2,nsig),b_q(lat2,lon2,nsig),&
           b_tv(lat2,lon2,nsig),b_u(lat2,lon2,nsig),b_v(lat2,lon2,nsig))

  do it=1,nfldsig
     call comp2d_(b_ps   ,bias_ps   ,hours(it))
     call comp2d_(b_tskin,bias_tskin,hours(it))
     call comp3d_(b_u    ,bias_u    ,hours(it))
     call comp3d_(b_v    ,bias_v    ,hours(it))
     call comp3d_(b_tv   ,bias_tv   ,hours(it))
     call comp3d_(b_vor  ,bias_vor  ,hours(it))
     call comp3d_(b_div  ,bias_div  ,hours(it))
     call comp3d_(b_cwmr ,bias_cwmr ,hours(it))
     call comp3d_(b_q    ,bias_q    ,hours(it))
     call comp3d_(b_oz   ,bias_oz   ,hours(it))
                                                                                                               
     bias_hour=hours(ntguessig)

     do j=1,lon2
        do i=1,lat2
           ges_ps(i,j,it)= ges_ps(i,j,it) + b_ps(i,j)
        end do
     end do
     do k=1,nsig
        do j=1,lon2
           do i=1,lat2
              ges_u(i,j,k,it)   =                 ges_u(i,j,k,it)    + b_u(i,j,k)
              ges_v(i,j,k,it)   =                 ges_v(i,j,k,it)    + b_v(i,j,k)
              ges_vor(i,j,k,it) =                 ges_vor(i,j,k,it)  + b_vor(i,j,k)
              ges_div(i,j,k,it) =                 ges_div(i,j,k,it)  + b_div(i,j,k)
              ges_cwmr(i,j,k,it)=                 ges_cwmr(i,j,k,it) + b_cwmr(i,j,k)
              ges_q(i,j,k,it)   = max(tiny_r_kind,ges_q(i,j,k,it)    + b_q(i,j,k))
              ges_oz(i,j,k,it)  = max(tiny_r_kind,ges_oz(i,j,k,it)   + b_oz(i,j,k))
              ges_tv(i,j,k,it)  = max(tiny_r_kind,ges_tv(i,j,k,it)   + b_tv(i,j,k))
           end do
        end do
     end do
     do j=1,lon2
        do i=1,lat2
           sfct(i,j,it)= sfct(i,j,it) + b_tskin(i,j)
        end do
     end do
  end do

! Clean up bias-related arrays

  deallocate(hours)
  deallocate(b_ps,b_tskin,b_vor,b_div,b_cwmr,b_oz,b_q,b_tv,b_u,b_v)

end subroutine correct_

end module m_gsiBiases
