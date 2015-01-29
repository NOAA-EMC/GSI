module m_gsiBiases
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    module m_gsiBiases
!   prgmmr:      j guo <jguo@nasa.gov>
!      org:      NASA/GSFC, Global Modeling and Assimilation Office, 900.3
!     date:      2010-03-24
!
! abstract: background bias estimation/correction
!
! program history log:
!   2010-03-24  j guo   - added this document block
!   2011-08-01  lueken  - replaced F90 with f90, replace izero/ione with 0/1, remove _i_kind
!   2012-04-06  todling - change all refs from cvars to svars (never to depend on c-vec)
!
!   input argument list: see Fortran 90 style document below
!
!   output argument list: see Fortran 90 style document below
!
! attributes:
!   language: Fortran 90 and/or above
!   machine:
!
!$$$  end subprogram documentation block

! module interface:

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
!   2013-10-19  todling - metguess now holds background
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
  public :: bias_gust
  public :: bias_vis
  public :: bias_pblh

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
  real(r_kind),allocatable,dimension(:,:,:)   :: bias_gust
  real(r_kind),allocatable,dimension(:,:,:)   :: bias_vis
  real(r_kind),allocatable,dimension(:,:,:)   :: bias_pblh
  real(r_kind),allocatable,dimension(:,:,:,:) :: bias_vor
  real(r_kind),allocatable,dimension(:,:,:,:) :: bias_div
  real(r_kind),allocatable,dimension(:,:,:,:) :: bias_cwmr
  real(r_kind),allocatable,dimension(:,:,:,:) :: bias_q
  real(r_kind),allocatable,dimension(:,:,:,:) :: bias_oz
  real(r_kind),allocatable,dimension(:,:,:,:) :: bias_tv
  real(r_kind),allocatable,dimension(:,:,:,:) :: bias_u
  real(r_kind),allocatable,dimension(:,:,:,:) :: bias_v

  logical,save:: initialized_=.false.

  character(len=*),parameter::myname='m_gsiBiases'
contains

subroutine init_()
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

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
           bias_gust(lat2,lon2,nbc),bias_vis(lat2,lon2,nbc),&
           bias_pblh(lat2,lon2,nbc),bias_vor(lat2,lon2,nsig,nbc),&
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
           bias_gust (i,j,n)=zero
           bias_vis  (i,j,n)=zero
           bias_pblh (i,j,n)=zero
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
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    clean_
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

   implicit none

   integer(i_kind):: istatus

   if (.not.initialized_) return 
   if ( nbc < 0 ) return
   deallocate(bias_ps,bias_tskin,bias_gust,bias_vis,bias_pblh,bias_vor,bias_div,&
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

   use constants, only: one
   implicit none

! !INPUT PARAMETERS:

   real   (r_kind),dimension(:,:,:),intent(in   ) :: bias
   integer(i_kind)                 ,intent(in   ) :: hour

! !INPUT/OUTPUT PARAMETERS:

   real   (r_kind),dimension(:,:)  ,intent(inout) :: bias2d_

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

   twopi=8._r_kind*atan(one)
   coshr=one*cos(twopi*hour/24._r_kind)*diurnalbc
   sinhr=one*sin(twopi*hour/24._r_kind)*diurnalbc

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

   use constants, only: one
   implicit none

! !INPUT PARAMETERS:

   real   (r_kind),dimension(:,:,:,:),intent(in   ) :: bias
   integer(i_kind)                   ,intent(in   ) :: hour

! !INPUT/OUTPUT PARAMETERS:

   real   (r_kind),dimension(:,:,:)  ,intent(inout) :: bias3d_

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

   twopi=8._r_kind*atan(one)
   coshr=one*cos(twopi*hour/24._r_kind)*diurnalbc
   sinhr=one*sin(twopi*hour/24._r_kind)*diurnalbc

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

  use constants, only: half,one,two
  implicit none

! !INPUT PARAMETERS:

  integer(i_kind)                     ,intent(in   ) :: lat2,lon2
  real   (r_kind),dimension(lat2*lon2),intent(in   ) :: xhat
  integer(i_kind),optional            ,intent(in   ) :: hour

! !INPUT/OUTPUT PARAMETERS:

  real   (r_kind),dimension(:,:,:)    ,intent(inout) :: bias

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

  dumpcor_=one
  if (bcoption == 1) dumpcor_=0.98_r_kind
  if (bcoption == 2) dumpcor_=one-half*biascor

  if(present(hour)) then
     twopi=8._r_kind*atan(one)
     coshr=cos(twopi*hour/24._r_kind)*two*biascor
     sinhr=sin(twopi*hour/24._r_kind)*two*biascor

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

  use constants, only: half,one,two
  implicit none

! !INPUT PARAMETERS:

  integer(i_kind)                          ,intent(in   ) :: lat2,lon2,nsig
  real   (r_kind),dimension(lat2*lon2*nsig),intent(in   ) :: xhat
  integer(i_kind),optional                 ,intent(in   ) :: hour

! !INPUT/OUTPUT PARAMETERS:

  real   (r_kind),dimension(:,:,:,:)       ,intent(inout) :: bias

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

  dumpcor_=one
  if (bcoption == 1) dumpcor_=0.98_r_kind
  if (bcoption == 2) dumpcor_=one-half*biascor

  if(present(hour)) then
     twopi=8._r_kind*atan(one)
     coshr=cos(twopi*hour/24._r_kind)*two*biascor
     sinhr=sin(twopi*hour/24._r_kind)*two*biascor
 
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

subroutine update3d3d_(bias,xhat,hour)

! !USES:

  use constants, only: half,one,two
  implicit none

! !INPUT PARAMETERS:

  real   (r_kind),dimension(:,:,:)  ,intent(in   ) :: xhat
  integer(i_kind),optional          ,intent(in   ) :: hour

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
                                                                                                                                                             
  dumpcor_=one
  if (bcoption == 1) dumpcor_=0.98_r_kind
  if (bcoption == 2) dumpcor_=one-half*biascor
                                                                                                                                                             
  if(present(hour)) then
     twopi=8._r_kind*atan(one)
     coshr=cos(twopi*hour/24._r_kind)*two*biascor
     sinhr=sin(twopi*hour/24._r_kind)*two*biascor
                                                                                                                                                             
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

  real(r_kind),dimension(:)    ,intent(in   ) :: xhat
  real(r_kind),dimension(:)    ,intent(in   ) :: xhatuv
  real(r_kind),dimension(:,:,:),intent(in   ) :: xhat_vor,xhat_div,xhat_q
  integer(i_kind),optional     ,intent(in   ) :: hour

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

  nst=1                               ! streamfunction
  nvp=nst+latlon1n                    ! velocity potential
  nt=nvp +latlon1n                    ! t
  nq=nt  +latlon1n                    ! q
  noz=nq +latlon1n                    ! oz
  ncw=noz+latlon1n                    ! cloud water
  np=ncw +latlon1n                    ! surface pressure
  nsst=np+latlon11                    ! skin temperature

! Define pointers for isolated u,v on subdomains work vector
  nu=1                                ! zonal wind
  nv=nu+latlon1n                      ! meridional wind

  call update3d_  (bias_u    ,lat2,lon2,nsig,xhatuv(nu:nu+l3)  ,hour)
  call update3d_  (bias_v    ,lat2,lon2,nsig,xhatuv(nv:nv+l3)  ,hour)
  call update3d_  (bias_tv   ,lat2,lon2,nsig,xhat(nt:nt+l3)    ,hour)
  call update3d_  (bias_q    ,lat2,lon2,nsig,xhat_q            ,hour)
  call update3d_  (bias_oz   ,lat2,lon2,nsig,xhat(noz:noz+l3)  ,hour)
  call update3d_  (bias_cwmr ,lat2,lon2,nsig,xhat(ncw:ncw+l3)  ,hour)
  call update3d3d_(bias_vor  ,               xhat_div          ,hour)
  call update3d3d_(bias_div  ,               xhat_vor          ,hour)
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

  use gridmod, only: lat2,lon2,nsig,twodvar_regional
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer

  implicit none

  type(gsi_bundle)             ,intent(in   ) :: xhat
  real(r_kind),dimension(:,:,:),intent(in   ) :: xhat_vor,xhat_div
  integer(i_kind),optional     ,intent(in   ) :: hour

! !DESCRIPTION: state vector interface to update bias of all fields
!
! !REVISION HISTORY:
!
!   2007-04-13  tremolet - initial code
!   2010-05-13  todling  - update to use gsi_bundle (not fully up-to-date)
!   2011-02-11  zhu      - add gust,vis,pblh
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP; Altix Linux
!
!EOP
!-------------------------------------------------------------------------

   character(len=*),parameter::myname_='update_st'
   integer(i_kind) ier,istatus
   integer(i_kind) i_gust,i_vis,i_pblh
   real(r_kind),pointer,dimension(:,:)   :: sv_ps,sv_sst,sv_gust,sv_vis,sv_pblh
   real(r_kind),pointer,dimension(:,:,:) :: sv_u,sv_v,sv_q,sv_tv,sv_oz,sv_cw
!  real(r_kind),pointer,dimension(:,:,:) :: sv_prse,sv_tsen

!  Get pointers to require state variables
   ier=0
   call gsi_bundlegetpointer (xhat,'u'   ,sv_u,   istatus); ier=istatus+ier
   call gsi_bundlegetpointer (xhat,'v'   ,sv_v,   istatus); ier=istatus+ier
   call gsi_bundlegetpointer (xhat,'tv'  ,sv_tv,  istatus); ier=istatus+ier
   call gsi_bundlegetpointer (xhat,'q'   ,sv_q ,  istatus); ier=istatus+ier
   call gsi_bundlegetpointer (xhat,'oz'  ,sv_oz , istatus); ier=istatus+ier
   call gsi_bundlegetpointer (xhat,'cw'  ,sv_cw , istatus); ier=istatus+ier
   call gsi_bundlegetpointer (xhat,'ps'  ,sv_ps,  istatus); ier=istatus+ier
!  call gsi_bundlegetpointer (xhat,'prse',sv_prse,istatus); ier=istatus+ier
!  call gsi_bundlegetpointer (xhat,'tsen',sv_tsen,istatus); ier=istatus+ier
   call gsi_bundlegetpointer (xhat,'sst' ,sv_sst, istatus); ier=istatus+ier
   if (twodvar_regional) then
      call gsi_bundlegetpointer (xhat,'gust' ,i_gust, istatus)
      if (i_gust>0) then
         call gsi_bundlegetpointer (xhat,'gust' ,sv_gust, istatus); ier=istatus+ier
      end if

      call gsi_bundlegetpointer (xhat,'vis' ,i_vis, istatus)
      if (i_vis>0) then
         call gsi_bundlegetpointer (xhat,'vis' ,sv_vis, istatus); ier=istatus+ier
      end if

      call gsi_bundlegetpointer (xhat,'pblh' ,i_pblh, istatus)
      if (i_pblh>0) then
         call gsi_bundlegetpointer (xhat,'pblh' ,sv_pblh, istatus); ier=istatus+ier
      end if
   end if
   if(ier/=0) then
      write(6,*) trim(myname_), ': trouble getting SV pointers, ier=',ier
      call stop2(999)
   endif

 
  call update3d_  (bias_u    ,lat2,lon2,nsig,sv_u    ,hour)
  call update3d_  (bias_v    ,lat2,lon2,nsig,sv_v    ,hour)
  call update3d_  (bias_tv   ,lat2,lon2,nsig,sv_tv   ,hour)
  call update3d_  (bias_q    ,lat2,lon2,nsig,sv_q    ,hour)
  call update3d_  (bias_oz   ,lat2,lon2,nsig,sv_oz   ,hour)
  call update3d_  (bias_cwmr ,lat2,lon2,nsig,sv_cw   ,hour)
  call update3d3d_(bias_vor  ,               xhat_div,hour)
  call update3d3d_(bias_div  ,               xhat_vor,hour)
  call update2d_  (bias_ps   ,lat2,lon2     ,sv_ps   ,hour)
  call update2d_  (bias_tskin,lat2,lon2     ,sv_sst  ,hour)
  if (twodvar_regional) then
     if (i_gust>0) call update2d_  (bias_gust,lat2,lon2,sv_gust,hour)
     if (i_vis>0 ) call update2d_  (bias_vis ,lat2,lon2,sv_vis ,hour)
     if (i_pblh>0) call update2d_  (bias_pblh,lat2,lon2,sv_pblh,hour)
  end if

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

  use gridmod, only: lat2,lon2,nsig,latlon1n,twodvar_regional
  use guess_grids, only: nfldsig,ntguessig
  use guess_grids, only: sfct
  use constants, only: tiny_r_kind
  use gsi_metguess_mod, only: gsi_metguess_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use mpeu_util, only: die
  use state_vectors, only: svars2d
  use mpeu_util, only: getindex

  implicit none

! !INPUT PARAMETERS:
! !OUTPUT PARAMETERS:
! !DESCRIPTION: correct background field with bias estimate
!
! !REVISION HISTORY:
!
!   2006-12-04  todling - initial code
!   2011-02-11  zhu     - add gust,vis,pblh
!   2011-05-01  todling - cwmr no longer in guess-grids; use metguess bundle now
!
! !TO DO:
!
!   1. check dims of input xhat arrays
!       real(r_kind),dimension(nclen)     ,intent(inout) :: xhat
!       real(r_kind),dimension(2*latlon1n),intent(in   ) :: xhatuv

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

  character(len=*),parameter::myname_=myname//'correct_'

  real(r_kind),pointer,dimension(:,:  )  :: ptr2dges   =>NULL()
  real(r_kind),pointer,dimension(:,:  )  :: ges_ps_it  =>NULL()
  real(r_kind),pointer,dimension(:,:,:)  :: ges_u_it   =>NULL()
  real(r_kind),pointer,dimension(:,:,:)  :: ges_v_it   =>NULL()
  real(r_kind),pointer,dimension(:,:,:)  :: ges_div_it =>NULL()
  real(r_kind),pointer,dimension(:,:,:)  :: ges_vor_it =>NULL()
  real(r_kind),pointer,dimension(:,:,:)  :: ges_tv_it  =>NULL()
  real(r_kind),pointer,dimension(:,:,:)  :: ges_q_it   =>NULL()
  real(r_kind),pointer,dimension(:,:,:)  :: ges_oz_it  =>NULL()
  real(r_kind),pointer,dimension(:,:,:)  :: ges_cwmr_it=>NULL()

  real(r_kind),allocatable,dimension(:,:)  :: b_ps
  real(r_kind),allocatable,dimension(:,:)  :: b_tskin
  real(r_kind),allocatable,dimension(:,:)  :: b_gust
  real(r_kind),allocatable,dimension(:,:)  :: b_vis
  real(r_kind),allocatable,dimension(:,:)  :: b_pblh
  real(r_kind),allocatable,dimension(:,:,:):: b_vor
  real(r_kind),allocatable,dimension(:,:,:):: b_div
  real(r_kind),allocatable,dimension(:,:,:):: b_cwmr
  real(r_kind),allocatable,dimension(:,:,:):: b_q
  real(r_kind),allocatable,dimension(:,:,:):: b_oz
  real(r_kind),allocatable,dimension(:,:,:):: b_tv
  real(r_kind),allocatable,dimension(:,:,:):: b_u
  real(r_kind),allocatable,dimension(:,:,:):: b_v

  integer(i_kind),allocatable,dimension(:) :: hours
  integer(i_kind) :: i,j,k,it,ier,istatus

! Get memory for bias-related arrays

  allocate(hours(nfldsig))
  allocate(b_ps(lat2,lon2),b_tskin(lat2,lon2),b_gust(lat2,lon2),&
           b_vis(lat2,lon2),b_pblh(lat2,lon2),b_vor(lat2,lon2,nsig),&
           b_div(lat2,lon2,nsig),b_cwmr(lat2,lon2,nsig),&
           b_oz(lat2,lon2,nsig),b_q(lat2,lon2,nsig),&
           b_tv(lat2,lon2,nsig),b_u(lat2,lon2,nsig),b_v(lat2,lon2,nsig))

  do it=1,nfldsig

!    Get pointer to could water mixing ratio
     ier=0
     call gsi_bundlegetpointer (gsi_metguess_bundle(it),'ps',ges_ps_it,istatus)
     ier=ier+istatus
     call gsi_bundlegetpointer (gsi_metguess_bundle(it),'u',ges_u_it,istatus)
     ier=ier+istatus
     call gsi_bundlegetpointer (gsi_metguess_bundle(it),'v',ges_v_it,istatus)
     ier=ier+istatus
     call gsi_bundlegetpointer (gsi_metguess_bundle(it),'div',ges_div_it,istatus)
     ier=ier+istatus
     call gsi_bundlegetpointer (gsi_metguess_bundle(it),'vor',ges_vor_it,istatus)
     ier=ier+istatus
     call gsi_bundlegetpointer (gsi_metguess_bundle(it),'tv',ges_tv_it,istatus)
     ier=ier+istatus
     call gsi_bundlegetpointer (gsi_metguess_bundle(it),'q',ges_q_it,istatus)
     ier=ier+istatus
     call gsi_bundlegetpointer (gsi_metguess_bundle(it),'oz',ges_oz_it,istatus)
     ier=ier+istatus
     call gsi_bundlegetpointer (gsi_metguess_bundle(it),'cw',ges_cwmr_it,istatus)
     ier=ier+istatus
     if (ier/=0) call die(trim(myname_),'cannot get pointers for bias correction, ier =',ier)

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

     if (twodvar_regional) then
        if (getindex(svars2d,'gust')>0) &
        call comp2d_(b_gust,bias_gust,hours(it))
        if (getindex(svars2d,'vis')>0) &
        call comp2d_(b_vis ,bias_vis ,hours(it))
        if (getindex(svars2d,'pblh')>0) &
        call comp2d_(b_pblh,bias_pblh,hours(it))
     end if

     bias_hour=hours(ntguessig)

     do j=1,lon2
        do i=1,lat2
           ges_ps_it(i,j)= ges_ps_it(i,j) + b_ps(i,j)
        end do
     end do
     do k=1,nsig
        do j=1,lon2
           do i=1,lat2
              ges_u_it(i,j,k)   =                 ges_u_it(i,j,k)    + b_u(i,j,k)
              ges_v_it(i,j,k)   =                 ges_v_it(i,j,k)    + b_v(i,j,k)
              ges_vor_it(i,j,k) =                 ges_vor_it(i,j,k)  + b_vor(i,j,k)
              ges_div_it(i,j,k) =                 ges_div_it(i,j,k)  + b_div(i,j,k)
              ges_cwmr_it(i,j,k)=                 ges_cwmr_it(i,j,k) + b_cwmr(i,j,k)
              ges_q_it(i,j,k)   = max(tiny_r_kind,ges_q_it(i,j,k)    + b_q(i,j,k))
              ges_oz_it(i,j,k)  = max(tiny_r_kind,ges_oz_it(i,j,k)   + b_oz(i,j,k))
              ges_tv_it(i,j,k)  = max(tiny_r_kind,ges_tv_it(i,j,k)   + b_tv(i,j,k))
           end do
        end do
     end do
     do j=1,lon2
        do i=1,lat2
           sfct(i,j,it)= sfct(i,j,it) + b_tskin(i,j)
        end do
     end do

     if (twodvar_regional) then
        call gsi_bundlegetpointer (gsi_metguess_bundle(it),'gust',ptr2dges,ier)
        if (ier==0.and.getindex(svars2d,'gust')>0) then
           ptr2dges = ptr2dges + b_gust
        end if
        call gsi_bundlegetpointer (gsi_metguess_bundle(it),'vis',ptr2dges,ier)
        if (ier==0.and.getindex(svars2d,'vis')>0) then
           ptr2dges = ptr2dges + b_vis
        end if
        call gsi_bundlegetpointer (gsi_metguess_bundle(it),'pblh',ptr2dges,ier)
        if (ier==0.and.getindex(svars2d,'pblh')>0) then
           ptr2dges = ptr2dges + b_pblh
        end if
     end if
  end do

! Clean up bias-related arrays

  deallocate(hours)
  deallocate(b_ps,b_tskin,b_gust,b_vis,b_pblh,b_vor,b_div,b_cwmr,b_oz,b_q,b_tv,b_u,b_v)

end subroutine correct_

end module m_gsiBiases
