module nonlinmod
!$$$  module documentation block
!                .      .    .                                       .
! module:    nonlinmod       contains stuff for nonlinear model i/o
!   prgmmr: rancic                                    
!
! abstract: allocate memory for i/o of the nonlinear model for 4dvar
!
! program history log:
!   2010-02-25  rancic

! subroutines included:
!   sub create_nonlinvars      - allocate related variables
!   sub destroy_nonlinvars     - delallocate relate variables
!
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: i_kind,r_kind
  use tends4pertmod, only: time_step,time_step_half,itime_max,itime_out,ab_par
  use tends4pertmod, only: itrajfreq
  implicit none
! private

  interface ncep_model_nl_init
            module procedure init_
  end interface
  interface ncep_model_nl_set
            module procedure set_
  end interface
  interface ncep_model_nl
            module procedure model_nl_
  end interface
  interface ncep_model_nl_final
            module procedure final_
  end interface

  real(r_kind),allocatable,dimension(:,:,:):: bck_u,bck_v,bck_tv,bck_q,bck_oz, &
         bck_cw,bck_u_lon,bck_u_lat, bck_v_lon,bck_v_lat,bck_tvlat, &
         bck_tvlon,bck_qlon,bck_qlat,bck_ozlon,bck_ozlat,bck_cwlon,bck_cwlat
  real(r_kind),allocatable,dimension(:,:,:):: what_bck,prsth_bck,prsum_bck,r_prsum_bck,prdif_bck, &
         r_prdif_bck,pr_xsum_bck,pr_ysum_bck,rdtop_bck
  real(r_kind),allocatable,dimension(:,:):: bck_ps

  logical, save :: nlmodel_initialized_     =.false.
  logical, save :: nlmodel_tend_initialized_=.false.
  
  integer(i_kind) itime_step,itrajout_hrs,itrajfrq_hrs
  namelist/pertmodel/ab_par,itime_step,itrajout_hrs,itrajfrq_hrs

  character(len=*),parameter :: myname='nonlinmod'
contains

  subroutine create_nonlinvars_(rc)
  use kinds, only: i_kind
  use gridmod, only: lat2,lon2,nsig
  implicit none
  integer(i_kind),intent(out) :: rc

  rc=0
  if(nlmodel_tend_initialized_) return

    allocate( bck_u(lat2,lon2,nsig),bck_v(lat2,lon2,nsig), &
              bck_tv(lat2,lon2,nsig),bck_q(lat2,lon2,nsig),bck_oz(lat2,lon2,nsig), &
              bck_cw(lat2,lon2,nsig),bck_ps(lat2,lon2),bck_u_lon(lat2,lon2,nsig), &
              bck_u_lat(lat2,lon2,nsig),bck_v_lon(lat2,lon2,nsig),bck_v_lat(lat2,lon2,nsig), &
              bck_tvlat(lat2,lon2,nsig),bck_tvlon(lat2,lon2,nsig),bck_qlon(lat2,lon2,nsig), &
              bck_qlat(lat2,lon2,nsig),bck_ozlon(lat2,lon2,nsig),bck_ozlat(lat2,lon2,nsig), &
              bck_cwlon(lat2,lon2,nsig),bck_cwlat(lat2,lon2,nsig),stat=rc )

    allocate( what_bck(lat2,lon2,nsig+1),prsth_bck(lat2,lon2,nsig+1),&
             prsum_bck(lat2,lon2,nsig),r_prsum_bck(lat2,lon2,nsig),&
             prdif_bck(lat2,lon2,nsig),r_prdif_bck(lat2,lon2,nsig),&
             pr_xsum_bck(lat2,lon2,nsig),pr_ysum_bck(lat2,lon2,nsig),&
             rdtop_bck(lat2,lon2,nsig), stat=rc )

    nlmodel_tend_initialized_ =.true.
   
    return
  end subroutine create_nonlinvars_

  subroutine init_(rc)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    gsi_4dmodel_init_tl      tlm entry for for 4dvar
!   prgmmr: rancic                            date: 2011-01-21
!
! abstract: 
!
! program history log:
!   2011-01-21  rancic - update to use gsi_bundle
!
!   input argument list:
!     
!
!   output argument list:      
!     ntime_step       - perturbation model time-step
!
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: i_kind,r_kind
  use mpimod, only: mype
  use gsi_4dvar, only: idmodel
  use pblmod, only: create_pblvars
  use mp_compact_diffs_mod2, only: init_mp_compact_diffs2
  use tends4pertmod, only: create_tend4pertvars
  use gridmod, only: nsig
  implicit none
  integer(i_kind),intent(out) :: rc
  
  rc=0
  if(nlmodel_initialized_) return

  if(idmodel) return

  call create_tend4pertvars
  call init_mp_compact_diffs2(nsig,mype,.false.)
  call create_pblvars
  call create_nonlinvars_(rc)

  nlmodel_initialized_=.true.

  return
  end subroutine init_

  subroutine set_
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    set_               set parameters for pertmodel
!   prgmmr: rancic                            date: 2011-01-21
!
! abstract: 
!
! program history log:
!   2011-01-21  rancic - update to use gsi_bundle
!
!   input argument list:
!     
!
!   output argument list:      
!     ntime_step       - perturbation model time-step
!
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: i_kind,r_kind
  use mpimod, only: mype
  use constants, only: half,r3600
  use gsi_4dvar, only: l4dvar,nhr_assimilation
  use mpeu_util, only: die

! set the following:
  use dynamics_adams_bashforth, only: init_dynam_ab
  implicit none
  
  character(len=*),parameter :: myname_=myname//'set_'
  integer(i_kind) ios
  
  if(.not.l4dvar) return

  call set_defaults_()

  open(11,file='pertmodel.nl')
  read(11,pertmodel,iostat=ios)
      if(ios/=0) call die(myname_,'read(pertmodel.nl)',ios)
  close(11)

  time_step=real(itime_step,r_kind)
  time_step_half=time_step*half

  itime_max=NINT(nhr_assimilation*r3600/itime_step)
  itime_out=itime_max/itrajout_hrs

  if(itrajfrq_hrs==0) then
     itrajfreq=0
  else if(itrajfrq_hrs>0) then
     itrajfreq=NINT(itrajfrq_hrs*r3600/itime_step)
  else
     itrajfreq=1
  endif

  call init_dynam_ab

  if(mype==0) then 
     write(6,*) 'GSI_4DMODEL_INIT_NL: time_step=',time_step
     write(6,*) 'GSI_4DMODEL_INIT_NL: itime_max=',itime_max
  end if

  return
  end subroutine set_

  subroutine set_defaults_
!$$$ subprogram documentation block
!               .      .    .                                      .
! subprogram:   set_defaults_    set default parameters for perturbation models
!   prgmmr: todling                         date: 2011-05-24
!
! abstract: Set default parameters for perturbation models. This routine
!           is NEVER TO BE MADE public.
! 
! program history log:
!   2011-05-24  todling
!
! usage: 
!
! output argument list:
! 
! $$$$    
  use kinds, only: r_kind,i_kind

! Select time-differencing accuracy (2,3 or 4) 

   ab_par=2         ! order of finite-difference scheme

! Time Stepping Parameters

   itime_step=180   ! in seconds
   itrajout_hrs=6   ! frequency of grads-trajectory output (hrs)

! Frequency of trajectory i/o
   itrajfrq_hrs=-1  ! default: i/o every time step


  end subroutine set_defaults_

  subroutine model_nl_

!$$$ subprogram documentation block
!               .      .    .                                      .
! subprogram:   gsi_model_nl      run perturbation model for 4dvar 
!   prgmmr: rancic                          date: 2010-02-24
!
! abstract: Run perturbation model using Adams-Bashforth 2nd, 3rd or 4th order
!           scheme for dynamics and implicit Crank-Nicolson scheme for pbl
!           and save produced fields needed for tlm and adm
! 
! program history log:
!   2010-02-24  rancic 
!
! usage: 
!   input argument list:
!     u     - zonal wind on subdomain
!     v     - meridional wind on subdomain  
!     tv    - virtual temperature  on subdomain
!     q     - moisture on subdomain
!     oz    - ozone on subdomain
!     cw    - cloud water on subdomain
!     ps    - surface presure on subdomain
!     z     - sfc terrain height  on subdomain
!     mype     - task id
!
!   output argument list:
! 
! $$$$    
  use kinds, only: r_kind,i_kind
  use mpimod, only: mype
  use constants, only: zero,one,two,half
  use gridmod, only: lat2,lon2,nsig,nsig1o
  use tends4pertmod, only: time_step,time_step_half,itime,itime_out,itime_max,ab_par
  use mpimod, only: levs_id
  use dynamics_adams_bashforth, only: dynam_ab  
  use guess_grids, only: ges_z,ges_ps,ges_u,ges_v,ges_tv,ges_q,ges_oz
  use gsi_metguess_mod, only: gsi_metguess_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use mpeu_util, only: die
  implicit none

! Declare local variables
  real(r_kind),dimension(lat2,lon2,nsig):: u_NL,v_NL,tv_NL,q_NL,oz_NL,cw_NL
  real(r_kind),dimension(lat2,lon2):: ps_NL,z_NL
  integer(i_kind) i,j,k,it,nnn,nout,nltl_mask

  integer(i_kind) it0,ier
  real(r_kind),pointer,dimension(:,:,:):: ges_cwmr

! Set mask that controls choice of model (nonlinear = 1 ; tangent linear = 2)

   nltl_mask=1
   nout=itime_out

! Used throughout
    nnn=0
    do k=1,nsig1o
      if (levs_id(k)/=0) nnn=nnn+1
    end do

! Copy arrays
   
   it0=1  ! _RT needs attention

   call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it0), 'cw', ges_cwmr, ier)
   if(ier/=0) then
      call die('ncep_model_nl_','unable to get CW from Guess',ier)
   endif

   u_NL=ges_u(:,:,:,it0) ;  v_NL=ges_v(:,:,:,it0)  ; tv_NL=ges_tv  (:,:,:,it0)
   q_NL=ges_q(:,:,:,it0) ; oz_NL=ges_oz(:,:,:,it0) ; cw_NL=ges_cwmr(:,:,:)
   ps_NL=ges_ps(:,:,it0) ;  z_NL=ges_z(:,:,it0)

!#############################################################################
    if(mype==0) write(6,*) &
 '--------- START INTEGRATION -------------------------------------------'
!#############################################################################

        itime=0

       call write_bkgvars_grid_mod(u_NL,v_NL,tv_NL,ps_NL,itime,mype,'bkgvar_')

       call write_nonlinear(u_NL,v_NL,tv_NL,q_NL,oz_NL,cw_NL,ps_NL,itime,mype)    

    TIME_NL: do itime=1,itime_max

   if(mype==0) write(6,*) 'itime=',itime

       call dynam_ab(u_NL,v_NL,tv_NL,q_NL,oz_NL,cw_NL,ps_NL,z_NL,nnn,mype,nltl_mask)
       call pbl(u_NL,v_NL,tv_NL,ps_NL,1,lon2)
       call write_nonlinear(u_NL,v_NL,tv_NL,q_NL,oz_NL,cw_NL,ps_NL,itime,mype)    

       if(mod(itime,nout)==0.or.itime==itime_max) then
         call write_bkgvars_grid_mod(u_NL,v_NL,tv_NL,ps_NL,itime,mype,'bkgvar_')
       end if

  end do TIME_NL
!#############################################################################
    if(mype==0) write(6,*) &
 '--------- FINISHED INTEGRATION -------------------------------------------'
!#############################################################################

! End of routine
  return

  end subroutine model_nl_

  subroutine final_(rc)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    final_                finalize nonlinear model
!   prgmmr: rancic                            date: 2011-01-21
!
! abstract: 
!
! program history log:
!   2011-01-21  rancic - update to use gsi_bundle
!
!   input argument list:
!     
!
!   output argument list:      
!     ntime_step       - perturbation model time-step
!
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: i_kind
  use pblmod, only: destroy_pblvars
  use mp_compact_diffs_mod2, only: destroy_mp_compact_diffs2
  use tends4pertmod, only: destroy_tend4pertvars
  implicit none
  integer(i_kind),intent(out) :: rc
  
  rc=0
  if(.not.nlmodel_initialized_) return

  call destroy_mp_compact_diffs2
  call destroy_nonlinvars_(rc)
  call destroy_pblvars
  call destroy_tend4pertvars

  nlmodel_initialized_=.false.

  return
  end subroutine final_
  
  subroutine destroy_nonlinvars_(rc)
  
  use kinds, only: i_kind
  implicit none
  integer(i_kind),intent(out) :: rc

  rc=0
  if(.not.nlmodel_tend_initialized_) return

    deallocate( bck_u,bck_v,bck_tv,bck_q,bck_oz, &
         bck_cw,bck_ps,bck_u_lon,bck_u_lat,bck_v_lon,bck_v_lat,bck_tvlat, &
         bck_tvlon,bck_qlon,bck_qlat,bck_ozlon,bck_ozlat,bck_cwlon,bck_cwlat,stat=rc )
    deallocate(what_bck,prsum_bck,prsth_bck,r_prsum_bck,r_prdif_bck,prdif_bck,pr_xsum_bck,&
         pr_ysum_bck,rdtop_bck,stat=rc)
    nlmodel_tend_initialized_=.false.

    return
  end subroutine destroy_nonlinvars_

end module nonlinmod
