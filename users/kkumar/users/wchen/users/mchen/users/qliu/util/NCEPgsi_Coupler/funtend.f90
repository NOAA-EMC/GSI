subroutine funtend(u,v,tv,q,oz,cw,ps,z,nnn,mype, &
                   u_t,v_t,tv_t,q_t,oz_t,cw_t,ps_t)
!$$$ subprogram documentation block
!                .      .    .                                       .
! subprogram:    funtend        envoke subprogram for time tendencies 
!   prgmmr: rancic
!
! abstract:  envoke subprogram that calculates time tendencies for 
!            perturbation model used in 4dvar  
!
! program history log:
!   2010-02-24  rancic 
!
! usage: 
!   input argument list:
!     u        - zonal wind on subdomain 
!     v        - meridional wind on subdomain 
!     tv       - virtural temperature on subdomain 
!     q        - moisture on subdomain 
!     oz       - ozone on subdomain 
!     cw       - cloud water on subdomain 
!     ps       - surface pressure subdomain 
!     z        - sfc terrain height
!     nnn      - 
!     mype     - task id
!      
!   output argument list:
!     u_t      - time tendency of u
!     v_t      - time tendency of v
!     tv_t     - time tendency of tv 
!     q_t      - time tendency of q
!     oz_t     - time tendency of oz 
!     cw_t     - time tendency of cw
!     ps_t     - time tendency of ps
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$  end documentation block

  use kinds, only: r_kind,i_kind
  use constants, only: ione,zero
  use gridmod, only: lat2,lon2,nsig
  use tends4pertmod, only: time_step,itime
  implicit none

! Declare passed variables
  integer(i_kind)                       ,intent(in   ) :: nnn,mype
  real(r_kind),dimension(lat2,lon2,nsig),intent(in   ) :: u,v,tv,q,oz,cw
  real(r_kind),dimension(lat2,lon2)     ,intent(in   ) :: ps,z
  real(r_kind),dimension(lat2,lon2,nsig),intent(  out) :: u_t,v_t,tv_t,q_t,oz_t,cw_t
  real(r_kind),dimension(lat2,lon2)     ,intent(  out) :: ps_t

! Declare local variables
  real(r_kind),dimension(lat2,lon2,nsig):: u_x,v_x,tv_x,q_x,oz_x,cw_x,&
                                  u_y,v_y,tv_y,q_y,oz_y,cw_y,phl,phl_x,phl_y
  real(r_kind),dimension(lat2,lon2,nsig+ione):: pri,pri_x,pri_y

  integer(i_kind) i,j,k,mype_out

! Damp the variables within the model top  boundary sponge layer
!
!        call sponge(u,3*nsig/4)
!        call sponge(v,3*nsig/4)
!        call sponge(tv,3*nsig/4)

         u_t=zero
         v_t=zero
         tv_t=zero
         q_t=zero
         oz_t=zero
         cw_t=zero
         ps_t=zero
 
! Get spatial derivatives using compact differencing

        call mp_compact_dlon2(u,u_x,.false.,nsig,mype)
        call mp_compact_dlat2(u,u_y,.false.,nsig,mype)
        call mp_compact_dlon2(v,v_x,.false.,nsig,mype)
        call mp_compact_dlat2(v,v_y,.false.,nsig,mype)
        call mp_compact_dlon2(tv,tv_x,.false.,nsig,mype)
        call mp_compact_dlat2(tv,tv_y,.false.,nsig,mype)
        call mp_compact_dlon2(q,q_x,.false.,nsig,mype)
        call mp_compact_dlat2(q,q_y,.false.,nsig,mype)
        call mp_compact_dlon2(oz,oz_x,.false.,nsig,mype)
        call mp_compact_dlat2(oz,oz_y,.false.,nsig,mype)
        call mp_compact_dlon2(cw,cw_x,.false.,nsig,mype)
        call mp_compact_dlat2(cw,cw_y,.false.,nsig,mype)

! Get 3d pressure
         call getprs_bck(ps,tv,pri)


! Get geopotential on mid-levels
         call getphi(tv,pri,z,phl)     

! Get gradients of geopotential and 3d pressure
        call mp_compact_dlon2(phl,phl_x,.false.,nsig,mype)
        call mp_compact_dlat2(phl,phl_y,.false.,nsig,mype)


        call mp_compact_dlon1(pri,pri_x,.false.,nsig+ione,mype)
        call mp_compact_dlat1(pri,pri_y,.false.,nsig+ione,mype)

! Get tendency based on current time level
        call calctends_model(u,v,tv,q,oz,cw,pri,phl,phl_x,phl_y,u_x,u_y, &
             v_x,v_y,tv_x,tv_y,pri_x,pri_y,q_x,q_y,oz_x,oz_y,cw_x,cw_y,z,mype, &
             u_t,v_t,tv_t,q_t,oz_t,cw_t,ps_t)


! Filter out pole with g2s2g 
        call smooth_tends(u_t,v_t,tv_t,q_t,oz_t,cw_t,ps_t,nnn,mype)

end subroutine funtend

subroutine funtend_tl(u,v,tv,q,oz,cw,ps,z,nnn,mype, &
                      u_t,v_t,tv_t,q_t,oz_t,cw_t,ps_t)

  use kinds, only: r_kind,i_kind
  use constants, only: ione
  use gridmod, only: lat2,lon2,nsig
  implicit none

! Declare passed variables
  integer(i_kind)                       ,intent(in   ) :: nnn,mype
  real(r_kind),dimension(lat2,lon2,nsig),intent(in   ) :: u,v,tv,q,oz,cw
  real(r_kind),dimension(lat2,lon2)     ,intent(in   ) :: ps,z
  real(r_kind),dimension(lat2,lon2,nsig),intent(  out) :: u_t,v_t,tv_t,q_t,oz_t,cw_t
  real(r_kind),dimension(lat2,lon2)     ,intent(  out) :: ps_t

! Declare local variables
  real(r_kind),dimension(lat2,lon2,nsig):: u_x,v_x,tv_x,q_x,oz_x,cw_x,&
                                  u_y,v_y,tv_y,q_y,oz_y,cw_y,phl_x,phl_y,phl
  real(r_kind),dimension(lat2,lon2):: ps_x,ps_y
  real(r_kind),dimension(lat2,lon2,nsig+ione):: pri,pri_x,pri_y

  integer(i_kind) i,j,k

! Damp the variables within the model top boundary sponge layer
!
!        call sponge(u,3*nsig/4)
!        call sponge(v,3*nsig/4)
!        call sponge(tv,3*nsig/4)

 
! Get spatial derivatives using compact differencing

        call mp_compact_dlon2(u,u_x,.false.,nsig,mype)
        call mp_compact_dlat2(u,u_y,.false.,nsig,mype)
        call mp_compact_dlon2(v,v_x,.false.,nsig,mype)
        call mp_compact_dlat2(v,v_y,.false.,nsig,mype)
        call mp_compact_dlon2(tv,tv_x,.false.,nsig,mype)
        call mp_compact_dlat2(tv,tv_y,.false.,nsig,mype)
        call mp_compact_dlon2(q,q_x,.false.,nsig,mype)
        call mp_compact_dlat2(q,q_y,.false.,nsig,mype)
        call mp_compact_dlon2(oz,oz_x,.false.,nsig,mype)
        call mp_compact_dlat2(oz,oz_y,.false.,nsig,mype)
        call mp_compact_dlon2(cw,cw_x,.false.,nsig,mype)
        call mp_compact_dlat2(cw,cw_y,.false.,nsig,mype)

! Get 3d pressure
        call getprs_bck_tl(ps,tv,pri)

! Get geopotential on mid-levels
        call getphi_tl(tv,pri,z,phl)

! Get gradient of geopotential
        call mp_compact_dlat2(phl,phl_y,.false.,nsig,mype)
        call mp_compact_dlon2(phl,phl_x,.false.,nsig,mype)

        call mp_compact_dlon1(pri,pri_x,.false.,nsig+ione,mype)
        call mp_compact_dlat1(pri,pri_y,.false.,nsig+ione,mype)

! Get tendency based on current time level
       call calctends_model_tl(u,v,tv,q,oz,cw,pri,phl_x,phl_y,u_x,u_y,    &
            v_x,v_y,tv_x,tv_y,pri_x,pri_y,q_x,q_y,oz_x,oz_y,cw_x,cw_y, &
            mype,u_t,v_t,tv_t,q_t,oz_t,cw_t,ps_t)

! Filter out pole with g2s2g 
        call smooth_tends(u_t,v_t,tv_t,q_t,oz_t,cw_t,ps_t,nnn,mype)

end subroutine funtend_tl

subroutine funtend_ad(u,v,tv,q,oz,cw,ps,z,nnn,mype, &
                      u_t,v_t,tv_t,q_t,oz_t,cw_t,ps_t)
             

  use kinds, only: r_kind,i_kind
  use constants, only: zero,ione
  use gridmod, only: lat2,lon2,nsig
  implicit none

! Declare passed variables
  integer(i_kind)                       ,intent(in   ) :: nnn,mype
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout) :: u,v,tv,q,oz,cw
  real(r_kind),dimension(lat2,lon2)     ,intent(inout) :: ps,z
  real(r_kind),dimension(lat2,lon2,nsig),intent(in   ) :: u_t,v_t,tv_t,q_t,oz_t,cw_t
  real(r_kind),dimension(lat2,lon2)     ,intent(in   ) :: ps_t

! Declare local variables
  real(r_kind),dimension(lat2,lon2,nsig):: u_x,v_x,tv_x,q_x,oz_x,cw_x,&
                                   u_y,v_y,tv_y,q_y,oz_y,cw_y,phl_x,phl_y,phl
  real(r_kind),dimension(lat2,lon2):: ps_x,ps_y
  real(r_kind),dimension(lat2,lon2,nsig+ione):: pri,pri_x,pri_y

  integer(i_kind) i,j,k

  pri=zero; phl=zero
  phl_x=zero; phl_y=zero; u_x=zero; u_y=zero; v_x=zero; v_y=zero
  tv_x=zero; tv_y=zero; pri_x=zero; pri_y=zero; q_x=zero; q_y=zero
  oz_x=zero; oz_y=zero; cw_x=zero; cw_y=zero


         call smooth_tends_ad(u_t,v_t,tv_t,q_t,oz_t,cw_t,ps_t,nnn,mype)

! Adjoint of tendecy based on current time level


       call calctends_model_ad(u,v,tv,q,oz,cw,pri,phl_x,phl_y,u_x,u_y,    &
             v_x,v_y,tv_x,tv_y,pri_x,pri_y,q_x,q_y,oz_x,oz_y,cw_x,cw_y, &
             mype,u_t,v_t,tv_t,q_t,oz_t,cw_t,ps_t)

        call mp_compact_dlat1_ad(pri,pri_y,.false.,nsig+ione,mype)
        call mp_compact_dlon1_ad(pri,pri_x,.false.,nsig+ione,mype)

! Adjoint of gradient of geopotential

        call mp_compact_dlat2_ad(phl,phl_y,.false.,nsig,mype)
        call mp_compact_dlon2_ad(phl,phl_x,.false.,nsig,mype)

! Adjoint of geopotential on mid-levels
        call getphi_ad(tv,pri,z,phl)

! Adjoint of 3d pressure
        call getprs_bck_ad(ps,tv,pri)


! Adjoint of spatial derivatives using compact differencing

        call mp_compact_dlat2_ad(u,u_y,.false.,nsig,mype)
        call mp_compact_dlon2_ad(u,u_x,.false.,nsig,mype)
        call mp_compact_dlat2_ad(v,v_y,.false.,nsig,mype)
        call mp_compact_dlon2_ad(v,v_x,.false.,nsig,mype)
        call mp_compact_dlat2_ad(tv,tv_y,.false.,nsig,mype)
        call mp_compact_dlon2_ad(tv,tv_x,.false.,nsig,mype)
        call mp_compact_dlat2_ad(q,q_y,.false.,nsig,mype)
        call mp_compact_dlon2_ad(q,q_x,.false.,nsig,mype)
        call mp_compact_dlat2_ad(oz,oz_y,.false.,nsig,mype)
        call mp_compact_dlon2_ad(oz,oz_x,.false.,nsig,mype)
        call mp_compact_dlat2_ad(cw,cw_y,.false.,nsig,mype)
        call mp_compact_dlon2_ad(cw,cw_x,.false.,nsig,mype)


end subroutine funtend_ad
