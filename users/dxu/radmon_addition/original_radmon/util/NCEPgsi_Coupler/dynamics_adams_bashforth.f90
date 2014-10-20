module dynamics_adams_bashforth
!$$$   module documentation block
!                .      .    .                                       .
! module: dynamics_adams_bashforth  contains adams-bashofrt scheme
!   prgmmr: rancic
!
! abstract:  contains evertyhing related to adams-bashforth time
!            differencing schemes of 2nd, 3th and 4th order of accuracy
!            including their adjoint
!
! program history log:
!   2010-02-24  rancic
!
! subroutines included:
!   sub init_dynam_ab      - definition of adams-bashforth coefficients
!   sub dynam_ab           - driver for adams-bashorth time stepping
!   sub dynam_ab2          - 2nd order adams-bashforth scheme
!   sub dynam_ab3          - 3rd order adams-bashforth scheme
!   sub dynam_ab4          - 4th order adams-bashforth scheme
!   sub dunam_ab_ad        - driver for adjoint of adams-aashforth time-stepping
!   sub dynam_ab2_ad       - adjoint of 2nd order adams-bashforth scheme
!   sub dynam_ab3_ad       - adjoint of 3rd order adams-bashforth scheme
!   sub dynam_ab4_ad       - adjoint of 4th order adams-bashforth scheme
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind
  use constants, only: zero,one,two,half,three,four,five
  implicit none

  real(r_kind) ab21,ab22,ab31,ab32,ab33,ab41,ab42,ab43,ab44  
!m-------------------------------------------------------------------------------B
! offcentering of the scheme
  real(r_kind) cvrc
!m-------------------------------------------------------------------------------E

  contains

subroutine init_dynam_ab
  implicit none
    real(r_kind) twelvth,twentyfourth

    twelvth=one/12.0_r_kind
    twentyfourth=half*twelvth

!m-------------------------------------------------------------------------------B
    cvrc=0.1_r_kind
    ab21=three*half+cvrc
    ab22=-half-cvrc
!m-------------------------------------------------------------------------------E
!m    ab21=three*half
!m    ab22=-half

    ab31=23.0_r_kind*twelvth
    ab32=-16.0_r_kind*twelvth
    ab33=5.0_r_kind*twelvth

    ab41=55.0_r_kind*twentyfourth
    ab42=-59.0_r_kind*twentyfourth
    ab43=37.0_r_kind*twentyfourth
    ab44=-9.0_r_kind*twentyfourth
   
end subroutine init_dynam_ab


subroutine dynam_ab(u,v,tv,q,oz,cw,ps,z,nnn,mype,nltl_mask)
!
! Driver for Adams-Bashforth time stepping
!
  use kinds, only: r_kind,i_kind
  use gridmod, only: lat2,lon2,nsig
  use tends4pertmod, only: ab_par
  implicit none

! Declare passed variables
  integer(i_kind)                       ,intent(in   ) :: nnn,mype,nltl_mask
  real(r_kind),dimension(lat2,lon2)     ,intent(in   ) :: z
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout) :: u,v,tv,q,oz,cw
  real(r_kind),dimension(lat2,lon2)     ,intent(inout) :: ps

  select case(ab_par)
    case(2)
       call dynam_ab2(u,v,tv,q,oz,cw,ps,z,nnn,mype,nltl_mask)
     case(4)
       call dynam_ab4(u,v,tv,q,oz,cw,ps,z,nnn,mype,nltl_mask)
    case default
       call dynam_ab3(u,v,tv,q,oz,cw,ps,z,nnn,mype,nltl_mask)
  end select

end subroutine dynam_ab

subroutine dynam_ab2(u,v,tv,q,oz,cw,ps,z,nnn,mype,nltl_mask)
!
! Adams-Bashforth 2-nd order for time stepping
!
  use kinds, only: r_kind,i_kind
  use tends4pertmod, only: time_step,itime,itime_max
  use tends4pertmod, only: u1_t,v1_t,tv1_t,q1_t,oz1_t,cw1_t,ps1_t
  use gridmod, only: lat2,lon2,nsig
  implicit none

! Declare passed variables
  integer(i_kind)                       ,intent(in   ) :: nnn,mype,nltl_mask
  real(r_kind),dimension(lat2,lon2)     ,intent(in   ) :: z
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout) :: u,v,tv,q,oz,cw
  real(r_kind),dimension(lat2,lon2)     ,intent(inout) :: ps

! Declare local variables
  integer(i_kind) i,j,n 
  real(r_kind),dimension(lat2,lon2,nsig):: u_t,v_t,tv_t,q_t,oz_t,cw_t
  real(r_kind),dimension(lat2,lon2):: ps_t

    select case(nltl_mask)
      case(1)
        call funtend(u,v,tv,q,oz,cw,ps,z,nnn,mype, &
                     u_t,v_t,tv_t,q_t,oz_t,cw_t,ps_t)
      case(2)
        call funtend_tl(u,v,tv,q,oz,cw,ps,z,nnn,mype, &
                      u_t,v_t,tv_t,q_t,oz_t,cw_t,ps_t)
    end select

    if(itime==1) then
      u=u+time_step*u_t
      v=v+time_step*v_t
      tv=tv+time_step*tv_t
      q=q+time_step*q_t
      oz=oz+time_step*oz_t
      cw=cw+time_step*cw_t
      ps=ps+time_step*ps_t
  else
      u=u+time_step*(ab21*u_t+ab22*u1_t)
      v=v+time_step*(ab21*v_t+ab22*v1_t)
      tv=tv+time_step*(ab21*tv_t+ab22*tv1_t)
      q=q+time_step*(ab21*q_t+ab22*q1_t)
      oz=oz+time_step*(ab21*oz_t+ab22*oz1_t)
      cw=cw+time_step*(ab21*cw_t+ab22*cw1_t)
      ps=ps+time_step*(ab21*ps_t+ab22*ps1_t)
   end if
     if(itime<itime_max) then
          u1_t=u_t
          v1_t=v_t
          tv1_t=tv_t
          q1_t=q_t
          oz1_t=oz_t
          cw1_t=cw_t
          ps1_t=ps_t
     end if
  
end subroutine dynam_ab2

subroutine dynam_ab3(u,v,tv,q,oz,cw,ps,z,nnn,mype,nltl_mask)
!
! Adams-Bashforth 3-rd order for time stepping
!
  use kinds, only: r_kind,i_kind
  use tends4pertmod, only: time_step,itime,itime_max
  use tends4pertmod, only: u1_t,v1_t,tv1_t,q1_t,oz1_t,cw1_t,ps1_t, & 
                      u2_t,v2_t,tv2_t,q2_t,oz2_t,cw2_t,ps2_t
  use gridmod, only: lat2,lon2,nsig
  implicit none

! Declare passed variables
  integer(i_kind)                       ,intent(in   ) :: nnn,mype,nltl_mask
  real(r_kind),dimension(lat2,lon2)     ,intent(in   ) :: z
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout) :: u,v,tv,q,oz,cw
  real(r_kind),dimension(lat2,lon2)     ,intent(inout) :: ps

! Declare local variables
  real(r_kind),dimension(lat2,lon2,nsig):: u_t,v_t,tv_t,q_t,oz_t,cw_t
  real(r_kind),dimension(lat2,lon2):: ps_t
  integer(i_kind) i,j,n 

    select case(nltl_mask)
      case(1)
        call funtend(u,v,tv,q,oz,cw,ps,z,nnn,mype, &
                    u_t,v_t,tv_t,q_t,oz_t,cw_t,ps_t)
      case(2)
        call funtend_tl(u,v,tv,q,oz,cw,ps,z,nnn,mype, &
                       u_t,v_t,tv_t,q_t,oz_t,cw_t,ps_t)
    end select

  if(itime==1) then
      u=u+time_step*u_t
      v=v+time_step*v_t
      tv=tv+time_step*tv_t
      q=q+time_step*q_t
      oz=oz+time_step*oz_t
      cw=cw+time_step*cw_t
      ps=ps+time_step*ps_t
  else if(itime==2) then
      u=u+time_step*(ab21*u_t+ab22*u1_t)
      v=v+time_step*(ab21*v_t+ab22*v1_t)
      tv=tv+time_step*(ab21*tv_t+ab22*tv1_t)
      q=q+time_step*(ab21*q_t+ab22*q1_t)
      oz=oz+time_step*(ab21*oz_t+ab22*oz1_t)
      cw=cw+time_step*(ab21*cw_t+ab22*cw1_t)
      ps=ps+time_step*(ab21*ps_t+ab22*ps1_t)
  else
      u=u+time_step*(ab31*u_t+ab32*u1_t+ab33*u2_t)
      v=v+time_step*(ab31*v_t+ab32*v1_t+ab33*v2_t)
      tv=tv+time_step*(ab31*tv_t+ab32*tv1_t+ab33*tv2_t)
      q=q+time_step*(ab31*q_t+ab32*q1_t+ab33*q2_t)
      oz=oz+time_step*(ab31*oz_t+ab32*oz1_t+ab33*oz2_t)
      cw=cw+time_step*(ab31*cw_t+ab32*cw1_t+ab33*cw2_t)
      ps=ps+time_step*(ab31*ps_t+ab32*ps1_t+ab33*ps2_t)
  end if
  if(itime<itime_max) then
        u2_t=u1_t
        v2_t=v1_t
        tv2_t=tv1_t
        q2_t=q1_t
        oz2_t=oz1_t
        cw2_t=cw1_t
        ps2_t=ps1_t
          u1_t=u_t
          v1_t=v_t
          tv1_t=tv_t
          q1_t=q_t
          oz1_t=oz_t
          cw1_t=cw_t
          ps1_t=ps_t
  end if
  
end subroutine dynam_ab3

subroutine dynam_ab4(u,v,tv,q,oz,cw,ps,z,nnn,mype,nltl_mask)
!
! Adams-Bashforth 4-th order for time stepping
!
  use kinds, only: r_kind,i_kind
  use tends4pertmod, only: time_step,itime,itime_max
  use tends4pertmod, only: u1_t,v1_t,tv1_t,q1_t,oz1_t,cw1_t,ps1_t, & 
                      u2_t,v2_t,tv2_t,q2_t,oz2_t,cw2_t,ps2_t, &
                      u3_t,v3_t,tv3_t,q3_t,oz3_t,cw3_t,ps3_t
  use gridmod, only: lat2,lon2,nsig
  implicit none

! Declare passed variables
  integer(i_kind)                       ,intent(in   ) :: nnn,mype,nltl_mask
  real(r_kind),dimension(lat2,lon2)     ,intent(in   ) :: z
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout) :: u,v,tv,q,oz,cw
  real(r_kind),dimension(lat2,lon2)     ,intent(inout) :: ps

! Declare local variables
  real(r_kind),dimension(lat2,lon2,nsig):: u_t,v_t,tv_t,q_t,oz_t,cw_t
  real(r_kind),dimension(lat2,lon2):: ps_t
  integer(i_kind) i,j,n 

    select case(nltl_mask)
      case(1)
        call funtend(u,v,tv,q,oz,cw,ps,z,nnn,mype, &
                    u_t,v_t,tv_t,q_t,oz_t,cw_t,ps_t)
      case(2)
        call funtend_tl(u,v,tv,q,oz,cw,ps,z,nnn,mype, &
                       u_t,v_t,tv_t,q_t,oz_t,cw_t,ps_t)
    end select

  if(itime==1) then
      u=u+time_step*u_t
      v=v+time_step*v_t
      tv=tv+time_step*tv_t
      q=q+time_step*q_t
      oz=oz+time_step*oz_t
      cw=cw+time_step*cw_t
      ps=ps+time_step*ps_t
        u1_t=u_t
        v1_t=v_t
        tv1_t=tv_t
        q1_t=q_t
        oz1_t=oz_t
        cw1_t=cw_t
        ps1_t=ps_t
  else if(itime==2) then
      u=u+time_step*(ab21*u_t+ab22*u1_t)
      v=v+time_step*(ab21*v_t+ab22*v1_t)
      tv=tv+time_step*(ab21*tv_t+ab22*tv1_t)
      q=q+time_step*(ab21*q_t+ab22*q_t)
      oz=oz+time_step*(ab21*oz_t+ab22*oz1_t)
      cw=cw+time_step*(ab21*cw_t+ab22*cw1_t)
      ps=ps+time_step*(ab21*ps_t+ab22*ps1_t)
        u2_t=u1_t
        v2_t=v1_t
        tv2_t=tv1_t
        q2_t=q1_t
        oz2_t=oz1_t
        cw2_t=cw1_t
        ps2_t=ps1_t
          u1_t=u_t
          v1_t=v_t
          tv1_t=tv_t
          q1_t=q_t
          oz1_t=oz_t
          cw1_t=cw_t
          ps1_t=ps_t
  else if(itime==3) then
      u=u+time_step*(ab31*u_t+ab32*u1_t+ab33*u2_t)
      v=v+time_step*(ab31*v_t+ab32*v1_t+ab33*v2_t)
      tv=tv+time_step*(ab31*tv_t+ab32*tv1_t+ab33*tv2_t)
      q=q+time_step*(ab31*q_t+ab32*q1_t+ab33*q2_t)
      oz=oz+time_step*(ab31*oz_t+ab32*oz1_t+ab33*oz2_t)
      cw=cw+time_step*(ab31*cw_t+ab32*cw1_t+ab33*cw2_t)
      ps=ps+time_step*(ab31*ps_t+ab32*ps1_t+ab33*ps2_t)
        u3_t=u2_t
        v3_t=v2_t
        tv3_t=tv2_t
        q3_t=q2_t
        oz3_t=oz2_t
        cw3_t=cw2_t
        ps3_t=ps2_t
          u2_t=u1_t
          v2_t=v1_t
          tv2_t=tv1_t
          q2_t=q1_t
          oz2_t=oz1_t
          cw2_t=cw1_t
          ps2_t=ps1_t
            u1_t=u_t
            v1_t=v_t
            tv1_t=tv_t
            q1_t=q_t
            oz1_t=oz_t
            cw1_t=cw_t
            ps1_t=ps_t
  else 
      u=u+time_step*(ab41*u_t+ab42*u1_t+ab43*u2_t+ab44*u3_t)
      v=v+time_step*(ab41*v_t+ab42*v1_t+ab43*v2_t+ab44*v3_t)
      tv=tv+time_step*(ab41*tv_t+ab42*tv1_t+ab43*tv2_t+ab44*tv3_t)
      q=q+time_step*(ab41*q_t+ab42*q1_t+ab43*q2_t+ab44*q3_t)
      oz=oz+time_step*(ab41*oz_t+ab42*oz1_t+ab43*oz2_t+ab44*oz3_t)
      cw=cw+time_step*(ab41*cw_t+ab42*cw1_t+ab43*cw2_t+ab44*cw3_t)
      ps=ps+time_step*(ab41*ps_t+ab42*ps1_t+ab43*ps2_t+ab44*ps3_t)
        u3_t=u2_t
        v3_t=v2_t
        tv3_t=tv2_t
        q3_t=q2_t
        oz3_t=oz2_t
        cw3_t=cw2_t
        ps3_t=ps2_t
          u2_t=u1_t
          v2_t=v1_t
          tv2_t=tv1_t
          q2_t=q1_t
          oz2_t=oz1_t
          cw2_t=cw1_t
          ps2_t=ps1_t
            u1_t=u_t
            v1_t=v_t
            tv1_t=tv_t
            q1_t=q_t
            oz1_t=oz_t
            cw1_t=cw_t
            ps1_t=ps_t
  end if
  
end subroutine dynam_ab4


subroutine dynam_ab_ad(u,v,tv,q,oz,cw,ps,z,nnn,mype)
!
! Driver for adjoint of Adams-Bashforth time stepping
!
  use kinds, only: r_kind,i_kind
  use gridmod, only: lat2,lon2,nsig
  use tends4pertmod, only: ab_par
  implicit none

! Declare passed variables
  integer(i_kind)                       ,intent(in   ) :: nnn,mype
  real(r_kind),dimension(lat2,lon2)     ,intent(in   ) :: z
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout) :: u,v,tv,q,oz,cw
  real(r_kind),dimension(lat2,lon2)     ,intent(inout) :: ps

  select case(ab_par)
    case(2)
       call dynam_ab2_ad(u,v,tv,q,oz,cw,ps,z,nnn,mype)
     case(4)
       call dynam_ab4_ad(u,v,tv,q,oz,cw,ps,z,nnn,mype)
    case default
       call dynam_ab3_ad(u,v,tv,q,oz,cw,ps,z,nnn,mype)
  end select

end subroutine dynam_ab_ad


subroutine dynam_ab2_ad(u,v,tv,q,oz,cw,ps,z,nnn,mype)
!
! Adams-Bashforth 2-nd order for time stepping
!
  use kinds, only: r_kind,i_kind
  use constants, only: zero
  use tends4pertmod, only: time_step,itime,itime_max
  use tends4pertmod, only: u1_t,v1_t,tv1_t,q1_t,oz1_t,cw1_t,ps1_t
  use gridmod, only: lat2,lon2,nsig
  implicit none

! Declare passed variables
  integer(i_kind)                       ,intent(in   ) :: nnn,mype
  real(r_kind),dimension(lat2,lon2)     ,intent(in   ) :: z
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout) :: u,v,tv,q,oz,cw
  real(r_kind),dimension(lat2,lon2)     ,intent(inout) :: ps

! Declare local variables
  integer(i_kind) i,j,n 
  real(r_kind),dimension(lat2,lon2,nsig):: u_t,v_t,tv_t,q_t,oz_t,cw_t
  real(r_kind),dimension(lat2,lon2):: ps_t

  if(itime==itime_max) then
    u1_t=zero
    v1_t=zero
    tv1_t=zero
    q1_t=zero
    oz1_t=zero
    cw1_t=zero
    ps1_t=zero
  end if

  if(itime>1) then
     u_t  = ab21*time_step*u+u1_t
     v_t  = ab21*time_step*v+v1_t
     tv_t  = ab21*time_step*tv+tv1_t
     q_t  = ab21*time_step*q+q1_t
     oz_t  = ab21*time_step*oz+oz1_t
     cw_t  = ab21*time_step*cw+cw1_t
     ps_t  = ab21*time_step*ps+ps1_t

     u1_t  = ab22*time_step*u
     v1_t  = ab22*time_step*v
     tv1_t  = ab22*time_step*tv
     q1_t = ab22*time_step*q
     oz1_t = ab22*time_step*oz
     cw1_t = ab22*time_step*cw
     ps1_t = ab22*time_step*ps
  else
     u_t  = time_step*u+u1_t
     v_t  = time_step*v+v1_t
     tv_t  = time_step*tv+tv1_t
     q_t  = time_step*q+q1_t
     oz_t  = time_step*oz+oz1_t
     cw_t  = time_step*cw+cw1_t
     ps_t  = time_step*ps+ps1_t
  end if
        call funtend_ad(u,v,tv,q,oz,cw,ps,z,nnn,mype, &
                        u_t,v_t,tv_t,q_t,oz_t,cw_t,ps_t)
  
end subroutine dynam_ab2_ad

subroutine dynam_ab3_ad(u,v,tv,q,oz,cw,ps,z,nnn,mype)
!
! Adams-Bashforth 3-rd order for time stepping
!
  use kinds, only: r_kind,i_kind
  use constants, only: zero
  use tends4pertmod, only: time_step,itime,itime_max
  use tends4pertmod, only: u1_t,v1_t,tv1_t,q1_t,oz1_t,cw1_t,ps1_t
  use tends4pertmod, only: u2_t,v2_t,tv2_t,q2_t,oz2_t,cw2_t,ps2_t
  use gridmod, only: lat2,lon2,nsig
  implicit none

! Declare passed variables
  integer(i_kind)                       ,intent(in   ) :: nnn,mype
  real(r_kind),dimension(lat2,lon2)     ,intent(in   ) :: z
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout) :: u,v,tv,q,oz,cw
  real(r_kind),dimension(lat2,lon2)     ,intent(inout) :: ps

! Declare local variables
  integer(i_kind) i,j,n 
  real(r_kind),dimension(lat2,lon2,nsig):: u_t,v_t,tv_t,q_t,oz_t,cw_t
  real(r_kind),dimension(lat2,lon2):: ps_t

  if(itime==itime_max) then
    u_t=zero
    v_t=zero
    tv_t=zero
    q_t=zero
    oz_t=zero
    cw_t=zero
    ps_t=zero
      u1_t=zero
      v1_t=zero
      tv1_t=zero
      q1_t=zero
      oz1_t=zero
      cw1_t=zero
      ps1_t=zero
  end if

  if(itime<itime_max) then
    u_t=u1_t
    v_t=v1_t
    tv_t=tv1_t
    q_t=q1_t
    oz_t=oz1_t
    cw_t=cw1_t
    ps_t=ps1_t
      u1_t=u2_t
      v1_t=v2_t
      tv1_t=tv2_t
      q1_t=q2_t
      oz1_t=oz2_t
      cw1_t=cw2_t
      ps1_t=ps2_t
  end if  
  if(itime>2) then
     u_t  = ab31*time_step*u+u_t
     v_t  = ab31*time_step*v+v_t
     tv_t  = ab31*time_step*tv+tv_t
     q_t  = ab31*time_step*q+q_t
     oz_t  = ab31*time_step*oz+oz_t
     cw_t  = ab31*time_step*cw+cw_t
     ps_t  = ab31*time_step*ps+ps_t
       u1_t  = ab32*time_step*u+u1_t
       v1_t  = ab32*time_step*v+v1_t
       tv1_t  = ab32*time_step*tv+tv1_t
       q1_t = ab32*time_step*q+q1_t
       oz1_t = ab32*time_step*oz+oz1_t
       cw1_t = ab32*time_step*cw+cw1_t
       ps1_t = ab32*time_step*ps+ps1_t
         u2_t  = ab33*time_step*u
         v2_t  = ab33*time_step*v
         tv2_t  = ab33*time_step*tv
         q2_t = ab33*time_step*q
         oz2_t = ab33*time_step*oz
         cw2_t = ab33*time_step*cw
         ps2_t = ab33*time_step*ps
  else if(itime>1) then
     u_t  = ab21*time_step*u+u_t
     v_t  = ab21*time_step*v+v_t
     tv_t  = ab21*time_step*tv+tv_t
     q_t  = ab21*time_step*q+q_t
     oz_t  = ab21*time_step*oz+oz_t
     cw_t  = ab21*time_step*cw+cw_t
     ps_t  = ab21*time_step*ps+ps_t
       u1_t  = ab22*time_step*u+u1_t
       v1_t  = ab22*time_step*v+v1_t
       tv1_t  = ab22*time_step*tv+tv1_t
       q1_t  = ab22*time_step*q+q1_t
       oz1_t  = ab22*time_step*oz+oz1_t
       cw1_t  = ab22*time_step*cw+cw1_t
       ps1_t  = ab22*time_step*ps+ps1_t
  else
     u_t  = time_step*u+u_t
     v_t  = time_step*v+v_t
     tv_t  = time_step*tv+tv_t
     q_t  = time_step*q+q_t
     oz_t  = time_step*oz+oz_t
     cw_t  = time_step*cw+cw_t
     ps_t  = time_step*ps+ps_t
  end if
        call funtend_ad(u,v,tv,q,oz,cw,ps,z,nnn,mype, &
                        u_t,v_t,tv_t,q_t,oz_t,cw_t,ps_t)
  
end subroutine dynam_ab3_ad

subroutine dynam_ab4_ad(u,v,tv,q,oz,cw,ps,z,nnn,mype)
!
! Adams-Bashforth 4-th order for time stepping
!
  use kinds, only: r_kind,i_kind
  use constants, only: zero
  use tends4pertmod, only: time_step,itime,itime_max
  use tends4pertmod, only: u1_t,v1_t,tv1_t,q1_t,oz1_t,cw1_t,ps1_t
  use tends4pertmod, only: u2_t,v2_t,tv2_t,q2_t,oz2_t,cw2_t,ps2_t
  use tends4pertmod, only: u3_t,v3_t,tv3_t,q3_t,oz3_t,cw3_t,ps3_t
  use gridmod, only: lat2,lon2,nsig
  implicit none

! Declare passed variables
  integer(i_kind)                       ,intent(in   ) :: nnn,mype
  real(r_kind),dimension(lat2,lon2)     ,intent(in   ) :: z
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout) :: u,v,tv,q,oz,cw
  real(r_kind),dimension(lat2,lon2)     ,intent(inout) :: ps

! Declare local variables
  integer(i_kind) i,j,n 
  real(r_kind),dimension(lat2,lon2,nsig):: u_t,v_t,tv_t,q_t,oz_t,cw_t
  real(r_kind),dimension(lat2,lon2):: ps_t

  if(itime==itime_max) then
    u_t=zero
    v_t=zero
    tv_t=zero
    q_t=zero
    oz_t=zero
    cw_t=zero
    ps_t=zero
      u1_t=zero
      v1_t=zero
      tv1_t=zero
      q1_t=zero
      oz1_t=zero
      cw1_t=zero
      ps1_t=zero
        u2_t=zero
        v2_t=zero
        tv2_t=zero
        q2_t=zero
        oz2_t=zero
        cw2_t=zero
        ps2_t=zero
  end if

  if(itime<itime_max) then
    u_t=u1_t
    v_t=v1_t
    tv_t=tv1_t
    q_t=q1_t
    oz_t=oz1_t
    cw_t=cw1_t
    ps_t=ps1_t
      u1_t=u2_t
      v1_t=v2_t
      tv1_t=tv2_t
      q1_t=q2_t
      oz1_t=oz2_t
      cw1_t=cw2_t
      ps1_t=ps2_t
        u2_t=u3_t
        v2_t=v3_t
        tv2_t=tv3_t
        q2_t=q3_t
        oz2_t=oz3_t
        cw2_t=cw3_t
        ps2_t=ps3_t
  end if  
  if(itime>3) then
     u_t  = ab41*time_step*u+u_t
     v_t  = ab41*time_step*v+v_t
     tv_t  = ab41*time_step*tv+tv_t
     q_t  = ab41*time_step*q+q_t
     oz_t  = ab41*time_step*oz+oz_t
     cw_t  = ab41*time_step*cw+cw_t
     ps_t  = ab41*time_step*ps+ps_t
       u1_t  = ab42*time_step*u+u1_t
       v1_t  = ab42*time_step*v+v1_t
       tv1_t  = ab42*time_step*tv+tv1_t
       q1_t = ab42*time_step*q+q1_t
       oz1_t = ab42*time_step*oz+oz1_t
       cw1_t = ab42*time_step*cw+cw1_t
       ps1_t = ab42*time_step*ps+ps1_t
         u2_t  = ab43*time_step*u+u2_t
         v2_t  = ab43*time_step*v+v2_t
         tv2_t  = ab43*time_step*tv+tv2_t
         q2_t = ab43*time_step*q+q2_t
         oz2_t = ab43*time_step*oz+oz2_t
         cw2_t = ab43*time_step*cw+cw2_t
         ps2_t = ab43*time_step*ps+ps2_t
           u3_t  = ab44*time_step*u
           v3_t  = ab44*time_step*v
           tv3_t  = ab44*time_step*tv
           q3_t = ab44*time_step*q
           oz3_t = ab44*time_step*oz
           cw3_t = ab44*time_step*cw
           ps3_t = ab44*time_step*ps
  else if(itime>2) then
     u_t  = ab31*time_step*u+u_t
     v_t  = ab31*time_step*v+v_t
     tv_t  = ab31*time_step*tv+tv_t
     q_t  = ab31*time_step*q+q_t
     oz_t  = ab31*time_step*oz+oz_t
     cw_t  = ab31*time_step*cw+cw_t
     ps_t  = ab31*time_step*ps+ps_t
       u1_t  = ab32*time_step*u+u1_t
       v1_t  = ab32*time_step*v+v1_t
       tv1_t  = ab32*time_step*tv+tv1_t
       q1_t = ab32*time_step*q+q1_t
       oz1_t = ab32*time_step*oz+oz1_t
       cw1_t = ab32*time_step*cw+cw1_t
       ps1_t = ab32*time_step*ps+ps1_t
         u2_t  = ab33*time_step*u+u2_t
         v2_t  = ab33*time_step*v+v2_t
         tv2_t  = ab33*time_step*tv+tv2_t
         q2_t = ab33*time_step*q+q2_t
         oz2_t = ab33*time_step*oz+oz2_t
         cw2_t = ab33*time_step*cw+oz2_t
         ps2_t = ab33*time_step*ps+ps2_t
  else if(itime>1) then
     u_t  = ab21*time_step*u+u_t
     v_t  = ab21*time_step*v+v_t
     tv_t  = ab21*time_step*tv+tv_t
     q_t  = ab21*time_step*q+q_t
     oz_t  = ab21*time_step*oz+oz_t
     cw_t  = ab21*time_step*cw+cw_t
     ps_t  = ab21*time_step*ps+ps_t
       u1_t  = ab22*time_step*u+u1_t
       v1_t  = ab22*time_step*v+v1_t
       tv1_t  = ab22*time_step*tv+tv1_t
       q1_t  = ab22*time_step*q+q1_t
       oz1_t  = ab22*time_step*oz+oz1_t
       cw1_t  = ab22*time_step*cw+cw1_t
       ps1_t  = ab22*time_step*ps+ps1_t
  else
     u_t  = time_step*u+u_t
     v_t  = time_step*v+v_t
     tv_t  = time_step*tv+tv_t
     q_t  = time_step*q+q_t
     oz_t  = time_step*oz+oz_t
     cw_t  = time_step*cw+cw_t
     ps_t  = time_step*ps+ps_t
  end if
        call funtend_ad(u,v,tv,q,oz,cw,ps,z,nnn,mype, &
                        u_t,v_t,tv_t,q_t,oz_t,cw_t,ps_t)
  
end subroutine dynam_ab4_ad

end module dynamics_adams_bashforth
