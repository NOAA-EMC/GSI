subroutine hdiff(u_x,u_y,v_x,v_y,t_x,t_y,u_t,v_t,t_t,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    hdiff           calculate horiziontal diffusion
!   prgmmr: rancic                                    
!
! abstract: compute horizontal diffusion for wind, and virtual 
!           temperature
!
! program history log:
!   2010-02-25  rancic
!
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind  
  use constants, only: zero,half,one,two
  use gridmod, only: lat2,lon2,nsig
  use tends4pertmod, only: time_step
  implicit none

! Declare passed variables
  integer(i_kind)                       ,intent(in   ) :: mype
  real(r_kind),dimension(lat2,lon2,nsig),intent(in   ) :: u_x,u_y,v_x,v_y,t_x,t_y
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout) :: u_t,v_t,t_t

! Declare local variables
  real(r_kind),dimension(lat2,lon2,nsig):: uc_x,uc_y,vc_x,vc_y,tc_x,tc_y
  real(r_kind),dimension(lat2,lon2,nsig):: u_xx,u_yy,v_xx,v_yy,t_xx,t_yy
  real(r_kind) uave,vave,up,vp
  real(r_kind) khdff,kht,facdec
  integer(i_kind) i,j,k

  khdff=1.73e04_r_kind
  kht=khdff*time_step

   t_xx=zero; t_yy=zero
   u_xx=zero; u_yy=zero
   v_xx=zero; v_yy=zero
 
  do k=1,nsig
    if(k<nsig-2) then
      facdec=exp(-3.*(k-1)/(nsig-1))   
    else
      facdec=one
    end if
      facdec=facdec*kht
  do j=1,lon2
  do i=1,lat2
    uc_x(i,j,k)=u_x(i,j,k)*facdec
    vc_x(i,j,k)=v_x(i,j,k)*facdec
    tc_x(i,j,k)=t_x(i,j,k)*facdec
    uc_y(i,j,k)=u_y(i,j,k)*facdec
    vc_y(i,j,k)=v_y(i,j,k)*facdec
    tc_y(i,j,k)=t_y(i,j,k)*facdec
  end do
  end do
  end do

  call mp_compact_dlon2(uc_x,u_xx,.false.,nsig,mype)
  call mp_compact_dlon2(vc_x,v_xx,.false.,nsig,mype)
  call mp_compact_dlon2(tc_x,t_xx,.false.,nsig,mype)
  call mp_compact_dlat2(uc_y,u_yy,.false.,nsig,mype)     
  call mp_compact_dlat2(vc_y,v_yy,.false.,nsig,mype)
  call mp_compact_dlat2(tc_y,t_yy,.false.,nsig,mype)
      
  do k=1,nsig
    do j=1,lon2
    do i=1,lat2
      u_t(i,j,k)=u_t(i,j,k)+u_xx(i,j,k)+u_yy(i,j,k)
      v_t(i,j,k)=v_t(i,j,k)+v_xx(i,j,k)+v_yy(i,j,k)
      t_t(i,j,k)=t_t(i,j,k)+t_xx(i,j,k)+t_yy(i,j,k)
    end do
    end do
  end do


end subroutine hdiff

subroutine hdiff_ad(u_x,u_y,v_x,v_y,t_x,t_y,u_t,v_t,t_t,mype)
  use kinds,only: r_kind,i_kind  
  use constants, only: zero,half,one,two
  use gridmod, only: lat2,lon2,nsig
  use tends4pertmod, only: time_step
  implicit none

! Declare passed variables
  integer(i_kind)                       ,intent(in   ) :: mype
  real(r_kind),dimension(lat2,lon2,nsig),intent(  out) :: u_x,u_y,v_x,v_y,t_x,t_y
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout) :: u_t,v_t,t_t

! Declare local variables
  integer(i_kind) i,j,k
  real(r_kind),dimension(lat2,lon2,nsig):: uc_x,uc_y,vc_x,vc_y,tc_x,tc_y
  real(r_kind),dimension(lat2,lon2,nsig):: u_xx,u_yy,v_xx,v_yy,t_xx,t_yy
  real(r_kind) khdff,kht,facdec
!
! Preliminaries
!

  khdff=1.73e04_r_kind
  kht=khdff*time_step

!
! Start adjoint
!
   t_xx=t_t   ;   t_yy=t_t
   u_xx=u_t   ;   u_yy=u_t
   v_xx=v_t   ;   v_yy=v_t

  tc_x=zero   ;   tc_y=zero
  uc_x=zero   ;   uc_y=zero
  vc_x=zero   ;   vc_y=zero

  call mp_compact_dlon2_ad(tc_x,t_xx,.false.,nsig,mype)
  call mp_compact_dlon2_ad(vc_x,v_xx,.false.,nsig,mype)
  call mp_compact_dlon2_ad(uc_x,u_xx,.false.,nsig,mype)  
  call mp_compact_dlat2_ad(tc_y,t_yy,.false.,nsig,mype)
  call mp_compact_dlat2_ad(vc_y,v_yy,.false.,nsig,mype)
  call mp_compact_dlat2_ad(uc_y,u_yy,.false.,nsig,mype)     


  do k=nsig,1,-1
    if(k<nsig-2) then
      facdec=exp(-3.*(k-1)/(nsig-1))   
    else
      facdec=one
    end if
      facdec=facdec*kht
  do j=1,lon2
  do i=1,lat2
    u_x(i,j,k)=uc_x(i,j,k)*facdec
    v_x(i,j,k)=vc_x(i,j,k)*facdec
    t_x(i,j,k)=tc_x(i,j,k)*facdec
    u_y(i,j,k)=uc_y(i,j,k)*facdec
    v_y(i,j,k)=vc_y(i,j,k)*facdec
    t_y(i,j,k)=tc_y(i,j,k)*facdec
  end do
  end do
  end do

end subroutine hdiff_ad
