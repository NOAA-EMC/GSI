subroutine sfcdrag(u,v,t,pri,u_t,v_t)
  use kinds,only: r_kind,i_kind  
  use constants, only: zero,half,one,two,rd,grav,izero
  use gridmod, only: lat2,lon2,nsig
  use guess_grids, only: isli2
  use pblmod, only: cdu,cdv, &
                    adragu,bdragu,cdragu,ddragu, &
                    adragv,bdragv,cdragv,ddragv
  implicit none

! Declare passed variables
  real(r_kind),dimension(lat2,lon2,nsig),intent(in):: u,v,t
  real(r_kind),dimension(lat2,lon2,nsig+1),intent(in):: pri
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout):: u_t,v_t
  real(r_kind) cd_land,cd_watr,g2rd,const_a,cd
  real(r_kind) pri1,pri2,rp12,rt12,rpt12,ku,kv, &
        u12,u12sq,v12,v12sq,ax1_u,ax2_u,ax3_u,ax1_v,ax2_v,ax3_v

! Declare local variables
  integer(i_kind) i,j

  
!------from Garratt(1977) MWR -------------------------------------!
  cd_land=1.64e-03_r_kind 
  cd_watr=0.41e-03_r_kind 
!!  cd_land=1.64e-03_r_kind *10.0
!!  cd_watr=0.41e-03_r_kind *10.0
!------------------------------------------------------------------!
  g2rd=two*grav/rd

  do j=1,lon2
  do i=1,lat2
    if(isli2(i,j)==1) then
        cd=cd_land*g2rd
    else
        cd=cd_watr*g2rd
    end if
    if(u(i,j,1)>zero) then
       cdu(i,j)=-cd
    else
       cdu(i,j)= cd
    end if
    if(v(i,j,1)>zero) then
       cdv(i,j)=-cd
    else
       cdv(i,j)= cd
    end if
!------------------------------------------------------------------
!const_a=pri(i,j,2)/( (t(i,j,1)+t(i,j,2))*(pri(i,j,1)-pri(i,j,2)) )
! u_t(i,j,1)=u_t(i,j,1)+const_a*cdu(i,j)*(u(i,j,1)+u(i,j,2))**2
! v_t(i,j,1)=v_t(i,j,1)+const_a*cdv(i,j)*(v(i,j,1)+v(i,j,2))**2
!------------------------------------------------------------------

    pri1=pri(i,j,1)
    pri2=pri(i,j,2)
    rp12=one/(pri1-pri2)
    rt12=one/(t(i,j,1)+t(i,j,2))
    rpt12=rp12*rt12
    ku=cdu(i,j)*rpt12
    kv=cdv(i,j)*rpt12
    u12=u(i,j,1)+u(i,j,2)
    u12sq=u12*u12
    v12=v(i,j,1)+v(i,j,2)
    v12sq=v12*v12
      u_t(i,j,1)=u_t(i,j,1)+ku*u12sq*pri2
      v_t(i,j,1)=v_t(i,j,1)+kv*v12sq*pri2
    ax1_u=ku*u12sq*rp12 
    ax1_v=kv*v12sq*rp12 
    ax2_u=ku*pri2*u12
    ax2_v=kv*pri2*v12
    ax3_u=ku*pri2*u12sq
    ax3_v=kv*pri2*v12sq
      adragu(i,j)=-ax1_u*pri2
      bdragu(i,j)= ax1_u*pri1
      cdragu(i,j)= ax2_u*two
      ddragu(i,j)=-ax3_u*rt12
      adragv(i,j)=-ax1_v*pri2
      bdragv(i,j)= ax1_v*pri1
      cdragv(i,j)= ax2_v*two
      ddragv(i,j)=-ax3_v*rt12
  end do
  end do

end subroutine sfcdrag

subroutine sfcdrag_tl(u,v,t,pri,u_t,v_t)
  use kinds,only: r_kind,i_kind  
  use constants, only: zero,one,half,two,izero
  use gridmod, only: lat2,lon2,nsig
  use pblmod, only: adragu,bdragu,cdragu,ddragu, &
                    adragv,bdragv,cdragv,ddragv
  implicit none

! Declare passed variables
  real(r_kind),dimension(lat2,lon2,nsig),intent(in):: u,v,t
  real(r_kind),dimension(lat2,lon2,nsig+1),intent(in):: pri 
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout):: u_t,v_t

! Declare local variables
  integer(i_kind) i,j


  do j=1,lon2
  do i=1,lat2
      u_t(i,j,1)=u_t(i,j,1)+adragu(i,j)*pri(i,j,1) &
                           +bdragu(i,j)*pri(i,j,2) &
                           +cdragu(i,j)*u  (i,j,1) &
                           +cdragu(i,j)*u  (i,j,2) &
                           +ddragu(i,j)*t  (i,j,1) &
                           +ddragu(i,j)*t  (i,j,2)  
      v_t(i,j,1)=v_t(i,j,1)+adragv(i,j)*pri(i,j,1) &
                           +bdragv(i,j)*pri(i,j,2) &
                           +cdragv(i,j)*v  (i,j,1) &
                           +cdragv(i,j)*v  (i,j,2) &
                           +ddragv(i,j)*t  (i,j,1) &
                           +ddragv(i,j)*t  (i,j,2) 
  end do
  end do

end subroutine sfcdrag_tl

subroutine sfcdrag_ad(u,v,t,pri,u_t,v_t)
  use kinds,only: r_kind,i_kind  
  use constants, only: izero
  use gridmod, only: lat2,lon2,nsig
  use pblmod, only: adragu,bdragu,cdragu,ddragu, &
                    adragv,bdragv,cdragv,ddragv
  implicit none

! Declare passed variables
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout):: u,v,t
  real(r_kind),dimension(lat2,lon2,nsig+1),intent(inout):: pri 
  real(r_kind),dimension(lat2,lon2,nsig),intent(in):: u_t,v_t

  integer(i_kind) i,j
  

  do j=1,lon2
  do i=1,lat2
    pri(i,j,1)=pri(i,j,1)+adragu(i,j)*u_t(i,j,1)
    pri(i,j,2)=pri(i,j,2)+bdragu(i,j)*u_t(i,j,1)
    u(i,j,1)=u(i,j,1)+cdragu(i,j)*u_t(i,j,1)
    u(i,j,2)=u(i,j,2)+cdragu(i,j)*u_t(i,j,1)
    t(i,j,1)=t(i,j,1)+ddragu(i,j)*u_t(i,j,1)
    t(i,j,2)=t(i,j,2)+ddragu(i,j)*u_t(i,j,1)
    pri(i,j,1)=pri(i,j,1)+adragv(i,j)*v_t(i,j,1)
    pri(i,j,2)=pri(i,j,2)+bdragv(i,j)*v_t(i,j,1)
    v(i,j,1)=v(i,j,1)+cdragv(i,j)*v_t(i,j,1)
    v(i,j,2)=v(i,j,2)+cdragv(i,j)*v_t(i,j,1)
    t(i,j,1)=t(i,j,1)+ddragv(i,j)*v_t(i,j,1)
    t(i,j,2)=t(i,j,2)+ddragv(i,j)*v_t(i,j,1)
  end do
  end do

end subroutine sfcdrag_ad
