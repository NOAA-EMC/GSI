subroutine calctends_tl(u,v,t,ps,q,oz,cw,u_x,u_y,v_x,v_y,t_x,t_y,ps_x,ps_y,&
   q_x,q_y,oz_x,oz_y,cw_x,cw_y,divtflg,mype,u_t,v_t,t_t,p_t,q_t,oz_t,cw_t,&
   dtrs,atrs)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    calctends_tl       tlm of calctends
!   prgmmr: kleist           org: np20                date: 2005-09-29
!
! abstract: TLM of routine that compute tendencies for pressure, Tv, u, v 
!
! program history log:
!   2005-09-29  kleist
!   2005-10-17  kleist - changes to improve computational efficiency
!   2005-11-21  kleist - add tracer tendencies, use new module
!   2006-04-12  treadon - replace sigi with bk5
!   2006-04-21  kleist - add divergence tendency parts
!   2006-07-31  kleist - changes to use ps instead of ln(ps)
!   2006-09-21  kleist - add rescaling to divergence tendency formulation
!   2006-10-04  rancic - correct bug in tracer advection terms
!
! usage:
!   input argument list:
!     u        - zonal wind on subdomain
!     v        - meridional wind on subdomain
!     t        - virtual temperature on subdomain
!     ps       - ps on subdomain
!     q        - q on subdomain
!     oz       - ozone on subdomain
!     cw       - cloud water mixing ratio on subdomain 
!     u_x      - zonal derivative of u
!     u_y      - meridional derivative of u
!     v_x      - zonal derivative of v
!     v_y      - meridional derivative of v
!     t_x      - zonal derivative of t
!     t_y      - meridional derivative of t
!     ps_x     - zonal derivative of ps
!     ps_y     - meridional derivative of ps
!     q_x      - zonal derivative of q
!     q_y      - meridional derivative of q
!     oz_x     - zonal derivative of ozone
!     oz_y     - meridional derivative of ozone
!     cw_x     - zonal derivative of cloud water
!     cw_y     - meridional derivative of cloud water
!     mype     - task id
!     divtflg  - logical for divergence tendency calculation
!
!   output argument list:
!     u_t      - time tendency of u
!     v_t      - time tendency of v
!     t_t      - time tendency of t
!     p_t      - time tendency of 3d pressure
!     q_t      - time tendency of q
!     oz_t     - time tendency of ozone
!     cw_t     - time tendency of cloud water
!     dtrs     - rescaled time tendency of divergence
!     atrs     - rescaled time tendency of ageostrophic voriticity
!
!   notes:
!     TLM check performed & verified on 2005-09-29 by d. kleist
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds,only: r_kind,i_kind
  use gridmod, only: lat2,lon2,nsig,istart,rlats,nlat,bk5,&
      wrf_nmm_regional,eta2_ll,nsig1o
  use constants, only: zero,half,one,two,rearth,rd,rcp,omega,grav
  use tendsmod, only: what9,prsth9,r_prsum9,prdif9,r_prdif9,pr_xsum9,pr_xdif9,&
      pr_ysum9,pr_ydif9,curvfct,coriolis,dp_over_pbar,t_over_pbar
  use guess_grids, only: ntguessig,ges_u,&
      ges_u_lon,ges_u_lat,ges_v,ges_v_lon,ges_v_lat,ges_tv,ges_tvlat,ges_tvlon,&
      ges_q,ges_qlon,ges_qlat,ges_oz,ges_ozlon,ges_ozlat,ges_cwmr,ges_cwmr_lon,&
      ges_cwmr_lat
  use mpimod, only: levs_id
  implicit none

! Declare passed variables
  real(r_kind),dimension(lat2,lon2,nsig),intent(in):: u,v,t,u_x,u_y,&
     v_x,v_y,t_x,t_y
  real(r_kind),dimension(lat2,lon2,nsig),intent(in):: q,oz,cw,q_x,q_y,&
     oz_x,oz_y,cw_x,cw_y
  real(r_kind),dimension(lat2,lon2),intent(in):: ps,ps_x,ps_y
  integer(i_kind),intent(in):: mype
  logical,intent(in):: divtflg
  real(r_kind),dimension(lat2,lon2,nsig),intent(out):: u_t,v_t,t_t,q_t,oz_t,cw_t,&
     dtrs,atrs
  real(r_kind),dimension(lat2,lon2,nsig+1),intent(out):: p_t

! Declare local variables
  real(r_kind),dimension(lat2,lon2,nsig+1):: pri_x,pri_y,pri
  real(r_kind),dimension(lat2,lon2,nsig+1):: prsth,what
  real(r_kind),dimension(lat2,lon2,nsig):: prsum,prdif,pr_xsum,pr_xdif,&
       pr_ysum,pr_ydif
  real(r_kind),dimension(lat2,lon2,nsig):: utx,vty,futy,fvtx,philap,massv_t
  real(r_kind),dimension(lat2,lon2):: termu,termv,termt
  real(r_kind),dimension(lat2,lon2):: sumk,sumvk,sum2k,sum2vk,sumkm1,sumvkm1,&
    sum2km1,sum2vkm1
  real(r_kind),dimension(lat2,lon2,nsig):: div_t,agv_t

  real(r_kind) tmp,tmp2,tmp3,tmp4
  integer(i_kind) i,j,k,ix,it,nnn

! linearized about guess solution, so set it flag accordingly
  it=ntguessig

  call getprs_tl(ps,ps_x,ps_y,pri,pri_x,pri_y)

! preliminaries:
  do k=1,nsig
    do j=1,lon2
      do i=1,lat2
        prsum(i,j,k)=pri(i,j,k)+pri(i,j,k+1)
        prdif(i,j,k)=pri(i,j,k)-pri(i,j,k+1)
        pr_xsum(i,j,k)=pri_x(i,j,k)+pri_x(i,j,k+1)
        pr_xdif(i,j,k)=pri_x(i,j,k)-pri_x(i,j,k+1)
        pr_ysum(i,j,k)=pri_y(i,j,k)+pri_y(i,j,k+1)
        pr_ydif(i,j,k)=pri_y(i,j,k)-pri_y(i,j,k+1)
      end do
    end do
  end do

! 1) Compute horizontal part of tendency for 3d pressure
   do j=1,lon2
     do i=1,lat2
       prsth(i,j,nsig+1)=zero
     end do
   end do
   do k=nsig,1,-1
     do j=1,lon2
       do i=1,lat2
         prsth(i,j,k)=prsth(i,j,k+1) - ( u(i,j,k)*pr_xdif9(i,j,k) + &
            ges_u(i,j,k,it)*pr_xdif(i,j,k) + v(i,j,k)*pr_ydif9(i,j,k) + &
            ges_v(i,j,k,it)*pr_ydif(i,j,k) + &
            (u_x(i,j,k) + v_y(i,j,k))*(prdif9(i,j,k)) + &
            (ges_u_lon(i,j,k,it) + ges_v_lat(i,j,k,it))*(prdif(i,j,k)) )
       end do
     end do
   end do

! 2) calculate vertical velocity term:  z(dp/dz) (zero at top/bottom interfaces)
  do k=2,nsig
    do j=1,lon2
      do i=1,lat2
        if(wrf_nmm_regional) then
          what(i,j,k)=prsth(i,j,k)-eta2_ll(k)*prsth(i,j,1)
        else
          what(i,j,k)=prsth(i,j,k)-bk5(k)*prsth(i,j,1)
        end if
      end do
    end do
  end do
! top/bottom boundary condition:
  what(:,:,1)=zero ; what(:,:,nsig+1)=zero

! 3) load actual dp/dt
  do k=1,nsig+1
    do j=1,lon2
      do i=1,lat2
        p_t(i,j,k)=prsth(i,j,k)-what(i,j,k)
      end do
    end do
  end do

! before big k loop, zero out the km1 summation arrays
  do j=1,lon2
    do i=1,lat2
      sumkm1(i,j)=zero
      sum2km1(i,j)=zero
      sumvkm1(i,j)=zero
      sum2vkm1(i,j)=zero
    end do
  end do

! 4) Compute terms for tendencies of wind components & Temperature
  do k=1,nsig
    termu=zero ; termv=zero ; termt=zero
    do j=1,lon2
      do i=1,lat2
        termu(i,j)=-u(i,j,k)*ges_u_lon(i,j,k,it) - ges_u(i,j,k,it)*u_x(i,j,k) - &
           v(i,j,k)*ges_u_lat(i,j,k,it) - ges_v(i,j,k,it)*u_y(i,j,k) + &
           coriolis(i,j)*v(i,j,k)
        termv(i,j)=-u(i,j,k)*ges_v_lon(i,j,k,it) - ges_u(i,j,k,it)*v_x(i,j,k) - &
           v(i,j,k)*ges_v_lat(i,j,k,it) - ges_v(i,j,k,it)*v_y(i,j,k) - &
           coriolis(i,j)*u(i,j,k) - two*curvfct(i,j)*(ges_u(i,j,k,it)*u(i,j,k) + &
           ges_v(i,j,k,it)*v(i,j,k))
        termt(i,j)=-u(i,j,k)*ges_tvlon(i,j,k,it) - ges_u(i,j,k,it)*t_x(i,j,k) - &
           v(i,j,k)*ges_tvlat(i,j,k,it) - ges_v(i,j,k,it)*t_y(i,j,k)

! tracer advection terms
        q_t(i,j,k) = -u(i,j,k)*ges_qlon(i,j,k,it) - ges_u(i,j,k,it)*q_x(i,j,k) - &
           v(i,j,k)*ges_qlat(i,j,k,it) - ges_v(i,j,k,it)*q_y(i,j,k)
        oz_t(i,j,k) = -u(i,j,k)*ges_ozlon(i,j,k,it) - ges_u(i,j,k,it)*oz_x(i,j,k) - &
           v(i,j,k)*ges_ozlat(i,j,k,it) - ges_v(i,j,k,it)*oz_y(i,j,k)
        cw_t(i,j,k) = -u(i,j,k)*ges_cwmr_lon(i,j,k,it) - ges_u(i,j,k,it)*cw_x(i,j,k) - &
           v(i,j,k)*ges_cwmr_lat(i,j,k,it) - ges_v(i,j,k,it)*cw_y(i,j,k)

        tmp=rd*ges_tv(i,j,k,it)*r_prsum9(i,j,k)
        tmp2=rd*t(i,j,k)*r_prsum9(i,j,k)
        
        termu(i,j) = termu(i,j)-tmp*( pr_xsum(i,j,k) - &
           (prsum(i,j,k)*pr_xsum9(i,j,k)*r_prsum9(i,j,k)) ) - &
           tmp2*pr_xsum9(i,j,k)

        termv(i,j) = termv(i,j)-tmp*( pr_ysum(i,j,k) - &
           (prsum(i,j,k)*pr_ysum9(i,j,k)*r_prsum9(i,j,k)) ) - &
           tmp2*pr_ysum9(i,j,k)

        termt(i,j)=termt(i,j)+tmp*rcp*( ges_u(i,j,k,it)*pr_xsum(i,j,k) + &
           u(i,j,k)*pr_xsum9(i,j,k) + &
           ges_v(i,j,k,it)*pr_ysum(i,j,k) + &
           v(i,j,k)*pr_ysum9(i,j,k) + &
           prsth(i,j,k) + prsth(i,j,k+1))
        termt(i,j) = termt(i,j) + rcp*( ges_u(i,j,k,it)* &
           pr_xsum9(i,j,k) + &
           ges_v(i,j,k,it)*pr_ysum9(i,j,k) + &
           prsth9(i,j,k)+prsth9(i,j,k+1) ) * &
           (tmp2 - tmp*prsum(i,j,k)*r_prsum9(i,j,k) )

! vertical flux terms
        if (k.gt.1) then
          tmp=half*what(i,j,k)*r_prdif9(i,j,k)
          tmp2=half*what9(i,j,k)*r_prdif9(i,j,k)

          termu(i,j) = termu(i,j) - tmp*(ges_u(i,j,k-1,it)-ges_u(i,j,k,it)) - &
             tmp2*( (u(i,j,k-1)-u(i,j,k)) - (ges_u(i,j,k-1,it)-ges_u(i,j,k,it))* &
             prdif(i,j,k)*r_prdif9(i,j,k))          
          termv(i,j) = termv(i,j) - tmp*(ges_v(i,j,k-1,it)-ges_v(i,j,k,it)) - &
             tmp2*( (v(i,j,k-1)-v(i,j,k)) - (ges_v(i,j,k-1,it)-ges_v(i,j,k,it))* &
             prdif(i,j,k)*r_prdif9(i,j,k))
          termt(i,j) = termt(i,j) - tmp*(ges_tv(i,j,k-1,it)-ges_tv(i,j,k,it)) - &
             tmp2*( (t(i,j,k-1)-t(i,j,k)) - (ges_tv(i,j,k-1,it)-ges_tv(i,j,k,it))* &
             prdif(i,j,k)*r_prdif9(i,j,k))
          q_t(i,j,k) = q_t(i,j,k) - tmp*(ges_q(i,j,k-1,it)-ges_q(i,j,k,it)) - &
             tmp2*( (q(i,j,k-1)-q(i,j,k)) - (ges_q(i,j,k-1,it)-ges_q(i,j,k,it))* &
             prdif(i,j,k)*r_prdif9(i,j,k))
          oz_t(i,j,k) = oz_t(i,j,k) - tmp*(ges_oz(i,j,k-1,it)-ges_oz(i,j,k,it)) - &
             tmp2*( (oz(i,j,k-1)-oz(i,j,k)) - (ges_oz(i,j,k-1,it)-ges_oz(i,j,k,it))* &
             prdif(i,j,k)*r_prdif9(i,j,k))
          cw_t(i,j,k) = cw_t(i,j,k) - tmp*(ges_cwmr(i,j,k-1,it)-ges_cwmr(i,j,k,it)) - &
             tmp2*( (cw(i,j,k-1)-cw(i,j,k)) - (ges_cwmr(i,j,k-1,it)-ges_cwmr(i,j,k,it))* &
             prdif(i,j,k)*r_prdif9(i,j,k))
        end if
        if (k.lt.nsig) then
          tmp=half*what(i,j,k+1)*r_prdif9(i,j,k)
          tmp2=half*what9(i,j,k+1)*r_prdif9(i,j,k)
          termu(i,j) = termu(i,j) - tmp*(ges_u(i,j,k,it)-ges_u(i,j,k+1,it)) - &
             tmp2*( (u(i,j,k)-u(i,j,k+1)) - (ges_u(i,j,k,it)-ges_u(i,j,k+1,it))* &
             prdif(i,j,k)*r_prdif9(i,j,k))
          termv(i,j) = termv(i,j) - tmp*(ges_v(i,j,k,it)-ges_v(i,j,k+1,it)) - &
             tmp2*( (v(i,j,k)-v(i,j,k+1)) - (ges_v(i,j,k,it)-ges_v(i,j,k+1,it))* &
             prdif(i,j,k)*r_prdif9(i,j,k))
          termt(i,j) = termt(i,j) - tmp*(ges_tv(i,j,k,it)-ges_tv(i,j,k+1,it)) - &
             tmp2*( (t(i,j,k)-t(i,j,k+1)) - (ges_tv(i,j,k,it)-ges_tv(i,j,k+1,it))* &
             prdif(i,j,k)*r_prdif9(i,j,k))
          q_t(i,j,k) = q_t(i,j,k) - tmp*(ges_q(i,j,k,it)-ges_q(i,j,k+1,it)) - &
             tmp2*( (q(i,j,k)-q(i,j,k+1)) - (ges_q(i,j,k,it)-ges_q(i,j,k+1,it))* &
             prdif(i,j,k)*r_prdif9(i,j,k))
          oz_t(i,j,k) = oz_t(i,j,k) - tmp*(ges_oz(i,j,k,it)-ges_oz(i,j,k+1,it)) - &
             tmp2*( (oz(i,j,k)-oz(i,j,k+1)) - (ges_oz(i,j,k,it)-ges_oz(i,j,k+1,it))* &
             prdif(i,j,k)*r_prdif9(i,j,k))
          cw_t(i,j,k) = cw_t(i,j,k) - tmp*(ges_cwmr(i,j,k,it)-ges_cwmr(i,j,k+1,it)) - &
             tmp2*( (cw(i,j,k)-cw(i,j,k+1)) - (ges_cwmr(i,j,k,it)-ges_cwmr(i,j,k+1,it))* &
             prdif(i,j,k)*r_prdif9(i,j,k))
        end if
      end do   !end do i
    end do     !end do j

! first sum to level k-1     
    do j=1,lon2
      do i=1,lat2
        tmp=rd*t(i,j,k)*r_prsum9(i,j,k)
        tmp2=rd*ges_tv(i,j,k,it)*r_prsum9(i,j,k)
        tmp3=prdif9(i,j,k)*r_prsum9(i,j,k)

        sumk(i,j) = sumkm1(i,j) + ( tmp - tmp2*prsum(i,j,k)*r_prsum9(i,j,k) ) * &
           ( pr_xdif9(i,j,k) - ( pr_xsum9(i,j,k)*tmp3 ) )
        sumk(i,j) = sumk(i,j) + tmp2*( pr_xdif(i,j,k) - &
           (tmp3 *pr_xsum(i,j,k) + pr_xsum9(i,j,k)*( ( &
           prdif(i,j,k) - tmp3* prsum(i,j,k) )*r_prsum9(i,j,k) ) ) )

        sumvk(i,j) = sumvkm1(i,j) + ( tmp - tmp2*prsum(i,j,k)*r_prsum9(i,j,k) ) * &
           ( pr_ydif9(i,j,k) - ( pr_ysum9(i,j,k)*tmp3 ) )
        sumvk(i,j) = sumvk(i,j) + tmp2*( pr_ydif(i,j,k) - &
           (tmp3*pr_ysum(i,j,k) + pr_ysum9(i,j,k)*( ( &
           prdif(i,j,k) - tmp3* prsum(i,j,k) )*r_prsum9(i,j,k) ) ) )

        sum2k(i,j) = sum2km1(i,j) + t_x(i,j,k)*tmp3 + &
           ges_tvlon(i,j,k,it)*( (prdif(i,j,k) - &
           tmp3*prsum(i,j,k))*r_prsum9(i,j,k))

        sum2vk(i,j) = sum2vkm1(i,j) + t_y(i,j,k)*tmp3 + &
           ges_tvlat(i,j,k,it)*( (prdif(i,j,k) - &
           tmp3*prsum(i,j,k))*r_prsum9(i,j,k))

        termu(i,j) = termu(i,j) - sumkm1(i,j) - rd*sum2km1(i,j) - &
           sumk(i,j) - rd*sum2k(i,j)
        termv(i,j) = termv(i,j) - sumvkm1(i,j) - rd*sum2vkm1(i,j) - &
           sumvk(i,j) - rd*sum2vk(i,j)

! load up the km1 arrays for next k loop
        sumkm1(i,j)=sumk(i,j)
        sumvkm1(i,j)=sumvk(i,j)
        sum2km1(i,j)=sum2k(i,j)
        sum2vkm1(i,j)=sum2vk(i,j)

      end do !end do i
    end do   !end do j


! 5) Sum 2d individual terms into 3d tendency arrays
    do j=1,lon2
      do i=1,lat2
        ix=istart(mype+1)+i-2
        if ((ix.eq.1 .OR. ix.eq.nlat) .AND. (.not.wrf_nmm_regional)) then
          u_t(i,j,k)=zero
          v_t(i,j,k)=zero
        else
          u_t(i,j,k)=termu(i,j) ; v_t(i,j,k)=termv(i,j)
        end if
        t_t(i,j,k)=termt(i,j)
      end do
    end do

  end do  !end do k 

! Compute divergence tendency and ageostrophic vorticity if flag is true
! Get Mass Variable
!    in practice, lift code from calctends for contribution to termu from (R*T*p_x/p + phi_x)
!       with _x replaced with _t.
!      rtovpbar = global average of R*T/p, probably of background.
!

  if (divtflg) then
! before big k loop, zero out the km1 summation arrays
    do j=1,lon2
      do i=1,lat2
        sumkm1(i,j)=zero
        sum2km1(i,j)=zero
        termt(i,j)=zero
      end do
    end do

    do k=1,nsig
      tmp = rd*t_over_pbar(k)
      tmp2 = dp_over_pbar(k)
      do j=1,lon2
        do i=1,lat2
          termt(i,j)=(p_t(i,j,k)+p_t(i,j,k+1))*tmp
        end do
      end do

      do j=1,lon2
        do i=1,lat2
            sumk(i,j)=tmp* ( (p_t(i,j,k)-p_t(i,j,k+1)) - &
                       tmp2*(p_t(i,j,k)+p_t(i,j,k+1)) )
            sum2k(i,j)= t_t(i,j,k)*tmp2
            termt(i,j) = termt(i,j) + sumkm1(i,j) + rd*sum2km1(i,j) + &
               half*sumk(i,j) + half*rd*sum2k(i,j)

! load up the km1 arrays for next k loop
          sumkm1(i,j)=sumkm1(i,j)+sumk(i,j)
          sum2km1(i,j)=sum2km1(i,j)+sum2k(i,j)
        end do
      end do
      do j=1,lon2
        do i=1,lat2
          massv_t(i,j,k)=termt(i,j)
        end do
      end do
    end do  ! end do k

! get derivatives here
    nnn=0
    do k=1,nsig1o
      if (levs_id(k)/=0) nnn=nnn+1
    end do
    call get_tend_derivs(u_t,v_t,massv_t,utx,vty,futy,fvtx,philap,nnn,mype)

    do k=1,nsig
      do j=1,lon2
        do i=1,lat2
          div_t(i,j,k)=utx(i,j,k) + vty(i,j,k)
          agv_t(i,j,k)=fvtx(i,j,k)-futy(i,j,k)-philap(i,j,k)
        end do
      end do
    end do

! Call rescaling routine to put in units of energy
    dtrs=zero ; atrs=zero
    call jcrescale(div_t,agv_t,dtrs,atrs)

  end if  ! End if div tendency block

  return
end subroutine calctends_tl
