subroutine calctends(u,v,t,ps,q,oz,cw,u_x,u_y,v_x,v_y,t_x,t_y,ps_x,ps_y,&
   q_x,q_y,oz_x,oz_y,cw_x,cw_y,z_x,z_y,divtflg,mype,u_t,v_t,t_t,p_t,q_t,oz_t,&
   cw_t,div_t,agv_t)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    calctends       calculate u,v,t,p tendencies
!   prgmmr: kleist           org: np20                date: 2005-09-29
!
! abstract: compute tendencies for pressure, wind, and virtual 
!           temperature
!
! program history log:
!   2005-09-29  kleist
!   2005-10-17  kleist - changes to improve computational efficiency
!   2005-11-21  kleist - add tracer tendencies, use new module
!   2006-04-12  treadon - replace sigi with bk5
!   2006-04-21  kleist - add divergence tendency
!   2006-07-31  kleist - changes to use ps instead of ln(ps)
!
! usage:
!   input argument list:
!     u        - zonal wind on subdomain
!     v        - meridional wind on subdomain
!     t        - virtual temperature on subdomain
!     ps       - ps on subdomain
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
!     z_x      - zonal derivative of sfc terrain height
!     z_y      - meridional derivative of sfc terrain height
!     mype     - task id
!     divtflg  - logical for calculating divergence tendency
!
!   output argument list:
!     u_t      - time tendency of u
!     v_t      - time tendency of v
!     t_t      - time tendency of t
!     p_t      - time tendency of 3d prs
!     q_t      - time tendency of q
!     oz_t     - time tendency of ozone
!     cw_t     - time tendency of cloud water
!     div_t    - time tendency of divergence
!     agv_t    - time tendency of ageostrophic vorticity
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds,only: r_kind,i_kind
  use gridmod, only: lat2,lon2,nsig,istart,rlats,nlat,bk5,&
     jstart,region_lat,region_lon,eta2_ll,wrf_nmm_regional,nlon,nsig1o
  use constants, only: zero,half,one,two,rearth,rd,rcp,omega,grav
  use tendsmod, only: what9,prsth9,r_prsum9,r_prdif9,prdif9,pr_xsum9,pr_xdif9,pr_ysum9,&
     pr_ydif9,curvfct,coriolis,ctph0,stph0,tlm0,t_over_pbar,dp_over_pbar
  use mpimod, only: levs_id,mpi_rtype,mpi_sum,mpi_comm_world,ierror
  implicit none

! Declare passed variables
  real(r_kind),dimension(lat2,lon2,nsig),intent(in):: u,v,t,u_x,u_y,v_x,v_y,&
     t_x,t_y,q,oz,cw,q_x,q_y,oz_x,oz_y,cw_x,cw_y
  real(r_kind),dimension(lat2,lon2),intent(in):: ps,ps_x,ps_y,z_x,z_y
  integer(i_kind),intent(in):: mype
  logical,intent(in):: divtflg
  real(r_kind),dimension(lat2,lon2,nsig),intent(out):: u_t,v_t,t_t,q_t,oz_t,cw_t,&
     div_t,agv_t
  real(r_kind),dimension(lat2,lon2,nsig+1),intent(out):: p_t

! Declare local variables
  real(r_kind),dimension(lat2,lon2,nsig+1):: pri_x,pri_y,pri
  real(r_kind),dimension(lat2,lon2):: sumk,sumvk,sum2k,sum2vk,sumkm1,sumvkm1,&
    sum2km1,sum2vkm1
  real(r_kind),dimension(lat2,lon2,nsig):: utx,vty,futy,fvtx,philap,massv_t
  real(r_kind),dimension(lat2,lon2):: termu,termv,termt
  real(r_kind),dimension(nsig):: t_over_p,dp_over_p
  real(r_kind) tmp,tmp2,count,count0,pmid
  integer(i_kind) i,j,k,ix,jx,nnn
  real(r_kind) relm,crlm,aph,sph,cph,cc,tph

! NOTES:
!  - equations taken from NCEP Office Note 445 (Juang 2005)
!  - this is the nonlinear routine, which currently in the GSI is only
!    called based on the current guess solution.  As such, basic state
!    variables that are needed for the TLM are loaded here (in the *9
!    arrays)

  p_t=zero ; t_t=zero ; u_t=zero ; v_t=zero ; q_t=zero ; oz_t=zero ; cw_t=zero

! constants
  if(wrf_nmm_regional) then
    do j=1,lon2
      jx=j+jstart(mype+1)-2
      jx=min(max(1,jx),nlon)
      do i=1,lat2
        ix=istart(mype+1)+i-2
        ix=min(max(ix,1),nlat)
        coriolis(i,j)=two*omega*sin(region_lat(ix,jx))
        relm=region_lon(ix,jx)-tlm0
        crlm=cos(relm)
        aph=region_lat(ix,jx)
        sph=sin(aph)
        cph=cos(aph)
        cc=cph*crlm
        tph=asin(ctph0*sph-stph0*cc)
        curvfct(i,j)=tan(tph)/rearth
      end do
    end do
  else
    do j=1,lon2
      do i=1,lat2
        ix=istart(mype+1)+i-2
        ix=min(max(ix,2),nlat-1)
        coriolis(i,j)=two*omega*sin(rlats(ix))
        curvfct(i,j)=tan(rlats(ix))/rearth
      end do
    end do
  end if

  call getprs(ps,ps_x,ps_y,pri,pri_x,pri_y)

! preliminaries
  do k=1,nsig
    do j=1,lon2
      do i=1,lat2
        r_prsum9(i,j,k)=one/(pri(i,j,k)+pri(i,j,k+1))
        prdif9(i,j,k)=pri(i,j,k)-pri(i,j,k+1)
        r_prdif9(i,j,k)=one/prdif9(i,j,k)
        pr_xsum9(i,j,k)=pri_x(i,j,k)+pri_x(i,j,k+1)
        pr_xdif9(i,j,k)=pri_x(i,j,k)-pri_x(i,j,k+1)
        pr_ysum9(i,j,k)=pri_y(i,j,k)+pri_y(i,j,k+1)
        pr_ydif9(i,j,k)=pri_y(i,j,k)-pri_y(i,j,k+1)
      end do
    end do
  end do


! need horizontal mean quantities for pseudo-geopotential tendency calculation
  t_over_p = zero ; dp_over_p=zero
  do k=1,nsig
    do j=2,lon2-1
      do i=2,lat2-1
        t_over_p(k)=t_over_p(k)+t(i,j,k)*r_prsum9(i,j,k)
        dp_over_p(k) = dp_over_p(k)+prdif9(i,j,k)*r_prsum9(i,j,k)
      end do
    end do
  end do
  count=(lat2-2)*(lon2-2)

  call mpi_allreduce(t_over_p,t_over_pbar,nsig,mpi_rtype,mpi_sum,mpi_comm_world,ierror)
  call mpi_allreduce(dp_over_p,dp_over_pbar,nsig,mpi_rtype,mpi_sum,mpi_comm_world,ierror)
  call mpi_allreduce(count,count0,1,mpi_rtype,mpi_sum,mpi_comm_world,ierror)
  do k=1,nsig
    t_over_pbar(k)=t_over_pbar(k)/count0
    dp_over_pbar(k)=dp_over_pbar(k)/count0
  end do


! 1) Compute horizontal part of tendency for 3d pressure (so dps/dt is the same
!    as prsth9(i,j,1) . . . also note that at the top, dp/dt=0
!    or: prsth9(i,j,nsig+1)=0
  do j=1,lon2
    do i=1,lat2
      prsth9(i,j,nsig+1)=zero
    end do
  end do
  do k=nsig,1,-1
    do j=1,lon2
      do i=1,lat2
        prsth9(i,j,k)=prsth9(i,j,k+1) - ( u(i,j,k)*pr_xdif9(i,j,k) + &
             v(i,j,k)*pr_ydif9(i,j,k) + (u_x(i,j,k) + v_y(i,j,k))* &
             prdif9(i,j,k) )
      end do
    end do
  end do   


! 2) calculate vertical velocity term:  z(dp/dz) (zero at top/bottom interfaces)
  what9=zero
  do k=2,nsig
    do j=1,lon2
      do i=1,lat2
        if(wrf_nmm_regional) then
          what9(i,j,k)=prsth9(i,j,k)-eta2_ll(k)*prsth9(i,j,1)
        else
          what9(i,j,k)=prsth9(i,j,k)-bk5(k)*prsth9(i,j,1)
        end if
      end do
    end do
  end do


! 3) load actual dp/dt here now, as prsth9 is reused in 
!    what9(i,k,1) & what9(i,j,nsig+1) = zero
!    p_t(i,j,1) is the same as the surface pressure tendency
  do k=1,nsig+1
    do j=1,lon2
      do i=1,lat2
        p_t(i,j,k)=prsth9(i,j,k)-what9(i,j,k)
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


! 4) Compute tendencies for wind components & Temperature
  do k=1,nsig
    termu=zero ; termv=zero ; termt=zero
    do j=1,lon2
      do i=1,lat2
        tmp=-rd*t(i,j,k)*r_prsum9(i,j,k)
        termu(i,j)=-u(i,j,k)*u_x(i,j,k) - v(i,j,k)*u_y(i,j,k) + &
           coriolis(i,j)*v(i,j,k)
        termu(i,j)=termu(i,j) + pr_xsum9(i,j,k)*tmp - grav*z_x(i,j)

        termv(i,j)=-u(i,j,k)*v_x(i,j,k) - v(i,j,k)*v_y(i,j,k) - &
           coriolis(i,j)*u(i,j,k) - curvfct(i,j)*(u(i,j,k)*u(i,j,k) + &
           v(i,j,k)*v(i,j,k))
        termv(i,j)=termv(i,j) + pr_ysum9(i,j,k)*tmp - grav*z_y(i,j)

        termt(i,j)=-u(i,j,k)*t_x(i,j,k) - v(i,j,k)*t_y(i,j,k)
        termt(i,j)=termt(i,j) -tmp*rcp * ( u(i,j,k)*pr_xsum9(i,j,k) + &
           v(i,j,k)*pr_ysum9(i,j,k) + &
           prsth9(i,j,k) + prsth9(i,j,k+1) )

! horizontal advection of "tracer" quantities
        q_t(i,j,k) = -u(i,j,k)*q_x(i,j,k) - v(i,j,k)*q_y(i,j,k)
        oz_t(i,j,k) = -u(i,j,k)*oz_x(i,j,k) - v(i,j,k)*oz_y(i,j,k)
        cw_t(i,j,k) = -u(i,j,k)*cw_x(i,j,k) - v(i,j,k)*cw_y(i,j,k)

! vertical flux terms
        if (k.gt.1) then
          tmp = half*what9(i,j,k)*r_prdif9(i,j,k)
          termu(i,j) = termu(i,j) - tmp*(u(i,j,k-1)-u(i,j,k))
          termv(i,j) = termv(i,j) - tmp*(v(i,j,k-1)-v(i,j,k))
          termt(i,j) = termt(i,j) - tmp*(t(i,j,k-1)-t(i,j,k))
          q_t(i,j,k) = q_t(i,j,k) - tmp*(q(i,j,k-1)-q(i,j,k))
          oz_t(i,j,k) = oz_t(i,j,k) - tmp*(oz(i,j,k-1)-oz(i,j,k))
          cw_t(i,j,k) = cw_t(i,j,k) - tmp*(cw(i,j,k-1)-cw(i,j,k))
        end if
        if (k.lt.nsig) then
          tmp = half*what9(i,j,k+1)*r_prdif9(i,j,k)
          termu(i,j) = termu(i,j) - tmp*(u(i,j,k)-u(i,j,k+1))
          termv(i,j) = termv(i,j) - tmp*(v(i,j,k)-v(i,j,k+1))
          termt(i,j) = termt(i,j) - tmp*(t(i,j,k)-t(i,j,k+1))
          q_t(i,j,k) = q_t(i,j,k) - tmp*(q(i,j,k)-q(i,j,k+1))
          oz_t(i,j,k) = oz_t(i,j,k) - tmp*(oz(i,j,k)-oz(i,j,k+1))
          cw_t(i,j,k) = cw_t(i,j,k) - tmp*(cw(i,j,k)-cw(i,j,k+1))
        end if        
      end do  !end do j
    end do    !end do i

    do j=1,lon2
      do i=1,lat2
        tmp = rd*t(i,j,k)*r_prsum9(i,j,k)
        tmp2 = prdif9(i,j,k)*r_prsum9(i,j,k)
        sumk(i,j)=sumkm1(i,j) + tmp*( pr_xdif9(i,j,k) - &
            tmp2*pr_xsum9(i,j,k) )
        sumvk(i,j)=sumvkm1(i,j) + tmp*( pr_ydif9(i,j,k) - &
            tmp2*pr_ysum9(i,j,k) )
        sum2k(i,j)=sum2km1(i,j) + t_x(i,j,k)*tmp2
        sum2vk(i,j)=sum2vkm1(i,j) + t_y(i,j,k)*tmp2

        termu(i,j) = termu(i,j) - sumkm1(i,j) - rd*sum2km1(i,j) - &
           sumk(i,j) - rd*sum2k(i,j) 
        termv(i,j) = termv(i,j) - sumvkm1(i,j) - rd*sum2vkm1(i,j) - &
           sumvk(i,j) - rd*sum2vk(i,j) 

! load up the km1 arrays for next k loop
        sumkm1(i,j)=sumk(i,j)
        sumvkm1(i,j)=sumvk(i,j)
        sum2km1(i,j)=sum2k(i,j)
        sum2vkm1(i,j)=sum2vk(i,j)
      end do
    end do

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


! NOTE:  not used for anything, can eventually remove to
! not use this memory allocation
  if (divtflg) then
    do k=1,nsig
      do j=1,lon2
        do i=1,lat2
          div_t(i,j,k)=zero
          agv_t(i,j,k)=zero
        end do
      end do
    end do
  end if  !end if divt block

  return
end subroutine calctends
