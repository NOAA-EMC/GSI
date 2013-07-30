subroutine calctends_model_ad(u,v,t,q,oz,cw,pri, & 
   phi_x,phi_y,u_x,u_y,v_x,v_y,t_x,t_y,pri_x,pri_y,q_x,q_y,oz_x,oz_y,cw_x,cw_y, &
   mype,u_t,v_t,t_t,q_t,oz_t,cw_t,ps_t)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    calctends_model_ad         adjoint of calctends_model_tl
!   prgmmr: kleist           org: np20                date: 2005-09-29
!
! abstract: adjoint of routine that compute tendencies for u,v,Tv,prs
!
! program history log:
!   2005-09-29  kleist
!   2005-10-17  kleist - changes to improve computational efficiency
!   2005-11-21  kleist - add tracer tendencies, use new module
!   2006-01-31  kleist - add indices to sum* variables being initialized in loop
!   2006-04-12  treadon - replace sigi with bk5
!   2006-04-21  kleist - add divergence tendency bits
!   2006-07-31  kleist - changes to use ps instead of ln(ps)
!   2006-09-21  kleist - add rescaling to divergence tendency formulation
!   2007-03-13  kleist - move jcrescale_ad (and related code) into if (divtflg) block
!   2007-04-16  kleist - move constraint specific items elsewhere
!   2007-05-08  kleist - add bits for fully generalized vertical coordinate
!   2007-06-21  rancic - add pbl 
!   2007-07-26  cucurull - add 3d pressure pri in argument list;
!                          move getprs_ad outside calctends_ad;
!                          call getprs_horiz_ad; remove ps from argument
!                          list
!   2007-08-08  derber - optimize
!   2008-06-05  safford - rm unused var "nnn" and unused uses
!   2010-02-24  rancic - adjust for use in 4dvar perturbation model 
!
! usage:
!   input argument list:
!     u        - zonal wind on subdomain
!     v        - meridional wind on subdomain
!     t        - virtual temperature on subdomain
!     q        - q on subdomain
!     oz       - ozone on subdomain
!     cw       - cloud water mixing ratio on subdomain
!     u_x      - zonal derivative of u
!     u_y      - meridional derivative of u
!     v_x      - zonal derivative of v
!     v_y      - meridional derivative of v
!     t_x      - zonal derivative of t
!     t_y      - meridional derivative of t
!!!     ps_x     - zonal derivative of ps
!!!     ps_y     - meridional derivative of ps
!     pri_x
!     pri_y
!     q_x      - zonal derivative of q
!     q_y      - meridional derivative of q
!     oz_x     - zonal derivative of ozone
!     oz_y     - meridional derivative of ozone
!     cw_x     - zonal derivative of cloud water
!     cw_y     - meridional derivative of cloud water
!     u_t      - time tendency of u
!     v_t      - time tendency of v
!     t_t      - time tendency of t
!     p_t      - time tendency of 3d prs
!     q_t      - time tendency of q
!     oz_t     - time tendency of ozone
!     cw_t     - time tendency of cloud water
!     mype     - mpi integer task id
!
!   output argument list:
!     u        - zonal wind on subdomain
!     v        - meridional wind on subdomain
!     t        - virtual temperature on subdomain
!     q        - q on subdomain
!     oz       - ozone on subdomain
!     cw       - cloud water mixing ratio on subdomain
!     u_x      - zonal derivative of u
!     u_y      - meridional derivative of u
!     v_x      - zonal derivative of v
!     v_y      - meridional derivative of v
!     t_x      - zonal derivative of t
!     t_y      - meridional derivative of t
!!!     ps_x     - zonal derivative of ps
!!!     ps_y     - meridional derivative of ps
!     pri_x     - zonal derivative of pri
!     pri_y     - meridional derivative of pri
!     q_x      - zonal derivative of q
!     q_y      - meridional derivative of q
!     oz_x     - zonal derivative of ozone
!     oz_y     - meridional derivative of ozone
!     cw_x     - zonal derivative of cloud water
!     cw_y     - meridional derivative of cloud water
!
!   notes:
!     adjoint check performed succesfully on 2005-09-29 by d. kleist
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds,only: r_kind,i_kind
  use gridmod, only: lat2,lon2,nsig,istart,rlats,nlon,nlat,idvc5,bk5,&
     eta2_ll,wrf_nmm_regional,regional,region_lat,region_lon,jstart,corlats
  use constants, only: ione,zero,half,one,two,rd,rcp,rearth,grav,pi,omega
  use tendsmod, only: coriolis,ctph0,stph0,tlm0
  use tends4pertmod, only: curvfct,time_step
  use nonlinmod, only: bck_u,bck_v,bck_tv,bck_q,bck_oz,bck_cw, &
     bck_u_lon,bck_u_lat,bck_v_lon,bck_v_lat,bck_tvlat,bck_tvlon, &
     bck_qlon,bck_qlat,bck_ozlon,bck_ozlat,bck_cwlon,bck_cwlat, &
     what_bck,prsth_bck,prsum_bck,r_prsum_bck,prdif_bck,r_prdif_bck,pr_xsum_bck,&
     pr_ysum_bck,rdtop_bck
  implicit none

! Declare passed variables
  real(r_kind),dimension(lat2,lon2,nsig)     ,intent(in   ) :: t_t,q_t,oz_t,cw_t
  real(r_kind),dimension(lat2,lon2)          ,intent(in   ) :: ps_t
  integer(i_kind)                            ,intent(in   ) :: mype
  real(r_kind),dimension(lat2,lon2,nsig)     ,intent(inout) :: u_t,v_t
  real(r_kind),dimension(lat2,lon2,nsig)     ,intent(inout) :: u,v,t,u_x,u_y,v_x,v_y,&
     t_x,t_y,q,oz,cw,q_x,q_y,oz_x,oz_y,cw_x,cw_y,phi_x,phi_y
  real(r_kind),dimension(lat2,lon2,nsig+ione),intent(inout) :: pri,pri_x,pri_y

! Declare local variables
  real(r_kind),dimension(lat2,lon2,nsig+ione):: prsth,what
  real(r_kind),dimension(lat2,lon2,nsig):: prsum,prdif,pr_xsum,pr_xdif,pr_ysum,&
       pr_ydif
  real(r_kind),dimension(lat2,lon2,nsig):: prdifu,prdifv
  real(r_kind),dimension(lat2,lon2,nsig):: pgf_x,pgf_y
  real(r_kind),dimension(lat2,lon2,nsig):: rdtop,div,prdifu_x,prdifv_y2
  real(r_kind),dimension(lat2,lon2):: rdrdx2,rdrdy2
  real(r_kind) psum,psum9,psump1,psum9p1
  real(r_kind) rr,dlam,dphi,wpdar,pdif
  real(r_kind) relm,crlm,aph,sph,cph,cc,tph
  real(r_kind),dimension(lat2,lon2,nsig):: t_thor9
  real(r_kind) tmp,tmp1,tmp2,tmp3,var,sumk,sumvk,sum2k,sum2vk
  real(r_kind) tmp9,tmp9u,tmp9v,tmp9t,tmp9q,tmp9oz,tmp9cw
  integer(i_kind) i,j,k,ix,jx
  integer(i_kind) :: jjstart,jjstop
  integer(i_kind) :: nth,tid,omp_get_num_threads,omp_get_thread_num

  jjstart=1
  jjstop=lon2


! Preliminaries for (0)

! constants
  if(wrf_nmm_regional) then
    do j=1,lon2
      jx=j+jstart(mype+ione)-2_i_kind
      jx=min(max(1,jx),nlon)
      do i=1,lat2
        ix=istart(mype+ione)+i-2_i_kind
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
        ix=istart(mype+ione)+i-2_i_kind
        ix=min(max(ix,2),nlat-1)
        coriolis(i,j)=corlats(ix)
        curvfct(i,j)=tan(rlats(ix))/rearth
      end do
    end do
  end if

  rr=rd/rearth**2
  dlam=two*pi/nlon
  dphi=pi/nlat      

    do j=1,lon2
      do i=1,lat2
        ix=istart(mype+ione)+i-2_i_kind
        ix=min(max(ix,2),nlat-ione)
        rdrdx2(i,j)=rr/(cos(rlats(ix))*dlam)**2
        rdrdy2(i,j)=rr/dphi**2
      end do
    end do

! zero arrays
  do k=1,nsig+ione
    do j=jjstart,jjstop
      do i=1,lat2
        what(i,j,k)=zero
        prsth(i,j,k)=zero
        pri(i,j,k)=zero
      end do
    end do
  end do
  do k=1,nsig
    do j=jjstart,jjstop
      do i=1,lat2
        prsum(i,j,k)=zero
        prdif(i,j,k)=zero
        pr_xsum(i,j,k)=zero
        pr_xdif(i,j,k)=zero
        pr_ysum(i,j,k)=zero
        pr_ydif(i,j,k)=zero
        pgf_x(i,j,k)=zero
        pgf_y(i,j,k)=zero
        prdifu_x(i,j,k)=zero
        prdifv_y2(i,j,k)=zero
        rdtop(i,j,k)=zero
        div(i,j,k)=zero
        prdifu(i,j,k)=zero
        prdifv(i,j,k)=zero
      end do
    end do
  end do

  if(.not.wrf_nmm_regional)then
    do k=1,nsig
      do j=jjstart,jjstop
        do i=1,lat2
          ix=istart(mype+ione)+i-2_i_kind
          if (ix == 1 .or. ix == nlat) then
            u_t(i,j,k)=zero ; v_t(i,j,k)=zero
          end if
        end do
      end do
    end do                         
  end if

        call sfcdrag_ad(u,v,t,pri,u_t,v_t)
        call hdiff_ad(u_x,u_y,v_x,v_y,t_x,t_y,u_t,v_t,t_t,mype)

! adjoint of vertical flux terms

    do k=nsig,1,-ione
      do j=jjstart,jjstop
        do i=1,lat2
          if(k < nsig) then
              tmp2=half*what_bck(i,j,k+ione)*r_prdif_bck(i,j,k)
              tmp9q=bck_q(i,j,k)-bck_q(i,j,k+ione) 
              tmp9oz=bck_oz(i,j,k)-bck_oz(i,j,k+ione) 
              tmp9cw=bck_cw(i,j,k)-bck_cw(i,j,k+ione) 
              tmp9u=bck_u(i,j,k)-bck_u(i,j,k+ione) 
              tmp9v=bck_v(i,j,k)-bck_v(i,j,k+ione) 
              tmp9t=bck_tv(i,j,k)-bck_tv(i,j,k+ione) 
              tmp =-u_t(i,j,k)*tmp9u-v_t(i,j,k)*tmp9v-t_t(i,j,k)*tmp9t &
                   -q_t(i,j,k)*tmp9q-oz_t(i,j,k)*tmp9oz-cw_t(i,j,k)*tmp9cw
              pdif= -tmp2*tmp     
            q(i,j,k) = q(i,j,k) - q_t(i,j,k)*tmp2
            q(i,j,k+ione) = q(i,j,k+ione) + q_t(i,j,k)*tmp2
            oz(i,j,k) = oz(i,j,k) - oz_t(i,j,k)*tmp2
            oz(i,j,k+ione) = oz(i,j,k+ione) + oz_t(i,j,k)*tmp2
            cw(i,j,k) = cw(i,j,k) - cw_t(i,j,k)*tmp2
            cw(i,j,k+ione) = cw(i,j,k+ione) + cw_t(i,j,k)*tmp2
            t(i,j,k) = t(i,j,k) - t_t(i,j,k)*tmp2
            t(i,j,k+ione) = t(i,j,k+ione) + t_t(i,j,k)*tmp2
            u(i,j,k) = u(i,j,k) - u_t(i,j,k)*tmp2
            u(i,j,k+ione) = u(i,j,k+ione) + u_t(i,j,k)*tmp2
            v(i,j,k) = v(i,j,k) - v_t(i,j,k)*tmp2
            v(i,j,k+ione) = v(i,j,k+ione) + v_t(i,j,k)*tmp2
            prdif(i,j,k) = prdif(i,j,k)+pdif*r_prdif_bck(i,j,k) 
            what(i,j,k+ione) = what(i,j,k+ione) + half*tmp*r_prdif_bck(i,j,k)
          end if

          if(k > 1)then
              tmp2=half*what_bck(i,j,k)*r_prdif_bck(i,j,k)
              tmp9q=bck_q(i,j,k-ione)-bck_q(i,j,k) 
              tmp9oz=bck_oz(i,j,k-ione)-bck_oz(i,j,k) 
              tmp9cw=bck_cw(i,j,k-ione)-bck_cw(i,j,k) 
              tmp9u=bck_u(i,j,k-ione)-bck_u(i,j,k) 
              tmp9v=bck_v(i,j,k-ione)-bck_v(i,j,k) 
              tmp9t=bck_tv(i,j,k-ione)-bck_tv(i,j,k) 
              tmp=-q_t(i,j,k)*tmp9q-oz_t(i,j,k)*tmp9oz-cw_t(i,j,k)*tmp9cw &
                  -u_t(i,j,k)*tmp9u-v_t(i,j,k)*tmp9v-t_t(i,j,k)*tmp9t
              pdif= -tmp2*tmp     
            q(i,j,k-ione) = q(i,j,k-ione) - q_t(i,j,k)*tmp2
            q(i,j,k) = q(i,j,k) + q_t(i,j,k)*tmp2
            oz(i,j,k-ione) = oz(i,j,k-ione) - oz_t(i,j,k)*tmp2
            oz(i,j,k) = oz(i,j,k) + oz_t(i,j,k)*tmp2
            cw(i,j,k-ione) = cw(i,j,k-ione) - cw_t(i,j,k)*tmp2
            cw(i,j,k) = cw(i,j,k) + cw_t(i,j,k)*tmp2
            t(i,j,k-ione) = t(i,j,k-ione) - t_t(i,j,k)*tmp2
            t(i,j,k) = t(i,j,k) + t_t(i,j,k)*tmp2
            u(i,j,k-ione) =u(i,j,k-ione) - u_t(i,j,k)*tmp2
            u(i,j,k) = u(i,j,k) + u_t(i,j,k)*tmp2
            v(i,j,k-ione) = v(i,j,k-ione) - v_t(i,j,k)*tmp2
            v(i,j,k) = v(i,j,k) + v_t(i,j,k)*tmp2
            prdif(i,j,k) = prdif(i,j,k)+pdif*r_prdif_bck(i,j,k) 
            what(i,j,k) = what(i,j,k)+half*tmp*r_prdif_bck(i,j,k)
          end if
        end do
      end do
    end do


    do k=nsig,1,-ione
      do j=jjstart,jjstop
        do i=1,lat2
! adjoint of "tracer" horizontal advective terms
          u(i,j,k) = u(i,j,k) - q_t(i,j,k)*bck_qlon(i,j,k) -  &
             oz_t(i,j,k)*bck_ozlon(i,j,k)-cw_t(i,j,k)*bck_cwlon(i,j,k)
          v(i,j,k) = v(i,j,k) - q_t(i,j,k)*bck_qlat(i,j,k) -  &
             oz_t(i,j,k)*bck_ozlat(i,j,k)-cw_t(i,j,k)*bck_cwlat(i,j,k)
          q_x(i,j,k) = q_x(i,j,k) - q_t(i,j,k)*bck_u(i,j,k)
          q_y(i,j,k) = q_y(i,j,k) - q_t(i,j,k)*bck_v(i,j,k)
          oz_x(i,j,k) = oz_x(i,j,k) - oz_t(i,j,k)*bck_u(i,j,k)
          oz_y(i,j,k) = oz_y(i,j,k) - oz_t(i,j,k)*bck_v(i,j,k)
          cw_x(i,j,k) = cw_x(i,j,k) - cw_t(i,j,k)*bck_u(i,j,k)
          cw_y(i,j,k) = cw_y(i,j,k) - cw_t(i,j,k)*bck_v(i,j,k)

        end do
      end do
    end do


    do k=nsig,1,-ione
      do j=jjstart,jjstop
        do i=1,lat2

! adjoint of horizontal momentum equation
        u(i,j,k)=u(i,j,k)  &
            -v_t(i,j,k)*coriolis(i,j)   & 
            +u_t(i,j,k)*    curvfct(i,j)*bck_v(i,j,k)  &
            -v_t(i,j,k)*two*curvfct(i,j)*bck_u(i,j,k)  &
            -u_t(i,j,k)*bck_u_lon(i,j,k)  &
            -v_t(i,j,k)*bck_v_lon(i,j,k)  
        v(i,j,k)=v(i,j,k)  &
            +u_t(i,j,k)*coriolis(i,j)  &
            +u_t(i,j,k)*curvfct(i,j)*bck_u(i,j,k)  & 
            -u_t(i,j,k)*bck_u_lat(i,j,k)  &
            -v_t(i,j,k)*bck_v_lat(i,j,k) 
        u_x(i,j,k)=u_x(i,j,k)-u_t(i,j,k)*bck_u(i,j,k)
        v_x(i,j,k)=v_x(i,j,k)-v_t(i,j,k)*bck_u(i,j,k)
        u_y(i,j,k)=u_y(i,j,k)-u_t(i,j,k)*bck_v(i,j,k)
        v_y(i,j,k)=v_y(i,j,k)-v_t(i,j,k)*bck_v(i,j,k)
        pgf_x(i,j,k)=pgf_x(i,j,k)-u_t(i,j,k)
        pgf_y(i,j,k)=pgf_y(i,j,k)-v_t(i,j,k)

        end do
      end do
    end do


! 3) adjoint of calculating full three-dimensional dp/dt
!!  do k=1,nsig+ione
    do j=jjstart,jjstop
      do i=1,lat2
        prsth(i,j,1)=prsth(i,j,1) + ps_t(i,j)
        what(i,j,1)=what(i,j,1) - ps_t(i,j)
      end do
    end do
!!  end do


! 2) adjoint of calculation of vertical velocity
  if ( (.not.regional) .AND. (idvc5.eq.3)) then
!   Basic state horizontal temperature tendency
!   1.1) Get horizontal part of temperature tendency for vertical velocity term
    do k=1,nsig
      do j=jjstart,jjstop
        do i=1,lat2
          tmp=-rdtop_bck(i,j,k)
          t_thor9(i,j,k)=-bck_u(i,j,k)*bck_tvlon(i,j,k) - &
             bck_v(i,j,k)*bck_tvlat(i,j,k)
          t_thor9(i,j,k)=t_thor9(i,j,k) - &
             tmp*rcp * ( bck_u(i,j,k)*pr_xsum_bck(i,j,k) + &
             bck_v(i,j,k)*pr_ysum_bck(i,j,k) + &
             prsth_bck(i,j,k) + prsth_bck(i,j,k+ione) )
        end do
      end do
    end do
    call getvvel_ad(t,t_t,t_thor9,prsth,prdif,what,jjstart,jjstop)
  else
    do k=2,nsig
      do j=jjstart,jjstop
        do i=1,lat2
          if (wrf_nmm_regional) then
            prsth(i,j,1) = prsth(i,j,1)-eta2_ll(k)*what(i,j,k)
            prsth(i,j,k) = prsth(i,j,k) + what(i,j,k)
          else
            prsth(i,j,1) = prsth(i,j,1)-bk5(k)*what(i,j,k)
            prsth(i,j,k) = prsth(i,j,k) + what(i,j,k)
          end if
        end do
      end do
    end do 
  end if


! 1.1) Horizontal Part of temperature tendency, now that adjoint of
!      vertical velocity is done
  do k=1,nsig
    do j=jjstart,jjstop
      do i=1,lat2
          tmp9=-rdtop_bck(i,j,k)*rcp
          tmp=t_t(i,j,k)
        u(i,j,k)=u(i,j,k)-(bck_tvlon(i,j,k)-tmp9*pr_xsum_bck(i,j,k))*tmp
        v(i,j,k)=v(i,j,k)-(bck_tvlat(i,j,k)-tmp9*pr_ysum_bck(i,j,k))*tmp
        t_x(i,j,k)=t_x(i,j,k)-bck_u(i,j,k)*tmp
        t_y(i,j,k)=t_y(i,j,k)-bck_v(i,j,k)*tmp
        pr_xsum(i,j,k)=pr_xsum(i,j,k)+tmp9*bck_u(i,j,k)*tmp
        pr_ysum(i,j,k)=pr_ysum(i,j,k)+tmp9*bck_v(i,j,k)*tmp
        prsth(i,j,k)=prsth(i,j,k)+tmp9*tmp
        prsth(i,j,k+ione)=prsth(i,j,k+ione)+tmp9*tmp
        rdtop(i,j,k)=rdtop(i,j,k)-rcp*(prsth_bck(i,j,k)+prsth_bck(i,j,k+ione)+&
          bck_u(i,j,k)*pr_xsum_bck(i,j,k)+bck_v(i,j,k)*pr_ysum_bck(i,j,k))*tmp
      end do
    end do
  end do



! 1) adjoint of horizontal portion of pressure tendency
  do k=1,nsig
    do j=jjstart,jjstop
      do i=1,lat2
        prsth(i,j,k+ione) = prsth(i,j,k+ione) + prsth(i,j,k)
        div(i,j,k)=div(i,j,k)-prsth(i,j,k)
      end do
    end do
  end do

! 0) Adjoint of A-grid modification for divergence 

  do k=1,nsig
    do j=1,lon2
      do i=1,lat2
        prdifu_x(i,j,k)=prdifu_x(i,j,k)+div(i,j,k)
        prdifv_y2(i,j,k)=prdifv_y2(i,j,k)+div(i,j,k)
      end do
    end do
  end do


  call mp_compact_dlon2_ad(prdifu,prdifu_x,.false.,nsig,mype)
  call mp_compact_dlat2_ad(prdifv,prdifv_y2,.true.,nsig,mype)

  do k=1,nsig
    do j=1,lon2
      do i=1,lat2
        u(i,j,k)=u(i,j,k)+prdif_bck(i,j,k)*prdifu(i,j,k)
        v(i,j,k)=v(i,j,k)+prdif_bck(i,j,k)*prdifv(i,j,k)
        prdif(i,j,k)=prdif(i,j,k)+bck_u(i,j,k)*prdifu(i,j,k) &
                                 +bck_v(i,j,k)*prdifv(i,j,k)
      end do
    end do
  end do


  do k=nsig,1,-ione
    do j=1,lon2
      do i=1,lat2
        pr_xsum(i,j,k)=pr_xsum(i,j,k)-rdtop_bck(i,j,k)*pgf_x(i,j,k)
        pr_ysum(i,j,k)=pr_ysum(i,j,k)-rdtop_bck(i,j,k)*pgf_y(i,j,k)
        rdtop(i,j,k)=rdtop(i,j,k)-pr_xsum_bck(i,j,k)*pgf_x(i,j,k) &
                                 -pr_ysum_bck(i,j,k)*pgf_y(i,j,k)
        phi_x(i,j,k)=phi_x(i,j,k)+pgf_x(i,j,k)
        phi_y(i,j,k)=phi_y(i,j,k)+pgf_y(i,j,k)
        t(i,j,k)=t(i,j,k)-rd*r_prsum_bck(i,j,k)*rdtop(i,j,k)
        prsum(i,j,k)=prsum(i,j,k)-rdtop_bck(i,j,k)*r_prsum_bck(i,j,k)*rdtop(i,j,k)
      end do
    end do
  end do


jjstart=1
jjstop=lon2

! adjoint of pressure preliminaries
  do k=1,nsig+ione
    do j=jjstart,jjstop
      do i=1,lat2
       pri_x(i,j,k)=zero
       pri_y(i,j,k)=zero
      end do
    end do
  end do
  do k=nsig,1,-ione
    do j=jjstart,jjstop
      do i=1,lat2
        pri(i,j,k+ione)=pri(i,j,k+ione) + prsum(i,j,k)-prdif(i,j,k)
        pri(i,j,k)=                 prsum(i,j,k)+prdif(i,j,k)
        pri_x(i,j,k+ione)=pri_x(i,j,k+ione) + pr_xsum(i,j,k)
        pri_x(i,j,k)=                   pr_xsum(i,j,k)
        pri_y(i,j,k+ione)=pri_y(i,j,k+ione) + pr_ysum(i,j,k)
        pri_y(i,j,k)=                   pr_ysum(i,j,k)
      end do
    end do
  end do


!T if Global only 

!!  call mp_compact_dlon1_ad(pri,pri_x,.false.,nsig+ione,mype)
!!  call mp_compact_dlat1_ad(pri,pri_y,.false.,nsig+ione,mype)

  return
end subroutine calctends_model_ad
