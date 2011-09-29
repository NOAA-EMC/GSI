subroutine calctends_model_tl(u,v,t,q,oz,cw,pri, &
   phi_x,phi_y,u_x,u_y,v_x,v_y,t_x,t_y,pri_x,pri_y,q_x,q_y,oz_x,oz_y,cw_x,cw_y, &
   mype,u_t,v_t,t_t,q_t,oz_t,cw_t,ps_t)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    calctends_model_tl       tlm of calctends_model
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
!   2007-04-16  kleist - move constraint specific items elsewhere
!   2007-05-08  kleist - add bits for fully generalized vertical coordinate
!   2007-06-21  rancic - add pbl 
!   2007-07-26  cucurull - add 3d pressure pri in argument list ;
!                          move getprs_tl outside calctends_tl;
!                          call getprs_horiz_tl;
!                          remove ps from argument list
!   2007-08-08  derber - optimize
!   2008-06-05  safford - rm unused uses
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
!     pri      - 3d pressure
!     u_x      - zonal derivative of u
!     u_y      - meridional derivative of u
!     v_x      - zonal derivative of v
!     v_y      - meridional derivative of v
!     t_x      - zonal derivative of t
!     t_y      - meridional derivative of t
!!!     ps_x     - zonal derivative of ps
!!!     ps_y     - meridional derivative of ps
!     pri_x    - zonal gradient of pri
!     pri_y    - meridional gradient of por
!     q_x      - zonal derivative of q
!     q_y      - meridional derivative of q
!     oz_x     - zonal derivative of ozone
!     oz_y     - meridional derivative of ozone
!     cw_x     - zonal derivative of cloud water
!     cw_y     - meridional derivative of cloud water
!     mype     - task id
!     tracer   - logical flag if true tracer time derivatives calculated
!
!   output argument list:
!     u_t      - time tendency of u
!     v_t      - time tendency of v
!     t_t      - time tendency of t
!!!     p_t      - time tendency of 3d pressure
!     p_t      - time tendency of surface pressure
!     q_t      - time tendency of q
!     oz_t     - time tendency of ozone
!     cw_t     - time tendency of cloud water
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
  use gridmod, only: lat2,lon2,nsig,istart,nlon,nlat,rlats,idvc5,bk5,&
     wrf_nmm_regional,eta2_ll,regional,region_lat,region_lon,jstart,corlats
  use constants, only: ione,zero,half,one,two,rd,rcp,grav,rearth,pi,omega
  use tendsmod, only: coriolis,ctph0,stph0,tlm0
  use tends4pertmod, only: curvfct,time_step
  use nonlinmod, only: bck_u,bck_v,bck_tv,bck_q,bck_oz,bck_cw, &
     bck_u_lon,bck_u_lat,bck_v_lon,bck_v_lat,bck_tvlat,bck_tvlon, &
     bck_qlon,bck_qlat,bck_ozlon,bck_ozlat,bck_cwlon,bck_cwlat, &
     what_bck,prsth_bck,prsum_bck,r_prsum_bck,prdif_bck,r_prdif_bck,pr_xsum_bck,&
     pr_ysum_bck,rdtop_bck
  implicit none

! Declare passed variables
  real(r_kind),dimension(lat2,lon2,nsig)     ,intent(in   ) :: u,v,t,u_x,u_y,v_x,v_y,&
     t_x,t_y,q,oz,cw,q_x,q_y,oz_x,oz_y,cw_x,cw_y,phi_x,phi_y
  real(r_kind),dimension(lat2,lon2,nsig+ione),intent(in   ) :: pri,pri_x,pri_y
  integer(i_kind)                            ,intent(in   ) :: mype
  real(r_kind),dimension(lat2,lon2,nsig)     ,intent(  out) :: u_t,v_t,t_t,q_t,oz_t,cw_t
  real(r_kind),dimension(lat2,lon2)          ,intent(  out) :: ps_t

! Declare local variables
  real(r_kind),dimension(lat2,lon2,nsig+ione):: prsth,what
  real(r_kind),dimension(lat2,lon2,nsig):: prsum,prdif,pr_xsum,pr_ysum
  real(r_kind),dimension(lat2,lon2,nsig):: t_thor9
!m-----------------------------------------------------------------------------B
  real(r_kind),dimension(lat2,lon2,nsig):: prdifu,prdifv
  real(r_kind),dimension(lat2,lon2,nsig):: pgf_x,pgf_y,pgf_xx,pgf_yy
  real(r_kind),dimension(lat2,lon2,nsig):: pgf1_x,pgf1_y,pgf1_xx,pgf1_yy
  real(r_kind),dimension(lat2,lon2,nsig):: rdtop,div,prdifu_x,prdifv_y2
  real(r_kind),dimension(lat2,lon2):: rdrdx2,rdrdy2
  real(r_kind) psum,psum9,psump1,psum9p1
  real(r_kind) rr,dlam,dphi,wpdar,pdif
  real(r_kind) relm,crlm,aph,sph,cph,cc,tph
!m-----------------------------------------------------------------------------E
  real(r_kind) tmp,tmp2,tmp9,tmp9u,tmp9v,tmp9t,tmp9q,tmp9oz,tmp9cw
  integer(i_kind) i,j,k,ix,jx
  integer(i_kind) :: jjstart,jjstop
  integer(i_kind) :: nth,tid,omp_get_num_threads,omp_get_thread_num

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  real(r_kind), parameter:: dts=600.,eps_damp=0.2,fac_nk_damp=2.6
  integer(i_kind) k_top,nk_damp,k_damp
  real(r_kind) dampwt,rdampwt,pihalf,arg,rnk_damp
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

! linearized about guess solution, so set it flag accordingly
  jjstart=1
  jjstop=lon2

! preliminaries:

  do k=1,nsig
    do j=jjstart,jjstop
      do i=1,lat2
        prsum(i,j,k)=pri(i,j,k)+pri(i,j,k+ione)
        prdif(i,j,k)=pri(i,j,k)-pri(i,j,k+ione)
        pr_xsum(i,j,k)=pri_x(i,j,k)+pri_x(i,j,k+ione)
        pr_ysum(i,j,k)=pri_y(i,j,k)+pri_y(i,j,k+ione)
      end do
    end do
  end do

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

! Preliminaries for 0)


  rr=rd/rearth**2
  dlam=two*pi/nlon
  dphi=pi/nlat      
!!MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
!!  wpdar=  grav*time_step  * 0.09 !m>>>> test
!!  wpdar=  grav*time_step  * 0.0 !m>>>> test
!!MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM

    do j=1,lon2
      do i=1,lat2
        ix=istart(mype+ione)+i-2_i_kind
        ix=min(max(ix,2),nlat-ione)
        rdrdx2(i,j)=rr/(cos(rlats(ix))*dlam)**2
        rdrdy2(i,j)=rr/dphi**2
      end do
    end do


! 0) TLM of A-grid modification for divergence 

  do k=1,nsig
    do j=1,lon2
      do i=1,lat2
        rdtop(i,j,k)=-rd*t(i,j,k)*r_prsum_bck(i,j,k) &
                     -rdtop_bck(i,j,k)*r_prsum_bck(i,j,k)*prsum(i,j,k)
        pgf_x(i,j,k)=-rdtop_bck(i,j,k)*pr_xsum(i,j,k) &
                     -rdtop(i,j,k)*pr_xsum_bck(i,j,k)+phi_x(i,j,k)
        pgf_y(i,j,k)=-rdtop_bck(i,j,k)*pr_ysum(i,j,k) &
                     -rdtop(i,j,k)*pr_ysum_bck(i,j,k)+phi_y(i,j,k)
      end do
    end do
  end do


!!MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
!!    pgf1_x(:,:,:)=zero
!!    pgf1_y(:,:,:)=zero

!!  call mp_compact_dlon2(pgf_x,pgf_xx,.false.,nsig,mype)
!!  call mp_compact_dlat2(pgf_y,pgf_yy,.false.,nsig,mype)


!!  do k=1,nsig
!!    do j=1,lon2-ione
!!      do i=1,lat2
!!          psum9=prsum_bck(i,j,k)
!!          psum9p1=prsum_bck(i,j+ione,k)
!!          psum=prsum(i,j,k)
!!          psump1=prsum(i,j+ione,k)
!!          tmp9=one/(psum9+psum9p1)
!!        pgf1_x(i,j,k)=rdrdx2(i,j)*tmp9*( &
!!          (psum9p1-psum9)*(t(i,j+ione,k)+t(i,j,k))+&
!!          two*tmp9*(bck_tv(i,j+ione,k)+bck_tv(i,j,k))* &
!!                    (psum9*psump1-psum9p1*psum) )
!!      end do
!!    end do
!!  end do


!!  do k=1,nsig
!!    do j=1,lon2
!!      do i=1,lat2-ione
!!          psum9=prsum_bck(i,j,k)
!!          psum9p1=prsum_bck(i+ione,j,k)
!!          psum=prsum(i,j,k)
!!          psump1=prsum(i+ione,j,k)
!!          tmp9=one/(psum9+psum9p1)
!!        pgf1_y(i,j,k)=rdrdy2(i,j)*tmp9*( &
!!          (psum9p1-psum9)*(t(i+ione,j,k)+t(i,j,k))+&
!!          two*tmp9*(bck_tv(i+ione,j,k)+bck_tv(i,j,k))* &
!!                    (psum9*psump1-psum9p1*psum) )
!!      end do
!!    end do
!!  end do


!!  do k=1,nsig
!!    do j=2,lon2-ione    
!!      do i=1,lat2
!!        pgf1_xx(i,j,k)=pgf1_x(i,j,k)-pgf1_x(i,j-ione,k)
!!      end do
!!    end do
!!  end do
!!  do k=1,nsig
!!    do j=1,lon2
!!      do i=2,lat2-ione   
!!        pgf1_yy(i,j,k)=pgf1_y(i,j,k)-pgf1_y(i-ione,j,k)
!!      end do
!!    end do
!!  end do
!!MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM


  do k=1,nsig
    do j=1,lon2
      do i=1,lat2
        prdifu(i,j,k)=prdif_bck(i,j,k)*u(i,j,k)+prdif(i,j,k)*bck_u(i,j,k)
        prdifv(i,j,k)=prdif_bck(i,j,k)*v(i,j,k)+prdif(i,j,k)*bck_v(i,j,k)
      end do
    end do
  end do


  call mp_compact_dlon2(prdifu,prdifu_x,.false.,nsig,mype)
  call mp_compact_dlat2(prdifv,prdifv_y2,.true.,nsig,mype)


  div(:,:,:)=zero 
  do k=1,nsig
    do j=1,lon2
      do i=1,lat2
!!    do j=2,lon2-ione
!!      do i=2,lat2-ione
!!MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
!!        div(i,j,k)=wpdar*( pgf1_xx(i,j,k) + pgf1_yy(i,j,k)- &
!!                           pgf_xx(i,j,k) -  pgf_yy(i,j,k) ) 
!!MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
        div(i,j,k)=div(i,j,k)+prdifu_x(i,j,k)+prdifv_y2(i,j,k)
      end do
    end do
  end do


! 1) Compute horizontal part of tendency for 3d pressure
   do j=jjstart,jjstop
     do i=1,lat2
       prsth(i,j,nsig+ione)=zero
     end do
   end do
   do k=nsig,1,-ione
     do j=jjstart,jjstop
       do i=1,lat2
         prsth(i,j,k)=prsth(i,j,k+ione) - div(i,j,k)
       end do
     end do
   end do


! 1.1) Get horizontal part of temperature tendency for vertical velocity term
  do k=1,nsig
    do j=jjstart,jjstop
      do i=1,lat2
        tmp9=-rdtop_bck(i,j,k)
        tmp =-rdtop(i,j,k)
        t_t(i,j,k)=-u(i,j,k)*bck_tvlon(i,j,k)-bck_u(i,j,k)*t_x(i,j,k) - &
                    v(i,j,k)*bck_tvlat(i,j,k)-bck_v(i,j,k)*t_y(i,j,k) + &
          tmp9*rcp*(bck_u(i,j,k)*pr_xsum(i,j,k)+u(i,j,k)*pr_xsum_bck(i,j,k) + &
                    bck_v(i,j,k)*pr_ysum(i,j,k)+v(i,j,k)*pr_ysum_bck(i,j,k) + &
                    prsth(i,j,k) + prsth(i,j,k+ione) ) + &
          tmp *rcp*(bck_u(i,j,k)*pr_xsum_bck(i,j,k)+bck_v(i,j,k)*pr_ysum_bck(i,j,k)+&
                    prsth_bck(i,j,k)+prsth_bck(i,j,k+ione) ) 
      end do
    end do
  end do


! 2) calculate vertical velocity term:  z(dp/dz) (zero at top/bottom interfaces)
! if running global, and there is a c(k) coefficient, we call the 'getvvel'
! subroutine

  if ( (.not.regional) .AND. (idvc5.eq.3)) then
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
    call getvvel_tl(t,t_t,t_thor9,prsth,prdif,what,jjstart,jjstop)
  else
    do k=2,nsig
      do j=jjstart,jjstop
        do i=1,lat2
          if(wrf_nmm_regional) then
            what(i,j,k)=prsth(i,j,k)-eta2_ll(k)*prsth(i,j,1)
          else
            what(i,j,k)=prsth(i,j,k)-bk5(k)*prsth(i,j,1)
          end if
        end do
      end do
    end do
  end if

! top/bottom boundary condition:
    do j=jjstart,jjstop
      do i=1,lat2
        what(i,j,1)=zero
        what(i,j,nsig+ione)=zero
      enddo
    enddo

   
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! Shiqiu Peng added implicit Rayleigh damping of veritical velocity

!  pihalf=pi*half

!   k_top=nsig
!   nk_damp=k_top/fac_nk_damp+ione
!   k_damp=k_top-nk_damp
!   rnk_damp=one/nk_damp

!    do k=2,nsig
!      do j=1,lon2
!        do i=1,lat2
!            if(k.ge.k_damp) then
!              arg=pihalf*(k-k_damp)*rnk_damp
!              dampwt=eps_damp*dts**sin(arg)**2
!              rdampwt=one/(one+dampwt)
!            else 
!              rdampwt=one
!            end if
!          what(i,j,k)=what(i,j,k)*rdampwt
!        end do
!      end do
!    end do

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

! 3) load actual dp/dt
!!  do k=1,nsig+ione
    do j=jjstart,jjstop
      do i=1,lat2
!!        p_t(i,j,k)=prsth(i,j,k)-what(i,j,k)
        ps_t(i,j)=prsth(i,j,1)-what(i,j,1)
      end do
    end do
!!  end do


! 4) Compute terms for tendencies of wind components & Temperature
  do k=1,nsig
    do j=jjstart,jjstop
      do i=1,lat2
! horizontal part of momentum equations
        u_t(i,j,k)= &
           +coriolis(i,j)*v(i,j,k)  &
           -pgf_x(i,j,k) &
           +curvfct(i,j)*(bck_u(i,j,k)*v(i,j,k) + u(i,j,k)*bck_v(i,j,k)) &
           -u(i,j,k)*bck_u_lon(i,j,k)-bck_u(i,j,k)*u_x(i,j,k)  &
           -v(i,j,k)*bck_u_lat(i,j,k)-bck_v(i,j,k)*u_y(i,j,k) 
        v_t(i,j,k)= &
           -coriolis(i,j)*u(i,j,k)  &
           -pgf_y(i,j,k) &
           -two*curvfct(i,j)*bck_u(i,j,k)*u(i,j,k) &
           -u(i,j,k)*bck_v_lon(i,j,k)-bck_u(i,j,k)*v_x(i,j,k) &
           -v(i,j,k)*bck_v_lat(i,j,k)-bck_v(i,j,k)*v_y(i,j,k) 
      end do   !end do i
    end do     !end do j
  end do       !end do k


  do k=1,nsig
    do j=jjstart,jjstop
      do i=1,lat2
! horizontal advection of "tracer" quantities
        q_t(i,j,k)=-u(i,j,k)*bck_qlon(i,j,k)-bck_u(i,j,k)*q_x(i,j,k) &
                   -v(i,j,k)*bck_qlat(i,j,k)-bck_v(i,j,k)*q_y(i,j,k)
        oz_t(i,j,k)=-u(i,j,k)*bck_ozlon(i,j,k)-bck_u(i,j,k)*oz_x(i,j,k) &
                    -v(i,j,k)*bck_ozlat(i,j,k)-bck_v(i,j,k)*oz_y(i,j,k)
        cw_t(i,j,k)=-u(i,j,k)*bck_cwlon(i,j,k)-bck_u(i,j,k)*cw_x(i,j,k) &
                    -v(i,j,k)*bck_cwlat(i,j,k)-bck_v(i,j,k)*cw_y(i,j,k)
      end do   !end do i
    end do     !end do j
  end do       !end do k

! vertical flux terms
  do k=1,nsig
    do j=jjstart,jjstop
      do i=1,lat2
          pdif=prdif(i,j,k)*r_prdif_bck(i,j,k)
        if (k > 1) then
          tmp=half*what(i,j,k)*r_prdif_bck(i,j,k)
          tmp2=half*what_bck(i,j,k)*r_prdif_bck(i,j,k)
          tmp9u=bck_u(i,j,k-ione)-bck_u(i,j,k)
          tmp9v=bck_v(i,j,k-ione)-bck_v(i,j,k)
          tmp9t=bck_tv(i,j,k-ione)-bck_tv(i,j,k)
          tmp9q=bck_q(i,j,k-ione)-bck_q(i,j,k) 
          tmp9oz=bck_oz(i,j,k-ione)-bck_oz(i,j,k) 
          tmp9cw=bck_cw(i,j,k-ione)-bck_cw(i,j,k) 
        u_t(i,j,k) = u_t(i,j,k) - tmp*tmp9u - &
                     tmp2*( (u(i,j,k-ione)-u(i,j,k))-tmp9u*pdif )   
        v_t(i,j,k) = v_t(i,j,k) - tmp*tmp9v - &
                     tmp2*( (v(i,j,k-ione)-v(i,j,k))-tmp9v*pdif )
        t_t(i,j,k) = t_t(i,j,k)-tmp*tmp9t - &
                     tmp2*( (t(i,j,k-ione)-t(i,j,k))-tmp9t*pdif )
        q_t(i,j,k)= q_t(i,j,k) - tmp*tmp9q - &
                     tmp2*( (q(i,j,k-ione)-q(i,j,k))-tmp9q*pdif )
        oz_t(i,j,k)= oz_t(i,j,k)-tmp*tmp9oz - &
                     tmp2*( (oz(i,j,k-ione)-oz(i,j,k))-tmp9oz*pdif )
        cw_t(i,j,k)= cw_t(i,j,k)-tmp*tmp9cw - &
                     tmp2*( (cw(i,j,k-ione)-cw(i,j,k))-tmp9cw*pdif )
        end if
        if (k < nsig) then
          tmp=half*what(i,j,k+ione)*r_prdif_bck(i,j,k)
          tmp2=half*what_bck(i,j,k+ione)*r_prdif_bck(i,j,k)
          tmp9u=bck_u(i,j,k)-bck_u(i,j,k+ione)
          tmp9v=bck_v(i,j,k)-bck_v(i,j,k+ione)
          tmp9t=bck_tv(i,j,k)-bck_tv(i,j,k+ione)
          tmp9q=bck_q(i,j,k)-bck_q(i,j,k+ione) 
          tmp9oz=bck_oz(i,j,k)-bck_oz(i,j,k+ione) 
          tmp9cw=bck_cw(i,j,k)-bck_cw(i,j,k+ione) 
        u_t(i,j,k)= u_t(i,j,k) - tmp*tmp9u - &
                     tmp2*( (u(i,j,k)-u(i,j,k+ione))-tmp9u*pdif )
        v_t(i,j,k)= v_t(i,j,k) - tmp*tmp9v - &
                     tmp2*( (v(i,j,k)-v(i,j,k+ione))-tmp9v*pdif )
        t_t(i,j,k)= t_t(i,j,k)- tmp*tmp9t - &
                     tmp2*( (t(i,j,k)-t(i,j,k+ione))-tmp9t*pdif )
        q_t(i,j,k)= q_t(i,j,k) - tmp*tmp9q - &
                     tmp2*( (q(i,j,k)-q(i,j,k+ione)) - tmp9q*pdif )
        oz_t(i,j,k)= oz_t(i,j,k) - tmp*tmp9oz - &
                     tmp2*( (oz(i,j,k)-oz(i,j,k+ione))-tmp9oz*pdif )
        cw_t(i,j,k)= cw_t(i,j,k)-tmp*tmp9cw - &
                     tmp2*( (cw(i,j,k)-cw(i,j,k+ione))-tmp9cw*pdif )
        end if
      end do   !end do i
    end do     !end do j
  end do       !end do k

  call hdiff(u_x,u_y,v_x,v_y,t_x,t_y,u_t,v_t,t_t,mype)
  call sfcdrag_tl(u,v,t,pri,u_t,v_t)
 
  if(.not.wrf_nmm_regional)then
    do k=1,nsig
      do j=1,lon2
        do i=1,lat2
          ix=istart(mype+ione)+i-2_i_kind
          if (ix == 1 .OR. ix == nlat) then
            u_t(i,j,k)=zero
            v_t(i,j,k)=zero
          end if
        end do 
      end do
    end do  !end do k
  end if

  return
end subroutine calctends_model_tl
