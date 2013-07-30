subroutine pbl_ad(u,v,t,ps,jstart,jstop) 
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    pbl_ad
!
!  prgrmmr:      m. rancic
!
! abstract:      adjoint of pbl_tl
!
! program history log:
!   2008-04-02  safford -- add subprogram doc block, rm unused uses
!
!   output argument list:
!     ps    -
!     u     - 
!     v     - 
!     t     - 
!
!   input argument list:
!     u     - 
!     v     - 
!     t     - 
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$

  use kinds,only: r_kind,i_kind
  use constants,only: one,zero,two,four,five,half,rd_over_g,rd_over_cp,grav
  use gridmod,only: lat2,lon2,nsig
  use pblmod, only: dudz,dvdz,dodz,zi,rdzi,rdzl,eps_m
  use pblmod, only: lmbd,karm,karm0,alph,beta,epxilon,nsig_pbl
  use pblmod, only: uges0,vges0,oges0,pges0,tges0,uges1,vges1,oges1  
  use tends4pertmod, only: time_step_half
  implicit none

! Declare passed variables
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout):: u,v,t
  real(r_kind),dimension(lat2,lon2)     ,intent(  out):: ps
  integer(i_kind)                       ,intent(in   ):: jstart,jstop

! Declare local parameters
  real(r_kind),parameter:: r10 = 10.0_r_kind
  real(r_kind),parameter:: r20 = 20.0_r_kind

! Declare local variables
  real(r_kind),dimension(lat2,lon2,nsig+1):: prs
  real(r_kind),dimension(nsig_pbl):: u1_bg,v1_bg,o1_bg
  real(r_kind),dimension(nsig_pbl):: u_bg,v_bg,t_bg,o_bg,rdzi_bg,rdzi_ad
  real(r_kind),dimension(nsig_pbl):: rdzl_ad,dudz_ad,dvdz_ad,dodz_ad,km_ad
  real(r_kind),dimension(nsig_pbl+1):: p_bg,zi_bg
  real(r_kind),dimension(2:nsig_pbl):: rdzl_bg
  real(r_kind),dimension(2:nsig_pbl):: lmix_bg,ri_bg,km_bg,zmix_bg,ssq_bg
  real(r_kind),dimension(2:nsig_pbl):: dudz_bg,dvdz_bg,dodz_bg
  real(r_kind),dimension(2:nsig_pbl):: lmix_ad,ri_ad,rho_bg
  real(r_kind),dimension(nsig_pbl):: u_ad,v_ad,t_ad,zl_ad,o_ad
  real(r_kind),dimension(nsig_pbl+1):: p_ad,zi_ad,ck_ad,ck_bg
  real(r_kind),dimension(nsig_pbl+1):: dzi_ad
  real(r_kind),dimension(nsig_pbl):: aux_bg,ckdif_bg,cksum_bg

  real(r_kind)  ckplus_bg,ckmnus_bg,ckplus_ad,ckmnus_ad
  real(r_kind) rssq_bg,ssq_ad
  real(r_kind) zmix_ad
  real(r_kind):: acoef,bcoef,ccoef
  real(r_kind):: ax1,ax2
  real(r_kind):: aux_ad
  real(r_kind) rho_ad

  real(r_kind),dimension(nsig_pbl):: acoef0,bcoef0,ccoef0
  real(r_kind),dimension(nsig_pbl):: acoef1,bcoef1,ccoef1
  real(r_kind),dimension(nsig_pbl):: a_ad,b_ad,c_ad
  real(r_kind),dimension(nsig_pbl):: wu,ua,ub,uc,qu,pu
  real(r_kind),dimension(nsig_pbl):: wv,va,vb,vc,qv,pv
  real(r_kind),dimension(nsig_pbl):: wo,oa,ob,oc,qo,po
  real(r_kind),dimension(nsig_pbl):: fu,fv,fo

  integer(i_kind) i,j,k

  
! Preliminaries

  ua(1)=zero; va(1)=zero;  oa(1)=zero
  uc(nsig_pbl)=zero; vc(nsig_pbl)=zero; oc(nsig_pbl)=zero 

  do j=jstart,jstop
    do i=1,lat2

! Background fields saved from the nonlinear model

      do k=1,nsig_pbl
        t_bg(k)=tges0(i,j,k)
        u_bg(k)=uges0(i,j,k)
        v_bg(k)=vges0(i,j,k)
        o1_bg(k)=oges1(i,j,k)
        u1_bg(k)=uges1(i,j,k)
        v1_bg(k)=vges1(i,j,k)
        p_bg(k)=pges0(i,j,k)
        o_bg(k)=oges0(i,j,k)
        zi_bg(k)=zi(i,j,k)
        rdzi_bg(k)=rdzi(i,j,k)
      end do
        p_bg(nsig_pbl+1)=pges0(i,j,nsig_pbl+1)
        zi_bg(nsig_pbl+1)=zi(i,j,nsig_pbl+1)

      do k=2,nsig_pbl
        rdzl_bg(k)=rdzl(i,j,k)
        dodz_bg(k)=dodz(i,j,k)
        dudz_bg(k)=dudz(i,j,k)
        dvdz_bg(k)=dvdz(i,j,k)
      end do

      do k=2,nsig_pbl
        zmix_bg(k)=zi(i,j,k)-zi(i,j,1)
        lmix_bg(k)=karm*zmix_bg(k)/(one+karm0*zmix_bg(k))
        ssq_bg(k)=dudz_bg(k)**2+dvdz_bg(k)**2
          if(ssq_bg(k) < eps_m) then 
            ri_bg(k)=two*grav*dodz(i,j,k)/((o_bg(k)+o_bg(k-1))*eps_m)
          else
            ri_bg(k)=two*grav*dodz(i,j,k)/((o_bg(k)+o_bg(k-1))*ssq_bg(k))
          end if
            if( ri_bg(k) < zero) then
              rho_bg(k)=sqrt(one-r20*ri_bg(k))
            else
              rho_bg(k)=one/(one+five*ri_bg(k))**2
            end if
          if(ssq_bg(k) < eps_m) then 
            km_bg(k)=lmix_bg(k)**2 *sqrt(eps_m    )*rho_bg(k)
          else
            km_bg(k)=lmix_bg(k)**2 *sqrt(ssq_bg(k))*rho_bg(k)
          end if
      end do

        ck_bg(1)=zero  
      do k=2,nsig_pbl
        ck_bg(k)=km_bg(k)*rdzl_bg(k)            ! << NL model repeat >>  
      end do
        ck_bg(nsig_pbl+1)=zero   

! Preliminaries 

      do k=nsig_pbl,1,-1                           ! << NL model repeat >>  
        aux_bg(k)=time_step_half*rdzi_bg(k)
        ckplus_bg=ck_bg(k+1)+ck_bg(k)
        ckmnus_bg=(ck_bg(k+1)-ck_bg(k))*epxilon
        ckdif_bg(k)=ckplus_bg-ckmnus_bg
        cksum_bg(k)=ckplus_bg+ckmnus_bg
          acoef=aux_bg(k)*ckdif_bg(k)
          ccoef=aux_bg(k)*cksum_bg(k)
          bcoef=-acoef-ccoef

          acoef0(k)=-acoef*beta
          bcoef0(k)=-bcoef*beta-one
          ccoef0(k)=-ccoef*beta
          
          acoef1(k)=acoef*alph
          bcoef1(k)=bcoef*alph-one
          ccoef1(k)=ccoef*alph
      end do

      wu(1:nsig_pbl)=alph*u1_bg(1:nsig_pbl)+beta*u_bg(1:nsig_pbl)
      wv(1:nsig_pbl)=alph*v1_bg(1:nsig_pbl)+beta*v_bg(1:nsig_pbl)
      wo(1:nsig_pbl)=alph*o1_bg(1:nsig_pbl)+beta*o_bg(1:nsig_pbl)

      ua(2:nsig_pbl)=wu(1:nsig_pbl-1)
      ub(1:nsig_pbl)=wu(1:nsig_pbl)
      uc(1:nsig_pbl-1)=wu(2:nsig_pbl)

      va(2:nsig_pbl)=wv(1:nsig_pbl-1)
      vb(1:nsig_pbl)=wv(1:nsig_pbl)
      vc(1:nsig_pbl-1)=wv(2:nsig_pbl)

      oa(2:nsig_pbl)=wo(1:nsig_pbl-1)
      ob(1:nsig_pbl)=wo(1:nsig_pbl)
      oc(1:nsig_pbl-1)=wo(2:nsig_pbl)

! Perturbation fields 

      do k=1,nsig_pbl
        t_ad(k)=t(i,j,k)
        u_ad(k)=u(i,j,k)
        v_ad(k)=v(i,j,k)
      end do

!!        p_ad(nsig_pbl+1)=zero
        p_ad=zero
        rdzl_ad(1)=zero  
        zi_ad(1)=zero


!(5) Adjoint of trapezoidal scheme

!5.4) Adjoint of perturbation of temperature

      do k=nsig_pbl,1,-1
         ax1=t_bg(k)/o_bg(k)
         ax2=t_bg(k)/(p_bg(k)+p_bg(k+1))*rd_over_cp
            o_ad(k)=ax1*t_ad(k)
            p_ad(k)=ax2*t_ad(k)
            p_ad(k+1)=ax2*t_ad(k)+p_ad(k+1)
      end do


!5.3)  Adjoint of tridiagonal system

      fo(1:nsig_pbl)=o_ad(1:nsig_pbl)
      fu(1:nsig_pbl)=u_ad(1:nsig_pbl)
      fv(1:nsig_pbl)=v_ad(1:nsig_pbl)

      call tridiag_ad(acoef1,bcoef1,ccoef1,fo,nsig_pbl)
      call tridiag_ad(acoef1,bcoef1,ccoef1,fu,nsig_pbl)
      call tridiag_ad(acoef1,bcoef1,ccoef1,fv,nsig_pbl)

!5.2)  Adjoint of u,v,o

      call multi_tridiag_ad(acoef0,bcoef0,ccoef0,fo,o_ad,nsig_pbl)
      call multi_tridiag_ad(acoef0,bcoef0,ccoef0,fu,u_ad,nsig_pbl)
      call multi_tridiag_ad(acoef0,bcoef0,ccoef0,fv,v_ad,nsig_pbl)


!5.1)  Adjoint of a_ad,b_ad,c_ad

      do k=nsig_pbl,1,-1
        a_ad(k)=-(ua(k)*fu(k)+va(k)*fv(k)+oa(k)*fo(k))
        b_ad(k)=-(ub(k)*fu(k)+vb(k)*fv(k)+ob(k)*fo(k))
        c_ad(k)=-(uc(k)*fu(k)+vc(k)*fv(k)+oc(k)*fo(k))
      end do


        ck_ad(nsig_pbl+1)=zero
      do k=nsig_pbl,1,-1
        a_ad(k)=a_ad(k)-b_ad(k)
        c_ad(k)=c_ad(k)-b_ad(k)
          ckplus_ad=aux_bg(k)*( a_ad(k)+c_ad(k) )
          ckmnus_ad=aux_bg(k)*(-a_ad(k)+c_ad(k) )
          aux_ad   =ckdif_bg(k)*a_ad(k)+cksum_bg(k)*c_ad(k)
          ck_ad(k)   = ckplus_ad-epxilon*ckmnus_ad
          ck_ad(k+1) = ckplus_ad+epxilon*ckmnus_ad + ck_ad(k+1)
          rdzi_ad(k)=aux_ad*time_step_half
     end do


     do k=nsig_pbl,2,-1
       km_ad(k)=ck_ad(k)*rdzl_bg(k)
       rdzl_ad(k)=ck_ad(k)*km_bg(k)
     end do
       km_ad(1)=zero
       rdzl_ad(1)=zero  


!(4) Adjoint of perturbation of mixing coefficients

      do k=nsig_pbl,2,-1

!Preliminaries

          ax1=one/(o_bg(k)+o_bg(k-1))
      if(ssq_bg(k)<eps_m) then
!Eq(46)
        lmix_ad(k)=two*km_bg(k)/lmix_bg(k)*km_ad(k)
        rho_ad    =    km_bg(k)/rho_bg(k) *km_ad(k)
        ssq_ad    =    zero                          !! test
      else
!Eq(44)
          rssq_bg=one/ssq_bg(k)
        lmix_ad(k)=two*km_bg(k)/lmix_bg(k)*km_ad(k)
        rho_ad    =    km_bg(k)/rho_bg(k) *km_ad(k)
        ssq_ad    =    km_bg(k)*rssq_bg   *km_ad(k)
      end if
!Eq(43)
            if(ri_bg(k)<zero)then             
              ri_ad(k)=-r10/rho_bg(k) *rho_ad
            else
              ri_ad(k)=-r10*(sqrt(rho_bg(k)))**3*rho_ad
            end if
      if(ssq_bg(k)<eps_m) then
!Eq(45)
        dodz_ad(k)=(ri_bg(k)/dodz_bg(k))*ri_ad(k)
        o_ad(k  )=-ri_bg(k)*ax1*ri_ad(k)+o_ad(k)
        o_ad(k-1)=-ri_bg(k)*ax1*ri_ad(k)+o_ad(k-1)  !<<test
      else
!Eq(42)
        dodz_ad(k)=(ri_bg(k)/dodz_bg(k))*ri_ad(k)
        o_ad(k  )=-ri_bg(k)*ax1*ri_ad(k)+o_ad(k)
        o_ad(k-1)=-ri_bg(k)*ax1*ri_ad(k)+o_ad(k-1)  !<<test
        ssq_ad   =-ri_bg(k)*rssq_bg*two*ri_ad(k)+ ssq_ad  
      end if
!Eq(41)
        zmix_ad=karm*lmix_ad(k)/(one+karm0*zmix_bg(k))**2
!Eq(40)
        dudz_ad(k)=dudz_bg(k)*ssq_ad 
        dvdz_ad(k)=dvdz_bg(k)*ssq_ad 
!Eq(39)
        zi_ad(k)=zmix_ad

      end do


!(3) Adjoint of perturbation of vertical gradiens

      do k=nsig_pbl,2,-1
        u_ad(k-1) = -dudz_ad(k)*rdzl_bg(k)+u_ad(k-1)  !!<test
        u_ad(k  ) =  dudz_ad(k)*rdzl_bg(k)+u_ad(k)
        v_ad(k-1) = -dvdz_ad(k)*rdzl_bg(k)+v_ad(k-1)  !!<test 
        v_ad(k  ) =  dvdz_ad(k)*rdzl_bg(k)+v_ad(k)
        o_ad(k-1) = -dodz_ad(k)*rdzl_bg(k)+o_ad(k-1) 
        o_ad(k  ) =  dodz_ad(k)*rdzl_bg(k)+o_ad(k)
        rdzl_ad(k)= &
                  +(u_bg(k)-u_bg(k-1))*dudz_ad(k) &
                  +(v_bg(k)-v_bg(k-1))*dvdz_ad(k) &
                  +(o_bg(k)-o_bg(k-1))*dodz_ad(k) &
                  +rdzl_ad(k) ! <<test
      end do


!(2) Adjoint of perturbation of heights

        zl_ad(nsig_pbl)=zero
     do k=nsig_pbl,2,-1
        zl_ad(k-1)= rdzl_bg(k)**2*rdzl_ad(k)
        zl_ad(k  )=-rdzl_bg(k)**2*rdzl_ad(k)+zl_ad(k)
     end do

        zi_ad(nsig_pbl+1)=zero
        zi_ad(1)=zero
     do k=nsig_pbl,1,-1
        zi_ad(k  )=half*zl_ad(k)+rdzi_bg(k)**2*rdzi_ad(k) + zi_ad(k) 
        zi_ad(k+1)=half*zl_ad(k)-rdzi_bg(k)**2*rdzi_ad(k) + zi_ad(k+1)
     end do

        dzi_ad(nsig_pbl+1)=zero
     do k=nsig_pbl,1,-1
        dzi_ad(k)=dzi_ad(k+1)+zi_ad(k+1) 
     end do

     do k=nsig_pbl,1,-1
       ax1 = four*rd_over_g*t_bg(k)/(p_bg(k)+p_bg(k+1))**2 
         t_ad(k)=(zi_bg(k+1)-zi_bg(k))/t_bg(k)*dzi_ad(k)
         p_ad(k  )= ax1 *p_bg(k+1)*dzi_ad(k)+p_ad(k)
         p_ad(k+1)=-ax1 *p_bg(k)*dzi_ad(k)+p_ad(k+1)
     end do


!(1) Adjoint of perturbation of potential temperature

      do k=nsig_pbl,1,-1
          ax1=o_bg(k)/t_bg(k)
          ax2=o_bg(k)/(p_bg(k)+p_bg(k+1))*rd_over_cp
        t_ad(k)= ax1*o_ad(k) + t_ad(k)   
        p_ad(k  )=-ax2*o_ad(k)+p_ad(k  )  
        p_ad(k+1)=-ax2*o_ad(k)+p_ad(k+1)
      end do

!(0) Copy back to 3d output arrays

     do k=nsig_pbl,1,-1
       u(i,j,k)=u_ad(k)
       v(i,j,k)=v_ad(k)
       t(i,j,k)=t_ad(k)
       prs(i,j,k)=p_ad(k)
    end do
       prs(i,j,nsig_pbl+1)=p_ad(nsig_pbl+1)

    end do
  end do

      ps=zero
        call getprs_bck_ad(ps,t,prs)

  return
end subroutine pbl_ad

 subroutine tridiag_ad(a,b,c,f,jmx)

!  Adjoint of solver for a standard tridiagonal system
!   -  'Thomas' tridiagonal algorithm  -
!   (Adapted from Durran (1999), p. 440)

!  a - sub (lower) diagonal
!  b - center diagonal
!  c - super (upper) diagonal
!  f - right  hand side and solution
!
!  ( a(1) and c(jmx) need not to be initialized )

  use kinds,only: r_kind,i_kind,r_quad
  use constants,only: one,zero
  implicit none

  integer(i_kind), intent(in):: jmx
  real(r_kind), dimension(jmx), intent(in):: a,b,c
  real(r_kind), dimension(jmx), intent(inout):: f
  real(r_kind), dimension(jmx):: q
  real(r_kind) p
  integer(i_kind) j

! Forward elimination sweep

  q(1)=-a(2)/b(1)
  f(1)= f(1)/b(1)

  do j=2,jmx
    p=one/(b(j)+c(j-1)*q(j-1))
    q(j)=-a(j+1)*p
    f(j)=( f(j)-c(j-1)*f(j-1) )*p
  end do

! Backward pass

  do j=jmx-1,1,-1
    f(j)=f(j)+q(j)*f(j+1)
  end do

end subroutine tridiag_ad

subroutine multi_tridiag_ad(a,b,c,u,f,jmx)

!  multimply adjoint of tridiagonal matrix (a,b,c) with 
!  vector u and store result in vector f

  use kinds,only: r_kind,i_kind
  implicit none

  integer(i_kind), intent(in):: jmx
  real(r_kind), dimension(jmx), intent(in):: a,b,c,u
  real(r_kind), dimension(jmx), intent(out):: f
  integer(i_kind) j

   do j=2,jmx-1
     f(j)=u(j-1)*c(j-1)+u(j)*b(j)+u(j+1)*a(j+1)
   end do
     f(1)=            u(1)*b(1)+u(1+1)*a(1+1)
     f(jmx)=u(jmx-1)*c(jmx-1)+u(jmx)*b(jmx)

end subroutine multi_tridiag_ad


