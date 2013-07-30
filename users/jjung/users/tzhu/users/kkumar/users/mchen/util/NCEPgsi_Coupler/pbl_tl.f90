subroutine pbl_tl(u,v,t,ps,jstart,jstop) 
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    pbl_tl
!
! prgrmmr: m. rancic
!
! abstract:     tangent linear of pbl
!
! program history log:
!   2008-04-02  safford -- add subprogram doc block, rm unused uses
!
!   input argument list:
!     u     -
!     v     -
!     ps    - 
!     t     - 
!
!   output argument list:
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
  use pblmod, only: uges0,vges0,oges0,pges0,tges0,uges1,vges1,oges1  
  use pblmod, only: dudz,dvdz,dodz,zi,rdzi,rdzl,eps_m,nsig_pbl
  use pblmod, only: lmbd,karm,karm0,alph,beta,epxilon
  use tends4pertmod, only: time_step_half
  implicit none

! Declare passed variables
  real(r_kind),dimension(lat2,lon2)     ,intent(in   ):: ps
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout):: u,v,t
  integer(i_kind)                       ,intent(in   ):: jstart,jstop

! Declare local parameters
  real(r_kind),parameter:: r10 = 10.0_r_kind
  real(r_kind),parameter:: r20 = 20.0_r_kind

! Declare local variables
  real(r_kind),dimension(lat2,lon2,nsig+1):: prs 
  real(r_kind),dimension(nsig_pbl):: u1_bg,v1_bg,o1_bg
  real(r_kind),dimension(nsig_pbl):: u_bg,v_bg,t_bg,o_bg,rdzi_bg,rdzi_tl,dzi_tl
  real(r_kind),dimension(nsig_pbl):: rdzl_tl,dudz_tl,dvdz_tl,dodz_tl,km_tl
  real(r_kind),dimension(nsig_pbl+1):: p_bg,zi_bg
  real(r_kind),dimension(2:nsig_pbl):: rdzl_bg
  real(r_kind),dimension(2:nsig_pbl):: lmix_bg,ri_bg,km_bg,zmix_bg,ssq_bg
  real(r_kind),dimension(2:nsig_pbl):: dudz_bg,dvdz_bg,dodz_bg,rho_bg
  real(r_kind),dimension(2:nsig_pbl):: lmix_tl,ri_tl
  real(r_kind),dimension(nsig_pbl):: u_tl,v_tl,t_tl,zl_tl,o_tl
  real(r_kind),dimension(nsig_pbl+1):: p_tl,zi_tl,ck_tl,ck_bg
  real(r_kind),dimension(nsig_pbl):: u_out,v_out,t_out

  real(r_kind) rssq_bg,ssq_tl
  real(r_kind) zmix_tl
  real(r_kind):: acoef,bcoef,ccoef
  real(r_kind):: ax1,ax2
  real(r_kind):: aux_tl,ckplus_bg,ckmnus_bg,ckplus_tl,ckmnus_tl
  real(r_kind) rho_tl

  real(r_kind),dimension(nsig_pbl):: aux_bg,ckdif_bg,cksum_bg
  real(r_kind),dimension(nsig_pbl):: acoef0,bcoef0,ccoef0
  real(r_kind),dimension(nsig_pbl):: acoef1,bcoef1,ccoef1
  real(r_kind),dimension(nsig_pbl):: a_tl,b_tl,c_tl
  real(r_kind),dimension(nsig_pbl):: wu,ua,ub,uc,qu,pu
  real(r_kind),dimension(nsig_pbl):: wv,va,vb,vc,qv,pv
  real(r_kind),dimension(nsig_pbl):: wo,oa,ob,oc,qo,po
  real(r_kind),dimension(nsig_pbl):: fu,fv,fo

  integer(i_kind) i,j,k
  
     call getprs_bck_tl(ps,t,prs)

  ua(1)=zero; va(1)=zero;  oa(1)=zero
  uc(nsig_pbl)=zero; vc(nsig_pbl)=zero; oc(nsig_pbl)=zero 

  dudz_tl(1)=zero; dvdz_tl(1)=zero;  dodz_tl(1)=zero
  zi_tl(1)=zero; rdzl_tl(1)=zero; km_tl(1)=zero


  do j=jstart,jstop
    do i=1,lat2

  u_out=zero; v_out=zero; t_out=zero

! Background fields saved from the nonlinear model

      do k=1,nsig_pbl
        t_bg(k)=tges0(i,j,k)
        u_bg(k)=uges0(i,j,k)
        v_bg(k)=vges0(i,j,k)
        o_bg(k)=oges0(i,j,k)
        p_bg(k)=pges0(i,j,k)
        u1_bg(k)=uges1(i,j,k)
        v1_bg(k)=vges1(i,j,k)
        o1_bg(k)=oges1(i,j,k)
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

      do k=2,nsig_pbl
        ck_bg(k)=km_bg(k)*rdzl_bg(k)            ! << NL model repeat >> 
      end do
        ck_bg(1)=zero  ;   ck_bg(nsig_pbl+1)=zero   

! Preliminaries

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

      do k=1,nsig_pbl
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


! Start tangent linear

! Perturbation fields 

      do k=1,nsig_pbl
        t_tl(k)=t(i,j,k)
        u_tl(k)=u(i,j,k)
        v_tl(k)=v(i,j,k)
        p_tl(k)=prs(i,j,k)
      end do
        p_tl(nsig_pbl+1)=prs(i,j,nsig_pbl+1)


!(1) Perturbation of potential temperature

      do k=1,nsig_pbl
          ax1=o_bg(k)/t_bg(k)
          ax2=o_bg(k)/(p_bg(k)+p_bg(k+1))*rd_over_cp
        o_tl(k)=ax1*t_tl(k)-ax2*(p_tl(k)+p_tl(k+1))
      end do



!(2) Perturbation of heights

      do k=1,nsig_pbl
        dzi_tl(k)=(zi_bg(k+1)-zi_bg(k))/t_bg(k)*t_tl(k)  &
                 -four*rd_over_g*t_bg(k)/(p_bg(k)+p_bg(k+1))**2  &
                 *(p_bg(k)*p_tl(k+1)-p_bg(k+1)*p_tl(k))
      end do

      do k=1,nsig_pbl
        zi_tl(k+1)=zi_tl(k)+dzi_tl(k)
      end do

      do k=1,nsig_pbl
        zl_tl(k)=half*(zi_tl(k+1)+zi_tl(k))
        rdzi_tl(k)=-rdzi_bg(k)**2 *(zi_tl(k+1)-zi_tl(k))
      end do

      do k=2,nsig_pbl
        rdzl_tl(k)=-rdzl_bg(k)**2 *(zl_tl(k)-zl_tl(k-1))
      end do


!(3) Perturbation of vertical gradients

      do k=2,nsig_pbl
        dodz_tl(k)=(o_tl(k)-o_tl(k-1))*rdzl_bg(k)  &
                  +(o_bg(k)-o_bg(k-1))*rdzl_tl(k) 
        dudz_tl(k)=(u_tl(k)-u_tl(k-1))*rdzl_bg(k)  &
                  +(u_bg(k)-u_bg(k-1))*rdzl_tl(k)  
        dvdz_tl(k)=(v_tl(k)-v_tl(k-1))*rdzl_bg(k)  &
                  +(v_bg(k)-v_bg(k-1))*rdzl_tl(k) 
      end do

!(4) Perturbation of mixing coefficients

      do k=2,nsig_pbl
          ax1=one/(o_bg(k)+o_bg(k-1))
        zmix_tl=zi_tl(k)
        lmix_tl(k)=karm*zmix_tl/(one+karm0*zmix_bg(k))**2

        ssq_tl=dudz_bg(k)*dudz_tl(k)+dvdz_bg(k)*dvdz_tl(k)
 
        if(ssq_bg(k)< eps_m) then
          ri_tl(k)=ri_bg(k)*( dodz_tl(k)/dodz_bg(k)-&
                              ax1*(o_tl(k)+o_tl(k-1)))
        else
          rssq_bg=one/ssq_bg(k)
          ri_tl(k)=ri_bg(k)*( dodz_tl(k)/dodz_bg(k)-&
                              ax1*(o_tl(k)+o_tl(k-1))-&
                              rssq_bg*two*ssq_tl )
        end if
            if(ri_bg(k)<zero)then             
              rho_tl=-r10/rho_bg(k) *ri_tl(k)  
            else
              rho_tl=-r10*(sqrt(rho_bg(k)))**3*ri_tl(k)
            end if
        if(ssq_bg(k)< eps_m) then
            km_tl(k)=km_bg(k)*( two*lmix_tl(k)/lmix_bg(k) &
                                              +rho_tl/rho_bg(k) )
        else
            km_tl(k)=km_bg(k)*( two*lmix_tl(k)/lmix_bg(k) &
                               +ssq_tl*rssq_bg+rho_tl/rho_bg(k) )
        end if
      end do

      do k=2,nsig_pbl
        ck_tl(k)=ck_bg(k)*(km_tl(k)/km_bg(k)+rdzl_tl(k)/rdzl_bg(k))
      end do
        ck_tl(1)=zero  ;   ck_tl(nsig_pbl+1)=zero


!(5) Perturbation of trapezoidal scheme

! Start tangent linear

!(5.1)
      do k=1,nsig_pbl
        aux_tl=time_step_half*rdzi_tl(k)             
        ckplus_tl=ck_tl(k+1)+ck_tl(k)
        ckmnus_tl=(ck_tl(k+1)-ck_tl(k))*epxilon
          a_tl(k)=aux_bg(k)*(ckplus_tl-ckmnus_tl)+ &
                  aux_tl*ckdif_bg(k)
          c_tl(k)=aux_bg(k)*(ckplus_tl+ckmnus_tl)+ &
                  aux_tl*cksum_bg(k)
          b_tl(k)=-a_tl(k)-c_tl(k)
      end do

      do k=1,nsig_pbl
        pu(k)=ua(k)*a_tl(k)+ub(k)*b_tl(k)+uc(k)*c_tl(k)
        pv(k)=va(k)*a_tl(k)+vb(k)*b_tl(k)+vc(k)*c_tl(k)
        po(k)=oa(k)*a_tl(k)+ob(k)*b_tl(k)+oc(k)*c_tl(k)
      end do

!(5.2)
      call multi_tridiag(acoef0,bcoef0,ccoef0,u_tl,qu,nsig_pbl)
      call multi_tridiag(acoef0,bcoef0,ccoef0,v_tl,qv,nsig_pbl)
      call multi_tridiag(acoef0,bcoef0,ccoef0,o_tl,qo,nsig_pbl)
 
        fu(1:nsig_pbl)=qu(1:nsig_pbl)-pu(1:nsig_pbl)     
        fv(1:nsig_pbl)=qv(1:nsig_pbl)-pv(1:nsig_pbl)
        fo(1:nsig_pbl)=qo(1:nsig_pbl)-po(1:nsig_pbl)

!(5.3)
      call tridiag(acoef1,bcoef1,ccoef1,fu,nsig_pbl)
      call tridiag(acoef1,bcoef1,ccoef1,fv,nsig_pbl)
      call tridiag(acoef1,bcoef1,ccoef1,fo,nsig_pbl)

! Update perturbations of wind and potential temperature

           u_out(1:nsig_pbl)=fu(1:nsig_pbl)    
           v_out(1:nsig_pbl)=fv(1:nsig_pbl)   
           o_tl(1:nsig_pbl)=fo(1:nsig_pbl)  

!(5.4)
! Update perturbation of temperature

     do k=1,nsig_pbl
         ax1=t_bg(k)/o_bg(k)
         ax2=t_bg(k)/(p_bg(k)+p_bg(k+1))*rd_over_cp
       t_out(k)=ax1*o_tl(k)+ax2*(p_tl(k)+p_tl(k+1))
     end do

     do k=1,nsig_pbl
      u(i,j,k)=u_out(k)
      v(i,j,k)=v_out(k)
      t(i,j,k)=t_out(k)
     end do


    end do
  end do

  return
end subroutine pbl_tl


subroutine tridiag(a,b,c,f,jmx)

!  Solves a standard tridiagonal system
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

  q(1)=-c(1)/b(1)
  f(1)= f(1)/b(1)

  do j=2,jmx
    p=one/(b(j)+a(j)*q(j-1))
    q(j)=-c(j)*p
    f(j)=( f(j)-a(j)*f(j-1) )*p
  end do

! Backward pass

  do j=jmx-1,1,-1
    f(j)=f(j)+q(j)*f(j+1)
  end do

end subroutine tridiag


subroutine multi_tridiag(a,b,c,u,f,jmx)

!  multiply a tridiagonal matrix (a,b,c)  with a vector u
!  store result in vector f

  use kinds,only: r_kind,i_kind
  implicit none

  integer(i_kind), intent(in):: jmx
  real(r_kind), dimension(jmx), intent(in):: a,b,c,u
  real(r_kind), dimension(jmx), intent(out):: f
  integer(i_kind) j

   do j=2,jmx-1
     f(j)=u(j-1)*a(j)+u(j)*b(j)+u(j+1)*c(j)
   end do
     f(1)=            u(1)*b(1)+u(1+1)*c(1)
     f(jmx)=u(jmx-1)*a(jmx)+u(jmx)*b(jmx)

end subroutine multi_tridiag
