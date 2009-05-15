module stpspdmod

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpspdmod    module for stpspd and its tangent linear stpspd_tl
!
! abstract: module for stpspd and its tangent linear stpspd_tl
!
! program history log:
!   2005-05-20  Yanqiu zhu - wrap stpspd and its tangent linear stpspd_tl into one module
!   2005-11-16  Derber - remove interfaces
!   2008-12-02  Todling - remove stpspd_tl
!

implicit none

PRIVATE
PUBLIC stpspd

contains

subroutine stpspd(spdhead,ru,rv,su,sv,out,sges)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpspd  calculate penalty and stepsize terms
!                for wind speed, with nonlinear qc.
!   prgmmr: derber           org: np23                date: 1991-02-26
!
! abstract: calculate penalty and stepsize terms for wind speed
!
! program history log:
!   1991-02-26  derber
!   1998-02-03  derber
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-07-29  treadon - add only to module use, add intent in/out
!   2004-10-08  parrish - add nonlinear qc option
!   2005-04-11  treadon - merge stpspd and stpspd_qc into single routine
!   2005-08-02  derber  - modify for variational qc parameters for each ob
!   2005-09-28  derber  - consolidate location and weight arrays
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!                       - unify NL qc
!   2006-09-18  derber  - modify output b1 and b3
!   2007-02-15  rancic  - add foto
!   2007-03-19  tremolet - binning of observations
!   2007-05-10  tremolet - add opt to run as linear procedure
!   2007-06-04  derber  - use quad precision to get reproducability over number of processors
!   2008-12-03  todling - changed handling of ptr%time
!   2009-01-19  todling - re-implement Tremolet's linearization for q1fy10
!
!   input argument list:
!     ru       - search direction for u
!     rv       - search direction for v
!     su       - analysis increment for u
!     sv       - analysis increment for v
!     sges     - step size estimates (4)
!
!   output argument list 
!     out(1)   - contribution to penalty from wind speed sges(1)
!     out(2)   - contribution to penalty from wind speed sges(2)
!     out(3)   - contribution to penalty from wind speed sges(3)
!     out(4)   - contribution to penalty from wind speed sges(4)
!     out(5)   - contribution to numerator from wind speed
!     out(6)   - contribution to denomonator from wind speed
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind,r_quad
  use obsmod, only: spd_ob_type
  use qcmod, only: nlnqc_iter,varqc_iter
  use constants, only: zero,half,one,two,tiny_r_kind,cg_term,zero_quad,r3600
  use gridmod, only: latlon1n
  use jfunc, only: l_foto,xhat_dt,dhat_dt
  use gsi_4dvar, only: ltlint
  implicit none

! Declare passed variables
  type(spd_ob_type),pointer,intent(in):: spdhead
  real(r_kind),dimension(4),intent(in):: sges
  real(r_quad),dimension(6),intent(out):: out
  real(r_kind),dimension(latlon1n),intent(in):: ru,rv,su,sv

! Declare local variables
  integer(i_kind) i,j1,j2,j3,j4
  real(r_kind) w1,w2,w3,w4,time_spd
  real(r_kind) valu,valv,ucur,vcur,spdnl,spdtl
  real(r_kind) pen(3),pentl(3),spd(0:3),uu(0:3),vv(0:3)
  real(r_kind) pen1tl,pen2tl,pen3tl,penctl,cctl
  real(r_kind) cg_spd,pen1,pen2,pen3,pencur,wgross,wnotgross
  real(r_kind) alpha,ccoef,bcoef1,bcoef2,cc,pg_spd
  type(spd_ob_type), pointer :: spdptr

  out=zero_quad
  alpha=one/(sges(3)-sges(2))
  ccoef=half*alpha*alpha
  bcoef1=half*half*alpha
  bcoef2=sges(3)*ccoef
  time_spd=zero

  if(ltlint.and.l_foto) then
    write(6,*)'ltlint & foto not compatible at this time',ltlint,l_foto
    call stop2(314)
  end if

  spdptr => spdhead
  do while (associated(spdptr))

    if(spdptr%luse)then
     j1 = spdptr%ij(1)
     j2 = spdptr%ij(2)
     j3 = spdptr%ij(3)
     j4 = spdptr%ij(4)
     w1 = spdptr%wij(1)
     w2 = spdptr%wij(2)
     w3 = spdptr%wij(3)
     w4 = spdptr%wij(4)

     valu=w1* ru(j1)+w2* ru(j2)+w3* ru(j3)+w4* ru(j4)
     valv=w1* rv(j1)+w2* rv(j2)+w3* rv(j3)+w4* rv(j4)
     ucur=w1* su(j1)+w2* su(j2)+w3* su(j3)+w4* su(j4)+spdptr%uges
     vcur=w1* sv(j1)+w2* sv(j2)+w3* sv(j3)+w4* sv(j4)+spdptr%vges

     if(l_foto) then 
        time_spd=spdptr%time*r3600
        valu=valu +&
            (w1*dhat_dt%u(j1)+w2*dhat_dt%u(j2)+ &
             w3*dhat_dt%u(j3)+w4*dhat_dt%u(j4))*time_spd
        valv=valv +&
            (w1*dhat_dt%v(j1)+w2*dhat_dt%v(j2)+ &
             w3*dhat_dt%v(j3)+w4*dhat_dt%v(j4))*time_spd
        ucur=ucur +&
            (w1*xhat_dt%u(j1)+w2*xhat_dt%u(j2)+ &
             w3*xhat_dt%u(j3)+w4*xhat_dt%u(j4))*time_spd
        vcur=vcur +&
            (w1*xhat_dt%v(j1)+w2*xhat_dt%v(j2)+ &
             w3*xhat_dt%v(j3)+w4*xhat_dt%v(j4))*time_spd
     endif

     do i=0,3
        uu(i)=ucur+sges(i+1)*valu; vv(i)=vcur+sges(i+1)*valv
     enddo

     spd(0)=sqrt(uu(0)*uu(0)+vv(0)*vv(0))-spdptr%res
     pencur = spd(0)*spd(0)*spdptr%err2

     if (ltlint) then

       do i=1,3
          spdnl=sqrt(uu(i)*uu(i)+vv(i)*vv(i))
          spdtl=uu(i)*valu+vv(i)*valv
          if (spdnl>tiny_r_kind*100._r_kind) then
            spdtl=spdtl/spdnl
          else
            spdtl=zero
          endif
          spd(i)=sqrt(uu(i)*uu(i)+vv(i)*vv(i))-spdptr%res
          pentl(i)=two*spdtl*spd(i)*spdptr%err2
          pen(i)= spd(i)*spd(i)*spdptr%err2
       enddo
 
       cc  = (pen  (1)+pen  (3)-two*pen  (2))*spdptr%raterr2
       cctl= (pentl(1)+pentl(3)-two*pentl(2))*spdptr%raterr2
       out(1) = out(1)+pencur*spdptr%raterr2
       out(2) = out(2)+pen(1)*spdptr%raterr2
       out(3) = out(3)+pen(2)*spdptr%raterr2
       out(4) = out(4)+pen(3)*spdptr%raterr2
       out(5) = out(5)+(pen  (1)-pen  (3))*spdptr%raterr2*bcoef1+cctl*bcoef2 +&
                       (pentl(1)-pentl(3))*spdptr%raterr2*bcoef1+cc  *bcoef2
       out(6) = out(6)+cc*ccoef

     else ! <ltlint>

       do i=1,3
         spd(i)=sqrt(uu(i)*uu(i)+vv(i)*vv(i))-spdptr%res
         pen(i)=spd(i)*spd(i)*spdptr%err2
       enddo

!  Modify penalty term if nonlinear QC
       if (nlnqc_iter .and. spdptr%pg > tiny_r_kind .and. &
                            spdptr%b  > tiny_r_kind) then
          pg_spd=spdptr%pg*varqc_iter
          cg_spd=cg_term/spdptr%b
          wnotgross= one-pg_spd
          wgross = pg_spd*cg_spd/wnotgross
          pencur = -two*log((exp(-half*pencur) + wgross)/(one+wgross))
          do i=1,3
             pen(i) = -two*log((exp(-half*pen(i)  ) + wgross)/(one+wgross))
          enddo
       endif

       cc  = (pen(1)+pen(3)-two*pen(2))*spdptr%raterr2
       out(1) = out(1)+pencur*spdptr%raterr2
       out(2) = out(2)+pen(1)*spdptr%raterr2
       out(3) = out(3)+pen(2)*spdptr%raterr2
       out(4) = out(4)+pen(3)*spdptr%raterr2
       out(5) = out(5)+(pen(1)-pen(3))*spdptr%raterr2*bcoef1+cc*bcoef2
       out(6) = out(6)+cc*ccoef

     endif ! < ltlint >

    end if
    
    spdptr => spdptr%llpoint

  end do
  return
end subroutine stpspd

end module stpspdmod
