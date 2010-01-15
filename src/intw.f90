module intwmod
!$$$ module documentation block
!           .      .    .                                       .
! module:   intwmod    module for intw and its tangent linear intw_tl
!   prgmmr:
!
! abstract: module for intw and its tangent linear intw_tl
!
! program history log:
!   2005-05-16  Yanqiu zhu - wrap intw and its tangent linear intw_tl into one module
!   2005-11-16  Derber - remove interfaces
!   2008-11-26  Todling - remove intw_tl; add interface back
!   2009-08-13  lueken - update documentation
!
! subroutines included:
!   sub intw_
!
! variable definitions:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

implicit none

PRIVATE
PUBLIC intw

interface intw; module procedure &
          intw_
end interface

contains

subroutine intw_(whead,ru,rv,su,sv)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intw        apply nonlin qc obs operator for winds
!   prgmmr: derber           org: np23                date: 1991-02-26
!
! abstract: apply observation operator and adjoint for winds with
!             nonlinear qc operator
!
! program history log:
!   1991-02-26  derber
!   1997-12-12  weiyu yang
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2004-10-09  parrish - add nonlinear qc option
!   2005-03-01  parrish - nonlinear qc change to account for inflated obs error
!   2005-04-11  treadon - merge intw and intw_qc into single routine
!   2005-08-02  derber  - modify for variational qc parameters for each ob
!   2005-09-28  derber  - consolidate location and weight arrays
!   2005-10-21  su      - modify for variational qc
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!                       - unify NL qc
!   2006-10-20  rancic  - add foto
!   2007-03-19  tremolet - binning of observations
!   2007-06-05  tremolet - use observation diagnostics structure
!   2007-07-09  tremolet - observation sensitivity
!   2008-01-04  tremolet - Don't apply H^T if l_do_adjoint is false
!   2008-11-28  todling  - turn FOTO optional; changed ptr%time handle
!
!   input argument list:
!     whead    - obs type pointer to obs structure
!     su       - u increment in grid space
!     sv       - v increment in grid space
!     ru
!     rv
!
!   output argument list:
!     ru       - u results from observation operator 
!     rv       - v results from observation operator 
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: half,one,tiny_r_kind,cg_term,r3600
  use obsmod, only: w_ob_type,lsaveobsens,l_do_adjoint
  use qcmod, only: nlnqc_iter,varqc_iter
  use gridmod, only: latlon1n
  use jfunc, only: jiter,l_foto,xhat_dt,dhat_dt
  implicit none

! Declare passed variables
  type(w_ob_type),pointer         ,intent(in   ) :: whead
  real(r_kind),dimension(latlon1n),intent(in   ) :: su,sv
  real(r_kind),dimension(latlon1n),intent(inout) :: ru,rv

! Declare local variables
  integer(i_kind) i1,i2,i3,i4,i5,i6,i7,i8
! real(r_kind) penalty
  real(r_kind) valu,valv,w1,w2,w3,w4,w5,w6,w7,w8,time_w
  real(r_kind) cg_w,p0,gradu,gradv,wnotgross,wgross,term,w_pg
  type(w_ob_type), pointer :: wptr

  wptr => whead
  do while(associated(wptr))
     i1=wptr%ij(1)
     i2=wptr%ij(2)
     i3=wptr%ij(3)
     i4=wptr%ij(4)
     i5=wptr%ij(5)
     i6=wptr%ij(6)
     i7=wptr%ij(7)
     i8=wptr%ij(8)
     w1=wptr%wij(1)
     w2=wptr%wij(2)
     w3=wptr%wij(3)
     w4=wptr%wij(4)
     w5=wptr%wij(5)
     w6=wptr%wij(6)
     w7=wptr%wij(7)
     w8=wptr%wij(8)
  
!    Forward model
     valu=w1* su(i1)+w2* su(i2)+w3* su(i3)+w4* su(i4)+&
          w5* su(i5)+w6* su(i6)+w7* su(i7)+w8* su(i8)
     valv=w1* sv(i1)+w2* sv(i2)+w3* sv(i3)+w4* sv(i4)+&
          w5* sv(i5)+w6* sv(i6)+w7* sv(i7)+w8* sv(i8)
     if (l_foto) then
        time_w=wptr%time*r3600
        valu=valu+&
          (w1*xhat_dt%u(i1)+w2*xhat_dt%u(i2)+ &
           w3*xhat_dt%u(i3)+w4*xhat_dt%u(i4)+ &
           w5*xhat_dt%u(i5)+w6*xhat_dt%u(i6)+ &
           w7*xhat_dt%u(i7)+w8*xhat_dt%u(i8))*time_w
        valv=valv+&
          (w1*xhat_dt%v(i1)+w2*xhat_dt%v(i2)+ &
           w3*xhat_dt%v(i3)+w4*xhat_dt%v(i4)+ &
           w5*xhat_dt%v(i5)+w6*xhat_dt%v(i6)+ &
           w7*xhat_dt%v(i7)+w8*xhat_dt%v(i8))*time_w
     endif

     if (lsaveobsens) then
        wptr%diagu%obssen(jiter) = valu*wptr%raterr2*wptr%err2
        wptr%diagv%obssen(jiter) = valv*wptr%raterr2*wptr%err2
     else
        if (wptr%luse) then
           wptr%diagu%tldepart(jiter)=valu
           wptr%diagv%tldepart(jiter)=valv
        endif
     endif

     if (l_do_adjoint) then
        if (lsaveobsens) then
           gradu = wptr%diagu%obssen(jiter)
           gradv = wptr%diagv%obssen(jiter)

        else
           valu=valu-wptr%ures
           valv=valv-wptr%vres

!          gradient of nonlinear operator
 
           if (nlnqc_iter .and. wptr%pg > tiny_r_kind .and.  &
                                wptr%b  > tiny_r_kind) then
              w_pg=wptr%pg*varqc_iter
              cg_w=cg_term/wptr%b
              wnotgross= one-w_pg
              wgross =w_pg*cg_w/wnotgross                ! wgross is gama in Enderson
              p0=wgross/(wgross+                      &  ! p0 is P in Enderson
              exp(-half*wptr%err2*(valu**2+valv**2))) 
              term=one-p0                                !  term is Wqc in Enderson
              valu = valu*term
              valv = valv*term
           endif

           gradu = valu*wptr%raterr2*wptr%err2
           gradv = valv*wptr%raterr2*wptr%err2
        endif

!       Adjoint
        ru(i1)=ru(i1)+w1*gradu
        ru(i2)=ru(i2)+w2*gradu
        ru(i3)=ru(i3)+w3*gradu
        ru(i4)=ru(i4)+w4*gradu
        ru(i5)=ru(i5)+w5*gradu
        ru(i6)=ru(i6)+w6*gradu
        ru(i7)=ru(i7)+w7*gradu
        ru(i8)=ru(i8)+w8*gradu

        rv(i1)=rv(i1)+w1*gradv
        rv(i2)=rv(i2)+w2*gradv
        rv(i3)=rv(i3)+w3*gradv
        rv(i4)=rv(i4)+w4*gradv
        rv(i5)=rv(i5)+w5*gradv
        rv(i6)=rv(i6)+w6*gradv
        rv(i7)=rv(i7)+w7*gradv
        rv(i8)=rv(i8)+w8*gradv
     
        if (l_foto) then
           gradu=gradu*time_w
           gradv=gradv*time_w
           dhat_dt%u(i1)=dhat_dt%u(i1)+w1*gradu
           dhat_dt%u(i2)=dhat_dt%u(i2)+w2*gradu
           dhat_dt%u(i3)=dhat_dt%u(i3)+w3*gradu
           dhat_dt%u(i4)=dhat_dt%u(i4)+w4*gradu
           dhat_dt%u(i5)=dhat_dt%u(i5)+w5*gradu
           dhat_dt%u(i6)=dhat_dt%u(i6)+w6*gradu
           dhat_dt%u(i7)=dhat_dt%u(i7)+w7*gradu
           dhat_dt%u(i8)=dhat_dt%u(i8)+w8*gradu

           dhat_dt%v(i1)=dhat_dt%v(i1)+w1*gradv
           dhat_dt%v(i2)=dhat_dt%v(i2)+w2*gradv
           dhat_dt%v(i3)=dhat_dt%v(i3)+w3*gradv
           dhat_dt%v(i4)=dhat_dt%v(i4)+w4*gradv
           dhat_dt%v(i5)=dhat_dt%v(i5)+w5*gradv
           dhat_dt%v(i6)=dhat_dt%v(i6)+w6*gradv
           dhat_dt%v(i7)=dhat_dt%v(i7)+w7*gradv
           dhat_dt%v(i8)=dhat_dt%v(i8)+w8*gradv
        endif
     endif

     wptr => wptr%llpoint

  end do
  return
end subroutine intw_

end module intwmod
