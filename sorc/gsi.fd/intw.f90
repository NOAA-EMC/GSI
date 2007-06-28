module intwmod

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intwmod    module for intw and its tangent linear intw_tl
!
! abstract: module for intw and its tangent linear intw_tl
!
! program history log:
!   2005-05-16  Yanqiu zhu - wrap intw and its tangent linear intw_tl into one module
!   2005-11-16  Derber - remove interfaces
!

implicit none

PRIVATE
PUBLIC intw,intw_tl

contains

subroutine intw(ru,rv,su,sv)
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
!
!   input argument list:
!     su       - increment in grid space
!     sv       - increment in grid space
!
!   output argument list:
!     ru       - results from observation operator (0 for no data)
!     rv       - results from observation operator (0for no data)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: zero,half,one,two,tiny_r_kind,cg_term
  use obsmod, only: whead,wptr
  use qcmod, only: nlnqc_iter
  use gridmod, only: latlon1n
  implicit none

! Declare passed variables
  real(r_kind),dimension(latlon1n),intent(in):: su,sv
  real(r_kind),dimension(latlon1n),intent(inout):: ru,rv

! Declare local variables
  integer(i_kind) i,i1,i2,i3,i4,i5,i6,i7,i8
! real(r_kind) penalty
  real(r_kind) valu,valv,w1,w2,w3,w4,w5,w6,w7,w8
  real(r_kind) cg_w,p0,gradu,gradv,wnotgross,wgross,term

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
     valu=w1*su(i1)+w2*su(i2)+w3*su(i3)+w4*su(i4)+&
          w5*su(i5)+w6*su(i6)+w7*su(i7)+w8*su(i8)-wptr%ures

     valv=w1*sv(i1)+w2*sv(i2)+w3*sv(i3)+w4*sv(i4)+&
          w5*sv(i5)+w6*sv(i6)+w7*sv(i7)+w8*sv(i8)-wptr%vres

!    gradient of nonlinear operator
     if (nlnqc_iter .and. wptr%pg > tiny_r_kind .and.  &
                          wptr%b  > tiny_r_kind) then
        cg_w=cg_term/wptr%b
        wnotgross= one-wptr%pg
        wgross =wptr%pg*cg_w/wnotgross          ! wgross is gama in Enderson
        p0=wgross/(wgross+                      &  ! p0 is P in Enderson
         exp(-half*wptr%err2*(valu**2+valv**2))) 
        term=one-p0                                !  term is Wqc in Enderson
        valu = valu*term
        valv = valv*term
     endif

     gradu = valu*wptr%raterr2*wptr%err2
     gradv = valv*wptr%raterr2*wptr%err2

!    Adjoint
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

     wptr => wptr%llpoint

  end do
  return
end subroutine intw


subroutine intw_tl(ru,rv,su,sv,ru_tl,rv_tl,su_tl,sv_tl)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intw_tl        the tangent linear of the operator that applies 
!                              nonlin qc obs operator for winds
!   prgmmr: yanqiu zhu           org: GMAO                date: 2005-05-16
!
! abstract: the tangent linear of the operator that applies observation operator 
!           and adjoint for winds with nonlinear qc operator
!
! program history log:
!   2005-05-16  yanqiu zhu - tangent linear of intw
!   2005-08-02  derber  - modify for variational qc parameters for each ob
!   2005-10-21  su      - modify for variational qc
!
!   input argument list:
!     su       - increment in grid space
!     sv       - increment in grid space
!     su_tl     - tangent linear increment in grid space
!     sv_tl     - tangent linear increment in grid space
!
!   output argument list:
!     ru       - results from observation operator (0 for no data)
!     rv       - results from observation operator (0for no data)
!     ru_tl     - tangent linear results from observation operator (0 for no data)
!     rv_tl     - tangent linear results from observation operator (0for no data)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: zero,half,one,two,tiny_r_kind,cg_term
  use obsmod, only: whead,wptr
  use obsmod_tl, only: ures_tl, vres_tl
  use qcmod, only: nlnqc_iter
  use gridmod, only: latlon1n
  implicit none

! Declare passed variables
  real(r_kind),dimension(latlon1n),intent(in):: su,sv
  real(r_kind),dimension(latlon1n),intent(in):: su_tl,sv_tl
  real(r_kind),dimension(latlon1n),intent(inout):: ru,rv
  real(r_kind),dimension(latlon1n),intent(inout):: ru_tl,rv_tl

! Declare local variables
  integer(i_kind) i,i1,i2,i3,i4,i5,i6,i7,i8
! real(r_kind) penalty
  real(r_kind) valu,valv,w1,w2,w3,w4,w5,w6,w7,w8
  real(r_kind) valu_tl,valv_tl
  real(r_kind) cg_w,p0,gradu,gradv,wnotgross,wgross,term
  real(r_kind) p0_tl,gradu_tl,gradv_tl,term_tl

  wptr => whead
  i=0
  do while(associated(wptr))
     i=i+1
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
     valu_tl=w1*su_tl(i1)+w2*su_tl(i2)+&
             w3*su_tl(i3)+w4*su_tl(i4)+&
             w5*su_tl(i5)+w6*su_tl(i6)+&
             w7*su_tl(i7)+w8*su_tl(i8)-ures_tl(i)

     valv_tl=w1*sv_tl(i1)+w2*sv_tl(i2)+&
             w3*sv_tl(i3)+w4*sv_tl(i4)+&
             w5*sv_tl(i5)+w6*sv_tl(i6)+&
             w7*sv_tl(i7)+w8*sv_tl(i8)-vres_tl(i)

     valu=w1*su(i1)+w2*su(i2)+&
          w3*su(i3)+w4*su(i4)+&
          w5*su(i5)+w6*su(i6)+&
          w7*su(i7)+w8*su(i8)-wptr%ures

     valv=w1*sv(i1)+w2*sv(i2)+&
          w3*sv(i3)+w4*sv(i4)+&
          w5*sv(i5)+w6*sv(i6)+&
          w7*sv(i7)+w8*sv(i8)-wptr%vres

!    gradient of nonlinear operator
     if (nlnqc_iter .and. wptr%pg > tiny_r_kind .and. wptr%b >tiny_r_kind) then
        cg_w=cg_term/wptr%b
        wnotgross= one-wptr%pg
        wgross =wptr%pg*cg_w/wnotgross                  ! wgross is gama in the reference by Enderson
        p0=wgross/(wgross+exp(-half*wptr%err2*(valu**2+valv**2))) ! p0 is P in the reference by Enderson
        term=one-p0                                     !  term is Wqc in the reference by Enderson
        p0_tl = (p0*exp(-half*wptr%err2*(valu**2+valv**2))*(valu*valu_tl+valv*valv_tl))/(wgross+exp(-half*wptr%err2*(valu**2+valv**2)))
        term_tl = -p0_tl
     else
        term = one
        term_tl = zero
     endif
     gradu = valu*term
     gradv = valv*term
     gradu_tl = valu_tl*term + valu*term_tl
     gradv_tl = valv_tl*term + valv*term_tl

     gradu = gradu*wptr%raterr2*wptr%err2
     gradv = gradv*wptr%raterr2*wptr%err2
     gradu_tl = gradu_tl*wptr%raterr2*wptr%err2
     gradv_tl = gradv_tl*wptr%raterr2*wptr%err2

!    Adjoint
     ru_tl(i1)=ru_tl(i1)+w1*gradu_tl
     ru_tl(i2)=ru_tl(i2)+w2*gradu_tl
     ru_tl(i3)=ru_tl(i3)+w3*gradu_tl
     ru_tl(i4)=ru_tl(i4)+w4*gradu_tl
     ru_tl(i5)=ru_tl(i5)+w5*gradu_tl
     ru_tl(i6)=ru_tl(i6)+w6*gradu_tl
     ru_tl(i7)=ru_tl(i7)+w7*gradu_tl
     ru_tl(i8)=ru_tl(i8)+w8*gradu_tl

     rv_tl(i1)=rv_tl(i1)+w1*gradv_tl
     rv_tl(i2)=rv_tl(i2)+w2*gradv_tl
     rv_tl(i3)=rv_tl(i3)+w3*gradv_tl
     rv_tl(i4)=rv_tl(i4)+w4*gradv_tl
     rv_tl(i5)=rv_tl(i5)+w5*gradv_tl
     rv_tl(i6)=rv_tl(i6)+w6*gradv_tl
     rv_tl(i7)=rv_tl(i7)+w7*gradv_tl
     rv_tl(i8)=rv_tl(i8)+w8*gradv_tl

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

     wptr => wptr%llpoint

  end do
  return
end subroutine intw_tl

end module intwmod
