module intsrwmod

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intsrwmod    module for intsrw and its tangent linear intsrw_tl
!
! abstract: module for intsrw and its tangent linear intsrw_tl
!
! program history log:
!   2005-05-12  Yanqiu zhu - wrap intsrw and its tangent linear intsrw_tl into one module
!   2005-11-16  Derber - remove interfaces
!

implicit none

PRIVATE
PUBLIC intsrw,intsrw_tl


contains

subroutine intsrw(ru,rv,su,sv)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intsrw      apply nonlin qc operator for radar superob winds
!   prgmmr: parrish          org: np22                date: 2004-06-22
!
! abstract: apply radar superob wind operator with nonlinear qc operator
!
! program history log:
!   2004-06-22  parrish, document
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2004-10-09  parrish - add nonlinear qc option
!   2005-03-01  parrish - nonlinear qc change to account for inflated obs error
!   2005-04-11  treadon - merge intsrw and intsrw_qc into single routine
!   2005-08-02  derber  - modify for variational qc parameters for each ob
!   2005-09-28  derber  - consolidate location and weight arrays
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!                       - unify NL qc
!
!   input argument list:
!     su       - increment in grid space
!     sv       - increment in grid space
!
!   output argument list:
!     ru       - results from observation operator (0 for no data)
!     rv       - results from observation operator (0 for no data)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: half,one,two,zero,tiny_r_kind,cg_term
  use obsmod, only: srwhead,srwptr
  use qcmod, only: nlnqc_iter
  use gridmod, only: latlon1n
  implicit none

! Declare passed variables
  real(r_kind),dimension(latlon1n),intent(in):: su,sv
  real(r_kind),dimension(latlon1n),intent(inout):: ru,rv

! Declare local variables  
  integer(i_kind) i,i1,i2,i3,i4,i5,i6,i7,i8
! real(r_kind) penalty
  real(r_kind) valu,valv,w1,w2,w3,w4,w5,w6,w7,w8,valsrw1,valsrw2
  real(r_kind) bigu11,bigu21,bigu12,bigu22
  real(r_kind) cg_srw,p0,gradsrw1,gradsrw2,wnotgross,wgross,term

  srwptr => srwhead
  do while (associated(srwptr))
     i1=srwptr%ij(1)
     i2=srwptr%ij(2)
     i3=srwptr%ij(3)
     i4=srwptr%ij(4)
     i5=srwptr%ij(5)
     i6=srwptr%ij(6)
     i7=srwptr%ij(7)
     i8=srwptr%ij(8)
     w1=srwptr%wij(1)
     w2=srwptr%wij(2)
     w3=srwptr%wij(3)
     w4=srwptr%wij(4)
     w5=srwptr%wij(5)
     w6=srwptr%wij(6)
     w7=srwptr%wij(7)
     w8=srwptr%wij(8)

!    Forward model
     bigu11=srwptr%rsrw(1)
     bigu21=srwptr%rsrw(2)
     bigu12=srwptr%rsrw(3)
     bigu22=srwptr%rsrw(4)
     valu=w1*su(i1)+w2*su(i2)+w3*su(i3)+w4*su(i4)+&
          w5*su(i5)+w6*su(i6)+w7*su(i7)+w8*su(i8)
     valv=w1*sv(i1)+w2*sv(i2)+w3*sv(i3)+w4*sv(i4)+&
          w5*sv(i5)+w6*sv(i6)+w7*sv(i7)+w8*sv(i8)
     valsrw1=bigu11*valu+bigu12*valv-srwptr%res1
     valsrw2=bigu21*valu+bigu22*valv-srwptr%res2

!    gradient of nonlinear operator
     if (nlnqc_iter .and. srwptr%pg > tiny_r_kind .and.  &
                          srwptr%b  > tiny_r_kind) then
        cg_srw=cg_term/srwptr%b
        wnotgross= one-srwptr%pg
        wgross = srwptr%pg*cg_srw/wnotgross
        p0   = wgross/(wgross+exp(-half*srwptr%err2*(valsrw1**2+valsrw2**2)))
        term = (one-p0)
        valsrw1=valsrw1*term
        valsrw2=valsrw2*term
     endif

     gradsrw1 = valsrw1*srwptr%raterr2*srwptr%err2
     gradsrw2 = valsrw2*srwptr%raterr2*srwptr%err2

     valu=bigu11*gradsrw1+bigu21*gradsrw2
     valv=bigu12*gradsrw1+bigu22*gradsrw2

!    Adjoint
     ru(i1)=ru(i1)+w1*valu
     ru(i2)=ru(i2)+w2*valu
     ru(i3)=ru(i3)+w3*valu
     ru(i4)=ru(i4)+w4*valu
     ru(i5)=ru(i5)+w5*valu
     ru(i6)=ru(i6)+w6*valu
     ru(i7)=ru(i7)+w7*valu
     ru(i8)=ru(i8)+w8*valu
     rv(i1)=rv(i1)+w1*valv
     rv(i2)=rv(i2)+w2*valv
     rv(i3)=rv(i3)+w3*valv
     rv(i4)=rv(i4)+w4*valv
     rv(i5)=rv(i5)+w5*valv
     rv(i6)=rv(i6)+w6*valv
     rv(i7)=rv(i7)+w7*valv
     rv(i8)=rv(i8)+w8*valv

     srwptr => srwptr%llpoint

  end do
  return
end subroutine intsrw


subroutine intsrw_tl(ru,rv,su,sv,ru_tl,rv_tl,su_tl,sv_tl)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intsrw_tl      the tangent linear of the operator that applies 
!                              nonlin qc operator for radar superob winds
!   prgmmr: yanqiu zhu          org: GMAO                date: 2005-05-12
!
! abstract: the tangent linear of the operator that applies radar superob 
!           wind operator with nonlinear qc operator
!
! program history log:
!   2005-05-12  yanqiu zhu - tangent linear of intsrw
!   2005-08-02  derber  - modify for variational qc parameters for each ob
!
!   input argument list:
!     su       - increment in grid space
!     sv       - increment in grid space
!     su_tl     - tangent linear increment in grid space
!     sv_tl     - tangent linear increment in grid space
!
!   output argument list:
!     ru       - results from observation operator (0 for no data)
!     rv       - results from observation operator (0 for no data)
!     ru_tl     - tangent linear results from observation operator (0 for no data)
!     rv_tl     - tangent linear results from observation operator (0 for no data)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: half,one,two,zero,tiny_r_kind,cg_term
  use obsmod, only: srwhead,srwptr
  use obsmod_tl, only: srw1res_tl, srw2res_tl
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
  real(r_kind) valu,valv,w1,w2,w3,w4,w5,w6,w7,w8,valsrw1,valsrw2
  real(r_kind) valu_tl,valv_tl,valsrw1_tl,valsrw2_tl
  real(r_kind) bigu11,bigu21,bigu12,bigu22
  real(r_kind) cg_srw,p0,gradsrw1,gradsrw2,wnotgross,wgross,term
  real(r_kind) p0_tl,gradsrw1_tl,gradsrw2_tl,term_tl

  srwptr => srwhead
  i=0
  do while (associated(srwptr))
     i=i+1
     i1=srwptr%ij(1)
     i2=srwptr%ij(2)
     i3=srwptr%ij(3)
     i4=srwptr%ij(4)
     i5=srwptr%ij(5)
     i6=srwptr%ij(6)
     i7=srwptr%ij(7)
     i8=srwptr%ij(8)
     w1=srwptr%wij(1)
     w2=srwptr%wij(2)
     w3=srwptr%wij(3)
     w4=srwptr%wij(4)
     w5=srwptr%wij(5)
     w6=srwptr%wij(6)
     w7=srwptr%wij(7)
     w8=srwptr%wij(8)

!    Forward model
     bigu11=srwptr%rsrw(1)
     bigu21=srwptr%rsrw(2)
     bigu12=srwptr%rsrw(3)
     bigu22=srwptr%rsrw(4)
     valu=w1*su(i1)+w2*su(i2)+w3*su(i3)+w4*su(i4)+&
          w5*su(i5)+w6*su(i6)+w7*su(i7)+w8*su(i8)
     valv=w1*sv(i1)+w2*sv(i2)+w3*sv(i3)+w4*sv(i4)+&
          w5*sv(i5)+w6*sv(i6)+w7*sv(i7)+w8*sv(i8)
     valsrw1=bigu11*valu+bigu12*valv-srwptr%res1
     valsrw2=bigu21*valu+bigu22*valv-srwptr%res2
     valu_tl=w1*su_tl(i1)+w2*su_tl(i2)+w3*su_tl(i3)+w4*su_tl(i4)+&
             w5*su_tl(i5)+w6*su_tl(i6)+w7*su_tl(i7)+w8*su_tl(i8)
     valv_tl=w1*sv_tl(i1)+w2*sv_tl(i2)+w3*sv_tl(i3)+w4*sv_tl(i4)+&
             w5*sv_tl(i5)+w6*sv_tl(i6)+w7*sv_tl(i7)+w8*sv_tl(i8)
     valsrw1_tl=bigu11*valu_tl+bigu12*valv_tl-srw1res_tl(i)
     valsrw2_tl=bigu21*valu_tl+bigu22*valv_tl-srw2res_tl(i)


!    gradient of nonlinear operator
     if (nlnqc_iter .and. srwptr%pg > tiny_r_kind) then
        cg_srw=cg_term/srwptr%b
        wnotgross= one-srwptr%pg
        wgross = srwptr%pg*cg_srw
        p0   = wnotgross*exp(-half*srwptr%err2*(valsrw1**2+valsrw2**2))+wgross
        term = (p0-wgross)/p0
        p0_tl = -srwptr%err2*(valsrw1*valsrw1_tl+valsrw2*valsrw2_tl)*(p0-wgross)
        term_tl = wgross/(p0*p0)*p0_tl
     else
        term = one
        term_tl = zero
     endif
     gradsrw1=valsrw1*term
     gradsrw2=valsrw2*term
     gradsrw1_tl=valsrw1_tl*term + valsrw1*term_tl
     gradsrw2_tl=valsrw2_tl*term + valsrw2*term_tl

     gradsrw1 = gradsrw1*srwptr%raterr2*srwptr%err2
     gradsrw2 = gradsrw2*srwptr%raterr2*srwptr%err2
     gradsrw1_tl = gradsrw1_tl*srwptr%raterr2*srwptr%err2
     gradsrw2_tl = gradsrw2_tl*srwptr%raterr2*srwptr%err2

     valu=bigu11*gradsrw1+bigu21*gradsrw2
     valv=bigu12*gradsrw1+bigu22*gradsrw2
     valu_tl=bigu11*gradsrw1_tl+bigu21*gradsrw2_tl
     valv_tl=bigu12*gradsrw1_tl+bigu22*gradsrw2_tl

!    Adjoint
     ru(i1)=ru(i1)+w1*valu
     ru(i2)=ru(i2)+w2*valu
     ru(i3)=ru(i3)+w3*valu
     ru(i4)=ru(i4)+w4*valu
     ru(i5)=ru(i5)+w5*valu
     ru(i6)=ru(i6)+w6*valu
     ru(i7)=ru(i7)+w7*valu
     ru(i8)=ru(i8)+w8*valu
     rv(i1)=rv(i1)+w1*valv
     rv(i2)=rv(i2)+w2*valv
     rv(i3)=rv(i3)+w3*valv
     rv(i4)=rv(i4)+w4*valv
     rv(i5)=rv(i5)+w5*valv
     rv(i6)=rv(i6)+w6*valv
     rv(i7)=rv(i7)+w7*valv
     rv(i8)=rv(i8)+w8*valv
     ru_tl(i1)=ru_tl(i1)+w1*valu_tl
     ru_tl(i2)=ru_tl(i2)+w2*valu_tl
     ru_tl(i3)=ru_tl(i3)+w3*valu_tl
     ru_tl(i4)=ru_tl(i4)+w4*valu_tl
     ru_tl(i5)=ru_tl(i5)+w5*valu_tl
     ru_tl(i6)=ru_tl(i6)+w6*valu_tl
     ru_tl(i7)=ru_tl(i7)+w7*valu_tl
     ru_tl(i8)=ru_tl(i8)+w8*valu_tl
     rv_tl(i1)=rv_tl(i1)+w1*valv_tl
     rv_tl(i2)=rv_tl(i2)+w2*valv_tl
     rv_tl(i3)=rv_tl(i3)+w3*valv_tl
     rv_tl(i4)=rv_tl(i4)+w4*valv_tl
     rv_tl(i5)=rv_tl(i5)+w5*valv_tl
     rv_tl(i6)=rv_tl(i6)+w6*valv_tl
     rv_tl(i7)=rv_tl(i7)+w7*valv_tl
     rv_tl(i8)=rv_tl(i8)+w8*valv_tl

     srwptr => srwptr%llpoint

  end do
  return
end subroutine intsrw_tl

end module intsrwmod
