module stpcldmod

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpqmod    module for stpq and its tangent linear stpq_tl
!
! abstract: module for stpq and its tangent linear stpq_tl
!
! program history log:
!   2005-05-19  Yanqiu zhu - wrap stpq and its tangent linear stpq_tl into one module
!   2005-11-16  Derber - remove interfaces
!

implicit none

PRIVATE
PUBLIC stpcld

contains

subroutine stpcld(rq,sq,rc,sc,sges,out)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpcld      calcuate penalty and stepsize from cld
!                            with addition of nonlinear qc.
!   prgmmr: derber           org: np23                date: 1991-02-26
!
! abstract: calculate penalty and contribution to stepsize from cld
!           using nonlinear qc.
!
! program history log:
!   2007-02-27  derber
!   2007-06-04  derber  - use quad precision to get reproducability over number
of processors
!
!   input argument list:
!     rq       - search direction for q
!     sq       - analysis increment for q
!     rc       - search direction for cld
!     sc       - analysis increment for cld
!     sges     - step size estimates (4)
!
!   output argument list:
!     out(1) - contribution of penalty from q - sges(1)
!     out(2) - contribution of penalty from q - sges(2)
!     out(3) - contribution of penalty from q - sges(3)
!     out(4) - contribution of penalty from q - sges(4)
!     out(5) - pen(sges1)-pen(sges2)
!     out(6) - pen(sges3)-pen(sges2)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind,r_quad
  use obsmod, only: cldptr,cldhead
  use qcmod, only: nlnqc_iter
  use gridmod, only: latlon1n
  use constants, only: zero,half,one,two,tiny_r_kind,cg_term,zero_quad
  implicit none

! Declare passed variables
  real(r_quad),dimension(6),intent(out):: out
  real(r_kind),dimension(latlon1n),intent(in):: rq,sq,rc,sc
  real(r_kind),dimension(4),intent(in):: sges

! Declare local variables
  integer(i_kind) i,j1,j2,j3,j4,j5,j6,j7,j8,itype
  real(r_kind) cg_q,pen1,pen2,pen3,pencur,c1,c2,c3,val,val2,wgross,wnotgross
  real(r_kind) w1,w2,w3,w4,w5,w6,w7,w8,c0
  real(r_kind) alpha,ccoef,bcoef1,bcoef2,cc

  out=zero_quad
  alpha=one/(sges(3)-sges(2))
  ccoef=half*alpha*alpha
  bcoef1=half*half*alpha
  bcoef2=sges(3)*ccoef

  cldptr => cldhead
  do while (associated(cldptr))
    if(cldptr%luse)then
     itype = cldptr%itype
     j1=cldptr%ij(1)
     j2=cldptr%ij(2)
     j3=cldptr%ij(3)
     j4=cldptr%ij(4)
     j5=cldptr%ij(5)
     j6=cldptr%ij(6)
     j7=cldptr%ij(7)
     j8=cldptr%ij(8)
     w1=cldptr%wij(1)
     w2=cldptr%wij(2)
     w3=cldptr%wij(3)
     w4=cldptr%wij(4)
     w5=cldptr%wij(5)
     w6=cldptr%wij(6)
     w7=cldptr%wij(7)
     w8=cldptr%wij(8)

     if(itype == 0)then
       val= w1*rq(j1)+w2*rq(j2)+w3*rq(j3)+w4*rq(j4)+ &
            w5*rq(j5)+w6*rq(j6)+w7*rq(j7)+w8*rq(j8)
       val2=w1*sq(j1)+w2*sq(j2)+w3*sq(j3)+w4*sq(j4)+ &
            w5*sq(j5)+w6*sq(j6)+w7*sq(j7)+w8*sq(j8)-cldptr%res
     else
       val= w1*rc(j1)+w2*rc(j2)+w3*rc(j3)+w4*rc(j4)+ &
            w5*rc(j5)+w6*rc(j6)+w7*rc(j7)+w8*rc(j8)
       val2=w1*sc(j1)+w2*sc(j2)+w3*sc(j3)+w4*sc(j4)+ &
            w5*sc(j5)+w6*sc(j6)+w7*sc(j7)+w8*sc(j8)-cldptr%res
     end if
     c0=val2+sges(1)*val
     c1=val2+sges(2)*val
     c2=val2+sges(3)*val
     c3=val2+sges(4)*val

     pencur = c0*c0*cldptr%err2
     pen1   = c1*c1*cldptr%err2
     pen2   = c2*c2*cldptr%err2
     pen3   = c3*c3*cldptr%err2

!  Modify penalty term if nonlinear QC
     if (nlnqc_iter .and. cldptr%pg > tiny_r_kind .and. &
                          cldptr%b  > tiny_r_kind) then
        cg_q=cg_term/cldptr%b
        wnotgross= one-cldptr%pg
        wgross = cldptr%pg*cg_q/wnotgross
        pencur = -two*log((exp(-half*pencur)+wgross)/(one+wgross))
        pen1   = -two*log((exp(-half*pen1  )+wgross)/(one+wgross))
        pen2   = -two*log((exp(-half*pen2  )+wgross)/(one+wgross))
        pen3   = -two*log((exp(-half*pen3  )+wgross)/(one+wgross))
     endif
     
     cc  = (pen1+pen3-two*pen2)*cldptr%raterr2
     out(1) = out(1)+pencur*cldptr%raterr2
     out(2) = out(2)+pen1  *cldptr%raterr2
     out(3) = out(3)+pen2  *cldptr%raterr2
     out(4) = out(4)+pen3  *cldptr%raterr2
     out(5) = out(5)+(pen1-pen3)*cldptr%raterr2*bcoef1+cc*bcoef2
     out(6) = out(6)+cc*ccoef
    end if

    cldptr => cldptr%llpoint

  end do

  return
end subroutine stpcld

end module stpcldmod
