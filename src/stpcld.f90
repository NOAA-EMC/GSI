module stpcldmod

!$$$ module documentation block
!           .      .    .                                       .
! module:   stpqmod    module for stpq and its tangent linear stpq_tl
!  prgmmr:
!
! abstract: module for stpq and its tangent linear stpq_tl
!
! program history log:
!   2005-05-19  Yanqiu zhu - wrap stpq and its tangent linear stpq_tl into one module
!   2005-11-16  Derber - remove interfaces
!   2009-08-12  lueken - updated documentation
!
! subroutines included:
!   sub stpcld
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

implicit none

PRIVATE
PUBLIC stpcld

contains

subroutine stpcld(rq,sq,rc,sc,out,sges,nstep)
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
!     sges     - step size estimates (nstep)
!     nstep    - number of input stepsize estimates (== 0 means use current outer loop guess)
!
!   output argument list:
!     out(1:nstep) - contribution of penalty from q - sges(1:nstep)
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
  use constants, only: izero,ione,half,one,two,tiny_r_kind,cg_term,zero_quad
  implicit none

! Declare passed variables
  integer(i_kind)                        ,intent(in   ) : :nstep
  real(r_quad),dimension(max(ione,nstep)),intent(  out) :: out
  real(r_kind),dimension(latlon1n)       ,intent(in   ) :: rq,sq,rc,sc
  real(r_kind),dimension(max(ione,nstep)),intent(in   ) :: sges

! Declare local variables
  integer(i_kind) j1,j2,j3,j4,j5,j6,j7,j8,itype
  real(r_kind) cg_q,val,val2,wgross,wnotgross
  real(r_kind),dimension(max(ione,nstep):: pen
  real(r_kind) w1,w2,w3,w4,w5,w6,w7,w8,cc

  out=zero_quad

  cldptr => cldhead
  do while (associated(cldptr))
     if(cldptr%luse)then
        itype = cldptr%itype
        if(nstep > izero)then
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
 
           if(itype == izero)then
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
           do kk=1,nstep
              cc=val2+sges(1)*val
              pen(kk)=cc*cc*cldptr%err2
           end do
        else
           pen(1)=cldptr%res*cldptr%res*cldptr%err2
        end if



!       Modify penalty term if nonlinear QC
        if (nlnqc_iter .and. cldptr%pg > tiny_r_kind .and. &
                             cldptr%b  > tiny_r_kind) then
           cg_q=cg_term/cldptr%b
           wnotgross= one-cldptr%pg
           wgross = cldptr%pg*cg_q/wnotgross
           do kk=1,max(nstep,ione)
              pen(kk)= -two*log((exp(-half*pen(kk))+wgross)/(one+wgross))
           end do
        endif
     
        out(1) = out(1)+pen(1)*cldptr%raterr2
        do kk=2,nstep
           out(kk) = out(1) + (pen(kk)-pen(1))*cldptr%raterr2
        end do
     end if

     cldptr => cldptr%llpoint

  end do

  return
end subroutine stpcld

end module stpcldmod
