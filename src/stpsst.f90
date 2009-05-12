module stpsstmod

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpsstmod    module for stpsst and its tangent linear stpsst_tl
!
! abstract: module for stpsst and its tangent linear stpsst_tl
!
! program history log:
!   2005-05-20  Yanqiu zhu - wrap stpsst and its tangent linear stpsst_tl into one module
!   2005-11-16  Derber - remove interfaces
!   2008-12-02  Todling - remove stpsst_tl
!

implicit none

PRIVATE
PUBLIC stpsst

contains

subroutine stpsst(ssthead,rsst,ssst,out,sges)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpsst      calculate penalty and contribution to stepsize
!   prgmmr: derber           org: np23                date: 2004-07-20
!
! abstract: calculate penalty and contribution to stepsize for surface pressure
!            with addition of nonlinear qc
!
! program history log:
!   2004-07-20  derber
!   2004-07-30  treadon - add only to module use, add intent in/out
!   2004-10-09  parrish - add nonlinear qc option
!   2005-04-11  treadon - merge stpsst and stpsst_qc into single routine
!   2005-08-02  derber  - modify for variational qc parameters for each ob
!   2005-09-28  derber  - consolidate location and weight arrays
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!                       - unify NL qc
!   2006-09-18  derber  - modify output for b1 and b3
!   2006-12-11  li      - correct bug in alpha and add cc
!   2007-03-19  tremolet - binning of observations
!   2007-06-04  derber  - use quad precision to get reproducability over number of processors
!   2008-06-02  safford - rm unused var and uses
!
!   input argument list:
!     rsst     - search direction for sst
!     ssst     - analysis increment for sst
!     sges     - step size estimate (4)
!                                         
!   output argument list:         
!     out(1)   - contribution to penalty for conventional sst - sges(1)
!     out(2)   - contribution to penalty for conventional sst - sges(2)
!     out(3)   - contribution to penalty for conventional sst - sges(3)
!     out(4)   - contribution to penalty for conventional sst - sges(4)
!     out(5)   - contribution to numerator for conventional sst
!     out(6)   - contribution to denomenator for conventional sst
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind,r_quad
  use obsmod, only: sst_ob_type
  use qcmod, only: nlnqc_iter,varqc_iter
  use constants, only: zero,half,one,two,tiny_r_kind,cg_term,zero_quad
  use gridmod, only: latlon11
  implicit none

! Declare passed variables
  type(sst_ob_type),pointer,intent(in):: ssthead
  real(r_quad),dimension(6),intent(out):: out
  real(r_kind),dimension(latlon11),intent(in):: rsst,ssst
  real(r_kind),dimension(4),intent(in)::sges

! Declare local variables  
  integer(i_kind) j1,j2,j3,j4
  real(r_kind) w1,w2,w3,w4
  real(r_kind) val,val2
  real(r_kind) cg_sst,pen1,pen2,pen3,pencur,sst0,sst1,sst2,sst3,wgross,wnotgross
  real(r_kind) alpha,ccoef,bcoef1,bcoef2,cc,pg_sst
  type(sst_ob_type), pointer :: sstptr

  out=zero_quad
  alpha=one/(sges(3)-sges(2))
  ccoef=half*alpha*alpha
  bcoef1=half*half*alpha
  bcoef2=sges(3)*ccoef

  sstptr => ssthead
  do while (associated(sstptr))
    if(sstptr%luse)then
     j1=sstptr%ij(1)
     j2=sstptr%ij(2)
     j3=sstptr%ij(3)
     j4=sstptr%ij(4)
     w1=sstptr%wij(1)
     w2=sstptr%wij(2)
     w3=sstptr%wij(3)
     w4=sstptr%wij(4)

     val =w1*rsst(j1)+w2*rsst(j2)+w3*rsst(j3)+w4*rsst(j4)
     val2=w1*ssst(j1)+w2*ssst(j2)+w3*ssst(j3)+w4*ssst(j4)-sstptr%res

     sst0=val2+sges(1)*val
     sst1=val2+sges(2)*val
     sst2=val2+sges(3)*val
     sst3=val2+sges(4)*val

     pencur = sst0*sst0*sstptr%err2
     pen1   = sst1*sst1*sstptr%err2
     pen2   = sst2*sst2*sstptr%err2
     pen3   = sst3*sst3*sstptr%err2

!  Modify penalty term if nonlinear QC
     if (nlnqc_iter .and. sstptr%pg > tiny_r_kind .and.  &
                          sstptr%b  > tiny_r_kind) then
        pg_sst=sstptr%pg*varqc_iter
        cg_sst=cg_term/sstptr%b
        wnotgross= one-pg_sst
        wgross = pg_sst*cg_sst/wnotgross
        pencur = -two*log((exp(-half*pencur) + wgross)/(one+wgross))
        pen1   = -two*log((exp(-half*pen1  ) + wgross)/(one+wgross))
        pen2   = -two*log((exp(-half*pen2  ) + wgross)/(one+wgross))
        pen3   = -two*log((exp(-half*pen3  ) + wgross)/(one+wgross))
     endif

     cc     = (pen1+pen3-two*pen2)*sstptr%raterr2
     out(1) = out(1)+pencur*sstptr%raterr2
     out(2) = out(2)+pen1  *sstptr%raterr2
     out(3) = out(3)+pen2  *sstptr%raterr2
     out(4) = out(4)+pen3  *sstptr%raterr2
     out(5) = out(5)+(pen1-pen3)*sstptr%raterr2*bcoef1+bcoef2*cc
     out(6) = out(6)+cc*ccoef
    end if

    sstptr => sstptr%llpoint

  end do
  
  return
end subroutine stpsst

end module stpsstmod
