module intpsmod

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intpsmod    module for intps and its tangent linear intps_tl
!
! abstract: module for intps and its tangent linear intps_tl
!
! program history log:
!   2005-05-12  Yanqiu zhu - wrap intps and its tangent linear intps_tl into one module
!   2005-11-16  Derber - remove interfaces
!

implicit none

PRIVATE
PUBLIC intps,intps_tl

contains

subroutine intps(rp,sp)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intps       apply nonlin qc obs operator for ps
!   prgmmr: derber           org: np23                date: 1991-02-26
!
! abstract: apply observation operator and adjoint for ps observations
!           with nonlinear qc operator
!
! program history log:
!   1991-02-26  derber
!   1997-12-12  weiyu yang
!   1999-08-24  derber, j., yang, w., first frozen mpp version
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2004-10-08  parrish - add nonlinear qc option
!   2005-03-01  parrish - nonlinear qc change to account for inflated obs error
!   2005-04-11  treadon - merge intps and intps_qc into single routine
!   2005-08-02  derber  - modify for variational qc parameters for each ob
!   2005-09-28  derber  - consolidate location and weight arrays
!   2005-10-21  su      - modify for variational qc
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!
!   input argument list:
!     sp      - increment in grid space
!
!   output argument list:
!     rp      - results from observation operator (0 for no data)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: half,one,two,zero,tiny_r_kind,cg_term
  use obsmod, only: psptr,pshead
  use qcmod, only: nlnqc_iter
  use gridmod, only: latlon11
  implicit none

! Declare passed variables
  real(r_kind),dimension(latlon11),intent(in):: sp
  real(r_kind),dimension(latlon11),intent(inout):: rp

! Declare local variables
  integer(i_kind) i,j1,j2,j3,j4
! real(r_kind) penalty
  real(r_kind) cg_ps,val,p0,grad,wnotgross,wgross
  real(r_kind) w1,w2,w3,w4

  psptr => pshead
  do while (associated(psptr))
     j1=psptr%ij(1)
     j2=psptr%ij(2)
     j3=psptr%ij(3)
     j4=psptr%ij(4)
     w1=psptr%wij(1)
     w2=psptr%wij(2)
     w3=psptr%wij(3)
     w4=psptr%wij(4)
     
!    Forward model
     val=w1*sp(j1)+w2*sp(j2)+w3*sp(j3)+w4*sp(j4)-psptr%res

!    gradient of nonlinear operator
     if (nlnqc_iter .and. psptr%pg > tiny_r_kind .and.  &
                          psptr%b  > tiny_r_kind) then
        cg_ps=cg_term/psptr%b                          ! b is d in Enderson
        wnotgross= one-psptr%pg                        ! pg is A in Enderson
        wgross =psptr%pg*cg_ps/wnotgross               ! wgross is gama in Enderson
        p0=wgross/(wgross+exp(-half*psptr%err2*val**2)) ! p0 is P in Enderson
        val=val*(one-p0)                                  ! term is Wqc in Enderson
     endif

     grad     = val*psptr%raterr2*psptr%err2

!    Adjoint
     rp(j1)=rp(j1)+w1*grad
     rp(j2)=rp(j2)+w2*grad
     rp(j3)=rp(j3)+w3*grad
     rp(j4)=rp(j4)+w4*grad

     psptr => psptr%llpoint
  end do
  return
end subroutine intps


subroutine intps_tl(rp,sp,rp_tl,sp_tl)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intps_tl       the tangent linear of the operator that applies 
!                              nonlin qc obs operator for ps
!   prgmmr: yanqiu zhu           org: GMAO                date: 2005-05-12
!
! abstract: the tangent linear of the operator that applies observation operator 
!           and adjoint for ps observations with nonlinear qc operator
!
! program history log:
!   2005-05-12  Yanqiu zhu - tangent linear of intps
!   2005-08-02  derber  - modify for variational qc parameters for each ob
!   2005-10-21  su      - modify for variational qc
!
!   input argument list:
!     sp      - increment in grid space
!     sp_tl    - tangent linear increment in grid space
!
!   output argument list:
!     rp      - results from observation operator (0 for no data)
!     rp_tl    - tangent linear results from observation operator (0 for no data)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: half,one,two,zero,tiny_r_kind,cg_term
  use obsmod, only:pshead,psptr
  use obsmod_tl, only: presier_tl
  use qcmod, only: nlnqc_iter
  use gridmod, only: latlon11
  implicit none

! Declare passed variables
  real(r_kind),dimension(latlon11),intent(in):: sp
  real(r_kind),dimension(latlon11),intent(in):: sp_tl
  real(r_kind),dimension(latlon11),intent(inout):: rp
  real(r_kind),dimension(latlon11),intent(inout):: rp_tl

! Declare local variables
  integer(i_kind) i,j1,j2,j3,j4
! real(r_kind) penalty
  real(r_kind) cg_ps,val,p0,grad,wnotgross,wgross,term
  real(r_kind) val_tl,p0_tl,grad_tl,term_tl
  real(r_kind) w1,w2,w3,w4

  psptr => pshead
  i=0
  do while (associated(psptr))
     i=i+1
     j1=psptr%ij(1)
     j2=psptr%ij(2)
     j3=psptr%ij(3)
     j4=psptr%ij(4)
     
!    Forward model
     w1=psptr%wij(1)
     w2=psptr%wij(2)
     w3=psptr%wij(3)
     w4=psptr%wij(4)
     
!    Forward model
     val=w1*sp(j1)+w2*sp(j2)+w3*sp(j3)+w4*sp(j4)-psptr%res
     val_tl=w1*sp_tl(j1)+w2*sp_tl(j2)                      &
           +w3*sp_tl(j3)+w4*sp_tl(j4)-presier_tl(i)

!    gradient of nonlinear operator
     if (nlnqc_iter .and. psptr%pg > tiny_r_kind .and. psptr%b >tiny_r_kind) then
        cg_ps=cg_term/psptr%b                ! b is d in the reference by Enderson
        wnotgross= one-psptr%pg              ! pg is A in the reference by Enderson
        wgross =psptr%pg*cg_ps/wnotgross     ! wgross is gama in the reference by Enderson
        p0=wgross/(wgross+exp(-half*psptr%err2*val**2)) ! p0 is P in the reference by Enderson
        term=one-p0                          ! term is Wqc in the reference by Enderson
        p0_tl = (p0*val*exp(-half*psptr%err2*val**2)*val_tl)/(wgross+exp(-half*psptr%err2*val**2))
        term_tl = -p0_tl
     else
        term = one
        term_tl = zero
     endif
     grad     = val * term
     grad_tl   = val_tl * term + val * term_tl
     grad     = grad*psptr%raterr2*psptr%err2
     grad_tl   = grad_tl*psptr%raterr2*psptr%err2

!    Adjoint
     rp_tl(j1)=rp_tl(j1)+w1*grad_tl
     rp_tl(j2)=rp_tl(j2)+w2*grad_tl
     rp_tl(j3)=rp_tl(j3)+w3*grad_tl
     rp_tl(j4)=rp_tl(j4)+w4*grad_tl

     rp(j1)=rp(j1)+w1*grad
     rp(j2)=rp(j2)+w2*grad
     rp(j3)=rp(j3)+w3*grad
     rp(j4)=rp(j4)+w4*grad

     psptr => psptr%llpoint
  end do
  return
end subroutine intps_tl

end module intpsmod
