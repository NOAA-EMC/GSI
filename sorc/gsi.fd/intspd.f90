module intspdmod

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intspdmod    module for intspd and its tangent linear intspd_tl
!
! abstract: module for intspd and its tangent linear intspd_tl
!
! program history log:
!   2005-05-11  Yanqiu zhu - wrap intspd and its tangent linear intspd_tl into one module
!   2005-11-16  Derber - remove interfaces
!

implicit none

PRIVATE
PUBLIC intspd,intspd_tl

contains

subroutine intspd(ru,rv,su,sv)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intspd      apply nonlin qc obs operator for wind speed
!   prgmmr: derber           org: np23                date: 1991-02-26
!
! abstract: apply nonlinear observation operator and adjoint for winds
!
! program history log:
!   1991-02-26  derber
!   1997-12-12  weiyu yang
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2004-10-08 parrish  - add nonlinear qc option
!   2005-03-01  parrish - nonlinear qc change to account for inflated obs error
!   2005-04-11  treadon - merge intspd and intspd_qc into single routine
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
  use obsmod, only: spdhead,spdptr
  use qcmod, only: nlnqc_iter
  use constants, only: zero, half, one, two,tiny_r_kind,cg_term
  use gridmod, only: latlon1n
  implicit none

! Declare passed variables
  real(r_kind),dimension(latlon1n),intent(in):: su,sv
  real(r_kind),dimension(latlon1n),intent(inout):: ru,rv

! Declare local variables
  integer(i_kind) i,j1,j2,j3,j4
  real(r_kind) w1,w2,w3,w4,term
! real(r_kind) penalty
  real(r_kind) uanl,vanl,spdanl,spd,spdn,valv,valu
  real(r_kind) cg_spd,p0,wnotgross,wgross

  spdptr => spdhead
  do while (associated(spdptr))

     j1 = spdptr%ij(1)
     j2 = spdptr%ij(2)
     j3 = spdptr%ij(3)
     j4 = spdptr%ij(4)
     w1 = spdptr%wij(1)
     w2 = spdptr%wij(2)
     w3 = spdptr%wij(3)
     w4 = spdptr%wij(4)

!    Forward model
     uanl=spdptr%uges+w1*su(j1)+w2*su(j2)+w3*su(j3)+w4*su(j4)
     vanl=spdptr%vges+w1*sv(j1)+w2*sv(j2)+w3*sv(j3)+w4*sv(j4)
     spdanl=sqrt(uanl*uanl+vanl*vanl)
     valu=zero
     valv=zero
!    spdn=zero
     spd=spdanl-spdptr%res
!    spdanl=sqrt(spdptr%err2)*spdanl

     spdn=spdptr%raterr2*spdptr%err2*spd
!    Adjoint
     if(spdanl > tiny_r_kind*100._r_kind) then
        valu=uanl/spdanl
        valv=vanl/spdanl
     else
        write(6,*) ' spd ',uanl,vanl,spdanl
        valu=zero
        valv=zero
     end if
     if (nlnqc_iter .and. spdptr%pg > tiny_r_kind .and.  &
                          spdptr%b  > tiny_r_kind) then
        cg_spd=cg_term/spdptr%b
        wnotgross= one-spdptr%pg
        wgross = spdptr%pg*cg_spd/wnotgross
        p0 = wgross/(wgross+exp(-half*spdptr%err2*spd**2))
        term = (one-p0)
        spdn = spdn*term
     endif

     valu=valu*spdn
     valv=valv*spdn
     ru(j1)=ru(j1)+w1*valu
     ru(j2)=ru(j2)+w2*valu
     ru(j3)=ru(j3)+w3*valu
     ru(j4)=ru(j4)+w4*valu
     rv(j1)=rv(j1)+w1*valv
     rv(j2)=rv(j2)+w2*valv
     rv(j3)=rv(j3)+w3*valv
     rv(j4)=rv(j4)+w4*valv

     spdptr => spdptr%llpoint

  end do
  return
end subroutine intspd


subroutine intspd_tl(ru,rv,su,sv,ru_tl,rv_tl,su_tl,sv_tl)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intspd_tl      the tangent linear of the operator that applies 
!                              nonlin qc obs operator for wind speed
!   prgmmr: yanqiu zhu           org: GMAO                date: 2005-05-11
!
! abstract: the tangent linear of the operator that applies nonlinear observation 
!           operator and adjoint for winds
!
! program history log:
!   2005-05-11  yanqiu zhu - tangent linear of intspd
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
  use obsmod, only: spdhead,spdptr
  use obsmod_tl, only: usges_tl, vsges_tl, spdres_tl
  use qcmod, only: nlnqc_iter
  use constants, only: zero, half, one, two, tiny_r_kind,cg_term
  use gridmod, only: latlon1n
  implicit none

! Declare passed variables
  real(r_kind),dimension(latlon1n),intent(in):: su,sv
  real(r_kind),dimension(latlon1n),intent(in):: su_tl,sv_tl
  real(r_kind),dimension(latlon1n),intent(inout):: ru,rv
  real(r_kind),dimension(latlon1n),intent(inout):: ru_tl,rv_tl

! Declare local variables
  integer(i_kind) i,j1,j2,j3,j4
  real(r_kind) w1,w2,w3,w4
! real(r_kind) penalty
  real(r_kind) uanl,vanl,spdanl,spd,spdn,valv,valu
  real(r_kind) uanl_tl,vanl_tl,spdanl_tl,spd_tl,spdn_tl,valv_tl,valu_tl
  real(r_kind) cg_spd,p0,wnotgross,wgross,term
  real(r_kind) p0_tl,term_tl

  spdptr => spdhead
  i=0
  do while (associated(spdptr))
     i=i+1
     j1 = spdptr%ij(1)
     j2 = spdptr%ij(2)
     j3 = spdptr%ij(3)
     j4 = spdptr%ij(4)
     w1 = spdptr%wij(1)
     w2 = spdptr%wij(2)
     w3 = spdptr%wij(3)
     w4 = spdptr%wij(4)

!    Forward model
     uanl_tl=usges_tl(i)+w1*su_tl(j1)+w2*su_tl(j2)+w3*su_tl(j3)+w4*su_tl(j4)
     vanl_tl=vsges_tl(i)+w1*sv_tl(j1)+w2*sv_tl(j2)+w3*sv_tl(j3)+w4*sv_tl(j4)
     uanl=spdptr%uges+w1*su(j1)+w2*su(j2)+w3*su(j3)+w4*su(j4)
     vanl=spdptr%vges+w1*sv(j1)+w2*sv(j2)+w3*sv(j3)+w4*sv(j4)
     spdanl=sqrt(uanl*uanl+vanl*vanl)
     spdanl_tl=(uanl*uanl_tl+vanl*vanl_tl)/spdanl
     spdn_tl=zero
     spdn=zero
     spd_tl=spdanl_tl-spdres_tl(i)
     spd=spdanl-spdptr%res

!    Adjoint
     if(spdanl>zero) then
        if (nlnqc_iter .and. spdptr%pg > tiny_r_kind) then
           cg_spd=cg_term/spdptr%b
           wnotgross= one-spdptr%pg
           wgross = spdptr%pg*cg_spd
           p0 = wnotgross*exp(-half*spdptr%err2*spd**2)+wgross
           term = (p0-wgross)/p0
           p0_tl = -spd*(p0-wgross)*spd_tl*spdptr%err2
           term_tl = wgross/(p0*p0)*p0_tl
        else
           term = one
           term_tl = zero
        endif
        spdn=term * spd/max(half*sqrt(spdptr%raterr2*spdptr%err2),spdanl)
        if (half*sqrt(spdptr%raterr2*spdptr%err2) .ge. spdanl) then
           spdn_tl = (term_tl*spd + term*spd_tl)/(half*spdptr%raterr2*spdptr%err2)
        else
           spdn_tl = term_tl*spd/spdanl + term*spd_tl/spdanl & 
                  - spdn/spdanl*spdanl_tl
        end if
     end if
     valu=uanl*spdn; valv=vanl*spdn
     valu=valu*spdptr%raterr2*spdptr%err2
     valv=valv*spdptr%raterr2*spdptr%err2
     ru(j1)=ru(j1)+w1*valu
     ru(j2)=ru(j2)+w2*valu
     ru(j3)=ru(j3)+w3*valu
     ru(j4)=ru(j4)+w4*valu
     rv(j1)=rv(j1)+w1*valv
     rv(j2)=rv(j2)+w2*valv
     rv(j3)=rv(j3)+w3*valv
     rv(j4)=rv(j4)+w4*valv
     valu_tl=uanl*spdn_tl + uanl_tl*spdn
     valv_tl=vanl*spdn_tl + vanl_tl*spdn
     valu_tl=valu_tl*spdptr%raterr2*spdptr%err2
     valv_tl=valv_tl*spdptr%raterr2*spdptr%err2
     ru_tl(j1)=ru_tl(j1)+w1*valu_tl
     ru_tl(j2)=ru_tl(j2)+w2*valu_tl
     ru_tl(j3)=ru_tl(j3)+w3*valu_tl
     ru_tl(j4)=ru_tl(j4)+w4*valu_tl
     rv_tl(j1)=rv_tl(j1)+w1*valv_tl
     rv_tl(j2)=rv_tl(j2)+w2*valv_tl
     rv_tl(j3)=rv_tl(j3)+w3*valv_tl
     rv_tl(j4)=rv_tl(j4)+w4*valv_tl

     spdptr => spdptr%llpoint

  end do
  return
end subroutine intspd_tl

end module intspdmod
