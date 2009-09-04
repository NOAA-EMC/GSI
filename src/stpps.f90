module stppsmod

!$$$ module documentation block
!           .      .    .                                       .
! module:   stppsmod    module for stpps and its tangent linear stpps_tl
!  prgmmr:
!
! abstract: module for stpps and its tangent linear stpps_tl
!
! program history log:
!   2005-05-18  Yanqiu zhu - wrap stpps and its tangent linear stpps_tl into one module
!   2005-11-16  Derber - remove interfaces
!   2008-12-02  Todling - remove stpps_tl
!   2009-08-12  lueken - update documentation
!
! subroutines included:
!   sub stpps
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

implicit none

PRIVATE
PUBLIC stpps

contains

subroutine stpps(pshead,rp,sp,out,sges)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpps       calculate penalty and contribution to
!                             stepsize for sfcp, using nonlinear qc
!   prgmmr: derber           org: np23                date: 1991-02-26
!
! abstract: calculate penalty and contribution to stepsize for
!           surface pressure with nonlinear qc.
!
! program history log:
!   1991-02-26  derber
!   1997-12-14  weiyu yang
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2004-10-08  parrish - add nonlinear qc option
!   2005-04-11  treadon - merge stpps and stpps_qc into single routine
!   2005-08-02  derber  - modify for variational qc parameters for each ob
!   2005-09-28  derber  - consolidate location and weight arrays
!   2005-10-21  su      - modify for variational qc
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!   2006-09-18  derber  - modify to output of b1 and b3
!   2007-02-15  rancic  - add foto
!   2007-06-04  derber  - use quad precision to get reproducability over number of processors
!   2008-12-03  todling - changed handling of ptr%time
!
!   input argument list:
!     pshead
!     rp       - search direction for ps
!     sp       - analysis increment for ps
!     sges     - step size estimates (4)
!                                         
!   output argument list:         
!     out(1)   - contribution to penalty for surface pressure - sges(1)
!     out(2)   - contribution to penalty for surface pressure - sges(2)
!     out(3)   - contribution to penalty for surface pressure - sges(3)
!     out(4)   - contribution to penalty for surface pressure - sges(4)
!     out(5)   - contribution to numerator for surface pressure
!     out(6)   - contribution to denomonator for surface pressure
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind,r_quad
  use obsmod, only: ps_ob_type
  use qcmod, only: nlnqc_iter,varqc_iter
  use constants, only: half,one,two,tiny_r_kind,cg_term,zero_quad,r3600
  use gridmod, only: latlon1n1
  use jfunc, only: l_foto,xhat_dt,dhat_dt
  implicit none

! Declare passed variables
  type(ps_ob_type),pointer,intent(in):: pshead
  real(r_quad),dimension(6),intent(out):: out
  real(r_kind),dimension(latlon1n1),intent(in):: rp,sp
  real(r_kind),dimension(4),intent(in):: sges

! Declare local variables
  integer(i_kind) j1,j2,j3,j4
  real(r_kind) val,val2,w1,w2,w3,w4,time_ps
  real(r_kind) alpha,ccoef,bcoef1,bcoef2,cc,ps0
  real(r_kind) cg_ps,pen1,pen2,pen3,pencur,ps1,ps2,ps3,wgross,wnotgross,ps_pg
  type(ps_ob_type), pointer :: psptr

  out=zero_quad
  alpha=one/(sges(3)-sges(2))
  ccoef=half*alpha*alpha
  bcoef1=half*half*alpha
  bcoef2=sges(3)*ccoef

  psptr => pshead
  do while (associated(psptr))
    if(psptr%luse)then
     j1 = psptr%ij(1)
     j2 = psptr%ij(2)
     j3 = psptr%ij(3)
     j4 = psptr%ij(4)
     w1 = psptr%wij(1)
     w2 = psptr%wij(2)
     w3 = psptr%wij(3)
     w4 = psptr%wij(4)
     val =w1* rp(j1)+w2* rp(j2)+w3* rp(j3)+w4* rp(j4)
     val2=w1* sp(j1)+w2* sp(j2)+w3* sp(j3)+w4* sp(j4)-psptr%res
     if(l_foto) then
       time_ps = psptr%time*r3600
       val =val +(w1*dhat_dt%p3d(j1)+w2*dhat_dt%p3d(j2)+ &
                  w3*dhat_dt%p3d(j3)+w4*dhat_dt%p3d(j4))*time_ps
       val2=val2+(w1*xhat_dt%p3d(j1)+w2*xhat_dt%p3d(j2)+ &
                  w3*xhat_dt%p3d(j3)+w4*xhat_dt%p3d(j4))*time_ps
     end if

     ps0=val2+sges(1)*val
     ps1=val2+sges(2)*val
     ps2=val2+sges(3)*val
     ps3=val2+sges(4)*val

     pencur = ps0*ps0*psptr%err2
     pen1   = ps1*ps1*psptr%err2
     pen2   = ps2*ps2*psptr%err2
     pen3   = ps3*ps3*psptr%err2

!  Modify penalty term if nonlinear QC

     if (nlnqc_iter .and. psptr%pg > tiny_r_kind .and.  &
                          psptr%b  > tiny_r_kind) then
        ps_pg=psptr%pg*varqc_iter
        cg_ps=cg_term/psptr%b
        wnotgross= one-ps_pg
        wgross =ps_pg*cg_ps/wnotgross
        pencur = -two*log((exp(-half*pencur)+wgross)/(one+wgross))
        pen1   = -two*log((exp(-half*pen1  )+wgross)/(one+wgross))
        pen2   = -two*log((exp(-half*pen2  )+wgross)/(one+wgross))
        pen3   = -two*log((exp(-half*pen3  )+wgross)/(one+wgross))
     endif
     
     cc  = (pen1+pen3-two*pen2)*psptr%raterr2
     out(1) = out(1)+pencur*psptr%raterr2
     out(2) = out(2)+pen1  *psptr%raterr2
     out(3) = out(3)+pen2  *psptr%raterr2
     out(4) = out(4)+pen3  *psptr%raterr2
     out(5) = out(5)+(pen1-pen3)*psptr%raterr2*bcoef1+cc*bcoef2
     out(6) = out(6)+cc*ccoef
    end if

    psptr => psptr%llpoint
  end do
  
  return
end subroutine stpps

end module stppsmod
