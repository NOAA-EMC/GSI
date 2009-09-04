module stprwmod

!$$$ module documentation block
!           .      .    .                                       .
! module:   stprwmod    module for stprw and its tangent linear stprw_tl
!  prgmmr:
!
! abstract: module for stprw and its tangent linear stprw_tl
!
! program history log:
!   2005-05-19  Yanqiu zhu - wrap stprw and its tangent linear stprw_tl into one module
!   2005-11-16  Derber - remove interfaces
!   2008-12-02  Todling - remove stprw_tl
!   2009-08-12  lueken - update documentation
!
! subroutines included:
!   sub stprw
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

implicit none

PRIVATE
PUBLIC stprw

contains

subroutine stprw(rwhead,ru,rv,su,sv,out,sges)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stprw       calculate penalty and contribution to
!                            stepsize with nonlinear qc added.
!   prgmmr: derber           org: np23                date: 1991-02-26
!
! abstract: calculate penalty and contribution to stepsize from radar winds
!
! program history log:
!   1991-02-26  derber
!   1999-11-22  yang
!   2004-07-29  treadon - add only to module use, add intent in/out
!   2004-10-07  parrish - add nonlinear qc option
!   2005-04-11  treadon - merge stprw and stprw_qc into single routine
!   2005-08-02  derber  - modify for variational qc parameters for each ob
!   2005-09-28  derber  - consolidate location and weight arrays
!   2007-03-19  tremolet - binning of observations
!   2007-07-28  derber  - modify to use new inner loop obs data structure
!                       - unify NL qc
!   2007-02-15  rancic - add foto
!   2007-06-04  derber  - use quad precision to get reproducability over number of processors
!   2008-06-02  safford - rm unused var and uses
!   2008-12-03  todling - changed handling of ptr%time
!
!   input argument list:
!     rwhead
!     ru       - search direction for u
!     rv       - search direction for v
!     su       - analysis increment for u
!     sv       - analysis increment for v
!     sges     - step size estimates (4)
!
!   output argument list     - output for step size calculation
!     out(1)   - penalty from radar winds sges(1)
!     out(2)   - penalty from radar winds sges(1)
!     out(3)   - penalty from radar winds sges(1)
!     out(4)   - penalty from radar winds sges(1)
!     out(5)   - pen(sges1)-pen(sges2)
!     out(6)   - pen(sges3)-pen(sges2)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind,r_quad
  use obsmod, only: rw_ob_type
  use qcmod, only: nlnqc_iter,varqc_iter
  use constants, only: half,one,two,tiny_r_kind,cg_term,zero_quad,r3600
  use gridmod, only: latlon1n
  use jfunc, only: l_foto,xhat_dt,dhat_dt
  implicit none

! Declare passed variables
  type(rw_ob_type),pointer,intent(in):: rwhead
  real(r_quad),dimension(6),intent(out):: out
  real(r_kind),dimension(latlon1n),intent(in):: ru,rv,su,sv
  real(r_kind),dimension(4),intent(in):: sges

! Declare local variables
  integer(i_kind) j1,j2,j3,j4,j5,j6,j7,j8
  real(r_kind) valrw,facrw,w1,w2,w3,w4,w5,w6,w7,w8,time_rw
  real(r_kind) cg_rw,rw0,rw1,rw2,rw3,pen1,pen2,pen3,pencur,wgross,wnotgross
  real(r_kind) alpha,ccoef,bcoef1,bcoef2,cc,pg_rw
  type(rw_ob_type), pointer :: rwptr

  out=zero_quad
  alpha=one/(sges(3)-sges(2))
  ccoef=half*alpha*alpha
  bcoef1=half*half*alpha
  bcoef2=sges(3)*ccoef

  rwptr => rwhead
  do while (associated(rwptr))
    if(rwptr%luse)then
     j1=rwptr%ij(1)
     j2=rwptr%ij(2)
     j3=rwptr%ij(3)
     j4=rwptr%ij(4)
     j5=rwptr%ij(5)
     j6=rwptr%ij(6)
     j7=rwptr%ij(7)
     j8=rwptr%ij(8)
     w1=rwptr%wij(1)
     w2=rwptr%wij(2)
     w3=rwptr%wij(3)
     w4=rwptr%wij(4)
     w5=rwptr%wij(5)
     w6=rwptr%wij(6)
     w7=rwptr%wij(7)
     w8=rwptr%wij(8)
     valrw=(w1* ru(j1)+w2* ru(j2)+w3* ru(j3)+w4* ru(j4)+              &
            w5* ru(j5)+w6* ru(j6)+w7* ru(j7)+w8* ru(j8))*rwptr%cosazm+&
           (w1* rv(j1)+w2* rv(j2)+w3* rv(j3)+w4* rv(j4)+              &
            w5* rv(j5)+w6* rv(j6)+w7* rv(j7)+w8* rv(j8))*rwptr%sinazm
     facrw=(w1* su(j1)+w2* su(j2)+w3* su(j3)+w4* su(j4)+              &
            w5* su(j5)+w6* su(j6)+w7* su(j7)+w8* su(j8))*rwptr%cosazm+&
           (w1* sv(j1)+w2* sv(j2)+w3* sv(j3)+w4* sv(j4)+              &
            w5* sv(j5)+w6* sv(j6)+w7* sv(j7)+w8* sv(j8))*rwptr%sinazm-&
            rwptr%res
     if(l_foto) then
       time_rw=rwptr%time*r3600
       valrw=valrw+((w1*dhat_dt%u(j1)+w2*dhat_dt%u(j2)+ &
                     w3*dhat_dt%u(j3)+w4*dhat_dt%u(j4)+              &
                     w5*dhat_dt%u(j5)+w6*dhat_dt%u(j6)+ &
                     w7*dhat_dt%u(j7)+w8*dhat_dt%u(j8))*rwptr%cosazm+&
                    (w1*dhat_dt%v(j1)+w2*dhat_dt%v(j2)+ &
                     w3*dhat_dt%v(j3)+w4*dhat_dt%v(j4)+              &
                     w5*dhat_dt%v(j5)+w6*dhat_dt%v(j6)+ &
                     w7*dhat_dt%v(j7)+w8*dhat_dt%v(j8))*rwptr%sinazm)*time_rw
       facrw=facrw+((w1*xhat_dt%u(j1)+w2*xhat_dt%u(j2)+ &
                     w3*xhat_dt%u(j3)+w4*xhat_dt%u(j4)+              &
                     w5*xhat_dt%u(j5)+w6*xhat_dt%u(j6)+ &
                     w7*xhat_dt%u(j7)+w8*xhat_dt%u(j8))*rwptr%cosazm+&
                    (w1*xhat_dt%v(j1)+w2*xhat_dt%v(j2)+ &
                     w3*xhat_dt%v(j3)+w4*xhat_dt%v(j4)+              &
                     w5*xhat_dt%v(j5)+w6*xhat_dt%v(j6)+ &
                     w7*xhat_dt%v(j7)+w8*xhat_dt%v(j8))*rwptr%sinazm)*time_rw  
     end if
     rw0=facrw+sges(1)*valrw
     rw1=facrw+sges(2)*valrw
     rw2=facrw+sges(3)*valrw
     rw3=facrw+sges(4)*valrw

     pencur = rw0*rw0*rwptr%err2
     pen1   = rw1*rw1*rwptr%err2
     pen2   = rw2*rw2*rwptr%err2
     pen3   = rw3*rw3*rwptr%err2

!  Modify penalty term if nonlinear QC
     if (nlnqc_iter .and. rwptr%pg > tiny_r_kind .and.  &
                          rwptr%b  > tiny_r_kind) then
        pg_rw=rwptr%pg*varqc_iter
        cg_rw=cg_term/rwptr%b
        wnotgross= one-pg_rw
        wgross = pg_rw*cg_rw/wnotgross
        pencur = -two*log((exp(-half*pencur) + wgross)/(one+wgross))
        pen1   = -two*log((exp(-half*pen1  ) + wgross)/(one+wgross))
        pen2   = -two*log((exp(-half*pen2  ) + wgross)/(one+wgross))
        pen3   = -two*log((exp(-half*pen3  ) + wgross)/(one+wgross))
     endif

     cc     = (pen1+pen3-two*pen2)*rwptr%raterr2
     out(1) = out(1)+pencur*rwptr%raterr2
     out(2) = out(2)+pencur*rwptr%raterr2
     out(3) = out(3)+pencur*rwptr%raterr2
     out(4) = out(4)+pencur*rwptr%raterr2
     out(5) = out(5)+(pen1-pen3)*rwptr%raterr2*bcoef1+cc*bcoef2
     out(6) = out(6)+cc*ccoef
    end if

    rwptr => rwptr%llpoint

  end do
  return
end subroutine stprw

end module stprwmod
