module stptcpmod

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stptcpmod    module for stptcp
!
! abstract: module for stptcp 
!
! program history log:
!   2005-05-18  Yanqiu zhu - wrap stptcp
!

implicit none

PRIVATE
PUBLIC stptcp

contains
subroutine stptcp(tcphead,rp,sp,out,sges)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stptcp       calculate penalty and contribution to
!                             stepsize for synthetic tc-mslp obs
!
!   prgmmr: kleist            org: np23                date: 2009-02-02
!
! abstract: calculate penalty and contribution to stepsize for
!           surface pressure with nonlinear qc.
!
! program history log:
!   2009-02-02  kleist
!
!   input argument list:
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
  use obsmod, only: tcp_ob_type
  use qcmod, only: nlnqc_iter,varqc_iter
  use constants, only: zero,half,one,two,tiny_r_kind,cg_term,zero_quad,r3600
  use gridmod, only: latlon1n1
  use jfunc, only: iter,jiter,niter_no_qc,jiterstart,l_foto,xhat_dt,dhat_dt
  implicit none

! Declare passed variables
  type(tcp_ob_type),pointer,intent(in):: tcphead
  real(r_quad),dimension(6),intent(out):: out
  real(r_kind),dimension(latlon1n1),intent(in):: rp,sp
  real(r_kind),dimension(4),intent(in):: sges

! Declare local variables
  integer(i_kind) i,j1,j2,j3,j4
  real(r_kind) val,val2,w1,w2,w3,w4,time_tcp
  real(r_kind) alpha,ccoef,bcoef1,bcoef2,cc,ps0
  real(r_kind) cg_ps,pen1,pen2,pen3,pencur,ps1,ps2,ps3,wgross,wnotgross,ps_pg
  type(tcp_ob_type), pointer :: tcpptr

  out=zero_quad
  alpha=one/(sges(3)-sges(2))
  ccoef=half*alpha*alpha
  bcoef1=half*half*alpha
  bcoef2=sges(3)*ccoef

  tcpptr => tcphead
  do while (associated(tcpptr))
    if(tcpptr%luse)then
     j1 = tcpptr%ij(1)
     j2 = tcpptr%ij(2)
     j3 = tcpptr%ij(3)
     j4 = tcpptr%ij(4)
     w1 = tcpptr%wij(1)
     w2 = tcpptr%wij(2)
     w3 = tcpptr%wij(3)
     w4 = tcpptr%wij(4)
     val =w1* rp(j1)+w2* rp(j2)+w3* rp(j3)+w4* rp(j4)
     val2=w1* sp(j1)+w2* sp(j2)+w3* sp(j3)+w4* sp(j4)-tcpptr%res
     if(l_foto) then
       time_tcp = tcpptr%time*r3600
       val =val +(w1*dhat_dt%p3d(j1)+w2*dhat_dt%p3d(j2)+ &
                  w3*dhat_dt%p3d(j3)+w4*dhat_dt%p3d(j4))*time_tcp
       val2=val2+(w1*xhat_dt%p3d(j1)+w2*xhat_dt%p3d(j2)+ &
                  w3*xhat_dt%p3d(j3)+w4*xhat_dt%p3d(j4))*time_tcp
     end if

     ps0=val2+sges(1)*val
     ps1=val2+sges(2)*val
     ps2=val2+sges(3)*val
     ps3=val2+sges(4)*val

     pencur = ps0*ps0*tcpptr%err2
     pen1   = ps1*ps1*tcpptr%err2
     pen2   = ps2*ps2*tcpptr%err2
     pen3   = ps3*ps3*tcpptr%err2

!  Modify penalty term if nonlinear QC

     if (nlnqc_iter .and. tcpptr%pg > tiny_r_kind .and.  &
                          tcpptr%b  > tiny_r_kind) then
        ps_pg=tcpptr%pg*varqc_iter
        cg_ps=cg_term/tcpptr%b
        wnotgross= one-ps_pg
        wgross =ps_pg*cg_ps/wnotgross
        pencur = -two*log((exp(-half*pencur)+wgross)/(one+wgross))
        pen1   = -two*log((exp(-half*pen1  )+wgross)/(one+wgross))
        pen2   = -two*log((exp(-half*pen2  )+wgross)/(one+wgross))
        pen3   = -two*log((exp(-half*pen3  )+wgross)/(one+wgross))
     endif
     
     cc  = (pen1+pen3-two*pen2)*tcpptr%raterr2
     out(1) = out(1)+pencur*tcpptr%raterr2
     out(2) = out(2)+pen1  *tcpptr%raterr2
     out(3) = out(3)+pen2  *tcpptr%raterr2
     out(4) = out(4)+pen3  *tcpptr%raterr2
     out(5) = out(5)+(pen1-pen3)*tcpptr%raterr2*bcoef1+cc*bcoef2
     out(6) = out(6)+cc*ccoef
    end if

    tcpptr => tcpptr%llpoint
  end do
  
  return
end subroutine stptcp

end module stptcpmod
