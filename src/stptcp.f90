module stptcpmod

!$$$ module documentation block
!           .      .    .                                       .
! module:   stptcpmod    module for stptcp
!  prgmmr:
!
! abstract: module for stptcp 
!
! program history log:
!   2005-05-18  Yanqiu zhu - wrap stptcp
!   2009-08-12  lueken - update documentation
!
! subroutines included:
!   sub stptcp
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

implicit none

PRIVATE
PUBLIC stptcp

contains
subroutine stptcp(tcphead,rp,sp,out,sges,nstep)
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
!     tcphead
!     rp       - search direction for ps
!     sp       - analysis increment for ps
!     sges     - step size estimates (nstep)
!     nstep    - number of step sizes   (==0 means use outer iteration values)
!
!   output argument list:
!     out(1:nstep)   - contribution to penalty for surface pressure - sges(1:nstep)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind,r_quad
  use obsmod, only: tcp_ob_type
  use qcmod, only: nlnqc_iter,varqc_iter
  use constants, only: half,one,two,tiny_r_kind,cg_term,zero_quad,r3600
  use gridmod, only: latlon1n1
  use jfunc, only: l_foto,xhat_dt,dhat_dt
  implicit none

! Declare passed variables
  type(tcp_ob_type),pointer,intent(in):: tcphead
  integer(i_kind),intent(in)::nstep
  real(r_quad),dimension(max(1,nstep)),intent(out):: out
  real(r_kind),dimension(latlon1n1),intent(in):: rp,sp
  real(r_kind),dimension(max(1,nstep)),intent(in):: sges

! Declare local variables
  integer(i_kind) j1,j2,j3,j4,kk
  real(r_kind) val,val2,w1,w2,w3,w4,time_tcp
  real(r_kind) cg_ps,wgross,wnotgross,ps_pg,ps
  real(r_kind),dimension(max(1,nstep))::pen
  type(tcp_ob_type), pointer :: tcpptr

  out=zero_quad

  tcpptr => tcphead
  do while (associated(tcpptr))
    if(tcpptr%luse)then
     if(nstep > 0)then
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
       
       do kk=1,nstep
         ps=val2+sges(kk)*val
         pen(kk)=ps*ps*tcpptr%err2
       end do
    
     else
       pen(1)=tcpptr%res*tcpptr%res*tcpptr%err2
     end if

!  Modify penalty term if nonlinear QC

     if (nlnqc_iter .and. tcpptr%pg > tiny_r_kind .and.  &
                          tcpptr%b  > tiny_r_kind) then
        ps_pg=tcpptr%pg*varqc_iter
        cg_ps=cg_term/tcpptr%b
        wnotgross= one-ps_pg
        wgross =ps_pg*cg_ps/wnotgross
        do kk=1,max(1,nstep)
          pen(kk) = -two*log((exp(-half*pen(kk))+wgross)/(one+wgross))
        end do
     endif
     
     out(1) = out(1)+pen(1)*tcpptr%raterr2
     do kk=2,nstep
       out(kk) = out(kk)+(pen(kk)-pen(1))*tcpptr%raterr2
     end do
    end if

    tcpptr => tcpptr%llpoint
  end do
  
  return
end subroutine stptcp

end module stptcpmod
