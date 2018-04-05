module nltransf

!$$$   module documentation block
! module:    nltransf
! program history log:
! 2018-01-18 yang/Guo ! Jim Purser's nonlinear transformation for vis and cldch.
!  will work on the document late 
!$$$   end documentation block
!                .      .    .                                       .

  use kinds, only: r_kind, i_kind

  implicit none
  private

  public:: nltransf_forward
        interface nltransf_forward ; module procedure  forward; end interface
  public:: nltransf_inverse
        interface nltransf_inverse ; module procedure  inverse; end interface

!

CONTAINS

subroutine forward(zin,zout,powerp)

!--------------------------------------------------------------
! input argument:
!   zin - vis or cldch
!   powerp - parameter for nltr
! output argument:
!   zout - the vis or cldch after the nonlinear transformation 
!--------------------------------------------------------------
  implicit none
  real(r_kind),intent(in):: zin
  real(r_kind),intent(in):: powerp
  real(r_kind)           :: zout

! local variable
  real(r_kind) :: scaling
  real(r_kind) :: temp                      ! after the nltransformation
  scaling=1.0
  if (powerp ==0.0) then
! log conversion
     zout=log(zin/scaling)
  else
! non log transformation
     temp =(zin/scaling)**powerp
     zout =(temp-1.0)/powerp
  endif
  return
end subroutine forward

subroutine inverse(zin,zout,powerp)
  implicit none
  real(r_kind),intent(in):: zin
  real(r_kind),intent(in):: powerp
  real(r_kind)           :: zout

! Local variable
  real(4) :: scaling
  real(4) :: temp      ! apply inverse nltr to it
  real(4) :: powerpinv
  real(4) :: z1

!change zin from nltr space back to physical space
  scaling=1.0
! NLTR transformation
  if (powerp  == 0.0) then
    zout=exp(zin)/scaling
  else
     powerpinv=1.0/powerp
     z1=(powerp*zin + 1.0)
     z1=z1**powerpinv
     zout=z1*scaling
  endif
  return
end subroutine inverse

end module nltransf
