module blendmod

!  tool for blend function:

   use kinds, only: r_kind,i_kind

   implicit none

! set default to private
   private
! set subroutines to public
   public :: init_blend
   public :: blend_f
   public :: blend_df
! set passed variables to public

   integer(i_kind) mm,iblend(0:40)
   real(r_kind) x0,dxx,dnorm
  
   contains

   subroutine init_blend(xbegin,xend,iord,ierror)

     use constants, only: half,one
     implicit none

     integer(i_kind),intent(in):: iord
     real(r_kind),intent(in):: xbegin,xend
     integer(i_kind),intent(inout):: ierror

     real(r_kind) x1,xmid,y,dy

!      ierror = 0 for normal return, /=0 if xend=xbegin

     if(xend==xbegin) then
        ierror=1
        return
     end if
     mm=iord
     x0=xbegin
     x1=xend
     dxx=one/(x1-x0)
     call blend(mm,iblend)
     xmid=x0+half*(x1-x0)
     dnorm=one
     call blend_df(xmid,y,dy)
     dnorm=one/dy

   end subroutine init_blend

   subroutine blend_f(x_in,y)

     use constants, only: zero,one
     implicit none

     real(r_kind),intent(in):: x_in
     real(r_kind),intent(out):: y
     real(r_kind) x
     integer(i_kind) j

     x=(x_in-x0)*dxx
     if(x < zero) then
        y=zero
     elseif(x > one) then
        y=one
     else
        y=iblend(mm)
        do j=mm-1,0,-1
           y=x*y+iblend(j)
        end do
        y=y*x**(mm+1)
     end if

   end subroutine blend_f

   subroutine blend_df(x_in,y,dy)

     use constants, only: zero,one
     implicit none

     real(r_kind),intent(in):: x_in
     real(r_kind),intent(out):: y,dy
     real(r_kind) x
     integer(i_kind) j

     x=(x_in-x0)*dxx
     if(x < zero) then
        y=zero ; dy=zero
     elseif(x > one) then
        y=one ; dy=zero
     else
        dy=zero
        y=iblend(mm)
        do j=mm-1,0,-1
           dy=x*dy+y
           y=x*y+iblend(j)
        end do
        dy=((mm+one)*y+x*dy)*x**mm
        y=y*x**(mm+1)
     end if
     dy=dy*dnorm

end subroutine blend_df

subroutine blend(n,iblend)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    blend
!   prgmmr: purser           org: w/nmc22     date:1998
!
! abstract: put coefficients for n+1,..,2n+1, into iblend(0),..
!           iblend(n)
!
! program history log:
!   2004-05-13  kleist  documentation
!   2008-04-23  safford - rm unused uses
!
!   input argument list:
!     n      - number of powers to blend
!
!   output argument list:
!     iblend - blended coefficients
!
! remarks: put the coefficients for powers n+1,..,2n+1, into iblend(0),
!          ..iblend(n),for the "blending polynomial" of continuity-
!          degree n in the interval [0,1].  For example, with n=1, the
!          blending polynomial has up to 1st derivatives continuous
!          with y(0)=0, y(1)=1, y'(0)=y'(1)=0, when y(x)=3x^2-2x^3.
!          Hence iblend={3,-2}
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
  use kinds, only: i_kind
  implicit none

! Declare passed variables
  integer(i_kind)               ,intent(in   ) :: n
  integer(i_kind),dimension(0:n),intent(  out) :: iblend

! Declare local parameters
  integer(i_kind),parameter:: nn=12_i_kind

! Declare local variables
  integer(i_kind) np,i,j,ib
  integer(i_kind),dimension(0:nn):: ipascal(0:nn)

  if(n>nn)stop
  np=n+1
  do i=0,n
    ipascal(i)=0
  enddo

  ipascal(0)=1
  do i=0,n
     do j=i,1,-1
        ipascal(j)=ipascal(j)-ipascal(j-1)
     enddo
  enddo

  ib=1
  do i=1,n
     ib=(ib*2*(2*i+1))/i
  enddo
  do j=0,n
     iblend(j)=(ib*ipascal(j))/(np+j)
  enddo

  return
end subroutine blend

end module blendmod
