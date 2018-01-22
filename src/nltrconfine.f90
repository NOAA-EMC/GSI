module nltrconfine
!$$$   module documentation block
! module:    nltrconfine
! program history log:
! 2018-01-18 yang/Guo
!  will work on the doc late 
!$$$   end documentation block
!                .      .    .                                       .

  use kinds, only: r_kind, i_kind
  use qcmod, only: nltrcv,powerp,adjvisoe,zlow,zhigh,smpara 
  implicit none
  private

!
! Algorithm of Jim Purser's nonlinear transformation and confine for vis and cldch.

  public:: nltrconfine_forward
        interface nltrconfine_forward ; module procedure  forward_; end interface
  public:: nltrconfine_inverse
        interface nltrconfine_inverse ; module procedure  inverse_; end interface

! Usecase:
!
!       use gsi_visconv, only: visconv_namelist
!       use gsi_visconv, only: visconv_confine
!       use gsi_visconv, only: visconv_inverse
!       ...
!       call visconv_namelist(lun=5)
!       ...
!       do i=1,size(z)          ! for forward vis-conversion
!         zt(i)=visconv_forward(z(i))
!       enddo
!       ...
!       do i=1,size(z)          ! for inverse vis-conversion
!         z(i)=visconv_inverse(zt(i))
!       enddo
!


contains

function forward_(z) result(zt)
  implicit none
  real(r_kind) :: zt
  real(r_kind),intent(in):: z
! local variable
  real(r_kind) :: scaling
  real(r_kind) :: temp
  scaling=1.0
  zt = 0.1_r_kind
  temp = (z/scaling)**powerp
  temp =(temp-1.0)/powerp
  write(6,*) 'powerp=',powerp
  write(6,*) 'zlow=',zlow,'zhigh=',zhigh,'smpara=',smpara
  zt=confine(zlow,zhigh,smpara,temp)
end function forward_

function inverse_(zt) result(z)
  implicit none
  real(r_kind) :: z
  real(r_kind),intent(in):: zt
! local variable
  real(r_kind) :: scaling
  real(r_kind) :: temp
  scaling=1.0
inverse calculation of gxp
  


  z=confine_inverse(za,zb,s,zt)
end function inverse_




function confine(za,zb,s,z)  result(zt)
! 
!Evaluate the smooth function, zt of z, that approximates z between za <
!zb, but is smoothly and monotonically
!distorted so as to remain confined between these asymptotic values,
!with a characteristic scale of the distortion being s.

implicit none
real(r_kind), intent (in) :: za, zb,s, z
real (r_kind):: zt
!---real(r_kind):: fun
real (r_kind):: em, g, gt, zc

zc=(za+zb)/2
em=(zb - za)/(2*s)
g=(z-zc)/s
gt=fun(em,g)
zt=gt*s +zc
end function confine

function confine_inverse(za,zb,s,zt) result (z)
!---use pkind, only: dp
implicit none
real(dp), intent(in) :: za,zb,s,zt
real(dp) :: z

!---real(dp) :: funi
real(dp) :: em,g,gt,zc
zc=(za+zb)/2
em=(zb-za)/(2*s)
gt=(zt-zc)/s
g=funi(em,gt)
z=g*s + zc
end function confine_inverse

function fun(em,g) result (gt)
!---use pkind, only : dp
implicit none
real(dp),intent(in) :: em,g
real(dp) :: gt
real(dp) :: a0,a1,r0,r1
a0= g+em
a1=g-em
r0= (sqrt(1+ a0**2) + a0)/2
r1=(sqrt(1+a1**2) + a1)/2
gt=r0 -r1 -em
end function fun
function funi(em,gt) result(g)
!---use pkind, only: dp
implicit none
real(dp), intent (in) :: em,gt
real(dp) :: g

real(dp):: p,r
p=gt/em
r=1-p**2
if (r>0) then
g=p*sqrt(em**2 +1 /r)
else if (p >=-1) then
     g=-1000000
endif
end function funi

end module gsi_visconv
