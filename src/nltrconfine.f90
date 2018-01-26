module nltrconfine

!$$$   module documentation block
! module:    nltrconfine
! program history log:
! 2018-01-18 yang/Guo ! Jim Purser's nonlinear transformation and confine function  for vis and cldch.
!  will work on the document late 
!  zlow and zhigh values given a power p value, given scaling =1. with
!  vislow=0.1, vishigh=16000
!powerp=    1.000000     gxp_vislow= -0.9000000
!powerp=    1.000000     gxp_vishigh=   15999.00
!powerp=   0.8000000     gxp_vislow=  -1.051888
!powerp=   0.8000000     gxp_vishigh=   2884.150
!powerp=   0.6000000     gxp_vislow=  -1.248019
!powerp=   0.6000000     gxp_vishigh=   553.3689
!powerp=   0.4000000     gxp_vislow=  -1.504732
!powerp=   0.4000000     gxp_vishigh=   117.6124
!powerp=   0.2000000     gxp_vislow=  -1.845213
!powerp=   0.2000000     gxp_vishigh=   29.65724
!$$$   end documentation block
!                .      .    .                                       .

  use kinds, only: r_kind, i_kind
  use qcmod, only: nltrcv,powerp,adjvisoe,zlow,zhigh,smpara

  implicit none
  private

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


CONTAINS

subroutine forward_(zin,zout)

!--------------------------------------------------------------
! input argument:
!   zin - visibility, could be the one passed the threshold=16000
! output argument:
!   zout - the final visibility after the nonlinear transformation 
!         and confining
!--------------------------------------------------------------
  implicit none
  real(r_kind),intent(in):: zin
  real(r_kind) :: zout

! local variable
  real(r_kind) :: scaling
  real(r_kind) :: temp                      ! after the nltransformation
  real (r_kind):: zc,em,g
  real(r_kind) :: a0,a1,r0,r1,gt

  scaling=1.0
! default zt value: do not know an appropriate value
!  zout = ??? 1.0_r_kind

! nonlinear transformation
  temp = (zin/scaling)**powerp
  temp =(temp-1.0)/powerp
! RY: for check, clean it later
  write(6,*) 'powerp=',powerp
  write(6,*) 'zlow=',zlow,'zhigh=',zhigh,'smpara=',smpara
! confine function 
  zc=(zlow+zhigh)/2.0
  em=(zhigh-zlow)/(2.0*smpara)
  g=(temp-zc)/smpara
  a0= g+em
  a1=g-em
  r0= (sqrt(1.0 + a0**2.0) + a0)/2.0
  r1=(sqrt(1.+a1**2.0) + a1)/2.0
  gt=r0 -r1 -em
  zout=gt*smpara +zc
  return
end subroutine forward_

subroutine inverse_(zin,zout)
  implicit none
  real(r_kind),intent(in):: zin
  real(r_kind) :: zout
! local variable
  real (r_kind):: zc,em,gt
  real(r_kind) :: p,r,g
  real(r_kind) :: temp      ! apply inverse nltr to it
  real(r_kind) :: powerpinv
  real(r_kind) :: z1
  real(r_kind) :: scaling
  real(r_kind) :: scalinginv
  scaling=1.0
  scalinginv=1.0/scaling
  powerpinv=1.0/powerp

  zc=(zlow+zhigh)/2.0
  em=(zhigh-zlow)/(2.0*smpara)
  gt=(zin-zc)/smpara
  p=gt/em
  r=1.-p**2
  if (r>0.0) then
    g=p*sqrt(em**2 +1./r)
  elseif (p >=1.0) then
     g=1000000.0
  elseif (p <=-1.0) then
     g=-1000000.0
  endif
  temp=g*smpara + zc
!RY:  change z back to vis
  z1=(powerp*temp + 1.0) 
  z1=z1**powerpinv
  zout=z1*scalinginv
  return
end subroutine inverse_

end module nltrconfine
