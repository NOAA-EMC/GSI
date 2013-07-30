subroutine smooth_tends(u,v,t,q,oz,cw,ps,nlevs,mype)
!
! prgram history log:
!    2009-10-1  kleist - define filtering of tendencies
!    2010-9-30  rancic - start modifying code with new coding infrastructure
!
!    [At this stage, the idea is to exploit existing coding structure and 
!    quickly generate some results.  To this end, sub2grid2, etc., will be 
!    used in smoothing of all fields.  At some later stage, the new sub2grid, 
!    etc., using bundle structure will be applied. - Rancic, Jan 2011]
!    [Also, note that here we use surface pressure instead of 3d preasure.
!    This also may be changed later on - but for now hydrostatic modeling
!    paradigm should be more efficient (once the bundle is applied). - Rancic, Jan 2011]
!
  use kinds, only: r_kind,i_kind
  use constants, only: zero
  use gridmod, only: nlat,nlon,lat2,lon2,nsig,sp_a,grd_a
  use mpimod, only: nnnvsbal

  implicit none

! Passed variables
  integer(i_kind),intent(in):: mype
  integer(i_kind),intent(in):: nlevs
  real(r_kind),dimension(lat2,lon2),intent(inout):: ps
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout):: t,q,cw,oz,u,v

! Local Variables
  real(r_kind),dimension(lat2,lon2,nsig+1):: pri_dum
  real(r_kind),dimension(lat2,lon2):: t_dum
  real(r_kind),dimension(nlat,nlon,nnnvsbal):: hwork
  real(r_kind),dimension(sp_a%nc):: spc1
  integer(i_kind) iflg,k,i,j

  iflg=1
!
! First pass (smooth tendencies of balanced variables)
!
  hwork=zero
  pri_dum(:,:,1:nsig)=q(:,:,1:nsig)
  pri_dum(:,:,nsig+1)=ps(:,:)

! Put tendencies on grid
  call sub2grid2(hwork,u,v,pri_dum,t,iflg)

! Transform to spectral coefficients
  do k=1,nnnvsbal
    call general_g2s0(grd_a,sp_a,spc1,hwork(1,1,k))

! Truncate
    call general_jcaptrans(sp_a,spc1)

! Back to the grid
    call general_s2g0(grd_a,sp_a,spc1,hwork(1,1,k))

  end do  !End do k

! Put back on subdomains
  call grid2sub2(hwork,u,v,pri_dum,t)

  q(:,:,1:nsig)=pri_dum(:,:,1:nsig)
  ps(:,:)=pri_dum(:,:,nsig+1)

!
! Second pass (smooth tendencies of remaining variables)
!
  hwork=zero
  pri_dum=zero
  t_dum=zero

! Put tendencies on grid
  call sub2grid2(hwork,oz,cw,pri_dum,t_dum,iflg)

! Transform to spectral coefficients
  do k=1,nnnvsbal
    call general_g2s0(grd_a,sp_a,spc1,hwork(1,1,k))

! Truncate
    call general_jcaptrans(sp_a,spc1)

! Back to the grid
    call general_s2g0(grd_a,sp_a,spc1,hwork(1,1,k))

  end do  !End do k

! Put back on subdomains
  call grid2sub2(hwork,oz,cw,pri_dum,t_dum)

  return
end subroutine smooth_tends

subroutine smooth_tends_ad(u,v,t,q,oz,cw,ps,nlevs,mype)
!
! prgram history log:
!    2009-10-1  kleist - define filtering of tendencies
!    2010-9-30  rancic - start modifying code with new coding infrastructure
!
  use kinds, only: r_kind,i_kind
  use constants, only: zero
  use gridmod, only: nlat,nlon,lat2,lon2,nsig,sp_a,grd_a
  use mpimod, only: nnnvsbal

  implicit none

! Passed variables
  integer(i_kind),intent(in):: mype
  integer(i_kind),intent(in):: nlevs
  real(r_kind),dimension(lat2,lon2),intent(inout):: ps
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout):: t,q,cw,oz,u,v

! Local Variables
  real(r_kind),dimension(lat2,lon2,nsig+1):: pri_dum
  real(r_kind),dimension(lat2,lon2):: t_dum
  real(r_kind),dimension(nlat,nlon,nnnvsbal):: hwork
  real(r_kind),dimension(sp_a%nc):: spc1
  integer(i_kind) iflg,k,i,j

  iflg=1
!
! First pass (smooth tendencies of balanced variables)
!
  hwork=zero
  pri_dum(:,:,1:nsig)=q(:,:,1:nsig)
  pri_dum(:,:,nsig+1)=ps(:,:)

! Put tendencies on grid
  call sub2grid2(hwork,u,v,pri_dum,t,iflg)

! Transform to spectral coefficients
  do k=1,nnnvsbal
    call general_s2g0_ad(grd_a,sp_a,spc1,hwork(1,1,k))

! Truncate
    call general_jcaptrans_ad(sp_a,spc1)

! Back to the grid
    call general_g2s0_ad(grd_a,sp_a,spc1,hwork(1,1,k))

  end do  !End do k

! Put back on subdomains
  call grid2sub2(hwork,u,v,pri_dum,t)

  q(:,:,1:nsig)=pri_dum(:,:,1:nsig)
  ps(:,:)=pri_dum(:,:,nsig+1)

!
! Second pass (smooth tendencies of remaining variables)
!
  hwork=zero
  pri_dum=zero
  t_dum=zero

! Put tendencies on grid
  call sub2grid2(hwork,oz,cw,pri_dum,t_dum,iflg)

! Transform to spectral coefficients
  do k=1,nnnvsbal
    call general_s2g0_ad(grd_a,sp_a,spc1,hwork(1,1,k))

! Truncate
    call general_jcaptrans_ad(sp_a,spc1)

! Back to the grid
    call general_g2s0_ad(grd_a,sp_a,spc1,hwork(1,1,k))

  end do  !End do k

! Put back on subdomains
  call grid2sub2(hwork,oz,cw,pri_dum,t_dum)

  return
end subroutine smooth_tends_ad

subroutine general_jcaptrans(sp,z)
  use kinds, only: r_kind
  use constants, only: zero
  use general_specmod, only: spec_vars
  implicit none

  type(spec_vars),intent(in   ) :: sp
  real(r_kind),dimension(sp%nc),intent(inout) :: z
  real(r_kind),dimension(sp%nc_trunc):: z_trunc
  integer j,l,n,ks1,ks2

! First load temp array
  do l=0,sp%jcap_trunc
    do n=l,sp%jcap_trunc
      ks2=l*(2*sp%jcap_trunc+(-1)*(l-1))+2*n
      if (l.LE.sp%jcap.AND.n.LE.sp%jcap) then
        ks1=l*(2*sp%jcap+(-1)*(l-1))+2*n
          z_trunc(ks2+1)=z(ks1+1)
          z_trunc(ks2+2)=z(ks1+2)
        else
          z_trunc(ks2+1)=zero
          z_trunc(ks2+2)=zero
        endif
     end do
  end do

! Now put temp array into full truncation and pad
  do l=0,sp%jcap
    do n=l,sp%jcap
      ks2=l*(2*sp%jcap+(-1)*(l-1))+2*n
      if (l.LE.sp%jcap_trunc.AND.n.LE.sp%jcap_trunc) then
        ks1=l*(2*sp%jcap_trunc+(-1)*(l-1))+2*n
          z(ks2+1)=z_trunc(ks1+1)
          z(ks2+2)=z_trunc(ks1+2)
        else
          z(ks2+1)=zero
          z(ks2+2)=zero
        endif
     end do
  end do

  return
end subroutine general_jcaptrans

subroutine general_jcaptrans_ad(sp,z)
  use kinds, only: r_kind
  use constants, only: zero
  use general_specmod, only: spec_vars
  implicit none

  type(spec_vars),intent(in   ) :: sp
  real(r_kind),dimension(sp%nc),intent(inout) :: z
  real(r_kind),dimension(sp%nc_trunc):: z_trunc
  integer j,l,n,ks1,ks2

  z_trunc=zero
! Now put temp array into full truncation and pad
  do l=0,sp%jcap
    do n=l,sp%jcap
      ks2=l*(2*sp%jcap+(-1)*(l-1))+2*n
      if (l.LE.sp%jcap_trunc.AND.n.LE.sp%jcap_trunc) then
        ks1=l*(2*sp%jcap_trunc+(-1)*(l-1))+2*n
          z_trunc(ks1+1)=z_trunc(ks1+1)+z(ks2+1)
          z_trunc(ks1+2)=z_trunc(ks1+2)+z(ks2+2)
        else
          z_trunc(ks2+1)=zero
          z_trunc(ks2+2)=zero
        endif
     end do
  end do

  z=zero
! First load temp array
  do l=0,sp%jcap_trunc
    do n=l,sp%jcap_trunc
      ks2=l*(2*sp%jcap_trunc+(-1)*(l-1))+2*n
      if (l.LE.sp%jcap.AND.n.LE.sp%jcap) then
        ks1=l*(2*sp%jcap+(-1)*(l-1))+2*n
          z(ks1+1)=z(ks1+1)+z_trunc(ks2+1)
          z(ks1+2)=z(ks1+2)+z_trunc(ks2+2)
        else
          z(ks1+1)=zero
          z(ks2+2)=zero
        endif
     end do
  end do

  return
end subroutine general_jcaptrans_ad
