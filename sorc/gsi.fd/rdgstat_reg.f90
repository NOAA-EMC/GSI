subroutine rdgstat_reg(msig,mlat,inerr,&
     hwll,hwllp,vz,agvi,bvi,wgvi,corz,corp,rlsig)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    rdgstat_reg   read in and setup berror               
!   prgmmr: wu               org: np22                date: 2000-03-15
!
! abstract: read in background error stats and interpolate in vertical 
!
! program history log:
!   2000-03-15  wu         
!   2004-08-03  treadon - add only to module use, add intent in/out
!   2005-01-21  parrish - correct indexing error (affects corz, vz, hwll)
!   2005-02-07  treadon - add deallocate rsigo
!   2005-03-28  wu - change dim of variance arrays and loop index
!   2005-04-22  treadon - change berror file to 4-byte reals
!   2005-04_27  wu - read in stats for both qoption=1 and 2
!   2006-04-17  treadon - include rlsig as subroutine output
!
!   input argument list:
!     msig     - number of sigma levels of stats
!     mlat     - number of y grid      
!     sigl     - mean grid sigma     
!     inerr    - unit number for input statistics
!
!   output argument list:
!   agvi,wgvi,bvi - balance correlation matrix for t,p,velocity potential
!   hwll,hwllp    - horizontal scale for the error covariance
!     vz          - vertical scale for the error covariance
!   corz,corp     - variance for the error covariance
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!$$$
  use kinds, only: r_kind,i_kind,r_single
  use constants, only: zero,one_tenth,one
  use gridmod, only: nsig
  use guess_grids, only:  ges_psfcavg,ges_prslavg
  use jfunc, only: qoption
  implicit none

! Declare passed variables
  integer(i_kind),intent(in):: msig,mlat,inerr
  real(r_kind),dimension(0:mlat+1,nsig,nsig),intent(out):: agvi
  real(r_kind),dimension(0:mlat+1,nsig),intent(out):: wgvi
  real(r_kind),dimension(0:mlat+1,nsig),intent(out):: bvi
  real(r_kind),dimension(nsig,0:mlat+1,1:6),intent(out):: vz
  real(r_kind),dimension(0:mlat+1,nsig,4),intent(out):: hwll
  real(r_kind),dimension(0:mlat+1),intent(out):: hwllp
  real(r_kind),dimension(1:mlat,nsig,4),intent(inout):: corz
  real(r_kind),dimension(1:mlat),intent(out):: corp
  real(r_kind),dimension(nsig),intent(out):: rlsig

! Declare local parameters
  real(r_kind),parameter:: r101_324 = 101.324_r_kind
  real(r_kind),parameter:: r1013_24 = 1013.24_r_kind
  real(r_kind),parameter:: vz_oz    = 0.53333333_r_kind

! Declare local variables
  integer(i_kind) k,i,iv,m,n,j,m1,l1,l
  integer(i_kind) lsig(nsig)
  real(r_kind),dimension(nsig):: coef1,coef2
  real(r_kind) ps0
  real(r_kind),dimension(:),allocatable::  rlsigo

  real(r_single),dimension(:),allocatable::  clat_avn,sigma_avn,corp_avn,hwllp_avn
  real(r_single),dimension(:,:),allocatable::  bv_avn,wgv_avn,corqq_avn
  real(r_single),dimension(:,:,:),allocatable:: corz_avn,hwll_avn,&
       vztdq_avn,agv_avn

  allocate ( corz_avn(1:mlat,1:msig,1:4) )
  allocate ( corqq_avn(1:mlat,1:msig) )
  allocate ( sigma_avn(1:msig) )
  allocate ( rlsigo(1:msig) )
  allocate ( hwll_avn(0:mlat+1,1:msig,1:4) )
  allocate ( vztdq_avn(1:msig,0:mlat+1,1:4) )
  allocate ( agv_avn(0:mlat+1,1:msig,1:msig) )
  allocate ( bv_avn(0:mlat+1,1:msig),wgv_avn(0:mlat+1,1:msig) )
  allocate ( clat_avn(mlat),corp_avn(mlat),hwllp_avn(0:mlat+1) )
  

! Read in berror 
  rewind inerr
  read(inerr)
  
  read(inerr)clat_avn,(sigma_avn(k),k=1,msig)
  read(inerr)corz_avn,corp_avn,corqq_avn
  read(inerr)hwll_avn,hwllp_avn
  read(inerr)vztdq_avn
  read(inerr)agv_avn,bv_avn,wgv_avn

! choose which q stats to use
  if(qoption==2)then
     do k=1,msig
        do j=1,mlat
           corz_avn(j,k,4)=corqq_avn(j,k)
        enddo
     enddo
  endif

  do i=1,mlat
     corp(i)=corp_avn(i)
     hwllp(i)=hwllp_avn(i)
  end do
  hwllp(0)=hwllp_avn(0)
  hwllp(mlat+1)=hwllp_avn(mlat+1)

! compute vertical(pressure) interpolation index and weight
  do k=1,nsig
     rlsig(k)=log(ges_prslavg(k)/ges_psfcavg)
  enddo
  do k=1,msig
     rlsigo(k)=log(sigma_avn(k))
  enddo
  
  do k=1,nsig
     if(rlsig(k).ge.rlsigo(1))then
        m=1
        m1=2
        lsig(k)=1
        coef1(k)=one
        coef2(k)=zero
     else if(rlsig(k).le.rlsigo(msig))then
        m=msig-1
        m1=msig
        lsig(k)=msig-1
        coef1(k)=zero
        coef2(k)=one
     else
        m_loop: do m=1,msig-1
           m1=m+1
           if((rlsig(k).le.rlsigo(m))   .and.  &
                (rlsig(k).gt.rlsigo(m1))     )then
              lsig(k)=m
              exit m_loop
           end if
        enddo m_loop
        coef1(k)=(rlsigo(m1)-rlsig(k))/(rlsigo(m1)-rlsigo(m))
        coef2(k)=one-coef1(k)
        if(lsig(k)==msig)then
           lsig(k)=msig-1
           coef2(k)=one
           coef1(k)=zero
        endif
     endif
  end do

! Load agv wgv bv
  do k=1,nsig
     m=lsig(k)
     m1=m+1
     do i=1,mlat
        wgvi(i,k)=wgv_avn(i,m)*coef1(k)+wgv_avn(i,m1)*coef2(k)
        bvi(i,k)=bv_avn(i,m)*coef1(k)+bv_avn(i,m1)*coef2(k)
     enddo
     
     do j=1,nsig
        l=lsig(j)
        l1=l+1
        do i=1,mlat
           agvi(i,j,k)=(agv_avn(i,l,m)*coef1(j)+agv_avn(i,l1,m)*coef2(j))*coef1(k) &
                +(agv_avn(i,l,m1)*coef1(j)+agv_avn(i,l1,m1)*coef2(j))*coef2(k)
        enddo
     enddo
  enddo

  agvi(0,:,:)=agvi(1,:,:)
  wgvi(0,:)=wgvi(1,:)
  bvi(0,:)=bvi(1,:)
  agvi(mlat+1,:,:)=agvi(mlat,:,:)
  wgvi(mlat+1,:)=wgvi(mlat,:)
  bvi(mlat+1,:)=bvi(mlat,:)
!--------------------------------------------------------------------

  do n=1,4
     do k=1,nsig
        m=lsig(k)
        m1=m+1
        do i=1,mlat
           corz(i,k,n)=corz_avn(i,m,n)*coef1(k)+corz_avn(i,m1,n)*coef2(k)
        enddo
     enddo
  enddo
  
  do n=1,4
     do k=1,nsig
        m=lsig(k)
        m1=m+1
        do i=0,mlat+1   
           hwll(i,k,n)=hwll_avn(i,m,n)*coef1(k)+hwll_avn(i,m1,n)*coef2(k)
           vz(k,i,n)=vztdq_avn(m,i,n)*coef1(k)+vztdq_avn(m1,i,n)*coef2(k)
        enddo
     enddo
  enddo

  vz(:,:,5)=vz_oz
  vz(:,:,6)=vz(:,:,4)

  deallocate ( corz_avn,corqq_avn,sigma_avn,rlsigo,hwll_avn,vztdq_avn,&
       agv_avn,bv_avn,wgv_avn,clat_avn,corp_avn,hwllp_avn)
  return
end subroutine rdgstat_reg
