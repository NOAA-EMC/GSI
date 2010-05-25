!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! NASA/GSFC, Global Modeling and Assimilation Office, 900.3, GEOS/DAS  !
!BOP -------------------------------------------------------------------
!
! !MODULE: m_berror_stats_reg - a module of berror_stats input
!
! !DESCRIPTION:
!
! !INTERFACE:

    module m_berror_stats_reg
      use kinds,only : i_kind,r_kind
      use constants, only: izero,ione,zero,one
      use gridmod, only: nsig

      implicit none

      private	! except

        ! reconfigurable parameters, via NAMELIST/setup/
      public :: berror_stats	! reconfigurable filename

        ! interfaces to file berror_stats.
      public :: berror_get_dims_reg	! get dimensions, jfunc::createj_func()
      public :: berror_read_bal_reg	! get cross-cov.stats., balmod::prebal()
      public :: berror_read_wgt_reg	! get auto-cov.stats., prewgt()


! !REVISION HISTORY:
!       25Feb10 - Zhu - adopt code format from m_berror_stats
!                     - extract from rdgstat_reg
!                     - change sructure of error file
!                     - make changes for generalized control variables
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname='m_berror_stats_reg'

  	! Reconfigurable parameters, vai NAMELISt/setup/
  character(len=256),save :: berror_stats = "berror_stats"	! filename

  integer(i_kind),parameter :: default_unit_ = 22_i_kind
  integer(i_kind),parameter :: ERRCODE=2_i_kind

  integer(i_kind),allocatable,dimension(:):: lsig
  real(r_kind),allocatable,dimension(:):: coef1,coef2

contains
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! NASA/GSFC, Global Modeling and Assimilation Office, 900.3, GEOS/DAS  !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: berror_get_dims_reg - get dimensions
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine berror_get_dims_reg(msig,mlat,unit)

      implicit none

      integer(i_kind)         ,intent(  out) :: msig  ! dimension of levels
      integer(i_kind)         ,intent(  out) :: mlat  ! dimension of latitudes
      integer(i_kind),optional,intent(in   ) :: unit  ! logical unit [22]

! !REVISION HISTORY:
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::berror_get_dims_reg'

  integer(i_kind) :: inerr

! Read dimension of stats file
  inerr=default_unit_
  if(present(unit)) inerr = unit
  open(inerr,file=berror_stats,form='unformatted',status='old')
  rewind inerr
  read(inerr) msig,mlat
  close(inerr)
end subroutine berror_get_dims_reg
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! NASA/GSFC, Global Modeling and Assimilation Office, 900.3, GEOS/DAS  !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: berror_read_bal_reg - get cross-corr. coefficients
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine berror_read_bal_reg(msig,mlat,agvi,bvi,wgvi,mype,unit)
      use kinds,only : r_single
      use gridmod,only : nlat,nlon
      use guess_grids, only:  ges_psfcavg,ges_prslavg

      implicit none

      integer(i_kind)                              ,intent(in   ) :: msig,mlat
      real(r_kind),dimension(0:mlat+ione,nsig,nsig),intent(  out) :: agvi
      real(r_kind),dimension(0:mlat+ione,nsig)     ,intent(  out) :: wgvi
      real(r_kind),dimension(0:mlat+ione,nsig)     ,intent(  out) :: bvi
      integer(i_kind)                              ,intent(in   ) :: mype  ! "my" processor ID
      integer(i_kind),optional                     ,intent(in   ) :: unit ! an alternative unit

! !REVISION HISTORY:
!       25Feb10 - Zhu  - extracted from rdgstat_reg
!                      - change the structure of error file
!                      - make changes for generalized control variables
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::berror_read_bal_reg'

!   workspaces/variables for data not returned

  integer(i_kind) k,i,m,n,j,m1,l1,l
  integer(i_kind):: nsigstat,nlatstat
  integer(i_kind):: inerr

  real(r_kind),dimension(nsig) :: rlsig
  real(r_single),dimension(:),allocatable::  clat_avn,sigma_avn
  real(r_single),dimension(:,:),allocatable::  bv_avn,wgv_avn
  real(r_single),dimension(:,:,:),allocatable:: agv_avn
  real(r_kind),dimension(:),allocatable::  rlsigo

!   Open background error statistics file
    inerr=default_unit_
    if(present(unit)) inerr=unit
    open(inerr,file=berror_stats,form='unformatted',status='old')

!   Read header. 
    rewind inerr
    read(inerr) nsigstat,nlatstat

    if(mype==izero) then
       write(6,*) myname_,'(PREBAL_REG):  get balance variables', &
         '"',trim(berror_stats),'".  ', &
         'mype,nsigstat,nlatstat =', &
          mype,nsigstat,nlatstat
    end if

    allocate ( clat_avn(mlat) )
    allocate ( sigma_avn(1:msig) )
    allocate ( rlsigo(1:msig) )
    allocate ( agv_avn(0:mlat+ione,1:msig,1:msig) )
    allocate ( bv_avn(0:mlat+ione,1:msig),wgv_avn(0:mlat+ione,1:msig) )

!   Read background error file to get balance variables
    read(inerr)clat_avn,(sigma_avn(k),k=1,msig)
    read(inerr)agv_avn,bv_avn,wgv_avn
    close(inerr)

!   compute vertical(pressure) interpolation index and weight
    do k=1,nsig
       rlsig(k)=log(ges_prslavg(k)/ges_psfcavg)
    enddo
    do k=1,msig
       rlsigo(k)=log(sigma_avn(k))
    enddo

    allocate(lsig(nsig),coef1(nsig),coef2(nsig))
    do k=1,nsig
       if(rlsig(k)>=rlsigo(1))then
          m=ione
          m1=2_i_kind
          lsig(k)=ione
          coef1(k)=one
          coef2(k)=zero
       else if(rlsig(k)<=rlsigo(msig))then
          m=msig-ione
          m1=msig
          lsig(k)=msig-ione
          coef1(k)=zero
          coef2(k)=one
       else
          m_loop: do m=1,msig-ione
             m1=m+ione
             if((rlsig(k)<=rlsigo(m))   .and.  &
                  (rlsig(k)>rlsigo(m1))     )then
                lsig(k)=m
                exit m_loop
             end if
          enddo m_loop
          coef1(k)=(rlsigo(m1)-rlsig(k))/(rlsigo(m1)-rlsigo(m))
          coef2(k)=one-coef1(k)
          if(lsig(k)==msig)then
             lsig(k)=msig-ione
             coef2(k)=one
             coef1(k)=zero
          endif
       endif
    end do

!   Load agv wgv bv
    do k=1,nsig
       m=lsig(k)
       m1=m+ione
       do i=1,mlat
          wgvi(i,k)=wgv_avn(i,m)*coef1(k)+wgv_avn(i,m1)*coef2(k)
          bvi (i,k)=bv_avn (i,m)*coef1(k)+bv_avn (i,m1)*coef2(k)
       enddo

       do j=1,nsig
          l=lsig(j)
          l1=l+ione
          do i=1,mlat
             agvi(i,j,k)=(agv_avn(i,l,m)*coef1(j)+agv_avn(i,l1,m)*coef2(j))*coef1(k) &
                      +(agv_avn(i,l,m1)*coef1(j)+agv_avn(i,l1,m1)*coef2(j))*coef2(k)
          enddo
       enddo
    enddo

    agvi(0,:,:)=agvi(1,:,:)
    wgvi(0,:)=wgvi(1,:)
    bvi(0,:)=bvi(1,:)
    agvi(mlat+ione,:,:)=agvi(mlat,:,:)
    wgvi(mlat+ione,:)=wgvi(mlat,:)
    bvi(mlat+ione,:)=bvi(mlat,:)
     
    deallocate (agv_avn,bv_avn,wgv_avn,clat_avn,sigma_avn,rlsigo)
    return
end subroutine berror_read_bal_reg
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! NASA/GSFC, Global Modeling and Assimilation Office, 900.3, GEOS/DAS  !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: berror_read_wgt_reg - read auto-corr. coeffs.
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine berror_read_wgt_reg(msig,mlat,corz,corp,hwll,hwllp,vz,rlsig,mype,unit)

      use kinds,only : r_single,r_kind
      use gridmod,only : nlat,nlon,nsig
      use control_vectors,only: nrf,nrf2,nrf3,nrf_var,nrf2_loc,nrf3_loc, &
                      nrf3_oz,nrf3_q,nrf3_cw,nrf3_sf,nrf2_sst,nvars
      use jfunc,only: varq,qoption
      use guess_grids, only:  ges_psfcavg,ges_prslavg
      use constants, only: ione,zero,one

      implicit none

      integer(i_kind)                    ,intent(in   ) :: msig,mlat
      integer(i_kind)                    ,intent(in   ) :: mype  ! "my" processor ID
      integer(i_kind),optional           ,intent(in   ) :: unit ! an alternative unit

      real(r_kind),dimension(1:mlat,nsig,nrf3),intent(inout):: corz
      real(r_kind),dimension(1:mlat,nrf2),intent(out):: corp

      real(r_kind),dimension(0:mlat+1,nsig,nrf3),intent(out):: hwll
      real(r_kind),dimension(0:mlat+1,nvars-nrf3),intent(out):: hwllp
      real(r_kind),dimension(nsig,0:mlat+1,1:nrf3),intent(out):: vz
     
      real(r_kind),dimension(nsig),intent(out):: rlsig

! !REVISION HISTORY:
!       25Feb10 - Zhu - extract from rdgstat_reg
!                     - change the structure of background error file
!                     - make changes for generalizing control variables
!                     - move varq,factoz here from prewgt_reg
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::berror_read_wgt_reg'

  real(r_kind),parameter:: r25      = one/25.0_r_kind
  real(r_kind),parameter:: zero_3   = 0.3_r_kind
  real(r_kind),parameter:: vz_oz    = 0.53333333_r_kind

!  workspace variables not returned
  real(r_single),dimension(:),allocatable::  clat_avn,sigma_avn
  real(r_single),dimension(:,:),allocatable::  bv_avn,wgv_avn,corqq_avn
  real(r_single),dimension(:,:,:),allocatable:: agv_avn
  real(r_single),dimension(:,:),allocatable:: corz_avn,hwll_avn,vztdq_avn

  real(r_single),dimension(1:mlat,msig,nrf):: corz_tmp
  real(r_single),dimension(0:mlat+1,msig,nrf):: hwll_tmp
  real(r_single),dimension(msig,0:mlat+1,nrf):: vz_tmp


  character*5 var
  logical,dimension(nrf):: nrf_err

  integer(i_kind) :: inerr,istat
  integer(i_kind) :: nsigstat,nlatstat,isig
  integer(i_kind) :: loc,nn,m1,m,i,n,j,k
  real(r_kind) :: corq2x
  real(r_kind) :: factoz

  allocate ( clat_avn(mlat) )
  allocate ( sigma_avn(1:msig) )
  allocate ( agv_avn(0:mlat+ione,1:msig,1:msig) )
  allocate ( bv_avn(0:mlat+ione,1:msig),wgv_avn(0:mlat+ione,1:msig) )

! Open background error statistics file
  inerr=default_unit_
  if(present(unit)) inerr=unit
  open(inerr,file=berror_stats,form='unformatted',status='old')

! Read header.
  rewind inerr
  read(inerr) nsigstat,nlatstat

! Read background error file to get balance variables
  read(inerr)clat_avn,(sigma_avn(k),k=1,msig)
  read(inerr)agv_avn,bv_avn,wgv_avn

! compute vertical(pressure) interpolation index and weight
  do k=1,nsig
     rlsig(k)=log(ges_prslavg(k)/ges_psfcavg)
  enddo

  if(mype==izero) then
     write(6,*) myname_,'(PREWGT_REG):  read error amplitudes ', &
       '"',trim(berror_stats),'".  ', &
       'mype,nsigstat,nlatstat =', &
        mype,nsigstat,nlatstat
  end if

! Read amplitudes
  nrf_err=.false.
  read: do
     read(inerr,iostat=istat) var, isig
     if (istat /= 0) exit
     allocate ( corz_avn(1:mlat,1:isig) )
     allocate ( hwll_avn(0:mlat+1,1:isig) )
     allocate ( vztdq_avn(1:isig,0:mlat+1) )

     if (var/='q') then
        read(inerr) corz_avn
     else
        allocate ( corqq_avn(1:mlat,1:isig) )
        read(inerr) corz_avn,corqq_avn
     end if

     read(inerr) hwll_avn
     if (isig>1) then
        read(inerr) vztdq_avn
     end if

!    load the variances
     do n=1,nrf
        if (var==nrf_var(n)) then
           nrf_err(n)=.true.
           loc=n
           exit
        end if
     end do

     if (isig==msig) then
        do n=1,nrf3
           if (nrf3_loc(n)==loc) then
              if (var=='q' .and. qoption==2) then
!                choose which q stat to use
                 do k=1,msig
                    do i=1,mlat
                       corz_tmp(i,k,n)=corqq_avn(i,k)
                    end do
                 end do
              else
                 do k=1,msig
                    do i=1,mlat
                       corz_tmp(i,k,n)=corz_avn(i,k)
                    end do
                 end do
              end if
              do k=1,msig
                 do i=0,mlat+ione
                    hwll_tmp(i,k,n)=hwll_avn(i,k)
                    vz_tmp(k,i,n)=vztdq_avn(k,i)
                 end do
              end do
              exit
           end if
        end do
     end if

     if (isig==1) then
       do n=1,nrf2
          if (nrf2_loc(n)==loc) then
             do i=1,mlat
                 corp(i,n)=corz_avn(i,1)
                 hwllp(i,n)=hwll_avn(i,1)
             end do
             hwllp(0,n)=hwll_avn(0,1)
             hwllp(mlat+1,n)=hwll_avn(mlat+1,1)
             exit
          end if
       end do
     end if

     deallocate ( corz_avn )
     deallocate ( hwll_avn )
     deallocate ( vztdq_avn )
     if (var=='q') deallocate ( corqq_avn )
  enddo read
  close(inerr)

  deallocate(clat_avn,sigma_avn)
  deallocate(agv_avn,bv_avn,wgv_avn)

! 3d variable
  do n=1,nrf3
     loc=nrf3_loc(n)
     if (nrf_err(loc)) then
        do k=1,nsig
           m=lsig(k)
           m1=m+1
           do i=1,mlat
              corz(i,k,n)=corz_tmp(i,m,n)*coef1(k)+corz_tmp(i,m1,n)*coef2(k)
           enddo

           do i=0,mlat+ione
              hwll(i,k,n)=hwll_tmp(i,m,n)*coef1(k)+hwll_tmp(i,m1,n)*coef2(k)
              vz(k,i,n)=vz_tmp(m,i,n)*coef1(k)+vz_tmp(m1,i,n)*coef2(k)
           enddo
        enddo
     end if
  enddo

  if(nrf3_q>0 .and. qoption==2)then
     do k=1,nsig
        do j=1,mlat
           varq(j,k)=min(max(real(corz(j,k,nrf3_q),r_kind),0.0015_r_kind),one)
           corz(j,k,nrf3_q)=one
        enddo
     enddo
  endif

  do n=1,nrf3
     loc=nrf3_loc(n)
     if (nrf_err(loc)) cycle
     if (n==nrf3_oz) then
        factoz = 0.0002_r_kind*r25
        corz(:,:,nrf3_oz)=factoz
!       hwll(:,:,nrf3_oz)=400000.0_r_kind
        vz(:,:,nrf3_oz)=vz_oz
        nrf_err(loc)=.true.
     else if (n==nrf3_cw) then
        corz(:,:,nrf3_cw)=corz(:,:,nrf3_q)
        hwll(:,:,nrf3_cw)=hwll(:,:,nrf3_q)
        vz(:,:,nrf3_cw)=vz(:,:,nrf3_q)
        nrf_err(loc)=.true.
     end if
  end do

! 2d variable
  do n=1,nrf2
     loc=nrf2_loc(n)
     if (nrf_err(loc)) cycle
     if (n==nrf2_sst) then
        do i=1,mlat
           corp(i,n)=zero_3
        end do
        do i=0,mlat+1
           hwllp(i,n)=hwll(i,1,nrf3_sf)
           hwllp(i,nrf2+1)=hwll(i,1,nrf3_sf)
           hwllp(i,nrf2+2)=hwll(i,1,nrf3_sf)
        end do
        nrf_err(loc)=.true.
     end if
  enddo

! Do final check to make sure that background errors have been loaded for all variables
  if(mype==0) then
     do n=1,nrf
        if (.not. nrf_err(n)) then
           write(6,*) 'READ_WGT_REG: ***ERROR*** fail to load error variance for ', nrf_var(n)
           call stop2(333)
        end if
     end do
  end if


  return
end subroutine berror_read_wgt_reg

end module m_berror_stats_reg
