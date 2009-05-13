subroutine get_tend_derivs(ut,vt,phit,ut_x,vt_y,fut_y,fvt_x,phit_lap,&
                 nlevs,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    get_tend_derivs     get horizontal derivs of tends
!   prgmmr: kleist          org: np20                date: 2006-02-24
!
! abstract: get various spatial derivatives of time tendencies
!
! program history log:
!   2006-02-24  kleist
!   2008-06-04  safford - rm unused vars and uses
!
!   input argument list:
!     ut       - zonal wind tendency
!     vt       - meridional wind tendency
!     phit     - mass variable time tendency
!     nlevs    - number of levels for each processer to compute derivs
!     mype     - integer task id
!
!   output argument list:
!     ut_x     - zonal derivative of zonal wind tendency
!     vt_y     - meridional derivative of meridional wind tendency
!     fut_y    - meridional derivative of coriolis*ut
!     fvt_x    - zonal derivative of coriolis*vt
!     phit_lap - laplacian of mass variable tendency
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: zero
  use gridmod, only: regional,nlat,nlon,lat2,lon2,nsig,nnnn1o
  use compact_diffs, only: compact_dlat,compact_dlon,compact_delsqr
  use mpimod, only: nvar_id
  use tendsmod, only: coriolis
  implicit none

! Passed variables
  integer(i_kind) mype
  integer(i_kind) nlevs

  real(r_kind),dimension(lat2,lon2,nsig),intent(in):: ut,vt,phit
  real(r_kind),dimension(lat2,lon2,nsig),intent(out):: ut_x,vt_y,fut_y,fvt_x,phit_lap

! Local Variables
  integer(i_kind) iflg,k,i,j
  real(r_kind),dimension(lat2,lon2):: dum1,dum2,dum3,dum4
  real(r_kind),dimension(lat2,lon2,nsig):: dum3d
  real(r_kind),dimension(nlat,nlon,nnnn1o):: hwork,hworkd
  logical vectflg

! ----------------------------------------- !
! Borrowing the mass mpi structure built in for all anl vars, here
! is now the nvarid is used:
! NVARID:     OLD            VS   NEW 
!    1: streamfunction          | ut_x
!    2: velocity potential      | vt_y
!    3: log surface pressure    | --
!    4: temperature             | phit_lap
!    5: q                       | fut_y
!    6: ozone                   | fvt_x
!    7: sea surface temperature | --
!    8: cloud water             | --
!    9: land skin temperature   | --
!   10: sfc ice temperature     | --
! ----------------------------------------- !

  iflg=1
  vectflg=.true.

  do k=1,nsig
    do j=1,lon2
      do i=1,lat2
        ut_x(i,j,k)=ut(i,j,k)
        vt_y(i,j,k)=vt(i,j,k)
        fut_y(i,j,k)=ut(i,j,k)*coriolis(i,j)
        fvt_x(i,j,k)=vt(i,j,k)*coriolis(i,j)
        phit_lap(i,j,k)=phit(i,j,k)
        dum3d(i,j,k)=zero
      end do
    end do
  end do
  do j=1,lon2
    do i=1,lat2
      dum1(i,j)=zero
      dum2(i,j)=zero
      dum3(i,j)=zero
      dum4(i,j)=zero
    end do
  end do


! here is the old/general calling sequence for comparison:
! call sub2grid(hwork,       t, lnps,    q,   oz, sst, slndt, sicet, cwmr,   st,  vp,iflg)
  hwork=zero
  call sub2grid(hwork,phit_lap, dum1,fut_y,fvt_x,dum2, dum3,  dum4, dum3d, ut_x,vt_y,iflg)
  do k=1,nnnn1o
    if(k.gt.nlevs) then
      do j=1,nlon
        do i=1,nlat
          hworkd(i,j,k)=zero
        end do
      end do
    else

! Laplacian of tendency of mass variable
      if (nvar_id(k).eq.4) then
        if (regional) then
          call get_delsqr_reg(hwork(1,1,k),hworkd(1,1,k))
        else
          call compact_delsqr(hwork(1,1,k),hworkd(1,1,k))
        end if
! x-derivative of utend and f*vtend
      else if ( (nvar_id(k).eq.1) .or. (nvar_id(k).eq.6)) then
        if (regional) then
          call get_dlon_reg(hwork(1,1,k),hworkd(1,1,k))
        else
          call compact_dlon(hwork(1,1,k),hworkd(1,1,k),vectflg)
        end if
! y-derivative of vtend and f*utend
      else if ( (nvar_id(k).eq.2) .or. (nvar_id(k).eq.5) ) then
        if (regional) then
          call get_dlat_reg(hwork(1,1,k),hworkd(1,1,k))
        else
          call compact_dlat(hwork(1,1,k),hworkd(1,1,k),vectflg)
        end if
! else just passing around dummy arrays
      else
        do j=1,nlon
          do i=1,nlat
            hworkd(i,j,k)=zero
          end do
        end do
      end if

    end if ! end if k-levs
  end do 

  call grid2sub(hworkd,phit_lap,dum1,fut_y,fvt_x,dum2,dum3, dum4, dum3d, ut_x,vt_y)
      
  return
end subroutine get_tend_derivs


subroutine tget_tend_derivs(ut,vt,phit,ut_x,vt_y,fut_y,fvt_x,phit_lap,&
                 nlevs,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    tget_tend_derivs     adjoint get horiz derivs of tends
!   prgmmr: kleist          org: np20                date: 2006-02-24
!
! abstract: adjoint of routine that gets various spatial derivatives of time tendencies
!
! program history log:
!   2006-02-24  kleist
!   2008-06-04  safford - rm unused vars and uses
!
!   input argument list:
!     ut_x     - zonal derivative of zonal wind tendency
!     vt_y     - meridional derivative of meridional wind tendency
!     fut_y    - meridional derivative of coriolis*ut
!     fvt_x    - zonal derivative of coriolis*vt
!     phit_lap - laplacian of mass variable tendency
!     ut       - zonal wind tendency
!     vt       - meridional wind tendency
!     phit     - mass variable time tendency
!     nlevs    - number of levels for each processer to compute derivs
!     mype     - integer task id
!
!   output argument list:
!     ut       - zonal wind tendency
!     vt       - meridional wind tendency
!     phit     - mass variable time tendency
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: zero
  use gridmod, only: regional,nlat,nlon,lat2,lon2,nsig,nnnn1o
  use compact_diffs, only: tcompact_dlat,tcompact_dlon,tcompact_delsqr
  use mpimod, only: nvar_id
  use tendsmod, only: coriolis
  implicit none

! Passed variables
  integer(i_kind) mype
  integer(i_kind) nlevs

  real(r_kind),dimension(lat2,lon2,nsig),intent(inout):: ut,vt,phit
  real(r_kind),dimension(lat2,lon2,nsig),intent(in):: ut_x,vt_y,fut_y,fvt_x,phit_lap

! Local Variables
  integer(i_kind) iflg,k,i,j
  real(r_kind),dimension(lat2,lon2):: dum1,dum2,dum3,dum4
  real(r_kind),dimension(lat2,lon2,nsig):: dum3d
  real(r_kind),dimension(nlat,nlon,nnnn1o):: hwork,hworkd
  logical vectflg

  iflg=1
  vectflg=.true.
!             initialize hwork to zero, so can accumulate contribution from
!             all derivatives
  hwork=zero
  hworkd=zero
!             for now zero out slndt,sicet
  dum1=zero
  dum2=zero
  dum3=zero
  dum4=zero
  dum3d=zero

  call sub2grid(hworkd,phit_lap,dum1,fut_y,fvt_x,dum2,dum3, dum4, dum3d, ut_x,vt_y,iflg)

  do k=1,nlevs
! adjoint of y-derivative of vtend and f*utend
    if ( (nvar_id(k).eq.2) .or. (nvar_id(k).eq.5) ) then
      if (regional) then
        call tget_dlat_reg(hworkd(1,1,k),hwork(1,1,k))
      else
        call tcompact_dlat(hwork(1,1,k),hworkd(1,1,k),vectflg)
      end if
! adjoint of x-derivative of utend and f*vtend
    else if ( (nvar_id(k).eq.1) .or. (nvar_id(k).eq.6)) then
      if (regional) then
        call tget_dlon_reg(hworkd(1,1,k),hwork(1,1,k))
      else
        call tcompact_dlon(hwork(1,1,k),hworkd(1,1,k),vectflg)
      end if
! Laplacian of tendency of mass variable
    else if (nvar_id(k).eq.4) then
      if (regional) then
        call tget_delsqr_reg(hworkd(1,1,k),hwork(1,1,k))
      else
        call tcompact_delsqr(hwork(1,1,k),hworkd(1,1,k))
      end if
    else
      do j=1,nlon
        do i=1,nlat
          hwork(i,j,k)=zero
        end do
      end do
    end if
  end do

  call grid2sub(hwork,phit_lap,dum1,fut_y,fvt_x,dum2,dum3, dum4, dum3d, ut_x,vt_y)

  do k=1,nsig
    do j=1,lon2
      do i=1,lat2
        phit(i,j,k)=phit(i,j,k)+phit_lap(i,j,k)
        ut(i,j,k)=ut(i,j,k) + ut_x(i,j,k) + fut_y(i,j,k)*coriolis(i,j)
        vt(i,j,k)=vt(i,j,k) + vt_y(i,j,k) + fvt_x(i,j,k)*coriolis(i,j)
      end do
    end do
  end do

end subroutine tget_tend_derivs
