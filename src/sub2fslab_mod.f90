module sub2fslab_mod
!$$$   module documentation block
!                .      .    .                                       .
! module:    sub2fslab_mod
!   prgmmr: sato             org: np23                date: 2008-01-23
!
! abstract: contains stuffs to convert the subdomain data
!           into filter-space slab data.
!
! program history log:
!   2008-01-23  sato
!
! subroutines included:
!   sub setup_sub2fslab
!   sub destroy_sub2fslab
!   sub sub2fslab
!   sub sub2fslabdz
!   sub sub2slab2d
!
! Variable Definitions:
!   var work_*  - work array for sub2grid
!   var hfine   - work array for agrid2fgrid/grd2patch input
!   var hfilter - work array for agrid2fgrid output (for regional)
!   var hflt?   - work array for grd2patch   output (for global)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  use kinds, only: r_kind,i_kind,r_single,r_double,i_long
  use gridmod, only: nsig1o, nlon, nlat, & ! for slab mode
                     nsig  , lat2, lon2, & ! for subdomain
                     regional, twodvar_regional
  use anberror, only: prm0 => pf2aP1, &
                      prm2 => pf2aP2,  &
                      prm3 => pf2aP3

  implicit none

  real(r_kind),allocatable,dimension(:,:,:)::work_st,work_vp,work_t,work_q, &
                                             work_oz,work_cwmr
  real(r_kind),allocatable,dimension(:,:)  ::work_p,work_sst,work_slndt,work_sicet

  real(r_kind)  ,allocatable,dimension(:,:,:)  :: hfine
  real(r_kind)  ,allocatable,dimension(:,:)    :: hfilter
  real(r_kind)  ,allocatable,dimension(:,:)    :: hflt0,hflt2,hflt3

contains
!=======================================================================
subroutine setup_sub2fslab
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:   setup_sub2fslab
!   prgmmr: sato             org: np23                date: 2008-01-23
!
! abstract: set up work arrays
!
! program history log:
!   2008-01-23  sato
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  allocate(work_st(lat2,lon2,nsig),work_vp(lat2,lon2,nsig),work_t   (lat2,lon2,nsig))
  allocate(work_q (lat2,lon2,nsig),work_oz(lat2,lon2,nsig),work_cwmr(lat2,lon2,nsig))
  allocate(work_p    (lat2,lon2),work_sst  (lat2,lon2))
  allocate(work_slndt(lat2,lon2),work_sicet(lat2,lon2))

  allocate(hfine(nlat,nlon,nsig1o))

  if( regional ) then
  ! for regional mode
    allocate(hfilter(prm0%nlatf,prm0%nlonf))
  else
  ! for global mode
    allocate(hflt0(prm0%nlatf,prm0%nlonf))
    allocate(hflt2(prm2%nlatf,prm2%nlonf))
    allocate(hflt3(prm3%nlatf,prm3%nlonf))
  end if

end subroutine setup_sub2fslab
!=======================================================================
!=======================================================================
subroutine destroy_sub2fslab
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:   destroy_sub2fslab
!   prgmmr: sato             org: np23                date: 2008-01-23
!
! abstract: destroy work arrays
!
! program history log:
!   2008-01-23  sato
!
!   input argument list:
!
!   output argument list:
!
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  deallocate(work_st, work_vp, work_t, work_q, work_oz, work_cwmr)
  deallocate(work_p, work_sst, work_slndt, work_sicet)

  if( regional ) then
  ! for regional mode
    deallocate(hfilter)
  else
  ! for global mode
    deallocate(hflt0)
    deallocate(hflt2)
    deallocate(hflt3)
  end if

  deallocate(hfine)

end subroutine destroy_sub2fslab
!=======================================================================
!=======================================================================
subroutine sub2fslab(fsub,fslab)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:   sub2fslab
!   prgmmr: sato             org: np23                date: 2008-01-23
!
! abstract: convert subdomain data into filter-space slab data
!
! program history log:
!   2008-01-23  sato
!
!   input argument list:
!    parm     - filter-space information
!    fsub     - subdomain data array
!
!   output argument list:
!    fslab    - filter-space data array
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block\
  use fgrid2agrid_mod, only: agrid2fgrid
! Declare passed variables
  real(r_kind)  ,intent(in) ::fsub(lat2,lon2,nsig)
  real(r_single),intent(out)::fslab(prm0%nlatf,prm0%nlonf,nsig1o)

! Declare local variables
  integer(i_kind):: k,iflg

  work_t =fsub; work_st=fsub; work_vp  =fsub
  work_q =fsub; work_oz=fsub; work_cwmr=fsub
  work_p    (:,:)=fsub(:,:,1)
  work_sst  (:,:)=fsub(:,:,1)
  work_slndt(:,:)=fsub(:,:,1)
  work_sicet(:,:)=fsub(:,:,1)

  iflg=1
  call sub2grid(hfine,work_t,work_p,work_q,work_oz,work_sst, &
                work_slndt,work_sicet,work_cwmr,work_st,work_vp,iflg)

  do k=1,nsig1o
    call agrid2fgrid(prm0,hfine(1,1,k),hfilter) !analysis to filter grid
    fslab(:,:,k)=real(hfilter(:,:),r_single)
  end do

  return
end subroutine sub2fslab
!=======================================================================
!=======================================================================
subroutine sub2fslab_glb(fsub,fslb0,fslb2,fslb3)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:   sub2fslab
!   prgmmr: sato             org: np23                date: 2008-01-23
!
! abstract: convert subdomain data into filter-space slab data
!           for global mode
!
! program history log:
!   2008-01-23  sato
!
!   input argument list:
!    parm     - filter-space information
!    fsub     - subdomain data array
!
!   output argument list:
!    fslab    - filter-space data array
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  use patch2grid_mod, only: grid2patch

! Declare passed variables
  real(r_kind)  ,intent(in) ::fsub(lat2,lon2,nsig)
  real(r_single),intent(out)::fslb0(prm0%nlatf,prm0%nlonf,nsig1o)
  real(r_single),intent(out)::fslb2(prm2%nlatf,prm2%nlonf,nsig1o)
  real(r_single),intent(out)::fslb3(prm3%nlatf,prm3%nlonf,nsig1o)

! Declare local variables
  integer(i_kind):: k,iflg

  work_t =fsub; work_st=fsub; work_vp  =fsub
  work_q =fsub; work_oz=fsub; work_cwmr=fsub
  work_p    (:,:)=fsub(:,:,1)
  work_sst  (:,:)=fsub(:,:,1)
  work_slndt(:,:)=fsub(:,:,1)
  work_sicet(:,:)=fsub(:,:,1)

  iflg=1
  call sub2grid(hfine,work_t,work_p,work_q,work_oz,work_sst, &
                work_slndt,work_sicet,work_cwmr,work_st,work_vp,iflg)

  do k=1,nsig1o
    call grid2patch(hfine(1,1,k),hflt0,hflt2,hflt3) !analysis to filter grid
    fslb0(:,:,k)=real(hflt0(:,:),r_single)
    fslb2(:,:,k)=real(hflt2(:,:),r_single)
    fslb3(:,:,k)=real(hflt3(:,:),r_single)
  end do

  return
end subroutine sub2fslab_glb
!=======================================================================
!=======================================================================
subroutine sub2fslabdz(fsub,fslab)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:   sub2fslabdz
!   prgmmr: sato             org: np23                date: 2008-01-23
!
! abstract: convert subdomain data into filter-space slab data with
!           vertical derivative
!
! program history log:
!   2008-01-23  sato
!
!   input argument list:
!    parm     - filter-space information
!    fsub     - subdomain data array
!
!   output argument list:
!    fslab    - filter-space data array
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  use constants, only: zero, one

! Declare passed variables
  real(r_kind)  ,intent(in) ::fsub(lat2,lon2,nsig)
  real(r_single),intent(out)::fslab(prm0%nlatf,prm0%nlonf,nsig1o)

! Declare local variables
  real(r_kind):: fsubdz(lat2,lon2,nsig),dzi
  integer(i_kind):: k,kp,km,iflg

  do k=1,nsig
    km=max(   1,k-1)
    kp=min(nsig,k+1)
    if (twodvar_regional) then; dzi=zero
    else;                       dzi=one/real(kp-km,r_kind)
    end if
    fsubdz(:,:,k)=dzi*(fsub(:,:,kp)-fsub(:,:,km))
  end do

  call sub2fslab(fsubdz,fslab)

  return
end subroutine sub2fslabdz
!=======================================================================
!=======================================================================
subroutine sub2fslabdz_glb(fsub,fslb0,fslb2,fslb3)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:   sub2fslabdz
!   prgmmr: sato             org: np23                date: 2008-01-23
!
! abstract: convert subdomain data into filter-space slab data with
!           vertical derivative for global mode
!
! program history log:
!   2008-01-23  sato
!
!   input argument list:
!    parm     - filter-space information
!    fsub     - subdomain data array
!
!   output argument list:
!    fslab    - filter-space data array
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  use constants, only: zero, one

! Declare passed variables
  real(r_kind)  ,intent(in) ::fsub(lat2,lon2,nsig)
  real(r_single),intent(out)::fslb0(prm0%nlatf,prm0%nlonf,nsig1o)
  real(r_single),intent(out)::fslb2(prm2%nlatf,prm2%nlonf,nsig1o)
  real(r_single),intent(out)::fslb3(prm3%nlatf,prm3%nlonf,nsig1o)

! Declare local variables
  real(r_kind):: fsubdz(lat2,lon2,nsig),dzi
  integer(i_kind):: k,kp,km,iflg

  do k=1,nsig
    km=max(   1,k-1)
    kp=min(nsig,k+1)
    dzi=one/real(kp-km,r_kind)
    fsubdz(:,:,k)=dzi*(fsub(:,:,kp)-fsub(:,:,km))
  end do

  call sub2fslab_glb(fsubdz,fslb0,fslb2,fslb3)

  return
end subroutine sub2fslabdz_glb
!=======================================================================
!=======================================================================
subroutine sub2slab2d(fsub,slab)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:   sub2slab2d
!   prgmmr: sato             org: np23                date: 2008-01-23
!
! abstract: convert 2d subdomain data into anlaysis-space slab data
!
! program history log:
!   2008-01-23  sato
!
!   input argument list:
!    fsub     - subdomain data array
!
!   output argument list:
!    slab     - anlaysis-space data array
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
! Declare passed variables
  real(r_kind),intent(in) ::fsub(lat2,lon2)
  real(r_kind),intent(out)::slab(nlat,nlon,nsig1o)

! Declare local variables
  integer(i_kind):: k,iflg

  do k=1,nsig
    work_t(:,:,k)=fsub(:,:)
  end do

  work_st=work_t; work_vp=work_t
  work_q =work_t; work_oz=work_t; work_cwmr=work_t
  work_p    (:,:)=fsub(:,:)
  work_sst  (:,:)=fsub(:,:)
  work_slndt(:,:)=fsub(:,:)
  work_sicet(:,:)=fsub(:,:)

  iflg=1
  call sub2grid(slab,work_t,work_p,work_q,work_oz,work_sst, &
                work_slndt,work_sicet,work_cwmr,work_st,work_vp,iflg)

  return
end subroutine sub2slab2d
!=======================================================================
!=======================================================================
subroutine sub2fslab2d(fsub,fslab)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:   sub2fslab2d
!   prgmmr: sato             org: np23                date: 2008-01-23
!
! abstract: convert 2d subdomain data into filter-space slab data
!
! program history log:
!   2008-01-23  sato
!
!   input argument list:
!    fsub     - subdomain data array
!
!   output argument list:
!    fslab    - filter-space data array
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  use fgrid2agrid_mod, only: agrid2fgrid

! Declare passed variables
  real(r_kind)  ,intent(in) ::fsub(lat2,lon2)
  real(r_single),intent(out)::fslab(prm0%nlatf,prm0%nlonf)


! Declare local variables
  integer(i_kind):: k,iflg

  call sub2slab2d(fsub,hfine)

  call agrid2fgrid(prm0,hfine,hfilter) !analysis to filter grid
  fslab(:,:)=real(hfilter(:,:),r_single)

  return
end subroutine sub2fslab2d
!=======================================================================
!=======================================================================
subroutine sub2fslab2d_glb(fsub,fslb0,fslb2,fslb3)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:   sub2fslab2d
!   prgmmr: sato             org: np23                date: 2008-01-23
!
! abstract: convert 2d subdomain data into filter-space slab data
!           for global mode.
!
! program history log:
!   2008-01-23  sato
!
!   input argument list:
!    fsub     - subdomain data array
!
!   output argument list:
!    fslab    - filter-space data array
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  use patch2grid_mod, only: grid2patch

! Declare passed variables
  real(r_kind)  ,intent(in) ::fsub(lat2,lon2)
  real(r_single),intent(out)::fslb0(prm0%nlatf,prm0%nlonf)
  real(r_single),intent(out)::fslb2(prm2%nlatf,prm2%nlonf)
  real(r_single),intent(out)::fslb3(prm3%nlatf,prm3%nlonf)

! Declare local variables
  integer(i_kind):: k,iflg

  call sub2slab2d(fsub,hfine)

  call grid2patch(hfine,hflt0,hflt2,hflt3) !analysis to filter grid
  fslb0(:,:)=real(hflt0(:,:),r_single)
  fslb2(:,:)=real(hflt2(:,:),r_single)
  fslb3(:,:)=real(hflt3(:,:),r_single)

  return
end subroutine sub2fslab2d_glb
!=======================================================================
end module sub2fslab_mod
