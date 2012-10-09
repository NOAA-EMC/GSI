subroutine anbkerror(gradx,grady)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    anbkerror apply anisotropic background error covariance  
!   prgmmr: parrish          org: np22                date: 2005-02-03
!
! abstract: apply regional anisotropic background error.
!
! program history log:
!   2005-02-08  parrish
!   2005-04-29  parrish - replace coarse2fine with fgrid2agrid;
!                         remove ansmoothrf_reg_d
!   2006-11-30  todling - add fpsproj as arg to (t)balance routine(s)
!   2008-10-10  derber - add strong constraint to background error
!   2008-12-29  todling - update interface to strong_bk/bk_ad
!   2009-04-13  derber - move strong_bk into balance
!   2009-07-01  sato - update for global mode
!   2010-05-05  todling - update to use gsi_bundle
!   2010-06-22  todling - update to better handle bundle pointers
!   2010-06-29  lueken - replaced tv with t in call to gsi_bundlegetpointer
!   2010-08-19  lueken - add only to module use
!
!   input argument list:
!     gradx    - input field  
!
!   output
!     grady    - background structure * gradx 
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  use kinds, only: r_kind,i_kind
  use gridmod, only: lat2,lon2
  use jfunc, only: nsclen,npclen
  use balmod, only: balance,tbalance
  use berror, only: varprd,fpsproj
  use constants, only: zero
  use control_vectors, only: control_vector,assignment(=)
  use gsi_4dvar, only: nsubwin
  use gsi_bundlemod, only: gsi_bundlegetpointer
  implicit none

! Declare passed variables
  type(control_vector),intent(inout) :: gradx
  type(control_vector),intent(inout) :: grady

! Declare local variables
  integer(i_kind) i,j,ii,istatus
  real(r_kind),dimension(lat2,lon2):: sst,slndt,sicet
  real(r_kind),dimension(:,:,:),pointer::p_t,p_st,p_vp,p_cw
  real(r_kind),dimension(:,:  ),pointer::p_ps
  logical lc_sf,lc_vp,lc_ps,lc_t
  logical do_balance
  integer(i_kind), parameter :: myvars = 4
  integer(i_kind) :: ipnts(myvars)
  character(len=3), parameter :: myvnames(myvars) = (/  &
                               'sf ', 'vp ', 'ps ', 't  '/)


! Put things in grady first since operations change input variables
  grady=gradx

! Zero arrays for land, ocean, ice skin (surface) temperature.
  do j=1,lon2
     do i=1,lat2
        slndt(i,j)=zero
        sst(i,j)  =zero
        sicet(i,j)=zero
     end do
  end do

! Since each internal vector [step(jj)] of grad has the same structure, pointers
! are the same independent of the subwindow jj
call gsi_bundlegetpointer (grady%step(1),myvnames,ipnts,istatus)
lc_sf =ipnts(1)>0;lc_vp =ipnts(2)>0;lc_ps=ipnts(3)>0;lc_t=ipnts(4)>0

! Define what to do depending on what's in CV and SV
do_balance=lc_sf.and.lc_vp.and.lc_ps .and.lc_t

! Loop on control steps
  do ii=1,nsubwin

!    Get pointers to this subwin require state variables
     call gsi_bundlegetpointer (grady%step(ii),'sf',p_st,  istatus)
     call gsi_bundlegetpointer (grady%step(ii),'vp',p_vp,  istatus)
     call gsi_bundlegetpointer (grady%step(ii),'ps',p_ps,  istatus)
     call gsi_bundlegetpointer (grady%step(ii),'t ',p_t,   istatus)

!    Transpose of balance equation
     if(do_balance) call tbalance(p_t,p_ps,p_st,p_vp,fpsproj)

!    Apply variances, as well as vertical & horizontal parts of background error
     call anbkgcov(grady%step(ii),sst,slndt,sicet)

!    Balance equation
     if(do_balance) call balance(p_t,p_ps,p_st,p_vp,fpsproj)

  end do

! Take care of background error for bias correction terms
  if(nsclen>0)then
     do i=1,nsclen
        grady%predr(i)=grady%predr(i)*varprd(i)
     end do
  end if
  if(npclen>0)then
     do i=1,npclen
        grady%predp(i)=grady%predp(i)*varprd(nsclen+i)
     end do
  end if

end subroutine anbkerror


subroutine anbkgcov(bundle,sst,slndt,sicet)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    anbkgcov    apply anisotropic background error covar
!   prgmmr: parrish        org: np22                date: 2005-02-14
!
! abstract: apply regional anisotropic background error covariance
!
! program history log:
!   2005-02-14  parrish
!   2009-07-01  sato - update for global mode
!   2010-05-20  todling - update fit interface to sug2grid/grid2sub (bundle)
!   2010-06-22  todling - update interface (remove cwmr since it's in bunlde)
!   2010-06-29  lueken - added if(ipnts(2)>0) to second call of anbkgvar
!   2011-02-22  zhu - replace the argument list of ansmoothrf_reg_subdomain_option by a bundle
!
!   input argument list:
!     t        - t on subdomain
!     p        - p surface pressure on subdomain
!     q        - q on subdomain
!     oz       - ozone on subdomain
!     skint    - skin temperature on subdomain
!     sst      - sea surface temperature on subdomain
!     slndt    - land surface temperature on subdomain
!     sicet    - ice surface temperature on subdomain
!     st       - streamfunction on subdomain
!     vp       - velocity potential on subdomain
!
!   output argument list:
!                 all after smoothing, combining scales
!     t        - t on subdomain
!     p        - p surface pressure on subdomain
!     q        - q on subdomain
!     oz       - ozone on subdomain
!     skint    - skin temperature on subdomain
!     sst      - sea surface temperature on subdomain
!     slndt    - land surface temperature on subdomain
!     sicet    - ice surface temperature on subdomain
!     st       - streamfunction on subdomain
!     vp       - velocity potential on subdomain
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!$$$
  use kinds, only: r_kind,i_kind
  use gridmod, only: lat2,lon2,nlat,nlon,nsig,nsig1o
  use anberror, only: rtma_subdomain_option,nsmooth, nsmooth_shapiro
  use constants, only: zero
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  implicit none

! Passed Variables
  real(r_kind),dimension(lat2,lon2),intent(inout) :: sst,slndt,sicet
  type(gsi_bundle),                 intent(inout) :: bundle

! Local Variables
  integer(i_kind) iflg,ier,istatus
  real(r_kind),dimension(nlat,nlon,nsig1o):: hwork
  real(r_kind),pointer,dimension(:,:)  :: p,skint
  real(r_kind),pointer,dimension(:,:,:):: t,q,cwmr,oz,st,vp

  logical do_ansmoothrf
  integer(i_kind), parameter :: myvars = 8 
  integer(i_kind) :: ipnts(myvars)
  character(len=3), parameter :: myvnames(myvars) = (/  &
                               'ps ', 'sst', 't  ', 'q  ', 'oz ', &
                               'cw ', 'sf ', 'vp '/)

! Get pointer indexes
  call gsi_bundlegetpointer (bundle,myvnames,ipnts,istatus)

! Get required pointers
  call gsi_bundlegetpointer (bundle, 'ps', p,    istatus)
  call gsi_bundlegetpointer (bundle, 'sst',skint,istatus)
  call gsi_bundlegetpointer (bundle, 't',  t,    istatus)
  call gsi_bundlegetpointer (bundle, 'q',  q,    istatus)
  call gsi_bundlegetpointer (bundle, 'cw',cwmr,  istatus)
  call gsi_bundlegetpointer (bundle, 'oz',oz,    istatus)
  call gsi_bundlegetpointer (bundle, 'sf',st,    istatus)
  call gsi_bundlegetpointer (bundle, 'vp',vp,    istatus)

! break up skin temp into components
  if(ipnts(2)>0) call anbkgvar(skint,sst,slndt,sicet,0)

! Perform simple vertical smoothing while fields are in sudomain mode.
! The accompanying smoothing in the horizontal is performed inside the
! recursive filter. Motivation: Reduce possible high frequency noise in
! the analysis that would arise from the use of a "non-blending" RF algorithm.

  if(ipnts(3)>0) call vert_smther(t   ,nsmooth,nsmooth_shapiro)
  if(ipnts(4)>0) call vert_smther(q   ,nsmooth,nsmooth_shapiro)
  if(ipnts(5)>0) call vert_smther(oz  ,nsmooth,nsmooth_shapiro)
  if(ipnts(6)>0) call vert_smther(cwmr,nsmooth,nsmooth_shapiro)
  if(ipnts(7)>0) call vert_smther(st  ,nsmooth,nsmooth_shapiro)
  if(ipnts(8)>0) call vert_smther(vp  ,nsmooth,nsmooth_shapiro)

  if(rtma_subdomain_option) then

     do_ansmoothrf=ipnts(1)>0.and.ipnts(3)>0.and.ipnts(4)>0.and.ipnts(7)>0.and.ipnts(8)>0
!!!     oz=zero
!!!     cwmr=zero
!!!     sst=zero
!!!     slndt=zero
!!!     sicet=zero
     if(do_ansmoothrf) call ansmoothrf_reg_subdomain_option(bundle)

  else

! Convert from subdomain to full horizontal field distributed among processors
     iflg=1
     call sub2grid(hwork,bundle,sst,slndt,sicet,iflg)

! Apply horizontal smoother for number of horizontal scales
     call ansmoothrf(hwork)

! Put back onto subdomains
     call grid2sub(hwork,bundle,sst,slndt,sicet)

  end if

  if(ipnts(8)>0) call tvert_smther(vp  ,nsmooth,nsmooth_shapiro)
  if(ipnts(7)>0) call tvert_smther(st  ,nsmooth,nsmooth_shapiro)
  if(ipnts(6)>0) call tvert_smther(cwmr,nsmooth,nsmooth_shapiro)
  if(ipnts(5)>0) call tvert_smther(oz  ,nsmooth,nsmooth_shapiro)
  if(ipnts(4)>0) call tvert_smther(q   ,nsmooth,nsmooth_shapiro)
  if(ipnts(3)>0) call tvert_smther(t   ,nsmooth,nsmooth_shapiro)

! combine sst,sldnt, and sicet into skin temperature field
  if(ipnts(2)>0) call anbkgvar(skint,sst,slndt,sicet,1)

end subroutine anbkgcov


subroutine anbkgvar(skint,sst,slndt,sicet,iflg)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    anbkgvar_reg      manipulate skin temp
!   prgmmr: parrish          org: np22                date: 1990-10-06
!
! abstract: manipulate skin temp <--> sst,sfc temp, and ice temp fields
!
! program history log:
!   2005-01-22  parrish
!   2008-06-05  safford - rm unused uses
!
!   input argument list:
!     skint    - skin temperature grid values
!     sst      - sst grid values
!     slndt    - land surface temperature grid values
!     sicet    - snow/ice covered surface temperature grid values
!     iflg     - flag for skin temperature manipulation
!                0: skint --> sst,slndt,sicet
!                1: sst,slndt,sicet --> skint
!
!   output argument list:
!     skint    - skin temperature grid values
!     sst      - sst grid values
!     slndt    - land surface temperature grid values
!     sicet    - snow/ice covered surface temperature grid values
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use kinds, only: r_kind,i_kind
  use gridmod, only: lat2,lon2
  use guess_grids, only: isli2
  implicit none

! Declare passed variables
  integer(i_kind)                  ,intent(in   ) :: iflg
  real(r_kind),dimension(lat2,lon2),intent(inout) :: skint,sst,slndt,sicet

! Declare local variables
  integer(i_kind) i,j

       do j=1,lon2
          do i=1,lat2
             if(iflg == 0) then
! Break skin temperature into components
!          If land point
                if(isli2(i,j) == 1) then
                   slndt(i,j)=skint(i,j)
!          If ice
                else if(isli2(i,j) == 2_i_kind) then
                   sicet(i,j)=skint(i,j)
!          Else treat as a water point
                else
                   sst(i,j)=skint(i,j)
                end if

             else if (iflg==1) then
! Combine sst,slndt, and sicet into skin temperature field
!          Land point, load land sfc t into skint
                if(isli2(i,j) == 1) then
                   skint(i,j)=slndt(i,j)
!          Ice, load ice temp into skint
                else if(isli2(i,j) == 2_i_kind) then
                   skint(i,j)=sicet(i,j)
!          Treat as a water point, load sst into skint
                else
                   skint(i,j)=sst(i,j)
                end if
             end if
          end do
       end do

  return
end subroutine anbkgvar


subroutine ansmoothrf(work)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    ansmoothrf  anisotropic rf for regional mode
!   prgmmr: parrish          org: np22                date: 2005-02-14
!
! abstract: apply anisotropic rf for regional mode
!
! program history log:
!   2005-02-14  parrish
!   2008-12-04  sato - update for global mode
!   2009-01-02  todling - get mype from mpimod directly
!
!   input argument list:
!     work     - fields to be smoothed
!
!   output argument list:
!     work     - smoothed fields
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!$$$
  use kinds, only: r_kind,i_kind,r_single
  use anberror, only: indices,indices_p,ngauss,pf2aP1, &
                      filter_all,filter_p2,filter_p3
  use patch2grid_mod, only: patch2grid, tpatch2grid
  use mpimod, only:  npe
  use constants, only: zero
  use gridmod, only: nlat,nlon,regional
  use fgrid2agrid_mod, only: fgrid2agrid,tfgrid2agrid
  use raflib, only: raf4_ad_wrap,raf4_wrap
  implicit none

! Declare passed variables
  real(r_kind),dimension(nlat,nlon,indices%kps:indices%kpe),intent(inout) :: work

! Declare local variables
  integer(i_kind) i,igauss,j,k

  real(r_kind)  ,dimension( indices%ips:indices%ipe, &
                            indices%jps:indices%jpe, &
                            indices%kps:indices%kpe ):: worka
  real(r_single),dimension(ngauss, &
                           indices%ips:indices%ipe, &
                           indices%jps:indices%jpe, &
                           indices%kps:indices%kpe ):: workb

  real(r_kind)  ,allocatable,dimension(:,:,:)  :: workanp,workasp
  real(r_single),allocatable,dimension(:,:,:,:):: workbnp,workbsp

  if(.not.regional) then
     allocate(workanp(indices_p%ips:indices_p%ipe, &
                      indices_p%jps:indices_p%jpe, &
                      indices_p%kps:indices_p%kpe ))
     allocate(workbnp(ngauss, &
                      indices_p%ips:indices_p%ipe, &
                      indices_p%jps:indices_p%jpe, &
                      indices_p%kps:indices_p%kpe ))
     allocate(workasp(indices_p%ips:indices_p%ipe, &
                      indices_p%jps:indices_p%jpe, &
                      indices_p%kps:indices_p%kpe ))
     allocate(workbsp(ngauss, &
                      indices_p%ips:indices_p%ipe, &
                      indices_p%jps:indices_p%jpe, &
                      indices_p%kps:indices_p%kpe ))
  end if

!  adjoint of coarse to fine grid
  do k=indices%kps,indices%kpe
     if(regional) then
        call tfgrid2agrid(pf2aP1,work(1,1,k),worka(indices%ips,indices%jps,k))
     else
        call tpatch2grid(work(1,1,k), &
                         worka  (indices%ips,  indices%jps,k),   &
                         workanp(indices_p%ips,indices_p%jps,k), &
                         workasp(indices_p%ips,indices_p%jps,k))
     end if
  end do

!  transfer coarse grid fields to ngauss copies
  do k=indices%kps,indices%kpe
     do j=indices%jps,indices%jpe
        do i=indices%ips,indices%ipe
           do igauss=1,ngauss
              workb(igauss,i,j,k)=worka(i,j,k)
           end do
        end do
     end do
  end do

  if(.not.regional) then
     do k=indices_p%kps,indices_p%kpe
        do j=indices_p%jps,indices_p%jpe
           do i=indices_p%ips,indices_p%ipe
              do igauss=1,ngauss
                 workbnp(igauss,i,j,k)=workanp(i,j,k)
                 workbsp(igauss,i,j,k)=workasp(i,j,k)
              end do
           end do
        end do
     end do
  end if

!   apply recursive filter

  call raf4_wrap(   workb,filter_all,ngauss,indices,npe)
  call raf4_ad_wrap(workb,filter_all,ngauss,indices,npe)

  if(.not.regional) then
     call raf4_wrap(   workbnp,filter_p2,ngauss,indices_p,npe)
     call raf4_ad_wrap(workbnp,filter_p2,ngauss,indices_p,npe)

     call raf4_wrap(   workbsp,filter_p3,ngauss,indices_p,npe)
     call raf4_ad_wrap(workbsp,filter_p3,ngauss,indices_p,npe)
  end if

!  add together ngauss copies
  worka=zero
  do k=indices%kps,indices%kpe
     do j=indices%jps,indices%jpe
        do i=indices%ips,indices%ipe
           do igauss=1,ngauss
              worka(i,j,k)=worka(i,j,k)+workb(igauss,i,j,k)
           end do
        end do
     end do
  end do

  if(.not.regional) then
     workanp=zero
     workasp=zero
     do k=indices_p%kps,indices_p%kpe
        do j=indices_p%jps,indices_p%jpe
           do i=indices_p%ips,indices_p%ipe
              do igauss=1,ngauss
                 workanp(i,j,k)=workanp(i,j,k)+workbnp(igauss,i,j,k)
                 workasp(i,j,k)=workasp(i,j,k)+workbsp(igauss,i,j,k)
              end do
           end do
        end do
     end do
  end if

!  coarse to fine grid
  do k=indices%kps,indices%kpe
     if(regional) then
        call fgrid2agrid(pf2aP1,worka(indices%ips,indices%jps,k),work(1,1,k))
     else
        call patch2grid(work(1,1,k), &
                        worka  (indices%ips  ,indices%jps  ,k),&
                        workanp(indices_p%ips,indices_p%jps,k),&
                        workasp(indices_p%ips,indices_p%jps,k))
     end if
  end do

  if(.not.regional) then
     deallocate(workanp)
     deallocate(workbnp)
     deallocate(workasp)
     deallocate(workbsp)
  end if

end subroutine ansmoothrf

subroutine vert_smther(g,nsmooth,nsmooth_shapiro)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    vert_smther
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-09-15  lueken - added subprogram doc block
!
!   input argument list:
!    nsmooth
!    nsmooth_shapiro
!    g
!
!   output argument list:
!    g
!
! Notes:  nsmooth > 0 ==> apply 1-2-1 smoother
!         nsmooth_shapiro 0 ==> apply second moment preserving
!                               "shapiro smoother"
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use constants, only: quarter,half
  use kinds, only: r_kind,i_kind
  use gridmod, only: lat2,lon2,nsig
  implicit none


! Declare passed variables
  integer(i_kind)                             ,intent(in   ) :: nsmooth,nsmooth_shapiro
  real(r_kind),dimension(1:lat2,1:lon2,1:nsig),intent(inout) :: g

! Declare local variables
  integer(i_kind) i,j,l,k,kp,km,kp3,km3
  real(r_kind), allocatable:: gaux(:)

  if (nsig==1)return

  allocate(gaux(1:nsig))

  if (nsmooth > 0 ) then
     do i=1,lat2
        do j=1,lon2
           do l=1,nsmooth
              gaux(1:nsig)=g(i,j,1:nsig)
              do k=1,nsig
                 kp=min(k+1,nsig) ; km=max(1,k-1)
                 g(i,j,k)=quarter*(gaux(kp)+gaux(km))+half*gaux(k)
              enddo
           enddo
        enddo
     enddo
  endif

  if (nsmooth_shapiro > 0 .and. nsmooth <= 0) then
     do i=1,lat2
        do j=1,lon2
           do l=1,nsmooth_shapiro
              gaux(1:nsig)=g(i,j,1:nsig)
              do k=1,nsig
                 kp=min(k+1,nsig) ; km=max(1,k-1)
                 kp3=min(k+3_i_kind,nsig) ; km3=max(1,k-3)
                 g(i,j,k)=.28125_r_kind*(gaux(kp)+gaux(km))+half*gaux(k)-.03125_r_kind*(gaux(kp3)+gaux(km3))
              enddo
           enddo
        enddo
     enddo
  endif
  deallocate(gaux)

  return
end subroutine vert_smther


subroutine tvert_smther(g,nsmooth,nsmooth_shapiro)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    tvert_smther
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-09-15  lueken - added subprogram doc block
!
!   input argument list:
!    nsmooth
!    nsmooth_shapiro
!    g
!
!   output argument list:
!    g
!
! Notes:  nsmooth > 0 ==>  1-2-1 smoother
!         nsmooth_shapiro 0 ==>  second moment preserving
!                               "shapiro smoother"
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use constants, only: zero,quarter,half
  use kinds, only: r_kind,i_kind
  use gridmod, only: lat2,lon2,nsig
  implicit none


! Declare passed variables
  integer(i_kind)                             ,intent(in   ) :: nsmooth,nsmooth_shapiro
  real(r_kind),dimension(1:lat2,1:lon2,1:nsig),intent(inout) :: g

! Declare local variables
  integer(i_kind) i,j,l,k,kp,km,kp3,km3
  real(r_kind), allocatable:: gaux(:)

  if (nsig==1)return

  allocate(gaux(1:nsig))

  if (nsmooth > 0 ) then
     do i=1,lat2
        do j=1,lon2
           do l=1,nsmooth
              gaux(1:nsig)=zero
              do k=1,nsig
                 kp=min(k+1,nsig) ; km=max(1,k-1)
                 gaux(k)=gaux(k)+half*g(i,j,k)
                 gaux(km)=gaux(km)+quarter*g(i,j,k)
                 gaux(kp)=gaux(kp)+quarter*g(i,j,k)
              enddo
              g(i,j,1:nsig)=gaux(1:nsig)
           enddo
        enddo
     enddo
  endif

  if (nsmooth_shapiro > 0 .and. nsmooth <= 0) then
     do i=1,lat2
        do j=1,lon2
           do l=1,nsmooth_shapiro
              gaux(1:nsig)=zero
              do k=1,nsig
                 kp=min(k+1,nsig) ; km=max(1,k-1)
                 kp3=min(k+3_i_kind,nsig) ; km3=max(1,k-3)
                 gaux(km3)=gaux(km3)-.03125_r_kind*g(i,j,k)
                 gaux(kp3)=gaux(kp3)-.03125_r_kind*g(i,j,k)
                 gaux(k)=gaux(k)+half*g(i,j,k)
                 gaux(km)=gaux(km)+.28125_r_kind*g(i,j,k)
                 gaux(kp)=gaux(kp)+.28125_r_kind*g(i,j,k)
              enddo
              g(i,j,1:nsig)=gaux(1:nsig)
           enddo
        enddo
     enddo
  endif
  deallocate(gaux)

  return
end subroutine tvert_smther


subroutine ansmoothrf_reg_subdomain_option(cstate)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    ansmoothrf_reg_subdomain_option  anisotropic rf for regional mode
!   prgmmr: parrish          org: np22                date: 2005-02-14
!
! abstract: apply anisotropic rf for regional mode (using subdomains instead of slabs)
!              NOTE: only works if filter grid is same as analysis grid
!
! program history log:
!   2005-02-14  parrish
!   2011-02-22  zhu - use cstate to replace argument list such as p,t,q,vp,st 
!
!   input argument list:
!     t,p,q,oz,st,slndt,sicet,cwmr,st,vp   -  fields to be smoothed
!
!   output argument list:
!     t,p,q,oz,st,slndt,sicet,cwmr,st,vp   -  smoothed fields
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!$$$
  use kinds, only: r_kind,i_kind,r_single
  use anberror, only: indices, filter_all,ngauss,halo_update_reg
  use mpimod, only: mype,npe
  use constants, only: zero
  use gridmod, only: lat2,lon2,istart,jstart,nsig
  use raflib, only: raf4_ad_wrap,raf4_wrap
  use control_vectors, only: nrf,nrf_var,nrf_3d
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  implicit none

! Declare passed variables
! real(r_kind),dimension(lat2,lon2)     ,intent(inout) :: p
! real(r_kind),dimension(lat2,lon2,nsig),intent(inout) :: t,q,vp,st
  type(gsi_bundle),intent(inout) :: cstate

! Declare local variables
  integer(i_kind) i,igauss,iloc,j,jloc,k,kk,mm1,n,istatus
  real(r_single),dimension(ngauss, &
                           indices%ips:indices%ipe,&
                           indices%jps:indices%jpe,&
                           indices%kps:indices%kpe):: workb
  real(r_kind),pointer::rank2(:,:)
  real(r_kind),pointer::rank3(:,:,:)

  integer(i_kind):: ids,ide,jds,jde,kds,kde,ips,ipe,jps,jpe,kps,kpe

  ids=indices%ids; ide=indices%ide
  jds=indices%jds; jde=indices%jde
  kds=indices%kds; kde=indices%kde
  ips=indices%ips; ipe=indices%ipe
  jps=indices%jps; jpe=indices%jpe
  kps=indices%kps; kpe=indices%kpe

  mm1=mype+1

!  transfer variables to ngauss copies
  kk=0
  do n=1,nrf
     if (nrf_3d(n)) then
        call gsi_bundlegetpointer (cstate,trim(nrf_var(n)),rank3,istatus)
        if(istatus==0) then
           do k=1,nsig
              kk=kk+1
              do j=jps,jpe
                 jloc=j-jstart(mm1)+2
                 do i=ips,ipe
                    iloc=i-istart(mm1)+2
                    do igauss=1,ngauss
                       workb(igauss,i,j,kk)=rank3(iloc,jloc,k)
                    end do
                 end do
              end do
           end do
        endif
     else
        call gsi_bundlegetpointer (cstate,trim(nrf_var(n)),rank2,istatus)
        if(istatus==0) then
           kk=kk+1
           do j=jps,jpe
              jloc=j-jstart(mm1)+2
              do i=ips,ipe
                 iloc=i-istart(mm1)+2
                 do igauss=1,ngauss
                    workb(igauss,i,j,kk)=rank2(iloc,jloc)
                 end do 
              end do
           end do
        endif
     endif
  end do

!   apply recursive filter

  call raf4_wrap(workb,filter_all,ngauss,indices,npe)
  call raf4_ad_wrap(workb,filter_all,ngauss,indices,npe)

!  add together ngauss copies
  kk=0
  do n=1,nrf
     if (nrf_3d(n)) then
        call gsi_bundlegetpointer (cstate,trim(nrf_var(n)),rank3,istatus)
        if(istatus==0) then
           do k=1,nsig
              kk=kk+1
              do j=jps,jpe
                 jloc=j-jstart(mm1)+2
                 do i=ips,ipe
                    iloc=i-istart(mm1)+2
                    rank3(iloc,jloc,k)=zero
                    do igauss=1,ngauss
                       rank3(iloc,jloc,k)=rank3(iloc,jloc,k)+workb(igauss,i,j,kk)
                    end do
                 end do
              end do
           end do
           call halo_update_reg(rank3,nsig)
        endif
     else
        call gsi_bundlegetpointer (cstate,trim(nrf_var(n)),rank2,istatus)
        if(istatus==0) then
           kk=kk+1
           do j=jps,jpe
              jloc=j-jstart(mm1)+2
              do i=ips,ipe
                 iloc=i-istart(mm1)+2
                 rank2(iloc,jloc)=zero
                 do igauss=1,ngauss
                    rank2(iloc,jloc)=rank2(iloc,jloc)+workb(igauss,i,j,kk)
                 end do
              end do
           end do
           call halo_update_reg(rank2,1)
        endif
     endif
  end do

end subroutine ansmoothrf_reg_subdomain_option
