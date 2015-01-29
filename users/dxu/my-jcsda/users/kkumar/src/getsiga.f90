subroutine getsiga ()
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    getsiga
!   prgmmr: todling
!
! abstract:  Calculate analysis errors from Lanczos-CG results
!
! program history log:
!   2010-03-16  todling  - initial code
!   2010-05-14  todling  - update to use gsi_bundle
!   2010-05-27  todling  - gsi_4dcoupler; remove all user-specific TL-related references
!   2010-08-19  lueken   - add only to module use;no machine code, so use .f90
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

use kinds, only: i_kind, r_kind
use mpimod, only: mype
use constants, only: zero,one
use gsi_4dvar, only: nsubwin,ibdate,lsqrtb,jsiga
use jfunc, only: jiter,miter
use lanczos, only : congrad_siga
use state_vectors, only: allocate_state,deallocate_state
use gsi_4dcouplermod, only: gsi_4dcoupler_putpert
use gsi_bundlemod, only: gsi_bundle
implicit none
! declare local variables
character(len=*),parameter:: myname_ = "getsiga"
type(gsi_bundle)     :: siga                      ! vector to analysis errors
integer(i_kind)      :: nymd                      ! date as in YYYYMMDD
integer(i_kind)      :: nhms                      ! time as in HHMMSS
integer(i_kind)      :: nvecs
integer(i_kind)      :: ier

! consistency checks
if (jiter/=jsiga) return
if (.not.lsqrtb) then
   write(6,*)trim(myname_),': must set lsqrt=.t. to get analysis errors'
   call stop2(331)
end if

nymd = 10000*ibdate(1)+ibdate(2)*100+ibdate(3)
nhms = 10000*ibdate(4)
if(mype==0) write(6,'(2a,i8.8,2x,i6.6)')trim(myname_),': starting to calculate analysis errors at ',&
             nymd, nhms

! allocate memory for working arrays
call allocate_state(siga)

! calculate estimate of analysis errors
call congrad_siga(siga,nvecs,ier)

! write out analysis errors
if(ier==0) then
   call gsi_4dcoupler_putpert (siga,nymd,nhms,'tlm','siga')
   if(mype==0) write(6,'(2a,i5,a)')trim(myname_),': complete calculating analysis errors using ',&
                                   nvecs, ' eigenvectors'
else
   if(mype==0) write(6,'(2a,i6)')trim(myname_),': failed to calculate analysis errors, ier= ', ier
endif

! clean up
call deallocate_state(siga)

return
end subroutine getsiga

subroutine view_cv (xhat,mydate,filename,writecv)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    view_cv
!   prgmmr: todling
!
! abstract:  this allow writing CV to file for visualization
!
! program history log:
!   2011-02-23  todling  - initial code
!                          (not sure we'll keep this here)
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

use kinds, only: i_kind, r_kind
use mpimod, only: mype
use constants, only: zero,one
use gsi_4dvar, only: nsubwin,lsqrtb
use jfunc, only: jiter,miter
use state_vectors, only: allocate_state,deallocate_state
use gsi_4dcouplermod, only: gsi_4dcoupler_putpert
use gsi_bundlemod, only: gsi_bundle
use control_vectors, only: control_vector,write_cv
use state_vectors, only: allocate_state,deallocate_state,prt_state_norms
use bias_predictors, only: predictors,allocate_preds,deallocate_preds
use bias_predictors, only: write_preds
implicit none
type(control_vector)        :: xhat
integer(i_kind), intent(in) :: mydate(5) ! as in iadate or ibdate, or similar
character(len=*),intent(in) :: filename
logical,         intent(in) :: writecv   ! when .t., simply write out CV directly
! declare local variables
character(len=*),parameter:: myname_ = "view_cv"
integer(i_kind)      :: nymd                      ! date as in YYYYMMDD
integer(i_kind)      :: nhms                      ! time as in HHMMSS
integer(i_kind)      :: ii,ier
type(gsi_bundle) :: mval(nsubwin)
type(predictors) :: sbias

! in case CV not required to be transformed ...
if (writecv) then
   call write_cv(xhat,filename)
   return
else
   if(mype==0) write(6,*) trim(myname_),': not writing CV to disk for now'
   return
endif

! otherwise, transform CV to state-space and write out ...
nymd = 10000*mydate(1)+mydate(2)*100+mydate(3)
nhms = 10000*mydate(4)
if(mype==0) write(6,'(2a,i8.8,2x,i6.6)')trim(myname_),': start writing state on ',&
             nymd, nhms

! Allocate local variables
do ii=1,nsubwin
   call allocate_state(mval(ii))
end do
call allocate_preds(sbias)

if (lsqrtb) then
   call control2model(xhat,mval,sbias)
else
   call control2state(xhat,mval,sbias)
endif

! write out analysis errors
do ii=1,nsubwin
   call gsi_4dcoupler_putpert (mval(ii),nymd,nhms,'tlm',filename) ! will need to be smart for nsubwin>1
   call prt_state_norms(mval(ii),'output-state')
enddo
call write_preds(sbias,'preds_'//trim(filename),mype)

! Allocate local variables
call deallocate_preds(sbias)
do ii=1,nsubwin
   call deallocate_state(mval(ii))
end do

if(mype==0) write(6,'(3a)')trim(myname_),': complete writing state ', trim(filename)

return
end subroutine view_cv

subroutine view_cv_ad (xhat,mydate,filename,readcv)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    view_cv_ad
!   prgmmr: todling
!
! abstract:  this allows reading state/control vector and transforming to CV
!
! program history log:
!   2012-05-22  todling  - based on view_ad, performs opposite of view_cv
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

use kinds, only: i_kind, r_kind
use mpimod, only: mype
use constants, only: zero,one
use gsi_4dvar, only: nsubwin,lsqrtb
use jfunc, only: jiter,miter
use state_vectors, only: allocate_state,deallocate_state
use gsi_4dcouplermod, only: gsi_4dcoupler_getpert
use gsi_bundlemod, only: gsi_bundle
use gsi_bundlemod, only: gsi_bundlegetpointer
use gsi_bundlemod, only: assignment(=)
use control_vectors, only: control_vector,read_cv,assignment(=)
use state_vectors, only: allocate_state,deallocate_state,prt_state_norms
use bias_predictors, only: predictors,allocate_preds,deallocate_preds,assignment(=)
use bias_predictors, only: read_preds
implicit none
type(control_vector)        :: xhat
integer(i_kind), intent(in) :: mydate(5) ! as in iadate or ibdate, or similar
character(len=*),intent(in) :: filename
logical,         intent(in) :: readcv    ! when .t. simply read in CV
! declare local variables
character(len=*),parameter:: myname_ = "view_cv_ad"
integer(i_kind)      :: nymd                      ! date as in YYYYMMDD
integer(i_kind)      :: nhms                      ! time as in HHMMSS
integer(i_kind)      :: ii,ier
real(r_kind),pointer,dimension(:,:,:):: mv_3d
real(r_kind),pointer,dimension(:,:  ):: mv_2d
type(gsi_bundle) :: mval(nsubwin)
type(predictors) :: sbias

! in case CV not required to be transformed ...
if (readcv) then
   call read_cv(xhat,filename)
   return
else ! for now only
   xhat=zero
   if(mype==0) write(6,*) trim(myname_),': input vector set to zero for now'
   return
endif

! otherwise read state-vector and transform to control-space ...
nymd = 10000*mydate(1)+mydate(2)*100+mydate(3)
nhms = 10000*mydate(4)
if(mype==0) write(6,'(2a,i8.8,2x,i6.6)')trim(myname_),': start reading state on ',&
             nymd, nhms

! Allocate local variables
do ii=1,nsubwin
   call allocate_state(mval(ii))
   mval(ii)=zero
end do
call allocate_preds(sbias)
sbias=zero
xhat=zero

if (lsqrtb) then
   call control2model(xhat,mval,sbias) ! dirty trick
endif
! read in (model/state) vector
do ii=1,nsubwin
   mval(ii)=zero
   call gsi_4dcoupler_getpert (mval(ii),'tlm',filename) ! will need better for nsubwin>1
   call prt_state_norms(mval(ii),'input-state')
enddo
call read_preds(sbias,'preds_'//trim(filename))

! convert to control vector
if (lsqrtb) then
   call model2control(mval,sbias,xhat)
else
   call state2control(mval,sbias,xhat)
endif

! Allocate local variables
call deallocate_preds(sbias)
do ii=1,nsubwin
   call deallocate_state(mval(ii))
end do

if(mype==0) write(6,'(3a)')trim(myname_),': complete reading state ', trim(filename)
return
end subroutine view_cv_ad

subroutine put_cv(xcv,cdfile)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    put_cv
!   prgmmr: tremolet         org:  gmao               date:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!
!   input argument list:
!    xcv
!    cdfile
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use kinds, only: i_kind
  use mpimod, only: mype
  use file_utility, only : get_lun
  use gridmod, only: nlat,nlon
  use control_vectors, only: control_vector
  implicit none
  type(control_vector), intent(in   ) :: xcv
  character(len=*)    , intent(in   ) :: cdfile

  character(len=100) :: clfile
  character(len=5) :: clmype
  integer(i_kind):: iunit

  iunit=get_lun()
  clmype='.YYYY'
  write(clmype(2:5),'(I4.4)')mype
  clfile=trim(cdfile)//clmype
  if (mype==0) write(6,*)'Writing control vector to file ',clfile

  open(iunit,file=trim(clfile),form='unformatted')
  write(iunit)xcv%lencv,xcv%step(1)%ndim,nlat,nlon  ! _RT not quite general having nlat/nlon
  write(iunit)xcv%values(1:xcv%lencv)
  close(iunit)

  return
end subroutine put_cv
! ----------------------------------------------------------------------
subroutine get_cv(xcv,cdfile)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    get_cv
!   prgmmr:  tremolet        org: gmao                date:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!
!   input argument list:
!    xcv
!    cdfile
!
!   output argument list:
!    xcv
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use kinds, only: i_kind,r_kind
  use mpimod, only: mype
  use file_utility, only : get_lun
  use gsi_4dvar, only: nsubwin,lsqrtb
  use gsi_bundlemod, only: gsi_grid
  use gsi_bundlemod, only: gsi_gridcreate
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlecreate
  use gsi_bundlemod, only: gsi_bundledestroy
  use jfunc, only: nsclen,npclen
  use control_vectors, only: control_vector
  use control_vectors, only: cvars2d,cvars3d
  use general_sub2grid_mod, only: sub2grid_info,general_sube2suba
  use general_sub2grid_mod, only: general_sub2grid_create_info,general_sub2grid_destroy_info
  use general_specmod, only: spec_vars,general_init_spec_vars,general_destroy_spec_vars
  use egrid2agrid_mod, only: egrid2agrid_parm
  use egrid2agrid_mod,only: g_create_egrid2agrid
  use gridmod, only: jcap,nlat,rlats,nlon,nsig,rlons,use_sp_eqspace

  implicit none

  type(control_vector), intent(inout) :: xcv
  character(len=*)    , intent(in   ) :: cdfile

  character(len=100) :: clfile
  character(len=5) :: clmype
  integer(i_kind):: ii,jj,kk,iunit,ilen,nord_e2a,n2dfields,istatus
  integer(i_kind):: ndim_in,nlat_in,nlon_in
  logical,allocatable,dimension(:):: vector
  real(r_kind),allocatable,dimension(:)::aux
  type(gsi_bundle) :: xvtmp
  type(gsi_grid) :: gridtmp
  type(sub2grid_info):: grd_anl,grd_low
  type(spec_vars),save :: sp_low
  type(egrid2agrid_parm) :: p_e2a

  iunit=get_lun()
  clmype='.YYYY'
  write(clmype(2:5),'(I4.4)')mype
  clfile=trim(cdfile)//clmype
  if (mype==0) write(6,*)'Reading control vector from file ',clfile

  open(iunit,file=trim(clfile),form='unformatted')
  read(iunit)ilen,ndim_in,nlat_in,nlon_in
  if (ilen==xcv%lencv) then
     read(iunit)xcv%values(1:xcv%lencv)
  else
     if(lsqrtb) then
        write(6,*)'get_cv: (lsqrtb) wrong length',ilen,xcv%lencv
        call stop2(116)
     endif
     write(6,*)'get_cv: inconsistent length',ilen,xcv%lencv
     write(6,*)'get_cv: trying to interpolate ...'
!
     n2dfields=size(cvars3d)*nsig+size(cvars2d)
     allocate(vector(n2dfields))
!    initialize low-res grid
!    TODO: 
!      1) pass dims of low-res grid
!      2) set vector properly
     vector=.false.
     nord_e2a=4
     call general_sub2grid_create_info(grd_anl,1,nlat   ,nlon   ,nsig,n2dfields,.false.,vector)
     call general_sub2grid_create_info(grd_low,1,nlat_in,nlon_in,nsig,n2dfields,.false.,vector)
!    create the grid for a control vector in low-res grid
     call gsi_gridcreate(gridtmp,grd_low%lat2,grd_low%lon2,grd_low%nsig)
!    initialize low-to-high transforms
     call general_init_spec_vars(sp_low,jcap,jcap,grd_low%nlat,grd_low%nlon,eqspace=use_sp_eqspace)
     call g_create_egrid2agrid(nlat,rlats,nlon,rlons,grd_low%nlat,sp_low%rlats,grd_low%nlon,sp_low%rlons, &
                               nord_e2a,p_e2a,eqspace=use_sp_eqspace)
!    read low-res control vector
     allocate(aux(ilen))
     read(iunit)aux
!    interpolate from low to high resolution
     ii=0;kk=0
     do jj=1,nsubwin
        call gsi_bundlecreate(xvtmp,gridtmp,'temporary CV',istatus, &
                              names2d=cvars2d,names3d=cvars3d,bundle_kind=r_kind)
!       if (jj==1.and.ilen/=nsubwin*size(xvtmp%values)) then
!          write(6,*)'get_cv: wrong length',ilen,nsubwin,nsubwin*size(xvtmp%values)
!          call stop2(116)
!       endif
        xvtmp%values=aux(ii+1:ii+ndim_in)
        call general_sube2suba(grd_low,grd_anl,p_e2a,xvtmp%values,xcv%step(jj)%values,.false.)
        call gsi_bundledestroy(xvtmp,istatus)
        ii=ii+xcv%step(jj)%ndim
        kk=kk+ndim_in
!       ensemble ?
!       now the bias predictors
        xcv%step(jj)%values(ii+1:ii+nsclen+npclen) = aux(kk+1:kk+nsclen+npclen)
        ii=ii+nsclen+npclen
        kk=kk+nsclen+npclen

     enddo
     deallocate(aux)
     if (ii/=xcv%lencv) then
        write(6,*)'get_cv: wrong total length',ii,xcv%lencv
        call stop2(116)
     endif
!    finalize stuff needed for interpolation
     call general_destroy_spec_vars(sp_low)
  end if
  close(iunit)
  call general_sub2grid_destroy_info(grd_anl)
  call general_sub2grid_destroy_info(grd_low)

  return
end subroutine get_cv

subroutine view_st (sval,filename)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    view_st
!   prgmmr: todling
!
! abstract:  this allow writing CV to file for visualization
!
! program history log:
!   2011-02-23  todling  - initial code
!                          (not sure we'll keep this here)
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

use kinds, only: i_kind, r_kind
use mpimod, only: mype
use constants, only: zero,one
use gsi_4dvar, only: ibdate,nobs_bins,nhr_obsbin
use gsi_4dcouplermod, only: gsi_4dcoupler_putpert
use gsi_bundlemod, only: gsi_bundle
implicit none
type(gsi_bundle)            :: sval(nobs_bins)
character(len=*),intent(in) :: filename
! declare local variables
character(len=*),parameter:: myname_ = "view_st"
integer(i_kind)      :: nymd                      ! date as in YYYYMMDD
integer(i_kind)      :: nhms                      ! time as in HHMMSS
integer(i_kind)      :: ii,ier
integer(i_kind)      :: mydate(5)

integer(i_kind),dimension(8) :: ida,jda
real(r_kind),dimension(5)    :: fha


! write out analysis errors
mydate = ibdate
do ii=1,nobs_bins
   nymd = 10000*mydate(1)+mydate(2)*100+mydate(3)
   nhms = 10000*mydate(4)
   ! iwrtinc ...

   if(mype==0) write(6,'(2a,i8.8,2x,i6.6)')trim(myname_),': start writing state on ', nymd, nhms
   call gsi_4dcoupler_putpert (sval(ii),nymd,nhms,'tlm',filename)
!  call prt_state_norms(sval(ii),'output-state')

   ! increment mydate ...
   fha(:)=0.0; ida=0; jda=0
   fha(2)=nhr_obsbin! relative time interval in hours
   ida(1)=mydate(1) ! year
   ida(2)=mydate(2) ! month
   ida(3)=mydate(3) ! day
   ida(4)=0         ! time zone
   ida(5)=mydate(4) ! hour
   ! Move date-time forward by nhr_assimilation hours
   call w3movdat(fha,ida,jda)
   mydate(1)=jda(1)
   mydate(2)=jda(2)
   mydate(3)=jda(3)
   mydate(4)=jda(5)
enddo

if(mype==0) write(6,'(3a)')trim(myname_),': complete writing state ', trim(filename)

return
end subroutine view_st
