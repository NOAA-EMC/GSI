module gsi_4dvar
!$$$   module documentation block
!
! module:  gsi_4dvar
! prgmmr:  tremolet          org: GMAO                date: 2007-02-02
!
! abstract: Contains variables and routines to control GSI 4D-Var
!
! program history log:
!   2007-02-02 tremolet
!   2007-05-29 todling  - add initialization of GCM TLM/ADM  
!   2007-07-10 todling  - flag to allow writing of increment
!
! Subroutines Included:
!   sub setup_4dvar   - initialize 4dvar parameters
!
! Variable Definitions:
!
!   l4dvar            - 4D-Var on/off
!   lsqrtb            - Use sqrt(B) preconditioning
!   lcongrad          - Use conjugate gradient/Lanczos minimizer
!   lbfgsmin          - Use L-BFGS minimizer
!   ltlint            - Use TL inner loop (ie TL intall)
!   lanczosave        - Save Lanczos vectors to file
!   nwrvecs           - Number of precond vectors (Lanczos) or pairs of vectors (QN)
!                       being saved
!
!   ibdate            - Date and time at start of 4dvar window
!   iadatebgn         - Date and time at start of 4dvar window
!   iedate            - Date and time at  end  of 4dvar window
!   iadateend         - Date and time at  end  of 4dvar window
!
!   iwinbgn           - Time since ref at start of 4dvar window (hours)
!   winlen            - Length of 4dvar window (hours)
!   winoff            - Main analysis time within 4dvar window (hours)
!
!   nhr_obsbin        - Length of observation bins (temporary control)
!   nobs_bins         - Number of observation bins in assimilation window
!   hr_obsbin         - Length of observation bins (hours)
!
!   nhr_subwin        - Length of 4dvar sub-windows (weak constraint)
!   nsubwin           - Number of time-points in 4D control variable
!   winsub            - Length of 4dvar sub-windows (weak constraint)
!
!   ladtest           - Run adjoint test
!   lgrtest           - Run gradient test
!   lwrtinc           - When .t., writes out increment instead of analysis
!
!   idmodel           - Run w/ identity GCM TLM and ADM; test mode
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

! --------------------------------------------------------------------
  use kinds, only: r_kind,i_kind
  use constants, only: one
  use geos_pertmod, only: model_init
  use geos_pertmod, only: model_clean
! --------------------------------------------------------------------

  implicit none

  logical         :: l4dvar
  logical         :: lsqrtb
  logical         :: lcongrad
  logical         :: lbfgsmin
  logical         :: ltlint
  logical         :: ladtest
  logical         :: lgrtest
  logical         :: idmodel
  logical         :: lwrtinc
  logical         :: lanczosave

  integer(i_kind) :: iadatebgn, iadateend
  integer(i_kind) :: ibdate(5), iedate(5)
  integer(i_kind) :: nhr_obsbin, nobs_bins
  integer(i_kind) :: nhr_subwin, nsubwin
  integer(i_kind) :: nhr_assimilation,nhr_offset
  integer(i_kind) :: nwrvecs
  real(r_kind) :: iwinbgn, winlen, winoff, winsub, hr_obsbin

! --------------------------------------------------------------------
contains
! --------------------------------------------------------------------
subroutine init_4dvar ()
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_4dvar
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
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

use gridmod, only: regional

implicit none

l4dvar = .false.
lsqrtb = .false.
lcongrad = .false.
lbfgsmin = .false.
ltlint = .false.
nhr_assimilation=6
if(regional)nhr_assimilation=3
nhr_offset=3
nhr_subwin=-1
nhr_obsbin=-1
ladtest=.false.
lgrtest=.false.
idmodel= .false.
lwrtinc= .false.
lanczosave = .false.
nwrvecs=-1

end subroutine init_4dvar
! --------------------------------------------------------------------
subroutine setup_4dvar(miter,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    setup_4dvar
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!
!   input argument list:
!    mype     - mpi task id
!    miter
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

implicit none
integer(i_kind),intent(in) :: mype
integer(i_kind),intent(in) :: miter

! local variables
integer(i_kind) :: ibin,ierr

winlen = real(nhr_assimilation,r_kind)
winoff = real(nhr_offset,r_kind)

if (nhr_obsbin>0.and.nhr_obsbin<=nhr_assimilation) then
  hr_obsbin = real(nhr_obsbin,r_kind)
else
  if (l4dvar) then
!   Should depend on resolution of TLM, etc...
      hr_obsbin = one
  else
      hr_obsbin = winlen
  end if
end if

! Initialize atmospheric AD and TL model
if (l4dvar) then
  if (.not.idmodel) call model_init (ierr)
endif

! Setup observation bins
IF (hr_obsbin<winlen) THEN
  ibin = NINT(winlen/hr_obsbin)
  IF (NINT(ibin*hr_obsbin)/=nhr_assimilation) THEN
    write(6,*)'SETUP_4DVAR: Error=',ibin,hr_obsbin,nhr_assimilation
    write(6,*)'SETUP_4DVAR: Error in observation binning'
    call stop2(132)
  ENDIF
ELSE
  ibin = 0
ENDIF

nobs_bins = ibin + 1

! Setup weak constraint 4dvar
if (nhr_subwin<=0) nhr_subwin = nhr_assimilation
winsub = real(nhr_subwin,r_kind)

IF (nhr_subwin<nhr_assimilation) THEN
  nsubwin = nhr_assimilation/nhr_subwin
  IF (nsubwin*nhr_subwin/=nhr_assimilation) THEN
    write(6,*)'SETUP_4DVAR: Error=',nsubwin,nhr_subwin,nhr_assimilation
    write(6,*)'SETUP_4DVAR: Error in sub-windows definition'
    call stop2(133)
  ENDIF
ELSE
  nsubwin = 1
ENDIF

if (nwrvecs<0) then
  if (lbfgsmin) nwrvecs=10
  if (lcongrad) nwrvecs=20
endif

!! Consistency check: presently, can only write inc when miter=1
!if (lwrtinc) then
!    if (miter>1) then
!        write(6,*) 'SETUP_4DVAR: Not able to write increment when miter>1, lwrtinc,miter=',lwrtinc,miter
!        write(6,*)'SETUP_4DVAR: Unable to fullfil request for increment output'
!        call stop2(134)
!    endif
!endif
if (lwrtinc .neqv. l4dvar) then
  write(6,*)'SETUP_4DVAR: lwrtinc l4dvar inconsistent',lwrtinc,l4dvar
  call stop2(135)
end if

! Prints
if (mype==0) then
  write(6,*)'SETUP_4DVAR: l4dvar=',l4dvar
  write(6,*)'SETUP_4DVAR: winlen=',winlen
  write(6,*)'SETUP_4DVAR: winoff=',winoff
  write(6,*)'SETUP_4DVAR: hr_obsbin=',hr_obsbin
  write(6,*)'SETUP_4DVAR: nobs_bins=',nobs_bins
  write(6,*)'SETUP_4DVAR: nsubwin,nhr_subwin=',nsubwin,nhr_subwin
  write(6,*)'SETUP_4DVAR: lsqrtb=',lsqrtb
  write(6,*)'SETUP_4DVAR: lcongrad=',lcongrad
  write(6,*)'SETUP_4DVAR: lbfgsmin=',lbfgsmin
  write(6,*)'SETUP_4DVAR: ltlint=',ltlint
  write(6,*)'SETUP_4DVAR: ladtest,lgrtest=',ladtest,lgrtest
  write(6,*)'SETUP_4DVAR: lwrtinc=',lwrtinc
  write(6,*)'SETUP_4DVAR: lanczosave=',lanczosave
  write(6,*)'SETUP_4DVAR: nwrvecs=',nwrvecs
endif

end subroutine setup_4dvar
! --------------------------------------------------------------------
subroutine time_4dvar(idate,step4d)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:   time_4dvar
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!
!   input argument list:
!    idate    - Date (yyyymmddhh)
!
!   output argument list:
!    step4d   - Time since start of 4D-Var window (hours)
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

use constants, only: r60
implicit none
integer(i_kind),intent(in) :: idate   ! Date (yyyymmddhh)
real(r_kind),intent(out)   :: step4d  ! Time since start of 4D-Var window (hours)

integer(i_kind) iyr,imo,idy,ihr,nmin_obs,nhrobs,nhrbgn,nhroff

ihr=idate
iyr=ihr/1000000
ihr=ihr-1000000*iyr
imo=ihr/10000
ihr=ihr-10000*imo
idy=ihr/100
ihr=ihr-100*idy
call w3fs21((/iyr,imo,idy,ihr,0/),nmin_obs)
if (MOD(nmin_obs,60)/=0) then
  write(6,*)'time_4dvar: minutes should be 0',nmin_obs
  call stop2(136)
end if

nhrobs=nmin_obs/60
nhrbgn=NINT(real(iwinbgn,r_kind)/r60)
nhroff=nhrobs-nhrbgn

step4d=real(nhroff,r_kind)

return
end subroutine time_4dvar
! --------------------------------------------------------------------
subroutine clean_4dvar()
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    clean_4dvar
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
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

implicit none
! Initialize atmospheric AD and TL model
if (l4dvar) then
  if(.not.idmodel) call model_clean ()
endif
end subroutine clean_4dvar
! --------------------------------------------------------------------
end module gsi_4dvar
