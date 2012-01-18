module gsi_4dvar
!$$$ module documentation block
!           .      .    .                                       .
! module:   gsi_4dvar
!   prgmmr: tremolet    org: GMAO                date: 2007-02-02
!
! abstract: Contains variables and routines to control GSI 4D-Var
!
! program history log:
!   2007-02-02 tremolet
!   2007-05-29 todling  - add initialization of GCM TLM/ADM  
!   2007-07-10 todling  - flag to allow writing of increment
!   2009-10-09 wu       - replace nhr_offset with min_offset and
!                         set default 1.5 hr for regional not for 4dvar but for FGAT
!   2010-03-16 todling  - add knob to calculate analysis error from lanczos
!   2010-05-27 todling  - add gsi_4dcoupler; remove dependence on GMAO's geos pertmod
!   2010-10-05 todling  - add bi-cg option
!   2011-03-14 guo      - Moved gsi_4dcoupler calls out of this module, to split
!			  gsi_4dcoupler_init_traj() from gsimain_initialize(),
!			  and gsi_4dcoupler_final_traj() from gsimain_finalize(),
!   2011-07-10 guo/zhang- add liauon
!
! Subroutines Included:
!   sub init_4dvar    -
!   sub setup_4dvar   - initialize 4dvar parameters
!   sub time_4dvar    -
!   sub clean_4dvar   -
!
! Variable Definitions:
!
!   l4dvar            - 4D-Var on/off
!   lsqrtb            - Use sqrt(B) preconditioning
!   lbicg             - Use B preconditioning with bi-conjugate gradient
!   lcongrad          - Use conjugate gradient/Lanczos minimizer
!   lbfgsmin          - Use L-BFGS minimizer
!   ltlint            - Use TL inner loop (ie TL intall)
!   lanczosave        - Save Lanczos vectors to file
!   jsiga             - Calculate approximate analysis errors for iteration jiter=jsiga
!   nwrvecs           - Number of precond vectors (Lanczos) or pairs of vectors (QN)
!                       being saved
!   iorthomax         - max number of vectors used for orthogonalization of various CG options
!   liauon            - turn on IAU mode.  The default value is set to .false.
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
!   iwrtinc           - When >0, writes out increment from iwrtinc-index slot
!
!   ladtest           - Run adjoint test
!   lgrtest           - Run gradient test
!   ltcost            - When .t., calc true cost within Lanczos (expensive)
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
! --------------------------------------------------------------------

  implicit none

! set default to private
  private
! set subroutines to public
  public :: init_4dvar
  public :: setup_4dvar
  public :: time_4dvar
  public :: clean_4dvar
! set passed variables to public
  public :: iadatebgn,l4dvar,nobs_bins,nhr_assimilation,lsqrtb,lbicg,nsubwin
  public :: hr_obsbin,ltlint,idmodel,iwrtinc,winsub,winlen,iwinbgn
  public :: min_offset,iadateend,ibdate,iedate,lanczosave,lbfgsmin
  public :: ladtest,lgrtest,lcongrad,nhr_obsbin,nhr_subwin,nwrvecs
  public :: jsiga,ltcost,iorthomax,liauon

  logical         :: l4dvar
  logical         :: lsqrtb
  logical         :: lbicg
  logical         :: lcongrad
  logical         :: lbfgsmin
  logical         :: ltlint
  logical         :: ladtest
  logical         :: lgrtest
  logical         :: idmodel
  logical         :: lanczosave
  logical         :: ltcost
  logical         :: liauon

  integer(i_kind) :: iwrtinc
  integer(i_kind) :: iadatebgn, iadateend
  integer(i_kind) :: ibdate(5), iedate(5)
  integer(i_kind) :: nhr_obsbin, nobs_bins
  integer(i_kind) :: nhr_subwin, nsubwin
  integer(i_kind) :: nhr_assimilation,min_offset
  integer(i_kind) :: nwrvecs
  integer(i_kind) :: iorthomax
  integer(i_kind) :: jsiga
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
lbicg = .false.
lcongrad = .false.
lbfgsmin = .false.
ltlint = .false.
ltcost = .false.
liauon = .false.

nhr_assimilation=6
min_offset=180

if(regional)then
   nhr_assimilation=3
   min_offset=90
endif

nhr_subwin=-1
nhr_obsbin=-1
ladtest=.false.
lgrtest=.false.
idmodel= .true.
lanczosave = .false.
iwrtinc=-1
nwrvecs=-1
jsiga  =-1
iorthomax=0

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
!   2010-05-27  todling - provide general interface to initialization of
!                         tangent linear and adjoint model trajectory
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
integer(i_kind),intent(in   ) :: mype
integer(i_kind),intent(in   ) :: miter

! local variables
integer(i_kind) :: ibin,ierr

winlen = real(nhr_assimilation,r_kind)
winoff = real(min_offset/60._r_kind,r_kind)

if (nhr_obsbin>0.and.nhr_obsbin<=nhr_assimilation) then
   hr_obsbin = real(nhr_obsbin,r_kind)
else
   if (l4dvar) then
!     Should depend on resolution of TLM, etc...
      hr_obsbin = one
   else
      hr_obsbin = winlen
   end if
end if

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
   if (lcongrad .or. lbicg) nwrvecs=20
endif

!! Consistency check: presently, can only write inc when miter=1
!if (iwrtinc) then
!   if (miter>1) then
!      write(6,*) 'SETUP_4DVAR: Not able to write increment when miter>1, iwrtinc,miter=',iwrtinc,miter
!      write(6,*)'SETUP_4DVAR: Unable to fullfil request for increment output'
!      call stop2(134)
!   endif
!endif
if (iwrtinc>0 .neqv. l4dvar) then
   write(6,*)'SETUP_4DVAR: iwrtinc l4dvar inconsistent',iwrtinc,l4dvar
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
   write(6,*)'SETUP_4DVAR: lbicg=',lbicg
   write(6,*)'SETUP_4DVAR: lcongrad=',lcongrad
   write(6,*)'SETUP_4DVAR: lbfgsmin=',lbfgsmin
   write(6,*)'SETUP_4DVAR: ltlint=',ltlint
   write(6,*)'SETUP_4DVAR: ladtest,lgrtest=',ladtest,lgrtest
   write(6,*)'SETUP_4DVAR: iwrtinc=',iwrtinc
   write(6,*)'SETUP_4DVAR: lanczosave=',lanczosave
   write(6,*)'SETUP_4DVAR: ltcost=',ltcost
   write(6,*)'SETUP_4DVAR: jsiga=',jsiga
   write(6,*)'SETUP_4DVAR: nwrvecs=',nwrvecs
   write(6,*)'SETUP_4DVAR: iorthomax=',iorthomax
   write(6,*)'SETUP_4DVAR: liauon=',liauon
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

use constants, only: r60inv
implicit none

integer(i_kind),intent(in   ) :: idate   ! Date (yyyymmddhh)
real(r_kind)   ,intent(  out) :: step4d  ! Time since start of 4D-Var window (hours)

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

nhrobs=nmin_obs*r60inv
nhrbgn=NINT(real(iwinbgn,r_kind)*r60inv)
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
!   2010-05-27  todling - provide general interface to initialization of
!                         tangent linear and adjoint model trajectory
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
! no-op left
end subroutine clean_4dvar
! --------------------------------------------------------------------
end module gsi_4dvar
