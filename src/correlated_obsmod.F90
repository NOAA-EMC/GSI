!BOI

!  !TITLE: Correlated\_ObsMod: Inter-channel Observation Correlation Module

!  !AUTHORS: Ricardo Todling

!  !AFFILIATION: Global Modeling and Assimilation Office, NASA/GSFC, Greenbelt, MD 20771

!  !DATE: 13 April 2014

!  !INTRODUCTION: Overview
#ifdef __PROTEX__

This module introduces the ability for GSI to account for inter-channel
correlated errors for radiance observations. It assumes an offline estimate of
an observation error covariance for a given instrument is available. 

At GMAO, the offline estimation of the error covariances required by this module
is performed by a FORTRAN program that reads the GSI-diag files and performs 
statistics on the observation-minus-background and observation-minus-analysis
residuals, following the so-called Desroziers approach (e.g., Desroziers et al.
2005; Q. J. R. Meteorol. Soc., 131, 3385-3396).

This module defines the so-called Obs\_Error\_Cov.

As Met\_Guess and other like-modules, the idea is for this module to define nearly 
opaque object. However, so far, we have had no need to add inquire-like functions - that
is, no code outside this code needs to what what is inside GSI\_Obs\_Error\_Cov. 
So far, only very general `methods'' are made public from this module, these
being, 

\begin{verbatim}
public :: corr_ob_initialize
public :: corr_ob_amiset
public :: corr_ob_scale_jac
public :: corr_ob_finalize
\end{verbatim}

and never the variables themselves; the only exception being the GSI\_MetGuess\_Bundle itself 
(until it is no longer treated as a common-block).  Some of the public methods above are 
overloaded and all have internal interfaces (name of which appears in the index of this protex 
document. It should be a rule here that any new routine to be made public should
have a declared interface procedure.

\begin{center}
\fbox{Obs\_Error\_Cov is defined via the {\it correlated\_observations} table in a resource file}
\end{center}

\underline{Defining Observation Error Covariance Models} is done via the table {\it correlated\_observations},
usually embedded in the {\it anavinfo} file. An example of such table follows:
\begin{verbatim}
correlated_observations::
! obtype   method   kreq   type    cov_file
  airs        2      60.   ocean   airs_rcov.bin
# cris        2     -99.   global  cris_rcov.bin
  iasi        2      60.   global  iasi_rcov.bin
# ssmis       2     -99.   global  ssmis_rcov.bin
::
\end{verbatim}

As usual, this table follows INPAK/ESMF convention, begining with a name
(correlated\_observations), followed by double colons (::) to open the table and 
ending with double colons.  Any line starting with an exclamation mark or a pound sign 
is taken as a comment.

The current {\it correlated\_observations} table has four columns defined as follows:

\begin{verbatim}
Column 1: obtype - refers to instrument type (follows, typical GSI nomenclature)
Column 2: method - specify different possibilities for handling the corresponding 
          cov(R) at present:
           0 - diag of estimated R only
           1 - correlations of estimated R with variances as percentage of prescribed R
           2 - full R
Column 3: kreq   - level of required condition for the corresponding cov(R)
          at present:
          if<0    does not recondition matrix
          if>0    recondition matrix following the 2nd method in Weston et al. (2014; 
                  Q. J. R. Meteorol. Soc., DOI: 10.1002/qj.2306)
Column 4: type - determines whether to apply covariance globally, or over ocean or land
Column 5: cov_file - name of file holding estimate of error covariance for the
                     instrument specified in column 1
\end{verbatim}
For the time being, the file holding the covariance is taken to be a little
ending file; this will be changed to a NetCDF-type file soon.

#endif
!EOI

!-------------------------------------------------------------------------
!  NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1, GMAO  !
!-------------------------------------------------------------------------
!BOP
!  
! !MODULE: correlated_obsmod -- inter-channel correlation ob error capability
!
! !INTERFACE:

module correlated_obsmod
!
! !DESCRIPTION: Module to handle options and components related to
!               specifying inter-channel correlation observation error 
!               covariances.
!
!               This uses wired-in type arrays to hold error covariances
!               for different instruments. This should be revisited in
!               the future.
!
! !USES:

use kinds, only: i_kind, r_single, r_kind, r_double
use constants, only: zero,one
use mpimod, only: mype
use timermod, only: timer_ini, timer_fnl
use mpeu_util, only: gettablesize
use mpeu_util, only: gettable
use mpeu_util, only: die
use mpeu_util, only: luavail
private
save

! !PUBLIC MEMBER FUNCTIONS:

public corr_ob_initialize
public corr_ob_finalize
public corr_ob_scale_jac
public corr_ob_amiset
public idnames
public ObsErrorCov
public GSI_BundleErrorCov

! !METHOD OVERLOADING:

interface corr_ob_initialize; module procedure ini_; end interface
interface corr_ob_amiset; module procedure amIset_; end interface
interface corr_ob_scale_jac; module procedure scale_jac_; end interface
interface corr_ob_finalize; module procedure fnl_; end interface

! !REVISION HISTORY:
!
!   15Apr2014 Todling  Initial code.
!
!EOP
!-------------------------------------------------------------------------
!BOC

integer(i_kind),parameter::MAXSTR=256
character(len=MAXSTR),allocatable :: instruments(:)
character(len=MAXSTR),allocatable :: idnames(:)

integer :: ninstr=-1   ! single instrument for now
logical :: iamroot_

! !PRIVATE TYPES:

type ObsErrorCov
     character(len=40) :: name                        ! R covariance name
     character(len=20) :: instrument                  ! instrument
     integer(i_kind)   :: nch_active=-1               ! active channels
     integer(i_kind)   :: method    =-1               ! define method of computation
     real(r_kind)      :: kreq      =-99.             ! Weston et al-like spectrum adjustment factor
     character(len=20) :: mask      ='global'         ! Apply covariance for profiles over all globe
     integer(i_kind),pointer :: indxR(:)   =>NULL()   ! indexes of active channels
     real(r_kind),   pointer :: R(:,:)     =>NULL()   ! nch_active x nch_active
     real(r_kind),   pointer :: Revals(:)  =>NULL()   ! eigenvalues of R
     real(r_kind),   pointer :: Revecs(:,:)=>NULL()   ! eigenvectors of R
end type

! !PUBLIC TYPES:

type(ObsErrorCov),pointer :: GSI_BundleErrorCov(:)

! strictly internal quantities
character(len=*),parameter :: myname='correlated_obsmod'
logical :: initialized_=.false.
logical, parameter :: VERBOSE_=.true.
integer(i_kind),parameter :: methods_avail(4)=(/-1, & ! do nothing
                                                 0, & ! use dianonal of estimate(R)
                                                 1, & ! use full est(R), but decompose once for all
                                                 2/)  ! use full est(R), but re-decomp at each profile
contains

!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  ini_ --- Initialize info about correlated obs (read resource table)
!
! !INTERFACE:
!
subroutine ini_ (iamroot)
! !USES:
use mpeu_util, only: die
implicit none
! !INPUT PARAMETERS:
   logical,optional,intent(in) :: iamroot 
! !DESCRIPTION: Define parameters and setting for handling correlated
!               observation errors via resouce file reading.
!
! !REVISION HISTORY:
!   2014-04-13  todling  initial code
!
! !REMARKS:
!   language: f90
!   machine:  discover
!
! !AUTHOR:
!   Ricardo Todling  org: gmao      date: 2014-04-13
!
!EOP
!-------------------------------------------------------------------------
!BOC
character(len=*),parameter:: rcname='anavinfo'  ! filename should have extension
character(len=*),parameter:: tbname='correlated_observations::'
integer(i_kind) luin,ii,ntot,nrows,method
character(len=MAXSTR),allocatable,dimension(:):: utable
character(len=20) instrument, filename, mask
real(r_single) kreq4
real(r_kind) kreq
character(len=*),parameter::myname_=myname//'*ini_'

if(initialized_) return

iamroot_=mype==0
if(present(iamroot)) iamroot_=iamroot 

! load file
luin=luavail()
open(luin,file=rcname,form='formatted')

! Scan file for desired table first
! and get size of table
call gettablesize(tbname,luin,ntot,nrows)
if(nrows==0) then
   close(luin)
   return
endif
ninstr=nrows

! Get contents of table
allocate(utable(ninstr),instruments(ninstr),idnames(ninstr))
call gettable(tbname,luin,ntot,ninstr,utable)

! release file unit
close(luin)

allocate(GSI_BundleErrorCov(ninstr))

! Retrieve each token of interest from table and define
! variables participating in state vector

! Count variables first
if(iamroot_) write(6,*) myname_,': Correlated-Obs for the following instruments'
do ii=1,ninstr
   read(utable(ii),*) instrument, method, kreq4, mask, filename ! if adding col to table leave fname as last
   instruments(ii) = trim(instrument)
   idnames(ii) = trim(instrument)//':'//trim(mask)
   kreq=kreq4
   if(iamroot_) then
      write(6,'(1x,2(a,1x),i4,1x,f7.2,1x,a)') trim(instrument), trim(mask), method, kreq4, trim(filename)
   endif
!  check method validity
   if(ALL(methods_avail/=method)) then
     call die(myname_,' invalid choice of method, aborting')
   endif
   call set_(trim(instrument),trim(filename),mask,method,kreq,GSI_BundleErrorCov(ii))
enddo

! release table
deallocate(utable)

end subroutine ini_
!EOC

!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  set_ --- set error covariances for different instruments
!
! !INTERFACE:
!
subroutine set_(instrument,fname,mask,method,kreq,ErrorCov)
implicit none

! !INPUT PARAMETERS:

character(len=*),intent(in) :: instrument  ! name of instrument
character(len=*),intent(in) :: fname       ! filename holding cov(R)
character(len=*),intent(in) :: mask        ! land/sea/etc mask
integer,intent(in):: method                ! method to apply when using this cov(R)
real(r_kind),intent(in) :: kreq            ! conditioning factor for cov(R)
type(ObsErrorCov) :: ErrorCov              ! cov(R) for this instrument

! !DESCRIPTION: Given basic information on the instrument type
!               this routine reads an available estimate
!               of the corresponding fully-correlated error
!               covariance and fills the FORTRAN type defined
!               as ObsErrorCov.
!
! !REVISION HISTORY:
!   2014-04-13  todling  initial code
!
! !REMARKS:
!   language: f90
!   machine:  discover
!
! !AUTHOR:
!   Ricardo Todling  org: gmao      date: 2014-04-13
!
!EOP
!-------------------------------------------------------------------------
!BOC

character(len=*),parameter :: myname_=myname//'*set'
integer(i_kind) nch_active,lu,ii,ioflag

real(r_single),allocatable, dimension(:,:) :: readR  ! nch_active x nch_active x ninstruments
real(r_kind),allocatable, dimension(:) :: diag

   ErrorCov%instrument = trim(instrument)
   ErrorCov%mask = trim(mask)
   ErrorCov%name = trim(instrument)//':'//trim(mask)
   ErrorCov%method = method
   ErrorCov%kreq   = kreq

   lu = luavail()
   open(lu,file=trim(fname),convert='little_endian',form='unformatted')
   read(lu,IOSTAT=ioflag) nch_active
   if(ioflag/=0) call die(myname_,' failed to read nch from '//trim(fname))
   ErrorCov%nch_active = nch_active

   call create_(nch_active,ErrorCov)

!  Read GSI-like channel numbers used in estimating R for this instrument
   read(lu,IOSTAT=ioflag) ErrorCov%indxR
   if(ioflag/=0) call die(myname_,' failed to read indx from '//trim(fname))

!  Read estimate of observation error covariance
   allocate(readR(nch_active,nch_active))
   read(lu,IOSTAT=ioflag) readR
   if(ioflag/=0) call die(myname_,' failed to read R from '//trim(fname))
   ErrorCov%R = readR
   deallocate(readR)

!  Done reading file
   close(lu)

!  If method<0 there is really nothing else to do
!  ----------------------------------------------
   if (method<0) then
      initialized_=.true.
      return
   endif

   if (VERBOSE_) then
       allocate(diag(nch_active))
       do ii=1,nch_active
          diag(ii)=ErrorCov%R(ii,ii)
       enddo
       if(iamroot_) then
          write(6,'(2a)') 'Rcov(stdev) for instrument: ', trim(ErrorCov%name)
          write(6,'(9(1x,es10.3))') sqrt(diag)
          write(6,'(3a)') 'Channels used in estimating Rcov(', trim(ErrorCov%name), ')'
          write(6,'(12(1x,i5))') ErrorCov%indxR
       endif
       deallocate(diag)
   endif

!  Now decompose R
   call solver_(ErrorCov)

   if (VERBOSE_ .and. ErrorCov%method>=0) then
       allocate(diag(nch_active))
       do ii=1,nch_active
          diag(ii)=ErrorCov%R(ii,ii)
       enddo
       if(iamroot_) then
          write(6,'(3a)') 'Rcov(stdev) for instrument: ', trim(ErrorCov%name), ' recond'
          write(6,'(9(1x,es10.3))') sqrt(diag)
       endif
       deallocate(diag)
   endif

   initialized_=.true.
end subroutine set_
!EOC

!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  create_ --- creates type to hold observation error covariance
!
! !INTERFACE:
!
subroutine create_ (nch,ErrorCov)
implicit none
! !INPUT PARAMETERS:
integer(i_kind),intent(in) :: nch
! !INPUT/OUTPUT PARAMETERS:
type(ObsErrorCov) :: ErrorCov
! !DESCRIPTION: Allocates space for FORTRAN type hold observation error
!               covariance and required information.
!
! !REVISION HISTORY:
!   2014-04-13  todling  initial code
!
! !REMARKS:
!   language: f90
!   machine:  discover
!
! !AUTHOR:
!   Ricardo Todling  org: gmao      date: 2014-04-13
!
!EOP
!-------------------------------------------------------------------------
!BOC
   allocate(ErrorCov%R(nch,nch))
   allocate(ErrorCov%indxR(nch))
   allocate(ErrorCov%Revals(nch),ErrorCov%Revecs(nch,nch))
end subroutine create_
!EOC

!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  destroy_ --- destroy type holding observation error covariance
!
! !INTERFACE:
!
subroutine destroy_ (ErrorCov)
implicit none
! !INPUT/OUTPUT PARAMETERS:
type(ObsErrorCov) :: ErrorCov
! !DESCRIPTION: Deallocates space held for observation error covariance.
!
! !REVISION HISTORY:
!   2014-04-13  todling  initial code
!
! !REMARKS:
!   language: f90
!   machine:  discover
!
! !AUTHOR:
!   Ricardo Todling  org: gmao      date: 2014-04-13
!
!EOP
!-------------------------------------------------------------------------
!BOC
   deallocate(ErrorCov%Revals,ErrorCov%Revecs)
   deallocate(ErrorCov%indxR)
   deallocate(ErrorCov%R)
end subroutine destroy_
!EOC


!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  solver_ --- entry-point to the decomposition of cov(R)
!
! !INTERFACE:
!

subroutine solver_(ErrorCov)
implicit none
! !INPUT/OUTPUT PARAMETERS:
type(ObsErrorCov) :: ErrorCov

! !DESCRIPTION: This routine is the entry point to the eigen-decomposition
!               of the obs error covariance. Depending on the method chosen
!               by the user, it might call the proper routines to recondition
!               the offline estimate of cov(R).
!
! !REVISION HISTORY:
!   2014-04-13  todling  initial code
!
! !REMARKS:
!   language: f90
!   machine:  discover
!
! !AUTHOR:
!   Ricardo Todling  org: gmao      date: 2014-04-13
!
!EOP
!-------------------------------------------------------------------------
!BOC
character(len=*), parameter :: myname_=myname//'*solver_'
real(r_kind) lambda_max,lambda_min,lambda_inc
integer(i_kind) ii,jj,ndim
logical adjspec
real(r_kind),allocatable,dimension(:):: invstd

ndim = size(ErrorCov%R,1)
 
! This extracts the diagonal of R (error variances), setting the 
! eigenvalues as such and the eigenvectors as the unit vectors
! This is to allow using the estimated error variances, but
! but still pretend the covariance is diagnoal - no correlations.
! This is largely for testing consistency of the implementation.
if ( ErrorCov%method==0 ) then
   ErrorCov%Revecs = zero
   do ii=1,ndim
      ErrorCov%Revals(ii)    = ErrorCov%R(ii,ii) 
      ErrorCov%Revecs(ii,ii) = one
   enddo
   call westonEtAl_spectrum_boost_(adjspec)
   if (adjspec) then
      call rebuild_rcov_
   endif
endif ! method=0

! This does the actual full eigendecomposition of the R matrix
! Here, recondioning is of correlation matrix
if ( ErrorCov%method==1 ) then
   ! reduce R to correlation matrix
   allocate(invstd(ndim))
   do jj=1,ndim
      invstd(jj) = one/sqrt(ErrorCov%R(jj,jj))
   enddo
   do jj=1,ndim
      do ii=1,ndim
         ErrorCov%R(ii,jj) = invstd(ii)*ErrorCov%R(ii,jj)*invstd(jj)
      enddo
   enddo
   deallocate(invstd)
   ErrorCov%Revecs=ErrorCov%R
   call decompose_(trim(ErrorCov%name),ErrorCov%Revals,ErrorCov%Revecs,ndim,.true.)
   call westonEtAl_spectrum_boost_(adjspec)
   if (adjspec) then
      call rebuild_rcov_
      allocate(invstd(ndim))
      do jj=1,ndim
         invstd(jj) = one/sqrt(ErrorCov%R(jj,jj))
      enddo
      do jj=1,ndim
         do ii=1,ndim
            ErrorCov%R(ii,jj) = invstd(ii)*ErrorCov%R(ii,jj)*invstd(jj)
         enddo
      enddo
      deallocate(invstd)
   endif
endif ! method=1

! This does the actual full eigendecomposition of the R matrix
! Here, recondioning is of covariance matrix
if ( ErrorCov%method==2 ) then
   ErrorCov%Revecs=ErrorCov%R
   call decompose_(trim(ErrorCov%name),ErrorCov%Revals,ErrorCov%Revecs,ndim,.true.)
   call westonEtAl_spectrum_boost_(adjspec)
   if (adjspec) then
      call rebuild_rcov_
   endif
endif ! method=2

! In this case, we can wipe out the eigen-decomp since it will be redone for
! each profile at each location at setup time.
if ( ErrorCov%method==2 ) then
   ErrorCov%Revals=zero
   ErrorCov%Revecs=zero
endif

  contains
  subroutine westonEtAl_spectrum_boost_(adjspec)
    implicit none
    logical,intent(out) :: adjspec
    adjspec=.false.
    if(ErrorCov%kreq < zero) return
    lambda_max=maxval(ErrorCov%Revals)
    lambda_min=minval(ErrorCov%Revals)
    lambda_inc=(lambda_max - (lambda_min * ErrorCov%kreq))/(ErrorCov%kreq-1)
    if(lambda_inc>zero) then
       ErrorCov%Revals = ErrorCov%Revals + lambda_inc
    else
       if (iamroot_) then
         write(6,'(2a,1x,es10.3)') myname_, ' Spectrum of cov(R) not changed, poor choice of kreq = ', &
                         ErrorCov%kreq
       endif
    endif
    adjspec=.true.
  end subroutine westonEtAl_spectrum_boost_
  subroutine rebuild_rcov_
  integer(i_kind) ii,jj,kk
  real(r_kind), allocatable, dimension(:,:) :: tmp
  allocate(tmp(ndim,ndim))
  ! D*U^T
  do jj=1,ndim
     tmp(:,jj) = ErrorCov%Revals(:) * ErrorCov%Revecs(jj,:)
  enddo
  ! U*(D*U^T)
  ErrorCov%R = matmul(ErrorCov%Revecs,tmp)
  ErrorCov%Revecs =ErrorCov%R
  call decompose_(trim(ErrorCov%name),ErrorCov%Revals,ErrorCov%Revecs,ndim,.true.)
  ! clean up
  deallocate(tmp)
  end subroutine rebuild_rcov_
end subroutine solver_
!EOC


!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  decompose_ --- calculates eigen-decomposition of cov(R)
!
! !INTERFACE:
!
subroutine decompose_(instrument,Evals,Evecs,ndim,lprt)
! !USES:
use constants, only: tiny_r_kind
  implicit none
! !INPUT PARAMETERS:
  character(len=*),intent(in):: instrument
  integer(i_kind),intent(in) :: ndim
  logical,intent(in)         :: lprt
! !INPUT/OUTPUT PARAMETERS:
  real(r_kind),intent(inout) :: Evals(:)
  real(r_kind),intent(inout) :: Evecs(:,:) ! on entry: matrix to decompose
                                           ! on exit: eigenvectors

! !DESCRIPTION: This routine makes a LAPACK call to eigen-decompose cov(R).
!               Its initial implementation is the crudest possible; it does
!               not make use of the fact that only the upper or lower triangles
!               of the matrix are needed; the problems solver are so small that
!               at present this does not seem to be an issue; this could be 
!               easily revisited in the future.
!
! !REVISION HISTORY:
!   2014-04-13  todling  initial code
!
! !REMARKS:
!   language: f90
!   machine:  discover
!
! !AUTHOR:
!   Ricardo Todling  org: gmao      date: 2014-04-13
!
!EOP
!-------------------------------------------------------------------------
!BOC
  character(len=*),parameter :: myname_=myname//'decompose_'
  character*1 jobz
  integer(i_kind) lwork,info
  real(r_kind) lambda_max,lambda_min,cond
  real(r_kind),allocatable, dimension(:) :: work
  jobz = 'V' ! evals & evecs
  lwork = max(1,3*ndim-1)
  allocate(work(lwork))
  if(r_kind==r_single) then ! this trick only works because this uses the f77 lapack interfaces
     call SSYEV( jobz, 'U', ndim, Evecs, ndim, Evals, WORK, lwork, info )
  else if(r_kind==r_double) then
     call DSYEV( jobz, 'U', ndim, Evecs, ndim, Evals, WORK, lwork, info )
  else
     call die(myname_,'no corresponding LAPACK call for solving eigenproblem')
  endif
  if (info==0) then
     if (lprt) then
        cond=-999._r_kind
        lambda_max=maxval(Evals)
        lambda_min=minval(Evals)
        if(lambda_min>tiny_r_kind) cond=abs(lambda_max/lambda_min) ! formal definition (lambda>0 for SPD matrix)
        if (iamroot_) then
           write(6,'(2a,1x,a,1x,es10.3)') 'Rcov(Evals) for Instrument: ', trim(instrument), ' cond= ', cond
           write(6,'(9(1x,es10.3))') Evals
        endif
     endif
  else
     call die(myname_,'trouble solving eigenproblem')
  endif
  deallocate(work)
end subroutine decompose_
!EOC

!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  scale_jac_ ---  scale Jacbian, residuals, and related errors
!
! !INTERFACE:
!
logical function scale_jac_(depart,obvarinv,adaptinf,jacobian, &
                            nchanl,jpch_rad,varinv,iuse,ich,ErrorCov)
! !USES:
use constants, only: tiny_r_kind
use mpeu_util, only: die
implicit none
! !INPUT PARAMETERS:
integer(i_kind),intent(in) :: nchanl   ! total number of channels in instrument
integer(i_kind),intent(in) :: jpch_rad ! total number of channels in GSI
integer(i_kind),intent(in) :: ich(:)   ! true channel numeber
integer(i_kind),intent(in) :: iuse(0:jpch_rad) ! flag indicating whether channel used or not
real(r_kind),   intent(in) :: varinv(:)    ! inverse of specified ob-error-variance 
! !INPUT/OUTPUT PARAMETERS:
real(r_kind),intent(inout) :: depart(:)    ! observation-minus-guess departure
real(r_kind),intent(inout) :: obvarinv(:)  ! inverse of eval(diag(R))
real(r_kind),intent(inout) :: adaptinf(:)  ! stdev error
real(r_kind),intent(inout) :: jacobian(:,:)! Jacobian matrix
type(ObsErrorCov) :: ErrorCov              ! ob error covariance for given instrument

! !DESCRIPTION: This routine is the main entry-point to the outside world. 
!               It redefines the Jacobian matrix so it embeds the eigen-vectors
!               of the observation error covariance. Only the sub-matrix related
!               to the active and accepted channels in the given profile is 
!               taken into account.
!
! !REVISION HISTORY:
!   2014-04-13  todling  initial code
!
! !REMARKS:
!   language: f90
!   machine:  discover
!
! !AUTHOR:
!   Ricardo Todling  org: gmao      date: 2014-04-13
!
!EOP
!-------------------------------------------------------------------------
!BOC

character(len=*),parameter :: myname_=myname//'*scale_jac'
integer(i_kind) :: nch_active,ii,jj,iii,jjj,mm,nn,ncp,ifound,nsigjac
integer(i_kind),allocatable,dimension(:)   :: ircv
integer(i_kind),allocatable,dimension(:)   :: ijac
integer(i_kind),allocatable,dimension(:)   :: IRsubset
integer(i_kind),allocatable,dimension(:)   :: IJsubset
real(r_kind),   allocatable,dimension(:)   :: adapt
real(r_kind),   allocatable,dimension(:)   :: col
real(r_kind),   allocatable,dimension(:,:) :: row
real(r_kind) coeff
logical subset

scale_jac_=.false.
nch_active=ErrorCov%nch_active
if(nch_active<0) return

call timer_ini('scljac')

! get indexes for the internal channels matching those
! used in estimating the observation error covariance
allocate(ircv(nchanl))
allocate(ijac(nchanl))
allocate(adapt(nchanl))
ircv = -1
ijac = -1
adapt=zero
coeff=one
if(ErrorCov%method==1.and.ErrorCov%kreq>zero) then
  coeff=100._r_kind/ErrorCov%kreq
endif
do jj=1,nchanl
   mm=ich(jj)       ! true channel number (has no bearing here except in iuse)
   if (varinv(jj)>tiny_r_kind .and. iuse(mm)>=1) then
      ifound=-1
      do ii=1,nch_active
         if(jj==ErrorCov%indxR(ii)) then
            ifound=ii       
            exit
         endif
      enddo
      if(ifound/=-1) then
         ijac(jj)=jj      ! index value applies to the jacobian and departure
         ircv(jj)=ifound  ! index value applies to ErrorCov
      endif
      if(ErrorCov%method==1) then
         adapt(jj) = coeff/adaptinf(jj)
      else
         adapt(jj) = one
      endif
   endif
enddo
ncp=count(ircv>0) ! number of active channels in profile
! following should never happen, but just in case ...
if(ncp==0 .or. ncp>ErrorCov%nch_active) then
   call die(myname_,'serious inconsitency in handling correlated obs')
endif
! Get subset indexes; without QC and other on-the-fly analysis choices these
!                     two indexes would be the same, but because the analysis
!                     remove data here and there, most often there will be less
!                     channels being processed for a given profile than the set
!                     of active channels used to get an offline estimate of R.
allocate(IRsubset(ncp)) ! these indexes apply to the matrices/vec in ErrorCov
allocate(IJsubset(ncp)) ! these indexes apply to the Jacobian/departure 
iii=0;jjj=0
do ii=1,nchanl
   if(ircv(ii)>0) then
      iii=iii+1
      IRsubset(iii)=ircv(ii)  ! subset indexes in R presently in use
   endif
   if(ijac(ii)>0) then
      jjj=jjj+1
      IJsubset(iii)=ijac(ii)  ! subset indexes in Jac/dep presently in use
   endif
enddo
if (iii/=ncp) then
   if (iamroot_) then
       write(6,'') myname, ' iii,ncp= ',iii,ncp
   endif
   call die(myname_,' serious dimensions insconsistency (R), aborting')
endif
if (jjj/=ncp) then
   if (iamroot_) then
       write(6,'') myname, ' jjj,ncp= ',jjj,ncp
   endif
   call die(myname_,' serious dimensions insconsistency (J), aborting')
endif

! decompose the sub-matrix - returning the result in the 
!                            structure holding the full covariance
if( ErrorCov%method==2 ) then
   subset = decompose_subset_ (IRsubset,ErrorCov)
   if(.not.subset) then
      call die(myname_,' failed to decompose correlated R')
   endif
endif

if( ErrorCov%method<0 ) then
!  Keep departures and Jacobian unchanged
!  Do as GSI would do otherwise
   do jj=1,ncp
      mm=IJsubset(jj)
      obvarinv(mm) = one/adaptinf(mm)**2
      adaptinf(mm) = adaptinf(mm)**2*varinv(mm)
   enddo
else
   nsigjac=size(jacobian,1)
!  Multiply Jacobian with matrix of eigenvectors
!  Multiply departure with "right" eigenvectors
   allocate(row(nsigjac,ncp))
   allocate(col(ncp))
   row=zero
   col=zero
   do ii=1,ncp
      do jj=1,ncp
         nn=IJsubset(jj)
         col(ii)   = col(ii)   + ErrorCov%Revecs(IRsubset(ii),IRsubset(jj)) * adapt(nn) * depart(nn)
         row(:,ii) = row(:,ii) + ErrorCov%Revecs(IRsubset(ii),IRsubset(jj)) * adapt(nn) * jacobian(:,nn)
      enddo
   enddo

!  Place Jacobian and departure in output arrays
   do jj=1,ncp
      mm=IJsubset(jj)
      depart(mm)=col(jj)
      jacobian(:,mm)=row(:,jj)
      obvarinv(mm) = one/ErrorCov%Revals(IRsubset(jj))
      if(ErrorCov%method==1) then
         adaptinf(mm) = one
      else
         adaptinf(mm) = adapt(mm)
      endif
   enddo
   deallocate(col)
   deallocate(row)
endif

! clean up
deallocate(IJsubset)
deallocate(IRsubset)
deallocate(adapt)
deallocate(ijac)
deallocate(ircv)

call timer_fnl('scljac')
scale_jac_=.true.
end function scale_jac_
!EOC

!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  decompose_subset_ --- extract subset cov(R) and decompose it
!
! !INTERFACE:
!

! !DESCRIPTION: Given an index-set of channels really operative in a given
!               profile, this routine extracts those rows and columns from
!               the offline estimate of cov(R), creating a subset(cov(R))
!               that is eigen-decomposed. The resulting partial eigen-
!               decomposition is stored back in the corresponding 
!               rows and columns of the temporary space in ObErroCov
!               responsible for holding the eigen-values/vectors. 
!
! !REVISION HISTORY:
!   2014-04-13  todling  initial code
!
! !REMARKS:
!   language: f90
!   machine:  discover
!
! !AUTHOR:
!   Ricardo Todling  org: gmao      date: 2014-04-13
!
!EOP
!-------------------------------------------------------------------------
!BOC
logical function decompose_subset_ (Isubset,ErrorCov)
implicit none
! in this approach, we take only the rows and columns of R related 
! to the channels used, and eigendecompose them ... instead of 
! eigendecomposing once, which I think ends up leading to the wrong
! mixt of eigenvalues and eigenvectors.
integer(i_kind),intent(in) :: Isubset(:)
type(ObsErrorCov) :: ErrorCov

character(len=*), parameter :: myname_=myname//'*subset_'
real(r_kind),allocatable,dimension(:)   :: Evals
real(r_kind),allocatable,dimension(:,:) :: Evecs
integer(i_kind) ii,jj,ncp

decompose_subset_=.false. 
ncp=size(Isubset) ! number of channels actually used in this profile
allocate(Evals(ncp),Evecs(ncp,ncp))

! extract subcomponent of R
!Evecs = ErrorCov%R(Isubset,Isubset)
do jj=1,ncp
   do ii=1,ncp
      Evecs(ii,jj) = ErrorCov%R(Isubset(ii),Isubset(jj))
   enddo
enddo
! decompose subset matrix
call decompose_(ErrorCov%instrument,Evals,Evecs,ncp,.false.)
! copy decomposition onto ErrorCov
do jj=1,ncp
   do ii=1,ncp
      ErrorCov%Revecs(Isubset(ii),Isubset(jj)) = Evecs(ii,jj)
   enddo
   ErrorCov%Revals(Isubset(jj)) = Evals(jj)
enddo
! clean up
deallocate(Evals,Evecs)

decompose_subset_=.true.
end function decompose_subset_
!EOC

!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  amIset_ --- checks whether a particular cov(R) has been set or not
!
! !INTERFACE:
!

logical function amIset_ (ErrorCov)
implicit none
! !INPUT/OUTPUT PARAMETERS:
type(ObsErrorCov) :: ErrorCov

! !DESCRIPTION: This routine returns the status of a particular instance of 
!               the FORTRAN typing holding the observation error covariance.
!
! !REVISION HISTORY:
!   2014-04-13  todling  initial code
!
! !REMARKS:
!   language: f90
!   machine:  discover
!
! !AUTHOR:
!   Ricardo Todling  org: gmao      date: 2014-04-13
!
!EOP
!-------------------------------------------------------------------------
!BOC
logical failed
failed=.false.
amIset_=.false.
if(ErrorCov%nch_active<0) failed=.true.
if(.not.associated(ErrorCov%indxR)) failed=.true.
if(.not.associated(ErrorCov%R)) failed=.true.
if(.not.associated(ErrorCov%REvals)) failed=.true.
if(.not.associated(ErrorCov%REvecs)) failed=.true.
if(.not.failed) amIset_=.true.
end function amIset_
!EOC

!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  fnl_ --- destroy all instances of cov(R)
!
! !INTERFACE:
!
subroutine fnl_
implicit none

! !DESCRIPTION: Deallocates space held for observation error covariance.
!
! !REVISION HISTORY:
!   2014-04-13  todling  initial code
!
! !REMARKS:
!   language: f90
!   machine:  discover
!
! !AUTHOR:
!   Ricardo Todling  org: gmao      date: 2014-04-13
!
!EOP
!-------------------------------------------------------------------------
!BOC
integer(i_kind) ii,ndim
if(.not.initialized_) return
ndim=size(GSI_BundleErrorCov)
do ii=1,ndim
   call destroy_(GSI_BundleErrorCov(ii))
enddo
deallocate(GSI_BundleErrorCov)
if(allocated(idnames)) deallocate(idnames)
if(allocated(instruments)) deallocate(instruments)
end subroutine fnl_
!EOC

end module correlated_obsmod
