subroutine update_guess(sval,sbias)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    update_guess          add analysis increment to guess
!   prgmmr: parrish          org: np22                date: 1990-10-06
!
! abstract:  This routine adds the analysis increment from the inner 
!            loop to the guess.  For certain variables, a change in
!            in units or represenation is made.
!
!            For ozone, a change of units is made from the units used
!            in the minimization to those used in the guess.  Stream 
!            function and velocity potential are converted into 
!            vorticity and divergence, the guess variables.
!
!            If the guess bias correction is turned on (biascor>=0.0),
!            then use the analysis increment to adjust the bias 
!            correction fields 
!
! program history log:
!   1990-10-06  parrish - original code
!   1994-02-02  parrish
!   1997-12-03  yang,w. - original mpi code
!   1999-06-28  yang w. - second structure mpp version
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   1999-12-07  wu      - grid version
!   2003-10-31  kleist  - add hybrid and sigma vertical coordinate
!   2003-12-22  derber  
!   2004-01-15  parrish - unified grid version (regional mode added)
!   2004-05-15  treadon - remove spectral output, leave updated guess in grid space
!   2004-06-15  treadon - reformat documenation
!   2004-07-28  treadon - add only to module use, add intent in/out
!   2004-08-27  treadon - use splib routines for grid <---> spectral transforms
!   2004-10-15  kleist  - fix sign error in laplacian computation, use separate
!               u,v work vector to update ges u,v
!   2005-02-23  wu - obtain q from normalized rh (only active when qoption=2)
!   2005-03-25  treadon - replace spectral sf,vp --> vor,div conversion with
!                         compact differencing of u,v --> vor,div
!   2005-03-28  treadon - combine hopers.f90 and update_ggrid.f90 into single
!                         routine, update_guess.f90
!   2005-06-27  guo     - support for interface to GMAO gridded fields
!   2005-09-28  parrish - fix bug in zeroing of regional xhat(noz) & xhat(ncw) arrays
!   2005-12-01  guo     - replaced reshape() in ggDivo() through a pass-
!			  by-reference interface pbr_ggDivo().
!   2005-12-09  guo     - remove GMAO divr-vort computation code.  Use
!                         unified NCEP compact_diff procedures.
!   2006-02-02  treadon - replace prsi_oz with ges_prsi
!   2006-03-27  treadon - bug fix:  use xhat_q, not xhat(nq), to update bias_q
!   2006-04-05  treadon - add update to skin temperature and bias correction
!   2006-06-08  zhang,b - change "biascor>0" to "biascor>=0" for debug purposes
!   2006-06-10  zhang,b - add update_bias and m_gsiBiases reference
!   2006-07-28  derber  - include sensible temperature update
!   2006-07-31  kleist  - change to ps instead of ln(ps)
!   2006-12-13  todling - add brute-force lower bound for positive quantities
!   2007-04-13  tremolet - use state vectors
!   2007-04-24  tremolet - 4dvar version
!   2007-07-05  todling - moved vor/div calculation elsewhere
!   2007-05-30  h.liu - remove ozone units conversion
!   2008-10-10  derber  - flip indices for predx and predxp
!   2009-01-28  todling - remove reference to original GMAO interface
!   2009-07-08  pondeca - add logical 'tsensible' for use with 2dvar only
!
!   input argument list:
!    sval
!    sbias
!
!   output argument list:
!    sval
!    sbias
!
!   comments:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use mpimod, only: mype
  use constants, only: izero,ione,zero,one,fv
  use jfunc, only: iout_iter,biascor,tsensible
  use gridmod, only: lat2,lon2,nsig,&
       regional,twodvar_regional
  use guess_grids, only: ges_div,ges_vor,ges_ps,ges_cwmr,ges_tv,ges_q,&
       ges_tsen,ges_oz,ges_u,ges_v,nfldsig,hrdifsig,hrdifsfc,&
       nfldsfc,dsfct
  use xhat_vordivmod, only: xhat_vor,xhat_div
  use gsi_4dvar, only: nobs_bins, hr_obsbin
  use radinfo, only: npred,jpch_rad,predx
  use pcpinfo, only: npredp,npcptype,predxp
  use m_gsiBiases,only : bias_hour, update_bias
  use bias_predictors
  use state_vectors

  implicit none

! Declare passed variables
  type(state_vector), intent(inout) :: sval(nobs_bins)
  type(predictors)  , intent(inout) :: sbias

! Declare local variables
  integer(i_kind) i,j,k,it,ij,ijk,ii
  real(r_kind) :: zt

!*******************************************************************************
! In 3dvar, nobs_bins=1 is smaller than nfldsig. This subroutine is
! written in a way that is more efficient in that case but might not
! be the best in 4dvar.

! Initialize local arrays
  if (regional) then
   do ii=1,nobs_bins
     ijk=izero
     do k=1,nsig
        do j=1,lon2
           do i=1,lat2
              ijk=ijk+ione
              sval(ii)%oz(ijk)=zero
              sval(ii)%cw(ijk)=zero
           end do
        end do
     end do
   end do
  endif

! Add increment to background
  do it=1,nfldsig
     if (nobs_bins>ione) then
       zt = hrdifsig(it)
       ii = NINT(zt/hr_obsbin)+ione
     else
       ii = ione
     endif
     ijk=izero
     do k=1,nsig
        do j=1,lon2
           do i=1,lat2
              ijk=ijk+ione
              ges_u(i,j,k,it)    =                 ges_u(i,j,k,it)    + sval(ii)%u(ijk)
              ges_v(i,j,k,it)    =                 ges_v(i,j,k,it)    + sval(ii)%v(ijk)
              ges_q(i,j,k,it)    =             max(ges_q(i,j,k,it)    + sval(ii)%q(ijk),1.e-10_r_kind) 
              if (.not.twodvar_regional .or. .not.tsensible) then
                ges_tv(i,j,k,it)   =                ges_tv(i,j,k,it)   + sval(ii)%t(ijk)
!  produce sensible temperature
                ges_tsen(i,j,k,it) = ges_tv(i,j,k,it)/(one+fv*ges_q(i,j,k,it))
              else
                ges_tsen(i,j,k,it) =                ges_tsen(i,j,k,it)   + sval(ii)%t(ijk)
!  produce virtual temperature
                ges_tv(i,j,k,it)   = ges_tsen(i,j,k,it)*(one+fv*ges_q(i,j,k,it))
              endif

!             Note:  Below variables only used in NCEP GFS model
              ges_oz(i,j,k,it)   =                 ges_oz(i,j,k,it)   + sval(ii)%oz(ijk)
              ges_cwmr(i,j,k,it) =                 ges_cwmr(i,j,k,it) + sval(ii)%cw(ijk)
              ges_div(i,j,k,it)  =                 ges_div(i,j,k,it)  + xhat_div(i,j,k,ii)
              ges_vor(i,j,k,it)  =                 ges_vor(i,j,k,it)  + xhat_vor(i,j,k,ii)
           end do
        end do
     end do
     ij=izero
     do j=1,lon2
        do i=1,lat2
           ij=ij+ione
           ges_ps(i,j,it) = ges_ps(i,j,it) + sval(ii)%p(ij)
        end do
     end do
  end do

  do k=1,nfldsfc
     if (nobs_bins>ione) then
       zt = hrdifsfc(it)
       ii = NINT(zt/hr_obsbin)+ione
     else
       ii = ione
     endif
     ij=izero
     do j=1,lon2
        do i=1,lat2
           ij=ij+ione
           dsfct(i,j,k)=dsfct(i,j,k)+sval(ii)%sst(ij)
        end do
     end do
  end do


! If requested, update background bias correction
  if (biascor >= zero) then
     if (mype==izero) write(iout_iter,*) &
          'UPDATE_GUESS:  update background bias correction.  biascor=',biascor

!    Update bias correction field

     call update_bias(sval(1),xhat_div(:,:,:,1),xhat_vor(:,:,:,1),hour=bias_hour)

  endif

 
! Update bias correction coefficients.
! Not necessary if running in 2dvar mode.

  if (.not.twodvar_regional) then

!    Satellite radiance biases
     ij=izero
     do j=1,jpch_rad
        do i=1,npred
           ij=ij+ione
           predx(i,j)=predx(i,j)+sbias%predr(ij)
        end do
     end do

!    Precipitation biases
     ij=izero
     do j=1,npcptype
        do i=1,npredp
           ij=ij+ione
           predxp(i,j)=predxp(i,j)+sbias%predp(ij)
        end do
     end do
  endif

  return
end subroutine update_guess
