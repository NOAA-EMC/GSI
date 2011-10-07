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
!   2010-04-30  wu - setup for regional ozone analysis
!   2010-05-13  todling - update to use gsi_bundle
!   2010-06-01  todling - skip upd when pointer not defined
!   2010-06-02  todling - bug in upd of chem
!   2010-11-02  ting - replace loop index k in nfldsfc loop with it (bug fix)
!   2011-02-25  zhu  - add gust,vis,pblh
!   2010-05-01  todling - add support for generalized guess (use met-guess)
!                       - cwmr now in met-guess
!   2011-06-29  todling - no explict reference to internal bundle arrays
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
  use constants, only: zero,one,fv,max_varname_length
  use jfunc, only: iout_iter,biascor,tsensible
  use gridmod, only: lat2,lon2,nsig,&
       regional,twodvar_regional,regional_ozone
  use guess_grids, only: ges_div,ges_vor,ges_ps,ges_tv,ges_q,&
       ges_tsen,ges_oz,ges_u,ges_v,ges_gust,ges_vis,ges_pblh,&
       nfldsig,hrdifsig,hrdifsfc,nfldsfc,dsfct
  use state_vectors, only: svars3d,svars2d
  use xhat_vordivmod, only: xhat_vor,xhat_div
  use gsi_4dvar, only: nobs_bins, hr_obsbin
  use radinfo, only: npred,jpch_rad,predx
  use pcpinfo, only: npredp,npcptype,predxp
  use m_gsiBiases,only : bias_hour, update_bias
  use bias_predictors, only: predictors
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use gsi_bundlemod, only: gsi_bundlegetvar
  use gsi_metguess_mod, only: gsi_metguess_bundle
  use gsi_metguess_mod, only: gsi_metguess_get
  use gsi_chemguess_mod, only: gsi_chemguess_bundle
  use gsi_chemguess_mod, only: gsi_chemguess_get
  use mpeu_util, only: getindex

  implicit none

! Declare passed variables
  type(gsi_bundle), intent(inout) :: sval(nobs_bins)
  type(predictors), intent(inout) :: sbias

! Declare local variables
  character(max_varname_length),allocatable,dimension(:) :: gases
  character(max_varname_length),allocatable,dimension(:) :: guess
  character(max_varname_length),allocatable,dimension(:) :: cloud
  integer(i_kind) i,j,k,it,ij,ii,ic,id,ngases,nguess,istatus
  integer(i_kind) is_u,is_v,is_t,is_q,is_oz,is_cw,is_ps,is_sst
  integer(i_kind) is_gust,is_vis,is_pblh
  integer(i_kind) ipinc,ipges,icloud,ncloud
  real(r_kind) :: zt
  real(r_kind),pointer,dimension(:,:  ) :: ptr2dinc=>NULL()
  real(r_kind),pointer,dimension(:,:  ) :: ptr2dges=>NULL()
  real(r_kind),pointer,dimension(:,:,:) :: ptr3dinc=>NULL()
  real(r_kind),pointer,dimension(:,:,:) :: ptr3dges=>NULL()
  real(r_kind),pointer,dimension(:,:,:) :: p_u     =>NULL()
  real(r_kind),pointer,dimension(:,:,:) :: p_v     =>NULL()
  real(r_kind),pointer,dimension(:,:,:) :: p_q     =>NULL()
  real(r_kind),pointer,dimension(:,:,:) :: p_tv    =>NULL()
  real(r_kind),pointer,dimension(:,:,:) :: p_oz    =>NULL()
  real(r_kind),pointer,dimension(:,:  ) :: ptr2daux=>NULL()

!*******************************************************************************
! In 3dvar, nobs_bins=1 is smaller than nfldsig. This subroutine is
! written in a way that is more efficient in that case but might not
! be the best in 4dvar.

! Get required pointers and abort if not found (RTod: needs revision)
  call gsi_bundlegetpointer(sval(1),'u',  is_u,  istatus)
  call gsi_bundlegetpointer(sval(1),'v',  is_v,  istatus)
  call gsi_bundlegetpointer(sval(1),'tv', is_t,  istatus)
  call gsi_bundlegetpointer(sval(1),'q',  is_q,  istatus)
  call gsi_bundlegetpointer(sval(1),'oz', is_oz, istatus)
  call gsi_bundlegetpointer(sval(1),'cw', is_cw, istatus)
  call gsi_bundlegetpointer(sval(1),'ps', is_ps, istatus)
  call gsi_bundlegetpointer(sval(1),'sst',is_sst,istatus)
  call gsi_bundlegetpointer(sval(1),'gust',is_gust,istatus)
  call gsi_bundlegetpointer(sval(1),'vis',is_vis,istatus)
  call gsi_bundlegetpointer(sval(1),'pblh',is_pblh,istatus)

! Inquire about guess fields
  call gsi_metguess_get('dim',nguess,istatus)
  if (nguess>0) then
     allocate(guess(nguess))
     call gsi_metguess_get('gsinames',guess,istatus)
  endif

! Inquire about clouds
  call gsi_metguess_get('clouds::3d',ncloud,istatus)
  if (ncloud>0) then
     allocate(cloud(ncloud))
     call gsi_metguess_get('clouds::3d',cloud,istatus)
  endif

! Inquire about chemistry fields
  call gsi_chemguess_get('dim',ngases,istatus)
  if (ngases>0) then
     allocate(gases(ngases))
     call gsi_chemguess_get('gsinames',gases,istatus)
  endif

! Initialize local arrays
  if (regional) then
     if(is_cw>0)then
        do ii=1,nobs_bins
           call gsi_bundlegetpointer (sval(ii),'cw',ptr3dinc,istatus)
           do k=1,nsig
              do j=1,lon2
                 do i=1,lat2
                    ptr3dinc(i,j,k)=zero
                 end do
              end do
           end do
        end do
     endif
     if(.not.regional_ozone) then
        if(is_oz>0)then
           do ii=1,nobs_bins
              call gsi_bundlegetpointer (sval(ii),'oz',ptr3dinc,istatus)
              do k=1,nsig
                 do j=1,lon2
                    do i=1,lat2
                       ptr3dinc(i,j,k)=zero
                    end do
                 end do
              end do
           end do
        endif
     endif
  endif

! Add increment to background
  do it=1,nfldsig
     if (nobs_bins>1) then
        zt = hrdifsig(it)
        ii = NINT(zt/hr_obsbin)+1
     else
        ii = 1
     endif
     call gsi_bundlegetpointer (sval(ii),'u' ,p_u ,istatus)
     call gsi_bundlegetpointer (sval(ii),'v' ,p_v ,istatus)
     call gsi_bundlegetpointer (sval(ii),'q' ,p_q ,istatus)
     call gsi_bundlegetpointer (sval(ii),'tv',p_tv,istatus)
     call gsi_bundlegetpointer (sval(ii),'oz',p_oz,istatus)
     do k=1,nsig
        do j=1,lon2
           do i=1,lat2
              if(is_u>0) ges_u(i,j,k,it) =     ges_u(i,j,k,it)    + p_u(i,j,k)
              if(is_v>0) ges_v(i,j,k,it) =     ges_v(i,j,k,it)    + p_v(i,j,k)
              if(is_q>0) ges_q(i,j,k,it) = max(ges_q(i,j,k,it)    + p_q(i,j,k),1.e-10_r_kind) 
              if(is_t > 0) then
                 if (.not.twodvar_regional .or. .not.tsensible) then
                    ges_tv(i,j,k,it)   = ges_tv(i,j,k,it)   + p_tv(i,j,k)
!  produce sensible temperature
                    ges_tsen(i,j,k,it) = ges_tv(i,j,k,it)/(one+fv*ges_q(i,j,k,it))
                 else
                    ges_tsen(i,j,k,it) = ges_tsen(i,j,k,it) + p_tv(i,j,k)
!  produce virtual temperature
                    ges_tv(i,j,k,it)   = ges_tsen(i,j,k,it)*(one+fv*ges_q(i,j,k,it))
                 endif
              endif

!             Note:  Below variables only used in NCEP GFS model
              if(is_oz>0) ges_oz(i,j,k,it)  = ges_oz(i,j,k,it)   + p_oz(i,j,k)
                          ges_div(i,j,k,it) = ges_div(i,j,k,it)  + xhat_div(i,j,k,ii)
                          ges_vor(i,j,k,it) = ges_vor(i,j,k,it)  + xhat_vor(i,j,k,ii)
           end do
        end do
     end do
     if(is_ps>0) then
        call gsi_bundlegetpointer (sval(ii),'ps',ptr2dinc,istatus)
        do j=1,lon2
           do i=1,lat2
              ges_ps(i,j,it) = ges_ps(i,j,it) + ptr2dinc(i,j)
           end do
        end do
     endif

!    Update extra met-guess fields
     do ic=1,nguess
        id=getindex(svars3d,guess(ic))
        if (id>0) then
           call gsi_bundlegetpointer (sval(ii),               guess(ic),ptr3dinc,istatus)
           call gsi_bundlegetpointer (gsi_metguess_bundle(it),guess(ic),ptr3dges,istatus)
           ptr3dges = ptr3dges + ptr3dinc
           icloud=getindex(cloud,guess(ic))
           if(icloud>0) then
              ptr3dges = max(ptr3dges,1.e-10_r_kind)
           endif
        endif
        id=getindex(svars2d,guess(ic))
        if (id>0) then
           call gsi_bundlegetpointer (sval(ii),               guess(ic),ptr2dinc,istatus)
           call gsi_bundlegetpointer (gsi_metguess_bundle(it),guess(ic),ptr2dges,istatus)
           ptr2dges = ptr2dges + ptr2dinc
        endif
     enddo

!    Update trace gases
     do ic=1,ngases
        id=getindex(svars3d,gases(ic))
        if (id>0) then
           call gsi_bundlegetpointer (sval(ii),                gases(ic),ptr3dinc,istatus)
           call gsi_bundlegetpointer (gsi_chemguess_bundle(it),gases(ic),ptr3dges,istatus)
           ptr3dges = ptr3dges + ptr3dinc
        endif
        id=getindex(svars2d,gases(ic))
        if (id>0) then
           call gsi_bundlegetpointer (sval(ii),                gases(ic),ptr2dinc,istatus)
           call gsi_bundlegetpointer (gsi_chemguess_bundle(it),gases(ic),ptr2dges,istatus)
           ptr2dges = ptr2dges + ptr2dinc
        endif
     enddo

     if (twodvar_regional) then
        if(is_gust>0) then
           call gsi_bundlegetpointer (sval(ii),'gust',ptr2daux,istatus)
           do j=1,lon2
              do i=1,lat2
                 ges_gust(i,j,it) = ges_gust(i,j,it) + ptr2daux(i,j)
                 ges_gust(i,j,it) = max(zero,ges_gust(i,j,it))
              end do
           end do
        endif
        if(is_vis>0) then
           call gsi_bundlegetpointer (sval(ii),'vis',ptr2daux,istatus)
           do j=1,lon2
              do i=1,lat2
                 ges_vis(i,j,it) = ges_vis(i,j,it) + ptr2daux(i,j)
                 if (ges_vis(i,j,it)<=zero) ges_vis(i,j,it)=1.0e-4_r_kind
                 if (ges_vis(i,j,it)>20000.0_r_kind) ges_vis(i,j,it)=20000.0_r_kind
              end do
           end do
        endif
        if(is_pblh>0) then
           call gsi_bundlegetpointer (sval(ii),'pblh',ptr2daux,istatus)
           do j=1,lon2
              do i=1,lat2
                 ges_pblh(i,j,it) = ges_pblh(i,j,it) + ptr2daux(i,j)
                 ges_pblh(i,j,it) = max(zero,ges_pblh(i,j,it))
              end do
           end do
        endif
     end if
  end do

  if(ngases>0)then
     deallocate(gases)
  endif

  if(is_sst>0) then
     do it=1,nfldsfc
        if (nobs_bins>1) then
           zt = hrdifsfc(it)
           ii = NINT(zt/hr_obsbin)+1
        else
           ii = 1
        endif
        ij=0
        call gsi_bundlegetpointer (sval(ii),'sst',ptr2dinc,istatus)
        do j=1,lon2
           do i=1,lat2
              dsfct(i,j,it)=dsfct(i,j,it)+ptr2dinc(i,j)
           end do
        end do
     end do
  endif

  if (regional_ozone) then
     if(is_oz>0) then
        do it=1,nfldsig
           do k=1,nsig
              do j=1,lon2
                 do i=1,lat2
                    if(ges_oz(i,j,k,it)<zero) ges_oz(i,j,k,it) = 1.e-10_r_kind
                 enddo
              enddo
           enddo
        enddo
     endif
  endif

! If requested, update background bias correction
  if (biascor >= zero) then
     if (mype==0) write(iout_iter,*) &
        'UPDATE_GUESS:  update background bias correction.  biascor=',biascor

!    Update bias correction field

     call update_bias(sval(1),xhat_div(:,:,:,1),xhat_vor(:,:,:,1),hour=bias_hour)

  endif

 
! Update bias correction coefficients.
! Not necessary if running in 2dvar mode.

  if (.not.twodvar_regional) then

!    Satellite radiance biases
     ij=0
     do j=1,jpch_rad
        do i=1,npred
           ij=ij+1
           predx(i,j)=predx(i,j)+sbias%predr(ij)
        end do
     end do

!    Precipitation biases
     ij=0
     do j=1,npcptype
        do i=1,npredp
           ij=ij+1
           predxp(i,j)=predxp(i,j)+sbias%predp(ij)
        end do
     end do
  endif

  return
end subroutine update_guess
