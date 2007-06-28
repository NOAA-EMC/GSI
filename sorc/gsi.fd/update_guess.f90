subroutine update_guess(xhat,xhatuv,mype)
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
!            If the guess bias correction is turned on (biascor>0.0),
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
!   2006-07-28  derber  - include sensible temperature update
!   2006-07-31  kleist  - change to ps instead of ln(ps)
!
!   input argument list:
!     xhat     - analysis increment in grid space
!     xhatuv    - u,v increment in grid space
!     mype     - mpi task number
!
!   output argument list:
!
!   comments:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use mpimod, only: iscuv_s,ierror,mpi_comm_world,irduv_s,ircuv_s,&
       isduv_g,iscuv_g,nuvlevs,irduv_g,ircuv_g,mpi_rtype,isduv_s,&
       strip,reorder,reorder2
  use constants, only: zero, one, ozcon, fv
  use jfunc, only: ncw,nq,nt,iout_iter,biascor,np,noz,nclen,&
       nvp,nst,nuvlen,nu,nv,nsst,nclen1,nclen2
  use gridmod, only: lat1,lon1,lat2,lon2,itotsub,nsig,ltosi,ltosj,nlon,nlat,iglobal,&
       ltosi_s,ltosj_s,regional,twodvar_regional
  use guess_grids, only: ges_div,ges_vor,ges_ps,ges_cwmr,ges_tv,ges_q,&
       ges_tsen,ges_oz,ges_u,ges_v,bias_oz,bias_cwmr,bias_ps,bias_q,bias_vor,&
       bias_div,bias_tv,bias_u,bias_v,nfldsig,ntguessig,ges_prsi,&
       bias_tskin,nfldsfc,sfct
  use compact_diffs, only: uv2vordiv
  use gridmod,only : gmao_intfc
  use radinfo, only: npred,jpch,predx
  use pcpinfo, only: npredp,jtype,predxp

  implicit none

! Declare passed variables
  integer(i_kind),intent(in):: mype
  real(r_kind),dimension(nclen),intent(inout):: xhat
  real(r_kind),dimension(nuvlen),intent(in):: xhatuv

! Declare local variables
  integer(i_kind) i,j,k,it,ij,ijk,i2,i2m1,ni1,ni2,kk
  real(r_kind),dimension(lat1,lon1,nsig):: usm,vsm
  real(r_kind),dimension(lat2,lon2,nsig):: xhat_vor,xhat_div,xhat_q
  real(r_kind),dimension(itotsub,nuvlevs):: work1,work2
  real(r_kind),dimension(nlon,nlat):: grid_vor,grid_div

!*******************************************************************************
! Initialize local arrays
  do k=1,nsig
     do j=1,lon2
        do i=1,lat2
           xhat_vor(i,j,k) = zero
           xhat_div(i,j,k) = zero
        end do
     end do
  end do

  if (regional) then
     ijk=-1
     do k=1,nsig
        do j=1,lon2
           do i=1,lat2
              ijk=ijk+1
              xhat(noz+ijk)=zero
              xhat(ncw+ijk)=zero
           end do
        end do
     end do
  endif
              


! Convert ozone units (on subdomains) from the analysis units 
! to the guess (model) units.
  ijk=-1
  do k=1,nsig
     do j=1,lon2
        do i=1,lat2
           ijk=ijk+1
           xhat(noz+ijk)=xhat(noz+ijk)*ozcon/&
                (ges_prsi(i,j,k,ntguessig)-ges_prsi(i,j,k+1,ntguessig))
        end do
     end do
  end do


! The GSI analyzes stream function (sf) and velocity potential (vp).  
! Wind field observations are in terms of zonal (u) and meridional 
! (v) wind components or wind speed.  Thus, the GSI carries wind 
! increments in terms of both u,v and sf,vp.  
!
! The NCEP GFS (global) model uses vorticity and divergence as
! wind field variable.  The code below converts increments in 
! u and v to those in vorticity and divergence.  The wind variables
! in the NCEP regional model are u and v.  Hence, the block of code
! below is only used for the NCEP GFS (.not.regional). 

! Other users may need to change the logical below to obtain the
! proper behavior for their specific guess (model background)

! For NCEP GFS convert increment in u,v to increments in vor,div
  if (.not.regional) then

!    NCEP GFS interface
!    Zero work arrays
     do k=1,nuvlevs
        do j=1,itotsub
           work1(j,k)=zero
           work2(j,k)=zero
        end do
     end do
  
!    Strip off halo for u,v grids on subdomains
     call strip(xhatuv(nu),usm,nsig)
     call strip(xhatuv(nv),vsm,nsig)

!    Put u,v subdomains on global slabs
!    Note:  u --> work1, v --> work2
     call mpi_alltoallv(usm,iscuv_g,isduv_g,&
          mpi_rtype,work1,ircuv_g,irduv_g,mpi_rtype,&
          mpi_comm_world,ierror)
     call mpi_alltoallv(vsm,iscuv_g,isduv_g,&
          mpi_rtype,work2,ircuv_g,irduv_g,mpi_rtype,&
          mpi_comm_world,ierror)

!    Reorder work arrays before converting u,v to vor,div
     call reorder(work1,nuvlevs)
     call reorder(work2,nuvlevs)

!    Call u,v --> vor,div routine (conversion uses compact differences)
     do k=1,nuvlevs
        call uv2vordiv(work1(1,k),work2(1,k))
     end do

!    Reorder work arrays for mpi communication
     call reorder2(work1,nuvlevs)
     call reorder2(work2,nuvlevs)

!    Get vor,div on subdomains
!    Note:  work1 --> vor, work2 --> div
     call mpi_alltoallv(work1,iscuv_s,isduv_s,&
          mpi_rtype,xhat_vor(1,1,1),ircuv_s,irduv_s,mpi_rtype,&
          mpi_comm_world,ierror)
     call mpi_alltoallv(work2,iscuv_s,isduv_s,&
          mpi_rtype,xhat_div(1,1,1),ircuv_s,irduv_s,mpi_rtype,&
          mpi_comm_world,ierror)

! End of NCEP GFS block

  endif

  
! Obtain q from normalized rh
  call normal_rh_to_q(xhat(nq),xhat(nt),xhat(np),xhat_q)

! Add increment to background
  do it=1,nfldsig
     ijk=-1
     do k=1,nsig
        do j=1,lon2
           do i=1,lat2
              ijk=ijk+1
              ges_u(i,j,k,it)    = ges_u(i,j,k,it)    + xhatuv(nu+ijk)
              ges_v(i,j,k,it)    = ges_v(i,j,k,it)    + xhatuv(nv+ijk)
              ges_tv(i,j,k,it)   = ges_tv(i,j,k,it)   + xhat(nt+ijk)
              ges_q(i,j,k,it)    = ges_q(i,j,k,it)    + xhat_q(i,j,k)
!  produce sensible temperature
              ges_tsen(i,j,k,it) = ges_tv(i,j,k,it)/(one+fv*max(zero,ges_q(i,j,k,it)))

!             Note:  Below variables only used in NCEP GFS model
              ges_oz(i,j,k,it)   = ges_oz(i,j,k,it)   + xhat(noz+ijk)
              ges_cwmr(i,j,k,it) = ges_cwmr(i,j,k,it) + xhat(ncw+ijk)
              ges_div(i,j,k,it)  = ges_div(i,j,k,it)  + xhat_div(i,j,k)
              ges_vor(i,j,k,it)  = ges_vor(i,j,k,it)  + xhat_vor(i,j,k)
           end do
        end do
     end do
     ij=-1
     do j=1,lon2
        do i=1,lat2
           ij=ij+1
           ges_ps(i,j,it) = ges_ps(i,j,it) + xhat(np+ij)
        end do
     end do
  end do

  do k=1,nfldsfc
     ij=-1
     do j=1,lon2
        do i=1,lat2
           ij=ij+1
           sfct(i,j,k)=sfct(i,j,k)+xhat(nsst+ij)
        end do
     end do
  end do


! If requested, update background bias correction
  if(.not.gmao_intfc) then
  if (biascor > zero) then
     if (mype==0) write(iout_iter,*) &
          'UPDATE_GUESS:  update background bias correction.  biascor=',biascor

!    Update bias correction field     
     ijk=-1
     do k=1,nsig
        do j=1,lon2
           do i=1,lat2
              ijk=ijk+1
              bias_u(i,j,k)    = bias_u(i,j,k)    + biascor*xhatuv(nu+ijk)
              bias_v(i,j,k)    = bias_v(i,j,k)    + biascor*xhatuv(nv+ijk)
              bias_tv(i,j,k)   = bias_tv(i,j,k)   + biascor*xhat(nt+ijk)
              bias_q(i,j,k)    = bias_q(i,j,k)    + biascor*xhat_q(i,j,k)
              bias_oz(i,j,k)   = bias_oz(i,j,k)   + biascor*xhat(noz+ijk)
              bias_cwmr(i,j,k) = bias_cwmr(i,j,k) + biascor*xhat(ncw+ijk)
              bias_div(i,j,k)  = bias_div(i,j,k)  + biascor*xhat_div(i,j,k)
              bias_vor(i,j,k)  = bias_vor(i,j,k)  + biascor*xhat_vor(i,j,k)
           end do
        end do
     end do
     ij=-1
     do j=1,lon2
        do i=1,lat2
           ij=ij+1
           bias_ps(i,j) = bias_ps(i,j)  + biascor*xhat(np+ij)
           bias_tskin(i,j)= bias_tskin(i,j) + biascor*xhat(nsst+ij)
        end do
     end do
  endif
  endif

 
! Update bias correction coefficients.
! Not necessary if running in 2dvar mode.

  if (.not.twodvar_regional) then

!    Satellite radiance biases
     ij=nclen1
     do j=1,npred
        do i=1,jpch
           ij=ij+1
           predx(i,j)=predx(i,j)+xhat(ij)
        end do
     end do

!    Precipitation biases
     ij=nclen2
     do j=1,npredp
        do i=1,jtype
           ij=ij+1
           predxp(i,j)=predxp(i,j)+xhat(ij)
        end do
     end do
  endif

  return
end subroutine update_guess
