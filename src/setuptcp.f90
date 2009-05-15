subroutine setuptcp(lunin,mype,bwork,awork,nele,nobs,conv_diagsave)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    setuptcp                     setup tcpel data
!   prgmmr: kleist            org: np20                date: 2009-02-02
!
! abstract:  Setup routine for TC MSLP BOGUS
!
! program history log:
!   2009-02-02  kleist
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,r_single,r_double,i_kind
  use obsmod, only: tcptail,tcphead,obsdiags,lobsdiagsave,i_tcp_ob_type, &
             nobskeep,lobsdiag_allocated,oberror_tune,perturb_obs
  use gsi_4dvar, only: nobs_bins,hr_obsbin
  use qcmod, only: npres_print,ptop,pbot,dfact,dfact1
  use guess_grids, only: ges_z,tropprs,ges_ps,ges_lnprsl,nfldsig,hrdifsig,ges_tv, &
          ntguessig
  use gridmod, only: get_ij,nsig,lat2,lon2
  use constants, only: zero,half,one,tiny_r_kind,two,one_tenth,cg_term,r3600, &
          four,wgtlim,g_over_rd,huge_r_kind,pi,three
  use convinfo, only: nconvtype,cermin,cermax,cgross,cvar_b,cvar_pg,ictype
  use convinfo, only: icsubtype
  use jfunc, only: jiter,first,last,jiterstart,l_foto,miter
  implicit none

  integer(i_kind),intent(in) :: lunin,mype,nele,nobs
  logical,intent(in):: conv_diagsave

  real(r_kind),dimension(npres_print,nconvtype,5,3),intent(inout) :: bwork ! obs-ges stats
  real(r_kind),dimension(100+7*nsig),intent(inout) :: awork ! data counts and gross checks

! DECLARE LOCAL PARMS HERE
  logical,dimension(nobs):: luse,muse

  real(r_kind) scale,ratio,obserror,obserrlm
  real(r_kind) residual,ress,ressw2,val,val2,valqc2
  real(r_kind) valqc,rlow,rhgh,drpx,prsfc,tges,tges2
  real(r_kind) cg_w,wgross,wnotgross,wgt,arg,exp_arg,term,rat_err2
  real(r_kind) presw,factw,dpres,rwgt,dpressave,cg_ps,drbx
  real(r_kind) sfcchk,prsln2,error,dtime,dlon,dlat,r0_001,r2_5,r0_2,rsig,thirty
  real(r_kind) ratio_errors,psges,zsges,rdp,drdp
  real(r_kind) pob,pges,pgesorig,half_tlapse,r10,ddiff,halfpi,r0_005,rdelz,psges2
  real(r_kind) alpha,resfct,error_orig

  real(r_kind),dimension(nele,nobs):: data
  real(r_kind),dimension(nobs):: dup
  real(r_kind),dimension(nsig)::prsltmp

  integer(i_kind) i,k,j,n,l,it,ii,itype
  integer(i_kind) jlat,jlon,jsig,mm1
  integer(i_kind) ikxx,nn,istat,iuse,ibin,iptrb
  integer(i_kind) ier,ilon,ilat,ipres,id,itime,ikx


!******************************************************************************
! Read and reformat observations in work arrays.
  read(lunin)data,luse

!    index information for data array (see reading routine)
  ier=1       ! index of obs error
  ilon=2      ! index of grid relative obs location (x)
  ilat=3      ! index of grid relative obs location (y)
  ipres=4     ! index of pressure
  itime=5     ! index of time observation
  ikxx=6      ! index of observation type in data array
  iuse=9      ! index of usage parameter

  mm1=mype+1
  scale=one
  rsig=nsig
  halfpi = half*pi
  r10=10.0_r_kind
  r0_005 = 0.005_r_kind
  r0_2=0.2_r_kind
  r2_5=2.5_r_kind
  half_tlapse=0.00325_r_kind  ! half of 6.5K/1km
  r0_001=0.001_r_kind

  do i=1,nobs
     muse(i)=nint(data(iuse,i)) <= jiter
  end do

  do i=1,nobs
    dlat=data(ilat,i)
    dlon=data(ilon,i)
    pob=data(ipres,i)
    dtime=data(itime,i)
    error=data(ier,i)
    ikx=nint(data(ikxx,i))

!   Link observation to appropriate observation bin
    if (nobs_bins>1) then
      ibin = NINT( dtime/hr_obsbin ) + 1
    else
      ibin = 1
    endif
    IF (ibin<1.OR.ibin>nobs_bins) write(6,*)mype,'Error nobs_bins,ibin= ',nobs_bins,ibin
!     Link obs to diagnostics structure
    if (.not.lobsdiag_allocated) then
      if (.not.associated(obsdiags(i_tcp_ob_type,ibin)%head)) then
	allocate(obsdiags(i_tcp_ob_type,ibin)%head,stat=istat)
	if (istat/=0) then
          write(6,*)'setuptcp: failure to allocate obsdiags',istat
          call stop2(301)
        end if
	obsdiags(i_tcp_ob_type,ibin)%tail => obsdiags(i_tcp_ob_type,ibin)%head
      else
	allocate(obsdiags(i_tcp_ob_type,ibin)%tail%next,stat=istat)
	if (istat/=0) then
          write(6,*)'setuptcp: failure to allocate obsdiags',istat
          call stop2(302)
        end if
	obsdiags(i_tcp_ob_type,ibin)%tail => obsdiags(i_tcp_ob_type,ibin)%tail%next
      end if
      allocate(obsdiags(i_tcp_ob_type,ibin)%tail%muse(miter+1))
      allocate(obsdiags(i_tcp_ob_type,ibin)%tail%nldepart(miter+1))
      allocate(obsdiags(i_tcp_ob_type,ibin)%tail%tldepart(miter))
      allocate(obsdiags(i_tcp_ob_type,ibin)%tail%obssen(miter))
      obsdiags(i_tcp_ob_type,ibin)%tail%indxglb=i
      obsdiags(i_tcp_ob_type,ibin)%tail%nchnperobs=-99999
      obsdiags(i_tcp_ob_type,ibin)%tail%luse=.false.
      obsdiags(i_tcp_ob_type,ibin)%tail%muse(:)=.false.
      obsdiags(i_tcp_ob_type,ibin)%tail%nldepart(:)=-huge(zero)
      obsdiags(i_tcp_ob_type,ibin)%tail%tldepart(:)=zero
      obsdiags(i_tcp_ob_type,ibin)%tail%wgtjo=-huge(zero)
      obsdiags(i_tcp_ob_type,ibin)%tail%obssen(:)=zero
    else
      if (.not.associated(obsdiags(i_tcp_ob_type,ibin)%tail)) then
	obsdiags(i_tcp_ob_type,ibin)%tail => obsdiags(i_tcp_ob_type,ibin)%head
      else
	obsdiags(i_tcp_ob_type,ibin)%tail => obsdiags(i_tcp_ob_type,ibin)%tail%next
      end if
      if (obsdiags(i_tcp_ob_type,ibin)%tail%indxglb/=i) then
        write(6,*)'setuptcp: index error'
        call stop2(303)
      end if
    endif

! Get guess sfc hght at obs location
    call intrp2a(ges_z(1,1,ntguessig),zsges,dlat,dlon,1,1,mype)

! Interpolate to get log(ps) and log(pres) at mid-layers
! at obs location/time
    call tintrp2a(ges_ps,psges,dlat,dlon,dtime,hrdifsig,&
       1,1,mype,nfldsig)
    call tintrp2a(ges_lnprsl,prsltmp,dlat,dlon,dtime,hrdifsig,&
       1,nsig,mype,nfldsig)

! Convert pressure to grid coordinates
    pgesorig = psges

! Take log for vertical interpolation
    psges = log(psges)
    call grdcrd(psges,1,prsltmp,nsig,-1)

! Get guess temperature at observation location and surface
    call tintrp3(ges_tv,tges,dlat,dlon,psges,dtime, &
         hrdifsig,1,mype,nfldsig)

! Adjust observation error and obs value due to differences in surface height
    rdelz=-zsges

!  No observed temperature
    psges2=data(ipres,i)
    call grdcrd(psges2,1,prsltmp,nsig,-1)
    call tintrp3(ges_tv,tges2,dlat,dlon,psges2,dtime, &
         hrdifsig,1,mype,nfldsig)

    drbx = half*abs(tges-tges2)+r2_5+r0_005*abs(rdelz)
    tges = half*(tges+tges2)

! Extrapolate surface temperature below ground at 6.5 k/km
! note only extrapolating .5dz, if no surface temp available.
    if(rdelz < zero)then
      tges=tges-half_tlapse*rdelz
      drbx=drbx-half_tlapse*rdelz
    end if

! Adjust guess hydrostatically
    rdp = g_over_rd*rdelz/tges

! Subtract off dlnp correction, then convert to pressure (cb)
    pges = exp(log(pgesorig) - rdp)

! Compute innovations
    ddiff=pob-pges  ! in cb

! Oberror Tuning and Perturb Obs
    if(muse(i)) then
       if(oberror_tune )then
          if( jiter > jiterstart ) then
             ddiff=ddiff+data(iptrb,i)/error/ratio_errors
          endif
       else if(perturb_obs )then
          ddiff=ddiff+data(iptrb,i)/error/ratio_errors
       endif
    endif

! Redefine observation error basied on residual
! inflate the ob error linearly as residual increases (max of O-F of 20 mb), 
    error_orig = error
    alpha = min((abs(r10*ddiff)/20.0_r_kind),one)

! Here is the error factor added, much like 'drdp'
! It varies from 0 to 1.5 (so ob error varies from 0.75 to 2.25 with this)
    resfct = alpha*1.5_r_kind/r10

! observational error adjustment
    drdp = pges*(g_over_rd*abs(rdelz)*drbx/(tges**2))

!  find adjustment to observational error (in terms of ratio)
   ratio_errors=error/(error+drdp+resfct)
   error=one/error

!    Gross error checks
    obserror = min(r10/max(ratio_errors*error,tiny_r_kind),huge_r_kind)
    obserrlm = max(cermin(ikx),min(cermax(ikx),obserror))
    residual = abs(r10*ddiff)
    ratio    = residual/obserrlm
    if (ratio > cgross(ikx) .or. ratio_errors < tiny_r_kind) then
       if (luse(i)) awork(6) = awork(6)+one
       error = zero
       ratio_errors = zero
    else
! No duplicate check 
    end if

    if (ratio_errors*error <= tiny_r_kind) muse(i)=.false.

     if (nobskeep>0) muse(i)=obsdiags(i_tcp_ob_type,ibin)%tail%muse(nobskeep)

     val      = error*ddiff
     if(luse(i))then

!    Compute penalty terms (linear & nonlinear qc).
       val2     = val*val
       exp_arg  = -half*val2
       rat_err2 = ratio_errors**2
       if (cvar_pg(ikx) > tiny_r_kind .and. error >tiny_r_kind) then
          arg  = exp(exp_arg)
          wnotgross= one-cvar_pg(ikx)
          cg_ps=cvar_b(ikx)
          wgross = cg_term*cvar_pg(ikx)/(cg_ps*wnotgross)
          term =log((arg+wgross)/(one+wgross))
          wgt  = one-wgross/(arg+wgross)
          rwgt = wgt/wgtlim
       else
          term = exp_arg
          wgt  = wgtlim
          rwgt = wgt/wgtlim
       endif
       valqc = -two*rat_err2*term


       if (muse(i)) then
!       Accumulate statistics for obs used belonging to this task
          if(rwgt < one) awork(21) = awork(21)+one
          awork(4)=awork(4)+val2*rat_err2
          awork(5)=awork(5)+one
          awork(22)=awork(22)+valqc
          nn=1
       else

!       rejected obs
          nn=2
!       monitored obs
          if(ratio_errors*error >=tiny_r_kind)nn=3
       end if


!     Accumulate statistics for each ob type

       ress   = ddiff*r10
       ressw2 = ress*ress
       bwork(1,ikx,1,nn)  = bwork(1,ikx,1,nn)+one              ! count
       bwork(1,ikx,2,nn)  = bwork(1,ikx,2,nn)+ress             ! (o-g)
       bwork(1,ikx,3,nn)  = bwork(1,ikx,3,nn)+ressw2           ! (o-g)**2
       bwork(1,ikx,4,nn)  = bwork(1,ikx,4,nn)+val2*rat_err2    ! penalty
       bwork(1,ikx,5,nn)  = bwork(1,ikx,5,nn)+valqc            ! nonlin qc penalty

     end if

     obsdiags(i_tcp_ob_type,ibin)%tail%luse=luse(i)
     obsdiags(i_tcp_ob_type,ibin)%tail%muse(jiter)=muse(i)
     obsdiags(i_tcp_ob_type,ibin)%tail%nldepart(jiter)=ddiff
     obsdiags(i_tcp_ob_type,ibin)%tail%wgtjo= (error*ratio_errors)**2

     if (.not. last .and. muse(i)) then

        if(.not. associated(tcphead(ibin)%head))then
            allocate(tcphead(ibin)%head,stat=istat)
            if(istat /= 0)write(6,*)' failure to write tcphead '
            tcptail(ibin)%head => tcphead(ibin)%head
        else
            allocate(tcptail(ibin)%head%llpoint,stat=istat)
            tcptail(ibin)%head => tcptail(ibin)%head%llpoint
            if(istat /= 0)write(6,*)' failure to write tcptail%llpoint '
        end if

        call get_ij(mm1,dlat,dlon,tcptail(ibin)%head%ij(1),tcptail(ibin)%head%wij(1))

        tcptail(ibin)%head%res      = ddiff
        tcptail(ibin)%head%err2     = error**2
        tcptail(ibin)%head%raterr2  = ratio_errors**2
        tcptail(ibin)%head%time     = dtime      
        tcptail(ibin)%head%b        = cvar_b(ikx)
        tcptail(ibin)%head%pg       = cvar_pg(ikx)
        tcptail(ibin)%head%luse     = luse(i)
        if(oberror_tune) then
           tcptail(ibin)%head%kx    = ikx        ! data type for oberror tuning
           tcptail(ibin)%head%ppertb= data(iptrb,i)/error/ratio_errors ! obs perturbation
        endif
        tcptail(ibin)%head%diags => obsdiags(i_tcp_ob_type,ibin)%tail

     endif

! End of loop over observations
  end do

  return
end subroutine setuptcp
