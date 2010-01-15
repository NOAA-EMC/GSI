!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  setupt --- Compute rhs of oi for temperature obs
!
! !INTERFACE:
!
subroutine setupt(lunin,mype,bwork,awork,nele,nobs,conv_diagsave)

! !USES:

  use kinds, only: r_kind,r_single,r_double,i_kind

  use obsmod, only: ttail,thead,sfcmodel,perturb_obs,oberror_tune,&
       i_t_ob_type,obsdiags,lobsdiagsave,nobskeep,lobsdiag_allocated,time_offset
  use gsi_4dvar, only: nobs_bins,hr_obsbin

  use qcmod, only: npres_print,dfact,dfact1,ptop,pbot

  use oneobmod, only: oneobtest
  use oneobmod, only: maginnov
  use oneobmod, only: magoberr

  use gridmod, only: nsig,twodvar_regional,regional
  use gridmod, only: get_ijk
  use jfunc, only: jiter,last,jiterstart,miter

  use guess_grids, only: nfldsig, hrdifsig,ges_ps,ges_lnprsl,ges_tv,ges_q,&
       ges_u,ges_v,geop_hgtl,ges_tsen,pt_ll

  use constants, only: zero, one, four,t0c,rd_over_cp,ione
  use constants, only: tiny_r_kind,half,two,cg_term
  use constants, only: huge_single,r1000,wgtlim,izero
  use convinfo, only: nconvtype,cermin,cermax,cgross,cvar_b,cvar_pg,ictype,icsubtype
  use converr, only: ptabl 

  implicit none

! !INPUT PARAMETERS:

  integer(i_kind)                                  , intent(in   ) :: lunin   ! file unit from which to read observations
  integer(i_kind)                                  , intent(in   ) :: mype    ! mpi task id
  integer(i_kind)                                  , intent(in   ) :: nele    ! number of data elements per observation
  integer(i_kind)                                  , intent(in   ) :: nobs    ! number of observations
  logical                                          , intent(in   ) :: conv_diagsave   ! logical to save innovation dignostics


! !INPUT/OUTPUT PARAMETERS:

                                                            ! array containing information ...
  real(r_kind),dimension(npres_print,nconvtype,5,3), intent(inout) :: bwork !  about o-g stats
  real(r_kind),dimension(100+7*nsig)               , intent(inout) :: awork !  for data counts and gross checks

! !DESCRIPTION:  For temperature observations, this routine
! \begin{enumerate}
!       \item reads obs assigned to given mpi task (geographic region),
!       \item simulates obs from guess,
!       \item apply some quality control to obs,
!       \item load weight and innovation arrays used in minimization
!       \item collects statistics for runtime diagnostic output
!       \item writes additional diagnostic information to output file
! \end{enumerate}
!
! !REVISION HISTORY:
!
!   1990-10-06  parrish
!   1998-04-10  weiyu yang
!   1999-03-01  wu - ozone processing moved into setuprhs from setupoz
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-06-17  treadon - update documentation
!   2004-07-15  todling - protex-compliant prologue; added intent/only's
!   2004-10-06  parrish - increase size of twork array for nonlinear qc
!   2004-11-22  derber - remove weight, add logical for boundary point
!   2004-12-22  treadon - move logical conv_diagsave from obsmod to argument list
!   2005-03-02  dee - remove garbage from diagnostic file
!   2005-03-09  parrish - nonlinear qc change to account for inflated obs error
!   2005-05-27  derber - level output change
!   2005-07-27  derber  - add print of monitoring and reject data
!   2005-09-28  derber  - combine with prep,spr,remove tran and clean up
!   2005-10-14  derber  - input grid location and fix regional lat/lon
!   2005-10-21  su  -modified variational qc and diagnostic output
!   2005-10-27  su - correct error in longitude index for diagnostic output
!   2005-11-03  treadon - correct error in ilone,ilate data array indices
!   2005-11-22  wu - add option to perturb conventional obs
!   2005-11-29 derber - remove psfcg and use ges_lnps instead
!   2005-12-20  parrish - add boundary layer forward model option
!   2005-12-20  parrish - correct dimension error in declaration of prsltmp
!   2006-01-31  todling - storing wgt/wgtlim in diag file instead of wgt only
!   2006-02-02  treadon - rename lnprsl as ges_lnprsl
!   2006-02-24  derber  - modify to take advantage of convinfo module
!   2006-03-21  treadon - modify optional perturbation to observation
!   2006-04-03  derber  - optimize and fix bugs due to virtual temperature
!   2006-04-11  park    - reset land mask for surface data based on observation type
!   2006-04-27  park    - remove sensitivity test for surface TLM routine
!   2006-05-30  su,derber,treadon - modify diagnostic output
!   2006-06-06  su - move to wgtlim to constants module
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!                       - modify handling of multiple data at same location
!                       - unify NL qc for surface model
!   2006-07-31  kleist - use ges_ps instead of lnps
!   2006-08-28      su - fix a bug in variational qc
!   2006-09-28  treadon - add 10m wind factor to sfc_wtq_fwd call
!   2006-10-28       su - turn off rawinsonde Vqc at south hemisphere
!   2007-03-09      su - modify the observation perturbation 
!   2007-03-19  tremolet - binning of observations
!   2007-06-05  tremolet - add observation diagnostics structure
!   2007-08-28      su - modify the observation gross check error 
!   2008-03-24      wu - oberror tuning and perturb obs
!   2008-05-21  safford - rm unused vars
!   2008-09-08  lueken  - merged ed's changes into q1fy09 code
!   2008-12-03  todling - changed handle of tail%time
!
! !REMARKS:
!   language: f90
!   machine:  ibm RS/6000 SP; SGI Origin 2000; Compaq/HP
!
! !AUTHOR: 
!   parrish          org: np22                date: 1990-10-06
!
!EOP
!-------------------------------------------------------------------------

! Declare local parameters
  real(r_kind),parameter:: r10=10.0_r_kind
  real(r_kind),parameter:: r0_001 = 0.001_r_kind
  real(r_kind),parameter:: r8 = 8.0_r_kind

! Declare local variables

  
  real(r_double) rstation_id
  real(r_kind) rsig,drpx,rsigp
  real(r_kind) psges,sfcchk,pres_diff,rlow,rhgh
  real(r_kind) tges
  real(r_kind) obserror,ratio,val2,obserrlm
  real(r_kind) residual,ressw2,scale,ress,ratio_errors,tob,ddiff
  real(r_kind) val,valqc,dlon,dlat,dtime,dpres,error,prest,rwgt
  real(r_kind) errinv_input,errinv_adjst,errinv_final
  real(r_kind) err_input,err_adjst,err_final,tfact
  real(r_kind) cg_t,wgross,wnotgross,wgt,arg,exp_arg,term,rat_err2
  real(r_kind),dimension(nobs)::dup
  real(r_kind),dimension(nsig):: prsltmp
  real(r_kind),dimension(nele,nobs):: data
  real(r_single),allocatable,dimension(:,:)::rdiagbuf
  real(r_kind) tgges,roges
  real(r_kind),dimension(nsig):: tvtmp,qtmp,utmp,vtmp,hsges
  real(r_kind) u10ges,v10ges,t2ges,q2ges,psges2,f10ges

  real(r_kind),dimension(nsig):: prsltmp2

  integer(i_kind) i,nchar,nreal,k,ii,jj,l,nn,ibin,idia
  integer(i_kind) mm1,jsig,iqt
  integer(i_kind) itype,msges
  integer(i_kind) ier,ilon,ilat,ipres,itob,id,itime,ikx,iqc,iptrb
  integer(i_kind) ier2,iuse,ilate,ilone,ikxx,istnelv,iobshgt
  integer(i_kind) regime,istat
  integer(i_kind) idomsfc,iskint,iff10,isfcr
  
  character(8) station_id
  character(8),allocatable,dimension(:):: cdiagbuf

  logical,dimension(nobs):: luse,muse
  logical sfctype
  logical iqtflg

  equivalence(rstation_id,station_id)

!*********************************************************************************
! Read and reformat observations in work arrays.
  read(lunin)data,luse

!    index information for data array (see reading routine)
  ier=ione           ! index of obs error
  ilon=2_i_kind      ! index of grid relative obs location (x)
  ilat=3_i_kind      ! index of grid relative obs location (y)
  ipres=4_i_kind     ! index of pressure
  itob=5_i_kind      ! index of t observation
  id=6_i_kind        ! index of station id
  itime=7_i_kind     ! index of observation time in data array
  ikxx=8_i_kind      ! index of ob type
  iqt=9_i_kind       ! index of flag indicating if moisture ob available
  iqc=10_i_kind      ! index of quality mark
  ier2=11_i_kind     ! index of original-original obs error ratio
  iuse=12_i_kind     ! index of use parameter
  idomsfc=13_i_kind  ! index of dominant surface type
  iskint=14_i_kind   ! index of surface skin temperature
  iff10=15_i_kind    ! index of 10 meter wind factor
  isfcr=16_i_kind    ! index of surface roughness
  ilone=17_i_kind    ! index of longitude (degrees)
  ilate=18_i_kind    ! index of latitude (degrees)
  istnelv=19_i_kind  ! index of station elevation (m)
  iobshgt=20_i_kind  ! index of observation height (m)
  iptrb=21_i_kind    ! index of t perturbation

  do i=1,nobs
     muse(i)=nint(data(iuse,i)) <= jiter
  end do

  dup=one
  do k=1,nobs
     do l=k+ione,nobs
        if(data(ilat,k) == data(ilat,l) .and.  &
           data(ilon,k) == data(ilon,l) .and.  &
           data(ipres,k) == data(ipres,l) .and. &
           data(ier,k) < r1000 .and. data(ier,l) < r1000 .and. &
           muse(k) .and. muse(l))then

           tfact=min(one,abs(data(itime,k)-data(itime,l))/dfact1)
           dup(k)=dup(k)+one-tfact*tfact*(one-dfact)
           dup(l)=dup(l)+one-tfact*tfact*(one-dfact)
        end if
     end do
  end do

! If requested, save select data for output to diagnostic file
  if(conv_diagsave)then
     ii=izero
     nchar=ione
     nreal=19_i_kind
     if (lobsdiagsave) nreal=nreal+4*miter+ione
     allocate(cdiagbuf(nobs),rdiagbuf(nreal,nobs))
  end if
  scale=one
  rsig=float(nsig)
  mm1=mype+ione

!  rsli=isli
  rsigp=rsig+one
  do i=1,nobs
! Convert obs lats and lons to grid coordinates
     dlat=data(ilat,i)
     dlon=data(ilon,i)
     dpres=data(ipres,i)
     dtime=data(itime,i)
     error=data(ier2,i)
     ikx=nint(data(ikxx,i))
     itype=ictype(ikx)
     prest=r10*exp(dpres)     ! in mb
     sfctype=itype>179_i_kind.and.itype<190_i_kind
  
     iqtflg=nint(data(iqt,i)) == izero

!    Load observation value and observation error into local variables
     tob=data(itob,i)
     obserror = max(cermin(ikx),min(cermax(ikx),data(ier,i)))

!    Link observation to appropriate observation bin
     if (nobs_bins>ione) then
        ibin = NINT( dtime/hr_obsbin ) + ione
     else
        ibin = ione
     endif
     IF (ibin<ione.OR.ibin>nobs_bins) write(6,*)mype,'Error nobs_bins,ibin= ',nobs_bins,ibin

!    Link obs to diagnostics structure
     if (.not.lobsdiag_allocated) then
        if (.not.associated(obsdiags(i_t_ob_type,ibin)%head)) then
           allocate(obsdiags(i_t_ob_type,ibin)%head,stat=istat)
           if (istat/=izero) then
              write(6,*)'setupt: failure to allocate obsdiags',istat
              call stop2(298)
           end if
           obsdiags(i_t_ob_type,ibin)%tail => obsdiags(i_t_ob_type,ibin)%head
        else
           allocate(obsdiags(i_t_ob_type,ibin)%tail%next,stat=istat)
           if (istat/=izero) then
              write(6,*)'setupt: failure to allocate obsdiags',istat
              call stop2(298)
           end if
           obsdiags(i_t_ob_type,ibin)%tail => obsdiags(i_t_ob_type,ibin)%tail%next
        end if
        allocate(obsdiags(i_t_ob_type,ibin)%tail%muse(miter+ione))
        allocate(obsdiags(i_t_ob_type,ibin)%tail%nldepart(miter+ione))
        allocate(obsdiags(i_t_ob_type,ibin)%tail%tldepart(miter))
        allocate(obsdiags(i_t_ob_type,ibin)%tail%obssen(miter))
        obsdiags(i_t_ob_type,ibin)%tail%indxglb=i
        obsdiags(i_t_ob_type,ibin)%tail%nchnperobs=-99999_i_kind
        obsdiags(i_t_ob_type,ibin)%tail%luse=.false.
        obsdiags(i_t_ob_type,ibin)%tail%muse(:)=.false.
        obsdiags(i_t_ob_type,ibin)%tail%nldepart(:)=-huge(zero)
        obsdiags(i_t_ob_type,ibin)%tail%tldepart(:)=zero
        obsdiags(i_t_ob_type,ibin)%tail%wgtjo=-huge(zero)
        obsdiags(i_t_ob_type,ibin)%tail%obssen(:)=zero
     else
        if (.not.associated(obsdiags(i_t_ob_type,ibin)%tail)) then
           obsdiags(i_t_ob_type,ibin)%tail => obsdiags(i_t_ob_type,ibin)%head
        else
           obsdiags(i_t_ob_type,ibin)%tail => obsdiags(i_t_ob_type,ibin)%tail%next
        end if
        if (obsdiags(i_t_ob_type,ibin)%tail%indxglb/=i) then
           write(6,*)'setupt: index error'
           call stop2(300)
        end if
     endif


! Interpolate log(ps) & log(pres) at mid-layers to obs locations/times
     call tintrp2a(ges_ps,psges,dlat,dlon,dtime,hrdifsig,&
          ione,ione,mype,nfldsig)
     call tintrp2a(ges_lnprsl,prsltmp,dlat,dlon,dtime,hrdifsig,&
          ione,nsig,mype,nfldsig)

     drpx=zero
     if(sfctype) then
        drpx=abs(one-((one/exp(dpres-log(psges))))**rd_over_cp)*t0c
     end if

!    Put obs pressure in correct units to get grid coord. number
     call grdcrd(dpres,ione,prsltmp(1),nsig,-ione)

! Implementation of forward model ----------

     if(sfctype.and.sfcmodel) then
        tgges=data(iskint,i)
        roges=data(isfcr,i)

        msges = izero
        if(itype == 180_i_kind .or. itype == 182_i_kind .or. itype == 183_i_kind) then    !sea
           msges=izero
        elseif(itype == 181_i_kind .or. itype == 187_i_kind .or. itype == 188_i_kind) then  !land
           msges=ione
        endif

        call tintrp2a(ges_tv,tvtmp,dlat,dlon,dtime,hrdifsig,&
             ione,nsig,mype,nfldsig)
        call tintrp2a(ges_q,qtmp,dlat,dlon,dtime,hrdifsig,&
             ione,nsig,mype,nfldsig)
        call tintrp2a(ges_u,utmp,dlat,dlon,dtime,hrdifsig,&
             ione,nsig,mype,nfldsig)
        call tintrp2a(ges_v,vtmp,dlat,dlon,dtime,hrdifsig,&
             ione,nsig,mype,nfldsig)
        call tintrp2a(geop_hgtl,hsges,dlat,dlon,dtime,hrdifsig,&
             ione,nsig,mype,nfldsig)
  
        psges2  = psges          ! keep in cb
        prsltmp2 = prsltmp       ! keep in cb
        call SFC_WTQ_FWD (psges2, tgges,&
             prsltmp2(1), tvtmp(1), qtmp(1), utmp(1), vtmp(1), &
             prsltmp2(2), tvtmp(2), qtmp(2), hsges(1), roges, msges, &
             f10ges,u10ges,v10ges, t2ges, q2ges, regime, iqtflg)
        tges = t2ges

     else
        if(iqtflg)then
!          Interpolate guess tv to observation location and time
           call tintrp3(ges_tv,tges,dlat,dlon,dpres,dtime, &
                hrdifsig,ione,mype,nfldsig)

        else
!          Interpolate guess tsen to observation location and time
           call tintrp3(ges_tsen,tges,dlat,dlon,dpres,dtime, &
                hrdifsig,ione,mype,nfldsig)
        end if
     endif

!    Get approximate k value of surface by using surface pressure
     sfcchk=log(psges)
     call grdcrd(sfcchk,ione,prsltmp(1),nsig,-ione)

!    Check to see if observations is above the top of the model (regional mode)
     if(sfctype)then
        if(abs(dpres)>four) drpx=1.0e10_r_kind
        pres_diff=prest-r10*psges
        if (twodvar_regional .and. abs(pres_diff)>=r10) drpx=1.0e10_r_kind
     end if
     rlow=max(sfcchk-dpres,zero)

     rhgh=max(zero,dpres-rsigp-r0_001)

     if(sfctype.and.sfcmodel)  dpres = one     ! place sfc T obs at the model sfc

     if(luse(i))then
        awork(1) = awork(1) + one
        if(rlow/=zero) awork(2) = awork(2) + one
        if(rhgh/=zero) awork(3) = awork(3) + one
     end if
     
     ratio_errors=error/(data(ier,i)+drpx+1.0e6_r_kind*rhgh+r8*rlow)
     error=one/error
!    if (dpres > rsig) ratio_errors=zero
     if (dpres > rsig )then
        if( regional .and. prest > pt_ll )then
           dpres=rsig
        else
           ratio_errors=zero
        endif
     endif


!     if(mype == izero ) then
!        write(6,*) itype,tob,tges
!     endif

! Compute innovation
     ddiff = tob-tges

! If requested, setup for single obs test.
     if (oneobtest) then
        ddiff = maginnov
        error=one/magoberr
        ratio_errors=one
     endif

!    Gross error checks

     obserror = one/max(ratio_errors*error,tiny_r_kind)
     obserrlm = max(cermin(ikx),min(cermax(ikx),obserror))
     residual = abs(ddiff)
     ratio    = residual/obserrlm
     if (ratio > cgross(ikx) .or. ratio_errors < tiny_r_kind) then
        if (luse(i)) awork(4) = awork(4)+one
        error = zero
        ratio_errors = zero
     else
        ratio_errors = ratio_errors/sqrt(dup(i))
     end if
     
     if (ratio_errors*error <=tiny_r_kind) muse(i)=.false.

     if (nobskeep>0) muse(i)=obsdiags(i_t_ob_type,ibin)%tail%muse(nobskeep)

!   Oberror Tuning and Perturb Obs
     if(muse(i)) then
        if(oberror_tune )then
           if( jiter > jiterstart ) then
              ddiff=ddiff+data(iptrb,i)/error/ratio_errors
           endif
        else if(perturb_obs )then
           ddiff=ddiff+data(iptrb,i)/error/ratio_errors
        endif
     endif

!    Compute penalty terms
     val      = error*ddiff
     if(luse(i))then
        val2     = val*val
        exp_arg  = -half*val2
        rat_err2 = ratio_errors**2
        if (cvar_pg(ikx) > tiny_r_kind .and. error >tiny_r_kind) then
           arg  = exp(exp_arg)
           wnotgross= one-cvar_pg(ikx)
           cg_t=cvar_b(ikx)
           wgross = cg_term*cvar_pg(ikx)/(cg_t*wnotgross)
           term =log((arg+wgross)/(one+wgross))
           wgt  = one-wgross/(arg+wgross)
           rwgt = wgt/wgtlim
        else
           term = exp_arg
           wgt  = wgtlim
           rwgt = wgt/wgtlim
        endif
        valqc = -two*rat_err2*term

!       Accumulate statistics for obs belonging to this task
        if(muse(i))then
           if(rwgt < one) awork(21) = awork(21)+one
           jsig = dpres
           jsig=max(ione,min(jsig,nsig))
           awork(jsig+3*nsig+100_i_kind)=awork(jsig+3*nsig+100_i_kind)+valqc
           awork(jsig+5*nsig+100_i_kind)=awork(jsig+5*nsig+100_i_kind)+one
           awork(jsig+6*nsig+100_i_kind)=awork(jsig+6*nsig+100_i_kind)+val2*rat_err2
        end if

! Loop over pressure level groupings and obs to accumulate statistics
! as a function of observation type.
        ress   = ddiff*scale
        ressw2 = ress*ress
        nn=ione
        if (.not. muse(i)) then
           nn=2_i_kind
           if(ratio_errors*error >=tiny_r_kind)nn=3_i_kind
        end if
        do k = 1,npres_print
           if(prest >=ptop(k) .and. prest <= pbot(k))then
              bwork(k,ikx,1,nn)  = bwork(k,ikx,1,nn)+one            ! count
              bwork(k,ikx,2,nn)  = bwork(k,ikx,2,nn)+ress           ! (o-g)
              bwork(k,ikx,3,nn)  = bwork(k,ikx,3,nn)+ressw2         ! (o-g)**2
              bwork(k,ikx,4,nn)  = bwork(k,ikx,4,nn)+val2*rat_err2  ! penalty
              bwork(k,ikx,5,nn)  = bwork(k,ikx,5,nn)+valqc          ! nonlin qc penalty
              
           end if
        end do
     end if

!    Fill obs diagnostics structure
     obsdiags(i_t_ob_type,ibin)%tail%luse=luse(i)
     obsdiags(i_t_ob_type,ibin)%tail%muse(jiter)=muse(i)
     obsdiags(i_t_ob_type,ibin)%tail%nldepart(jiter)=ddiff
     obsdiags(i_t_ob_type,ibin)%tail%wgtjo= (error*ratio_errors)**2

!    If obs is "acceptable", load array with obs info for use
!    in inner loop minimization (int* and stp* routines)
     if ( .not. last .and. muse(i)) then

        if(.not. associated(thead(ibin)%head))then
           allocate(thead(ibin)%head,stat=istat)
           if(istat /= izero)write(6,*)' failure to write thead '
           ttail(ibin)%head => thead(ibin)%head
        else
           allocate(ttail(ibin)%head%llpoint,stat=istat)
           if(istat /= izero)write(6,*)' failure to write ttail%llpoint '
           ttail(ibin)%head => ttail(ibin)%head%llpoint
        end if

!       Set (i,j,k) indices of guess gridpoint that bound obs location
        call get_ijk(mm1,dlat,dlon,dpres,ttail(ibin)%head%ij(1),ttail(ibin)%head%wij(1))

        ttail(ibin)%head%res     = ddiff
        ttail(ibin)%head%err2    = error**2
        ttail(ibin)%head%raterr2 = ratio_errors**2      
        ttail(ibin)%head%time    = dtime
        ttail(ibin)%head%b       = cvar_b(ikx)
        ttail(ibin)%head%pg      = cvar_pg(ikx)
        ttail(ibin)%head%use_sfc_model = sfctype.and.sfcmodel
        if(ttail(ibin)%head%use_sfc_model) then
           call get_tlm_tsfc(ttail(ibin)%head%tlm_tsfc(1), &
                psges2,tgges,prsltmp2(1), &
                tvtmp(1),qtmp(1),utmp(1),vtmp(1),hsges(1),roges,msges, &
                regime,iqtflg)
        else
           ttail(ibin)%head%tlm_tsfc = zero
        endif
        ttail(ibin)%head%luse    = luse(i)
        ttail(ibin)%head%tv_ob   = iqtflg

        if(oberror_tune) then
           ttail(ibin)%head%kx=ikx
           ttail(ibin)%head%tpertb=data(iptrb,i)/error/ratio_errors
           if(prest > ptabl(2))then
              ttail(ibin)%head%k1=ione
           else if( prest <= ptabl(33)) then
              ttail(ibin)%head%k1=33_i_kind
           else
              k_loop: do k=2,32
                 if(prest > ptabl(k+ione) .and. prest <= ptabl(k)) then
                    ttail(ibin)%head%k1=k
                    exit k_loop
                 endif
              enddo k_loop
           endif
        endif

        ttail(ibin)%head%diags => obsdiags(i_t_ob_type,ibin)%tail

     endif

! Save select output for diagnostic file
     if (conv_diagsave .and. luse(i)) then
        ii=ii+ione
        rstation_id     = data(id,i)
        cdiagbuf(ii)    = station_id         ! station id

        rdiagbuf(1,ii)  = ictype(ikx)        ! observation type
        rdiagbuf(2,ii)  = icsubtype(ikx)     ! observation subtype
    
        rdiagbuf(3,ii)  = data(ilate,i)      ! observation latitude (degrees)
        rdiagbuf(4,ii)  = data(ilone,i)      ! observation longitude (degrees)
        rdiagbuf(5,ii)  = data(istnelv,i)    ! station elevation (meters)
        rdiagbuf(6,ii)  = prest              ! observation pressure (hPa)
        rdiagbuf(7,ii)  = data(iobshgt,i)    ! observation height (meters)
        rdiagbuf(8,ii)  = dtime-time_offset  ! obs time (hours relative to analysis time)

        rdiagbuf(9,ii)  = data(iqc,i)        ! input prepbufr qc or event mark
        rdiagbuf(10,ii) = data(iqt,i)        ! setup qc or event mark (currently qtflg only)
        rdiagbuf(11,ii) = data(iuse,i)       ! read_prepbufr data usage flag
        if(muse(i)) then
           rdiagbuf(12,ii) = one             ! analysis usage flag (1=use, -1=not used)
        else
           rdiagbuf(12,ii) = -one
        endif

        err_input = data(ier2,i)
        err_adjst = data(ier,i)
        if (ratio_errors*error>tiny_r_kind) then
           err_final = one/(ratio_errors*error)
        else
           err_final = huge_single
        endif

        errinv_input = huge_single
        errinv_adjst = huge_single
        errinv_final = huge_single
        if (err_input>tiny_r_kind) errinv_input=one/err_input
        if (err_adjst>tiny_r_kind) errinv_adjst=one/err_adjst
        if (err_final>tiny_r_kind) errinv_final=one/err_final

        rdiagbuf(13,ii) = rwgt               ! nonlinear qc relative weight
        rdiagbuf(14,ii) = errinv_input       ! prepbufr inverse obs error (K**-1)
        rdiagbuf(15,ii) = errinv_adjst       ! read_prepbufr inverse obs error (K**-1)
        rdiagbuf(16,ii) = errinv_final       ! final inverse observation error (K**-1)

        rdiagbuf(17,ii) = data(itob,i)       ! temperature observation (K)
        rdiagbuf(18,ii) = ddiff              ! obs-ges used in analysis (K)
        rdiagbuf(19,ii) = tob-tges           ! obs-ges w/o bias correction (K) (future slot)

        if (lobsdiagsave) then
           idia=19_i_kind
           do jj=1,miter
              idia=idia+ione
              if (obsdiags(i_t_ob_type,ibin)%tail%muse(jj)) then
                 rdiagbuf(idia,ii) = one
              else
                 rdiagbuf(idia,ii) = -one
              endif
           enddo
           do jj=1,miter+ione
              idia=idia+ione
              rdiagbuf(idia,ii) = obsdiags(i_t_ob_type,ibin)%tail%nldepart(jj)
           enddo
           do jj=1,miter
              idia=idia+ione
              rdiagbuf(idia,ii) = obsdiags(i_t_ob_type,ibin)%tail%tldepart(jj)
           enddo
           do jj=1,miter
              idia=idia+ione
              rdiagbuf(idia,ii) = obsdiags(i_t_ob_type,ibin)%tail%obssen(jj)
           enddo
        endif

     end if

! End of loop over observations
  end do


! Write information to diagnostic file
  if(conv_diagsave)then
     write(7)'  t',nchar,nreal,ii,mype
     write(7)cdiagbuf(1:ii),rdiagbuf(:,1:ii)
     deallocate(cdiagbuf,rdiagbuf)
  end if


! End of routine
end subroutine setupt
  
  


