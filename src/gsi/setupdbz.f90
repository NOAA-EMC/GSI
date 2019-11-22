module dbz_setup
  implicit none
  private
  public:: setup
        interface setup; module procedure setupdbz; end interface

contains
subroutine setupdbz(obsLL,odiagLL,lunin,mype,bwork,awork,nele,nobs,is,radardbz_diagsave,init_pass)
! modified from setupdbz, now dbz is also a state variable
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    setupdbz     compute rhs of oi for radar reflectivity (dBZ)
!   prgmmr: carley          org: np22                date: 2011-04-05
!
! abstract: For radar reflectivity observations, this routine
!              a) reads obs assigned to given mpi task (geographic region),
!              b) simulates obs from guess,
!              c) apply some quality control to obs,
!              d) load weight and innovation arrays used in minimization
!              e) collects statistics for runtime diagnostic output
!              f) writes additional diagnostic information to output file
!
! program history log:
!   2011-05-19  carley  - Cleaned up fields loaded into dbzptr.
!                         Removed linearization from inner loop routines
!                         and placed it here (see jqr and jqli).
!   2011-08-11  carley  - Turn on gross error checks.
!   2011-09-19  carley  - Include temporary fix from setuprw to prevent out of
!                         bounds array references associated with dpres<zero
!   2012-02-12  carley  - Update to include use of metguess bundle with qr and qli
!   2016-02-15  Johnson, Y. Wang, X. Wang - Develop the reflectivity operator for WRF ARW 
!                                           (Johnson et al. 2015 MWR; Wang and Wang 2017 MWR).
!                                           Two options were developed,
!                                           1) Explicitly apply the operator H(qr, qs, qg) to hydrometeors
!                                           2) Directly use the reflectivity from the wrfout
!                                           POC: xuguang.wang@ou.edu
!   2017-02-09  guo     - Removed m_alloc, n_alloc.
!                       . Removed my_node with corrected typecast().
!   2017-05-12 Y. Wang and X. Wang - Following Guo replacing ob_type with polymorphic obsNode through type casting,
!                                           POC: xuguang.wang@ou.edu
!
!   input argument list:
!     lunin    - unit from which to read observations
!     mype     - mpi task id
!     nele     - number of data elements per observation
!     nobs     - number of observations
!
!   output argument list:
!     bwork    - array containing information about obs-ges statistics
!     awork    - array containing information for data counts and gross checks
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use mpeu_util, only: die,perr
  use kinds, only: r_kind,r_single,r_double,i_kind
  use m_obsdiagNode, only: obs_diag
  use m_obsdiagNode, only: obs_diags
  use m_obsdiagNode, only: obsdiagLList_nextNode
  use m_obsdiagNode, only: obsdiagNode_set
  use m_obsdiagNode, only: obsdiagNode_get
  use m_obsdiagNode, only: obsdiagNode_assert
  use obsmod, only: rmiss_single,&
                    lobsdiagsave,nobskeep,lobsdiag_allocated,time_offset,&
                    ens_hx_dbz_cut,static_gsi_nopcp_dbz

  use m_obsNode, only: obsNode
  use m_dbzNode, only: dbzNode
  use m_dbzNode, only: dbzNode_appendto
  use m_obsLList,only: obsLList
                     
  use hybrid_ensemble_parameters,only: l_hyb_ens
  use obsmod, only: luse_obsdiag, netcdf_diag, binary_diag, dirname, ianldate
  use obsmod, only: doradaroneob,oneobddiff,oneobvalue
  use nc_diag_write_mod, only: nc_diag_init, nc_diag_header, nc_diag_metadata, &
       nc_diag_write, nc_diag_data2d
  use nc_diag_read_mod, only: nc_diag_read_init, nc_diag_read_get_dim,nc_diag_read_close
  use oneobmod, only: oneobtest
  use oneobmod, only: maginnov
  use oneobmod, only: magoberr
  use gsi_4dvar, only: nobs_bins,hr_obsbin
  use qcmod, only: npres_print,ptop,pbot 
  use guess_grids, only: hrdifsig,geop_hgtl,nfldsig,&
       ges_lnprsl,ges_rho,ges_tsen
  use gridmod, only: nsig,get_ijk
  use gsi_metguess_mod, only: gsi_metguess_bundle,gsi_metguess_get
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use constants, only: flattening,semi_major_axis,grav_ratio,zero,grav,wgtlim,&
       half,one,two,grav_equator,eccentricity,somigliana,rad2deg,deg2rad,&
       r60,tiny_r_kind,cg_term,huge_single
  use jfunc, only: jiter,last,miter
  use convinfo, only: nconvtype,cermin,cermax,cgross,cvar_b,cvar_pg,ictype
  use convinfo, only: icsubtype
  use m_dtime, only: dtime_setup, dtime_check
  use obsmod, only   : if_model_dbz, inflate_obserr
  use setupdbz_lib, only:hx_dart,jqr_dart,jqs_dart,jqg_dart 
  use gridmod, only: wrf_mass_regional,nems_nmmb_regional 
  use sparsearr, only: sparr2, new, size, writearray, fullarray
  use state_vectors, only: nsdim
 
  implicit none
! Declare passed variables
  type(obsLList ),target,dimension(:),intent(in):: obsLL
  type(obs_diags),target,dimension(:),intent(in):: odiagLL

  integer(i_kind)                                  ,intent(in   ) :: lunin,mype,nele,nobs
  real(r_kind),dimension(100+7*nsig)               ,intent(inout) :: awork
  real(r_kind),dimension(npres_print,nconvtype,5,3),intent(inout) :: bwork
  integer(i_kind)                                  ,intent(in   ) :: is ! ndat index
  logical                                          ,intent(in   ) :: radardbz_diagsave
  logical                                          ,intent(in   ) :: init_pass ! state of "setup" parameters

! Declare local parameters
  real(r_kind),parameter:: r0_001 = 0.001_r_kind
  real(r_kind),parameter:: r8     = 8.0_r_kind
  real(r_kind),parameter:: ten    = 10.0_r_kind

! Declare external calls for code analysis
  external:: tintrp2a1, tintrp2a11
  external:: tintrp3
  external:: grdcrd
  external:: stop2
! Declare local variables
  real(r_kind) rlow,rhgh,rsig
!  real(r_kind) dz,denom,jqr_num,jqli_num,jqr,jqli !modified
  real(r_kind) dz,jqr,jqs,jqg
  real(r_kind) dlnp,pobl,zob
  real(r_kind) sin2,termg,termr,termrg
  real(r_kind) psges,zsges
  real(r_kind),dimension(nsig):: zges,hges
  real(r_kind) prsltmp(nsig)
  real(r_kind) sfcchk 
  real(r_kind) residual,obserrlm,obserror,ratio,scale,val2
  real(r_kind) ress,ressw
  real(r_kind) val,valqc,rwgt
  real(r_kind) cg_w,wgross,wnotgross,wgt,arg,exp_arg,term,rat_err2
  real(r_double) rstation_id
  real(r_kind) dlat,dlon,dtime,dpres,ddiff,error,slat
 
  real(r_kind) ratio_errors
  real(r_kind)dbzgesin,qrgesin,qsgesin,qggesin,rhogesin,tempgesin,qligesin
  real(r_kind) qrgesin1,qsgesin1,qggesin1, qligesin1
  real(r_kind) rdBZ,presw,dbznoise
  real(r_kind) errinv_input,errinv_adjst,errinv_final
  real(r_kind) err_input,err_adjst,err_final
  real(r_kind),dimension(nele,nobs):: data
  real(r_single),allocatable,dimension(:,:)::rdiagbuf
  integer(i_kind) i,nchar,nreal,k,k1,ii
  integer(i_kind) mm1,jj,k2
  integer(i_kind) jsig,ikxx,nn,ibin,ioff, ioff0
  integer(i_kind) ier,ilat,ilon,ihgt,idbzob,ikx,itime,iuse
  integer(i_kind) ielev,id,itilt,iazm,ilone,ilate,irange
  integer(i_kind) ier2,idbznoise,idmiss2opt
  character(8) station_id
  character(8),allocatable,dimension(:):: cdiagbuf
  character(80):: string
  character(128):: diag_file
  logical :: diagexist
  integer(i_kind):: lu_diag

  integer(i_kind),dimension(nobs):: ioid ! initial (pre-distribution) obs ID
  logical :: proceed
  logical,dimension(nobs):: luse,muse
  equivalence(rstation_id,station_id)
  real(r_kind) wrange
  integer(i_kind) numequal,numnotequal
 
  logical:: debugging

  type(dbzNode),pointer:: my_head
  type(obs_diag),pointer:: my_diag
  type(obs_diags),pointer:: my_diagLL

  character(len=*),parameter:: myname='setupdbz'
  integer(i_kind) irefsmlobs, irejrefsmlobs

  real(r_kind),allocatable,dimension(:,:,:  ) :: ges_ps
  real(r_kind),allocatable,dimension(:,:,:  ) :: ges_z

  real(r_kind),allocatable,dimension(:,:,:,: ) :: ges_qr
  real(r_kind),allocatable,dimension(:,:,:,: ) :: ges_qs
  real(r_kind),allocatable,dimension(:,:,:,: ) :: ges_qg
  real(r_kind),allocatable,dimension(:,:,:,: ) :: ges_qli
  real(r_kind),allocatable,dimension(:,:,:,: ) :: ges_dbz

  type(obsLList),pointer,dimension(:):: dbzhead
  dbzhead => obsLL(:)

!******************************************************************************* 
  ! Read and reformat observations in work arrays.
  read(lunin)data,luse, ioid
!    index information for data array (see reading routine)
  ier=1        ! index of obs error
  ilon=2       ! index of grid relative obs location (x)
  ilat=3       ! index of grid relative obs location (y)
  ihgt=4       ! index of obs elevation
  idbzob=5     ! index of radar reflectivity observation (dBZ)
  iazm=6       ! index of azimuth angle in data array
  itime=7      ! index of observation time in data array (hour)        ! Analysis relative time!
  ikxx=8       ! index of obs type in data array                       ! from the convinfo file (order in the list)
  itilt=9      ! index of tilt angle in data array
  ielev=10     ! index of radar elevation
  id=11        ! index of station id
  iuse=12      ! index of use parameter
  ilone=13     ! index of longitude (degrees)
  ilate=14     ! index of latitude (degrees)
  irange=15    ! index of range in m of obs from radar
  ier2=16      ! index of original-original obs error
  idbznoise=17 ! index of noise threshold for reflectivity (dBZ)
  idmiss2opt=18 ! index of if it is converted from the missing value
 
  numequal=0
  numnotequal=0
  irefsmlobs=0
  irejrefsmlobs=0 


!
! If requested, save select data for output to diagnostic file
  if(radardbz_diagsave)then
     ii=0
     nchar=1
     ioff0=25
     nreal=27
     if (lobsdiagsave) nreal=nreal+4*miter+1
     allocate(cdiagbuf(nobs),rdiagbuf(nreal,nobs))
  end if
  mm1=mype+1
  scale=one
  rsig=nsig


! Check to see if required guess fields are available
  call check_vars_(proceed)
  if(.not.proceed) return  ! not all vars available, simply return

! If require guess vars available, extract from bundle ...
  call init_vars_

  do i=1,nobs
     muse(i)=nint(data(iuse,i)) <= jiter
  end do
  
! - Observation times are checked in read routine - comment out for now
 
!  call dtime_setup()
  do i=1,nobs
     debugging=.false.
     if(doradaroneob) debugging=.true.
     dtime=data(itime,i)
     dlat=data(ilat,i)
     dlon=data(ilon,i)
     dbznoise=data(idbznoise,i)
     dpres=data(ihgt,i)
     ikx = nint(data(ikxx,i))
     error=data(ier2,i)
     slat=data(ilate,i)*deg2rad
     wrange=data(irange,i)
     if(debugging) then
       print * , "============="
       print *, dlat,dlon,dpres
       print *, data(ilate,i),data(ilone,i)
     endif


!    Link observation to appropriate observation bin
     if (nobs_bins>1) then
        ibin = NINT( dtime/hr_obsbin ) + 1
     else
        ibin = 1
     endif
     IF (ibin<1.OR.ibin>nobs_bins) write(6,*)mype,'Error nobs_bins,ibin= ',nobs_bins,ibin

     if (luse_obsdiag) my_diagLL => odiagLL(ibin)

!    Link obs to diagnostics structure
     if(luse_obsdiag)then
        my_diag => obsdiagLList_nextNode(my_diagLL      ,&
                create = .not.lobsdiag_allocated        ,&
                   idv = is             ,&
                   iob = ioid(i)        ,&
                   ich = 1              ,&
                  elat = data(ilate,i)  ,&
                  elon = data(ilone,i)  ,&
                  luse = luse(i)        ,&
                 miter = miter          )
        if(.not.associated(my_diag)) call die(myname, &
                'obsdiagLList_nextNode(), create =',.not.lobsdiag_allocated)
     endif

     call tintrp2a11(ges_z,zsges,dlat,dlon,dtime,hrdifsig,&
          mype,nfldsig)
     dpres=dpres-zsges
     if(dpres > 10000.0_r_kind) cycle !don't need obs above 10 km
     if (dpres<zero) then
       cycle  !  temporary fix to prevent out of bounds array reference in zges,prsltmp
     endif
     call tintrp2a11(ges_ps,psges,dlat,dlon,dtime,hrdifsig,&
          mype,nfldsig)
     call tintrp2a1(ges_lnprsl,prsltmp,dlat,dlon,dtime,hrdifsig,&
          nsig,mype,nfldsig)
     call tintrp2a1(geop_hgtl,hges,dlat,dlon,dtime,hrdifsig,&
          nsig,mype,nfldsig)
   
!    Convert geopotential height at layer midpoints to geometric height using
!    equations (17, 20, 23) in MJ Mahoney's note "A discussion of various
!    measures of altitude" (2001).  Available on the web at
!    http://mtp.jpl.nasa.gov/notes/altitude/altitude.html
!
!    termg  = equation 17
!    termr  = equation 21
!    termrg = first term in the denominator of equation 23
!    zges   = equation 23
     sin2  = sin(slat)*sin(slat)
     termg = grav_equator * &
          ((one+somigliana*sin2)/sqrt(one-eccentricity*eccentricity*sin2))
     termr = semi_major_axis /(one + flattening + grav_ratio -  &
          two*flattening*sin2)
     termrg = (termg/grav)*termr
     do k=1,nsig
        zges(k) = (termr*hges(k)) / (termrg-hges(k))  ! eq (23)
     end do
!    Given observation height (1) convert height to grid relative units, (2) compute
!    compute observation pressure (for diagnostic purposes only), and
!    (3) compute location of midpoint of first model layer above surface
!    in grid relative units
!    Convert observation height (in dpres) from meters to grid relative
!    units.  Save the observation height in zob for later use.
     zob = dpres
     call grdcrd(dpres,1,zges,nsig,1)
!    Set indices of model levels below (k1) and above (k2) observation.
     k=dpres
     k1=max(1,k)
     k2=min(k+1,nsig)
!    Compute observation pressure (only used for diagnostics)
     dz     = zges(k2)-zges(k1)
     dlnp   = prsltmp(k2)-prsltmp(k1)
     pobl   = prsltmp(k1) + (dlnp/dz)*(zob-zges(k1))



     presw  = ten*exp(pobl)
    if( (k1 == k2) .and. (k1 == 1) ) presw=ten*exp(prsltmp(k1)) 
!    solution to Nan in some members only for EnKF which causes problem?
!    Determine location in terms of grid units for midpoint of
!    first layer above surface
     sfcchk=log(psges)
     call grdcrd(sfcchk,1,prsltmp,nsig,-1)
!    Check to see if observation is below midpoint of first
!    above surface layer.  If so, set rlow to that difference
     rlow=max(sfcchk-dpres,zero)
!    Check to see if observation is above midpoint of layer
!    at the top of the model.  If so, set rhgh to that difference.
     rhgh=max(dpres-r0_001-nsig,zero)
!    Increment obs counter along with low and high obs counters
     if(luse(i))then
        awork(1)=awork(1)+one
        if(rhgh/=zero) awork(2)=awork(2)+one
        if(rlow/=zero) awork(3)=awork(3)+one
     end if
     
     !Not adjusting obs error based upon ob vertical location relative to grid box
     ratio_errors = error/(abs(data(ier,i)))   
   
   
     error = one/error

     if(dpres < zero .or. dpres > rsig)ratio_errors = zero


!    Interpolate guess dbz to observation location and time.
     if(if_model_dbz) then
     call tintrp31(ges_dbz,dbzgesin,dlat,dlon,dpres,dtime,& !modified
          hrdifsig,mype,nfldsig)
     endif
!    Interpolate guess qr, qli, and rho to observation location and time.
     call tintrp31(ges_qr,qrgesin,dlat,dlon,dpres,dtime,& !modified
          hrdifsig,mype,nfldsig)
     if( wrf_mass_regional )then
       call tintrp31(ges_qs,qsgesin,dlat,dlon,dpres,dtime,& 
            hrdifsig,mype,nfldsig)
       call tintrp31(ges_qg,qggesin,dlat,dlon,dpres,dtime,& 
            hrdifsig,mype,nfldsig)
     else if(nems_nmmb_regional) then
       call tintrp31(ges_qli,qligesin,dlat,dlon,dpres,dtime,&
            hrdifsig,mype,nfldsig)
     endif
     call tintrp31(ges_rho,rhogesin,dlat,dlon,dpres,dtime,&
          hrdifsig,mype,nfldsig)
     call tintrp31(ges_tsen,tempgesin,dlat,dlon,dpres,dtime,&
          hrdifsig,mype,nfldsig)


     if( nems_nmmb_regional ) then
       qrgesin1  = max(qrgesin,1.e-6_r_kind)
       qligesin1 = max(qligesin,1.e-6_r_kind)
     else if( wrf_mass_regional ) then
       qrgesin1  = max(qrgesin,1.e-6_r_kind)
       qsgesin1  = max(qsgesin,1.e-6_r_kind) 
       qggesin1  = max(qggesin,1.e-5_r_kind) 
     end if

     if(if_model_dbz) then
       rDBZ=dbzgesin
     else
       if( wrf_mass_regional )then
          call hx_dart(qrgesin,qggesin,qsgesin,rhogesin,tempgesin,rDBZ,debugging)
       else if( nems_nmmb_regional ) then
          write(6,*) "if_model_dbz should be set as .true."
          STOP
       endif
     endif !if_model_dbz


     if(miter == 0.or.l_hyb_ens) then !ie an enkf run
! DCD 1 March 2019:  changed 0.0 to static_gsi_nopcp_dbz
!       if(rDBZ < 0_r_kind) rDBZ=0.0_r_kind ! should be the same as in the read_dbz when nopcp=.true.
       if(rDBZ < static_gsi_nopcp_dbz) rDBZ=static_gsi_nopcp_dbz ! should be the same as in the read_dbz when nopcp=.true.
     endif
     if(miter == 0.and.ens_hx_dbz_cut) then !ie an enkf run
       if(rDBZ > 60_r_kind) rDBZ=60_r_kind
     endif

     jqr = 0.0_r_kind
     jqs = 0.0_r_kind
     jqg = 0.0_r_kind

     if( .not. if_model_dbz )then
     if( wrf_mass_regional ) then
         call jqr_dart(qrgesin1,qsgesin1,qggesin1,rhogesin,tempgesin,jqr)
         call jqs_dart(qrgesin1,qsgesin1,qggesin1,rhogesin,tempgesin,jqs)
         call jqg_dart(qrgesin1,qsgesin1,qggesin1,rhogesin,tempgesin,jqg)
     else if( nems_nmmb_regional ) then
          write(6,*) "if_model_dbz should be set as .true."
          STOP
     endif
     end if


     if(rdBZ==data(idbzob,i)) then
        numequal=numequal+1
     else
        numnotequal=numnotequal+1
     end if
     
     !--------------Calculate departure from observation----------------!

     
     ddiff = data(idbzob,i) - rdBZ
     if(miter > 0.and..not.l_hyb_ens) ddiff = max(min(ddiff,20.0_r_kind),-20.0_r_kind)


     if(debugging) print *, "DDIFF1: ",ddiff,data(idbzob,i),rdBZ

! If requested, setup for single obs test.
     if (oneobtest) then
        ddiff = maginnov
        error=one/magoberr
        ratio_errors=one
     endif

     if (doradaroneob) then
       if(oneobvalue > -900_r_kind) then
         data(idbzob,i) = oneobvalue
         ddiff = data(idbzob,i) - rdBZ
       else
         ddiff = oneobddiff
         data(idbzob,i) = rdBZ+ddiff
       endif
     endif !oneob
     if(rdBZ >= 5_r_kind) irefsmlobs=irefsmlobs+1

     if(debugging) print *, "DDIFF2: ",ddiff,data(idbzob,i),rdBZ

!    Gross error checks
     obserror = one/max(ratio_errors*error,tiny_r_kind)     
     obserrlm = max(cermin(ikx),min(cermax(ikx),obserror))
     
     residual = abs(ddiff)
     ratio    = residual/obserrlm
     if (ratio > cgross(ikx) .or. ratio_errors < tiny_r_kind) then
        if ( inflate_obserr .and. (ratio-cgross(ikx)) <= cgross(ikx) .and. ratio_errors >= tiny_r_kind) then 
          ! Since radar reflectivity can be very different from the model background
          ! good observations may be rejected during this QC step.  However, if these observations
          ! are allowed through, they can yield problems with convergence.  Therefore the error
          ! is inflated here up to twice the observation error in a manner that is
          ! proportional to the residual.  If this IF-TEST for this inflation fails, the
          ! observation is subsequently rejected.
                    
           obserror = residual/cgross(ikx)
           error = one/obserror
           
        else
           if (luse(i)) awork(4) = awork(4)+one 
           error = zero 
           ratio_errors = zero 
       
           if(rdBZ <= 5_r_kind) irejrefsmlobs=irejrefsmlobs+1
        end if
     end if

     if (ratio_errors*error <=tiny_r_kind) muse(i)=.false. 
     !-- if (nobskeep>0 .and. luse_obsdiag) muse(i)=obsdiags(i_dbz_ob_type,ibin)%tail%muse(nobskeep)
     if (nobskeep>0.and.luse_obsdiag) call obsdiagNode_get(my_diag, jiter=nobskeep, muse=muse(i))
     
     val     = error*ddiff
             
!    Compute penalty terms (linear & nonlinear qc).
     if(luse(i))then
        exp_arg  = -half*val**2
        rat_err2 = ratio_errors**2
        val2=val*val
        if (cvar_pg(ikx) > tiny_r_kind .and. error > tiny_r_kind) then
           arg  = exp(exp_arg)
           wnotgross= one-cvar_pg(ikx)
           cg_w=cvar_b(ikx)
           wgross = cg_term*cvar_pg(ikx)/(cg_w*wnotgross)
           term = log((arg+wgross)/(one+wgross))
           wgt  = one-wgross/(arg+wgross)
           rwgt = wgt/wgtlim
        else
           term = exp_arg
           wgt  = wgtlim
           rwgt = wgt/wgtlim
        endif
        valqc = -two*rat_err2*term
       
!       Accumulate statistics for obs belonging to this task
        if (muse(i)) then
           if(rwgt < one) awork(21) = awork(21)+one
           jsig = dpres
           jsig=max(1,min(jsig,nsig))
           awork(6*nsig+jsig+100)=awork(6*nsig+jsig+100)+val2*rat_err2
           awork(5*nsig+jsig+100)=awork(5*nsig+jsig+100)+one
           awork(3*nsig+jsig+100)=awork(3*nsig+jsig+100)+valqc
        end if
!       Loop over pressure level groupings and obs to accumulate
!       statistics as a function of observation type.
        ress  = scale*ddiff
        ressw = ress*ress
        nn=1
        if (.not. muse(i)) then
           nn=2
           if(ratio_errors*error >=tiny_r_kind)nn=3
        end if
        do k = 1,npres_print
           if(presw >=ptop(k) .and. presw<=pbot(k))then
              bwork(k,ikx,1,nn) = bwork(k,ikx,1,nn)+one            ! count
              bwork(k,ikx,2,nn) = bwork(k,ikx,2,nn)+ddiff          ! bias
              bwork(k,ikx,3,nn) = bwork(k,ikx,3,nn)+ressw          ! (o-g)**2
              bwork(k,ikx,4,nn) = bwork(k,ikx,4,nn)+val2*rat_err2  ! penalty
              bwork(k,ikx,5,nn) = bwork(k,ikx,5,nn)+valqc          ! nonlin qc penalty
             
           end if
        end do
     end if
     if(luse_obsdiag)then
        call obsdiagNode_set(my_diag, wgtjo=(error*ratio_errors)**2, &
           jiter=jiter, muse=muse(i), nldepart=ddiff)
     end if

     
!    If obs is "acceptable", load array with obs info for use
!    in inner loop minimization (int* and stp* routines)
     if ( .not. last .and. muse(i)) then             
       
        allocate(my_head)
        call dbzNode_appendto(my_head,dbzhead(ibin))

        my_head%idv = is
        my_head%iob = ioid(i)
        my_head%elat= data(ilate,i)
        my_head%elon= data(ilone,i)

!       Set (i,j,k) indices of guess gridpoint that bound obs location
        call get_ijk(mm1,dlat,dlon,dpres,my_head%ij,my_head%wij)   

        my_head%raterr2 = ratio_errors**2
        my_head%res     = ddiff
        my_head%err2    = error**2
        my_head%time    = dtime
        my_head%luse    = luse(i)
        my_head%b       = cvar_b(ikx)
        my_head%pg      = cvar_pg(ikx)
        my_head%jqr     = jqr
        if ( wrf_mass_regional ) then
          my_head%jqs     = jqs
          my_head%jqg     = jqg
        end if
 
        if(luse_obsdiag)then
          call obsdiagNode_assert(my_diag,my_head%idv,my_head%iob,1,myname,'my_diag:my_head')
          my_head%diags => my_diag
        endif

        my_head => null()
     endif
!    Save select output for diagnostic file
     if(radardbz_diagsave .and. luse(i) )then


        ii=ii+1
        rstation_id     = data(id,i)
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
        if (err_input>tiny_r_kind) errinv_input = one/err_input
        if (err_adjst>tiny_r_kind) errinv_adjst = one/err_adjst
        if (err_final>tiny_r_kind) errinv_final = one/err_final

        if(binary_diag) call contents_binary_diag_(my_diag)
        if(netcdf_diag) call contents_netcdf_diag_(my_diag)


     end if
  end do

! Release memory of local guess arrays
  call final_vars_


! Write information to diagnostic file
  if(radardbz_diagsave  .and. ii>0 )then

     write(string,600) jiter
600  format('radardbz_',i2.2)
     diag_file=trim(dirname) // trim(string)
     if(init_pass) then
        open(newunit=lu_diag,file=trim(diag_file),form='unformatted',status='unknown',position='rewind')
     else
        inquire(file=trim(diag_file),exist=diagexist)
        if (diagexist) then
           open(lu_diag,file=trim(diag_file),form='unformatted',status='old',position='append')
        else
           open(lu_diag,file=trim(diag_file),form='unformatted',status='unknown',position='rewind')
        endif
     endif
     if(init_pass .and. mype == 0) then
        write(lu_diag) ianldate
        write(6,*)'SETUPDBZ:   write time record to file ',&
                trim(diag_file), ' ',ianldate
     endif

     write(lu_diag)'dbz',nchar,nreal,ii,mype,ioff0
     write(lu_diag)cdiagbuf(1:ii),rdiagbuf(:,1:ii)
     deallocate(cdiagbuf,rdiagbuf)
     close(lu_diag)
  end if
  write(6,*)'mype, irefsmlobs,irejrefsmlobs are ',mype,' ',irefsmlobs, ' ',irejrefsmlobs
! close(52) !simulated obs
! End of routine
  contains

  subroutine check_vars_ (proceed)
  logical,intent(inout) :: proceed
  integer(i_kind) ivar, istatus
! Check to see if required guess fields are available
  call gsi_metguess_get ('var::ps', ivar, istatus )
  proceed=ivar>0
  call gsi_metguess_get ('var::z' , ivar, istatus )
  proceed=proceed.and.ivar>0
  call gsi_metguess_get ('var::tv', ivar, istatus )
  proceed=proceed.and.ivar>0
  if( if_model_dbz ) then
      call gsi_metguess_get ('var::dbz', ivar, istatus )
      proceed=proceed.and.ivar>0
  end if
  call gsi_metguess_get ('var::qr', ivar, istatus )
  proceed=proceed.and.ivar>0
  if(wrf_mass_regional)then
    call gsi_metguess_get ('var::qs', ivar, istatus )
    proceed=proceed.and.ivar>0
    call gsi_metguess_get ('var::qg', ivar, istatus )
    proceed=proceed.and.ivar>0
  end if
  if(nems_nmmb_regional)then
    call gsi_metguess_get ('var::qli', ivar, istatus )
    proceed=proceed.and.ivar>0
  end if
  end subroutine check_vars_

  subroutine init_vars_

  real(r_kind),dimension(:,:  ),pointer:: rank2
  real(r_kind),dimension(:,:,:),pointer:: rank3
  character(len=5) :: varname
  integer(i_kind) ifld, istatus

! If require guess vars available, extract from bundle ...
  if(size(gsi_metguess_bundle)==nfldsig) then
!    get ps ...
     varname='ps'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank2,istatus)
     if (istatus==0) then
         if(allocated(ges_ps))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_ps(size(rank2,1),size(rank2,2),nfldsig))
         ges_ps(:,:,1)=rank2
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank2,istatus)
            ges_ps(:,:,ifld)=rank2
         enddo
     else
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     endif
!    get z ...
     varname='z'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank2,istatus)
     if (istatus==0) then
         if(allocated(ges_z))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_z(size(rank2,1),size(rank2,2),nfldsig))
         ges_z(:,:,1)=rank2
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank2,istatus)
            ges_z(:,:,ifld)=rank2
         enddo
     else
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     endif

     if(if_model_dbz)then
     !    get dbz ....
         varname='dbz'
         call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank3,istatus)
         if (istatus==0) then
           if(allocated(ges_dbz))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
           endif
           allocate(ges_dbz(size(rank3,1),size(rank3,2),size(rank3,3),nfldsig))
           ges_dbz(:,:,:,1)=rank3
           do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank3,istatus)
            ges_dbz(:,:,:,ifld)=rank3
           enddo
         else
           write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
           call stop2(999)
         endif
     endif

!    get qr ...
     varname='qr'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank3,istatus)
     if (istatus==0) then
         if(allocated(ges_qr))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_qr(size(rank3,1),size(rank3,2),size(rank3,3),nfldsig))
         ges_qr(:,:,:,1)=rank3
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank3,istatus)
            ges_qr(:,:,:,ifld)=rank3
         enddo
     else
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     endif

     if(wrf_mass_regional)then
!    get qs ...
     varname='qs'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank3,istatus)
     if (istatus==0) then
         if(allocated(ges_qs))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_qs(size(rank3,1),size(rank3,2),size(rank3,3),nfldsig))
         ges_qs(:,:,:,1)=rank3
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank3,istatus)
            ges_qs(:,:,:,ifld)=rank3
         enddo
     else
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     endif

!    get qg ...
     varname='qg'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank3,istatus)
     if (istatus==0) then
         if(allocated(ges_qg))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_qg(size(rank3,1),size(rank3,2),size(rank3,3),nfldsig))
         ges_qg(:,:,:,1)=rank3
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank3,istatus)
            ges_qg(:,:,:,ifld)=rank3
         enddo
     else
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     endif


     end if

     if(nems_nmmb_regional)then
!      get qli ...
       varname='qli'
       call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank3,istatus)
       if (istatus==0) then
           if(allocated(ges_qli))then
              write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
              call stop2(999)
           endif
           allocate(ges_qli(size(rank3,1),size(rank3,2),size(rank3,3),nfldsig))
           ges_qli(:,:,:,1)=rank3
           do ifld=2,nfldsig
              call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank3,istatus)
              ges_qli(:,:,:,ifld)=rank3
           enddo
       else
           write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
           call stop2(999)
       endif
     end if

  else
     write(6,*) trim(myname), ': inconsistent vector sizes (nfldsig,size(metguess_bundle) ',&
                 nfldsig,size(gsi_metguess_bundle)
     call stop2(999)
  endif
  end subroutine init_vars_

  subroutine init_netcdf_diag_
  character(len=80) string
  character(len=128) diag_conv_file
  integer(i_kind) ncd_fileid,ncd_nobs
  logical append_diag
  logical,parameter::verbose=.false.
     write(string,900) jiter
900  format('conv_dbz_',i2.2,'.nc4')
     diag_conv_file=trim(dirname) // trim(string)

     inquire(file=diag_conv_file, exist=append_diag)

     if (append_diag) then
        call nc_diag_read_init(diag_conv_file,ncd_fileid)
        ncd_nobs = nc_diag_read_get_dim(ncd_fileid,'nobs')
        call nc_diag_read_close(diag_conv_file)

        if (ncd_nobs > 0) then
           if(verbose) print *,'file ' // trim(diag_conv_file) // ' exists. Appending.  nobs,mype=',ncd_nobs,mype
        else
           if(verbose) print *,'file ' // trim(diag_conv_file) // ' exists but contains no obs.  Not appending. nobs,mype=',ncd_nobs,mype
           append_diag = .false. ! if there are no obs in existing file, then do not try to append
        endif
     end if

     call nc_diag_init(diag_conv_file, append=append_diag)

     if (.not. append_diag) then ! don't write headers on append - the module will break?
        call nc_diag_header("date_time",ianldate )
        call nc_diag_header("Number_of_state_vars", nsdim          )
     endif
  end subroutine init_netcdf_diag_
  subroutine contents_binary_diag_(odiag)
  type(obs_diag),pointer,intent(in):: odiag

        cdiagbuf(ii)    = station_id         ! station id

        rdiagbuf(1,ii)  = ictype(ikx)        ! observation type
        rdiagbuf(2,ii)  = icsubtype(ikx)     ! observation subtype

        rdiagbuf(3,ii)  = data(ilate,i)      ! observation latitude (degrees)
        rdiagbuf(4,ii)  = data(ilone,i)      ! observation longitude (degrees)
        rdiagbuf(5,ii)  = data(ielev,i)      ! station elevation (meters)
        rdiagbuf(6,ii)  = presw              ! observation pressure (hPa)
        rdiagbuf(7,ii)  = data(ihgt,i)       ! observation height (meters)
        rdiagbuf(8,ii)  = (dtime*r60)-time_offset  ! obs time (sec relative to analysis time)
        rdiagbuf(9,ii)  = rmiss_single       ! input prepbufr qc or event mark
        rdiagbuf(10,ii) = rmiss_single       ! setup qc or event mark
        rdiagbuf(11,ii) = data(iuse,i)       ! read_prepbufr data usage flag
        if(muse(i)) then
           rdiagbuf(12,ii) = one             ! analysis usage flag (1=use,-1=not used)
        else
           rdiagbuf(12,ii) = -one
        endif

        rdiagbuf(13,ii) = rwgt                 ! nonlinear qc relative weight
        rdiagbuf(14,ii) = errinv_input         ! prepbufr inverse obs error (dBZ)**-1
        rdiagbuf(15,ii) = errinv_adjst         ! read_prepbufr inverse obs error (dBZ)**-1
        rdiagbuf(16,ii) = errinv_final         ! final inverse observation error (dBZ)**-1
        rdiagbuf(17,ii) = data(idbzob,i)       ! radar reflectivity observation (dBZ)
        rdiagbuf(18,ii) = ddiff                ! obs-ges (dBZ)
        rdiagbuf(19,ii) = data(idbzob,i)-rdBZ  ! obs-ges w/o bias correction (dBZ) (future slot)
        rdiagbuf(20,ii)=data(iazm,i)*rad2deg   ! azimuth angle
        rdiagbuf(21,ii)=data(itilt,i)*rad2deg  ! tilt angle
        rdiagbuf(22,ii)=data(irange,i) ! the range in km
        rdiagbuf(23,ii)=data(idmiss2opt,i) ! the range in km

        rdiagbuf(23,ii) = 1.e+10_r_single    ! ges ensemble spread (filled in EnKF)
        rdiagbuf(24,ii) = 1.e+10_r_single    ! ges ensemble spread (filled in EnKF)

        if (lobsdiagsave) then
            write(6,*)'wrong here, stop in setupdbz.f90 '
            stop
           ioff=nreal
           do jj=1,miter
              ioff=ioff+1
              if (odiag%muse(jj)) then
                 rdiagbuf(ioff,ii) = one
              else
                 rdiagbuf(ioff,ii) = -one
              endif
           enddo
           do jj=1,miter+1
              ioff=ioff+1
              rdiagbuf(ioff,ii) = odiag%nldepart(jj)
           enddo
           do jj=1,miter
              ioff=ioff+1
              rdiagbuf(ioff,ii) = odiag%tldepart(jj)
           enddo
           do jj=1,miter
              ioff=ioff+1
              rdiagbuf(ioff,ii) = odiag%obssen(jj)
           enddo
        endif

  end subroutine contents_binary_diag_
  subroutine contents_netcdf_diag_(odiag)
  type(obs_diag),pointer,intent(in):: odiag
! Observation class
  character(7),parameter     :: obsclass = '    dbz'
  real(r_kind),dimension(miter) :: obsdiag_iuse
           call nc_diag_metadata("Station_ID",              station_id          )
           call nc_diag_metadata("Observation_Class",       obsclass            )
           call nc_diag_metadata("Observation_Type",        ictype(ikx)         )
           call nc_diag_metadata("Observation_Subtype",     icsubtype(ikx)      )
           call nc_diag_metadata("Latitude",                sngl(data(ilate,i)) )
           call nc_diag_metadata("Longitude",               sngl(data(ilone,i)) )
           call nc_diag_metadata("Station_Elevation",       sngl(data(ielev,i)) )
           call nc_diag_metadata("Pressure",                sngl(presw)         )
           call nc_diag_metadata("Height",                  sngl(data(ihgt,i))  )
           call nc_diag_metadata("Time",                    sngl(dtime-time_offset))
           call nc_diag_metadata("Prep_QC_Mark",            sngl(zero)          )
           call nc_diag_metadata("Prep_Use_Flag",           sngl(data(iuse,i))  )
!          call nc_diag_metadata("Nonlinear_QC_Var_Jb",     var_jb   !          )
           call nc_diag_metadata("Nonlinear_QC_Rel_Wgt",    sngl(rwgt)          )
           if(muse(i)) then
              call nc_diag_metadata("Analysis_Use_Flag",    sngl(one)           )
           else
              call nc_diag_metadata("Analysis_Use_Flag",    sngl(-one)          )
           endif

           call nc_diag_metadata("Errinv_Input",            sngl(errinv_input)  )
           call nc_diag_metadata("Errinv_Adjust",           sngl(errinv_adjst)  )
           call nc_diag_metadata("Errinv_Final",            sngl(errinv_final)  )

           call nc_diag_metadata("Observation",             sngl(data(idbzob,i)) )
           call nc_diag_metadata("Obs_Minus_Forecast_adjusted",   sngl(ddiff)   )
           call nc_diag_metadata("Obs_Minus_Forecast_unadjusted", sngl(data(idbzob,i)-rdBZ) )

           if (lobsdiagsave) then
              do jj=1,miter
                 if (odiag%muse(jj)) then
                       obsdiag_iuse(jj) =  one
                 else
                       obsdiag_iuse(jj) = -one
                 endif
              enddo

              call nc_diag_data2d("ObsDiagSave_iuse",     obsdiag_iuse              )
              call nc_diag_data2d("ObsDiagSave_nldepart", odiag%nldepart )
              call nc_diag_data2d("ObsDiagSave_tldepart", odiag%tldepart )
              call nc_diag_data2d("ObsDiagSave_obssen"  , odiag%obssen   )
           endif

  end subroutine contents_netcdf_diag_

  subroutine final_vars_
    if(allocated(ges_z )) deallocate(ges_z )
    if(allocated(ges_ps)) deallocate(ges_ps)
    if(allocated(ges_qr)) deallocate(ges_qr)
    if(allocated(ges_qs)) deallocate(ges_qs)
    if(allocated(ges_qg)) deallocate(ges_qg)
    if(allocated(ges_qli)) deallocate(ges_qli)
    if(allocated(ges_dbz)) deallocate(ges_dbz)
  end subroutine final_vars_
end subroutine setupdbz
end module dbz_setup
