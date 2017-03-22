subroutine setupgust(lunin,mype,bwork,awork,nele,nobs,is,conv_diagsave)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    setupgust    compute rhs for conventional surface gust
!   prgmmr: derber           org: np23                date: 2004-07-20
!
! abstract: For sea surface temperature observations
!              a) reads obs assigned to given mpi task (geographic region),
!              b) simulates obs from guess,
!              c) apply some quality control to obs,
!              d) load weight and innovation arrays used in minimization
!              e) collects statistics for runtime diagnostic output
!              f) writes additional diagnostic information to output file
!
! program history log:
!   2009-03-10  zhu
!   2011-02-18  zhu - update
!   2013-01-26  parrish - change from grdcrd to grdcrd1, 
!                          tintrp2a to tintrp2a1, tintrp2a11 (to allow successful debug compile on WCOSS)
!   2013-10-19  todling - metguess now holds background
!   2014-01-28  todling - write sensitivity slot indicator (ioff) to header of diagfile
!   2014-07-21  carley - ensure no division by 0 when calculating presw
!   2014-12-30  derber - Modify for possibility of not using obsdiag
!   2015-10-01  guo   - full res obvsr: index to allow redistribution of obsdiags
!   2016-05-18  guo     - replaced ob_type with polymorphic obsNode through type casting
!   2016-06-24  guo     - fixed the default value of obsdiags(:,:)%tail%luse to luse(i)
!                       . removed (%dlat,%dlon) debris.
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

  use guess_grids, only: hrdifsig,nfldsig,ges_lnprsl, &
               geop_hgtl,sfcmod_gfs,sfcmod_mm5,comp_fact10     
  use m_obsdiags, only: gusthead
  use obsmod, only: rmiss_single,i_gust_ob_type,obsdiags,&
                    lobsdiagsave,nobskeep,lobsdiag_allocated,time_offset
  use m_obsNode, only: obsNode
  use m_gustNode, only: gustNode
  use m_obsLList, only: obsLlist_appendNode
  use obsmod, only: obs_diag,bmiss,luse_obsdiag
  use gsi_4dvar, only: nobs_bins,hr_obsbin
  use oneobmod, only: magoberr,maginnov,oneobtest
  use gridmod, only: nsig
  use gridmod, only: get_ij,twodvar_regional
  use constants, only: zero,tiny_r_kind,one,one_tenth,half,wgtlim,rd,grav,&
            two,cg_term,three,four,huge_single,r1000,r3600,&
            grav_ratio,flattening,grav,deg2rad,grav_equator,somigliana, &
            semi_major_axis,eccentricity
  use jfunc, only: jiter,last,miter
  use qcmod, only: dfact,dfact1,npres_print
  use convinfo, only: nconvtype,cermin,cermax,cgross,cvar_b,cvar_pg,ictype
  use convinfo, only: icsubtype
  use m_dtime, only: dtime_setup, dtime_check, dtime_show
  use gsi_bundlemod, only : gsi_bundlegetpointer
  use gsi_metguess_mod, only : gsi_metguess_get,gsi_metguess_bundle

  implicit none

! Declare passed variables
  logical                                          ,intent(in   ) :: conv_diagsave
  integer(i_kind)                                  ,intent(in   ) :: lunin,mype,nele,nobs
  real(r_kind),dimension(100+7*nsig)               ,intent(inout) :: awork
  real(r_kind),dimension(npres_print,nconvtype,5,3),intent(inout) :: bwork
  integer(i_kind)                                  ,intent(in   ) :: is ! ndat index

! Declare external calls for code analysis
  external:: tintrp2a1,tintrp2a11
  external:: stop2

! Declare local parameters
  real(r_kind),parameter:: r0_1_bmiss=one_tenth*bmiss
  character(len=*),parameter:: myname='setupgust'

! Declare local variables
  
  real(r_double) rstation_id

  real(r_kind) gustges,dlat,dlon,ddiff,dtime,error,r0_001,thirty
  real(r_kind) scale,val2,rsig,rsigp,ratio,ressw2,ress,residual
  real(r_kind) obserrlm,obserror,val,valqc,rlow,rhgh,drpx
  real(r_kind) term,rwgt
  real(r_kind) cg_gust,wgross,wnotgross,wgt,arg,exp_arg,rat_err2
  real(r_kind) presw,factw,dpres,sfcchk
  real(r_kind) ratio_errors,tfact,fact,wflate,ten,psges,goverrd,zsges
  real(r_kind) slat,sin2,termg,termr,termrg,pobl
  real(r_kind) dz,zob,z1,z2,p1,p2,dz21,dlnp21,dstn
  real(r_kind) errinv_input,errinv_adjst,errinv_final
  real(r_kind) err_input,err_adjst,err_final,skint,sfcr
  real(r_kind),dimension(nobs):: dup
  real(r_kind),dimension(nsig)::prsltmp,zges
  real(r_kind),dimension(nele,nobs):: data
  real(r_single),allocatable,dimension(:,:)::rdiagbuf


  integer(i_kind) ier,ilon,ilat,ihgt,igust,ipres,id,itime,ikx,imaxerr,iqc
  integer(i_kind) iuse,ilate,ilone,istnelv,iprvd,isprvd
  integer(i_kind) i,nchar,nreal,k,k1,k2,ii,ikxx,nn,isli,ibin,ioff,ioff0,jj
  integer(i_kind) l,mm1
  integer(i_kind) istat
  integer(i_kind) idomsfc,iskint,iff10,isfcr
  
  logical,dimension(nobs):: luse,muse
  integer(i_kind),dimension(nobs):: ioid ! initial (pre-distribution) obs ID
  logical proceed

  character(8) station_id
  character(8),allocatable,dimension(:):: cdiagbuf
  character(8),allocatable,dimension(:):: cprvstg,csprvstg
  character(8) c_prvstg,c_sprvstg
  real(r_double) r_prvstg,r_sprvstg

  logical:: in_curbin, in_anybin
  integer(i_kind),dimension(nobs_bins) :: n_alloc
  integer(i_kind),dimension(nobs_bins) :: m_alloc
  class(obsNode),pointer:: my_node
  type(gustNode),pointer:: my_head
  type(obs_diag),pointer:: my_diag


  equivalence(rstation_id,station_id)
  equivalence(r_prvstg,c_prvstg)
  equivalence(r_sprvstg,c_sprvstg)
  
  real(r_kind),allocatable,dimension(:,:,:) :: ges_ps
  real(r_kind),allocatable,dimension(:,:,:) :: ges_z
  real(r_kind),allocatable,dimension(:,:,:) :: ges_gust

! Check to see if required guess fields are available
  call check_vars_(proceed)
  if(.not.proceed) return  ! not all vars available, simply return

! If require guess vars available, extract from bundle ...
  call init_vars_

  n_alloc(:)=0
  m_alloc(:)=0
!*********************************************************************************
! Read and reformat observations in work arrays.
  read(lunin)data,luse,ioid

!  index information for data array (see reading routine)
  ier=1       ! index of obs error
  ilon=2      ! index of grid relative obs location (x)
  ilat=3      ! index of grid relative obs location (y)
  ipres=4     ! index of pressure
  ihgt=5      ! index of observation elevation
  igust=6     ! index of gust observation
  id=7        ! index of station id
  itime=8     ! index of observation time in data array
  ikxx=9      ! index of ob type
  imaxerr=10  ! index of gust max error
  iqc=11      ! index of qulaity mark
  iuse=12     ! index of use parameter
  idomsfc=13  ! index of dominant surface type
  iskint=14   ! index of surface skin temperature
  iff10=15    ! index of 10 meter wind factor
  isfcr=16    ! index of surface roughness
  ilone=17    ! index of longitude (degrees)
  ilate=18    ! index of latitude (degrees)
  istnelv=19  ! index of station elevation (m)
  iprvd=20    ! index of provider
  isprvd=21   ! index of subprovider

  mm1=mype+1
  scale=one
  rsig=nsig
  thirty = 30.0_r_kind
  ten = 10.0_r_kind
  r0_001=0.001_r_kind
  rsigp=rsig+one
  goverrd=grav/rd

  do i=1,nobs
     muse(i)=nint(data(iuse,i)) <= jiter
  end do

! Check for missing data
  if (.not. oneobtest) then
  do i=1,nobs
    if (data(igust,i) > r0_1_bmiss)  then
       muse(i)=.false.
       data(igust,i)=rmiss_single   ! for diag output
    end if
  end do
  end if

! Check for duplicate observations at same location
  dup=one
  do k=1,nobs
     do l=k+1,nobs
        if(data(ilat,k) == data(ilat,l) .and.  &
           data(ilon,k) == data(ilon,l) .and.  &
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
     ii=0
     nchar=1
     ioff0=22
     nreal=ioff0
     if (lobsdiagsave) nreal=nreal+4*miter+1
     allocate(cdiagbuf(nobs),rdiagbuf(nreal,nobs))
     allocate(cprvstg(nobs),csprvstg(nobs))
  end if

  call dtime_setup()
  do i=1,nobs
     dtime=data(itime,i)
     call dtime_check(dtime, in_curbin, in_anybin)
     if(.not.in_anybin) cycle

     if(in_curbin) then
        dlat=data(ilat,i)
        dlon=data(ilon,i)

        ikx  = nint(data(ikxx,i))
        error=data(ier,i)
        isli=data(idomsfc,i)
     endif

!    Link observation to appropriate observation bin
     if (nobs_bins>1) then
        ibin = NINT( dtime/hr_obsbin ) + 1
     else
        ibin = 1
     endif
     IF (ibin<1.OR.ibin>nobs_bins) write(6,*)mype,'Error nobs_bins,ibin= ',nobs_bins,ibin

!    Link obs to diagnostics structure
     if (luse_obsdiag) then
        if (.not.lobsdiag_allocated) then
           if (.not.associated(obsdiags(i_gust_ob_type,ibin)%head)) then
              obsdiags(i_gust_ob_type,ibin)%n_alloc = 0
              allocate(obsdiags(i_gust_ob_type,ibin)%head,stat=istat)
              if (istat/=0) then
                 write(6,*)'setupgust: failure to allocate obsdiags',istat
                 call stop2(295)
              end if
              obsdiags(i_gust_ob_type,ibin)%tail => obsdiags(i_gust_ob_type,ibin)%head
           else
              allocate(obsdiags(i_gust_ob_type,ibin)%tail%next,stat=istat)
              if (istat/=0) then
                 write(6,*)'setupgust: failure to allocate obsdiags',istat
                 call stop2(295)
              end if
              obsdiags(i_gust_ob_type,ibin)%tail => obsdiags(i_gust_ob_type,ibin)%tail%next
           end if
           obsdiags(i_gust_ob_type,ibin)%n_alloc = obsdiags(i_gust_ob_type,ibin)%n_alloc +1
    
           allocate(obsdiags(i_gust_ob_type,ibin)%tail%muse(miter+1))
           allocate(obsdiags(i_gust_ob_type,ibin)%tail%nldepart(miter+1))
           allocate(obsdiags(i_gust_ob_type,ibin)%tail%tldepart(miter))
           allocate(obsdiags(i_gust_ob_type,ibin)%tail%obssen(miter))
           obsdiags(i_gust_ob_type,ibin)%tail%indxglb=ioid(i)
           obsdiags(i_gust_ob_type,ibin)%tail%nchnperobs=-99999
           obsdiags(i_gust_ob_type,ibin)%tail%luse=luse(i)
           obsdiags(i_gust_ob_type,ibin)%tail%muse(:)=.false.
           obsdiags(i_gust_ob_type,ibin)%tail%nldepart(:)=-huge(zero)
           obsdiags(i_gust_ob_type,ibin)%tail%tldepart(:)=zero
           obsdiags(i_gust_ob_type,ibin)%tail%wgtjo=-huge(zero)
           obsdiags(i_gust_ob_type,ibin)%tail%obssen(:)=zero
    
           n_alloc(ibin) = n_alloc(ibin) +1
           my_diag => obsdiags(i_gust_ob_type,ibin)%tail
           my_diag%idv = is
           my_diag%iob = ioid(i)
           my_diag%ich = 1
           my_diag%elat= data(ilate,i)
           my_diag%elon= data(ilone,i)
        else
           if (.not.associated(obsdiags(i_gust_ob_type,ibin)%tail)) then
              obsdiags(i_gust_ob_type,ibin)%tail => obsdiags(i_gust_ob_type,ibin)%head
           else
              obsdiags(i_gust_ob_type,ibin)%tail => obsdiags(i_gust_ob_type,ibin)%tail%next
           end if
           if (.not.associated(obsdiags(i_gust_ob_type,ibin)%tail)) then
              call die(myname,'.not.associated(obsdiags(i_gust_ob_type,ibin)%tail)')
           end if
           if (obsdiags(i_gust_ob_type,ibin)%tail%indxglb/=ioid(i)) then
              write(6,*)'setupgust: index error'
              call stop2(297)
           end if
        endif
     endif

     if(.not.in_curbin) cycle

! Interpolate to get gust at obs location/time
     call tintrp2a11(ges_gust,gustges,dlat,dlon,dtime,hrdifsig,&
          mype,nfldsig)

!   Process observations with reported height
    drpx = zero
    dpres = data(ihgt,i)
    dstn = data(istnelv,i)

!   Get guess surface elevation and geopotential height profile
!   at observation location.
    call tintrp2a11(ges_z,zsges,dlat,dlon,dtime,hrdifsig,&
            mype,nfldsig)
!   Subtract off combination of surface station elevation and
!   model elevation depending on how close to surface
    fact = zero
    if(dpres-dstn > 10._r_kind)then
      if(dpres-dstn > 1000._r_kind)then
         fact = one
      else
         fact=(dpres-dstn)/990._r_kind
      end if
    end if
    dpres=dpres-(dstn+fact*(zsges-dstn))
    drpx=0.003*abs(dstn-zsges)*(one-fact)

    if (.not. twodvar_regional) then
       call tintrp2a1(geop_hgtl,zges,dlat,dlon,dtime,hrdifsig,&
               nsig,mype,nfldsig)
!      For observation reported with geometric height above sea level,
!      convert geopotential to geometric height.
!      Convert geopotential height at layer midpoints to geometric
!      height using equations (17, 20, 23) in MJ Mahoney's note
!      "A discussion of various measures of altitude" (2001).
!      Available on the web at
!      http://mtp.jpl.nasa.gov/notes/altitude/altitude.html
!
!      termg  = equation 17
!      termr  = equation 21
!      termrg = first term in the denominator of equation 23
!      zges  = equation 23

       slat = data(ilate,i)*deg2rad
       sin2  = sin(slat)*sin(slat)
       termg = grav_equator * &
            ((one+somigliana*sin2)/sqrt(one-eccentricity*eccentricity*sin2))
       termr = semi_major_axis /(one + flattening + grav_ratio -  &
            two*flattening*sin2)
       termrg = (termg/grav)*termr
       do k=1,nsig
          zges(k) = (termr*zges(k)) / (termrg-zges(k))  ! eq (23)
       end do
    else
       zges(1) = ten
    end if

!   Given observation height, (1) adjust 10 meter wind factor if
!   necessary, (2) convert height to grid relative units, (3) compute
!   compute observation pressure (for diagnostic purposes only), and
!   (4) compute location of midpoint of first model layer above surface
!   in grid relative units

!   Convert observation height (in dpres) from meters to grid relative
!   units.  Save the observation height in zob for later use.
    zob = dpres
    call grdcrd1(dpres,zges,nsig,1)

    if (zob >= zges(1)) then
       factw=one
    else
       factw = data(iff10,i)
       if(sfcmod_gfs .or. sfcmod_mm5) then
          sfcr = data(isfcr,i)
          skint = data(iskint,i)
          call comp_fact10(dlat,dlon,dtime,skint,sfcr,isli,mype,factw)
       end if
       if (.not. twodvar_regional) then
         if (zob <= ten) then
            if(zob < ten)then
              term = max(zob,zero)/ten
              factw = term*factw
            end if
         else
            term = (zges(1)-zob)/(zges(1)-ten)
            factw = one-term+factw*term
         end if
       else
          if(zob < ten)then
             term = max(zob,zero)/ten
             factw = term*factw
          end if
       end if
       gustges=factw*gustges
    endif

!   Compute observation pressure (only used for diagnostics & for type 2**)
!   Get guess surface pressure and mid layer pressure
!   at observation location.
    if (ictype(ikx)>=280 .and. ictype(ikx)<290) then
       call tintrp2a11(ges_ps,psges,dlat,dlon,dtime,hrdifsig,&
            mype,nfldsig)
       call tintrp2a1(ges_lnprsl,prsltmp,dlat,dlon,dtime,hrdifsig,&
            nsig,mype,nfldsig)
       if ((dpres-one) < tiny_r_kind) then
          z1=zero;    p1=log(psges)
          z2=zges(1); p2=prsltmp(1)
       elseif (dpres>nsig) then
          z1=zges(nsig-1); p1=prsltmp(nsig-1)
          z2=zges(nsig);   p2=prsltmp(nsig)
          drpx = 1.e6_r_kind
       else
          k=dpres
          k1=min(max(1,k),nsig)
          k2=max(1,min(k+1,nsig))
          z1=zges(k1); p1=prsltmp(k1)
          z2=zges(k2); p2=prsltmp(k2)
       endif

       dz21     = z2-z1
       if(dz21==zero)cycle
       dlnp21   = p2-p1
       dz       = zob-z1
       pobl     = p1 + (dlnp21/dz21)*dz
       presw    = ten*exp(pobl)
    else
       presw = ten*exp(data(ipres,i))
    end if


!   Determine location in terms of grid units for midpoint of
!   first layer above surface
    sfcchk=zero
    call grdcrd1(sfcchk,zges,nsig,1)

!    Checks based on observation location relative to model surface and top
     rlow=max(sfcchk-dpres,zero)
     rhgh=max(dpres-r0_001-rsigp,zero)
     if(luse(i))then
        awork(1) = awork(1) + one
        if(rlow/=zero) awork(2) = awork(2) + one
        if(rhgh/=zero) awork(3) = awork(3) + one
     end if
 
!    Adjust observation error
!    ratio_errors=error/((data(ier,i)+adjustment)*sqrt(dup(i)))  ! qc dependent adjustment
     wflate=zero
     if (ictype(ikx)==188 .or. ictype(ikx)==288 .or. ictype(ikx)==195 .or. ictype(ikx)==295) then  !inflate Mesonet obs error for gusts<7.2m/s
       if (data(igust,i)<7.2) then
          wflate=4.0_r_kind*data(ier,i)
       else
          wflate=0.8_r_kind*data(ier,i)
       end if
     end if
     ratio_errors=error/((data(ier,i)+drpx+wflate+1.0e6*rhgh+four*rlow)*sqrt(dup(i)))
     error=one/error

!    Compute innovations
     ddiff=data(igust,i)-gustges

!    If requested, setup for single obs test.
     if (oneobtest) then
        ddiff=maginnov
        error=one/magoberr
        ratio_errors=one
     endif

!    Gross check using innovation normalized by error
     obserror = one/max(ratio_errors*error,tiny_r_kind)
     obserrlm = max(cermin(ikx),min(cermax(ikx),obserror))
     residual = abs(ddiff)
     ratio    = residual/obserrlm
     if (ratio> cgross(ikx) .or. ratio_errors < tiny_r_kind) then
        if (luse(i)) awork(6) = awork(6)+one
        error = zero
        ratio_errors=zero
     end if
     if (ratio_errors*error <=tiny_r_kind) muse(i)=.false.

     if (nobskeep>0 .and. luse_obsdiag) muse(i)=obsdiags(i_gust_ob_type,ibin)%tail%muse(nobskeep)

!    Compute penalty terms (linear & nonlinear qc).
     val      = error*ddiff
     if(luse(i))then
        val2     = val*val
        exp_arg  = -half*val2
        rat_err2 = ratio_errors**2
        if (cvar_pg(ikx) > tiny_r_kind .and. error > tiny_r_kind) then
           arg  = exp(exp_arg)
           wnotgross= one-cvar_pg(ikx)
           cg_gust=cvar_b(ikx)
           wgross = cg_term*cvar_pg(ikx)/(cg_gust*wnotgross)
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
           awork(4)=awork(4)+val2*rat_err2
           awork(5)=awork(5)+one
           awork(22)=awork(22)+valqc
        end if
        ress   = ddiff*scale
        ressw2 = ress*ress
        val2   = val*val
        rat_err2 = ratio_errors**2
        nn=1
        if (.not. muse(i)) then
           nn=2
           if(ratio_errors*error >=tiny_r_kind)nn=3
        end if
        if (abs(data(igust,i)-rmiss_single) >=tiny_r_kind) then
           bwork(1,ikx,1,nn)  = bwork(1,ikx,1,nn)+one           ! count
           bwork(1,ikx,2,nn)  = bwork(1,ikx,2,nn)+ress          ! (o-g)
           bwork(1,ikx,3,nn)  = bwork(1,ikx,3,nn)+ressw2        ! (o-g)**2
           bwork(1,ikx,4,nn)  = bwork(1,ikx,4,nn)+val2*rat_err2 ! penalty
           bwork(1,ikx,5,nn)  = bwork(1,ikx,5,nn)+valqc         ! nonlin qc penalty
        end if

     endif

     if (luse_obsdiag) then
        obsdiags(i_gust_ob_type,ibin)%tail%muse(jiter)=muse(i)
        obsdiags(i_gust_ob_type,ibin)%tail%nldepart(jiter)=ddiff
        obsdiags(i_gust_ob_type,ibin)%tail%wgtjo= (error*ratio_errors)**2
     endif

!    If obs is "acceptable", load array with obs info for use
!    in inner loop minimization (int* and stp* routines)
     if (.not. last .and. muse(i)) then

        allocate(my_head)
	m_alloc(ibin) = m_alloc(ibin) + 1
        my_node => my_head        ! this is a workaround
        call obsLList_appendNode(gusthead(ibin),my_node)
        my_node => null()

        my_head%idv = is
        my_head%iob = ioid(i)
        my_head%elat= data(ilate,i)
        my_head%elon= data(ilone,i)

!       Set (i,j) indices of guess gridpoint that bound obs location
        call get_ij(mm1,dlat,dlon,my_head%ij(1),my_head%wij(1))

        my_head%res     = ddiff
        my_head%err2    = error**2
        my_head%raterr2 = ratio_errors**2    
        my_head%time    = dtime
        my_head%b       = cvar_b(ikx)
        my_head%pg      = cvar_pg(ikx)
        my_head%luse    = luse(i)
        if(luse_obsdiag) then
           my_head%diags => obsdiags(i_gust_ob_type,ibin)%tail
 
           my_diag => my_head%diags
           if(my_head%idv /= my_diag%idv .or. &
              my_head%iob /= my_diag%iob ) then
              call perr(myname,'mismatching %[head,diags]%(idv,iob,ibin) =', &
                (/is,ioid(i),ibin/))
              call perr(myname,'my_head%(idv,iob) =',(/my_head%idv,my_head%iob/))
              call perr(myname,'my_diag%(idv,iob) =',(/my_diag%idv,my_diag%iob/))
              call die(myname)
           endif
        endif
        my_head => null()
     endif


!    Save stuff for diagnostic output
     if(conv_diagsave .and. luse(i))then
        ii=ii+1
        rstation_id     = data(id,i)
        cdiagbuf(ii)    = station_id         ! station id
 
        rdiagbuf(1,ii)  = ictype(ikx)        ! observation type
        rdiagbuf(2,ii)  = icsubtype(ikx)     ! observation subtype
 
        rdiagbuf(3,ii)  = data(ilate,i)      ! observation latitude (degrees)
        rdiagbuf(4,ii)  = data(ilone,i)      ! observation longitude (degrees)
        rdiagbuf(5,ii)  = data(istnelv,i)    ! station elevation (meters)
        rdiagbuf(6,ii)  = presw              ! observation pressure (hPa)
        rdiagbuf(7,ii)  = data(ihgt,i)       ! observation height (meters)
        rdiagbuf(8,ii)  = dtime-time_offset  ! obs time (hours relative to analysis time)

        rdiagbuf(9,ii)  = data(iqc,i)        ! input prepbufr qc or event mark
        rdiagbuf(10,ii) = rmiss_single       ! setup qc or event mark
        rdiagbuf(11,ii) = data(iuse,i)       ! read_prepbufr data usage flag
        if(muse(i)) then
           rdiagbuf(12,ii) = one             ! analysis usage flag (1=use, -1=not used)
        else
           rdiagbuf(12,ii) = -one
        endif

        err_input = data(ier,i)
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

        rdiagbuf(13,ii) = rwgt               ! nonlinear qc relative weight
        rdiagbuf(14,ii) = errinv_input       ! prepbufr inverse obs error (K**-1)
        rdiagbuf(15,ii) = errinv_adjst       ! read_prepbufr inverse obs error (K**-1)
        rdiagbuf(16,ii) = errinv_final       ! final inverse observation error (K**-1)
 
        rdiagbuf(17,ii) = data(igust,i)      ! GUST observation (K)
        rdiagbuf(18,ii) = ddiff              ! obs-ges used in analysis (K)
        rdiagbuf(19,ii) = data(igust,i)-gustges! obs-ges w/o bias correction (K) (future slot)
 
        rdiagbuf(20,ii) = factw              ! 10m wind reduction factor

        rdiagbuf(21,ii) = data(idomsfc,i)    ! dominate surface type
        rdiagbuf(22,ii) = zsges              ! model terrain at ob location
        r_prvstg        = data(iprvd,i)
        cprvstg(ii)     = c_prvstg           ! provider name
        r_sprvstg       = data(isprvd,i)
        csprvstg(ii)    = c_sprvstg          ! subprovider name

        if (lobsdiagsave) then
           ioff=ioff0
           do jj=1,miter 
              ioff=ioff+1 
              if (obsdiags(i_gust_ob_type,ibin)%tail%muse(jj)) then
                 rdiagbuf(ioff,ii) = one
              else
                 rdiagbuf(ioff,ii) = -one
              endif
           enddo
           do jj=1,miter+1
              ioff=ioff+1
              rdiagbuf(ioff,ii) = obsdiags(i_gust_ob_type,ibin)%tail%nldepart(jj)
           enddo
           do jj=1,miter
              ioff=ioff+1
              rdiagbuf(ioff,ii) = obsdiags(i_gust_ob_type,ibin)%tail%tldepart(jj)
           enddo
           do jj=1,miter
              ioff=ioff+1
              rdiagbuf(ioff,ii) = obsdiags(i_gust_ob_type,ibin)%tail%obssen(jj)
           enddo
        endif
 
     end if


  end do

! Release memory of local guess arrays
  call final_vars_

! Write information to diagnostic file
  if(conv_diagsave .and. ii>0)then
     call dtime_show(myname,'diagsave:gust',i_gust_ob_type)
     write(7)'gst',nchar,nreal,ii,mype,ioff0
     write(7)cdiagbuf(1:ii),rdiagbuf(:,1:ii)
     deallocate(cdiagbuf,rdiagbuf)

     write(7)cprvstg(1:ii),csprvstg(1:ii)
     deallocate(cprvstg,csprvstg)
  end if

! End of routine

  return
  contains

  subroutine check_vars_ (proceed)
  logical,intent(inout) :: proceed
  integer(i_kind) ivar, istatus
! Check to see if required guess fields are available
  call gsi_metguess_get ('var::ps', ivar, istatus )
  proceed=ivar>0
  call gsi_metguess_get ('var::z' , ivar, istatus )
  proceed=proceed.and.ivar>0
  end subroutine check_vars_ 

  subroutine init_vars_

  real(r_kind),dimension(:,:  ),pointer:: rank2=>NULL()
  character(len=5) :: varname
  integer(i_kind) ifld, istatus

! If require guess vars available, extract from bundle ...
  if(size(gsi_metguess_bundle)==nfldsig) then
!    get gust ...
     varname='gust'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank2,istatus)
     if (istatus==0) then
         if(allocated(ges_gust))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_gust(size(rank2,1),size(rank2,2),nfldsig))
         ges_gust(:,:,1)=rank2
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank2,istatus)
            ges_gust(:,:,ifld)=rank2
         enddo
     else
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     endif
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
  else
     write(6,*) trim(myname), ': inconsistent vector sizes (nfldsig,size(metguess_bundle) ',&
                 nfldsig,size(gsi_metguess_bundle)
     call stop2(999)
  endif
  end subroutine init_vars_

  subroutine final_vars_
    if(allocated(ges_z   )) deallocate(ges_z   )
    if(allocated(ges_ps  )) deallocate(ges_ps  )
    if(allocated(ges_gust)) deallocate(ges_gust)
  end subroutine final_vars_

end subroutine setupgust

