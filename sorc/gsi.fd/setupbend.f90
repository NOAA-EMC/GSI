subroutine setupbend(lunin,mype,awork,nele,nobs,conv_diagsave,super_gps,toss_gps)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    setupbend    compute rhs of oi for gps bending angle
!   prgmmr: cucurull, l.    org: JCSDA/NCEP           date: 2005-12-01
!
! abstract:  For gps bending angle observations, this routine
!              a) reads obs assigned to given mpi task (geographic region),
!              b) simulates obs from guess,
!              c) apply some quality control to obs,
!              d) load weight and innovation arrays used in minimization
!              e) collects statistics for runtime diagnostic output
!              f) writes additional diagnostic information to output file
!
! program history log:
!   2005-12-01  cucurull - original code
!   2005-12-09  derber   - remove psfcg and use ges_lnps instead
!   2005-12-21  treadon  - add super_gps, move some diagnostics statistics
!                         to genstat_gps
!   2006-01-04  treadon  - correct inconsistency when using qcfail
!   2006-02-02  treadon  - rename lnprsl as ges_lnprsl
!   2006-02-24  derber  - modify to take advantage of convinfo module
!   2006-02-24 cucurull - update QC parameters and compute representativeness error
!                       - fix bug when counting obs that fail gross qc check
!                       - fix bug when rejecting obs that fail sats QC
!   2006-04-14  middlecoff - changed IF test to avoid out-of-bounds-reference on DATA
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!   2006-07-31  kleist  - change to use ges_ps instead of lnps
!   2006-09-20  cucurull - use geopotential heights at intermediate levels instead 
!                          of midpoint,levels,remove psges, generalize minimization terms
!                          to hybrid coordinate,penalize high level obs,new QC checks,
!                          remove obs above 30 km,add b_tkges, remove dtime,  
!                          improve obs pressure calculation for diagnostic purposes,
!                          increase the extended atmosphere from 6 to 10 levels
!   2006-10-20 cucurull - update QC statistical checks and representativeness error with 
!                         COSMIC data
!                       - add information to diagnostic file
!   2007-01-29 cucurull - remove observations below 6 km
!   2007-03-01 derber - add toss_gps; simplify profile qc loop
!
!   input argument list:
!     lunin    - unit from which to read observations
!     mype     - mpi task id
!     nele     - number of data elements per observation
!     nobs     - number of observations
!
!   output argument list:
!     gpswork  - array containing information about obs-ges statistics
!     awork    - array containing information for data counts and gross checks
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind,r_single
  use obsmod, only: gpshead,nprof_gps,grids_dim,gpstail,&
       gps_allhead,gps_alltail

  use guess_grids, only: ges_lnprsi,hrdifsig,geop_hgti,nfldsig,&
       ntguessig,ges_z,ges_tv,ges_q
  use gridmod, only: lat2,lon2,nsig,bk5
  use gridmod, only: get_ij,latlon11
  use constants, only: fv,n_a,n_b,deg2rad,tiny_r_kind
  use constants, only: zero,half,one,two,eccentricity,semi_major_axis,&
       grav_equator,somigliana,flattening,grav_ratio,grav,rd,eps,five,four
  use constants, only: one_tenth,izero,ione,cg_term,three
  use qcmod, only: npres_print,repe_gps,ptop,pbot
  use lagmod
  use jfunc, only: jiter,last
  use convinfo, only: nconvtype,cermin,cermax,cgross,cvar_b,cvar_pg,ictype
  implicit none

! Declare local parameters
  real(r_kind),parameter:: r0_01 = 0.01_r_kind
  real(r_kind),parameter:: r0_8= 0.8_r_kind
  real(r_kind),parameter:: r1_3 = 1.3_r_kind
  real(r_kind),parameter:: ten = 10.0_r_kind
  real(r_kind),parameter:: eight = 8.0_r_kind
  real(r_kind),parameter:: six = 6.0_r_kind
  real(r_kind),parameter:: ds=5000.0_r_kind
  real(r_kind),parameter:: r15=15.0_r_kind
  real(r_kind),parameter:: r30=30.0_r_kind
  real(r_kind),parameter:: r1em3 = 1.0e-3_r_kind
  real(r_kind),parameter:: r1em6 = 1.0e-6_r_kind

! Declare passed variables
  logical,intent(in):: conv_diagsave
  integer(i_kind),intent(in):: lunin,mype,nele,nobs
  real(r_kind),dimension(100+7*nsig),intent(inout):: awork
  real(r_kind),dimension(nsig,max(1,nprof_gps)),intent(inout):: super_gps
  real(r_kind),dimension(max(1,nprof_gps)),intent(inout):: toss_gps

! Declare local variables

  real(r_kind),dimension(grids_dim,nobs) :: dbend_loc,xj
  real(r_kind),dimension(grids_dim):: dnj, ddnj,grid_s,ref_rad_s,grad_ref_s

  real(r_kind) rsig,rsig_up,ddbend,tmean,qmean
  real(r_kind) sin2,termg,termr,termrg,hob
  real(r_kind) fact,pw,nrefges1,nrefges2
  real(r_kind) ratio,residual,obserror,obserrlm
  real(r_kind) val2,ressw2,scale,ress
  real(r_kind) val,valqc

  real(r_kind),dimension(nobs):: dpres,dpressure,dbend
  real(r_kind),dimension(3,nobs):: gps2work
  real(r_kind),dimension(nele,nobs):: data
  real(r_kind),dimension(nobs):: ratio_errors,cutoff
  real(r_kind),dimension(nsig):: dup
  real(r_kind),dimension(lat2,lon2,nsig,nfldsig)::geop_height
  real(r_kind),dimension(lat2,lon2,nfldsig):: h_in
  real(r_kind) pressure,hob_s,d_ref_rad
  real(r_kind),dimension(4) :: w4,dw4
  real(r_single),allocatable,dimension(:,:)::rdiagbuf
  real(r_single),dimension(nobs):: qcfail_loc,qcfail_high,qcfail_gross,qcfail_stats_1,qcfail_stats_2
  
  integer(i_kind) ier,ilon,ilat,ihgt,igps,itime,ikx,iuse, &
                  iprof,ipctc,iroc,isatid,iptid,ilate,ilone
  integer(i_kind) i,j,k,n,it,nreal,ii,jj,l,ikxx,ip1
  integer(i_kind) mm1,nsig_up,ihob,istatus
  integer(i_kind) kprof,istat,jprof
  integer(i_kind),dimension(8):: jgrd
  integer(i_kind),dimension(4) :: gps_ij

  real(r_kind),dimension(3,nsig+10) :: q_w
  real(r_kind),dimension(nsig,nobs):: n_t,n_q,n_p,rges,gp2gm,prsltmp_o,tges_o
  real(r_kind),dimension(nsig) :: hges,irefges,zges
  real(r_kind),dimension(nsig+1) :: prsltmp
  real(r_kind),dimension(0:nsig+11) :: ref_rad
  real(r_kind),dimension(nsig+10,nobs):: nrefges
  real(r_kind) :: tplats,tplatse,tplons,rocprof,tpdpres,dtptimes,dpress
  real(r_kind),dimension(nsig):: tges,qges

  logical,dimension(nobs):: luse,muse
  logical,dimension(nobs):: qcfail

!*******************************************************************************
! Read and reformat observations in work arrays.
  read(lunin)data,luse


  ier=1        ! index of obs error in data array (now 1/(original obs error))
  ilon=2       ! index of grid relative obs location (x)
  ilat=3       ! index of grid relative obs location (y)
  ihgt=4       ! index of obs vertical coordinate in data array
  igps=5       ! index of gps data (or residual) in data array
  itime=6      ! index of obs time in data array
  ikxx=7       ! index of observation type
  iprof=8      ! index of profile
  ipctc=9      ! index of percent confidence
  iroc=10      ! index of local radius of curvature
  isatid=11    ! index of satellite identifier
  iptid=12     ! index of platform transmitter id number
  iuse=13      ! index of use parameter
  ilone=14     ! index of earth relative longitude (degrees)
  ilate=15     ! index of earth relative latitude (degrees)


! Intialize variables
  nsig_up=nsig+10 ! extend 10 levels above interface level nsig
  rsig=float(nsig)
  rsig_up=float(nsig_up)
  mm1=mype+1
  qcfail=.false.
  qcfail_loc=zero;qcfail_gross=zero;qcfail_stats_1=zero;qcfail_stats_2=zero
  qcfail_high=zero
  toss_gps=zero

! define new equally spaced grid s
  do j=0,grids_dim-1
    grid_s(j+1)=j*ds
  enddo

! Convert model geopotential heights at interface levels to msl units
  do jj=1,nfldsig
     do j=1,lon2
        do i=1,lat2
           do k=1,nsig
              geop_height(i,j,k,jj) = geop_hgti(i,j,k,jj)
              if (ges_z(i,j,jj)>zero) geop_height(i,j,k,jj) = &
                   geop_height(i,j,k,jj) + ges_z(i,j,jj)
           end do
        end do
     end do
  end do

! If requested, save select data for output to diagnostic file
  if(conv_diagsave)then
     nreal=16
     allocate(rdiagbuf(nreal,nobs))
  endif

  do i=1,nobs ! loop over obs 

    tplats=data(ilat,i)
    tplons=data(ilon,i)
    tplatse= data(ilate,i)
    rocprof=data(iroc,i)
    tpdpres=data(ihgt,i)
    ikx=nint(data(ikxx,i))
    dtptimes=data(itime,i)

! Interpolate log(p),temperature,specific humidity to obs location

   call tintrp2a(ges_lnprsi,prsltmp,tplats,tplons,dtptimes,hrdifsig,&
        1,nsig+1,mype,nfldsig)
   call tintrp2a(ges_tv,tges,tplats,tplons,dtptimes,hrdifsig,&
        1,nsig,mype,nfldsig)
   call tintrp2a(ges_q,qges,tplats,tplons,dtptimes,hrdifsig,&
        1,nsig,mype,nfldsig)
   call tintrp2a(geop_height,hges,tplats,tplons,dtptimes,hrdifsig,&
        1,nsig,mype,nfldsig)

   prsltmp_o(1:nsig,i)=prsltmp(1:nsig) ! needed in minimization

! Convert geopotential height at layer midpoints to geometric height using
! equations (17, 20, 23) in MJ Mahoney's note "A discussion of various
! measures of altitude" (2001).  Available on the web at
! http://mtp.jpl.nasa.gov/notes/altitude/altitude.html
!
!   termg  = equation 17
!   termr  = equation 21
!   termrg = first term in the denominator of equation 23
!   zges   = equation 23

   sin2= sin(tplatse*deg2rad)**2
   termg = grav_equator * &
           ((one+somigliana*sin2)/sqrt(one-eccentricity*eccentricity*sin2))
   termr = semi_major_axis / (one + flattening + grav_ratio - two*flattening*sin2)
   termrg = (termg/grav)*termr
   do k=1,nsig
        zges(k) = (termr*hges(k)) / (termrg-hges(k))  ! eq (23)
        gp2gm(k,i)= termr/(termrg-hges(k))+((termr*hges(k))/(termrg-hges(k))**2)
        rges(k,i) = zges(k) + rocprof                  ! radius r_i
   end do

   do k=1,nsig
    if(k>1) then
     qmean=(qges(k)+qges(k-1))/two
     tmean=(tges(k)+tges(k-1))/two
    else
     qmean=qges(1)
     tmean=tges(1)
    endif
    tges_o(k,i)=tmean  ! needed in minimization
    fact=(one+fv*qmean)
    pw=eps+qmean*(one-eps)
    pressure=ten*exp(prsltmp(k)) ! pressure of interface level in mb
    nrefges1=n_a*(pressure/tmean)*fact
    nrefges2=n_b*qmean*pressure*fact**2/(tmean**2*pw)
    nrefges(k,i)=nrefges1+nrefges2         ! refractivity N_i
    irefges(k)= one+(r1em6*nrefges(k,i))   ! index of refractivity n_i
    ref_rad(k)=irefges(k)*rges(k,i)        ! refractivity index-radius product x_i

! Terms for the minimization
    n_q(k,i)= &
       n_a*(pressure/tmean)*fv+&
       (n_b/(tmean**2*pw))*pressure*fact**2+&
       (n_b/(tmean**2*pw))*pressure*qmean*two*fv*fact-&
       (n_b/(tmean**2*pw**2))*pressure*qmean*fact**2*(one-eps)
    n_p(k,i)= &
        (n_a/tmean)*fact+&
        (n_b/(tmean**2*pw))*qmean*fact**2
        n_p(k,i)= n_p(k,i)*ten*bk5(k)
    n_t(k,i)= &
        -n_a*fact*(pressure/tmean**2)-n_b*qmean*fact**2*two*&
        (pressure/(tmean**3*pw))

! Get obs pressure if inside vertical grid for diagnostic 
    hob=tpdpres
    call grdcrd(hob,1,ref_rad(1),nsig,1)
    if (hob>one .and. hob<rsig) then 
      call tintrp3(ges_lnprsi,dpress,tplats,tplons,hob,&
             dtptimes,hrdifsig,1,mype,nfldsig)
      dpressure(i)=ten*exp(dpress) !in mb
    endif 

   end do 

! Extending atmosphere above interface level nsig
   d_ref_rad=ref_rad(nsig)-ref_rad(nsig-1)
   do k=1,10
     ref_rad(nsig+k)=ref_rad(nsig)+ k*d_ref_rad ! extended x_i
     nrefges(nsig+k,i)=nrefges(nsig+k-1,i)**2/nrefges(nsig+k-2,i) ! exended N_i
   end do

! Lagrange coefficients
   ref_rad(0)=ref_rad(3)
   ref_rad(nsig_up+1)=ref_rad(nsig_up-2)
   do k=1,nsig_up
     call setq(q_w(:,k),ref_rad(k-1:k+1),3)
   enddo

! Get observation bending angle
   dbend_loc(:,i)=zero;dbend(i)=zero

   gps2work(1,i)=r1em3*(data(ihgt,i)-data(iroc,i))! impact height in km
   gps2work(2,i)=data(ilate,i)                           ! lat in degree
   dpres(i)=data(ihgt,i)
   muse(i)=nint(data(iuse,i)) <= jiter

! Locate obs in the corresponding profile grid. Compute bending if inside vertical grid
   hob=dpres(i)     
   call grdcrd(hob,1,ref_rad(1),nsig_up,1)
   if (hob>one .and. hob<rsig) then ! obs inside model grid - reject obs if above level nsig

! Get refractivity index-radius and [d(ln(n))/dx] in new grid.
   intloop: do j=1,grids_dim
     ref_rad_s(j)=sqrt(grid_s(j)*grid_s(j)+dpres(i)*dpres(i)) !x_j
     xj(j,i)=ref_rad_s(j)
     hob_s=ref_rad_s(j)
     call grdcrd(hob_s,1,ref_rad(1),nsig_up,1)
     dbend_loc(j,i)=hob_s  ! location of x_j with respect to extended x_i

     if (hob_s < rsig_up) then  ! obs insided the new grid
      ihob=hob_s

! Compute refractivity and derivative at target points using Lagrange interpolators
      call slagdw(ref_rad(ihob-1:ihob+2),ref_rad_s(j),&
               q_w(:,ihob),q_w(:,ihob+1),&
               w4,dw4,4)
      if(ihob==1) then
          w4(4)=w4(4)+w4(1); w4(1:3)=w4(2:4);w4(4)=zero
          dw4(4)=dw4(4)+dw4(1);dw4(1:3)=dw4(2:4);dw4(4)=zero
          ihob=ihob+1
      endif
      if(ihob==nsig_up-1) then
         w4(1)=w4(1)+w4(4); w4(2:4)=w4(1:3);w4(1)=zero
         dw4(1)=dw4(1)+dw4(4); dw4(2:4)=dw4(1:3);dw4(1)=zero
         ihob=ihob-1
      endif

!     dnj(j)=dot_product(w4,nrefges(ihob-1:ihob+2,i)) ! refractivity N_j
      ddnj(j)=dot_product(dw4,nrefges(ihob-1:ihob+2,i)) ! derivative (dN/dx)_j
!     dnj(j)=max(zero,abs(dnj(j)))
      ddnj(j)=max(zero,abs(ddnj(j)))
      else
        write(6,*) 'GPS obs outside integration grid','obs=',i
        call stop2(92)
      endif
    end do intloop

    dbend(i)=ds*(r1em6*ddnj(1)/ref_rad_s(1))
    do j=2,grids_dim
      ddbend=ds*(r1em6*ddnj(j)/ref_rad_s(j))
      dbend(i)=dbend(i)+two*ddbend
    end do
    dbend(i)=dpres(i)*dbend(i)  

 end if 

! Tune observation error to account for representativeness
! error. Preliminary values

  repe_gps=one
  if(gps2work(1,i) > r30) then
!     repe_gps=0.2_r_kind
  else
    if(gps2work(1,i) < eight)then
       repe_gps=0.9_r_kind
    else
       repe_gps=0.5_r_kind
    endif
  end if

! Penalize high level obs
  if (gps2work(1,i) >= 25_r_kind) then
      repe_gps = (0.6_r_kind*gps2work(1,i)-14_r_kind)*repe_gps
  endif

  ratio_errors(i) = data(ier,i)/abs(data(ier,i)*repe_gps)
  data(ier,i)=one/data(ier,i)

! Check to see if observations is above the top of the model
! Remove observation if obs pressure is below the first
! mid sigma level by setting observation (1/error) to zero.
! Make no adjustment if observation falls within vertical
! domain.

    if (hob < one .or. hob > rsig) ratio_errors(i)=zero

! Increment obs counter along with low and high obs counters
    if(luse(i))then
      awork(1)=awork(1)+one
      if(hob > rsig) awork(3)=awork(3)+one
      if(hob <  one) awork(2)=awork(2)+one
    endif

! Accumulate diagnostic information        

    gps2work(3,i)=(data(igps,i)-dbend(i))/((data(igps,i)+dbend(i))/two)

    if(conv_diagsave)then
     rdiagbuf(1,i)=gps2work(1,i)  ! impact height in km
     rdiagbuf(2,i)=data(ilate,i)  ! lat in degree
     rdiagbuf(3,i)=data(ilone,i)  ! lon in degree
     rdiagbuf(4,i)=data(itime,i)  ! time to analysis
     rdiagbuf(5,i)=data(igps,i)   ! obs
     rdiagbuf(6,i)=dbend(i)       ! model simulation
     rdiagbuf(7,i)=gps2work(3,i)  ! incremental bending angle
     rdiagbuf(9,i)=data(isatid,i) ! satellite identifier
     rdiagbuf(10,i)=data(iptid,i) ! platform transmitter id number
     rdiagbuf(11,i)=data(ipctc,i) ! index of percent confidence
    endif

    data(igps,i)=data(igps,i)-dbend(i) !innovation vector
    data(ihgt,i)=hob

! Gross error checks
    obserror = one/max(ratio_errors(i)*data(ier,i),tiny_r_kind)
    obserrlm = max(cermin(ikx),min(cermax(ikx),obserror))
    residual = abs(data(igps,i))
    ratio    = residual/obserrlm
    if (ratio > cgross(ikx) .or. ratio_errors(i) < tiny_r_kind) then
       if (luse(i) .and. (ratio_errors(i) < tiny_r_kind)) qcfail_loc(i)=one
       if (luse(i) .and. (ratio > cgross(ikx))) then
          awork(4) = awork(4)+one
          qcfail_gross(i)=one
       endif
       data(ier,i) = zero
       ratio_errors(i) = zero
       muse(i)=.false.
    end if

! Remove observations above 30 km in order to penalize increments at top model
   if(gps2work(1,i) > r30) then
      data(ier,i) = zero
      ratio_errors(i) = zero
      qcfail_high(i)=one
      muse(i)=.false.
   endif
! QC checks based on latitude and height

  if(gps2work(2,i) < -r30) then                !SH
    if(gps2work(1,i) >= r30)then
      cutoff(i)=three              
    else
      if(gps2work(1,i) <= ten)then
        cutoff(i)=-r0_8*gps2work(1,i)+ten
      else 
        cutoff(i)=two
      end if
    end if
  else if(gps2work(2,i) > r30) then            !NH
    if(gps2work(1,i) >= r30)then
      cutoff(i)=three 
    else
      if(gps2work(1,i) <= ten)then
        cutoff(i)=-r0_8*gps2work(1,i)+ten
      else
        cutoff(i)=two
      end if
    end if
  else                                         !TR
    if(gps2work(1,i) >= r30)then
      cutoff(i)=three
    else
      if(gps2work(1,i) <= ten)then
        cutoff(i)=-r1_3*gps2work(1,i)+r15
      else 
        cutoff(i)=two
      end if
    end if
  end if

  if(gps2work(1,i) >= r30)then
     cutoff(i)=two*cutoff(i)*r0_01
  else
    if(gps2work(1,i) <= ten)then
!       cutoff(i)=three*cutoff(i)*r0_01
       cutoff(i)=one*cutoff(i)*r0_01
    else
       cutoff(i)=four*cutoff(i)*r0_01
    end if
  end if

  if(abs(gps2work(3,i))> cutoff(i)) then
      qcfail(i)=.true.
      qcfail_stats_1(i)=one

  endif

! Remove data below 6 km.
  if (gps2work(1,i) <= six)then
     data(ier,i) = zero
     ratio_errors(i) = zero
     muse(i)=.false.
  endif



! End of loop over obs
 end do

 do i=1,nobs
    kprof = data(iprof,i)

    if(qcfail(i) .and. gps2work(1,i) <= ten)then
      do j=1,nobs
        jprof = data(iprof,j)
        if(kprof == iprof .and. .not. qcfail(j) .and. gps2work(1,j) < gps2work(1,i))then

!       Remove data below when suspicious of error
            qcfail(j) = .true. 
            qcfail_stats_2(j)=one
        end if
      end do
    end if
 end do
 do i=1,nobs
    kprof = data(iprof,i)
    if(qcfail(i))then
      data(ier,i)=zero
      ratio_errors(i) = zero
      if (gps2work(1,i) <= ten )then
        toss_gps(kprof) = max(toss_gps(kprof),data(ihgt,i))
      end if
      if (luse(i))then
! Counting obs per level and per profile that failed the stats QC checks
        if(gps2work(2,i)> r30) then
          awork(22) = awork(22)+one                !NH
        else if(gps2work(2,i)< -r30)then
          awork(23) = awork(23)+one                !SH
        else
          awork(24) = awork(24)+one                !TR
        end if
      end if
    else
! Tuning errors (superobs)
      if(ratio_errors(i)*data(ier,i)>tiny_r_kind .and. luse(i)) then
         j=data(ihgt,i)
         super_gps(j,kprof) = super_gps(j,kprof)+one
      endif
    end if
! Testing superrefraction (pending)

  end do

! Allocate arrays used to pass values to genstats_gps

  do i=1,nobs
     if (ratio_errors(i)*data(ier,i) <= tiny_r_kind) muse(i) = .false.
     ikx=nint(data(ikxx,i))



!    Save values needed for generate of statistics for all observations

    if(.not. associated(gps_allhead))then
         allocate(gps_allhead,stat=istat)
         if(istat /= 0)write(6,*)' failure to write gps_allhead '
         gps_alltail => gps_allhead
     else
         allocate(gps_alltail%llpoint,stat=istat)
         gps_alltail => gps_alltail%llpoint
          if(istat /= 0)write(6,*)' failure to write gps_alltail%llpoint '
     end if
     gps_alltail%ratio_err= ratio_errors(i)
     gps_alltail%obserr   = data(ier,i)
     gps_alltail%luse     = luse(i)
     gps_alltail%muse     = muse(i)
     gps_alltail%dataerr  = data(ier,i)*data(igps,i)
     gps_alltail%pg       = cvar_pg(ikx)
     gps_alltail%b        = cvar_b(ikx)
     gps_alltail%loc      = data(ihgt,i)
     gps_alltail%pressure = dpressure(i)
     gps_alltail%type     = data(ikxx,i)
     gps_alltail%rinc     = gps2work(3,i)
     gps_alltail%kprof    = data(iprof,i)


! If obs is "acceptable", load array with obs info for use
! in inner loop minimization (int* and stp* routines)
     if (.not. last .and. muse(i) ) then
       if(.not. associated(gpshead))then
            allocate(gpshead,stat=istat)
            if(istat /= 0)write(6,*)' failure to write gpshead '
            gpstail => gpshead
        else
            allocate(gpstail%llpoint,stat=istat)
            gpstail => gpstail%llpoint
            if(istat /= 0)write(6,*)' failure to write gpstail%llpoint '
        end if
        allocate(gpstail%termtlin(nsig),gpstail%b_tkges(nsig),gpstail%b_qin(nsig), &
                 gpstail%b_pin(nsig),gpstail%b_tin(nsig), &
                 gpstail%b_rges(nsig),gpstail%b_gp2gm(nsig), &
                 gpstail%b_n(nsig+10),gpstail%b_pkges(nsig), &
                 gpstail%b_loc(grids_dim),gpstail%b_xj(grids_dim), &
                 gpstail%ij(4,nsig),stat=istatus)
        if (istatus/=0) write(6,*)'SETUPBEND:  allocate error for gps_point, istatus=',istatus

        gps_alltail%mmpoint  => gpstail

! Set (i,j) indices of guess gridpoint that bound obs location
        call get_ij(mm1,data(ilat,i),data(ilon,i),gps_ij,gpstail%wij(1))

        do j=1,nsig
          gpstail%ij(1,j)=gps_ij(1)+(j-1)*latlon11
          gpstail%ij(2,j)=gps_ij(2)+(j-1)*latlon11
          gpstail%ij(3,j)=gps_ij(3)+(j-1)*latlon11
          gpstail%ij(4,j)=gps_ij(4)+(j-1)*latlon11
        enddo
        do k=1,nsig
         gpstail%b_qin(k)=n_q(k,i)
         gpstail%b_pin(k)=n_p(k,i)
         gpstail%b_tin(k)=n_t(k,i)
         gpstail%b_rges(k)=rges(k,i)
         gpstail%b_gp2gm(k)=gp2gm(k,i)
         gpstail%b_pkges(k)=prsltmp_o(k,i)
         gpstail%b_tkges(k)=tges_o(k,i)
        end do
        do k=1,nsig_up
         gpstail%b_n(k)=nrefges(k,i)
        enddo
        do j=1,grids_dim
          gpstail%b_loc(j)=dbend_loc(j,i)
          gpstail%b_xj(j)=xj(j,i)
        enddo

        gpstail%raterr2= ratio_errors(i)**2     
        gpstail%res    = data(igps,i)
        gpstail%err2   = data(ier,i)**2
        gpstail%time   = data(itime,i)
        gpstail%b      = cvar_b(ikx)
        gpstail%pg     = cvar_pg(ikx)
        gpstail%b_imp  = dpres(i)
        gpstail%luse   = luse(i)

     end if

  end do

! Write information to diagnostic file
  if(conv_diagsave)then
     ii=0
     do i=1,nobs
        rdiagbuf(8,i)= data(ier,i)               ! 1/obserror
        rdiagbuf(12,i)=qcfail_loc(i)
        rdiagbuf(13,i)=qcfail_gross(i)
        rdiagbuf(14,i)=qcfail_high(i)
        rdiagbuf(15,i)=qcfail_stats_1(i)
        rdiagbuf(16,i)=qcfail_stats_2(i)
        if(luse(i))then
         ii=ii+1
         do j=1,nreal
          rdiagbuf(j,ii)=rdiagbuf(j,i)
         end do
       end if
     end do
     write(7)'gps',nreal,ii,mype
     write(7)rdiagbuf(:,1:ii)
     deallocate(rdiagbuf)
  end if

end subroutine setupbend
