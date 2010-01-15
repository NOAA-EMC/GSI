
subroutine setupoz(lunin,mype,stats_oz,nlevs,nreal,nobs,&
     obstype,isis,is,ozone_diagsave)

!$$$  subprogram documentation block
!                .      .    .
! subprogram:    setupoz --- Compute rhs of oi for sbuv ozone obs
!
!   prgrmmr:     parrish          org: np22                date: 1990-10-06
!
! abstract:      For sbuv ozone observations (layer amounts and total 
!                column, this routine 
!                  a) reads obs assigned to given mpi task (geographic region),
!                  b) simulates obs from guess,
!                  c) apply some quality control to obs,
!                  d) load weight and innovation arrays used in minimization
!                  e) collects statistics for runtime diagnostic output
!                  f) writes additional diagnostic information to output file
!
! program history log:
!   1990-10-06  parrish
!   1998-04-10  weiyu yang
!   1999-03-01  wu, ozone processing moved into setuprhs from setupoz
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2003-12-23  kleist - modify to use pressure as vertical coordinate
!   2004-05-28  kleist - subroutine call update
!   2004-06-17  treadon - update documentation
!   2004-07-08  todling - added only's; removed gridmod; bug fix in diag
!   2004-07-15  todling - protex-compliant prologue; added intent's
!   2004-10-06  parrish - increase size of stats_oz for nonlinear qc,
!                         add nonlin qc penalty calc and obs count                 
!   2004-11-22  derber - remove weight, add logical for boundary point
!   2004-12-22  treadon - add outer loop number to name of diagnostic file
!   2005-03-02  dee - reorganize diagnostic file writes so that
!                         concatenated files are self-contained
!   2005-03-09  parrish - nonlinear qc change to account for inflated obs error
!   2005-03-16  derber  - change call to sproz to save observation time
!   2005-04-11  treadon - add logical to toggle on/off nonlinear qc code
!   2005-05-18  wu - add use of OMI total ozone data
!   2005-09-22  derber - modify extensively - combine with sproz - no change
!   2005-09-28  derber  - combine with prep,spr,remove tran and clean up
!   2005-10-07  treadon - fix bug in increment of ii
!   2005-10-14  derber  - input grid location and fix regional lat/lon
!   2006-01-09  treadon - remove unused variables
!   2006-02-03  derber  - modify for new obs control
!   2006-02-17  treadon - correct bug when processing data not assimilated
!   2006-03-21  treadon - add option to perturb observation
!   2006-06-06  su - move to wgtlim to constants module
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!                       - unify NL qc
!   2007-03-09      su  - remove option to perturb observation
!   2007-03-19  tremolet - binning of observations
!   2007-05-30  h.liu   - include rozcon with interpolation weights
!   2007-06-08  kleist/treadon - add prefix (task id or path) to diag_ozone_file
!   2007-06-05  tremolet - add observation diagnostics structure
!   2008-05-23  safford - add subprogram doc block, rm unused uses and vars
!   2008-01-20  todling - add obsdiag info to diag files
!   2009-01-08  todling - re-implemented obsdiag/tail
!
!   input argument list:
!     lunin          - unit from which to read observations
!     mype           - mpi task id
!     nlevs          - number of levels (layer amounts + total column) per obs   
!     nreal          - number of pieces of non-ozone info (location, time, etc) per obs
!     nobs           - number of observations
!     isis           - sensor/instrument/satellite id
!     is             - integer(i_kind) counter for number of obs types to process
!     obstype        - type of ozone obs
!     ozone_diagsave - switch on diagnostic output (.false.=no output)
!     stats_oz       - sums for various statistics as a function of level
!
!   output argument list:
!     stats_oz       - sums for various statistics as a function of level
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP; SGI Origin 2000; Compaq HP
!
!$$$ end documentation block
     
! !USES:

  use kinds, only: r_kind,r_single,i_kind

  use constants, only : izero,ione,zero,half,one,two,tiny_r_kind
  use constants, only : rozcon,cg_term,wgtlim,h300

  use obsmod, only : ozhead,oztail,i_oz_ob_type,dplat,nobskeep
  use obsmod, only : mype_diaghdr,dirname,time_offset,ianldate
  use obsmod, only : obsdiags,lobsdiag_allocated,lobsdiagsave

  use gsi_4dvar, only: nobs_bins,hr_obsbin

  use gridmod, only : get_ij,nsig

  use guess_grids, only : nfldsig,ges_prsi,ntguessig,ges_oz,hrdifsig

  use ozinfo, only : jpch_oz,error_oz,pob_oz,gross_oz,nusis_oz
  use ozinfo, only : iuse_oz,b_oz,pg_oz

  use jfunc, only : jiter,last,miter
  
  implicit none
  
! !INPUT PARAMETERS:

  integer(i_kind)                  , intent(in   ) :: lunin  ! unit from which to read observations
  integer(i_kind)                  , intent(in   ) :: mype   ! mpi task id
  integer(i_kind)                  , intent(in   ) :: nlevs  ! number of levels (layer amounts + total column) per obs   
  integer(i_kind)                  , intent(in   ) :: nreal  ! number of pieces of non-ozone info (location, time, etc) per obs
  integer(i_kind)                  , intent(in   ) :: nobs   ! number of observations
  character(20)                    , intent(in   ) :: isis   ! sensor/instrument/satellite id
  integer(i_kind)                  , intent(in   ) :: is     ! integer(i_kind) counter for number of obs types to process

  character(10)                    , intent(in   ) :: obstype          ! type of ozone obs
  logical                          , intent(in   ) :: ozone_diagsave   ! switch on diagnostic output (.false.=no output)

! !INPUT/OUTPUT PARAMETERS:

  real(r_kind),dimension(9,jpch_oz), intent(inout) :: stats_oz ! sums for various statistics as 
                                                               ! a function of level
!-------------------------------------------------------------------------

! Declare local parameters  
  integer(i_kind),parameter:: iint=ione
  integer(i_kind),parameter:: ireal=3_i_kind
  real(r_kind),parameter:: r10=10.0_r_kind

! Declare local variables  
  
  real(r_kind) ozobs,omg,rat_err2,dlat,dtime,dlon
  real(r_kind) cg_oz,wgross,wnotgross,wgt,arg,exp_arg,term
  real(r_kind) psi,errorinv
  real(r_kind),dimension(nlevs):: ozges,varinv3,ozone_inv
  real(r_kind),dimension(nlevs):: ratio_errors,error
  real(r_kind),dimension(nlevs-ione):: ozp
  real(r_kind),dimension(nlevs):: pobs,gross,tnoise
  real(r_kind),dimension(nreal+nlevs,nobs):: data
  real(r_kind),dimension(nsig+ione)::prsitmp
  real(r_single),dimension(nlevs):: pob4,grs4,err4
  real(r_single),dimension(ireal,nobs):: diagbuf
  real(r_single),allocatable,dimension(:,:,:)::rdiagbuf

  integer(i_kind) i,nlev,ii,jj,iextra,istat,ibin
  integer(i_kind) k,j,nz,jc,idia,irdim1,istatus
  integer(i_kind) ioff,itoss,ikeep,nkeep,ierror_toq,ierror_poq
  integer(i_kind) isolz,isolaz,icldmnt,isnoc,iacidx,istko,ifovn
  integer(i_kind) mm1,itime,ilat,ilon,isd,ilate,ilone,itoq,ipoq
  integer(i_kind),dimension(iint,nobs):: idiagbuf
  integer(i_kind),dimension(nlevs):: ipos,iouse

  real(r_kind),dimension(4):: tempwij
  integer(i_kind) nlevp
  
  character(12) string
  character(10) filex
  character(128) diag_ozone_file

  logical,dimension(nobs):: luse
  logical:: l_may_be_passive

  mm1=mype+ione


!
!*********************************************************************************
! Initialize arrays
  do j=1,nlevs
     ipos(j)=izero
     iouse(j)=-2_i_kind
     tnoise(j)=1.e10_r_kind
     gross(j)=1.e10_r_kind
     pobs(j)=1.e10_r_kind
  end do

  if(ozone_diagsave)then
     irdim1=3_i_kind
     if(lobsdiagsave) irdim1=irdim1+4*miter+ione
     allocate(rdiagbuf(irdim1,nlevs,nobs))
  end if

! Locate data for satellite in ozinfo arrays
  itoss =ione
  l_may_be_passive=.false.
  jc=izero
  do j=1,jpch_oz
     if (isis == nusis_oz(j)) then
        jc=jc+ione
        if (jc > nlevs) then
           write(6,*)'SETUPOZ:  ***ERROR*** in level numbers, jc,nlevs=',jc,nlevs,&
                ' ***STOP IN SETUPOZ***'
           call stop2(71)
        endif
        ipos(jc)=j

        iouse(jc)=iuse_oz(j)
        tnoise(jc)=error_oz(j)
        gross(jc)=min(r10*gross_oz(j),h300)
        if (obstype == 'sbuv2' ) then
           pobs(jc)=pob_oz(j) * 1.01325_r_kind
        else
           pobs(jc)=pob_oz(j)
        endif

        if (iouse(jc)<-ione .or. (iouse(jc)==-ione .and. &
             .not.ozone_diagsave)) then
           tnoise(jc)=1.e10_r_kind
           gross(jc) =1.e10_r_kind
           pobs(jc)  = zero
        endif
        if (iouse(jc)>-ione) l_may_be_passive=.true.
        if (tnoise(jc)<1.e4_r_kind) itoss=izero
     endif
  end do
  nlev=jc

! Handle error conditions
  if (nlevs>nlev) write(6,*)'SETUPOZ:  level number reduced for ',obstype,' ', &
       nlevs,' --> ',nlev
  if (nlev == izero) then
     if (mype==izero) write(6,*)'SETUPOZ:  no levels found for ',isis
     if (nobs>izero) read(lunin) 
     goto 135
  endif
  if (itoss==ione) then
     if (mype==izero) write(6,*)'SETUPOZ:  all obs variances > 1.e4.  Do not use ',&
          'data from satellite ',isis
     if (nobs>izero) read(lunin)
     goto 135
  endif

! Initialize variables used in ozone processing
  nkeep=izero
  do i=1,nobs
     ikeep=izero
     do k=1,nlev
        if (iouse(k)>izero .or. ozone_diagsave) ikeep=ione
     end do
     nkeep=nkeep+ikeep
  end do

! Read and transform ozone data
  read(lunin) data,luse

! If none of the data will be assimilated and don't need diagnostics,
! return to calling program
  if (nkeep==izero) return

!    index information for data array (see reading routine)

  isd=ione           ! index of satellite
  itime=2_i_kind     ! index of analysis relative obs time
  ilon=3_i_kind      ! index of grid relative obs location (x)
  ilat=4_i_kind      ! index of grid relative obs location (y)
  ilone=5_i_kind     ! index of earth relative longitude (degrees)
  ilate=6_i_kind     ! index of earth relative latitude (degrees)
  itoq=7_i_kind      ! index of total ozone error flag (sbuv2 only)
  ipoq=8_i_kind      ! index of profile ozone error flag (sbuv2 only)
  isolz=8_i_kind     ! index of solar zenith angle   (gome and omi only)
  isolaz=9_i_kind    ! index of solar azimuth angle   (gome only)
  icldmnt=10_i_kind  ! index of CLOUD AMOUNT IN SEGMENT (gome and omi only)
  isnoc=11_i_kind    ! index of snow cover (gome only)
  iacidx=12_i_kind   ! AEROSOL CONTAMINATION INDEX (gome and omi only)
  istko=13_i_kind    ! index of ASCENDING/DESCENDING ORBIT QUALIFIER (gome and omi only)
  ifovn=14_i_kind    ! index of scan position (gome and omi only)


! If requested, save data for diagnostic ouput
  if(ozone_diagsave)ii=izero

! Convert observation (lat,lon) from earth to grid relative values
  do i=1,nobs

     dlat=data(ilat,i)
     dlon=data(ilon,i)
     dtime=data(itime,i)

     if (obstype == 'sbuv2' ) then
        if (nobskeep>izero) then
           write(6,*)'setupoz: nobskeep',nobskeep
           call stop2(259)
        end if

        ierror_toq = nint(data(itoq,i))
        ierror_poq = nint(data(ipoq,i))

!       Note:  ozp as log(pobs)
        call intrp2a(ges_prsi(1,1,1,ntguessig),prsitmp,dlat,&
          dlon,ione,nsig+ione,mype)
  
!       Map observation pressure to guess vertical coordinate
        psi=one/(prsitmp(1)*r10)  ! factor of 10 converts to hPa
        do nz=1,nlevs-ione
           if ((pobs(nz)*psi) < one) then
              ozp(nz) = pobs(nz)/r10
           else
              ozp(nz) = prsitmp(1)
           end if
           call grdcrd(ozp(nz),ione,prsitmp,nsig+ione,-ione)
        enddo
     end if

     call intrp3oz(ges_oz,ozges,dlat,dlon,ozp,dtime,&
          ione,nlevs,mype)

     if(ozone_diagsave)then
        ii=ii+ione
        idiagbuf(1,ii)=mype                  ! mpi task number
        diagbuf(1,ii) = data(ilate,i)        ! lat (degree)
        diagbuf(2,ii) = data(ilone,i)        ! lon (degree)
        diagbuf(3,ii) = data(itime,i)-time_offset ! time (hours relative to analysis)
     endif

!    Interpolate interface pressure to obs location
!    Calculate innovations, perform gross checks, and accumualte
!    numbers for statistics

!    For OMI/GOME, nlev=1 
     do k=1,nlev
        j=ipos(k)
        ioff=nreal+k

!       Compute innovation and load obs error into local array
        ozobs = data(ioff,i)
        ozone_inv(k) = ozobs-ozges(k)
        error(k)     = tnoise(k)

!       Set inverse obs error squared and ratio_errors
        if (error(k)<1.e4_r_kind) then
           varinv3(k) = one/(error(k)**2)
           ratio_errors(k) = one
        else
           varinv3(k) = zero
           ratio_errors(k) = zero
        endif

!       Perform gross check
        if(abs(ozone_inv(k)) > gross(k) .or. ozobs > 1000._r_kind .or. &
             ozges(k)<tiny_r_kind) then
           varinv3(k)=zero
           ratio_errors(k)=zero
!          write(6,*)'SETUPOZ:  reset O3 varinv3=',varinv3(k)
           if(luse(i))stats_oz(2,j) = stats_oz(2,j) + one ! number of obs tossed
        endif

!       Accumulate numbers for statistics
        rat_err2 = ratio_errors(k)**2
        if (varinv3(k)>tiny_r_kind .or. &
             (iouse(k)==-ione .and. ozone_diagsave)) then
           if(luse(i))then
              omg=ozone_inv(k)
              stats_oz(1,j) = stats_oz(1,j) + one                          ! # obs
              stats_oz(3,j) = stats_oz(3,j) + omg                          ! (o-g)
              stats_oz(4,j) = stats_oz(4,j) + omg*omg                      ! (o-g)**2
              stats_oz(5,j) = stats_oz(5,j) + omg*omg*varinv3(k)*rat_err2  ! penalty
              stats_oz(6,j) = stats_oz(6,j) + ozobs                        ! obs
 
              exp_arg = -half*varinv3(k)*omg**2
              errorinv = sqrt(varinv3(k))
              if (pg_oz(j) > tiny_r_kind .and. errorinv > tiny_r_kind) then
                 arg  = exp(exp_arg)
                 wnotgross= one-pg_oz(j)
                 cg_oz=b_oz(j)*errorinv
                 wgross = cg_term*pg_oz(j)/(cg_oz*wnotgross)
                 term = log((arg+wgross)/(one+wgross))
                 wgt  = one-wgross/(arg+wgross)
              else
                 term = exp_arg
                 wgt  = one
              endif
              stats_oz(8,j) = stats_oz(8,j) -two*rat_err2*term
              if(wgt < wgtlim) stats_oz(9,j)=stats_oz(9,j)+one
           end if
        endif

!       If not assimilating this observation, reset inverse variance to zero
        if (iouse(k)<ione) then
           varinv3(k)=zero
           ratio_errors(k)=zero
           rat_err2 = zero
        end if
        if (rat_err2*varinv3(k)>tiny_r_kind .and. luse(i)) &
           stats_oz(7,j) = stats_oz(7,j) + one

!       Optionally save data for diagnostics
        if (ozone_diagsave) then
           rdiagbuf(1,k,ii) = ozobs
           rdiagbuf(2,k,ii) = ozone_inv(k)           ! obs-ges
           rdiagbuf(3,k,ii) = varinv3(k)*rat_err2    ! inverse (obs error )**2
        endif

     end do
!    Check all information for obs.  If there is at least one piece of
!    information that passed quality control, use this observation.
     ikeep=izero
     do k=1,nlevs
        if ((ratio_errors(k)**2)*varinv3(k)>1.e-10_r_kind) ikeep=ione
     end do

!    In principle, we want ALL obs in the diagnostics structure but for
!    passive obs (monitoring), it is difficult to do if rad_diagsave
!    is not on in the first outer loop. For now we use l_may_be_passive...
     if (l_may_be_passive) then
!       Link observation to appropriate observation bin
        if (nobs_bins>ione) then
           ibin = NINT( dtime/hr_obsbin ) + ione
        else
           ibin = ione
        endif
        IF (ibin<ione.OR.ibin>nobs_bins) write(6,*)mype,'Error nobs_bins,ibin= ',nobs_bins,ibin

!       Process obs have at least one piece of information that passed qc checks
        if (.not. last .and. ikeep==ione) then

           if(.not. associated(ozhead(ibin)%head))then
              allocate(ozhead(ibin)%head,stat=istat)
              if(istat /= izero)write(6,*)' failure to write ozhead '
              oztail(ibin)%head => ozhead(ibin)%head
           else
              allocate(oztail(ibin)%head%llpoint,stat=istat)
              if(istat /= izero)write(6,*)' failure to write oztail%llpoint '
              oztail(ibin)%head => oztail(ibin)%head%llpoint
           end if
           nlevp=max(nlev-ione,ione)
           allocate(oztail(ibin)%head%res(nlev),oztail(ibin)%head%diags(nlev),&
                    oztail(ibin)%head%err2(nlev),oztail(ibin)%head%raterr2(nlev),&
                    oztail(ibin)%head%prs(nlevp), &
                    oztail(ibin)%head%wij(4,nsig), &
                    oztail(ibin)%head%ipos(nlev),stat=istatus)
           if (istatus/=izero) write(6,*)'SETUPOZ:  allocate error for oz_point, istatus=',istatus

!          Set number of levels for this obs
           oztail(ibin)%head%nloz = nlev-ione  ! NOTE: for OMI/GOME, nloz=0

!          Set (i,j) indices of guess gridpoint that bound obs location
           call get_ij(mm1,dlat,dlon,oztail(ibin)%head%ij(1),tempwij(1))

           call tintrp2a(ges_prsi,prsitmp,dlat,dlon,dtime,hrdifsig,&
                ione,nsig+ione,mype,nfldsig)

           do k = 1,nsig
              oztail(ibin)%head%wij(1,k)=tempwij(1)*rozcon*(prsitmp(k)-prsitmp(k+1))
              oztail(ibin)%head%wij(2,k)=tempwij(2)*rozcon*(prsitmp(k)-prsitmp(k+1))
              oztail(ibin)%head%wij(3,k)=tempwij(3)*rozcon*(prsitmp(k)-prsitmp(k+1))
              oztail(ibin)%head%wij(4,k)=tempwij(4)*rozcon*(prsitmp(k)-prsitmp(k+1))
           end do

!          Increment data counter and save information used in
!          inner loop minimization (int* and stp* routines)

           oztail(ibin)%head%luse=luse(i)
           oztail(ibin)%head%time=dtime

           if (obstype == 'sbuv2' ) then
              do k=1,nlevs-ione
                 oztail(ibin)%head%prs(k) = ozp(k)
              enddo
           else
              oztail(ibin)%head%prs(1) = zero   ! any value is OK, never used
           endif

        endif ! < .not.last >

!       Link obs to diagnostics structure
        do k=1,nlevs
           if (.not.lobsdiag_allocated) then
              if (.not.associated(obsdiags(i_oz_ob_type,ibin)%head)) then
                 allocate(obsdiags(i_oz_ob_type,ibin)%head,stat=istat)
                 if (istat/=izero) then
                    write(6,*)'setupoz: failure to allocate obsdiags',istat
                    call stop2(260)
                 end if
                 obsdiags(i_oz_ob_type,ibin)%tail => obsdiags(i_oz_ob_type,ibin)%head
              else
                 allocate(obsdiags(i_oz_ob_type,ibin)%tail%next,stat=istat)
                 if (istat/=izero) then
                    write(6,*)'setupoz: failure to allocate obsdiags',istat
                    call stop2(261)
                 end if
                 obsdiags(i_oz_ob_type,ibin)%tail => obsdiags(i_oz_ob_type,ibin)%tail%next
              end if
              allocate(obsdiags(i_oz_ob_type,ibin)%tail%muse(miter+ione))
              allocate(obsdiags(i_oz_ob_type,ibin)%tail%nldepart(miter+ione))
              allocate(obsdiags(i_oz_ob_type,ibin)%tail%tldepart(miter))
              allocate(obsdiags(i_oz_ob_type,ibin)%tail%obssen(miter))
              obsdiags(i_oz_ob_type,ibin)%tail%indxglb=i
              obsdiags(i_oz_ob_type,ibin)%tail%nchnperobs=-99999_i_kind
              obsdiags(i_oz_ob_type,ibin)%tail%luse=.false.
              obsdiags(i_oz_ob_type,ibin)%tail%muse(:)=.false.
              obsdiags(i_oz_ob_type,ibin)%tail%nldepart(:)=-huge(zero)
              obsdiags(i_oz_ob_type,ibin)%tail%tldepart(:)=zero
              obsdiags(i_oz_ob_type,ibin)%tail%wgtjo=-huge(zero)
              obsdiags(i_oz_ob_type,ibin)%tail%obssen(:)=zero
           else
              if (.not.associated(obsdiags(i_oz_ob_type,ibin)%tail)) then
                 obsdiags(i_oz_ob_type,ibin)%tail => obsdiags(i_oz_ob_type,ibin)%head
              else
                 obsdiags(i_oz_ob_type,ibin)%tail => obsdiags(i_oz_ob_type,ibin)%tail%next
              end if
              if (obsdiags(i_oz_ob_type,ibin)%tail%indxglb/=i) then
                 write(6,*)'setupoz: index error'
                 call stop2(262)
              end if
           endif
           obsdiags(i_oz_ob_type,ibin)%tail%luse=luse(i)
           obsdiags(i_oz_ob_type,ibin)%tail%muse(jiter)= (ikeep==ione)
           obsdiags(i_oz_ob_type,ibin)%tail%nldepart(jiter)=ozone_inv(k)
           obsdiags(i_oz_ob_type,ibin)%tail%wgtjo= varinv3(k)*ratio_errors(k)**2
 
           if (.not. last .and. ikeep==ione) then
              oztail(ibin)%head%ipos(k)    = ipos(k)
              oztail(ibin)%head%res(k)     = ozone_inv(k)
              oztail(ibin)%head%err2(k)    = varinv3(k)
              oztail(ibin)%head%raterr2(k) = ratio_errors(k)**2
              oztail(ibin)%head%diags(k)%ptr => obsdiags(i_oz_ob_type,ibin)%tail
           endif

           if (ozone_diagsave.and.lobsdiagsave) then
              idia=3_i_kind
              do jj=1,miter
                 idia=idia+ione
                 if (obsdiags(i_oz_ob_type,ibin)%tail%muse(jj)) then
                    rdiagbuf(idia,k,ii) = one
                 else
                    rdiagbuf(idia,k,ii) = -one
                 endif
              enddo
              do jj=1,miter+ione
                 idia=idia+ione
                 rdiagbuf(idia,k,ii) = obsdiags(i_oz_ob_type,ibin)%tail%nldepart(jj)
              enddo
              do jj=1,miter
                 idia=idia+ione
                 rdiagbuf(idia,k,ii) = obsdiags(i_oz_ob_type,ibin)%tail%tldepart(jj)
              enddo
              do jj=1,miter
                 idia=idia+ione
                 rdiagbuf(idia,k,ii) = obsdiags(i_oz_ob_type,ibin)%tail%obssen(jj)
              enddo
           endif

        enddo ! < over nlevs >

     else

        if (ozone_diagsave.and.lobsdiagsave) then
           rdiagbuf(4:irdim1,1:nlevs,ii) = zero
        endif
 
     endif ! < l_may_be_passive >

  end do   ! end do i=1,nobs

! If requested, write to diagnostic file
  if (ozone_diagsave) then
     filex=obstype
     write(string,100) jiter
100  format('_',i2.2)
     diag_ozone_file = trim(dirname) // trim(filex) // '_' // trim(dplat(is)) // (string)
     open(4,file=trim(diag_ozone_file),form='unformatted')     
     rewind(4)
     iextra=izero
     if (mype==mype_diaghdr(is)) then
        write(4) isis,dplat(is),obstype,jiter,nlevs,ianldate,iint,ireal,iextra
        write(6,*)'SETUPOZ:   write header record for ',&
             isis,iint,ireal,iextra,' to file ',trim(diag_ozone_file),' ',ianldate
        do i=1,nlevs
           pob4(i)=pobs(i)
           grs4(i)=gross(i)
           err4(i)=tnoise(i)
        end do
        write(4) pob4,grs4,err4,iouse
     endif
     write(4) ii
     write(4) idiagbuf(:,1:ii),diagbuf(:,1:ii),rdiagbuf(:,:,1:ii)
     close(4)
  endif

! Jump to this line if problem with data
135 continue        

! clean up
  if(ozone_diagsave) deallocate(rdiagbuf)
  return
end subroutine setupoz
