!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  setupoz --- Compute rhs of oi for sbuv2 ozone obs
!
! !INTERFACE:
!

subroutine setupoz(lunin,mype,stats_oz,nlevs,nreal,nobs,&
     obstype,isis,is,ozone_diagsave,perturb)
     
! !USES:

  use kinds, only: r_kind,r_single,i_kind

  use constants, only : zero,izero,one_tenth,half,one,two,tiny_r_kind,rad2deg
  use constants, only : rozcon,cg_term,wgtlim

  use obsmod, only:  ozhead,oztail,ozohead,ozotail,nloz,dplat
  use obsmod, only : iadate,mype_diaghdr,dirname

  use gridmod, only : lat2,lon2,get_ij,nsig

  use guess_grids, only : nfldsig,ges_prsi,ntguessig,ges_oz

  use ozinfo, only : jpch_oz,error_oz,pob_oz,gross_oz,nusis_oz
  use ozinfo, only : iuse_oz,b_oz,pg_oz

  use jfunc, only : jiter,last
  
  implicit none
  
! !INPUT PARAMETERS:

  integer(i_kind), intent(in) :: lunin  ! unit from which to read observations
  integer(i_kind), intent(in) :: mype   ! mpi task id
  integer(i_kind), intent(in) :: nlevs  ! number of levels (layer amounts + total column) per obs   
  integer(i_kind), intent(in) :: nreal  ! number of pieces of non-ozone info (location, time, etc) per obs
  integer(i_kind), intent(in) :: nobs   ! number of observations
  character(20), intent(in) :: isis     ! sensor/instrument/satellite id
  integer(i_kind), intent(in) :: is     ! integer(i_kind) counter for number of obs types to process

  character(10), intent(in)        :: obstype          ! type of ozone obs
  logical, intent(in) :: ozone_diagsave                ! switch on diagnostic output (.false.=no output)
  real(r_kind),dimension(nobs),intent(in):: perturb    ! observation perturbation factor

! !INPUT/OUTPUT PARAMETERS:

  real(r_kind),dimension(9,jpch_oz),intent(inout):: stats_oz ! sums for various statistics as 
                                                             ! a function of level
!
! !DESCRIPTION:  For sbuv2 ozone observations (layer amounts and total 
!            column, this routine 
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
!   2007-06-08  kleist/treadon - add prefix (task id or path) to diag_ozone_file
!
! !REMARKS:
!   language: f90
!   machine:  ibm RS/6000 SP; SGI Origin 2000; Compaq HP
!
! !AUTHOR: parrish          org: np22                date: 1990-10-06
!
!EOP
!-------------------------------------------------------------------------

! Declare local parameters  
  integer(i_kind),parameter:: iint=1
  integer(i_kind),parameter:: ireal=3
  real(r_kind),parameter:: r10=10.0_r_kind
  real(r_kind),parameter:: r300=300.0_r_kind

! Declare local variables  
  
  real(r_kind) ozobs,omg,rat_err2,dlat,dtime,dlon
  real(r_kind) cg_oz,wgross,wnotgross,wgt,arg,exp_arg,term
  real(r_kind) psi,error
  real(r_kind),dimension(nlevs):: ozges,varinv3,ozone_inv
  real(r_kind),dimension(nlevs):: ratio_errors
  real(r_kind),dimension(nlevs-1):: ozp
  real(r_kind),dimension(nlevs):: pobs,gross,tnoise
  real(r_kind),dimension(nreal+nlevs,nobs):: data
! real(r_kind),dimension(lat2,lon2,nsig,nfldsig)::grid2
  real(r_kind),dimension(lat2,lon2,nsig,nfldsig)::grid1
  real(r_kind),dimension(nsig+1)::prsitmp
  real(r_single),dimension(nlevs):: pob4,grs4,err4
  real(r_single),dimension(ireal,nobs):: diagbuf
  real(r_single),dimension(3,nlevs,nobs)::rdiagbuf

  integer(i_kind) i,nlev,ii,iextra,istat
  integer(i_kind) k,j,nz,idate,jc,istatus
  integer(i_kind) n,ioff,itoss,ikeep,nkeep
  integer(i_kind) jj,mm1,itime,ilat,ilon,isd,ilate,ilone
  integer(i_kind),dimension(iint,nobs):: idiagbuf
  integer(i_kind),dimension(nlevs):: ipos,iouse
  
  character(12) string
  character(10) filex
  character(128) diag_ozone_file

  logical,dimension(nobs):: luse

  mm1=mype+1


!
!*********************************************************************************
! Initialize arrays
  do j=1,nlevs
     ipos(j)=0
     iouse(j)=-2
     tnoise(j)=1.e10
     gross(j)=1.e10
     pobs(j)=1.e10
  end do

! Locate data for satellite in ozinfo arrays
  itoss =1
  jc=0
  do j=1,jpch_oz
    if (isis == nusis_oz(j)) then
       jc=jc+1
       if (jc > nlevs) then
          write(6,*)'SETUPOZ:  ***ERROR*** in level numbers, jc,nlevs=',jc,nlevs,&
               ' ***STOP IN SETUPOZ***'
          call stop2(71)
       endif
       ipos(jc)=j

       iouse(jc)=iuse_oz(j)
       tnoise(jc)=error_oz(j)
       gross(jc)=min(r10*gross_oz(j),r300)
       pobs(jc)=pob_oz(j) * 1.01324_r_kind

       if (iouse(jc)<-1 .or. (iouse(jc)==-1 .and. &
            .not.ozone_diagsave)) then
          tnoise(jc)=1.e10
          gross(jc) =1.e10
          pobs(jc)  = zero
       endif
       if (tnoise(jc)<1.e4) itoss=izero
    endif
  end do
  nlev=jc

! Handle error conditions
  if (nlevs>nlev) write(6,*)'SETUPOZ:  level number reduced for ',obstype,' ', &
       nlevs,' --> ',nlev
  if (nlev == 0) then
     if (mype==0) write(6,*)'SETUPOZ:  no levels found for ',isis
     if (nobs>0) read(lunin) 
     goto 135
  endif
  if (itoss==1) then
     if (mype==0) write(6,*)'SETUPOZ:  all obs variances > 1.e4.  Do not use ',&
          'data from satellite ',isis
     if (nobs>0) read(lunin)
     goto 135
  endif

! Initialize variables used in ozone processing
  nkeep=0
  do i=1,nobs
     ikeep=0
     do k=1,nlev
        if (iouse(k)>0 .or. ozone_diagsave) ikeep=1
     end do
     nkeep=nkeep+ikeep
  end do

! Read and transform ozone data
  read(lunin) data,luse

! If none of the data will be assimilated and don't need diagnostics,
! return to calling program
  if (nkeep==0) return

!    index information for data array (see reading routine)

  isd=1        ! index of satellite
  itime=2     ! index of analysis relative obs time
  ilon=3      ! index of grid relative obs location (x)
  ilat=4      ! index of grid relative obs location (y)
  ilone=5     ! index of earth relative longitude (degrees)
  ilate=6     ! index of earth relative latitude (degrees)

! Interpolate guess ozone and pressure to observation location
  do jj=1,nfldsig
     do k = 1,nsig
        do j = 1,lon2
           do i = 1,lat2
              grid1(i,j,k,jj)=ges_oz(i,j,k,jj)*rozcon* &
                   (ges_prsi(i,j,k,jj)-ges_prsi(i,j,k+1,jj))
!             grid2(i,j,k,jj)=rozcon*(ges_prsi(i,j,k,jj)-ges_prsi(i,j,k+1,jj))
           end do
        end do
     end do
  end do

! If requested, save data for diagnostic ouput
 if(ozone_diagsave)ii=izero

 if (obstype == 'sbuv2' ) then

  if (nloz+1 /= nlevs ) then
     if (mype == 0) write(6,*)'SETUPOZ: nloz+1 /= nlevs ',nloz+1,nlevs,nlev
     goto 135
  endif

! Convert observation (lat,lon) from earth to grid relative values
  do i=1,nobs

     dlat=data(ilat,i)
     dlon=data(ilon,i)
     dtime=data(itime,i)

! Note:  ozp as log(pobs)
     call intrp2a(ges_prsi(1,1,1,ntguessig),prsitmp,dlat,&
       dlon,1,nsig+1,mype)

  
! Map observation pressure to guess vertical coordinate
     psi=one/(prsitmp(1)*r10)  ! factor of 10 converts to hPa
     do nz=1,nlevs-1
        if ((pobs(nz)*psi) < one) then
           ozp(nz) = pobs(nz)/r10
        else
           ozp(nz) = prsitmp(1)
        end if
        call grdcrd(ozp(nz),1,prsitmp,nsig+1,-1)
     enddo
  
     call intrp3oz(grid1,ozges,dlat,dlon,ozp,dtime,&
          1,nlevs,mype,obstype)

     if(ozone_diagsave)then
        ii=ii+1
        idiagbuf(1,ii)=mype                  ! mpi task number
        diagbuf(1,ii) = data(ilate,i)        ! lat (degree)
        diagbuf(2,ii) = data(ilone,i)        ! lon (degree)
        diagbuf(3,ii) = data(itime,i)        ! time (hours relative to analysis)
     endif

! Interpolate interface pressure to obs location
! Calculate innovations, perform gross checks, and accumualte
! numbers for statistics
     do k=1,nlev
        j=ipos(k)
        ioff=nreal+k
        ozobs = data(ioff,i)

!       Perturb observation.  Note:  if perturb_obs=.false., the
!       array perturb=0.0 and observation is unchanged.
        ozobs = ozobs + perturb(i)*tnoise(k)

        ozone_inv(k) = ozobs-ozges(k)
        varinv3(k) = zero
        ratio_errors(k) = zero
        if (tnoise(k)<1.e4) then
          varinv3(k) = one/(tnoise(k)**2)
     !    ratio_errors(k) = tnoise(k)/tnoise(k)     !  obs error not altered yet
          ratio_errors(k) = one
        end if

!       Perform gross check
        if(abs(ozone_inv(k)) > gross(k) .or. ozobs > 1000. .or. &
             ozges(k)<tiny_r_kind) then
           varinv3(k)=zero
           ratio_errors(k)=zero
           write(6,*)'SETUPOZ:  reset O3 varinv3=',varinv3(k)
!           write(6,*)'SETUPOZ:  reset sbuv O3 varinv3=',ozobs,ozges(k,i),gross(k),k
           if(luse(i))stats_oz(2,j) = stats_oz(2,j) + one ! number of obs tossed
        endif

!       Accumulate numbers for statistics
        rat_err2 = ratio_errors(k)**2
        if (varinv3(k)>tiny_r_kind .or. &
             (iouse(k)==-1 .and. ozone_diagsave)) then
          if(luse(i))then
           omg=ozone_inv(k)
           stats_oz(1,j) = stats_oz(1,j) + one                          ! # obs
           stats_oz(3,j) = stats_oz(3,j) + omg                          ! (o-g)
           stats_oz(4,j) = stats_oz(4,j) + omg*omg                      ! (o-g)**2
           stats_oz(5,j) = stats_oz(5,j) + omg*omg*varinv3(k)*rat_err2  ! penalty
           stats_oz(6,j) = stats_oz(6,j) + ozobs                        ! obs

           exp_arg = -half*varinv3(k)*omg**2
           error = sqrt(varinv3(k))
           if (pg_oz(j) > tiny_r_kind .and. error > tiny_r_kind) then
              arg  = exp(exp_arg)
              wnotgross= one-pg_oz(j)
              cg_oz=b_oz(j)*error
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
        if (iouse(k)<1) then
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
     ikeep=0
     do k=1,nlevs
        if ((ratio_errors(k)**2)*varinv3(k)>1.e-10) ikeep=1
     end do

!    Process obs have at least one piece of information that passed qc checks
     if (.not. last .and. ikeep==1) then

        if(.not. associated(ozhead))then
            allocate(ozhead,stat=istat)
            if(istat /= 0)write(6,*)' failure to write ozhead '
            oztail => ozhead
        else
            allocate(oztail%llpoint,stat=istat)
            oztail => oztail%llpoint
            if(istat /= 0)write(6,*)' failure to write oztail%llpoint '
        end if
        allocate(oztail%res(nloz+1),oztail%err2(nloz+1), &
                 oztail%raterr2(nloz+1),oztail%prs(nloz), &
                 oztail%ipos(nloz+1),stat=istatus)
        if (istatus/=0) write(6,*)'SETUPOZ:  allocate error for oz_point, istatus=',istatus


!       Set (i,j) indices of guess gridpoint that bound obs location
        call get_ij(mm1,dlat,dlon,oztail%ij(1),oztail%wij(1))

!       Increment data counter and save information used in
!       inner loop minimization (int* and stp* routines)

        oztail%luse=luse(i)
        oztail%time=dtime

        do k=1,nlevs-1
           oztail%prs(k) = ozp(k)
        enddo
        do k=1,nlevs
           oztail%ipos(k)    = ipos(k)
           oztail%res(k)     = ozone_inv(k)
           oztail%err2(k)    = varinv3(k)
           oztail%raterr2(k) = ratio_errors(k)**2
        enddo

     endif


  end do


 else if ( obstype == 'omi' ) then

  if (nlevs /= 1 .or. nlev /= 1) then
     if (mype == 0) write(6,*)'SETUPOZ: nlevs,nlev /=1 for omi',nlevs,nlev
     goto 135
  endif

! Convert observation (lat,lon) from earth to grid relative values
  do i=1,nobs

     dlat=data(ilat,i)
     dlon=data(ilon,i)
     dtime=data(itime,i)

     call intrp3oz(grid1,ozges,dlat,dlon,ozp,dtime,&
          1,nlevs,mype,obstype)

     if(ozone_diagsave)then
        ii=ii+1
        idiagbuf(1,ii)=mype                  ! mpi task number
        diagbuf(1,ii) = data(ilate,i)        ! lat (degree)
        diagbuf(2,ii) = data(ilone,i)        ! lon (degree)
        diagbuf(3,ii) = data(itime,i)        ! time (hours relative to analysis)
     endif

! Calculate innovations, perform gross checks, and accumualte
! numbers for statistics

!    only one level for omi

     j=ipos(1)
     ioff=nreal+1
     ozobs = data(ioff,i)
     ozone_inv(1) = data(ioff,i)-ozges(1)
     varinv3(1) = zero
     ratio_errors(1) = zero
     if (tnoise(1)<1.e4) then
       varinv3(1) = one/(tnoise(1)**2)
  !    ratio_errors(1) = tnoise(1)/tnoise(1)     !  obs error not altered yet
       ratio_errors(1) = one
     end if

!    Perform gross check
     if(abs(ozone_inv(1)) > gross(1) .or. ozobs > 1000. .or. &
          ozges(1)<tiny_r_kind) then
        varinv3(1)=zero
        ratio_errors(1)=zero
        write(6,*)'SETUPOZ:  reset omi O3 varinv3=',ozobs,ozges(1),gross(1)
        if(luse(i))stats_oz(2,j) = stats_oz(2,j) + one ! number of obs tossed
     endif

!    Accumulate numbers for statistics
     rat_err2 = ratio_errors(1)**2
     if (varinv3(1)>tiny_r_kind .or. (iouse(1)==-1 .and. ozone_diagsave)) then
       if(luse(i))then
        omg=ozone_inv(1)
        stats_oz(1,j) = stats_oz(1,j) + one                             ! # obs
        stats_oz(3,j) = stats_oz(3,j) + omg                             ! (o-g)
        stats_oz(4,j) = stats_oz(4,j) + omg*omg                         ! (o-g)**2
        stats_oz(5,j) = stats_oz(5,j) + omg*omg*varinv3(1)*rat_err2     ! penalty
        stats_oz(6,j) = stats_oz(6,j) + ozobs                           ! obs

        exp_arg = -half*varinv3(1)*omg**2
        error = sqrt(varinv3(1))
        if (pg_oz(j) > tiny_r_kind .and. error > tiny_r_kind) then
           arg  = exp(exp_arg)
           wnotgross= one-pg_oz(j)
           cg_oz=b_oz(j)*error
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

!    If not assimilating this observation, reset inverse variance to zero
     if (iouse(1)<1) then
         varinv3(1)=zero
         ratio_errors(1)=zero
         rat_err2 = zero
     end if
     if (rat_err2*varinv3(1)>tiny_r_kind .and. luse(i)) &
          stats_oz(7,j) = stats_oz(7,j) + one

!    Optionally save data for diagnostics
     if (ozone_diagsave) then
        rdiagbuf(1,1,ii) = ozobs
        rdiagbuf(2,1,ii) = ozone_inv(1)           ! obs-ges
        rdiagbuf(3,1,ii) = varinv3(1)*rat_err2    ! inverse (obs error )**2
     endif

!    Check all information for obs.  If there is at least one piece of
!    information that passed quality control, use this observation.
     ikeep=0
     if ((ratio_errors(1)**2)*varinv3(1)>1.e-10) ikeep=1

!    Process obs have at least one piece of information that passed qc checks
     if ( .not. last .and. ikeep==1) then

        if(.not. associated(ozohead))then
            allocate(ozohead,stat=istat)
            if(istat /= 0)write(6,*)' failure to write ozohead '
            ozotail => ozohead
        else
            allocate(ozotail%llpoint,stat=istat)
            ozotail => ozotail%llpoint
            if(istat /= 0)write(6,*)' failure to write ozotail%llpoint '
        end if

!       Set (i,j) indices of guess gridpoint that bound obs location
        call get_ij(mm1,dlat,dlon,ozotail%ij(1),ozotail%wij(1))

!       Increment data counter and save information used in
!       inner loop minimization (int* and stp* routines)


        ozotail%res    = ozone_inv(1)
        ozotail%err2   = varinv3(1)
        ozotail%raterr2= ratio_errors(1)**2
        ozotail%time   = dtime
        ozotail%ipos   = ipos(1)
        ozotail%luse   = luse(i)

     endif
  end do

 endif   ! endif obstype

! If requested, write to diagnostic file
  if (ozone_diagsave) then
     filex=obstype
     write(string,100) jiter
100  format('_',i2.2)
     diag_ozone_file = trim(dirname) // trim(filex) // '_' // trim(dplat(is)) // (string)
     idate=iadate(4)+iadate(3)*100+iadate(2)*10000+iadate(1)*1000000
     iextra=0
     if (mype==mype_diaghdr(is)) then
        write(6,*)'SETUPOZ:   write header record for ',&
             isis,iint,ireal,iextra,' to file ',trim(diag_ozone_file),' ',iadate
     endif
     do i=1,nlevs
        pob4(i)=pobs(i)
        grs4(i)=gross(i)
        err4(i)=tnoise(i)
     end do
     open(4,file=diag_ozone_file,form='unformatted')
     rewind(4)
     write(4) isis,dplat(is),obstype,jiter,nlevs,ii,idate,iint,ireal,iextra
     write(4) pob4,grs4,err4,iouse
     write(4) idiagbuf(:,1:ii),diagbuf(:,1:ii),rdiagbuf(:,:,1:ii)
     close(4)
  endif

! Jump to this line if problem with data
135 continue        
  return
end subroutine setupoz
