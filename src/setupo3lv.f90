!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  setupo3lv --- Compute rhs of oi for ozone level obs
!
! !INTERFACE:

subroutine setupo3lv(lunin,mype,bwork,owork,nele,nobs,isis,is,&
              obstype,ozone_diagsave)

! !USES

  use kinds, only: r_kind,r_single,i_kind

  use obsmod, only: o3ltail,o3lhead,iadate,dplat,i_o3l_ob_type,obsdiags,&
                    lobsdiagsave,nobskeep,lobsdiag_allocated,dirname,&
                    ianldate,time_offset,mype_diaghdr
  use oneobmod, only: oneobtest,maginnov,magoberr,pctswitch
  use guess_grids, only: ges_lnprsl,ges_oz,hrdifsig,nfldsig,ges_ps,&
       ges_prsi, ntguessig
  use gridmod, only: nlat,nlon,lat2,lon2,nsig
  use gridmod, only: get_ijk
  use gsi_4dvar, only: nobs_bins,hr_obsbin
  use constants, only: zero,one,four,r1000,wgtlim
  use constants, only: tiny_r_kind,half,one_tenth,two,cg_term
  use constants, only: constoz,ozcon
  use qcmod, only: npres_print,ptopo3,pboto3,dfact,dfact1
  use jfunc, only: jiter,last, miter
  use convinfo, only: nconvtype,cermin,cermax,cgross,cvar_b,cvar_pg,ictype

  implicit none


! !INPUT PARAMETERS:

  integer(i_kind), intent(in) :: lunin  ! unit from which to read observations
  integer(i_kind), intent(in) :: mype   ! mpi task id
  integer(i_kind), intent(in) :: nele   ! no. of data elements per observation
  integer(i_kind), intent(in) :: nobs   ! no. of observations
  character(20), intent(in) :: isis     ! sensor/instrument/satellite id
  integer(i_kind), intent(in) :: is     ! integer(i_kind) counter for number of obs types to process
  character(10),   intent(in) :: obstype          ! type of ozone obs
  logical, intent(in) :: ozone_diagsave ! switch on diagnostic output (.false.=no output)

! !INPUT/OUTPUT PARAMETERS:
  
  real(r_kind),dimension(100+7*nsig),intent(inout):: owork   ! data counts and gross chk
  real(r_kind),dimension(npres_print,nconvtype,5,3),intent(inout):: bwork ! obs-ges statistics
  
! !DESCRIPTION:
!      For ozone level observations, this routine
!  \begin{enumerate}
!       \item reads obs assigned to given mpi task (geographic region),
!       \item simulates obs from guess,
!       \item applies some quality control to obs,
!       \item loads weight and innovation arrays used in minimization
!       \item collects statistics for runtime diagnostic output
!       \item writes additional diagnostic information to output file
!
! !REVISION HISTORY:
!
!  2006-09-12  Sienkiewicz    New routine based on setupoz and setupq, setupt
!  2006-10-24  Sienkiewicz    Modified pressure levels for print
!  2006-12-28  Sienkiewicz    'dobsget' for Dobson unit conversion
!  2007-01-10  Sienkieiwcz    Modify to use new inner loop obs data structure
!                           - modify handling of multiple data at same location
!                           - use ges_ps instead of ln(ps)
!  2007-04-12  Sienkiewicz    fix bug in weight calculation
!  2007-05-02  Todling      - updated to account for obsevation binning
!  2007-06-05  tremolet     - add observation diagnostics structure
!  2007-12-15  todling      - add prefix for diag filenames
!  2008-12-06  todling      - replace prefix with dirname
!  2008-12-30  todling      - remove unused vars
!  2009-01-20  Sienkiewicz  - adjust for new analysis (g/g not Dobson units)
!  2009-04-28  Sienkiewicz  - adjust 'one-ob' test, add pctswitch option
!
! !REMARKS:
!   language: f90
!   machine:  ?
!
!EOP
!-------------------------------------------------------------------------


! Declare local parameters
  real(r_kind),parameter:: r10=10.0_r_kind
  real(r_kind),parameter:: r0_001 = 0.001_r_kind

! Declare local variables    

  real(r_kind) o3ges, o3ppmv
  real(r_kind) ratio_errors,dlat,dlon,dtime,dpres,error,rwgt
  real(r_kind) rsig,rlow,rhgh,preso3l,tfact
  real(r_kind) psges,sfcchk,ddiff
  real(r_kind) cg_o3l,wgross,wnotgross,wgt,arg,exp_arg,term,rat_err2
  real(r_kind) ratio,val2,obserror
  real(r_kind) obserrlm,residual,scale
  real(r_kind) ress, ressw2
  real(r_kind) val,valqc
  real(r_kind),dimension(nele,nobs):: data
  real(r_kind),dimension(nobs):: dup
  real(r_kind),dimension(nsig):: prsltmp
  real(r_single),allocatable,dimension(:,:)::rdiagbuf
  real(r_kind),dimension(lat2,lon2,nsig,nfldsig)::grid1
  real(r_kind),dimension(8) ::  dobsconv

  integer(i_kind) i,nreal,j,ii,l,jj,mm1,nn
  integer(i_kind) jsig,k
  integer(i_kind) ier,ilon,ilat,ipres,io3ob,id,itime,ikx, &
       iuse,ilate,ilone,istat,iqual,iprec,isat,ikxx,ibin,ioff

  logical,dimension(nobs):: luse,muse

  character(10) filex
  character(128)diag_o3lev_file
  character(12) string


!*******************************************************************************
! Read and reformat observations in work arrays.
  read(lunin)data,luse

  isat=1      ! index of satellite
  itime=2     ! index of observation time in data array
  ilon=3      ! index of grid relative obs location (x)
  ilat=4      ! index of grid relative obs location (y)
  ilone=5     ! index of longitude (degrees)
  ilate=6     ! index of latitude (degrees)
  io3ob=7     ! index of o3 level observation (gm/gm)
  ipres=8     ! index of pressure
  id= 9       ! index of sounding/retrieval id
  ikxx=10     ! index of ob type
  iuse=11     ! index of use parameter
  ier=12      ! index of obs error
  istat = 13  ! index of status flag
  iqual = 14  ! index of quality flag
  iprec = 15  ! index of precision value


! Set flag for observation use, based on current outer iteration
! number (jiter).  Generally data(iuse,i)=100. for passive obs; 
! typically we do not have more than a few outer loop iterations.
  do i=1,nobs
     muse(i)=nint(data(iuse,i)) <= jiter
  end do

! Duplicated obs are downweighted; calculate factor for
! adjustment
  dup=one
  do k=1,nobs
     do l=k+1,nobs
        if(data(ilat,k) == data(ilat,l) .and.  &
             data(ilon,k) == data(ilon,l) .and.  &
             data(ipres,k) == data(ipres,l) .and. &
             data(ier,k) < r1000 .and. data(ier,l) < r1000 .and. &
             muse(k) .and. muse(l))then
           tfact = min(one,abs(data(itime,k)-data(itime,l))/dfact1)
           dup(k)=dup(k)+one-tfact*tfact*(one-dfact)
           dup(l)=dup(l)+one-tfact*tfact*(one-dfact)
        end if
     end do
  end do


! If requested, save select data for output to diagnostic file
  if(ozone_diagsave)then
     ii=0
     nreal=15
     if (lobsdiagsave) nreal=nreal+4*miter+1
     allocate(rdiagbuf(nreal,nobs))
  end if

  rsig=float(nsig)

  mm1=mype+1

  scale=one

! Prepare ozone level data
  do i=1,nobs

! Convert obs lats and lons to grid coordinates
     dlat=data(ilat,i)
     dlon=data(ilon,i)
     dpres=data(ipres,i)
     dtime=data(itime,i)
     error=data(ier,i)
     ikx=nint(data(ikxx,i))

!    Link observation to appropriate observation bin
     if (nobs_bins>1) then
       ibin = NINT( dtime/hr_obsbin ) + 1
     else
       ibin = 1
     endif
     IF (ibin<1.OR.ibin>nobs_bins) write(6,*)mype,'Error nobs_bins,ibin= ',nobs_bins,ibin

!    Link obs to diagnostics structure
     if (.not.lobsdiag_allocated) then
       if (.not.associated(obsdiags(i_o3l_ob_type,ibin)%head)) then
         allocate(obsdiags(i_o3l_ob_type,ibin)%head,stat=istat)
         if (istat/=0) then
            write(6,*)'setupo3l: failure to allocate obsdiags',istat
            call stop2(256)
         end if
         obsdiags(i_o3l_ob_type,ibin)%tail => obsdiags(i_o3l_ob_type,ibin)%head
       else
         allocate(obsdiags(i_o3l_ob_type,ibin)%tail%next,stat=istat)
         if (istat/=0) then
            write(6,*)'setupo3l: failure to allocate obsdiags',istat
            call stop2(257)
         end if
         obsdiags(i_o3l_ob_type,ibin)%tail => obsdiags(i_o3l_ob_type,ibin)%tail%next
       end if
       allocate(obsdiags(i_o3l_ob_type,ibin)%tail%muse(miter+1))
       allocate(obsdiags(i_o3l_ob_type,ibin)%tail%nldepart(miter+1))
       allocate(obsdiags(i_o3l_ob_type,ibin)%tail%tldepart(miter))
       allocate(obsdiags(i_o3l_ob_type,ibin)%tail%obssen(miter))
       obsdiags(i_o3l_ob_type,ibin)%tail%indxglb=i
       obsdiags(i_o3l_ob_type,ibin)%tail%nchnperobs=-99999
       obsdiags(i_o3l_ob_type,ibin)%tail%luse=.false.
       obsdiags(i_o3l_ob_type,ibin)%tail%muse(:)=.false.
       obsdiags(i_o3l_ob_type,ibin)%tail%nldepart(:)=-huge(zero)
       obsdiags(i_o3l_ob_type,ibin)%tail%tldepart(:)=zero
       obsdiags(i_o3l_ob_type,ibin)%tail%wgtjo=-huge(zero)
       obsdiags(i_o3l_ob_type,ibin)%tail%obssen(:)=zero
     else
       if (.not.associated(obsdiags(i_o3l_ob_type,ibin)%tail)) then
         obsdiags(i_o3l_ob_type,ibin)%tail => obsdiags(i_o3l_ob_type,ibin)%head
       else
         obsdiags(i_o3l_ob_type,ibin)%tail => obsdiags(i_o3l_ob_type,ibin)%tail%next
       end if
       if (obsdiags(i_o3l_ob_type,ibin)%tail%indxglb/=i) then
         write(6,*)'setupo3l: index error'
         call stop2(258)
       end if
     endif

! Interpolate ps & log(pres) at mid-layers to obs locations/times
     call tintrp2a(ges_ps,psges,dlat,dlon,dtime,hrdifsig,&
          1,1,mype,nfldsig)
     call tintrp2a(ges_lnprsl,prsltmp,dlat,dlon,dtime,hrdifsig,&
          1,nsig,mype,nfldsig)

     preso3l =r10*exp(dpres)


! Pressure level of data (dpres) converted to grid coordinate
! (wrt mid-layer pressure)
     call grdcrd(dpres,1,prsltmp,nsig,-1)

! Get approximate k value of surface by using surface pressure
! for surface check.
     sfcchk=log(psges)
     call grdcrd(sfcchk,1,prsltmp,nsig,-1)

! Check if observation above model top or below model surface

     rlow=max(sfcchk-dpres,zero)
     rhgh=max(dpres-r0_001-rsig,zero)

     if(luse(i))then
        owork(1) = owork(1) + one
        if(rlow/=zero) owork(2) = owork(2) + one
        if(rhgh/=zero) owork(3) = owork(3) + one
     end if


! calculate factor for error adjustment if too (high,low) or duplicate
     ratio_errors=error/((error+1.0e6*rhgh+four*rlow)*sqrt(dup(i)))

! Check to see if observations is above the top of the model
     if (dpres > rsig) ratio_errors=zero

! set 'error' as 1./(obs error)
     error = one/error

! Interpolate guess ozone to observation location and time
     call tintrp3(ges_oz,o3ges,dlat,dlon,dpres,dtime, &
          hrdifsig,1,mype,nfldsig)

! Compute innovations - background o3ges in g/g so adjust units
! Leave increment in ppmv for gross checks,  etc.

     o3ppmv = o3ges * constoz
     ddiff=  data(io3ob,i) - o3ppmv

! If requested, setup for single obs test.   (code not tested)
     if (oneobtest) then
        ddiff = maginnov
        error=one/magoberr
        ratio_errors=one
        if (pctswitch) then
           ddiff = maginnov * o3ppmv
           error = one/(magoberr*o3ppmv)
        endif
     end if

! Gross error checks

     obserror=one/max(ratio_errors*error,tiny_r_kind)
     obserrlm=max(cermin(ikx),min(cermax(ikx),obserror))
     residual=abs(ddiff)
     ratio=residual/obserrlm
     if(ratio > cgross(ikx) .or. ratio_errors < tiny_r_kind) then
        if(luse(i))owork(4)=owork(4)+one
        error=zero
        ratio_errors=zero
     end if

! check if gross check failed, mark failed obs for non-use
     if (ratio_errors*error <=tiny_r_kind) muse(i)=.false.
     if (nobskeep>0) muse(i)=obsdiags(i_o3l_ob_type,ibin)%tail%muse(nobskeep)

! Compute penalty terms   (note: nonlin QC not tested)
     val = error*ddiff
     if(luse(i))then
        val2     = val*val
        exp_arg  = -half*val2
        rat_err2 = ratio_errors**2
        if (cvar_pg(ikx) > tiny_r_kind .and. error >tiny_r_kind) then
           arg  = exp(exp_arg)
           wnotgross= one-cvar_pg(ikx)
           cg_o3l=cvar_b(ikx)
           wgross = cg_term*cvar_pg(ikx)/(cg_o3l*wnotgross)
           term =log((arg+wgross)/(one+wgross))
           wgt  = one-wgross/(arg+wgross)
           rwgt = wgt/wgtlim
        else
           term = exp_arg
           wgt  = wgtlim
           rwgt = wgt/wgtlim
        endif
        valqc = -two*rat_err2*term

        jsig = dpres
        jsig=max(1,min(jsig,nsig))

! Accumulate statistics for obs belonging to this task
        if(muse(i))then
           if(rwgt < one) owork(21) = owork(21)+one
           owork(jsig+3*nsig+100)=owork(jsig+3*nsig+100)+valqc
           owork(jsig+5*nsig+100)=owork(jsig+5*nsig+100)+one
           owork(jsig+6*nsig+100)=owork(jsig+6*nsig+100)+val2*rat_err2
        end if

! Loop over pressure level groupings and obs to accumulate statistics
! as a function of observation type.
        ress  = scale*ddiff
        ressw2= ress*ress
        nn = 1
        if (.not. muse(i)) then
           nn = 2
           if(ratio_errors*error >=tiny_r_kind)nn=3
        end if

        do k = 1,npres_print
           if(preso3l >= ptopo3(k) .and. preso3l <= pboto3(k))then

              bwork(k,ikx,1,nn)  = bwork(k,ikx,1,nn)+one           ! count
              bwork(k,ikx,2,nn)  = bwork(k,ikx,2,nn)+ress          ! (o-g)
              bwork(k,ikx,3,nn)  = bwork(k,ikx,3,nn)+ressw2        ! (o-g)**2
              bwork(k,ikx,4,nn)  = bwork(k,ikx,4,nn)+val2*rat_err2 ! penalty
              bwork(k,ikx,5,nn)  = bwork(k,ikx,5,nn)+valqc         ! nonlin qc penalty
           end if
        end do
     end if

     obsdiags(i_o3l_ob_type,ibin)%tail%luse=luse(i)
     obsdiags(i_o3l_ob_type,ibin)%tail%muse(jiter)=muse(i)
     obsdiags(i_o3l_ob_type,ibin)%tail%nldepart(jiter)=ddiff
     obsdiags(i_o3l_ob_type,ibin)%tail%wgtjo= (error*ratio_errors)**2

! If obs is "acceptable", load array with obs info for use
! in inner loop minimization (int* and stp* routines)
     if (.not. last .and. muse(i)) then

        if (.not. associated(o3lhead(ibin)%head)) then
           allocate(o3lhead(ibin)%head,stat=istat)
           if(istat /=0)write(6,*)'failure to write o3lhead'
           o3ltail(ibin)%head => o3lhead(ibin)%head
        else
           allocate(o3ltail(ibin)%head%llpoint,stat=istat)
           if(istat /= 0)write(6,*)' failure to write o3ltail%llpoint '
           o3ltail(ibin)%head => o3ltail(ibin)%head%llpoint
        end if

! Set (i,j,k) indices of guess gridpoint that bound obs location
        call get_ijk(mm1,dlat,dlon,dpres,&
             o3ltail(ibin)%head%ij(1),o3ltail(ibin)%head%wij(1))

!
! o3ltail%wij -> forward model normalized by obs error
!          (1/obserror) * (interpolation weight) * (units conversion)

        do j=1,8
           o3ltail(ibin)%head%wij(j)=o3ltail(ibin)%head%wij(j)*constoz
        end do

        o3ltail(ibin)%head%res     = ddiff
        o3ltail(ibin)%head%err2    = error**2
        o3ltail(ibin)%head%raterr2 = ratio_errors**2
        o3ltail(ibin)%head%time    = dtime
        o3ltail(ibin)%head%b       = cvar_b(ikx)
        o3ltail(ibin)%head%pg      = cvar_pg(ikx)
        o3ltail(ibin)%head%luse    = luse(i)
        o3ltail(ibin)%head%diags => obsdiags(i_o3l_ob_type,ibin)%tail

     endif

! Save select output for diagnostic file   (still need reorganize for new)
     if(ozone_diagsave .and. luse(i))then
        ii=ii+1
        rdiagbuf(1,ii)= ictype(ikx)              ! type
        rdiagbuf(2,ii)= data(ilate,i)            ! lat
        rdiagbuf(3,ii)= data(ilone,i)            ! lon
        rdiagbuf(4,ii)= preso3l                  ! pressure (hPa)
        rdiagbuf(5,ii)= dtime-time_offset        ! time
        rdiagbuf(6,ii)= rwgt                     ! relative weight
        rdiagbuf(7,ii)= error                    ! 1/obserror
        rdiagbuf(8,ii)= data(io3ob,i)            ! ozone ob 
        rdiagbuf(9,ii)= o3ges * constoz          ! simulated ozone
        rdiagbuf(10,ii)= ratio_errors            ! (original error)/(inflated error)
        rdiagbuf(11,ii)= data(id,i)              ! sounding index
        rdiagbuf(12,ii)= data(istat,i)           ! status flag
        rdiagbuf(13,ii)= data(iqual,i)           ! quality flag
        rdiagbuf(14,ii)= data(iprec,i)           ! precision value
        if(muse(i)) then
           rdiagbuf(15,ii) = one                 ! analysis usage flag
        else
           rdiagbuf(15,ii) = -one
        endif

        if (lobsdiagsave) then
          ioff=24
          do jj=1,miter
            ioff=ioff+1
            if (obsdiags(i_o3l_ob_type,ibin)%tail%muse(jj)) then
              rdiagbuf(ioff,ii) = one
            else
              rdiagbuf(ioff,ii) = -one
            endif
          enddo
          do jj=1,miter+1
            ioff=ioff+1
            rdiagbuf(ioff,ii) = obsdiags(i_o3l_ob_type,ibin)%tail%nldepart(jj)
          enddo
          do jj=1,miter
            ioff=ioff+1
            rdiagbuf(ioff,ii) = obsdiags(i_o3l_ob_type,ibin)%tail%tldepart(jj)
          enddo
          do jj=1,miter
            ioff=ioff+1
            rdiagbuf(ioff,ii) = obsdiags(i_o3l_ob_type,ibin)%tail%obssen(jj)
          enddo
        endif

     end if
  end do

! Write information to diagnostic file
  if(ozone_diagsave)then
     filex = obstype
     write(string,'(''_'',i2.2,''.'',i4.4)') jiter,mype
     diag_o3lev_file = trim(dirname) // trim(filex) // '_' // trim(dplat(is)) // string
     open(4,file=trim(diag_o3lev_file),form='unformatted')
     rewind(4)
     if (mype==mype_diaghdr(is)) then
        write(4) isis, dplat(is),obstype,jiter,ianldate,nreal
        write(6,*)'SETUPO3LV:   write header record for ',&
             isis,nreal,' to file ',trim(diag_o3lev_file),' ',ianldate
     endif
     write(4) ii
     write(4) rdiagbuf(:,1:ii)
     deallocate(rdiagbuf)
     close(4)
  end if

! End of routine
end subroutine setupo3lv

