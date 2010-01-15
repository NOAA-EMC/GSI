subroutine setupsst(lunin,mype,bwork,awork,nele,nobs,conv_diagsave)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    setupsst    compute rhs for conventional surface sst
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
!   2004-07-20  derber
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2004-08-28  derber  - fix some bugs
!   2004-10-06  parrish - increase size of sstwork array for nonlinear qc
!   2004-11-22  derber - remove weight, add logical for boundary point
!   2004-12-22  treadon - move logical conv_diagsave from obsmod to argument list
!   2005-03-02  dee - remove garbage from diagnostic file
!   2005-03-09  parrish - nonlinear qc change to account for inflated obs error
!   2005-07-27  derber  - add print of monitoring and reject data
!   2005-09-28  derber  - combine with prep,spr,remove tran and clean up
!   2005-10-14  derber  - input grid location and fix regional lat/lon
!   2005-11-08  todling - bug fix: lat/lon arrays were inverted to diag file
!   2005-11-14  pondeca - correct error in diagnostic array index
!   2006-01-31  todling/treadon - store wgt/wgtlim in rdiagbuf(6,ii)
!   2006-02-24  derber  - modify to take advantage of convinfo module
!   2006-05-30  su,derber,treadon - modify diagnostic output
!   2006-06-06  su - move to wgtlim to constants module
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!                       - modify handling of multiple data at same location
!                       - unify NL qc
!   2006-08-28      su - fix a bug in variational qc
!   2007-03-19  tremolet - binning of observations
!   2007-06-05  tremolet - add observation diagnostics structure
!   2007-08-28      su - modify the gross check error 
!   2008-05-21  safford - rm unused vars and uses
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
  use kinds, only: r_kind,r_single,r_double,i_kind

  use guess_grids, only: dsfct
  use obsmod, only: ssthead,ssttail,rmiss_single,i_sst_ob_type,obsdiags,&
                    lobsdiagsave,nobskeep,lobsdiag_allocated,time_offset
  use gsi_4dvar, only: nobs_bins,hr_obsbin
  use oneobmod, only: magoberr,maginnov,oneobtest
  use gridmod, only: nlat,nlon,istart,jstart,lon1,nsig
  use gridmod, only: get_ij
  use constants, only: izero,ione,zero,tiny_r_kind,one,half,wgtlim, &
            two,cg_term,pi,huge_single,r1000
  use jfunc, only: jiter,last,miter
  use qcmod, only: dfact,dfact1,npres_print
  use convinfo, only: nconvtype,cermin,cermax,cgross,cvar_b,cvar_pg,ictype
  use convinfo, only: icsubtype
  implicit none

! Declare passed variables
  logical                                          ,intent(in   ) :: conv_diagsave
  integer(i_kind)                                  ,intent(in   ) :: lunin,mype,nele,nobs
  real(r_kind),dimension(100_i_kind+7*nsig)        ,intent(inout) :: awork
  real(r_kind),dimension(npres_print,nconvtype,5,3),intent(inout) :: bwork

! Declare local variables
  
  real(r_double) rstation_id

  real(r_kind) sstges,dlat,dlon,ddiff,dtime,error
  real(r_kind) scale,val2,ratio,ressw2,ress,residual
  real(r_kind) obserrlm,obserror,val,valqc
  real(r_kind) term,halfpi,rwgt
  real(r_kind) cg_sst,wgross,wnotgross,wgt,arg,exp_arg,rat_err2
  real(r_kind) ratio_errors,tfact
  real(r_kind) errinv_input,errinv_adjst,errinv_final
  real(r_kind) err_input,err_adjst,err_final
  real(r_kind),dimension(nobs):: dup
  real(r_kind),dimension(nele,nobs):: data
  real(r_single),allocatable,dimension(:,:)::rdiagbuf


  integer(i_kind) ier,ilon,ilat,isst,id,itime,ikx,imaxerr,iqc
  integer(i_kind) ier2,iuse,idepth,iotype,ilate,ilone,istnelv
  integer(i_kind) i,nchar,nreal,k,ii,ikxx,nn,isli,ibin,ioff,jj
  integer(i_kind) l,ix,iy,ix1,iy1,ixp,iyp,mm1
  integer(i_kind) istat
  integer(i_kind) idomsfc,iskint,iff10,isfcr
  
  logical,dimension(nobs):: luse,muse

  character(8) station_id
  character(8),allocatable,dimension(:):: cdiagbuf

  equivalence(rstation_id,station_id)
  

!*********************************************************************************
! Read and reformat observations in work arrays.
  read(lunin)data,luse
!  index information for data array (see reading routine)
  ier=ione           ! index of obs error
  ilon=2_i_kind      ! index of grid relative obs location (x)
  ilat=3_i_kind      ! index of grid relative obs location (y)
  isst=4_i_kind      ! index of sst observation - background
  id=5_i_kind        ! index of station id
  itime=6_i_kind     ! index of observation time in data array
  ikxx=7_i_kind      ! index of ob type
  imaxerr=8_i_kind   ! index of sst max error
  idepth=9_i_kind    ! index of flag indicating depth of observation
  iotype=10_i_kind   ! index of measurement type
  iqc=11_i_kind      ! index of qulaity mark
  ier2=12_i_kind     ! index of original-original obs error ratio
  iuse=13_i_kind     ! index of use parameter
  idomsfc=14_i_kind  ! index of dominant surface type
  iskint=15_i_kind   ! index of surface skin temperature
  iff10=16_i_kind    ! index of 10 meter wind factor
  isfcr=17_i_kind    ! index of surface roughness
  ilone=18_i_kind    ! index of longitude (degrees)
  ilate=19_i_kind    ! index of latitude (degrees)
  istnelv=20_i_kind  ! index of station elevation (m)


  do i=1,nobs
     muse(i)=nint(data(iuse,i)) <= jiter
  end do

  dup=one
  do k=1,nobs
     do l=k+ione,nobs
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
     ii=izero
     nchar=ione
     nreal=20_i_kind
     if (lobsdiagsave) nreal=nreal+4*miter+ione
     allocate(cdiagbuf(nobs),rdiagbuf(nreal,nobs))
  end if

  halfpi = half*pi
  mm1=mype+ione
  scale=one

  do i=1,nobs

     dlat=data(ilat,i)
     dlon=data(ilon,i)
     dtime=data(itime,i)
     ikx  = nint(data(ikxx,i))
     error=data(ier2,i)
     isli=data(idomsfc,i)

!    Link observation to appropriate observation bin
     if (nobs_bins>ione) then
        ibin = NINT( dtime/hr_obsbin ) + ione
     else
        ibin = ione
     endif
     IF (ibin<ione.OR.ibin>nobs_bins) write(6,*)mype,'Error nobs_bins,ibin= ',nobs_bins,ibin

!    Link obs to diagnostics structure
     if (.not.lobsdiag_allocated) then
        if (.not.associated(obsdiags(i_sst_ob_type,ibin)%head)) then
           allocate(obsdiags(i_sst_ob_type,ibin)%head,stat=istat)
           if (istat/=izero) then
              write(6,*)'setupsst: failure to allocate obsdiags',istat
              call stop2(295)
           end if
           obsdiags(i_sst_ob_type,ibin)%tail => obsdiags(i_sst_ob_type,ibin)%head
        else
           allocate(obsdiags(i_sst_ob_type,ibin)%tail%next,stat=istat)
           if (istat/=izero) then
              write(6,*)'setupsst: failure to allocate obsdiags',istat
              call stop2(295)
           end if
           obsdiags(i_sst_ob_type,ibin)%tail => obsdiags(i_sst_ob_type,ibin)%tail%next
        end if
        allocate(obsdiags(i_sst_ob_type,ibin)%tail%muse(miter+ione))
        allocate(obsdiags(i_sst_ob_type,ibin)%tail%nldepart(miter+ione))
        allocate(obsdiags(i_sst_ob_type,ibin)%tail%tldepart(miter))
        allocate(obsdiags(i_sst_ob_type,ibin)%tail%obssen(miter))
        obsdiags(i_sst_ob_type,ibin)%tail%indxglb=i
        obsdiags(i_sst_ob_type,ibin)%tail%nchnperobs=-99999_i_kind
        obsdiags(i_sst_ob_type,ibin)%tail%luse=.false.
        obsdiags(i_sst_ob_type,ibin)%tail%muse(:)=.false.
        obsdiags(i_sst_ob_type,ibin)%tail%nldepart(:)=-huge(zero)
        obsdiags(i_sst_ob_type,ibin)%tail%tldepart(:)=zero
        obsdiags(i_sst_ob_type,ibin)%tail%wgtjo=-huge(zero)
        obsdiags(i_sst_ob_type,ibin)%tail%obssen(:)=zero
     else
        if (.not.associated(obsdiags(i_sst_ob_type,ibin)%tail)) then
           obsdiags(i_sst_ob_type,ibin)%tail => obsdiags(i_sst_ob_type,ibin)%head
        else
           obsdiags(i_sst_ob_type,ibin)%tail => obsdiags(i_sst_ob_type,ibin)%tail%next
        end if
        if (obsdiags(i_sst_ob_type,ibin)%tail%indxglb/=i) then
           write(6,*)'setupsst: index error'
           call stop2(297)
        end if
     endif

! Interpolate to get sst at obs location/time
     call intrp2a(dsfct,sstges,dlat,dlon,ione,ione,mype)

     sstges = sstges+data(iskint,i)
! Adjust observation error
     ratio_errors=error/data(ier,i)
     error=one/error

!    Check to ensure point surrounded by water
     ix1=dlat; iy1=dlon
     ix1=max(ione,min(ix1,nlat))
     ix=ix1-istart(mm1)+2_i_kind; iy=iy1-jstart(mm1)+2_i_kind
     if(iy<ione) then
        iy1=iy1+nlon
        iy=iy1-jstart(mm1)+2_i_kind
     end if
     if(iy>lon1+ione) then
        iy1=iy1-nlon
        iy=iy1-jstart(mm1)+2_i_kind
     end if
     ixp=ix+ione; iyp=iy+ione
     if(ix1==nlat) then
        ixp=ix
     end if
 
     if(isli > izero ) error = zero


     ddiff=data(isst,i)-sstges

! If requested, setup for single obs test.
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
     else
        ratio_errors=ratio_errors/sqrt(dup(i))
     end if

     if (ratio_errors*error <=tiny_r_kind) muse(i)=.false.
     if (nobskeep>izero) muse(i)=obsdiags(i_sst_ob_type,ibin)%tail%muse(nobskeep)

!    Compute penalty terms (linear & nonlinear qc).
     val      = error*ddiff
     if(luse(i))then
        val2     = val*val
        exp_arg  = -half*val2
        rat_err2 = ratio_errors**2
        if (cvar_pg(ikx) > tiny_r_kind .and. error > tiny_r_kind) then
           arg  = exp(exp_arg)
           wnotgross= one-cvar_pg(ikx)
           cg_sst=cvar_b(ikx)
           wgross = cg_term*cvar_pg(ikx)/(cg_sst*wnotgross)
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
        nn=ione
        if (.not. muse(i)) then
           nn=2_i_kind
           if(ratio_errors*error >=tiny_r_kind)nn=3_i_kind
        end if
        bwork(1,ikx,1,nn)  = bwork(1,ikx,1,nn)+one           ! count
        bwork(1,ikx,2,nn)  = bwork(1,ikx,2,nn)+ress          ! (o-g)
        bwork(1,ikx,3,nn)  = bwork(1,ikx,3,nn)+ressw2        ! (o-g)**2
        bwork(1,ikx,4,nn)  = bwork(1,ikx,4,nn)+val2*rat_err2 ! penalty
        bwork(1,ikx,5,nn)  = bwork(1,ikx,5,nn)+valqc         ! nonlin qc penalty

     endif

     obsdiags(i_sst_ob_type,ibin)%tail%luse=luse(i)
     obsdiags(i_sst_ob_type,ibin)%tail%muse(jiter)=muse(i)
     obsdiags(i_sst_ob_type,ibin)%tail%nldepart(jiter)=ddiff
     obsdiags(i_sst_ob_type,ibin)%tail%wgtjo= (error*ratio_errors)**2

!    If obs is "acceptable", load array with obs info for use
!    in inner loop minimization (int* and stp* routines)
     if (.not. last .and. muse(i)) then

        if(.not. associated(ssthead(ibin)%head))then
           allocate(ssthead(ibin)%head,stat=istat)
           if(istat /= izero)write(6,*)' failure to write ssthead '
           ssttail(ibin)%head => ssthead(ibin)%head
        else
           allocate(ssttail(ibin)%head%llpoint,stat=istat)
           if(istat /= izero)write(6,*)' failure to write ssttail%llpoint '
           ssttail(ibin)%head => ssttail(ibin)%head%llpoint
        end if

!       Set (i,j) indices of guess gridpoint that bound obs location
        call get_ij(mm1,dlat,dlon,ssttail(ibin)%head%ij(1),ssttail(ibin)%head%wij(1))

        ssttail(ibin)%head%res     = ddiff
        ssttail(ibin)%head%err2    = error**2
        ssttail(ibin)%head%raterr2 = ratio_errors**2    
        ssttail(ibin)%head%time    = dtime
        ssttail(ibin)%head%b       = cvar_b(ikx)
        ssttail(ibin)%head%pg      = cvar_pg(ikx)
        ssttail(ibin)%head%luse    = luse(i)
        ssttail(ibin)%head%diags => obsdiags(i_sst_ob_type,ibin)%tail
 
     endif


!    Save stuff for diagnostic output
     if(conv_diagsave .and. luse(i))then
        ii=ii+ione
        rstation_id     = data(id,i)
        cdiagbuf(ii)    = station_id         ! station id
 
        rdiagbuf(1,ii)  = ictype(ikx)        ! observation type
        rdiagbuf(2,ii)  = icsubtype(ikx)     ! observation subtype
 
        rdiagbuf(3,ii)  = data(ilate,i)      ! observation latitude (degrees)
        rdiagbuf(4,ii)  = data(ilone,i)      ! observation longitude (degrees)
        rdiagbuf(5,ii)  = data(istnelv,i)    ! station elevation (meters)
        rdiagbuf(6,ii)  = rmiss_single       ! observation pressure (hPa)
        rdiagbuf(7,ii)  = data(idepth,i)     ! observation height (meters)
        rdiagbuf(8,ii)  = dtime-time_offset  ! obs time (hours relative to analysis time)

        rdiagbuf(9,ii)  = data(iqc,i)        ! input prepbufr qc or event mark
        rdiagbuf(10,ii) = rmiss_single       ! setup qc or event mark
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
        if (err_input>tiny_r_kind) errinv_input = one/err_input
        if (err_adjst>tiny_r_kind) errinv_adjst = one/err_adjst
        if (err_final>tiny_r_kind) errinv_final = one/err_final

        rdiagbuf(13,ii) = rwgt               ! nonlinear qc relative weight
        rdiagbuf(14,ii) = errinv_input       ! prepbufr inverse obs error (K**-1)
        rdiagbuf(15,ii) = errinv_adjst       ! read_prepbufr inverse obs error (K**-1)
        rdiagbuf(16,ii) = errinv_final       ! final inverse observation error (K**-1)
 
        rdiagbuf(17,ii) = data(isst,i)       ! SST observation (K)
        rdiagbuf(18,ii) = ddiff              ! obs-ges used in analysis (K)
        rdiagbuf(19,ii) = data(isst,i)-sstges! obs-ges w/o bias correction (K) (future slot)
 
        rdiagbuf(20,ii) = data(iotype,i)     ! type of measurement

        if (lobsdiagsave) then
           ioff=20_i_kind
           do jj=1,miter 
              ioff=ioff+ione 
              if (obsdiags(i_sst_ob_type,ibin)%tail%muse(jj)) then
                 rdiagbuf(ioff,ii) = one
              else
                 rdiagbuf(ioff,ii) = -one
              endif
           enddo
           do jj=1,miter+ione
              ioff=ioff+ione
              rdiagbuf(ioff,ii) = obsdiags(i_sst_ob_type,ibin)%tail%nldepart(jj)
           enddo
           do jj=1,miter
              ioff=ioff+ione
              rdiagbuf(ioff,ii) = obsdiags(i_sst_ob_type,ibin)%tail%tldepart(jj)
           enddo
           do jj=1,miter
              ioff=ioff+ione
              rdiagbuf(ioff,ii) = obsdiags(i_sst_ob_type,ibin)%tail%obssen(jj)
           enddo
        endif
 
     end if


  end do


! Write information to diagnostic file
  if(conv_diagsave)then
     write(7)'sst',nchar,nreal,ii,mype
     write(7)cdiagbuf(1:ii),rdiagbuf(:,1:ii)
     deallocate(cdiagbuf,rdiagbuf)
  end if

! End of routine
end subroutine setupsst

