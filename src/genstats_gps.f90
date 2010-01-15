subroutine genstats_gps(bwork,awork,toss_gps_sub,high_gps_sub,conv_diagsave,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    genstats_gps    generate statistics for gps observations
!   prgmmr: treadon          org: np20                date: 2005-12-21
!
! abstract:  For gps observations, this routine
!              a) collects statistics for runtime diagnostic output
!              f) adjusts observation error ratio based on superobs factor
!
! program history log:
!   2005-12-21  treadon
!   2006-02-24  derber  - modify to take advantage of convinfo module
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!   2006-09-20  cucurull - replace superobs factor for obs in a top (non-full) layer 
!   2007-03-01  treadon - add array toss_gps
!   2007-03-19  tremolet - binning of observations
!   2007-06-21 cucurull - add conv_diagsave and mype in argument list; 
!                         modify qc and output for diagnostic file based on toss_gps
!                         print out diagnostic files if requested
!                         add wgtlim and huge_single in constants module
!   2008-02-27 cucurull - modify diagnostics output file
!   2008-04-14 treadon  - compute super_gps within this routine
!   2008-06-04 safford  - rm unused vars and uses
!   2008-09-05 lueken   - merged ed's changes into q1fy09 code
!   2008-25-08 todling  - adapt obs-binning change to GSI-May2008
!   2009-02-05 cucurull - modify latitude range four statistics output
!   2009-10-22     shen - add high_gps
!
!   input argument list:
!     toss_gps_sub  - array of qc'd profile heights
!     high_gps_sub
!     conv_diagsave - logical to save innovation dignostics
!     mype          - mpi task id
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
  use kinds, only: r_kind,i_kind,r_single
  use obsmod, only: gps_allhead,gps_allptr,nprof_gps,&
       destroy_genstats_gps,gpsptr,obs_diag
  use gridmod, only: nsig,regional
  use constants, only: tiny_r_kind,half,izero,ione,wgtlim,one,two,zero,five
  use qcmod, only: npres_print,ptop,pbot
  use mpimod, only: ierror,mpi_comm_world,mpi_rtype,mpi_sum,mpi_max
  use jfunc, only: jiter,last
  use gsi_4dvar, only: nobs_bins
  use convinfo, only: nconvtype
  implicit none

! Declare passed variables
  logical                                          ,intent(in):: conv_diagsave
  integer(i_kind)                                  ,intent(in) :: mype
  real(r_kind),dimension(100_i_kind+7*nsig)        ,intent(inout):: awork
  real(r_kind),dimension(npres_print,nconvtype,5,3),intent(inout):: bwork
  real(r_kind),dimension(max(ione,nprof_gps))      ,intent(in):: toss_gps_sub
  real(r_kind),dimension(max(ione,nprof_gps))      ,intent(in):: high_gps_sub

! Declare local parameters

! Declare local variables
  logical:: luse,muse
  integer(i_kind):: k,jsig,icnt,khgt,kprof,ikx,nn,j,nchar,nreal,ii,iii
  real(r_kind):: pressure,arg,wgross,wgt,term,cg_gps,valqc,ressw2
  real(r_kind):: ress,val,ratio_errors,val2
  real(r_kind):: exp_arg,data_ikx,data_rinc,cg_term,rat_err2,elat
  real(r_kind):: wnotgross,data_ipg,data_ier,data_ib,factor,super_gps_up,rhgt
  real(r_kind),dimension(nsig,max(ione,nprof_gps),nobs_bins):: super_gps_sub,super_gps
  real(r_kind),dimension(max(ione,nprof_gps)):: toss_gps
  real(r_kind),dimension(max(ione,nprof_gps)):: high_gps
  real(r_kind),allocatable,dimension(:,:)::rdiag
  real(r_single),allocatable,dimension(:,:)::sdiag
  character(8),allocatable,dimension(:):: cdiag
  
  real(r_kind),parameter:: r20 = 20.0_r_kind
  real(r_kind),parameter:: scale = 100.0_r_kind
  type(obs_diag), pointer :: obsptr => NULL()
  

!*******************************************************************************
! Check to see if there are any profiles to process.  If none, return.
  if (nprof_gps==izero) then
     if (mype==izero) write(6,*)'GENSTATS_GPS:  no profiles to process (nprof_gfs=',nprof_gps,'), EXIT routine'
     return
  endif

! Reduce sub-domain specific QC'd profile height cutoff values to
! maximum global value for each profile
  toss_gps=zero
  call mpi_allreduce(toss_gps_sub,toss_gps,nprof_gps,mpi_rtype,mpi_max,&
       mpi_comm_world,ierror)

! Reduce sub-domain and get the height of the highest observation in a profile.
! Only effective in regional assimilation.
  high_gps=zero
  if (regional) then
     call mpi_allreduce(high_gps_sub,high_gps,nprof_gps,mpi_rtype,mpi_max,&
          mpi_comm_world,ierror)
  endif

! Compute superobs factor on sub-domains using global QC'd profile height
  super_gps_sub=zero
  DO ii=1,nobs_bins
     gps_allptr => gps_allhead(ii)%head
     do while (associated(gps_allptr))

!       Load local work variables
        ratio_errors = gps_allptr%ratio_err
        data_ier     = gps_allptr%obserr
        luse         = gps_allptr%luse
        khgt         = gps_allptr%loc
        rhgt         = gps_allptr%loc
        kprof        = gps_allptr%kprof

!       Accumulate superobs factors
        if (rhgt >toss_gps(kprof)) then
           if(ratio_errors*data_ier>tiny_r_kind .and. luse) then
              k=min(max(ione,khgt),nsig)
              super_gps_sub(k,kprof,ii)=super_gps_sub(k,kprof,ii)+one
           endif
        endif

        gps_allptr => gps_allptr%llpoint
 
!    End loop over observations
     end do


!    Reduce sub-domain specifc superobs factors to global values for each profile
     super_gps(:,:,ii)=zero
     call mpi_allreduce(super_gps_sub(:,:,ii),super_gps(:,:,ii),nsig*nprof_gps,mpi_rtype,mpi_sum,&
          mpi_comm_world,ierror)

  END DO


! If generating diagnostic output, need to determine dimension of output arrays.
  if (conv_diagsave) then
     icnt = zero
     DO ii=1,nobs_bins
        gps_allptr => gps_allhead(ii)%head
        do while (associated(gps_allptr))
           icnt=icnt+ione
           gps_allptr => gps_allptr%llpoint
        end do
     END DO
     nreal =19_i_kind
     nchar = ione
     allocate(cdiag(icnt),rdiag(nreal,icnt),sdiag(nreal,icnt))
  endif



! Loop over data to apply final qc, superobs factors, accumulate
! statistics and (optionally) load diagnostic output arrays
  icnt=izero
  DO ii=1,nobs_bins
     gps_allptr => gps_allhead(ii)%head
     do while (associated(gps_allptr))

!       Load local work variables
        ratio_errors = gps_allptr%ratio_err
        data_ier     = gps_allptr%obserr
        luse         = gps_allptr%luse
        muse         = gps_allptr%muse
        val          = gps_allptr%dataerr
        data_ipg     = gps_allptr%pg
        data_ib      = gps_allptr%b
        khgt         = gps_allptr%loc
        kprof        = gps_allptr%kprof
        data_ikx     = gps_allptr%type
        pressure     = gps_allptr%rdiag(6)
        data_rinc    = gps_allptr%rdiag(5)
        elat         = gps_allptr%rdiag(3)
        ikx          = nint(data_ikx)
        gpsptr       => gps_allptr%mmpoint
        if(muse .and. associated(gpsptr))then
           obsptr       => gpsptr%diags
        endif


!       Transfer diagnostic information to output arrays
        if(conv_diagsave) then
           icnt=icnt+ione
           cdiag(icnt) = gps_allptr%cdiag
           do j=1,nreal
              rdiag(j,icnt)= gps_allptr%rdiag(j)
           enddo
        endif

!       Determine model level to which observation is mapped to
        k=min(max(ione,khgt),nsig)
 
!       Normalize ratio_errors by superobs factor.  Update ratio_error 
!       term used in minimization
        super_gps_up=zero
 
        if (super_gps(k,kprof,ii)>tiny_r_kind) then
           do j=min(k+ione,nsig),nsig
              super_gps_up = max(super_gps_up,super_gps(j,kprof,ii))
           enddo

           if (super_gps_up >tiny_r_kind) then
              factor = one / sqrt(super_gps(k,kprof,ii))
           else
              factor = one / sqrt(max(super_gps(k-1,kprof,ii),super_gps(k,kprof,ii)))
           endif
           ratio_errors = ratio_errors * factor
           if(conv_diagsave) then
              if(rdiag(16,icnt) >tiny_r_kind) rdiag(16,icnt)=ratio_errors*data_ier
           endif
 
!          Adjust error ratio for observations used in inner loop
           if (associated(gpsptr)) then
              gpsptr%raterr2 = ratio_errors **2
              if(associated(obsptr))then
                 obsptr%wgtjo=(ratio_errors*data_ier)**2
              end if
           endif
        endif


!       For given profile, check if observation level is below level at 
!       which profile data is tossed.   If so, set error parameter to 
!       zero (effectively tossing the obs).
 
        rhgt = gps_allptr%loc
        if (rhgt<=toss_gps(kprof)) then
           if(ratio_errors*data_ier > tiny_r_kind) then ! obs was good
              if(conv_diagsave) then
                 rdiag(10,icnt) = five
                 rdiag(12,icnt) = -one
              endif
              if (luse) then
                 if(elat > r20) then
                    awork(22) = awork(22)+one
                 else if(elat< -r20)then
                    awork(23) = awork(23)+one
                 else
                    awork(24) = awork(24)+one
                 end if
              endif
           endif
           ratio_errors = zero
           if (associated(gpsptr)) then
              gpsptr%raterr2 = ratio_errors **2
              if(associated(obsptr))then
                 obsptr%wgtjo=zero
                 obsptr%muse(jiter)=.false.
              end if
           endif
        endif


!       if the highest observation in a profile below 30 km is less than 5 km,
!       the observations in this profile would be tossed. It will set error
!       parameter to zero.    Only effective in regional assimilation.

        if (regional) then
           if (high_gps(kprof) < five) then
              ratio_errors = zero
              if (associated(gpsptr)) then
                 gpsptr%raterr2 = ratio_errors **2
              endif
           endif
        endif
        
!       Compute penalty terms
        if (ratio_errors*data_ier <= tiny_r_kind) muse = .false.
        if(luse)then
           val2     = val*val
           exp_arg  = -half*val2
           rat_err2 = ratio_errors**2
           if (data_ipg > tiny_r_kind) then
              cg_gps=cg_term/data_ib
              wnotgross= one-data_ipg
              wgross   = data_ipg*cg_gps
              arg      = exp(exp_arg)
              term     = log(wnotgross*arg+wgross)
              wgt      = wnotgross*arg/(wnotgross*arg+wgross)
           else
              term = exp_arg
              wgt  = one
           endif
           if(conv_diagsave) rdiag(13,icnt) = wgt/wgtlim
           valqc = -two*rat_err2*term
         

!          Accumulate statistics for obs belonging to this task
           if(muse)then
              if(wgt < wgtlim) awork(21) = awork(21)+one

!             Accumulate values for penalty and data count
              jsig=max(ione,khgt)
              awork(jsig+3*nsig+100_i_kind)=awork(jsig+3*nsig+100_i_kind)+valqc
              awork(jsig+5*nsig+100_i_kind)=awork(jsig+5*nsig+100_i_kind)+one
              awork(jsig+6*nsig+100_i_kind)=awork(jsig+6*nsig+100_i_kind)+val2*rat_err2
           endif


!          Loop over pressure level groupings and obs to accumulate
!          statistics as a function of observation type.
           do k = 1,npres_print
              if(pressure>=ptop(k) .and. pressure<=pbot(k))then
                 ress=data_rinc*scale
                 ressw2=ress*ress
                 nn=ione
                 if (.not. muse) then
                    nn=2_i_kind
                    if(ratio_errors*data_ier >=tiny_r_kind)nn=3_i_kind
                 end if

                 bwork(k,ikx,1,nn)  = bwork(k,ikx,1,nn)+one           ! count
                 bwork(k,ikx,2,nn)  = bwork(k,ikx,2,nn)+ress          ! (o-g)
                 bwork(k,ikx,3,nn)  = bwork(k,ikx,3,nn)+ressw2        ! (o-g)**2
                 bwork(k,ikx,4,nn)  = bwork(k,ikx,4,nn)+val2*rat_err2 ! penalty
                 bwork(k,ikx,5,nn)  = bwork(k,ikx,5,nn)+valqc         ! nonlin qc penalty

              end if
           end do
        end if
 
        gps_allptr => gps_allptr%llpoint

!    End loop over observations

     end do
  END DO

! If requested, write information to diagnostic file
  if(conv_diagsave)then
     icnt  = izero
     iii   = izero
     DO ii=1,nobs_bins
        gps_allptr => gps_allhead(ii)%head
        do while (associated(gps_allptr))
           icnt=icnt+ione
           luse = gps_allptr%luse
           if(luse)then
              iii=iii+ione
              cdiag(iii)=cdiag(icnt)
              do j=1,nreal
                 sdiag(j,iii)=rdiag(j,icnt)
              end do
           end if
           gps_allptr => gps_allptr%llpoint
        end do
     END DO
     write(7)'gps',nchar,nreal,iii,mype
     write(7)cdiag(1:iii),sdiag(:,1:iii)
     deallocate(cdiag,rdiag,sdiag)
  endif


! Destroy arrays holding gps data
  call destroy_genstats_gps

end subroutine genstats_gps
