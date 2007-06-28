subroutine genstats_gps(bwork,awork,super_gps,toss_gps)
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
!
!   input argument list:
!     super_gps   - array of superob factors
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
  use kinds, only: r_kind,i_kind
  use obsmod, only: gps_allhead,gps_allptr,nprof_gps,&
       destroy_genstats_gps,gpsptr

  use gridmod, only: nsig
  use constants, only: tiny_r_kind,half,izero,ione,one,two,zero
  use qcmod, only: npres_print,ptop,pbot
  use jfunc, only: last
  use convinfo, only: nconvtype,cermin,cermax,cgross,cvar_b,cvar_pg,ictype
  implicit none

! Declare local parameters

! Declare passed variables
  real(r_kind),dimension(100+7*nsig),intent(inout):: awork
  real(r_kind),dimension(npres_print,nconvtype,5,3),intent(inout):: bwork
  real(r_kind),dimension(nsig,max(1,nprof_gps)),intent(in):: super_gps
  real(r_kind),dimension(max(1,nprof_gps)),intent(in):: toss_gps

! Declare local variables
  logical:: luse,muse
  integer(i_kind):: k,itype,jsig,i,khgt,kprof,ikx,nn,j
  real(r_kind):: pressure,arg,wgross,wgt,term,cg_gps,valqc,ressw2
  real(r_kind):: scale,wgtlim,ress,val,ratio_errors,val2
  real(r_kind):: exp_arg,data_ikx,data_rinc,cg_term,rat_err2
  real(r_kind):: wnotgross,data_ipg,data_ier,data_ib,factor,super_gps_up,rhgt


!*******************************************************************************
! Initialize variables
  scale = one

! Loop over data to be processed
  gps_allptr => gps_allhead
  do while (associated(gps_allptr))


!    Load local work variables
     ratio_errors = gps_allptr%ratio_err
     data_ier     = gps_allptr%obserr
     luse         = gps_allptr%luse
     muse         = gps_allptr%muse
     val          = gps_allptr%dataerr
     data_ipg     = gps_allptr%pg
     data_ib      = gps_allptr%b
     khgt         = gps_allptr%loc
     pressure     = gps_allptr%pressure
     data_ikx     = gps_allptr%type
     data_rinc    = gps_allptr%rinc
     kprof        = gps_allptr%kprof
     ikx          = nint(data_ikx)
     gpsptr       => gps_allptr%mmpoint
     

!    Determine model level to which observation is mapped to
     k=min(max(1,khgt),nsig)

!    Normalize ratio_errors by superobs factor.  Update ratio_error 
!    term used in minimization
     super_gps_up=zero

     if (super_gps(k,kprof)>tiny_r_kind) then
        do j=min(k+1,nsig),nsig
          super_gps_up = max(super_gps_up,super_gps(j,kprof))
        enddo

        if (super_gps_up >tiny_r_kind) then
            factor = one / sqrt(super_gps(k,kprof))
        else
            factor = one / sqrt(max(super_gps(k-1,kprof),super_gps(k,kprof)))
        endif
        ratio_errors = ratio_errors * factor

!       Adjust error ratio for observations used in inner loop
        if (associated(gpsptr)) then
           gpsptr%raterr2 = ratio_errors **2
        endif

     endif


!    For given profile, check if observation level is below level at 
!    which profile data is tossed.   If so, set error parameter to 
!    zero (effectively tossing the obs).
     rhgt = gps_allptr%loc
     if (rhgt<toss_gps(kprof)) then
        ratio_errors = zero
        if (associated(gpsptr)) then
           gpsptr%raterr2 = ratio_errors **2
        endif
     endif

        
!    Compute penalty terms
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
        valqc = -two*rat_err2*term
        

!       Accumulate statistics for obs belonging to this task
        if(muse)then
           if(wgt < wgtlim) awork(21) = awork(21)+one
  
!          Accumulate values for penalty and data count
           jsig=max(ione,khgt)
           awork(jsig+3*nsig+100)=awork(jsig+3*nsig+100)+valqc
           awork(jsig+5*nsig+100)=awork(jsig+5*nsig+100)+one
           awork(jsig+6*nsig+100)=awork(jsig+6*nsig+100)+val2*rat_err2
        endif
          

!       Loop over pressure level groupings and obs to accumulate
!       statistics as a function of observation type.
        do k = 1,npres_print
           if(pressure>=ptop(k) .and. pressure<=pbot(k))then
              ress=data_rinc*scale
              ressw2=ress*ress
              nn=1
              if (.not. muse) then
                 nn=2
                 if(ratio_errors*data_ier >=tiny_r_kind)nn=3
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

! End loop over observations
  end do


! Destroy arrays holding gps data
  call destroy_genstats_gps

end subroutine genstats_gps
