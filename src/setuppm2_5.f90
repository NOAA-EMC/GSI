subroutine setuppm2_5(lunin,mype,nreal,nobs,isis,is)

!$$$  subprogram documentation block
!                .      .    .
! subprogram:    setuppm2_5 --- Compute rhs of oi for in-situ pm2_5 obs
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
!                  g) converted to pm2_5
!

! program history log:
!   1990-10-06  parrish
!   2010-04-01  tangborn - created from Parrish et al. setupoz.f90
!   2010-05-29  todling - add ihave-co check; revisit treatment of guess
!   2010-10-03  pagowski - converted for pm2_5
!
!   input argument list:
!     lunin          - unit from which to read observations
!     mype           - mpi task id
!     nreal          - number of pieces of info (location, time, etc) per obs
!     nobs           - number of observations
!     isis           - sensor/instrument/satellite id
!     is             - integer(i_kind) counter for number of obs types to process
!     
!     obstype        - type of pm2_5 obs
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP; SGI Origin 2000; Compaq HP
!
!$$$ end documentation block
     
! !uses:

  use mpeu_util, only: die,perr
  use kinds, only: r_kind,i_kind
  
  use constants, only : zero,half,one,two,tiny_r_kind
  use constants, only : cg_term,wgtlim
  
  use obsmod, only : pm2_5head,pm2_5tail,&
       pm2_5_ob_type,i_pm2_5_ob_type
  use obsmod, only : obsdiags,lobsdiag_allocated
  use obsmod, only : obs_diag
  use qcmod, only : dfact,dfact1
  
  use gsi_4dvar, only: nobs_bins,hr_obsbin
  
  use gridmod, only : get_ij,get_ijk,nsig
  
  use guess_grids, only : nfldsig,hrdifsig,ges_z
  use gsi_bundlemod, only : gsi_bundlegetpointer
  use gsi_chemtracer_mod, only : gsi_chem_bundle
  
  use convinfo, only: cgross,cvar_b,cvar_pg,&
        ihave_pm2_5,icuse
  
  use jfunc, only : jiter,last,miter
  
  use m_dtime, only: dtime_setup, dtime_check

  use chemmod, only : &
        iconc,ierror,ilat,ilon,itime,ielev,isite,iikx,&
        elev_tolerance,elev_missing,pm2_5_teom_max
  use chemmod, only : oneobtest_chem,maginnov_chem


  implicit none
  
! !input parameters:
  
  integer(i_kind)                  , intent(in   ) :: lunin  ! unit from which to read observations
  integer(i_kind)                  , intent(in   ) :: mype   ! mpi task id
  integer(i_kind)                  , intent(in   ) :: nreal  ! number of pieces of non-co info (location, time, etc) per obs
  integer(i_kind)                  , intent(inout   ) :: nobs   ! number of observations
  character(20)                    , intent(in   ) :: isis   ! sensor/instrument/satellite id
  integer(i_kind)                  , intent(in   ) :: is     

  
! a function of level
!-------------------------------------------------------------------------
  
! declare local parameters  

  character(len=*),parameter:: myname="setuppm2_5"
  
! declare local variables  
  
  real(r_kind) rat_err2,dlat,dtime,dlon
  real(r_kind) cg_pm2_5,wgross,wnotgross,wgt,arg,exp_arg,term
  real(r_kind) :: pm2_5ges
  real(r_kind) :: ratio_errors,error
  real(r_kind) :: innov,innov_error2,rwgt,valqc,tfact,innov_error,elevges,&
        elevdiff,conc,elevobs
  
  real(r_kind) ,dimension(nreal,nobs):: data
  real(r_kind),allocatable,dimension(:,:,:,:):: ges_pm2_5
  real(r_kind),pointer,dimension(:,:,:):: rank3
  
  integer(i_kind) i,k,ier,ibin,ifld,l,istat,ikx
  integer(i_kind) ikeep,nkeep
  integer(i_kind) mm1

  real(r_kind),dimension(4):: tempwij

  logical,dimension(nobs):: luse,muse
  integer(i_kind),dimension(nobs) :: dup

  logical:: in_curbin, in_anybin
  integer(i_kind),dimension(nobs_bins) :: n_alloc
  integer(i_kind),dimension(nobs_bins) :: m_alloc
  type(pm2_5_ob_type),pointer:: my_head
  type(obs_diag),pointer:: my_diag

  n_alloc(:)=0
  m_alloc(:)=0

  mm1=mype+1

!
!*********************************************************************************
! get pointer to pm2_5 guess state, if not present return 
  
  if (.not.ihave_pm2_5) then 
     write(6,*)'pm2_5 not in chem_guess - returning from setuppm2_5'
     return
  endif

  if (size(gsi_chem_bundle)==nfldsig) then
     call gsi_bundlegetpointer(gsi_chem_bundle(1),'pm2_5',rank3,ier)
     allocate(ges_pm2_5(size(rank3,1),size(rank3,2),size(rank3,3),&
           nfldsig))
     ges_pm2_5(:,:,:,1)=rank3
     do ifld=2,nfldsig
        call gsi_bundlegetpointer(gsi_chem_bundle(ifld),'pm2_5',rank3,ier)
        ges_pm2_5(:,:,:,ifld)=rank3
    enddo
  else
     write(6,*) 'setuppm2_5: inconsistent vector sizes (nfldsig,size(chem_bundle) ',&
          nfldsig,size(gsi_chem_bundle)
     call stop2(420)
  endif

! initialize arrays

  read(lunin)data,luse

  dup=one

  do i=1,nobs
     muse(i) = (icuse(nint(data(iikx,i))) <= jiter)
  enddo

  do k=1,nobs
     do l=k+1,nobs
        if(data(ilat,k) == data(ilat,l) .and.  &
             data(ilon,k) == data(ilon,l) .and.  &
             data(ielev,k) == data(ielev,l) .and.  &
             data(ielev,k) == data(ielev,l) .and.  &
             data(isite,k) == data(isite,l) .and.  &
             muse(k) .and. muse(l)) then
           tfact = min(one,abs(data(itime,k)-data(itime,l))/dfact1)
           dup(k)=dup(k)+one-tfact*tfact*(one-dfact)
           dup(l)=dup(l)+one-tfact*tfact*(one-dfact)
        end if
     end do
  end do
  
  nkeep=0
  
  do i=1,nobs
     ikeep=0
     if ( muse(i) ) ikeep=1
     nkeep=nkeep+ikeep
  end do
  
! if none of the data will be assimilated and don't need diagnostics,
! return to calling program
  if (nkeep==0) then
     deallocate(ges_pm2_5)
     return
  endif

  call dtime_setup()
  
  if (trim(isis)=='TEOM') then

     do i=1,nobs
        
        dtime=data(itime,i)

        call dtime_check(dtime, in_curbin, in_anybin)
        if(.not.in_anybin) then 
           if (oneobtest_chem) then
              write(6,*)'oneobtest_chem outside time window'
              call stop2(414)
           else
              cycle
           endif
        endif

        if (nobs_bins > 1) then
           ibin = nint( dtime/hr_obsbin ) + 1
        else
           ibin = 1
        endif

        if (ibin < 1 .or. ibin > nobs_bins) &
              write(6,*)mype,'error nobs_bins,ibin= ',nobs_bins,ibin
        
!    link obs to diagnostics structure
        if (.not.lobsdiag_allocated) then
           if (.not.associated(obsdiags(i_pm2_5_ob_type,ibin)%head)) then
              allocate(obsdiags(i_pm2_5_ob_type,ibin)%head,stat=istat)
              if (istat/=0) then
                 write(6,*)'setupq: failure to allocate obsdiags',istat
                 call stop2(421)
              end if
              obsdiags(i_pm2_5_ob_type,ibin)%tail => obsdiags(i_pm2_5_ob_type,ibin)%head
           else
              allocate(obsdiags(i_pm2_5_ob_type,ibin)%tail%next,stat=istat)
              if (istat/=0) then
                 write(6,*)'setupq: failure to allocate obsdiags',istat
                 call stop2(422)
              end if
              obsdiags(i_pm2_5_ob_type,ibin)%tail => obsdiags(i_pm2_5_ob_type,ibin)%tail%next
           end if

           allocate(obsdiags(i_pm2_5_ob_type,ibin)%tail%muse(miter+1))
           allocate(obsdiags(i_pm2_5_ob_type,ibin)%tail%nldepart(miter+1))
           allocate(obsdiags(i_pm2_5_ob_type,ibin)%tail%tldepart(miter))
           allocate(obsdiags(i_pm2_5_ob_type,ibin)%tail%obssen(miter))

           obsdiags(i_pm2_5_ob_type,ibin)%tail%indxglb=i
           obsdiags(i_pm2_5_ob_type,ibin)%tail%nchnperobs=-99999
           obsdiags(i_pm2_5_ob_type,ibin)%tail%luse=.false.
           obsdiags(i_pm2_5_ob_type,ibin)%tail%muse(:)=.false.
           obsdiags(i_pm2_5_ob_type,ibin)%tail%nldepart(:)=-huge(zero)
           obsdiags(i_pm2_5_ob_type,ibin)%tail%tldepart(:)=zero
           obsdiags(i_pm2_5_ob_type,ibin)%tail%wgtjo=-huge(zero)
           obsdiags(i_pm2_5_ob_type,ibin)%tail%obssen(:)=zero
           
           n_alloc(ibin) = n_alloc(ibin) +1
           my_diag => obsdiags(i_pm2_5_ob_type,ibin)%tail
           my_diag%idv = is
           my_diag%iob = i
           my_diag%ich = 1
           
        else
           if (.not.associated(obsdiags(i_pm2_5_ob_type,ibin)%tail)) then
              obsdiags(i_pm2_5_ob_type,ibin)%tail => obsdiags(i_pm2_5_ob_type,ibin)%head
           else
              obsdiags(i_pm2_5_ob_type,ibin)%tail => obsdiags(i_pm2_5_ob_type,ibin)%tail%next
           end if
           if (obsdiags(i_pm2_5_ob_type,ibin)%tail%indxglb/=i) then
              write(6,*)'setuppm2_5: index error'
              call stop2(423)
           end if
        endif
        
        if(.not.in_curbin) cycle
        
        dlat=data(ilat,i)
        dlon=data(ilon,i)
        conc=data(iconc,i)
        elevobs=data(ielev,i)
        ikx=nint(data(iikx,i))
        
        call tintrp2a(ges_z,elevges,dlat,dlon,dtime,hrdifsig,&
             1,1,mype,nfldsig)
        
!if elevobs is known than calculate difference otherwise
!assume that difference is acceptable
        
        if (elevobs > elev_missing) then
           elevdiff=abs(elevobs-elevges)
        else
           elevdiff=zero
!if elevation unknown include observation nevertheless
        endif
        
        if (oneobtest_chem) then
           innov = min(maginnov_chem,cgross(ikx))
        else
           call tintrp2a(ges_pm2_5,pm2_5ges,dlat,dlon,dtime,hrdifsig,&
                1,1,mype,nfldsig)
           innov = conc - pm2_5ges
        end if

        error=one/data(ierror,i)
        ratio_errors=one/sqrt(real(dup(i)))
        innov_error = error*innov
        
        if (abs(innov) > cgross(ikx) .or. &
              conc > pm2_5_teom_max  .or. &
              elevdiff > elev_tolerance) then
           muse(i)=.false.
        endif

        rat_err2 = ratio_errors**2
        
        if(luse(i))then
           
           innov_error2     = innov_error*innov_error
           exp_arg = -half*innov_error2
           rat_err2 = ratio_errors**2
                 
           if (cvar_pg(ikx) > tiny_r_kind ) then
              arg  = exp(exp_arg)
              wnotgross= one-cvar_pg(ikx)
              cg_pm2_5=cvar_b(ikx)
              wgross = cg_term*cvar_pg(ikx)/(cg_pm2_5*wnotgross)
              term = log((arg+wgross)/(one+wgross))
              wgt  = one-wgross/(arg+wgross)
              rwgt = wgt/wgtlim
           else
              term = exp_arg
              wgt  = wgtlim
              rwgt = wgt/wgtlim
           endif
           
           valqc = -two*rat_err2*term

        endif

        obsdiags(i_pm2_5_ob_type,ibin)%tail%luse=luse(i)
        obsdiags(i_pm2_5_ob_type,ibin)%tail%muse(jiter)=muse(i)
        obsdiags(i_pm2_5_ob_type,ibin)%tail%nldepart(jiter)=innov
        obsdiags(i_pm2_5_ob_type,ibin)%tail%wgtjo= (error*ratio_errors)**2

        if (.not. last .and. muse(i)) then
           
           if(.not. associated(pm2_5head(ibin)%head))then
              allocate(pm2_5head(ibin)%head,stat=istat)
              if(istat /= 0)write(6,*)' failure to write pm2_5head '
              pm2_5tail(ibin)%head => pm2_5head(ibin)%head
           else
              allocate(pm2_5tail(ibin)%head%llpoint,stat=istat)
              if(istat /= 0)write(6,*)' failure to write pm2_5tail%llpoint '
              pm2_5tail(ibin)%head => pm2_5tail(ibin)%head%llpoint
           end if
           
           m_alloc(ibin) = m_alloc(ibin) +1
           my_head => pm2_5tail(ibin)%head
           my_head%idv = is
           my_head%iob = i
           
           call get_ij(mm1,dlat,dlon,&
                 pm2_5tail(ibin)%head%ij(1),tempwij(1))

           pm2_5tail(ibin)%head%ij(5:8)=pm2_5tail(ibin)%head%ij(1:4)
           pm2_5tail(ibin)%head%wij(1:4)=tempwij
           pm2_5tail(ibin)%head%wij(5:8)=zero
           pm2_5tail(ibin)%head%res    = innov
           pm2_5tail(ibin)%head%err2   = error**2
           pm2_5tail(ibin)%head%raterr2= ratio_errors**2
           pm2_5tail(ibin)%head%time   = dtime
           pm2_5tail(ibin)%head%b      = cvar_b(ikx)
           pm2_5tail(ibin)%head%pg     = cvar_pg(ikx)
           pm2_5tail(ibin)%head%luse   = luse(i)
           pm2_5tail(ibin)%head%diags => &
                 obsdiags(i_pm2_5_ob_type,ibin)%tail

           my_head => pm2_5tail(ibin)%head
           my_diag => pm2_5tail(ibin)%head%diags
           if(my_head%idv /= my_diag%idv .or. &
                my_head%iob /= my_diag%iob ) then
              call perr(myname,'mismatching %[head,diags]%(idv,iob,ibin) =',&
                    (/is,i,ibin/))
              call perr(myname,'my_head%(idv,iob) =',&
                    (/my_head%idv,my_head%iob/))
              call perr(myname,'my_diag%(idv,iob) =',&
                    (/my_diag%idv,my_diag%iob/))
              call die(myname)
           endif

        endif

     enddo

  else


!will be similar except for get_ijk 
!if not teom isis fill in 
!should be used for other in-situ obs e.g. soundings/aircraft e.g.

  endif

  return

end subroutine setuppm2_5
