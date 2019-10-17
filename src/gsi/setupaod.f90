module aero_setup
  implicit none
  private
  public:: setup
        interface setup; module procedure setupaod; end interface

contains
subroutine setupaod(obsLL,odiagLL,lunin,mype,nchanl,nreal,nobs,&
     obstype,isis,is,aero_diagsave,init_pass)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    setupaod    compute rhs of oi equation for aod
!   prgmmr: hclin            org: ncar/mmm                date: 2010-10-20
!
! abstract: read in data, first guess, and obtain rhs of oi equation
!        for aod.
!
! program history log:
!   2010-10-20  hclin - modified from setuprad for aod
!   2014-01-28  todling - write sensitivity slot indicator (ioff) to header of diagfile
!   2014-12-30  derber  - Modify for possibility of not using obsdiag
!   2015-10-01  guo   - full res obvsr: index to allow redistribution of obsdiags
!   2015-09-10  zhu  - generalize enabling all-sky and aerosol usage in radiance
!                      assimilation. Use radiance_obstype_search & type extentions
!   2016-02-20  pagowski - added NASA nnr AOD
!   2016-05-18  guo     - replaced ob_type with polymorphic obsNode through type casting
!   2016-06-24  guo     - fixed the default value of obsdiags(:,:)%tail%luse to luse(i)
!                       . removed (%dlat,%dlon) debris.
!   2017-02-09  guo     - Remove m_alloc, n_alloc.
!                       . Remove my_node with corrected typecast().
!   2018-05-19  eliu    - updated crtm interface 
!
!  input argument list:
!     lunin   - unit from which to read radiance (brightness temperature, tb) obs
!     mype    - mpi task id
!     nchanl  - number of channels per obs
!     nreal   - number of pieces of non-tb information per obs
!     nobs    - number of tb observations to process
!     obstype - type of tb observation
!     isis    - sensor/instrument/satellite id  ex.amsua_n15
!     is      - integer counter for number of observation types to process
!     aero_diagsave - logical to switch on diagnostic output (.false.=no output)
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use radinfo, only: nsigradjac
  use aeroinfo, only: nsigaerojac
  use crtm_interface, only: init_crtm,call_crtm,destroy_crtm,sensorindex, &
      isatid,itime,ilon,ilat,iszen_ang,isazi_ang
  use mpeu_util, only: die,perr
  use kinds, only: r_kind,r_single,i_kind
  use crtm_spccoeff, only: sc
  use obsmod, only: ianldate,mype_diaghdr,nchan_total, &
           dplat,lobsdiagsave,lobsdiag_allocated,&
           dirname,time_offset
  use obsmod, only: luse_obsdiag
  use obsmod, only: dirname
  use nc_diag_write_mod, only: nc_diag_init, nc_diag_header, nc_diag_metadata, &
       nc_diag_write, nc_diag_data2d
  use nc_diag_read_mod, only: nc_diag_read_init, nc_diag_read_get_dim, nc_diag_read_close
  use gsi_4dvar, only: nobs_bins,hr_obsbin
  use gridmod, only: nsig,get_ij
  use constants, only: tiny_r_kind,zero,one,three,r10
  use jfunc, only: jiter,miter
  use m_dtime, only: dtime_setup, dtime_check
  use chemmod, only: laeroana_gocart, l_aoderr_table
  use aeroinfo, only: jpch_aero, nusis_aero, nuchan_aero, iuse_aero, &
       error_aero, gross_aero
  use m_obsdiagNode, only: obs_diag
  use m_obsdiagNode, only: obs_diags
  use m_obsdiagNode, only: obsdiagLList_nextNode
  use m_obsdiagNode, only: obsdiagNode_set
  use m_obsdiagNode, only: obsdiagNode_get
  use m_obsdiagNode, only: obsdiagNode_assert

  use m_obsNode, only: obsNode
  use m_aeroNode, only: aeroNode
  use m_aeroNode, only: aeroNode_appendto
  use m_obsLList, only: obsLList
  use m_obsLList, only: obsLList_tailNode
  use obsmod, only: rmiss_single
  use qcmod, only: ifail_crtm_qc
  use radiance_mod, only: rad_obs_type,radiance_obstype_search

  implicit none

! Declare passed variables
  type(obsLList ),target,dimension(:),intent(in):: obsLL
  type(obs_diags),target,dimension(:),intent(in):: odiagLL
  logical                           ,intent(in   ) :: aero_diagsave
  character(10)                     ,intent(in   ) :: obstype
  character(20)                     ,intent(in   ) :: isis
  integer(i_kind)                   ,intent(in   ) :: lunin,mype,nchanl,nreal,nobs,is
  logical                           ,intent(in   ) :: init_pass  ! state of "setup" processing

! Declare external calls for code analysis
  external:: stop2

! Declare local parameters
  integer(i_kind),parameter:: ipchan=4
  integer(i_kind),parameter:: ireal=5

  real(r_kind),parameter:: r1e10=1.0e10_r_kind

! Declare local variables
  character(128) diag_aero_file

  integer(i_kind) error_status
  integer(i_kind) m,jc
  integer(i_kind) icc
  integer(i_kind) j,k,ncnt,i
  integer(i_kind) mm1
  integer(i_kind) n,ibin,ioff,ioff0,iii
  integer(i_kind) ii,jj,idiag

  real(r_single) freq4,pol4,wave4,varch4
  real(r_kind) errinv,useflag
  real(r_kind) trop5,pangs
  real(r_kind) cenlon,cenlat,slats,slons,dtime
  real(r_kind) val_obs

! Declare local arrays

  real(r_single),dimension(ireal):: diagbuf
  real(r_single),allocatable,dimension(:,:):: diagbufchan

  real(r_kind),dimension(nchanl):: varinv,error0
  real(r_kind),dimension(nchanl):: tnoise,errmax
  real(r_kind),dimension(nchanl):: var,ratio_aoderr,aodinv
  real(r_kind),dimension(nreal+nchanl,nobs)::data_s
  real(r_kind),dimension(nsig):: prsltmp
  real(r_kind),dimension(nsig):: qvp,tvp
  real(r_kind),dimension(nsig+1):: prsitmp
  real(r_kind) dtsavg
  real(r_single) :: psfc

  integer(i_kind),dimension(nchanl):: ich,id_qc

  character(10) filex
  character(12) string

  logical toss,l_may_be_passive
  logical,dimension(nobs):: luse
  integer(i_kind),dimension(nobs):: ioid ! initial (pre-distribution) obs ID
  integer(i_kind):: nperobs     ! No. of data points, in channels, levels, or components, per obs.

  logical:: in_curbin, in_anybin
  type(aeroNode),pointer:: my_head
  type(obs_diag),pointer:: my_diag, obsptr
  type(obs_diags),pointer:: my_diagLL
  type(rad_obs_type) :: radmod
  character(len=*),parameter:: myname="setupaod"

  real(r_kind), dimension(nchanl) :: total_aod, aod_obs, aod

  integer(i_kind) :: istyp, idbcf, ilone, ilate
  real(r_kind)    :: styp, dbcf

  real(r_kind),dimension(nchanl):: emissivity,ts,emissivity_k
  real(r_kind),dimension(nchanl):: tsim
  real(r_kind),dimension(nsig,nchanl):: wmix,temp,ptau5
  real(r_kind),dimension(nsigradjac,nchanl):: jacobian
  real(r_kind),dimension(nsigaerojac,nchanl):: jacobian_aero
  real(r_kind),dimension(nsig,nchanl):: layer_od
  real(r_kind) :: clw_guess, tzbgr, sfc_speed,ciw_guess,rain_guess,snow_guess

  type(obsLList),pointer,dimension(:):: aerohead
  aerohead => obsLL(:)

  if ( .not. laeroana_gocart ) then
     return
  endif

!**************************************************************************************
! Initialize variables and constants.
  mm1        = mype+1
  ncnt       = 0
  icc   = 0

  isatid    = 1  ! index of satellite id
  itime     = 2  ! index of analysis relative obs time 
  ilon      = 3  ! index of grid relative obs location (x)
  ilat      = 4  ! index of grid relative obs location (y)
  ilone     = 5  ! index of earth relative longitude (degrees)
  ilate     = 6  ! index of earth relative latitude (degrees)
  iszen_ang = 8  ! index of solar zenith angle (degrees)
  isazi_ang = 9  ! index of solar azimuth angle (degrees)
  istyp     = 10 ! index of surface type
  idbcf     = 11 ! index of deep blue confidence flag

! Determine cloud & aerosol usages in radiance assimilation
  call radiance_obstype_search(obstype,radmod)

! Initialize channel related information
  tnoise = r1e10
  errmax = r1e10
  l_may_be_passive = .false.
  toss = .true.
  jc=0
  do j=1,jpch_aero
     if(isis == nusis_aero(j))then 
        jc=jc+1
        if(jc > nchanl)then
           write(6,*)'setupaod:  ***ERROR*** in channel numbers, jc,nchanl=',jc,nchanl,&
                '  ***STOP IN setupaod***'
           call stop2(71)
        end if

!       Load channel numbers into local array based on satellite type

        ich(jc)=j
!
!       Set error instrument channels
        tnoise(jc)=error_aero(j)
        errmax(jc)=gross_aero(j)
        if (iuse_aero(j)< -1 .or. (iuse_aero(j) == -1 .and.  &
              .not.aero_diagsave)) tnoise(jc)=r1e10
        if (iuse_aero(j)>-1) l_may_be_passive=.true.
        if (tnoise(jc) < 1.e4_r_kind) toss = .false.
     end if
  end do
  if ( mype == 0 .and. .not.l_may_be_passive) write(6,*)mype,'setupaod: passive obs',is,isis
  if(nchanl > jc) write(6,*)'setupaod:  channel number reduced for ', &
       obstype,nchanl,' --> ',jc
  if(jc == 0) then
     if(mype == 0) write(6,*)'setupaod: No channels found for ', &
          obstype,isis
     if(nobs > 0)read(lunin)
     return
  end if
  if (toss) then
     if(mype == 0)write(6,*)'setupaod: all obs var > 1e4.  do not use ',&
          'data from satellite is=',isis
     if(nobs >0)read(lunin)                    
     return
  endif

  ioff0=0
  if (lobsdiagsave) then
     if (l_may_be_passive) then
         ioff0=4
     else
         ioff0=5
     endif
  endif

! Initialize radiative transfer
  call init_crtm(init_pass,mype_diaghdr(is),mype,nchanl,nreal,isis,obstype,radmod)

! If diagnostic file requested, open unit to file and write header.
  if (aero_diagsave) then
     filex=obstype
     write(string,1976) jiter
1976 format('_',i2.2)
     diag_aero_file= trim(dirname) // trim(filex) // '_' // trim(dplat(is)) // trim(string)
     if(init_pass) then
        open(4,file=trim(diag_aero_file),form='unformatted',status='unknown',position='rewind')
     else
        open(4,file=trim(diag_aero_file),form='unformatted',status='old',position='append')
     endif

!    Initialize/write parameters for satellite diagnostic file on
!    first outer iteration.
     if (init_pass .and. mype==mype_diaghdr(is)) then
        write(4) isis,dplat(is),obstype,jiter,nchanl,ianldate,ireal,ipchan,nsig,ioff0
        write(6,*)'setupaod:  write header record for ',&
             isis,ireal,' to file ',trim(diag_aero_file),' ',ianldate
        do i=1,nchanl
           n=ich(i)
           if( n < 1 )cycle
           varch4=error_aero(n)
           freq4=sc(sensorindex)%frequency(i)
           pol4=sc(sensorindex)%polarization(i)
           wave4=sc(sensorindex)%wavenumber(i)
           write(4)freq4,pol4,wave4,varch4,iuse_aero(n),&
                nuchan_aero(n),ich(i)
        end do
     endif
  endif

  idiag=ipchan
  if (lobsdiagsave) idiag=idiag+4*miter+1
  allocate(diagbufchan(idiag,nchanl))
  
! Load data array for current satellite
  read(lunin) data_s,luse,ioid

  write(*,*) 'read in data',nobs
! Loop over data in this block
  call dtime_setup()
  do n = 1,nobs
!    Extract analysis relative observation time.
     dtime = data_s(itime,n)
     call dtime_check(dtime, in_curbin, in_anybin)
     if(.not.in_anybin) cycle

     if(in_curbin) then

        id_qc = 0

!       Extract lon and lat.
        slons  = data_s(ilon,n)    ! grid relative longitude
        slats  = data_s(ilat,n)    ! grid relative latitude                     
        cenlon = data_s(ilone,n)   ! earth relative longitude (degrees)
        cenlat = data_s(ilate,n)   ! earth relative latitude (degrees)                       
        pangs  = data_s(iszen_ang,n)
        styp   = data_s(istyp,n)
        dbcf   = data_s(idbcf,n)
 
!       Set relative weight value
        val_obs=one

!       Load channel data into work array.
        aod_obs = rmiss_single
        do i = 1, nchanl
           aod_obs(i) = data_s(i+nreal,n)
        end do

        if ( .not. l_aoderr_table ) then
!          set observation error
           select case ( nint(styp) )
              case ( 0 )        ! water
                 tnoise = 0.03_r_kind+0.05_r_kind*aod_obs
              case ( 1, 2, 3 )  ! coast, desert, land
                 tnoise = 0.05_r_kind+0.15_r_kind*aod_obs
              case ( 4 )        ! deep blue
                 if ( nint(dbcf) >= 0 .and. nint(dbcf) <= 3 ) then
                    tnoise = 0.05_r_kind+0.15_r_kind*aod_obs+0.01_r_kind*(three-dbcf)
                 end if
              case ( 5 )  ! nnr ocean
                 tnoise = 0.2_r_kind*(aod_obs+0.01_r_kind)
              case ( 6 )  ! nnr land
                 tnoise = 0.2_r_kind*(aod_obs+0.01_r_kind)

                 
           end select
        end if
 
!       Interpolate model fields to observation location, call crtm and create jacobians
        call call_crtm(obstype,dtime,data_s(:,n),nchanl,nreal,ich, &
             tvp,qvp,clw_guess,ciw_guess,rain_guess,snow_guess,prsltmp,prsitmp, &
             trop5,tzbgr,dtsavg,sfc_speed, &
             tsim,emissivity,ptau5,ts,emissivity_k, &
             temp,wmix,jacobian,error_status,layer_od=layer_od,jacobian_aero=jacobian_aero)


! If the CRTM returns an error flag, do not assimilate any channels for this ob
! and set the QC flag to ifail_crtm_qc.
! We currently go through the rest of the QC steps, ensuring that the diagnostic
! files are populated, but this could be changed if it causes problems.
        if (error_status /=0) then
           id_qc(1:nchanl) = ifail_crtm_qc
           varinv(1:nchanl) = zero
        endif

        total_aod = zero
        do i = 1, nchanl
           total_aod(i) =sum(layer_od(:,i))
        enddo 

        do i = 1, nchanl
           aod(i) = aod_obs(i) - total_aod(i)
           error0(i)     = tnoise(i)
           if(aod_obs(i)>zero .and. tnoise(i) < 1.e4_r_kind .or. (iuse_aero(ich(i))==-1  &
              .and. aero_diagsave))then
              varinv(i)     = val_obs/tnoise(i)**2
           else
              if(id_qc(i) == 0)id_qc(i)=1
              varinv(i)     = zero
           endif
        end do

        !do i = 1, nchanl
        !   if ( aod_obs(i) > zero ) then
        !     write(6,'(A,3i6,4f8.3,2f8.2)') 'mype, iobs, ichan, aod_crtm, aod_obs, omb, err, lat, lon : ',  &
        !         mype, n, i, total_aod(i), aod_obs(i),aod(i), tnoise(i), cenlat, cenlon
        !   end if
        !end do

        icc = 0
        do i = 1, nchanl
           ! Only process observations to be assimilated
           if (varinv(i) > tiny_r_kind ) then
               m = ich(i)
               ! Only "good" obs are included in J calculation.
               if (iuse_aero(m) >= 1)then
                  icc = icc + 1
                  aodinv(icc) = aod(i)            ! obs-ges innovation
                  var(icc) = one/error0(i)**2     ! 1/(obs error)**2  (original uninflated error)
                  ratio_aoderr(icc)=error0(i)**2*varinv(i) ! (original error)/(inflated error)
               endif
           endif
        end do
     endif ! (in_curbin)

!    In principle, we want ALL obs in the diagnostics structure but for
!    passive obs (monitoring), it is difficult to do if aero_diagsave
!    is not on in the first outer loop. For now we use l_may_be_passive...
     if (l_may_be_passive) then
!       Link observation to appropriate observation bin
        if (nobs_bins>1) then
           ibin = NINT( dtime/hr_obsbin ) + 1
        else
           ibin = 1
        endif
        if (ibin<1.OR.ibin>nobs_bins) write(6,*)mype,'Error nobs_bins,ibin= ',nobs_bins,ibin

        if(luse_obsdiag) my_diagLL => odiagLL(ibin)

        if (in_curbin) then
!          Load data into output arrays
           if (icc > 0) then
              ncnt =ncnt+1
              nchan_total=nchan_total+icc

              allocate(my_head)
              call aeroNode_appendto(my_head,aerohead(ibin))

              my_head%idv = is
              my_head%iob = ioid(n)
              my_head%elat= data_s(ilate,n)
              my_head%elon= data_s(ilone,n)
 
              allocate(my_head%res(icc),my_head%err2(icc), &
                       my_head%raterr2(icc), &
                       my_head%daod_dvar(nsigaerojac,icc), &
                       my_head%ich(icc),&
                       my_head%icx(icc))
              if(luse_obsdiag)allocate (my_head%diags(icc))

              my_head%nlaero  = icc         ! profile observation count
              call get_ij(mm1,slats,slons,my_head%ij,my_head%wij)

              my_head%time=dtime
              my_head%luse=luse(n)
              my_head%ich(:)=-1

              iii=0
              do ii=1,nchanl
                 m=ich(ii)
                 if (varinv(ii)>tiny_r_kind .and. iuse_aero(m)>=1) then
                    iii=iii+1
                    my_head%res(iii)=aodinv(iii)
                    my_head%err2(iii)=var(iii)
                    my_head%raterr2(iii)=ratio_aoderr(iii)
                    my_head%icx(iii)=m
                    do k = 1, nsigaerojac
                       my_head%daod_dvar(k,iii)=jacobian_aero(k,ii)
                    end do
                    my_head%ich(iii)=ii
                 end if
              end do

              my_head => null()
           end if ! icc
        endif ! (in_curbin)

!       Link obs to diagnostics structure
        if(luse_obsdiag) then
           iii=0
           obsptr => null()
           do ii=1,nchanl
              nperobs=-99999; if(ii==1) nperobs=nchanl
              my_diag => obsdiagLList_nextNode(my_diagLL        ,&
                 create = .not.lobsdiag_allocated               ,&
                    idv = is                    ,&
                    iob = ioid(n)               ,&
                    ich = ii                    ,&
                   elat = data_s(ilate,n)       ,&
                   elon = data_s(ilone,n)       ,&
                   luse = luse(n)               ,&
                  miter = miter                 )

              if(.not.associated(my_diag)) call die(myname,'not associated(my_diag)')

              if (ii==1) obsptr => my_diag      ! this is the lead node

              if (in_curbin.and.icc>0) then
                 my_head => tailNode_typecast_(aerohead(ibin))
                 if(.not.associated(my_head)) &
                    call die(myname,'unexpected, associated(my_head) =',associated(my_head))

                 call obsdiagNode_set(my_diag, wgtjo=varinv(ii), jiter=jiter, nldepart=aod(ii) )
 
!                Load data into output arrays
                 m=ich(ii)
                 if (varinv(ii)>tiny_r_kind .and. iuse_aero(m)>=1) then
                    iii=iii+1

                    call obsdiagNode_assert(my_diag,my_head%idv,my_head%iob,my_head%ich(iii),myname,'my_diag:my_head error')

                    call obsdiagNode_set(my_diag, jiter=jiter, muse=.true.)

                    my_head%diags(iii)%ptr => my_diag
                 endif

                 my_head => null()
              endif ! (in_curbin)
           enddo ! do ii=1,nchanl
           if (in_curbin) then
              if( iii/=icc ) then
                 write(6,*)'setupaod: error iii icc',iii,icc
                 call stop2(279)
              endif
           endif ! (in_curbin)
        endif ! (luse_obsdiag)
 
!    End of l_may_be_passive block
     endif

     if(in_curbin) then
!       Write diagnostics to output file.
        if (aero_diagsave .and. luse(n)) then
           diagbuf(1)  = cenlat                         ! observation latitude (degrees)
           diagbuf(2)  = cenlon                         ! observation longitude (degrees)
           diagbuf(3)  = dtime-time_offset              ! observation time (hours relative to analysis time)
           diagbuf(4)  = pangs                          ! solar zenith angle (degrees)
           diagbuf(5)  = data_s(isazi_ang,n)            ! solar azimuth angle (degrees)
 
           do i=1,nchanl
              diagbufchan(1,i)=aod_obs(i)      ! observed brightness temperature (K)
!              diagbufchan(2,i)=total_aod(i)   ! observed - simulated Tb with no bias corrrection (K) - this should be innovation
              diagbufchan(2,i)=aod(i)          ! innovation
              errinv = sqrt(varinv(i))
              diagbufchan(3,i)=errinv          ! inverse observation error
              useflag=one
              if (iuse_aero(ich(i)) < 1) useflag=-one
              diagbufchan(4,i)= id_qc(i)*useflag! quality control mark or event indicator
           end do

           if (lobsdiagsave) then
              if (l_may_be_passive) then
                 do ii=1,nchanl
                    if (.not.associated(obsptr)) then
                       write(6,*)'setupaod: error obsptr'
                       call stop2(280)
                    end if

                    ioff=ioff0
                    do jj=1,miter
                       ioff=ioff+1
                       if (obsptr%muse(jj)) then
                          diagbufchan(ioff,ii) = one
                       else
                          diagbufchan(ioff,ii) = -one
                       endif
                    enddo
                    do jj=1,miter+1
                       ioff=ioff+1
                       diagbufchan(ioff,ii) = obsptr%nldepart(jj)
                    enddo
                    do jj=1,miter
                       ioff=ioff+1
                       diagbufchan(ioff,ii) = obsptr%tldepart(jj)
                    enddo
                    do jj=1,miter
                       ioff=ioff+1
                       diagbufchan(ioff,ii) = obsptr%obssen(jj)
                    enddo
 
                    obsptr => obsptr%next
                 enddo
              else
                 ioff=ioff0
                 diagbufchan(ioff+1:ioff+4*miter+1,1:nchanl) = zero
              endif
           endif

           psfc=prsitmp(1)*r10 ! convert to hPa
           write(4) psfc,diagbuf,diagbufchan

        end if
     endif ! (in_curbin)

100  continue

! End of n-loop over obs
  end do

! Jump here when there is no data to process for current satellite
! Deallocate arrays
  deallocate(diagbufchan)

  if (aero_diagsave) then
     close(4)
  endif

  call destroy_crtm

! End of routine

  return

contains
  function tailNode_typecast_(oll) result(ptr_)
!>  Cast the tailNode of oll to an aeroNode, as in
!>      ptr_ => typecast_(tailNode_(oll))

    use m_aeroNode, only: aeroNode, typecast_ => aeroNode_typecast
    use m_obsLList, only: obsLList, tailNode_ => obsLList_tailNode
    use m_obsNode , only: obsNode
    implicit none
    type(aeroNode),pointer:: ptr_
    type(obsLList),target ,intent(in):: oll

    class(obsNode),pointer:: inode_
    inode_ => tailNode_(oll)
    ptr_   => typecast_(inode_)
  end function tailNode_typecast_

  subroutine init_netcdf_diag_
  end subroutine init_netcdf_diag_
  subroutine contents_binary_diag_
  end subroutine contents_binary_diag_
  subroutine contents_netcdf_diag_
! Observation class
  character(7),parameter     :: obsclass = '    aod'
  end subroutine contents_netcdf_diag_
end subroutine setupaod
end module aero_setup
