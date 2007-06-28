subroutine read_obs(ndata,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_obs              read, select, reformat obs data
!   prgmmr: parrish          org: np22                date: 1990-10-07
!
! abstract:  This routine is a driver for routines which read different
!            types of observational data.
!
! program history log:
!   1990-10-07  parrish
!   1998-05-15  weiyu yang 
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-06-16  treadon - update documentation
!   2004-07-23  derber - modify to include conventional sst
!   2004-07-29  treadon - add only to module use, add intent in/out
!   2005-01-20  okamoto - add calling read_ssmi
!   2005-06-14  wu      - add OMI oz
!   2005-07-06  derber - add mhs, hirs/4 and ears data 
!   2005-08-02  derber - modify to use convinfo file
!   2005-09-08  derber - modify to use input group time window
!   2005-09-20  xu & pawlak - modify calling read_ssmis and read_amsre
!   2005-09-28  derber - modify to simplify obs handling      
!   2005-10-17  derber - pass obs_load1 into read_amsre and read_ssmis 
!   2005-10-18  treadon - remove obs_load and obs_load1
!   2005-10-20  kazumori - modify to read real AMSR-E data
!   2005-11-28  derber move determination of which ob data sets to read inside read_obs
!   2005-11-14  li, xu - modify sst obs read and add avhrr gac 1b obs read
!   2006-01-25  treadon - remove read_ieeetovs
!   2006-02-01  parrish - add getsfc and destroy_sfc for full surface fields
!   2006-02-03  derber  - modify for new obs control and obs count
!   2005-02-03  treadon - gather guess 3d pressure to full grid array
!   2006-02-08  derber  - modify to use new convinfo module
!   2006-02-01  liu - add ssu
!   2006-02-24  derber  - modify to take advantage of convinfo module
!   2006-03-07  derber - consolidate processing of 1x1 and 5x5 goessndr
!   2006-05-25  treadon - rename goesimg and goes_img and pcp_ssm/i as
!                         pcp_ssmi to make consistent with other obstype
!   2006-09-20  treadon - add mpi_io for select data file (obstype)s
!   2006-10-12  treadon - remove tendsflag check for pcp data (now in gsimain)
!   2007-06-05  treadon - restructure mpi_querybf section to improve efficiency
!
!   input argument list:
!     mype     - mpi task id
!
!   output argument list:
!     ndata(*,1)- number of prefiles retained for further processing
!     ndata(*,2)- number of observations read
!     ndata(*,3)- number of observations keep after read
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
    use kinds, only: r_kind,i_kind
    use gridmod, only: nlon,nlat,nsig,iglobal,ijn,itotsub,lat1,lon1,&
         ltosi,ltosj,displs_g
    use obsmod, only: iadate,ndat,time_window,ipoint,dplat,dfile,dthin, &
           dtype,dval,dmesh,obsfile_all,ref_obs,nprof_gps,dsis,ditype
    use satthin, only: super_val,super_val1,superp,makegvals,getsfc,destroy_sfc
    use mpimod, only: ierror,mpi_comm_world,mpi_sum,mpi_rtype,mpi_integer,npe,&
         strip,reorder,setcomm
    use mpi_bufr_mod, only: mpi_querybf
    use constants, only: zero,quarter,izero,one,half,ione
    use convinfo, only: convinfo_read
    use guess_grids, only: ges_prsl,ntguessig
    use satthin, only: indexx

    implicit none

!   Declare passed variables
    integer(i_kind),intent(in):: mype
    integer(i_kind),dimension(ndat,3),intent(out):: ndata

!   Declare local variables
    logical:: lexist
    logical,dimension(npe,ndat):: belong
    character(10):: obstype,infile,platid
    character(13):: string
    character(20):: sis
    integer(i_kind) i,j,k,ii,nmind,lunout,ithinx,ithin,nread,npuse,nouse
    integer(i_kind) nprof_gps1,npem1
    integer(i_kind):: iworld,iworld_group,next_mype,nhalf,mm1
    integer(i_kind):: iworld_task,iworld_group_task
    integer(i_kind):: mype_root
    integer(i_kind),dimension(ndat):: member_task
    integer(i_kind),dimension(ndat):: ntasks_read,nread_tasks,indx
    integer(i_kind),dimension(ndat):: npe_sub,mpi_comm_sub
    integer(i_kind),dimension(ndat):: ntasks,ntasks1,mpi_comm_task
    integer(i_kind),dimension(ndat,3):: ndata1
    integer(i_kind),dimension(npe,ndat):: mype_work
    integer(i_kind),dimension(npe,ndat):: mype_sub
    real(r_kind) gstime,val_dat,rmesh,twind,rtasks_all
    real(r_kind),dimension(lat1*lon1,nsig):: prslsm
    real(r_kind),dimension(max(iglobal,itotsub)):: work1
    real(r_kind),dimension(nlat,nlon,nsig):: prsl_full
    real(r_kind),dimension(ndat):: sort_key

    data lunout / 81 /

!*****************************************************************************
!   Create full horizontal surface fields from local fields in guess_grids
    call getsfc(mype)


!   Set analysis time and allocate/initialize arrays and variables
    call w3fs21(iadate,nmind)
    gstime=float(nmind)

    call makegvals
    do ii=1,ndat
      ndata1(ii,1)=izero
      ndata1(ii,2)=izero
      ndata1(ii,3)=izero
      ntasks(ii)  =izero
      ntasks1(ii) =izero
    end do
    npem1=npe-1
    nprof_gps1=izero


!   Read convinfo file
    call convinfo_read(mype)


!   Get guess 3d pressure on full grid
    call strip(ges_prsl(1,1,1,ntguessig),prslsm,nsig)
    do k=1,nsig
       call mpi_allgatherv(prslsm(1,k),ijn(mype+1),mpi_rtype,&
            work1,ijn,displs_g,mpi_rtype,mpi_comm_world,ierror)
       call reorder(work1,1)
       do ii=1,iglobal
          i=ltosi(ii)
          j=ltosj(ii)
          prsl_full(i,j,k)=work1(ii)
       end do
    end do


!   Create sub-communicators to query bufr files
    ii=0
    do i=1,ndat
       mpi_comm_task(i)= mpi_comm_world
       iworld_task     = mpi_comm_world
       member_task(i)  = ii

       call mpi_comm_group(iworld_task,iworld_group_task,ierror)
       call setcomm(iworld_task,iworld_group_task,ione,member_task(i),mpi_comm_task(i),ierror)

       ii=ii+1
       if (ii>npem1) ii=0
    end do


!   Loop over data sets to determine optimal number of reader tasks for each bufr file
    do i=1,ndat
       inquire(file=dfile(i),exist=lexist)
       if (lexist .and. (index(dfile(i),'bufr') /=0) .and. mype==member_task(i)) &
            call mpi_querybf(dfile(i),mpi_comm_task(i),ntasks1(i))
    end do


!   Distribute optimal number of reader tasks to all mpi tasks
    call mpi_allreduce(ntasks1,ntasks,ndat,mpi_integer,mpi_sum,mpi_comm_world,ierror)


!   Set data class and number of reader tasks.  Set logical flag to indicate 
!   type type of GPS data (if present)
    ref_obs = .false.    !.false. = assimilate GPS bending angle
    do i=1,ndat
       obstype=dtype(i)                   !     obstype  - observation types to process
       if (obstype == 't'  .or. obstype == 'uv' .or. &
           obstype == 'q'  .or. obstype == 'ps' .or. &
           obstype == 'pw' .or. obstype == 'spd'.or. &
           obstype == 'sst'.or. obstype == 'srw'.or. &
           obstype == 'dw' .or. obstype == 'rw' ) then
           ditype(i) = 'conv'
       else if(obstype == 'hirs2'     .or. obstype == 'hirs3'     .or.  &
               obstype == 'hirs4'     .or. obstype == 'sndr'      .or.  &
               obstype == 'sndrd1'    .or. obstype == 'sndrd2'    .or.  &
               obstype == 'sndrd3'    .or. obstype == 'sndrd4'    .or.  &
               obstype == 'airs'      .or. obstype == 'amsua'     .or.  &
               obstype == 'msu'       .or.  &
               obstype == 'amsub'     .or. obstype == 'mhs'       .or.  &
               obstype == 'hsb'       .or. obstype == 'goes_img'  .or.  &
               obstype == 'avhrr'     .or. obstype == 'avhrr_navy'.or.  &
               obstype == 'amsre_low' .or. obstype == 'amsre_mid' .or.  &
               obstype == 'amsre_hig' .or. obstype == 'ssmi'      .or.  &
               obstype == 'ssmis'     .or. obstype == 'ssmis_las' .or.  &
               obstype == 'ssmis_uas' .or. obstype == 'ssmis_img' .or.  &
               obstype == 'ssmis_env' .or. obstype == 'ssu' ) then
            ditype(i) = 'rad'
       else if (obstype == 'sbuv2' .or. obstype == 'omi') then
            ditype(i) = 'ozone'
       else if (obstype == 'pcp_ssmi'  .or. obstype == 'pcp_tmi' &
           .or. obstype == 'pcp_amsu'  .or. obstype == 'pcp_stage3')then
            ditype(i) = 'pcp'
       else if (obstype == 'gps_ref' .or. obstype == 'gps_bnd') then
            ditype(i) = 'gps'
       else
            write(6,*)'READ_OBS:  ***ERROR*** - unknown ob type ',obstype
       end if

       if (index(dtype(i),'gps_ref') /= 0) ref_obs = .true.


!      Initialize number of reader tasks to 1.  For the time being
!      only allow number of reader tasks > 1 for select obstype.
       ntasks_read(i)=1
       if ( ditype(i)=='rad') then
          if (obstype=='airs') ntasks_read(i)=ntasks(i)
          if (obstype=='amsua' .and. dplat(i)=='aqua') ntasks_read(i)=ntasks(i)
          if (index(obstype,'amsre')/=0) ntasks_read(i)=ntasks(i)
          if (index(obstype,'sndr')/=0) ntasks_read(i)=ntasks(i)
          if (index(obstype,'ssmis')/=0) ntasks_read(i)=ntasks(i)
          if (obstype=='ssmi') ntasks_read(i)=ntasks(i)
       endif
       
       sort_key(i)=ntasks_read(i)
       indx(i)=i
    end do


!   Sort number of reader tasks in ascending order
    call indexx(ndat,sort_key,indx)


!   Define sub-communicators for each data file
    mm1=mype+1
    belong=.false.
    mype_sub=-999
    mype_root=0
    next_mype=0
    do i=1,ndat
       ii=indx(i)
       npe_sub(ii) = min(ntasks_read(ii),npe)
       nhalf = half*npe_sub(ii)
       if (npe_sub(ii)>0) then
          j=0
          mype_loop:  do k=1,npe_sub(ii)
             j=j+1
             mype_work(k,ii) = next_mype
             belong(next_mype+1,ii) = .true.
             next_mype = next_mype + 1
             if (next_mype>npe-1) then
                next_mype=0
                if (j>nhalf) then
                   npe_sub(ii)=j
                   exit mype_loop
                endif
             endif
          end do mype_loop

          do k=1,npe_sub(ii)
             mype_sub(mype_work(k,ii)+1,ii)=k-1
          end do

          mpi_comm_sub(ii)=mpi_comm_world
          iworld=mpi_comm_world
          call mpi_comm_group(iworld,iworld_group,ierror)
          call setcomm(iworld,iworld_group,npe_sub(ii),mype_work(1,ii),&
               mpi_comm_sub(ii),ierror)
       endif

    end do


!   Loop over data files.  Each data file is read by a sub-communicator
    do i=1,ndat

       if (belong(mm1,i)) then

          if (mype_sub(mm1,i)==mype_root) then
             open(lunout,file=obsfile_all(i),form='unformatted')
             rewind(lunout)
          endif

          platid=dplat(i)                    !     platid   - satellites to read
          obstype=dtype(i)                   !     obstype  - observation types to process
          sis=dsis(i)                        !     sensor/instrument/satellite indicator
          infile=dfile(i)                    !     infile   - units from which to read data
          val_dat=dval(i)                    !     weighting factors applied to super obs
          ithin=dthin(i)                     !     ithin    - flags to thin data
          ithinx=max(1,abs(ithin))
          rmesh=dmesh(ithinx)                !     rmesh    - thinning mesh sizes (km)
          twind=time_window(i)               !     time window (hours) for input group
          nread=izero
          nouse=izero
          npuse=izero

!         Process conventional (prepbufr) data
          if(ditype(i) == 'conv')then
             if (obstype == 't'  .or. obstype == 'uv' .or. &
                  obstype == 'q'  .or. obstype == 'ps' .or. &
                  obstype == 'pw' .or. obstype == 'spd' ) then
                call read_prepbufr(nread,npuse,nouse,infile,obstype,lunout,twind,sis,&
                     prsl_full)
                string='READ_PREPBUFR'

!            Process conventional SST (modsbufr, at this moment) data
             elseif ( obstype == 'sst' ) then
                if ( platid == 'mods') then
                   call read_modsbufr(nread,npuse,nouse,gstime,infile,obstype, &
                        lunout,twind,sis)
                   string='READ_MODSBUFR'
                else
                   call read_prepbufr(nread,npuse,nouse,infile,obstype,lunout,twind,sis,&
                        prsl_full)
                   string='READ_PREPBUFR'
                endif

!            Process radar winds
             else if (obstype == 'rw') then
                call read_radar(nread,npuse,nouse,infile,lunout,obstype,twind,sis)
                string='READ_RADAR'

!            Process lidar winds
             else if (obstype == 'dw') then
                call read_lidar(nread,npuse,nouse,infile,obstype,lunout,twind,sis)
                string='READ_LIDAR'

!            Process radar superob winds
             else if (obstype == 'srw') then
                call read_superwinds(nread,npuse,nouse,infile,obstype,lunout, &
                     twind,sis)
                string='READ_SUPRWNDS'
             end if

          else if (ditype(i) == 'rad')then

!            Process TOVS 1b data
             if (platid /= 'aqua' .and. (obstype == 'amsua' .or. &
                  obstype == 'amsub' .or. obstype == 'msu'   .or.  &
                  obstype == 'mhs'   .or. obstype == 'hirs4' .or.  &
                  obstype == 'hirs3' .or. obstype == 'hirs2' .or.  &
                  obstype == 'ssu')) then
                call read_bufrtovs(mype,val_dat,ithin,rmesh,platid,gstime,&
                     infile,lunout,obstype,nread,npuse,nouse,twind,sis)
                string='READ_BUFRTOVS'

!            Process airs data        
             else if(platid == 'aqua' .and. (obstype == 'airs' .or.   &
                  obstype == 'amsua'  .or.  obstype == 'hsb' ))then
                call read_airs(mype,val_dat,ithin,rmesh,platid,gstime,&
                     infile,lunout,obstype,nread,npuse,nouse,twind,sis,&
                     mype_root,mype_sub(mm1,i),npe_sub(i),mpi_comm_sub(i))
                string='READ_AIRS'

!            Process GOES sounder data
!            Process raw or prepbufr files (1x1 or 5x5)
             else if (obstype == 'sndr' .or.                            &
                  obstype == 'sndrd1' .or. obstype == 'sndrd2' .or. &
                  obstype == 'sndrd3' .or. obstype == 'sndrd4') then
                call read_goesndr(mype,val_dat,ithin,rmesh,platid,&
                     infile,lunout,obstype,nread,npuse,nouse,twind,gstime,sis,&
                     mype_root,mype_sub(mm1,i),npe_sub(i),mpi_comm_sub(i))
                string='READ_GOESNDR'
                
!            Process ssmi data
             else if (obstype == 'ssmi' ) then 
                call read_ssmi(mype,val_dat,ithin,rmesh,platid,gstime,&
                     infile,lunout,obstype,nread,npuse,nouse,twind,sis,&
                     mype_root,mype_sub(mm1,i),npe_sub(i),mpi_comm_sub(i))
                string='READ_SSMI'

!            Process amsre data
             else if ( obstype == 'amsre_low' .or. obstype == 'amsre_mid' .or. &
                  obstype == 'amsre_hig' ) then
                call read_amsre(mype,val_dat,ithin,rmesh,platid,gstime,&
                     infile,lunout,obstype,nread,npuse,nouse,twind,sis,&
                     mype_root,mype_sub(mm1,i),npe_sub(i),mpi_comm_sub(i))
                string='READ_AMSRE'
                
!            Process ssmis data
             else if ( obstype == 'ssmis'     .or. &
                  obstype == 'ssmis_las' .or. obstype == 'ssmis_uas' .or. &
                  obstype == 'ssmis_img' .or. obstype == 'ssmis_env' ) then
                call read_ssmis(mype,val_dat,ithin,rmesh,platid,gstime,&
                     infile,lunout,obstype,nread,npuse,nouse,twind,sis,&
                     mype_root,mype_sub(mm1,i),npe_sub(i),mpi_comm_sub(i))
                string='READ_SSMIS'

!            Process GOES IMAGER RADIANCE  data
             else if(obstype == 'goes_img') then
                call read_goesimg(mype,val_dat,ithin,rmesh,platid,gstime,&
                     infile,lunout,obstype,nread,npuse,nouse,twind,sis)
                string='READ_GOESMIMG'

!            Process NAVY AVHRR RADIANCE  data
             else if(obstype == 'avhrr_navy') then
                call read_avhrr_navy(mype,val_dat,ithin,rmesh,platid,gstime,&
                     infile,lunout,obstype,nread,npuse,nouse,twind,sis)
                string='READ_AVH_NAVY'

!            Process NESDIS AVHRR RADIANCE  data
             else if(obstype == 'avhrr') then
                call read_avhrr(mype,val_dat,ithin,rmesh,platid,gstime,&
                     infile,lunout,obstype,nread,npuse,nouse,twind,sis)
                string='READ_AVHRR'
             end if

!         Process ozone data
          else if (ditype(i) == 'ozone')then
             call read_ozone(nread,npuse,nouse,&
                  platid,infile,gstime,lunout,obstype,twind,sis)
             string='READ_OZONE'

!         Process precipitation             
          else if (ditype(i) == 'pcp')then
             call read_pcp(nread,npuse,nouse,mype,platid,infile, &
                  lunout,obstype,twind,sis)
             string='READ_PCP'

!         Process gps observations
          else if (ditype(i) == 'gps')then
             call read_gps(nread,npuse,nouse,infile,lunout,obstype,twind, &
                  nprof_gps1,sis)
             string='READ_GPS'
             
          end if

!         Close unit to data file
          if (mype_sub(mm1,i)==mype_root) close(lunout)

!         Accumulate data counts on "root" task
          if (mype_sub(mm1,i)==mype_root) then
             ndata1(i,1)=ndata1(i,1)+npuse
             ndata1(i,2)=ndata1(i,2)+nread
             ndata1(i,3)=ndata1(i,3)+nouse

             write(6,8000) adjustl(string),infile,obstype,sis,nread,ithin,&
                  rmesh,nouse,npe_sub(i)
8000         format(a13,': file=',a10,' type=',a10,' sis=',a20,&
                  ' nread=',i10,'  ithin=',i2,'  rmesh=',f7.3,&
                  '  ndata=',i10,'  ntask=',i3)
             
          endif
       endif

    end do


!   Sum and distribute number of obs read and used for each input ob group
    call mpi_allreduce(ndata1,ndata,ndat*3,mpi_integer,mpi_sum,mpi_comm_world,&
       ierror)

!   Collect super obs factors
    call mpi_allreduce(super_val,super_val1,superp+1,mpi_rtype,&
         mpi_sum,mpi_comm_world,ierror)
    super_val1(0)=1.
    deallocate(super_val)

!   Collect number of gps profiles (needed later for qc)
    call mpi_allreduce(nprof_gps1,nprof_gps,1,mpi_integer,mpi_sum,mpi_comm_world,ierror)

!   Deallocate arrays containing full horizontal surface fields
    call destroy_sfc

!   End of routine
    return
  end subroutine read_obs
