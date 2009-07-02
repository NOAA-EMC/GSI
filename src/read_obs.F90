module read_obsmod
!$$$   module documentation block
!                .      .    .                                       .
! module:    read_obsmod extra inquire routine for reading obs
!   prgmmr: parrish          org: np22                date: 2005-06-06
!
! abstract:
!
! program history log:
!   2009-01-05  todling - add gsi_inquire
!
! subroutines included:
!   sub gsi_inquire   -  inquire statement supporting fortran earlier than 2003
!   sub read_obs      -  read, select, reformat obs data
!
! Variable Definitions:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

contains

subroutine gsi_inquire (lbytes,lexist,filename,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    gsi_inquire        inquire file presence and size
!   prgmmr: todling      org: np22                date: 2009-01-05
!
! abstract:  Inquire file presence and size; to be used when fortran
!            2003 not available or non-compliant.
!
! program history log:
!   2009-01-05  todling
!
!   input argument list:
!     mype     - mpi task id
!    filename  - input filename
!
!   output argument list:
!    lexist     - file presence flag
!    lbytes     - file size (bytes)
!
! attributes:
!   language: f90
!   machine:  Linux-cluster
!
!$$$  end documentation block

  use kinds, only: i_kind
  implicit none
  integer(8),intent(out) :: lbytes
  logical,intent(out) :: lexist
  character(len=*),intent(in) :: filename
  integer(i_kind),intent(in) :: mype

  integer(i_kind) :: lenb
  character(len=256) command, fname

#ifdef ibm_sp
  inquire(file=trim(filename),exist=lexist,size=lbytes)
#else
  lenb=0; lbytes = lenb
  inquire(file=trim(filename),exist=lexist)
  if(lexist)then
    write(fname,'(2a,i4.4)') 'fsize_',trim(filename),mype
   write(command,'(4a)') 'wc -c ', trim(filename),' > ', trim(fname)
    call system(command)
    open(unit=999,file=trim(fname),form='formatted')
    read(999,*) lenb
    close(999)
    lbytes=lenb
  endif
#endif
  return
  end subroutine gsi_inquire

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
!   2006-04-20  kistler - moved conv_read to gsisub
!   2006-05-25  treadon - rename goesimg and goes_img and pcp_ssm/i as
!                         pcp_ssmi to make consistent with other obstype
!   2006-09-20  treadon - add mpi_io for select data file (obstype)s
!   2006-10-12  treadon - remove tendsflag check for pcp data (now in gsimain)
!   2007-02-21  sienkiewicz - bring in changes for MLS ozone
!   2007-03-15       su - add reading conventional error table option
!   2007-06-05  treadon - restructure mpi_querybf section to improve efficiency
!   2007-10-03  todling - skip most of this in 4dvar inner loop
!   2008-03-28       wu - move random seed for perturb_obs from setuprhsall
!   2008-04-18  safford - rm unused vars and uses
!   2008-05-01    h.liu - add gome ozone
!   2008-06-20   derber - move destroy_sfc to this routine 
!   2008-09-08   lueken - merged ed's cahnges into q1fy09 code
!   2008-12-30  todling - handle inquire for diff versions of fortran
!   2009-01-05  todling - need tendency alloc in observer mode
!   2009-01-23  todling - echo surface state info 
!   2009-03-18  meunier - add a if statement to read lagrangian data
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
!$$$  end documentation block
    use kinds, only: r_kind,i_kind,i_llong
    use gridmod, only: nlon,nlat,nsig,iglobal,ijn,itotsub,lat1,lon1,&
         ltosi,ltosj,displs_g
    use obsmod, only: iadate,ndat,time_window,dplat,dsfcalc,dfile,dthin, &
           dtype,dval,dmesh,obsfile_all,ref_obs,nprof_gps,dsis,ditype,&
           oberrflg,perturb_obs,lobserver
    use gsi_4dvar, only: l4dvar
    use satthin, only: super_val,super_val1,superp,makegvals,getsfc,destroy_sfc
    use mpimod, only: ierror,mpi_comm_world,mpi_sum,mpi_rtype,mpi_integer,npe,&
         strip,reorder,setcomm
    use constants, only: izero,half
    use converr, only: converr_read
    use guess_grids, only: ges_prsl,ntguessig,destroy_sfc_grids
    use guess_grids, only: create_ges_grids
    use m_gsiBiases, only: create_bias_grids
    use guess_grids, only: ges_prsl,ntguessig
    use radinfo, only: nusis,iuse_rad,jpch_rad,diag_rad
    use ozinfo, only: nusis_oz,iuse_oz,jpch_oz,diag_ozone
    use pcpinfo, only: npcptype,nupcp,iusep,diag_pcp
    use convinfo, only: nconvtype,ioctype,icuse,diag_conv
    use tendsmod, only: create_tendvars
    use jfunc, only: tendsflag,switch_on_derivatives

    implicit none

!   Declare passed variables
    integer(i_kind),intent(in):: mype
    integer(i_kind),dimension(ndat,3),intent(out):: ndata

!   Declare local parameters
    integer(i_llong),parameter:: lenbuf=8388608  ! lenbuf=8*1024*1024

!   Declare local variables
    logical :: lexist,ssmis,amsre,sndr,hirs,avhrr,lexistears,use_prsl_full
    logical :: use_sfc,nuse
    logical,dimension(ndat):: belong
    character(10):: obstype,platid
    character(13):: string,infile
    character(20):: sis
    integer(i_kind) i,j,k,ii,nmind,lunout,isfcalc,ithinx,ithin,nread,npuse,nouse
    integer(i_kind) nprof_gps1,npem1,krsize,len4file
    integer(8) :: lenbytes
!   integer(i_kind) isum
    integer(i_kind):: iworld,iworld_group,next_mype,mm1
    integer(i_kind):: mype_root,ntask_read,mpi_comm_sub_read,lll
    integer(i_kind):: mype_sub_read,minuse
    integer(i_kind):: iworld_group_r1,iworld_r1,iworld_group_r2,iworld_r2
    integer(i_kind),dimension(ndat):: npe_sub,mpi_comm_sub,mype_root_sub
    integer(i_kind),dimension(ndat):: mpi_comm_sub_r1,mpi_comm_sub_r2
    integer(i_kind),dimension(ndat,2):: ntasks1,ntasks
    integer(i_kind),dimension(ndat,3):: ndata1
    integer(i_kind),dimension(npe,ndat):: mype_work,mype_work_r1,mype_work_r2
    integer(i_kind),dimension(npe,ndat):: mype_sub,mype_sub_r1,mype_sub_r2
    integer(i_kind),allocatable,dimension(:):: nrnd

    real(r_kind) gstime,val_dat,rmesh,twind,rseed
    real(r_kind),dimension(lat1*lon1):: prslsm
    real(r_kind),dimension(max(iglobal,itotsub)):: work1
    real(r_kind),allocatable,dimension(:,:,:):: prsl_full

    data lunout / 81 /

!*****************************************************************************

!   Set analysis time and allocate/initialize arrays and variables
    call w3fs21(iadate,nmind)
    gstime=real(nmind,r_kind)

    call makegvals
    do ii=1,ndat
      ndata1(ii,1)=izero
      ndata1(ii,2)=izero
      ndata1(ii,3)=izero
      ntasks1(ii,1) =izero
      ntasks1(ii,2) =izero
    end do
    npem1=npe-1
    nprof_gps1=izero

    if(oberrflg .or. perturb_obs) then
      call converr_read(mype)
    endif

!   Optionally set random seed to perturb observations
    if (perturb_obs) then
       rseed=iadate(4)+iadate(3)*100+iadate(2)*10000+iadate(1)*1000000+mype
       call random_seed(size=krsize)
       allocate(nrnd(krsize))
       do i=1,krsize
          nrnd(i)=rseed
       end do
       call random_seed(put=nrnd)
       deallocate(nrnd)
    endif



!   Set data class and number of reader tasks.  Set logical flag to indicate 
!   type type of GPS data (if present)
    ii=0
    ref_obs = .false.    !.false. = assimilate GPS bending angle
    do i=1,ndat
       obstype=dtype(i)                   !     obstype  - observation types to process
       amsre= index(obstype,'amsre') /= 0
       ssmis= index(obstype,'ssmis') /= 0
       sndr = index(obstype,'sndr') /= 0
       hirs = index(obstype,'hirs') /= 0
       avhrr = index(obstype,'avhrr') /= 0
       if (obstype == 't'  .or. obstype == 'uv' .or. &
           obstype == 'q'  .or. obstype == 'ps' .or. &
           obstype == 'pw' .or. obstype == 'spd'.or. &
           obstype == 'sst'.or. obstype == 'srw'.or. &
           obstype == 'tcp'.or. obstype == "lag".or. &
           obstype == 'dw' .or. obstype == 'rw' ) then
           ditype(i) = 'conv'
       else if( hirs   .or. sndr      .or.  &
               obstype == 'airs'      .or. obstype == 'amsua'     .or.  &
               obstype == 'msu'       .or. obstype == 'iasi'      .or.  &
               obstype == 'amsub'     .or. obstype == 'mhs'       .or.  &
               obstype == 'hsb'       .or. obstype == 'goes_img'  .or.  &
               avhrr .or.  &
               amsre  .or. ssmis      .or. obstype == 'ssmi'      .or.  &
               obstype == 'ssu' ) then
            ditype(i) = 'rad'
       else if (obstype == 'sbuv2' .or. obstype == 'omi' &
           .or. obstype == 'gome'  .or. obstype == 'o3lev') then
            ditype(i) = 'ozone'
       else if (index(obstype,'pcp')/=0 )then
            ditype(i) = 'pcp'
       else if (obstype == 'gps_ref' .or. obstype == 'gps_bnd') then
            ditype(i) = 'gps'
       else
            write(6,*)'READ_OBS:  ***ERROR*** - unknown ob type ',obstype
       end if

!   Set data class and number of reader tasks.  Set logical flag to indicate 
!   type type of GPS data (if present)
       if (index(dtype(i),'gps_ref') /= 0) ref_obs = .true.

!   Check info files to see if data is used.

       nuse=.false.
       minuse=-1
       if(ditype(i) == 'conv')then
         if(diag_conv)minuse=-2
         do j=1,nconvtype
           if(trim(dtype(i)) == trim(ioctype(j)) .and. icuse(j) > minuse)nuse=.true.
         end do
       else if(ditype(i) == 'rad')then
         if(diag_rad)minuse=-2
         do j=1,jpch_rad
          if(trim(dsis(i)) == trim(nusis(j)) .and. iuse_rad(j) > minuse)nuse=.true.
         end do
       else if(ditype(i) == 'ozone')then
         if(diag_ozone)minuse=-2
         if (dtype(i) == 'o3lev') then
           do j=1,nconvtype
             if(trim(dtype(i)) == trim(ioctype(j)) .and. icuse(j) > minuse)nuse=.true.
           end do
         else
           do j=1,jpch_oz
             if(trim(dsis(i)) == trim(nusis_oz(j)) .and. iuse_oz(j) > minuse)nuse=.true.
           end do
         endif
       else if(ditype(i) == 'pcp')then
         if(diag_pcp)minuse=-2
         do j=1,npcptype
          if(trim(dsis(i)) == trim(nupcp(j)) .and. iusep(j) > minuse)nuse=.true.
         end do
       else
         nuse=.true.
       end if
       if(.not. nuse)then
         if(mype == 0)write(6,*) 'data type ',dsis(i), &
               'not used in info file -- do not read file',dfile(i)
       end if


!   Inquire data set to deterimine if input data available and size of dataset
       ii=ii+1
       if (ii>npem1) ii=0
       if(mype==ii)then
          call gsi_inquire(lenbytes,lexist,dfile(i),mype)

!      Initialize number of reader tasks to 1.  For the time being
!      only allow number of reader tasks >= 1 for select obstype.

          if(lexist .and. nuse) then
            ntasks1(i,1)=1
            if(ditype(i) == 'rad' .and. ( .not. obstype=='goes_img' .and. &
                 .not. avhrr)) then

                 len4file=lenbytes/4
                 ntasks1(i,1)=len4file/lenbuf
                 if(ntasks1(i,1)*lenbuf < len4file) ntasks1(i,1)=ntasks1(i,1)+1
            end if
          end if
          if (ditype(i) == 'rad' .and. nuse .and.           &
                    dplat(i) /= 'aqua' .and. (obstype == 'amsua' .or.  &
                    obstype == 'amsub' .or.                          &
                    obstype == 'mhs' )) then
!                   obstype == 'mhs'   .or. hirs )) then

              call gsi_inquire(lenbytes,lexistears,trim(dfile(i))//'ears',mype)

              if(lexistears)then
                len4file=lenbytes/4
                ntasks1(i,2)=len4file/lenbuf
                if(ntasks1(i,2)*lenbuf < len4file) ntasks1(i,2)=ntasks1(i,2)+1
                lexist=lexist .or. lexistears
              end if
          end if
       end if
    end do


!   Distribute optimal number of reader tasks to all mpi tasks
    call mpi_allreduce(ntasks1,ntasks,2*ndat,mpi_integer,mpi_sum,mpi_comm_world,ierror)

    do i=1,ndat
       npe_sub(i)=ntasks(i,1)+ntasks(i,2)
    end do

    if(l4dvar.and.(.not.lobserver)) return

!   Define sub-communicators for each data file
    mm1=mype+1
    belong=.false.
    mype_sub=-999
    mype_sub_r1=-999
    mype_sub_r2=-999
    mype_root=0
    next_mype=0
    mype_root_sub=0
    do i=1,ndat
      if(npe_sub(i) > izero)then
         do k=1,npe_sub(i)
            if(k == 1) mype_root_sub(i)=next_mype
            mype_work(k,i) = next_mype
            mype_sub(mype_work(k,i)+1,i)=k-1
            if(k > ntasks(i,1)) then
                mype_work_r2(k-ntasks(i,1),i) = next_mype
                mype_sub_r2(next_mype+1,i)=k-1-ntasks(i,1)
            else
                mype_work_r1(k,i) = next_mype
                mype_sub_r1(next_mype+1,i)=k-1
            end if
            if(next_mype == mype)belong(i) = .true.
            next_mype = next_mype + 1
            if (next_mype>npem1) next_mype=0
         end do               

         call setcomm(iworld,iworld_group,npe_sub(i),mype_work(1,i),&
                mpi_comm_sub(i),ierror)
         if(ntasks(i,1) > izero)then
            call setcomm(iworld_r1,iworld_group_r1,ntasks(i,1), &
                   mype_work_r1(1,i),mpi_comm_sub_r1(i),ierror)
         end if
         if(ntasks(i,2) > izero)then
            call setcomm(iworld_r2,iworld_group_r2,ntasks(i,2), &
                   mype_work_r2(1,i),mpi_comm_sub_r2(i),ierror)
         end if
      end if

    end do
    do i=1,ndat
       if(mype == 0)write(6,*)'READ_OBS:  read ',i,dtype(i),dsis(i),' using ntasks=',ntasks(i,1),ntasks(i,2),mype_root_sub(i),npe_sub(i) 
    end do


    use_prsl_full=.false.
    use_sfc=.false.
    do i=1,ndat
      if(belong(i) .and. ditype(i) =='conv')then
        obstype=dtype(i)                  
        if(obstype /= 'dw' .and. obstype /= 'rw' .and. obstype /= 'srw')then
          use_prsl_full=.true.
        end if
      else if(belong(i) .and. (ditype(i) == 'rad' .or. ditype(i)=='pcp') .and. &
              mype_root_sub(i)==mype)then
!     else if(belong(i) .and. (ditype(i) == 'rad' .or. ditype(i)=='pcp'))then
         use_sfc=.true.
      end if
    end do
!   Get guess 3d pressure on full grid
    if(use_prsl_full)allocate(prsl_full(nlat,nlon,nsig))
    do k=1,nsig
       call strip(ges_prsl(1,1,k,ntguessig),prslsm,1)
       call mpi_allgatherv(prslsm,ijn(mype+1),mpi_rtype,&
            work1,ijn,displs_g,mpi_rtype,mpi_comm_world,ierror)
       if(use_prsl_full)then
         call reorder(work1,1,1)
         do ii=1,iglobal
            i=ltosi(ii)
            j=ltosj(ii)
            prsl_full(i,j,k)=work1(ii)
         end do
       end if
    end do
!   Create full horizontal surface fields from local fields in guess_grids
    call getsfc(mype,use_sfc)
    if(use_sfc) call prt_guessfc2('sfcges2')
    call destroy_sfc_grids

!   Loop over data files.  Each data file is read by a sub-communicator
    do i=1,ndat

       if (belong(i)) then

          platid=dplat(i)                    !     platid   - satellites to read
          obstype=dtype(i)                   !     obstype  - observation types to process
          infile=dfile(i)                    !     infile   - units from which to read data
          sis=dsis(i)                        !     sensor/instrument/satellite indicator
          val_dat=dval(i)                    !     weighting factors applied to super obs
          ithin=dthin(i)                     !     ithin    - flags to thin data
          ithinx=max(1,abs(ithin))
          rmesh=dmesh(ithinx)                !     rmesh    - thinning mesh sizes (km)
          twind=time_window(i)               !     time window (hours) for input group
          isfcalc=dsfcalc(i)                 !     method to calculate surface fields within fov
          nread=izero
          nouse=izero
          npuse=izero

          if (mype_sub(mm1,i)==mype_root) then
             open(lunout,file=obsfile_all(i),form='unformatted')
             rewind(lunout)
          endif

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

!            Process lagrangian data
              else if (obstype == 'lag') then
                  call read_lag(nread,npuse,nouse,infile,lunout,obstype,&
                  &twind,gstime,sis)
                  string='READ_LAG'

!            Process lidar winds
             else if (obstype == 'dw') then
                call read_lidar(nread,npuse,nouse,infile,obstype,lunout,twind,sis)
                string='READ_LIDAR'

!            Process synthetic tc-mslp obs
             else if (obstype == 'tcp') then
                call read_tcps(nread,npuse,nouse,infile,obstype,lunout, &
                     twind,sis)
                string='READ_TCPS'

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
                if(mype_sub_r1(mm1,i) >=izero)then
                   mype_sub_read = mype_sub_r1(mm1,i)
                   ntask_read=ntasks(i,1)
                   mpi_comm_sub_read=mpi_comm_sub_r1(i)
                   lll=1
                else if(mype_sub_r2(mm1,i) >= izero)then
                   mype_sub_read = mype_sub_r2(mm1,i)
                   ntask_read=ntasks(i,2)
                   mpi_comm_sub_read=mpi_comm_sub_r2(i)
                   lll=2
                else
                  write(6,*)'READ_OBS:  ***ERROR*** in calling sequence ',mype_sub_r1(mm1,i), &
                      mype_sub_r2(mm1,i), mype
                end if
                call read_bufrtovs(mype,val_dat,ithin,isfcalc,rmesh,platid,gstime,&
                     infile,lunout,obstype,nread,npuse,nouse,twind,sis, &
                     mype_root,mype_sub(mm1,i),npe_sub(i),mpi_comm_sub(i), &
                     mype_sub_read,ntask_read,mpi_comm_sub_read,lll)
                string='READ_BUFRTOVS'

!            Process airs data        
             else if(platid == 'aqua' .and. (obstype == 'airs' .or.   &
                  obstype == 'amsua'  .or.  obstype == 'hsb' ))then
                call read_airs(mype,val_dat,ithin,isfcalc,rmesh,platid,gstime,&
                     infile,lunout,obstype,nread,npuse,nouse,twind,sis,&
                     mype_root,mype_sub(mm1,i),npe_sub(i),mpi_comm_sub(i))
                string='READ_AIRS'

!            Process iasi data
             else if(obstype == 'iasi')then
                 call read_iasi(mype,val_dat,ithin,rmesh,platid,gstime,&
                      infile,lunout,obstype,nread,npuse,nouse,twind,sis,&
                      mype_root,mype_sub(mm1,i),npe_sub(i),mpi_comm_sub(i))
                  string='READ_IASI'

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
             else if (obstype == 'ssmis'     .or. &
                      obstype == 'ssmis_las' .or. obstype == 'ssmis_uas' .or. &
                      obstype == 'ssmis_img' .or. obstype == 'ssmis_env' ) then
                call read_ssmis(mype,val_dat,ithin,isfcalc,rmesh,platid,gstime,&
                     infile,lunout,obstype,nread,npuse,nouse,twind,sis,&
                     mype_root,mype_sub(mm1,i),npe_sub(i),mpi_comm_sub(i))
                string='READ_SSMIS'

!            Process GOES IMAGER RADIANCE  data
             else if(obstype == 'goes_img') then
                call read_goesimg(mype,val_dat,ithin,rmesh,platid,gstime,&
                     infile,lunout,obstype,nread,npuse,nouse,twind,sis, &
                     mype_root,mype_sub(mm1,i),npe_sub(i),mpi_comm_sub(i))
                string='READ_GOESMIMG'

!            Process NAVY AVHRR RADIANCE  data
             else if(obstype == 'avhrr_navy') then
                call read_avhrr_navy(mype,val_dat,ithin,rmesh,platid,gstime,&
                     infile,lunout,obstype,nread,npuse,nouse,twind,sis, &
                     mype_root,mype_sub(mm1,i),npe_sub(i),mpi_comm_sub(i))
                string='READ_AVH_NAVY'

!            Process NESDIS AVHRR RADIANCE  data
             else if(obstype == 'avhrr') then
                call read_avhrr(mype,val_dat,ithin,rmesh,platid,gstime,&
                     infile,lunout,obstype,nread,npuse,nouse,twind,sis, &
                     mype_root,mype_sub(mm1,i),npe_sub(i),mpi_comm_sub(i))
                string='READ_AVHRR'
             end if

!         Process ozone data
          else if (ditype(i) == 'ozone')then
             call read_ozone(nread,npuse,nouse,&
                  platid,infile,gstime,lunout,obstype,twind,sis,ithin,rmesh)
             string='READ_OZONE'

!         Process precipitation             
          else if (ditype(i) == 'pcp')then
             call read_pcp(nread,npuse,nouse,mype,platid,gstime,infile, &
                  lunout,obstype,twind,sis)
             string='READ_PCP'

!         Process gps observations
          else if (ditype(i) == 'gps')then
             call read_gps(nread,npuse,nouse,infile,lunout,obstype,twind, &
                  nprof_gps1,sis)
             string='READ_GPS'
             
          end if

!         Close unit to data file

!         Accumulate data counts on "root" task
          if (mype_sub(mm1,i)==mype_root) then
             close(lunout)
             ndata1(i,1)=ndata1(i,1)+npuse
             ndata1(i,2)=ndata1(i,2)+nread
             ndata1(i,3)=ndata1(i,3)+nouse

             write(6,8000) adjustl(string),infile,obstype,sis,nread,ithin,&
                  rmesh,isfcalc,nouse,npe_sub(i)
8000         format(a13,': file=',a10,&
                  ' type=',a10,  ' sis=',a20,  ' nread=',i10,&
                  ' ithin=',i2, ' rmesh=',f7.3,' isfcalc=',i2,&
                  ' ndata=',i10,' ntask=',i3)

          endif
       endif

    end do
    if(use_prsl_full)deallocate(prsl_full)

!   Deallocate arrays containing full horizontal surface fields
    call destroy_sfc

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


!   End of routine
    return
  end subroutine read_obs


end module read_obsmod
