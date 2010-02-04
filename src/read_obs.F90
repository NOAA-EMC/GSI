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

! set default to private
  private
! set subroutines to public
  public :: gsi_inquire
  public :: read_obs

contains

subroutine gsi_inquire (lbytes,lexist,filename,mype,jsatid,dtype,minuse)
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

  use kinds, only: i_kind,i_llong,r_kind,r_double
  use constants, only: izero
  use gsi_4dvar, only: iadatebgn,iadateend
  use obsmod, only: offtime_data
  use convinfo, only: nconvtype,ictype,ioctype,icuse

  implicit none

  integer(i_llong),intent(  out) :: lbytes
  logical         ,intent(  out) :: lexist
  character(len=*),intent(in   ) :: filename
  character(len=*),intent(in   ) :: jsatid
  character(len=*),intent(in   ) :: dtype
  integer(i_kind) ,intent(in   ) :: mype,minuse

  logical :: lhere
  integer(i_kind) :: lenb,lnbufr,idate,idate2,iret,kidsat
  integer(i_kind) :: ireadsb,ireadmg,kx,nc
  real(r_double) :: satid,type
  character(len=256) command, fname
  character(8) subset
  

#ifdef ibm_sp
  inquire(file=trim(filename),exist=lhere,size=lbytes)
  lexist = lhere .and. lbytes>0_i_llong
#else
  lenb=izero; lbytes = lenb
  inquire(file=trim(filename),exist=lhere)
  if(lhere)then
    write(fname,'(2a,i4.4)') 'fsize_',trim(filename),mype
    write(command,'(4a)') 'wc -c ', trim(filename),' > ', trim(fname)
    call system(command)
    open(unit=999,file=trim(fname),form='formatted')
    read(999,*) lenb
    close(999)
    lbytes=lenb
  endif
  lexist = lhere .and. lbytes>0_i_llong
#endif
  idate=0
  if(lexist)then
      lnbufr = 15_i_kind
      open(lnbufr,file=trim(filename),form='unformatted',status = 'old')
      call openbf(lnbufr,'IN',lnbufr)
      call datelen(10)
      call readmg(lnbufr,subset,idate,iret)

!     Extract date and check for consistency with analysis date
      if (idate<iadatebgn.or.idate>iadateend) then
         if(offtime_data) then
           write(6,*)'***gsi_inquire analysis and data file date differ, but use anyway'
         else
            write(6,*)'***gsi_inquire*** ',&
              'incompatable analysis and observation date/time'
         end if
         write(6,*)'Analysis start  :',iadatebgn
         write(6,*)'Analysis end    :',iadateend
         write(6,*)'Observation time:',idate
         if(.not.offtime_data) lexist=.false.
      endif
      kidsat=0
      if(jsatid == 'metop-a')kidsat=4_i_kind
      if(jsatid == 'metop-b')kidsat=5_i_kind
      if(jsatid == 'metop-c')kidsat=6_i_kind
      if(jsatid == 'n08')kidsat=200_i_kind
      if(jsatid == 'n09')kidsat=201_i_kind
      if(jsatid == 'n10')kidsat=202_i_kind
      if(jsatid == 'n11')kidsat=203_i_kind
      if(jsatid == 'n12')kidsat=204_i_kind
      if(jsatid == 'n14')kidsat=205_i_kind
      if(jsatid == 'n15')kidsat=206_i_kind
      if(jsatid == 'n16')kidsat=207_i_kind
      if(jsatid == 'n17')kidsat=208_i_kind
      if(jsatid == 'n18')kidsat=209_i_kind
      if(jsatid == 'n19')kidsat=223_i_kind
      if(jsatid == 'f08')kidsat=241_i_kind
      if(jsatid == 'f10')kidsat=243_i_kind
      if(jsatid == 'f11')kidsat=244_i_kind
      if(jsatid == 'f13')kidsat=246_i_kind
      if(jsatid == 'f14')kidsat=247_i_kind
      if(jsatid == 'f15')kidsat=248_i_kind
      if(jsatid == 'f16')kidsat=249_i_kind
      if(jsatid == 'f17')kidsat=250_i_kind
      if(jsatid == 'g08' .or. jsatid == 'g08_prep')kidsat=252_i_kind
      if(jsatid == 'g09' .or. jsatid == 'g09_prep')kidsat=253_i_kind
      if(jsatid == 'g10' .or. jsatid == 'g10_prep')kidsat=254_i_kind
      if(jsatid == 'g11' .or. jsatid == 'g11_prep')kidsat=255_i_kind
      if(jsatid == 'g12' .or. jsatid == 'g12_prep')kidsat=256_i_kind
      if(jsatid == 'g13' .or. jsatid == 'g13_prep')kidsat=257_i_kind
      if(jsatid == 'n05')kidsat=705_i_kind
      if(jsatid == 'n06')kidsat=706_i_kind
      if(jsatid == 'n07')kidsat=707_i_kind
      if(jsatid == 'tirosn')kidsat=708_i_kind

      if(lexist)then
       if(kidsat /= 0)then
        lexist=.false.
        do while(ireadmg(lnbufr,subset,idate2) >= izero)
           if(ireadsb(lnbufr)==izero)then
              call ufbint(lnbufr,satid,1,1,iret,'SAID')
           end if
           if(nint(satid) == kidsat) then
             lexist=.true.
             exit
           end if
        end do
       else if(trim(filename) == 'prepbufr')then
         lexist = .false.
         fileloop: do while(ireadmg(lnbufr,subset,idate2) >= izero)
          do while(ireadsb(lnbufr)>=izero)
           call ufbint(lnbufr,type,1,1,iret,'TYP')
           kx=nint(type)
           do nc=1,nconvtype
             if(trim(ioctype(nc)) == trim(dtype) .and. kx == ictype(nc) .and. icuse(nc) > minuse)then
               lexist = .true.
               exit fileloop
             end if
           end do
          end do 
         end do fileloop
       end if
      end if

      call closbf(lnbufr)
  end if
  if(lexist)then
      write(6,*)'gsi_inquire: bufr file date is ',idate,trim(filename),' ',dtype
  else
      write(6,*)'gsi_inquire: bufr file date is ',idate,trim(filename),' ',dtype,' not used '
  end if
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
!   2009-12-20  gayno - modify argument lists so that fov-based surface
!                       calculation may be used.
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
    use constants, only: izero,ione,one,zero
    use converr, only: converr_read
    use guess_grids, only: ges_prsl,ntguessig,destroy_sfc_grids
    use radinfo, only: nusis,iuse_rad,jpch_rad,diag_rad
    use ozinfo, only: nusis_oz,iuse_oz,jpch_oz,diag_ozone
    use pcpinfo, only: npcptype,nupcp,iusep,diag_pcp
    use convinfo, only: nconvtype,ioctype,icuse,diag_conv

    implicit none

!   Declare passed variables
    integer(i_kind)                  ,intent(in   ) :: mype
    integer(i_kind),dimension(ndat,3),intent(  out) :: ndata

!   Declare local parameters
    integer(i_llong),parameter:: lenbuf=8388608_i_llong  ! lenbuf=8*1024*1024

!   Declare local variables
    logical :: lexist,ssmis,amsre,sndr,hirs,avhrr,lexistears,use_prsl_full
    logical :: use_sfc,nuse
    logical,dimension(ndat):: belong
    character(10):: obstype,platid
    character(13):: string,infile
    character(20):: sis
    integer(i_kind) i,j,k,ii,nmind,lunout,isfcalc,ithinx,ithin,nread,npuse,nouse
    integer(i_kind) nprof_gps1,npem1,krsize,len4file,npemax,ilarge,nlarge,npestart
    integer(i_llong) :: lenbytes
    integer(i_kind):: npetot,npeextra,mmdat
    integer(i_kind):: iworld,iworld_group,next_mype,mm1,iix
    integer(i_kind):: mype_root,ntask_read,mpi_comm_sub_read,lll,llb
    integer(i_kind):: mype_sub_read,minuse
    integer(i_kind):: iworld_group_r1,iworld_r1,iworld_group_r2,iworld_r2
    integer(i_kind),dimension(ndat):: npe_sub,npe_sub3,mpi_comm_sub,mype_root_sub,npe_order
    integer(i_kind),dimension(ndat):: mpi_comm_sub_r1,mpi_comm_sub_r2
    integer(i_kind),dimension(ndat):: ntasks1,ntasks
    integer(i_kind),dimension(ndat,3):: ndata1
    integer(i_kind),dimension(npe,ndat):: mype_work,mype_work_r1,mype_work_r2
    integer(i_kind),dimension(npe,ndat):: mype_sub,mype_sub_r1,mype_sub_r2
    integer(i_kind),allocatable,dimension(:):: nrnd

    real(r_kind) gstime,val_dat,rmesh,twind,rseed
    real(r_kind),dimension(lat1*lon1):: prslsm
    real(r_kind),dimension(max(iglobal,itotsub)):: work1
    real(r_kind),allocatable,dimension(:,:,:):: prsl_full

    data lunout / 81_i_kind /

!*****************************************************************************

!   Set analysis time and allocate/initialize arrays and variables
    call w3fs21(iadate,nmind)
    gstime=real(nmind,r_kind)

    call makegvals
    do ii=1,ndat
       ndata1(ii,1)=izero
       ndata1(ii,2)=izero
       ndata1(ii,3)=izero
       ntasks1(ii) =izero
    end do
    npem1=npe-ione
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
    ii=izero
    ref_obs = .false.    !.false. = assimilate GPS bending angle
    do i=1,ndat
       obstype=dtype(i)                   !     obstype  - observation types to process
       amsre= index(obstype,'amsre') /= izero
       ssmis= index(obstype,'ssmis') /= izero
       sndr = index(obstype,'sndr') /= izero
       hirs = index(obstype,'hirs') /= izero
       avhrr = index(obstype,'avhrr') /= izero
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
       else if (index(obstype,'pcp')/=izero )then
          ditype(i) = 'pcp'
       else if (obstype == 'gps_ref' .or. obstype == 'gps_bnd') then
          ditype(i) = 'gps'
       else
          write(6,*)'READ_OBS:  ***ERROR*** - unknown ob type ',obstype
       end if

!   Set data class and number of reader tasks.  Set logical flag to indicate 
!   type type of GPS data (if present)
       if (index(dtype(i),'gps_ref') /= izero) ref_obs = .true.

!   Check info files to see if data is used.

       nuse=.false.
       minuse=-ione
       if(ditype(i) == 'conv')then
          if(diag_conv)minuse=-2_i_kind
          do j=1,nconvtype
             if(trim(dtype(i)) == trim(ioctype(j)) .and. icuse(j) > minuse)nuse=.true.
          end do
       else if(ditype(i) == 'rad')then
          if(diag_rad)minuse=-2_i_kind
          do j=1,jpch_rad
             if(trim(dsis(i)) == trim(nusis(j)) .and. iuse_rad(j) > minuse)nuse=.true.
          end do
       else if(ditype(i) == 'ozone')then
          if(diag_ozone)minuse=-2_i_kind
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
          if(diag_pcp)minuse=-2_i_kind
             do j=1,npcptype
                if(trim(dsis(i)) == trim(nupcp(j)) .and. iusep(j) > minuse)nuse=.true.
             end do
          else
             nuse=.true.
          end if
       if(.not. nuse)then
          if(mype == izero)write(6,*) 'data type ',dsis(i), &
                'not used in info file -- do not read file',dfile(i)
       end if


!   Inquire data set to deterimine if input data available and size of dataset
       ii=ii+ione
       if (ii>npem1) ii=izero
       if(mype==ii)then
          call gsi_inquire(lenbytes,lexist,dfile(i),mype,dplat(i),dtype(i),minuse)

          len4file=lenbytes/4
          if (ditype(i) == 'rad' .and. nuse .and.           &
                    dplat(i) /= 'aqua' .and. dplat(i) /= 'metop-a' .and. &
                   (obstype == 'amsua' .or.  obstype == 'amsub' .or.     &
                    obstype == 'mhs' )) then
!                   obstype == 'mhs'   .or. hirs )) then

             call gsi_inquire(lenbytes,lexistears,trim(dfile(i))//'ears',mype,dplat(i),dtype(i),minuse)

             lexist=lexist .or. lexistears
             len4file=len4file+lenbytes/4
          end if
!      Initialize number of reader tasks to 1.  For the time being
!      only allow number of reader tasks >= 1 for select obstype.

          if(lexist .and. nuse) then
             ntasks1(i)=ione
             if(ditype(i) == 'rad' .and. dthin(i) > zero) then

!  Allow up to 16 processors/file increase loop bounds to increase number of processors allowed
                do j=1,4
                   if(len4file < lenbuf)exit
                   ntasks1(i)=2*ntasks1(i)
                   len4file=len4file/2
                end do
!               if(ntasks1(i)*lenbuf < len4file) ntasks1(i)=ntasks1(i)+ione
             end if
          end if
       end if
    end do


!   Distribute optimal number of reader tasks to all mpi tasks
    call mpi_allreduce(ntasks1,ntasks,ndat,mpi_integer,mpi_sum,mpi_comm_world,ierror)

    npemax=izero
    npetot=izero
    do i=1,ndat
       npe_sub(i)=ntasks(i)
       npetot=npetot+npe_sub(i)
       npemax=max(npemax,npe_sub(i))
    end do

    if(l4dvar.and.(.not.lobserver)) return
    
    npeextra=npe-mod(npetot,npe)
    if(npeextra > izero)then
       if(mype == izero)write(6,*) ' number of extra processors ',npeextra
       npe_sub3=npe_sub
       extraloop: do j=1,npeextra
          iix=1
          do ii=1,5
             do i=1,ndat
                if(iix == npe_sub3(i) .and. ditype(i) == 'rad' .and. dthin(i) > zero)then
                   if(ntasks(i) > izero .and. ntasks(i) <= npeextra)then
                      npeextra=npeextra-ntasks(i)
                      npe_sub(i)=npe_sub(i)+ntasks(i)
                      ntasks(i)=2*ntasks(i)
                      if(npeextra < iix)cycle extraloop
                   end if
                end if
             end do
          end do
          iix=max(min(2*iix,8),npeextra)
       end do extraloop
    end if

!   Set up locations of first processor

    ilarge=izero
    npestart=izero
    npe_sub3=npe_sub
    mype_root_sub=izero
    mmdat=izero
    loopx: do j=1,ndat
       nlarge=izero
       do i=1,ndat
          if(npe_sub3(i) > nlarge .and. npe_sub3(i)+npestart <= npe)then
             ilarge=i
             nlarge=npe_sub3(i)
          end if
       end do
       if(nlarge == izero)exit loopx
       npe_order(j)=ilarge
       mype_root_sub(ilarge)=npestart
       npestart=npestart+npe_sub3(ilarge)
       mmdat=mmdat+ione
       if(npestart == npe)npestart=izero
       npe_sub3(ilarge)=izero
    end do loopx

        
!   Define sub-communicators for each data file
    mm1=mype+ione
    belong=.false.
    mype_sub=-999_i_kind
    mype_sub_r1=-999_i_kind
    mype_sub_r2=-999_i_kind
    mype_root=izero
    next_mype=izero
    do ii=1,mmdat
       i=npe_order(ii)
       if(npe_sub(i) > izero)then
          next_mype=mype_root_sub(i)
          do k=1,npe_sub(i)
             mype_work(k,i) = next_mype
             mype_sub(mype_work(k,i)+ione,i)=k-ione
             if(next_mype == mype)belong(i) = .true.
             next_mype = next_mype + ione
             if (next_mype>npem1) next_mype=izero
          end do               

          call setcomm(iworld,iworld_group,npe_sub(i),mype_work(1,i),&
                 mpi_comm_sub(i),ierror)
       end if

    end do
    do ii=1,mmdat
       i=npe_order(ii)
       if(mype == izero .and. npe_sub(i) > izero)write(6,*)'READ_OBS:  read ',i,dtype(i),dsis(i),' using ntasks=',ntasks(i),mype_root_sub(i),npe_sub(i) 
    end do


    use_prsl_full=.false.
    use_sfc=.false.
    do i=1,ndat
       if(belong(i) .and. ditype(i) =='conv')then
          obstype=dtype(i)                  
          if(obstype /= 'dw' .and. obstype /= 'rw' .and. obstype /= 'srw')then
             use_prsl_full=.true.
          end if
       else if(belong(i) .and. (ditype(i) == 'rad' .or. ditype(i)=='pcp'))then
          use_sfc=.true.
       end if
    end do
!   Get guess 3d pressure on full grid
    if(use_prsl_full)allocate(prsl_full(nlat,nlon,nsig))
    do k=1,nsig
       call strip(ges_prsl(1,1,k,ntguessig),prslsm,ione)
       call mpi_allgatherv(prslsm,ijn(mype+ione),mpi_rtype,&
            work1,ijn,displs_g,mpi_rtype,mpi_comm_world,ierror)
       if(use_prsl_full)then
          call reorder(work1,ione,ione)
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
    do ii=1,mmdat

       i=npe_order(ii)
       if (i > izero .and. belong(i)) then

          platid=dplat(i)                    !     platid   - satellites to read
          obstype=dtype(i)                   !     obstype  - observation types to process
          infile=dfile(i)                    !     infile   - units from which to read data
          sis=dsis(i)                        !     sensor/instrument/satellite indicator
          val_dat=dval(i)                    !     weighting factors applied to super obs
          ithin=dthin(i)                     !     ithin    - flags to thin data
          ithinx=max(ione,abs(ithin))
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
                call read_tcps(nread,npuse,nouse,infile,obstype,lunout,sis)
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
                llb=1
                lll=1
                if((obstype == 'amsua' .or. obstype == 'amsub' .or. obstype == 'mhs') .and. &
                   (platid /= 'metop-a' .or. platid /='metop-b' .or. platid /= 'metop-c'))lll=2
                call read_bufrtovs(mype,val_dat,ithin,isfcalc,rmesh,platid,gstime,&
                     infile,lunout,obstype,nread,npuse,nouse,twind,sis, &
                     mype_root,mype_sub(mm1,i),npe_sub(i),mpi_comm_sub(i),llb,lll)
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
                call read_iasi(mype,val_dat,ithin,isfcalc,rmesh,platid,gstime,&
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
                call read_amsre(mype,val_dat,ithin,isfcalc,rmesh,gstime,&
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
             call read_pcp(nread,npuse,nouse,gstime,infile, &
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
    super_val1(0)=one
    deallocate(super_val)

!   Collect number of gps profiles (needed later for qc)
    call mpi_allreduce(nprof_gps1,nprof_gps,ione,mpi_integer,mpi_sum,mpi_comm_world,ierror)


!   End of routine
    return
end subroutine read_obs


end module read_obsmod
