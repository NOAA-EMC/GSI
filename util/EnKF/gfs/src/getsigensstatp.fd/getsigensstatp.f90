!< --- next few lines under version control, D O  N O T  E D I T --->
! $Date: 2017-06-16 14:38:14 -0400 (Fri, 16 Jun 2017) $
! $Revision: 94539 $
! $Author: Michael.Lueken@noaa.gov $
! $Id: getsigensstatp.f90 94539 2017-06-16 18:38:14Z Michael.Lueken@noaa.gov $
!<------------------------------------------------------------------>
program getsigensstatp
!$$$  main program documentation block
!
! program:  getsigensstatp           compute ensemble mean and spread
!
! prgmmr: mahajan          org: emc/ncep               date: 2014-10-01
!
! abstract:  compute ensemble mean and spread from NCEP GFS spectral or nemsio binary files.
!
! program history log:
!   2014-08-23  Initial version.
!   2018-07-21  Add hydrometeor (optional) 
!
! usage:
!   input files:
!
!   output files:
!
! attributes:
!   language: f95
!
!$$$

    use netcdf
    use mpi
    use sigio_module,  only: sigio_head,sigio_data,sigio_srohdc, &
                             sigio_axdata,sigio_sclose
    use nemsio_module, only: nemsio_init,nemsio_open,nemsio_close, &
                             nemsio_gfile,nemsio_getfilehead,nemsio_charkind8, &
                             nemsio_readrec,nemsio_readrecv
    use module_fv3gfs_ncio, only: open_dataset, create_dataset, read_attribute, &
                             Dataset, Dimension, close_dataset, &
                             read_vardata, write_attribute, write_vardata, &
                             get_dim, quantize_data


    implicit none

    integer,parameter :: r_single=4,r_double=8
    integer,parameter :: iunit=21
    integer,parameter :: idrt=4
    character(nemsio_charkind8) :: dtype
    character(len=3)   :: charnanal
    character(len=500) :: filenamein,datapath,filepref
    integer :: nanals,nlevs,ntrac,ntrunc,latb,lonb,iret
    integer :: k,krecu,krecv,krect,krecq,krecoz,kreccwmr,krecicmr
    integer :: krecsnmr,krecrwmr,krecgrle
    integer :: nsize,npts,nrec,nflds
    real(r_double) :: rnanals,rnanalsm1
    character(len=16),allocatable,dimension(:) :: recnam
    integer :: mype,mype1,npe,orig_group,new_group,new_comm
    integer,dimension(:),allocatable :: new_group_members,reclev
    real(r_single),allocatable,dimension(:,:) :: rwork_mem,rwork_avg
    real(r_single),allocatable,dimension(:) :: glats,gwts
    real(4), allocatable, dimension(:,:,:) :: values_3d
    real(4), allocatable, dimension(:,:) :: values_2d
    logical :: sigio,nemsio,ncio
!   logical :: do_icmr = .false.
    logical :: do_hydro = .false.

    type(sigio_head)   :: sigheadi
    type(sigio_data)   :: sigdatai
    type(nemsio_gfile) :: gfile
    type(Dataset) :: dset
    type(Dimension) :: londim,latdim,levdim

    ! Initialize mpi, mype is process number, npe is total number of processes.
    call mpi_init(iret)
    call MPI_Comm_rank(MPI_COMM_WORLD,mype,iret)
    call MPI_Comm_size(MPI_COMM_WORLD,npe,iret)
    mype1 = mype + 1

    if ( mype == 0 ) call w3tagb('GETSIGENSSTATP',2014,1025,0055,'NP25')

    ! Get user input from command line
    call getarg(1,datapath)
    call getarg(2,filepref)
    call getarg(3,charnanal)
    if ( iargc() < 3 ) then
        write(6,'(a)') "USAGE: ./getsigensstatp.x datapath filepref nanals"
        call mpi_abort(mpi_comm_world,99,iret)
        stop
    endif
    read(charnanal,'(i3)') nanals
    rnanals=nanals
    rnanals=1.0_r_double/rnanals
    rnanalsm1=nanals-1.0_r_double
    rnanalsm1=1.0_r_double/rnanalsm1

    if ( mype == 0 ) then
        write(6,'(1a)')'Command line input'
        write(6,'(2a)')' datapath      = ',trim(datapath)
        write(6,'(2a)')' fileprefix    = ',trim(filepref)
        write(6,'(2a)')' nanals        = ',trim(charnanal)
        write(6,'(3a)')' emean fileout = ',trim(adjustl(filepref))//'_ensmean.nc4'
        write(6,'(3a)')' esprd fileout = ',trim(adjustl(filepref))//'_ensspread.nc4'
        write(6,'(1a)')' '
    endif

    if ( npe < nanals ) then
        write(6,'(2(a,i4))')'***ERROR***  npe too small.  npe = ',npe,' < nanals = ',nanals
        call mpi_abort(mpi_comm_world,99,iret)
        stop
    end if

! Create sub-communicator to handle number of cases (nanals)
    call mpi_comm_group(mpi_comm_world,orig_group,iret)

    allocate(new_group_members(nanals))
    do k=1,nanals
        new_group_members(k)=k-1
    enddo

    call mpi_group_incl(orig_group,nanals,new_group_members,new_group,iret)
    call mpi_comm_create(mpi_comm_world,new_group,new_comm,iret)
    if ( iret /= 0 ) then
        write(6,'(a,i5)')'***ERROR*** after mpi_comm_create with iret = ',iret
        call mpi_abort(mpi_comm_world,99,iret)
        stop
    endif

    nemsio = .false.
    sigio  = .false.
    ncio = .false.

! Process input files (one file per task)
    if ( mype1 <= nanals ) then

        write(charnanal,'(i3.3)') mype1
        filenamein = trim(adjustl(datapath)) // trim(adjustl(filepref)) // '_mem' // charnanal

        dset = open_dataset(filenamein,errcode=iret)
        if (iret == 0) then
           ncio = .true.
        else
           ncio = .false.
        end if
        if (.not. ncio) then
            call nemsio_init(iret=iret)
            call nemsio_open(gfile,trim(filenamein),'READ',iret=iret)
            if (iret == 0) then
               nemsio = .true.
            else
               nemsio = .false.
            endif
        endif
        if (.not. ncio .and. .not. nemsio) then
            call sigio_srohdc(iunit,trim(filenamein),sigheadi,sigdatai,iret)
            if (iret == 0) then
               sigio = .true.
            else
               sigio = .false.
            endif
        endif

        if ( .not. ncio .and. .not. nemsio .and. .not. sigio ) then
           write(6,'(3a)')'***ERROR*** ',trim(filenamein),' contains unrecognized file format. ABORT!'
           call mpi_abort(mpi_comm_world,99,iret)
           stop
        end if

!    Read each ensemble member
        ! Read each ensemble member
        if (ncio) then
           if (mype == 0) write(6,*) 'Read netcdf ',trim(filenamein)
           londim = get_dim(dset,'grid_xt'); lonb = londim%len
           latdim = get_dim(dset,'grid_yt'); latb = latdim%len
           levdim = get_dim(dset,'pfull');   nlevs = levdim%len
           call read_attribute(dset, 'ncnsto', ntrac)
           ntrunc = latb-2
        end if
        if (sigio) then
            write(6,'(3a,i5)')'Read sigio ',trim(filenamein),' iret = ',iret
            ntrunc  = sigheadi%jcap
            ntrac   = sigheadi%ntrac
            nlevs   = sigheadi%levs
            latb    = sigheadi%latf
            lonb    = sigheadi%lonf
        end if
        if (nemsio) then
           call nemsio_getfilehead(gfile, nrec=nrec, jcap=ntrunc, &
              dimx=lonb, dimy=latb, dimz=nlevs, ntrac=ntrac, gdatatype=dtype, iret=iret)
           write(6,'(5a,i5)')'Read nemsio ',trim(filenamein), ' dtype = ', trim(adjustl(dtype)),' iret = ',iret
           if ( ntrunc < 0 ) ntrunc = latb - 2
           allocate(reclev(nrec),recnam(nrec))
           call nemsio_getfilehead(gfile,reclev=reclev,iret=iret)
           call nemsio_getfilehead(gfile,recname=recnam,iret=iret)
           !do_icmr = variable_exist('icmr')
           do_hydro = .false.  ! set to false to keep the file size small
        endif

        if ( mype == 0 ) then
            write(6,'(a)')   ' '
            write(6,'(2(a,l1))')'Computing ensemble mean and spread with nemsio = ',nemsio,' , sigio = ',sigio, ' , ncio = ',ncio
            write(6,'(a)')   ' '
        endif

        if ( mype == 0 ) then
            write(6,'(a)')   ' '
            write(6,'(2a)')  'Read header information from ',trim(filenamein)
            write(6,'(a,i5)')' ntrunc  = ',ntrunc
            write(6,'(a,i5)')' ntrac   = ',ntrac
            write(6,'(a,i5)')' nlevs   = ',nlevs
            write(6,'(a,i5)')' lonb    = ',lonb
            write(6,'(a,i5)')' latb    = ',latb
            write(6,'(a)')   ' '
        endif

        npts  = latb*lonb
        nflds = 1 + 6*nlevs
        if (do_hydro) nflds = 1 + 10*nlevs
        nsize = npts*nflds

        if ( mype == 0 ) then
            allocate(glats(latb),gwts(latb))
            call splat(idrt,latb,glats,gwts)
            glats = 180.0_r_single / acos(-1.0_r_single) * asin(glats(latb:1:-1))
            deallocate(gwts)
        endif

        allocate(rwork_mem(npts,nflds))
        allocate(rwork_avg(npts,nflds))

        rwork_mem = 0.0_r_single
        rwork_avg = 0.0_r_single

        if ( sigio ) then

            call sptez(0,ntrunc,idrt,lonb,latb,sigdatai%ps,rwork_mem(:,1),1)
            do k = 1,nlevs
                krecu    = 1 + 0*nlevs + k
                krecv    = 1 + 1*nlevs + k
                krect    = 1 + 2*nlevs + k
                krecq    = 1 + 3*nlevs + k
                krecoz   = 1 + 4*nlevs + k
                kreccwmr = 1 + 5*nlevs + k
                call sptezv(0,ntrunc,idrt,lonb,latb,&
                            sigdatai%d(:,k),sigdatai%z(:,k),&
                            rwork_mem(:,krecu),rwork_mem(:,krecv),1)
                call sptez(0,ntrunc,idrt,lonb,latb,sigdatai%t(:,k  ),rwork_mem(:,krect   ),1)
                call sptez(0,ntrunc,idrt,lonb,latb,sigdatai%q(:,k,1),rwork_mem(:,krecq   ),1)
                call sptez(0,ntrunc,idrt,lonb,latb,sigdatai%q(:,k,2),rwork_mem(:,krecoz  ),1)
                call sptez(0,ntrunc,idrt,lonb,latb,sigdatai%q(:,k,3),rwork_mem(:,kreccwmr),1)
            enddo

            call sigio_axdata(sigdatai,iret)

        elseif ( nemsio ) then

            call nemsio_readrecv(gfile,'pres','sfc',1,rwork_mem(:,1),iret=iret)
            do k = 1,nlevs
                krecu    = 1 + 0*nlevs + k
                krecv    = 1 + 1*nlevs + k
                krect    = 1 + 2*nlevs + k
                krecq    = 1 + 3*nlevs + k
                krecoz   = 1 + 4*nlevs + k
                kreccwmr = 1 + 5*nlevs + k
              ! if ( do_icmr ) krecicmr = 1 + 6*nlevs + k
                if ( do_hydro ) then
                   krecicmr = 1 + 6*nlevs + k
                   krecrwmr = 1 + 7*nlevs + k
                   krecsnmr = 1 + 8*nlevs + k
                   krecgrle = 1 + 9*nlevs + k
                endif
                call nemsio_readrecv(gfile,'ugrd', 'mid layer',k,rwork_mem(:,krecu),   iret=iret)
                call nemsio_readrecv(gfile,'vgrd', 'mid layer',k,rwork_mem(:,krecv),   iret=iret)
                call nemsio_readrecv(gfile,'tmp',  'mid layer',k,rwork_mem(:,krect),   iret=iret)
                call nemsio_readrecv(gfile,'spfh', 'mid layer',k,rwork_mem(:,krecq),   iret=iret)
                call nemsio_readrecv(gfile,'o3mr', 'mid layer',k,rwork_mem(:,krecoz),  iret=iret)
                call nemsio_readrecv(gfile,'clwmr','mid layer',k,rwork_mem(:,kreccwmr),iret=iret)
              ! if ( do_icmr ) call nemsio_readrecv(gfile,'icmr', 'mid layer',k,rwork_mem(:,krecicmr),iret=iret) 
                if ( do_hydro ) then
                   call nemsio_readrecv(gfile,'icmr', 'mid layer',k,rwork_mem(:,krecicmr),   iret=iret)
                   call nemsio_readrecv(gfile,'rwmr', 'mid layer',k,rwork_mem(:,krecrwmr),   iret=iret)
                   call nemsio_readrecv(gfile,'snmr', 'mid layer',k,rwork_mem(:,krecsnmr),   iret=iret)
                   call nemsio_readrecv(gfile,'grle', 'mid layer',k,rwork_mem(:,krecgrle),   iret=iret)
                endif

            enddo
            call nemsio_close(gfile,iret=iret)

        elseif ( ncio ) then
           call read_vardata(dset,'pressfc',values_2d) 
           rwork_mem(:,1) = reshape(values_2d,(/npts/))
           deallocate(values_2d)
           call read_vardata(dset,'ugrd',values_3d)
           do k = 1,nlevs
              krecu    = 1 + 0*nlevs + k
              rwork_mem(:,krecu) = reshape(values_3d(:,:,k),(/npts/))
           end do
           call read_vardata(dset,'vgrd',values_3d)
           do k = 1,nlevs
              krecv    = 1 + 1*nlevs + k
              rwork_mem(:,krecv) = reshape(values_3d(:,:,k),(/npts/))
           end do
           call read_vardata(dset,'tmp',values_3d)
           do k = 1,nlevs
              krect    = 1 + 2*nlevs + k
              rwork_mem(:,krect) = reshape(values_3d(:,:,k),(/npts/))
           end do
           call read_vardata(dset,'spfh',values_3d)
           do k = 1,nlevs
              krecq    = 1 + 3*nlevs + k
              rwork_mem(:,krecq) = reshape(values_3d(:,:,k),(/npts/))
           end do
           call read_vardata(dset,'o3mr',values_3d)
           do k = 1,nlevs
              krecoz   = 1 + 4*nlevs + k
              rwork_mem(:,krecoz) = reshape(values_3d(:,:,k),(/npts/))
           end do
           call read_vardata(dset,'clwmr',values_3d)
           do k = 1,nlevs
              kreccwmr = 1 + 5*nlevs + k
              rwork_mem(:,kreccwmr) = reshape(values_3d(:,:,k),(/npts/))
           end do
           call read_vardata(dset,'icmr',values_3d)
           do k = 1,nlevs
              krecicmr = 1 + 6*nlevs + k
              rwork_mem(:,krecicmr) = reshape(values_3d(:,:,k),(/npts/))
           end do
           call close_dataset(dset)
           deallocate(values_3d)
        endif

        call mpi_allreduce(rwork_mem,rwork_avg,nsize,mpi_real4,mpi_sum,new_comm,iret)

        rwork_avg = rwork_avg * rnanals

        if ( mype == 0 ) call write_to_disk('mean')

        rwork_mem = (rwork_mem - rwork_avg) * (rwork_mem - rwork_avg)

        call mpi_allreduce(rwork_mem,rwork_avg,nsize,mpi_real4,mpi_sum,new_comm,iret)

        rwork_avg = sqrt(rwork_avg * rnanalsm1)

        if ( mype == 0 ) call write_to_disk('spread')


    ! Jump here if more mpi processors than files to process
    else
        write(6,'(a,i5)') 'No files to process for mpi task = ',mype
    endif

    call mpi_barrier(mpi_comm_world,iret)
    deallocate(rwork_mem,rwork_avg)

    deallocate(new_group_members)
    if ( mype == 0 ) deallocate(glats)

    if ( mype == 0 ) call w3tage('GETSIGENSSTATP')

999 continue

    call mpi_finalize(iret)

    stop

contains

subroutine write_to_disk(statstr)

   implicit none

   integer, parameter :: lunit=63
   character(len=*), intent(in) :: statstr

   character(len=500) :: filenameout
   character(len=24),parameter :: myname='write_to_disk'
   integer :: ncid,varid,vardim(3)
   real(r_single) :: var2d(lonb,latb),var3d(lonb,latb,nlevs),glons(lonb)
   integer :: glevs(nlevs)
   integer :: i,kbeg,kend

   glons(1) = 0._r_single
   do i=2,lonb
      glons(i) = glons(i-1) + 360._r_single / lonb
   enddo
   do i = 1,nlevs
      glevs(i) = i
   enddo

   filenameout = trim(adjustl(datapath)) // trim(adjustl(filepref)) // '_ens' // trim(adjustl(statstr)) // '.nc4'

   call nc_check( nf90_create(trim(adjustl(filenameout)),cmode=ior(NF90_CLOBBER,NF90_64BIT_OFFSET),ncid=ncid),myname,'create '//trim(filenameout) )
   call nc_check( nf90_def_dim(ncid,'lon',lonb,vardim(1)),myname,'def_dim lon '//trim(filenameout) )
   call nc_check( nf90_def_dim(ncid,'lat',latb,vardim(2)),myname,'def_dim lat '//trim(filenameout) )
   call nc_check( nf90_def_dim(ncid,'lev',nlevs,vardim(3)),myname,'def_dim lev '//trim(filenameout) )
   call nc_check( nf90_def_var(ncid,'lon',nf90_float,vardim(1),varid),myname,'def_var lon '//trim(filenameout) )
   call nc_check( nf90_put_att(ncid, varid, 'long_name','longitude'),myname, 'put_att, long_name lon '//trim(filenameout) )
   call nc_check( nf90_put_att(ncid, varid, 'units','degrees_east'),myname, 'put_att, units lon '//trim(filenameout) )
   call nc_check( nf90_put_att(ncid, varid, 'axis','X'),myname, 'put_att, axis lon '//trim(filenameout) )
   call nc_check( nf90_def_var(ncid,'lat',nf90_float,vardim(2),varid),myname,'def_var lat '//trim(filenameout) )
   call nc_check( nf90_put_att(ncid, varid, 'long_name','latitude'),myname, 'put_att, long_name lat'//trim(filenameout) )
   call nc_check( nf90_put_att(ncid, varid, 'units','degrees_north'),myname, 'put_att, units lat '//trim(filenameout) )
   call nc_check( nf90_put_att(ncid, varid, 'axis','Y'),myname, 'put_att, axis lat '//trim(filenameout) )
   call nc_check( nf90_def_var(ncid,'lev',nf90_int,vardim(3),varid),myname,'def_var lev '//trim(filenameout) )
   call nc_check( nf90_put_att(ncid, varid, 'long_name','level'),myname, 'put_att, long_name lev'//trim(filenameout) )
   call nc_check( nf90_put_att(ncid, varid, 'units','up'),myname, 'put_att, units lev '//trim(filenameout) )
   call nc_check( nf90_put_att(ncid, varid, 'axis','Z'),myname, 'put_att, axis lev '//trim(filenameout) )
   call nc_check( nf90_def_var(ncid,'ps',nf90_float,vardim(1:2),varid),myname,'def_var ps '//trim(filenameout) )
   call nc_check( nf90_put_att(ncid, varid, 'long_name','surface pressure'),myname, 'put_att, long_name ps '//trim(filenameout) )
   call nc_check( nf90_put_att(ncid, varid, 'units','hPa'),myname, 'put_att, units ps '//trim(filenameout) )
   call nc_check( nf90_def_var(ncid,'u',nf90_float,vardim,varid),myname,'def_var u '//trim(filenameout) )
   call nc_check( nf90_put_att(ncid, varid, 'long_name','zonal wind'),myname, 'put_att, long_name u '//trim(filenameout) )
   call nc_check( nf90_put_att(ncid, varid, 'units','m/s'),myname, 'put_att, units u '//trim(filenameout) )
   call nc_check( nf90_def_var(ncid,'v',nf90_float,vardim,varid),myname,'def_var v '//trim(filenameout) )
   call nc_check( nf90_put_att(ncid, varid, 'long_name','meridional wind'),myname, 'put_att, long_name v '//trim(filenameout) )
   call nc_check( nf90_put_att(ncid, varid, 'units','m/s'),myname, 'put_att, units v '//trim(filenameout) )
   call nc_check( nf90_def_var(ncid,'t',nf90_float,vardim,varid),myname,'def_var t '//trim(filenameout) )
   call nc_check( nf90_put_att(ncid, varid, 'long_name','temperature'),myname, 'put_att, long_name t '//trim(filenameout) )
   call nc_check( nf90_put_att(ncid, varid, 'units','K'),myname, 'put_att, units t '//trim(filenameout) )
   call nc_check( nf90_def_var(ncid,'q',nf90_float,vardim,varid),myname,'def_var q '//trim(filenameout) )
   call nc_check( nf90_put_att(ncid, varid, 'long_name','specific humidity'),myname, 'put_att, long_name q '//trim(filenameout) )
   call nc_check( nf90_put_att(ncid, varid, 'units','kg/kg'),myname, 'put_att, units q '//trim(filenameout) )
   call nc_check( nf90_def_var(ncid,'oz',nf90_float,vardim,varid),myname,'def_var oz '//trim(filenameout) )
   call nc_check( nf90_put_att(ncid, varid, 'long_name','ozone mixing ratio'),myname, 'put_att, long_name oz '//trim(filenameout) )
   call nc_check( nf90_put_att(ncid, varid, 'units','kg/kg'),myname, 'put_att, units oz '//trim(filenameout) )
   call nc_check( nf90_def_var(ncid,'cw',nf90_float,vardim,varid),myname,'def_var cw '//trim(filenameout) )
   call nc_check( nf90_put_att(ncid, varid, 'long_name','cloud-water mixing ratio'),myname, 'put_att, long_name cw '//trim(filenameout) )
   call nc_check( nf90_put_att(ncid, varid, 'units','kg/kg'),myname, 'put_att, units cw '//trim(filenameout) )
   if (do_hydro) then
      call nc_check( nf90_def_var(ncid,'qi',nf90_float,vardim,varid),myname,'def_var qi '//trim(filenameout) )
      call nc_check( nf90_put_att(ncid, varid, 'long_name','cloud-ice mixing ratio'),myname, 'put_att, long_name qi '//trim(filenameout) )
      call nc_check( nf90_put_att(ncid, varid, 'units','kg/kg'),myname, 'put_att, units qi '//trim(filenameout) )
      call nc_check( nf90_def_var(ncid,'qr',nf90_float,vardim,varid),myname,'def_var qr '//trim(filenameout) )
      call nc_check( nf90_put_att(ncid, varid, 'long_name','rain water mixing ratio'),myname, 'put_att, long_name qr '//trim(filenameout) )
      call nc_check( nf90_put_att(ncid, varid, 'units','kg/kg'),myname, 'put_att, units qr '//trim(filenameout) )
      call nc_check( nf90_def_var(ncid,'qs',nf90_float,vardim,varid),myname,'def_var qs '//trim(filenameout) )
      call nc_check( nf90_put_att(ncid, varid, 'long_name','snow water mixing ratio'),myname, 'put_att, long_name qs '//trim(filenameout) )
      call nc_check( nf90_put_att(ncid, varid, 'units','kg/kg'),myname, 'put_att, units qs '//trim(filenameout) )
      call nc_check( nf90_def_var(ncid,'qg',nf90_float,vardim,varid),myname,'def_var qg '//trim(filenameout) )
      call nc_check( nf90_put_att(ncid, varid, 'long_name','graupel water mixing ratio'),myname, 'put_att, long_name qg '//trim(filenameout) )
      call nc_check( nf90_put_att(ncid, varid, 'units','kg/kg'),myname, 'put_att, units qg '//trim(filenameout) )
   endif
   call nc_check( nf90_enddef(ncid),myname,'enddef, '//trim(filenameout) )
   call nc_check( nf90_close(ncid),myname,'close, '//trim(filenameout) )

   call nc_check( nf90_open(trim(adjustl(filenameout)),NF90_WRITE,ncid),myname,'open '//trim(filenameout) )
   call nc_check( nf90_inq_varid(ncid,'lon',varid),myname,'inq_varid, lon '// trim(filenameout) )
   call nc_check( nf90_put_var(ncid,varid,glons,(/1/),(/lonb/)),myname, 'put_var, lon '//trim(filenameout) )
   call nc_check( nf90_inq_varid(ncid,'lat',varid),myname,'inq_varid, lat '// trim(filenameout) )
   call nc_check( nf90_put_var(ncid,varid,glats,(/1/),(/latb/)),myname, 'put_var, lat '//trim(filenameout) )
   call nc_check( nf90_inq_varid(ncid,'lev',varid),myname,'inq_varid, lev '// trim(filenameout) )
   call nc_check( nf90_put_var(ncid,varid,glevs,(/1/),(/nlevs/)),myname, 'put_var, lev '//trim(filenameout) )
   kbeg = 1 ; kend = 1
   var2d = reshape(rwork_avg(:,kbeg:kend),(/lonb,latb/))
   var2d = var2d(:,latb:1:-1)
   call nc_check( nf90_inq_varid(ncid,'ps',varid),myname,'inq_varid, ps '// trim(filenameout) )
   call nc_check( nf90_put_var(ncid,varid,var2d,(/1,1/),(/lonb,latb/)),myname, 'put_var, ps '//trim(filenameout) )
   kbeg = kend + 1 ; kend = kend + nlevs
   var3d = reshape(rwork_avg(:,kbeg:kend),(/lonb,latb,nlevs/))
   var3d = var3d(:,latb:1:-1,:)
   call nc_check( nf90_inq_varid(ncid,'u',varid),myname,'inq_varid, u '// trim(filenameout) )
   call nc_check( nf90_put_var(ncid,varid,var3d,(/1,1,1/),(/lonb,latb,nlevs/)),myname, 'put_var, u '//trim(filenameout) )
   kbeg = kend + 1 ; kend = kend + nlevs
   var3d = reshape(rwork_avg(:,kbeg:kend),(/lonb,latb,nlevs/))
   var3d = var3d(:,latb:1:-1,:)
   call nc_check( nf90_inq_varid(ncid,'v',varid),myname,'inq_varid, v '// trim(filenameout) )
   call nc_check( nf90_put_var(ncid,varid,var3d,(/1,1,1/),(/lonb,latb,nlevs/)),myname, 'put_var, v '//trim(filenameout) )
   kbeg = kend + 1 ; kend = kend + nlevs
   var3d = reshape(rwork_avg(:,kbeg:kend),(/lonb,latb,nlevs/))
   var3d = var3d(:,latb:1:-1,:)
   call nc_check( nf90_inq_varid(ncid,'t',varid),myname,'inq_varid, t '// trim(filenameout) )
   call nc_check( nf90_put_var(ncid,varid,var3d,(/1,1,1/),(/lonb,latb,nlevs/)),myname, 'put_var, t '//trim(filenameout) )
   kbeg = kend + 1 ; kend = kend + nlevs
   var3d = reshape(rwork_avg(:,kbeg:kend),(/lonb,latb,nlevs/))
   var3d = var3d(:,latb:1:-1,:)
   call nc_check( nf90_inq_varid(ncid,'q',varid),myname,'inq_varid, q '// trim(filenameout) )
   call nc_check( nf90_put_var(ncid,varid,var3d,(/1,1,1/),(/lonb,latb,nlevs/)),myname, 'put_var, q '//trim(filenameout) )
   kbeg = kend + 1 ; kend = kend + nlevs
   var3d = reshape(rwork_avg(:,kbeg:kend),(/lonb,latb,nlevs/))
   var3d = var3d(:,latb:1:-1,:)
   call nc_check( nf90_inq_varid(ncid,'oz',varid),myname,'inq_varid, oz '// trim(filenameout) )
   call nc_check( nf90_put_var(ncid,varid,var3d,(/1,1,1/),(/lonb,latb,nlevs/)),myname, 'put_var, oz '//trim(filenameout) )
   kbeg = kend + 1 ; kend = kend + nlevs
   var3d = reshape(rwork_avg(:,kbeg:kend),(/lonb,latb,nlevs/))
   var3d = var3d(:,latb:1:-1,:)
   call nc_check( nf90_inq_varid(ncid,'cw',varid),myname,'inq_varid, cw '// trim(filenameout) )
   call nc_check( nf90_put_var(ncid,varid,var3d,(/1,1,1/),(/lonb,latb,nlevs/)),myname, 'put_var, cw '//trim(filenameout) )
   if (do_hydro) then
      kbeg = kend + 1 ; kend = kend + nlevs
      var3d = reshape(rwork_avg(:,kbeg:kend),(/lonb,latb,nlevs/))
      var3d = var3d(:,latb:1:-1,:)
      call nc_check( nf90_inq_varid(ncid,'qi',varid),myname,'inq_varid, qi '// trim(filenameout) )
      call nc_check( nf90_put_var(ncid,varid,var3d,(/1,1,1/),(/lonb,latb,nlevs/)),myname, 'put_var, qi '//trim(filenameout) )
      kbeg = kend + 1 ; kend = kend + nlevs
      var3d = reshape(rwork_avg(:,kbeg:kend),(/lonb,latb,nlevs/))
      var3d = var3d(:,latb:1:-1,:)
      call nc_check( nf90_inq_varid(ncid,'qr',varid),myname,'inq_varid, qr '// trim(filenameout) )
      call nc_check( nf90_put_var(ncid,varid,var3d,(/1,1,1/),(/lonb,latb,nlevs/)),myname, 'put_var, qr '//trim(filenameout) )
      kbeg = kend + 1 ; kend = kend + nlevs
      var3d = reshape(rwork_avg(:,kbeg:kend),(/lonb,latb,nlevs/))
      var3d = var3d(:,latb:1:-1,:)
      call nc_check( nf90_inq_varid(ncid,'qs',varid),myname,'inq_varid, qs '// trim(filenameout) )
      call nc_check( nf90_put_var(ncid,varid,var3d,(/1,1,1/),(/lonb,latb,nlevs/)),myname, 'put_var, qs '//trim(filenameout) )
      kbeg = kend + 1 ; kend = kend + nlevs
      var3d = reshape(rwork_avg(:,kbeg:kend),(/lonb,latb,nlevs/))
      var3d = var3d(:,latb:1:-1,:)
      call nc_check( nf90_inq_varid(ncid,'qg',varid),myname,'inq_varid, qg '// trim(filenameout) )
      call nc_check( nf90_put_var(ncid,varid,var3d,(/1,1,1/),(/lonb,latb,nlevs/)),myname, 'put_var, qg '//trim(filenameout) )
   endif
   call nc_check( nf90_close(ncid),myname,'close, '//trim(filenameout) )

   write(6,'(3a,i5)')'Wrote netcdf4 ',trim(filenameout)

   return

end subroutine write_to_disk

SUBROUTINE nc_check(ierr,subr_name,context)

  ! check for netcdf errors

  implicit none

  integer,          intent(in) :: ierr
  character(len=*), intent(in) :: subr_name, context

  character(len=129) :: error_msg

  if (ierr /= nf90_noerr) then
    error_msg = trim(subr_name) // ': ' // trim(context) // ': ' // trim(nf90_strerror(ierr))
    print*,trim(adjustl(error_msg))
    stop
  end if

  return
END SUBROUTINE nc_check

function variable_exist(varname) result(varexist)

  character(len=*) :: varname
  logical :: varexist

  integer :: n

  varexist = .false.
  do n=1,nrec
    if ( trim(recnam(n)) == trim(varname) ) then
      varexist = .true.
      return
    endif
  enddo

end function variable_exist

end program getsigensstatp
