program getsigensmeanp_smooth
!$$$  main program documentation block
!
! program:  getsigensmean              compute ensemble mean
!
! prgmmr: whitaker         org: esrl/psd               date: 2009-02-23
!
! abstract:  create ensemble mean NCEP GFS spectral sigma file.
!
! program history log:
!   2009-02-23  Initial version.
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
  use sigio_module, only: sigio_head,sigio_data,sigio_srohdc, &
                          sigio_swohdc,sigio_aldata,sigio_axdata
  use nemsio_module, only: nemsio_init,nemsio_open,nemsio_close
  use nemsio_module, only: nemsio_gfile,nemsio_getfilehead,nemsio_charkind8, &
                           nemsio_readrec,nemsio_writerec, &
                           nemsio_readrecv,nemsio_writerecv
  use module_ncio, only: open_dataset, create_dataset, read_attribute, &
                         Dataset, Dimension, close_dataset, has_attr, &
                         read_vardata, write_attribute, write_vardata, &
                         get_dim, quantize_data, has_var

  implicit none

  real,parameter :: zero=0.0_4
  integer,parameter :: iunit=21
  integer,parameter :: window=1 ! cosine bell window for smoothing

  logical :: lexist,dosmooth,nemsio,sigio,ncio,increment,quantize,write_spread_ncio
  logical,allocatable,dimension(:) :: notuv,smooth_fld
  character(nemsio_charkind8) :: dtype
  character(len=3) :: charnanal
  character(len=500) :: filenamein,filenameout,filenameouts,datapath,fileprefix,fname,&
                        filenameoutsprd
  character(len=16),allocatable,dimension(:) :: recnam
  integer :: iret,nlevs,ntrac,ntrunc,nanals,ngrd,k,ndims,nvar,nbits
  integer :: nsize,nsize2,nsize3,nsize3t
  integer :: mype,mype1,npe,orig_group,new_group,new_comm
  integer :: nrec,latb,lonb,npts,n,idrt
  integer,allocatable,dimension(:) :: new_group_members,reclev,krecu,krecv
  integer,allocatable,dimension(:) :: smoothparm
  real(8) :: rnanals,rnanalsm1,t1,t2
  real(8),allocatable,dimension(:,:,:) :: smoothfact
  real(4),allocatable, dimension(:,:,:) :: values_3d, values_3d_avg, &
                                           values_3d_tmp, values_3d_sprd
  real(4),allocatable, dimension(:,:,:) :: values_3dv, values_3dv_avg, &
                                           values_3dv_tmp, values_3dv_sprd
  real(4),allocatable,dimension(:) :: sigdatapert_ps,sigdatapert_z,sigdatapert_d,&
                                      sigdatapert_t,sigdatapert_q,sigdatapert_oz,&
                                      sigdatapert_cw
  real(4),allocatable,dimension(:,:) :: rwork_mem,rwork_avg, values_2d, &
                                        values_2d_sprd,values_2d_avg, values_2d_tmp
  real(4),allocatable,dimension(:) :: rwork_hgt
  real(4),allocatable,dimension(:) :: rwork_lev,rwork_lev2,rwork_spc,rwork_spc2
  real(4) compress_err


  type(sigio_head) :: sigheadi,sigheadm
  type(sigio_data) :: sigdatai,sigdatam
  type(nemsio_gfile) :: gfile,gfileo,gfileos
  type(Dataset) :: dset,dseto,dseto_smooth,dseto_sprd
  type(Dimension) :: londim,latdim,levdim

! mpi definitions.
  include 'mpif.h'

! Initialize mpi, mype is process number, npe is total number of processes.
  call mpi_init(iret)
  call mpi_comm_rank(mpi_comm_world,mype,iret)
  call mpi_comm_size(mpi_comm_world,npe,iret)

  mype1 = mype + 1

  if ( mype == 0 ) call w3tagb('GETSIGENSMEAN_SMOOTH',2011,0319,0055,'NP25')

! Get user input from command line
  call getarg(1,datapath)
  call getarg(2,filenameout)
  call getarg(3,fileprefix)
  call getarg(4,charnanal)
  read(charnanal,'(i3)') nanals

  rnanals = nanals
  rnanals = 1.0_8/rnanals
  rnanalsm1 = nanals-1
  rnanalsm1 = 1.0_8/rnanalsm1
  filenameout = trim(adjustl(datapath)) // trim(adjustl(filenameout))
  ! if a 5th arg present, it's a filename to write out ensemble spread
  ! (only used for ncio)
  if (iargc() > 5) then
     call getarg(5,filenameoutsprd)
     write_spread_ncio = .true.
     if (mype == 0) print *,'computing ensemble spread'
     filenameoutsprd = trim(adjustl(datapath)) // trim(adjustl(filenameoutsprd))
  endif

  if ( mype == 0 ) then
     write(6,'(a)')  'Command line input'
     write(6,'(a,a)')' datapath    = ',trim(datapath)
     write(6,'(a,a)')' filenameout = ',trim(filenameout)
     write(6,'(a,a)')' fileprefix  = ',trim(fileprefix)
     write(6,'(a,a)')' nanals      = ',trim(charnanal)
     if (write_spread_ncio) then
     write(6,'(a,a)')' filenameoutsprd = ',trim(filenameoutsprd)
     endif
     write(6,'(a)')  ' '
  endif

  if ( npe < nanals ) then
     write(6,'(2(a,i4))')'***FATAL ERROR***  npe too small.  npe = ',npe,' < nanals = ',nanals
     call mpi_abort(mpi_comm_world,99,iret)
     stop
  end if

! Create sub-communicator to handle number of cases (nanals)
  call mpi_comm_group(mpi_comm_world,orig_group,iret)

  allocate(new_group_members(nanals))
  do k=1,nanals
     new_group_members(k)=k-1
  end do

  call mpi_group_incl(orig_group,nanals,new_group_members,new_group,iret)
  call mpi_comm_create(mpi_comm_world,new_group,new_comm,iret)
  if ( iret /= 0 ) then
     write(6,'(a,i5)')'***FATAL ERROR*** after mpi_comm_create with iret = ',iret
     call mpi_abort(mpi_comm_world,101,iret)
  endif

  sigio  = .false.
  nemsio = .false.
  ncio = .false.
  increment = .false.

! Process input files (one file per task)
  if ( mype1 <= nanals ) then

     write(charnanal,'(i3.3)') mype1
     filenamein = trim(adjustl(datapath)) // &
          trim(adjustl(fileprefix)) // '_mem' // charnanal

     dset = open_dataset(filenamein,errcode=iret)
     if (iret == 0) then
        ! this is a netCDF file but now we need to determine
        ! if it is a netCDF analysis or increment
        ! going to assume all increment files will have temperature increment
        if (has_var(dset,'T_inc')) then
           increment = .true.
        else ! otherwise assume it is a netCDF analysis file
           ncio = .true.
        end if
     endif
     if (.not. ncio .and. .not. increment ) then
         call nemsio_init(iret=iret)
         call nemsio_open(gfile,trim(filenamein),'READ',iret=iret)
         if (iret == 0) then
            nemsio = .true.
         else
            nemsio = .false.
         endif
     endif
     if (.not. ncio .and. .not. nemsio .and. .not. increment) then
         call sigio_srohdc(iunit,trim(filenamein),sigheadi,sigdatai,iret)
         if (iret == 0) then
            sigio = .true.
         else
            sigio = .false.
         endif
     endif

     if ( .not. ncio .and. .not. nemsio .and. .not. sigio .and. .not. increment) goto 100

!    Read each ensemble member
     if (ncio) then
        if (mype == 0) write(6,*) 'Read netcdf'
        londim = get_dim(dset,'grid_xt'); lonb = londim%len
        latdim = get_dim(dset,'grid_yt'); latb = latdim%len
        levdim = get_dim(dset,'pfull');   nlevs = levdim%len
        call read_attribute(dset, 'ncnsto', ntrac)
        ntrunc = latb-2
     endif
     if (increment) then
        if (mype == 0) write(6,*) 'Read netCDF increment'
        londim = get_dim(dset,'lon'); lonb = londim%len
        latdim = get_dim(dset,'lat'); latb = latdim%len
        levdim = get_dim(dset,'lev');   nlevs = levdim%len
        ntrac = 9999
        ntrunc = latb-2
     endif
     if (sigio) then
        if (mype == 0) write(6,*) 'Read sigio'
        ntrunc  = sigheadi%jcap
        ntrac   = sigheadi%ntrac
        nlevs   = sigheadi%levs
     endif
     if (nemsio) then
        call nemsio_getfilehead(gfile, nrec=nrec, jcap=ntrunc, &
             dimx=lonb, dimy=latb, dimz=nlevs, ntrac=ntrac, gdatatype=dtype, iret=iret)
        write(6,'(5a,i5)')'Read nemsio ',trim(filenamein), ' dtype = ', trim(adjustl(dtype)),' iret = ',iret
        allocate(reclev(nrec),recnam(nrec))
        call nemsio_getfilehead(gfile,reclev=reclev,iret=iret)
        call nemsio_getfilehead(gfile,recname=recnam,iret=iret)
        if ( ntrunc < 0 ) ntrunc = latb - 2
     endif

     if ( mype == 0 ) then
        write(6,'(a)')   ' '
        write(6,'(2a)')  'Read header information from ',trim(filenamein)
        write(6,'(a,i9)')' ntrunc  = ',ntrunc
        write(6,'(a,i9)')' ntrac   = ',ntrac
        write(6,'(a,i9)')' nlevs   = ',nlevs
        if ( ncio .or. nemsio .or. increment ) then
           write(6,'(a,i9)')' lonb    = ',lonb
           write(6,'(a,i9)')' latb    = ',latb
        endif
        if ( nemsio ) then
           write(6,'(a,i9)')' nrec    = ',nrec
        endif
        write(6,'(a)')   ' '
     endif

!    Read smoothing parameters, if available
     fname='hybens_smoothinfo'
     inquire(file=trim(fname),exist=lexist)
     if ( lexist ) then
        allocate(smoothparm(nlevs))
        smoothparm = -1
        open(9,form='formatted',file=fname)
        do k=1,nlevs
           read(9,'(i3)') smoothparm(k)
        enddo
        close(9)
        dosmooth = maxval(smoothparm)>0
     else
        if ( mype == 0 ) write(6,'(a)')'***WARNING***  hybens_smoothinfo not found - no smoothing'
        dosmooth = .false.
     endif
     if ( mype == 0 ) write(6,'(a,l1)')'dosmooth = ',dosmooth

!    Abort if smoothing requested with increment option
     if (dosmooth .and. increment) then
        write(6,'(a,l7,a,l7)')'***FATAL ERROR***  can not run dosmooth ',dosmooth,' and increment ',increment
        call mpi_abort(mpi_comm_world,100,iret)
        stop
     endif

     if ( dosmooth ) then
!       Set up smoother
        allocate(smoothfact(0:ntrunc,0:ntrunc,nlevs))
        smoothfact = 1.0_8
        call setup_smooth(ntrunc,nlevs,smoothparm,window,smoothfact)
        filenameouts = trim(adjustl(datapath)) // &
             trim(adjustl(fileprefix)) // 's' // '_mem' // charnanal
        idrt = 4
     endif

     if ( sigio ) then
        nsize2  = (ntrunc+1)*(ntrunc+2)
        nsize3  = nsize2*nlevs
        nsize3t = nsize3*ntrac

!       Compute ensemble sums.
        call sigio_aldata(sigheadi,sigdatam,iret)
        call mpi_allreduce(sigdatai%z,sigdatam%z,  nsize3, mpi_real,mpi_sum,new_comm,iret)
        call mpi_allreduce(sigdatai%d,sigdatam%d,  nsize3, mpi_real,mpi_sum,new_comm,iret)
        call mpi_allreduce(sigdatai%t,sigdatam%t,  nsize3, mpi_real,mpi_sum,new_comm,iret)
        call mpi_allreduce(sigdatai%q,sigdatam%q,  nsize3t,mpi_real,mpi_sum,new_comm,iret)
        call mpi_allreduce(sigdatai%ps,sigdatam%ps,nsize2, mpi_real,mpi_sum,new_comm,iret)

!       Compute ensemble mean on all tasks
        sigdatam%hs = sigdatai%hs
        sigdatam%ps = sigdatam%ps*rnanals
        sigdatam%z  = sigdatam%z*rnanals
        sigdatam%d  = sigdatam%d*rnanals
        sigdatam%t  = sigdatam%t*rnanals
        sigdatam%q  = sigdatam%q*rnanals

!       Write ensemble mean from task 0
        if ( mype == 0 ) then
           sigheadm = sigheadi
           ngrd = sigheadi%nxgr
           if ( ngrd > 0 ) sigdatam%xgr = sigdatai%xgr

           sigheadm%iens(1) = 1 ! unperturbed control
           sigheadm%iens(2) = 2 ! low res control
           sigheadm%icen2 = 2 ! sub-center, must be 2 or ens info not used
           call sigio_swohdc(iunit,filenameout,sigheadm,sigdatam,iret)
           write(6,'(3a,i5)')'Write sigio ensemble mean ',trim(filenameout),' iret = ',iret
        endif

     elseif ( nemsio ) then

        npts=lonb*latb
        nsize=npts*nrec
        allocate(rwork_mem(npts,nrec))
        allocate(rwork_avg(npts,nrec))
        allocate(rwork_hgt(npts))

        allocate(krecu(nlevs))
        allocate(krecv(nlevs))
        allocate(notuv(nrec ))
        allocate(smooth_fld(nrec))

        krecu = 0
        krecv = 0
        notuv = .true.
        smooth_fld = .true.

        rwork_mem = zero
        do n = 1,nrec
           call nemsio_readrec(gfile,n,rwork_mem(:,n),iret=iret)
           if ( index(recnam(n),'ugrd') /= 0 ) then
              krecu(reclev(n)) = n
              notuv(n) = .false.
           endif
           if ( index(recnam(n),'vgrd') /= 0 ) then
              krecv(reclev(n)) = n
              notuv(n) = .false.
           endif
           if ( index(recnam(n),'dzdt') /= 0 ) then
              smooth_fld(n) = .false.
           endif
           if ( index(recnam(n),'delz') /= 0 ) then
              smooth_fld(n) = .false.
           endif
           if ( index(recnam(n),'dpres') /= 0 ) then
              smooth_fld(n) = .false.
           endif
        enddo
        call nemsio_readrecv(gfile,'hgt','sfc',1,rwork_hgt,iret=iret)

        rwork_avg = zero
        call mpi_allreduce(rwork_mem,rwork_avg,nsize,mpi_real,mpi_sum,new_comm,iret)
        rwork_avg = rwork_avg * rnanals

        if ( mype == 0 ) then
           gfileo=gfile
           call nemsio_open(gfileo,trim(filenameout),'WRITE',iret=iret )
           do n = 1,nrec
              call nemsio_writerec(gfileo,n,rwork_avg(:,n),iret=iret)
           end do
           call nemsio_writerecv(gfileo,'hgt','sfc',1,rwork_hgt,iret=iret)
           call nemsio_close(gfileo,iret=iret)
           write(6,'(3a,i5)')'Write nemsio ensemble mean ',trim(filenameout),' iret = ', iret
        endif

     else if (ncio) then

        if (mype == 0) then
           t1 = mpi_wtime()
           dseto = create_dataset(filenameout, dset, copy_vardata=.true.)
           if (write_spread_ncio) then
              dseto_sprd = create_dataset(filenameoutsprd, dset, copy_vardata=.true.)
           endif
        endif
        if (dosmooth) then
           dseto_smooth = create_dataset(filenameouts, dset, copy_vardata=.true.)
        endif
        allocate(values_2d_avg(lonb,latb))
        allocate(values_2d_tmp(lonb,latb))
        if (dosmooth) then
           allocate(rwork_spc((ntrunc+1)*(ntrunc+2)),rwork_spc2((ntrunc+1)*(ntrunc+2)))
        endif
        if (write_spread_ncio) allocate(values_2d_sprd(lonb,latb))
        do nvar=1,dset%nvars
           ! if smoothing is on, u&v done together.
           if (dosmooth .and. trim(dset%variables(nvar)%name) == 'vgrd') cycle
           ndims = dset%variables(nvar)%ndims
           if (ndims > 2) then
               if (ndims == 3 .and. trim(dset%variables(nvar)%name) /= 'hgtsfc') then
                  ! pressfc
                  if (mype == 0) print *,'processing ',trim(dset%variables(nvar)%name)
                  call read_vardata(dset,trim(dset%variables(nvar)%name),values_2d)
                  call mpi_allreduce(values_2d,values_2d_avg,lonb*latb,mpi_real4,mpi_sum,new_comm,iret)
                  ! ens mean
                  values_2d_avg = values_2d_avg*rnanals
                  if (write_spread_ncio) then
                     ! ens spread
                     values_2d_tmp = values_2d - values_2d_avg ! ens pert
                     values_2d_tmp = values_2d_tmp**2
                     call mpi_reduce(values_2d_tmp,values_2d_sprd,lonb*latb,mpi_real4,mpi_sum,0,new_comm,iret)
                     values_2d_sprd= sqrt(values_2d_sprd*rnanalsm1)
                     if (mype == 0) print *,trim(dset%variables(nvar)%name),' min/max spread',minval(values_2d_sprd),maxval(values_2d_sprd)
                  endif
                  if (has_attr(dset, 'nbits', trim(dset%variables(nvar)%name))) then
                      call read_attribute(dset, 'nbits', nbits, &
                           trim(dset%variables(nvar)%name))
                      quantize = .true.
                      if (nbits < 1) quantize = .false.
                  else
                      quantize = .false.
                  endif
                  ! smooth ens pert and write out?
                  ! don't smooth 2d fields
                  if (dosmooth) then
                     ! don't smooth 2d fields
                     !values_2d = values_2d - values_2d_avg ! ens pert
                     !call sptez(0,ntrunc,idrt,lonb,latb,rwork_spc,values_2d,-1)
                     !call smooth(rwork_spc,ntrunc,smoothfact(:,:,nlevs))
                     !call sptez(0,ntrunc,idrt,lonb,latb,rwork_spc,values_2d,1)
                     !values_2d = values_2d + values_2d_avg ! add mean back
                     if (quantize) then
                        values_2d_tmp = values_2d
                        call quantize_data(values_2d_tmp, values_2d, nbits, compress_err)
                        call write_attribute(dseto_smooth,&
                        'max_abs_compression_error',compress_err,trim(dset%variables(nvar)%name))
                     endif
                     call write_vardata(dseto_smooth,trim(dset%variables(nvar)%name),values_2d)
                  endif
                  ! write ens mean
                  if (mype == 0) then
                     if (quantize) then
                       values_2d_tmp = values_2d_avg
                       call quantize_data(values_2d_tmp, values_2d_avg, nbits, compress_err)
                       call write_attribute(dseto,&
                       'max_abs_compression_error',compress_err,trim(dset%variables(nvar)%name))
                     endif
                     call write_vardata(dseto,trim(dset%variables(nvar)%name),values_2d_avg)
                     if (write_spread_ncio) then
                        if (quantize) then
                          values_2d_tmp = values_2d_sprd
                          call quantize_data(values_2d_tmp, values_2d_sprd, nbits, compress_err)
                          call write_attribute(dseto_sprd,&
                          'max_abs_compression_error',compress_err,trim(dset%variables(nvar)%name))
                        endif
                        call write_vardata(dseto_sprd,trim(dset%variables(nvar)%name),values_2d_sprd)
                     endif
                  endif
               else if (ndims == 4) then
                  ! 3d variables (extra dim is time)
                  call read_vardata(dset,trim(dset%variables(nvar)%name),values_3d)
                  if (allocated(values_3d_avg)) deallocate(values_3d_avg)
                  allocate(values_3d_avg, mold=values_3d)
                  if (allocated(values_3d_tmp)) deallocate(values_3d_tmp)
                  allocate(values_3d_tmp, mold=values_3d_avg)
                  if (write_spread_ncio) then
                     if (allocated(values_3d_sprd)) deallocate(values_3d_sprd)
                     allocate(values_3d_sprd, mold=values_3d_avg)
                  endif
                  if (mype == 0) print *,'processing ',trim(dset%variables(nvar)%name)
                  call mpi_allreduce(values_3d,values_3d_avg,lonb*latb*nlevs,mpi_real4,mpi_sum,new_comm,iret)
                  values_3d_avg = values_3d_avg*rnanals
                  if (write_spread_ncio) then
                     ! ens spread
                     values_3d_tmp = values_3d - values_3d_avg ! ens pert
                     values_3d_tmp = values_3d_tmp**2
                     call mpi_reduce(values_3d_tmp,values_3d_sprd,lonb*latb*nlevs,mpi_real4,mpi_sum,0,new_comm,iret)
                     values_3d_sprd= sqrt(values_3d_sprd*rnanalsm1)
                     if (mype == 0) print *,trim(dset%variables(nvar)%name),' min/max spread',minval(values_3d_sprd),maxval(values_3d_sprd)
                  endif
                  if (has_attr(dset, 'nbits', trim(dset%variables(nvar)%name))) then
                      call read_attribute(dset, 'nbits', nbits, &
                           trim(dset%variables(nvar)%name))
                      quantize = .true.
                      if (nbits < 1) quantize = .false.
                  else
                      quantize = .false.
                  endif
                  ! if smoothing on, read u and v together
                  if (dosmooth .and. trim(dset%variables(nvar)%name) == 'ugrd') then
                     call read_vardata(dset,'vgrd',values_3dv)
                     if (allocated(values_3dv_avg)) deallocate(values_3dv_avg)
                     allocate(values_3dv_avg, mold=values_3dv)
                     if (allocated(values_3dv_tmp)) deallocate(values_3dv_tmp)
                     allocate(values_3dv_tmp, mold=values_3dv)
                     if (allocated(values_3dv_sprd)) deallocate(values_3dv_sprd)
                     allocate(values_3dv_sprd, mold=values_3dv)
                     if (mype == 0) print *,'processing vgrd'
                     call mpi_allreduce(values_3dv,values_3dv_avg,lonb*latb*nlevs,mpi_real4,mpi_sum,new_comm,iret)
                     values_3dv_avg = values_3dv_avg*rnanals
                     if (write_spread_ncio) then
                        ! ens spread
                        values_3dv_tmp = values_3dv - values_3dv_avg ! ens pert
                        values_3dv_tmp = values_3dv_tmp**2
                        call mpi_reduce(values_3d_tmp,values_3dv_sprd,lonb*latb*nlevs,mpi_real4,mpi_sum,0,new_comm,iret)
                        values_3dv_sprd= sqrt(values_3dv_sprd*rnanalsm1)
                        if (mype == 0) print *,'vgrd min/max spread',minval(values_3d_sprd),maxval(values_3d_sprd)
                     endif
                  endif
                  ! smooth ens pert and write out?
                  if (dosmooth) then
                     if (trim(dset%variables(nvar)%name) == 'ugrd') then
                        ! do u,v together
!$omp parallel do schedule(dynamic,1) private(k,rwork_spc,rwork_spc2)
                        do k=1,nlevs
                          if ( smoothparm(nlevs-k+1) > 0 ) then
                           values_3d(:,:,k) = values_3d(:,:,k) - values_3d_avg(:,:,k) ! ens pert
                           values_3dv(:,:,k) = values_3dv(:,:,k) - values_3dv_avg(:,:,k) ! ens pert
                           call sptezv(0,ntrunc,idrt,lonb,latb,rwork_spc,rwork_spc2,&
                                       values_3d(:,:,k),values_3dv(:,:,k),-1)
                           call smooth(rwork_spc, ntrunc,smoothfact(:,:,nlevs-k+1))
                           call smooth(rwork_spc2,ntrunc,smoothfact(:,:,nlevs-k+1))
                           call sptezv(0,ntrunc,idrt,lonb,latb,rwork_spc,rwork_spc2,&
                                       values_3d(:,:,k),values_3dv(:,:,k),1)
                           values_3d(:,:,k) = values_3d(:,:,k) + values_3d_avg(:,:,k) ! add mean back
                           values_3dv(:,:,k) = values_3dv(:,:,k) + values_3dv_avg(:,:,k) ! add mean back
                          endif
                        enddo
                        if (quantize) then
                           values_3d_tmp = values_3d
                           values_3dv_tmp = values_3dv
                           call quantize_data(values_3d_tmp, values_3d, nbits, compress_err)
                           call write_attribute(dseto_smooth,&
                           'max_abs_compression_error',compress_err,'ugrd')
                           call quantize_data(values_3dv_tmp, values_3dv, nbits, compress_err)
                           call write_attribute(dseto_smooth,&
                           'max_abs_compression_error',compress_err,'vgrd')
                        endif
                        call write_vardata(dseto_smooth,'ugrd',values_3d)
                        call write_vardata(dseto_smooth,'vgrd',values_3dv)
                     else
                        ! do scalars.
                        if (trim(dset%variables(nvar)%name) /= 'ugrd' .and. &
                            trim(dset%variables(nvar)%name) /= 'dzdt' .and. &
                            trim(dset%variables(nvar)%name) /= 'delz' .and. &
                            trim(dset%variables(nvar)%name) /= 'dpres' .and. &
                            trim(dset%variables(nvar)%name) /= 'vgrd') then
!$omp parallel do schedule(dynamic,1) private(k,rwork_spc)
                           do k=1,nlevs
                             if ( smoothparm(nlevs-k+1) > 0 ) then
                              values_3d(:,:,k) = values_3d(:,:,k) - values_3d_avg(:,:,k) ! ens pert
                              call sptez(0,ntrunc,idrt,lonb,latb,rwork_spc,values_3d(:,:,k),-1)
                              call smooth(rwork_spc,ntrunc,smoothfact(:,:,nlevs-k+1))
                              call sptez(0,ntrunc,idrt,lonb,latb,rwork_spc,values_3d(:,:,k),1)
                              values_3d(:,:,k) = values_3d(:,:,k) + values_3d_avg(:,:,k) ! add mean back
                             endif
                           enddo
                        endif
                        if (quantize) then
                           values_3d_tmp = values_3d
                           call quantize_data(values_3d_tmp, values_3d, nbits, compress_err)
                           call write_attribute(dseto_smooth,&
                           'max_abs_compression_error',compress_err,trim(dset%variables(nvar)%name))
                        endif
                        call write_vardata(dseto_smooth,trim(dset%variables(nvar)%name),values_3d)
                     endif
                  endif
                  if (mype == 0) then
                     if (quantize) then
                       values_3d_tmp = values_3d_avg
                       call quantize_data(values_3d_tmp, values_3d_avg, nbits, compress_err)
                       call write_attribute(dseto,&
                       'max_abs_compression_error',compress_err,trim(dset%variables(nvar)%name))
                     endif
                     call write_vardata(dseto,trim(dset%variables(nvar)%name),values_3d_avg)
                     ! if smoothing on, write u and v together
                     if (dosmooth .and. trim(dset%variables(nvar)%name) == 'ugrd') then
                        if (quantize) then
                          values_3dv_tmp = values_3dv_avg
                          call quantize_data(values_3dv_tmp, values_3dv_avg, nbits, compress_err)
                          call write_attribute(dseto,&
                          'max_abs_compression_error',compress_err,'vgrd')
                        endif
                        call write_vardata(dseto,'vgrd',values_3dv_avg)
                     endif
                     if (write_spread_ncio) then
                        if (quantize) then
                          values_3d_tmp = values_3d_sprd
                          call quantize_data(values_3d_tmp, values_3d_sprd, nbits, compress_err)
                          call write_attribute(dseto_sprd,&
                          'max_abs_compression_error',compress_err,trim(dset%variables(nvar)%name))
                        endif
                        call write_vardata(dseto_sprd,trim(dset%variables(nvar)%name),values_3d_sprd)
                        ! if smoothing on, write u and v together
                        if (dosmooth .and. trim(dset%variables(nvar)%name) == 'ugrd') then
                           if (quantize) then
                             values_3dv_tmp = values_3dv_sprd
                             call quantize_data(values_3dv_tmp, values_3dv_sprd, nbits, compress_err)
                             call write_attribute(dseto_sprd,&
                             'max_abs_compression_error',compress_err,'vgrd')
                           endif
                           call write_vardata(dseto_sprd,'vgrd',values_3dv_sprd)
                        endif
                     endif
                  endif
               endif
           endif ! ndims > 2
        enddo  ! nvars
        deallocate(values_2d, values_3d, values_2d_avg, values_3d_avg)
        deallocate(values_2d_tmp, values_3d_tmp)
        if (dosmooth) then
           deallocate(rwork_spc)
           call close_dataset(dseto_smooth)
        endif
        if (write_spread_ncio) then
           deallocate(values_2d_sprd, values_3d_sprd)
        endif
        if (mype == 0) then
           call close_dataset(dseto)
           t2 = mpi_wtime()
           print *,'time to write ens mean on root',t2-t1
           write(6,'(3a,i5)')'Write ncio ensemble mean ',trim(filenameout),' iret = ', iret
           if (write_spread_ncio) then
             call close_dataset(dseto_sprd)
              write(6,'(3a,i5)')'Write ncio ensemble spread ',trim(filenameoutsprd),' iret = ', iret
           endif
        endif

     else if (increment) then

        if (mype == 0) then
           t1 = mpi_wtime()
           dseto = create_dataset(filenameout, dset, copy_vardata=.true.)
           if (write_spread_ncio) then
              dseto_sprd = create_dataset(filenameoutsprd, dset, copy_vardata=.true.)
           endif
        endif
        do nvar=1,dset%nvars
           ndims = dset%variables(nvar)%ndims
           if (ndims == 3) then
              call read_vardata(dset,trim(dset%variables(nvar)%name),values_3d)
              if (allocated(values_3d_avg)) deallocate(values_3d_avg)
              allocate(values_3d_avg, mold=values_3d)
              if (allocated(values_3d_tmp)) deallocate(values_3d_tmp)
              allocate(values_3d_tmp, mold=values_3d_avg)
              if (write_spread_ncio) then
                 if (allocated(values_3d_sprd)) deallocate(values_3d_sprd)
                 allocate(values_3d_sprd, mold=values_3d_avg)
              endif
              if (mype == 0) print *,'processing ',trim(dset%variables(nvar)%name)
              call mpi_allreduce(values_3d,values_3d_avg,lonb*latb*nlevs,mpi_real4,mpi_sum,new_comm,iret)
              values_3d_avg = values_3d_avg*rnanals
              if (write_spread_ncio) then
                 ! ens spread
                 values_3d_tmp = values_3d - values_3d_avg ! ens pert
                 values_3d_tmp = values_3d_tmp**2
                 call mpi_reduce(values_3d_tmp,values_3d_sprd,lonb*latb*nlevs,mpi_real4,mpi_sum,0,new_comm,iret)
                 values_3d_sprd= sqrt(values_3d_sprd*rnanalsm1)
                 if (mype == 0) print *,trim(dset%variables(nvar)%name),' min/max spread',minval(values_3d_sprd),maxval(values_3d_sprd)
              endif
              if (mype == 0) then
                 call write_vardata(dseto,trim(dset%variables(nvar)%name),values_3d_avg)
                 if (write_spread_ncio) then
                    call write_vardata(dseto_sprd,trim(dset%variables(nvar)%name),values_3d_sprd)
                 end if
              end if
           end if ! end if 3D var
        end do ! end loop through variables
        deallocate(values_3d, values_3d_avg)
        deallocate(values_3d_tmp)
        if (write_spread_ncio) then
           deallocate(values_3d_sprd)
        endif
        if (mype == 0) then
           call close_dataset(dseto)
           t2 = mpi_wtime()
           print *,'time to write ens mean on root',t2-t1
           write(6,'(3a,i5)')'Write increment ensemble mean ',trim(filenameout),' iret = ', iret
           if (write_spread_ncio) then
             call close_dataset(dseto_sprd)
              write(6,'(3a,i5)')'Write increment ensemble spread ',trim(filenameoutsprd),' iret = ', iret
           endif
        endif
     endif

!    If smoothing requested, loop over and smooth analysis fields (for sigio and
!    nemsio)
     if ( dosmooth ) then

        if ( sigio ) then

           allocate(sigdatapert_z(nsize2),sigdatapert_d(nsize2), sigdatapert_t(nsize2), &
                sigdatapert_q(nsize2),sigdatapert_oz(nsize2),sigdatapert_cw(nsize2),&
                sigdatapert_ps(nsize2))

           k=1
           if (smoothparm(k)>0) then
              sigdatapert_ps  = sigdatai%ps(:)  - sigdatam%ps(:)
              call smooth(sigdatapert_ps,ntrunc,smoothfact(:,:,k))
              sigdatai%ps(:) = sigdatam%ps(:) + sigdatapert_ps
           endif

           do k=2,nlevs
              if (smoothparm(k)>0) then
                 sigdatapert_z  = sigdatai%z(:,k)   - sigdatam%z(:,k)
                 sigdatapert_d  = sigdatai%d(:,k)   - sigdatam%d(:,k)
                 sigdatapert_t  = sigdatai%t(:,k)   - sigdatam%t(:,k)
                 sigdatapert_q  = sigdatai%q(:,k,1) - sigdatam%q(:,k,1)
                 sigdatapert_oz = sigdatai%q(:,k,2) - sigdatam%q(:,k,2)
                 sigdatapert_cw = sigdatai%q(:,k,3) - sigdatam%q(:,k,3)

                 call smooth(sigdatapert_z, ntrunc,smoothfact(:,:,k))
                 call smooth(sigdatapert_d, ntrunc,smoothfact(:,:,k))
                 call smooth(sigdatapert_t, ntrunc,smoothfact(:,:,k))
                 call smooth(sigdatapert_q, ntrunc,smoothfact(:,:,k))
                 call smooth(sigdatapert_oz,ntrunc,smoothfact(:,:,k))
                 call smooth(sigdatapert_cw,ntrunc,smoothfact(:,:,k))

                 sigdatai%z(:,k)   = sigdatam%z(:,k)   + sigdatapert_z
                 sigdatai%d(:,k)   = sigdatam%d(:,k)   + sigdatapert_d
                 sigdatai%t(:,k)   = sigdatam%t(:,k)   + sigdatapert_t
                 sigdatai%q(:,k,1) = sigdatam%q(:,k,1) + sigdatapert_q
                 sigdatai%q(:,k,2) = sigdatam%q(:,k,2) + sigdatapert_oz
                 sigdatai%q(:,k,3) = sigdatam%q(:,k,3) + sigdatapert_cw

              endif
           enddo

           deallocate(sigdatapert_z, sigdatapert_d, sigdatapert_t, sigdatapert_q,&
                      sigdatapert_oz,sigdatapert_cw,sigdatapert_ps)

!          Write smoothed member
           call sigio_swohdc(iunit,trim(filenameouts),sigheadi,sigdatai,iret)
           write(6,'(3a,i5)')'Write smoothed sigio ',trim(filenameouts),' iret = ',iret

        elseif ( nemsio ) then

           allocate(rwork_lev(npts),rwork_lev2(npts))
           allocate(rwork_spc((ntrunc+1)*(ntrunc+2)),rwork_spc2((ntrunc+1)*(ntrunc+2)))
           idrt = 4

!          Smoothing loop over fields (first do scalar fields only)
!$omp parallel do schedule(dynamic,1) private(n,rwork_lev,rwork_spc)
           do n = 1,nrec
              if ( smooth_fld(n) .and. notuv(n) .and. smoothparm(reclev(n)) > 0 ) then
                 rwork_lev = rwork_mem(:,n) - rwork_avg(:,n)
                 call sptez(0,ntrunc,idrt,lonb,latb,rwork_spc,rwork_lev,-1)
                 call smooth(rwork_spc,ntrunc,smoothfact(:,:,reclev(n)))
                 call sptez(0,ntrunc,idrt,lonb,latb,rwork_spc,rwork_lev,1)
                 rwork_mem(:,n) = rwork_avg(:,n) + rwork_lev
              endif
           enddo

!          Smoothing loop over vector fields u and v
!$omp parallel do schedule(dynamic,1) private(k,rwork_lev,rwork_lev2,rwork_spc,rwork_spc2)
           do k = 1,nlevs
              if ( smoothparm(k) > 0 ) then
                 rwork_lev  = rwork_mem(:,krecu(k)) - rwork_avg(:,krecu(k))
                 rwork_lev2 = rwork_mem(:,krecv(k)) - rwork_avg(:,krecv(k))
                 call sptezv(0,ntrunc,idrt,lonb,latb,rwork_spc,rwork_spc2,rwork_lev,rwork_lev2,-1)
                 call smooth(rwork_spc, ntrunc,smoothfact(:,:,k))
                 call smooth(rwork_spc2,ntrunc,smoothfact(:,:,k))
                 call sptezv(0,ntrunc,idrt,lonb,latb,rwork_spc,rwork_spc2,rwork_lev,rwork_lev2,1)
                 rwork_mem(:,krecu(k)) = rwork_avg(:,krecu(k)) + rwork_lev
                 rwork_mem(:,krecv(k)) = rwork_avg(:,krecv(k)) + rwork_lev2
              endif
           enddo

           deallocate(rwork_lev,rwork_lev2)
           deallocate(rwork_spc,rwork_spc2)

!          Write smoothed member
           gfileos=gfile
           call nemsio_open(gfileos,trim(filenameouts),'WRITE',iret=iret )
           do n = 1,nrec
              call nemsio_writerec(gfileos,n,rwork_mem(:,n),iret=iret)
           enddo

!          Write unsmoothed member orography to smoothed output file
           call nemsio_writerecv(gfileos,'hgt','sfc',1,rwork_hgt,iret=iret)

           call nemsio_close(gfileos,iret=iret)
           write(6,'(3a,i5)')'Write smoothed nemsio ',trim(filenameouts),' iret = ',iret

        endif

!       Deallocate smoothing factors
        if (allocated(smoothfact)) deallocate(smoothfact)

!    End of smoothing block
     endif

!    Deallocate structures and arrays
     if (allocated(smoothparm)) deallocate(smoothparm)
     if ( sigio ) then
        call sigio_axdata(sigdatai,iret)
        call sigio_axdata(sigdatam,iret)
     elseif ( nemsio ) then
        call nemsio_close(gfile,iret=iret)
        if (allocated(rwork_mem)) deallocate(rwork_mem)
        if (allocated(rwork_avg)) deallocate(rwork_avg)
        if (allocated(rwork_hgt)) deallocate(rwork_hgt)
        deallocate(krecu,krecv,notuv,smooth_fld)
     elseif (ncio) then
        call close_dataset(dset)
     endif

! Jump here if more mpi processors than files to process
  else
     write(6,'(a,i5)') 'No files to process for mpi task = ',mype
  endif

100 continue
  call mpi_barrier(mpi_comm_world,iret)

  if ( mype1 <= nanals .and. .not. nemsio .and. .not. sigio .and. .not. ncio .and. .not. increment) then
     write(6,'(a)')'***FATAL ERROR***  invalid atmospheric file format'
     call mpi_abort(mpi_comm_world,98,iret)
     stop
  endif

  if ( mype == 0 ) call w3tage('GETSIGENSMEAN_SMOOTH')

 deallocate(new_group_members)

 call mpi_finalize(iret)


end program getsigensmeanp_smooth

subroutine smooth(specdat,ntrunc,smoothfact)
  implicit none
  integer :: m,nm,n
  integer, intent(in) :: ntrunc
  real(8),intent(in)  ::  smoothfact(0:ntrunc,0:ntrunc) ! smoothing factor
  real(4),intent(inout) :: specdat((ntrunc+1)*(ntrunc+2))

  nm = 1
  m_loop: do m=0,ntrunc
     n_loop: do n=m,ntrunc
        specdat(nm)   = smoothfact(n,m)*specdat(nm)
        specdat(nm+1) = smoothfact(n,m)*specdat(nm+1)
        nm = nm + 2
     enddo n_loop
  enddo m_loop
end subroutine smooth

subroutine setup_smooth(ntrunc,nlevs,smoothparm,window,smoothfact)
  implicit none
  integer :: m,n,k
  integer, intent(in) :: ntrunc,nlevs,window
  integer,dimension(nlevs),intent(in) ::  smoothparm ! smoothing parameter.
  real(8),dimension(0:ntrunc,0:ntrunc,nlevs),intent(out) :: smoothfact
  real(8) zero,half,one,pi,smoothval,rsmoothval,waven

  zero = 0.0_8
  half = 0.5_8
  one  = 1.0_8
  pi   = 4.0_8*atan(one)

  k_loop: do k=1,nlevs
     if (smoothparm(k) .le. 0.) cycle k_loop
     smoothval=smoothparm(k)
     rsmoothval=one/smoothval
     m_loop: do m=0,ntrunc
        n_loop: do n=m,ntrunc
           waven=real(n)
           if (window .eq. 1) then
              ! Hann window (cosine bell).
              if (n <= smoothparm(k)) then
                 smoothfact(n,m,k) = half*(one + cos(pi*waven*rsmoothval))
              else
                 smoothfact(n,m,k) = zero
              endif
           else if (window .eq. 2) then
              ! gaussian window.
              smoothfact(n,m,k) = exp(-(waven*rsmoothval)**2)
           else
              ! rectangular window (simple truncation)
              if (n <= smoothparm(k)) then
                 smoothfact(n,m,k) = one
              else
                 smoothfact(n,m,k) = zero
              endif
           endif
        enddo n_loop
     enddo m_loop
  enddo k_loop
end subroutine setup_smooth
