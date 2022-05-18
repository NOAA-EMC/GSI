program recenterncio_hybgain
!$$$  main program documentation block
!
! program:  recenterncio_hybgain               recenter
!
! prgmmr: whitaker         org: esrl/psd               date: 2009-02-23
!
! abstract:  Recenter ensemble analysis files about new
!            mean, computed from blended 3DVar and EnKF increments 
!            (optionally applying RTPS inflation).
!
! program history log:
!   2019-02-10  Initial version.
!
! usage:
!   input files:
!
!   output files:
!
! attributes:
!   language: f95
!
!
!$$$

  use module_ncio, only: open_dataset, create_dataset, read_attribute, &
                         Dataset, Dimension, close_dataset, has_attr, &
                         read_vardata, write_attribute, write_vardata, &
                         get_dim, quantize_data

  implicit none

  include "mpif.h"

! Declare externals
  external :: MPI_Init, MPI_Comm_rank, MPI_Comm_size, w3tagb, MPI_Abort,&
     MPI_Barrier, w3tage, MPI_Finalize

  character*500 filename_varfg,filename_varanal,filename_enkfanal,filenamein,&
  filename_enkffg,filenameout,filename_anal,filename,filename_fsprd,filename_asprd
  character*3 charnanal
  character(len=4) charnin
  integer mype,mype1,npe,nanals,iret,ialpha,ibeta,irtps,i,j,k
  integer:: nlats,nlons,nlevs,nvar,ndims,nbits
  real alpha,beta,rtps,infmin,infmax,clip
  real(4),allocatable,dimension(:,:) :: values_2d_varanal,values_2d_enkfanal,&
                         values_2d_varfg,values_2d_enkffg,values_2d_anal,&
                         values_2d,asprd_2d,fsprd_2d,inf_2d,values_2d_tmp 
  real(4),allocatable,dimension(:,:,:) :: values_3d_varanal,values_3d_enkfanal,&
                         values_3d_varfg,values_3d_enkffg,values_3d_anal,&
                         values_3d,asprd_3d,fsprd_3d,inf_3d,values_3d_tmp
  real(4) compress_err
  type(Dataset) :: dseti,dseto,dset_blendanal,dset_varfg,dset_varanal,dset_enkffg,dset_enkfanal,dset_asprd,dset_fsprd
  type(Dimension) :: londim,latdim,levdim
  logical cliptracers,tracer

! Initialize mpi
  call MPI_Init(iret)

! mype is process number, npe is total number of processes.
  call MPI_Comm_rank(MPI_COMM_WORLD,mype,iret)
  call MPI_Comm_size(MPI_COMM_WORLD,npe,iret)

  if (mype==0) call w3tagb('RECENTERNCIO_HYBGAIN',2011,0319,0055,'NP25')

  call getarg(1,filename_varfg)   ! first guess 3dvar netcdf file
  call getarg(2,filename_varanal) ! 3dvar analysis
  call getarg(3,filename_enkffg)  ! first guess enkf netcdf file
  call getarg(4,filename_enkfanal) ! enkf mean analysis
  call getarg(5,filename_anal)  ! blended analysis (to recenter ensemble around)
  call getarg(6,filenamein) ! prefix for input ens member files (append _mem###)
  call getarg(7,filenameout) ! prefix for output ens member files (append _mem###)
! blending coefficients
  call getarg(8,charnin)
  read(charnin,'(i4)') ialpha ! wt for varanal (3dvar)
  alpha = ialpha/1000.
  call getarg(9,charnin)
  read(charnin,'(i4)') ibeta ! wt for enkfanal (enkf)
  beta = ibeta/1000.
  call getarg(10,charnin)
  read(charnin,'(i4)') irtps ! rtps relaxation coeff
  rtps = irtps/1000.
! new_anal = fg + alpha*(varanal-fg) + beta*(enkfanal-fg)
! how many ensemble members to process
  call getarg(11,charnin)
  read(charnin,'(i4)') nanals
  if (rtps > 0) then
     call getarg(13,filename_fsprd) ! first guess ensemble spread
     call getarg(14,filename_asprd) ! analysis ensemble spread
  endif

  infmin=1.0; infmax=10.
  clip = tiny(rtps)

  if (mype==0) then
     write(6,*)'RECENTERNCIO_HYBGAIN:  PROCESS ',nanals,' ENSEMBLE MEMBERS'
     write(6,*)'3dvar background in ',trim(filename_varfg)
     write(6,*)'EnKF background in ',trim(filename_enkffg)
     write(6,*)'3dvar analysis in ',trim(filename_varanal)
     write(6,*)'EnKF mean analysis in ',trim(filename_enkfanal)
     write(6,*)'Blended mean analysis to be written to ',trim(filename_anal)
     write(6,*)'Prefix for member input files ',trim(filenamein)
     write(6,*)'Prefix for member output files ',trim(filenameout)
     write(6,*)'3dvar weight, EnKF weight, RTPS relaxation =',alpha,beta,rtps
     if (rtps > 0) then
        write(6,*)'ens spread background in ',trim(filename_fsprd)
        write(6,*)'ens spread posterior in ',trim(filename_asprd)
     endif
  endif

  mype1 = mype+1
  if (mype1 <= nanals) then
     dset_varfg = open_dataset(filename_varfg,errcode=iret)
     if (iret == 0 ) then
        if (mype == 0) write(6,*)'Read netcdf ',trim(filename_varfg)
        londim = get_dim(dset_varfg,'grid_xt'); nlons = londim%len
        latdim = get_dim(dset_varfg,'grid_yt'); nlats = latdim%len
        levdim = get_dim(dset_varfg,'pfull');   nlevs = levdim%len
        if (mype == 0) write(6,*)' nlons=',nlons,' nlats=',nlats,' nlevs=',nlevs
     else
        write(6,*) 'error opening ',trim(filename_varfg)
        call MPI_Abort(MPI_COMM_WORLD,98,iret)
        stop
     endif

     ! read in 3dvar, enkf analyses and backgrounds, blend increments
     if (alpha > 0) then
     dset_varanal = open_dataset(filename_varanal,errcode=iret)
     if (iret /= 0) then
       print *,'error opening ',trim(filename_varanal)
       call MPI_Abort(MPI_COMM_WORLD,98,iret)
       stop
     endif
     endif
     dset_enkfanal = open_dataset(filename_enkfanal,errcode=iret)
     if (iret /= 0) then
       print *,'error opening ',trim(filename_enkfanal)
       call MPI_Abort(MPI_COMM_WORLD,98,iret)
       stop
     endif
     dset_enkffg = open_dataset(filename_enkffg,errcode=iret)
     if (iret /= 0) then
       print *,'error opening ',trim(filename_enkffg)
       call MPI_Abort(MPI_COMM_WORLD,98,iret)
       stop
     endif
     if (rtps > 0) then
        dset_fsprd = open_dataset(filename_fsprd,errcode=iret)
        if (iret /= 0) then
          print *,'error opening ',trim(filename_fsprd)
          call MPI_Abort(MPI_COMM_WORLD,98,iret)
          stop
        endif
        dset_asprd = open_dataset(filename_asprd,errcode=iret)
        if (iret /= 0) then
          print *,'error opening ',trim(filename_asprd)
          call MPI_Abort(MPI_COMM_WORLD,98,iret)
          stop
        endif
     endif
     if (mype == 0 .and. alpha > 0) then
        dset_blendanal = create_dataset(filename_anal, dset_enkfanal, &
                    copy_vardata=.true.,  errcode=iret)
        if (iret /= 0) then
          print *,'error opening ',trim(filename_anal)
          call MPI_Abort(MPI_COMM_WORLD,98,iret)
          stop
        endif
     endif
     write(charnanal,'(i3.3)') mype1
     filename = trim(filenamein)//"_mem"//charnanal
     dseti = open_dataset(filename)
     if (iret /= 0) then
       print *,'error opening ',trim(filename)
       call MPI_Abort(MPI_COMM_WORLD,98,iret)
       stop
     endif
     filename = trim(filenameout)//"_mem"//charnanal
     dseto = create_dataset(filename, dset_enkfanal, copy_vardata=.true.,&
             errcode=iret)
     if (iret /= 0) then
       print *,'error opening ',trim(filename)
       call MPI_Abort(MPI_COMM_WORLD,98,iret)
       stop
     endif
     allocate(inf_2d(nlons,nlats),fsprd_2d(nlons,nlats),asprd_2d(nlons,nlats))
     allocate(inf_3d(nlons,nlats,nlevs),fsprd_3d(nlons,nlats,nlevs),asprd_3d(nlons,nlats,nlevs))

     do nvar=1,dset_varfg%nvars
        ndims = dset_varfg%variables(nvar)%ndims
        if (trim(dset_varfg%variables(nvar)%name) == 'spfh' .or. &
            trim(dset_varfg%variables(nvar)%name) == 'o3mr' .or. &
            trim(dset_varfg%variables(nvar)%name) == 'clwmr' .or. &
            trim(dset_varfg%variables(nvar)%name) == 'icmr') then
              tracer = .true.
        else
              tracer = .false.
        endif
        if (ndims > 2) then
            if (ndims == 3 .and. trim(dset_varfg%variables(nvar)%name) /= 'hgtsfc') then
               ! pressfc
               call read_vardata(dset_varfg,trim(dset_varfg%variables(nvar)%name),values_2d_varfg)
               call read_vardata(dset_enkffg,trim(dset_enkffg%variables(nvar)%name),values_2d_enkffg)
               if (alpha>0) call read_vardata(dset_varanal,trim(dset_varfg%variables(nvar)%name),values_2d_varanal)
               call read_vardata(dset_enkfanal,trim(dset_enkffg%variables(nvar)%name),values_2d_enkfanal)
               call read_vardata(dseti,trim(dset_enkffg%variables(nvar)%name),values_2d)
               ! blended analysis
               values_2d_anal = values_2d_enkffg + beta*(values_2d_enkfanal-values_2d_enkffg)
               if (alpha > 0) &
               values_2d_anal = values_2d_anal + alpha*(values_2d_varanal-values_2d_varfg) 
               ! recentered ensemble member
               if (rtps > 0) then ! RTPS inflation
                  call read_vardata(dset_fsprd,trim(dset_enkffg%variables(nvar)%name),fsprd_2d)
                  call read_vardata(dset_asprd,trim(dset_enkffg%variables(nvar)%name),asprd_2d)
                  fsprd_2d = max(fsprd_2d,tiny(fsprd_2d))
                  asprd_2d = max(asprd_2d,tiny(asprd_2d))
                  inf_2d = rtps*((fsprd_2d-asprd_2d)/asprd_2d) + 1.0
                  do j=1,nlats
                  do i=1,nlons
                      inf_2d(i,j) = max(infmin,min(inf_2d(i,j),infmax))
                  enddo
                  enddo
                  values_2d = inf_2d*(values_2d - values_2d_enkfanal) + values_2d_anal
                  if (mype == 0) &
                  print *,'min/max ',trim(dset_enkffg%variables(nvar)%name),&
                  ' inflation = ',minval(inf_2d),maxval(inf_2d)
               else
                  values_2d = values_2d - values_2d_enkfanal + values_2d_anal
               endif
               if (has_attr(dset_enkffg, 'nbits', trim(dset_enkffg%variables(nvar)%name))) then
                   call read_attribute(dset_enkffg, 'nbits', nbits, &
                        trim(dset_enkffg%variables(nvar)%name),errcode=iret)
               else
                   iret = 1
               endif
               if (mype == 0 .and. alpha > 0) then ! write out blended analysis on root task
                  if (iret == 0 .and. nbits > 0) then
                    values_2d_tmp = values_2d_anal
                    call quantize_data(values_2d_tmp, values_2d_anal, nbits, compress_err)
                    call write_attribute(dset_blendanal,&
                    'max_abs_compression_error',compress_err,trim(dset_enkffg%variables(nvar)%name))
                  endif
                  call write_vardata(dset_blendanal,trim(dset_enkffg%variables(nvar)%name),values_2d_anal)
               endif
               if (iret == 0 .and. nbits > 0) then
                 values_2d_tmp = values_2d
                 call quantize_data(values_2d_tmp, values_2d, nbits, compress_err)
                 call write_attribute(dseto,&
                 'max_abs_compression_error',compress_err,trim(dset_enkffg%variables(nvar)%name))
               endif
               if (tracer) then
                 if (cliptracers)  where (values_2d < clip) values_2d = clip
                 if (mype == 0) &
                 print *,'clipping ',trim(dset_enkffg%variables(nvar)%name)
               endif
               call write_vardata(dseto,trim(dset_enkffg%variables(nvar)%name),values_2d)
            else if (ndims == 4) then
               call read_vardata(dset_varfg,trim(dset_varfg%variables(nvar)%name),values_3d_varfg)
               call read_vardata(dset_enkffg,trim(dset_enkffg%variables(nvar)%name),values_3d_enkffg)
               if (alpha>0) call read_vardata(dset_varanal,trim(dset_varfg%variables(nvar)%name),values_3d_varanal)
               call read_vardata(dset_enkfanal,trim(dset_enkffg%variables(nvar)%name),values_3d_enkfanal)
               call read_vardata(dseti,trim(dset_enkffg%variables(nvar)%name),values_3d)
               ! blended analysis
               values_3d_anal = values_3d_enkffg + beta*(values_3d_enkfanal-values_3d_enkffg) 
               if (alpha > 0)  &
               values_3d_anal = values_3d_anal + alpha*(values_3d_varanal-values_3d_varfg) 
               ! recentered ensemble member
               if (rtps > 0) then ! RTPS inflation
                  call read_vardata(dset_fsprd,trim(dset_enkffg%variables(nvar)%name),fsprd_3d)
                  call read_vardata(dset_asprd,trim(dset_enkffg%variables(nvar)%name),asprd_3d)
                  fsprd_3d = max(fsprd_3d,tiny(fsprd_3d))
                  asprd_3d = max(asprd_3d,tiny(asprd_3d))
                  inf_3d = rtps*((fsprd_3d-asprd_3d)/asprd_3d) + 1.0
                  do k=1,nlevs
                  do j=1,nlats
                  do i=1,nlons
                      inf_3d(i,j,k) = max(infmin,min(inf_3d(i,j,k),infmax))
                  enddo
                  enddo
                  enddo
                  values_3d = inf_3d*(values_3d - values_3d_enkfanal) + values_3d_anal
                  if (mype == 0) &
                  print *,'min/max ',trim(dset_enkffg%variables(nvar)%name),&
                  ' inflation = ',minval(inf_3d),maxval(inf_3d)
               else
                  values_3d = values_3d - values_3d_enkfanal + values_3d_anal
               endif
               if (has_attr(dset_enkffg, 'nbits', trim(dset_enkffg%variables(nvar)%name))) then
                   call read_attribute(dset_enkffg, 'nbits', nbits, &
                        trim(dset_enkffg%variables(nvar)%name),errcode=iret)
               else
                   iret = 1
               endif
               if (mype == 0 .and. alpha > 0) then ! write out blended analysis on root task
                  if (iret == 0 .and. nbits > 0) then
                    values_3d_tmp = values_3d_anal
                    call quantize_data(values_3d_tmp, values_3d_anal, nbits, compress_err)
                    call write_attribute(dset_blendanal,&
                    'max_abs_compression_error',compress_err,trim(dset_enkffg%variables(nvar)%name))
                  endif
                  call write_vardata(dset_blendanal,trim(dset_enkffg%variables(nvar)%name),values_3d_anal)
               endif
               if (iret == 0 .and. nbits > 0) then
                 values_3d_tmp = values_3d
                 call quantize_data(values_3d_tmp, values_3d, nbits, compress_err)
                 call write_attribute(dseto,&
                 'max_abs_compression_error',compress_err,trim(dset_enkffg%variables(nvar)%name))
               endif
               if (tracer) then
                 if (cliptracers)  where (values_3d < clip) values_3d = clip
                 if (mype == 0) &
                 print *,'clipping ',trim(dset_enkffg%variables(nvar)%name)
               endif
               call write_vardata(dseto,trim(dset_enkffg%variables(nvar)%name),values_3d)
            endif
        endif ! ndims > 2
     enddo  ! nvars


     if (mype == 0 .and. alpha > 0) call close_dataset(dset_blendanal)
     call close_dataset(dseti)
     call close_dataset(dseto)
     call close_dataset(dset_enkffg)
     call close_dataset(dset_varfg)
     if (alpha > 0) call close_dataset(dset_varanal)
     call close_dataset(dset_enkfanal)
     if (rtps > 0) then
        call close_dataset(dset_fsprd)
        call close_dataset(dset_asprd)
     endif
     write(6,*)'task mype=',mype,' process ',trim(filenameout)//"_mem"//charnanal,' iret=',iret

! Jump here if more mpi processors than files to process
  else
     write (6,*) 'no files to process for mpi task = ',mype
  end if  ! end if mype

100 continue
  deallocate(asprd_2d,asprd_3d)
  deallocate(fsprd_2d,fsprd_3d)
  deallocate(inf_2d,inf_3d)
  call MPI_Barrier(MPI_COMM_WORLD,iret)

  if (mype==0) call w3tage('RECENTERSIGP_HYBGAIN2')

  call MPI_Finalize(iret)
  if (mype == 0 .and. iret /= 0) then
     print *, 'MPI_Finalize error status = ',iret
  end if

END program recenterncio_hybgain
