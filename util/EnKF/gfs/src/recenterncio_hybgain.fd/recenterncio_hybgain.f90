program recenterncio_hybgain
!$$$  main program documentation block
!
! program:  recenterncio_hybgain               recenter
!
! prgmmr: whitaker         org: esrl/psd               date: 2009-02-23
!
! abstract:  Recenter ensemble analysis files about new
!            mean, computed from blended 3DVar and EnKF increments.
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

  use module_fv3gfs_ncio, only: open_dataset, create_dataset, read_attribute, &
                           Dataset, Dimension, close_dataset, has_attr, &
                           read_vardata, write_attribute, write_vardata, &
                           get_dim, quantize_data

  implicit none

  include "mpif.h"

  character*500 filename_fg,filename_varanal,filename_enkfanal,filenamein,&
                filenameout,filename_anal,filename
  character*3 charnanal
  character(len=4) charnin
  integer mype,mype1,npe,nanals,iret,ialpha,ibeta
  integer:: nlats,nlons,nlevs,nvar,ndims,nbits
  real alpha,beta
  real(4),allocatable,dimension(:,:) :: values_2d_varanal,values_2d_enkfanal,values_2d_fg,values_2d_anal,&
                                        values_2d_tmp, values_2d
  real(4),allocatable,dimension(:,:,:) :: values_3d_varanal,values_3d_enkfanal,values_3d_fg,values_3d_anal,&
                                          values_3d_tmp, values_3d
  real(4) compress_err
  type(Dataset) :: dseti,dseto,dset_anal,dset_fg,dset_varanal,dset_enkfanal
  type(Dimension) :: londim,latdim,levdim

! Initialize mpi
  call MPI_Init(iret)

! mype is process number, npe is total number of processes.
  call MPI_Comm_rank(MPI_COMM_WORLD,mype,iret)
  call MPI_Comm_size(MPI_COMM_WORLD,npe,iret)

  if (mype==0) call w3tagb('RECENTERNCIO_HYBGAIN',2011,0319,0055,'NP25')

  call getarg(1,filename_fg)    ! first guess ensmean background netcdf file
  call getarg(2,filename_varanal) ! 3dvar analysis
  call getarg(3,filename_enkfanal) ! enkf mean analysis
  call getarg(4,filename_anal)  ! blended analysis (to recenter ensemble around)
  call getarg(5,filenamein) ! prefix for input ens member files (append _mem###)
  call getarg(6,filenameout) ! prefix for output ens member files (append _mem###)
! blending coefficients
  call getarg(7,charnin)
  read(charnin,'(i4)') ialpha ! wt for varanal (3dvar)
  alpha = ialpha/1000.
  call getarg(8,charnin)
  read(charnin,'(i4)') ibeta ! wt for enkfanal (enkf)
  beta = ibeta/1000.
! new_anal = fg + alpha*(varanal-fg) + beta(enkfanal-fg)
!          = (1.-alpha-beta)*fg + alpha*varanal + beta*enkfanal
! how many ensemble members to process
  call getarg(9,charnin)
  read(charnin,'(i4)') nanals

  if (mype==0) then
     write(6,*)'RECENTERNCIO_HYBGAIN:  PROCESS ',nanals,' ENSEMBLE MEMBERS'
     write(6,*)'ens mean background in ',trim(filename_fg)
     write(6,*)'3dvar analysis in ',trim(filename_varanal)
     write(6,*)'EnKF mean analysis in ',trim(filename_enkfanal)
     write(6,*)'Blended mean analysis to be written to ',trim(filename_anal)
     write(6,*)'Prefix for member input files ',trim(filenamein)
     write(6,*)'Prefix for member output files ',trim(filenameout)
     write(6,*)'3dvar weight, EnKF weight =',alpha,beta
  endif

  mype1 = mype+1
  if (mype1 <= nanals) then
     dset_fg = open_dataset(filename_fg,errcode=iret)
     if (iret == 0 ) then
        if (mype == 0) write(6,*)'Read netcdf ',trim(filename_fg)
        londim = get_dim(dset_fg,'grid_xt'); nlons = londim%len
        latdim = get_dim(dset_fg,'grid_yt'); nlats = latdim%len
        levdim = get_dim(dset_fg,'pfull');   nlevs = levdim%len
        if (mype == 0) write(6,*)' nlons=',nlons,' nlats=',nlats,' nlevs=',nlevs
     else
        write(6,*) 'error opening ',trim(filename_fg)
        call MPI_Abort(MPI_COMM_WORLD,98,iret)
        stop
     endif

     ! readin in 3dvar, enkf analyses, plus ens mean background, blend
     dset_varanal = open_dataset(filename_varanal,errcode=iret)
     if (iret /= 0) then
       print *,'error opening ',trim(filename_varanal)
       call MPI_Abort(MPI_COMM_WORLD,98,iret)
       stop
     endif
     dset_enkfanal = open_dataset(filename_enkfanal,errcode=iret)
     if (iret /= 0) then
       print *,'error opening ',trim(filename_enkfanal)
       call MPI_Abort(MPI_COMM_WORLD,98,iret)
       stop
     endif
     if (mype == 0) then
        dset_anal = create_dataset(filename_anal, dset_enkfanal, &
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

     do nvar=1,dset_fg%nvars
        ndims = dset_fg%variables(nvar)%ndims
        if (ndims > 2) then
            if (ndims == 3 .and. trim(dset_fg%variables(nvar)%name) /= 'hgtsfc') then
               ! pressfc
               call read_vardata(dset_fg,trim(dset_fg%variables(nvar)%name),values_2d_fg)
               call read_vardata(dset_varanal,trim(dset_fg%variables(nvar)%name),values_2d_varanal)
               call read_vardata(dset_enkfanal,trim(dset_fg%variables(nvar)%name),values_2d_enkfanal)
               call read_vardata(dseti,trim(dset_fg%variables(nvar)%name),values_2d)
               ! blended analysis
               values_2d_anal = (1.-alpha-beta)*values_2d_fg + &
                            alpha*values_2d_varanal + &
                            beta*values_2d_enkfanal
               ! recentered ensemble member
               values_2d = values_2d - values_2d_enkfanal + values_2d_anal
               if (has_attr(dset_fg, 'nbits', trim(dset_fg%variables(nvar)%name))) then
                   call read_attribute(dset_fg, 'nbits', nbits, &
                        trim(dset_fg%variables(nvar)%name),errcode=iret)
               else
                   iret = 1
               endif
               if (mype == 0) then ! write out blended analysis on root task
                  if (iret == 0 .and. nbits > 0) then
                    values_2d_tmp = values_2d_anal
                    call quantize_data(values_2d_tmp, values_2d_anal, nbits, compress_err)
                    call write_attribute(dset_anal,&
                    'max_abs_compression_error',compress_err,trim(dset_fg%variables(nvar)%name))
                  endif
                  call write_vardata(dset_anal,trim(dset_fg%variables(nvar)%name),values_2d_anal)
               endif
               if (iret == 0 .and. nbits > 0) then
                 values_2d_tmp = values_2d
                 call quantize_data(values_2d_tmp, values_2d, nbits, compress_err)
                 call write_attribute(dseto,&
                 'max_abs_compression_error',compress_err,trim(dset_fg%variables(nvar)%name))
               endif
               call write_vardata(dseto,trim(dset_fg%variables(nvar)%name),values_2d)
            else if (ndims == 4) then
               call read_vardata(dset_fg,trim(dset_fg%variables(nvar)%name),values_3d_fg)
               call read_vardata(dset_varanal,trim(dset_fg%variables(nvar)%name),values_3d_varanal)
               call read_vardata(dset_enkfanal,trim(dset_fg%variables(nvar)%name),values_3d_enkfanal)
               call read_vardata(dseti,trim(dset_fg%variables(nvar)%name),values_3d)
               ! blended analysis
               values_3d_anal = (1.-alpha-beta)*values_3d_fg + &
                            alpha*values_3d_varanal + &
                            beta*values_3d_enkfanal
               ! recentered ensemble member
               values_3d = values_3d - values_3d_enkfanal + values_3d_anal
               if (has_attr(dset_fg, 'nbits', trim(dset_fg%variables(nvar)%name))) then
                   call read_attribute(dset_fg, 'nbits', nbits, &
                        trim(dset_fg%variables(nvar)%name),errcode=iret)
               else
                   iret = 1
               endif
               if (mype == 0) then ! write out blended analysis on root task
                  if (iret == 0 .and. nbits > 0) then
                    values_3d_tmp = values_3d_anal
                    call quantize_data(values_3d_tmp, values_3d_anal, nbits, compress_err)
                    call write_attribute(dset_anal,&
                    'max_abs_compression_error',compress_err,trim(dset_fg%variables(nvar)%name))
                  endif
                  call write_vardata(dset_anal,trim(dset_fg%variables(nvar)%name),values_3d_anal)
               endif
               if (iret == 0 .and. nbits > 0) then
                 values_3d_tmp = values_3d
                 call quantize_data(values_3d_tmp, values_3d, nbits, compress_err)
                 call write_attribute(dseto,&
                 'max_abs_compression_error',compress_err,trim(dset_fg%variables(nvar)%name))
               endif
               call write_vardata(dseto,trim(dset_fg%variables(nvar)%name),values_3d)
            endif
        endif ! ndims > 2
     enddo  ! nvars


     if (mype == 0) call close_dataset(dset_anal)
     call close_dataset(dseti)
     call close_dataset(dseto)
     call close_dataset(dset_fg)
     call close_dataset(dset_varanal)
     call close_dataset(dset_enkfanal)
     write(6,*)'task mype=',mype,' process ',trim(filenameout)//"_mem"//charnanal,' iret=',iret

! Jump here if more mpi processors than files to process
  else
     write (6,*) 'no files to process for mpi task = ',mype
  end if  ! end if mype

100 continue
  call MPI_Barrier(MPI_COMM_WORLD,iret)

  if (mype==0) call w3tage('RECENTERSIGP_HYBGAIN')

  call MPI_Finalize(iret)
  if (mype == 0 .and. iret /= 0) then
     print *, 'MPI_Finalize error status = ',iret
  end if

END program recenterncio_hybgain
