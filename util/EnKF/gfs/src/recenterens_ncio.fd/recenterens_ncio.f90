program recenterens_ncio
!$$$  main program documentation block
!
! program:  recenterens_ncio               recenter
!
! prgmmr: whitaker         org: esrl/psd               date: 2009-02-23
!
! abstract:  Read ensemble from netcdf history files,
!            remove mean specified from another file, add a
!            new mean specified from a third file, and write
!            out result to a fourth file. 'Partial' recentering
!            by weighting old and new means.
!
! program history log:
!   2020-11-04  Initial version.
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
                         Dataset, Dimension, close_dataset, has_attr, has_var, &
                         read_vardata, write_attribute, write_vardata, &
                         get_dim, quantize_data

  implicit none

  include "mpif.h"

  real,parameter:: zero=0.0_4

  logical:: quantize
  character(len=500) filename_meani,filename_meano,filenamein,filenameout
  character(len=3) charnanal, charwgt_ensmean, charwgt_control
  character(len=4) charnin
  integer iret,mype,mype1,npe,nanals,ierr, iwgt_ensmean, iwgt_control
  integer:: latb,lonb,levs,nbits,nvar,ndims
  real(4),allocatable, dimension(:,:) :: values_2d, values_2d_i, values_2d_mi,&
                                         values_2d_mo
  real(4),allocatable, dimension(:,:,:) :: values_3d, values_3d_i, values_3d_mi,&
                                         values_3d_mo
  real(4) compress_err, rwgt_control, rwgt_ensmean

  type(Dataset) :: dseti,dseto,dsetmi,dsetmo
  type(Dimension) :: londim,latdim,levdim

! Initialize mpi
  call MPI_Init(ierr)

! mype is process number, npe is total number of processes.
  call MPI_Comm_rank(MPI_COMM_WORLD,mype,ierr)
  call MPI_Comm_size(MPI_COMM_WORLD,npe,ierr)

  if (mype==0) call w3tagb('RECENTERENS_NCIO',2011,0319,0055,'NP25')

! read data from this file
  call getarg(1,filenamein) ! increment or analysis

! subtract this mean
  call getarg(2,filename_meani)  ! mean increment or analysis

! then add to this mean
  call getarg(3,filename_meano) ! new mean analysis

! and put in this file.
  call getarg(4,filenameout) ! new increment of analysis

! how many ensemble members to process
  call getarg(5,charnin)
  read(charnin,'(i4)') nanals

! weight given to original ens mean
  call getarg(6,charwgt_ensmean)
  read(charwgt_ensmean,'(i3)') iwgt_ensmean
  rwgt_ensmean = iwgt_ensmean/100.

! weight given to new ens mean
  call getarg(7,charwgt_control)
  read(charwgt_control,'(i3)') iwgt_control
  rwgt_control = iwgt_control/100.

  if (mype==0) then
     write(6,*)'RECENTERENS_NCIO:  PROCESS ',nanals,' ENSEMBLE MEMBERS'
     write(6,*)'filenamein=',trim(filenamein)
     write(6,*)'filename_meani=',trim(filename_meani)
     write(6,*)'filename_meano=',trim(filename_meano)
     write(6,*)'filenameout=',trim(filenameout)
     write(6,*)'rwgt_ensmean,rwgt_control=',rwgt_ensmean,rwgt_control
  endif

  mype1 = mype+1
  if (mype1 <= nanals) then

     dsetmi = open_dataset(filename_meani,errcode=iret)

     londim = get_dim(dsetmi,'grid_xt'); lonb = londim%len
     latdim = get_dim(dsetmi,'grid_yt'); latb = latdim%len
     levdim = get_dim(dsetmi,'pfull');   levs = levdim%len
     write(charnanal,'(i3.3)') mype1
     dsetmo = open_dataset(filename_meano)
     dseti  = open_dataset(trim(filenamein)//"_mem"//charnanal)
     dseto  = create_dataset(trim(filenameout)//"_mem"//charnanal, dseti, copy_vardata=.true.)
     do nvar=1,dseti%nvars
        ndims = dseti%variables(nvar)%ndims
        if (ndims > 2) then
            if (ndims == 3 .and. trim(dseti%variables(nvar)%name) /= 'hgtsfc') then
               ! pressfc
               if (mype == 0) print *,'recentering ',&
                trim(dseti%variables(nvar)%name) 
               call read_vardata(dseti,trim(dseti%variables(nvar)%name),values_2d_i)
               call read_vardata(dsetmi,trim(dseti%variables(nvar)%name),values_2d_mi)
               call read_vardata(dsetmo,trim(dseti%variables(nvar)%name),values_2d_mo)
               values_2d = values_2d_i - values_2d_mi + rwgt_ensmean*values_2d_mi + rwgt_control*values_2d_mo
               if (has_attr(dseti, 'nbits', trim(dseti%variables(nvar)%name))) then
                   call read_attribute(dseti, 'nbits', nbits, &
                        trim(dseti%variables(nvar)%name))
                   quantize = .true.
                   if (nbits < 1) quantize = .false.
               else
                   quantize = .false.
               endif
               if (quantize) then
                 values_2d_mi = values_2d
                 call quantize_data(values_2d_mi, values_2d, nbits, compress_err)
                 call write_attribute(dseto,&
                 'max_abs_compression_error',compress_err,trim(dseti%variables(nvar)%name))
               endif
               call write_vardata(dseto,trim(dseti%variables(nvar)%name),values_2d)
            else if (ndims == 4) then
               if (mype == 0) print *,'recentering ',&
                trim(dseti%variables(nvar)%name) 
               call read_vardata(dseti,trim(dseti%variables(nvar)%name),values_3d_i)
               call read_vardata(dsetmi,trim(dseti%variables(nvar)%name),values_3d_mi)
               call read_vardata(dsetmo,trim(dseti%variables(nvar)%name),values_3d_mo)
               values_3d = values_3d_i - values_3d_mi + rwgt_ensmean*values_3d_mi + rwgt_control*values_3d_mo
               if (has_attr(dseti, 'nbits', trim(dseti%variables(nvar)%name))) then
                   call read_attribute(dseti, 'nbits', nbits, &
                        trim(dseti%variables(nvar)%name))
                   quantize = .true.
                   if (nbits < 1) quantize = .false.
               else
                   quantize = .false.
               endif
               if (quantize) then
                 values_3d_mi = values_3d
                 call quantize_data(values_3d_mi, values_3d, nbits, compress_err)
                 call write_attribute(dseto,&
                 'max_abs_compression_error',compress_err,trim(dseti%variables(nvar)%name))
               endif
               call write_vardata(dseto,trim(dseti%variables(nvar)%name),values_3d)
            endif
        endif ! ndims > 2
     enddo  ! nvars

     deallocate(values_2d,values_2d_i,values_2d_mi,values_2d_mo)
     deallocate(values_3d,values_3d_i,values_3d_mi,values_3d_mo)
     call close_dataset(dsetmi)
     call close_dataset(dsetmo)
     call close_dataset(dseti)
     call close_dataset(dseto)

! Jump here if more mpi processors than files to process
  else
     write (6,*) 'no files to process for mpi task = ',mype
  end if  ! end if mype

  call MPI_Barrier(MPI_COMM_WORLD,ierr)

  if (mype==0) call w3tage('RECENTERENS_NCIO')

  call MPI_Finalize(ierr)
  if (mype .eq. 0 .and. ierr .ne. 0) then
     print *, 'MPI_Finalize error status = ',ierr
  end if

END program recenterens_ncio
