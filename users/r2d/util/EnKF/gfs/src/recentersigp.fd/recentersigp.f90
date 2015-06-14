program recentersigp
!$$$  main program documentation block
!
! program:  recentersigp               recenter
!
! prgmmr: whitaker         org: esrl/psd               date: 2009-02-23
!
! abstract:  Read NCEP GFS spectral sigma file from a file, 
!            remove mean specified from another file, add a 
!            new mean specified from a third file, and write
!            out result to a fourth file.
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
!
!$$$

  USE SIGIO_MODULE
  implicit none

  include "mpif.h"

  TYPE(SIGIO_HEAD) :: SIGHEADI,SIGHEADO,SIGHEADMI,SIGHEADMO
  TYPE(SIGIO_DATA) :: SIGDATAI,SIGDATAO,SIGDATAMI,SIGDATAMO
  character*500 filename_meani,filename_meano,filenamein,filenameout
  character*3 charnanal
  character(len=4) charnin
  integer nsigi,nsigo,iret,nproc,numproc,nanal,nanals,ierr

! Initialize mpi
  call MPI_Init(ierr)

! nproc is process number, numproc is total number of processes.
  call MPI_Comm_rank(MPI_COMM_WORLD,nproc,ierr)
  call MPI_Comm_size(MPI_COMM_WORLD,numproc,ierr)

  if (nproc==0) call w3tagb('RECENTERSIGP',2011,0319,0055,'NP25')
  
  NSIGI=21
  NSIGO=61

! read data from this file
  call getarg(1,filenamein)

! subtract this mean
  call getarg(2,filename_meani)

! then add to this mean
  call getarg(3,filename_meano)

! and put in this file.
  call getarg(4,filenameout)

! how many ensemble members to process
  call getarg(5,charnin)
  read(charnin,'(i4)') nanals
  
  if (nproc==0) then
     write(6,*)'RECENTERSIGP:  PROCESS ',nanals,' ENSEMBLE MEMBERS'
     write(6,*)'filenamein=',trim(filenamein)
     write(6,*)'filename_meani=',trim(filename_meani)
     write(6,*)'filename_meano=',trim(filename_meano)
     write(6,*)'filenameout=',trim(filenameout)
  endif

  nanal = nproc+1
  if (nanal.gt.nanals) then
     write (6,*) 'no files to process for mpi task = ',nproc
  else
     call sigio_srohdc(nsigi,trim(filename_meani),  &
          sigheadmi,sigdatami,iret)
     call sigio_srohdc(nsigi,trim(filename_meano),  &
          sigheadmo,sigdatamo,iret)

     write(charnanal,'(i3.3)') nanal
     call sigio_srohdc(nsigi,trim(filenamein)//"_mem"//charnanal,  &
          sigheadi,sigdatai,iret)

     sigheado = sigheadmo 
     call sigio_aldata(sigheado,sigdatao,iret)

     sigdatao%hs = sigdatai%hs
     sigdatao%ps = sigdatai%ps - sigdatami%ps + sigdatamo%ps
     sigdatao%t = sigdatai%t - sigdatami%t + sigdatamo%t
     sigdatao%z = sigdatai%z - sigdatami%z + sigdatamo%z
     sigdatao%d = sigdatai%d - sigdatami%d + sigdatamo%d
     sigdatao%q = sigdatai%q - sigdatami%q + sigdatamo%q

     call sigio_swohdc(nsigo,trim(filenameout)//"_mem"//charnanal,sigheado,sigdatao,iret)
     write(6,*)'task nproc=',nproc,' process ',trim(filenameout)//"_mem"//charnanal,' iret=',iret
     
  end if  ! end if mype

  call MPI_Barrier(MPI_COMM_WORLD,ierr)

  if (nproc==0) call w3tage('RECENTERSIGP')

  call MPI_Finalize(ierr)
  if (nproc .eq. 0 .and. ierr .ne. 0) then
     print *, 'MPI_Finalize error status = ',ierr
  end if

END program recentersigp
