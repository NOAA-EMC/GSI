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
  use nemsio_module, only:  nemsio_init,nemsio_open,nemsio_close
  use nemsio_module, only:  nemsio_gfile,nemsio_getfilehead,nemsio_readrec,&
       nemsio_writerec,nemsio_readrecv,nemsio_writerecv,nemsio_getrechead
  use module_ncio, only: open_dataset, create_dataset, read_attribute, &
                         Dataset, Dimension, close_dataset, has_attr, has_var, &
                         read_vardata, write_attribute, write_vardata, &
                         get_dim, quantize_data

  implicit none

  include "mpif.h"

  real,parameter:: zero=0.0_4

! Declare externals
  external :: MPI_Init, MPI_Comm_rank, MPI_Comm_size, w3tagb, getorder,&
     MPI_Barrier, MPI_Abort, w3tage, MPI_Finalize

  TYPE(SIGIO_HEAD) :: SIGHEADI,SIGHEADO,SIGHEADMI,SIGHEADMO
  TYPE(SIGIO_DATA) :: SIGDATAI,SIGDATAO,SIGDATAMI,SIGDATAMO
  logical:: nemsio, sigio, ncio, increment, quantize
  character*500 filename_meani,filename_meano,filenamein,filenameout,filename_meang
  character*3 charnanal
  character(len=4) charnin
  character(16),dimension(:),allocatable:: fieldname_di,fieldname_mi,fieldname_mo
  character(16),dimension(:),allocatable:: fieldlevtyp_di,fieldlevtyp_mi,fieldlevtyp_mo
  integer,dimension(:),allocatable:: fieldlevel_di,fieldlevel_mi,fieldlevel_mo,orderdi,ordermi
  integer nsigi,nsigo,iret,mype,mype1,npe,nanals,ierr
  integer:: nrec,latb,lonb,levs,npts,n,i,nbits,nvar,ndims,j
  real,allocatable,dimension(:):: rwork1d
  real,allocatable,dimension(:,:)   :: rwork1di,rwork1do,rwork1dmi,rwork1dmo
  real(4),allocatable, dimension(:,:) :: values_2d, values_2d_i, values_2d_mi,&
                                         values_2d_mo
  real(4),allocatable, dimension(:,:,:) :: values_3d, values_3d_i, values_3d_mi,&
                                         values_3d_mo, values_3d_mb, values_3d_anl
  real(4) compress_err

  type(nemsio_gfile) :: gfilei, gfileo, gfilemi, gfilemo
  type(Dataset) :: dseti,dseto,dsetmi,dsetmo,dsetmg
  type(Dimension) :: londim,latdim,levdim

  namelist /recenter/ incvars_to_zero
  character(len=12),dimension(10) :: incvars_to_zero !just picking 10 arbitrarily

! Initialize mpi
  call MPI_Init(ierr)

! mype is process number, npe is total number of processes.
  call MPI_Comm_rank(MPI_COMM_WORLD,mype,ierr)
  call MPI_Comm_size(MPI_COMM_WORLD,npe,ierr)

  if (mype==0) call w3tagb('RECENTERSIGP',2011,0319,0055,'NP25')

  NSIGI=21
  NSIGO=61

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

! option for increment, read in ens mean guess
  call getarg(6,filename_meang) ! background ens mean fcst


  if (mype==0) then
     write(6,*)'RECENTERSIGP:  PROCESS ',nanals,' ENSEMBLE MEMBERS'
     write(6,*)'filenamein=',trim(filenamein)
     write(6,*)'filename_meani=',trim(filename_meani)
     write(6,*)'filename_meano=',trim(filename_meano)
     write(6,*)'filenameout=',trim(filenameout)
  endif

  sigio=.false.
  nemsio=.false.
  ncio=.false.
  increment=.false.

  mype1 = mype+1
  if (mype1 <= nanals) then

     dsetmi = open_dataset(filename_meani,errcode=iret)
     if (iret == 0) then
!       this is a netCDF file but now we need to determine
!       if it is a netCDF analysis or increment
!       going to assume all increment files will have temperature increments
        if (has_var(dsetmi,'T_inc')) then
           increment = .true.
        else
           ncio = .true.
        end if
     endif

     if (.not. ncio .and. .not. increment) then
        call sigio_srohdc(nsigi,trim(filename_meani),  &
             sigheadmi,sigdatami,iret)
        if (iret == 0 ) then
           sigio = .true.
           write(6,*)'Read sigio ',trim(filename_meani),' iret=',iret
        else
           call nemsio_init(iret=iret)
           call nemsio_open(gfilemi,trim(filename_meani),'READ',iret=iret)
           if (iret == 0 ) then
              nemsio = .true.
              write(6,*)'Read nemsio ',trim(filename_meani),' iret=',iret
              call nemsio_getfilehead(gfilemi, nrec=nrec, dimx=lonb, dimy=latb, dimz=levs, iret=iret)
              write(6,*)' lonb=',lonb,' latb=',latb,' levs=',levs,' nrec=',nrec
           else
              write(6,*)'***ERROR*** ',trim(filenamein)//"_mem"//charnanal,' contains unrecognized format.  ABORT'
           endif
        endif
     endif

     if (.not.nemsio .and. .not.sigio .and. .not.ncio .and. .not. increment ) goto 100

     if (mype==0) write(6,*)'processing files with nemsio=',nemsio,' sigio=',sigio,' ncio=',ncio,' increment=',increment


     if (sigio) then
        call sigio_srohdc(nsigi,trim(filename_meano),  &
             sigheadmo,sigdatamo,iret)
        write(charnanal,'(i3.3)') mype1
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
        write(6,*)'task mype=',mype,' process ',trim(filenameout)//"_mem"//charnanal,' iret=',iret

     elseif (nemsio) then
        call nemsio_open(gfilemo,trim(filename_meano),'READ',iret=iret)
        write(charnanal,'(i3.3)') mype1
        call nemsio_open(gfilei,trim(filenamein)//"_mem"//charnanal,'READ',iret=iret)

        gfileo=gfilemo
        call nemsio_open(gfileo,trim(filenameout)//"_mem"//charnanal,'WRITE',iret=iret)

        npts=lonb*latb
        allocate(rwork1di(npts,nrec))
        allocate(rwork1dmi(npts,nrec))
        allocate(rwork1dmo(npts,nrec))
        allocate(rwork1do(npts,nrec))

        allocate(fieldname_di(nrec), fieldlevtyp_di(nrec),fieldlevel_di(nrec))
        allocate(fieldname_mi(nrec),fieldlevtyp_mi(nrec),fieldlevel_mi(nrec))
        allocate(fieldname_mo(nrec),fieldlevtyp_mo(nrec),fieldlevel_mo(nrec))
        allocate(orderdi(nrec),ordermi(nrec))

        do n=1,nrec
           call nemsio_readrec(gfilei, n,rwork1di(:,n),iret=iret) ! member analysis
           call nemsio_getrechead(gfilei,n,fieldname_di(n),fieldlevtyp_di(n),fieldlevel_di(n),iret=iret)
        end do
        do n=1,nrec
           call nemsio_readrec(gfilemi,n,rwork1dmi(:,n),iret=iret) ! ensemble mean analysis
           call nemsio_getrechead(gfilemi,n,fieldname_mi(n),fieldlevtyp_mi(n),fieldlevel_mi(n),iret=iret)
        end do
        do n=1,nrec
           call nemsio_readrec(gfilemo,n,rwork1dmo(:,n),iret=iret) ! chgres hi-res analysis
           call nemsio_getrechead(gfilemo,n,fieldname_mo(n),fieldlevtyp_mo(n),fieldlevel_mo(n),iret=iret)
        end do


        call getorder(fieldname_mo,fieldname_di,fieldlevtyp_mo,fieldlevtyp_di,fieldlevel_mo,fieldlevel_di,nrec,orderdi)
        call getorder(fieldname_mo,fieldname_mi,fieldlevtyp_mo,fieldlevtyp_mi,fieldlevel_mo,fieldlevel_mi,nrec,ordermi)

!       Recenter ensemble member about chgres hi-res analysis
        do n=1,nrec
           do i=1,npts
              rwork1do(i,n) = rwork1di(i,orderdi(n)) - rwork1dmi(i,ordermi(n)) + rwork1dmo(i,n)
           end do
        end do

!       Write recentered member analysis using ordering of chgres hi-res analysis fields
        do n=1,nrec
           call nemsio_writerec(gfileo,n,rwork1do(:,n),iret=iret)
        end do
        deallocate(rwork1di)
        deallocate(rwork1dmi)
        deallocate(rwork1dmo)
        deallocate(rwork1do)

        call nemsio_close(gfilemi,iret=iret)
        call nemsio_close(gfilemo,iret=iret)

!       Preserve orography
        allocate(rwork1d(npts))
        rwork1d=zero
        call nemsio_readrecv(gfilei,'hgt','sfc',1,rwork1d,iret=iret)
        call nemsio_writerecv(gfileo,'hgt','sfc',1,rwork1d,iret=iret)
        deallocate(rwork1d)

        call nemsio_close(gfilei,iret=iret)
        call nemsio_close(gfileo,iret=iret)
        write(6,*)'task mype=',mype,' process ',trim(filenameout)//"_mem"//charnanal,' iret=',iret

     else if (increment) then

        ! read in namelist for incvars_to_zero
        incvars_to_zero(:) = 'NONE'
        open(912,file='recenter.nml',form="formatted")
        read(912,recenter)
        close(912)

        if (mype == 0) write(6,*) 'Read netcdf increment'
        londim = get_dim(dsetmi,'lon'); lonb = londim%len
        latdim = get_dim(dsetmi,'lat'); latb = latdim%len
        levdim = get_dim(dsetmi,'lev');   levs = levdim%len
        write(charnanal,'(i3.3)') mype1
        dsetmo = open_dataset(filename_meano)
        dsetmg = open_dataset(filename_meang)
        dseti  = open_dataset(trim(filenamein)//"_mem"//charnanal)
        dseto  = create_dataset(trim(filenameout)//"_mem"//charnanal, dseti, copy_vardata=.true.)
        allocate(values_3d(lonb,latb,levs))
        do nvar=1,dseti%nvars
           ndims = dseti%variables(nvar)%ndims
           if (ndims == 3) then ! only 3D fields need to be processed
              call read_vardata(dseti,trim(dseti%variables(nvar)%name),values_3d_i)
              call read_vardata(dsetmi,trim(dseti%variables(nvar)%name),values_3d_mi)
              ! need to do select case since ges/anl and increment have different varnames
              select case (dseti%variables(nvar)%name)
              case ('u_inc')
                 call read_vardata(dsetmg,'ugrd',values_3d_mb)
                 call read_vardata(dsetmo,'ugrd',values_3d_anl)
              case ('v_inc')
                 call read_vardata(dsetmg,'vgrd',values_3d_mb)
                 call read_vardata(dsetmo,'vgrd',values_3d_anl)
              case ('delp_inc')
                 call read_vardata(dsetmg,'dpres',values_3d_mb)
                 call read_vardata(dsetmo,'dpres',values_3d_anl)
              case ('delz_inc')
                 call read_vardata(dsetmg,'delz',values_3d_mb)
                 call read_vardata(dsetmo,'delz',values_3d_anl)
              case ('T_inc')
                 call read_vardata(dsetmg,'tmp',values_3d_mb)
                 call read_vardata(dsetmo,'tmp',values_3d_anl)
              case ('sphum_inc')
                 call read_vardata(dsetmg,'spfh',values_3d_mb)
                 call read_vardata(dsetmo,'spfh',values_3d_anl)
              case ('liq_wat_inc')
                 call read_vardata(dsetmg,'clwmr',values_3d_mb)
                 call read_vardata(dsetmo,'clwmr',values_3d_anl)
              case ('o3mr_inc')
                 call read_vardata(dsetmg,'o3mr',values_3d_mb)
                 call read_vardata(dsetmo,'o3mr',values_3d_anl)
              case ('icmr_inc')
                 call read_vardata(dsetmg,'icmr',values_3d_mb)
                 call read_vardata(dsetmo,'icmr',values_3d_anl)
              end select
              values_3d(:,:,:) = zero
              do j=1,latb
! updated member increment = original member increment - background ens mean - original
! mean increment + gsi control analysis
! = original member increment + gsi control analysis - enkf mean analysis
                 values_3d(:,j,:) = values_3d_i(:,j,:) - values_3d_mb(:,latb-j+1,:) - values_3d_mi(:,j,:) + values_3d_anl(:,latb-j+1,:)
              end do
              if (should_zero_increments_for(trim(dseti%variables(nvar)%name))) values_3d = zero
              call write_vardata(dseto,trim(dseti%variables(nvar)%name),values_3d)
           end if
        end do
        deallocate(values_3d,values_3d_i,values_3d_mi,values_3d_mb,values_3d_anl)
        call write_attribute(dseto,'comment','recentered analysis increment using recentersigp') 
        call close_dataset(dsetmi)
        call close_dataset(dsetmo)
        call close_dataset(dsetmg)
        call close_dataset(dseti)
        call close_dataset(dseto)

     else if (ncio) then

        if (mype == 0) write(6,*) 'Read netcdf'
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
                  call read_vardata(dseti,trim(dseti%variables(nvar)%name),values_2d_i)
                  call read_vardata(dsetmi,trim(dseti%variables(nvar)%name),values_2d_mi)
                  call read_vardata(dsetmo,trim(dseti%variables(nvar)%name),values_2d_mo)
                  values_2d = values_2d_i - values_2d_mi + values_2d_mo
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
                  call read_vardata(dseti,trim(dseti%variables(nvar)%name),values_3d_i)
                  call read_vardata(dsetmi,trim(dseti%variables(nvar)%name),values_3d_mi)
                  call read_vardata(dsetmo,trim(dseti%variables(nvar)%name),values_3d_mo)
                  values_3d = values_3d_i - values_3d_mi + values_3d_mo
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

     endif

! Jump here if more mpi processors than files to process
  else
     write (6,*) 'no files to process for mpi task = ',mype
  end if  ! end if mype

100 continue
  call MPI_Barrier(MPI_COMM_WORLD,ierr)

  if (mype1 <= nanals .and. .not.nemsio .and. .not.sigio .and. .not. ncio .and. .not. increment) then
     call MPI_Abort(MPI_COMM_WORLD,98,iret)
     stop
  endif

  if (mype==0) call w3tage('RECENTERSIGP')

  call MPI_Finalize(ierr)
  if (mype .eq. 0 .and. ierr .ne. 0) then
     print *, 'MPI_Finalize error status = ',ierr
  end if

contains

  !! Is this variable in incvars_to_zero?
  logical function should_zero_increments_for(check_var)

    character(len=*), intent(in) :: check_var !! Variable to search for

    ! Local variables

    character(len=12) :: varname ! temporary string for storing variable names
    integer :: i ! incvars_to_zero loop index

    should_zero_increments_for=.false.

    zeros_loop: do i=1,size(incvars_to_zero)
       varname = incvars_to_zero(i)
       if ( trim(varname) == check_var ) then
          should_zero_increments_for=.true.
          return
       endif
    end do zeros_loop

  end function should_zero_increments_for

END program recentersigp

subroutine getorder(flnm1,flnm2,fllevtyp1,fllevtyp2,fllev1,fllev2,nrec,order)
  integer nrec
  character(16):: flnm1(nrec),flnm2(nrec),fllevtyp1(nrec),fllevtyp2(nrec)
  integer ::  fllev1(nrec),fllev2(nrec)
  integer, intent(out) ::  order(nrec)

  integer i,j

  order=0
  do i=1,nrec
     doloopj: do j=1,nrec
        if(flnm1(i)==flnm2(j).and.fllevtyp1(i)==fllevtyp2(j).and.fllev1(i)==fllev2(j)) then
           order(i)=j
           exit doloopj
        endif
     enddo doloopj
  enddo

end subroutine getorder
