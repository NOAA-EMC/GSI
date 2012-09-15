module gsi_nemsio_mod
!$$$   module documentation block
!             .      .    .                                       .
! module:     gsi_nemsio_mod
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added module doc block
!
! subroutines included:
!   sub gsi_nemsio_open
!   sub gsi_nemsio_update
!   sub gsi_nemsio_close
!   sub gsi_nemsio_read
!   sub gsi_nemsio_write
!
! variable definitions:
!
! attributes:
!   langauge: f90
!   machine:
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind,r_single
  use nemsio_module, only: nemsio_gfile
  use gridmod, only: nlon_regional,nlat_regional
  implicit none

  type(nemsio_gfile) :: gfile
  save gfile

  real(r_single),allocatable::work_saved(:)

! set default to private
  private
! set subroutines to public
  public :: gsi_nemsio_open
  public :: gsi_nemsio_update
  public :: gsi_nemsio_close
  public :: gsi_nemsio_read
  public :: gsi_nemsio_write

contains

  subroutine gsi_nemsio_open(file_name,iostatus,message,mype,mype_io)
!$$$  subprogram documentation block
!                .      .    .                                        .
! subprogram:    gsi_nemsio_open
!   pgrmmr:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!
!   input argument list:
!    file_name
!    iostatus
!    message
!    mype     - mpi task id
!    mype_io
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
    use nemsio_module, only: nemsio_init,nemsio_open
    implicit none

    character(*)   ,intent(in   ) :: file_name        !  input file name
    character(*)   ,intent(in   ) :: iostatus         !  'READ' for read only, 'rdwr' for read/write
    character(*)   ,intent(in   ) :: message          !  info to appear in write statement on status of file open
    integer(i_kind),intent(in   ) :: mype,mype_io

    integer(i_kind) iret

    if(mype==mype_io) then
       call nemsio_init(iret=iret)
       if(iret/=0) then
          write(6,*)trim(message),'  problem with nemsio_init, Status = ',iret
          call stop2(74)
       end if
       call nemsio_open(gfile,file_name,trim(iostatus),iret=iret)
       if(iret/=0) then
          write(6,*)trim(message),'  problem opening file',trim(file_name),', Status = ',iret
          call stop2(74)
       end if
    end if
    allocate(work_saved(nlon_regional*nlat_regional))

  end subroutine gsi_nemsio_open

  subroutine gsi_nemsio_update(file_name,message,mype,mype_io)
!$$$  subprogram documentation block
!                .      .    .                                        .
! subprogram:    gsi_nemsio_update
!   pgrmmr:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!
!   input argument list:
!    file_name
!    message
!    mype     - mpi task id
!    mype_io
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
    use nemsio_module, only: nemsio_init,nemsio_open,nemsio_getfilehead,nemsio_close,nemsio_setheadvar
    use nemsio_module, only: nemsio_getheadvar
    use constants, only: zero
    use regional_io, only: preserve_restart_date
    implicit none

    character(*)   ,intent(in   ) :: file_name        !  input file name
    character(*)   ,intent(in   ) :: message          !  info to appear in write statement on status of file open
    integer(i_kind),intent(in   ) :: mype,mype_io

    integer(i_kind) iret,nrec
    integer(i_kind) idate(7),jdate(7),nfhour,nfminute,nfsecondn,nfday,ihrst,idat(3)
    integer(i_kind),dimension(8):: ida,jda
    real(r_kind),dimension(5):: fha
    integer(i_kind) im,jm,lm,nfsecondd,nframe,ntrac,nsoil,nmeta,ntimestep
    logical extrameta
    character(4) gdatatype,modelname
    character(32) gtype

    if(mype==mype_io) then
       call nemsio_init(iret=iret)
       if(iret/=0) then
          write(6,*)trim(message),'  problem with nemsio_init, Status = ',iret
          call stop2(74)
       end if
       call nemsio_open(gfile,file_name,'RDWR',iret=iret)
       if(iret/=0) then
          write(6,*)trim(message),'  problem opening file',trim(file_name),', Status = ',iret
          call stop2(74)
       end if
       call nemsio_getheadvar(gfile,'idat',idat,iret)
       write(6,*)' check old idat after getheadvar, idat,iret=',idat,iret
       call nemsio_getheadvar(gfile,'ihrst',ihrst,iret)
       write(6,*)' check old ihrst after getheadvar, ihrst,iret=',ihrst,iret
       call nemsio_getheadvar(gfile,'ntimestep',ntimestep,iret)
       write(6,*)' check old ntimestep after getheadvar, ntimestep,iret=',ntimestep,iret
       call nemsio_getfilehead(gfile,iret=iret,nrec=nrec,dimx=im,dimy=jm, &
         dimz=lm,idate=idate,gdatatype=gdatatype,gtype=gtype,modelname=modelname, &
         nfhour=nfhour,nfminute=nfminute,nfsecondn=nfsecondn,nfsecondd=nfsecondd, &
         nfday=nfday, &
         nframe=nframe,ntrac=ntrac,nsoil=nsoil,extrameta=extrameta,nmeta=nmeta)
       write(6,*)' at 3.1 in gsi_nemsio_update, iret,nrec=',iret,nrec         ! debug
       write(6,*)' at 3.1 in gsi_nemsio_update, dimxyz=',im,jm,lm             ! debug
       write(6,*)' at 3.1 in gsi_nemsio_update, idate =',idate                ! debug
       write(6,*)' at 3.1 in gsi_nemsio_update, gdatatype=',gdatatype         ! debug
       write(6,*)' at 3.1 in gsi_nemsio_update, gtype=',gtype                 ! debug
       write(6,*)' at 3.1 in gsi_nemsio_update, modelname=',modelname         ! debug
       write(6,*)' at 3.1 in gsi_nemsio_update, nfhour,min=',nfhour,nfminute  ! debug
       write(6,*)' at 3.1 in gsi_nemsio_update, nfday='   ,nfday              ! debug
       write(6,*)' at 3.1 in gsi_nemsio_update, nfsec,secd=',nfsecondn,nfsecondd ! debug
       write(6,*)' at 3.1 in gsi_nemsio_update, nframe,ntrac=',nframe,ntrac   ! debug
       write(6,*)' at 3.1 in gsi_nemsio_update, nsoil,nmeta=',nsoil,nmeta     ! debug
       write(6,*)' at 3.1 in gsi_nemsio_update, extrameta=',extrameta         ! debug
 
       write(6,*)' in gsi_nemsio_update, guess yr,mn,dy,hr,fhr=',idate(1:4),nfhour
       fha=zero ; ida=0 ; jda=0
       fha(2)=nfhour
       ida(1)=idate(1)    !  year
       ida(2)=idate(2)    !  month
       ida(3)=idate(3)    !  day
       ida(4)=0       !  time zone
       ida(5)=idate(4)    !  hour
       call w3movdat(fha,ida,jda)
       jdate(1)=jda(1)    !  new year
       jdate(2)=jda(2)    !  new month
       jdate(3)=jda(3)    !  new day
       jdate(4)=jda(5)    !  new hour
       jdate(5)=0     !  new minute
       jdate(6)=0     !  new scaled seconds
       jdate(7)=idate(7)  !  new seconds multiplier
       nfhour=0       !  new forecast hour
       nfminute=0
       nfsecondn=0
       ntimestep=0

       if(.not.preserve_restart_date) then

          call nemsio_setheadvar(gfile,'idate',jdate,iret)
          write(6,*)' after setheadvar, jdate,iret=',jdate,iret
          call nemsio_setheadvar(gfile,'nfhour',nfhour,iret)
          write(6,*)' after setheadvar, nfhour,iret=',nfhour,iret
          call nemsio_setheadvar(gfile,'nfminute',nfminute,iret)
          write(6,*)' after setheadvar, nfminute,iret=',nfminute,iret
          call nemsio_setheadvar(gfile,'nfsecondn',nfsecondn,iret)
          write(6,*)' after setheadvar, nfsecondn,iret=',nfsecondn,iret

!                  
          idat(3)=jdate(1)       !  forecast starting year
          idat(2)=jdate(2)       !  forecast starting month
          idat(1)=jdate(3)       !  forecast starting day  
          ihrst=jdate(4)         !  forecast starting hour (0-23)
          call nemsio_setheadvar(gfile,'idat',idat,iret)
          write(6,*)' after setheadvar, idat,iret=',idat,iret
          call nemsio_setheadvar(gfile,'ihrst',ihrst,iret)
          write(6,*)' after setheadvar, ihrst,iret=',ihrst,iret
          call nemsio_setheadvar(gfile,'ntimestep',ntimestep,iret)
          write(6,*)' after setheadvar, ntimestep,iret=',ntimestep,iret
 
       end if
    

!                        Following is diagnostic to check if date updated:

       call nemsio_getfilehead(gfile,iret=iret,nrec=nrec,dimx=im,dimy=jm, &
         dimz=lm,idate=idate,gdatatype=gdatatype,gtype=gtype,modelname=modelname, &
         nfhour=nfhour,nfminute=nfminute,nfsecondn=nfsecondn,nfsecondd=nfsecondd, &
         nfday=nfday, &
         nframe=nframe,ntrac=ntrac,nsoil=nsoil,extrameta=extrameta,nmeta=nmeta)
       write(6,*)' at 9.1 in gsi_nemsio_update, iret,nrec=',iret,nrec         ! debug
       write(6,*)' at 9.1 in gsi_nemsio_update, dimxyz=',im,jm,lm             ! debug
       write(6,*)' at 9.1 in gsi_nemsio_update, idate =',idate                ! debug
       write(6,*)' at 9.1 in gsi_nemsio_update, gdatatype=',gdatatype         ! debug
       write(6,*)' at 9.1 in gsi_nemsio_update, gtype=',gtype                 ! debug
       write(6,*)' at 9.1 in gsi_nemsio_update, modelname=',modelname         ! debug
       write(6,*)' at 9.1 in gsi_nemsio_update, nfhour,min=',nfhour,nfminute  ! debug
       write(6,*)' at 9.1 in gsi_nemsio_update, nfday=',nfday                 ! debug
       write(6,*)' at 9.1 in gsi_nemsio_update, nfsec,secd=',nfsecondn,nfsecondd ! debug
       write(6,*)' at 9.1 in gsi_nemsio_update, nframe,ntrac=',nframe,ntrac   ! debug
       write(6,*)' at 9.1 in gsi_nemsio_update, nsoil,nmeta=',nsoil,nmeta     ! debug
       write(6,*)' at 9.1 in gsi_nemsio_update, extrameta=',extrameta         ! debug
       write(6,*)' in gsi_nemsio_update, analysis yr,mn,dy,hr,fhr=',idate(1:4),nfhour
       call nemsio_getheadvar(gfile,'idat',idat,iret)
       write(6,*)' check new idat after getheadvar, idat,iret=',idat,iret
       call nemsio_getheadvar(gfile,'ihrst',ihrst,iret)
       write(6,*)' check new ihrst after getheadvar, ihrst,iret=',ihrst,iret
       call nemsio_getheadvar(gfile,'ntimestep',ntimestep,iret)
       write(6,*)' check new ntimestep after getheadvar, ntimestep,iret=',ntimestep,iret
       call nemsio_close(gfile,iret=iret)
       if(preserve_restart_date) write(6,*)' RESTART DATE PRESERVED FOR SHORT FORECASTS'
       if(iret/=0) then
          write(6,*)trim(message),'  problem closing file',trim(file_name),', Status = ',iret
          call stop2(74)
       end if
      
    end if

  end subroutine gsi_nemsio_update

  subroutine gsi_nemsio_close(file_name,message,mype,mype_io)
!$$$  subprogram documentation block
!                .      .    .                                        .
! subprogram:    gsi_nemsio_close
!   pgrmmr:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!
!   input argument list:
!    file_name
!    message
!    mype     - mpi task id
!    mype_io
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    use nemsio_module, only: nemsio_close
    implicit none

    character(*)   ,intent(in   ) :: file_name        !  input file name
    character(*)   ,intent(in   ) :: message          !  info to appear in write statement on status of file open
    integer(i_kind),intent(in   ) :: mype,mype_io

    integer(i_kind) iret

    if(mype==mype_io) then
       call nemsio_close(gfile,iret=iret)
       if(iret/=0) then
          write(6,*)trim(message),'  problem closing file',trim(file_name),', Status = ',iret
          call stop2(74)
       end if
    end if
    deallocate(work_saved)

  end subroutine gsi_nemsio_close

  subroutine gsi_nemsio_read(varname,vartype,gridtype,lev,var,mype,mype_io,good_var)
!$$$  subprogram documentation block
!                .      .    .                                        .
! subprogram:    gsi_nemsio_read
!   pgrmmr: parrish
!
! abstract:  intermediate level routine to read nmmb model fields using nems_io. 
!             the desired field is retrieved from the previously opened file as a
!             full 2d horizontal field, then interpolated to the analysis grid
!             from the nmmb model grid.  finally, the 2d field is scattered from
!             processor mype_io to subdomains in output array var. 
!             a copy of the original field on the nmmb grid is saved internally in array
!             work_saved in case this field is to be updated by the analysis
!             increment in a call to gsi_nemsio_write immediately after the call to
!             gsi_nemsio_read.
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!   2010-01-22  parrish - added optional variable good_var to detect read errors in calling program
!                            and have option to avoid program stop.
!
!   input argument list:
!    varname,vartype,gridtype - descriptors for variable to be retrieved from nmmb file
!    lev                      - vertical level number
!    mype     - mpi task id
!    mype_io  - mpi task where field is read from disk
!    good_var - optional, on input, set to .false.  if present(good_var) then error stop is
!                bypassed and good_var is returned .true. for successful read, .false. otherwise.
!
!   output argument list:
!    var      - for successful read, contains desired variable on subdomains.
!    good_var - see above
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    use mpimod, only:        mpi_rtype,mpi_comm_world,ierror,mpi_integer4
    use gridmod, only:       lat2,lon2,nlon,nlat
    use gridmod, only:       ijn_s,displs_s,itotsub,ltosi_s,ltosj_s
    use nemsio_module, only: nemsio_readrecv
    use mod_nmmb_to_a, only: nmmb_h_to_a,nmmb_v_to_a
    implicit none

    character(*)   ,intent(in   ) :: varname,vartype,gridtype      ! gridtype='H' or 'V'
    integer(i_kind),intent(in   ) :: lev              !   vertical level of desired variable
    real(r_kind)   ,intent(  out) :: var(lat2*lon2)
    integer(i_kind),intent(in   ) :: mype,mype_io
    logical,optional,intent(inout):: good_var

    integer(i_kind) i,iret,j,mm1,n
    real(r_kind) work(itotsub)
    real(r_kind) work_a(nlat,nlon)
    real(r_single) work_b(nlon_regional*nlat_regional)
    logical good_var_loc

    mm1=mype+1

    if(mype==mype_io) then

!            read field from file with nemsio

       call nemsio_readrecv(gfile,trim(varname),trim(vartype),lev,work_b,iret=iret)
       if(iret==0) then
          work_saved=work_b

!         interpolate to analysis grid

          if(trim(gridtype)=='H') call nmmb_h_to_a(work_b,work_a)
          if(trim(gridtype)=='V') call nmmb_v_to_a(work_b,work_a)


!        scatter to subdomains

          do n=1,itotsub
             i=ltosi_s(n)
             j=ltosj_s(n)
             work(n)=work_a(i,j)
          end do
       end if
    end if
    call mpi_bcast(iret,1,mpi_integer4,mype_io,mpi_comm_world,ierror)
    good_var_loc=.true.
    if(iret/=0) then
       good_var_loc=.false.
       if(mype==0) then
          write(6,*)'  problem reading varname=',trim(varname),', vartype=',trim(vartype),', Status = ',iret
          if(.not.present(good_var)) call stop2(74)
       end if
    end if
    if(present(good_var)) good_var=good_var_loc

    if(good_var_loc) &
      call mpi_scatterv(work,ijn_s,displs_s,mpi_rtype, &
                   var,ijn_s(mm1),mpi_rtype,mype_io,mpi_comm_world,ierror)

  end subroutine gsi_nemsio_read

  subroutine gsi_nemsio_write(varname,vartype,gridtype,lev,var,mype,mype_io,add_saved)
!$$$  subprogram documentation block
!                .      .    .                                        .
! subprogram:    gsi_nemsio_write
!   pgrmmr:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!
!   input argument list:
!    varname,vartype,gridtype
!    lev
!    add_saved
!    mype     - mpi task id
!    mype_io
!
!   output argument list:
!    var
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    use mpimod, only:        mpi_rtype,mpi_comm_world,ierror
    use gridmod, only:       lat2,lon2,nlon,nlat,lat1,lon1
    use gridmod, only:       ijn,displs_g,itotsub,iglobal,ltosi,ltosj
    use nemsio_module, only: nemsio_writerecv
    use mod_nmmb_to_a, only: nmmb_a_to_h,nmmb_a_to_v
    implicit none

    character(*)   ,intent(in   ) :: varname,vartype,gridtype      ! gridtype='H' or 'V'
    integer(i_kind),intent(in   ) :: lev              !   vertical level of desired variable
    real(r_kind)   ,intent(in   ) :: var(lat2,lon2)
    integer(i_kind),intent(in   ) :: mype,mype_io
    logical        ,intent(in   ) :: add_saved

    integer(i_kind) i,iret,j,mm1,n
    real(r_kind) work(itotsub),work_sub(lat1,lon1)
    real(r_kind) work_a(nlat,nlon)
    real(r_single) work_b(nlon_regional*nlat_regional)

    mm1=mype+1

    do i=1,lon1
       do j=1,lat1
          work_sub(j,i)=var(j+1,i+1)
       end do
    end do
    call mpi_gatherv(work_sub,ijn(mm1),mpi_rtype, &
                           work,ijn,displs_g,mpi_rtype,mype_io,mpi_comm_world,ierror)
    if(mype==mype_io) then
       do n=1,iglobal
          i=ltosi(n)
          j=ltosj(n)
          work_a(i,j)=work(n)
       end do
       if(trim(gridtype)=='H') call nmmb_a_to_h(work_a,work_b)
       if(trim(gridtype)=='V') call nmmb_a_to_v(work_a,work_b)
       if(add_saved) work_b=work_b+work_saved
       call nemsio_writerecv(gfile,trim(varname),trim(vartype),lev,work_b,iret=iret)
       if(iret/=0) then
          write(6,*)'  problem writing varname=',trim(varname),', vartype=',trim(vartype),', Status = ',iret
          call stop2(74)
       end if
    end if

  end subroutine gsi_nemsio_write

end module gsi_nemsio_mod
