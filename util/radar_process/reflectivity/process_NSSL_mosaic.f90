program process_NSSL_mosaic
!
!   PRGMMR: Ming Hu          ORG: GSD        DATE: 2007-12-17
!
! ABSTRACT: 
!     This routine read in NSSL reflectivity mosaic fiels and 
!     interpolate them into GSI mass grid
!
! PROGRAM HISTORY LOG:
!
!   variable list
!
! USAGE:
!   INPUT FILES:  mosaic_files
!
!   OUTPUT FILES:
!
! REMARKS:
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90 + EXTENSIONS
!   MACHINE:  wJET
!
!$$$
!
!_____________________________________________________________________
!
  use mpi
  use kinds, only: r_kind,i_kind

  implicit none
!
! MPI variables
  integer :: npe, mype, mypeLocal,ierror

  real     :: rad2deg = 180.0/3.1415926
!
  character*256 output_file
!
!  grid
  integer(i_kind) :: nlon,nlat
  real,allocatable:: xlon(:,:)    !
  real,allocatable:: ylat(:,:)    !
  REAL, allocatable :: ref3d(:,:,:)   ! 3D reflectivity
  REAL, allocatable :: ref0(:,:,:)   ! 3D reflectivity
  REAL, allocatable :: maxref(:,:)   ! composite reflectivity
  integer , allocatable :: imaxref(:,:)   ! composite reflectivity
  REAL(r_kind), allocatable :: ref3d_column(:,:)   ! 3D reflectivity in column
!
!  For reflectiivty mosaic
!
  CHARACTER*256   mosaicfile

  INTEGER ::   mscNlon   ! number of longitude of mosaic data
  INTEGER ::   mscNlat   ! number of latitude of mosaic data
  INTEGER ::   mscNlev   ! number of vertical levels of mosaic data
  REAL, allocatable :: msclon(:)        ! longitude of mosaic data
  REAL, allocatable :: msclat(:)        ! latitude of mosaic data
  REAL, allocatable :: msclev(:)        ! level of mosaic data
  REAL, allocatable :: mscValue(:,:)    ! reflectivity

  REAL   :: lonMin,latMin,lonMax,latMax
  REAL*8 :: dlon,dlat
!
!  4 Tile binary format
!
  integer           :: ntot, ntot2d, mt
  integer*4         :: nx,ny,nz
  integer*4         :: yr, mo, da, hr, mn, sc
  real*8            :: rdx,rdy
  real              :: rlatmax,rlonmin
  integer*4         :: var_scale

  integer*2, dimension(:),   allocatable  :: var
!
!
!  namelist files
!
  INTEGER(i_kind)  ::  tversion
  character*10 :: analysis_time
  CHARACTER*180   dataPath
  CHARACTER*180   bkfile
  namelist/setup/ tversion,analysis_time,dataPath,bkfile
  integer(i_kind)  ::  idate
!
!
!  ** misc
      
  real        ::rix  ! x-grid coordinate (grid units)
  real        ::riy  ! y-grid coordinate (grid units)
  logical     ::outside     ! .false., then point is inside x-y domain
                              ! .true.,  then point is outside x-y domain
  logical     :: fileexist

  integer i,j,k,itype,iymdh,ier,jret,ifn
  integer iz,n,nlv,isao,nflag,np,ilen,iflag,iostat

  integer :: NCID

  REAL ::  rlat,rlon
  INTEGER  :: ip,jp
  REAL ::  rip,rjp
  REAL ::  dip,djp
  REAL ::  w1,w2,w3,w4
  REAL ::  ref1,ref2,ref3,ref4,refl_ltng

  INTEGER(i_kind)  ::  maxlvl
  INTEGER(i_kind)  ::  numlvl,numref
  integer :: status
  REAL ::  rthresh_ref,rthresh_miss

!**********************************************************************
!
!            END OF DECLARATIONS....start of program
! MPI setup
  call MPI_INIT(ierror) 
  call MPI_COMM_SIZE(mpi_comm_world,npe,ierror)
  call MPI_COMM_RANK(mpi_comm_world,mype,ierror)

  write(*,*) mype, 'deal with mosaic'

  open(15, file='mosaic.namelist')
    read(15,setup)
  close(15)

  read(analysis_time,'(I10)') idate
  write(6,*) 'cycle time is :', idate

  maxlvl = 33
  rthresh_ref=-500.0
  rthresh_miss=-5000.0
!
! set lat lon 
!
  
  call nc_get_dim(bkfile,NLON,NLAT)
  write(*,*) 'NLON,NLAT',NLON,NLAT

  allocate(xlon(nlon,nlat))
  allocate(ylat(nlon,nlat))
  call nc_get_latlon(bkfile,Nlon,Nlat,ylat,xlon)
  write(*,*) 'xlon: max min',maxval(xlon),minval(xlon)
  write(*,*) 'ylat: max min',maxval(ylat),minval(ylat)
!
  mypeLocal=mype+1
  write(mosaicfile,'(a,a,I1)') trim(dataPath), '/mosaic_t',mypeLocal
!
!   deal with certain tile
!
  write(*,*) 'process tile:',trim(mosaicfile)
  fileexist=.false.
   
  open(99,file=trim(mosaicfile),form='unformatted', access='direct',&
             recl=6*4,status='old',err=225)
        rewind(99)
        read(99,rec=1,err=225) yr, mo, da, hr, mn, sc
        fileexist=.true.
225     continue
  close(99)

  if(fileexist) then
         call read_head_Mosaic4(mosaicfile,nx,ny,nz,rlonmin,rlatmax,&
                   rdx,rdy,var_scale)
         mscNlon=nx
         mscNlat=ny
         mscNlev=nz
         dlon=rdx
         dlat=rdy
         lonMin=rlonmin
         lonMax=lonMin+dlon*(mscNlon-1)
         latMax=rlatmax
         latMin=latMax-dlat*(mscNlat-1)

      if( maxlvl == mscNlev ) then
         allocate(ref3d(nlon,nlat,maxlvl))
      else
         write(*,*) 'Wrong vertical layers:', maxlvl, mscNlev
         stop 1234
      endif
      ref3d=-999.0

      allocate(msclon(mscNlon))
      allocate(msclat(mscNlat))
      allocate(msclev(mscNlev))
      allocate(mscValue(mscNlon,mscNlat))

      DO i=1,mscNlon
         msclon(i)=lonMin+(i-1)*dlon
      ENDDO
      DO i=1,mscNlat
         msclat(i)=latMin+(i-1)*dlat
      ENDDO
!
!  ingest mosaic file and interpolation
! 
      ntot = nx*ny*nz
      allocate(var(ntot))
      call read_data_Mosaic4(mosaicfile,ntot,var)
!
      DO k=1, mscNlev
!          write(*,*) mype, 'deal with level:', k,mscNlon,mscNlat
          ntot2d=nx*ny*(k-1)
          do j=1,ny
          do i=1,nx
                mscValue(i,j) = var(ntot2d+(j-1)*nx+i)
          enddo
          enddo
!             write(*,*) 'max min',k,maxval(mscValue),minval(mscValue)
          DO j=1,nlat
          DO i=1,nlon
             rlat=ylat(i,j)
             rlon=xlon(i,j)

             rip=(rlon-lonMin)/dlon+1
             rjp=(rlat-latMin)/dlat+1
             ip=int(rip)
             jp=int(rjp)
             dip=rip-ip
             djp=rjp-jp

             if( ip >= 1 .and. ip < mscNlon ) then
             if( jp >= 1 .and. jp < mscNlat ) then
! inside mosaic domain
               w1=(1.0-dip)*(1.0-djp)
               w2=dip*(1.0-djp)
               w3=dip*djp
               w4=(1.0-dip)*djp
               ref1=mscValue(ip,jp)
               ref2=mscValue(ip+1,jp)
               ref3=mscValue(ip+1,jp+1)
               ref4=mscValue(ip,jp+1)
               if(ref1 > rthresh_ref .and. ref2 > rthresh_ref .and.  &
                  ref3 > rthresh_ref .and. ref4 > rthresh_ref ) then
                  ref3d(i,j,k)=(ref1*w1+ref2*w2+ref3*w3+ref4*w4)/var_scale
               elseif(ref1 > rthresh_miss .and. ref2 > rthresh_miss .and.  &
                  ref3 > rthresh_miss .and. ref4 > rthresh_miss ) then
                  ref3d(i,j,k)=-99.0   ! clear
               else
                  ref3d(i,j,k)=-999.0  ! no observation
               endif
             endif
             endif
          ENDDO
          ENDDO
      ENDDO  ! mscNlev

      deallocate(var)

      deallocate(msclon)
      deallocate(msclat)
      deallocate(msclev)
      deallocate(mscValue)
   else
      allocate(ref3d(nlon,nlat,maxlvl))
      ref3d=-999.0
      write(*,*) trim(mosaicfile), '   does not exist!!!'
   ENDIF

   call mpi_barrier(MPI_COMM_WORLD,ierror)
!
!  collect data from all processes to root (0)
!
   if(mype==0) then
     allocate( ref0(nlon,nlat,maxlvl) )
     allocate( maxref(nlon,nlat) )
     maxref=-999.0
   endif
   call MPI_REDUCE(ref3d, ref0, nlon*nlat*maxlvl, MPI_REAL, MPI_MAX, 0, &
                     MPI_COMM_WORLD, ierror)
   deallocate(ref3d)
!
  if(mype==0) then
!
    allocate(ref3d_column(maxlvl+2,nlon*nlat))
    ref3d_column=-999.0
    numref=0
    DO j=1,nlat
    DO i=1,nlon
      numlvl=0
      DO k=1,maxlvl
        if(abs(ref0(i,j,k)) < 888.0 ) numlvl=numlvl+1
      ENDDO
      if(numlvl > 0 ) then
        numref=numref+1
        ref3d_column(1,numref)=float(i)
        ref3d_column(2,numref)=float(j)
        DO k=1,maxlvl
           ref3d_column(2+k,numref)=ref0(i,j,k)
           if(ref0(i,j,k) > maxref(i,j)) maxref(i,j)=ref0(i,j,k)
        ENDDO
      endif
    ENDDO
    ENDDO

    DO j=1,nlat
    DO i=1,nlon
      DO k=1,maxlvl
         if(ref0(i,j,k) > maxref(i,j)) maxref(i,j)=ref0(i,j,k)
      ENDDO
       if( maxref(i,j) < 0.0 .and. maxref(i,j) > -40.0 ) then
            maxref(i,j) = 0.0
       elseif(maxref(i,j) <=-40.0 .and. maxref(i,j) > -100.0)  then
            maxref(i,j)= -10.0
       elseif(maxref(i,j) <=-100.0) then
            maxref(i,j) = -20.0
       endif
    ENDDO
    ENDDO
    write(*,*) 'composite reflectivity max, min=', &
                        maxval(maxref),minval(maxref)
    open(11,file='compref.bin',form='unformatted')
       write(11) nlon,nlat
       write(11) maxref
    close(11)
    write(*,*) 'Start write_bufr_nsslref'
    call write_bufr_nsslref(maxlvl,nlon,nlat,numref,ref3d_column,idate)
  endif

  call MPI_FINALIZE(ierror)
!
end program process_NSSL_mosaic
