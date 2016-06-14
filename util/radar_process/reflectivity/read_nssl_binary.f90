
subroutine read_head_Mosaic4(infile,nx,ny,nz,rlon,rlat,rdx,rdy,var_scale)
  !=================================================================================
  !
  ! This program reads the head information from the new 4 NSSL tiles MRMS
  ! binary format
  !
  ! INPUTS: infile
  ! 
  ! OUTPUTS: 
  !
  ! Written by: Ming Hu
  ! Last Update: 15 AUG 2013
  !
  !=================================================================================

  implicit none

  ! variables for output NetCDF file
  character(len=256)     :: infile
  real*8,intent(out)     :: rdx,rdy
  real,intent(out)       :: rlat,rlon
  integer*4,intent(out)  :: nx,ny,nz
  integer*4,intent(out)  :: var_scale

  ! variables for reading input file
  integer                                 ::ntot, ntot2d, i, j, k, jrec, mt
  logical                                 ::looping
  integer*4                               :: yr,mo,da,hr,mn,sc
  integer*4                               :: nw_lon, nw_lat, dx, dy, dz
  integer*4                               :: map_scale, dxy_scale, z_scale
  integer*4                               :: missing_val, numrad
  integer*4                               :: dvals(4), blank(10), temp(42)
  integer*4,allocatable                   :: ilevel(:)
  character(len=4)                        :: d_val
  character(len=6)                        :: var_unit
  character(len=20)                       :: var_name

  !=================================================================================
  ! BEGIN PROGRAM

     print *, 'Reading: ', trim(infile)

     ! Open and set position to start of file
     open(99,file=trim(infile),form='unformatted',access='direct',&
             recl=10*4,status='old')
     rewind(99)

     ! Read header data
     read(99,rec=1) yr, mo, da, hr, mn, sc, nx, ny, nz, d_val
     read(99,rec=2) map_scale, dvals(1:3),nw_lon, nw_lat,dvals(4),dx,dy,dxy_scale

     allocate(ilevel(nz))
!     ntot = nx*ny*nz
!     allocate(var(ntot))

     read(99,rec=3) ilevel(1:10)
     read(99,rec=4) ilevel(11:20)
     read(99,rec=5) ilevel(21:30)
     read(99,rec=6) ilevel(31:33),z_scale,blank(1:6)
     read(99,rec=7) blank(7:10),var_name(1:20),var_unit(1:4)
     read(99,rec=8) var_unit(5:6),var_scale, missing_val, numrad 
     close(99)

     rdx=dx
     rdx=rdx/dxy_scale
     rdy=dy
     rdy=rdy/dxy_scale
     rlat=float(nw_lat)/dvals(4)
     rlon=float(nw_lon)/dvals(4)
     ! Print header data
     print *, "Year Month Day Hour Min Sec: ", yr, mo, da, hr, mn, sc
     print *, "nx, ny, nz: ", nx, ny, nz
     print *, "d_val, dvals: ", d_val, dvals
     print *, "nw_lon, nw_lat: ", rlon, rlat
     print *, "dx, dy: ", rdx, rdy
     print *, "map_scale, dxy_scale, z_scale:", map_scale, dxy_scale, z_scale
     print *, "blank:", blank
     print *, "var_name, var_unit:", var_name, ' ', var_unit
     print *, "var_scale, missing_val, numrad:", var_scale, missing_val, numrad
     print *, "levels:", ilevel

end subroutine read_head_Mosaic4

subroutine read_data_Mosaic4(infile,ntot,var)
  !=================================================================================
  !
  ! This program reads the new 4 NSSL tiles 
  !
  ! INPUTS: inputDir, inputFileName, varName, outputFileName
  ! 
  ! OUTPUTS: 
  !
  ! Written by: Ming Hu
  ! Last Update: 15 AUG 2013
  !
  !=================================================================================

  implicit none

  character(len=256),intent(in)     :: infile
  integer,intent(in)                :: ntot
  integer*2, dimension(ntot),intent(out)  :: var

  ! variables for reading input file
  integer                                 ::j,jrec
  logical                                 ::looping
  integer*4                               :: blank(74)
  integer*2                               :: rlist

  integer                                 :: irecl
  !=================================================================================
  ! BEGIN PROGRAM
     irecl=74

     ! Open and set position to start of file
     open(99,file=trim(infile),form='unformatted',access='direct',&
                   recl=irecl*4,status='old')
     rewind(99)
     read(99,rec=1) blank
     read(99,rec=2) rlist,var(1:irecl*2-1)

     ! Clunkily read 12 values at a time (ugh)
     j=irecl*2-1
     jrec=3
     looping = .True.
     do while(looping)
        if (irecl*2+j > ntot) then
           read(99,rec=jrec) var(1+j:ntot)
           looping = .False.
        else
           read(99,rec=jrec) var(1+j:irecl*2+j)
        endif
        jrec = jrec + 1
        j = j+irecl*2
     enddo
        
     close(99)

end subroutine read_data_Mosaic4
