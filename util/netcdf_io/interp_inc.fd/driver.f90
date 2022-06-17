 program interp_inc

!---------------------------------------------------------------------
!
! Read a gaussian atmospheric increment file in netcdf.  Interpolate
! all fields to another gaussian resolution.  Output the result
! in another netcdf file.
!
! Namelist variables:
! -------------------
! lon_out               - 'i' dimension of output gaussian grid
! lat_out               - 'j' dimension of output gaussian grid
! lev                   - Number of vertical levels.  Must be
!                         the same for the input and output grids.
! infile                - Path/name of input gaussian increment
!                         file (netcdf)
! outfile               - Path/name of output gaussian increment
!                         file (netcdf)
!
! 2019-10-24        Initial version.
! 2020-01-27        Martin - added in some simple MPI to speed up a bit
!
!---------------------------------------------------------------------

 use netcdf
 use mpi

 implicit none

 integer, parameter :: num_recs = 9

! Declare externals
 external :: w3tagb, netcdf_err, splat, ipolatev, &
    ipolates, w3tage

 character(len=128) :: outfile, infile
 character(len=11)  :: records(num_recs) 

 integer :: i, j, mi, iret, mo, rec
 integer :: lon_in, lat_in
 integer :: lon_out, lat_out
 integer :: lev, ilev, lev_in
 integer :: ncid_in, id_var
 integer :: ncid_out, error
 integer :: dim_lon_out, dim_lat_out
 integer :: dim_lev_out, dim_ilev_out
 integer :: id_u_inc_out, id_v_inc_out
 integer :: id_lon_out, id_lat_out, id_lev_out
 integer :: id_pfull_out, id_ilev_out
 integer :: id_hyai_out, id_hybi_out
 integer :: id_delp_inc_out, id_delz_inc_out
 integer :: id_t_inc_out, id_sphum_inc_out
 integer :: id_liq_wat_inc_out, id_o3mr_inc_out
 integer :: id_icmr_inc_out, id_dim
 integer :: header_buffer_val = 16384
 integer :: kgds_in(200), kgds_out(200)
 integer :: ip, ipopt(20), no
 integer, allocatable :: ibi(:), ibo(:), levs(:)

 integer :: mpierr, mype, npes, mpistat(mpi_status_size)

 logical*1, allocatable :: li(:,:), lo(:,:)

 real, allocatable :: dummy_in(:,:,:)
 real, allocatable :: dummy_out(:,:,:)

 real(8) :: rad2deg,dlondeg
 real(8), allocatable :: latitude_in(:), longitude_in(:)
 real(8), allocatable :: latitude_out(:), longitude_out(:)
 real(8), allocatable :: slat(:), wlat(:)
 real(8), allocatable :: rlon(:), rlat(:), crot(:), srot(:)
 real(8), allocatable :: gi(:,:), gi2(:,:), go(:,:), go2(:,:), go3(:,:)


 ! NOTE: u_inc,v_inc must be consecutive
 data records /'u_inc', 'v_inc', 'delp_inc', 'delz_inc', 'T_inc', &
               'sphum_inc', 'liq_wat_inc', 'o3mr_inc', 'icmr_inc' /

 namelist /setup/ lon_out, lat_out, outfile, infile, lev


!-----------------------------------------------------------------
! MPI initialization
call mpi_init(mpierr)
call mpi_comm_rank(mpi_comm_world, mype, mpierr)
call mpi_comm_size(mpi_comm_world, npes, mpierr)
!-----------------------------------------------------------------

!-----------------------------------------------------------------
! Open and create output file records.  These will be filled
! with data below.
!-----------------------------------------------------------------

 if (mype == 0)  call w3tagb('INTERP_INC', 2019, 100, 0, 'EMC')

 if (mype == 0) print*,'- READ SETUP NAMELIST'
 open (43, file="./fort.43")
 read (43, nml=setup, iostat=error)
 if (error /= 0) then
   print*,"- FATAL ERROR READING NAMELIST. ISTAT IS: ", error
   stop 44
 endif
 close (43)

 if (mype == 0) print*,"- WILL INTERPOLATE TO GAUSSIAN GRID OF DIMENSION ",lon_out, lat_out

! Set constants
 rad2deg = 180.0_8 / (4.0_8 * atan(1.0_8))
 dlondeg = 360.0_8 / real(lon_out,8)

 ilev=lev+1

 call mpi_barrier(mpi_comm_world, mpierr)
 if (mype == 0) then
   print*,'- OPEN OUTPUT FILE: ', trim(outfile)
  
   error = nf90_create(outfile, cmode=IOR(NF90_CLOBBER,NF90_NETCDF4), ncid=ncid_out)
   call netcdf_err(error, 'CREATING FILE='//trim(outfile) )
  
   error = nf90_def_dim(ncid_out, 'lon', lon_out, dim_lon_out)
   call netcdf_err(error, 'defining dimension lon for file='//trim(outfile) )
  
   error = nf90_def_dim(ncid_out, 'lat', lat_out, dim_lat_out)
   call netcdf_err(error, 'defining dimension lat for file='//trim(outfile) )
  
   error = nf90_def_dim(ncid_out, 'lev', lev, dim_lev_out)
   call netcdf_err(error, 'defining dimension lev for file='//trim(outfile) )
  
   error = nf90_def_dim(ncid_out, 'ilev', ilev, dim_ilev_out)
   call netcdf_err(error, 'defining dimension ilev for file='//trim(outfile) )
  
   error = nf90_def_var(ncid_out, 'lon', nf90_double, (/dim_lon_out/), id_lon_out)
   call netcdf_err(error, 'defining variable lon for file='//trim(outfile) )
  
   error = nf90_put_att(ncid_out, id_lon_out, "units", "degrees_east")
   call netcdf_err(error, 'define lon attribute for file='//trim(outfile) )
  
   error = nf90_def_var(ncid_out, 'lat', nf90_double, (/dim_lat_out/), id_lat_out)
   call netcdf_err(error, 'defining varable lat for file='//trim(outfile) )
  
   error = nf90_put_att(ncid_out, id_lat_out, "units", "degrees_north")
   call netcdf_err(error, 'defining lat att for file='//trim(outfile) )
  
   error = nf90_def_var(ncid_out, 'lev', nf90_float, (/dim_lev_out/), id_lev_out)
   call netcdf_err(error, 'defining variable lev for file='//trim(outfile) )
  
   error = nf90_def_var(ncid_out, 'pfull', nf90_float, (/dim_lev_out/), id_pfull_out)
   call netcdf_err(error, 'defining variable pfull for file='//trim(outfile) )
  
   error = nf90_def_var(ncid_out, 'ilev', nf90_float, (/dim_ilev_out/), id_ilev_out)
   call netcdf_err(error, 'defining variable ilev for file='//trim(outfile) )
  
   error = nf90_def_var(ncid_out, 'hyai', nf90_float, (/dim_ilev_out/), id_hyai_out)
   call netcdf_err(error, 'defining variable hyai for file='//trim(outfile) )
  
   error = nf90_def_var(ncid_out, 'hybi', nf90_float, (/dim_ilev_out/), id_hybi_out)
   call netcdf_err(error, 'defining variable hybi for file='//trim(outfile) )
  
   error = nf90_def_var(ncid_out, 'u_inc', nf90_float, (/dim_lon_out,dim_lat_out,dim_lev_out/), id_u_inc_out)
   call netcdf_err(error, 'defining variable u_inc for file='//trim(outfile) )
  
   error = nf90_def_var(ncid_out, 'v_inc', nf90_float, (/dim_lon_out,dim_lat_out,dim_lev_out/), id_v_inc_out)
   call netcdf_err(error, 'defining variable v_inc for file='//trim(outfile) )
  
   error = nf90_def_var(ncid_out, 'delp_inc', nf90_float, (/dim_lon_out,dim_lat_out,dim_lev_out/), id_delp_inc_out)
   call netcdf_err(error, 'defining variable delp_inc for file='//trim(outfile) )
  
   error = nf90_def_var(ncid_out, 'delz_inc', nf90_float, (/dim_lon_out,dim_lat_out,dim_lev_out/), id_delz_inc_out)
   call netcdf_err(error, 'defining variable delz_inc for file='//trim(outfile) )
  
   error = nf90_def_var(ncid_out, 'T_inc', nf90_float, (/dim_lon_out,dim_lat_out,dim_lev_out/), id_t_inc_out)
   call netcdf_err(error, 'defining variable t_inc for file='//trim(outfile) )
  
   error = nf90_def_var(ncid_out, 'sphum_inc', nf90_float, (/dim_lon_out,dim_lat_out,dim_lev_out/), id_sphum_inc_out)
   call netcdf_err(error, 'defining variable sphum_inc for file='//trim(outfile) )
  
   error = nf90_def_var(ncid_out, 'liq_wat_inc', nf90_float, (/dim_lon_out,dim_lat_out,dim_lev_out/), id_liq_wat_inc_out)
   call netcdf_err(error, 'defining variable liq_wat_inc for file='//trim(outfile) )
  
   error = nf90_def_var(ncid_out, 'o3mr_inc', nf90_float, (/dim_lon_out,dim_lat_out,dim_lev_out/), id_o3mr_inc_out)
   call netcdf_err(error, 'defining variable o3mr_inc for file='//trim(outfile) )
  
   error = nf90_def_var(ncid_out, 'icmr_inc', nf90_float, (/dim_lon_out,dim_lat_out,dim_lev_out/), id_icmr_inc_out)
   call netcdf_err(error, 'defining variable icmr_inc for file='//trim(outfile) )
  
   error = nf90_put_att(ncid_out, nf90_global, 'source', 'GSI')
   call netcdf_err(error, 'defining source attribute for file='//trim(outfile) )
  
   error = nf90_put_att(ncid_out, nf90_global, 'comment', 'interpolated global analysis increment')
   call netcdf_err(error, 'defining comment attribute for file='//trim(outfile) )
  
   error = nf90_enddef(ncid_out, header_buffer_val, 4,0,4)
   call netcdf_err(error, 'end meta define for file='//trim(outfile) )
 end if

!-----------------------------------------------------------------
! Compute latitude and longitude of output grid.  
!-----------------------------------------------------------------

 allocate(latitude_out(lat_out))
 allocate(slat(lat_out))
 allocate(wlat(lat_out))

 call splat(4, lat_out, slat, wlat)
 do j = 1, lat_out
   latitude_out(j) = -( 90.0_8 - (acos(slat(j))* rad2deg) )
 enddo
 deallocate(slat, wlat)

!print*,'lat out ',latitude_out(1), latitude_out(lat_out)

 allocate(longitude_out(lon_out))
 do i = 1, lon_out
   longitude_out(i) = real(i-1,8) * dlondeg
 enddo

!print*,'lon out ',longitude_out(1), longitude_out(lon_out)

!-----------------------------------------------------------------
! Compute grib 1 grid description section for output gaussian
! grid.  The GDS is required for ipolates.
!-----------------------------------------------------------------

 kgds_out = 0
 kgds_out(1) = 4            ! oct 6 - type of grid (gaussian)
 kgds_out(2) = lon_out      ! oct 7-8 - # pts on latitude circle
 kgds_out(3) = lat_out      ! oct 9-10 - # pts on longitude circle
 kgds_out(4) = nint(latitude_out(1)*1000.0_8)  ! oct 11-13 - lat of origin
 kgds_out(5) = 0            ! oct 14-16 - lon of origin
 kgds_out(6) = 128          ! oct 17 - resolution flag
 kgds_out(7) = nint(latitude_out(lat_out)*1000.0_8)     ! oct 18-20 - lat of extreme pt
 kgds_out(8) = nint(longitude_out(lon_out)*1000.0_8)    ! oct 21-23 - lon of extreme pt
 kgds_out(9) = nint((360.0_8 / real(lon_out,8))*1000.0_8) ! oct 24-25 - long increment
 kgds_out(10) = lat_out / 2 ! oct 26-27 - number of circles pole to equator
 kgds_out(11) = 64          ! oct 28 - scan mode
 kgds_out(12) = 255         ! oct 29 - reserved
 kgds_out(19) = 0           ! oct 4 - # vert coordinate parameters
 kgds_out(20) = 255         ! oct 5 - not used set to 255 (missing)

!print*,'kgds out ',kgds_out(1:20)

!----------------------------------------------------
! Open and read input file
!----------------------------------------------------

 if (mype == 0) print*,'- OPEN INPUT FILE: ', trim(infile)

 error = nf90_open(trim(infile), ior(nf90_nowrite, nf90_mpiio), &
                   comm=mpi_comm_world, info = mpi_info_null, ncid=ncid_in)
 call netcdf_err(error, 'opening file='//trim(infile) )

 error = nf90_inq_dimid(ncid_in, 'lon', id_dim)
 call netcdf_err(error, 'inquiring lon dimension for file='//trim(infile) )
 error = nf90_inquire_dimension(ncid_in, id_dim, len=lon_in)
 call netcdf_err(error, 'reading lon dimension for file='//trim(infile) )
 allocate(longitude_in(lon_in))
 error = nf90_inq_varid(ncid_in, 'lon', id_dim)
 call netcdf_err(error, 'inquiring var lon dimension for file='//trim(infile) )
 error = nf90_get_var(ncid_in, id_dim, longitude_in)
 call netcdf_err(error, 'reading longitude_in for file='//trim(infile) )

!print*,'lon of input file is ',lon_in
!print*,'lon in ',longitude_in(1), longitude_in(lon_in)

 error = nf90_inq_dimid(ncid_in, 'lat', id_dim)
 call netcdf_err(error, 'inquiring lat dimension for file='//trim(infile) )
 error = nf90_inquire_dimension(ncid_in, id_dim, len=lat_in)
 call netcdf_err(error, 'reading lat dimension for file='//trim(infile) )
 allocate(latitude_in(lat_in))
 error = nf90_inq_varid(ncid_in, 'lat', id_dim)
 call netcdf_err(error, 'inquiring var lat dimension for file='//trim(infile) )
 error = nf90_get_var(ncid_in, id_dim, latitude_in)
 call netcdf_err(error, 'reading latitude_in for file='//trim(infile) )

!print*,'lat of input file is ',lat_in
!print*,'lat in ',latitude_in(1), latitude_in(lat_in)

 error = nf90_inq_dimid(ncid_in, 'lev', id_dim)
 call netcdf_err(error, 'inquiring lev dimension for file='//trim(infile) )
 error = nf90_inquire_dimension(ncid_in, id_dim, len=lev_in)
 call netcdf_err(error, 'reading lev dimension for file='//trim(infile) )

!print*,'lev of input file is ',lev_in

!-----------------------------------------------------------------
! Compute grib 1 grid description section for input gaussian
! grid.
!-----------------------------------------------------------------

 kgds_in = 0
 kgds_in(1) = 4            ! oct 6 - type of grid (gaussian)
 kgds_in(2) = lon_in       ! oct 7-8 - # pts on latitude circle
 kgds_in(3) = lat_in       ! oct 9-10 - # pts on longitude circle
 kgds_in(4) = nint(latitude_in(1)*1000.0_8)  ! oct 11-13 - lat of origin
 kgds_in(5) = 0            ! oct 14-16 - lon of origin
 kgds_in(6) = 128          ! oct 17 - resolution flag
 kgds_in(7) = nint(latitude_in(lat_in)*1000.0_8)  ! oct 18-20 - lat of extreme pt
 kgds_in(8) = nint(longitude_in(lon_in)*1000.0_8) ! oct 21-23 - lon of extreme pt
 kgds_in(9) = nint((360.0_8 / real(lon_in,8))*1000.0_8) ! oct 24-25 - long increment
 kgds_in(10) = lat_in / 2  ! oct 26-27 - number of circles pole to equator
 kgds_in(11) = 64          ! oct 28 - scan mode
 kgds_in(12) = 255         ! oct 29 - reserved
 kgds_in(19) = 0           ! oct 4 - # vert coordinate parameters
 kgds_in(20) = 255         ! oct 5 - not used set to 255 (missing)

!print*,'kgds in ',kgds_in(1:20)

 if (lev /= lev_in) then
   print*,'- FATAL ERROR: input and output levels dont match: ',lev_in, lev
   stop 56
 endif

!-----------------------------------------------------------------
! Loop over each record, then interpolate using ipolates.
! Interpolated data is then written to output file.
!-----------------------------------------------------------------

 mi = lon_in * lat_in
 mo = lon_out * lat_out
 allocate(dummy_out(lon_out,lat_out,lev))
 allocate(dummy_in(lon_in, lat_in, lev))
 allocate(ibi(lev))
 allocate(li(mi,lev))
 allocate(gi(mi,lev))
 allocate(gi2(mi,lev))
 allocate(rlat(mo),crot(mo))
 crot = 0; srot = 0
 allocate(rlon(mo),srot(mo))
 allocate(ibo(lev))
 allocate(lo(mo,lev))
 allocate(go(mo,lev))
 allocate(go2(mo,lev))
 allocate(go3(mo,lev))

 call mpi_barrier(mpi_comm_world, mpierr)
 do rec = 1, num_recs

   ! skip v_inc (done with u_inc, which comes first)
   if (trim(records(rec)) .eq. 'v_inc') cycle

   if (mype == rec) then
     print*,'- PROCESS RECORD: ', trim(records(rec))
  
     error = nf90_inq_varid(ncid_in, trim(records(rec)), id_var)
     call netcdf_err(error, 'inquiring ' // trim(records(rec)) // ' id for file='//trim(infile) )
     error = nf90_get_var(ncid_in, id_var, dummy_in)
     call netcdf_err(error, 'reading ' //  trim(records(rec)) // ' for file='//trim(infile) )
  
  
     ip = 0 ! bilinear
     ipopt = 0
     ibi = 0
     li = 0
     gi = 0.0_8
     rlat = 0.0_8
     rlon = 0.0_8
     ibo = 0
     lo = 0
     go = 0.0_8
     gi = reshape (dummy_in, (/mi, lev/))
  
     if (trim(records(rec)) .eq. 'u_inc') then
        ! do u_inc,v_inc at the same time
        error = nf90_inq_varid(ncid_in, 'v_inc', id_var)
        call netcdf_err(error, 'inquiring v_inc id for file='//trim(infile) )
        error = nf90_get_var(ncid_in, id_var, dummy_in)
        call netcdf_err(error, 'reading v_inc for file='//trim(infile) )
        gi2 = reshape (dummy_in, (/mi, lev/))
        call ipolatev(ip, ipopt, kgds_in, kgds_out, mi, mo,&
                      lev, ibi, li, gi, gi2,  &
                      no, rlat, rlon, crot, srot, ibo, lo, &
                      go, go3, iret)
        if (iret /= 0) then
          print*,'FATAL ERROR in ipolatev, iret: ',iret
          stop 76
        endif
        if (no /= mo) then
          print*,'FATAL ERROR: ipolatev returned wrong number of pts ',no
          stop 77
        endif
        call mpi_send(go(1,1), size(go), mpi_double_precision, &
                      0, 1000+rec, mpi_comm_world, mpierr)
        call mpi_send(go3(1,1), size(go3), mpi_double_precision, &
                      0, 2000+rec, mpi_comm_world, mpierr)
     else
        call ipolates(ip, ipopt, kgds_in, kgds_out, mi, mo, &
                   lev, ibi, li, gi, no, rlat, rlon, ibo, &
                   lo, go, iret)
        if (iret /= 0) then
          print*,'FATAL ERROR in ipolates, iret: ',iret
          stop 76
        endif
        if (no /= mo) then
          print*,'FATAL ERROR: ipolates returned wrong number of pts ',no
          stop 77
        endif
        !dummy_out = reshape(go, (/lon_out,lat_out,lev/))
        !print *, lon_out, lat_out, lev, 'send'
        call mpi_send(go(1,1), size(go), mpi_double_precision, &
                      0, 1000+rec, mpi_comm_world, mpierr)
     endif
   else if (mype == 0) then
     !print *, lon_out, lat_out, lev, 'recv'
     call mpi_recv(go2(1,1), size(go2), mpi_double_precision, &
                   rec, 1000+rec, mpi_comm_world, mpistat, mpierr)
     dummy_out = reshape(go2, (/lon_out,lat_out,lev/))
     error = nf90_inq_varid(ncid_out, trim(records(rec)), id_var)
     call netcdf_err(error, 'inquiring ' // trim(records(rec)) // ' id for file='//trim(outfile) )
     error = nf90_put_var(ncid_out, id_var, dummy_out)
     call netcdf_err(error, 'writing ' // trim(records(rec)) // ' for file='//trim(outfile) )
     if (trim(records(rec)) .eq. 'u_inc') then
        ! process v_inc also.
        call mpi_recv(go2(1,1), size(go2), mpi_double_precision, &
                      rec, 2000+rec, mpi_comm_world, mpistat, mpierr)
        dummy_out = reshape(go2, (/lon_out,lat_out,lev/))
        error = nf90_inq_varid(ncid_out, 'v_inc', id_var)
        call netcdf_err(error, 'inquiring v_inc id for file='//trim(outfile) )
        error = nf90_put_var(ncid_out, id_var, dummy_out)
        call netcdf_err(error, 'writing v_inc for file='//trim(outfile) )
     endif
   endif 
 enddo  ! records

 error = nf90_close(ncid_in)
 call mpi_barrier(mpi_comm_world, mpierr)

 deallocate(dummy_out)
 deallocate(dummy_in)
 deallocate(ibi)
 deallocate(li)
 deallocate(gi,gi2)
 deallocate(rlat, rlon, ibo, lo, go, go2, go3, crot, srot)

!------------------------------------------------------------------
! Update remaining output file records according to Cory's sample.
!------------------------------------------------------------------

 if (mype == 0) then
   print*,"- WRITE OUTPUT FILE: ", trim(outfile)
  
  ! lev
  
   allocate(levs(lev))
   do j = 1, lev
     levs(j) = j
   enddo
  
   error = nf90_put_var(ncid_out, id_lev_out, levs)
   call netcdf_err(error, 'writing levs for file='//trim(outfile) )
  
  ! pfull
  
   error = nf90_put_var(ncid_out, id_pfull_out, levs)
   call netcdf_err(error, 'writing pfull for file='//trim(outfile) )
  
   deallocate (levs)
   allocate (levs(ilev))
   do j = 1, ilev
     levs(j) = j
   enddo
  
  ! ilev
  
   error = nf90_put_var(ncid_out, id_ilev_out, levs)
   call netcdf_err(error, 'writing ilev for file='//trim(outfile) )
  
  ! hyai
  
   error = nf90_put_var(ncid_out, id_hyai_out, levs)
   call netcdf_err(error, 'writing hyai for file='//trim(outfile) )
  
  ! hybi
  
   error = nf90_put_var(ncid_out, id_hybi_out, levs)
   call netcdf_err(error, 'writing hybi for file='//trim(outfile) )
  
  ! latitude
  
   error = nf90_put_var(ncid_out, id_lat_out, latitude_out)
   call netcdf_err(error, 'writing latitude for file='//trim(outfile) )
  
  ! longitude
  
   error = nf90_put_var(ncid_out, id_lon_out, longitude_out)
   call netcdf_err(error, 'writing longitude for file='//trim(outfile) )
  
   deallocate(levs)
  
   error = nf90_close(ncid_out)
 end if

 call mpi_barrier(mpi_comm_world, mpierr)
 if (mype == 0) print*,'- NORMAL TERMINATION'

 if (mype == 0) call w3tage('INTERP_INC')
 call mpi_barrier(mpi_comm_world, mpierr)
 call mpi_finalize(mpierr)

 end program interp_inc

 subroutine netcdf_err( err, string )

 use netcdf

 implicit none
 integer, intent(in) :: err
 character(len=*), intent(in) :: string
 character(len=256) :: errmsg

 if( err.EQ.NF90_NOERR )return
 errmsg = NF90_STRERROR(err)
 print*,''
 print*,'FATAL ERROR: ', trim(string), ': ', trim(errmsg)
 print*,'STOP.'
 stop 999

 return
 end subroutine netcdf_err

