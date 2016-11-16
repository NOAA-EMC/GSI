module da_gen_be

   use da_control, only : stdout,vertical_ip, t0,es_beta,es_alpha, &
      es_gamma,kappa,rd_over_rv,rd_over_rv1,t_kelvin, gravity, &
      filename_len,vertical_ip_0, cv_options
   use da_reporting, only : da_error, message
   use da_tools_serial, only : da_get_unit, da_free_unit, da_array_print
   use da_lapack, only : dsyev
   use da_recursive_filters, only: da_perform_2drf, da_recursive_filter_1d
   
   implicit none

   real, parameter :: base_pres = 100000.0 
               
contains

subroutine da_create_bins(ni, nj, nk, bin_type, num_bins, num_bins2d, bin, bin2d, &
   lat_min, lat_max, binwidth_lat, hgt_min, hgt_max, binwidth_hgt, latitude, height)

   !----------------------------------------------------------------------------
   !
   ! Purpose: To create the bins for calculation of statistics.
   !
   ! Input:
   ! ni, nj, nk   Dimensions
   ! bin_type     0: No binning; 
   !              1: bins for X-direction mean; 
   !              2: bins for each of binwidth_lat/binwidth_hgt.  
   !              3: bins for each of binwidth_lat/nk.  
   !              4: num_hor_bins horizontal bins /nk.  
   !              5: Average over all horizontal points (nk bins for 3D fields)
   !              6: Average over all points (only 1 bin).
   ! Optional for bin_type = 2:
   ! lat_min, lat_max Minimum/maximum latitude ranges for bin_type = 2
   ! binwidth_lat interval between bins for latitude in degree for bin_type = 2
   ! binwidth_hgt interval between bins for height in meter for bin_type = 2
   ! num_bins_hgt the number of height bins for bin_type = 2
   ! latitude     3d field latitude in degree for bin_type = 2
   ! height       3d field height in meter for bin_type = 2
   !
   ! Output:
   ! num_bins,num_bins2d ---- the number of bins for 3d and 2d fields
   ! bin     ,     bin2d ---- Assigned bin to a gridpoints for 3d and 2d fields
   !
   !----------------------------------------------------------------------------

   implicit none

   integer,        intent(in)  :: ni, nj, nk          ! Dimensions read in.
   integer,        intent(in)  :: bin_type            ! Type of bin to average over
   integer,        intent(out) :: num_bins            ! Number of bins.
   integer,        intent(out) :: num_bins2d          ! Number of bins for 2D fields
   integer,        intent(out) :: bin(1:ni,1:nj,1:nk) ! Bin at each 3D point
   integer,        intent(out) :: bin2d(1:ni,1:nj)    ! Bin at each 2D point

   real(kind=8), optional, intent(inout)  :: lat_min, lat_max    ! Used if bin_type = 2 (deg)
   real(kind=8), optional, intent(inout)  :: binwidth_lat        ! Used if bin_type = 2 (deg)
   real(kind=8), optional, intent(inout)  :: hgt_min, hgt_max    ! Used if bin_type = 2 (deg)
   real(kind=8), optional, intent(in)  :: binwidth_hgt        ! Used if bin_type = 2 (m).
   real(kind=8), optional, intent(in)  :: latitude(1:ni,1:nj) ! Latitude (degrees).
   real(kind=8), optional, intent(in)  :: height(1:ni,1:nj,1:nk)     ! Height (m).

   integer           :: b, i, j, k                 ! Loop counters.
   integer           :: count                      ! Counter
   integer           :: num_bins_lat               ! Used if bin_type = 2.
   integer           :: num_bins_hgt               ! Used if bin_type = 2.
   integer           :: bin_lat                    ! Latitude bin.
   integer           :: bin_hgt                    ! Height bin.
   integer           :: num_bins_i, num_bins_j     ! Used if bin_type = 4.
   integer           :: nii, njj                   ! Used if bin_type = 4.
   integer           :: bin_i(1:ni), bin_j(1:nj)   ! Used if bin_type = 4.
   real, allocatable :: binstart_lat(:)            ! Used if bin_type = 2 (deg)
   real, allocatable :: binstart_hgt(:)            ! Used if bin_type = 2 (deg)

   if (bin_type == 0) then         ! No averaging in space

      num_bins = nk * nj * ni
      num_bins2d = nj * ni    ! Equals number of horizontal points.

      count = 1
      do k = 1, nk
         do j = 1, nj
            do i = 1, ni
               bin(i,j,k) = count
               count = count + 1
            end do
         end do
      end do
      bin2d(:,:) = bin(:,:,1)

   else if (bin_type == 1) then    ! Average over x-direction.

      num_bins = nj * nk
      num_bins2d = nj

      count = 1
      do k = 1, nk
         do j = 1, nj
            bin(1:ni,j,k) = count
            count = count + 1
         end do
      end do
      bin2d(:,:) = bin(:,:,1)

   else if (bin_type == 2) then    ! Global latitude/height bins:

      ! Setup latitude bins:
      write(unit=stdout,fmt='(/a,f12.5)')'   Minimum latitude = ', lat_min
      write(unit=stdout,fmt='(a,f12.5)')'    Maximum latitude = ', lat_max
      write(unit=stdout,fmt='(a,f12.5)') &
         '    Latitude bin width = ', binwidth_lat
      num_bins_lat = nint((lat_max - lat_min) / binwidth_lat)
      write(unit=stdout,fmt='(a,i8)') &
         '    Number of latitude bins = ', num_bins_lat
   
      allocate(binstart_lat(1:num_bins_lat))
      do b = 1, num_bins_lat ! Assume south to north (as in WRF).
         binstart_lat(b) = lat_min + real(b-1) * binwidth_lat
      end do

      ! Setup height bins:
      write(unit=stdout,fmt='(/a,f12.5)')'    Minimum height = ', hgt_min
      write(unit=stdout,fmt='(a,f12.5)')'    Maximum height = ', hgt_max
      write(unit=stdout,fmt='(a,f12.5)')'    Height bin width = ', binwidth_hgt
      num_bins_hgt = nint((hgt_max - hgt_min) / binwidth_hgt)
      write(unit=stdout,fmt='(a,i8)') &
         '    Number of height bins = ', num_bins_hgt

      allocate(binstart_hgt(1:num_bins_hgt))
      do b = 1, num_bins_hgt
         binstart_hgt(b) = hgt_min + real(b-1) * binwidth_hgt
      end do

      num_bins = num_bins_lat * num_bins_hgt
      num_bins2d = num_bins_lat

      ! Select height bins:
      do j = 1, nj
         do i = 1, ni
            do k = 1, nk
               if (height(i,j,k) < binstart_hgt(1)) then 
                  bin_hgt = 1 ! In first bin.
               else if (height(i,j,k) >= binstart_hgt(num_bins_hgt)) then
                  bin_hgt = num_bins_hgt ! In final bin.
               else 
                  do b = 1, num_bins_hgt-1
                     if (height(i,j,k) >= binstart_hgt(b) .and. &
                          height(i,j,k) <  binstart_hgt(b+1)) then
                        bin_hgt = b
                        exit
                     end if
                  end do
               end if

               ! Select latitude bin that point falls in:
               if (k == 1) then
                  do b = 1, num_bins_lat-1
                     if (latitude(i,j) >= binstart_lat(b) .and. &
                        latitude(i,j) < binstart_lat(b+1)) then
                        bin_lat = b
                        exit
                     end if
                  end do
                  if (latitude(i,j) >= binstart_lat(num_bins_lat)) then
                     ! In final bin.
                     bin_lat = num_bins_lat
                  end if
                  bin2d(i,j) = bin_lat
               end if
               bin(i,j,k) = bin_lat + num_bins_lat * (bin_hgt - 1)
            end do
         end do
      end do

      deallocate(binstart_lat)
      deallocate(binstart_hgt)

   else if (bin_type == 3) then    ! Latitude/nk bins:


      ! Setup latitude bins:
      lat_min = minval(latitude)
      lat_max = maxval(latitude)
      num_bins_lat = floor (abs((lat_max - lat_min) / binwidth_lat) ) + 1
      allocate(binstart_lat(1:num_bins_lat))
      binstart_lat(1) = lat_min
      do i=2, num_bins_lat 
         binstart_lat(i) = binstart_lat(1) + binwidth_lat*(i-1)
      end do
      if ( binstart_lat(num_bins_lat).gt.lat_max ) then
         num_bins_lat = num_bins_lat-1
      end if
      write(*,*)'limit between interval ',binstart_lat(1:num_bins_lat)
      write(unit=stdout,fmt='(/a,f12.5)')'   Minimum latitude = ', lat_min
      write(unit=stdout,fmt='(a,f12.5)')'    Maximum latitude = ', lat_max
      write(unit=stdout,fmt='(a,f12.5)')'    Latitude bin width = ',binwidth_lat
      write(unit=stdout,fmt='(a,i8)') '    Number of latitude bins = ',num_bins_lat 

      num_bins = num_bins_lat * nk
      num_bins2d = num_bins_lat

      ! Select bins:
      do j = 1, nj
         do i = 1, ni
            do k = 1, nk
               ! Select latitude bin that point falls in:
               if (k == 1) then
                  bin_lat = 1 !! Double check GSI is needed
                  do b = 1, num_bins_lat-1
                     if (latitude(i,j) >= binstart_lat(b) .and. &
                        latitude(i,j) < binstart_lat(b+1)) then
                        bin_lat = b
                        exit
                     end if
                  end do
                  if (latitude(i,j) >= binstart_lat(num_bins_lat)) then
                     ! In final bin.
                     bin_lat = num_bins_lat
                  end if
                  bin2d(i,j) = bin_lat
               end if
               bin(i,j,k) = bin_lat + num_bins_lat * (k - 1)
            end do
         end do
      end do

      deallocate(binstart_lat)

   else if (bin_type == 4) then    ! binwidth_lat/nk bins:

      ! Setup horizontal bins:
      write(unit=stdout,fmt='(/a,f12.5)') &
         '   Number of grid-cells to average over = ', binwidth_lat
      ! use binwidth_lat, but actually an integer number of points.
 
      num_bins_j = int(real(nj) / real(binwidth_lat))
      njj = int(binwidth_lat) * num_bins_j
      do j = 1, njj
         bin_j(j) = 1 + int(real(j-1) / binwidth_lat)
      end do
      if (nj > njj) bin_j(njj+1:nj) = bin_j(njj)

      num_bins_i = int(real(ni) / real(binwidth_lat))
      nii = int(binwidth_lat) * num_bins_i
      do i = 1, nii
         bin_i(i) = 1 + int(real(i-1) / binwidth_lat)
      end do
      if (ni > nii) bin_i(nii+1:ni) = bin_i(nii)

      num_bins2d = num_bins_i * num_bins_j
      num_bins = num_bins2d * nk

      do j = 1, nj
         do i = 1, ni
            bin2d(i,j) = bin_i(i) + (bin_j(j) - 1) * num_bins_i
            do k = 1, nk
               bin(i,j,k) = bin2d(i,j) + (k - 1) * num_bins2d
            end do
         end do
      end do

   else if (bin_type == 5) then    ! Average over all horizontal points.

      num_bins = nk
      num_bins2d = 1

      do k = 1, nk
         bin(:,:,k) = k
      end do
      bin2d(:,:) = 1

   else if (bin_type == 6) then    ! Average over all points.

      num_bins = 1
      num_bins2d = 1
      bin(:,:,:) = 1
      bin2d(:,:) = 1
      
   else if (bin_type == 7) then    ! rain/no-rain bins based on bin 5

      num_bins = 4*nk
      num_bins2d = 4

      do k = 1, nk
         bin(:,:,k) = k
      end do
      bin2d(:,:) = 1
   
   else if (bin_type == 8) then
       
      num_bins = 6*nk
      num_bins2d = 6 

      do k = 1, nk
         bin(:,:,k) = k
      end do
      bin2d(:,:) = 1 
      
   end if

end subroutine da_create_bins


subroutine da_filter_regcoeffs(ni, nj, nk, num_bins, num_bins2d, num_passes, &
   rf_scale, bin, regcoeff1, regcoeff2, regcoeff3)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer, intent(in)    :: ni, nj, nk                  ! Grid dimensions.
   integer, intent(in)    :: num_bins                    ! Number of bins for 3D coeffs.
   integer, intent(in)    :: num_bins2d                  ! Number of bins for 2D coeffs.
   integer, intent(in)    :: num_passes                  ! Number of passes for RF.
   real,    intent(in)    :: rf_scale                    ! Smoothing scale of RF.
   integer, intent(in)    :: bin(1:ni,1:nj,1:nk)         ! Bin assigned to each point.
   real,    intent(inout) :: regcoeff1(1:num_bins)       ! psi/chi regression cooefficient.
   real,    intent(inout) :: regcoeff2(1:nk,1:num_bins2d)! psi/ps regression cooefficient.
   real,    intent(inout) :: regcoeff3(1:nk,1:nk,1:num_bins2d) ! psi/T regression cooefficient.

   integer :: i, j, k              ! Loop counters.
   integer :: b                    ! Bin index.
   real*8  :: field(1:ni,1:nj)     ! Field for recursive filter.

   !----------------------------------------------------------------------------
   ! [1] Filter psi/chi coefficient:
   !----------------------------------------------------------------------------

   do k = 1, nk
      ! Prepare field for filtering:
      do j = 1, nj
         do i = 1, ni
            b = bin(i,j,k)
            field(i,j) = regcoeff1(b)
         end do
      end do

      call da_perform_2drf(ni, nj, num_passes, rf_scale, field)

      do j = 1, nj
         do i = 1, ni
            b = bin(i,j,k)
            regcoeff1(b) = field(i,j)
         end do
      end do
   end do

end subroutine da_filter_regcoeffs


subroutine da_get_field( input_file, var, field_dims, dim1, dim2, dim3, k, field)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

!
! netcdf version 3 fortran interface:
!

!
! external netcdf data types:
!
      integer nf_byte
      integer nf_int1
      integer nf_char
      integer nf_short
      integer nf_int2
      integer nf_int
      integer nf_float
      integer nf_real
      integer nf_double
      integer nf_ubyte
      integer nf_ushort
      integer nf_uint
      integer nf_int64
      integer nf_uint64
      integer nf_string

!
! the user doesn't use these classes of user defined type, but they
! are returned by nc_inq_user_type.
!
      integer nf_vlen
      integer nf_opaque
      integer nf_enum
      integer nf_compound

      parameter (nf_byte = 1)
      parameter (nf_int1 = nf_byte)
      parameter (nf_char = 2)
      parameter (nf_short = 3)
      parameter (nf_int2 = nf_short)
      parameter (nf_int = 4)
      parameter (nf_float = 5)
      parameter (nf_real = nf_float)
      parameter (nf_double = 6)
      parameter (nf_ubyte = 7)
      parameter (nf_ushort = 8)
      parameter (nf_uint = 9)
      parameter (nf_int64 = 10)
      parameter (nf_uint64 = 11)
      parameter (nf_string = 12)
      parameter (nf_vlen = 13)
      parameter (nf_opaque = 14)
      parameter (nf_enum = 15)
      parameter (nf_compound = 16)

!
! default fill values:
!
      integer           nf_fill_byte
      integer           nf_fill_int1
      integer           nf_fill_char
      integer           nf_fill_short
      integer           nf_fill_int2
      integer           nf_fill_int
      real              nf_fill_float
      real              nf_fill_real
      doubleprecision   nf_fill_double
      integer           nf_fill_ubyte
      integer           nf_fill_ushort
!      real              nf_fill_uint
!      real              nf_fill_int64
!      real              nf_fill_uint64

      parameter (nf_fill_byte = -127)
      parameter (nf_fill_int1 = nf_fill_byte)
      parameter (nf_fill_char = 0)
      parameter (nf_fill_short = -32767)
      parameter (nf_fill_int2 = nf_fill_short)
      parameter (nf_fill_int = -2147483647)
      parameter (nf_fill_float = 9.9692099683868690e+36)
      parameter (nf_fill_real = nf_fill_float)
      parameter (nf_fill_double = 9.9692099683868690e+36)
      parameter (nf_fill_ubyte = 255)
      parameter (nf_fill_ushort = 65535)

!
! mode flags for opening and creating a netcdf dataset:
!
      integer nf_nowrite
      integer nf_write
      integer nf_clobber
      integer nf_noclobber
      integer nf_fill
      integer nf_nofill
      integer nf_lock
      integer nf_share
      integer nf_64bit_offset
      integer nf_sizehint_default
      integer nf_align_chunk
      integer nf_format_classic
      integer nf_format_64bit
      integer nf_format_netcdf4
      integer nf_format_netcdf4_classic

      parameter (nf_nowrite = 0)
      parameter (nf_write = 1)
      parameter (nf_clobber = 0)
      parameter (nf_noclobber = 4)
      parameter (nf_fill = 0)
      parameter (nf_nofill = 256)
      parameter (nf_lock = 1024)
      parameter (nf_share = 2048)
      parameter (nf_64bit_offset = 512)
      parameter (nf_sizehint_default = 0)
      parameter (nf_align_chunk = -1)
      parameter (nf_format_classic = 1)
      parameter (nf_format_64bit = 2)
      parameter (nf_format_netcdf4 = 3)
      parameter (nf_format_netcdf4_classic = 4)

!
! size argument for defining an unlimited dimension:
!
      integer nf_unlimited
      parameter (nf_unlimited = 0)

!
! global attribute id:
!
      integer nf_global
      parameter (nf_global = 0)

!
! implementation limits:
!
      integer nf_max_dims
      integer nf_max_attrs
      integer nf_max_vars
      integer nf_max_name
      integer nf_max_var_dims

      parameter (nf_max_dims = 1024)
      parameter (nf_max_attrs = 8192)
      parameter (nf_max_vars = 8192)
      parameter (nf_max_name = 256)
      parameter (nf_max_var_dims = nf_max_dims)

!
! error codes:
!
      integer nf_noerr
      integer nf_ebadid
      integer nf_eexist
      integer nf_einval
      integer nf_eperm
      integer nf_enotindefine
      integer nf_eindefine
      integer nf_einvalcoords
      integer nf_emaxdims
      integer nf_enameinuse
      integer nf_enotatt
      integer nf_emaxatts
      integer nf_ebadtype
      integer nf_ebaddim
      integer nf_eunlimpos
      integer nf_emaxvars
      integer nf_enotvar
      integer nf_eglobal
      integer nf_enotnc
      integer nf_ests
      integer nf_emaxname
      integer nf_eunlimit
      integer nf_enorecvars
      integer nf_echar
      integer nf_eedge
      integer nf_estride
      integer nf_ebadname
      integer nf_erange
      integer nf_enomem
      integer nf_evarsize
      integer nf_edimsize
      integer nf_etrunc

      parameter (nf_noerr = 0)
      parameter (nf_ebadid = -33)
      parameter (nf_eexist = -35)
      parameter (nf_einval = -36)
      parameter (nf_eperm = -37)
      parameter (nf_enotindefine = -38)
      parameter (nf_eindefine = -39)
      parameter (nf_einvalcoords = -40)
      parameter (nf_emaxdims = -41)
      parameter (nf_enameinuse = -42)
      parameter (nf_enotatt = -43)
      parameter (nf_emaxatts = -44)
      parameter (nf_ebadtype = -45)
      parameter (nf_ebaddim = -46)
      parameter (nf_eunlimpos = -47)
      parameter (nf_emaxvars = -48)
      parameter (nf_enotvar = -49)
      parameter (nf_eglobal = -50)
      parameter (nf_enotnc = -51)
      parameter (nf_ests = -52)
      parameter (nf_emaxname = -53)
      parameter (nf_eunlimit = -54)
      parameter (nf_enorecvars = -55)
      parameter (nf_echar = -56)
      parameter (nf_eedge = -57)
      parameter (nf_estride = -58)
      parameter (nf_ebadname = -59)
      parameter (nf_erange = -60)
      parameter (nf_enomem = -61)
      parameter (nf_evarsize = -62)
      parameter (nf_edimsize = -63)
      parameter (nf_etrunc = -64)
!
! error handling modes:
!
      integer  nf_fatal
      integer nf_verbose

      parameter (nf_fatal = 1)
      parameter (nf_verbose = 2)

!
! miscellaneous routines:
!
      character*80   nf_inq_libvers
      external       nf_inq_libvers

      character*80   nf_strerror
!                         (integer             ncerr)
      external       nf_strerror

      logical        nf_issyserr
!                         (integer             ncerr)
      external       nf_issyserr

!
! control routines:
!
      integer         nf_inq_base_pe
!                         (integer             ncid,
!                          integer             pe)
      external        nf_inq_base_pe

      integer         nf_set_base_pe
!                         (integer             ncid,
!                          integer             pe)
      external        nf_set_base_pe

      integer         nf_create
!                         (character*(*)       path,
!                          integer             cmode,
!                          integer             ncid)
      external        nf_create

      integer         nf__create
!                         (character*(*)       path,
!                          integer             cmode,
!                          integer             initialsz,
!                          integer             chunksizehint,
!                          integer             ncid)
      external        nf__create

      integer         nf__create_mp
!                         (character*(*)       path,
!                          integer             cmode,
!                          integer             initialsz,
!                          integer             basepe,
!                          integer             chunksizehint,
!                          integer             ncid)
      external        nf__create_mp

      integer         nf_open
!                         (character*(*)       path,
!                          integer             mode,
!                          integer             ncid)
      external        nf_open

      integer         nf__open
!                         (character*(*)       path,
!                          integer             mode,
!                          integer             chunksizehint,
!                          integer             ncid)
      external        nf__open

      integer         nf__open_mp
!                         (character*(*)       path,
!                          integer             mode,
!                          integer             basepe,
!                          integer             chunksizehint,
!                          integer             ncid)
      external        nf__open_mp

      integer         nf_set_fill
!                         (integer             ncid,
!                          integer             fillmode,
!                          integer             old_mode)
      external        nf_set_fill

      integer         nf_set_default_format
!                          (integer             format,
!                          integer             old_format)
      external        nf_set_default_format

      integer         nf_redef
!                         (integer             ncid)
      external        nf_redef

      integer         nf_enddef
!                         (integer             ncid)
      external        nf_enddef

      integer         nf__enddef
!                         (integer             ncid,
!                          integer             h_minfree,
!                          integer             v_align,
!                          integer             v_minfree,
!                          integer             r_align)
      external        nf__enddef

      integer         nf_sync
!                         (integer             ncid)
      external        nf_sync

      integer         nf_abort
!                         (integer             ncid)
      external        nf_abort

      integer         nf_close
!                         (integer             ncid)
      external        nf_close

      integer         nf_delete
!                         (character*(*)       ncid)
      external        nf_delete

!
! general inquiry routines:
!

      integer         nf_inq
!                         (integer             ncid,
!                          integer             ndims,
!                          integer             nvars,
!                          integer             ngatts,
!                          integer             unlimdimid)
      external        nf_inq

      integer         nf_inq_ndims
!                         (integer             ncid,
!                          integer             ndims)
      external        nf_inq_ndims

      integer         nf_inq_nvars
!                         (integer             ncid,
!                          integer             nvars)
      external        nf_inq_nvars

      integer         nf_inq_natts
!                         (integer             ncid,
!                          integer             ngatts)
      external        nf_inq_natts

      integer         nf_inq_unlimdim
!                         (integer             ncid,
!                          integer             unlimdimid)
      external        nf_inq_unlimdim

      integer         nf_inq_format
!                         (integer             ncid,
!                          integer             format)
      external        nf_inq_format

!
! dimension routines:
!

      integer         nf_def_dim
!                         (integer             ncid,
!                          character(*)        name,
!                          integer             len,
!                          integer             dimid)
      external        nf_def_dim

      integer         nf_inq_dimid
!                         (integer             ncid,
!                          character(*)        name,
!                          integer             dimid)
      external        nf_inq_dimid

      integer         nf_inq_dim
!                         (integer             ncid,
!                          integer             dimid,
!                          character(*)        name,
!                          integer             len)
      external        nf_inq_dim

      integer         nf_inq_dimname
!                         (integer             ncid,
!                          integer             dimid,
!                          character(*)        name)
      external        nf_inq_dimname

      integer         nf_inq_dimlen
!                         (integer             ncid,
!                          integer             dimid,
!                          integer             len)
      external        nf_inq_dimlen

      integer         nf_rename_dim
!                         (integer             ncid,
!                          integer             dimid,
!                          character(*)        name)
      external        nf_rename_dim

!
! general attribute routines:
!

      integer         nf_inq_att
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          integer             xtype,
!                          integer             len)
      external        nf_inq_att

      integer         nf_inq_attid
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          integer             attnum)
      external        nf_inq_attid

      integer         nf_inq_atttype
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          integer             xtype)
      external        nf_inq_atttype

      integer         nf_inq_attlen
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          integer             len)
      external        nf_inq_attlen

      integer         nf_inq_attname
!                         (integer             ncid,
!                          integer             varid,
!                          integer             attnum,
!                          character(*)        name)
      external        nf_inq_attname

      integer         nf_copy_att
!                         (integer             ncid_in,
!                          integer             varid_in,
!                          character(*)        name,
!                          integer             ncid_out,
!                          integer             varid_out)
      external        nf_copy_att

      integer         nf_rename_att
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        curname,
!                          character(*)        newname)
      external        nf_rename_att

      integer         nf_del_att
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name)
      external        nf_del_att

!
! attribute put/get routines:
!

      integer         nf_put_att_text
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          integer             len,
!                          character(*)        text)
      external        nf_put_att_text

      integer         nf_get_att_text
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          character(*)        text)
      external        nf_get_att_text

      integer         nf_put_att_int1
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          integer             xtype,
!                          integer             len,
!                          nf_int1_t           i1vals(1))
      external        nf_put_att_int1

      integer         nf_get_att_int1
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          nf_int1_t           i1vals(1))
      external        nf_get_att_int1

      integer         nf_put_att_int2
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          integer             xtype,
!                          integer             len,
!                          nf_int2_t           i2vals(1))
      external        nf_put_att_int2

      integer         nf_get_att_int2
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          nf_int2_t           i2vals(1))
      external        nf_get_att_int2

      integer         nf_put_att_int
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          integer             xtype,
!                          integer             len,
!                          integer             ivals(1))
      external        nf_put_att_int

      integer         nf_get_att_int
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          integer             ivals(1))
      external        nf_get_att_int

      integer         nf_put_att_real
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          integer             xtype,
!                          integer             len,
!                          real                rvals(1))
      external        nf_put_att_real

      integer         nf_get_att_real
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          real                rvals(1))
      external        nf_get_att_real

      integer         nf_put_att_double
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          integer             xtype,
!                          integer             len,
!                          double              dvals(1))
      external        nf_put_att_double

      integer         nf_get_att_double
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          double              dvals(1))
      external        nf_get_att_double

!
! general variable routines:
!

      integer         nf_def_var
!                         (integer             ncid,
!                          character(*)        name,
!                          integer             datatype,
!                          integer             ndims,
!                          integer             dimids(1),
!                          integer             varid)
      external        nf_def_var

      integer         nf_inq_var
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          integer             datatype,
!                          integer             ndims,
!                          integer             dimids(1),
!                          integer             natts)
      external        nf_inq_var

      integer         nf_inq_varid
!                         (integer             ncid,
!                          character(*)        name,
!                          integer             varid)
      external        nf_inq_varid

      integer         nf_inq_varname
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name)
      external        nf_inq_varname

      integer         nf_inq_vartype
!                         (integer             ncid,
!                          integer             varid,
!                          integer             xtype)
      external        nf_inq_vartype

      integer         nf_inq_varndims
!                         (integer             ncid,
!                          integer             varid,
!                          integer             ndims)
      external        nf_inq_varndims

      integer         nf_inq_vardimid
!                         (integer             ncid,
!                          integer             varid,
!                          integer             dimids(1))
      external        nf_inq_vardimid

      integer         nf_inq_varnatts
!                         (integer             ncid,
!                          integer             varid,
!                          integer             natts)
      external        nf_inq_varnatts

      integer         nf_rename_var
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name)
      external        nf_rename_var

      integer         nf_copy_var
!                         (integer             ncid_in,
!                          integer             varid,
!                          integer             ncid_out)
      external        nf_copy_var

!
! entire variable put/get routines:
!

      integer         nf_put_var_text
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        text)
      external        nf_put_var_text

      integer         nf_get_var_text
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        text)
      external        nf_get_var_text

      integer         nf_put_var_int1
!                         (integer             ncid,
!                          integer             varid,
!                          nf_int1_t           i1vals(1))
      external        nf_put_var_int1

      integer         nf_get_var_int1
!                         (integer             ncid,
!                          integer             varid,
!                          nf_int1_t           i1vals(1))
      external        nf_get_var_int1

      integer         nf_put_var_int2
!                         (integer             ncid,
!                          integer             varid,
!                          nf_int2_t           i2vals(1))
      external        nf_put_var_int2

      integer         nf_get_var_int2
!                         (integer             ncid,
!                          integer             varid,
!                          nf_int2_t           i2vals(1))
      external        nf_get_var_int2

      integer         nf_put_var_int
!                         (integer             ncid,
!                          integer             varid,
!                          integer             ivals(1))
      external        nf_put_var_int

      integer         nf_get_var_int
!                         (integer             ncid,
!                          integer             varid,
!                          integer             ivals(1))
      external        nf_get_var_int

      integer         nf_put_var_real
!                         (integer             ncid,
!                          integer             varid,
!                          real                rvals(1))
      external        nf_put_var_real

      integer         nf_get_var_real
!                         (integer             ncid,
!                          integer             varid,
!                          real                rvals(1))
      external        nf_get_var_real

      integer         nf_put_var_double
!                         (integer             ncid,
!                          integer             varid,
!                          doubleprecision     dvals(1))
      external        nf_put_var_double

      integer         nf_get_var_double
!                         (integer             ncid,
!                          integer             varid,
!                          doubleprecision     dvals(1))
      external        nf_get_var_double

!
! single variable put/get routines:
!

      integer         nf_put_var1_text
!                         (integer             ncid,
!                          integer             varid,
!                          integer             index(1),
!                          character*1         text)
      external        nf_put_var1_text

      integer         nf_get_var1_text
!                         (integer             ncid,
!                          integer             varid,
!                          integer             index(1),
!                          character*1         text)
      external        nf_get_var1_text

      integer         nf_put_var1_int1
!                         (integer             ncid,
!                          integer             varid,
!                          integer             index(1),
!                          nf_int1_t           i1val)
      external        nf_put_var1_int1

      integer         nf_get_var1_int1
!                         (integer             ncid,
!                          integer             varid,
!                          integer             index(1),
!                          nf_int1_t           i1val)
      external        nf_get_var1_int1

      integer         nf_put_var1_int2
!                         (integer             ncid,
!                          integer             varid,
!                          integer             index(1),
!                          nf_int2_t           i2val)
      external        nf_put_var1_int2

      integer         nf_get_var1_int2
!                         (integer             ncid,
!                          integer             varid,
!                          integer             index(1),
!                          nf_int2_t           i2val)
      external        nf_get_var1_int2

      integer         nf_put_var1_int
!                         (integer             ncid,
!                          integer             varid,
!                          integer             index(1),
!                          integer             ival)
      external        nf_put_var1_int

      integer         nf_get_var1_int
!                         (integer             ncid,
!                          integer             varid,
!                          integer             index(1),
!                          integer             ival)
      external        nf_get_var1_int

      integer         nf_put_var1_real
!                         (integer             ncid,
!                          integer             varid,
!                          integer             index(1),
!                          real                rval)
      external        nf_put_var1_real

      integer         nf_get_var1_real
!                         (integer             ncid,
!                          integer             varid,
!                          integer             index(1),
!                          real                rval)
      external        nf_get_var1_real

      integer         nf_put_var1_double
!                         (integer             ncid,
!                          integer             varid,
!                          integer             index(1),
!                          doubleprecision     dval)
      external        nf_put_var1_double

      integer         nf_get_var1_double
!                         (integer             ncid,
!                          integer             varid,
!                          integer             index(1),
!                          doubleprecision     dval)
      external        nf_get_var1_double

!
! variable array put/get routines:
!

      integer         nf_put_vara_text
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          character(*)        text)
      external        nf_put_vara_text

      integer         nf_get_vara_text
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          character(*)        text)
      external        nf_get_vara_text

      integer         nf_put_vara_int1
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          nf_int1_t           i1vals(1))
      external        nf_put_vara_int1

      integer         nf_get_vara_int1
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          nf_int1_t           i1vals(1))
      external        nf_get_vara_int1

      integer         nf_put_vara_int2
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          nf_int2_t           i2vals(1))
      external        nf_put_vara_int2

      integer         nf_get_vara_int2
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          nf_int2_t           i2vals(1))
      external        nf_get_vara_int2

      integer         nf_put_vara_int
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             ivals(1))
      external        nf_put_vara_int

      integer         nf_get_vara_int
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             ivals(1))
      external        nf_get_vara_int

      integer         nf_put_vara_real
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          real                rvals(1))
      external        nf_put_vara_real

      integer         nf_get_vara_real
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          real                rvals(1))
      external        nf_get_vara_real

      integer         nf_put_vara_double
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          doubleprecision     dvals(1))
      external        nf_put_vara_double

      integer         nf_get_vara_double
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          doubleprecision     dvals(1))
      external        nf_get_vara_double

!
! strided variable put/get routines:
!

      integer         nf_put_vars_text
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          character(*)        text)
      external        nf_put_vars_text

      integer         nf_get_vars_text
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          character(*)        text)
      external        nf_get_vars_text

      integer         nf_put_vars_int1
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          nf_int1_t           i1vals(1))
      external        nf_put_vars_int1

      integer         nf_get_vars_int1
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          nf_int1_t           i1vals(1))
      external        nf_get_vars_int1

      integer         nf_put_vars_int2
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          nf_int2_t           i2vals(1))
      external        nf_put_vars_int2

      integer         nf_get_vars_int2
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          nf_int2_t           i2vals(1))
      external        nf_get_vars_int2

      integer         nf_put_vars_int
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          integer             ivals(1))
      external        nf_put_vars_int

      integer         nf_get_vars_int
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          integer             ivals(1))
      external        nf_get_vars_int

      integer         nf_put_vars_real
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          real                rvals(1))
      external        nf_put_vars_real

      integer         nf_get_vars_real
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          real                rvals(1))
      external        nf_get_vars_real

      integer         nf_put_vars_double
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          doubleprecision     dvals(1))
      external        nf_put_vars_double

      integer         nf_get_vars_double
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          doubleprecision     dvals(1))
      external        nf_get_vars_double

!
! mapped variable put/get routines:
!

      integer         nf_put_varm_text
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          integer             imap(1),
!                          character(*)        text)
      external        nf_put_varm_text

      integer         nf_get_varm_text
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          integer             imap(1),
!                          character(*)        text)
      external        nf_get_varm_text

      integer         nf_put_varm_int1
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          integer             imap(1),
!                          nf_int1_t           i1vals(1))
      external        nf_put_varm_int1

      integer         nf_get_varm_int1
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          integer             imap(1),
!                          nf_int1_t           i1vals(1))
      external        nf_get_varm_int1

      integer         nf_put_varm_int2
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          integer             imap(1),
!                          nf_int2_t           i2vals(1))
      external        nf_put_varm_int2

      integer         nf_get_varm_int2
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          integer             imap(1),
!                          nf_int2_t           i2vals(1))
      external        nf_get_varm_int2

      integer         nf_put_varm_int
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          integer             imap(1),
!                          integer             ivals(1))
      external        nf_put_varm_int

      integer         nf_get_varm_int
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          integer             imap(1),
!                          integer             ivals(1))
      external        nf_get_varm_int

      integer         nf_put_varm_real
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          integer             imap(1),
!                          real                rvals(1))
      external        nf_put_varm_real

      integer         nf_get_varm_real
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          integer             imap(1),
!                          real                rvals(1))
      external        nf_get_varm_real

      integer         nf_put_varm_double
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          integer             imap(1),
!                          doubleprecision     dvals(1))
      external        nf_put_varm_double

      integer         nf_get_varm_double
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          integer             imap(1),
!                          doubleprecision     dvals(1))
      external        nf_get_varm_double

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
! begin netcdf 2.4 backward compatibility:
!

!      
! functions in the fortran interface
!
      integer nccre
      integer ncopn
      integer ncddef
      integer ncdid
      integer ncvdef
      integer ncvid
      integer nctlen
      integer ncsfil

      external nccre
      external ncopn
      external ncddef
      external ncdid
      external ncvdef
      external ncvid
      external nctlen
      external ncsfil


      integer ncrdwr
      integer nccreat
      integer ncexcl
      integer ncindef
      integer ncnsync
      integer nchsync
      integer ncndirty
      integer nchdirty
      integer nclink
      integer ncnowrit
      integer ncwrite
      integer ncclob
      integer ncnoclob
      integer ncglobal
      integer ncfill
      integer ncnofill
      integer maxncop
      integer maxncdim
      integer maxncatt
      integer maxncvar
      integer maxncnam
      integer maxvdims
      integer ncnoerr
      integer ncebadid
      integer ncenfile
      integer nceexist
      integer nceinval
      integer nceperm
      integer ncenotin
      integer nceindef
      integer ncecoord
      integer ncemaxds
      integer ncename
      integer ncenoatt
      integer ncemaxat
      integer ncebadty
      integer ncebadd
      integer ncests
      integer nceunlim
      integer ncemaxvs
      integer ncenotvr
      integer nceglob
      integer ncenotnc
      integer ncfoobar
      integer ncsyserr
      integer ncfatal
      integer ncverbos
      integer ncentool


!
! netcdf data types:
!
      integer ncbyte
      integer ncchar
      integer ncshort
      integer nclong
      integer ncfloat
      integer ncdouble

      parameter(ncbyte = 1)
      parameter(ncchar = 2)
      parameter(ncshort = 3)
      parameter(nclong = 4)
      parameter(ncfloat = 5)
      parameter(ncdouble = 6)

!     
!     masks for the struct nc flag field; passed in as 'mode' arg to
!     nccreate and ncopen.
!     

!     read/write, 0 => readonly 
      parameter(ncrdwr = 1)
!     in create phase, cleared by ncendef 
      parameter(nccreat = 2)
!     on create destroy existing file 
      parameter(ncexcl = 4)
!     in define mode, cleared by ncendef 
      parameter(ncindef = 8)
!     synchronise numrecs on change (x'10')
      parameter(ncnsync = 16)
!     synchronise whole header on change (x'20')
      parameter(nchsync = 32)
!     numrecs has changed (x'40')
      parameter(ncndirty = 64)  
!     header info has changed (x'80')
      parameter(nchdirty = 128)
!     prefill vars on endef and increase of record, the default behavior
      parameter(ncfill = 0)
!     do not fill vars on endef and increase of record (x'100')
      parameter(ncnofill = 256)
!     isa link (x'8000')
      parameter(nclink = 32768)

!     
!     'mode' arguments for nccreate and ncopen
!     
      parameter(ncnowrit = 0)
      parameter(ncwrite = ncrdwr)
      parameter(ncclob = nf_clobber)
      parameter(ncnoclob = nf_noclobber)

!     
!     'size' argument to ncdimdef for an unlimited dimension
!     
      integer ncunlim
      parameter(ncunlim = 0)

!     
!     attribute id to put/get a global attribute
!     
      parameter(ncglobal  = 0)

!     
!     advisory maximums:
!     
      parameter(maxncop = 64)
      parameter(maxncdim = 1024)
      parameter(maxncatt = 8192)
      parameter(maxncvar = 8192)
!     not enforced 
      parameter(maxncnam = 256)
      parameter(maxvdims = maxncdim)

!     
!     global netcdf error status variable
!     initialized in error.c
!     

!     no error 
      parameter(ncnoerr = nf_noerr)
!     not a netcdf id 
      parameter(ncebadid = nf_ebadid)
!     too many netcdfs open 
      parameter(ncenfile = -31)   ! nc_syserr
!     netcdf file exists && ncnoclob
      parameter(nceexist = nf_eexist)
!     invalid argument 
      parameter(nceinval = nf_einval)
!     write to read only 
      parameter(nceperm = nf_eperm)
!     operation not allowed in data mode 
      parameter(ncenotin = nf_enotindefine )   
!     operation not allowed in define mode 
      parameter(nceindef = nf_eindefine)   
!     coordinates out of domain 
      parameter(ncecoord = nf_einvalcoords)
!     maxncdims exceeded 
      parameter(ncemaxds = nf_emaxdims)
!     string match to name in use 
      parameter(ncename = nf_enameinuse)   
!     attribute not found 
      parameter(ncenoatt = nf_enotatt)
!     maxncattrs exceeded 
      parameter(ncemaxat = nf_emaxatts)
!     not a netcdf data type 
      parameter(ncebadty = nf_ebadtype)
!     invalid dimension id 
      parameter(ncebadd = nf_ebaddim)
!     ncunlimited in the wrong index 
      parameter(nceunlim = nf_eunlimpos)
!     maxncvars exceeded 
      parameter(ncemaxvs = nf_emaxvars)
!     variable not found 
      parameter(ncenotvr = nf_enotvar)
!     action prohibited on ncglobal varid 
      parameter(nceglob = nf_eglobal)
!     not a netcdf file 
      parameter(ncenotnc = nf_enotnc)
      parameter(ncests = nf_ests)
      parameter (ncentool = nf_emaxname) 
      parameter(ncfoobar = 32)
      parameter(ncsyserr = -31)

!     
!     global options variable. used to determine behavior of error handler.
!     initialized in lerror.c
!     
      parameter(ncfatal = 1)
      parameter(ncverbos = 2)

!
!     default fill values.  these must be the same as in the c interface.
!
      integer filbyte
      integer filchar
      integer filshort
      integer fillong
      real filfloat
      doubleprecision fildoub

      parameter (filbyte = -127)
      parameter (filchar = 0)
      parameter (filshort = -32767)
      parameter (fillong = -2147483647)
      parameter (filfloat = 9.9692099683868690e+36)
      parameter (fildoub = 9.9692099683868690e+36)
   
   character(len=200), intent(in)  :: input_file       ! 1 file nane.
   character(len=12),  intent(in)  :: var              ! Variable to search for.
   integer,            intent(in)  :: field_dims       ! # Dimensions of field. 
   integer,            intent(in)  :: dim1             ! Dimension 1 of field. 
   integer,            intent(in)  :: dim2             ! Dimension 2 of field. 
   integer,            intent(in)  :: dim3             ! Dimension 3 of field. 
   integer,            intent(in)  :: k                ! Vertical index.
   real,               intent(out) :: field(1:dim1,1:dim2) ! Output field

   integer                   :: cdfid            ! 1 file id.
   integer                   :: rcode            ! Return code(0=ok).
   integer                   :: length           ! Length of filename.
   integer                   :: id_var           ! 1 variable ID. 

   integer                   :: istart(4)        ! Start value of arrays.
   integer                   :: iend(4)          ! End value of arrays.
   real(kind=4), allocatable :: field1d(:)       ! Used if 1D field read. 
   real(kind=4), allocatable :: field2d(:,:)     ! Used if 2D field read. 
   real(kind=4), allocatable :: field3d(:,:,:)   ! Used if 3D field read. 

   length = len_trim(input_file)
   rcode = nf_open( input_file(1:length), NF_NOwrite, cdfid)

   !  Check variable is in file:
   rcode = nf_inq_varid( cdfid, var, id_var)
   if (rcode /= 0) then
      write(unit=message(1),fmt='(2a)')var, ' variable is not in input file'
      call da_error("da_get_field.inc",40,message(1:1))
   end if

   istart = 1
   iend(1) = dim1
   iend(2) = dim2
   iend(4) = 1          ! Single time assumed.

   if (field_dims == 1) then
      iend(2) = 1
      iend(3) = 1
      allocate( field1d(1:dim1))
      call ncvgt( cdfid, id_var, istart, iend, field1d, rcode)
      field(:,1) = field1d(:)
      rcode = nf_close( cdfid)
      deallocate( field1d)
   else if (field_dims == 2) then
      iend(3) = 1
      allocate( field2d(1:dim1,1:dim2))
      call ncvgt( cdfid, id_var, istart, iend, field2d, rcode)
      field(:,:) = field2d(:,:)
      rcode = nf_close( cdfid)
      deallocate( field2d)
   else if (field_dims == 3) then
      iend(3) = dim3
      allocate( field3d(1:dim1,1:dim2,1:dim3))
      call ncvgt( cdfid, id_var, istart, iend, field3d, rcode)
      field(:,:) = field3d(:,:,k)
      deallocate( field3d)
   end if

   rcode = nf_close( cdfid)

end subroutine da_get_field


subroutine da_get_height( input_file, dim1, dim2, dim3, height)

   !---------------------------------------------------------------------------
   ! Purpose: Calculates T, RH from input WRF file.
   !---------------------------------------------------------------------------
   
   implicit none
   
   character(len=200), intent(in)  :: input_file       ! 1 file nane.
   integer,            intent(in)  :: dim1, dim2, dim3          ! Dimensions.
   real(kind=8),               intent(out) :: height(1:dim1,1:dim2,1:dim3) ! Height.

   character(len=12) :: var                            ! Variable to search for.
   integer           :: k                              ! Loop counter.
   real              :: gravity_inv                    ! 1/gravity.
   real              :: phb(1:dim1,1:dim2)             ! Base state geopotential.
   real              :: ph(1:dim1,1:dim2)              ! Perturbation geopotential.
   real              :: phfull(1:dim1,1:dim2,1:dim3+1) ! Geopotential.

   gravity_inv = 1.0 / gravity

   do k = 1, dim3+1
      var = "PHB"
      call da_get_field( input_file, var, 3, dim1, dim2, dim3+1, k, phb)
      var = "PH"
      call da_get_field( input_file, var, 3, dim1, dim2, dim3+1, k, ph)

      phfull(:,:,k) = phb + ph ! Calculate full geopotential on full(w) model levels:
   end do

   do k = 1, dim3
      height(:,:,k) = 0.5 *( phfull(:,:,k+1) + phfull(:,:,k)) * gravity_inv
   end do

end subroutine da_get_height


subroutine da_get_trh( input_file, dim1, dim2, dim3, k, temp, rh )

   !---------------------------------------------------------------------------
   ! Purpose: Calculates T, RH from input WRF file.
   !---------------------------------------------------------------------------

   implicit none

   character(len=200), intent(in)  :: input_file       ! 1 file name.
   integer,            intent(in)  :: dim1, dim2, dim3          ! Dimensions.
   integer,            intent(in)  :: k                         ! Model level.
   real,               intent(out) :: temp(1:dim1,1:dim2)       ! Temperature.
   real,               intent(out) :: rh(1:dim1,1:dim2)         ! Relative humidity.

   character(len=12) :: var                       ! Variable to search for. var = "T"
   integer           :: i, j                      ! Loop counters.

   real              :: thetap(1:dim1,1:dim2)     ! Perturbation potential temperature.
   real              :: pb(1:dim1,1:dim2)         ! Base state pressure.
   real              :: pp(1:dim1,1:dim2)         ! Pressure perturbation.
   real              :: x(1:dim1,1:dim2)          ! Vapor mixing ratio.

   real              :: theta                     ! Potential temperature.
   real              :: p                         ! Pressure.
   real              :: q                         ! Specific humidity.
   real              :: t_c                       ! Temp(Celsius).
   real              :: es                        ! Saturation vapor pressure.
   real              :: qs                        ! Saturation specific humidity.

   var = "T" ! Perturbation potential temperature in WRF.
   call da_get_field( input_file, var, 3, dim1, dim2, dim3, k, thetap)

   var = "PB"  ! Base state pressure in WRF.
   call da_get_field( input_file, var, 3, dim1, dim2, dim3, k, pb)

   var = "P" ! Perturbation pressure in WRF.
   call da_get_field( input_file, var, 3, dim1, dim2, dim3, k, pp)

   var = "QVAPOR"  ! Water vapor mixing ratio.
   call da_get_field( input_file, var, 3, dim1, dim2, dim3, k, x)

   do j = 1, dim2
      do i = 1, dim1
         ! Convert p', theta' to T:
         theta = t0 + thetap(i,j)                 ! Theta = Theta0 + Thetap
         p = pb(i,j) + pp(i,j)                     ! p = base p + pert p.
         temp(i,j) = theta *( p/base_pres)**kappa ! Theta to T.

         ! Convert to specific humidity.
         q = x(i,j) /( 1.0 + x(i,j))

         ! Calculate relative humidity:
         t_c = temp(i,j) - t_kelvin
         es = es_alpha * exp( es_beta * t_c /( t_c + es_gamma))
         qs = rd_over_rv * es /( p - rd_over_rv1 * es)
         rh(i,j) = 100.0 * q / qs
      end do
   end do

end subroutine da_get_trh


subroutine da_print_be_stats_h_global(outunit, variable, k, max_wavenumber, &
  total_power)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none 

   integer,      intent(inout) :: outunit        ! Output file unit.
   character*10, intent(in)    :: variable       ! Variable name.
   integer,      intent(in)    :: k              ! Vertical index.
   integer,      intent(in)    :: max_wavenumber ! Smallest scale required (ni/2-1)

   ! Total Power spectrum (averaged over time/members)
   real,         intent(in) :: total_power(0:max_wavenumber)

   integer :: n                     ! Loop counter.
   real    :: accum_power, variance ! Accumulated power, variance.

   accum_power = 0.0
   variance = sum(total_power(0:max_wavenumber))

   write(unit=stdout,fmt='(3a,i5,a,i5)')' Power spectra for variable ', trim(variable), &
                          ' and level ', k, ' in unit ', outunit

   do n = 0, max_wavenumber
      accum_power = accum_power + total_power(n)
      write(unit=outunit,fmt='(2i4,2f18.5,f8.3)')k, n, total_power(n), accum_power, &
                                        accum_power / variance
   end do

   outunit = outunit + 1
   write(unit=stdout,fmt=*) ''

end subroutine da_print_be_stats_h_global


subroutine da_print_be_stats_h_regional(outunit, variable, nk, scale_length)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none 

   integer,      intent(inout) :: outunit            ! Output file unit.
   character*10, intent(in)    :: variable           ! Variable name.
   integer,      intent(in)    :: nk                 ! Dimension of vertical index
   real,         intent(in)    :: scale_length(1:nk) ! Correlation scale lengths

   integer :: k                  ! Loop counter.

   write(unit=stdout,fmt='(3a,i5)') &
      ' Scale length for variable ', trim(variable), ' in unit ', outunit

   do k = 1, nk
     write(unit=outunit,fmt='(i4,1pe15.5)')k, scale_length(k)
   end do

   outunit = outunit + 1
   write(unit=stdout,fmt=*) ' '

end subroutine da_print_be_stats_h_regional


subroutine da_print_be_stats_p(outunit, ni, nj, nk, num_bins, num_bins2d, bin, &
   bin2d, regcoeff1, regcoeff2, regcoeff3)

   !----------------------------------------------------------------------------
   ! Purpose: To print out the regression coefficients for the physical
   !           transform.
   !
   ! Input   : outunit              --- Fortran unit for writing out
   !           ni,nj,nk             --- Dimensions
   !           num_bins, num_bins2d --- Number of the 3d and 2d bins
   !           bin, bin2d           --- bin index for the gridpoints
   !           regcoeff1   --- Reg coefs for chi = regcoeff1 * psi
   !           regcoeff2   --- Reg coefs for Ps  = sum(regcoeff2(1:k)*psi(1:k))
   !           regcoeff3   --- Reg coefs for T(k)= sum(regcoeff3(k,1:k)*psi(1:k))
   !
   ! Output  : fort.171    --- regcoef1
   !           fort.172    --- regcoeff2
   !           fort.173    --- regcoeff3
   !----------------------------------------------------------------------------

   implicit none

   integer, intent(inout) :: outunit                      ! Output file unit.
   integer, intent(in) :: ni, nj, nk                      ! Grid dimensions. 
   integer, intent(in) :: num_bins                        ! Number of 3D field bins.
   integer, intent(in) :: num_bins2d                      ! Number of 2D field bins.
   integer, intent(in) :: bin(1:ni,1:nj,1:nk)             ! Bin assigned to each 3D point.
   integer, intent(in) :: bin2d(1:ni,1:nj)                ! Bin assigned to each 3D point.
   real, intent(in)    :: regcoeff1(1:num_bins)           ! psi/chi regression cooefficient.
   real, intent(in)    :: regcoeff2(1:nk,1:num_bins2d)    ! psi/ps regression cooefficient.
   real, intent(in)    :: regcoeff3(1:nk,1:nk,1:num_bins2d) ! psi/T regression cooefficient.

   integer             :: k1, k2, i, k, j, b, number      ! Loop counters.

   write(unit=stdout,fmt='(a,i5)')' da_print_be_stats_p psi/chi regression coefficient in unit ', outunit

   open(unit=outunit)
   do b = 1, num_bins
      number = 0
      do i = 1,ni
         do j = 1,nj
            do k = 1,nk
               if (bin(i,j,k) == b) then
                  number = number + 1
               end if
            end do
         end do
      end do
      write(unit=outunit,fmt='("bin=",i6," number_in_bin=",i6,5X,1pE12.5)') &
         b, number, regcoeff1(b)
   end do
   close(unit=outunit)

   outunit = outunit + 1

   write(unit=stdout,fmt='(a,i5)') &
      ' psi/ps  regression coefficient in unit ', outunit

   open(unit=outunit)
   write(unit=outunit,fmt='(a,i5)') &
      ' psi/ps  regression coefficient in unit ', outunit

   do b = 1, num_bins2d
      number = 0
      do i = 1,ni
         do j = 1,nj
            if (bin2d(i,j) == b) then
               number = number + 1
            end if
         end do
      end do
      write(unit=outunit,fmt='(/"bin=",i6," number_in_bin=",i6)') b, number
      write(unit=outunit,fmt='((2X,9(i3,2x,1pE12.5)))') (k,regcoeff2(k,b), k=1,nk)
   end do
   close(unit=outunit)

   outunit = outunit + 1

   write(unit=stdout,fmt='(a,i5)') &
      ' psi/T   regression coefficient in unit ', outunit

   open(unit=outunit)
   write(unit=outunit,fmt='(a,i5)') &
      ' psi/T   regression coefficient in unit ', outunit
   do b = 1, num_bins2d
      number = 0
      do i = 1,ni
         do j = 1,nj
            if (bin2d(i,j) == b) then
               number = number + 1
            end if
         end do
      end do

      write(unit=outunit,fmt='(/"bin=",i6," number_in_bin=",i6)') b, number
      do k1 = 1,nk
         write(unit=outunit,fmt='(2X,"Temperature at k1=",i3)') k1
         write(unit=outunit,fmt='((2X,9(i3,2x,1pE12.5)))') (k2,regcoeff3(k1,k2,b), k2=1,nk)
      end do
   end do
   close(unit=outunit)

   write(unit=stdout,fmt=*) ' '

   outunit = outunit + 1

end subroutine da_print_be_stats_p


subroutine da_print_be_stats_v(outunit, variable, nk, num_bins2d, &
   e_vec, e_val, e_vec_loc, e_val_loc)

   !----------------------------------------------------------------------------
   ! Purpose: To print out the first global and local eigenvector and 
   !           eigenvalues for 'variable'.
   !
   ! Input   : outunit    --- output unit
   !           variable   --- variable name: psi, chi_u, t_u, rh
   !           nk         --- the number of vertical modes or levels
   !           num_bins2d --- the number of the 2d bins
   !           e_vec, e_val         --- global eigenvector and eigenvalues
   !           e_vec_loc, e_val_loc --- local eigenvectors and eigenvalues
   !
   ! Output  : fort.174,178,182,186,(190) --- The first 5 global eigenvectors
   !           fort.175,179,183,187,(191) --- The global eigenvalues
   !           fort.176,180,184,188,(192) --- The first 5 local eigenvectors
   !           fort.177,181,185,189,(193) --- The first 5 local eigenvalues
   !      
   !          * in parenthisis, the units for 2d fields ps_u (ps).
   !----------------------------------------------------------------------------

   implicit none

   integer, intent(inout)   :: outunit                    ! Output file unit.
   character*10, intent(in) :: variable                   ! Variable name
   integer, intent(in)      :: nk                         ! Vertical dimension.
   integer, intent(in)      :: num_bins2d                 ! # bins for 2D fields.
   real, intent(in)         :: e_vec(1:nk,1:nk)           ! Domain-averaged eigenvectors.
   real, intent(in)         :: e_val(1:nk)                ! Domain-averaged eigenvalues.
   real, intent(in) :: e_vec_loc(1:nk,1:nk,1:num_bins2d)  ! Latitudinally varying eigenvectors.
   real, intent(in) :: e_val_loc(1:nk,1:num_bins2d)       ! Latitudinally varying eigenvalues.

   integer                  :: k, m, b, mn                ! Loop counters.

   if (nk > 5) then
      mn = 5
   else
      mn = nk
   end if

   ! 1, Global vectors:
   write(unit=stdout,fmt='(3a,i5)')' First 5 Global eigenvectors for variable ', trim(variable), &
                     ' in unit ', outunit

   open(unit=outunit)
   do k = 1, nk
      write(unit=outunit,fmt='(i4,5f15.8)') k, (e_vec(k,m), m = 1, mn)
   end do
   close(unit=outunit)

   ! 2, Global values:
   outunit = outunit + 1

   write(unit=stdout,fmt='(3a,i5)')' Global eigenvalues for variable ', trim(variable), &
                     ' in unit ', outunit

   open(unit=outunit)
   do k = 1, nk
      write(unit=outunit,fmt='(i4,1pe18.5)') k, e_val(k)
   end do
   close(unit=outunit)

   ! 3, Local vectors:

   outunit = outunit + 1

   write(unit=stdout,fmt='(3a,i5)')' First 5 local eigenvectors for variable ', trim(variable), &
                     ' in unit ', outunit

   open(unit=outunit)
   do b = 1, num_bins2d
     write(unit=outunit,fmt='(/"bin =",i6)') b
     do k = 1, nk
       write(unit=outunit,fmt='(i4,5f15.8)') k, (e_vec_loc(k,m,b), m = 1, mn)
     end do
   end do
   close(unit=outunit)

   ! 4. Local values:

   outunit = outunit + 1

   write(unit=stdout,fmt='(3a,i5)')' First 5 local eigenvalues for variable ', trim(variable), &
                     ' in unit ', outunit

   open(unit=outunit)
   do b = 1, num_bins2d
      write(unit=outunit,fmt='(i4,5(1pe18.5))') b, (e_val_loc(m,b), m = 1, mn)
   end do
   close(unit=outunit)

   outunit = outunit + 1 
   write(unit=stdout,fmt=*) ' '

end subroutine da_print_be_stats_v


subroutine da_readwrite_be_stage2(outunit, nk)

   ! ----------------------------------------------------------------------
   ! Purpose: Read and write the dimensions, bin information, and 
   !          regression coefficients.
   !  Update: Multivariate BE option (cv_options=6)
   !          Syed RH Rizvi (MMM/NESL/NCAR)   Date: 02/01/2010
   !
   !  Note: Please acknowledge author/institute in work that uses this code.
   !
   ! ----------------------------------------------------------------------

   implicit none

   integer, intent(in)      :: outunit                    ! Output unit number.
   integer, intent(out)     :: nk                         ! Number of vertical levels/modes.
   character*10        :: start_date, end_date       ! Starting and ending dates.
   character*10        :: date, new_date             ! Current date (ccyymmddhh).
   integer             :: interval                   ! Period between dates (hours).
   integer             :: ne                         ! Number of ensemble members.
   real                :: rf_scale                   ! Recursive filter scale.
   integer             :: num_passes                 ! Recursive filter passes.
 
   character(len=filename_len) :: filename                   ! Input filename.
   character*80             :: variable_psi_chi
   character*80             :: variable_psi_t
   character*80             :: variable_psi_ps
   character*80             :: variable_psi_rh
   character*80             :: variable_chi_u_t
   character*80             :: variable_chi_u_ps
   character*80             :: variable_chi_u_rh
   character*80             :: variable_t_u_rh
   character*80             :: variable_ps_u_rh

   integer                  :: ni, nj                     ! Number of points in x/y direction.
   integer                  :: bin_type                   ! Type of bin to average over. !!!DALE ADD.
   integer                  :: num_bins                   ! Number of 3D bins.
   integer                  :: num_bins2d                 ! Number of 2D bins.

   real                :: hgt_min, hgt_max           ! Used if bin_type = 2 (m).
   real                :: lat_min, lat_max           ! Used if bin_type = 2 (degrees).
   real                :: binwidth_lat               ! Used if bin_type = 2 (degrees). !!!DALE ADD..
   real                :: binwidth_hgt               ! Used if bin_type = 2 (m). !!!DALE ADD..
   integer, allocatable:: bin(:,:,:)                 ! Bin assigned to each 3D point.
   integer, allocatable:: bin2d(:,:)                 ! Bin assigned to each 2D point.


   real, allocatable   :: regcoeff_psi_chi(:)        ! chi/psi regression cooefficient.
   real, allocatable   :: regcoeff_psi_ps(:,:)       ! ps/psi regression cooefficient.
   real, allocatable   :: regcoeff_psi_t(:,:,:)      ! t/psi regression cooefficient.
   real, allocatable   :: regcoeff_psi_rh(:,:,:)     ! rh/psi regression cooefficient.

   real, allocatable   :: regcoeff_chi_u_ps(:,:)     ! ps/chi_u regression coefficient
   real, allocatable   :: regcoeff_chi_u_t(:,:,:)    ! t/chi_u regression coefficient
   real, allocatable   :: regcoeff_ps_u_rh(:,:)      ! rh/ps_u regression coefficient
   real, allocatable   :: regcoeff_chi_u_rh(:,:,:)   ! rh/chi_u regression coefficient
   real, allocatable   :: regcoeff_t_u_rh(:,:,:)     ! rh/t_u regression coefficient

   character (len=4)   :: masscv                     ! Mass ctrl variable = press or temp.
   character (len=10)  :: balpres                    ! Balance pressure = purestats or linestats
   logical             :: nobaldiv                   ! True means chi_u = chi.


   integer :: iunit, namelist_unit

   namelist / gen_be_stage2a_nl / start_date, end_date, interval, &
                                 ne, num_passes, rf_scale, cv_options, &
                                 masscv, balpres, nobaldiv

   start_date = '2004030312'
   end_date = '2004033112'
   interval = 24
   ne = 1
   num_passes = 0
   rf_scale = 1.0
   cv_options = 5

   call da_get_unit(namelist_unit)

   ! Reading Namelist:            
   open(unit=namelist_unit, file='gen_be_stage2a_nl.nl', &
        form='formatted', status='old', action='read')
   read(namelist_unit, gen_be_stage2a_nl)
   close(namelist_unit)
  
   ! Read in the coefficients:

   call da_get_unit(iunit)
   filename = 'gen_be_stage2.dat'
   open (iunit, file = filename, form='unformatted')
   read(iunit)ni, nj, nk
   read(iunit)num_bins, num_bins2d

   allocate( regcoeff_psi_chi(1:num_bins) )
   allocate( regcoeff_psi_ps(1:nk,1:num_bins2d) )
   allocate( regcoeff_psi_t(1:nk,1:nk,1:num_bins2d) )
   allocate( regcoeff_psi_rh(1:nk,1:nk,1:num_bins2d) )

   allocate( regcoeff_chi_u_ps(1:nk,1:num_bins2d) )
   allocate( regcoeff_chi_u_t(1:nk,1:nk,1:num_bins2d) )
   allocate( regcoeff_chi_u_rh(1:nk,1:nk,1:num_bins2d) )

   allocate( regcoeff_ps_u_rh(1:nk,1:num_bins2d) )
   allocate( regcoeff_t_u_rh(1:nk,1:nk,1:num_bins2d) )

   if( cv_options == 5) then
   read(iunit)regcoeff_psi_chi
   read(iunit)regcoeff_psi_ps
   read(iunit)regcoeff_psi_t
   end if

   if( cv_options == 6) then
   read(iunit)variable_psi_chi
   read(iunit)regcoeff_psi_chi

   read(iunit)variable_psi_t
   read(iunit)regcoeff_psi_t

   read(iunit)variable_psi_ps
   read(iunit)regcoeff_psi_ps

   read(iunit)variable_psi_rh
   read(iunit)regcoeff_psi_rh

   read(iunit)variable_chi_u_t
   read(iunit)regcoeff_chi_u_t

   read(iunit)variable_chi_u_ps
   read(iunit)regcoeff_chi_u_ps

   read(iunit)variable_chi_u_rh
   read(iunit)regcoeff_chi_u_rh

   read(iunit)variable_t_u_rh
   read(iunit)regcoeff_t_u_rh

   read(iunit)variable_ps_u_rh
   read(iunit)regcoeff_ps_u_rh

   end if
   close(iunit)

   ! Read in the bin information:

   allocate(bin(1:ni,1:nj,1:nk))
   allocate(bin2d(1:ni,1:nj))
 
   filename = 'bin.data'
   open (iunit, file = filename, form='unformatted')

   read(iunit)bin_type
   read(iunit)lat_min, lat_max, binwidth_lat
   read(iunit)hgt_min, hgt_max, binwidth_hgt
   read(iunit)num_bins, num_bins2d
   read(iunit)bin(1:ni,1:nj,1:nk)
   read(iunit)bin2d(1:ni,1:nj)
   close(iunit)
   call da_free_unit(iunit)

   ! Write out the dimensions and bin information:

   write(outunit)ni, nj, nk

   write(outunit)bin_type
   write(outunit)lat_min, lat_max, binwidth_lat
   write(outunit)hgt_min, hgt_max, binwidth_hgt
   write(outunit)num_bins, num_bins2d
   write(outunit)bin(1:ni,1:nj,1:nk)
   write(outunit)bin2d(1:ni,1:nj)

   deallocate(bin)
   deallocate(bin2d)

   ! Write out the coefficients:

   if( cv_options == 5) then
   write(outunit)regcoeff_psi_chi
   write(outunit)regcoeff_psi_ps
   write(outunit)regcoeff_psi_t
   end if

   if( cv_options == 6) then
   write(outunit)variable_psi_chi
   write(outunit)regcoeff_psi_chi

   write(outunit)variable_psi_t
   write(outunit)regcoeff_psi_t

   write(outunit)variable_psi_ps
   write(outunit)regcoeff_psi_ps

   write(outunit)variable_psi_rh
   write(outunit)regcoeff_psi_rh

   write(outunit)variable_chi_u_t
   write(outunit)regcoeff_chi_u_t

   write(outunit)variable_chi_u_ps
   write(outunit)regcoeff_chi_u_ps

   write(outunit)variable_chi_u_rh
   write(outunit)regcoeff_chi_u_rh

   write(outunit)variable_t_u_rh
   write(outunit)regcoeff_t_u_rh

   write(outunit)variable_ps_u_rh
   write(outunit)regcoeff_ps_u_rh
   end if

   deallocate(regcoeff_psi_chi)
   deallocate(regcoeff_psi_ps)
   deallocate(regcoeff_psi_t)
   deallocate(regcoeff_psi_rh)

   deallocate( regcoeff_chi_u_ps)
   deallocate( regcoeff_chi_u_t)
   deallocate( regcoeff_chi_u_rh)

   deallocate( regcoeff_ps_u_rh)
   deallocate( regcoeff_t_u_rh)


end subroutine da_readwrite_be_stage2


subroutine da_readwrite_be_stage3(outunit, nk, variable)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer, intent(in)      :: outunit                    ! Output unit number.
   integer, intent(in)      :: nk                         ! Number of vertical levels/modes.
   character*10, intent(in) :: variable                   ! Variable name

   character(len=filename_len) :: filename                   ! Input filename.
   character*10             :: vardum                     ! Dummy variable name.

   integer                  :: num_bins2d                 ! Number of Eigenvector bins.
   integer                  :: nkdum                      ! Dummy nk variable.
   real                     :: e_vec(1:nk,1:nk)           ! Domain-averaged eigenvectors.
   real                     :: e_val(1:nk)                ! Domain-averaged eigenvalues.  
   real, allocatable        :: e_vec_loc(:,:,:)           ! Latitudinally varying eigenvectors.
   real, allocatable        :: e_val_loc(:,:)             ! Latitudinally varying eigenvalues.

   integer :: iunit
 
   call da_get_unit(iunit)
   filename = 'gen_be_stage3.'//trim(variable)//'.dat'
   open (iunit, file = filename, form='unformatted')
   read(iunit)vardum
   if (trim(vardum) /= trim(variable)) then
     call da_error("da_readwrite_be_stage3.inc",30, &
       (/"Inconsistent variable name"/))
   end if

   read(iunit)nkdum, num_bins2d
   if (nkdum /= nk) then
     call da_error("da_readwrite_be_stage3.inc",36, &
       (/"Inconsistent nk between regression coefficients and vertical modes"/))
   end if

   allocate(e_vec_loc(1:nk,1:nk,1:num_bins2d))
   allocate(e_val_loc(1:nk,1:num_bins2d))

   read(iunit)e_vec
   read(iunit)e_val
   read(iunit)e_vec_loc
   read(iunit)e_val_loc
   close(iunit)
   call da_free_unit(iunit)

   write(outunit)variable
   write(outunit)nk, num_bins2d
   write(outunit)e_vec
   write(outunit)e_val
   write(outunit)e_vec_loc
   write(outunit)e_val_loc

   deallocate(e_vec_loc)
   deallocate(e_val_loc)

end subroutine da_readwrite_be_stage3


subroutine da_readwrite_be_stage4(outunit, nk, uh_method, n_smth_sl, variable)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   real, parameter          :: spike_tolerance = 1.5      ! Threshold for detecting spikes in data. 

   integer, intent(in)      :: outunit                    ! Output unit number.
   integer, intent(in)      :: nk                         ! Number of vertical levels/modes.
   integer    , intent(in)  :: n_smth_sl                  ! Number of smoothing. 0: no smoothig
   character*5, intent(in)  :: uh_method
   character*10, intent(in) :: variable                   ! Variable name.

   character(len=filename_len) :: filename                   ! Input filename.
   character*10             :: cvar                       ! Dummy variable name.
   character*2              :: ck                         ! Loop index -> character.
   integer                  :: k                          ! Loop counter.
   integer                  :: kdum                       ! Dummy vertical index.
   integer                  :: max_wavenumber             ! Smallest scale required (ni/2 - 1).
   real                     :: ml                         ! Gradient (read from file but not used).
   real                     :: mean_scale                 ! Average scale between points.
   logical                  :: first_time                 ! True if first time through loop

   real, allocatable        :: total_power(:)             ! Total Power spectrum.
   real, allocatable        :: scale_length(:)
   real, allocatable        :: sl_smth(:)
   
   integer :: iunit

   first_time = .true.

   if (uh_method .eq. 'power') then
      do k = 1, nk
         write(ck,'(i2)')k
         if (k < 10) ck = '0'//ck(2:2)

         filename = trim(variable)//'/'//trim(variable)
         filename = trim(filename)//'.'//ck//'.spectrum'

         call da_get_unit(iunit)
         open (iunit, file = filename, form='unformatted')
         read(iunit)cvar
         if (trim(cvar) /=  trim(variable)) then
           call da_error("da_readwrite_be_stage4.inc",47, &
             (/"Variable name inconsistency"/))
         end if

         read(iunit)max_wavenumber, kdum
         if (kdum /= k) then
           call da_error("da_readwrite_be_stage4.inc",53, &
             (/"Inconsistent vertical label"/))
         end if

         if (first_time) then
            allocate(total_power(0:max_wavenumber))
         end if

         read(iunit)total_power(:)
         close(iunit)
         call da_free_unit(iunit)

         !write(outunit)variable
         !write(outunit)max_wavenumber, k
         !write(outunit) .false. ! preserve file format
         !write(outunit)total_power(:)

         first_time = .false.
      end do

      deallocate(total_power)

   else if (uh_method == 'scale   ') then
      if (first_time) then
         allocate(scale_length(1:nk))
         allocate(sl_smth(1:nk))
      end if

      call da_get_unit(iunit)
      filename = trim(variable)//'/'//'sl_print.'//trim(variable)
      open (iunit, file = filename)
      write(*,*)'after read'
      do k=1, nk
         read(iunit,'(a,2e20.8)') ck, ml, scale_length(k)
         write(iunit,'(a,2e20.8)') ck, ml, scale_length(k)
     
         ! If missing value is encountered, use the value from the last
         ! mode or level (YRG, 06/30/2005):
         !if ( k .ge. 1 ) then
           !write(*,*)'coucou gael ',k
           if (ml == 0.0 .and. scale_length(k) == 0.0) &
             scale_length(k) = scale_length(k-1)
      end do

      ! Remove spikes in lengthscales (extrapolate if spike detected):
      do k = 2, nk-1
         mean_scale = 0.5 * ( scale_length(k-1) + scale_length(k+1) )
         if ( scale_length(k) > spike_tolerance * mean_scale ) then
            scale_length(k) = mean_scale
         end if
      end do

      ! Smoothing the scale_length
      sl_smth =  scale_length
      do kdum = 1, n_smth_sl
         do k = 2, nk-1
            sl_smth(k) = scale_length(k) &
               + 0.25*(scale_length(k-1)+scale_length(k+1)-2.0*scale_length(k))
         end do
         scale_length = sl_smth 
      end do
     
      !write(outunit) variable
      !write(outunit) scale_length

      close(iunit)
      call da_free_unit(iunit)

      deallocate (scale_length)
   end if

end subroutine da_readwrite_be_stage4


subroutine da_stage0_initialize(input_file, var, dim1, dim2, dim3, ds)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

!
! netcdf version 3 fortran interface:
!

!
! external netcdf data types:
!
      integer nf_byte
      integer nf_int1
      integer nf_char
      integer nf_short
      integer nf_int2
      integer nf_int
      integer nf_float
      integer nf_real
      integer nf_double
      integer nf_ubyte
      integer nf_ushort
      integer nf_uint
      integer nf_int64
      integer nf_uint64
      integer nf_string

!
! the user doesn't use these classes of user defined type, but they
! are returned by nc_inq_user_type.
!
      integer nf_vlen
      integer nf_opaque
      integer nf_enum
      integer nf_compound

      parameter (nf_byte = 1)
      parameter (nf_int1 = nf_byte)
      parameter (nf_char = 2)
      parameter (nf_short = 3)
      parameter (nf_int2 = nf_short)
      parameter (nf_int = 4)
      parameter (nf_float = 5)
      parameter (nf_real = nf_float)
      parameter (nf_double = 6)
      parameter (nf_ubyte = 7)
      parameter (nf_ushort = 8)
      parameter (nf_uint = 9)
      parameter (nf_int64 = 10)
      parameter (nf_uint64 = 11)
      parameter (nf_string = 12)
      parameter (nf_vlen = 13)
      parameter (nf_opaque = 14)
      parameter (nf_enum = 15)
      parameter (nf_compound = 16)

!
! default fill values:
!
      integer           nf_fill_byte
      integer           nf_fill_int1
      integer           nf_fill_char
      integer           nf_fill_short
      integer           nf_fill_int2
      integer           nf_fill_int
      real              nf_fill_float
      real              nf_fill_real
      doubleprecision   nf_fill_double
      integer           nf_fill_ubyte
      integer           nf_fill_ushort
!      real              nf_fill_uint
!      real              nf_fill_int64
!      real              nf_fill_uint64

      parameter (nf_fill_byte = -127)
      parameter (nf_fill_int1 = nf_fill_byte)
      parameter (nf_fill_char = 0)
      parameter (nf_fill_short = -32767)
      parameter (nf_fill_int2 = nf_fill_short)
      parameter (nf_fill_int = -2147483647)
      parameter (nf_fill_float = 9.9692099683868690e+36)
      parameter (nf_fill_real = nf_fill_float)
      parameter (nf_fill_double = 9.9692099683868690e+36)
      parameter (nf_fill_ubyte = 255)
      parameter (nf_fill_ushort = 65535)

!
! mode flags for opening and creating a netcdf dataset:
!
      integer nf_nowrite
      integer nf_write
      integer nf_clobber
      integer nf_noclobber
      integer nf_fill
      integer nf_nofill
      integer nf_lock
      integer nf_share
      integer nf_64bit_offset
      integer nf_sizehint_default
      integer nf_align_chunk
      integer nf_format_classic
      integer nf_format_64bit
      integer nf_format_netcdf4
      integer nf_format_netcdf4_classic

      parameter (nf_nowrite = 0)
      parameter (nf_write = 1)
      parameter (nf_clobber = 0)
      parameter (nf_noclobber = 4)
      parameter (nf_fill = 0)
      parameter (nf_nofill = 256)
      parameter (nf_lock = 1024)
      parameter (nf_share = 2048)
      parameter (nf_64bit_offset = 512)
      parameter (nf_sizehint_default = 0)
      parameter (nf_align_chunk = -1)
      parameter (nf_format_classic = 1)
      parameter (nf_format_64bit = 2)
      parameter (nf_format_netcdf4 = 3)
      parameter (nf_format_netcdf4_classic = 4)

!
! size argument for defining an unlimited dimension:
!
      integer nf_unlimited
      parameter (nf_unlimited = 0)

!
! global attribute id:
!
      integer nf_global
      parameter (nf_global = 0)

!
! implementation limits:
!
      integer nf_max_dims
      integer nf_max_attrs
      integer nf_max_vars
      integer nf_max_name
      integer nf_max_var_dims

      parameter (nf_max_dims = 1024)
      parameter (nf_max_attrs = 8192)
      parameter (nf_max_vars = 8192)
      parameter (nf_max_name = 256)
      parameter (nf_max_var_dims = nf_max_dims)

!
! error codes:
!
      integer nf_noerr
      integer nf_ebadid
      integer nf_eexist
      integer nf_einval
      integer nf_eperm
      integer nf_enotindefine
      integer nf_eindefine
      integer nf_einvalcoords
      integer nf_emaxdims
      integer nf_enameinuse
      integer nf_enotatt
      integer nf_emaxatts
      integer nf_ebadtype
      integer nf_ebaddim
      integer nf_eunlimpos
      integer nf_emaxvars
      integer nf_enotvar
      integer nf_eglobal
      integer nf_enotnc
      integer nf_ests
      integer nf_emaxname
      integer nf_eunlimit
      integer nf_enorecvars
      integer nf_echar
      integer nf_eedge
      integer nf_estride
      integer nf_ebadname
      integer nf_erange
      integer nf_enomem
      integer nf_evarsize
      integer nf_edimsize
      integer nf_etrunc

      parameter (nf_noerr = 0)
      parameter (nf_ebadid = -33)
      parameter (nf_eexist = -35)
      parameter (nf_einval = -36)
      parameter (nf_eperm = -37)
      parameter (nf_enotindefine = -38)
      parameter (nf_eindefine = -39)
      parameter (nf_einvalcoords = -40)
      parameter (nf_emaxdims = -41)
      parameter (nf_enameinuse = -42)
      parameter (nf_enotatt = -43)
      parameter (nf_emaxatts = -44)
      parameter (nf_ebadtype = -45)
      parameter (nf_ebaddim = -46)
      parameter (nf_eunlimpos = -47)
      parameter (nf_emaxvars = -48)
      parameter (nf_enotvar = -49)
      parameter (nf_eglobal = -50)
      parameter (nf_enotnc = -51)
      parameter (nf_ests = -52)
      parameter (nf_emaxname = -53)
      parameter (nf_eunlimit = -54)
      parameter (nf_enorecvars = -55)
      parameter (nf_echar = -56)
      parameter (nf_eedge = -57)
      parameter (nf_estride = -58)
      parameter (nf_ebadname = -59)
      parameter (nf_erange = -60)
      parameter (nf_enomem = -61)
      parameter (nf_evarsize = -62)
      parameter (nf_edimsize = -63)
      parameter (nf_etrunc = -64)
!
! error handling modes:
!
      integer  nf_fatal
      integer nf_verbose

      parameter (nf_fatal = 1)
      parameter (nf_verbose = 2)

!
! miscellaneous routines:
!
      character*80   nf_inq_libvers
      external       nf_inq_libvers

      character*80   nf_strerror
!                         (integer             ncerr)
      external       nf_strerror

      logical        nf_issyserr
!                         (integer             ncerr)
      external       nf_issyserr

!
! control routines:
!
      integer         nf_inq_base_pe
!                         (integer             ncid,
!                          integer             pe)
      external        nf_inq_base_pe

      integer         nf_set_base_pe
!                         (integer             ncid,
!                          integer             pe)
      external        nf_set_base_pe

      integer         nf_create
!                         (character*(*)       path,
!                          integer             cmode,
!                          integer             ncid)
      external        nf_create

      integer         nf__create
!                         (character*(*)       path,
!                          integer             cmode,
!                          integer             initialsz,
!                          integer             chunksizehint,
!                          integer             ncid)
      external        nf__create

      integer         nf__create_mp
!                         (character*(*)       path,
!                          integer             cmode,
!                          integer             initialsz,
!                          integer             basepe,
!                          integer             chunksizehint,
!                          integer             ncid)
      external        nf__create_mp

      integer         nf_open
!                         (character*(*)       path,
!                          integer             mode,
!                          integer             ncid)
      external        nf_open

      integer         nf__open
!                         (character*(*)       path,
!                          integer             mode,
!                          integer             chunksizehint,
!                          integer             ncid)
      external        nf__open

      integer         nf__open_mp
!                         (character*(*)       path,
!                          integer             mode,
!                          integer             basepe,
!                          integer             chunksizehint,
!                          integer             ncid)
      external        nf__open_mp

      integer         nf_set_fill
!                         (integer             ncid,
!                          integer             fillmode,
!                          integer             old_mode)
      external        nf_set_fill

      integer         nf_set_default_format
!                          (integer             format,
!                          integer             old_format)
      external        nf_set_default_format

      integer         nf_redef
!                         (integer             ncid)
      external        nf_redef

      integer         nf_enddef
!                         (integer             ncid)
      external        nf_enddef

      integer         nf__enddef
!                         (integer             ncid,
!                          integer             h_minfree,
!                          integer             v_align,
!                          integer             v_minfree,
!                          integer             r_align)
      external        nf__enddef

      integer         nf_sync
!                         (integer             ncid)
      external        nf_sync

      integer         nf_abort
!                         (integer             ncid)
      external        nf_abort

      integer         nf_close
!                         (integer             ncid)
      external        nf_close

      integer         nf_delete
!                         (character*(*)       ncid)
      external        nf_delete

!
! general inquiry routines:
!

      integer         nf_inq
!                         (integer             ncid,
!                          integer             ndims,
!                          integer             nvars,
!                          integer             ngatts,
!                          integer             unlimdimid)
      external        nf_inq

      integer         nf_inq_ndims
!                         (integer             ncid,
!                          integer             ndims)
      external        nf_inq_ndims

      integer         nf_inq_nvars
!                         (integer             ncid,
!                          integer             nvars)
      external        nf_inq_nvars

      integer         nf_inq_natts
!                         (integer             ncid,
!                          integer             ngatts)
      external        nf_inq_natts

      integer         nf_inq_unlimdim
!                         (integer             ncid,
!                          integer             unlimdimid)
      external        nf_inq_unlimdim

      integer         nf_inq_format
!                         (integer             ncid,
!                          integer             format)
      external        nf_inq_format

!
! dimension routines:
!

      integer         nf_def_dim
!                         (integer             ncid,
!                          character(*)        name,
!                          integer             len,
!                          integer             dimid)
      external        nf_def_dim

      integer         nf_inq_dimid
!                         (integer             ncid,
!                          character(*)        name,
!                          integer             dimid)
      external        nf_inq_dimid

      integer         nf_inq_dim
!                         (integer             ncid,
!                          integer             dimid,
!                          character(*)        name,
!                          integer             len)
      external        nf_inq_dim

      integer         nf_inq_dimname
!                         (integer             ncid,
!                          integer             dimid,
!                          character(*)        name)
      external        nf_inq_dimname

      integer         nf_inq_dimlen
!                         (integer             ncid,
!                          integer             dimid,
!                          integer             len)
      external        nf_inq_dimlen

      integer         nf_rename_dim
!                         (integer             ncid,
!                          integer             dimid,
!                          character(*)        name)
      external        nf_rename_dim

!
! general attribute routines:
!

      integer         nf_inq_att
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          integer             xtype,
!                          integer             len)
      external        nf_inq_att

      integer         nf_inq_attid
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          integer             attnum)
      external        nf_inq_attid

      integer         nf_inq_atttype
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          integer             xtype)
      external        nf_inq_atttype

      integer         nf_inq_attlen
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          integer             len)
      external        nf_inq_attlen

      integer         nf_inq_attname
!                         (integer             ncid,
!                          integer             varid,
!                          integer             attnum,
!                          character(*)        name)
      external        nf_inq_attname

      integer         nf_copy_att
!                         (integer             ncid_in,
!                          integer             varid_in,
!                          character(*)        name,
!                          integer             ncid_out,
!                          integer             varid_out)
      external        nf_copy_att

      integer         nf_rename_att
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        curname,
!                          character(*)        newname)
      external        nf_rename_att

      integer         nf_del_att
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name)
      external        nf_del_att

!
! attribute put/get routines:
!

      integer         nf_put_att_text
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          integer             len,
!                          character(*)        text)
      external        nf_put_att_text

      integer         nf_get_att_text
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          character(*)        text)
      external        nf_get_att_text

      integer         nf_put_att_int1
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          integer             xtype,
!                          integer             len,
!                          nf_int1_t           i1vals(1))
      external        nf_put_att_int1

      integer         nf_get_att_int1
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          nf_int1_t           i1vals(1))
      external        nf_get_att_int1

      integer         nf_put_att_int2
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          integer             xtype,
!                          integer             len,
!                          nf_int2_t           i2vals(1))
      external        nf_put_att_int2

      integer         nf_get_att_int2
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          nf_int2_t           i2vals(1))
      external        nf_get_att_int2

      integer         nf_put_att_int
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          integer             xtype,
!                          integer             len,
!                          integer             ivals(1))
      external        nf_put_att_int

      integer         nf_get_att_int
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          integer             ivals(1))
      external        nf_get_att_int

      integer         nf_put_att_real
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          integer             xtype,
!                          integer             len,
!                          real                rvals(1))
      external        nf_put_att_real

      integer         nf_get_att_real
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          real                rvals(1))
      external        nf_get_att_real

      integer         nf_put_att_double
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          integer             xtype,
!                          integer             len,
!                          double              dvals(1))
      external        nf_put_att_double

      integer         nf_get_att_double
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          double              dvals(1))
      external        nf_get_att_double

!
! general variable routines:
!

      integer         nf_def_var
!                         (integer             ncid,
!                          character(*)        name,
!                          integer             datatype,
!                          integer             ndims,
!                          integer             dimids(1),
!                          integer             varid)
      external        nf_def_var

      integer         nf_inq_var
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          integer             datatype,
!                          integer             ndims,
!                          integer             dimids(1),
!                          integer             natts)
      external        nf_inq_var

      integer         nf_inq_varid
!                         (integer             ncid,
!                          character(*)        name,
!                          integer             varid)
      external        nf_inq_varid

      integer         nf_inq_varname
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name)
      external        nf_inq_varname

      integer         nf_inq_vartype
!                         (integer             ncid,
!                          integer             varid,
!                          integer             xtype)
      external        nf_inq_vartype

      integer         nf_inq_varndims
!                         (integer             ncid,
!                          integer             varid,
!                          integer             ndims)
      external        nf_inq_varndims

      integer         nf_inq_vardimid
!                         (integer             ncid,
!                          integer             varid,
!                          integer             dimids(1))
      external        nf_inq_vardimid

      integer         nf_inq_varnatts
!                         (integer             ncid,
!                          integer             varid,
!                          integer             natts)
      external        nf_inq_varnatts

      integer         nf_rename_var
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name)
      external        nf_rename_var

      integer         nf_copy_var
!                         (integer             ncid_in,
!                          integer             varid,
!                          integer             ncid_out)
      external        nf_copy_var

!
! entire variable put/get routines:
!

      integer         nf_put_var_text
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        text)
      external        nf_put_var_text

      integer         nf_get_var_text
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        text)
      external        nf_get_var_text

      integer         nf_put_var_int1
!                         (integer             ncid,
!                          integer             varid,
!                          nf_int1_t           i1vals(1))
      external        nf_put_var_int1

      integer         nf_get_var_int1
!                         (integer             ncid,
!                          integer             varid,
!                          nf_int1_t           i1vals(1))
      external        nf_get_var_int1

      integer         nf_put_var_int2
!                         (integer             ncid,
!                          integer             varid,
!                          nf_int2_t           i2vals(1))
      external        nf_put_var_int2

      integer         nf_get_var_int2
!                         (integer             ncid,
!                          integer             varid,
!                          nf_int2_t           i2vals(1))
      external        nf_get_var_int2

      integer         nf_put_var_int
!                         (integer             ncid,
!                          integer             varid,
!                          integer             ivals(1))
      external        nf_put_var_int

      integer         nf_get_var_int
!                         (integer             ncid,
!                          integer             varid,
!                          integer             ivals(1))
      external        nf_get_var_int

      integer         nf_put_var_real
!                         (integer             ncid,
!                          integer             varid,
!                          real                rvals(1))
      external        nf_put_var_real

      integer         nf_get_var_real
!                         (integer             ncid,
!                          integer             varid,
!                          real                rvals(1))
      external        nf_get_var_real

      integer         nf_put_var_double
!                         (integer             ncid,
!                          integer             varid,
!                          doubleprecision     dvals(1))
      external        nf_put_var_double

      integer         nf_get_var_double
!                         (integer             ncid,
!                          integer             varid,
!                          doubleprecision     dvals(1))
      external        nf_get_var_double

!
! single variable put/get routines:
!

      integer         nf_put_var1_text
!                         (integer             ncid,
!                          integer             varid,
!                          integer             index(1),
!                          character*1         text)
      external        nf_put_var1_text

      integer         nf_get_var1_text
!                         (integer             ncid,
!                          integer             varid,
!                          integer             index(1),
!                          character*1         text)
      external        nf_get_var1_text

      integer         nf_put_var1_int1
!                         (integer             ncid,
!                          integer             varid,
!                          integer             index(1),
!                          nf_int1_t           i1val)
      external        nf_put_var1_int1

      integer         nf_get_var1_int1
!                         (integer             ncid,
!                          integer             varid,
!                          integer             index(1),
!                          nf_int1_t           i1val)
      external        nf_get_var1_int1

      integer         nf_put_var1_int2
!                         (integer             ncid,
!                          integer             varid,
!                          integer             index(1),
!                          nf_int2_t           i2val)
      external        nf_put_var1_int2

      integer         nf_get_var1_int2
!                         (integer             ncid,
!                          integer             varid,
!                          integer             index(1),
!                          nf_int2_t           i2val)
      external        nf_get_var1_int2

      integer         nf_put_var1_int
!                         (integer             ncid,
!                          integer             varid,
!                          integer             index(1),
!                          integer             ival)
      external        nf_put_var1_int

      integer         nf_get_var1_int
!                         (integer             ncid,
!                          integer             varid,
!                          integer             index(1),
!                          integer             ival)
      external        nf_get_var1_int

      integer         nf_put_var1_real
!                         (integer             ncid,
!                          integer             varid,
!                          integer             index(1),
!                          real                rval)
      external        nf_put_var1_real

      integer         nf_get_var1_real
!                         (integer             ncid,
!                          integer             varid,
!                          integer             index(1),
!                          real                rval)
      external        nf_get_var1_real

      integer         nf_put_var1_double
!                         (integer             ncid,
!                          integer             varid,
!                          integer             index(1),
!                          doubleprecision     dval)
      external        nf_put_var1_double

      integer         nf_get_var1_double
!                         (integer             ncid,
!                          integer             varid,
!                          integer             index(1),
!                          doubleprecision     dval)
      external        nf_get_var1_double

!
! variable array put/get routines:
!

      integer         nf_put_vara_text
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          character(*)        text)
      external        nf_put_vara_text

      integer         nf_get_vara_text
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          character(*)        text)
      external        nf_get_vara_text

      integer         nf_put_vara_int1
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          nf_int1_t           i1vals(1))
      external        nf_put_vara_int1

      integer         nf_get_vara_int1
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          nf_int1_t           i1vals(1))
      external        nf_get_vara_int1

      integer         nf_put_vara_int2
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          nf_int2_t           i2vals(1))
      external        nf_put_vara_int2

      integer         nf_get_vara_int2
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          nf_int2_t           i2vals(1))
      external        nf_get_vara_int2

      integer         nf_put_vara_int
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             ivals(1))
      external        nf_put_vara_int

      integer         nf_get_vara_int
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             ivals(1))
      external        nf_get_vara_int

      integer         nf_put_vara_real
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          real                rvals(1))
      external        nf_put_vara_real

      integer         nf_get_vara_real
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          real                rvals(1))
      external        nf_get_vara_real

      integer         nf_put_vara_double
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          doubleprecision     dvals(1))
      external        nf_put_vara_double

      integer         nf_get_vara_double
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          doubleprecision     dvals(1))
      external        nf_get_vara_double

!
! strided variable put/get routines:
!

      integer         nf_put_vars_text
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          character(*)        text)
      external        nf_put_vars_text

      integer         nf_get_vars_text
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          character(*)        text)
      external        nf_get_vars_text

      integer         nf_put_vars_int1
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          nf_int1_t           i1vals(1))
      external        nf_put_vars_int1

      integer         nf_get_vars_int1
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          nf_int1_t           i1vals(1))
      external        nf_get_vars_int1

      integer         nf_put_vars_int2
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          nf_int2_t           i2vals(1))
      external        nf_put_vars_int2

      integer         nf_get_vars_int2
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          nf_int2_t           i2vals(1))
      external        nf_get_vars_int2

      integer         nf_put_vars_int
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          integer             ivals(1))
      external        nf_put_vars_int

      integer         nf_get_vars_int
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          integer             ivals(1))
      external        nf_get_vars_int

      integer         nf_put_vars_real
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          real                rvals(1))
      external        nf_put_vars_real

      integer         nf_get_vars_real
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          real                rvals(1))
      external        nf_get_vars_real

      integer         nf_put_vars_double
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          doubleprecision     dvals(1))
      external        nf_put_vars_double

      integer         nf_get_vars_double
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          doubleprecision     dvals(1))
      external        nf_get_vars_double

!
! mapped variable put/get routines:
!

      integer         nf_put_varm_text
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          integer             imap(1),
!                          character(*)        text)
      external        nf_put_varm_text

      integer         nf_get_varm_text
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          integer             imap(1),
!                          character(*)        text)
      external        nf_get_varm_text

      integer         nf_put_varm_int1
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          integer             imap(1),
!                          nf_int1_t           i1vals(1))
      external        nf_put_varm_int1

      integer         nf_get_varm_int1
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          integer             imap(1),
!                          nf_int1_t           i1vals(1))
      external        nf_get_varm_int1

      integer         nf_put_varm_int2
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          integer             imap(1),
!                          nf_int2_t           i2vals(1))
      external        nf_put_varm_int2

      integer         nf_get_varm_int2
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          integer             imap(1),
!                          nf_int2_t           i2vals(1))
      external        nf_get_varm_int2

      integer         nf_put_varm_int
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          integer             imap(1),
!                          integer             ivals(1))
      external        nf_put_varm_int

      integer         nf_get_varm_int
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          integer             imap(1),
!                          integer             ivals(1))
      external        nf_get_varm_int

      integer         nf_put_varm_real
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          integer             imap(1),
!                          real                rvals(1))
      external        nf_put_varm_real

      integer         nf_get_varm_real
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          integer             imap(1),
!                          real                rvals(1))
      external        nf_get_varm_real

      integer         nf_put_varm_double
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          integer             imap(1),
!                          doubleprecision     dvals(1))
      external        nf_put_varm_double

      integer         nf_get_varm_double
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          integer             imap(1),
!                          doubleprecision     dvals(1))
      external        nf_get_varm_double

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
! begin netcdf 2.4 backward compatibility:
!

!      
! functions in the fortran interface
!
      integer nccre
      integer ncopn
      integer ncddef
      integer ncdid
      integer ncvdef
      integer ncvid
      integer nctlen
      integer ncsfil

      external nccre
      external ncopn
      external ncddef
      external ncdid
      external ncvdef
      external ncvid
      external nctlen
      external ncsfil


      integer ncrdwr
      integer nccreat
      integer ncexcl
      integer ncindef
      integer ncnsync
      integer nchsync
      integer ncndirty
      integer nchdirty
      integer nclink
      integer ncnowrit
      integer ncwrite
      integer ncclob
      integer ncnoclob
      integer ncglobal
      integer ncfill
      integer ncnofill
      integer maxncop
      integer maxncdim
      integer maxncatt
      integer maxncvar
      integer maxncnam
      integer maxvdims
      integer ncnoerr
      integer ncebadid
      integer ncenfile
      integer nceexist
      integer nceinval
      integer nceperm
      integer ncenotin
      integer nceindef
      integer ncecoord
      integer ncemaxds
      integer ncename
      integer ncenoatt
      integer ncemaxat
      integer ncebadty
      integer ncebadd
      integer ncests
      integer nceunlim
      integer ncemaxvs
      integer ncenotvr
      integer nceglob
      integer ncenotnc
      integer ncfoobar
      integer ncsyserr
      integer ncfatal
      integer ncverbos
      integer ncentool


!
! netcdf data types:
!
      integer ncbyte
      integer ncchar
      integer ncshort
      integer nclong
      integer ncfloat
      integer ncdouble

      parameter(ncbyte = 1)
      parameter(ncchar = 2)
      parameter(ncshort = 3)
      parameter(nclong = 4)
      parameter(ncfloat = 5)
      parameter(ncdouble = 6)

!     
!     masks for the struct nc flag field; passed in as 'mode' arg to
!     nccreate and ncopen.
!     

!     read/write, 0 => readonly 
      parameter(ncrdwr = 1)
!     in create phase, cleared by ncendef 
      parameter(nccreat = 2)
!     on create destroy existing file 
      parameter(ncexcl = 4)
!     in define mode, cleared by ncendef 
      parameter(ncindef = 8)
!     synchronise numrecs on change (x'10')
      parameter(ncnsync = 16)
!     synchronise whole header on change (x'20')
      parameter(nchsync = 32)
!     numrecs has changed (x'40')
      parameter(ncndirty = 64)  
!     header info has changed (x'80')
      parameter(nchdirty = 128)
!     prefill vars on endef and increase of record, the default behavior
      parameter(ncfill = 0)
!     do not fill vars on endef and increase of record (x'100')
      parameter(ncnofill = 256)
!     isa link (x'8000')
      parameter(nclink = 32768)

!     
!     'mode' arguments for nccreate and ncopen
!     
      parameter(ncnowrit = 0)
      parameter(ncwrite = ncrdwr)
      parameter(ncclob = nf_clobber)
      parameter(ncnoclob = nf_noclobber)

!     
!     'size' argument to ncdimdef for an unlimited dimension
!     
      integer ncunlim
      parameter(ncunlim = 0)

!     
!     attribute id to put/get a global attribute
!     
      parameter(ncglobal  = 0)

!     
!     advisory maximums:
!     
      parameter(maxncop = 64)
      parameter(maxncdim = 1024)
      parameter(maxncatt = 8192)
      parameter(maxncvar = 8192)
!     not enforced 
      parameter(maxncnam = 256)
      parameter(maxvdims = maxncdim)

!     
!     global netcdf error status variable
!     initialized in error.c
!     

!     no error 
      parameter(ncnoerr = nf_noerr)
!     not a netcdf id 
      parameter(ncebadid = nf_ebadid)
!     too many netcdfs open 
      parameter(ncenfile = -31)   ! nc_syserr
!     netcdf file exists && ncnoclob
      parameter(nceexist = nf_eexist)
!     invalid argument 
      parameter(nceinval = nf_einval)
!     write to read only 
      parameter(nceperm = nf_eperm)
!     operation not allowed in data mode 
      parameter(ncenotin = nf_enotindefine )   
!     operation not allowed in define mode 
      parameter(nceindef = nf_eindefine)   
!     coordinates out of domain 
      parameter(ncecoord = nf_einvalcoords)
!     maxncdims exceeded 
      parameter(ncemaxds = nf_emaxdims)
!     string match to name in use 
      parameter(ncename = nf_enameinuse)   
!     attribute not found 
      parameter(ncenoatt = nf_enotatt)
!     maxncattrs exceeded 
      parameter(ncemaxat = nf_emaxatts)
!     not a netcdf data type 
      parameter(ncebadty = nf_ebadtype)
!     invalid dimension id 
      parameter(ncebadd = nf_ebaddim)
!     ncunlimited in the wrong index 
      parameter(nceunlim = nf_eunlimpos)
!     maxncvars exceeded 
      parameter(ncemaxvs = nf_emaxvars)
!     variable not found 
      parameter(ncenotvr = nf_enotvar)
!     action prohibited on ncglobal varid 
      parameter(nceglob = nf_eglobal)
!     not a netcdf file 
      parameter(ncenotnc = nf_enotnc)
      parameter(ncests = nf_ests)
      parameter (ncentool = nf_emaxname) 
      parameter(ncfoobar = 32)
      parameter(ncsyserr = -31)

!     
!     global options variable. used to determine behavior of error handler.
!     initialized in lerror.c
!     
      parameter(ncfatal = 1)
      parameter(ncverbos = 2)

!
!     default fill values.  these must be the same as in the c interface.
!
      integer filbyte
      integer filchar
      integer filshort
      integer fillong
      real filfloat
      doubleprecision fildoub

      parameter (filbyte = -127)
      parameter (filchar = 0)
      parameter (filshort = -32767)
      parameter (fillong = -2147483647)
      parameter (filfloat = 9.9692099683868690e+36)
      parameter (fildoub = 9.9692099683868690e+36)

   character (len=200), intent(in):: input_file       ! 1 file name.
   character (len=12), intent(in) :: var              ! Variable to search for.
   integer, intent(out)           :: dim1             ! Dimensions of field. 
   integer, intent(out)           :: dim2             ! Dimensions of field. 
   integer, intent(out)           :: dim3             ! Dimensions of field. 
   real, intent(out)              :: ds               ! Grid resolution.

   character (len=80)             :: att_name         ! Attribute name.
   integer                        :: i                ! Loop counters.
   integer                        :: attlen           ! Length of attribute.
   integer                        :: cdfid            ! 1 file id.
   integer                        :: rcode            ! Return code (0=ok).
   integer                        :: length           ! Length of filename.
   integer                        :: id_var           ! 1 variable ID. 
   integer                        :: ivtype           ! 4=integer, 5=real, 6=d.p.
   integer                        :: ndims            !
   integer                        :: natts            ! Number of attributes.

   integer                        :: dimids(10)       !
   integer                        :: dims(1:4)        ! Dimensions of field. 
   real (kind=4), allocatable     :: value_real(:)    ! real attribute value. 

   ! Check file exists:
   length = len_trim(input_file)
   rcode = nf_open(input_file(1:length), NF_NOwrite, cdfid)
   if (rcode /= 0) then
      write(unit=message(1),fmt='(A,A)') &
         ' Error opening netcdf file ', input_file(1:length)
      call da_error("da_stage0_initialize.inc",41,message(1:1))
   end if

   ! Check variable is in file:
   rcode = nf_inq_varid (cdfid, var, id_var)
   if (rcode /= 0) then 
      write(unit=message(1),fmt='(A,A)') &
         var, ' variable is not in input file'
      call da_error("da_stage0_initialize.inc",49,message(1:1))
   end if

   ! Get metadata (time, dimensions, global attributes):
   rcode = nf_inq_var(cdfid, id_var, var, ivtype, ndims, dimids, natts)
   do i = 1, ndims
     rcode = nf_inq_dimlen(cdfid, dimids(i), dims(i))
   end do
   dim1 = dims(1)
   dim2 = dims(2)
   dim3 = dims(3)

   ! Get resolution:
   att_name = 'DX'  ! Assume dx= dy.
   rcode = nf_inq_att(cdfid, nf_global, att_name, ivtype, attlen)
   allocate(value_real(1:attlen))
   rcode = NF_GET_ATT_real(cdfid, nf_global, att_name, value_real)
   ds = value_real(1)
   deallocate(value_real)

   rcode = nf_close(cdfid)

end subroutine da_stage0_initialize



   
subroutine da_transform_vptovv (evec, eval, vertical_wgt, vp, vv, mz, &
   kds,kde, ims,ime, jms,jme, kms,kme, its,ite, jts,jte, kts,kte)

   !---------------------------------------------------------------------------
   ! Purpose: Transform from fields on vertical levels to fields on vertical 
   ! EOFS.
   !
   ! Method: Perform vv(i,j,m) = L^{-1/2} E^{T} vp(i,j,k) transform.
   !---------------------------------------------------------------------------

   implicit none

   integer, intent(in)    :: mz                         ! # vertical modes.
   integer, intent(in)    :: kds,kde  ! domain dims.
   integer, intent(in)    :: ims,ime, jms,jme, kms,kme  ! memory dims.
   integer, intent(in)    :: its,ite, jts,jte, kts,kte  ! tile   dims
   real*8,  intent(in)    :: evec(jts:jte,kds:kde,1:mz) ! Eigenvectors.
   real*8,  intent(in)    :: eval(jts:jte,1:mz)         ! Eigenvalues.
   real(kind=8),    intent(in)    :: vertical_wgt(ims:ime,jms:jme,kms:kme) ! Weighting.
   real(kind=8),    intent(inout) :: vp(ims:ime,jms:jme,kms:kme)! CV in level space.
   real(kind=8),    intent(out)   :: vv(ims:ime,jms:jme,1:mz)   ! CV in EOF space.
   
   integer :: i, j, m                    ! Loop counters.
   real    :: ETVp                       ! E(k,m)^{T}*vp(i,j,k)
   
   !-------------------------------------------------------------------
   ! [1.0] Apply inner-product weighting if vertical_ip /= vertical_ip_0
   !------------------------------------------------------------------- 

   if (vertical_ip /= vertical_ip_0) then
      vp(its:ite,jts:jte,kts:kte) = vp(its:ite,jts:jte,kts:kte) * &
                                    vertical_wgt(its:ite,jts:jte,kts:kte)
   end if

   !-------------------------------------------------------------------
   ! [2.0] Perform vv(i,j,m) = L^{-1/2} E^T vp(i,j,k) transform:
   !-------------------------------------------------------------------

   do m = 1, mz
      do j = jts, jte
         do i = its, ite
            ETVp = sum(evec(j,kts:kte,m) * vp(i,j,kts:kte))
            vv(i,j,m) = ETVp / eval(j,m)
         end do
      end do
   end do

end subroutine da_transform_vptovv


subroutine da_eof_decomposition (kz, bx, e, l)
   
   !---------------------------------------------------------------------------
   ! Purpose: Compute eigenvectors E and eigenvalues L of vertical covariance 
   !          matrix
   !          B_{x} defined by equation:  E^{T} B_{x} E = L, given input kz x kz 
   !          BE field.
   !---------------------------------------------------------------------------
   
   implicit none

   integer, intent(in)  :: kz               ! Dimension of error matrix. 
   real,    intent(in)  :: bx(1:kz,1:kz)    ! Vert. background error.
   real*8,  intent(out) :: e(1:kz,1:kz)     ! Eigenvectors of Bx.
   real*8,  intent(out) :: l(1:kz)          ! Eigenvalues of Bx.

   integer :: work             ! Size of work array.
   integer :: m                ! Loop counters
   integer :: info             ! Info code.

   real*8  :: work_array(1:3*kz-1)
   real*8  :: ecopy(1:kz,1:kz)
   real*8  :: lcopy(1:kz)   

   !-------------------------------------------------------------------------
   ! [5.0]: Perform global eigenvalue decomposition using LAPACK software:
   !-------------------------------------------------------------------------
   
   work = 3 * kz - 1   
   ecopy(1:kz,1:kz) = bx(1:kz,1:kz)
   lcopy(1:kz) = 0.0

   call dsyev( 'V', 'U', kz, ecopy, kz, lcopy, work_array, work, info )
   
   if ( info /= 0 ) then
      write(unit=message(1),fmt='(A,I4)') &
         "Error in decomposition, info = ", info
      call da_error("da_eof_decomposition.inc",40,message(1:1))
   end if
   
   ! Swap order of eigenvalues, vectors so 1st is one with most variance:
   
   do m = 1, kz
      l(m) = lcopy(kz+1-m)
      e(1:kz,m) = ecopy(1:kz,kz+1-m)
   end do  

end subroutine da_eof_decomposition


subroutine da_eof_decomposition_test (kz, bx, e, l)
   
   !------------------------------------------------------------------------------
   ! Purpose: 
   ! [1] Print eigenvalues:
   ! [2] Test orthogonality of eigenvectors - sum_k (e_m(k) e_n(k)) = delta_mn:
   ! [3] Test eigenvectors completeness - sum_m (e_m(k1) e_m(k2)) = delta_k1k2:
   ! [4] Check B correctness: B = E*L*E^T
   !------------------------------------------------------------------------------
   
   implicit none

   integer, intent(in) :: kz               ! Dimension of BE matrix   
   real,    intent(in) :: bx(1:kz,1:kz)    ! Global vert. background error.
   real*8,  intent(in) :: e(1:kz,1:kz)     ! Eigenvectors of Bx.
   real*8,  intent(in) :: l(1:kz)          ! Eigenvalues of Bx.
   
   integer                  :: k, k1, k2, m     ! Loop counters
   real                     :: tot_variance     ! Total variance.
   real                     :: cum_variance     ! Cumulative variance.
   real                     :: max_off_diag     ! Maximum off-diagonal.

   real                     :: work(1:kz,1:kz)  ! 2D Work matrix.
   real                     :: bc(1:kz,1:kz)    ! 2D Work matrix.
   logical                  :: array_mask(1:kz) ! Array mask for MAXVAL.

   !------------------------------------------------------------------------- 
   ! [1] Print eigenvalues:
   !-------------------------------------------------------------------------

   tot_variance = sum(l(1:kz))
   cum_variance = 0.0
   
   write(unit=stdout,fmt='(A)')'  Mode    Eigenvalue     Cumulative Variance      e(k,k)'

   do k = 1, kz
      cum_variance = cum_variance + l(k)
      write(unit=stdout,fmt='(I4,4x,e12.4,10x,f8.4,4x,e12.4)') &
            k, l(k), cum_variance / tot_variance, e(k,k)
   end do

   write(unit=stdout,fmt=*)
   
   call da_array_print( 1, e, 'Global Eigenvectors' )

   !-------------------------------------------------------------------------
   ! [2] Test orthogonality of eigenvectors - sum_k (e_m(k) e_n(k)) = delta_mn:
   !-------------------------------------------------------------------------
   
   write(unit=stdout,fmt='(A)')' Eigenvector orthogonality check:'
   write(unit=stdout,fmt='(A)')' Mode     Diagonal         Maximum off-diagonal'

   do k1 = 1, kz
      do k2 = 1, kz
         work(k1,k2) = sum(e(1:kz,k1) * e(1:kz,k2))
      end do
   
      array_mask(1:kz) =.true.
      array_mask(k1) = .false.
      max_off_diag = maxval(abs(work(k1,:)),mask=array_mask(:))
      write(unit=stdout,fmt='(I4,4x,1pe12.4,10x,1pe12.4)')k1, work(k1,k1), max_off_diag
   end do
   write(unit=stdout,fmt=*)

   !-------------------------------------------------------------------------   
   ! [3] Test eigenvectors completeness - sum_m (e_m(k1) e_m(k2)) = delta_k1k2:
   !-------------------------------------------------------------------------   
   
   write(unit=stdout,fmt='(A)')' Eigenvector completeness check:'
   write(unit=stdout,fmt='(A)')' Level    Diagonal         Maximum off-diagonal'

   do k1 = 1, kz
      do k2 = 1, kz
         work(k1,k2) = sum(e(k1,1:kz) * e(k2,1:kz))
      end do
   
      array_mask(1:kz) =.true.
      array_mask(k1) = .false.
      max_off_diag = maxval(abs(work(k1,:)),mask=array_mask(:))
      write(unit=stdout,fmt='(I4,4x,1pe12.4,10x,1pe12.4)')k1, work(k1,k1), max_off_diag
   end do
   write(unit=stdout,fmt=*)

   !-------------------------------------------------------------------------
   ! [4]  check B correctness: B = E*L*E^T
   !-------------------------------------------------------------------------

   write(unit=stdout,fmt='(a/a)') &
        'real and Calculated B (diagonal)', &
        'lvl                 real-B                    Calculated-B'

   do k=1,kz
      do m=1,kz
         work(k,m)=l(k)*e(m,k)
         bc(k,m)=0.0
      end do
   end do
   
   do k1=1,kz
      do k2=1,kz
         do m=1,kz
            bc(k1,k2)=bc(k1,k2)+e(k1,m)*work(m,k2)
         end do
      end do

      write(unit=stdout,fmt='(I5,2F20.5)') k1, bx(k1,k1), bc(k1,k1)
   end do

   do k2=1,kz
      write(unit=stdout, fmt='(a,i4/a)') &
           'real and Calculated B (off diagonal):', k2, &
           'lvl                 real-B                    Calculated-B'

      do k1=1,kz
        write(unit=stdout,fmt='(I5,2F20.5)') k1, bx(k1,k2), bc(k1,k2)
      end do
   end do

end subroutine da_eof_decomposition_test

! -- gen_be I/O additionnal routines -- //

  !------------------------------------------------------------------------------------
  ! Read a 3D field
  !------------------------------------------------------------------------------------
  subroutine read_3d_field(variable, date, ce, iunit, ni, nj, nk, field_3d)

    character*10, intent(in)    :: variable         ! Variable name
    character*10, intent(in)    :: date             ! Current date (ccyymmddhh).
    character*3 , intent(in)    :: ce               ! Member index -> character.
    integer     , intent(in)    :: iunit            ! Unit where fiels is read
    integer     , intent(inout) :: ni, nj, nk       ! Grid dimensions.
    real        , intent(inout) :: field_3d(:,:,:)  ! Field 3D

    character(len=filename_len)    :: filename      ! Input filename.

    filename = trim(variable)//'/'//date(1:10)
    filename = trim(filename)//'.'//trim(variable)//'.e'//ce
    open (iunit, file = filename, form='unformatted')
    read(iunit)ni, nj, nk
    read(iunit)field_3d
    close(iunit)

  end subroutine read_3d_field
  !------------------------------------------------------------------------------------
  ! Read a 2D field
  !------------------------------------------------------------------------------------
  subroutine read_2d_field(variable, date, ce, iunit, ni, nj, nkdum, field_2d)

    character*10, intent(in)    :: variable         ! Variable name
    character*10, intent(in)    :: date             ! Current date (ccyymmddhh).
    character*3 , intent(in)    :: ce               ! Member index -> character.
    integer     , intent(in)    :: iunit            ! Unit where fiels is read
    integer     , intent(inout) :: ni, nj, nkdum    ! Grid dimensions.
    real        , intent(inout) :: field_2d(:,:)    ! Field 2D

    character(len=filename_len)    :: filename      ! Input filename.

    filename = trim(variable)//'/'//date(1:10)
    filename = trim(filename)//'.'//trim(variable)//'.e'//ce//'.01'
    open (iunit, file = filename, form='unformatted')
    read(iunit)ni, nj, nkdum
    read(iunit)field_2d
    close(iunit)

  end subroutine read_2d_field

  !------------------------------------------------------------------------------------
  ! Write a 3D field
  !------------------------------------------------------------------------------------
  subroutine write_3d_field(variable, date, ce, ounit, ni, nj, nk, field_3d)

    character*10, intent(in)    :: variable         ! Variable name
    character*10, intent(in)    :: date             ! Current date (ccyymmddhh).
    character*3 , intent(in)    :: ce               ! Member index -> character.
    integer     , intent(in)    :: ounit            ! Unit where fiels is read
    integer     , intent(in)    :: ni, nj, nk       ! Grid dimensions.
    real        , intent(in)    :: field_3d(:,:,:)  ! Field 3D

    character(len=filename_len)    :: filename      ! Input filename.

    filename = trim(variable)//'/'//date(1:10)
    filename = trim(filename)//'.'//trim(variable)//'.e'//ce
    open (ounit, file = filename, form='unformatted')
    write(ounit)ni, nj, nk
    write(ounit)field_3d
    close(ounit)

  end subroutine write_3d_field
  !------------------------------------------------------------------------------------
  ! Write a 2D field
  !------------------------------------------------------------------------------------
  subroutine write_2d_field(variable, date, ce, ounit, ni, nj, nkdum, field_2d)

    character*10, intent(in)    :: variable         ! Variable name
    character*10, intent(in)    :: date             ! Current date (ccyymmddhh).
    character*3 , intent(in)    :: ce               ! Member index -> character.
    integer     , intent(in)    :: ounit            ! Unit where fiels is read
    integer     , intent(in)    :: ni, nj, nkdum    ! Grid dimensions.
    real        , intent(in)    :: field_2d(:,:)    ! Field 2D

    character(len=filename_len)    :: filename      ! Input filename.

    filename = trim(variable)//'/'//date(1:10)
    filename = trim(filename)//'.'//trim(variable)//'.e'//ce//'.01'
    open (ounit, file = filename, form='unformatted')
    write(ounit)ni, nj, nkdum
    write(ounit)field_2d
    close(ounit)

  end subroutine write_2d_field
  
  !---------------------------------------------------------------------------------------------
  ! Update Rainy bins: uses I/O to read rain class, then updates bin and bin2d accordingly
  !---------------------------------------------------------------------------------------------
  subroutine update_rain_bin(ni, nj, nk, ce, date, bin2d, bin, dat_dir)

    implicit none

    integer, intent(inout)         :: ni
    integer, intent(inout)         :: nj
    integer, intent(inout)         :: nk
    character*3 ,    intent(in)    :: ce                         ! Member index -> character.
    character*10,    intent(in)    :: date
    integer, intent(inout)         :: bin2d(1:ni,1:nj)
    integer, intent(inout)         :: bin(1:ni,1:nj,1:nk)
    character(len=filename_len), intent(in), optional :: dat_dir
    
    integer             :: i, j, k, nkdum, member, k1, k2, k3, m ! Loop counters.
    integer             :: b, b2, b3                  ! Bin marker.
    integer             :: sdate, cdate, edate        ! Starting, current ending dates.
    integer             :: interval                   ! Period between dates (hours).
    integer             :: ne                         ! Number of ensemble members.
    integer             :: mmax                       ! Maximum mode (after variance truncation)..
    integer             :: bin_type                   ! Type of bin to average over.
    integer             :: num_bins                   ! Number of bins (3D fields).
    integer             :: num_bins2d                 ! Number of bins (2D fields).
    integer             :: iunit
    
    real                :: lat_min, lat_max           ! Used if bin_type = 2 (degrees).
    real                :: binwidth_lat               ! Used if bin_type = 2 (degrees).
    real                :: binwidth_lon               ! Used if bin_type = 9/10
    real                :: hgt_min, hgt_max           ! Used if bin_type = 2 (m).
    real                :: binwidth_hgt               ! Used if bin_type = 2 (m).
   
    character*10        :: start_date, end_date       ! Starting and ending dates.
    character*10        :: variable                   ! Variable name

    character(len=filename_len)    :: filename                   ! Input filename.
!!!DALE    character*3         :: ce                         ! Member index -> character.

    integer             :: rain_class(1:ni,1:nj)

    !        Read rain_class:
    variable = 'raincl'
    if (present(dat_dir)) then
    	filename = trim(dat_dir)//'/'//trim(variable)//'/'//date(1:10)
    else
    	filename = trim(variable)//'/'//date(1:10)
    end if
    filename = trim(filename)//'.'//trim(variable)//'.e'//ce//'.01'
    open (iunit, file = filename, form='unformatted')
    read(iunit)ni, nj, nkdum
    read(iunit)rain_class
    close(iunit)
    !           bin depends on rain class of perturbation - hard coded number of rain bins
    !           re-read bin info:
    if (present(dat_dir)) then
    	filename = trim(dat_dir)//'/bin.data'
    else
    	filename = 'bin.data'
    end if
    open (iunit, file = filename, form='unformatted')
    read(iunit)bin_type
    read(iunit)lat_min, lat_max, binwidth_lat
    read(iunit)hgt_min, hgt_max, binwidth_hgt
    read(iunit)num_bins, num_bins2d
    read(iunit)bin(1:ni,1:nj,1:nk)
    read(iunit)bin2d(1:ni,1:nj)
    close(iunit)
    ! update it 2D
    do j = 1, nj
       do i = 1, ni
          bin2d(i,j)=rain_class(i,j)*num_bins2d/4+bin2d(i,j)
       end do
    end do
    ! update it 3D
    do k = 1, nk
       do j = 1, nj
          do i = 1, ni
             bin(i,j,k)=rain_class(i,j)*num_bins/4+bin(i,j,k)
          end do
       end do
    end do
  end subroutine update_rain_bin
!-------------------------------------------------------------------------------------------------------!
subroutine da_invert_var(var_inv, var, num_bins2d, nk, testing_eofs)

!---------------------------------------------------------------------- 
! Purpose: Invert a 2D matrix
!
! History:
! Date     Author & Comment
! -------- ----------------
! dd/mm/yy Dale Barker
!          Initial version
! -------- End History
!
!---------------------------------------------------------------------
   
   implicit none

   real(kind=8), intent(in)   :: var(1:nk,1:nk,1:num_bins2d)          ! 2D variance matrix
   real(kind=8), intent(out)  :: var_inv(1:nk,1:nk,1:num_bins2d)      ! inverse 2D variance matrix

   logical, intent(in) :: testing_eofs               ! True if testing EOF decomposition.

   integer, intent(in)   :: nk                       ! Square matric size
   integer, intent(in)   :: num_bins2d               ! Bin numbers

   real, parameter     :: variance_threshold = 1e-6  ! Percentage of <psi psi> variance discarded.

   logical             :: testing_eofs_t

   integer             :: k, k2, m
   integer             :: mmax                       ! Maximum mode (after variance truncation)..
   integer             :: b                          ! Bin marker.
   real                :: total_variance             ! Total variance of <psi psi> matrix.
   real                :: cumul_variance             ! Cumulative variance of <psi psi> matrix.
   real                :: summ                       ! Summation dummy.

   real*8, allocatable :: eval(:)                    ! Gridpoint sqrt(eigenvalues).
   real*8, allocatable :: evec(:,:)                  ! Gridpoint eigenvectors.
   real, allocatable   :: work(:,:)                  ! EOF work array.
   real, allocatable   :: LamInvET(:,:)              ! ET/sqrt(Eigenvalue).

   allocate( work(1:nk,1:nk) )
   allocate( evec(1:nk,1:nk) )
   allocate( eval(1:nk) )
   allocate( LamInvET(1:nk,1:nk) )

   testing_eofs_t = testing_eofs

   do b = 1, num_bins2d   
      LamInvET(:,:) = 0.0
      work(1:nk,1:nk) = var(1:nk,1:nk,b)
      call da_eof_decomposition( nk, work, evec, eval )

      if ( testing_eofs_t ) then
         call da_eof_decomposition_test( nk, work, evec, eval )
         testing_eofs_t = .false.
      end if

!     Truncate eigenvalues to ensure inverse is not dominated by rounding error:
      summ = 0.0
      do m = 1, nk
         summ = summ + eval(m)
      end do
      total_variance = summ

      cumul_variance = 0.0
      mmax = nk
      do m = 1, nk
         cumul_variance = cumul_variance + eval(m) / total_variance
         if ( cumul_variance > 1.0 - variance_threshold ) then
            mmax = m - 1
            exit
         end if
      end do
      mmax=max(1,mmax)
      write(6,'(2(a,i6),2(a,1pe11.5))') &
      ' Bin = ', b, ', truncation = ', mmax, &
      ', Total Variance = ', total_variance, &
      ', Condition number = ', eval(1) / eval(mmax)

!     Lam{-1} . E^T:
      do k = 1, nk
         do m = 1, mmax
            LamInvET(m,k) = evec(k,m) / eval(m)
         end do
      end do

!     <psi psi>^{-1} = E . Lam{-1} . E^T:

      do k = 1, nk
         do k2 = 1, k
            summ = 0.0
            do m = 1, nk
               summ = summ + evec(k,m) * LamInvET(m,k2)
            end do
            var_inv(k,k2,b) = summ
         end do
      end do

      do k = 1, nk
         do k2 = k+1, nk ! Symmetry.
            var_inv(k,k2,b) = var_inv(k2,k,b)
         end do
      end do
   end do

   deallocate( work )
   deallocate( evec )
   deallocate( eval )
   deallocate( LamInvET )

end subroutine da_invert_var

subroutine da_transform_vptovv_bin7(evec,eval,vertical_wgt,vp,vv,nb,ni,nj,nk,bin2d)

   !---------------------------------------------------------------------------
   ! Purpose: Transform from fields on vertical levels to fields on vertical 
   ! EOFS.
   !
   ! Method: Perform vv(i,j,k) = L^{-1/2} E^{T} vp(i,j,k) transform.
   !---------------------------------------------------------------------------

   implicit none

   integer, intent(in)    :: nb, ni, nj, nk                ! Dimensions
   real(kind=8),    intent(in)    :: evec(1:nb,1:nk,1:nk)          ! Eigenvectors.
   real(kind=8),    intent(in)    :: eval(1:nb,1:nk)               ! Eigenvalues.
   real(kind=8),    intent(in)    :: vertical_wgt(1:ni,1:nj,1:nk)  ! Weighting.
   integer, intent(in)    :: bin2d(1:ni,1:nj)              ! Binning
   real(kind=8),    intent(inout) :: vp(1:ni,1:nj,1:nk)            ! CV in level space.
   real(kind=8),    intent(out)   :: vv(1:ni,1:nj,1:nk)            ! CV in EOF space.
   
   integer :: i, j, k, b                 ! Loop counters.
   real    :: ETVp                       ! E(k,m)^{T}*vp(i,j,k)
   
   !-------------------------------------------------------------------
   ! [1.0] Apply inner-product weighting if vertical_ip /= vertical_ip_0
   !------------------------------------------------------------------- 

!   if (vertical_ip /= vertical_ip_0) then
      vp(1:ni,1:nj,1:nk) = vp(1:ni,1:nj,1:nk) * &
                                    vertical_wgt(1:ni,1:nj,1:nk)
!   end if

   !-------------------------------------------------------------------
   ! [2.0] Perform vv(i,j,k) = L^{-1/2} E^T vp(i,j,k) transform:
   !-------------------------------------------------------------------

   do k = 1, nk
      do j = 1, nj
         do i = 1, ni
            b = bin2d(i,j)
            ETVp = sum(evec(b,1:nk,k)*vp(i,j,1:nk))
            vv(i,j,k) = ETVp / eval(b,k)
         end do
      end do
   end do

end subroutine da_transform_vptovv_bin7
end module da_gen_be
