 module native_endianness
!$$$ module documentation block
!           .      .    .                                       .
! module:   native_endianness
!   prgmmr: parrish          org: wx22                date: 2012-10-11
!
! abstract:  This module was written by Dusan Jovic and has been adapted to GSI for internal translation
!             of WRF ARW and NMM binary restart files as required to match the machine native 
!             endian storage format.  The original code only converted from big-endian to little-endian.
!             There are no restrictions in this version.
!             This is required for these two types of files, because they are read/written to using mpi-io,
!             which has no compiler option for automatic switching to machine native endian format
!             for fortran unformatted read/write.
!
! program history log:
!   2012-10-11  parrish - copy/modify original module native_endianness provided by Dusan Jovic, NCEP/EMC 2012
!
! subroutines included:
!   to_native_endianness - reverse bytes of argument (output overwrites input)
!
! functions included:
!   is_little_endian - no argument--returns true for little-endian machine, false for big-endian machine
!
! variables included:
!   byte_swap            - false if machine and wrf binary file are same endian, true if different
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

 use kinds, only: i_byte,i_short,i_long,r_single,r_double
 implicit none

 private

 public byte_swap
 public to_native_endianness
 public is_little_endian

 interface to_native_endianness
   module procedure to_native_endianness_i2
   module procedure to_native_endianness_i4
   module procedure to_native_endianness_r4
   module procedure to_native_endianness_r8
 end interface to_native_endianness

 logical byte_swap

 contains

 logical function is_little_endian()
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    is_little_endian
!   prgmmr: parrish          org: wx22                date: 2012-10-11
!
! abstract: test to see if machine is little-endian.  Returns true for little-endian, false for big-endian.
!
! program history log:
!   2012-10-11  parrish - add doc block
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

   integer(i_byte) :: i1
   integer(i_long) :: i2

   i1 = 1
   i2 = 0
   i2 = transfer(i1,i2)

   is_little_endian = (i1 == i2)

 end function is_little_endian

!----------------------------------------------------------------------
! convert 2-byte integer scalar from big-endian to native-endian
!----------------------------------------------------------------------

 subroutine to_native_endianness_i2(i2)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    to_native_endianness_i2
!   prgmmr: parrish          org: wx22                date: 2012-10-11
!
! abstract: swap bytes of argument.
!
! program history log:
!   2012-10-11  parrish - add doc block
!
!   input argument list:
!    i2 - input 2 byte integer argument
!
!   output argument list:
!    i2 - output 2 byte integer argument with bytes in reverse order
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

 implicit none

 integer(i_short), intent(inout) :: i2

 integer(i_byte), dimension(2) :: byte_arr, byte_arr_tmp
 integer(i_long) :: i

 byte_arr_tmp = transfer (i2, byte_arr)

 do i = 1, 2
   byte_arr(i) = byte_arr_tmp(3-i)
 end do

 i2 = transfer (byte_arr, i2)

 return

 end subroutine to_native_endianness_i2


!----------------------------------------------------------------------
! convert 4-byte integer scalar from big-endian to native-endian
!----------------------------------------------------------------------

 subroutine to_native_endianness_i4(i4)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    to_native_endianness_i4
!   prgmmr: parrish          org: wx22                date: 2012-10-11
!
! abstract: swap bytes of argument.
!
! program history log:
!   2012-10-11  parrish - add doc block
!
!   input argument list:
!    i4 - input 4 byte integer argument
!
!   output argument list:
!    i4 - output 4 byte integer argument with bytes in reverse order
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

 implicit none

 integer(i_long), intent(inout) :: i4

 integer(i_byte), dimension(4) :: byte_arr, byte_arr_tmp
 integer(i_long) :: i

 byte_arr_tmp = transfer (i4, byte_arr)

 do i = 1, 4
   byte_arr(i) = byte_arr_tmp(5-i)
 end do

 i4 = transfer (byte_arr, i4)

 return

 end subroutine to_native_endianness_i4

!----------------------------------------------------------------------
! convert 4-byte real scalar from big-endian to native-endian
!----------------------------------------------------------------------

 subroutine to_native_endianness_r4(r4)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    to_native_endianness_r4
!   prgmmr: parrish          org: wx22                date: 2012-10-11
!
! abstract: swap bytes of argument.
!
! program history log:
!   2012-10-11  parrish - add doc block
!
!   input argument list:
!    r4 - input 4 byte floating point argument
!
!   output argument list:
!    r4 - output 4 byte floating point argument with bytes in reverse order
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

 implicit none

 real(r_single), intent(inout) :: r4

 integer(i_byte), dimension(4) :: byte_arr, byte_arr_tmp
 integer :: i

 byte_arr_tmp = transfer (r4, byte_arr)

 do i = 1, 4
   byte_arr(i) = byte_arr_tmp(5-i)
 end do

 r4 = transfer (byte_arr, r4)

 return

 end subroutine to_native_endianness_r4

!----------------------------------------------------------------------
! convert 8-byte real scalar from big-endian to native-endian
!----------------------------------------------------------------------

 subroutine to_native_endianness_r8(r8)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    to_native_endianness_r4
!   prgmmr: parrish          org: wx22                date: 2012-10-11
!
! abstract: swap bytes of argument.
!
! program history log:
!   2012-10-11  parrish - add doc block
!
!   input argument list:
!    r8 - input 8 byte floating point argument
!
!   output argument list:
!    r8 - output 8 byte floating point argument with bytes in reverse order
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

 implicit none

 real(r_double), intent(inout) :: r8

 integer(i_byte), dimension(8) :: byte_arr, byte_arr_tmp
 integer(i_long) :: i

 byte_arr_tmp = transfer (r8, byte_arr)

 do i = 1, 8
   byte_arr(i) = byte_arr_tmp(9-i)
 end do

 r8 = transfer (byte_arr, r8)

 return

 end subroutine to_native_endianness_r8

 end module native_endianness

