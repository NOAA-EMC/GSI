
module read_benc

!------------------------------------------------------------------------
!  Purpose: new reader of be.nc for WRFDA applications 
!           gen_be (new features)
!
!  Auothor: Gael Descombes (MMM/NESL/NCAR)   Date: 01/07/2012
!
!------------------------------------------------------------------------

character (len=10) ::  StrLen 

interface read_field
   module procedure read_field3d
   module procedure read_field2d
   module procedure read_field1d
   module procedure read_field0d
   module procedure read_field1dstr
   module procedure read_field1dint
end interface read_field

contains

!! read  string

subroutine read_field1dstr(rd_ncid, dim1, StrLen, varname, field1d)

   implicit none
   integer, intent(in) :: rd_ncid, StrLen, dim1
   character (len=*), intent(in) :: varname
   character(len=*), dimension(1:dim1), intent(inout) :: field1d
   integer :: rdVarIDvarnd
   integer, dimension(2) :: start2, count2
   integer :: nferr

   include 'netcdf.inc'

   nferr = nf_inq_varid(rd_ncid, varname, rdVarIDvarnd)
   start2(2) = 1
   count2(2) = dim1
   start2(1) = 1
   count2(1) = StrLen
   nferr = nf_get_vara_text(rd_ncid, rdVarIDvarnd, start2, count2, field1d )

end subroutine  read_field1dstr

!! read integer

subroutine read_field1dint(rd_ncid, dim1, varname, field1d)

   implicit none
   integer, intent(in) :: dim1, rd_ncid
   integer, dimension(1:dim1), intent(inout) :: field1d
   character (len=*), intent(in) :: varname
   integer :: rdVarIDvarnd
   integer, dimension(1) :: start1, count1
   integer :: nferr

   include 'netcdf.inc'

   start1(1) = 1
   count1(1) = dim1
   nferr = nf_inq_varid(rd_ncid, trim(varname), rdVarIDvarnd)
   nferr = nf_get_vara_int( rd_ncid, rdVarIDvarnd, start1, count1, field1d)

end subroutine read_field1dint

subroutine read_field2dint(rd_ncid, rdVarIDvarnd, dim1, dim2, varname, field2d)

   implicit none
   integer, intent(in) :: dim1, dim2, rd_ncid
   integer, dimension(1:dim1,1:dim2), intent(inout) :: field2d
   character (len=*), intent(in) :: varname
   integer :: rdVarIDvarnd
   integer, dimension(2) :: start2, count2
   integer :: nferr

   include 'netcdf.inc'

   start2(1) = 1
   count2(1) = dim1
   start2(2) = 1
   count2(2) = dim2
   nferr = nf_inq_varid(rd_ncid, trim(varname), rdVarIDvarnd)
   nferr = nf_get_vara_int( rd_ncid, rdVarIDvarnd, start2, count2, field2d)

end subroutine read_field2dint


!! read Real

subroutine read_field0d(rd_ncid, rdVarIDvarnd, varname, field0d)

   implicit none
   integer, intent(in) :: rd_ncid
   real, intent(inout) :: field0d
   character (len=*), intent(in) :: varname
   integer :: rdVarIDvarnd
   integer, dimension(1) :: start1, count1
   integer :: nferr

   include 'netcdf.inc'

   start1(1) = 1
   count1(1) = 1
   nferr = nf_inq_varid(rd_ncid, trim(varname), rdVarIDvarnd)
   nferr = nf_get_vara_double( rd_ncid, rdVarIDvarnd, start1, count1, field0d)

end subroutine read_field0d

subroutine read_field1d(rd_ncid, rdVarIDvarnd, dim1, varname, field1d)

   implicit none
   integer, intent(in) :: dim1, rd_ncid
   real, dimension(1:dim1), intent(inout) :: field1d
   character (len=*), intent(in) :: varname
   integer :: rdVarIDvarnd
   integer, dimension(1) :: start1, count1
   integer :: nferr

   include 'netcdf.inc'

   start1(1) = 1
   count1(1) = dim1
   nferr = nf_inq_varid(rd_ncid, trim(varname), rdVarIDvarnd)
   nferr = nf_get_vara_double( rd_ncid, rdVarIDvarnd, start1, count1, field1d)

end subroutine read_field1d

subroutine read_field2d(rd_ncid, rdVarIDvarnd, dim1, dim2, varname, field2d)

   implicit none
   integer, intent(in) :: dim1, dim2, rd_ncid
   real, dimension(1:dim1,1:dim2), intent(inout) :: field2d
   character (len=*), intent(in) :: varname
   integer :: rdVarIDvarnd
   integer, dimension(2) :: start2, count2
   integer :: nferr

   include 'netcdf.inc'

   start2(1) = 1
   count2(1) = dim1
   start2(2) = 1
   count2(2) = dim2
   nferr = nf_inq_varid(rd_ncid, trim(varname), rdVarIDvarnd)
   nferr = nf_get_vara_double( rd_ncid, rdVarIDvarnd, start2, count2, field2d)

end subroutine read_field2d

subroutine read_field3d(rd_ncid, rdVarIDvarnd, dim1, dim2, dim3, varname, field3d)

   implicit none
   integer, intent(in) :: dim1, dim2, dim3, rd_ncid
   real, dimension(1:dim1,1:dim2,1:dim3), intent(inout) :: field3d
   character (len=*), intent(in) :: varname
   integer :: rdVarIDvarnd
   integer, dimension(3) :: start3, count3
   integer :: nferr

   include 'netcdf.inc'

   start3(1) = 1
   count3(1) = dim1
   start3(2) = 1
   count3(2) = dim2
   start3(3) = 1
   count3(3) = dim3
   nferr = nf_inq_varid(rd_ncid, trim(varname), rdVarIDvarnd)
   nferr = nf_get_vara_double( rd_ncid, rdVarIDvarnd, start3, count3, field3d)

end subroutine read_field3d



!============================================================================================

end module read_benc 


