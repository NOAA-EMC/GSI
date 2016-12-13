module da_reporting

   use da_control, only : stdout, use_html, documentation_url, &
      warnings_are_fatal

   implicit none

   character(len=10000) :: message(50)

contains

SUBROUTINE wrf_message( str )
  IMPLICIT NONE
  CHARACTER*(*) str
  write(0,*) TRIM(str)
  print*, TRIM(str)
END SUBROUTINE wrf_message

subroutine da_error( file_str, line, errors)

   !-----------------------------------------------------------------------
   ! Purpose: Standardised error reporting
   !-----------------------------------------------------------------------

   implicit none

   character(len=*), intent(in) :: file_str
   integer ,         intent(in) :: line  ! only print file and line if line > 0
   character(len=*), intent(in) :: errors(:)
   character*256 :: line_str
   character*256 :: html_file
   integer       :: i

   write(line_str,'(i6)') line

   html_file=file_str(1:LEN_trim(file_str)-4)//'.html'
   
   call wrf_message( &
      '---------------------------- FATAL ERROR -----------------------' )
   ! only print file and line if line is positive
   if (line > 0) then
      if (use_html) then
         call wrf_message( 'Fatal error in file: <A HREF="'// &
            trim(documentation_url)//'/'//trim(html_file)//'">'//file_str// &
            '</a>  LINE:  '//trim(line_str) )
      else
         call wrf_message( 'Fatal error in file:  '//trim(file_str)// &
            '  LINE:  '//trim(line_str) )
      end if
   end if
   do i=1,size(errors)
      call wrf_message(errors(i))
   end do
   call wrf_message( &
      '----------------------------------------------------------------' )
   stop
end subroutine da_error


subroutine da_warning(file_str, line, warnings)

   !--------------------------------------------------------------------
   ! Purpose: Standard interface for warning messages
   !--------------------------------------------------------------------

   implicit none

   character(len=*), intent(in) :: file_str
   integer,          intent(in) :: line
   character(len=*), intent(in) :: warnings(:)
   character*256 :: line_str
   character*256 :: html_file
   integer :: i

   if (warnings_are_fatal) then
      call da_error(file_str, line, warnings)
   else
      write(line_str,'(i6)') line

      html_file=file_str(1:LEN_trim(file_str)-4)//'.html'

      call wrf_message( &
         '--------------------------- WARNING ---------------------------')
      ! only print file and line if line is positive
      if (line > 0) then
         if (use_html) then
            call wrf_message('WARNING FROM FILE: <A HREF="'// &
               trim(documentation_url)//'/'//trim(html_file)//'">'// &
               trim(file_str)// &
                  '</a>  LINE:  '//trim(line_str))
         else
            call wrf_message('WARNING FROM FILE:  '//trim(file_str)// &
               '  LINE:  '//trim(line_str))
         end if
      end if
      do i=1,size(warnings) 
         call wrf_message(warnings(i))
      end do
      call wrf_message( &
         '---------------------------------------------------------------')
   end if

end subroutine da_warning


subroutine da_message(strs)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   character(len=*), intent(in) :: strs(:)
   integer :: i

   do i=1,size(strs) 
      write(unit=stdout,fmt='(a)') trim(strs(i))
   end do
   write(unit=stdout,fmt=*) " "

end subroutine da_message


subroutine da_message2(strs)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   character(len=*), intent(in) :: strs(:)
   integer :: i

   do i=1,size(strs) 
      write(0,*) strs(i)
   end do

end subroutine da_message2



end module da_reporting
