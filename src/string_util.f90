module string_util

   ! From PvD

   implicit none

   private

   public :: StrUppCase
   public :: StrLowCase

   character(len=26),private,parameter :: lower_case = 'abcdefghijklmnopqrstuvwxyz'
   character(len=26),private,parameter :: upper_case = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'

contains

   function StrUppCase ( Input_String ) result ( Output_String )
     ! -- Argument and result
     character( * ), intent( in )     :: Input_String
     character( len( Input_String ) ) :: Output_String
     ! -- Local variables
     integer :: i, n

     ! -- Copy input string
     Output_String = Input_String
     ! -- Loop over string elements
     do i = 1, len( Output_String )
       ! -- Find location of letter in lower case constant string
       n = index( lower_case, Output_String( i:i ) )
       ! -- If current substring is a lower case letter, make it upper case
       if ( n /= 0 ) Output_String( i:i ) = upper_case( n:n )
     enddo
   end function StrUppCase

   function StrLowCase ( Input_String ) result ( Output_String )
     ! -- Argument and result
     character( * ), intent( in )     :: Input_String
     character( len( Input_String ) ) :: Output_String
     ! -- Local variables
     integer :: i, n

     ! -- Copy input string
     Output_String = Input_String
     ! -- Loop over string elements
     do i = 1, len( Output_String )
       ! -- Find location of letter in upper case constant string
       n = index( upper_case, Output_String( i:i ) )
       ! -- If current substring is an upper case letter, make it lower case
       if ( n /= 0 ) Output_String( i:i ) = lower_case( n:n )
     enddo
   end function StrLowCase

end module string_util
