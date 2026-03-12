module crc32
  use iso_c_binding
  implicit none
  public :: digest

  interface
    integer function digest_c(message) bind(c)
      use iso_c_binding, only: c_char
      character(kind=c_char), intent(in) :: message(*)
    end function digest_c
  end interface

  contains

  integer function digest(m)
    use iso_c_binding, only: c_null_char
    implicit none
    character(len=*), intent(in) :: m
    digest=abs(digest_c(trim(m)//c_null_char))
  end function digest
end module crc32
