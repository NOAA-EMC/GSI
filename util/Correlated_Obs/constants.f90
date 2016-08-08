module constants
use kinds, only: r_kind, i_kind
implicit none
public:: zero, one, one_int, two, three_int, four_int, sixty, one_hundred, threesixty
public:: rad, pi
!numbers
real(r_kind), parameter:: zero=0.0_r_kind
real(r_kind), parameter:: one=1.0_r_kind
real(r_kind), parameter:: one_int=1
real(r_kind), parameter:: two=2.0_r_kind
integer(i_kind), parameter:: three_int=3
integer(i_kind), parameter:: four_int=4
real(r_kind), parameter:: sixty=60.0_r_kind
real(r_kind), parameter:: one_hundred=100.0_r_kind
real(r_kind), parameter:: threesixty=360.0_r_kind

!other constants
real(r_kind), parameter:: rad=6378.137_r_kind          !radius of the earth
real(r_kind), parameter:: pi=3.1415926535898_r_kind
end module constants
