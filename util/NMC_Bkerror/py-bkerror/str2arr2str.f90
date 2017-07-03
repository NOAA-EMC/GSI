!f2py -c str2arr2str.f90 -m str2arr2str --fcompiler=intelem
module str2arr2str

    implicit none

    private

    public :: str2arr
    public :: arr2str

contains

subroutine str2arr(strin, chararr, n_str)

    implicit none

    integer, intent(in) :: n_str
    character(len=n_str), intent(in) :: strin
    integer, dimension(n_str+1), intent(out) :: chararr

    integer :: j

    chararr = 32 ! space
    do j=1,n_str
        chararr(j) = ichar(strin(j:j))
    enddo
    chararr(n_str+1) = 124 ! '|'

    return

end subroutine str2arr

subroutine arr2str(chararr, strout, n_str)

    implicit none

    integer, intent(in) :: n_str
    integer, dimension(n_str+1), intent(in) :: chararr
    character(len=n_str), intent(out) :: strout

    integer :: j

    do j=1,n_str
        strout(j:j) = char(chararr(j))
    enddo

    return

end subroutine arr2str

end module str2arr2str
