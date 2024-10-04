module bkerror

    implicit none

    private

    public :: get_header
    public :: get_bkerror
    public :: put_bkerror

    integer, parameter :: r_kind = 4
    integer, parameter :: i_kind = 4

    integer(i_kind), parameter :: lunit = 10

contains

subroutine get_header(fname, x_nsig, x_nlat, x_nlon)

    implicit none

    character(len=255), intent(in) :: fname
    integer(i_kind), intent(out) :: x_nsig, x_nlat, x_nlon

    open(lunit, file=trim(adjustl(fname)), form='unformatted', convert='big_endian')
    rewind lunit
    read(lunit) x_nsig, x_nlat, x_nlon
    close(lunit)

    return

end subroutine get_header

subroutine get_bkerror(fname, nsig, nlat, nlon, &
        x_var, x_agvin, x_bgvin, x_wgvin, x_corzin, x_hscalesin, x_vscalesin, &
        x_corq2in, x_corsstin, x_hsstin, x_corpin, x_hscalespin)

    use str2arr2str, only: str2arr

    implicit none

    character(len=255), intent(in) :: fname
    integer(i_kind), intent(in) :: nsig, nlat, nlon
    integer(i_kind), dimension(10, 4), intent(out) :: x_var
    real(r_kind), dimension(nlat, nsig, nsig), intent(out) :: x_agvin
    real(r_kind), dimension(nlat, nsig), intent(out) :: x_bgvin, x_wgvin
    real(r_kind), dimension(nlat, nsig, 6), intent(out) :: x_corzin, x_hscalesin, x_vscalesin
    real(r_kind), dimension(nlat, nsig), intent(out) :: x_corq2in
    real(r_kind), dimension(nlat, nlon), intent(out) :: x_corsstin, x_hsstin
    real(r_kind), dimension(nlat), intent(out) :: x_corpin, x_hscalespin

    integer(i_kind) :: x_nsig, x_nlat, x_nlon, i, isig
    character(len=5) :: var

    open(lunit, file=trim(adjustl(fname)), form='unformatted', convert='big_endian')

    rewind lunit
    read(lunit) x_nsig, x_nlat, x_nlon

    if ( nsig /= x_nsig .or. nlat /= x_nlat .or. nlon /= x_nlon ) then
        write(6,*) 'Mismatch between incoming nsig, nlat, nlon and those from the file'
        write(6,*) 'nsig_in, nsig_file =', nsig, x_nsig
        write(6,*) 'nlat_in, nlat_file =', nlat, x_nlat
        write(6,*) 'nlon_in, nlon_file =', nlon, x_nlon
        close(lunit)
        stop
    endif

    read(lunit) x_agvin, x_bgvin, x_wgvin

    do i=1,6
        read(lunit) var, x_nsig
        call str2arr(var, x_var(i,:), 3)
        if ( i == 4 ) then
            read(lunit) x_corzin(:,:,i), x_corq2in
            read(lunit) x_hscalesin(:,:,i)
            read(lunit) x_vscalesin(:,:,i)
        else
            read(lunit) x_corzin(:,:,i)
            read(lunit) x_hscalesin(:,:,i)
            read(lunit) x_vscalesin(:,:,i)
        endif
    enddo

    i = 7
    read(lunit) var, isig
    call str2arr(var, x_var(i,:), 3)
    read(lunit) x_corpin
    read(lunit) x_hscalespin

    i = 8
    read(lunit) var, isig
    call str2arr(var, x_var(i,:), 3)
    read(lunit) x_corsstin
    read(lunit) x_hsstin

    close(lunit)

    return

end subroutine get_bkerror

subroutine put_bkerror(i_fname, i_nsig, i_nlat, i_nlon, i_var, &
        i_agvin, i_bgvin, i_wgvin, i_corzin, i_hscalesin, i_vscalesin, &
        i_corq2in, i_corsstin, i_hsstin, i_corpin, i_hscalespin)

    use str2arr2str, only: arr2str

    implicit none

    character(len=255), intent(in) :: i_fname
    integer(i_kind), intent(in) :: i_nsig, i_nlat, i_nlon
    integer(i_kind), dimension(10, 4), intent(in) :: i_var
    real(r_kind), dimension(i_nlat, i_nsig, i_nsig), intent(in) :: i_agvin
    real(r_kind), dimension(i_nlat, i_nsig), intent(in) :: i_bgvin, i_wgvin
    real(r_kind), dimension(i_nlat, i_nsig, 6), intent(in) :: i_corzin, i_hscalesin, i_vscalesin
    real(r_kind), dimension(i_nlat, i_nsig), intent(in) :: i_corq2in
    real(r_kind), dimension(i_nlat, i_nlon), intent(in) :: i_corsstin, i_hsstin
    real(r_kind), dimension(i_nlat), intent(in) :: i_corpin, i_hscalespin

    character(len=5) :: var
    integer :: i, isig

    open(lunit, file=trim(adjustl(i_fname)), form='unformatted', convert='big_endian')
    rewind lunit
    write(lunit) i_nsig, i_nlat, i_nlon
    write(lunit) i_agvin, i_bgvin, i_wgvin

    var='     '

    do i=1,6
        call arr2str(i_var(i,:), var, 3)
        write(6,*) i, var, i_nsig
        write(lunit) var, i_nsig
        if (i==4) then
            write(lunit) i_corzin(:,:,i), i_corq2in
            write(lunit) i_hscalesin(:,:,i)
            write(lunit) i_vscalesin(:,:,i)
        else
            write(lunit) i_corzin(:,:,i)
            write(lunit) i_hscalesin(:,:,i)
            write(lunit) i_vscalesin(:,:,i)
        end if
    enddo

    isig = 1

    i = 7
    call arr2str(i_var(i,:), var, 3)
    write(6,*) i, var, isig
    write(lunit) var, isig
    write(lunit) i_corpin
    write(lunit) i_hscalespin

    i = 8
    call arr2str(i_var(i,:), var, 3)
    write(6,*) i, var, isig
    write(lunit) var, isig
    write(lunit) i_corsstin
    write(lunit) i_hsstin

    close(lunit)

    return

end subroutine put_bkerror

end module bkerror
