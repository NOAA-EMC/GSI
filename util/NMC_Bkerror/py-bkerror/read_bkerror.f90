program read_bkerror

    use bkerror, only : get_header,get_bkerror
    use str2arr2str, only : arr2str

    implicit none

    integer,parameter :: i_kind=4
    integer,parameter :: r_kind=4

    character(len=255) :: fname
    integer(i_kind) :: nsig,nlat,nlon
    integer(i_kind),dimension(10,4) :: vararr
    real(r_kind),dimension(:,:,:),allocatable :: agvin
    real(r_kind),dimension(:,:),allocatable :: bgvin,wgvin
    real(r_kind),dimension(:,:,:),allocatable :: corzin,hscalesin,vscalesin
    real(r_kind),dimension(:,:),allocatable :: corq2in
    real(r_kind),dimension(:,:),allocatable :: corsstin,hsstin
    real(r_kind),dimension(:),allocatable :: corpin,hscalespin

    character(len=4) :: var
    integer(i_kind) :: i

    call getarg(1,fname)

    call get_header(fname,nsig,nlat,nlon)

    write(6,'(3(A,I4))') 'nsig = ',nsig,' nlat = ',nlat,' nlon = ',nlon

    allocate(agvin(nlat,nsig,nsig))
    allocate(bgvin(nlat,nsig),wgvin(nlat,nsig))
    allocate(corzin(nlat,nsig,6),hscalesin(nlat,nsig,6),vscalesin(nlat,nsig,6))
    allocate(corq2in(nlat,nsig))
    allocate(corsstin(nlat,nlon),hsstin(nlat,nlon))
    allocate(corpin(nlat),hscalespin(nlat))

    call get_bkerror(fname,nsig,nlat,nlon,&
        vararr,agvin,bgvin,wgvin,corzin,hscalesin,vscalesin,&
        corq2in,corsstin,hsstin,corpin,hscalespin)

    do i=1,10
        call arr2str(vararr(i,:),var,3)
        if ( var(1:3) /= ' ') write(6,'(A,A)') 'var = ', trim(var)
    enddo

    stop

end program read_bkerror
