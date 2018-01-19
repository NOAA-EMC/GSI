program zero_biascoeff

! Abstract:
! Zero-out radiances and aircraft bias correction coefficients

! Author: Rahul Mahajan

! Inputs:
! abias
! abias_air

! Outputs:
! abias.zeroed, abias_pc.zeroed
! abias_air.zeroed

implicit none

integer, parameter :: r_double=8

integer, parameter :: lunin=11,lunout=12,lunout_pc=13
integer :: istat,npred,ip
logical :: lexist

integer :: ich,ichan,count_tlap
real(r_double) :: tlapm,tsum,ostats
real(r_double), allocatable, dimension(:) :: pred
real(r_double), allocatable, dimension(:) :: varx
character(len=20) :: isis

integer :: ii,timeid
real(r_double), allocatable, dimension(:) :: vary
character(len=10) :: tailid

! Zero out radiance bias correction coefficients

inquire(file='abias', exist=lexist)
if ( .not. lexist ) then

    write(6,'(a)') 'abias does not exist!'

else

    write(6,'(a)') 'Zero-out radiance bias correction coefficients from: abias'

    open(lunin, file='abias', form='formatted')
    open(lunout, file='abias.zeroed', form='formatted')
    open(lunout_pc, file='abias_pc.zeroed', form='formatted')

    npred = 12

    allocate(pred(npred))
    allocate(varx(npred))

    abias: do

        read(lunin,101,iostat=istat) &
             ich,isis,ichan,tlapm,tsum,count_tlap,(pred(ip),ip=1,npred)
        if ( istat /= 0 ) exit

        pred  = 0.0
        tlapm = 0.0
        tsum  = 0.0
        count_tlap = 0

        write(lunout,101) &
              ich,isis,ichan,tlapm,tsum,count_tlap,(pred(ip),ip=1,npred)

        ostats = 0.0
        varx   = 0.0

        write(lunout_pc,102) &
              ich,isis,ichan,ostats,(varx(ip),ip=1,npred)

    enddo abias

101 format(i5,1x,a20,1x,i5,2e15.6,1x,i5/2(4x,10f12.6/))
102 format(i5,1x,a20,1x,i5,e15.7/2(4x,10e15.7/))

    deallocate(pred)
    deallocate(varx)

    close(lunin)
    close(lunout)
    close(lunout_pc)

endif ! if abias exists

! Zero out aircraft bias correction coefficients

inquire(file='abias_air', exist=lexist)
if ( .not. lexist ) then

    write(6,'(a)') 'abias_air does not exist!'

else

    write(6,'(a)') 'Zero-out aircraft bias correction coefficients from: abias_air'

    open(lunin, file='abias_air', form='formatted')
    open(lunout, file='abias_air.zeroed', form='formatted')

    npred = 3

    allocate(pred(npred))
    allocate(varx(npred))
    allocate(vary(npred))

    abias_air: do

        read(lunin,103,iostat=istat) &
            tailid,ii, &
            (pred(ip),ip=1,npred),(varx(ip),ip=1,npred),(vary(ip),ip=1,npred), &
            timeid
        if ( istat /= 0 ) exit

        pred = 0.0
        varx = 0.0
        vary = 0.0

        write(lunout,103) &
            tailid,ii, &
            (pred(ip),ip=1,npred),(varx(ip),ip=1,npred),(vary(ip),ip=1,npred), &
            timeid

    enddo abias_air

! should match format statement from aircraftinfo.f90
103 format(1x,a10,1x,i5,9(1x,f12.6),1x,i7)

    deallocate(pred)
    deallocate(varx)
    deallocate(vary)

    close(lunin)
    close(lunout)

endif ! if abias_air exists

stop

end program zero_biascoeff
