program chgdate

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! < next few line under version control, D O  N O T  E D I T >
! $URL$
! $Revision$
! $Date$
! $Author$
! $Id$
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Program to change the idate and ifhr in a spectral sigma 
! and/or surface file
! USAGE:
! ./chgdate SIG SIGFILENAMEIN NEWIDATE NEWIFHR SIGFILENAMEOUT
! ./chgdate SFC SFCFILENAMEIN NEWIDATE NEWIFHR SFCFILENAMEOUT
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

use sigio_module
use sfcio_module

implicit none

type(sigio_head) :: sighead
type(sigio_data) :: sigdata
type(sfcio_head) :: sfchead
type(sfcio_data) :: sfcdata

integer, parameter :: nsi=21, nso=61

integer :: iret,idateout(4),fhour
character(len=120) :: filenamein,filenameout
character(len=10)  :: datestring
character(len=3)   :: filetype,charfhr

! what type of file is this: SIG or SFC
call getarg(1,filetype)
! read data from this file
call getarg(2,filenamein)
! use this date
call getarg(3,datestring)
! and this forecast hour
call getarg(4,charfhr)
read(charfhr,'(i3)') fhour
! and put in this file.
call getarg(5,filenameout)

read(datestring(1:4),'(i4)') idateout(4)
read(datestring(5:6),'(i2)') idateout(2)
read(datestring(7:8),'(i2)') idateout(3)
read(datestring(9:10),'(i2)') idateout(1)

select case ( trim(filetype) )
    case ('sig','SIG')
        call sigio_srohdc(nsi,trim(filenamein),sighead,sigdata,iret)
    case ('sfc','SFC')
        call sfcio_srohdc(nsi,trim(filenamein),sfchead,sfcdata,iret)
    case default
        print *,'unknown filetype, cannot read',trim(filetype)
        stop
end select

if ( iret /= 0 ) then
    print *,'error reading',trim(filenamein)
    stop
else
    print *,trim(filenamein)
    print *,iret
endif

select case ( trim(filetype) )
    case ('sig','SIG')
        sighead%idate = idateout
        sighead%fhour = fhour
        call sigio_swohdc(nso,trim(filenameout),sighead,sigdata,iret)
    case ('sfc','SFC')
        sfchead%idate = idateout
        sfchead%fhour = fhour
        call sfcio_swohdc(nso,trim(filenameout),sfchead,sfcdata,iret)
    case default
        print *,'unknown filetype, cannot write',trim(filetype)
        stop
end select

if ( iret /= 0 ) then
    print *,'error writing',trim(filenameout)
    stop
else
    print *,trim(filenameout)
    print *,iret
endif

stop
end program chgdate
