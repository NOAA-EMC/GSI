      program chgdatesig

      USE SIGIO_MODULE
      implicit none
      TYPE(SIGIO_HEAD) :: SIGHEAD
      TYPE(SIGIO_DATA) :: SIGDATA
      character*120 filenamein,filenameout
      integer nsigi,nsigo,iret,idateout(4),fhour
      character*10 datestring
      character*3 charfhr
      NSIGI=21
      NSIGO=61
c read data from this file
      call getarg(1,filenamein)
c use this date
      call getarg(2,datestring)
c and this forecast hour
      call getarg(3,charfhr)
      read(charfhr,'(i3)') fhour
c and put in this file.
      call getarg(4,filenameout)

      read(datestring(1:4),'(i4)') idateout(4)
      read(datestring(5:6),'(i2)') idateout(2)
      read(datestring(7:8),'(i2)') idateout(3)
      read(datestring(9:10),'(i2)') idateout(1)

      call sigio_srohdc(nsigi,trim(filenamein),sighead,sigdata,iret)
      if (iret .ne. 0) then
       print *,'error reading',trim(filenamein)
       stop
      endif

      print *,trim(filenamein)
      print *,iret

      sighead%idate = idateout
      sighead%fhour = fhour
      call sigio_swohdc(nsigo,trim(filenameout),sighead,sigdata,iret)
      print *,trim(filenameout)
      print *,iret

      STOP
      END
