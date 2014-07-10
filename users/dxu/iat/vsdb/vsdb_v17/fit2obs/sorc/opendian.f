!----------------------------------------------------------------------
!----------------------------------------------------------------------
      subroutine opendian(iunit,file,iostat)
      character*(*) file
      character*15  convert,nativey,nativen,be,le
      character*1   byte                    
      integer ibyte,jbyte,iswap
      equivalence(byte,ibyte)
      logical exist
      data le/'little_endian'/
      data be/'big_endian   '/
!----------------------------------------------------------------------
!----------------------------------------------------------------------
! what is native?
      ibyte=1; convert='none'
      if(ichar(byte)==0) then 
         nativey=be;nativen=le
      else
         nativey=le;nativen=be
      endif
      !print*,'running on ',nativey,' platform'
      iostat=-1

! try to open the file natively, if yes, report, rewind, return

      inquire(file=file,exist=exist); if(.not.exist)goto 99

      open(iunit,file=file,form='unformatted')
      read(iunit,iostat=iostat) ibyte
      if(iostat==0)then ! file is opened as native
         print*,nativey,trim(file)
         rewind(iunit)
         return
      endif

! try to open the file non-natively, if yes, report, rewind, return

      close(iunit)
      open(iunit,file=file,form='unformatted',convert=nativen) 
      read(iunit,iostat=iostat) ibyte
      if(iostat==0)then ! file is opened as native
         print*,nativen,trim(file)
         rewind(iunit)
         return
      endif

99    print*,'unable to open ',trim(file)
      return
      end
