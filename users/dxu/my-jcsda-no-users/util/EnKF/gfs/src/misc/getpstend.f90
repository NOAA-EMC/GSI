      program getpstend
      USE SIGIO_MODULE
      implicit none
      TYPE(SIGIO_HEAD) :: SIGHEADI
      TYPE(SIGIO_DATA) :: SIGDATAI
      character*500 filenamesigin,datapath
      character*10 datestring,datestringm1
      character*2 charfhr
      character*10 charnanal
      integer nsigi,iret,nlats,nlons, &
              jcap,ngrd,i,j,nhr
      real sumdiff,sumcoslat
      real,allocatable,dimension(:,:,:) :: psg
      real,allocatable,dimension(:,:) :: psgdiff
      real,allocatable,dimension(:) :: gaulats, gauwts
      NSIGI=21
      call getarg(1,datapath)
      call getarg(2,datestring)
      call getarg(3,datestringm1)
      call getarg(4,charnanal)
      do nhr=0,9
         write(charfhr,'(i2.2)') nhr
           filenamesigin = trim(adjustl(datapath))// &
           '/'//datestring//'/'// &
      'sfg_'//datestring//'_fhr'//charfhr//'_'//trim(adjustl(charnanal))
         !print *,filenamesigin
         call sigio_srohdc(nsigi,filenamesigin,sigheadi,sigdatai,iret)
         if (iret .ne. 0) then
            print *,'error opening',trim(filenamesigin)
            stop
         end if
         if (nhr .eq. 0) then
            jcap = sigheadi%jcap
            nlats = sigheadi%latf
            nlons = sigheadi%lonf
            ngrd = sigheadi%nxgr
!           print *,'jcap,nlons,nlats,nlevs = ', &
!           jcap,nlons,nlats,sigheadi%levs
            allocate(psg(nlons,nlats,0:9))
            allocate(gaulats(nlats))
            allocate(gauwts(nlats))
            allocate(psgdiff(nlons,nlats))
            call splat(4,nlats,gaulats,gauwts)
         end if
         call sptez(0,jcap,4,nlons,nlats,sigdatai%ps,psg(:,:,nhr),1)
         psg(:,:,nhr) = 10.*exp(psg(:,:,nhr))
         call sigio_axdata(sigdatai,iret)
!        print *,nhr,minval(psg(:,:,nhr)),maxval(psg(:,:,nhr))
      enddo
      do nhr=1,9
         psgdiff = (psg(:,:,nhr)-psg(:,:,nhr-1))**2 ! first diff
      !do nhr=1,8
      !  second diff
      !  psgdiff = (psg(:,:,nhr+1)-2.*psg(:,:,nhr)+psg(:,:,nhr-1))**2 
         sumdiff = 0.
         sumcoslat = 0.
         do j=1,nlats
         do i=1,nlons
            sumdiff = sumdiff + psgdiff(i,j)*gauwts(j)
            sumcoslat = sumcoslat + gauwts(j)
         enddo
         enddo
         print *,'nhr,rms global mean pstend =', &
         nhr,sqrt(sumdiff/sumcoslat)
      enddo
      STOP
      END
