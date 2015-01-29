program gribmean
! compute ensemble mean and spread grib files.
! Jeff Whitaker <jeffrey.s.whitaker@noaa.gov> 20080509
implicit none
character(len=500) ingrib,outgrib,datapath
integer lunin, nmsg, iret, nlons, nlats, ndata, kskp, kpds(200), kgds(200), &
        kens(200),jpds(200),jgds(200),nanals,nrecs,nanal,lunout,iargc,i,j
real, allocatable, dimension(:,:) :: grid
integer, allocatable, dimension(:,:) :: kpdsa,kgdsa
! accumulators for one-pass mean and variance are double precision.
real(8), allocatable, dimension(:,:,:) :: data, datamean, dataspread, delta
logical*1, allocatable, dimension(:,:) :: lbms
logical*1, allocatable, dimension(:,:,:) :: bitmaps,bitmaptot
character(len=3) char3
character(len=10) filetype
character(len=10) gribdate
character(len=4) gribtype,charnlons,charnlats
character(len=3) fhr
character(len=3) nanalstr

! unit number for input grib files.
lunin = 11
! unit number for output grib files.
lunout = 12
! get command line args (same as gribmean.py)
call getarg(1, datapath)
call getarg(2, char3)
call getarg(3, gribdate)
call getarg(4, gribtype)
call getarg(5, filetype)
if (iargc() .lt. 5) then
  print *, "gribmean.x datapath nanals gribdate gribtype filetype fhr"
  print *,"gribtype is p or sflx"
  print *,"filetype is fg or anl"
  stop
endif
if (iargc() .gt. 5) then
    call getarg(6, fhr)
else
    fhr=''
endif
!if (trim(filetype) .ne. 'fg' .and. trim(filetype) .ne. 'anl') then
!   print *,"filetype (5th arg) must be 'fg' or 'anl' ",filetype
!   stop
!endif
read(char3,'(i3)') nanals
! get nlons,nlats from environment.
if (trim(gribtype) .eq. 'p') then
   call getenv("IO",charnlons)
   call getenv("JO",charnlats)
else if (trim(gribtype) .eq. 'sflx') then
   call getenv("LONB",charnlons)
   call getenv("LATB",charnlats)
endif
read(charnlons,'(i4)') nlons
read(charnlats,'(i4)') nlats
print *,'nlons, nlats = ',nlons,nlats,charnlons,charnlats
if (nlons <= 0 .or. nlats <= 0) then
   print *,'nlons,nlats not defined'
   stop
endif

! read the first member, just to find how many records there are.
nanal = 1
write(nanalstr,'(i3.3)') nanal
if (fhr .ne. '') then
if (trim(gribtype) .eq. 'p') then
   ingrib = trim(adjustl(datapath))//'/'//trim(adjustl(gribtype))//'grb'//trim(adjustl(filetype))//'_'//trim(adjustl(gribdate))//'_fhr'//trim(fhr)//'_mem'//trim(adjustl(nanalstr))
else
   ingrib = trim(adjustl(datapath))//'/'//trim(adjustl(gribtype))//'grb_'//trim(adjustl(gribdate))//'_fhr'//trim(fhr)//'_mem'//trim(adjustl(nanalstr))
endif
else
   ingrib = trim(adjustl(datapath))//'/'//trim(adjustl(gribtype))//'grb'//trim(adjustl(filetype))//'_'//trim(adjustl(gribdate))//'_mem'//trim(adjustl(nanalstr))
end if
call baopenr(lunin, trim(ingrib), iret)
!print *,trim(adjustl(ingrib)),iret
allocate(grid(nlons,nlats))
allocate(lbms(nlons,nlats))
jgds=-1; jpds=-1
kskp = 0; nmsg = 0; iret = 0
do while (iret .eq. 0) 
 CALL GETGB(LUNIN,-kskp,nlons*nlats,nmsg,jpds,jgds,NDATA,KSKP,KPDS,KGDS,LBMS,GRID,IRET)
 if (iret .eq. 0) then
    print *,kskp,ndata,minval(grid),maxval(grid),count(.not. lbms),iret
    nmsg = nmsg + 1
 end if
enddo
call baclose(lunin, iret)

!print *,nmsg,'messages'
nrecs = nmsg
! allocate arrays for all pds,gds,data,bitmaps, mean and spread.
allocate(data(nlons,nlats,nrecs))
allocate(delta(nlons,nlats,nrecs))
allocate(bitmaps(nlons,nlats,nrecs))
allocate(bitmaptot(nlons,nlats,nrecs))
bitmaptot = .true.
allocate(datamean(nlons,nlats,nrecs))
datamean = 0.
allocate(dataspread(nlons,nlats,nrecs))
dataspread = 0.
allocate(kpdsa(200,nrecs))
allocate(kgdsa(200,nrecs))
! loop over each ensemble member
! for the first time, accumulate quantities used to compute mean and variance.

do nanal=1,nanals
    ! open file
    write(nanalstr,'(i3.3)') nanal
    if (fhr .ne. '') then
       if (trim(gribtype) .eq. 'p') then
       ingrib = trim(adjustl(datapath))//'/'//trim(adjustl(gribtype))//'grb'//trim(adjustl(filetype))//'_'//trim(adjustl(gribdate))//'_fhr'//trim(fhr)//'_mem'//trim(adjustl(nanalstr))
       else
       ingrib = trim(adjustl(datapath))//'/'//trim(adjustl(gribtype))//'grb_'//trim(adjustl(gribdate))//'_fhr'//trim(fhr)//'_mem'//trim(adjustl(nanalstr))
       endif
    else
       ingrib = trim(adjustl(datapath))//'/'//trim(adjustl(gribtype))//'grb'//trim(adjustl(filetype))//'_'//trim(adjustl(gribdate))//'_mem'//trim(adjustl(nanalstr))
    end if
    call baopenr(lunin+nanal, trim(ingrib), iret)
    print *,trim(adjustl(ingrib)),iret
    ! read all data, bitmaps, pds and gds
    jgds=-1; jpds=-1
    kskp = 0; nmsg = 0; iret = 0
    do while (iret .eq. 0) 
       CALL GETGB(LUNIN+nanal,-kskp,nlons*nlats,nmsg,jpds,jgds,NDATA,KSKP,KPDS,KGDS,LBMS,GRID,IRET)
       !print *,iret,nmsg
       if (iret .ne. 0 .and. nmsg+1 .lt. nrecs) then
          print *,'failure reading',trim(adjustl(ingrib)),nmsg+1
          stop
       end if
       if (iret .eq. 0) then
          nmsg = nmsg + 1
          data(:,:,nmsg) = grid
          !if (nmsg .eq. nrecs) print *,nanal,nmsg,minval(grid),maxval(grid)
          bitmaps(:,:,nmsg) = lbms
          if (nanal .eq. 1) then
             kpdsa(:,nmsg) = kpds
             kgdsa(:,nmsg) = kgds
          end if
       end if
    enddo
    !print *,nmsg,nrecs
    ! close file.
    call baclose(lunin+nanal, iret)
!   recursive calculation of mean and variance.
!   Algorithm III (On-line Algorithm) from
!   http://en.wikipedia.org/wiki/Algorithms_for_calculating_variance
!   (due to Knuth "The Art of Scientific Computing", 3rd Ed, Vol 2, p232.
    delta = data - datamean
    datamean = datamean + delta/float(nanal)
    dataspread = dataspread + delta*(data - datamean)
    ! mask all points that are ever masked.
    do nmsg=1,nrecs
    do j=1,nlats
    do i=1,nlons
       if (.not. bitmaps(i,j,nmsg)) bitmaptot(i,j,nmsg) = .false.
    enddo
    enddo
    enddo
enddo       

dataspread = dataspread/float(nanals-1)

dataspread = sqrt(dataspread)
print *,'min/max mean = ',minval(datamean),maxval(datamean)
print *,'min/max spread = ',minval(dataspread),maxval(dataspread)

if (fhr .ne. '') then
   outgrib = trim(adjustl(datapath))//'/'//trim(adjustl(gribtype))//'grbensmean'//trim(adjustl(filetype))//'_'//trim(adjustl(gribdate))//'_fhr'//trim(fhr)
else
   outgrib = trim(adjustl(datapath))//'/'//trim(adjustl(gribtype))//'grbensmean'//trim(adjustl(filetype))//'_'//trim(adjustl(gribdate))
end if
call baopenwt(lunout, trim(outgrib), iret)
kens(1) = 1; kens(2) = 5; kens(3) = 0; kens(5) = 255
kens(4) = 1
do nmsg=1,nrecs
   grid = datamean(:,:,nmsg)
   CALL PUTGBE(lunout,nlons*nlats,KPDSA(:,nmsg),KGDSA(:,nmsg),KENS,bitmaptot(:,:,nmsg),grid,IRET)
   print *,nmsg,minval(datamean(:,:,nmsg)),maxval(datamean(:,:,nmsg))
enddo
call baclose(lunout, iret)
print *,outgrib

if (fhr .ne. '') then
   outgrib = trim(adjustl(datapath))//'/'//trim(adjustl(gribtype))//'grbenssprd'//trim(adjustl(filetype))//'_'//trim(adjustl(gribdate))//'_fhr'//trim(fhr)
else
   outgrib = trim(adjustl(datapath))//'/'//trim(adjustl(gribtype))//'grbenssprd'//trim(adjustl(filetype))//'_'//trim(adjustl(gribdate))
end if
call baopenwt(lunout+nanal, trim(outgrib), iret)
kens(4) = 11
! increase decimal scale factor by two powers of 10.
kpdsa(22,:) = kpdsa(22,:)+2
do nmsg=1,nrecs
   grid = dataspread(:,:,nmsg)
   CALL PUTGBE(lunout+nanal,nlons*nlats,KPDSA(:,nmsg),KGDSA(:,nmsg),KENS,bitmaptot(:,:,nmsg),grid,IRET)
   print *,nmsg,minval(dataspread(:,:,nmsg)),maxval(dataspread(:,:,nmsg))
enddo
call baclose(lunout+nanal, iret)
print *,outgrib

end program gribmean
