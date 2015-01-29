!< --- next few lines under version control, D O  N O T  E D I T --->
! $Date$
! $Revision$
! $Author$
! $Id$
!<------------------------------------------------------------------>

program gribmean

! compute ensemble mean and spread from grib files.
! Jeff Whitaker <jeffrey.s.whitaker@noaa.gov> 20080509
! Rahul Mahajan <rahul.mahajan@noaa.gov> generalized 2014102500

implicit none

integer, parameter :: lunin=11,lunout=12
integer :: iret,iargc,i,j
integer :: nlons,nlats,nmsg,nbyte,ndata,nanals,nanal,nrecs,kskp
integer,   dimension(200)                :: kpds,kgds,kens,jpds,jgds
integer,   dimension(:,:),   allocatable :: kpdsa,kgdsa
real,      dimension(:,:),   allocatable :: grid
real(8),   dimension(:,:,:), allocatable :: data,datamean,dataspread,delta
logical*1, dimension(:,:),   allocatable :: lbms
logical*1, dimension(:,:,:), allocatable :: bitmaps,bitmaptot
character(len=3)   :: charnanals,nanalstr
character(len=500) :: ingrib,outgrib,datapath,filepref

! get command line args
call getarg(1, datapath)
call getarg(2, filepref)
call getarg(3, charnanals)
if (iargc() < 3) then
  write(6,'(a)') "USAGE: ./gribmean.x datapath filepref nanals"
  stop
endif
read(charnanals,'(i3)') nanals

! read the first member, just to find how many records there are (and their dimensions).
nanal = 1
write(nanalstr,'(i3.3)') nanal
ingrib = trim(adjustl(datapath)) // '/' // trim(adjustl(filepref)) // '_mem' // trim(adjustl(nanalstr))
call baopenr(lunin, trim(ingrib), iret)
jgds=-1; jpds=-1
kskp = 0; iret = 0
call getgbh(lunin,-kskp,-kskp,jpds,jgds,NBYTE,NDATA,KSKP,KPDS,KGDS,IRET)
nlons = kgds(2)
nlats = kgds(3)
write(6,'("nlons, nlats =",2(1x,i6))') nlons,nlats
if (nlons <= 0 .or. nlats <= 0) then
   write(6,'(a)') 'nlons,nlats not defined'
   stop
endif
allocate(grid(nlons,nlats))
allocate(lbms(nlons,nlats))
jgds=-1; jpds=-1
kskp = 0; nmsg = 0; iret = 0
do while (iret == 0) 
 call getgb(lunin,-kskp,nlons*nlats,nmsg,jpds,jgds,NDATA,KSKP,KPDS,KGDS,LBMS,GRID,IRET)
 if (iret == 0) then
    print *,kskp,ndata,minval(grid),maxval(grid),count(.not. lbms),iret
    nmsg = nmsg + 1
 end if
enddo
call baclose(lunin, iret)

nrecs = nmsg
! allocate arrays for all pds,gds,data,bitmaps, mean and spread.
allocate(data(nlons,nlats,nrecs))
allocate(delta(nlons,nlats,nrecs))
allocate(bitmaps(nlons,nlats,nrecs))
allocate(bitmaptot(nlons,nlats,nrecs))
allocate(datamean(nlons,nlats,nrecs))
allocate(dataspread(nlons,nlats,nrecs))
allocate(kpdsa(200,nrecs))
allocate(kgdsa(200,nrecs))

! initialize bitmaps, mean and spread
bitmaptot = .true.
datamean = 0.
dataspread = 0.

! loop over each ensemble member
! for the first time, accumulate quantities used to compute mean and variance.
do nanal=1,nanals

    ! open file
    write(nanalstr,'(i3.3)') nanal
    ingrib = trim(adjustl(datapath)) // '/' // trim(adjustl(filepref)) // '_mem' // trim(adjustl(nanalstr))
    call baopenr(lunin+nanal, trim(adjustl(ingrib)), iret)
    write(6,'(a,1x,i5)') trim(adjustl(ingrib)),iret

    ! read all data, bitmaps, pds and gds
    jgds=-1; jpds=-1
    kskp = 0; nmsg = 0; iret = 0
    do while (iret == 0) 
       call getgb(lunin+nanal,-kskp,nlons*nlats,nmsg,jpds,jgds,NDATA,KSKP,KPDS,KGDS,LBMS,GRID,IRET)
       if (iret /= 0 .and. nmsg+1 < nrecs) then
          write(6,'(2(a,1x),i6)') 'failure reading',trim(adjustl(ingrib)),nmsg+1
          stop
       end if
       if (iret == 0) then
          nmsg = nmsg + 1
          data(:,:,nmsg) = grid
          bitmaps(:,:,nmsg) = lbms
          if (nanal == 1) then
             kpdsa(:,nmsg) = kpds
             kgdsa(:,nmsg) = kgds
          end if
       end if
    enddo

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

dataspread = sqrt(dataspread/float(nanals-1))
write(6,'("min/max mean   =",2(1x,f12.6))') minval(datamean),  maxval(datamean)
write(6,'("min/max spread =",2(1x,f12.6))') minval(dataspread),maxval(dataspread)

outgrib = trim(adjustl(datapath)) // '/' // trim(adjustl(filepref)) // '_ensmean'
call baopenwt(lunout, trim(adjustl(outgrib)), iret)
kens(1) = 1; kens(2) = 5; kens(3) = 0; kens(5) = 255
kens(4) = 1
do nmsg=1,nrecs
   grid = datamean(:,:,nmsg)
   call putgbe(lunout,nlons*nlats,KPDSA(:,nmsg),KGDSA(:,nmsg),KENS,bitmaptot(:,:,nmsg),grid,IRET)
   write(6,'(i6,2(1x,f12.6))') nmsg,minval(datamean(:,:,nmsg)),maxval(datamean(:,:,nmsg))
enddo
call baclose(lunout, iret)
write(6,'(a)') trim(adjustl(outgrib))

outgrib = trim(adjustl(datapath)) // '/' // trim(adjustl(filepref)) // '_enssprd'
call baopenwt(lunout+nanal, trim(adjustl(outgrib)), iret)
kens(4) = 11
! increase decimal scale factor by two powers of 10.
kpdsa(22,:) = kpdsa(22,:)+2
do nmsg=1,nrecs
   grid = dataspread(:,:,nmsg)
   call putgbe(lunout+nanal,nlons*nlats,KPDSA(:,nmsg),KGDSA(:,nmsg),KENS,bitmaptot(:,:,nmsg),grid,IRET)
   write(6,'(i6,2(1x,f12.6))') nmsg,minval(datamean(:,:,nmsg)),maxval(datamean(:,:,nmsg))
enddo
call baclose(lunout+nanal, iret)
write(6,'(a)') trim(adjustl(outgrib))

end program gribmean
