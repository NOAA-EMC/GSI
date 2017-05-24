    program satinfo_appnd
!
! abstract:  add a column of varch_cld to satinfo file
!
! program history log:
!   2011-06-08 Yanqiu Zhu -- extracted out from radinfo.f90

    use kinds, only: r_kind,i_kind
    implicit none

    integer(i_kind) jpch_rad      ! number of channels*sat
    real(r_kind),allocatable,dimension(:):: varch       ! variance for clear radiance
    real(r_kind),allocatable,dimension(:):: varch_cld   ! variance for cloudy radiance
    real(r_kind),allocatable,dimension(:):: ermax_rad   ! error maximum (qc)
    real(r_kind),allocatable,dimension(:):: b_rad       ! variational b value
    real(r_kind),allocatable,dimension(:):: pg_rad      ! variational pg value
    integer(i_kind),allocatable,dimension(:):: icld_det
    integer(i_kind),allocatable,dimension(:):: icloud
    integer(i_kind),allocatable,dimension(:):: iaerosol
    integer(i_kind),allocatable,dimension(:):: nuchan    ! satellite channel
    integer(i_kind),allocatable,dimension(:):: iuse_rad  ! use to turn off satellite radiance data
    integer(i_kind),allocatable,dimension(:):: ifactq    ! scaling parameter for d(Tb)/dq sensitivity
    character(len=20),allocatable,dimension(:):: nusis   ! sensor/instrument/satellite indicator

    integer(i_kind) i,j,k,ich,lunin,lunout,nlines
    integer(i_kind) ip,istat,n,ichan
    character(len=1):: cflg
    character(len=120) crecord
    character(len=20) :: isis
    character(len=20) :: satscan_sis
    character(len=20),allocatable,dimension(:):: satsenlist
    logical,allocatable,dimension(:):: nfound

    data lunin / 49 /
    data lunout / 51 /


    open(lunin,file='/scratch4/NCEPDEV/da/save/Yanqiu.Zhu/cloudy_radiance/fix/global_satinfo_clrsky.txt',form='formatted')
    open(lunout,file='global_satinfo_clrsky.txt.new',form='formatted')
    j=0
    nlines=0
    read1:  do
       read(lunin,100,iostat=istat) cflg,crecord
       if (istat /= 0) exit
       nlines=nlines+1
       if (cflg == '!') cycle
       j=j+1
    end do read1
    if (istat>0) then
       close(lunin)
       write(6,*)'RADINFO_READ:  ***ERROR*** error reading radinfo, istat=',istat
       write(6,*)'RADINFO_READ:  stop program execution'
       stop
    endif
    jpch_rad = j

!   Allocate arrays to hold radiance information
!     nuchan    - channel number
!     nusis     - sensor/instrument/satellite
!     iuse_rad  - use parameter
!     ifactq    - scaling parameter for d(Tb)/dq sensitivity
!     varch     - variance for each channel

    allocate(nuchan(jpch_rad),nusis(jpch_rad),iuse_rad(0:jpch_rad), &
         ifactq(jpch_rad),varch(jpch_rad),varch_cld(jpch_rad), &
         ermax_rad(jpch_rad),b_rad(jpch_rad),pg_rad(jpch_rad), &
         icld_det(jpch_rad),icloud(jpch_rad),iaerosol(jpch_rad))
    allocate(satsenlist(jpch_rad),nfound(jpch_rad))
    iuse_rad(0)=-999
    ifactq=0

    crecord='sensor/instr/sat      chan iuse  error  error_cld  ermax   var_b    var_pg  icld_det icloud iaerosol'
    write(lunout,'(a1,a100)') '!', trim(crecord)

    rewind(lunin)
    j=0
    do k=1,nlines
       read(lunin,100) cflg,crecord
       if (cflg == '!') cycle
       j=j+1
       read(crecord,*) nusis(j),nuchan(j),iuse_rad(j),&
            varch(j),varch_cld(j),ermax_rad(j),b_rad(j),pg_rad(j),icld_det(j)

       icloud(j)=-1 
!      if (index(trim(nusis(j)),'amsua')/=0) icloud(j)=1
       iaerosol(j)=-1

       write(lunout,110) nusis(j),nuchan(j),iuse_rad(j),varch(j),varch_cld(j), &
              ermax_rad(j),b_rad(j),pg_rad(j),icld_det(j),icloud(j),iaerosol(j)
    end do
    close(lunin)
    close(lunout)
100 format(a1,a120)
110 format(1x,a20,i5,i4,f9.3,f9.3,f9.3,f9.3,f9.3,i7,i7,i7)

    deallocate(nuchan,nusis,iuse_rad,ifactq,varch,varch_cld, &
         ermax_rad,b_rad,pg_rad,icld_det,icloud,iaerosol)

    end program satinfo_appnd
