! the program read temeprature files
  subroutine grads_sfctime(fileo,ifileo,nobs,nreal,nreal2,nlev,plev,iscater,igrads,isubtype,subtype)
     implicit none
  
  real(4),allocatable,dimension(:,:)  :: rdiag
  character(8),allocatable,dimension(:) :: cdiag
  real(4),dimension(nlev) :: plev
  integer,dimension(nlev) :: ndata
  real(4),dimension(nreal2,10000,nlev) :: tobs
  real(4),dimension(10000) :: rlat,rlon
  character(8),dimension(10000) :: stid
  character(8) :: stidend
  character(ifileo) :: fileo
  character(2) :: subtype 
  character(30) :: files,filein,filegrads
  integer :: nobs,nreal,nlfag,nflag0,nlev,nlev0,gettim,iscater,igrads
  real(4) :: bmiss,rtim,xlat0,xlon0,rtime

  integer(4):: ittype,ituse,ntumgrp,ntgroup,ntmiter,isubtype
  real(4) :: ttwind,gtross,etrmax,etrmin,vtar_b,vtar_pg
  integer nt,ifileo,k,i,ii,j,nflag,nreal2
  integer nlat,nlon,npres,ntime,nweight,ndup
  integer ilat,ilon,ipres,itime,iweight

  data bmiss/-999.0/

  tobs=bmiss 
  stid='        '
  ndata=0
  
  print *,'fileo=',fileo
  print *,'nobs=',nobs
  allocate(rdiag(nreal,nobs),cdiag(nobs))


!  write(subtype,'(i2)') isubtype
 filein=trim(fileo)//'_'//trim(subtype)//'.tmp'
  open(11,file=filein,form='unformatted') !  open input file
  rewind(11)
  do i=1,nobs
    read(11) cdiag(i),rdiag(1:nreal,i)
  enddo


  if(iscater ==1) then
   files=trim(fileo)//'_'//trim(subtype)//'.scater'
   open(51,file=files,form='unformatted')
   write(51) nobs,nreal
   write(51) rdiag
   close(51)
  endif

if( igrads ==1 )  then 

   filegrads=trim(fileo)//'_'//trim(subtype)//'_grads'
  open(21,file=filegrads,form='unformatted',status='new')     !  open output file 

  print *, rdiag(1,1),rdiag(2,1),rdiag(3,1),rdiag(4,1),rdiag(5,1),&
           rdiag(6,1),rdiag(7,1),rdiag(8,1),rdiag(9,1),rdiag(10,1)

  print *, rdiag(1,100),rdiag(2,100),rdiag(3,100),rdiag(4,100),rdiag(5,100),&
           rdiag(6,100),rdiag(7,100),rdiag(8,100),rdiag(9,100),rdiag(10,100)

  ilat=1                           ! the position of lat
  ilon=2                           ! the position of lon
  ipres=4                          ! the position of pressure
  itime=6                          ! the position of relative time
  iweight=11                       ! the position of weight 
! check wether data have duplicate

!  call hash(rdiag,nobs,nreal,nlat,nlon,npres,ntime,nweight,ndup)
  call hash(rdiag,nobs,nreal,ilat,ilon,ipres,itime,iweight,ndup)
!  call hash(rdiag,nobs,nreal,ilat,ilon,ipres,itime,iweight,ndup)

     ii=0
     do  i=1,nobs
        if(rdiag(iweight,i) >0.0 ) then
            rtime=rdiag(itime,i)
            ii=ii+1
            stid(ii)=trim(cdiag(i))
            rlat(ii)=rdiag(ilat,i)
            rlon(ii)=rdiag(ilon,i)
            k=gettim(rtime,plev,nlev)
            if(k /=0) then
               tobs(1:nreal2,ii,k)=rdiag(3:nreal,i) 
               ndata(k)=ndata(k)+1
            endif
            do j=i+1,nobs
               if( cdiag(j) == stid(ii) .and. rdiag(ilat,i) == rdiag(ilat,j) &
                   .and. rdiag(ilon,i)  == rdiag(ilon,j) .and. rdiag(iweight,j) >0.0 ) then
                   rtime=rdiag(itime,j)
                   k=gettim(rtime,plev,nlev)
                   if(k /=0) then
                      tobs(1:nreal2,ii,k)=rdiag(3:nreal,j) 
                      rdiag(iweight,j)=-rdiag(iweight,j)
                      ndata(k)=ndata(k)+1
                   endif
               endif
           enddo
         endif 

      enddo
   
     print *,'ii=',ii

!  write out into grads file
            
    do k=1,nlev
      do i=1,ii
        nflag=1
        rtim=0.0
        nlev0=1
        write(21) stid(i),rlat(i),rlon(i),rtim,nlev0,nflag
        write(21) (tobs(j,i,k),j=1,nreal2)
      enddo
        nflag=1
        rtim=0.0
        nlev0=0
        write(21) stid(i),rlat(i),rlon(i),rtim,nlev0,nflag
    enddo

   nflag0=0
   xlat0=0.0
   xlon0=0.0
   nlev0=0

!  the end of file
     stidend='        '
     write(21) stidend,xlat0,xlon0,rtim,nlev0,nflag0 
        
  close(21)

   nt=maxloc(ndata,dim=1)

   print *,nt

endif
  deallocate(rdiag,cdiag)


  return 
  end

  function gettim(p1,plev,nlevs)
  
  real*4 p1
  real*4,dimension(nlevs) :: plev
  integer gettim

  do i=1,nlevs
    gettim=0
    if(p1 <= plev(i)) then
      gettim=i
!     print *, p1,plev(i),i,gettim
      return 
    endif
  enddo

 return 
 end

  
