! the program read temeprature files
  subroutine grads_mandlev(fileo,ifileo,nobs,nreal,nreal2,nlev,plev,iscater,igrads,isubtype,subtype)
     implicit none
  
 real(4),allocatable,dimension(:,:)  :: rdiag
 character(8),allocatable,dimension(:) :: cdiag
  
   real(4),dimension(nreal) :: rdummy
  character(8) cdummy 
  real(4),dimension(nlev) :: plev
!  real(4),dimension(nreal2,5000,nlev) :: tobs
!  real(4),dimension(5000) :: rlat,rlon
  real(4) rlat,rlon
  character(2) subtype 
  character(8) stid
  character(ifileo) :: fileo 
  character(30)  :: files,filegrads 
  character(8)  :: stidend
  integer nobs,nreal,nlfag,nflag0,nlev,nlev0,getlev,iscater,igrads,nflg0
  real*4 bmiss,rtim,xlat0,xlon0
  character(30) filein

  integer nreal2,ifileo,i,j,ii,rp,k
  integer ilat,ilon,ipres,itime,iweight,ndup,tobs
  integer(4):: ittype,ituse,ntumgrp,ntgroup,ntmiter,isubtype
  real(4) :: ttwind,gtross,etrmax,etrmin,vtar_b,vtar_pg
 

  data bmiss/-999.0/

  tobs=bmiss 
  stid='        '
  nflag0=0
  rtim=0.0
  nflg0=0
  xlat0=0.0
  xlon0=0.0
  nlev0=0

 print *, fileo
 print *, 'plev ',plev(1),plev(2),plev(21)
 print *,'nobs=',nobs
 print *,'nlev',nlev

 allocate(rdiag(nreal,nobs),cdiag(nobs))

!  write(subtype,'(i2)') isubtype
 filein=trim(fileo)//'_'//trim(subtype)//'.tmp'
  
 open(11,file=filein,form='unformatted')
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


if (igrads ==1) then 
filegrads=trim(fileo)//'_'//trim(subtype)//'_grads'

 open(21,file=filegrads,form='unformatted')

    print *,cdiag(5),rdiag(1,5),rdiag(2,5),rdiag(3,5),rdiag(4,5),rdiag(6,5),rdiag(7,5)
    print *,cdiag(10),rdiag(1,10),rdiag(2,10),rdiag(3,10),rdiag(4,10),rdiag(6,10),rdiag(7,10)





  ilat=1                           ! the position of lat
  ilon=2                           ! the position of lon
  ipres=4                          ! the position of pressure
  itime=6                          ! the position of relative time
  iweight=11                       ! the position of weight 
! check wether data have duplicate

  call hash(rdiag,nobs,nreal,ilat,ilon,ipres,itime,iweight,ndup)

     ii=0
     k=0
     do  i=1,nobs
       if(rdiag(iweight,i) >0.0 ) then
          stid=cdiag(i)
          rlat=rdiag(ilat,i)
          rlon=rdiag(ilon,i)
          rp=rdiag(ipres,i)
          k=getlev(rp,plev,nlev)
          if(k /=0)  then
              write(21) stid,rlat,rlon,rtim,1,0
              write(21) plev(k),(rdiag(j,i),j=3,nreal)
           endif   
       endif
      enddo
   
     stidend='        '
     write(21) stidend,xlat0,xlon0,rtim,nlev0,nflag0 

  close(21)
endif

  deallocate(rdiag,cdiag)

  return 
  end

  function getlev(p1,plev,nlevs)
  
  real*4 p1
  real*4,dimension(nlevs) :: plev
  integer getlev,ip,np
 
!  print *,'nlevs=',nlevs
  ip=int(p1)
  
!  print *, 'ip=',ip
  getlev = 0
  do i=1,nlevs
    getlev=0
    np=int(plev(i))
!    print *,'function,np,ip ',np,ip
    if(ip == np) then
       getlev=i
!      print *, 'function getlev ',ip,plev(i),i,getlev
      return
    endif
  enddo
  return
 end

  
