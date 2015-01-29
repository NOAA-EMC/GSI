! the program read temeprature files
  subroutine grads_sig(fileo,ifileo,nobs,nreal,nreal2,nlev,plev,iscater,igrads,isubtype,subtype)
     implicit none
 
  integer nreal2,ifileo 
  real(4),allocatable,dimension(:,:)  :: rdiag
  character(8),allocatable,dimension(:) :: cdiag
  real(4),dimension(nlev) :: plev
  real(4),dimension(nreal2,5000,nlev) :: tobs
!  real(4),dimension(5000) :: rlat,rlon
  character(2) subtype 
  real(4) :: rlat,rlon
  character(8) :: stidend,stdid
  character(ifileo) :: fileo
  character(30) :: files,filein,filegrads
  integer :: nobs,nreal,nlfag,nflag0,nlev,nlev0,getpro,iscater,igrads
  real(4) :: bmiss,rtim,xlat0,xlon0
  integer(4):: ittype,ituse,ntumgrp,ntgroup,ntmiter,isubtype
  real(4) :: ttwind,gtross,etrmax,etrmin,vtar_b,vtar_pg
  integer i,j,k,ii,ilat,ilon,ipres,itime,iweight,ndup,nflg0,nflag
 
  data bmiss/-999.0/

  tobs=bmiss 
  stdid='        '
  nflag0=0
  rtim=0.0
  nflg0=0
  xlat0=0.0
  xlon0=0.0
  nlev0=0
  
  print *, 'nobs=',nobs
  print *, 'fileo=',fileo

   allocate(rdiag(nreal,nobs),cdiag(nobs))

!write(subtype,'(i2)') isubtype
 filein=trim(fileo)//'_'//trim(subtype)//'.tmp'
  open(11,file=filein,form='unformatted')
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


if (igrads ==1)  then 
filegrads=trim(fileo)//'_'//trim(subtype)//'_grads'
   open(21,file=filegrads,form='unformatted',status='new')    ! open output file

  print *, rdiag(1,1),rdiag(2,1),rdiag(3,1),rdiag(4,1),rdiag(5,1),&
           rdiag(6,1),rdiag(7,1),rdiag(8,1),rdiag(9,1),rdiag(10,1)

  print *, rdiag(1,100),rdiag(2,100),rdiag(3,100),rdiag(4,100),rdiag(5,100),&
           rdiag(6,100),rdiag(7,100),rdiag(8,100),rdiag(9,100),rdiag(10,100)

  ilat=1                           ! the position of lat
  ilon=2                           ! the position of lon
  ipres=4                          ! the position of pressure
  itime=6                          ! the position of relative time
  iweight=11                        ! the position of weight 
! check wether data have duplicate

  call hash(rdiag,nobs,nreal,ilat,ilon,ipres,itime,iweight,ndup)

     ii=0
     do  i=1,nobs
          if(rdiag(iweight,i) >0.0 ) then
            stdid=cdiag(i)
            rlat=rdiag(ilat,i)
            rlon=rdiag(ilon,i)
            k=getpro(rdiag(ipres,i),plev,nlev)
            if(k /=0) then
              write(21) stdid,rlat,rlon,rtim,1,nflag
              write(21) plev(k),rdiag(3:nreal,i)
            endif 
          endif
      enddo
   

!  the end of file
     stidend='        '
     write(21) stidend,xlat0,xlon0,rtim,nlev0,nflag0 
        
   close(21)

endif
  deallocate(rdiag,cdiag)
  return 
  end

  function getpro(p1,plev,nlevs)
  
  real*4 p1
  real*4,dimension(nlevs) :: plev
  integer getpro,ip,np,dp
 
  ip=int(p1)

  do i=1,nlevs
    getpro=0
    np=int(plev(i))
    
   if(ip >= np) then
     dp=abs(ip-np)
     if(dp <=5) getpro=i
     if (getpro ==0 .and. i>1 ) then 
       dp2=abs(ip-int(plev(i-1)))   
       if(dpi2 <=5) getpro=i-1
     endif
!      print *, ip,plev(i),i,getpro
      return 
   endif
  enddo

 return 
 end

  
