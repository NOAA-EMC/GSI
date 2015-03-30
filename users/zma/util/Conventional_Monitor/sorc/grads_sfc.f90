! the program read temeprature files
  subroutine grads_sfc(fileo,ifileo,nobs,nreal,nreal2,iscater,igrads,isubtype,subtype)
     implicit none
 
  integer ifileo 
  real(4),allocatable,dimension(:,:)  :: rdiag
  character(8),allocatable,dimension(:) :: cdiag
  character(8) :: stid
  character(ifileo) :: fileo
  character(30) :: files,filein,filegrads
  character(2) :: subtype
  integer nobs,nreal,nlfag,nflg0,nlev,nlev0,iscater,igrads
  real(4) bmiss,rtim,xlat0,xlon0,rlat,rlon
 
  integer(4):: ittype,ituse,ntumgrp,ntgroup,ntmiter,isubtype
  real(4) :: ttwind,gtross,etrmax,etrmin,vtar_b,vtar_pg
  integer i,j,ii,ilat,ilon,ipres,itime,iweight,ndup,nreal2

  data bmiss/-999.0/

  rtim=0.0
  nflg0=0
  xlat0=0.0
  xlon0=0.0
  nlev0=0
  stid='        '
  
  print *, 'nobs=',nobs
  print *, 'fileo=',fileo

!  write(subtype,'(i2)') isubtype
 filein=trim(fileo)//'_'//trim(subtype)//'.tmp'
  

  allocate(rdiag(nreal,nobs),cdiag(nobs))
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

if (igrads ==1) then 

  filegrads=trim(fileo)//'_'//trim(subtype)//'_grads'

  open(21,file=filegrads,form='unformatted',status='new')    ! open output file


  print *,'ituse=',ituse
  print *, rdiag(1,1),rdiag(2,1),rdiag(3,1),rdiag(4,1),rdiag(5,1),&
           rdiag(6,1),rdiag(7,1),rdiag(8,1),rdiag(9,1),rdiag(10,1)

!  print *, rdiag(1,100),rdiag(2,100),rdiag(3,100),rdiag(4,100),rdiag(5,100),&
!           rdiag(6,100),rdiag(7,100),rdiag(8,100),rdiag(9,100),rdiag(10,100)

  ilat=1                           ! the position of lat
  ilon=2                           ! the position of lon
  ipres=4                          ! the position of pressure
  itime=6                          ! the position of relative time
  iweight=11                       ! the position of weight 
! check wether data have duplicate

  call hash(rdiag,nobs,nreal,ilat,ilon,ipres,itime,iweight,ndup)

     ii=0
     do  i=1,nobs
         if(rdiag(iweight,i) >0.0) then
             ii=ii+1
             stid=cdiag(i)
             rlat=rdiag(ilat,i)
             rlon=rdiag(ilon,i)
              write(21) stid,rlat,rlon,rtim,1,1
              write(21) (rdiag(j,i),j=3,nreal)
          endif
      enddo

    print *, 'ii=',ii
   

!  write out into grads file
            

!  the end of file
     stid='        '
     write(21) stid,xlat0,xlon0,rtim,nlev0,nflg0 
 
  close(21)
  close(11)
endif
  deallocate(rdiag,cdiag)
  return 
  end


  
