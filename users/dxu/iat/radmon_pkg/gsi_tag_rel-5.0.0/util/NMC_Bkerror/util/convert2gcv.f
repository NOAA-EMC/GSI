program convert
! xlf90 convert2gcv.f -o convert.x /nwprod/lib/libbacio_4.a
! ifort convert2gcv.f -o convert.x /scratch1/portfolios/NCEPDEV/da/save/Daryl.Kleist/nwprod/lib/libbacio_4.a -convert big_endian -free
  implicit none
 
! Declare variables
  real*4,allocatable,dimension(:,:,:):: agvin,corzin,hscalesin,vscalesin
  real*4,allocatable,dimension(:,:):: corp,corq2in,&
      wgvin,bgvin,corsstin,hsstin
  real*4,allocatable,dimension(:):: corpin,hscalespin

  character(255) grdfile
  character*5 var(40)
  integer inerr,nlat,nlon,nlat2,nlon2,nsig,nsigstat,mlat,ncfggg,iret
  integer outerr,iter,isig
  integer i,k,kz,kd,kt,kq,koz,kc

!! t878      ; t574        t382       t190quad   t254       t170       t126       t62
!! nlat=882  ; nlat=578  ; nlat=386 ; nlat=290 ; nlat=258 ; nlat=192 ; nlat=130 ; nlat=96 
!! nlon=1760 ; nlon=1152 ; nlon=768 ; nlon=576 ; nlon=512 ; nlon=384 ; nlon=256 ; nlon=192

!! t1148
!! nlat=1154
!! nlon=2304

  inerr=15
  outerr=17      

! Open background error statistics file
  open(inerr,file='berror_stats_in1',form='unformatted', &
      convert='big_endian')
! Read amplitudes
  rewind inerr
  read(inerr) nsigstat,mlat

  write(6,*) 'nsigstat,nlat = ',nsigstat,mlat
  nsig=nsigstat
  nlat=mlat

  if (nlat==96) then
    nlon=192
  else if(nlat==130) then
    nlon=256
  else if(nlat==192) then
    nlon=384
  else if(nlat==258) then
    nlon=512
  else if(nlat==290) then
    nlon=576
  else if(nlat==386) then
    nlon=768
  else if(nlat==578) then
    nlon=1152
  else if(nlat==882) then
    nlon=1760
  else if(nlat==1154) then
    nlon=2304
  else
    WRITE(6,*) nlat, 'number of latitudes not supported, CANNOT CONVERT'
    stop
  end if

  allocate( agvin(nlat,nsig,nsig) )
  allocate( wgvin(nlat,nsig),bgvin(nlat,nsig) )
  allocate( corzin(nlat,nsig,6),corpin(nlat),corq2in(nlat,nsig) )
  allocate( hscalesin(nlat,nsig,6),hscalespin(nlat) )
  allocate( vscalesin(nlat,nsig,6) )
  allocate( corsstin(nlat,nlon),hsstin(nlat,nlon) )
 
  rewind(inerr)
  read(inerr) isig,mlat, &
       corzin(:,:,1:4),corq2in,corzin(:,:,5:6),corpin, &
       hscalesin,hscalespin,vscalesin, &
       agvin,bgvin,wgvin, &
       corsstin,hsstin
  close(inerr)

  var=' '
  var(1)='sf'
  var(2)='vp'
  var(3)='t'
  var(4)='q'
  var(5)='oz'
  var(6)='cw'
  var(7)='ps'
  var(8)='sst'


! Open background error statistics file
  open(outerr,file='berror_stats_genvar',form='unformatted', &
      convert='big_endian')

! Write amplitudes
  rewind outerr
  write(outerr) nsig,nlat,nlon
  write(outerr) agvin,bgvin,wgvin   
  do i=1,6
    write(6,*) i,var(i),nsig
    write(outerr) var(i),nsig
    if (i==4) then
      write(outerr) corzin(:,:,i),corq2in
      write(outerr) hscalesin(:,:,i)
      write(outerr) vscalesin(:,:,i)
    else
      write(outerr) corzin(:,:,i)
      write(outerr) hscalesin(:,:,i)
      write(outerr) vscalesin(:,:,i)
    end if
  end do

  isig=1
  i=7
  write(6,*) i,var(i),isig
  write(outerr) var(7),isig
  write(outerr) corpin
  write(outerr) hscalespin

  i=8
  write(6,*) i,var(i),isig
  write(outerr) var(8),isig
  write(outerr) corsstin
  write(outerr) hsstin

  close(outerr)

  deallocate( agvin,wgvin,bgvin )
  deallocate( corzin,corpin,corq2in)
  deallocate( hscalesin,vscalesin,hscalespin )
  deallocate( corsstin,hsstin)

end program convert
