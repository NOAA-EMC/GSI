program makegrid
! xlf90 fixtogrd.f -o fixtogrd.x /nwprod/lib/libbacio_4.a
  implicit none
 
! Declare variables
  real*4,allocatable,dimension(:,:,:):: agvin
  real*4,allocatable,dimension(:,:):: corq2in,corzin,hwllin,vscalein,&
      wgvin,bgvin,corsstin,hsstin

  real*4,allocatable,dimension(:,:):: corz,cord,cort,corq,corq2, &
     coroz,corc,corsst,hsst
  real*4,allocatable,dimension(:):: corp
  real*4,allocatable,dimension(:,:):: hz,hd,ht,hq,hoz,hc
  real*4,allocatable,dimension(:):: hp
  real*4,allocatable,dimension(:,:):: vz,vd,vt,vq,voz,vc

  character(255) grdfile
  character*5 var
  integer inerr,nlat,nlon,nlat2,nlon2,nsig,nsigstat,mlat,ncfggg,iret,mlon
  integer outerr,iter,isig,istat
  integer i,k,kz,kd,kt,kq,koz,kc

!!  t574        t382       t190quad   t254       t170       t126       t62
!!  nlat=578  ; nlat=386 ; nlat=290 ; nlat=258 ; nlat=192 ; nlat=130 ; nlat=96 
!!  nlon=1152 ; nlon=768 ; nlon=576 ; nlon=512 ; nlon=384 ; nlon=256 ; nlon=192

  inerr=15
  outerr=17      

! Open background error statistics file
  open(inerr,file='berror_stats_in1',form='unformatted')
! Read amplitudes
  rewind inerr
  read(inerr) nsigstat,mlat,mlon

  write(6,*) 'nsigstat,nlat,nlon = ',nsigstat,mlat,mlon
  nsig=nsigstat
  nlat=mlat
  nlon=mlon

  allocate( agvin(nlat,nsig,nsig) )
  allocate( wgvin(nlat,nsig),bgvin(nlat,nsig) )
  allocate( corq2in(nlat,nsig) )
  allocate( corsstin(nlat,nlon),hsstin(nlat,nlon) )

  allocate( corz(nlat,nsig),cord(nlat,nsig),cort(nlat,nsig),corq(nlat,nsig),&
      coroz(nlat,nsig),corc(nlat,nsig) )
  allocate( corp(nlat) )
  allocate( hz(nlat,nsig),hd(nlat,nsig),ht(nlat,nsig),hq(nlat,nsig), &
      hoz(nlat,nsig),hc(nlat,nsig),hp(nlat) )
  allocate( vz(nlat,nsig),vd(nlat,nsig),vt(nlat,nsig),vq(nlat,nsig), &
      voz(nlat,nsig),vc(nlat,nsig) )

  write(6,*) 'READ IN BACKGROUND ERROR FILE HERE'
  rewind(inerr)
  read(inerr) isig,mlat,mlon
  read(inerr) agvin,bgvin,wgvin
  
  read: do
    read(inerr,iostat=istat) var,isig
    if (istat/=0) exit

    allocate ( corzin(nlat,isig) )
    allocate ( hwllin(nlat,isig) )
    if (isig>1) allocate ( vscalein(nlat,isig) )

    if (var/='sst') then
      if (var=='q' .or. var=='Q') then
        write(6,*) 'read Q variance'
        read(inerr) corzin,corq2in
      else
        read(inerr) corzin
      end if
      read(inerr) hwllin
      if(isig>1) read(inerr) vscalein
    else
      read(inerr) corsstin
      read(inerr) hsstin
    end if

    if (var=='sf') then
      write(6,*) 'TRANSFER STREAMFUNCTION'
      corz(:,:)=corzin(:,:) ; hz(:,:)=hwllin(:,:) ; vz(:,:)=1./vscalein(:,:)
    else if (var=='vp') then
      cord(:,:)=corzin(:,:) ; hd(:,:)=hwllin(:,:) ; vd(:,:)=1./vscalein(:,:)
      write(6,*) 'TRANSFER VELOCITY POTENTIAL'
    else if (var=='t') then
      cort(:,:)=corzin(:,:) ; ht(:,:)=hwllin(:,:) ; vt(:,:)=1./vscalein(:,:)
      write(6,*) 'TRANSFER TEMPERATURE'
    else if (var=='q') then
      corq(:,:)=corzin(:,:) ; hq(:,:)=hwllin(:,:) ; vq(:,:)=1./vscalein(:,:)
      write(6,*) 'TRANSFER RELATIVE HUMIDITY'
    else if (var=='oz') then
      coroz(:,:)=corzin(:,:) ; hoz(:,:)=hwllin(:,:) ; voz(:,:)=1./vscalein(:,:)
      write(6,*) 'TRANSFER OZONE'
    else if (var=='cw') then
      corc(:,:)=corzin(:,:) ; hc(:,:)=hwllin(:,:) ; vc(:,:)=1./vscalein(:,:)
    else if (var=='ps') then
      corp(:)=corzin(:,1) ; hp(:)=hwllin(:,1)
    else if (var=='sst') then
!!    already have corsst and hsst
    else
      write(6,*) 'CANNOT PROCESS VAR ',var,' STOP HERE'
      stop
    end if

    deallocate(corzin,hwllin)
    if (isig>1) deallocate(vscalein)
  enddo read
  close(inerr)

! OF LATIDUDE DEPENDENT VARIABLES
   grdfile='bgstats_sp.grd'
   ncfggg=len_trim(grdfile)
   call baopenwt(22,grdfile(1:ncfggg),iret)
   call wryte(22,4*nlat*nsig,corz)
   call wryte(22,4*nlat*nsig,cord)
   call wryte(22,4*nlat*nsig,cort)
   call wryte(22,4*nlat*nsig,corq)
   call wryte(22,4*nlat*nsig,corq2in)
   call wryte(22,4*nlat*nsig,coroz)
   call wryte(22,4*nlat*nsig,corc)
   call wryte(22,4*nlat,corp)
   call wryte(22,4*nlat*nsig,hz)
   call wryte(22,4*nlat*nsig,hd)
   call wryte(22,4*nlat*nsig,ht)
   call wryte(22,4*nlat*nsig,hq)
   call wryte(22,4*nlat*nsig,hoz)
   call wryte(22,4*nlat*nsig,hc)
   call wryte(22,4*nlat,hp)
   call wryte(22,4*nlat*nsig,vz)
   call wryte(22,4*nlat*nsig,vd)
   call wryte(22,4*nlat*nsig,vt)
   call wryte(22,4*nlat*nsig,vq)
   call wryte(22,4*nlat*nsig,voz)
   call wryte(22,4*nlat*nsig,vc)
   call wryte(22,4*nlat*nsig*nsig,agvin)
   call wryte(22,4*nlat*nsig,bgvin)
   call wryte(22,4*nlat*nsig,wgvin)
   call baclose(22,iret)

   deallocate( agvin,wgvin,bgvin )
   deallocate( corq2in)
   deallocate( corsstin,hsstin)
   deallocate( corz,cord,cort,corq,corc,coroz,corp)
   deallocate( hz,hd,ht,hq,hoz,hc,hp,vz,vd,vt,vq,voz,vc )


  return
end program makegrid
