program convert_be2le
! ifort convert_be2le.f -o convert_be2le.x -free
  implicit none

! Declare variables
  real*4,allocatable,dimension(:,:,:):: agvin
  real*4,allocatable,dimension(:,:):: corq2in,corzin,hwllin,vscalein,&
      wgvin,bgvin,corsstin,hsstin

  character(255) filein,fileout
  character*5 var
  integer inerr,nlat,nlon,nsig,nsigstat,mlat,iret,mlon
  integer outerr,iter,isig,istat

  call getarg(1,filein)
  call getarg(2,fileout)

  write(6,*) 'BIG ENDIAN INPUT FILE == ',trim(filein)
  write(6,*) 'LITTLE ENDIAN/NATIVE OUTPUT FILE == ',trim(fileout)

  inerr=15
  outerr=25

! Open big_endian background error statistics file --> INPUT
  open(inerr,file=trim(filein),form='unformatted', &
      convert='big_endian')

! Open native format file for writing out
  open(outerr,file=trim(fileout),form='unformatted', &
      convert='native')

!!  open(outerr,file='berror_stats_out_le',form='unformatted', &
!!      convert='little_endian')

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

  write(6,*) 'READ IN BACKGROUND ERROR FILE HERE'
  rewind(inerr)
  read(inerr) isig,mlat,mlon
  read(inerr) agvin,bgvin,wgvin

  write(6,*) 'WRITE OUT HEADER FOR BACKGROUND ERROR FILE HERE'
  rewind(outerr)
  write(outerr) isig,mlat,mlon
  write(outerr) agvin,bgvin,wgvin

  read: do
    read(inerr,iostat=istat) var,isig
    if (istat/=0) exit

    write(outerr) var,isig

    allocate ( corzin(nlat,isig) )
    allocate ( hwllin(nlat,isig) )
    if (isig>1) allocate ( vscalein(nlat,isig) )

    if (var/='sst') then
      if (var=='q' .or. var=='Q') then
        write(6,*) 'read Q variance'
        read(inerr) corzin,corq2in
        write(outerr) corzin,corq2in
      else
        read(inerr) corzin
        write(outerr) corzin
      end if
      read(inerr) hwllin
      write(outerr) hwllin
      if(isig>1) then
         read(inerr) vscalein
         write(outerr) vscalein
      end if
    else
      read(inerr) corsstin
      read(inerr) hsstin
      write(outerr) corsstin
      write(outerr) hsstin
    end if

    deallocate(corzin,hwllin)
    if (isig>1) deallocate(vscalein)
  enddo read

  close(inerr)
  close(outerr)

end program convert_be2le
