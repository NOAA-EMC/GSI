program multiplier
! xlf90 reduce_rhvars.f -o reduce_rhvars.x /nwprod/lib/libbacio_4.a
  implicit none
 
! Declare variables
  real*4,allocatable,dimension(:,:,:):: agvin
  real*4,allocatable,dimension(:,:):: corq2in,corzin,hwllin,vscalein,&
      wgvin,bgvin,corsstin,hsstin
  real*4 corz2x,corq2x
  character*5 var


  integer inerr,nlat,nlon,nlat2,nlon2,nsig,nsigstat,mlat,iret,mlon
  integer outerr,iter,isig,istat
  integer i,k,kz,kd,kt,kq,koz,kc

!!  t574        t382       t190quad   t254       t170       t126       t62
!!  nlat=578  ; nlat=386 ; nlat=290 ; nlat=258 ; nlat=192 ; nlat=130 ; nlat=96 
!!  nlon=1152 ; nlon=768 ; nlon=576 ; nlon=512 ; nlon=384 ; nlon=256 ; nlon=192

  inerr=15
  outerr=17      

! Open original background error statistics file
  open(inerr,file='berror_stats_in',form='unformatted')
! Open new background error file with new variances
  open(outerr,file='berror_stats_out',form='unformatted')

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

  write(6,*) 'ALSO BEGIN WRITE OF NEW FILE'  
  rewind(outerr)
  write(outerr) isig,mlat,mlon


  read(inerr) agvin,bgvin,wgvin
! immediate write out to new file
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

        do k=1,nsig
          do i=1,nlat
! Temp arrays
             corz2x=corzin(i,k) ; corq2x=corq2in(i,k)
             corzin(i,k)=min(max(corz2x,0.00015),1.0)
             corq2in(i,k)=min(max(corq2x,0.00015),1.0)

             if (k >= 35 .and. k <= 37 ) corzin(i,k) = corzin(i,k)*0.4
             if (k >= 38 .and. k <= 41 ) corzin(i,k) = corzin(i,k)*0.24
             if (k == 42 ) corzin(i,k) = corzin(i,k)*0.6
             if (k == 43 ) corzin(i,k) = corzin(i,k)*0.7
             if (k == 44 ) corzin(i,k) = corzin(i,k)*0.7
             if (k >= 45 .and. k <= 51 ) corzin(i,k) = corzin(i,k)*0.5
             if (k == 52 ) corzin(i,k) = corzin(i,k)*0.2
             if (k == 53 ) corzin(i,k) = corzin(i,k)*0.2
             if (k == 54 ) corzin(i,k) = corzin(i,k)*0.1
             if (k == 55 ) corzin(i,k) = corzin(i,k)*0.1
             if (k == 56 ) corzin(i,k) = corzin(i,k)*0.03
             if (k == 57 ) corzin(i,k) = corzin(i,k)*0.03
             if (k == 58 ) corzin(i,k) = corzin(i,k)*0.06
             if (k == 59 ) corzin(i,k) = corzin(i,k)*0.01
             if (k == 60 ) corzin(i,k) = corzin(i,k)*0.014
             if (k == 61 ) corzin(i,k) = corzin(i,k)*0.007
             if (k == 62 ) corzin(i,k) = corzin(i,k)*0.01
             if (k == 63 ) corzin(i,k) = corzin(i,k)*0.01
             if (k >= 64 ) corzin(i,k) = corzin(i,k)*0.02

             if (k >= 35 .and. k <= 37 ) corq2in(i,k) = corq2in(i,k)*0.4
             if (k >= 38 .and. k <= 41 ) corq2in(i,k) = corq2in(i,k)*0.24
             if (k == 42 ) corq2in(i,k) = corq2in(i,k)*0.6
             if (k == 43 ) corq2in(i,k) = corq2in(i,k)*0.7
             if (k == 44 ) corq2in(i,k) = corq2in(i,k)*0.7
             if (k >= 45 .and. k <= 51 ) corq2in(i,k) = corq2in(i,k)*0.5
             if (k == 52 ) corq2in(i,k) = corq2in(i,k)*0.2
             if (k == 53 ) corq2in(i,k) = corq2in(i,k)*0.2
             if (k == 54 ) corq2in(i,k) = corq2in(i,k)*0.1
             if (k == 55 ) corq2in(i,k) = corq2in(i,k)*0.1
             if (k == 56 ) corq2in(i,k) = corq2in(i,k)*0.03
             if (k == 57 ) corq2in(i,k) = corq2in(i,k)*0.03
             if (k == 58 ) corq2in(i,k) = corq2in(i,k)*0.06
             if (k == 59 ) corq2in(i,k) = corq2in(i,k)*0.01
             if (k == 60 ) corq2in(i,k) = corq2in(i,k)*0.014
             if (k == 61 ) corq2in(i,k) = corq2in(i,k)*0.007
             if (k == 62 ) corq2in(i,k) = corq2in(i,k)*0.01
             if (k == 63 ) corq2in(i,k) = corq2in(i,k)*0.01
             if (k >= 64 ) corq2in(i,k) = corq2in(i,k)*0.02
           end do
         end do

! WRITE OUT NEW VARIANCES
         print *, 'NEW RH VARIANCES WRITTEN OUT HERE!'
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

   deallocate( agvin,wgvin,bgvin )
   deallocate( corq2in)
   deallocate( corsstin,hsstin)

  return
end program multiplier
