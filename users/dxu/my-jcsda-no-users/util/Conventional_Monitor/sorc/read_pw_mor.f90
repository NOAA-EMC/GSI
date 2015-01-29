! the program read humidity files
  subroutine read_pw_mor(nreal,mtype,fname,fileo,gtross,rlev)

     implicit none

  real(4),allocatable,dimension(:,:)  :: rdiag
  real(4),dimension(3,3000000) :: rpress
  integer,dimension(3) :: ncount,ncount_vqc,ncount_gros

  character*200 fname
  character*50 fileo
  character*15 mtype 

  real*4 tiny,huge,rlev
  real rgtross,gtross
  integer nobs,nreal,ntotal,ngross,nreal_in,nlev
  integer i,nlat,nlon,npres,ntime,ndup
  integer ilat,ilon,ipres,itime,iqc,iuse,imuse,iweight,ierr,ierr2,ierr3,iobs,iogs

  real(4) :: bmiss,vqclmt,vqclmte

  data bmiss/-999.0/ 


  ncount=0
  rpress=bmiss
  ncount_vqc=0
  ncount_gros=0


     ntotal=0
     open(unit=11,file=fname,form='unformatted')
     rewind(11)
     read(11) nobs,nreal_in
!     print *, 'nobs=',nobs
     if (nreal /= nreal_in) then
      print *,'nreal_in,nreal ',nreal_in,nreal
      stop
     endif 

    allocate(rdiag(nreal,nobs))
    read(11) rdiag


   ilat=1                           !  lat
    ilon=2                          !  lon
    ipres=4                         !  pressure
    itime=6                         !  relative time
    iqc=7                           !  qc flag
    iuse=9                          !  original data usage flag
    imuse=10                        !  data usage flag in the analsis
    iweight=11                      !  variational relative weight
    ierr=12                        !  original error from bufr error table
    ierr2=13                        !  error from read_bufr
    ierr3=14                        !  error from final adjusted
    iobs=15                         !  obs
    iogs=16                         !  obs-ges
! check weather data have duplicate

  call hash(rdiag,nobs,nreal,ilat,ilon,ipres,itime,iweight,ndup)

 do  i=1,nobs
          if(rdiag(iweight,i) >0.0) then
           ncount(3)=ncount(3)+1
            rpress(3,ncount(3))=rdiag(iogs,i)
          endif
 enddo

  deallocate(rdiag)


  


!  calculate the the areacd usr2
    
      
  rgtross=-gtross
  nlev=nint((gtross-rgtross)/rlev)
   print *,nlev
   print *,gtross, rgtross

   do i=1,3
    if(ncount(i) ==0) ncount(i)=1
   enddo

   call hist(mtype,rpress,3,3000000,ncount,rgtross,gtross,rlev,fileo,ncount_vqc,ncount_gros)

  return 
  end
