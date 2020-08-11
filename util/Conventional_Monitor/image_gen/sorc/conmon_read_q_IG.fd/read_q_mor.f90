!----------------------------------------------------------
!  subroutine read_q_mor
!
!----------------------------------------------------------

subroutine read_q_mor( nreal, dtype, fname, fileo, gtross, rlev, grads_info_file )

   implicit none

   integer,       intent( in )         :: nreal
   character*15,  intent( in )         :: dtype 
   character*200, intent( in )         :: fname
   character*50,  intent( in )         :: fileo
   real,          intent( in )         :: gtross
   character*50,  intent( in )         :: grads_info_file


   real(4),allocatable,dimension(:,:)  :: rdiag
   real(4),dimension(3,3000000) :: rpress
   integer,dimension(3) :: ncount,ncount_vqc,ncount_gros

   real(4) :: rgtross

   integer nobs,ntotal,ngross,nreal_in,nlev
   integer i,nlat,nlon,npres,ntime,ndup
   integer ilat,ilon,ipres,itime,iqc,iuse,imuse,iweight,ierr,ierr2,ierr3,iobs,iogs,iqsges
   real(4) :: rmiss,vqclmt,vqclmte,rlev

   data rmiss/-999.0/ 

   print *, '---> read_q_mor'
   print *, ' '
   print *, '          nreal  = ', nreal
   print *, '          dtype  = ', dtype
   print *, '          fname  = ', fname
   print *, '          fileo  = ', fileo
   print *, '          gtross = ', gtross
   print *, '          rlev   = ', rlev

   ncount=0
   rpress=rmiss
   ncount_vqc=0
   ncount_gros=0

   ntotal=0
   open(unit=11,file=fname,form='unformatted')
   rewind(11)

   read(11) nobs,nreal_in
   print *, '    From file ', fname
   print *, '          nobs    = ', nobs
   print *, '          nreal   = ', nreal


   print *,'nreal_in, nreal = ', nreal_in, nreal

   if (nreal /= nreal_in) then
      stop
   endif 

   allocate( rdiag( nreal, nobs ))
   read(11) rdiag

   close( 11 )


   ilat=1                          !  lat
   ilon=2                          !  lon
   ipres=4                         !  pressure
   itime=6                         !  relative time
   iqc=7                           !  qc flag
   iuse=9                          !  original data usage flag
   imuse=10                        !  data usage flag in the analsis
   iweight=11                      !  variational relative weight
   ierr=12                         !  original error from bufr error table
   ierr2=13                        !  error from read_bufr
   ierr3=14                        !  error from final adjusted
   iobs=15                         !  obs
   iogs=16                         !  obs-ges
   iqsges=18                       !  guess saturation specific humidity 

   call rm_dups( rdiag,nobs,nreal,ilat,ilon,ipres,itime,iweight,ndup )

   do  i=1,nobs
      if(rdiag(iweight,i) >0.0) then
         ncount(3)=ncount(3)+1
         rpress(3,ncount(3))=rdiag(iogs,i)/rdiag(iqsges,i)
      endif
   enddo

   deallocate(rdiag)


   !  calculate the the areacd usr2
    
   rgtross=-gtross
   nlev=nint((gtross-rgtross)/rlev)
   print *, 'nlev    = ', nlev
   print *, 'gtross  = ', gtross
   print *, 'rgtross = ', rgtross

   do i=1,3
      if(ncount(i) ==0) ncount(i)=1
   enddo

   call hist(dtype,rpress,3,3000000,ncount,rgtross,gtross,rlev,fileo,ncount_vqc,ncount_gros, grads_info_file )

!   print *, ' '
!   print *, '<--- read_q_mor '

  return 
end
