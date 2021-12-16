!------------------------------------------------------------
!  subroutine read_ps_mor
!
!------------------------------------------------------------
subroutine read_ps_mor(nreal,mtype,fname,fileo,gtross,rlev, grads_info_file)

   implicit none

   !-------------
   !  interface
   !
   integer,       intent(in)  :: nreal
   character*15,  intent(in)  :: mtype 
   character*200, intent(in)  :: fname
   character*50,  intent(in)  :: fileo
   real,          intent(in)  :: gtross
   real(4),       intent(in)  :: rlev
   character*50,  intent(in)  :: grads_info_file


   !-------------
   !  local vars
   !
   real(4),allocatable,dimension(:,:)  :: rdiag
   real(4),dimension(3,3000000) :: rpress
   integer,dimension(3) :: ncount,ncount_vqc,ncount_gros

   integer nobs,ntotal,ngross,nreal_in,nlev
   integer i,ilat,ilon,ipres,itime,iqc,iuse,imuse,iweight,ierr,ierr2,ierr3,iobs,iogs
   integer nlat,nlon,npres,ntime,ndup
   real(4) :: bmiss,vqclmt,vqclmte
   real rgtross

   data bmiss/-999.0/ 


   ncount=0
   rpress=bmiss
   ncount_vqc=0
   ncount_gros=0

   ntotal=0
   open(unit=11,file=fname,form='unformatted')
   rewind(11)
   read(11) nobs,nreal_in
!   print *, 'nobs=',nobs

   if (nreal /= nreal_in) then
      print *,'nreal_in,nreal ',nreal_in,nreal
      stop
   endif 

   allocate(rdiag(nreal,nobs))
   read(11) rdiag


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


   call rm_dups( rdiag,nobs,nreal,ilat,ilon,ipres,itime,iweight,ndup )

   do i=1,nobs
      if(rdiag(iweight,i) >0.0) then
         ncount(3)=ncount(3)+1
         rpress(3,ncount(3))=rdiag(iogs,i)
      endif
   enddo

   deallocate(rdiag)

  !---------------------------------
  !  calculate the the areacd usr2
  !
  !    This seems very odd to me.  Say gtross is n.  That means
  !      rgtross = -n
  !      gttross - rgtross is n - (-n) or 2n.  Always.  Really?!
  !      I'll have to ask Su about this.
  !    
  rgtross=-gtross
  nlev=nint((gtross-rgtross)/rlev)    
  print *,nlev
  print *,gtross, rgtross

  do i=1,3
     if(ncount(i) ==0) ncount(i)=1
  enddo

  call hist(mtype,rpress,3,3000000,ncount,rgtross,gtross,rlev,fileo,ncount_vqc,ncount_gros, grads_info_file )

  return 
end
