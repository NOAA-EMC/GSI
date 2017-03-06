!-------------------------------------------------------------
!  grads_mandlev
!
!       This subroutine extracts GrADS ready data files by
!       manditory level from the conventional data files.
!
!-------------------------------------------------------------

subroutine grads_mandlev(fileo,ifileo,nobs,nreal,nlev,plev,iscater,igrads,isubtype,subtype)

   implicit none
  
   real(4),allocatable,dimension(:,:)  :: rdiag
   character(8),allocatable,dimension(:) :: cdiag
  
   real(4),dimension(nreal) :: rdummy
   character(8) cdummy 
   real(4),dimension(nlev) :: plev

   real(4) rlat,rlon,rp
   character(2) subtype 
   character(8) stid
   character(ifileo) :: fileo 
   character(30)  :: files,filegrads 
   character(8)  :: stidend
   integer nobs,nreal,nlfag,nflag0,nlev,nlev0,getlev,iscater,igrads,nflg0
   real*4 rtim,xlat0,xlon0
   character(30) filein

   integer ifileo,i,j,ii,k
   integer ilat,ilon,ipres,itime,iweight,ndup
   integer(4):: isubtype
 
   stid='        '
   nflag0=0
   rtim=0.0
   nflg0=0
   xlat0=0.0
   xlon0=0.0
   nlev0=0

   print *, fileo
   print *,'nobs=',nobs
   print *,'nlev',nlev

   allocate(rdiag(nreal,nobs),cdiag(nobs))

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


      ilat=1                           ! the position of lat
      ilon=2                           ! the position of lon
      ipres=4                          ! the position of pressure
      itime=6                          ! the position of relative time
      iweight=11                       ! the position of weight 



      ! ####################################
      !  remove any duplicate data
      !
      call rm_dups( rdiag,nobs,nreal,ilat,ilon,ipres,itime,iweight,ndup )

      ii=0
      do  i=1,nobs
         if(rdiag(iweight,i) >0.0 ) then

            stid=cdiag(i)
            rlat=rdiag(ilat,i)
            rlon=rdiag(ilon,i)
            rp=rdiag(ipres,i)              ! bug was here -- rp must be real, not integer.

            k=0
            k=getlev(rp,plev,nlev)
            if(k /=0)  then
               write(21) stid,rlat,rlon,rtim,1,0

               !  I wonder about this j=3,nreal write.  see grads_lev for more
               !  info.
               write(21) plev(k),(rdiag(j,i),j=3,nreal)
               print *, 'added obs to level plev(k) ', plev(k)
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

!########################################################################
!  getlev
!
!  Given level p1, determine if it matches any level in the plev array.
!  The intention is to match levels exactly and ignore all non-matching
!  data.  
!
!  Return the index to the matched value in the plev array
!########################################################################
function getlev(p1,plev,nlevs)
  
   implicit none

   real*4 p1
   real*4,dimension(nlevs) :: plev
   integer getlev,ip,np,nlevs,ii

   ip=int(p1)                           ! convert p1 to integer value 
   getlev = 0

   do ii=1,nlevs
      np=int(plev(ii))                  ! convert plev values to integer

      if(ip == np) then
         getlev=ii
         return
      endif
   enddo

   return
end

