!===========================================================
!  grads_sig
!
!  extract uv data and write to scatter and GrADS files
!===========================================================

subroutine grads_sig(fileo,ifileo,nobs,nreal,nreal2,nlev,plev,iscater,igrads,isubtype,subtype)
   implicit none
 
   integer nreal2,ifileo 
   real(4),allocatable,dimension(:,:)  :: rdiag
   character(8),allocatable,dimension(:) :: cdiag
   real(4),dimension(nlev) :: plev
   real(4) :: rlat,rlon

   character(2) subtype 
   character(8) :: stidend,stdid
   character(ifileo) :: fileo
   character(30) :: files,filein,filegrads

   integer :: nobs,nreal,nlfag,nflag0,nlev,nlev0,getpro,iscater,igrads
   real(4) :: rtim,xlat0,xlon0
   integer(4):: isubtype,ctr
   integer i,j,k,ilat,ilon,ipres,itime,iweight,ndup,nflag
 

   stdid='        '
   nflag0=0
   rtim=0.0
   xlat0=0.0
   xlon0=0.0
   nlev0=0
 
   print *, '--> BEGIN grads_sig.x'
   print *, ' ' 
   print *, 'inputs to grads_sig.x ='
   print *, 'fileo    = ',fileo
   print *, 'ifileo   = ',ifileo
   print *, 'nobs     = ',nobs
   print *, 'nreal    = ',nreal
   print *, 'nreal2   = ',nreal2
   print *, 'nlev     = ',nlev  
   print *, 'plev     = ',plev  
   print *, 'iscater  = ',iscater  
   print *, 'igrads   = ',igrads   
   print *, 'isubtype = ',isubtype   
   print *, 'subtype  = ',subtype   

   allocate(rdiag(nreal,nobs),cdiag(nobs))

   filein=trim(fileo)//'_'//trim(subtype)//'.tmp'
   open(11,file=filein,form='unformatted')
   rewind(11)
   do i=1,nobs
      read(11) cdiag(i),rdiag(1:nreal,i)
   enddo

   !=========================
   ! write scatter file
   !
   if(iscater ==1) then
      files=trim(fileo)//'_'//trim(subtype)//'.scater'
      open(51,file=files,form='unformatted')
      write(51) nobs,nreal
      write(51) rdiag
      close(51)
   endif

   !====================================================================
   !  write grads data file and 
   !  avoid trying to write output if nobs == 0.  Seg faults are uncool.
   !
   if (igrads ==1 .AND. nobs > 0)  then 
      filegrads=trim(fileo)//'_'//trim(subtype)//'_grads'
      open(21,file=filegrads,form='unformatted',status='new')    ! open output file

      ilat=1                           ! the position of lat
      ilon=2                           ! the position of lon
      ipres=4                          ! the position of pressure
      itime=6                          ! the position of relative time
      iweight=11                       ! the position of weight 

      !==========================================
      !  check for duplicate data
      call hash(rdiag,nobs,nreal,ilat,ilon,ipres,itime,iweight,ndup)
      
      ctr=0
      do  i=1,nobs
         if(rdiag(iweight,i) >0.0 ) then
            stdid=cdiag(i)
            rlat=rdiag(ilat,i)
            rlon=rdiag(ilon,i)
            k=getpro(rdiag(ipres,i),plev,nlev)
            if(k /=0) then
               write(21) stdid,rlat,rlon,rtim,1,nflag
               write(21) plev(k),rdiag(3:nreal,i)
               ctr = ctr + 1
            endif 
         endif
      enddo
   

      !  write file end 
      stidend='        '
      write(21) stidend,xlat0,xlon0,rtim,nlev0,nflag0 
        
      close(21)
      print *, 'num recs written to GrADS file = ', ctr

   else
      write(6,*) "No output file generated, nobs, igrads = ", nobs, igrads
   endif

   deallocate(rdiag,cdiag)

   return 
end


!============================================================
!  getpro
!
!    Determines appropriate match of p1 value to plev array.
!
!    Note that values within +/- 5 of a specific plev array 
!    are determined to be a match. 
!============================================================
function getpro(p1,plev,nlevs)
  
   implicit none

   real*4 p1
   real*4,dimension(nlevs) :: plev
   integer getpro,ip,np,dp,ii,nlevs

   ip=int(p1)

   getpro=0
   do ii=1,nlevs
      np=int(plev(ii))
      dp=abs(ip-np)
   
      if(dp <=5) then
         getpro=ii
         return
      endif
   enddo

   return 
end

  
