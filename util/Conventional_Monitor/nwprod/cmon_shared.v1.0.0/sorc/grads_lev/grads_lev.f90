!=================================================================================
!  grads_lev
!
!    Read in data from the .tmp file, arrange by level and write into 
!    the GrADS data files (one for scatter plots, and one for horizontal plots)
!    depending on the iscater and igrads parameters.
!=================================================================================


subroutine grads_lev(fileo,ifileo,nobs,nreal,nlev,plev,iscater,igrads,&
                        levcard,hint,isubtype,subtype)

   implicit none
 
   integer ifileo 
   real(4),allocatable,dimension(:,:)  :: rdiag
   character(8),allocatable,dimension(:) :: cdiag
   real(4),dimension(nlev) :: plev,plev2
   character(8) :: stid
   character(2) ::  subtype
   character(ifileo) :: fileo 

   character(30) :: files,filegrad
   character(10) :: levcard 
   character(15) filein

   integer(4):: ittype,ituse,ntumgrp,ntgroup,ntmiter,isubtype
   integer i,j,k,ii,nflg0
   integer ilat,ilon,ipres,itime,iweight,ndup
   real(4) :: ttwind,gtross,etrmax,etrmin,vtar_b,vtar_pg 

   integer nobs,nreal,nlfag,nflag0,nlev,nlev0,getpres,iscater,igrads
   real*4 rmiss,rtim,xlat0,xlon0,rlat,rlon,hint

   data rmiss/-999.0/

   nflag0=0
   rtim=0.0
   nflg0=0
   xlat0=0.0
   xlon0=0.0
   nlev0=0
   stid='        '
   plev2=plev-hint

   print *, '--> BEGIN grads_lev.x'  
   print *, 'nobs=',nobs
   print *, 'fileo=',fileo
   print *, 'nreal=', nreal

   do i=1,nlev
      print *, 'i, plev2(i) = ', i, plev2(i)
   enddo


!  write(subtype,'(i2)') isubtype
   filein=trim(fileo)//'_'//trim(subtype)//'.tmp'

   filegrad=trim(fileo)//'_'//trim(subtype)//'_grads'
   
   print *, 'filein   = ', filein
   print *, 'filegrad = ', filegrad

   allocate(rdiag(nreal,nobs),cdiag(nobs))
   open(21,file=filein,form='unformatted')
   rewind(21)

   do i=1,nobs
      read(21) cdiag(i),rdiag(1:nreal,i)
   enddo

   if(iscater ==1) then
      files=trim(fileo)//'_'//trim(subtype)//'.scater'
      open(51,file=files,form='unformatted')
      write(51) nobs,nreal
      write(51) rdiag
      close(51)
   endif


   if (igrads ==1 .AND. nobs > 0)  then 

!      print *, rdiag(1,1),rdiag(2,1),rdiag(3,1),rdiag(4,1),rdiag(5,1),&
!               rdiag(6,1),rdiag(7,1),rdiag(8,1),rdiag(9,1),rdiag(10,1)

!      print *, rdiag(1,100),rdiag(2,100),rdiag(3,100),rdiag(4,100),rdiag(5,100),&
!               rdiag(6,100),rdiag(7,100),rdiag(8,100),rdiag(9,100),rdiag(10,100)

      ilat=1                           ! the position of lat
      ilon=2                           ! the position of lon
      ipres=4                          ! the position of pressure
      itime=6                          ! the position of relative time
      iweight=11                       ! the position of weight 

      call rm_dups( rdiag,nobs,nreal,ilat,ilon,ipres,itime,iweight,ndup )

      open(31,file=filegrad,form='unformatted',status='new')

      ii=0
      do  i=1,nobs
         if(rdiag(iweight,i) >0.0 ) then
            ii=ii+1
            stid=cdiag(i)
            rlat=rdiag(ilat,i)
            rlon=rdiag(ilon,i)
            k=getpres(rdiag(ipres,i),plev2,nlev)

            !==============================
            !  write out into grads file
            !
            if(k /=0) then
!               print *, ' k matches:: k, rdiag(ipres,i) = ', k, rdiag(ipres,i)
               write(31) stid,rlat,rlon,rtim,1,0
                !
                !  I wonder about this j=3,nreal.  Is that a mistake?
                !  keep an eye on it; if the plots are ok I guess we're ok, but
                !  I wonder if this is a cut&paste error taking code from
                !  read_conv2grads.f90?  not clear yet.
                !
               write(31) plev2(k),(rdiag(j,i),j=3,nreal)
            endif
         endif
      enddo

100 format(6e13.3)

      print *, 'ii=',ii
   
      !==============================
      ! write the end of file marker
      stid='        '
      write(31) stid,xlat0,xlon0,rtim,nlev0,nflag0 
 
      close(31)

   else
      write(6,*) "No output file generated, nobs, igrads = ", nobs, igrads
   endif

   deallocate(rdiag,cdiag)
   print *, '<-- END grads_lev.x'  
   return 
end


!========================================================
! getpres
!
!   return the index to the pressure level array (plev) 
!     to which a given input level (p1) belongs
!
!   NOTE:  This function works through the level array
!	from high to low and uses a >= condition not ==.
!	So a level is a range here and not a discrete
!       value like some of the other executables. 
!========================================================
function getpres(p1,plev,nlevs)

   implicit none
  
   real*4 p1
   real*4,dimension(nlevs) :: plev
   integer getpres,ip,ii,nlevs

   getpres = 0
!   ip = int(p1)

   do ii=1,nlevs
      if( p1 >=plev(ii) ) then 
         getpres=ii
         return 
      endif
   enddo

   return 
end

  
