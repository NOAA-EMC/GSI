!---------------------------------------------------------------------------------
!  grads_sfctime
!
!    Read in data from the .tmp file, arrange by level and write into 
!    the GrADS data files (one for scatter plots, and one for horizontal plots)
!    depending on the iscater and igrads parameters.
!---------------------------------------------------------------------------------

subroutine grads_sfctime(fileo,ifileo,nobs,nreal,nreal2,nlev,plev,iscater,igrads,isubtype,subtype)

   implicit none
  
   real(4),allocatable,dimension(:,:)  :: rdiag
   character(8),allocatable,dimension(:) :: cdiag
   real(4),dimension(nlev) :: plev
   integer,dimension(nlev) :: ndata
   real(4),dimension(nreal2,10000,nlev) :: tobs
   real(4),dimension(10000) :: rlat,rlon
   character(8),dimension(10000) :: stid
   character(8) :: stidend
   character(ifileo) :: fileo
   character(2) :: subtype 
   character(30) :: files,filein,filegrads
   integer :: nobs,nreal,nlfag,nflag0,nlev,nlev0,getlev,iscater,igrads
   real(4) :: rmiss,rtim,xlat0,xlon0,rtime
 
   integer(4):: isubtype
   integer nt,ifileo,k,i,ii,j,nflag,nreal2
   integer ilat,ilon,ipres,itime,iweight,ndup

   data rmiss/-999.0/

   tobs=rmiss 
   stid='        '
   ndata=0
  
   print *,'fileo=',fileo
   print *,'nobs=',nobs
   allocate(rdiag(nreal,nobs),cdiag(nobs))


!   write(subtype,'(i2)') isubtype
   filein=trim(fileo)//'_'//trim(subtype)//'.tmp'
   open(11,file=filein,form='unformatted') !  open input file
   rewind(11)

   do i=1,nobs
      read(11) cdiag(i),rdiag(1:nreal,i)
   enddo

   if(iscater ==1) then
      files=trim(fileo)//'_'//trim(subtype)//'.scater'
      print *, 'scatter files = ', files
      open(51,file=files,form='unformatted')
      write(51) nobs,nreal
      write(51) rdiag
      close(51)
   endif

   if( igrads == 1 )  then 

      filegrads=trim(fileo)//'_'//trim(subtype)//'_grads'
      open(21,file=filegrads,form='unformatted',status='new')     !  open output file 
      print *, 'filegrads = ', filegrads

!      print *, rdiag(1,1),rdiag(2,1),rdiag(3,1),rdiag(4,1),rdiag(5,1),&
!           rdiag(6,1),rdiag(7,1),rdiag(8,1),rdiag(9,1),rdiag(10,1)

!      print *, rdiag(1,100),rdiag(2,100),rdiag(3,100),rdiag(4,100),rdiag(5,100),&
!           rdiag(6,100),rdiag(7,100),rdiag(8,100),rdiag(9,100),rdiag(10,100)

      ilat=1                           ! the position of lat
      ilon=2                           ! the position of lon
      ipres=4                          ! the position of pressure
      itime=6                          ! the position of relative time
      iweight=11                       ! the position of weight 


      !--------------------------------
      !   remove duplicate data 
      !
      call rm_dups( rdiag,nobs,nreal,ilat,ilon,ipres,itime,iweight,ndup )

      ii=0
      do  i=1,nobs
         if(rdiag(iweight,i) >0.0 ) then
            rtime=rdiag(itime,i)
            ii=ii+1
            stid(ii)=trim(cdiag(i))
            rlat(ii)=rdiag(ilat,i)
            rlon(ii)=rdiag(ilon,i)
            k=getlev( rtime,plev,nlev )

            if(k /=0) then
               tobs(1:nreal2,ii,k)=rdiag(3:nreal,i) 
               ndata(k)=ndata(k)+1
            endif

            do j=i+1,nobs
               if( cdiag(j) == stid(ii) .and. rdiag(ilat,i) == rdiag(ilat,j) &
                   .and. rdiag(ilon,i)  == rdiag(ilon,j) .and. rdiag(iweight,j) >0.0 ) then
                   rtime=rdiag(itime,j)
                   k=getlev( rtime,plev,nlev )
            
                   if(k /=0) then
                      tobs(1:nreal2,ii,k)=rdiag(3:nreal,j) 
                      rdiag(iweight,j)=-rdiag(iweight,j)
                      ndata(k)=ndata(k)+1
                   endif
               endif
            enddo
         endif 

      enddo
   
!     print *,'ii=',ii

! ################################################################################
!  write out into grads file
!
!   NOTE:  This block used to write all nlev worth of data to the output file
!          and then also wrote a nt_{type}_00.yyyymmddcc file which contained
!          the value of nt.  The nt=maxloc(data,dim=1) statment below gives nt
!          the value of the largest element in the data array.  This nt value
!          was read and then used in the GrADS script to set the correct time
!          step.  I'm having problems making that work, but if we're only going
!          to use the step at nt, it makes more sense to write only that into
!          the data file, reducing the output data file size by ~90%, and always
!          using an nt value of 1 in the GrADS scripts.  Simple is better.
!
!   NOTE Further:  Per Su the idea behind the nlev arrangment is 
!          "... I try to plot the point which close to analysis time.  There
!          are multiple observations (every 30 minutes or every hour) for the six 
!          hour window (-3.0 to 3.0 relative to analysis time), so I divided time
!          -2.5,-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5."
!
!          Generally I see that the nt ends up as either 1 or 6.  Either way
!          things can be simplified by writing only the nt step data into the
!          output file, not creating the nt_{type}_00.yyyymmddcc file at all,
!          and always sending GrADS a nt value of 1. 
! ################################################################################
  
      nt=maxloc(ndata,dim=1)
      k=nt          

      nflag=1
      rtim=0.0
      nlev0=1
      do i=1,ii
         write(21) stid(i),rlat(i),rlon(i),rtim,nlev0,nflag
         write(21) (tobs(j,i,k),j=1,nreal2)
      enddo

      nlev0=0
      write(21) stid(i),rlat(i),rlon(i),rtim,nlev0,nflag

      !------------------------
      !  write file end marker
      !
      xlat0=0.0
      xlon0=0.0
      nflag0=0
      stidend='        '
      write(21) stidend,xlat0,xlon0,rtim,nlev0,nflag0 
        
      close(21)

   endif

   deallocate(rdiag,cdiag)
   return 

end



!-----------------------------------------------------------------------------
!  function getlev
!
!  This function returns the corresponding level for the p1 input value.
!-----------------------------------------------------------------------------
function getlev( p1,plev,nlevs )

   implicit none
  
   real*4 p1
   real*4,dimension(nlevs) :: plev
   integer getlev,ii,nlevs

   getlev=0

   do ii=1,nlevs
      if(p1 <= plev(ii)) then
         getlev=ii
         return 
      endif
   enddo

   return 
end

  
