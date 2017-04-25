!---------------------------------------------------------------------------------
!  grads_sfctime
!
!    Read in data from the .tmp file, arrange by level and write into 
!    the GrADS data files (one for scatter plots, and one for horizontal plots)
!    depending on the iscater and igrads parameters.
!---------------------------------------------------------------------------------

subroutine grads_sfctime(fileo,ifileo,nobs,nreal,nreal2,nlev,plev,iscater,&
                         igrads,isubtype,subtype,list)

   use generic_list
   use data

   implicit none
 
   type(list_node_t), pointer   :: list
   type(list_node_t), pointer   :: next => null()
   type(data_ptr)               :: ptr
 
   real(4),allocatable,dimension(:,:)  :: rdiag_m2
   character(8),allocatable,dimension(:) :: cdiag
   real(4),dimension(nlev) :: plev
   integer,dimension(nlev) :: ndata

   !-----------------------------------------------------------------------------
   !  Here's another opportunity for improvement:  hard coding a limit of 10,000
   !  is setting a time bomb.  This should be handled either with dynamic 
   !  array allocation or another linked list.  
   !
   !  I'll leave that until I get this working.  First order of business is
   !  removing the use of nreal2 -- this subroutine should be able to cope with
   !  a single input of the real value of nreal and then modify it as necessary
   !  internally.  I hate magic numbers.
   !
!   real(4),dimension(nreal2,10000,nlev) :: tobs
   real(4),allocatable,dimension(:,:,:) :: tobs
   real(4),dimension(10000) :: rlat,rlon
   character(8),dimension(10000) :: stid

   character(8) :: stidend
   character(ifileo) :: fileo
   character(2) :: subtype 
   character(30) :: files,filein,filegrads
   integer :: nobs,nreal,nlfag,nflag0,nlev,nlev0,getlev,iscater,igrads
   real(4) :: rmiss,rtim,xlat0,xlon0,rtime
   integer      :: first, second
 
   integer(4):: isubtype
   integer nt,ifileo,k,i,ii,j,nflag,nreal2,obs_ctr
   integer ilat,ilon,ipres,itime,iweight,ndup

   data rmiss/-999.0/

   stid='        '
   ndata=0
  
   print *,'fileo=',fileo
   print *,'nobs=',nobs
   print *,'nreal, nreal2 = ', nreal, nreal2

   allocate( tobs(nreal-4,10000,nlev) )
   tobs=rmiss 
   allocate( cdiag(nobs) )
   allocate( rdiag_m2(nreal-2,nobs) )


   !---------------------------------------------
   ! Retrieve data from the linked list and load
   !   into the cdiag and rdiag arrays
   !
   print *, 'Associated(list)   = ', associated( list )
   obs_ctr = 0
   next => list

   do while ( associated( next ) == .TRUE. )
      ptr = transfer(list_get( next ), ptr)
!      print *, 'node data:', ptr%p
      next => list_next( next )

      obs_ctr = obs_ctr + 1
      cdiag( obs_ctr ) = ptr%p%stn_id
      do i=3, nreal
         rdiag_m2(i-2, obs_ctr) = ptr%p%rdiag( i )
      end do
   end do

   print *, 'obs_ctr (list) = ', obs_ctr

   print *, 'begin scater file generation'
   if(iscater ==1) then
      files=trim(fileo)//'_'//trim(subtype)//'.scater'
      print *, 'scatter files = ', files
      open(51,file=files,form='unformatted')
      write(51) nobs,nreal-2
      write(51) rdiag_m2
      close(51)
   endif
   print *, 'end scater file generation'


   if( igrads == 1 )  then 

      filegrads=trim(fileo)//'_'//trim(subtype)//'_grads'
      open(21,file=filegrads,form='unformatted',status='new')     !  open output file 
      print *, 'filegrads = ', filegrads

      ilat      = idx_obs_lat -2        ! modified position of lat
      ilon      = idx_obs_lon -2        ! modified position of lon
      ipres     = idx_pres -2           ! modified position of pressure
      itime     = idx_time -2           ! modified position of relative time
      iweight   = idx_rwgt -2           ! modofied position of weight

      !--------------------------------
      !   remove duplicate data 
      !
      call rm_dups( rdiag_m2,nobs,nreal-2,ilat,ilon,ipres,itime,iweight,ndup )


      !------------------------------------------------------------------------
      !  As best I can tell this section is functioning as intended, which I 
      !  also assume to be correct.  I made no changes to the logic though I did
      !  change rdiag to rdiag_m2 to make it more clear that this process is
      !  shaving off the first 2 rdiag values (lat & lon).
      !
      !  It's a white-hot mess but the intent is to group the same station's data
      !  in the tobs (:,:,:) array and keep the middle dimension in sync with the
      !  rlat(:), rlon(:), and stid(:) arrays. I insterted a whole bunch of
      !  diagnostics and that appears to be working correctly.  I question the 
      !  location of the ii incrementation, but tobs is set to rmiss so any gaps 
      !  (and there are some) should come out as rmiss. 
      !
      ii=0
      do  i=1,nobs
         if(rdiag_m2(iweight,i) >0.0 ) then
            rtime=rdiag_m2(itime,i)
            ii=ii+1
            stid(ii)=trim(cdiag(i))
            rlat(ii)=rdiag_m2(ilat,i)
            rlon(ii)=rdiag_m2(ilon,i)

            k=getlev( rtime,plev,nlev )

            if(k /=0) then
               tobs(1:nreal-4,ii,k)=rdiag_m2(3:nreal-2,i) 
               ndata(k)=ndata(k)+1
            endif

            do j=i+1,nobs
               if( cdiag(j) == stid(ii) .and. &
                   rdiag_m2(ilat,i) == rdiag_m2(ilat,j) .and. &
                   rdiag_m2(ilon,i) == rdiag_m2(ilon,j) .and. &
                   rdiag_m2(iweight,j) >0.0 ) then

                   rtime=rdiag_m2(itime,j)
                   k=getlev( rtime,plev,nlev )
            
                   if(k /=0) then
                      tobs(1:nreal-4,ii,k)=rdiag_m2(3:nreal-2,j) 
                      rdiag_m2(iweight,j)=-rdiag_m2(iweight,j)
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

      print *, 'using ndata max value of nt = ', nt
!      do i=1,nlev
!         print *, 'ndata(i) = ', i, ndata(i)
!      end do
      
      nflag=1
      rtim=0.0
      nlev0=1
      do i=1,ii
!         print *,' writing stid(i), rlat(i),rlon(i),rtim,nlev0,nflag = ', &
!                        i, stid(i),rlat(i),rlon(i)
         write(21) stid(i),rlat(i),rlon(i),rtim,nlev0,nflag
         write(21) (tobs(j,i,k),j=1,nreal-4)
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
      print *, 'wrote ii tobs to file ', ii

   endif

   deallocate( cdiag,rdiag_m2,tobs )
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

  
