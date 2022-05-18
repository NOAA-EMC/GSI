!-----------------------------------------------------------
!  grads_sig
!
!    Read uv data from the .tmp file and write it to the 
!    scatter and horiz GrADS data files.
!-----------------------------------------------------------

subroutine grads_sig(fileo,ifileo,nobs,nreal,nlev,plev,iscater,igrads,isubtype,subtype,list,run)

   use generic_list
   use data

   implicit none

   type(list_node_t), pointer   :: list
   type(list_node_t), pointer   :: next => null()
   type(data_ptr)               :: ptr 

   integer ifileo 
   real(4),allocatable,dimension(:,:)  :: rdiag_m2
   character(8),allocatable,dimension(:) :: cdiag
   real(4),dimension(nlev) :: plev
   real(4) :: rlat,rlon

   character(3) subtype, run
   character(8) :: stidend,stdid
   character(ifileo) :: fileo
   character(30) :: files,filegrads, file_nobs

   integer :: nobs,nreal,nlfag,nflag0,nlev,nlev0,getpro,iscater,igrads,obs_ctr
   real(4) :: rtim,xlat0,xlon0
   integer(4):: isubtype,ctr,nreal_m2
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
   print *, 'nlev     = ',nlev  
   print *, 'plev     = ',plev  
   print *, 'iscater  = ',iscater  
   print *, 'igrads   = ',igrads   
   print *, 'isubtype = ',isubtype   
   print *, 'subtype  = ',subtype   


   if( nobs > 0 ) then

      allocate(cdiag(nobs))

      nreal_m2 = nreal - 2
      allocate(rdiag_m2(nreal-2,nobs))        ! this will contain rdiag except
                                              ! for the first 2 fields (type and subtype)

      !---------------------------------------------
      ! Retrieve data from the linked list and load
      !   into the cdiag and rdiag arrays
      !
      obs_ctr = 0
      next => list

      do while ( associated( next ) .eqv. .TRUE. ) 
         ptr = transfer(list_get( next ), ptr)
         next => list_next( next )

         obs_ctr = obs_ctr + 1
         cdiag( obs_ctr ) = ptr%p%stn_id
         do i=3, nreal
            rdiag_m2(i-2, obs_ctr) = ptr%p%rdiag( i )
         end do
      end do

      print *, 'obs_ctr (list) = ', obs_ctr

      !------------------------------
      ! write scatter plot data file
      !

      !  It's also a bit mysterious why we look for dups and match pressure levels
      !  for the one data file but dump the whole thing for the scatter file.
      !  Should ask Su about that.

      print *, 'begin writing scatter data file'

      if(iscater == 1) then
         files=trim(fileo)//'_'//trim(subtype)//'.scater.'//trim(run)
         open(51,file=files,form='unformatted')
         write(51) nobs,nreal_m2
         write(51) rdiag_m2

         close(51)
      endif

      print *, 'end writing scatter data file'


      !--------------------------------------------------------------------
      !  write horiz grads data file and avoid trying
      !  to write output if nobs == 0.  Seg faults are uncool.
      !
      if (igrads ==1 .AND. nobs > 0)  then 
         filegrads=trim(fileo)//'_'//trim(subtype)//'.grads.'//trim(run)
         open(21,file=filegrads,form='unformatted',status='new')    ! open output file

         ilat      = idx_obs_lat -2        ! modified position of lat
         ilon      = idx_obs_lon -2        ! modified position of lon
         ipres     = idx_pres -2           ! modified position of pressure
         itime     = idx_time -2           ! modified position of relative time
         iweight   = idx_rwgt -2           ! modofied position of weight 

         !------------------------------------------
         !  rm duplicate data
         !
         call rm_dups( rdiag_m2,nobs,nreal_m2,ilat,ilon,ipres,itime,iweight,ndup )
      
         ctr=0
         do  i=1,nobs
            if(rdiag_m2(iweight,i) >0.0 ) then
               stdid=cdiag(i)
               rlat=rdiag_m2(ilat,i)
               rlon=rdiag_m2(ilon,i)

               k=getpro( rdiag_m2( ipres,i ), plev, nlev )

               if(k /=0) then
                  write(21) stdid,rlat,rlon,rtim,1,nflag

                  !---------------------------------------------------------------
                  ! note this writes the rdiag_m2 record starting at station
                  ! elevation; lat and lon are written in the line above with the
                  ! station id info
                  write(21) plev(k),rdiag_m2(3:nreal_m2,i) 

                  ctr = ctr + 1
               endif 
            endif
         enddo
   
         !--------------------------
         !  write EOF marker
         stidend='        '
         write(21) stidend,xlat0,xlon0,rtim,nlev0,nflag0 
        
         close(21)
         print *, 'num recs written to GrADS file = ', ctr

         file_nobs=trim(fileo)//'_'//trim(subtype)//'.nobs.'//trim(run)
         open( 32, file=file_nobs, form='formatted', status='new' )
         write(32,*) trim(fileo), ',', trim(subtype), ',', ctr
         close( 32 )

      else
         write(6,*) "No output file generated, nobs, igrads = ", nobs, igrads
      endif

      deallocate(cdiag,rdiag_m2)

   else
      print *, 'exiting grads_sig, nobs = ', nobs
   end if

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

  
