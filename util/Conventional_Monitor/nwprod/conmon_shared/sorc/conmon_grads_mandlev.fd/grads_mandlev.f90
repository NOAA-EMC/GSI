!-------------------------------------------------------------
!  grads_mandlev
!
!       This subroutine extracts GrADS ready data files by
!       manditory level from the conventional data files.
!
!-------------------------------------------------------------

subroutine grads_mandlev(fileo,ifileo,nobs,nreal,nlev,plev,iscater,igrads,&
                isubtype,subtype,list,run)

   use generic_list
   use data

   implicit none
 
   type(list_node_t), pointer   :: list
   type(list_node_t), pointer   :: next => null()
   type(data_ptr)               :: ptr
 
   real(4),allocatable,dimension(:,:)  :: rdiag_m2
   character(8),allocatable,dimension(:) :: cdiag
  
   real(4),dimension(nreal) :: rdummy
   character(8) cdummy 
   real(4),dimension(nlev) :: plev

   real(4) rlat,rlon,rp
   character(3) subtype,run
   character(8) stid
   integer ifileo
   character(ifileo) :: fileo 
   character(30)  :: files,filegrads 
   character(8)  :: stidend
   integer nobs,nreal,nlfag,nflag0,nlev,nlev0,getlev,iscater,igrads,nflg0
   real*4               :: rtim,xlat0,xlon0
   character(30)        :: filein, file_nobs

   integer              :: i,j,ii,k,nreal_m2,ctr,obs_ctr
   integer              :: ilat,ilon,ipres,itime,iweight,ndup
   integer(4)           :: isubtype
 
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

   if( nobs > 0 ) then
      allocate(cdiag(nobs))

      nreal_m2 = nreal - 2
      allocate(rdiag_m2(nreal-2,nobs))        ! this will contain rdiag except
                                              ! for the first 2 fields (type and
                                              ! subtype)

      !---------------------------------------------
      ! Retrieve data from the linked list and load
      !   into the cdiag and rdiag_m2 arrays
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


      if(iscater ==1) then
         files=trim(fileo)//'_'//trim(subtype)//'.scater.'//trim(run)
         open(51,file=files,form='unformatted')
         write(51) nobs,nreal_m2
         write(51) rdiag_m2
         close(51)
      endif

      if (igrads ==1) then 
         filegrads=trim(fileo)//'_'//trim(subtype)//'.grads.'//trim(run)

         open(21,file=filegrads,form='unformatted')


         ilat      = idx_obs_lat -2        ! modified position of lat
         ilon      = idx_obs_lon -2        ! modified position of lon
         ipres     = idx_pres -2           ! modified position of pressure
         itime     = idx_time -2           ! modified position of relative time
         iweight   = idx_rwgt -2           ! modofied position of weight


         ! ####################################
         !  remove any duplicate data
         !
         call rm_dups( rdiag_m2,nobs,nreal_m2,ilat,ilon,ipres,itime,iweight,ndup )

         ctr=0
         ii=0
         do  i=1,nobs
            if(rdiag_m2(iweight,i) >0.0 ) then

               stid=cdiag(i)
               rlat=rdiag_m2(ilat,i)
               rlon=rdiag_m2(ilon,i)
               rp=rdiag_m2(ipres,i)       ! bug was here -- rp must be real, not integer

               k=0
               k=getlev(rp,plev,nlev)
               if(k /=0)  then
                  write(21) stid,rlat,rlon,rtim,1,0

                  write(21) plev(k),(rdiag_m2(j,i),j=3,nreal_m2)
                  ctr = ctr + 1
               endif   

            endif
         enddo
   
         stidend='        '
         write(21) stidend,xlat0,xlon0,rtim,nlev0,nflag0 
         close(21)

         print *, 'num recs written to GrADS file = ', ctr

         file_nobs=trim(fileo)//'_'//trim(subtype)//'.nobs.'//trim(run)
         open( 32, file=file_nobs, form='formatted', status='new' )
         write(32,*) trim(fileo), ',', trim(subtype), ',', ctr
         close( 32 )

      endif

      deallocate(rdiag_m2,cdiag)

   else
      write(6,*) "No output file generated, nobs, igrads = ", nobs, igrads
   endif

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

