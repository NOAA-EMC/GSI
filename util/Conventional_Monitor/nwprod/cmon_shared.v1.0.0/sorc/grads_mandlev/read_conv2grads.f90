!----------------------------------------------------------------------
!  read_conv2grads
!
!     This subroutine reads the diagnostic data from the cnvstat
!     file and writes it to the [stype]_[subtype].tmp file.
!
!     NOTE:  Next step in clean-up is to write directly to the 
!               GrADS output file rather than to the .tmp file,
!               which is then re-read and converted to the GrADS
!               output file.  Unnecessary I/O.
!     Note:
!        intype  : the observarion type like t for temp, uv for wind
!        stype   : the observation sub type, like t120 uv220
!----------------------------------------------------------------------

subroutine read_conv2grads(ctype,stype,intype,target_nreal,nobs,isubtype,subtype,list)

   use generic_list
   use data

   implicit none
  
   type(list_node_t), pointer :: list
   type(list_node_t), pointer :: next => null()
   type(data_ptr) :: ptr
 
   real(4),allocatable,dimension(:,:)     :: rdiag 
   character(8),allocatable,dimension(:)  :: cdiag 

   character(3)   :: dtype,ctype
   character(2)   :: subtype 
   character(10)  :: stype,otype
   character(15)  :: fileo,fileo_subtyp

   integer nchar,file_nreal,i,ii,mype,idate,iflag,itype,iscater,igrads
   integer lunin,lunot,target_nreal,ldtype,intype,isubtype,jsubtype
   integer nobs,idx

   data lunin / 11 /

   nobs=0
   print *, '--> read_conv2grads'

   open(lunin,file='conv_diag',form='unformatted')  
   rewind(lunin)

   read(lunin) idate
   print *, 'idate=',idate 

   loopd: do  

      read(lunin,IOSTAT=iflag) dtype,nchar,file_nreal,ii,mype
      if( iflag /= 0 ) exit loopd

      if( trim(dtype) == trim(ctype) .and. file_nreal /= target_nreal ) then
         print *, 'observation type:',dtype,' file_nreal=', file_nreal
         exit 
      endif

      if(trim(dtype) /= trim(ctype))  then
         cycle
      endif

      allocate(cdiag(ii),rdiag(file_nreal,ii))
      read(lunin,IOSTAT=iflag) cdiag,rdiag
      if( iflag /= 0 ) then
         deallocate( cdiag,rdiag )
         exit loopd
      end if

      do i=1,ii
         itype = int(rdiag(1,i)) 
         jsubtype = int(rdiag(2,i)) 

         if(itype == intype .and. jsubtype ==isubtype)  then 
            nobs=nobs+1

            !---------------------------------------------
            ! Allocate a new data element and load
            !
!            print *, 'Allocating new data element'
            allocate(ptr%p)
            ptr%p%stn_id = cdiag(i)
            do idx=1,max_rdiag_reals
               if( idx > file_nreal ) then
                  ptr%p%rdiag( idx ) = 0.00
               else
                  ptr%p%rdiag( idx ) = rdiag( idx,i )
               end if
            end do 

            if( nobs == 1 ) then
               !-------------------------------------------------
               ! Initialize the list with the first data element
               !
               call list_init(list, transfer(ptr, list_data))
!               print *, 'Initializing list with data:', ptr%p
               next => list

            else
               !-------------------------------------------------
               ! Insert subsequent nodes into the list
               !
!               print *, 'Inserting node with data:', ptr%p
               call list_insert(next, transfer(ptr, list_data))
               next => list_next(next)

            end if

         endif

      enddo

      deallocate(cdiag,rdiag)
   enddo   loopd               !  ending read data do loop

   close(lunin)

   print *, 'nobs added to list = ', nobs
   print *, '<-- read_conv2grads'

   return 
end 
