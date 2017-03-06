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

subroutine read_conv2grads(ctype,stype,intype,target_nreal,nobs,isubtype,subtype)

!       nreal1 is target_nreal 
!       nreal is file_nreal  (now renamed)

   implicit none

   real(4),allocatable,dimension(:,:)  :: rdiag 
   character(8),allocatable,dimension(:)  :: cdiag 

   character(3) :: dtype,ctype
   character(2) :: subtype 
   character(10)  :: stype,otype
   character(15)  :: fileo,fileo_subtyp

   integer nchar,file_nreal,i,ii,mype,idate,iflag,itype,iscater,igrads
   integer lunin,lunot,lunot2,target_nreal,ldtype,intype,isubtype,jsubtype
   integer nobs
 
   logical lexist
   data lunin / 11 /
   data lunot / 21 /
   data lunot2 / 22 /


   nobs=0
   print *,'target_nreal=',target_nreal
   print *,'intype=',intype
   
   fileo=trim(stype)//'_'//trim(subtype)//'.tmp' 
   print *,fileo

   print *,fileo
   open(lunin,file='conv_diag',form='unformatted')  
   open(lunot,file=fileo,form='unformatted')
   rewind(lunin)

   read(lunin) idate

   print *, 'idate=',idate 

   loopd: do  

      read(lunin,IOSTAT=iflag) dtype,nchar,file_nreal,ii,mype
      if( iflag /= 0 ) exit loopd

      print *, 'observation type:',dtype,' ctype=', ctype
      print *, 'file_nreal      :',file_nreal,' target_nreal=', target_nreal

      if( trim(dtype) == trim(ctype) .and. file_nreal /= target_nreal ) then
         print *, 'observation type:',dtype,' file_nreal=', file_nreal
         exit 
      endif

      allocate(cdiag(ii),rdiag(file_nreal,ii))
      read(lunin,IOSTAT=iflag) cdiag,rdiag
      if( iflag /= 0 ) exit loopd

      if(trim(dtype) /= trim(ctype))  then
         deallocate(cdiag,rdiag)
         cycle
      endif

      do i=1,ii
         itype = int(rdiag(1,i)) 
         jsubtype = int(rdiag(2,i)) 

         if(itype == intype .and. jsubtype ==isubtype)  then 
            nobs=nobs+1
            write(lunot) cdiag(i),rdiag(3:file_nreal,i)
         endif
      enddo

      deallocate(cdiag,rdiag)
   enddo   loopd               !  ending read data do loop

    
   close(lunin)
   close(lunot)

   print *, 'end of read_conv2grads subroutine'

   return 
end 
