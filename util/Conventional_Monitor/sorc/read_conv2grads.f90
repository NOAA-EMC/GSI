!   intype  : the observarion type like t for tem., uv for wind
!   stype   : the observation sub type, like t120 uv220

subroutine read_conv2grads(ctype,stype,intype,nreal1,nreal2,nobs,isubtype,subtype)
!  the program read conventional diagnose files from gdas

   implicit none

   real(4),allocatable,dimension(:,:)  :: rdiag 
   character(8),allocatable,dimension(:)  :: cdiag 

   character(3) :: dtype,ctype
   character(2) :: subtype 
   character(10)  :: stype,otype
   character(15)  :: fileo,fileo_subtyp

   integer nchar,nreal,i,ii,mype,idate,iflag,itype,nreal_c,iscater,igrads
   integer lunin,lunot,lunot2,nreal1,nreal2,ldtype,intype,isubtype,jsubtype
   integer nobs
 
   logical lexist
   data lunin / 11 /
   data lunot / 21 /
   data lunot2 / 22 /


  nobs=0
  print *,'nreal1=',nreal1
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

         read(lunin,IOSTAT=iflag) dtype,nchar,nreal,ii,mype
         if( iflag /= 0 ) exit loopd
!         print *, dtype,nchar,nreal,ii,mype
         if( trim(dtype) == trim(ctype) .and. nreal1 /= nreal-2) then
            print *, 'observation type:',dtype,' nreal=',nreal
            exit 
          endif
          allocate(cdiag(ii),rdiag(nreal,ii))
          read(lunin,IOSTAT=iflag) cdiag,rdiag
          if( iflag /= 0 ) exit loopd
          if(trim(dtype) /= trim(ctype))  then
             deallocate(cdiag,rdiag)
             cycle
          endif
          do i=1,ii
             itype = int(rdiag(1,i)) 
             jsubtype = int(rdiag(2,i)) 
!            print *,'itype=',itype
             if(itype == intype .and. jsubtype ==isubtype)  then 
                nobs=nobs+1
                write(lunot) cdiag(i),rdiag(3:nreal,i)
              endif
           enddo
        deallocate(cdiag,rdiag)
  enddo   loopd               !  ending read data do loop

    
 close(lunin)

close(lunot)

   print *, 'end of read_conv2grads subroutine'

  return 
   end 
