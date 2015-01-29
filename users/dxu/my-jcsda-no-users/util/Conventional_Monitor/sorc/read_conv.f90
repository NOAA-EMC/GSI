!   intype  : the observarion type like t for tem., uv for wind
!   stype   : the observation sub type, like t120 uv220
!   twork   : the array to hold statistics for temperature: the first variable of 
!             array is vertical level, the second variable is the number of data type
!             the third variable tatistics variable: 1: the total number
!             2:the number of data rejected by variational qc
!             3:bias,4:rms, 5: penalty,6: variational penalty 
!             the fourth variable is region, the fifth variable is the data usuage type
!             1, used, 2, rejected, 3, monited

   subroutine read_conv(filein,mregion,nregion,np,ptop,pbot,ptopq,pbotq,&
              rlatmin,rlatmax,rlonmin,rlonmax,iotype_ps,iotype_q,&
              iotype_t,iotype_uv,varqc_ps,varqc_q,varqc_t,varqc_uv,&
              ntype_ps,ntype_q,ntype_t,ntype_uv,&
              iosubtype_ps,iosubtype_q,iosubtype_t,iosubtype_uv)

      implicit none

      integer mregion
      real(4),allocatable,dimension(:,:)  :: rdiag 
      character(8),allocatable,dimension(:)  :: cdiag 
      real(4),dimension(np) :: ptop,pbot,ptopq,pbotq
      real(4),dimension(np,100,6,nregion,3) :: twork,qwork,uwork,vwork,uvwork
      real(4),dimension(1,100,6,nregion,3) :: pswork
      real,dimension(mregion):: rlatmin,rlatmax,rlonmin,rlonmax

   integer,dimension(100) :: iotype_ps,iotype_q,iotype_t,iotype_uv
   integer,dimension(100) :: iosubtype_ps,iosubtype_q,iosubtype_uv,iosubtype_t
   real(4),dimension(100,2) :: varqc_ps,varqc_q,varqc_t,varqc_uv
   character(20) :: filein
   character(3) :: dtype

   integer nchar,nreal,ii,mype,idate,iflag,itype
   integer lunin,lunot,nreal1,nreal2,ldtype,intype
   integer ilat,ilon,ipress,iqc,iuse,imuse,iwgt,ierr1
   integer ierr2,ierr3,ipsobs,iqobs
   integer i,j,k,np,nregion,ltype,iregion,ntype_uv
   integer iobg,iobgu,iobgv,ntype_ps,ntype_q,ntype_t

   real(4) ::  bmiss
   
   data lunin / 11 /
   data lunot / 21 /
   data bmiss /-999.0/



  twork=0.0;qwork=0.0;uwork=0.0;vwork=0.0;uvwork=0.0
  pswork=0.0

  itype=1;ilat=3;ilon=4;ipress=6;iqc=9;iuse=11;imuse=12
  iwgt=13;ierr1=14;ierr2=15;ierr3=16;iobg=18;iobgu=18;iobgv=21
   
  

   open(lunin,file=filein,form='unformatted')  
   rewind(lunin)

    read(lunin) idate

    print *, 'idate=',idate 
    print *,ptop(1),ptop(5)
    print *,pbot(1),pbot(5)

    loopd: do  
         read(lunin,IOSTAT=iflag) dtype,nchar,nreal,ii,mype
         if( iflag /= 0 ) exit loopd
         print *, dtype,nchar,nreal,ii,mype
         allocate(cdiag(ii),rdiag(nreal,ii))
         read(lunin,IOSTAT=iflag) cdiag,rdiag
         if( iflag /= 0 ) exit loopd
         if(trim(dtype) == ' ps') then
!        print *, dtype,nchar,nreal,ii,mype
            call stascal(dtype,rdiag,nreal,ii,iotype_ps,varqc_ps,ntype_ps,&
                         pswork,uwork,vwork,1,ptop,pbot,nregion,mregion,&
                         rlatmin,rlatmax,rlonmin,rlonmax,iosubtype_ps)
         else if(trim(dtype) == '  q') then
            call stascal(dtype,rdiag,nreal,ii,iotype_q,varqc_q,ntype_q,&
                         qwork,uwork,vwork,np,ptopq,pbotq,nregion,mregion,&
                         rlatmin,rlatmax,rlonmin,rlonmax,iosubtype_q)
         else if(trim(dtype) == '  t') then
            call stascal(dtype,rdiag,nreal,ii,iotype_t,varqc_t,ntype_t,&
                         twork,uwork,vwork,np,ptop,pbot,nregion,mregion,&
                         rlatmin,rlatmax,rlonmin,rlonmax,iosubtype_t)
         else if(trim(dtype) == ' uv') then
            call stascal(dtype,rdiag,nreal,ii,iotype_uv,varqc_uv,ntype_uv,&
                         uvwork,uwork,vwork,np,ptop,pbot,nregion,mregion,&
                         rlatmin,rlatmax,rlonmin,rlonmax,iosubtype_uv)
         endif
          
        deallocate(cdiag,rdiag)
  enddo   loopd               !  ending read data do loop

    
 close(lunin)

   print *,'end of the loop'

   print *, twork(1,1,1,1,1),twork(1,2,1,1,1)

        
 do iregion=1,nregion
 do j=1,3
 do ltype=1,ntype_ps
    pswork(1,ntype_ps+1,1,iregion,j)=pswork(1,ntype_ps+1,1,iregion,j)+pswork(1,ltype,1,iregion,j)
    pswork(1,ntype_ps+1,2,iregion,j)=pswork(1,ntype_ps+1,2,iregion,j)+pswork(1,ltype,2,iregion,j)
    pswork(1,ntype_ps+1,3,iregion,j)=pswork(1,ntype_ps+1,3,iregion,j)+pswork(1,ltype,3,iregion,j)
    pswork(1,ntype_ps+1,4,iregion,j)=pswork(1,ntype_ps+1,4,iregion,j)+pswork(1,ltype,4,iregion,j)
    pswork(1,ntype_ps+1,5,iregion,j)=pswork(1,ntype_ps+1,5,iregion,j)+pswork(1,ltype,5,iregion,j)
    pswork(1,ntype_ps+1,6,iregion,j)=pswork(1,ntype_ps+1,6,iregion,j)+pswork(1,ltype,6,iregion,j)
   if(pswork(1,ltype,1,iregion,j) >=1.0) then
     pswork(1,ltype,3,iregion,j)=pswork(1,ltype,3,iregion,j)/pswork(1,ltype,1,iregion,j)
     pswork(1,ltype,4,iregion,j)=sqrt(pswork(1,ltype,4,iregion,j)/pswork(1,ltype,1,iregion,j))
     pswork(1,ltype,5,iregion,j)=pswork(1,ltype,5,iregion,j)/pswork(1,ltype,1,iregion,j)
     pswork(1,ltype,6,iregion,j)=pswork(1,ltype,6,iregion,j)/pswork(1,ltype,1,iregion,j)
   endif
 enddo
!!! for the total surface pressure statistics
 if(pswork(1,ntype_ps+1,1,iregion,j) >=1.0) then
   pswork(1,ntype_ps+1,3,iregion,j)=pswork(1,ntype_ps+1,3,iregion,j)/&
                                    pswork(1,ntype_ps+1,1,iregion,j)
   pswork(1,ntype_ps+1,4,iregion,j)=sqrt(pswork(1,ntype_ps+1,4,iregion,j)&
                                    /pswork(1,ntype_ps+1,1,iregion,j))
   pswork(1,ntype_ps+1,5,iregion,j)=pswork(1,ntype_ps+1,5,iregion,j)/&
                                    pswork(1,ntype_ps+1,1,iregion,j)
   pswork(1,ntype_ps+1,6,iregion,j)=pswork(1,ntype_ps+1,6,iregion,j)/&
                                    pswork(1,ntype_ps+1,1,iregion,j)
 endif
                                    
 do k=1,np
 do ltype=1,ntype_q
   qwork(k,ntype_q+1,1,iregion,j)=qwork(k,ntype_q+1,1,iregion,j)+qwork(k,ltype,1,iregion,j)
   qwork(k,ntype_q+1,2,iregion,j)=qwork(k,ntype_q+1,2,iregion,j)+qwork(k,ltype,2,iregion,j)
   qwork(k,ntype_q+1,3,iregion,j)=qwork(k,ntype_q+1,3,iregion,j)+qwork(k,ltype,3,iregion,j)
   qwork(k,ntype_q+1,4,iregion,j)=qwork(k,ntype_q+1,4,iregion,j)+qwork(k,ltype,4,iregion,j)
   qwork(k,ntype_q+1,5,iregion,j)=qwork(k,ntype_q+1,5,iregion,j)+qwork(k,ltype,5,iregion,j)
   qwork(k,ntype_q+1,6,iregion,j)=qwork(k,ntype_q+1,6,iregion,j)+qwork(k,ltype,6,iregion,j)
   if(qwork(k,ltype,1,iregion,j) >=1.0) then
     qwork(k,ltype,3,iregion,j)=qwork(k,ltype,3,iregion,j)/qwork(k,ltype,1,iregion,j)
     qwork(k,ltype,4,iregion,j)=sqrt(qwork(k,ltype,4,iregion,j)/qwork(k,ltype,1,iregion,j))
     qwork(k,ltype,5,iregion,j)=qwork(k,ltype,5,iregion,j)/qwork(k,ltype,1,iregion,j)
     qwork(k,ltype,6,iregion,j)=qwork(k,ltype,6,iregion,j)/qwork(k,ltype,1,iregion,j)
   endif
 enddo
 if(qwork(k,ntype_q+1,1,iregion,j) >=1.0) then
  qwork(k,ntype_q+1,3,iregion,j)=qwork(k,ntype_q+1,3,iregion,j)/&
                                 qwork(k,ntype_q+1,1,iregion,j)
  qwork(k,ntype_q+1,4,iregion,j)=sqrt(qwork(k,ntype_q+1,4,iregion,j)/&
                                 qwork(k,ntype_q+1,1,iregion,j))
  qwork(k,ntype_q+1,5,iregion,j)=qwork(k,ntype_q+1,5,iregion,j)/&
                                 qwork(k,ntype_q+1,1,iregion,j)
  qwork(k,ntype_q+1,6,iregion,j)=qwork(k,ntype_q+1,6,iregion,j)/&
                                 qwork(k,ntype_q+1,1,iregion,j)
 endif
 do ltype=1,ntype_t
   twork(k,ntype_t+1,1,iregion,j)=twork(k,ntype_t+1,1,iregion,j)+twork(k,ltype,1,iregion,j)
   twork(k,ntype_t+1,2,iregion,j)=twork(k,ntype_t+1,2,iregion,j)+twork(k,ltype,2,iregion,j)
   twork(k,ntype_t+1,3,iregion,j)=twork(k,ntype_t+1,3,iregion,j)+twork(k,ltype,3,iregion,j)
   twork(k,ntype_t+1,4,iregion,j)=twork(k,ntype_t+1,4,iregion,j)+twork(k,ltype,4,iregion,j)
!if(j == 2) then
!   write(6,100) k,ltype,iregion,j,twork(k,ntype_t+1,4,iregion,j),twork(k,ltype,4,iregion,j),twork(k,ltype,1,iregion,j),&
!                twork(k,ntype_t+1,1,iregion,j) 
!100 format(4i6,4f12.4)
!endif
   twork(k,ntype_t+1,5,iregion,j)=twork(k,ntype_t+1,5,iregion,j)+twork(k,ltype,5,iregion,j)
   twork(k,ntype_t+1,6,iregion,j)=twork(k,ntype_t+1,6,iregion,j)+twork(k,ltype,6,iregion,j)
   if(twork(k,ltype,1,iregion,j) >=1.0) then
     twork(k,ltype,3,iregion,j)=twork(k,ltype,3,iregion,j)/twork(k,ltype,1,iregion,j)
     twork(k,ltype,4,iregion,j)=sqrt(twork(k,ltype,4,iregion,j)/twork(k,ltype,1,iregion,j))
     twork(k,ltype,5,iregion,j)=twork(k,ltype,5,iregion,j)/twork(k,ltype,1,iregion,j)
     twork(k,ltype,6,iregion,j)=twork(k,ltype,6,iregion,j)/twork(k,ltype,1,iregion,j)
   endif
 enddo
 if(twork(k,ntype_t+1,1,iregion,j) >=1.0) then
  twork(k,ntype_t+1,3,iregion,j)=twork(k,ntype_t+1,3,iregion,j)/&
                                 twork(k,ntype_t+1,1,iregion,j)
  twork(k,ntype_t+1,4,iregion,j)=sqrt(twork(k,ntype_t+1,4,iregion,j)/&
                                 twork(k,ntype_t+1,1,iregion,j))
  twork(k,ntype_t+1,5,iregion,j)=twork(k,ntype_t+1,5,iregion,j)/&
                                 twork(k,ntype_t+1,1,iregion,j)
  twork(k,ntype_t+1,6,iregion,j)=twork(k,ntype_t+1,6,iregion,j)/&
                                 twork(k,ntype_t+1,1,iregion,j)
 endif
 do ltype=1,ntype_uv
   uvwork(k,ntype_uv+1,1,iregion,j)=uvwork(k,ntype_uv+1,1,iregion,j)+uvwork(k,ltype,1,iregion,j)
   uvwork(k,ntype_uv+1,2,iregion,j)=uvwork(k,ntype_uv+1,2,iregion,j)+uvwork(k,ltype,2,iregion,j)
   uvwork(k,ntype_uv+1,3,iregion,j)=uvwork(k,ntype_uv+1,3,iregion,j)+uvwork(k,ltype,3,iregion,j)
   uvwork(k,ntype_uv+1,4,iregion,j)=uvwork(k,ntype_uv+1,4,iregion,j)+uvwork(k,ltype,4,iregion,j)
   uvwork(k,ntype_uv+1,5,iregion,j)=uvwork(k,ntype_uv+1,5,iregion,j)+uvwork(k,ltype,5,iregion,j)
   uvwork(k,ntype_uv+1,6,iregion,j)=uvwork(k,ntype_uv+1,6,iregion,j)+uvwork(k,ltype,6,iregion,j)
   uwork(k,ntype_uv+1,3,iregion,j)=uwork(k,ntype_uv+1,3,iregion,j)+uwork(k,ltype,3,iregion,j)
   uwork(k,ntype_uv+1,4,iregion,j)=uwork(k,ntype_uv+1,4,iregion,j)+uwork(k,ltype,4,iregion,j)
   vwork(k,ntype_uv+1,3,iregion,j)=vwork(k,ntype_uv+1,3,iregion,j)+vwork(k,ltype,3,iregion,j)
   vwork(k,ntype_uv+1,4,iregion,j)=vwork(k,ntype_uv+1,4,iregion,j)+vwork(k,ltype,4,iregion,j)
   if(uvwork(k,ltype,1,iregion,j) >=1.0) then
     uvwork(k,ltype,3,iregion,j)=uvwork(k,ltype,3,iregion,j)/uvwork(k,ltype,1,iregion,j)
     uvwork(k,ltype,4,iregion,j)=sqrt(uvwork(k,ltype,4,iregion,j)/uvwork(k,ltype,1,iregion,j))
     uvwork(k,ltype,5,iregion,j)=uvwork(k,ltype,5,iregion,j)/uvwork(k,ltype,1,iregion,j)
     uvwork(k,ltype,6,iregion,j)=uvwork(k,ltype,6,iregion,j)/uvwork(k,ltype,1,iregion,j)
     uwork(k,ltype,1,iregion,j)=uvwork(k,ltype,1,iregion,j)
     vwork(k,ltype,1,iregion,j)=uvwork(k,ltype,1,iregion,j)
     uwork(k,ltype,2,iregion,j)=uvwork(k,ltype,2,iregion,j)
     vwork(k,ltype,2,iregion,j)=uvwork(k,ltype,2,iregion,j)
     uwork(k,ltype,3,iregion,j)=uwork(k,ltype,3,iregion,j)/uvwork(k,ltype,1,iregion,j)
     uwork(k,ltype,4,iregion,j)=sqrt(uwork(k,ltype,4,iregion,j)/uvwork(k,ltype,1,iregion,j))
     vwork(k,ltype,3,iregion,j)=vwork(k,ltype,3,iregion,j)/uvwork(k,ltype,1,iregion,j)
     vwork(k,ltype,4,iregion,j)=sqrt(vwork(k,ltype,4,iregion,j)/uvwork(k,ltype,1,iregion,j))
   endif
 enddo

if(uvwork(k,ntype_uv+1,1,iregion,j) >=1.0) then
 uvwork(k,ntype_uv+1,3,iregion,j)=uvwork(k,ntype_uv+1,3,iregion,j)&
                                  /uvwork(k,ntype_uv+1,1,iregion,j)
 uvwork(k,ntype_uv+1,4,iregion,j)=sqrt(uvwork(k,ntype_uv+1,4,iregion,j)&
                                  /uvwork(k,ntype_uv+1,1,iregion,j))
 uvwork(k,ntype_uv+1,5,iregion,j)=uvwork(k,ntype_uv+1,5,iregion,j)&
                                  /uvwork(k,ntype_uv+1,1,iregion,j)
 uvwork(k,ntype_uv+1,6,iregion,j)=uvwork(k,ntype_uv+1,6,iregion,j)&
                                  /uvwork(k,ntype_uv+1,1,iregion,j)
 uwork(k,ntype_uv+1,1,iregion,j)=uvwork(k,ntype_uv+1,1,iregion,j)
 uwork(k,ntype_uv+1,2,iregion,j)=uvwork(k,ntype_uv+1,2,iregion,j)
 vwork(k,ntype_uv+1,1,iregion,j)=uvwork(k,ntype_uv+1,1,iregion,j)
 vwork(k,ntype_uv+1,2,iregion,j)=uvwork(k,ntype_uv+1,2,iregion,j)

 uwork(k,ntype_uv+1,3,iregion,j)=uwork(k,ntype_uv+1,3,iregion,j)&
                                  /uwork(k,ntype_uv+1,1,iregion,j)
 uwork(k,ntype_uv+1,4,iregion,j)=sqrt(uwork(k,ntype_uv+1,4,iregion,j)&
                                  /uwork(k,ntype_uv+1,1,iregion,j))
 vwork(k,ntype_uv+1,3,iregion,j)=vwork(k,ntype_uv+1,3,iregion,j)&
                                  /vwork(k,ntype_uv+1,1,iregion,j)
 vwork(k,ntype_uv+1,4,iregion,j)=sqrt(vwork(k,ntype_uv+1,4,iregion,j)&
                                  /vwork(k,ntype_uv+1,1,iregion,j))
endif
   
enddo    !!! enddo k height
enddo    !!! enddo j, j=1 assimilated, j=2 rejected, j=3 monitored 
enddo    !!! enddo iregion region 


   print *, 'end of diag2grad subroutine'


    !!! open the grads output file


    !!! for surface pressure files

    open(21,file='ps_stas',form='unformatted')    

    do j=1,3
    do i=1,6
      write(21) ((pswork(1,ltype,i,iregion,j),ltype=1,ntype_ps+1),iregion=1,nregion)
    enddo
    enddo

    open(31,file='q_stas',form='unformatted')

    do j=1,3
    do i=1,6
    do k=1,np
     write(31) ((qwork(k,ltype,i,iregion,j),ltype=1,ntype_q+1),iregion=1,nregion)
    enddo
    enddo
    enddo

    open(41,file='t_stas',form='unformatted')
    do j=1,3
    do i=1,6
    do k=1,np
     write(41) ((twork(k,ltype,i,iregion,j),ltype=1,ntype_t+1),iregion=1,nregion)
    enddo
    enddo
    enddo

    write(6,900) (twork(k,1,1,1,1),k=1,np) 

900 format(13f10.1)

    open(51,file='u_stas',form='unformatted')
    do j=1,3
    do i=1,6
    do k=1,np
     write(51) ((uwork(k,ltype,i,iregion,j),ltype=1,ntype_uv+1),iregion=1,nregion)
    enddo
    enddo
    enddo

    open(61,file='v_stas',form='unformatted')
    do j=1,3
    do i=1,6
    do k=1,np
     write(61) ((vwork(k,ltype,i,iregion,j),ltype=1,ntype_uv+1),iregion=1,nregion)
    enddo
    enddo
    enddo
     
    open(71,file='uv_stas',form='unformatted')
    do j=1,3
    do i=1,6
    do k=1,np
     write(71) ((uvwork(k,ltype,i,iregion,j),ltype=1,ntype_uv+1),iregion=1,nregion)
    enddo
    enddo
    enddo


   close(21)
   close(31)
   close(41)
   close(51)
   close(61)
   close(71)

  return 
   end 
