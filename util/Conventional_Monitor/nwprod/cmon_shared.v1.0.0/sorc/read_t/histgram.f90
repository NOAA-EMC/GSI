! The program is to calculate the histogram of each channel of GOES radiance


 subroutine hist(mtype,rmodnbc,nchan,nxdata,ndata,rmin,rmax,rlev,fileo,ncount_vqc,ncount_gros)

    implicit none

    integer nxdata,nchan,nlev,i,j,k
    real rlev,rmin,rmax,sqr2

    real,dimension(nchan,nxdata) :: rmodnbc
    real,dimension(nchan) :: rmean,rstd 
    integer,dimension(nchan) :: nmean,nobs,ndata,ncount_vqc,ncount_gros

    real,dimension(nchan,400) :: ys
    real,dimension(400) :: xs
    real,dimension(401) :: xs2
    real maxf

    character*50 fileo
    character*15 mtype

     sqr2=1.414213562

    nlev=(rmax-rmin)/rlev

     if(nlev >400) then
       print *,' too many bins, exceeding 400'
       return
      endif
     xs(1)=rmin+rlev/2.0
     xs2(1)=rmin

  print *, 'nlev=',nlev
  print *, 'nobs=',ndata(1),ndata(2),ndata(3)

    do i=2,nlev
      xs(i)=xs(i-1)+rlev
      xs2(i)=xs2(i-1)+rlev
    end do 

      xs2(nlev+1)=xs2(nlev)+rlev

    do i=1,nlev
    do j=1,nchan 
     ys(j,i)=0.0
    enddo
    enddo

     do k=1,nchan
      if(ndata(k) >1) then 
        do i=1, ndata(k)
         do j=1,nlev
            if( rmodnbc(k,i) >=xs2(j) .and. &
                rmodnbc(k,i) <xs2(j+1)) then
               ys(k,j)=ys(k,j) +1.0
               exit
            end if
         end do
       end do
     endif 
    enddo

! calculate the standard deviation

    rmean=0.0
    rstd=0.0
    nobs=0

   do j=1,nchan
    if(ndata(j) >1) then
     do i=1,ndata(j)
       if(rmodnbc(j,i) >=rmin .and. rmodnbc(j,i) <=rmax) then
          rmean(j)=rmean(j)+rmodnbc(j,i)
          rstd(j)=rstd(j)+rmodnbc(j,i)*rmodnbc(j,i)
          nobs(j)=nobs(j)+1
       endif
      enddo 
    endif
   enddo 
     
   do j=1,nchan
    if(nobs(j) >0) then
     if(nobs(j)==1) then
      rstd(j)=rstd(j)-rmean(j)*rmean(j)/nobs(j)
      rmean(j)=rmean(j)/nobs(j)
      rstd(j)=0.0
    else
     rstd(j)=rstd(j)-rmean(j)*rmean(j)/nobs(j)
      rmean(j)=rmean(j)/nobs(j)
      rstd(j)=sqrt(abs(rstd(j))/(nobs(j)-1))
    endif
   endif
   enddo

!  the frequency of real curve
 
!   print *, 'data type  no. std, mean ,rarea ',mtype,ndata,rstd,rmean,rarea
 write(6,200) mtype
 do j=1,nchan
   write(6,210) nobs(j),ncount_vqc(j),ncount_gros(j),rstd(j),rmean(j)
 enddo
200 format(a12,'no. std mean ')
210 format(3i10,2f10.3)

! write(6,220) (xs(1,i),i=1,100)
! write(6,230) (ys(1,i),i=1,100)
! write(6,230) (f(1,i),i=1,100)

!220 format(6e12.4)
!230 format(6e12.5)

!  trasform the frequecy so that gaussin distribution became a stright line

  
!   print *, 'maxf ',maxf

!   do i=1,nlev
!    if(ys(i) >0.0) then
!      ys(i)=sqrt(-2.0*log(ys(i)/maxf))
!    else
!      ys(i)=0.0
!    endif
!     f(i)=abs((xs(i)-rmean)/rstd)
!   enddo

    open (10,file=fileo,form='unformatted',access='direct',recl=nlev*4)
    
    write(10,rec=1) (xs(i),i=1,nlev)
    do j=1,nchan
     write(10,rec=2+j-1) (ys(j,i),i=1,nlev)
    enddo

   close(10)


    return 
    end

