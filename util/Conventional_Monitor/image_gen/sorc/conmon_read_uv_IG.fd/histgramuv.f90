!-----------------------------------------------------------------
!  subroutine histuv
!
!-----------------------------------------------------------------

subroutine histuv( dtype, rmodnbc, nchan, nxdata, ndata, rmin, rmax, &
                   rlev, fileo, ncount_vqc, ncount_gros, fileo2, wind )

   implicit none

   !-------------
   ! inteface 
   !
   character*15,                 intent(in) :: dtype
   real,dimension(nchan,nxdata), intent(in) :: rmodnbc
   integer,                      intent(in) :: nchan
   integer,                      intent(in) :: nxdata
   real,                         intent(in) :: rmin, rmax, rlev
   character*50,                 intent(in) :: fileo
   integer,dimension(nchan),     intent(in) :: ncount_vqc,ncount_gros
   character*50,                 intent(in) :: fileo2
   character*1,                  intent(in) :: wind     ! u or v

   !-------------
   ! local vars
   !
   real,dimension(nchan) :: rmean,rstd 
   integer,dimension(nchan) :: nmean,nobs,ndata
   integer i,j,k,nlev

   real,dimension(nchan,400) :: ys
   real,dimension(400) :: xs
   real,dimension(401) :: xs2
   real maxf,sqr2

   character*20        :: grads_info_file 



   print *, '--> histuv'
   print *, 'dtype = ', dtype
   print *, 'fileo = ', fileo
   print *, 'fileo2 = ', fileo2

   grads_info_file='grads_info_' // wind

   sqr2=1.414213562

   nlev=(rmax-rmin)/rlev
   xs(1)=rmin+rlev/2.0
   xs2(1)=rmin

   open (11,file=fileo2,form='formatted')

   write(11,100) 'nlev=', nlev

100 format(A,i5)

   write(11,110) ndata(1),ndata(2),ndata(3)

110 format(3i5)

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
            if(rmodnbc(j,i) >=rmin .and. rmodnbc(j,i) <=rmax ) then
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
   write(11,200) dtype
   do j=1,nchan
      write(11,210) nobs(j),ncount_vqc(j),ncount_gros(j),rstd(j),rmean(j)
   enddo

200 format(a12,'no. std mean ')
210 format(3i10,2f10.3)


   open( unit = 15, file = grads_info_file, form = 'formatted', status = 'new' )
   write( 15, * ) 'nlev        = ', nlev

   write( 15, * ) 'all_ncount  = ', nobs(1)
   write( 15, * ) 'all_rejqc   = ', ncount_vqc(1)
   write( 15, * ) 'all_gros    = ', ncount_gros(1)
   write( 15, 220 ) 'all_std     = ', rstd(1)
   write( 15, 220 ) 'all_mean    = ', rmean(1)

   write( 15, * ) 'ioqc_ncount = ', nobs(2)
   write( 15, * ) 'ioqc_rejqc  = ', ncount_vqc(2)
   write( 15, * ) 'ioqc_gros   = ', ncount_gros(2)
   write( 15, 220 ) 'ioqc_std    = ', rstd(2)
   write( 15, 220 ) 'ioqc_mean   = ', rmean(2)

   write( 15, * ) 'mon_ncount  = ', nobs(3)
   write( 15, * ) 'mon_rejqc   = ', ncount_vqc(3)
   write( 15, * ) 'mon_gros    = ', ncount_gros(3)
   write( 15, 220 ) 'mon_std     = ', rstd(3)
   write( 15, 220 ) 'mon_mean    = ', rmean(3)
220 format( a14, f6.3 )

   close( 15 )


!!--------------------------------------------------------
!!  NOTE:  This section was commented out when
!!         I took over maintenance of the ConMon.  I'm 
!!         intentionally leaving this in place in case
!!         transforming the frequency becomes desirable
!!         at some point.  Knowing how to do that might
!!         prove useful.
!!--------------------------------------------------------
!   transform the frequecy so that gaussin distribution became a stright line

!   print *, 'maxf ',maxf
!   do i=1,nlev
!      if(ys(i) >0.0) then
!         ys(i)=sqrt(-2.0*log(ys(i)/maxf))
!      else
!         ys(i)=0.0
!      endif
!      f(i)=abs((xs(i)-rmean)/rstd)
!   enddo

   open (10,file=fileo,form='unformatted',access='direct',recl=nlev*4)

   write(10,rec=1) (xs(i),i=1,nlev)
   do j=1,nchan
      write(10,rec=2+j-1) (ys(j,i),i=1,nlev)
   enddo

   close(10)
   close(11)

   print *, '<-- histuv'
   return 
end

