!----------------------------------------------------------------------------
! subroutine hist
!
!    This subroutine calculates the histogram of each data source.
!----------------------------------------------------------------------------

subroutine hist( mtype, rmodnbc, nchan, nxdata, ndata, rmin, rmax, rlev, &
                 fileo, ncount_vqc, ncount_gros, grads_info_file )

   implicit none

   !--------------
   !  interface
   !
   character*15,                 intent(in) :: mtype
   real,dimension(nchan,nxdata), intent(in) :: rmodnbc
   integer,                      intent(in) :: nchan,nxdata
   integer,dimension(nchan),     intent(in) :: ndata
   real,                         intent(in) :: rmin,rmax,rlev
   character*50,                 intent(in) :: fileo
   integer,dimension(nchan),     intent(in) :: ncount_vqc,ncount_gros
   character*50,                 intent(in) :: grads_info_file
   
   !--------------
   !  local vars
   !
   integer nlev,i,j,k
   real sqr2

   real,dimension(nchan) :: rmean,rstd 
   integer,dimension(nchan) :: nmean,nobs

   real,dimension(nchan,400) :: ys
   real,dimension(400) :: xs
   real,dimension(401) :: xs2
   real maxf


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

   !-------------------------------------
   ! calculate the standard deviation
   !
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


 
   write(6,200) mtype
   do j=1,nchan
      write(6,210) nobs(j),ncount_vqc(j),ncount_gros(j),rstd(j),rmean(j)
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

   open (10,file=fileo,form='unformatted',access='direct',recl=nlev*4)
    
   write(10,rec=1) (xs(i),i=1,nlev)
   do j=1,nchan
      write(10,rec=2+j-1) (ys(j,i),i=1,nlev)
   enddo

   close(10)


   return 
end

