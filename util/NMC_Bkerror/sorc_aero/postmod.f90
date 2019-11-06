module postmod

  use kinds, only: r_kind
  implicit none

  integer ndeg,nasm
  parameter(ndeg=6,nasm=560)

contains

 subroutine smoothlat(field,nlevs,degs)
   use variables,only: nlat
   implicit none

   real(r_kind),dimension(nlat,nlevs):: field
   real(r_kind),dimension(nlat):: field_sm
   real(r_kind),dimension(nlat,nlat):: weights
   real(r_kind) degs
   integer j,j2,k,nlevs

! get weights for smoothing in lat direction
   call get_weights(degs,weights)

! smooth the field array based on weights computed 
   do k=1,nlevs
     field_sm=0.0
     do j2=2,nlat-1
       do j=2,nlat-1
         field_sm(j)=field_sm(j)+weights(j,j2)*field(j2,k)
       end do
     end do

! redefine field to be smoothed 
     do j=2,nlat-1
       field(j,k)=field_sm(j)
     end do
     field(1,k)=field(2,k)
     field(nlat,k)=field(nlat-1,k)

   end do   !end do loop over levs

 return
 end subroutine smoothlat


 subroutine get_weights(degs,wsmooth)
   use variables,only: nlat,deg2rad,rlats
   implicit none

   real(r_kind),dimension(nlat):: rnorm,slat
   real(r_kind),dimension(nlat,nlat):: wsmooth

   real(r_kind) sum,errmax,degs,arg,denom
   integer j,jj

! use difference in sin(lat) to calculate weighting
   do j=2,nlat-1
     slat(j)=sin(rlats(j))
   end do
   denom=1.0/(deg2rad*degs)
   rnorm=0. 
   do j=2,nlat-1
     do jj=2,nlat-1
       arg=.5*(denom*(slat(j)-slat(jj)))**2
       wsmooth(j,jj)=exp(-arg)
       rnorm(j)=rnorm(j)+wsmooth(j,jj)
     end do
   end do

   do j=2,nlat-1
     rnorm(j)=1./rnorm(j)
   end do

   errmax=0.
   do j=2,nlat-1
     sum=0.0
     do jj=2,nlat-1
       wsmooth(j,jj)=rnorm(j)*wsmooth(j,jj)
       sum=sum+wsmooth(j,jj)
     end do
     errmax=max(abs(sum-1._r_kind),errmax)
   end do

 return
 end subroutine get_weights

 subroutine writefiles_aerosol
   use variables,only:d1var,d2var,d3var,d4var,d5var,s1var,s2var,s3var,s4var, &
       so4var,oc1var,oc2var,bc1var,bc2var,d1hln,d2hln,d3hln,d4hln,d5hln, &
       s1hln,s2hln,s3hln,s4hln,so4hln,oc1hln,oc2hln,bc1hln,bc2hln, &
       d1vln,d2vln,d3vln,d4vln,d5vln,s1vln,s2vln,s3vln,s4vln, &
       so4vln,oc1vln,oc2vln,bc1vln,bc2vln,nlat,nlon,nsig
   use kinds, only: r_single,r_double
!   use sstmod
   implicit none

!  Single precision variables for visualization
   real(r_single),allocatable,dimension(:,:,:):: stdev3d4,hscale3d4,vscale3d4

   integer i,j,k,m,outf,ncfggg,iret,isig,n
   character(255) grdfile
   character*5 var(40)
 
! Interpolate sst statistics
! go file for use in GSI analysis code
!   call create_sstvars(nlat,nlon)
!   call sst_stats

! allocate single precision arrays
   allocate(stdev3d4(nlat,nsig,14),hscale3d4(nlat,nsig,14),vscale3d4(nlat,nsig,14))

! Load single precision arrays for visualization
   do k=1,nsig
     do i=1,nlat
       stdev3d4(i,k,1)=sqrt(d1var(i,k))
       stdev3d4(i,k,2)=sqrt(d2var(i,k))
       stdev3d4(i,k,3)=sqrt(d3var(i,k))
       stdev3d4(i,k,4)=sqrt(d4var(i,k))
       stdev3d4(i,k,5)=sqrt(d5var(i,k))
       stdev3d4(i,k,6)=sqrt(s1var(i,k))
       stdev3d4(i,k,7)=sqrt(s2var(i,k))
       stdev3d4(i,k,8)=sqrt(s3var(i,k))
       stdev3d4(i,k,9)=sqrt(s4var(i,k))
       stdev3d4(i,k,10)=sqrt(so4var(i,k))
       stdev3d4(i,k,11)=sqrt(oc1var(i,k))
       stdev3d4(i,k,12)=sqrt(oc2var(i,k))
       stdev3d4(i,k,13)=sqrt(bc1var(i,k))
       stdev3d4(i,k,14)=sqrt(bc2var(i,k))

       hscale3d4(i,k,1)=d1hln(i,k)
       hscale3d4(i,k,2)=d2hln(i,k)
       hscale3d4(i,k,3)=d3hln(i,k)
       hscale3d4(i,k,4)=d4hln(i,k)
       hscale3d4(i,k,5)=d5hln(i,k)
       hscale3d4(i,k,6)=s1hln(i,k)
       hscale3d4(i,k,7)=s2hln(i,k)
       hscale3d4(i,k,8)=s3hln(i,k)
       hscale3d4(i,k,9)=s4hln(i,k)
       hscale3d4(i,k,10)=so4hln(i,k)
       hscale3d4(i,k,11)=oc1hln(i,k)
       hscale3d4(i,k,12)=oc2hln(i,k)
       hscale3d4(i,k,13)=bc1hln(i,k)
       hscale3d4(i,k,14)=bc2hln(i,k)

       vscale3d4(i,k,1)=d1vln(i,k)
       vscale3d4(i,k,2)=d2vln(i,k)
       vscale3d4(i,k,3)=d3vln(i,k)
       vscale3d4(i,k,4)=d4vln(i,k)
       vscale3d4(i,k,5)=d5vln(i,k)
       vscale3d4(i,k,6)=s1vln(i,k)
       vscale3d4(i,k,7)=s2vln(i,k)
       vscale3d4(i,k,8)=s3vln(i,k)
       vscale3d4(i,k,9)=s4vln(i,k)
       vscale3d4(i,k,10)=so4vln(i,k)
       vscale3d4(i,k,11)=oc1vln(i,k)
       vscale3d4(i,k,12)=oc2vln(i,k)
       vscale3d4(i,k,13)=bc1vln(i,k)
       vscale3d4(i,k,14)=bc2vln(i,k)
     end do
   end do

! write out files;
!   outf=45
!   open(outf,file='gsir4.berror_stats',form='unformatted')
!   rewind outf
!   write(outf) nsig,nlat,&
!               sfvar4,vpvar4,tvar4,qvar4,nrhvar4,ozvar4,cvar4,psvar4,&
!               sfhln4,vphln4,thln4,qhln4,ozhln4,chln4,pshln4,&
!               sfvln4,vpvln4,tvln4,qvln4,ozvln4,cvln4,&
!               tcon4,vpcon4,pscon4,&
!               varsst4,corlsst4
!   close(outf)

  var=' '
  var(1)='dust1'
  var(2)='dust2'
  var(3)='dust3'
  var(4)='dust4'
  var(5)='dust5'
  var(6)='seas1'
  var(7)='seas2'
  var(8)='seas3'
  var(9)='seas4'
  var(10)='sulf'
  var(11)='oc1'
  var(12)='oc2'
  var(13)='bc1'
  var(14)='bc2'

! write out files;
   outf=45
   open(outf,file='gsir4.berror_stats.gcv',form='unformatted')
   rewind outf
   write(outf) nsig,nlat,nlon

   do i=1,14
      write(6,*) i,var(i),nsig
      write(outf) var(i),nsig
      write(outf) stdev3d4(:,:,i)
      write(outf) hscale3d4(:,:,i)
      write(outf) vscale3d4(:,:,i)
   end do
   
!   write(201,*) stdev3d4 
!   write(202,*) hscale3d4
!   write(203,*) vscale3d4
  
   close(outf)
   
   do n=1,14
      do k=1,nsig
        do i=1,nlat
           vscale3d4(i,k,n)=1./vscale3d4(i,k,n)
        end do
      end do
   end do

! CREATE SINGLE PRECISION BYTE-ADDRESSABLE FILE FOR GRADS
! OF LATIDUDE DEPENDENT VARIABLES
   grdfile='bgstats_sp.grd'
   ncfggg=len_trim(grdfile)
   call baopenwt(22,grdfile(1:ncfggg),iret)
   do n=1,14
      call wryte(22,4*nlat*nsig,stdev3d4(:,:,n))
   end do
   do n=1,14
      call wryte(22,4*nlat*nsig,hscale3d4(:,:,n))
   end do
   do n=1,14
      call wryte(22,4*nlat*nsig,vscale3d4(:,:,n))
   end do
   call baclose(22,iret)

! CREATE SINGLE PRECISION BYTE-ADDRESSABLE FILE FOR GRADS
! OF SST STATISTICS
   deallocate(stdev3d4,hscale3d4,vscale3d4)

!  call destroy_sstvars

 return
 end subroutine writefiles_aerosol

end module postmod
