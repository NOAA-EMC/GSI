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

 subroutine writefiles
   use variables,only: sfvar,vpvar,tvar,qvar,ozvar,cvar,psvar,sfhln,vphln,&
       thln,qhln,ozhln,chln,pshln,sfvln,vpvln,tvln,qvln,ozvln,cvln,tcon,vpcon,pscon,&
       nlat,nlon,nsig,nrhvar
   use kinds, only: r_single
   use sstmod
   implicit none

!  Single precision variables for visualization
   real(r_single),allocatable,dimension(:,:,:):: tcon4
   real(r_single),allocatable,dimension(:,:,:):: stdev3d4,hscale3d4,vscale3d4
   real(r_single),allocatable,dimension(:,:):: nrhvar4, &
        vpcon4,pscon4,varsst4,corlsst4,varsst4_grads,corlsst4_grads
   real(r_single),allocatable,dimension(:):: psvar4,pshln4

   integer i,j,k,m,outf,ncfggg,iret,isig,n
   character(255) grdfile
   character*5 var(40)
 
! Interpolate sst statistics
! go file for use in GSI analysis code
   call create_sstvars(nlat,nlon)
   call sst_stats

! allocate single precision arrays
   allocate(stdev3d4(nlat,nsig,6),hscale3d4(nlat,nsig,6),vscale3d4(nlat,nsig,6))
   allocate(nrhvar4(nlat,nsig))
   allocate(pscon4(nlat,nsig),vpcon4(nlat,nsig))
   allocate(varsst4(nlat,nlon),corlsst4(nlat,nlon))
   allocate(varsst4_grads(nlon,nlat),corlsst4_grads(nlon,nlat))
   allocate(tcon4(nlat,nsig,nsig))
   allocate(psvar4(nlat),pshln4(nlat))

! Load single precision arrays for visualization
   do k=1,nsig
     do i=1,nlat
       stdev3d4(i,k,1)=sqrt(sfvar(i,k))
       stdev3d4(i,k,2)=sqrt(vpvar(i,k))
       stdev3d4(i,k,3)=sqrt(tvar(i,k))
       nrhvar4(i,k)=sqrt(nrhvar(i,k))
       stdev3d4(i,k,4)=sqrt(qvar(i,k))
       stdev3d4(i,k,5)=sqrt(ozvar(i,k))
       stdev3d4(i,k,6)=sqrt(cvar(i,k))

       hscale3d4(i,k,1)=sfhln(i,k)
       hscale3d4(i,k,2)=vphln(i,k)
       hscale3d4(i,k,3)=thln(i,k)
       hscale3d4(i,k,4)=qhln(i,k)
       hscale3d4(i,k,5)=ozhln(i,k)
       hscale3d4(i,k,6)=chln(i,k)

       vscale3d4(i,k,1)=sfvln(i,k)
       vscale3d4(i,k,2)=vpvln(i,k)
       vscale3d4(i,k,3)=tvln(i,k)
       vscale3d4(i,k,4)=qvln(i,k)
       vscale3d4(i,k,5)=ozvln(i,k)
       vscale3d4(i,k,6)=cvln(i,k)
     end do
   end do
   do i=1,nlat
     psvar4(i)=sqrt(psvar(i))
     pshln4(i)=pshln(i)
   end do
   do j=1,nlon
     do i=1,nlat
       varsst4(i,j)=varsst(i,j)
       corlsst4(i,j)=corlsst(i,j)
       varsst4_grads(j,i)=varsst(i,j)
       corlsst4_grads(j,i)=corlsst(i,j)
     end do
   end do
   do m=1,nsig
     do k=1,nsig
       do i=1,nlat
         tcon4(i,k,m)=tcon(i,k,m)
       end do
     end do
   end do
   do k=1,nsig
     do i=1,nlat
       pscon4(i,k)=pscon(i,k)
       vpcon4(i,k)=vpcon(i,k)
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
  var(1)='sf'
  var(2)='vp'
  var(3)='t'
  var(4)='q'
  var(5)='oz'
  var(6)='cw'
  var(7)='ps'
  var(8)='sst'

! write out files;
   outf=45
   open(outf,file='gsir4.berror_stats.gcv',form='unformatted')
   rewind outf
   write(outf) nsig,nlat,nlon
   write(outf) tcon4,vpcon4,pscon4

   do i=1,6
      write(6,*) i,var(i),nsig
      write(outf) var(i),nsig
      if (i==4) then
         write(outf) stdev3d4(:,:,i),nrhvar4
         write(outf) hscale3d4(:,:,i)
         write(outf) vscale3d4(:,:,i)
      else
         write(outf) stdev3d4(:,:,i)
         write(outf) hscale3d4(:,:,i)
         write(outf) vscale3d4(:,:,i)
      end if
   end do
   
   isig=1
   i=7
   write(6,*) i,var(i),isig
   write(outf) var(7),isig
   write(outf) psvar4
   write(outf) pshln4

   i=8
   write(6,*) i,var(i),isig
   write(outf) var(8),isig
   write(outf) varsst4
   write(outf) corlsst4

   close(outf)
   
   do n=1,6
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
   call wryte(22,4*nlat*nsig,stdev3d4(:,:,1))
   call wryte(22,4*nlat*nsig,stdev3d4(:,:,2))
   call wryte(22,4*nlat*nsig,stdev3d4(:,:,3))
   call wryte(22,4*nlat*nsig,stdev3d4(:,:,4))
   call wryte(22,4*nlat*nsig,nrhvar4)
   call wryte(22,4*nlat*nsig,stdev3d4(:,:,5))
   call wryte(22,4*nlat*nsig,stdev3d4(:,:,6))
   call wryte(22,4*nlat,psvar4)
   do n=1,6
      call wryte(22,4*nlat*nsig,hscale3d4(:,:,n))
   end do
   call wryte(22,4*nlat,pshln4)
   do n=1,6
      call wryte(22,4*nlat*nsig,vscale3d4(:,:,n))
   end do
   call wryte(22,4*nlat*nsig*nsig,tcon4)
   call wryte(22,4*nlat*nsig,vpcon4)
   call wryte(22,4*nlat*nsig,pscon4)
   call baclose(22,iret)

! CREATE SINGLE PRECISION BYTE-ADDRESSABLE FILE FOR GRADS
! OF SST STATISTICS
   grdfile='sststats_sp.grd'
   ncfggg=len_trim(grdfile)
   call baopenwt(23,grdfile(1:ncfggg),iret)
   call wryte(23,4*nlat*nlon,varsst4_grads)
   call wryte(23,4*nlat*nlon,corlsst4_grads)
   call baclose(23,iret)

   deallocate(tcon4)
   deallocate(stdev3d4,nrhvar4,hscale3d4,vscale3d4,&
              tcon4,vpcon4,pscon4,varsst4,corlsst4,&
              varsst4_grads,corlsst4_grads)
   deallocate(psvar4,pshln4)

  call destroy_sstvars

 return
 end subroutine writefiles

end module postmod
