module postmod

   use kinds, only: r_kind,r_single

   implicit none

   integer,parameter :: ndeg=6,nasm=560

contains

subroutine smoothlat(field,nlevs,degs)

   use variables,only: nlat

   implicit none

   real(r_kind),dimension(nlat,nlevs):: field
   real(r_kind),dimension(nlat):: field_sm
   real(r_kind),dimension(nlat,nlat):: weights
   real(r_kind) :: degs
   integer :: j,j2,k,nlevs

   ! get weights for smoothing in lat direction
   call get_weights(degs,weights)

   ! smooth the field array based on weights computed 
   do k=1,nlevs

      field_sm=0.0
      do j2=2,nlat-1
         do j=2,nlat-1
            field_sm(j)=field_sm(j)+weights(j,j2)*field(j2,k)
         enddo
      enddo

      ! redefine field to be smoothed 
      do j=2,nlat-1
         field(j,k)=field_sm(j)
      enddo
      field(1,k)=field(2,k)
      field(nlat,k)=field(nlat-1,k)

   enddo ! do k=1,nlevs

   return
end subroutine smoothlat

subroutine get_weights(degs,wsmooth)

   use variables,only: nlat,deg2rad,rlats

   implicit none

   real(r_kind),dimension(nlat) :: rnorm,slat
   real(r_kind),dimension(nlat,nlat) :: wsmooth

   real(r_kind) :: tmp_sum,errmax,degs,arg,denom
   integer :: j,jj

   ! use difference in sin(lat) to calculate weighting
   do j=2,nlat-1
      slat(j)=sin(rlats(j))
   enddo
   denom=1.0_r_kind / (deg2rad*degs)
   rnorm=0.0_r_kind
   do j=2,nlat-1
      do jj=2,nlat-1
         arg = 0.5_r_kind*(denom*(slat(j)-slat(jj)))**2
         wsmooth(j,jj)=exp(-arg)
         rnorm(j)=rnorm(j)+wsmooth(j,jj)
      enddo
   enddo

   do j=2,nlat-1
      rnorm(j)=1.0_r_kind/rnorm(j)
   enddo

   errmax=0.0_r_kind
   do j=2,nlat-1
      tmp_sum=0.0_r_kind
      do jj=2,nlat-1
         wsmooth(j,jj) = rnorm(j)*wsmooth(j,jj)
         tmp_sum = tmp_sum + wsmooth(j,jj)
      enddo
      errmax=max(abs(tmp_sum-1.0_r_kind),errmax)
   enddo

   return
end subroutine get_weights

subroutine writefiles

   use variables,only: sfvar,vpvar,tvar,qvar,ozvar,cvar,psvar,sfhln,vphln,&
       thln,qhln,ozhln,chln,pshln,sfvln,vpvln,tvln,qvln,ozvln,cvln,tcon,vpcon,pscon,&
       nlat,nlon,nsig,nrhvar
   use sstmod

   implicit none

   ! Single precision variables for visualization
   real(r_single),allocatable,dimension(:,:,:) :: tcon4
   real(r_single),allocatable,dimension(:,:,:) :: stdev3d4,hscale3d4,vscale3d4
   real(r_single),allocatable,dimension(:,:) :: nrhvar4,vpcon4,pscon4,pscon4_tmp
   real(r_single),allocatable,dimension(:,:) :: varsst4,corlsst4
   real(r_single),allocatable,dimension(:) :: psvar4,pshln4
   real(r_kind),dimension(nlat) :: slat,glat

   integer :: i,j,k,m,outf,ncfggg,iret,isig,n
   character(len=255) :: grdfile
   character(len=5) :: var(40)
 
   ! Interpolate sst statistics
   ! go file for use in GSI analysis code
   call create_sstvars(nlat,nlon)
   call sst_stats

   ! allocate single precision arrays
   allocate(stdev3d4(nlat,nsig,6),hscale3d4(nlat,nsig,6),vscale3d4(nlat,nsig,6))
   allocate(nrhvar4(nlat,nsig))
   allocate(pscon4(nlat,nsig),vpcon4(nlat,nsig))
   allocate(pscon4_tmp(nlat,nsig))
   allocate(varsst4(nlat,nlon),corlsst4(nlat,nlon))
   allocate(tcon4(nlat,nsig,nsig))
   allocate(psvar4(nlat),pshln4(nlat))

   ! Load single precision arrays for visualization
   stdev3d4(:,:,1) = real(sqrt(sfvar),r_single)
   stdev3d4(:,:,2) = real(sqrt(vpvar),r_single)
   stdev3d4(:,:,3) = real(sqrt(tvar),r_single)
   stdev3d4(:,:,4) = real(sqrt(qvar),r_single)
   stdev3d4(:,:,5) = real(sqrt(ozvar),r_single)
   stdev3d4(:,:,6) = real(sqrt(cvar),r_single)

   nrhvar4 = real(sqrt(nrhvar))

   hscale3d4(:,:,1) = real(sfhln,r_single)
   hscale3d4(:,:,2) = real(vphln,r_single)
   hscale3d4(:,:,3) = real(thln,r_single)
   hscale3d4(:,:,4) = real(qhln,r_single)
   hscale3d4(:,:,5) = real(ozhln,r_single)
   hscale3d4(:,:,6) = real(chln,r_single)

   vscale3d4(:,:,1) = real(sfvln,r_single)
   vscale3d4(:,:,2) = real(vpvln,r_single)
   vscale3d4(:,:,3) = real(tvln,r_single)
   vscale3d4(:,:,4) = real(qvln,r_single)
   vscale3d4(:,:,5) = real(ozvln,r_single)
   vscale3d4(:,:,6) = real(cvln,r_single)

   psvar4 = real(sqrt(psvar),r_single)
   pshln4 = real(pshln,      r_single)

   varsst4  = real(varsst, r_single)
   corlsst4 = real(corlsst,r_single)

   tcon4 = real(tcon,r_single)

   pscon4 = real(pscon,r_single)
   vpcon4 = real(vpcon,r_single)

   ! write out files;
!   outf=45
!   open(outf,file='gsir4.berror_stats',form='unformatted')
!   rewind outf
!   write(outf) nsig,nlat,&
!         sfvar4,vpvar4,tvar4,qvar4,nrhvar4,ozvar4,cvar4,psvar4,&
!         sfhln4,vphln4,thln4,qhln4,ozhln4,chln4,pshln4,&
!         sfvln4,vpvln4,tvln4,qvln4,ozvln4,cvln4,&
!         tcon4,vpcon4,pscon4,&
!         varsst4,corlsst4
!   close(outf)

   var=' '
   var(1) = 'sf'
   var(2) = 'vp'
   var(3) = 't'
   var(4) = 'q'
   var(5) = 'oz'
   var(6) = 'cw'
   var(7) = 'ps'
   var(8) = 'sst'

   pscon4_tmp = pscon4
   do k=2,nsig
      pscon4_tmp(:,k) = 0.0_r_single
   enddo

   ! write out files;
   outf=45
   open(outf,file='gsir4.berror_stats.gcv',form='unformatted')
   rewind outf
   write(outf) nsig,nlat,nlon
   write(outf) tcon4,vpcon4,pscon4_tmp

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
   enddo
   
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
   
   vscale3d4 = 1.0_r_single / vscale3d4

   call splat(4,nlat,slat,glat)
   slat = 180.0_r_single / acos(-1.0_r_single) * asin(slat(nlat:1:-1))

   pscon4_tmp = abs(pscon4)
   pscon4_tmp = ( pscon4_tmp + pscon4_tmp(nlat:1:-1,:) ) / 2.0_r_single
   do i=1,nlat
      pscon4_tmp(i,:) = pscon4_tmp(i,:) * sin(slat(i))
      pscon4_tmp(i,:) = pscon4_tmp(i,:) / sum(pscon4_tmp(i,:))
   enddo

   outf=45
   open(outf,file='gsir4.pscon_stats.gcv',form='unformatted')
   rewind outf
   write(outf) nsig,nlat
   write(outf) pscon4_tmp
   close(outf)

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

   ! ALSO CREATE GRADS CTL FILE
   open(24,file='bgstats_sp.ctl',form='formatted',status='replace',iostat=iret)
   write(24,'("DSET ",a)') trim(grdfile)
   write(24,'("UNDEF -9.99E+33")')
   write(24,'("TITLE bgstats_sp")')
   write(24,'("OPTIONS yrev")')
   write(24,'("XDEF 1 LINEAR 1 1")')
   write(24,'("YDEF",i6," LEVELS")') nlat
   write(24,'(5f12.6)') slat
   write(24,'("ZDEF",i6," LINEAR 1 1")') nsig
   write(24,'("TDEF",i6,1x,"LINEAR",1x,"00Z01Jan2000",1x,i3,"hr")') 1,12
   write(24,'("VARS",i6)') 87
   write(24,'("SF    ",i3," 0 SF VAR")') nsig
   write(24,'("VP    ",i3," 0 VP VAR")') nsig
   write(24,'("T     ",i3," 0 T  VAR")') nsig
   write(24,'("Q     ",i3," 0 Q  VAR")') nsig
   write(24,'("RH    ",i3," 0 RH VAR")') nsig
   write(24,'("OZ    ",i3," 0 OZ VAR")') nsig
   write(24,'("CW    ",i3," 0 CW VAR")') nsig
   write(24,'("PS    ",i3," 0 PS VAR")') 1
   write(24,'("HSF   ",i3," 0 SF HCOR")') nsig
   write(24,'("HVP   ",i3," 0 VP HCOR")') nsig
   write(24,'("HT    ",i3," 0 T  HCOR")') nsig
   write(24,'("HQ    ",i3," 0 Q  HCOR")') nsig
   write(24,'("HOZ   ",i3," 0 OZ HCOR")') nsig
   write(24,'("HCW   ",i3," 0 CW HCOR")') nsig
   write(24,'("HPS   ",i3," 0 PS HCOR")') 1
   write(24,'("VSF   ",i3," 0 SF VCOR")') nsig
   write(24,'("VVP   ",i3," 0 VP VCOR")') nsig
   write(24,'("VT    ",i3," 0 T  VCOR")') nsig
   write(24,'("VQ    ",i3," 0 Q  VCOR")') nsig
   write(24,'("VOZ   ",i3," 0 OZ VCOR")') nsig
   write(24,'("VCW   ",i3," 0 CW VCOR")') nsig
   do n=1,nsig
      write(24,'("AG",i3.3," ",i3," 0 BP Z",i3.3)') n,nsig,n
   enddo
   write(24,'("BG    ",i3," 0 STPS BP")') nsig
   write(24,'("WG    ",i3," 0 PSSF BP")') nsig
   write(24,'(a)') 'ENDVARS'
   close(24)

   ! CREATE SINGLE PRECISION BYTE-ADDRESSABLE FILE FOR GRADS
   ! OF SST STATISTICS
   grdfile='sststats_sp.grd'
   ncfggg=len_trim(grdfile)
   call baopenwt(23,grdfile(1:ncfggg),iret)
   call wryte(23,4*nlat*nlon,transpose(varsst4))
   call wryte(23,4*nlat*nlon,transpose(corlsst4))
   call baclose(23,iret)

   ! ALSO CREATE GRADS CTL FILE
   open(25,file='sststats_sp.ctl',form='formatted',status='replace',iostat=iret)
   write(25,'("DSET ",a)') trim(grdfile)
   write(25,'("UNDEF -9.99E+33")')
   write(25,'("TITLE bgstats_sp")')
   write(25,'("OPTIONS yrev")')
   write(25,'("XDEF 1 LINEAR 1 1")')
   write(25,'("YDEF",i6," LEVELS")') nlat
   write(25,'(5f12.6)') slat
   write(25,'("ZDEF 1 LINEAR 1 1")')
   write(25,'("TDEF",i6,1x,"LINEAR",1x,"00Z01Jan2000",1x,i3,"hr")') 1,12
   write(25,'("VARS",i6)') 2
   write(25,'("SST   ",i3," 0 SST VAR")') 1
   write(25,'("HSST  ",i3," 0 SST HCOR")') 1
   write(25,'(a)') 'ENDVARS'
   close(25)

   deallocate(stdev3d4,nrhvar4,hscale3d4,vscale3d4,&
              tcon4,vpcon4,pscon4,varsst4,corlsst4)
   deallocate(psvar4,pshln4)

   call destroy_sstvars

   return
end subroutine writefiles

end module postmod
