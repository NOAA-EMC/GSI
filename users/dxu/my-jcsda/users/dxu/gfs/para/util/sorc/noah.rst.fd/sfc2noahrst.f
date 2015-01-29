! % sfc2noahrst gdas1.t00z.sfcanl noahrst.gdas1.t00z.sfcanl
program gldas2sfc
  use sfcio_module
  implicit none
  integer narg,iargc
  integer(sfcio_intkind),parameter:: lusfc=11,luggg=51,luctl=52
  integer(sfcio_intkind):: irets
  character(255) inputsfc,outputsfc,inputgldas
  integer ninputsfc,noutputsfc,ninputgldas
  integer iret,nsfc,n,idrt,i,j,k,jj
  real,allocatable:: slon(:),slat(:),rlat(:),wlat(:)
  real,allocatable:: tsea(:,:),canopy(:,:),snwdph(:,:),sheleg(:,:),ch(:,:),land(:,:)
  real,allocatable:: smc(:,:,:),stc(:,:,:),slc(:,:,:)
  real :: sldph(4) = (/100.,300.,600.,1000./)
  type(sfcio_head):: head
  type(sfcio_data):: data
  narg=iargc()
  if(narg.ne.2) then
     print*, "sfc2noahrst gdas1.t00z.sfcanl noahrst.gdas1.t00z.sfcanl"
     call errexit(1)
  endif

! OUTPUT MERGED noahrst

  call getarg(2,outputsfc)
  noutputsfc=len_trim(outputsfc)
  print*,outputsfc(1:noutputsfc)
  open(luggg,file=outputsfc(1:noutputsfc),action='write',form='unformatted',iostat=iret)
  if(iret.ne.0) then
     call errmsg('sfc2noahrst: error opening file '//outputsfc(1:noutputsfc))
     call errexit(2)
  endif

! INPUT SFCANL

    call getarg(1,inputsfc)
    ninputsfc=len_trim(inputsfc)
    print*,inputsfc(1:ninputsfc)
    call sfcio_srohdc(lusfc,inputsfc(1:ninputsfc),head,data,irets)
    if(irets.ne.0) then
       print *,'irets=',irets
       call errmsg('sfc2noahrst: error opening file '//inputsfc(1:ninputsfc))
       call errexit(2)
    endif

     allocate(slon(head%lonb))
     allocate(slat(head%latb))
     allocate(rlat(head%latb))
     allocate(wlat(head%latb))
     call splat(4,head%latb,rlat,wlat)

     do i=1,head%lonb
        slon(i)=(i-1)*360./head%lonb
     end do
     do j=1,head%latb
        jj = head%latb - j + 1
        slat(j)=180.d0/acos(-1.d0)*asin(dble(rlat(jj)))
     end do
     write(*,'(10f10.3)') slon
     write(*,*)
     write(*,'(10f10.3)') slat

     allocate(land(head%lonb,head%latb))
     allocate(tsea(head%lonb,head%latb)) 
     allocate(canopy(head%lonb,head%latb)) 
     allocate(snwdph(head%lonb,head%latb)) 
     allocate(sheleg(head%lonb,head%latb)) 
     allocate(smc(head%lonb,head%latb,head%lsoil)) 
     allocate(stc(head%lonb,head%latb,head%lsoil)) 
     allocate(slc(head%lonb,head%latb,head%lsoil)) 
     allocate(ch(head%lonb,head%latb))

     land = 0.
     n = 0 !land point count
     do j=1,head%latb
        jj = head%latb - j + 1
     do i=1,head%lonb
       if(data%slmsk(i,j).gt.0. .and. data%slmsk(i,j).lt.2.0 )then
        n = n + 1
        land(i,jj)=1.
        tsea(i,jj)=data%tsea(i,j)
        canopy(i,jj)=data%canopy(i,j)
        snwdph(i,jj)=data%snwdph(i,j)
        sheleg(i,jj)=data%sheleg(i,j)
        do k=1,head%lsoil
         smc(i,jj,k)=data%smc(i,j,k)
         stc(i,jj,k)=data%stc(i,j,k)
         slc(i,jj,k)=data%slc(i,j,k)
         enddo
         ch(i,jj)=0.001
       endif
      enddo
      enddo

     print*,      1, head%lonb, head%latb, n
     write(luggg) 1, head%lonb, head%latb, n

     call fixio_r_gldas(luggg,land,tsea,head%lonb,head%latb,n)
     call fixio_r_gldas(luggg,land,canopy,head%lonb,head%latb,n)
     call fixio_r_gldas(luggg,land,snwdph,head%lonb,head%latb,n)
     call fixio_r_gldas(luggg,land,sheleg,head%lonb,head%latb,n)
     call fixio_r_gldas(luggg,land,stc(:,:,1),head%lonb,head%latb,n)
     call fixio_r_gldas(luggg,land,stc(:,:,2),head%lonb,head%latb,n)
     call fixio_r_gldas(luggg,land,stc(:,:,3),head%lonb,head%latb,n)
     call fixio_r_gldas(luggg,land,stc(:,:,4),head%lonb,head%latb,n)
     call fixio_r_gldas(luggg,land,smc(:,:,1),head%lonb,head%latb,n)
     call fixio_r_gldas(luggg,land,smc(:,:,2),head%lonb,head%latb,n)
     call fixio_r_gldas(luggg,land,smc(:,:,3),head%lonb,head%latb,n)
     call fixio_r_gldas(luggg,land,smc(:,:,4),head%lonb,head%latb,n)
     call fixio_r_gldas(luggg,land,slc(:,:,1),head%lonb,head%latb,n)
     call fixio_r_gldas(luggg,land,slc(:,:,2),head%lonb,head%latb,n)
     call fixio_r_gldas(luggg,land,slc(:,:,3),head%lonb,head%latb,n)
     call fixio_r_gldas(luggg,land,slc(:,:,4),head%lonb,head%latb,n)
     call fixio_r_gldas(luggg,land,ch,head%lonb,head%latb,n)
     call fixio_r_gldas(luggg,land,ch,head%lonb,head%latb,n)

     close(luggg)

contains

  subroutine fixio_r_gldas(luggg,land,grid,idim,jdim,n)

      implicit none
      integer  luggg,idim,jdim,i,j,k,n

      real land(idim,jdim)
      real grid(idim,jdim)
      real tile(n)

      tile = -9999.

      k = 0
      do j = 1, jdim
      do i = 1, idim
         if( land(i,j).gt.0. ) then
           k = k + 1
           tile(k) = grid(i,j) 
         endif
      enddo
      enddo
      print*, 'TILE N =',n,'      TILE K =',k

      write(luggg) tile

      return 
end subroutine
end program
