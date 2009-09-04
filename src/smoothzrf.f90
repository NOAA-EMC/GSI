subroutine frfhvo(p1,iv)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    frfhvo      performs vertical smoothing of fields
!   prgmmr: derber           org: np23                date: 2004-05-13
!
! abstract: performs vertical smoothing of fields
!
! program history log:
!   2004-05-13  derber, document
!   2005-01-22  parrish - make use of balmod and rename variables
!   2005-07-14  wu - add max bound to l2
!   2008-04-11  safford - rm unsed vars
!
!   input argument list:
!     p1       - input field to be smoothed (lat2,lon2,nsig)
!     iv       - location in alv for smoothing coefficients
!              - iv = 1 streamfunction
!              - iv = 2 velocity potential
!              - iv = 3 temperature        
!              - iv = 4 specific humidity  
!              - iv = 5 ozone           
!              - iv = 6 cloud condensate
!
!   output argument list
!     p1       - output field after vertical smoothing
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use kinds, only: r_kind,i_kind
  use constants, only:  zero,one
  use gridmod, only: nsig,regional,lat2,lon2
  use balmod, only: rllat1,llmax
  use berror, only: alv,be,ndeg
  implicit none

! lat2 = number of latitudes (lat2)
! lon2 = number of longitudes (lon2)
! nsig  = number of model levels 
! ndeg  = degree of smoothing (ndeg=4)

  integer(i_kind),intent(in)                          :: iv
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout):: p1

  integer(i_kind) kmod2,j,i,k,kr,ki,i1,l,l2
  real(r_kind) dl1,dl2,alvl,alvr,alvi,alvi1,alvr1
  real(r_kind) gaki,gakr,dekr,deki,bekr,beki
  real(r_kind),dimension(lat2,lon2,nsig):: p2
  real(r_kind),dimension(lat2,lon2,ndeg):: ga,de
  real(r_kind) newr,  newi,  oldr,  oldi
  real(r_kind) newr0, newi0, oldr0, oldi0
  real(r_kind) newr1, newi1, oldr1, oldi1

  kmod2=mod(ndeg,2)

! Zero output array
  do k=1,nsig
    do j=1,lon2
      do i=1,lat2
        p2(i,j,k)=p1(i,j,k)
        p1(i,j,k)=zero
      end do
    end do
  end do

! Zero local work arrays
  do k=1,ndeg
    do j=1,lon2
      do i=1,lat2
        ga(i,j,k)=zero
        de(i,j,k)=zero
      end do
    end do
  end do

 if (regional)then
! Regional mode, odd degree
  if (kmod2 == 1) then

!    advancing filter:
     do i=1,nsig
        do j=1,lon2
           do k=1,lat2
              l=int(rllat1(k,j))
              l2=min0(l+1,llmax)
              dl2=rllat1(k,j)-float(l)
              dl1=one-dl2
              alvl=dl1*alv(l,1,i,iv)+dl2*alv(l2,1,i,iv)
              ga(k,j,1)=alvl*ga(k,j,1)+be(1)*p2(k,j,i)
           end do
        enddo
        do j=1,lon2
           do k=1,lat2
              p1(k,j,i)=p1(k,j,i)+ga(k,j,1)
           end do
        enddo
!       treat remaining complex roots:
        do kr=kmod2+1,ndeg,2   ! <-- index of "real" components
           ki=kr+1            ! <-- index of "imag" components
           bekr=be(kr)
           beki=be(ki)
           do j=1,lon2
              do k=1,lat2
                 l=int(rllat1(k,j))
                 l2=min0(l+1,llmax)
                 dl2=rllat1(k,j)-float(l)
                 dl1=one-dl2
                 alvr=dl1*alv(l,kr,i,iv)+dl2*alv(l2,kr,i,iv)
                 alvi=dl1*alv(l,ki,i,iv)+dl2*alv(l2,ki,i,iv)
                 gakr=ga(k,j,kr)
                 gaki=ga(k,j,ki)
                 ga(k,j,kr)=alvr*gakr-alvi*gaki+bekr*p2(k,j,i)
                 ga(k,j,ki)=alvi*gakr+alvr*gaki+beki*p2(k,j,i)
              end do
           enddo
           do j=1,lon2
              do k=1,lat2
                 p1(k,j,i)=p1(k,j,i)+ga(k,j,kr)
              end do
           enddo
        enddo
     enddo

!    backing filter:
     do i=nsig,1,-1
        do j=1,lon2
           do k=1,lat2
              p1(k,j,i)=p1(k,j,i)+de(k,j,1)
           end do
        enddo
        do j=1,lon2
           do k=1,lat2
            l=int(rllat1(k,j))
            l2=min0(l+1,llmax)
            dl2=rllat1(k,j)-float(l)
            dl1=one-dl2
            alvl=dl1*alv(l,1,i,iv)+dl2*alv(l2,1,i,iv)
            de(k,j,1)=alvl*(de(k,j,1)+be(1)*p2(k,j,i))
           end do
        enddo
!       treat remaining complex roots:
        do kr=kmod2+1,ndeg,2   ! <-- index of "real" components
           ki=kr+1            ! <-- index of "imag" components
           bekr=be(kr)
           beki=be(ki)
           do j=1,lon2
              do k=1,lat2
                 p1(k,j,i)=p1(k,j,i)+de(k,j,kr)
              end do
           enddo
           do j=1,lon2
              do k=1,lat2
                 l=int(rllat1(k,j))
                 l2=min0(l+1,llmax)
                 dl2=rllat1(k,j)-float(l)
                 dl1=one-dl2
                 alvr=dl1*alv(l,kr,i,iv)+dl2*alv(l2,kr,i,iv)
                 alvi=dl1*alv(l,ki,i,iv)+dl2*alv(l2,ki,i,iv)
                 dekr=de(k,j,kr)+bekr*p2(k,j,i)
                 deki=de(k,j,ki)+beki*p2(k,j,i)
                 de(k,j,kr)=alvr*dekr-alvi*deki
                 de(k,j,ki)=alvi*dekr+alvr*deki
              end do
           enddo
        enddo
     enddo

! Regional mode, even degree
  else

     ! advancing filter:
     do i=1,nsig-1,2
        i1=i+1
        !       treat remaining complex roots:
        do kr=kmod2+1,ndeg,2   ! <-- index of "real" components
           ki=kr+1            ! <-- index of "imag" components
           bekr=be(kr)
           beki=be(ki)
           do j=1,lon2
              do k=1,lat2
                 l=int(rllat1(k,j))
                 l2=min0(l+1,llmax)
                 dl2=rllat1(k,j)-float(l)
                 dl1=one-dl2
                 alvr=dl1*alv(l,kr,i,iv)+dl2*alv(l2,kr,i,iv)
                 alvi=dl1*alv(l,ki,i,iv)+dl2*alv(l2,ki,i,iv)
                 alvr1=dl1*alv(l,kr,i1,iv)+dl2*alv(l2,kr,i1,iv)
                 alvi1=dl1*alv(l,ki,i1,iv)+dl2*alv(l2,ki,i1,iv)
                 gakr=ga(k,j,kr)
                 gaki=ga(k,j,ki)
                 ga(k,j,kr)=alvr*gakr-alvi*gaki+bekr*p2(k,j,i)
                 ga(k,j,ki)=alvi*gakr+alvr*gaki+beki*p2(k,j,i)
                 p1(k,j,i)=p1(k,j,i)+ga(k,j,kr)
                 gakr=ga(k,j,kr)
                 gaki=ga(k,j,ki)
                 ga(k,j,kr)=alvr1*gakr-alvi1*gaki+bekr*p2(k,j,i1)
                 ga(k,j,ki)=alvi1*gakr+alvr1*gaki+beki*p2(k,j,i1)
                 p1(k,j,i1)=p1(k,j,i1)+ga(k,j,kr)
              end do
           enddo
        enddo
     enddo
     if (mod(nsig,2)==1) then
        i=nsig
        !       treat remaining complex roots:
        do kr=kmod2+1,ndeg,2   ! <-- index of "real" components
           ki=kr+1            ! <-- index of "imag" components
           bekr=be(kr)
           beki=be(ki)
           do j=1,lon2
              do k=1,lat2
                 l=int(rllat1(k,j))
                 l2=min0(l+1,llmax)
                 dl2=rllat1(k,j)-float(l)
                 dl1=one-dl2
                 alvr=dl1*alv(l,kr,i,iv)+dl2*alv(l2,kr,i,iv)
                 alvi=dl1*alv(l,ki,i,iv)+dl2*alv(l2,ki,i,iv)

                 gakr=ga(k,j,kr)
                 gaki=ga(k,j,ki)
                 ga(k,j,kr)=alvr*gakr-alvi*gaki+bekr*p2(k,j,i)
                 ga(k,j,ki)=alvi*gakr+alvr*gaki+beki*p2(k,j,i)
                 p1(k,j,i)=p1(k,j,i)+ga(k,j,kr)
              end do
           enddo
       enddo
     endif

     !    backing filter:
     do i=nsig,2,-2
        i1=i-1
        !       treat remaining complex roots:
        do kr=kmod2+1,ndeg,2   ! <-- index of "real" components
           ki=kr+1            ! <-- index of "imag" components
           bekr=be(kr)
           beki=be(ki)
           do j=1,lon2
              do k=1,lat2
                 l=int(rllat1(k,j))
                 l2=min0(l+1,llmax)
                 dl2=rllat1(k,j)-float(l)
                 dl1=one-dl2
                 alvr=dl1*alv(l,kr,i,iv)+dl2*alv(l2,kr,i,iv)
                 alvi=dl1*alv(l,ki,i,iv)+dl2*alv(l2,ki,i,iv)
                 alvr1=dl1*alv(l,kr,i1,iv)+dl2*alv(l2,kr,i1,iv)
                 alvi1=dl1*alv(l,ki,i1,iv)+dl2*alv(l2,ki,i1,iv)
                 p1(k,j,i)=p1(k,j,i)+de(k,j,kr)
                 dekr=de(k,j,kr)+bekr*p2(k,j,i)
                 deki=de(k,j,ki)+beki*p2(k,j,i)
                 de(k,j,kr)=alvr*dekr-alvi*deki
                 de(k,j,ki)=alvi*dekr+alvr*deki

                 p1(k,j,i1)=p1(k,j,i1)+de(k,j,kr)
                 dekr=de(k,j,kr)+bekr*p2(k,j,i1)
                 deki=de(k,j,ki)+beki*p2(k,j,i1)
                 de(k,j,kr)=alvr1*dekr-alvi1*deki
                 de(k,j,ki)=alvi1*dekr+alvr1*deki
              end do
           enddo
        enddo
     enddo
     if (mod(nsig,2)==1) then
        i=1
        !       treat remaining complex roots:
        do kr=kmod2+1,ndeg,2   ! <-- index of "real" components
           ki=kr+1            ! <-- index of "imag" components
           bekr=be(kr)
           beki=be(ki)
           do j=1,lon2
              do k=1,lat2
                 l=int(rllat1(k,j))
                 l2=min0(l+1,llmax)
                 dl2=rllat1(k,j)-float(l)
                 dl1=one-dl2
                 alvr=dl1*alv(l,kr,i,iv)+dl2*alv(l2,kr,i,iv)
                 alvi=dl1*alv(l,ki,i,iv)+dl2*alv(l2,ki,i,iv)

                 p1(k,j,i)=p1(k,j,i)+de(k,j,kr)
                 dekr=de(k,j,kr)+bekr*p2(k,j,i)
                 deki=de(k,j,ki)+beki*p2(k,j,i)
                 de(k,j,kr)=alvr*dekr-alvi*deki
                 de(k,j,ki)=alvi*dekr+alvr*deki
              end do
           enddo
        enddo
     endif
  endif

! Global branch
 else
! Global mode, odd degree
  if (kmod2 == 1) then

!    advancing filter:
     do i=1,nsig
        do j=1,lon2
           do k=1,lat2
              ga(k,j,1)=alv(k,1,i,iv)*ga(k,j,1)+be(1)*p2(k,j,i)
           end do
        enddo
        do j=1,lon2
           do k=1,lat2
              p1(k,j,i)=p1(k,j,i)+ga(k,j,1)
           end do
        enddo
!       treat remaining complex roots:
        do kr=kmod2+1,ndeg,2   ! <-- index of "real" components
           ki=kr+1            ! <-- index of "imag" components
           bekr=be(kr)
           beki=be(ki)
           do j=1,lon2
              do k=1,lat2
                 gakr=ga(k,j,kr)
                 gaki=ga(k,j,ki)
                 ga(k,j,kr)=alv(k,kr,i,iv)*gakr-alv(k,ki,i,iv)*gaki+bekr*p2(k,j,i)
                 ga(k,j,ki)=alv(k,ki,i,iv)*gakr+alv(k,kr,i,iv)*gaki+beki*p2(k,j,i)
              end do
           enddo
           do j=1,lon2
              do k=1,lat2
                 p1(k,j,i)=p1(k,j,i)+ga(k,j,kr)
              end do
           enddo
        enddo
     enddo

!    backing filter:
     do i=nsig,1,-1
        do j=1,lon2
           do k=1,lat2
              p1(k,j,i)=p1(k,j,i)+de(k,j,1)
           end do
        enddo
        do j=1,lon2
           do k=1,lat2
              de(k,j,1)=alv(k,1,i,iv)*(de(k,j,1)+be(1)*p2(k,j,i))
           end do
        enddo
!       treat remaining complex roots:
        do kr=kmod2+1,ndeg,2   ! <-- index of "real" components
           ki=kr+1            ! <-- index of "imag" components
           bekr=be(kr)
           beki=be(ki)
           do j=1,lon2
              do k=1,lat2
                 p1(k,j,i)=p1(k,j,i)+de(k,j,kr)
              end do
           enddo
           do j=1,lon2
              do k=1,lat2
                 dekr=de(k,j,kr)+bekr*p2(k,j,i)
                 deki=de(k,j,ki)+beki*p2(k,j,i)
                 de(k,j,kr)=alv(k,kr,i,iv)*dekr-alv(k,ki,i,iv)*deki
                 de(k,j,ki)=alv(k,ki,i,iv)*dekr+alv(k,kr,i,iv)*deki
              end do
           enddo
        enddo
     enddo

! Global branch, even degree     
  else

     ! advancing filter:
     do i=1,nsig-1,2
        i1=i+1
        !       treat remaining complex roots:
        do kr=kmod2+1,ndeg,2   ! <-- index of "real" components
           ki=kr+1            ! <-- index of "imag" components
           bekr=be(kr)
           beki=be(ki)
           do j=1,lon2
              do k=1,lat2-1, 2
                 oldr0=ga(k  ,j,kr)
                 oldr1=ga(k+1,j,kr)
                 oldi0=ga(k  ,j,ki)
                 oldi1=ga(k+1,j,ki)
                 newr0=alv(k  ,kr,i,iv)*oldr0-alv(k  ,ki,i,iv)*oldi0+bekr*p2(k  ,j,i)
                 newr1=alv(k+1,kr,i,iv)*oldr1-alv(k+1,ki,i,iv)*oldi1+bekr*p2(k+1,j,i)
                 newi0=alv(k  ,ki,i,iv)*oldr0+alv(k  ,kr,i,iv)*oldi0+beki*p2(k  ,j,i)
                 newi1=alv(k+1,ki,i,iv)*oldr1+alv(k+1,kr,i,iv)*oldi1+beki*p2(k+1,j,i)
                 p1(k  ,j,i)=p1(k  ,j,i)+newr0
                 p1(k+1,j,i)=p1(k+1,j,i)+newr1

                 ga(k  ,j,kr)=alv(k  ,kr,i1,iv)*newr0-alv(k  ,ki,i1,iv)*newi0+bekr*p2(k  ,j,i1)
                 ga(k+1,j,kr)=alv(k+1,kr,i1,iv)*newr1-alv(k+1,ki,i1,iv)*newi1+bekr*p2(k+1,j,i1)
                 ga(k  ,j,ki)=alv(k  ,ki,i1,iv)*newr0+alv(k  ,kr,i1,iv)*newi0+beki*p2(k  ,j,i1)
                 ga(k+1,j,ki)=alv(k+1,ki,i1,iv)*newr1+alv(k+1,kr,i1,iv)*newi1+beki*p2(k+1,j,i1)
                 p1(k  ,j,i1)=p1(k  ,j,i1)+ga(k  ,j,kr)
                 p1(k+1,j,i1)=p1(k+1,j,i1)+ga(k+1,j,kr)
              end do
              do k=k,lat2
                 oldr=ga(k,j,kr)
                 oldi=ga(k,j,ki)
                 newr=alv(k,kr,i,iv)*oldr-alv(k,ki,i,iv)*oldi+bekr*p2(k,j,i)
                 newi=alv(k,ki,i,iv)*oldr+alv(k,kr,i,iv)*oldi+beki*p2(k,j,i)
                 p1(k,j,i)=p1(k,j,i)+newr

                 ga(k,j,kr)=alv(k,kr,i1,iv)*newr-alv(k,ki,i1,iv)*newi+bekr*p2(k,j,i1)
                 ga(k,j,ki)=alv(k,ki,i1,iv)*newr+alv(k,kr,i1,iv)*newi+beki*p2(k,j,i1)
                 p1(k,j,i1)=p1(k,j,i1)+ga(k,j,kr)
              end do
           enddo
        enddo
     enddo
     if (mod(nsig,2)==1) then
        i=nsig
        !       treat remaining complex roots:
        do kr=kmod2+1,ndeg,2   ! <-- index of "real" components
           ki=kr+1            ! <-- index of "imag" components
           bekr=be(kr)
           beki=be(ki)
           do j=1,lon2
              do k=1,lat2
                 gakr=ga(k,j,kr)
                 gaki=ga(k,j,ki)
                 ga(k,j,kr)=alv(k,kr,i,iv)*gakr-alv(k,ki,i,iv)*gaki+bekr*p2(k,j,i)
                 ga(k,j,ki)=alv(k,ki,i,iv)*gakr+alv(k,kr,i,iv)*gaki+beki*p2(k,j,i)
                 p1(k,j,i)=p1(k,j,i)+ga(k,j,kr)
              end do
           enddo
        enddo
     endif
     
     !    backing filter:
     do i=nsig,2,-2
        i1=i-1
        !       treat remaining complex roots:
        do kr=kmod2+1,ndeg,2   ! <-- index of "real" components
           ki=kr+1            ! <-- index of "imag" components
           bekr=be(kr)
           beki=be(ki)
           do j=1,lon2
              do k=1,lat2-1, 2
                 p1(k  ,j,i)=p1(k  ,j,i)+de(k  ,j,kr)
                 p1(k+1,j,i)=p1(k+1,j,i)+de(k+1,j,kr)
                 oldr0=de(k  ,j,kr)+bekr*p2(k  ,j,i)
                 oldr1=de(k+1,j,kr)+bekr*p2(k+1,j,i)
                 oldi0=de(k  ,j,ki)+beki*p2(k  ,j,i)
                 oldi1=de(k+1,j,ki)+beki*p2(k+1,j,i)
                 newr0=alv(k  ,kr,i,iv)*oldr0-alv(k  ,ki,i,iv)*oldi0
                 newr1=alv(k+1,kr,i,iv)*oldr1-alv(k+1,ki,i,iv)*oldi1
                 newi0=alv(k  ,ki,i,iv)*oldr0+alv(k  ,kr,i,iv)*oldi0
                 newi1=alv(k+1,ki,i,iv)*oldr1+alv(k+1,kr,i,iv)*oldi1

                 p1(k  ,j,i1)=p1(k  ,j,i1)+newr0
                 p1(k+1,j,i1)=p1(k+1,j,i1)+newr1
                 newr0=newr0+bekr*p2(k  ,j,i1)
                 newr1=newr1+bekr*p2(k+1,j,i1)
                 newi0=newi0+beki*p2(k  ,j,i1)
                 newi1=newi1+beki*p2(k+1,j,i1)
                 de(k  ,j,kr)=alv(k  ,kr,i1,iv)*newr0-alv(k  ,ki,i1,iv)*newi0
                 de(k+1,j,kr)=alv(k+1,kr,i1,iv)*newr1-alv(k+1,ki,i1,iv)*newi1
                 de(k  ,j,ki)=alv(k  ,ki,i1,iv)*newr0+alv(k  ,kr,i1,iv)*newi0
                 de(k+1,j,ki)=alv(k+1,ki,i1,iv)*newr1+alv(k+1,kr,i1,iv)*newi1
              end do
              do k=k,lat2
                 p1(k,j,i)=p1(k,j,i)+de(k,j,kr)
                 oldr=de(k,j,kr)+bekr*p2(k,j,i)
                 oldi=de(k,j,ki)+beki*p2(k,j,i)
                 newr=alv(k,kr,i,iv)*oldr-alv(k,ki,i,iv)*oldi
                 newi=alv(k,ki,i,iv)*oldr+alv(k,kr,i,iv)*oldi

                 p1(k,j,i1)=p1(k,j,i1)+newr
                 newr=newr+bekr*p2(k,j,i1)
                 newi=newi+beki*p2(k,j,i1)
                 de(k,j,kr)=alv(k,kr,i1,iv)*newr-alv(k,ki,i1,iv)*newi
                 de(k,j,ki)=alv(k,ki,i1,iv)*newr+alv(k,kr,i1,iv)*newi
              end do
           enddo
        enddo
     enddo
     if (mod(nsig,2)==1) then
        i=1
        !       treat remaining complex roots:
        do kr=kmod2+1,ndeg,2   ! <-- index of "real" components
           ki=kr+1            ! <-- index of "imag" components
           bekr=be(kr)
           beki=be(ki)
           do j=1,lon2
              do k=1,lat2
                 p1(k,j,i)=p1(k,j,i)+de(k,j,kr)
                 dekr=de(k,j,kr)+bekr*p2(k,j,i)
                 deki=de(k,j,ki)+beki*p2(k,j,i)
                 de(k,j,kr)=alv(k,kr,i,iv)*dekr-alv(k,ki,i,iv)*deki
                 de(k,j,ki)=alv(k,ki,i,iv)*dekr+alv(k,kr,i,iv)*deki
              end do
           enddo
        enddo
     endif
  endif
 end if
 return
end subroutine frfhvo

subroutine smoothzo(vx,samp,rate,iv,jx)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    smoothzo    initializes and renormalizes vertical smoothing coefs.
!   prgmmr: derber           org: np22                date: 2004-05-13
!
! abstract: initializes and renormalizes vertical smoothing coefficients
!           initializes dssv and alv
!
! program history log:
!   2004-05-13  derber, document
!   2004-11-30  treadon - add longitude dimension to variance array dssv
!
!   input argument list:
!     vx       - vertical smoothing scales
!     samp     - parameter for smoothing        
!     rate     - parameter for smoothing       
!     iv       - location in alv and dssv for smoothing coefficients
!              - iv = 1 streamfunction
!              - iv = 2 velocity potential
!              - iv = 3 temperature        
!              - iv = 4 specific humidity  
!              - iv = 5 ozone           
!              - iv = 6 cloud condensate
!     jx       - latitude index
!
!   output argument list
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only:  zero
  use gridmod, only: nsig,lon2
  use berror, only: dssv,alv,ndeg
  implicit none
 
  integer(i_kind),intent(in)             :: jx,iv
  real(r_kind),dimension(nsig),intent(in):: vx
  real(r_kind),intent(in)                :: samp
  real(r_kind),dimension(ndeg),intent(in):: rate
 
  integer(i_kind) i,k,m
  real(r_kind),dimension(nsig):: dss
  real(r_kind),dimension(nsig,nsig):: p1
  real(r_kind),dimension(nsig,ndeg):: al

  call rfdparv(vx,rate,al,nsig,ndeg)
  do m=1,ndeg
   do k=1,nsig
     alv(jx,m,k,iv)=al(k,m)
   end do
  end do
  p1=zero
  do k=1,nsig
    dss(k)=sqrt(samp*vx(k))
    p1(k,k)=dss(k)
  end do

  call rfhvo(p1,nsig,nsig,al)
  
  call rfhvo(p1,nsig,nsig,al)

  do k=1,nsig
     do i=1,lon2
        dssv(iv,jx,i,k)=sqrt(dss(k)/p1(k,k))
     end do
  end do

  return
end subroutine smoothzo
subroutine rfhvo(p1,nc,n,al)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    rfhvo   performs vertical smoothing for renormalization
!   prgmmr: derber           org: np22                date: 2004-05-13
!
! abstract: performs vertical smoothing of identity matrix for use with 
!           renormalization
!
! program history log:
!   2004-05-13  derber, document
!
!   input argument list:
!     p1       - input field to be smoothed (nc,n)
!     nc       - first array dimension for p1
!     n        - second array dimension for p1
!     al       - smoothing coefficients
!
!   output argument list
!     p1       - output field after vertical smoothing
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only:  zero
  use berror, only: be,ndeg
  implicit none
 
  integer(i_kind),intent(in)                :: n,nc
  real(r_kind),dimension(n,ndeg),intent(in) :: al
  real(r_kind),dimension(nc,n),intent(inout):: p1
 
  integer(i_kind) i,j,kmod2,kr,ki
  real(r_kind) gaki,dekr,deki,gakr
  real(r_kind),dimension(nc,n):: p2
  real(r_kind),dimension(nc,ndeg):: ga,de

  kmod2=mod(ndeg,2)

! Zero local work arrays.
  do j=1,ndeg
     do i=1,nc
        ga(i,j)=zero
        de(i,j)=zero
     end do
  end do

  do j=1,n
    do i=1,nc
     p2(i,j)=p1(i,j)
     p1(i,j)=zero
    end do
  end do

  if (kmod2 == 1) then
!    advancing filter:
     do i=1,n
        do j=1,nc
           ga(j,1)=al(i,1)*ga(j,1)+be(1)*p2(j,i)
        enddo
        do j=1,nc
           p1(j,i)=p1(j,i)+ga(j,1)
        enddo
                           ! treat remaining complex roots:
        do kr=kmod2+1,ndeg,2   ! <-- index of "real" components
           ki=kr+1            ! <-- index of "imag" components
           do j=1,nc
              gakr=ga(j,kr)
              gaki=ga(j,ki)
              ga(j,kr)=al(i,kr)*gakr-al(i,ki)*gaki+be(kr)*p2(j,i)
              ga(j,ki)=al(i,ki)*gakr+al(i,kr)*gaki+be(ki)*p2(j,i)
           enddo
           do j=1,nc
              p1(j,i)=p1(j,i)+ga(j,kr)
           enddo
        enddo
     enddo

!    backing filter:
     do i=n,1,-1
        do j=1,nc
           p1(j,i)=p1(j,i)+de(j,1)
        enddo
        do j=1,nc
           de(j,1)=al(i,1)*(de(j,1)+be(1)*p2(j,i))
        enddo
                           ! treat remaining complex roots:
        do kr=kmod2+1,ndeg,2   ! <-- index of "real" components
           ki=kr+1            ! <-- index of "imag" components
           do j=1,nc
              p1(j,i)=p1(j,i)+de(j,kr)
           enddo
           do j=1,nc
              dekr=de(j,kr)+be(kr)*p2(j,i)
              deki=de(j,ki)+be(ki)*p2(j,i)
              de(j,kr)=al(i,kr)*dekr-al(i,ki)*deki
              de(j,ki)=al(i,ki)*dekr+al(i,kr)*deki
           enddo
        enddo
     enddo
     
  else
! advancing filter:
     do i=1,n
                           ! treat remaining complex roots:
        do kr=kmod2+1,ndeg,2   ! <-- index of "real" components
           ki=kr+1            ! <-- index of "imag" components
           do j=1,nc
              gakr=ga(j,kr)
              gaki=ga(j,ki)
              ga(j,kr)=al(i,kr)*gakr-al(i,ki)*gaki+be(kr)*p2(j,i)
              ga(j,ki)=al(i,ki)*gakr+al(i,kr)*gaki+be(ki)*p2(j,i)
           enddo
           do j=1,nc
              p1(j,i)=p1(j,i)+ga(j,kr)
           enddo
        enddo
     enddo

! backing filter:
     do i=n,1,-1
        ! treat remaining complex roots:
        do kr=kmod2+1,ndeg,2   ! <-- index of "real" components
           ki=kr+1            ! <-- index of "imag" components
           do j=1,nc
              p1(j,i)=p1(j,i)+de(j,kr)
           enddo
           do j=1,nc
              dekr=de(j,kr)+be(kr)*p2(j,i)
              deki=de(j,ki)+be(ki)*p2(j,i)
              de(j,kr)=al(i,kr)*dekr-al(i,ki)*deki
              de(j,ki)=al(i,ki)*dekr+al(i,kr)*deki
           enddo
        enddo
     enddo
     
  endif
  return
end subroutine rfhvo
