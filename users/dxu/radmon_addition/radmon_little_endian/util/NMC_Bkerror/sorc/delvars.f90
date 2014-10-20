subroutine delvars(sf1,vp1,t1,rh1,oz1,cw1,ps1,sf2,vp2,t2,rh2,oz2,cw2,ps2,mype)
  use kinds, only: r_kind,r_single,i_kind
  use variables,only: nlat,nlon,nsig,lat1,lon1,zero,biasrm,&
      bbiasz,bbiasd,bbiast,bcorrz,bcorrd,bcorrt,bbiasp,bcorrp,&
      tcon,vpcon,half,pscon,vertavg,istart
  implicit none

  real(r_kind),dimension(lat1,lon1,nsig),intent(inout):: sf1,vp1,t1,rh1,oz1,cw1
  real(r_kind),dimension(lat1,lon1),intent(inout):: ps1
  real(r_kind),dimension(lat1,lon1,nsig),intent(inout):: sf2,vp2,t2,rh2,oz2,cw2
  real(r_kind),dimension(lat1,lon1),intent(inout):: ps2
  integer(i_kind),intent(in):: mype

  real(r_kind),dimension(lat1,lon1):: bal1
  real(r_kind) r025
  integer(i_kind) i,j,k,m,ix,mm1

  mm1=mype+1
  r025=0.25_r_kind

    if(biasrm) then
      do k=1,nsig
        do j=1,lon1
          do i=1,lat1
            sf1(i,j,k) = sf1(i,j,k)-bcorrz(i,j,k)*sf2(i,j,k)-bbiasz(i,j,k)
            vp1(i,j,k) = vp1(i,j,k)-bcorrd(i,j,k)*vp2(i,j,k)-bbiasd(i,j,k)
             t1(i,j,k) =  t1(i,j,k)-bcorrt(i,j,k)* t2(i,j,k)-bbiast(i,j,k)
          end do
        end do
      end do
      do j=1,lon1
        do i=1,lat1
          ps1(i,j) = ps1(i,j)-bcorrp(i,j)*ps2(i,j)-bbiasp(i,j)
        end do
      end do
    else
      do k=1,nsig
        do j=1,lon1
          do i=1,lat1
            sf1(i,j,k) = sf1(i,j,k)-sf2(i,j,k)
            vp1(i,j,k) = vp1(i,j,k)-vp2(i,j,k)
            t1(i,j,k)  =  t1(i,j,k)- t2(i,j,k)
          end do
        end do
      end do
      do j=1,lon1
        do i=1,lat1
          ps1(i,j) = ps1(i,j)-ps2(i,j)
        end do
      end do
    end if

    if (vertavg) then
      do k=2,nsig-1
        do j=1,lon1
          do i=1,lat1
            sf2(i,j,k) = half*sf1(i,j,k)+r025*(sf1(i,j,k+1)+sf1(i,j,k-1))
            vp2(i,j,k) = half*vp1(i,j,k)+r025*(vp1(i,j,k+1)+vp1(i,j,k-1))
             t2(i,j,k) = half* t1(i,j,k)+r025*( t1(i,j,k+1)+ t1(i,j,k-1))
          end do
        end do
      end do
! for k=1 and k=nsig now
      do j=1,lon1
        do i=1,lat1
          sf2(i,j,1) =half*sf1(i,j,1)+half*sf1(i,j,2)
          vp2(i,j,1) =half*vp1(i,j,1)+half*vp1(i,j,2)
           t2(i,j,1) =half* t1(i,j,1)+half* t1(i,j,2)
          sf2(i,j,nsig) =half*sf1(i,j,nsig)+half*sf1(i,j,nsig-1)
          vp2(i,j,nsig) =half*vp1(i,j,nsig)+half*vp1(i,j,nsig-1)
           t2(i,j,nsig) =half* t1(i,j,nsig)+half* t1(i,j,nsig-1)
        end do
      end do
      do k=1,nsig
        do j=1,lon1
          do i=1,lat1
            sf1(i,j,k)=sf2(i,j,k)
            vp1(i,j,k)=vp2(i,j,k)
             t1(i,j,k)= t2(i,j,k)
          end do
        end do
      end do
    end if

    do k=1,nsig
      do j=1,lon1
        do i=1,lat1
          rh1(i,j,k) = rh1(i,j,k)-rh2(i,j,k)
          oz1(i,j,k) = oz1(i,j,k)-oz2(i,j,k)
          cw1(i,j,k) = cw1(i,j,k)-cw2(i,j,k)
        end do
      end do
    end do

! Subtract off SF Component of Temperature difference
    do m=1,nsig
      bal1(:,:)=zero
      do k=1,nsig
        do i=1,lat1
          ix=istart(mm1)+i-1
          do j=1,lon1
            bal1(i,j)=bal1(i,j)+tcon(ix,m,k)*sf1(i,j,k)
          end do
        end do
      end do
      do j=1,lon1
        do i=1,lat1
          t1(i,j,m) = t1(i,j,m) - bal1(i,j)
        end do
      end do 
    end do !end do m

! Balanced part of VP
    do k=1,nsig
      bal1(:,:)=zero
      do i=1,lat1
        ix=istart(mm1)+i-1
        do j=1,lon1
          bal1(i,j)=vpcon(ix,k)*sf1(i,j,k)
          vp1(i,j,k) = vp1(i,j,k) - bal1(i,j)
        end do
      end do
    end do

! 'Balanced' part of SF
    bal1(:,:)=zero
    do k=1,nsig 
      do i=1,lat1
        ix=istart(mm1)+i-1
        do j=1,lon1
          bal1(i,j) = bal1(i,j) + pscon(ix,k)*sf1(i,j,k)
        end do
      end do
    end do
    do j=1,lon1
      do i=1,lat1
        ps1(i,j) = ps1(i,j) - bal1(i,j)
      end do
    end do

  return
end subroutine delvars


