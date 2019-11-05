subroutine delvars_aerosol(d1a,d2a,d3a,d4a,d5a,s1a,s2a,s3a,s4a,so4a,oc1a,oc2a,bc1a,bc2a, &
                   d1b,d2b,d3b,d4b,d5b,s1b,s2b,s3b,s4b,so4b,oc1b,oc2b,bc1b,bc2b,mype)
  use kinds, only: r_kind,r_single,i_kind
  use variables,only: nlat,nlon,nsig,lat1,lon1,zero,biasrm,&
      bbiasz,bbiasd,bbiast,bcorrz,bcorrd,bcorrt,bbiasp,bcorrp,&
      half,vertavg,istart
  implicit none

  real(r_kind),dimension(lat1,lon1,nsig),intent(inout):: d1a,d2a,d3a,d4a,d5a, &
      s1a,s2a,s3a,s4a,so4a,oc1a,oc2a,bc1a,bc2a
  real(r_kind),dimension(lat1,lon1,nsig),intent(inout):: d1b,d2b,d3b,d4b,d5b, &
      s1b,s2b,s3b,s4b,so4b,oc1b,oc2b,bc1b,bc2b
  integer(i_kind),intent(in):: mype

  real(r_kind) r025
  integer(i_kind) i,j,k,m,ix,mm1

  mm1=mype+1

  if (mype==0) write(6,*) 'delvars_aerosol'

    do k=1,nsig
      do j=1,lon1
        do i=1,lat1
          d1a(i,j,k) = d1a(i,j,k)-d1b(i,j,k)
          d2a(i,j,k) = d2a(i,j,k)-d2b(i,j,k)
          d3a(i,j,k) = d3a(i,j,k)-d3b(i,j,k)
          d4a(i,j,k) = d4a(i,j,k)-d4b(i,j,k)
          d5a(i,j,k) = d5a(i,j,k)-d5b(i,j,k)
          s1a(i,j,k) = s1a(i,j,k)-s1b(i,j,k)
          s2a(i,j,k) = s2a(i,j,k)-s2b(i,j,k)
          s3a(i,j,k) = s3a(i,j,k)-s3b(i,j,k)
          s4a(i,j,k) = s4a(i,j,k)-s4b(i,j,k)
          so4a(i,j,k) = so4a(i,j,k)-so4b(i,j,k)
          oc1a(i,j,k) = oc1a(i,j,k)-oc1b(i,j,k)
          oc2a(i,j,k) = oc2a(i,j,k)-oc2b(i,j,k)
          bc1a(i,j,k) = bc1a(i,j,k)-bc1b(i,j,k)
          bc2a(i,j,k) = bc2a(i,j,k)-bc2b(i,j,k)
        end do
      end do
    end do

  return
end subroutine delvars_aerosol


