subroutine prewgt_reg(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    prewgt_reg  setup bkerror
!   prgmmr: wu               org: np22                date: 2000-03-15
!
! abstract: setup smoothing and grid transform for bkerror
!
! program history log:
!   2000-03-15  wu
!   2004-08-03  treadon - add only to module use; add intent in/out;
!                         fix bug in which rdgstat_reg inadvertently
!                         recomputed sigl (s/b done in gridmod)
!   2004-10-26  wu - include factors hzscl in the range of RF table
!   2004-11-16  treadon - add longitude dimension to variance array dssv
!   2004-11-20  derber - modify to make horizontal table more reproducable and
!                        move most of table calculations to berror
!   2005-01-22  parrish - split out balance variables to subroutine prebal_reg
!                         contained in module balmod.f90.  change wlat,
!                         lmin, lmax to rllat, llmin, llmax and add
!                         "use balmod" to connect to rllat,llmin,llmax
!   2005-02-23  wu - setup background variance for qoption=2
!   2005-03-28  wu - replace mlath with mlat and modify dim of corz, corp
!   2005-07-15  wu - remove old print out, add max bound to lp
!   2005-11-29  derber - unify ozone variance calculation
!   2006-01-11  kleist - place upper/lower bounds on qoption=2 variance
!   2006-01-31  treadon - invert hzscl
!   2006-04-17  treadon - use rlsig from call rdgstat_reg; replace sigl
!                         with ges_prslavg/ges_psfcavg
!   2007-05-30  h.liu - remove ozmz
!   2008-04-23  safford - rm unused uses and vars
!   2010-03-12  zhu     - move interpolations of dssv and dssvs into this subroutine
!                       - move varq & factoz to berror_read_wgt_reg
!                       - add changes using nrf* for generalized control variables
!   2010-03-15  zhu     - move the calculation of compute_qvar3d here
!   2010-04-10  parrish - remove rhgues, no longer used
!
!   input argument list:
!     mype     - pe number
!
!   output argument list:
!
!   other important variables
!     nsig     - number of sigma levels
!     nx       - number of gaussian lats in one hemisphere
!     ny       - number of longitudes
!     dx       - cos of grid latitudes (radians)
!   agv,wgv,bv - balance correlation matrix for t,p,div
!      sli     - scale info for the 3 subdomain
!     alv,dssv - vertical smoother coef.

! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!$$$
  use kinds, only: r_kind,i_kind
  use balmod, only: rllat,rllat1,llmin,llmax
  use berror, only: as,dssvs,&
       bw,ny,nx,dssv,vs,be,ndeg,&
       init_rftable,hzscl,tsfc_sdv,slw
  use mpimod, only: nvar_id,levs_id
  use jfunc, only: qoption
  use control_vectors, only: nrf,nrf3_oz,nrf3,nrf2,nrf2_sst,nvars,nrf3_loc,nrf2_loc,nrf_var
  use gridmod, only: lon2,lat2,nsig,nnnn1o,&
       region_dx,region_dy
  use constants, only: ione,zero,half,one,two,four
  use guess_grids, only: ges_prslavg,ges_psfcavg
  use m_berror_stats_reg, only: berror_get_dims_reg,berror_read_wgt_reg

  implicit none

! Declare passed variables
  integer(i_kind),intent(in   ) :: mype

! Declare local parameters
! real(r_kind),parameter:: six          = 6.0_r_kind
! real(r_kind),parameter:: eight        = 8.0_r_kind
  real(r_kind),parameter:: r400000      = 400000.0_r_kind
  real(r_kind),parameter:: r800000      = 800000.0_r_kind
! real(r_kind),parameter:: r1000        = 1000.0_r_kind
  real(r_kind),parameter:: r015         = 0.15_r_kind


! Declare local variables
  integer(i_kind) k,i,ii
  integer(i_kind) n,nn
  integer(i_kind) j,k1,loc
  integer(i_kind) inerr,l,lp,l2
  integer(i_kind) msig,mlat              ! stats dimensions
  integer(i_kind),dimension(nnnn1o):: ks

  real(r_kind) samp2,dl1,dl2
  real(r_kind) samp,hwl,cc
  real(r_kind),dimension(nsig):: rate,dlsig,rlsig
  real(r_kind),dimension(nsig,nsig):: turn
  real(r_kind),dimension(ny,nx)::sl
  real(r_kind) fact,factoz,psfc015

  real(r_kind),dimension(lon2,nsig,llmin:llmax):: dsv
  real(r_kind),dimension(lon2,llmin:llmax):: dsvs

  real(r_kind),allocatable,dimension(:,:):: corp, hwllp
  real(r_kind),allocatable,dimension(:,:,:):: corz, hwll, vz
  real(r_kind),allocatable,dimension(:,:,:,:)::sli

! Initialize local variables
!  do j=1,nx
!     do i=1,ny
!        dx(i,j)=region_dx(i,j)
!        dy(i,j)=region_dy(i,j)
!     end do
!  end do


! Setup sea-land mask
  sl=one
  do j=1,nx
     do i=1,ny
        sl(i,j)=min(max(sl(i,j),zero),one)
     enddo
  enddo


! Read dimension of stats file
  inerr=22_i_kind
  call berror_get_dims_reg(msig,mlat,inerr)

! Allocate arrays in stats file
  allocate ( corz(1:mlat,1:nsig,1:nrf3) )
  allocate ( corp(1:mlat,nrf2) )
  allocate ( hwll(0:mlat+ione,1:nsig,1:nrf3),hwllp(0:mlat+ione,nvars-nrf3) )
  allocate ( vz(1:nsig,0:mlat+ione,1:nrf3) )

! Read in background error stats and interpolate in vertical to that specified in namelist
  call berror_read_wgt_reg(msig,mlat,corz,corp,hwll,hwllp,vz,rlsig,mype,inerr)

! Normalize vz with del sigmma and convert to vertical grid units!
  dlsig(1)=rlsig(1)-rlsig(2)
  do k=2,nsig-ione
     dlsig(k)=half*(rlsig(k-ione)-rlsig(k+ione))
  enddo
  dlsig(nsig)=rlsig(nsig-ione)-rlsig(nsig)

  do n=1,nrf3
     do j=0,mlat+ione
        do k=1,nsig
           vz(k,j,n)=vz(k,j,n)*dlsig(k)
        end do
     end do
  end do

! As used in the code, the horizontal length scale
! parameters are used in an inverted form.  Invert
! the parameter values here.
  do i=1,3
     hzscl(i)=one/hzscl(i)
  end do

! apply scaling to vertical length scales.  
! note:  parameter vs needs to be inverted
  vs=one/vs
  vz=vz*vs

  call rfdpar1(be,rate,ndeg)
  call rfdpar2(be,rate,turn,samp,ndeg)

  do n=1,nrf3
     loc=nrf3_loc(n)
     do j=llmin,llmax
        call smoothzo(vz(1,j,n),samp,rate,n,j,dsv(1,1,j))
        do k=1,nsig
           do i=1,lon2
              dsv(i,k,j)=dsv(i,k,j)*corz(j,k,n)*as(loc)
           end do
        end do
     end do

     do j=1,lat2
        do i=1,lon2
           l=int(rllat1(j,i))
           l2=min0(l+1,llmax)
           dl2=rllat1(j,i)-float(l)
           dl1=one-dl2
           do k=1,nsig
              dssv(j,i,k,n)=dl1*dsv(i,k,l)+dl2*dsv(i,k,l2)
           enddo
        end do
     end do
  end do

! Special case of dssv for qoption=2
  if (qoption==2) call compute_qvar3d

! Background error arrays for sfp, sst, land t, and ice t
  do n=1,nrf2
     loc=nrf2_loc(n)
     do j=llmin,llmax
        do i=1,lon2
           dsvs(i,j)  =corp(j,n)*as(loc)
        end do
     end do

     do j=1,lat2
        do i=1,lon2
           l=int(rllat1(j,i))
           l2=min0(l+1,llmax)
           dl2=rllat1(j,i)-float(l)
           dl1=one-dl2
           dssvs(j,i,n)=dl1*dsvs(i,l)+dl2*dsvs(i,l2)
           if (n==nrf2_sst) then
              dssvs(j,i,nrf2+1)=tsfc_sdv(1)*as(loc)  
              dssvs(j,i,nrf2+2)=tsfc_sdv(2)*as(loc)  
           end if
        end do
     end do
  end do


! hybrid sigma level structure calculated in rdgstat_reg   
! ks used to load constant horizontal scales for SF/VP
! above sigma level 0.15
! loop l for diff variable of each PE.

  psfc015=r015*ges_psfcavg
  do l=1,nnnn1o
     ks(l)=nsig+ione
     if(nrf_var(nvar_id(l))=='sf' .or. nrf_var(nvar_id(l))=='SF' &
       .or. nrf_var(nvar_id(l))=='vp' .or. nrf_var(nvar_id(l))=='VP')then
        k_loop: do k=1,nsig
           if (ges_prslavg(k) < psfc015) then
              ks(l)=k
              exit k_loop
           end if
        enddo k_loop
     endif
  end do

  allocate(sli(ny,nx,2,nnnn1o))

! sli in scale  unit (can add in sea-land mask)
  samp2=samp*samp
  do i=1,nx
     do j=1,ny
        fact=one/(one+(one-sl(j,i))*bw)
        slw((i-ione)*ny+j,1)=region_dx(j,i)*region_dy(j,i)*fact**2*samp2
        sli(j,i,1,1)=region_dy(j,i)*fact
        sli(j,i,2,1)=region_dx(j,i)*fact
     enddo
  enddo


! Set up scales


! This first loop for nnnn1o will be if we aren't dealing with
! surface pressure, skin temperature, or ozone
  do k=nnnn1o,1,-1
     k1=levs_id(k)
     n=nvar_id(k)

     nn=-ione
     do ii=1,nrf3
        if (nrf3_loc(ii)==n) then 
           nn=ii
           if (nn/=nrf3_oz) then
              if (k1 >= ks(k))then
                 l=int(rllat(ny/2,nx/2))
                 fact=one/hwll(l,k1,nn)
                 do i=1,nx
                    do j=1,ny
                       slw((i-ione)*ny+j,k)=slw((i-ione)*ny+j,1)*fact**2
                       sli(j,i,1,k)=sli(j,i,1,1)*fact
                       sli(j,i,2,k)=sli(j,i,2,1)*fact
                    enddo
                 enddo
              else
                 do i=1,nx
                   do j=1,ny
                     l=int(rllat(j,i))
                     lp=min0(l+ione,llmax)
                     dl2=rllat(j,i)-float(l)
                     dl1=one-dl2
                     fact=one/(dl1*hwll(l,k1,nn)+dl2*hwll(lp,k1,nn))
                     slw((i-ione)*ny+j,k)=slw((i-ione)*ny+j,1)*fact**2
                     sli(j,i,1,k)=sli(j,i,1,1)*fact
                     sli(j,i,2,k)=sli(j,i,2,1)*fact
                   enddo
                 enddo
              endif
           else
              if (k1 <= nsig*3/4)then
                 hwl=r400000
              else
                 hwl=(r800000-r400000*(nsig-k1)/(nsig-nsig*3/4))
              endif
              fact=one/hwl
              do i=1,nx
                 do j=1,ny
                    slw((i-ione)*ny+j,k)=slw((i-ione)*ny+j,1)*fact**2
                    sli(j,i,1,k)=sli(j,i,1,1)*fact
                    sli(j,i,2,k)=sli(j,i,2,1)*fact
                 enddo
              enddo
           end if 
           exit
        end if
     end do

     if (nn==-ione) then 
        do ii=1,nrf2
           if (nrf2_loc(ii)==n .or. n>nrf) then 
              nn=ii
              if (n>nrf) nn=n-nrf3
              cc=one 
              if (nn==nrf2_sst) cc=two
              if (nn==nrf2+ione .or. nn==nrf2+2_i_kind) cc=four
              do i=1,nx
                 do j=1,ny
                    l=int(rllat(j,i))
                    lp=min0(l+ione,llmax)
                    dl2=rllat(j,i)-float(l)
                    dl1=one-dl2
                    fact=cc/(dl1*hwllp(l,nn)+dl2*hwllp(lp,nn))
                    slw((i-ione)*ny+j,k)=slw((i-ione)*ny+j,1)*fact**2
                    sli(j,i,1,k)=sli(j,i,1,1)*fact
                    sli(j,i,2,k)=sli(j,i,2,1)*fact
                 end do
              end do
              exit
           end if
        end do
     end if 

  end do
  deallocate( corz,corp,hwll,hwllp,vz)


! Load tables used in recursive filters
  call init_rftable(mype,rate,nnnn1o,sli)

  deallocate( sli) 

  return
end subroutine prewgt_reg
