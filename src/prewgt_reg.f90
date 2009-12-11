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
  use balmod, only: rllat,llmin,llmax
  use berror, only: as,dssvp,dssvt,&
       bw,ny,nx,dssv,vs,be,ndeg,&
       init_rftable,hzscl,tsfc_sdv,slw
  use mpimod, only: nvar_id,levs_id
  use jfunc, only: qoption,varq          
  use gridmod, only: lon2,nsig,nnnn1o,&
       region_dx,region_dy
  use constants, only: ione,zero,half,one,two,four
  use guess_grids, only: ges_prslavg,ges_psfcavg

  implicit none

! Declare passed variables
  integer(i_kind),intent(in):: mype

! Declare local parameters
  real(r_kind),parameter:: zero_3       = 0.3_r_kind
! real(r_kind),parameter:: six          = 6.0_r_kind
! real(r_kind),parameter:: eight        = 8.0_r_kind
  real(r_kind),parameter:: r400000      = 400000.0_r_kind
  real(r_kind),parameter:: r800000      = 800000.0_r_kind
! real(r_kind),parameter:: r1000        = 1000.0_r_kind
  real(r_kind),parameter:: r25          = one/25.0_r_kind
  real(r_kind),parameter:: r015         = 0.15_r_kind


! Declare local variables
  integer(i_kind) k,i
  integer(i_kind) n
  integer(i_kind) j,k1
  integer(i_kind) inerr,l,lp
  integer(i_kind) msig,mlat              ! stats dimensions
  integer(i_kind),dimension(nnnn1o):: ks

  real(r_kind) samp2,dl1,dl2
  real(r_kind) samp,hwl
  real(r_kind),dimension(nsig):: rate,dlsig,rlsig
  real(r_kind),dimension(nsig,nsig):: turn
  real(r_kind),dimension(ny,nx)::sl
  real(r_kind) fact,factoz,psfc015

  real(r_kind),allocatable,dimension(:):: corp, hwllp
  real(r_kind),allocatable,dimension(:,:):: wgvi ,bvi
  real(r_kind),allocatable,dimension(:,:,:):: corz, hwll, agvi ,vz
  real(r_kind),allocatable,dimension(:,:,:,:)::sli

! Initialize local variables
!  do j=1,nx
!   do i=1,ny
!    dx(i,j)=region_dx(i,j)
!    dy(i,j)=region_dy(i,j)
!   end do
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
  open(inerr,file='berror_stats',form='unformatted')
  rewind inerr
  read(inerr)msig,mlat

! Allocate arrays in stats file
  allocate ( corz(1:mlat,1:nsig,1:4) )
  allocate ( corp(1:mlat) )
  allocate ( hwll(0:mlat+ione,1:nsig,1:4),hwllp(0:mlat+ione) )
  allocate ( vz(1:nsig,0:mlat+ione,1:6) )
  allocate ( agvi(0:mlat+ione,1:nsig,1:nsig) )
  allocate ( bvi(0:mlat+ione,1:nsig),wgvi(0:mlat+ione,1:nsig) )

! Read in background error stats and interpolate in vertical to that specified in namelist
  call rdgstat_reg(msig,mlat,inerr,&
       hwll,hwllp,vz,agvi,bvi,wgvi,corz,corp,rlsig)
  close(inerr)

! Normalize vz with del sigmma and convert to vertical grid units!
  dlsig(1)=rlsig(1)-rlsig(2)
  do k=2,nsig-ione
     dlsig(k)=half*(rlsig(k-ione)-rlsig(k+ione))
  enddo
  dlsig(nsig)=rlsig(nsig-ione)-rlsig(nsig)

  do n=1,6
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

  if(qoption==2_i_kind)then
     do k=1,nsig
        do j=1,mlat
           varq(j,k)=min(max(corz(j,k,4),0.0015_r_kind),one)
        enddo
     enddo
     do k=1,nsig
        do j=llmin,llmax
           corz(j,k,4)=one
        end do
     end do
  endif


  do n=1,6
     do j=llmin,llmax
        call smoothzo(vz(1,j,n),samp,rate,n,j)
     end do
  end do
  factoz = 0.0002_r_kind*r25
  do k=1,nsig
     do i=1,lon2
        do j=llmin,llmax
           dssv(1,j,i,k)=dssv(1,j,i,k)*corz(j,k,1)*as(1)      ! streamfunction
           dssv(2,j,i,k)=dssv(2,j,i,k)*corz(j,k,2)*as(2)      ! velocity potential
           dssv(3,j,i,k)=dssv(3,j,i,k)*corz(j,k,3)*as(4)      ! temperature
           dssv(4,j,i,k)=dssv(4,j,i,k)*corz(j,k,4)*as(5)      ! specific humidity
           dssv(5,j,i,k)=dssv(5,j,i,k)*factoz*as(6)           ! ozone 
           dssv(6,j,i,k)=dssv(6,j,i,k)*corz(j,k,4)*as(8)      ! cloud condensate mixing ratio
        enddo
     end do
  end do

! Background error arrays for sfp, sst, land t, and ice t
  do j=1,lon2
     do i=llmin,llmax
        dssvp(i,j)  =corp(i)*as(3)
        dssvt(i,j,1)=as(7)*zero_3       ! sea t
        dssvt(i,j,2)=tsfc_sdv(1)*as(7)  ! land t
        dssvt(i,j,3)=tsfc_sdv(2)*as(7)  ! ice t
     end do
  end do


! hybrid sigma level structure calculated in rdgstat_reg   
! ks used to load constant horizontal scales for SF/VP
! above sigma level 0.15
! loop l for diff variable of each PE.

  psfc015=r015*ges_psfcavg
  do l=1,nnnn1o
     ks(l)=nsig+ione
     if(nvar_id(l)<3_i_kind)then
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
     if (nvar_id(k)==ione) then
! streamfunction
        if(k1 >= ks(k))then
           l=int(rllat(ny/2,nx/2))
           fact=one/hwll(l,k1,1)
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
                 fact=one/(dl1*hwll(l,k1,1)+dl2*hwll(lp,k1,1))
                 slw((i-ione)*ny+j,k)=slw((i-ione)*ny+j,1)*fact**2
                 sli(j,i,1,k)=sli(j,i,1,1)*fact
                 sli(j,i,2,k)=sli(j,i,2,1)*fact
              enddo
           enddo
        endif
     else if (nvar_id(k)==2_i_kind) then
! velocity potential
        if(k1 >= ks(k))then
           l=int(rllat(ny/2,nx/2))
           fact=one/hwll(l,k1,2)
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
                 fact=one/(dl1*hwll(l,k1,2)+dl2*hwll(lp,k1,2))
                 slw((i-ione)*ny+j,k)=slw((i-ione)*ny+j,1)*fact**2
                 sli(j,i,1,k)=sli(j,i,1,1)*fact
                 sli(j,i,2,k)=sli(j,i,2,1)*fact
              enddo
           enddo
        endif
     elseif (nvar_id(k)==3_i_kind) then
! Surface pressure
        do i=1,nx
           do j=1,ny
              l=int(rllat(j,i))
              lp=min0(l+ione,llmax)
              dl2=rllat(j,i)-float(l)
              dl1=one-dl2
              fact=one/(dl1*hwllp(l)+dl2*hwllp(lp))
              slw((i-ione)*ny+j,k)=slw((i-ione)*ny+j,1)*fact**2
              sli(j,i,1,k)=sli(j,i,1,1)*fact
              sli(j,i,2,k)=sli(j,i,2,1)*fact
           enddo
        enddo

     else if (nvar_id(k)==4_i_kind) then    
! Temperature
        if(k1 >= ks(k))then
           l=int(rllat(ny/2,nx/2))
           fact=one/hwll(l,k1,3)
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
                 fact=one/(dl1*hwll(l,k1,3)+dl2*hwll(lp,k1,3))
                 slw((i-ione)*ny+j,k)=slw((i-ione)*ny+j,1)*fact**2
                 sli(j,i,1,k)=sli(j,i,1,1)*fact
                 sli(j,i,2,k)=sli(j,i,2,1)*fact
              enddo
           enddo
        endif
     else if (nvar_id(k)==5_i_kind) then
! Specific humidity
        if(k1 >= ks(k))then
           l=int(rllat(ny/2,nx/2))
           fact=one/hwll(l,k1,4)
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
                 fact=one/(dl1*hwll(l,k1,4)+dl2*hwll(lp,k1,4))
                 slw((i-ione)*ny+j,k)=slw((i-ione)*ny+j,1)*fact**2
                 sli(j,i,1,k)=sli(j,i,1,1)*fact
                 sli(j,i,2,k)=sli(j,i,2,1)*fact
              enddo
           enddo
        endif
     elseif (nvar_id(k)==6_i_kind) then
! Ozone
        if(k1 <= nsig*3/4)then
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


     elseif (nvar_id(k)==7_i_kind) then
! SST
        do i=1,nx
           do j=1,ny
              l=int(rllat(j,i))
              lp=min0(l+ione,llmax)
              dl2=rllat(j,i)-float(l)
              dl1=one-dl2
                fact=two/( dl1*hwll(l,1,1)+dl2*hwll(lp,1,1))
              slw((i-ione)*ny+j,k)=slw((i-ione)*ny+j,1)*fact**2
              sli(j,i,1,k)=sli(j,i,1,1)*fact
              sli(j,i,2,k)=sli(j,i,2,1)*fact
           enddo
        enddo

     else if (nvar_id(k)==8_i_kind) then
! Cloud water
        if(k1 >= ks(k))then
           l=int(rllat(ny/2,nx/2))
           fact=one/hwll(l,k1,4)
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
                 fact=one/(dl1*hwll(l,k1,4)+dl2*hwll(lp,k1,4))
                 slw((i-ione)*ny+j,k)=slw((i-ione)*ny+j,1)*fact**2
                 sli(j,i,1,k)=sli(j,i,1,1)*fact
                 sli(j,i,2,k)=sli(j,i,2,1)*fact
              enddo
           enddo
        endif

     elseif (nvar_id(k)==9_i_kind) then
! surface temp (land)
        do i=1,nx
           do j=1,ny
              l=int(rllat(j,i))
              lp=min0(l+ione,llmax)
              dl2=rllat(j,i)-float(l)
              dl1=one-dl2
                fact=four/( dl1*hwll(l,1,1)+dl2*hwll(lp,1,1))
              slw((i-ione)*ny+j,k)=slw((i-ione)*ny+j,1)*fact**2
              sli(j,i,1,k)=sli(j,i,1,1)*fact
              sli(j,i,2,k)=sli(j,i,2,1)*fact
           enddo
        enddo

     elseif (nvar_id(k)==10_i_kind) then
! surface temp (ice)
        do i=1,nx
           do j=1,ny
              l=int(rllat(j,i))
              lp=min0(l+ione,llmax)
              dl2=rllat(j,i)-float(l)
              dl1=one-dl2
                fact=four/( dl1*hwll(l,1,1)+dl2*hwll(lp,1,1))
              slw((i-ione)*ny+j,k)=slw((i-ione)*ny+j,1)*fact**2
              sli(j,i,1,k)=sli(j,i,1,1)*fact
              sli(j,i,2,k)=sli(j,i,2,1)*fact
           enddo
        enddo

     endif
  end do
  deallocate( corz,corp,hwll,hwllp,vz,agvi,bvi,wgvi)


! Load tables used in recursive filters
  call init_rftable(mype,rate,nnnn1o,sli)

  deallocate( sli) 

  return
end subroutine prewgt_reg
