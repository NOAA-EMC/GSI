subroutine prewgt(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    prewgt
!   prgmmr: wu               org: np22                date: 2000-03-15
!
! abstract: setup smoothing and grid transform for background error     
!
! program history log:
!   2000-03-15  wu           
!   2004-02-03  kleist, updated to load background stats according
!               to updated mpi distribution on horizontal slabs
!   2004-03-15  derber, kleist, incorporate variances into this routine
!               stats from single file, additional clean up
!   2004-05-17  kleist, documentation and clean up
!   2004-08-03  treadon - add only to module use, add intent in/out
!   2004-10-26  wu - include factors hzscl in the range of RF table
!   2004-11-02  treadon - add horizontal resolution error check on berror_stats
!   2004-11-16  treadon - add longitude dimension to variance array dssv
!   2004-11-20  derber - modify to make horizontal table more reproducable and  
!               move most of table calculations to berror 
!   2005-01-22  parrish - split out balance variables to subroutine prebal--this
!               to make anisotropic filtering option less confusing
!   2005-02-23  wu - setup background variance for qoption=2
!   2005-03-28  wu - change loop index (mlat+1 to mlat) over varq
!   2005-04-14  treadon - add corq2 to global berror_stats read
!   2005-04-22  treadon - change berror file to 4-byte reals
!   2005-05-27  kleist - add setup call for new patch interpolation
!   2005-08-16  guo - add gmao surface interface
!   2005-09-28  guo - set nrr=nlat to support the GMAO grid
!   2005-09-28  guo - fixed nrr to nlat-2 to avoid the subscript of
!		array rlats being out of the range, and to avoid the
!		singularity of rs at rlats(1)=-pi/2.
!   2005-11-16  wgu - set nolp=nr+1+(ny-nlat)/2 to make sure
!               nmix+nrmxb=nr in routine smoothrf no matter what
!               number nlat is.
!   2005-11-29  derber - unify ozone variance calculation
!   2006-01-10  treadon - replace rdsfull with read_gfssfc_full
!   2006-01-11  kleist - place upper/lower bounds on qoption=2 variance
!   2006-01-31  treadon - invert hzscl
!   2006-02-03  derber - fix up sl array stuff
!   2006-04-12  treadon - remove sigl (not used)
!   2006-04-21  kleist  - add capability to perturb background error parameters
!   2006-07-28  derber  - use r1000 from constants
!   2006-09-18  derber  - change minimum moisture variance
!   2007-05-30  h.liu   - use oz stats from berror file
!   2007-07-03  kleist  - add option for flow-dependent background error variances
!   2008-04-23  safford - rm unused uses and vars
!   2008-07-30  guo     - read stats using m_berror_stats
!   2009-01-12  gayno   - rm use of read_gfssfc_full
!   2010-04-01  treadon - move strip to gridmod
!
!   input argument list:
!     mype     - mpi task id
!
!   output argument list:
!
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind,r_single
  use berror, only: dssvp,dssvt,wtaxs,&
       bw,wtxrs,inaxs,inxrs,as,nr,ny,nx,mr,ndeg,&
       nf,vs,be,dssv,norh,bl2,bl,init_rftable,hzscl,&
       pert_berr,bkgv_flowdep,tsfc_sdv,slw,slw1,slw2
  use m_berror_stats,only : berror_read_wgt
  use mpimod, only: nvar_id,levs_id
  use mpimod, only: mpi_comm_world,ierror,mpi_rtype
  use jfunc, only: qoption,varq
  use gridmod, only: istart,jstart,lat2,lon2,rlats,nlat,nlon,nsig,&
       nnnn1o,lat1,lon1,itotsub,iglobal,ltosi,ltosj,ijn,displs_g,&
       strip
  use constants, only: izero,ione,zero,quarter,half,one,two,three,&
       rearth_equator,pi,r1000
  use guess_grids, only: isli2
  use smooth_polcarf, only: norsp,setup_smooth_polcas

  implicit none

! Declare passed variables
  integer(i_kind),intent(in   ) :: mype

! Declare local variables
  integer(i_kind) nrr,iii,jjj,nxg,i2,im,jm,j2
  integer(i_kind) i,j,k,nbuf,nmix,nxe,nor,ndx,ndy
  integer(i_kind) nlathh,mm1,nolp,mm,ir,k1
  integer(i_kind) ix,jx,mlat
  integer(i_kind) kd,kt,kq,kc,koz,nf2p
  integer(i_kind),dimension(0:40):: iblend

  real(r_kind) wlipi,wlipih,df
  real(r_kind) samp,y,s2u,x,dxx,df2,pi2
  real(r_kind),dimension(ndeg):: rate
  real(r_kind),dimension(ndeg,ndeg):: turn
  real(r_kind),dimension(nsig,0:nlat+ione):: vz,vd,vt,vq,voz,vcwm
  real(r_kind),dimension(0:nlat+ione,nsig,5):: hwll
  real(r_kind),dimension(lat2,lon2)::temp
  real(r_kind),dimension(0:nlat+ione):: hwllp
  real(r_kind),dimension(nlat,nlon):: sl,factx
  real(r_kind),dimension(-nf:nf,-nf:nf) :: fact1,fact2
  real(r_kind),dimension(mr:nlat-2_i_kind):: rs
  real(r_kind),dimension(lat1*lon1)::zsm
  real(r_kind),dimension(itotsub)::work1
  real(r_kind),dimension(ny,nx,3):: scsli
  real(r_kind),dimension(-nf:nf,-nf:nf,3):: scs12
  real(r_single),dimension(nlat,nsig):: corz,cord,corh,corq,corc,corq2,coroz
  real(r_single),dimension(nlat):: corp
  real(r_single),dimension(nlat,nlon):: corsst
  real(r_single) hsstmin
  real(r_kind) minhsst,corq2x
  real(r_kind),allocatable:: randfct(:)
  real(r_kind),allocatable,dimension(:,:,:,:):: sli,sli1,sli2

  real(r_single),dimension(nlat,nsig*6+ione):: hwllin
  real(r_single),dimension(nlat,nlon):: hsst
  real(r_single),dimension(nlat,nsig*6):: vscalesin

  real(r_kind),dimension(lat2,lon2,nsig):: sfvar,vpvar,tvar
  real(r_kind),dimension(lat2,lon2):: psvar

! real(r_kind),parameter:: eight_tenths = 0.8_r_kind
! real(r_kind),parameter:: six          = 6.0_r_kind
! real(r_kind),parameter:: r400         = 400.0_r_kind
! real(r_kind),parameter:: r800         = 800.0_r_kind
! real(r_kind),parameter:: r40000       = 40000.0_r_kind
! real(r_kind),parameter:: r25          = one/25.0_r_kind

! Initialize local variables
  pi2=two*pi
  ndy=(nlat-ny)/2
  nxe=nlon/8
  nor=norh*2
  mm1=mype+ione
  nlathh=nlat/4
  nf2p=2*nf+ione


! Setup blending
  mm=4_i_kind
  call blend(mm,iblend)

  nolp=nr+ione+(ny-nlat)/2
!  nbuf=nolp/4
  nbuf=izero
  nmix=nolp-nbuf*2
  dxx=one/(nmix+ione)
  bl2=zero
  k=izero
  do i=1,nmix
     k=k+ione
     x=i*dxx
     y=zero
     y=iblend(mm)
     do j=mm-ione,0,-1
        y=x*y+iblend(j)
     enddo
     y=y*x**(mm+ione)
     bl2(k)=one-y
  enddo
  do k=1,nmix    
    bl2(k)=sqrt(bl2(k))
  end do
  
  nmix=(nx-nlon)
  dxx=one/(nmix+ione)
  ndx=(nx-nlon)
  bl=zero
  k=ndx-nmix
  do i=1,nmix
     k=k+ione
     x=i*dxx
     y=zero
     y=iblend(mm)
     do j=mm-ione,0,-1
        y=x*y+iblend(j)
     enddo
     y=y*x**(mm+ione)
     bl(k)=one-y
  enddo
  do k=1,nmix
     bl(k)=sqrt(bl(k))
  end do

! Setup sea-land mask
  sl=zero
  if(bw /= zero)then
    do j=1,lon1*lat1
       zsm(j)=zero
     end do
     do j=1,lon2
        do i=1,lat2
           temp(i,j)=float(isli2(i,j))
        end do
     end do

     call strip(temp,zsm,ione)

     call mpi_allgatherv(zsm,ijn(mm1),mpi_rtype,&
        work1,ijn,displs_g,mpi_rtype,&
        mpi_comm_world,ierror)

     do k=1,iglobal
        i=ltosi(k) ; j=ltosj(k)
        sl(i,j)=work1(k)
     end do



     do j=1,nlon
        do i=1,nlat
           if(sl(i,j) > one)sl(i,j)=zero
        enddo
     enddo
     call smoothww(nlat,nlon,sl,half,2_i_kind,ione)
     do j=1,nlon
        do i=1,nlat
           sl(i,j)=min(max(sl(i,j),zero),one)
        enddo
     enddo
  end if

! Get background error statistics from a file ("berror_stats").
  call berror_read_wgt(corz,cord,corh,corq,corq2,coroz,corc,corp,&
    hwllin,vscalesin,corsst,hsst,mype)
  mlat=nlat


! load the horizontal length scales
  do k=1,nsig
     kd=nsig+k
     kt=nsig*2+k
     kq=nsig*3+k
     koz=nsig*4+k
     do i=1,nlat
        hwll(i,k,1)=hwllin(i,k)
        hwll(i,k,2)=hwllin(i,kd)
        hwll(i,k,3)=hwllin(i,kt)
        hwll(i,k,4)=hwllin(i,kq)
        hwll(i,k,5)=hwllin(i,koz)*three   !inflate scale
     end do
  end do

! surface pressure
  k=nsig*6+ione
  do i=1,nlat
     hwllp(i)=hwllin(i,k)
  end do


! sea surface temperature, convert from km to m
! also calculate a minimum horizontal length scale for
! sst to be used for land skin temp and ice temp
  hsstmin=1.e10_r_single
  minhsst=1.e10_r_kind
  do j=1,nlon
     do i=1,nlat
        hsst(i,j)=r1000*hsst(i,j)
        hsstmin=min(hsstmin,hsst(i,j))
     end do
  end do
  minhsst=hsstmin


! perturb background error
! Things to perturb: as(1-8), hzscl(1-3) and vs(1)
  if (pert_berr) then
     allocate(randfct(12))

     call get_randoms(12_i_kind,randfct)
     do i=1,8
        as(i)=as(i)+as(i)*randfct(i)
     end do
     do i=1,3
        hzscl(i)=hzscl(i)+hzscl(i)*randfct(8_i_kind+i)
     end do
     vs=vs+vs*randfct(12)
     if (mype==izero) then
        write(6,*) 'PREWGT: REDEFINE AS = ',as
        write(6,*) 'PREWGT: REDEFINE HZSCL = ',hzscl
        write(6,*) 'PREWGT: REDEFINE VS = ',vs
     end if
     deallocate(randfct)
  end if

! As used in the code, the horizontal length scale
! parameters are used in an inverted form.  Invert
! the parameter values here.
  do i=1,3
     hzscl(i)=one/hzscl(i)
  end do
! apply scaling (deflate/inflate) to vertical length scales
! note: parameter vs needs to be inverted
  vs=one/vs

! Initialize full array to zero before loading part of array below
  do i=0,nlat+ione
     do k=1,nsig
        vz(k,i)=zero
        vd(k,i)=zero
        vt(k,i)=zero
        vq(k,i)=zero
        voz(k,i)=zero
        vcwm(k,i)=zero
     end do
  end do

! load vertical length scales
  do k=1,nsig
     do i=1,nlat
        kd=nsig+k
        kt=nsig*2+k
        kq=nsig*3+k
        koz=nsig*4+k
        kc=nsig*5+k
        vz(k,i)=vs*vscalesin(i,k)
        vd(k,i)=vs*vscalesin(i,kd)
        vt(k,i)=vs*vscalesin(i,kt)
        vq(k,i)=vs*vscalesin(i,kq)
        voz(k,i)=vs*vscalesin(i,koz)
        vcwm(k,i)=vs*vscalesin(i,kc)
     end do
  end do
  vcwm=vq   ! for now use q error for cwm

  call rfdpar1(be,rate,ndeg)
  call rfdpar2(be,rate,turn,samp,ndeg)

  if(qoption==2_i_kind)then
     do k=1,nsig
        do j=1,mlat
           corq2x=corq2(j,k)
           varq(j,k)=min(max(corq2x,0.0015_r_kind),one)
        enddo
     enddo
     do k=1,nsig
        do j=1,nlat
           corq(j,k)=one
        end do
     end do
  endif

! Load background error variances onto subdomains
  do k=1,nsig
     do i=1,lat2
        ix=istart(mm1)+i-2_i_kind
        ix=max(ix,2_i_kind)
        ix=min(nlat-ione,ix)
        do j=1,lon2
           sfvar(i,j,k)=corz(ix,k)
           vpvar(i,j,k)=cord(ix,k)
           tvar(i,j,k)=corh(ix,k)
        end do
     end do
  end do

  do i=1,lat2
     ix=istart(mm1)+i-2_i_kind
     ix=max(ix,2_i_kind)
     ix=min(nlat-ione,ix)
     do j=1,lon2
        psvar(i,j)=corp(ix)
     end do
  end do

! Reweight the variances based on flow dependence if flag set
  if (bkgv_flowdep)  call bkgvar_rewgt(sfvar,vpvar,tvar,psvar,mype)

! vertical length scales
!$omp parallel do  schedule(dynamic,1) private(i,k,j,jx,ix)
  do j=1,lat2         
     jx=istart(mm1)+j-2_i_kind
     jx=max(jx,2_i_kind)
     jx=min(nlat-ione,jx)
     call smoothzo(vz(1,jx),samp,rate,ione,j)
     call smoothzo(vd(1,jx),samp,rate,2_i_kind,j)
     call smoothzo(vt(1,jx),samp,rate,3_i_kind,j)
     call smoothzo(vq(1,jx),samp,rate,4_i_kind,j)
     call smoothzo(voz(1,jx),samp,rate,5_i_kind,j)
     call smoothzo(vcwm(1,jx),samp,rate,6_i_kind,j)
!    load variances onto subdomains
     do k=1,nsig
        do i=1,lon2
           dssv(1,j,i,k)=dssv(1,j,i,k)*sfvar(j,i,k)*as(1)    ! streamfunction
           dssv(2,j,i,k)=dssv(2,j,i,k)*vpvar(j,i,k)*as(2)    ! velocity potential
           dssv(3,j,i,k)=dssv(3,j,i,k)*tvar(j,i,k)*as(4)     ! temperature

           dssv(4,j,i,k)=dssv(4,j,i,k)*corq(jx,k)*as(5)      ! specific humidity
           dssv(5,j,i,k)=dssv(5,j,i,k)*coroz(jx,k)*as(6)     ! ozone
           dssv(6,j,i,k)=dssv(6,j,i,k)*corc(jx,k)*as(8)      ! cloud condensate mixing ratio
        enddo
     enddo

     do i=1,lon2
        dssvp(j,i)=psvar(j,i)*as(3)             ! surface pressure

        if(isli2(j,i) == ione)then
          dssvt(j,i,2)= tsfc_sdv(1)               ! land surface temperature
        else if(isli2(j,i) == 2_i_kind)then
          dssvt(j,i,3)= tsfc_sdv(2)               ! ice surface temperature
        else
          ix=jstart(mm1)+i-2_i_kind
          if (ix==izero) ix=nlon
          ix=max(ix,ione)
          if (ix==nlon+ione) ix=ione
          ix=min(nlon,ix)
          dssvt(j,i,1)=corsst(jx,ix)*as(7)        ! sea surface temperature
        end if

     end do
  end do


! distance of gaussian lat from pole on stereogaphic map
! r=r/(1+z)
  do ir=mr,ubound(rs,1)		! ubound(rs,1) is nlat-2 to skip S.P.
     rs(ir)=cos(rlats(nlat-ir))/(one+sin(rlats(nlat-ir)))
  enddo
  df=tan(pi2/nlon*half)

! set up polcas
  call setwts(wtaxs,wtxrs,inaxs,inxrs,rs,df,nor,nxe,nf,mr,nr)

! set up smooth_polcas if desired (norsp > 0)
  if(norsp>izero) call setup_smooth_polcas

! load arrays which are in correct units, to be used to
! define the scales below
  do j=1,nlon
     do i=2,nlat-ione
        factx(i,j)=one/(one+(one-sl(i,j))*bw)
     end do
     factx(1,j)=factx(2,j)
     factx(nlat,j)=factx(nlat-ione,j)
  end do

  wlipi=nlon/pi2
  wlipih=nlon/pi2*half*samp*samp
  do j=1,nx
     jjj=j-ndx
     if(jjj<ione)jjj=nlon+jjj
     if(jjj>nlon)jjj=jjj-nlon
     do i=1,ny
        iii=i+ndy
        scsli(i,j,1)=(rlats(iii+ione)-rlats(iii-ione))*wlipih*cos(rlats(iii))* &
                     factx(iii,jjj)**2
        scsli(i,j,2)=(rlats(iii     )-rlats(iii-ione))*wlipi*factx(iii,jjj)
        scsli(i,j,3)=cos(rlats(iii))*factx(iii,jjj)
     enddo
  enddo

  nxg=nxe+norh
  nrr=ubound(rs,1)	! was nf*3/2
  ndx=(nx-nlon)/2
  call polcasl(factx,fact1,fact2,ione,nf,mr,nrr,nor,rs,df,nxe,nxg)
  fact1(0,0)=quarter*(fact1(1,0)+fact1(0,1)+fact1(-1,0)+fact1(0,-1))
  fact2(0,0)=quarter*(fact2(1,0)+fact2(0,1)+fact2(-1,0)+fact2(0,-1))

  df2=df*df
  do j=-nf,nf
     jm=j-ione
     j2=j*j
     do i=-nf,nf
        im=i-ione
        i2=i*i
        scs12(i,j,1)=(samp/(one+(i2+j2)*df2))**2*fact1(i,j)**2
        scs12(i,j,2)=one/(one+((im*im+i2)*half+j2)*df2)*fact1(i,j)
        scs12(i,j,3)=one/(one+(i2+(j2+jm*jm)*half)*df2)*fact1(i,j)
     enddo
  enddo


! Convert horizontal scales from physical units (m) to grid relative units
! rearth_equator is the equatorial radius from a 1999 IAG report.  The
! horizontal scales are defined at the equator, hence the need for the
! equatorial radius.
  s2u=(two*pi*rearth_equator)/float(nlon)


  allocate(sli(ny,nx,2,nnnn1o),sli1(-nf:nf,-nf:nf,2,nnnn1o), &
                            sli2(-nf:nf,-nf:nf,2,nnnn1o))

!$omp parallel do  schedule(dynamic,1) private(k,k1,j,iii,jjj,i,factx,fact1,fact2)
  do k=1,nnnn1o
     k1=levs_id(k)
     if (k1==izero) then
        do j=1,nlon
           do i=2,nlat-ione
              factx(i,j)=zero
           end do
        end do
     else if (nvar_id(k)==ione) then
! streamfunction
        do j=1,nlon
           do i=2,nlat-ione
              factx(i,j)=s2u/hwll(i,k1,1)
           end do
        end do
     else if (nvar_id(k)==2_i_kind) then
! velocity potential
        do j=1,nlon
           do i=2,nlat-ione
              factx(i,j)=s2u/hwll(i,k1,2)
           end do
        end do
     else if (nvar_id(k)==3_i_kind) then
! Surface pressure
        do j=1,nlon
           do i=2,nlat-ione
              factx(i,j)=s2u/hwllp(i)
           end do
        end do
     else if (nvar_id(k)==4_i_kind) then    
! Temperature
        do j=1,nlon
           do i=2,nlat-ione
              factx(i,j)=s2u/hwll(i,k1,3)
           end do
        end do
     else if (nvar_id(k)==5_i_kind) then
! Specific humidity
        do j=1,nlon
           do i=2,nlat-ione
              factx(i,j)=s2u/hwll(i,k1,4)
           end do
        end do
     else if(nvar_id(k)==6_i_kind)then
! Ozone
        do j=1,nlon
           do i=2,nlat-ione
              factx(i,j)=s2u/hwll(i,k1,5)
           end do
        end do
     else if(nvar_id(k)==7_i_kind)then
! sst
        do j=1,nlon
           do i=2,nlat-ione
              factx(i,j)=s2u/hsst(i,j)
           end do
        end do
     else if (nvar_id(k)==8_i_kind) then
! Cloud water
        do j=1,nlon
           do i=2,nlat-ione
              factx(i,j)=s2u/hwll(i,k1,4)
           end do
        end do
     else if(nvar_id(k)==9_i_kind)then
! surface temp (land)
        do j=1,nlon
           do i=2,nlat-ione
              factx(i,j)=two*s2u/minhsst
           end do
        end do
     else if(nvar_id(k)==10_i_kind)then
! surface temp (ice)
        do j=1,nlon
           do i=2,nlat-ione
              factx(i,j)=two*s2u/minhsst
           end do
        end do
     endif    ! end if over nvar_id
     do j=1,nlon
        factx(1,j)=factx(2,j)
        factx(nlat,j)=factx(nlat-ione,j)
     end do

     call polcasl(factx,fact1,fact2,ione,nf,mr,nrr,nor,rs,df,nxe,nxg)
     fact1(0,0)=quarter*(fact1(1,0)+fact1(0,1)+fact1(-1,0)+fact1(0,-1))
     fact2(0,0)=quarter*(fact2(1,0)+fact2(0,1)+fact2(-1,0)+fact2(0,-1))
! first sli
     do j=1,nx
        jjj=j-ndx
        if(jjj < ione)jjj=jjj+nlon
        if(jjj > nlon)jjj=jjj-nlon
        do i=1,ny
           iii=i+ndy
           slw((j-ione)*ny+i,k)=scsli(i,j,1)*factx(iii,jjj)**2
           sli(i,j,1,k)=scsli(i,j,2)*factx(iii,jjj)        
           sli(i,j,2,k)=scsli(i,j,3)*factx(iii,jjj)        
        enddo
     enddo           
! now load sli1/sli2 
     do j=-nf,nf
        do i=-nf,nf
           slw2((j+nf)*nf2p+nf+ione+i,k)=scs12(i,j,1)*fact1(i,j)**2
           slw1((j+nf)*nf2p+nf+ione+i,k)=scs12(i,j,1)*fact2(i,j)**2
           sli2(i,j,1,k)=scs12(i,j,2)*fact1(i,j)
           sli1(i,j,1,k)=scs12(i,j,2)*fact2(i,j)
           sli2(i,j,2,k)=scs12(i,j,3)*fact1(i,j)
           sli1(i,j,2,k)=scs12(i,j,3)*fact2(i,j)
        enddo
     enddo
  end do ! end do over nsig1o/loadling of sli arrays


! Load tables used in recursive filters
  call init_rftable(mype,rate,nnnn1o,sli,sli1,sli2)
  deallocate(sli,sli1,sli2)

  return
end subroutine prewgt

subroutine blend(n,iblend)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    blend
!   prgmmr: purser           org: w/nmc22     date:1998
!
! abstract: put coefficients for n+1,..,2n+1, into iblend(0),..
!           iblend(n)
!
! program history log:
!   2004-05-13  kleist  documentation
!   2008-04-23  safford - rm unused uses
!
!   input argument list:
!     n      - number of powers to blend
!
!   output argument list:
!     iblend - blended coefficients
!
! remarks: put the coefficients for powers n+1,..,2n+1, into iblend(0),
!          ..iblend(n),for the "blending polynomial" of continuity-
!          degree n in the interval [0,1].  For example, with n=1, the 
!          blending polynomial has up to 1st derivatives continuous 
!          with y(0)=0, y(1)=1, y'(0)=y'(1)=0, when y(x)=3x^2-2x^3. 
!          Hence iblend={3,-2}
! 
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
  use kinds, only: i_kind
  use constants, only: izero,ione
  implicit none

! Declare passed variables
  integer(i_kind)               ,intent(in   ) :: n
  integer(i_kind),dimension(0:n),intent(  out) :: iblend

! Declare local parameters
  integer(i_kind),parameter:: nn=12_i_kind

! Declare local variables
  integer(i_kind) np,i,j,ib
  integer(i_kind),dimension(0:nn):: ipascal(0:nn)

  if(n>nn)stop
  np=n+ione
  do i=0,n
    ipascal(i)=izero
  enddo

  ipascal(0)=ione
  do i=0,n
     do j=i,1,-1
        ipascal(j)=ipascal(j)-ipascal(j-ione)
     enddo
  enddo

  ib=ione
  do i=1,n
     ib=(ib*2*(2*i+1))/i
  enddo
  do j=0,n
     iblend(j)=(ib*ipascal(j))/(np+j)
  enddo

  return
end subroutine blend

subroutine get_randoms(count,randnums)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    get_randoms
!   prgmmr: kleist           org: np22              date: 2006-04-24
!
! abstract: get random numbers for perturbing background error parms
!
! program history log:
!   2006-04-21  kleist
!   2008-04-23  safford - rm unused uses
!
!   input argument list:
!     count    - number or random numbers to generate
!
!   output argument list:
!     randnums - array of scaled random numbers
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use kinds, only: r_kind,i_kind
  use obsmod, only: iadate
  use berror, only: pert_berr_fct
  use constants, only: ione, one, two
  implicit none

  integer(i_kind)              ,intent(in   ) :: count
  real(r_kind),dimension(count),intent(  out) :: randnums

  integer(i_kind),allocatable,dimension(:):: numrnds
  real(r_kind),dimension(count+ione):: temps
  real(r_kind):: rseed

  integer(i_kind) i,ksize

  call random_seed(size=ksize)
  allocate(numrnds(ksize))

! set seed as a function of analysis date
  rseed = 1e6_r_kind*iadate(1) + 1e4_r_kind*iadate(2) &
       + 1e2_r_kind*iadate(3) + iadate(4)

  do i=1,ksize
     numrnds(i)=rseed
  end do

  call random_seed(put=numrnds)
  deallocate(numrnds)

! this goes from 0-1, but want -1 to 1
  call random_number(temps)

! Set range to be +/- factor
! and don't use first random number generated based on date
  do i=1,count
     randnums(i) = pert_berr_fct*(one - two*temps(i+ione))
  end do

  return
end subroutine get_randoms
