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
!   2010-02-25  zhu     - mv varq to m_berror_stats
!                       - make changes for generalizing control variables,
!                         change interface of berror_read_wgt,use nrf*
!   2010-04-01  treadon - move strip to gridmod
!   2010-04-10  parrish - remove rhgues, no longer needed
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
  use berror, only: dssvs,wtaxs,&
       bw,wtxrs,inaxs,inxrs,as,nr,ny,nx,mr,ndeg,&
       nf,vs,be,dssv,norh,bl2,bl,init_rftable,hzscl,&
       pert_berr,bkgv_flowdep,tsfc_sdv,slw,slw1,slw2
  use m_berror_stats,only : berror_read_wgt
  use mpimod, only: nvar_id,levs_id
  use mpimod, only: mpi_comm_world,ierror,mpi_rtype
  use jfunc, only: qoption
  use control_vectors, only: nrf,nrf2,nrf3,nrf3_sf,nrf3_cw,nrf3_q,&
       nrf3_vp,nrf3_t,nrf3_oz,nrf2_ps,nrf2_sst,nrf3_loc,nrf2_loc
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
  integer(i_kind) n,nrr,iii,jjj,nxg,i2,im,jm,j2,loc
  integer(i_kind) i,j,k,ii,nn,nbuf,nmix,nxe,nor,ndx,ndy
  integer(i_kind) nlathh,mm1,nolp,mm,ir,k1
  integer(i_kind) ix,jx,mlat
  integer(i_kind) kd,kt,kq,kc,koz,nf2p
  integer(i_kind),dimension(0:40):: iblend

  real(r_kind) wlipi,wlipih,df
  real(r_kind) samp,y,s2u,x,dxx,df2,pi2
  real(r_kind),dimension(ndeg):: rate
  real(r_kind),dimension(ndeg,ndeg):: turn
  real(r_kind),dimension(nsig,0:nlat+ione,nrf3):: vz
  real(r_kind),dimension(0:nlat+ione,nsig,nrf3):: hwll
  real(r_kind),dimension(lat2,lon2)::temp
  real(r_kind),dimension(0:nlat+ione,nrf2):: hwllp
  real(r_kind),dimension(nlat,nlon):: sl,factx
  real(r_kind),dimension(-nf:nf,-nf:nf) :: fact1,fact2
  real(r_kind),dimension(mr:nlat-2_i_kind):: rs
  real(r_kind),dimension(lat1*lon1)::zsm
  real(r_kind),dimension(itotsub)::work1
  real(r_kind),dimension(ny,nx,3):: scsli
  real(r_kind),dimension(-nf:nf,-nf:nf,3):: scs12
  real(r_single),dimension(nlat,nsig,nrf3):: corz
  real(r_single),dimension(nlat,nrf2):: corp
  real(r_single),dimension(nlat,nlon):: corsst
  real(r_kind),dimension(lon2,nsig):: dsv
  real(r_single) hsstmin
  real(r_kind) minhsst
  real(r_kind),allocatable:: randfct(:)
  real(r_kind),allocatable,dimension(:,:,:,:):: sli,sli1,sli2

  real(r_single),dimension(nlat,nsig,nrf3):: hwllin
  real(r_single),dimension(nlat,nrf2):: hwllinp
  real(r_single),dimension(nlat,nlon):: hsst
  real(r_single),dimension(nsig,nlat,nrf3):: vscalesin

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
  call berror_read_wgt(corz,corp,hwllin,hwllinp,vscalesin,corsst,hsst,mype)
  mlat=nlat

! load the horizontal length scales
  hwll=zero
  do j=1,nrf3
     do k=1,nsig
        do i=1,nlat
           hwll(i,k,j)=hwllin(i,k,j)
        end do
     end do
  end do
  hwll(:,:,nrf3_oz)=hwll(:,:,nrf3_oz)*three   !inflate scale
  hwll(:,:,nrf3_cw)=hwll(:,:,nrf3_q)          !use hwll of q for cw for now

! surface pressure
  hwllp=zero
  do j=1,nrf2
     do i=1,nlat
        hwllp(i,j)=hwllinp(i,j)
     end do
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
  vz=zero

! load vertical length scales
  do j=1,nrf3
     do k=1,nsig
        do i=1,nlat
           vz(k,i,j)=vs*vscalesin(k,i,j)
        end do
     end do
  end do

! for now use q error for cwm
  do k=1,nsig
     do i=1,nlat
        vz(k,i,nrf3_cw)=vz(k,i,nrf3_q)
     end do
  end do

  call rfdpar1(be,rate,ndeg)
  call rfdpar2(be,rate,turn,samp,ndeg)

! Load background error variances onto subdomains
  do k=1,nsig
     do i=1,lat2
        ix=istart(mm1)+i-2_i_kind
        ix=max(ix,2_i_kind)
        ix=min(nlat-ione,ix)
        do j=1,lon2
           sfvar(i,j,k)=corz(ix,k,nrf3_sf)
           vpvar(i,j,k)=corz(ix,k,nrf3_vp)
           tvar(i,j,k)=corz(ix,k,nrf3_t)
        end do
     end do
  end do

  do i=1,lat2
     ix=istart(mm1)+i-2_i_kind
     ix=max(ix,2_i_kind)
     ix=min(nlat-ione,ix)
     do j=1,lon2
        psvar(i,j)=corp(ix,nrf2_ps)
     end do
  end do

! Reweight the variances based on flow dependence if flag set
  if (bkgv_flowdep)  call bkgvar_rewgt(sfvar,vpvar,tvar,psvar,mype)

! vertical length scales
!$omp parallel do  schedule(dynamic,1) private(i,n,k,j,jx,ix)
  do n=1,nrf3
     loc=nrf3_loc(n)
     do j=1,lat2         
        jx=istart(mm1)+j-2_i_kind
        jx=max(jx,2_i_kind)
        jx=min(nlat-ione,jx)
        call smoothzo(vz(1,jx,n),samp,rate,n,j,dsv)

!       load variances onto subdomains
        if (n==nrf3_sf) then
           do k=1,nsig
              do i=1,lon2
                 dssv(j,i,k,n)=dsv(i,k)*sfvar(j,i,k)*as(loc)   ! streamfunction
              end do
           end do
        else if (n==nrf3_vp) then
           do k=1,nsig
              do i=1,lon2
                 dssv(j,i,k,n)=dsv(i,k)*vpvar(j,i,k)*as(loc)   ! velocity potential
              end do
           end do
        else if (n==nrf3_t) then
           do k=1,nsig
              do i=1,lon2
                 dssv(j,i,k,n)=dsv(i,k)*tvar(j,i,k)*as(loc)    ! temperature
              end do
           end do
        else
           do k=1,nsig
              do i=1,lon2
                 dssv(j,i,k,n)=dsv(i,k)*corz(jx,k,n)*as(loc)
              end do
           end do
        end if
    enddo
  end do

! Special case of dssv for qoption=2
  if (qoption==2) call compute_qvar3d

!$omp parallel do  schedule(dynamic,1) private(i,n,j,jx,ix)
  do n=1,nrf2
     loc=nrf2_loc(n)
     if (n==nrf2_ps) then
        do j=1,lat2         
           do i=1,lon2
              dssvs(j,i,n)=psvar(j,i)*as(loc)             ! surface pressure
           end do
        end do
     else if (n==nrf2_sst) then
        do j=1,lat2         
           do i=1,lon2
              if(isli2(j,i) == ione)then
                 dssvs(j,i,nrf2+1)= tsfc_sdv(1)               ! land surface temperature
              else if(isli2(j,i) == 2_i_kind)then
                 dssvs(j,i,nrf2+2)= tsfc_sdv(2)               ! ice surface temperature
              else
                 jx=istart(mm1)+j-2_i_kind
                 jx=max(jx,2_i_kind)
                 jx=min(nlat-ione,jx)
                 ix=jstart(mm1)+i-2_i_kind
                 if (ix==izero) ix=nlon
                 ix=max(ix,ione)
                 if (ix==nlon+ione) ix=ione
                 ix=min(nlon,ix)
                 dssvs(j,i,n)=corsst(jx,ix)*as(loc)        ! sea surface temperature
              end if
           end do
        end do
     end if
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

!$omp parallel do  schedule(dynamic,1) private(k,k1,j,ii,iii,jjj,i,n,nn,factx,fact1,fact2)
  do k=1,nnnn1o
     k1=levs_id(k)
     if (k1==izero) then
        do j=1,nlon
           do i=2,nlat-ione
              factx(i,j)=zero
           end do
        end do
     else 
        n=nvar_id(k)
        nn=-ione
        do ii=1,nrf3
           if (nrf3_loc(ii)==n) then
              nn=ii
              do j=1,nlon
                 do i=2,nlat-ione
                    factx(i,j)=s2u/hwll(i,k1,nn)
                 end do
              end do
              exit
           end if
        end do

        if (nn==-ione) then
           do ii=1,nrf2
              if (nrf2_loc(ii)==n .or. n>nrf) then
                 nn=ii
                 if (n>nrf) nn=n-nrf3
                 if (nn==nrf2_sst) then
                    do j=1,nlon
                       do i=2,nlat-ione
                          factx(i,j)=s2u/hsst(i,j)
                       end do
                    end do
                 else if (nn>nrf2) then 
                    do j=1,nlon
                       do i=2,nlat-ione
                          factx(i,j)=two*s2u/minhsst
                       end do
                    end do
                 else  
                    do j=1,nlon
                       do i=2,nlat-ione
                          factx(i,j)=s2u/hwllp(i,nn)
                       end do
                    end do
                 end if
                 exit
              end if
           end do
        end if
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
