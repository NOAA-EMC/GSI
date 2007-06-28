module jcmod
!$$$   module documentation block
!                .      .    .                                       .
! module:    jcmod           contains stuff for Jc penalty
!
!   prgmmr: kleist           org: np20                date: 2005-07-01
!
! abstract:  contains routines and variables for dynamic constraint 
!            term
!
! program history log:
!   2005-07-01
!   2005-09-29  kleist, expand to include variables for more terms
!   2005-11-21  kleist, remove tendency arrays
!   2006-04-06  kleist, expand and redefine Jc term for two formulations
!
! subroutines included:
!   sub init_jcvars          - initialize Jc related variables
!   sub create_jcvars        - allocate load Jc related variables
!   sub destroy_jcvars       - deallocate Jc related arrays
!   sub get_jcwts            - load Jc normalization factor for Jc term
!
! Variable Definitions:
!   def jcterm               - if true, Jc linearized in inner loop
!                              about outer loop solution (for now)
!   def jcdivt               - if true, run Jc using divergence tendency formulation
!                              if false, run Jc using original formulation (based onut,vt,Tt,Pst)
!   def bamp_ext1            - multiplying factor for first external part of Jc
!   def bamp_ext2            - multiplying factor for second external part of Jc
!   def bamp_int1            - multiplying factor for first internal part of Jc
!   def bamp_int2            - multiplying factor for second internal part of Jc
!   def wt_ext1              - array of weights for first external part of Jc
!   def wt_ext2              - array of weights for second external part of Jc
!   def wt_int1              - array of weights for first internal part of Jc
!   def wt_int1              - array of weights for second internal part of Jc
!
!   The z_* arrays are used to accumulate information from previous outer loops regarding
!      congributions to the Jc term
!      Their definitions depend on formulation:
!
!   For (jcdivt=.false.)
!     def z_ext1      - from surface pressure tendency
!     def z_ext2      - from integrated u tendency
!     def z_ext3      - from integrated v tendency
!     def z_int1      - from weighted temperature tendency
!     def z_int2      - from internal u tendency-term (deviation from vertically integrated tends)
!     def z_int3      - from internal v tendency-term (deviation from vertically integrated tends)
!
!   For (jcdivt=.true)
!     def z_ext1      - from integrated divergence tendency
!     def z_ext2      - from integrated ageostrophic vorticity tendency
!     def z_int1      - from deviation of vertically integrated divergence tendency   
!     def z_int2      - from deviation of vertically integrated ageostrophic vorticity tendency
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind
  implicit none

  logical jcterm,jcdivt
  real(r_kind) bamp_ext1,bamp_ext2,bamp_int1,bamp_int2
  real(r_kind),allocatable,dimension(:,:):: wt_ext1,wt_ext2,z_ext1,z_ext2,z_ext3
  real(r_kind),allocatable,dimension(:,:,:):: wt_int1,wt_int2,z_int1,z_int2,z_int3
  real(r_kind),allocatable,dimension(:):: jcresc1
  real(r_kind),allocatable,dimension(:,:):: jcresc2

contains

  subroutine init_jcvars
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_jcvars     initial Jc related variables
!   prgmmr: kleist          org: np20                date: 2005-07-01
!
! abstract: initialize dynamic constraint term variables
!
! program history log:
!   2005-07-01  kleist
!   2005-09-29  kleist, expanded for new terms
!   2006-04-06  kleist, include both formulations
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
    use constants, only: one
    implicit none

! load defaults for non-allocatable arrays
    jcterm=.false.
    jcdivt=.false.
    bamp_ext1=one
    bamp_ext2=one
    bamp_int1=one
    bamp_int2=one

    return
  end subroutine init_jcvars

  subroutine create_jcvars
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    create_jcvars     allocate Jc related arrays
!   prgmmr: kleist          org: np20                date: 2005-07-01
!
! abstract: allocate dynamic constraint term arrays
!
! program history log:
!   2005-07-01  kleist
!   2005-09-29  kleist, expanded for new terms
!   2006-04-06  kleist, include both Jc formulations
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
    use constants, only: one,zero
    use gridmod, only: lat2,lon2,nsig
    use specmod, only: ncd2
    use mpimod, only: nuvlevs
    use kinds, only: i_kind 
    implicit none   

    integer(i_kind) i,j,k

    allocate(wt_ext1(lat2,lon2),wt_ext2(lat2,lon2),&
         z_ext1(lat2,lon2),z_ext2(lat2,lon2))
    allocate(wt_int1(lat2,lon2,nsig),wt_int2(lat2,lon2,nsig),&
         z_int1(lat2,lon2,nsig),z_int2(lat2,lon2,nsig))

    if (jcdivt) allocate(jcresc1(ncd2),jcresc2(ncd2,nuvlevs))

    if (.not. jcdivt) allocate(z_ext3(lat2,lon2),z_int3(lat2,lon2,nsig))
    
    do k=1,nsig
      do j=1,lon2
        do i=1,lat2
          wt_int1(i,j,k)=zero
          wt_int2(i,j,k)=zero
          z_int1(i,j,k)=zero
          z_int2(i,j,k)=zero
        end do
      end do
    end do

    do j=1,lon2
      do i=1,lat2
        wt_ext1(i,j)=zero
        wt_ext2(i,j)=zero
        z_ext1(i,j)=zero
        z_ext2(i,j)=zero
      end do
    end do

    if (.not. jcdivt) then
      do k=1,nsig
        do j=1,lon2
          do i=1,lat2
            z_int3(i,j,k)=zero
          end do
        end do
      end do
      do j=1,lon2
        do i=1,lat2
          z_ext3(i,j)=zero
        end do
      end do
    end if

    if (jcdivt) then
      do i=1,ncd2
        jcresc1(i)=zero
        do k=1,nuvlevs
           jcresc2(i,k)=zero
        end do
      end do
    end if

    return
  end subroutine create_jcvars

  subroutine destroy_jcvars
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    destroy_jcvars     deallocate Jc related arrays
!   prgmmr: kleist          org: np20                date: 2005-07-01
!
! abstract: deallocate dynamic constraint term arrays
!
! program history log:
!   2005-07-01  kleist
!   2005-09-29  kleist, new terms added
!   2006-04-06  kleist, include both formulations
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
    deallocate(wt_ext1,wt_ext2,z_ext1,z_ext2)
    deallocate(wt_int1,wt_int2,z_int1,z_int2)
    if (.not. jcdivt) deallocate(z_ext3,z_int3)
    if (jcdivt) deallocate(jcresc1,jcresc2)

    return
  end subroutine destroy_jcvars

  subroutine get_jcwts(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    get_jcwts     get weights for Jc penalty term
!   prgmmr: kleist          org: np20                date: 2005-07-01
!
! abstract: load array which contains normalization factor for
!           dynamic constraint term
!
! program history log:
!   2005-07-01  kleist
!   2005-09-29  kleist, new terms added
!   2006-04-06  kleist, include both formulations
!   2006-09-21  kleist, include pressure and areal-ave weightings
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
    use gridmod, only: lat2,lon2,nsig,nlat,nlon,istart,rlats
    use constants, only: one,pi,ione,half,izero,zero
    use mpimod, only: levsuv_id,nuvlevs
    use guess_grids, only: ges_prsi,ntguessig
    use kinds, only: i_kind
    use mod_vtrans, only: depths,nvmodes_keep
    use specmod, only: enn1,ncd2
    implicit none

! Passed variables
    integer(i_kind),intent(in):: mype

! Local variables
    integer(i_kind) i,j,k,ix,it,mm1,klev
    real(r_kind) latwgt,delp,cor

    mm1=mype+ione
    latwgt=one
    delp=one
    cor=1.e-4_r_kind

! Note that divergence tendency formulation has latitude
! dependence (area-weighted contributions to Jc)
    it=ntguessig
    do k=1,nsig
      do i=1,lat2
        if (jcdivt) then
          ix=istart(mm1)+i-2
          ix=max(ix,1)
          ix=min(nlat,ix)
          latwgt=cos(rlats(ix))/((float(nlat)-one)*float(nlon)) 
        end if
        do j=1,lon2
          if (jcdivt) then
            delp=(ges_prsi(i,j,k,it)-ges_prsi(i,j,k+1,it)) / &
                 (ges_prsi(i,j,1,it)-ges_prsi(i,j,nsig+1,it))
          end if
          wt_int1(i,j,k)=bamp_int1*latwgt*delp
          wt_int2(i,j,k)=bamp_int2*latwgt*delp
        end do
      end do
    end do
    do i=1,lat2
      if (jcdivt) then
        ix=istart(mm1)+i-2
        ix=max(ix,1)
        ix=min(nlat,ix)
        latwgt=cos(rlats(ix))/((float(nlat)-one)*float(nlon))
!!      write(mm1+300,*) 'i,ix,cos(lats),latwgt = ',i,ix,cos(rlats(ix)),latwgt
      end if
      do j=1,lon2
        wt_ext1(i,j)=bamp_ext1*latwgt
        wt_ext2(i,j)=bamp_ext2*latwgt
      end do
    end do

    if (jcdivt) then
      jcresc1(:)=zero
      do i=1,ncd2
        jcresc1(i)=one/sqrt(enn1(i))
      end do

      do k=1,nuvlevs
        klev=levsuv_id(k)
        if (klev.gt.izero .and. klev.le.nvmodes_keep) then
          do i=1,ncd2
            jcresc2(i,k)=cor*cor + depths(klev)/(jcresc1(i)*jcresc1(i))
            jcresc2(i,k)=jcresc1(i)/sqrt(jcresc2(i,k))
          end do
        else
          do i=1,ncd2
            jcresc2(i,k)=zero
          end do
        end if
      end do
      jcresc1(1)=zero
      jcresc2(1,:)=zero
    end if
    return
  end subroutine get_jcwts

  subroutine update_jcterms(xut,xvt,xtt,xpt,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    update_jcterms
!   prgmmr: kleist          org: np20                date: 2006-02-21
!
! abstract: save and update current incremental contribution to penalty
!           at end of each inner loop
!
! program history log:
!   2006-04-06  kleist
!
!   input argument list:
!       xut      - current incremental u tendency
!       xvt      - current incremental v tendency
!       xtt      - current incremental virtual temperature tendency
!       xpt      - current incremental pressure tendency
!       mype     - mpi task id
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
    use gridmod, only: lat2,lon2,nsig
    use guess_grids, only: ges_prsi,ges_tv,ntguessig
    use constants, only: zero,one,two,rd_over_cp
    use mpimod, only: mpi_rtype,mpi_comm_world,mpi_sum
    use kinds, only: i_kind,r_kind
    implicit none

! Passed variables
    real(r_kind),dimension(lat2,lon2,nsig),intent(in):: xut,xvt,xtt
    real(r_kind),dimension(lat2,lon2,nsig+1),intent(in):: xpt
    integer(i_kind),intent(in):: mype

! local variables
    real(r_kind),dimension(lat2,lon2):: ubar,vbar
    real(r_kind) factor,delp,uint,vint,tint
    real(r_kind),dimension(6):: tendrms,tendrms0
    integer(i_kind) i,j,k,it

    it=ntguessig
    if (mype==0) write(6,*) 'UPDATE JC TERMS FOR NEXT OUTER LOOP'

    do j=1,lon2
      do i=1,lat2
        ubar(i,j)=zero
        vbar(i,j)=zero
      end do
    end do

    do k=1,nsig
      do j=2,lon2-1
        do i=2,lat2-1
          delp=(ges_prsi(i,j,k,it)-ges_prsi(i,j,k+1,it)) / &
               (ges_prsi(i,j,1,it)-ges_prsi(i,j,nsig+1,it))
          ubar(i,j)=ubar(i,j) + xut(i,j,k)*delp
          vbar(i,j)=vbar(i,j) + xvt(i,j,k)*delp
        end do
      end do
    end do

! Update z_ext1, z_ext1 by adding on
    do j=1,lon2
      do i=1,lat2
        z_ext1(i,j)=z_ext1(i,j) + xpt(i,j,1)
        z_ext2(i,j)=z_ext2(i,j) + ubar(i,j)
        z_ext3(i,j)=z_ext3(i,j) + vbar(i,j)
      end do
    end do

    do k=1,nsig
      do j=1,lon2
        do i=1,lat2
          factor=rd_over_cp*ges_tv(i,j,k,it)/(ges_prsi(i,j,k,it)+ges_prsi(i,j,k+1,it))
          uint=xut(i,j,k)-ubar(i,j)
          vint=xvt(i,j,k)-vbar(i,j)
          tint=xtt(i,j,k)-factor*(xpt(i,j,k)+xpt(i,j,k+1))
 
          z_int1(i,j,k)=z_int1(i,j,k) + tint
          z_int2(i,j,k)=z_int2(i,j,k) + uint
          z_int3(i,j,k)=z_int3(i,j,k) + vint
        end do
      end do
    end do

    tendrms=zero
    do j=2,lon2-1
      do i=2,lat2-1
        tendrms(1)=tendrms(1) + z_ext1(i,j)**two
        tendrms(2)=tendrms(2) + z_ext2(i,j)**two + z_ext3(i,j)**two
        tendrms(5)=tendrms(5) + one
      end do
    end do

    do k=1,nsig
      do j=2,lon2-1
        do i=2,lat2-1
          tendrms(3)=tendrms(3) + z_int1(i,j,k)**two
          tendrms(4)=tendrms(4) + z_int2(i,j,k)**two + z_int3(i,j,k)**two
          tendrms(6)=tendrms(6) + one
        end do
      end do
    end do

    call mpi_reduce(tendrms,tendrms0,6,mpi_rtype,mpi_sum,0,mpi_comm_world,i)

    if(mype == 0) then
      write(6,*) 'UPDATEJC:  COMPUTE TENDENCIES ON CURRENT SOLUTION'
      write(6,'("  pst,uve,tin,uvi TOTAL RMS=",4e14.6)') (sqrt(tendrms0(i)),i=1,4)

      tendrms0(1)=sqrt(tendrms0(1)/tendrms0(5))
      tendrms0(2)=sqrt(tendrms0(2)/tendrms0(5))
      tendrms0(3)=sqrt(tendrms0(3)/tendrms0(6))
      tendrms0(4)=sqrt(tendrms0(4)/tendrms0(6))
      write(6,'("  pst,uve,tin,uvi AVERAGE RMS=",4e14.6)') (tendrms0(i),i=1,4)
    end if


    return
  end subroutine update_jcterms

  subroutine update_jcterms_divt(xdivt,xagvt,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    update_jcterms
!   prgmmr: kleist          org: np20                date: 2006-04-06
!
! abstract: save and update current incremental contribution to penalty
!           at end of each inner loop
!
! program history log:
!   2006-04-06  kleist
!
!   input argument list:
!       xdivt      - current incremental divergence tendency
!       xagvt      - current incrremental ageostrophic vorticity tendency
!       mype       - mpi task id
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
    use gridmod, only: lat2,lon2,nsig
    use guess_grids, only: ges_prsi,ntguessig
    use constants, only: zero,one,two
    use mpimod, only: mpi_rtype,mpi_comm_world,mpi_sum
    use kinds, only: i_kind,r_kind
    implicit none

! Passed variables
    real(r_kind),dimension(lat2,lon2,nsig),intent(in):: xdivt,xagvt
    integer(i_kind),intent(in):: mype

! local variables
    real(r_kind),dimension(lat2,lon2):: dbar,abar
    real(r_kind) dint,aint,delp
    real(r_kind),dimension(6):: tendrms,tendrms0
    integer(i_kind) i,j,k,it

    it=ntguessig
    if (mype==0) write(6,*) 'UPDATE JC TERMS FOR NEXT OUTER LOOP'

    do j=1,lon2
      do i=1,lat2
        dbar(i,j)=zero
        abar(i,j)=zero
      end do
    end do

    do k=1,nsig
      do j=1,lon2
        do i=1,lat2
          delp=(ges_prsi(i,j,k,it)-ges_prsi(i,j,k+1,it)) / &
               (ges_prsi(i,j,1,it)-ges_prsi(i,j,nsig+1,it))
          dbar(i,j)=dbar(i,j) + xdivt(i,j,k)*delp
          abar(i,j)=abar(i,j) + xagvt(i,j,k)*delp
        end do
      end do
    end do

! Update z_dbar, z_abar by adding on
    do j=1,lon2
      do i=1,lat2
        z_ext1(i,j)=z_ext1(i,j) + dbar(i,j)
        z_ext2(i,j)=z_ext2(i,j) + abar(i,j)
      end do
    end do

! Update internal contributions now
    do k=1,nsig
      do j=1,lon2
        do i=1,lat2
          dint=xdivt(i,j,k)-dbar(i,j)
          aint=xagvt(i,j,k)-abar(i,j)
          z_int1(i,j,k) = z_int1(i,j,k) + dint
          z_int2(i,j,k) = z_int2(i,j,k) + aint
        end do
      end do
    end do

    tendrms=zero
    do j=2,lon2-1
      do i=2,lat2-1
        tendrms(1)=tendrms(1) + z_ext1(i,j)**two
        tendrms(2)=tendrms(2) + z_ext2(i,j)**two
        tendrms(5)=tendrms(5) + one
      end do
    end do

    do k=1,nsig
      do j=2,lon2-1
        do i=2,lat2-1
          tendrms(3)=tendrms(3) + z_int1(i,j,k)**two
          tendrms(4)=tendrms(4) + z_int2(i,j,k)**two
          tendrms(6)=tendrms(6) + one
        end do
      end do
    end do

    call mpi_reduce(tendrms,tendrms0,6,mpi_rtype,mpi_sum,0,mpi_comm_world,i)

    if(mype == 0) then
      write(6,*) 'UPDATEJC:  COMPUTE TENDENCIES ON CURRENT SOLUTION'
      write(6,'("  dte,avte,dti,avti TOTAL RMS=",4e14.6)') (sqrt(tendrms0(i)),i=1,4)

      tendrms0(1)=sqrt(tendrms0(1)/tendrms0(5))
      tendrms0(2)=sqrt(tendrms0(2)/tendrms0(5))
      tendrms0(3)=sqrt(tendrms0(3)/tendrms0(6))
      tendrms0(4)=sqrt(tendrms0(4)/tendrms0(6))
      write(6,'("  dte,avte,dti,avti AVERAGE RMS=",4e14.6)') (tendrms0(i),i=1,4)
    end if

    return
  end subroutine update_jcterms_divt


end module jcmod
