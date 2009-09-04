subroutine penal(xhat)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    penal       oberror tuning
!   prgmmr: wu               org: np23                date: 2005-08-26
!
! abstract: randomized estimation of Tr(KH) and Tr(HK) and 
!            adaptive tuning  
!
!
! program history log:
!   2005-08-15  wu - oberror tuning
!   2008-03-24  wu - use convinfo ikx as index for oberr tune
!   2008-05-27  safford - rm unused vars
!   2008-12-03  todling - update in light of state vector and obs binning
!
! usage: intt(st,rt)
!   input argument list:
!     xhat    - increment in grid space
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use mpimod, only: ierror,mpi_comm_world,mpi_sum,mpi_rtype,mype
  use constants, only: one,zero
  use gsi_4dvar, only: nobs_bins
  use obsmod, only: qhead,qptr,thead,tptr,whead,wptr,pshead,psptr
  use converr, only:etabl
  use jfunc, only: jiterstart,jiter
  use convinfo, only:ictype,nconvtype,ioctype
  use state_vectors
  implicit none

! Declare passed variables

  type(state_vector),intent(in):: xhat

! Declare passed variables
  real(r_kind),save,dimension(33,200) ::  penalty,trace

! Declare local variables
  real(r_kind) err2

  integer(i_kind) i,n,k,l,m,ibin
  real(r_kind) tpenalty(33,nconvtype),ttrace(33,nconvtype)
  real(r_kind) valu,valv,val,so(33,nconvtype),cat_num(33,nconvtype),sosum,tcat_num(33,nconvtype)
  integer(i_kind) itype,ncat,k1


  ncat=nconvtype*33

  if(jiter==jiterstart)then
     trace=zero
     penalty=zero

   do ibin=1,nobs_bins

!    Moisture
     qptr => qhead(ibin)%head
     do while (associated(qptr))
        n=qptr%kx
        itype=ictype(n)
        
        if(itype==120)then
           k1=qptr%k1
        else
           k1=1 
        endif

        err2=qptr%raterr2*qptr%err2
!       err=sqrt(err2)
!       Forward model
        val= qptr%wij(1)* xhat%q(qptr%ij(1))+qptr%wij(2)* xhat%q(qptr%ij(2))&
            +qptr%wij(3)* xhat%q(qptr%ij(3))+qptr%wij(4)* xhat%q(qptr%ij(4))&
            +qptr%wij(5)* xhat%q(qptr%ij(5))+qptr%wij(6)* xhat%q(qptr%ij(6))&
            +qptr%wij(7)* xhat%q(qptr%ij(7))+qptr%wij(8)* xhat%q(qptr%ij(8))
        
        trace(k1,n)=trace(k1,n)-qptr%qpertb*val*err2
        penalty(k1,n)=penalty(k1,n)+(val-qptr%res)**2*err2
        qptr => qptr%llpoint
     end do
!    if(mype==29)write(0,*)'q2 trace,pen=',trace(k1,n),penalty(k1,n),k1,n

!    Temperature
     tptr => thead(ibin)%head
     do while (associated(tptr))
        n=tptr%kx
        itype=ictype(n)
        
        if(itype==120)then
           k1=tptr%k1
        else
           k1=1
        endif

        err2=tptr%raterr2*tptr%err2
!       err=sqrt(err2)
!       Forward model
        val= tptr%wij(1)* xhat%t(tptr%ij(1))+tptr%wij(2)* xhat%t(tptr%ij(2))&
            +tptr%wij(3)* xhat%t(tptr%ij(3))+tptr%wij(4)* xhat%t(tptr%ij(4))&
            +tptr%wij(5)* xhat%t(tptr%ij(5))+tptr%wij(6)* xhat%t(tptr%ij(6))&
            +tptr%wij(7)* xhat%t(tptr%ij(7))+tptr%wij(8)* xhat%t(tptr%ij(8))
        
        trace(k1,n)=trace(k1,n)-tptr%tpertb*val*err2
        penalty(k1,n)=penalty(k1,n)+(val-tptr%res)**2*err2
        tptr => tptr%llpoint
     end do
     
!    Surface pressure
     psptr => pshead(ibin)%head
     do while (associated(psptr))
        n=psptr%kx
        itype=ictype(n)
        k1=1

        err2=psptr%raterr2*psptr%err2
!       err=sqrt(err2)
!       Forward model
        val= psptr%wij(1)* xhat%p(psptr%ij(1))+psptr%wij(2)* xhat%p(psptr%ij(2))&
            +psptr%wij(3)* xhat%p(psptr%ij(3))+psptr%wij(4)* xhat%p(psptr%ij(4))
        
        trace(k1,n)=trace(k1,n)-psptr%ppertb*val*err2
        penalty(k1,n)=penalty(k1,n)+(val-psptr%res)**2*err2
        psptr => psptr%llpoint
     end do
     
!    Winds
     wptr => whead(ibin)%head
     do while (associated(wptr))
        n=wptr%kx
        itype=ictype(n)
        
        if(itype==220 .or. itype==223 .or. itype==233 .or. itype==245)then
           k1=wptr%k1
        else
           k1=1
        endif

        err2=wptr%raterr2*wptr%err2
!       err=sqrt(err2)
!       Forward model
        valu= wptr%wij(1)* xhat%u(wptr%ij(1))+wptr%wij(2)* xhat%u(wptr%ij(2))&
             +wptr%wij(3)* xhat%u(wptr%ij(3))+wptr%wij(4)* xhat%u(wptr%ij(4))&
             +wptr%wij(5)* xhat%u(wptr%ij(5))+wptr%wij(6)* xhat%u(wptr%ij(6))&
             +wptr%wij(7)* xhat%u(wptr%ij(7))+wptr%wij(8)* xhat%u(wptr%ij(8))
        valv= wptr%wij(1)* xhat%v(wptr%ij(1))+wptr%wij(2)* xhat%v(wptr%ij(2))&
             +wptr%wij(3)* xhat%v(wptr%ij(3))+wptr%wij(4)* xhat%v(wptr%ij(4))&
             +wptr%wij(5)* xhat%v(wptr%ij(5))+wptr%wij(6)* xhat%v(wptr%ij(6))&
             +wptr%wij(7)* xhat%v(wptr%ij(7))+wptr%wij(8)* xhat%v(wptr%ij(8))
        
        trace(k1,n)=trace(k1,n)-(wptr%upertb*valu+wptr%vpertb*valv)*err2
        penalty(k1,n)=penalty(k1,n)+((valu-wptr%ures)**2+(valv-wptr%vres)**2)*err2
        wptr => wptr%llpoint
     end do

   end do ! ibin
     

  else ! jiter
    cat_num=zero

    do ibin=1,nobs_bins

! Moisture
!    ratiomin=1.
     qptr => qhead(ibin)%head
     do while (associated(qptr))
        n=qptr%kx
        itype=ictype(n)
        
        if(itype==120)then
           k1=qptr%k1
        else
           k1=1
        endif
        
        err2=qptr%raterr2*qptr%err2
!       err=sqrt(err2)
!       Forward model
        val= qptr%wij(1)* xhat%q(qptr%ij(1))+qptr%wij(2)* xhat%q(qptr%ij(2))&
            +qptr%wij(3)* xhat%q(qptr%ij(3))+qptr%wij(4)* xhat%q(qptr%ij(4))&
            +qptr%wij(5)* xhat%q(qptr%ij(5))+qptr%wij(6)* xhat%q(qptr%ij(6))&
            +qptr%wij(7)* xhat%q(qptr%ij(7))+qptr%wij(8)* xhat%q(qptr%ij(8))
        
        cat_num(k1,n)=cat_num(k1,n)+1.
        trace(k1,n)=trace(k1,n)+qptr%qpertb*val*err2
        qptr => qptr%llpoint
     end do
     
!    if(mype==29)write(0,*)'q2 trace,pen=',trace(k1,n),cat_num(k1,n),k1,n
!    Temperature
     tptr => thead(ibin)%head
     do while (associated(tptr))
        n=tptr%kx
        itype=ictype(n)
        
        if(itype==120)then
           k1=tptr%k1
        else
           k1=1
        endif
        
        err2=tptr%raterr2*tptr%err2
!       err=sqrt(err2)
!       Forward model
        val= tptr%wij(1)* xhat%t(tptr%ij(1))+tptr%wij(2)* xhat%t(tptr%ij(2))&
            +tptr%wij(3)* xhat%t(tptr%ij(3))+tptr%wij(4)* xhat%t(tptr%ij(4))&
            +tptr%wij(5)* xhat%t(tptr%ij(5))+tptr%wij(6)* xhat%t(tptr%ij(6))&
            +tptr%wij(7)* xhat%t(tptr%ij(7))+tptr%wij(8)* xhat%t(tptr%ij(8))
        
        cat_num(k1,n)=cat_num(k1,n)+1.
        trace(k1,n)=trace(k1,n)+tptr%tpertb*val*err2
        tptr => tptr%llpoint
     end do
!    Surface pressure
     psptr => pshead(ibin)%head
     do while (associated(psptr))
        n=psptr%kx
        itype=ictype(n)
        k1=1
        
        err2=psptr%raterr2*psptr%err2
!       err=sqrt(err2)
!       Forward model
        val= psptr%wij(1)* xhat%p(psptr%ij(1))+psptr%wij(2)* xhat%p(psptr%ij(2))&
            +psptr%wij(3)* xhat%p(psptr%ij(3))+psptr%wij(4)* xhat%p(psptr%ij(4))
        
        cat_num(k1,n)=cat_num(k1,n)+1.
        trace(k1,n)=trace(k1,n)+psptr%ppertb*val*err2
        psptr => psptr%llpoint
     end do
!    Winds
     wptr => whead(ibin)%head
     do while (associated(wptr))
        n=wptr%kx
        itype=ictype(n)
        
        if(itype==220 .or. itype==223 .or. itype==233 .or. itype==245)then
           k1=wptr%k1
        else
           k1=1
        endif

        err2=wptr%raterr2*wptr%err2
!       err=sqrt(err2)
!       Forward model
        valu= wptr%wij(1)* xhat%u(wptr%ij(1))+wptr%wij(2)* xhat%u(wptr%ij(2))&
             +wptr%wij(3)* xhat%u(wptr%ij(3))+wptr%wij(4)* xhat%u(wptr%ij(4))&
             +wptr%wij(5)* xhat%u(wptr%ij(5))+wptr%wij(6)* xhat%u(wptr%ij(6))&
             +wptr%wij(7)* xhat%u(wptr%ij(7))+wptr%wij(8)* xhat%u(wptr%ij(8))
        valv= wptr%wij(1)* xhat%v(wptr%ij(1))+wptr%wij(2)* xhat%v(wptr%ij(2))&
             +wptr%wij(3)* xhat%v(wptr%ij(3))+wptr%wij(4)* xhat%v(wptr%ij(4))&
             +wptr%wij(5)* xhat%v(wptr%ij(5))+wptr%wij(6)* xhat%v(wptr%ij(6))&
             +wptr%wij(7)* xhat%v(wptr%ij(7))+wptr%wij(8)* xhat%v(wptr%ij(8))
        
        cat_num(k1,n)=cat_num(k1,n)+1.
        trace(k1,n)=trace(k1,n)+(wptr%upertb*valu+wptr%vpertb*valv)*err2
        wptr => wptr%llpoint
     end do
     
     do n=1,nconvtype
        do k=1,33
           trace(k,n)=cat_num(k,n)-trace(k,n)
        enddo
     enddo

    end do ! ibin

     call mpi_allreduce(trace,ttrace,ncat,mpi_rtype,mpi_sum, &
          mpi_comm_world,ierror)
     call mpi_allreduce(penalty,tpenalty,ncat,mpi_rtype,mpi_sum, &
          mpi_comm_world,ierror)
     call mpi_allreduce(cat_num,tcat_num,ncat,mpi_rtype,mpi_sum, &
          mpi_comm_world,ierror)
     if(mype==0)then
        do n=1,nconvtype 
           write(233,*)'obs type=',ictype(n),trim(ioctype(n))
           do k=1,33
              if(tcat_num(k,n)>0. .and. tcat_num(k,n)<10.)write(223,*)k,n,tcat_num(k,n)
              write(233,*)k,n,tpenalty(k,n),ttrace(k,n),tcat_num(k,n)
           enddo
        enddo
        
        so=one
        do n=1,nconvtype 
           do k=1,33
              if(ttrace(k,n) .ne. 0. .and. tcat_num(k,n)>10.) then
                 so(k,n)=tpenalty(k,n)/ttrace(k,n)
                 write(234,*)k,n,ictype(n),trim(ioctype(n)),so(k,n)
              endif
              if(so(k,n) .ge. zero) then
                 so(k,n)=sqrt(so(k,n))
              else
                 so(k,n)=one
              endif
           enddo
        enddo
        sosum=zero
        do i=1,ncat
           sosum=sosum+(so(i,1)-one)**2
        enddo
        write(235,*)'sosum=',sosum
        
!       Update etabl
        do i=1,nconvtype 
           if(trim(ioctype(i))=='t')then
              m=2
           elseif(trim(ioctype(i))=='q')then
              m=3
           elseif(trim(ioctype(i))=='uv')then
              m=4
           elseif(trim(ioctype(i))=='ps')then
              m=5
           else
              cycle
           endif
           l=ictype(i)
           
!          Enough obs to define the vertical profile
           if((l==120.and.m/=5) .or. l==220 .or. l==223 .or. l==233 .or. l==245)then
              write(235,*)l,trim(ioctype(i)),'33'
              do k=1,33
                 if( etabl(l,k,m) < 1.e8) etabl(l,k,m)=etabl(l,k,m)*so(k,i)
              end do
           else
              write(235,*)l,trim(ioctype(i)),'1'
              do k=1,33
                 if( etabl(l,k,m) < 1.e8) etabl(l,k,m)=etabl(l,k,m)*so(1,i)
              end do
           endif
        enddo
        
!       Write out err table 
        open(59,file='errtable_out',form='formatted')
        rewind 59
        do l=100,299
           if(etabl(l,1,1)==1100.)then
              write(59,100)l      
100           format(1x,i3,' OBSERVATION TYPE')
              do k=1,33
                 write(59,110)(etabl(l,k,i),i=1,6)
110              format(1x,6e12.5)
              end do
           endif !  etable1=1100
        end do
        close(59)
     endif ! mype==0
     
     call mpi_finalize(ierror)
     stop
  endif ! jiter
  return
end subroutine penal


