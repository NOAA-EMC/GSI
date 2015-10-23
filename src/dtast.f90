subroutine dtast(work1,nlev,pbot,ptop,mesage,jiter,iout,pflag)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    dtast       print table of scalar innovations
!   prgmmr: parrish          org: np22                date: 1990-10-11
!
! abstract: print table of scalar innovations by data type.
!
! program history log:
!   1990-10-11  parrish
!   1998-04-05  weiyu yang
!   2004-06-15  treadon - reformat documenation
!   2004-08-25  derber - remove hardwire of ntype and add intent
!   2004-10-12  parrish - modifications for nonlinear qc
!   2005-07-27  derber  - add print of monitoring and reject data
!   2006-02-24  derber  - modify to take advantage of convinfo module
!   2006-04-03  derber  - modify to print individual ob types
!   2008-06-04  safford - rm unused var
!
!   input argument list:
!     work1    - array containing innovation (o-g) sums
!     nlev     - number of pressure levels
!     pbot     - pressure at bottom of layer
!     ptop     - pressure at top of layer
!     mesage   - message to appear at top of table ($ signals end)
!     jiter    - external iteration
!     iout     - unit to which to write statistics
!     pflag    - flag whether to use this data
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  use kinds, only: r_kind,i_kind
  use constants, only: zero
  use convinfo, only: nconvtype,ictype,icsubtype,ioctype
  use qcmod, only: npres_print
  implicit none
   
  integer(i_kind)                                     ,intent(in   ) :: iout,nlev,jiter
  real(r_kind)   ,dimension(npres_print,nconvtype,5,3),intent(in   ) :: work1
  real(r_kind)   ,dimension(nlev)                     ,intent(in   ) :: pbot,ptop
  logical        ,dimension(nconvtype)                ,intent(in   ) :: pflag
  character(100)                                      ,intent(in   ) :: mesage

  character(1),dimension(240):: cline
  character(8) obstyp
  character(4),dimension(3):: typx
  character(14):: label
  integer(i_kind) i,ilin,j,imsg,k,nn
  integer(i_kind),dimension(nlev):: countall
  integer(i_kind),dimension(nlev,nconvtype,3):: count
  real(r_kind),dimension(nlev):: rmsall,biasall,ratall,qcratall
  real(r_kind),dimension(nlev):: rmsx,biasx,ratx,qcratx
  real(r_kind),dimension(nlev,nconvtype,3):: rms,bias,rat,qcrat
  

! Initialize variables
  count=0; rms=zero; bias=zero; rat=zero; qcrat=zero 
  typx(1)='    '
  typx(2)='rej '
  typx(3)='mon '

! First, print message and level information

  do k=1,240
     cline(k) = '-'
  end do
  imsg=max(1,index(mesage,'$')-1)
  ilin=max(imsg,min(nlev*9+34,240))
  write(iout,505) mesage(1:imsg)
  if (nlev > 1) then
     write(iout,510)'                              ptop  ',(ptop(k),k=1,nlev)
     write(iout,510)'     it     obs    type styp  pbot  ',(pbot(k),k=1,nlev)
  end if
  write(iout,500) (cline(i),i=1,ilin)
500 format(240a1)
505 format(a)
510 format(a35,12(f8.1,1x))

! Transfer to local work arrays.  Compute statistics
  do j = 1,nconvtype
     if(pflag(j))then
        do i = 1,nlev
           count(i,j,1) = nint(work1(i,j,1,1))    ! data count used
           count(i,j,2) = nint(work1(i,j,1,2))    ! data count rejected
           count(i,j,3) = nint(work1(i,j,1,3))    ! data count monitored
           if(count(i,j,1) > 0)then
              bias(i,j,1)  = work1(i,j,2,1)        ! bias used
              rms(i,j,1)   = work1(i,j,3,1)        ! rms used
              rat(i,j,1)   = work1(i,j,4,1)        ! penalty used
              qcrat(i,j,1) = work1(i,j,5,1)        ! nonlin qc penalty used
           end if
           if(count(i,j,2) > 0)then
              bias(i,j,2)  = work1(i,j,2,2)        ! bias rejected
              rms(i,j,2)   = work1(i,j,3,2)        ! rms rejected
              rat(i,j,2)   = work1(i,j,4,2)        ! penalty rejected
              qcrat(i,j,2) = work1(i,j,5,2)        ! nonlin qc penalty rejected
           end if
           if(count(i,j,3) > 0)then
              bias(i,j,3)  = work1(i,j,2,3)        ! bias monitored
              rms(i,j,3)   = work1(i,j,3,3)        ! rms monitored
              rat(i,j,3)   = work1(i,j,4,3)        ! penalty monitored
              qcrat(i,j,3) = work1(i,j,5,3)        ! nonlin qc penalty monitored
           end if
        end do
     end if
  end do


! Print statistics for single level obs (e.g., surface pressure)     
  if (nlev == 1) then
     write(iout,600) ptop(1),pbot(1)
     write(iout,610)
600  format(1x,'pressure levels (hPa)=',f6.1,1x,f6.1)
610  format(5x,'it ',4x,'obs    type stype',4x,'count',6x,'bias',7x,'rms',6x,'cpen',5x,'qcpen')
     do nn=1,3
        countall(1)=0
        biasall(1)=zero
        rmsall(1)=zero
        ratall(1)=zero
        qcratall(1)=zero
        do i=1,nconvtype
           if(pflag(i) .and. count(1,i,nn) > 0)then
              biasx(1)=bias(1,i,nn)/count(1,i,nn)
              rmsx(1)=sqrt(rms(1,i,nn)/count(1,i,nn))
              ratx(1)=rat(1,i,nn)/count(1,i,nn)
              qcratx(1)=qcrat(1,i,nn)/count(1,i,nn)
              countall(1)=countall(1)+count(1,i,nn)
              biasall(1)=biasall(1)+bias(1,i,nn)
              rmsall(1)=rmsall(1)+rms(1,i,nn)
              ratall(1)=ratall(1)+rat(1,i,nn)
              qcratall(1)=qcratall(1)+qcrat(1,i,nn)
              write(obstyp,690) ictype(i),icsubtype(i)
              write(label,100) jiter,trim(ioctype(i))
              write(iout,700) label,typx(nn),obstyp,count(nlev,i,nn),biasx(1),rmsx(1),ratx(1),qcratx(1)
           end if
        end do
        if(countall(1) > 0)then
           biasx(1)=biasall(1)/countall(1)
           rmsx(1)=sqrt(rmsall(1)/countall(1))
           ratx(1)=ratall(1)/countall(1)
           qcratx(1)=qcratall(1)/countall(1)
           obstyp='all'
           write(label,100) jiter,'   '
           write(iout,700) label,typx(nn),obstyp,countall(1),biasx(1),rmsx(1),ratx(1),qcratx(1)
        end if
     end do
690  format(i3.3,1x,i4.4)
700  format(1x,a14,1x,a4,a8,1x,i9,1x,f9.4,1x,f9.4,1x,f9.4,1x,f9.4)

! Print statistics for multi-level obs
  else
     do nn=1,3
        countall=0
        biasall=zero
        rmsall=zero
        ratall=zero
        qcratall=zero
        do i = 1,nconvtype
           if(pflag(i) .and. count(nlev,i,nn) > 0)then
              biasx=zero
              rmsx=zero
              ratx=zero
              qcratx=zero
              do k=1,nlev
                 if(count(k,i,nn) > 0)then
                    biasx(k)=bias(k,i,nn)/count(k,i,nn)
                    rmsx(k)=sqrt(rms(k,i,nn)/count(k,i,nn))
                    ratx(k)=rat(k,i,nn)/count(k,i,nn)
                    qcratx(k)=qcrat(k,i,nn)/count(k,i,nn)
                    countall(k)=countall(k)+count(k,i,nn)
                    biasall(k)=biasall(k)+bias(k,i,nn)
                    rmsall(k)=rmsall(k)+rms(k,i,nn)
                    ratall(k)=ratall(k)+rat(k,i,nn)
                    qcratall(k)=qcratall(k)+qcrat(k,i,nn)
                 end if
              end do
              write(obstyp,690) ictype(i),icsubtype(i)
              write(label,100) jiter,trim(ioctype(i))
              write(iout,720) label,typx(nn),obstyp,'count',(count(k,i,nn),k=1,nlev)
              write(iout,800) label,typx(nn),obstyp,'bias',(biasx(k),k=1,nlev)
              write(iout,800) label,typx(nn),obstyp,'rms',(rmsx(k),k=1,nlev)
              write(iout,800) label,typx(nn),obstyp,'cpen',(ratx(k),k=1,nlev)
              write(iout,800) label,typx(nn),obstyp,'qcpen',(qcratx(k),k=1,nlev)
           end if
        end do
        if(countall(nlev) > 0)then
           biasx=zero
           rmsx=zero
           ratx=zero
           qcratx=zero
           do k=1,nlev
              if(countall(k) > 0)then
                 biasx(k)=biasall(k)/countall(k)
                 rmsx(k)=sqrt(rmsall(k)/countall(k))
                 ratx(k)=ratall(k)/countall(k)
                 qcratx(k)=qcratall(k)/countall(k)
              end if
           end do
           obstyp='all'
           write(label,100) jiter,'   '
           write(iout,720) label,typx(nn),obstyp,'count',(countall(k),k=1,nlev)
           write(iout,800) label,typx(nn),obstyp,'bias',(biasx(k),k=1,nlev)
           write(iout,800) label,typx(nn),obstyp,'rms',(rmsx(k),k=1,nlev)
           write(iout,800) label,typx(nn),obstyp,'cpen',(ratx(k),k=1,nlev)
           write(iout,800) label,typx(nn),obstyp,'qcpen',(qcratx(k),k=1,nlev)
        end if

     end do
100  format('o-g ',i2.2,1x,a7)
720  format(1x,a14,1x,a4,a8,1x,a5,1x,i8,1x,10(i8,1x),i8)
800  format(1x,a14,1x,a4,a8,1x,a5,1x,f8.2,1x,10(f8.2,1x),f8.2)
     
  endif
  return
end subroutine dtast
