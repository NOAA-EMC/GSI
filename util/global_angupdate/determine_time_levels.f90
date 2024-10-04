subroutine determine_time_levels(iyy1,imm1,idd1,ihh1,&
       iyy2,imm2,idd2,ihh2,dth,ntime,iyy,imm,idd,ihh,&
       maxtim)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    determine_time_levels                deter time levls
!   prgmmr: treadon          org: np20                date: 2005-07-22
!
! abstract:  This routine counts the number of time levels
!            between date1 and date2 when stepping by dth
!            hours.
!
! program history log:
!   2005-07-22 treadon - original code
!
!   input argument list:
!     iyy1,imm1,idd1,ihh1 - starting year,month,day,hour
!     iyy2,imm2,idd2,ihh2 - ending year,month,day,hour
!     dth - time increment in hours
!
!   output argument list:
!     ntime - number of time levels between date1 and date2
!     iyy,imm,idd,ihh - array containing each time level
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  implicit none
  
  real,parameter:: zero = 0.0

  logical:: done
  integer:: iyy1,imm1,idd1,ihh1,iyy2,imm2,idd2,ihh2
  integer:: ntime,maxtim
  integer,dimension(maxtim):: iyy,imm,idd,ihh
  integer,dimension(8):: ida,jda,lda
  real:: dth
  real,dimension(5):: fha

!---------------------------------------------------------------------------
! Load date/time arrays with starting and ending time levels
  fha=zero
  ida=0; lda=0
  fha(2)=dth
  ida(1)=iyy1
  ida(2)=imm1
  ida(3)=idd1
  ida(4)=0
  ida(5)=ihh1
  jda=ida

  lda(1)=iyy2
  lda(2)=imm2
  lda(3)=idd2
  lda(4)=0
  lda(5)=ihh2

! Use w3 routine w3movdat to step from date1 to date2
! at the interval of dth hours.  Count the number of
! time levels between date1 and date2.

  ntime=0
  done=.false.
  do while (.not.done)
     ntime=ntime+1
     if (ntime>maxtim) then
        write(6,*)'GLOBAL_ANGUPDATE:  ***WARNING*** ntime=',ntime,' > ',maxtim
        ntime=maxtim
     endif

     if ( (jda(1)==lda(1)) .and. (jda(2)==lda(2)) .and. &
          (jda(3)==lda(3)) .and. (jda(5)==lda(5)) ) then
        done = .true.
     endif

     iyy(ntime)=jda(1)
     imm(ntime)=jda(2)
     idd(ntime)=jda(3)
     ihh(ntime)=jda(5)

     ida=jda
     jda=0
     call w3movdat(fha,ida,jda)

  end do
  return
end subroutine determine_time_levels
