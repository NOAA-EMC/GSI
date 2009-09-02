!
!=============================================================================
subroutine hilbert(mskip,nob,xob,yob,test_set)
!=============================================================================
use phil
use phil1
use kinds, only: r_kind,i_kind

implicit none

integer(i_kind),parameter       :: n=100,ngen=15,nskip=10
!real(r_kind),parameter          :: delta=.001_r_kind,xhskip=.00035_r_kind
 real(r_kind),parameter          :: delta=.001_r_kind,xhskip=.00030_r_kind
!real(r_kind),parameter          :: delta=.001_r_kind,xhskip=.00025_r_kind
!real(r_kind),parameter          :: delta=.001_r_kind,xhskip=.00001_r_kind
!real(r_kind),parameter          :: delta=.001_r_kind,xhskip=.005_r_kind

integer(i_kind),             intent(  IN ):: mskip,nob
real(r_kind), dimension(nob),intent(  IN ):: xob,yob

integer(i_kind), dimension(nob),intent(  OUT):: test_set

real(r_kind)                    :: x0,s,b,f,cran,cranp,cranm,x,xp,xm &
                          ,dcrandx,tcdx,dcdx &
                          ,xhprev
real(r_kind)                    :: cran1,cran2
real(r_kind),dimension(nob)     :: xh
real(r_kind),dimension(0:n)     :: xg,yg
integer(i_kind),dimension(nob)  :: next,nextr
integer(i_kind),dimension(ngen) :: hil4
integer(i_kind)                 :: iob,i,iskip,isamp         &
                          ,ioba,iobr,firsta,count
integer(i_kind),dimension(mskip):: firstb
!=============================================================================
! Map these data to a Hilbert curve using xy_to_hil, convert the base-4
! representation of the hilbert parameter to an ordinary real, using
! hil_to_r. Then order the obs along this space-filling curve by invoking
! the efficient sorting procedure, bsort. The resulting linked list of obs
! is accessed by "firsta" and terminated when "next(iob)" ==0.
!
! From this linked list, "A", construct linked subsets, "B", that can be used
! us validation subsets. Members of old A not able to be put into any of the
! mskip B subset are gathered into a reconstritued lined list, new "A". All
! this is done in subroutine getvalsets.
!=============================================================================

do iob=1,nob
   call xy_to_hil(ngen,xob(iob),yob(iob),hil4)
   call hil_to_r(1,ngen,hil4,xh(iob))
enddo
call bsort(1,nob,xh,next,firsta)

do iob=1,nob
!   write(4,'(i4,2f10.3,15i3,f10.3,3x,i5)') iob,xob(iob),yob(iob),hil4,xh(iob),next(iob)
enddo


count=0
iob=firsta
do isamp=1,nob
   if(iob==0)exit
   count=count+1
   iob=next(iob)
!write(9,'(3i5,2x,f10.3,2x,i5)')isamp,count,iob,xh(iob),next(iob)
enddo
!print'('' total count='',i6)',count

call getvalsets(nob, mskip,xhskip,xh,next, firsta,firstb)

!write(113,'(11i5)') firsta,firstb
do isamp=1,nob
!write(113,'(2i5)')isamp,next(isamp)
enddo

test_set=0

do iskip=1,mskip
   count=0
   iob=firstb(iskip)
   do isamp=1,nob
      if(iob==0)exit
      count=count+1
!        write(8,'(a2,5i5,2x,2f10.3,i5)')'A:',iskip,firstb(iskip),isamp,count,iob,xob(iob),yob(iob),iskip
      test_set(iob)=iskip
      iob=next(iob)
!        write(8,'(a2,5i5,2x,2f10.3,i5)')'  ',iskip,firstb(iskip),isamp,count,iob,xob(iob),yob(iob),test_set(iob)
   enddo
!   print'('' count for subset,iskip='',i4,''  is count='',i5)',iskip,count
!   write(8,'(a6,2i5)')'count:',iskip,count
enddo

count=0
iob=firsta
do isamp=1,nob
   if(iob==0)exit
   count=count+1
   iob=next(iob)
!   xob_set_residual(count)=xob(iob)
!   yob_set_residual(count)=yob(iob)
!   icount_residual=count
enddo
!print'('' residual count='',i6)',count

end subroutine hilbert
