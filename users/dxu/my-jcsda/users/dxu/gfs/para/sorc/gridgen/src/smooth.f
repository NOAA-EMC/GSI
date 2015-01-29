   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: smth_desmth_egrid
   !
   ! Purpose: Apply the smoother-desmoother from the MM5 program TERRAIN
   !   (found in smther.F) to array.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine smth_desmth_egrid(array, lsmask, start_x, end_x, &
               start_y, end_y, start_z, end_z, npass, hflag)

      implicit none

      ! Arguments
      integer, intent(in) :: start_x, start_y, start_z
      integer, intent(in) :: end_x, end_y, end_z
      integer, intent(in) :: npass
      integer, intent(in) :: hflag
      real, dimension(start_x:end_x, start_y:end_y), intent(in) :: lsmask
      real, dimension(start_x:end_x, start_y:end_y, start_z:end_z), &
               intent(inout) :: array

      ! Local variables
      integer :: ix, iy, iz, ipass
      real, pointer, dimension(:,:,:) :: scratch, array_save
      integer :: ihe(start_y:end_y),ihw(start_y:end_y),istart(start_y:end_y)
      real, parameter:: cenwgt = 1.52
      real, parameter:: endwgt = 0.13

      allocate(scratch(start_x:end_x, start_y:end_y, start_z:end_z))
      allocate(array_save(start_x:end_x, start_y:end_y, start_z:end_z))
      array_save = array

      do iy=start_y,end_y
         if (hflag == 1) then
            ihe(iy) = abs(mod(iy+1,2))
            ihw(iy) = ihe(iy)-1
         else
            ! assign ive,ivw equivs to ihe,ihw
            ihe(iy) = abs(mod(iy,2))
            ihw(iy) = ihe(iy)-1
         end if
      end do

      do iy=start_y,end_y
         if (hflag == 1) then
            if (mod(iy,2) == 0) then
               istart(iy) = start_x
            else
               istart(iy) = start_x+1
            endif
         else ! v points
            if (abs(mod(iy,2)) == 1) then
               istart(iy) = start_x
            else
               istart(iy) = start_x+1
            endif
         endif
      end do

      do ipass=1,npass

         !
         ! Smoothing pass
         !

         do iy=start_y,end_y
            do ix=start_x,end_x
               do iz=start_z,end_z
                 scratch(ix,iy,iz) = array(ix,iy,iz) 
               end do
            end do
         end do

         do iy=start_y+1,end_y-1
            do ix=istart(iy),end_x-1
               do iz=start_z,end_z
                  scratch(ix,iy,iz) = 0.50*array(ix,iy,iz)+ &
                                      0.125*(array(ix+ihw(iy),iy-1,iz)+array(ix+ihe(iy),iy+1,iz)+ &
                                             array(ix+ihw(iy),iy+1,iz)+array(ix+ihe(iy),iy-1,iz))
               end do
            end do
         end do

         !
         ! Desmoothing pass
         !

         do iy=start_y+2,end_y-2
            do ix=istart(iy),end_x-1
               do iz=start_z,end_z
                  array(ix,iy,iz) = cenwgt*scratch(ix,iy,iz) - &
                                    endwgt*(scratch(ix+ihw(iy),iy-1,iz)+scratch(ix+ihe(iy),iy+1,iz) + &
                                            scratch(ix+ihw(iy),iy+1,iz)+scratch(ix+ihe(iy),iy-1,iz))
               end do
            end do
         end do

      end do

      deallocate(scratch)

!cggg don't allow smoother to affect any water points.  it makes the waterfall removing
! code less effective.

      do iy=start_y, end_y
      do ix=start_x, end_x     
        if (lsmask(ix,iy) == 0.0) then
          array(ix,iy,:) = array_save(ix,iy,:)
        endif
      enddo
      enddo

      deallocate (array_save)


 end subroutine smth_desmth_egrid

 subroutine smdhld(ime,jme,h,s,lines,nsmud)

! INPUTS:

!   IME - x dimension limit (IM)
!   JME - j dimension limit (JM)
!     H - input/output topography field
!     S - input land/sea mask (1=water, 0=land)
! LINES - number of rows over which the more severe 
!         5-point smoothing is applied (=12 in SI)
! NSMUD - number of smoothing passes (=12 in SI)

! OUTPUTS:
!     H - input/output topography field

!cggg setting to zero removes orog from channel islands
      parameter(hthresh=0.0)
!cggg      parameter(hthresh=50.0)
      dimension ihw(jme),ihe(jme)
      dimension h(ime,jme),s(ime,jme),h_save(ime,jme)   &
               ,hbms(ime,jme),hne(ime,jme),hse(ime,jme)
!-----------------------------------------------------------------------
          do j=1,jme
      ihw(j)=-mod(j,2)
      ihe(j)=ihw(j)+1
          enddo
!-----------------------------------------------------------------------

              do j=1,jme
          do i=1,ime
      h_save(i,j)=h(i,j)
      hbms(i,j)=1.-s(i,j)
          enddo
              enddo
!
      jmelin=jme-lines+1
      ibas=lines/2
      m2l=mod(lines,2)
!
              do j=lines,jmelin
          ihl=ibas+mod(j,2)+m2l*mod(j+1,2)
          ihh=ime-ibas-m2l*mod(j+1,2)

          do i=ihl,ihh
      hbms(i,j)=0.
          enddo
              enddo
!-----------------------------------------------------------------------
!cggg                  do ks=1,nsmud
                  do ks=1,12

		write(6,*) 'H(1,1): ', h(1,1)
		write(6,*) 'H(3,1): ', h(1,1)
!-----------------------------------------------------------------------
              do j=1,jme-1
          do i=1,ime-1
      hne(i,j)=h(i+ihe(j),j+1)-h(i,j)
          enddo
              enddo
              do j=2,jme
          do i=1,ime-1
      hse(i,j)=h(i+ihe(j),j-1)-h(i,j)
          enddo
              enddo
!
              do j=2,jme-1
          do i=1+mod(j,2),ime-1
      h(i,j)=(hne(i,j)-hne(i+ihw(j),j-1)     &
             +hse(i,j)-hse(i+ihw(j),j+1))*hbms(i,j)*0.125+h(i,j)
          enddo
              enddo
!-----------------------------------------------------------------------

!	special treatment for four corners

	if (hbms(1,1) .eq. 1) then
	h(1,1)=0.75*h(1,1)+0.125*h(1+ihe(1),2)+    &
                                  0.0625*(h(2,1)+h(1,3))
	endif

	if (hbms(ime,1) .eq. 1) then
	h(ime,1)=0.75*h(ime,1)+0.125*h(ime+ihw(1),2)+    &
                                  0.0625*(h(ime-1,1)+h(ime,3))
	endif

	if (hbms(1,jme) .eq. 1) then	
	h(1,jme)=0.75*h(1,jme)+0.125*h(1+ihe(jme),jme-1)+   &
                                 0.0625*(h(2,jme)+h(1,jme-2))
	endif

	if (hbms(ime,jme) .eq. 1) then	
	h(ime,jme)=0.75*h(ime,jme)+0.125*h(ime+ihw(jme),jme-1)+  &
                                  0.0625*(h(ime-1,jme)+h(ime,jme-2))
	endif


!	S bound
	
	J=1
	do I=2,ime-1
	if (hbms(I,J) .eq. 1) then	
	h(I,J)=0.75*h(I,J)+0.125*(h(I+ihw(J),J+1)+h(I+ihe(J),J+1))
	endif
	enddo

!	N bound

	J=JME
	do I=2,ime-1
	if (hbms(I,J) .eq. 1) then	
	h(I,J)=0.75*h(I,J)+0.125*(h(I+ihw(J),J-1)+h(I+ihe(J),J-1))
	endif
	enddo

! 	W bound

	I=1
	do J=3,jme-2
	if (hbms(I,J) .eq. 1) then	
	h(I,J)=0.75*h(I,J)+0.125*(h(I+ihe(J),J+1)+h(I+ihe(J),J-1))
	endif
	enddo

! 	E bound

	I=IME
	do J=3,jme-2
	if (hbms(I,J) .eq. 1) then	
	h(I,J)=0.75*h(I,J)+0.125*(h(I+ihw(J),J+1)+h(I+ihw(J),J-1))
	endif
	enddo


                      enddo   ! end ks loop

!!	touch with a 5-point filter over isolated peaks?

       do ks=1,nsmud

	do J=lines-1,jme-(lines-1)
          ihl=ibas+mod(j,2)+m2l*mod(j+1,2)
          ihh=ime-ibas-m2l*mod(j+1,2)

!	do I=lines-1,ime-(lines-1)
	do I=ihl,ihh

!cggg don't smooth isolated islands
        if ( s(i+ihw(J),J+1) > 0. .and.   &
             s(I+ihe(J),J+1) > 0. .and.   &
             s(i+ihw(J),J-1) > 0. .and.   &
             s(I+ihe(J),J-1) > 0. ) cycle

	if (s(I,J) .eq. 0 ) then

        if( (h(I,J)-h(i+ihw(J),J+1) .gt. hthresh .and.   &
            h(I,J)-h(I+ihe(J),J+1) .gt. hthresh .and.    &
            h(I,J)-h(i+ihw(J),J-1) .gt. hthresh .and.    &
            h(I,J)-h(I+ihe(J),J-1) .gt. hthresh ) ) then


	h(I,J)=h(I,J)+0.125*( h(i+ihw(J),J+1) + h(I+ihe(J),J+1) +   &
        	 	      h(i+ihw(J),J-1) + h(I+ihe(J),J-1) -   &
                              4*h(I,J) )

	endif
	endif
        enddo
        enddo

	enddo

	write(6,*) 'smoothing change to topography:'
	do j=jme,1,-jme/30
	write(6,683) ( (h(i,j)-h_save(i,j)),i=1,ime,ime/15 )
	enddo
	
  683	format(20(f5.0,1x))

      return
      end
