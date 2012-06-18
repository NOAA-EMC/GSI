subroutine read_satang(lunsat,satsis,nstep,mstep,n_chan,rmiss,satang)

  implicit none

  integer lunsat,n_chan,nstep,mstep,iflag
  integer ich,isat,ichan,ip,iang,j,isatid,i
  real rmiss,tlapm
  real,dimension(90):: cbiasx
  real,dimension(mstep,n_chan):: satang

   character(len=20) :: isis,satsis


!***************************************************************
! Initialize output array to missin value
  do j=1,n_chan
     do i=1,mstep
        satang(i,j) = rmiss
     end do
  end do


! Loop through file.  Find information for given satellite'

  j=0
  do  
     read(lunsat,100,IOSTAT=iflag) ich,isis,ichan,tlapm,(cbiasx(ip),ip=1,90)
     if( iflag /= 0 ) exit
100  format(I5,1x,A20,1x,I5,e15.6/9(4x,10f7.3/))
     if (trim(isis) == trim(satsis)) then
        j=j+1
        if (j>n_chan) then
           write(6,*)'*** READ_SATANG *** ERROR:  too many channels'
           write(6,*)' passed n_chan=',n_chan
           write(6,*)'   currently j=',j
           call errexit(94)
        endif
        do ip=1,nstep
           satang(ip,j) = cbiasx(ip)
        end do
        cycle 
     endif
enddo

! End of routine
  return
end subroutine read_satang
