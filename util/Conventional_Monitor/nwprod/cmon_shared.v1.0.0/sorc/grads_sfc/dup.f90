! this is a subroutine to check th eduplicate and remove them

subroutine hash(duparr,n,m,ilat,ilon,ipress,itime,iweight,nndup)

!
!	remove duplicate lat/lon from duparr
!
           implicit none

	real(4),intent(inout),dimension(m,n) :: duparr

        integer ihash(2*n)
        integer ifn
        integer n,m,ilat,ilon,iweight,nndup,i
        integer itime,jj,ipress
        real dlat,dlon,dtime,dpress

        write(*,*) 'n,m,ilat,ilon,itime,iweight,nndup = ',n,m,ilat,ilon,itime,iweight,nndup
        ihash = 0

        nndup=0
        do i = 1, n
        do jj=i+1,n
                dlat=abs(duparr(ilat,i)-duparr(ilat,jj))
                dlon=abs(duparr(ilon,i)-duparr(ilon,jj))
                dtime=abs(duparr(itime,i)-duparr(itime,jj))
                dpress=abs(duparr(ipress,i)-duparr(ipress,jj))

                if (dlat <10.0e-5 .and. dlon <10.0e-5 .and. dtime <10.0e-5 &
                     .and. dpress <10.0e-5 ) then 

!                if(duparr(ilat,i) == duparr(ilat,jj) .and. duparr(ilon,i) == duparr(ilon,jj) &
!                   .and.duparr(itime,i) == duparr(itime,jj) .and. duparr(ipress,i) == duparr(ipress,jj)) then
                   duparr(iweight,i)=-1.0
                    nndup=nndup+1
                endif
        enddo
        enddo
        write(*,*) 'num dups found by hash is ',nndup
        return
     end 
