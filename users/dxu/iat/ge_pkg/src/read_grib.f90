SUBROUTINE read_grib(lu, filename,leng, idim, jdim, nlev, hgt, temp, uwind, vwind, rh, press, ierr)
!******************************************************
       INTEGER, PARAMETER :: isize =100
       INTEGER, INTENT(IN) :: lu, leng, idim, jdim, nlev
       CHARACTER (len=80), INTENT(IN) :: filename
       INTEGER, INTENT(OUT) :: ierr
       REAL, DIMENSION(idim, jdim, nlev), INTENT(INOUT) :: hgt, temp, uwind, vwind, rh
       REAL, DIMENSION(idim, jdim), INTENT(INOUT) :: press
!
       REAL, ALLOCATABLE, DIMENSION(:,:) :: grid
       LOGICAL, ALLOCATABLE, DIMENSION(:,:) :: lbms 
!
       INTEGER :: jpds(isize), jgds(isize), kpds(isize), kgds(isize)
!
       INTEGER :: i,j,k,n, ii, ll, status, IRET
!
       INTEGER, DIMENSION(14) :: levelmb = (/ 10, 20, 50, 100, 150, 200, 250, 300, 400, 500, 700, 850, 925, 1000 /)  
! Logarithm of surface pressure [Pa] kpds5=152 kpds6=109 kpds7=1
!       INTEGER, DIMENSION(6,2) :: idxkpds(6,2) = RESHAPE ((/ 7, 11, 33, 34, 52, 1, 156, 130, 131, 132, 157, 152 /), (/6,2/))
! Surface pressure [Pa] kpds5=134 kpds6=1 kpds7=0
       INTEGER, DIMENSION(6,2) :: idxkpds(6,2) = RESHAPE ((/ 7, 11, 33, 34, 52, 1, 156, 130, 131, 132, 157, 134 /), (/6,2/))
!
       ierr = 0
       call baopenr(lu,filename(1:leng),ierr)
       if(ierr.ne.0) then
       print *,'error opening file ',filename(1:leng)
       call abort
       endif
!
       N=-1
       print*,'idim,jdim  ',idim,jdim
       ALLOCATE (grid(idim,jdim), lbms(idim,jdim),  STAT=status)
       If (status .ne. 0 ) THEN
           write(*,*) "Allocation is not successful! Stop here."
           ierr = -1
           RETURN
       END IF 
       DO i = 1, idim
       DO j = 1, jdim
            press(i,j) = -999
       DO k = 1, nlev
            hgt(i,j,k) = -999
            temp(i,j,k) = -999
            uwind(i,j,k) = -999
            vwind(i,j,k) = -999
            rh(i,j,k) = -999
       END DO
       END DO
       END DO 
 100   continue
       N=N+1
!
       DO k=1, isize
          jpds(k)=-1
          kpds(k)=0
          jgds(k)=-1
          kgds(k)=0
       END DO
!
       CALL GETGB(lu,0,idim*jdim,N,JPDS,JGDS,      &
                 NDATA,KSKP,KPDS,KGDS,LBMS,GRID,IRET)
!
       IF (iret.eq.0) THEN
!
!   If it is press surface
!   ECMWF - Logarithm of surface pressure [Pa] kpds5=152 kpds6=109 kpds7=1
!   ECMWF - Surface pressure [Pa] kpds5=134 kpds6=1 kpds7=0
!
           IF (((kpds(5).eq.idxkpds(6,1)).or.(kpds(5).eq.idxkpds(6,2))).and.((kpds(6).eq.1).or.(kpds(6).eq.1))) Then
                  DO i=1,idim
                  DO j=1,jdim
                         press(i,j)=grid(i,j)
                  END DO
                  END DO
            END IF

           IF (kpds(6) .eq. 100) THEN
!   Find whether the field is  hgt, temp, uwind, vwind, rh, press?
               do5: DO i = 1, 5
                  ii = 0
                  IF ( (kpds(5) .eq. idxkpds(i,1)) .or. (kpds(5) .eq. idxkpds(i,2))) THEN
                      ii = i
                      EXIT do5
                  END IF
                END DO do5
                IF (ii .ne. 0 ) THEN
!   Find which level
                   do14: DO i = 1, 14   
                       ll = 0
                       IF ( kpds(7) .eq. levelmb(i)) THEN
                          ll = i
                          EXIT do14
                       END IF
                   END DO do14
                   IF ( ll .ne. 0 ) THEN
                       SELECT CASE ( ii )
                       CASE (1)
                          DO i=1,idim
                          DO j=1,jdim
                             hgt(i,j,ll)=grid(i,j)
                          END DO
                          END DO
                       CASE (2)
                          DO i=1,idim
                          DO j=1,jdim
                             temp(i,j,ll)=grid(i,j)
                          END DO
                          END DO
                       CASE (3)
                          DO i=1,idim
                          DO j=1,jdim
                             uwind(i,j,ll)=grid(i,j)
                          END DO
                          END DO
                       CASE (4)
                          DO i=1,idim
                          DO j=1,jdim
                             vwind(i,j,ll)=grid(i,j)
                          END DO
                          END DO
                       CASE (5)
                          DO i=1,idim
                          DO j=1,jdim
                             rh(i,j,ll)=grid(i,j)
                          END DO
                          END DO
                       END SELECT
                    END IF 
                END IF 
           END IF
       ELSE

          PRINT *,'rec ',n+1, IRET
          call baclose(lu,ierr)
          DEALLOCATE(grid, lbms,  STAT=status)
          If (status .ne. 0 ) THEN
             write(*,*) "Deallocation is not successful! Stop here."
             ierr = -1
          END IF
 
          RETURN
!
       END IF
!
       go to 100
!
    
       END SUBROUTINE read_grib
