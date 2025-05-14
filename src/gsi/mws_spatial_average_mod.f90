Module mws_spatial_average_mod
!
!
! abstract:  This routine reads BUFR format MWS radiance 
!            (brightness temperature) files, spaatially 
!            averages the data using the AAPP averaging routines
!            and writes the data back into BUFR format
!
!
!
! Program history log:
!    2025-01-01   j.jin   - Modified from ATMS_Spatial_Average_Mod
! 

  use kinds, only: r_kind,r_double,i_kind
  use ATMS_Spatial_Average_Mod, only: MODIFY_BEAMWIDTH_atms
  implicit none     


! Declare module level parameters
  real(r_double), parameter    :: Missing_Value=1.e11_r_double

CONTAINS 

  SUBROUTINE mws_spatial_average(Num_Obs, NChanl, FOV, Time, BT_InOut, &
       Scanline, Error_Status)

    IMPLICIT NONE
    
    ! Declare passed variables
    integer(i_kind) ,intent(in   ) :: Num_Obs, NChanl
    integer(i_kind) ,intent(in   ) :: Fov(num_obs)
    real(r_kind)    ,intent(in   ) :: Time(Num_Obs)
    real(r_kind)    ,intent(inout) :: BT_InOut(NChanl,Num_Obs)
    integer(i_kind) ,intent(  out) :: Scanline(Num_Obs)
    integer(i_kind) ,intent(  out) :: Error_Status

    ! Declare local parameters
    INTEGER(I_KIND), PARAMETER :: nxmax_mws=128  !Max number of spots per scan line
    INTEGER(I_KIND), PARAMETER :: nymax_mws=9600 !Max number of lines. Allows 6hrs of MWS.
                                                 !MWS conducts a scan every 2.254 second.
    integer(i_kind), parameter :: mws1c_h_wmosatid=24
    integer(i_kind), parameter :: lninfile=15
    integer(i_kind), parameter :: max_fov=95
    real(r_kind), parameter    :: Scan_Interval = 2.254_r_kind ! second
    ! Maximum number of channels 
    integer(i_kind), parameter :: MaxChans = 24
    ! Minimum allowed BT as a function of channel number
    real(r_kind), parameter :: MinBT(MaxChans) = &
         (/ 120.0_r_kind, 120.0_r_kind, 190.0_r_kind, 200.0_r_kind, &
            200.0_r_kind, 200.0_r_kind, 200.0_r_kind, 200.0_r_kind, &
            190.0_r_kind, 190.0_r_kind, 180.0_r_kind, 180.0_r_kind, &
            180.0_r_kind, 190.0_r_kind, 200.0_r_kind, 200.0_r_kind, &
            120.0_r_kind, 120.0_r_kind, 120.0_r_kind, 150.0_r_kind, &
            170.0_r_kind, 180.0_r_kind, 190.0_r_kind, 190.0_r_kind /)
    ! Maximum allowed BT as a function of channel number
    real(r_kind), parameter :: MaxBT(MaxChans) = &
         (/ 320.0_r_kind, 320.0_r_kind, 300.0_r_kind, 300.0_r_kind, &
            300.0_r_kind, 270.0_r_kind, 270.0_r_kind, 250.0_r_kind, &
            240.0_r_kind, 240.0_r_kind, 250.0_r_kind, 250.0_r_kind, &
            270.0_r_kind, 280.0_r_kind, 290.0_r_kind, 300.0_r_kind, &
            320.0_r_kind, 320.0_r_kind, 300.0_r_kind, 300.0_r_kind, &
            300.0_r_kind, 300.0_r_kind, 300.0_r_kind, 300.0_r_kind /)
    

    ! Declare local variables
    character(30) :: Cline

    integer(i_kind) :: i, iscan, ifov, ichan, nchannels, wmosatid, version
    integer(i_kind) :: ios, max_scan, mintime
    integer(i_kind) :: nxaverage(nchanl), nyaverage(nchanl)
    integer(i_Kind) :: channelnumber(nchanl),qc_dist(nchanl)
    integer(i_kind), ALLOCATABLE ::  scanline_back(:,:)

    real(r_kind) :: sampling_dist, beamwidth(nchanl) 
    real(r_kind) :: newwidth(nchanl), cutoff(nchanl),err(nchanl)
    real(r_kind), allocatable, target :: bt_image(:,:,:)

    Error_Status=0

    IF (NChanl > MaxChans) THEN
       WRITE(0,*) 'Unexpected number of MWS channels: ',nchanl
       Error_Status = 1
       RETURN
    END IF

    ! Read the beamwidth requirements
    OPEN(lninfile,file='mws_beamwidth.txt',form='formatted',status='old', &
         iostat=ios)
    IF (ios /= 0) THEN
       WRITE(*,*) 'Unable to open mws_beamwidth.txt'
       Error_Status=1
       RETURN
    ENDIF
    wmosatid=999
    read(lninfile,'(a30)',iostat=ios) Cline
    DO WHILE (wmosatid /= mws1c_h_wmosatid .AND. ios == 0)
       DO WHILE (Cline(1:1) == '#')
          read(lninfile,'(a30)') Cline
       ENDDO
       READ(Cline,*) wmosatid
       
       read(lninfile,'(a30)') Cline
       DO WHILE (Cline(1:1) == '#')
          read(lninfile,'(a30)') Cline
       ENDDO
       READ(Cline,*) version
       
       read(lninfile,'(a30)') Cline
       DO WHILE (Cline(1:1) == '#')
          read(lninfile,'(a30)') Cline
       ENDDO
       READ(Cline,*) sampling_dist
       
       read(lninfile,'(a30)') Cline
       DO WHILE (Cline(1:1) == '#')
          read(lninfile,'(a30)') Cline
       ENDDO
       READ(Cline,*) nchannels
      
       read(lninfile,'(a30)') Cline
       if (nchannels > 0) then 
          DO ichan=1,nchannels
             read(lninfile,'(a30)') Cline
             DO WHILE (Cline(1:1) == '#')
                read(lninfile,'(a30)') Cline
             ENDDO
             READ(Cline,*) channelnumber(ichan),beamwidth(ichan), &
                  newwidth(ichan),cutoff(ichan),nxaverage(ichan), &
                  nyaverage(ichan), qc_dist(ichan)
          ENDDO
       end if
       read(lninfile,'(a30)',iostat=ios) Cline
    ENDDO
    IF (wmosatid /= mws1c_h_wmosatid) THEN
       WRITE(*,*) 'MWS_Spatial_Averaging: sat id not matched in mws_beamwidth.dat'
       Error_Status=1
       RETURN
    ENDIF
    CLOSE(lninfile)
  

    ! Determine scanline from time
    MinTime = MINVAL(Time)
    Scanline(:)   = NINT((Time(1:Num_Obs)-MinTime)/Scan_Interval)+1
    Max_Scan=MAXVAL(Scanline)
    
    ALLOCATE(BT_Image(Max_FOV,Max_Scan,nchanl))
    ALLOCATE(Scanline_Back(Max_FOV,Max_Scan))
    BT_Image(:,:,:) = 1000.0_r_kind
    
    ScanLine_Back(:,:) = -1
    DO I=1,Num_Obs
       bt_image(FOV(I),Scanline(I),:)=bt_inout(:,I)
       Scanline_Back(FOV(I),Scanline(I))=I
    END DO

!$omp parallel do schedule(dynamic,1) private(i,ichan,iscan,ios,ifov)
    DO IChan=1,nchanl
    
       err(ichan)=0
   
       ! Set all scan positions to missing in a scanline if one is missing
       do iscan=1,max_scan
          if (ANY(bt_image(:,iscan,ichan) > 500.0_r_kind)) &
             bt_image(:,iscan,ichan)=1000.0_r_kind
       enddo

       ! If the channel number is present in the channelnumber array we should process it 
       ! (otherwise bt_inout just keeps the same value):
       do i=1,nchannels
          if (channelnumber(i) == ichan) then
             CALL MODIFY_BEAMWIDTH_atms ( nxmax_mws,nymax_mws, max_fov, max_scan, bt_image(:,:,ichan), &
                  sampling_dist, beamwidth(i), newwidth(i), &
                  cutoff(i), nxaverage(i), nyaverage(i), &
                  qc_dist(i), MinBT(Ichan), MaxBT(IChan), IOS)

             IF (IOS == 0) THEN
                do iscan=1,max_scan
                   do ifov=1,max_fov
                      IF (Scanline_Back(IFov, IScan) > 0) &
                        bt_inout(ichan,Scanline_Back(IFov, IScan)) = &
                        BT_Image(ifov,iscan,ichan)
                   end do
                end do
             ELSE
                err(ichan)=1
             END IF
          end if
       end do
    END DO

    do ichan=1,nchanl
      if(err(ichan) >= 1)then
         error_status = 1
         return
      end if
    end do

    DEALLOCATE(BT_Image, Scanline_Back)
    
END Subroutine mws_spatial_average


END MODULE mws_spatial_average_mod
