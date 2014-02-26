!--------------------------------------------------------------------------------
!M+
! NAME:
!     CLOUD_OPT_CAL 
!
! PURPOSE:
!       Module containing routines for CALCULATIONS OF CLOUD OPTICAL PARAMETERS 
!  BASED UPON THE LOOKUP TABLE PRODUCED BY MIE CODE
!
!
! CALLING SEQUENCE:
!       USE CLOUD_OPT_CAL
!
! MODULES:
!
! CONTAINS:
!
!    LOAD_CLOUD_OPTICS:    LOAD OPTICAL PROPERTIES 
!
!    CLOUD_OPTOUT :    GET THREE OPTICAL PARAMETERS OF CLOUDS (TAU, ALBEDO, GG)
!
! EXTERNALS:
!       None.
!
! INCLUDE FILES:
!       None.
!
! COMMON BLOCKS:
!       None.
!
! CREATION HISTORY:
!       Written by:     BANGHUA YAN, NOAA/NESDIS, JCSDA, Nov., 2006
!
! version 1: de : 0.05 ~ 2.0 mm
!
! version 2: de: 0.001,0.01,0.02,0.03,0.04,0.05 ~ 2.0 mm
!M-
!--------------------------------------------------------------------------------

MODULE CLOUD_OPT_CAL

! ---------------------------
! Disable all implicit typing
! ---------------------------

  IMPLICIT NONE

! ------------------
! Default visibility
! ------------------

  PRIVATE
  PUBLIC :: LOAD_CLOUD_OPTICS
  PUBLIC :: CLOUD_OPTOUT
  INTEGER(2), PARAMETER, PUBLIC  :: nch = 6, mtc = 46, mrwp = 51,mde = 45, npara = 3
  INTEGER(2), PARAMETER          :: SUCCESS = 0, FAILURE = 1
  REAL(4),SAVE,ALLOCATABLE :: CLOUDOPT(:,:,:,:,:)

  CONTAINS

  SUBROUTINE LOAD_CLOUD_OPTICS(CLOUDOPT_FILENAME,error_status)

   CHARACTER( * ), INTENT( IN ) :: CLOUDOPT_FILENAME
   INTEGER(2) :: error_status
   ! local
   INTEGER(2) :: ich,iu,irec,fid
   REAL(4), ALLOCATABLE, DIMENSION(:,:,:,:) :: VX

! INITIALIZATION
   iu = 10
   irec = 1
   error_status = SUCCESS
   ALLOCATE(VX(mtc,mrwp,mde,npara),STAT = error_status )
   ALLOCATE(CLOUDOPT(mtc,mrwp,mde,npara,nch),STAT = error_status )

   IF(error_status /= SUCCESS) THEN
      print *,'LOAD_CLOUD_OPTICS: cannot allocate memory'
      error_status = FAILURE
      RETURN
    ENDIF

! OPEN/LOAD

   fid = Get_Lun()
   OPEN(fid, file = CLOUDOPT_FILENAME, form='unformatted',  &
             recl = 316710*4, access='direct',status = 'old')
   
   DO ich = 1, nch
      read(iu,rec = irec) VX
      CLOUDOPT(1:mtc,1:mrwp,1:mde,1:npara,ich) = VX(1:mtc,1:mrwp,1:mde,1:npara)
      irec = irec + 1
   ENDDO
!!   print*,'OKcloud11',CLOUDOPT(33,2,6,1,1)
 !!  print*,'OKcloud12',CLOUDOPT(33,3,6,1,1)
 !!  print*,'OKcloud13',CLOUDOPT(33,2,6,1,2)
 !!  print*,'OKcloud14',CLOUDOPT(33,2,6,1,3)
 !!  print*,'OKcloud15',CLOUDOPT(33,2,6,1,4)
 !!  print*,'OKcloud16',CLOUDOPT(33,2,6,1,5)
 !!  print*,'OKcloud17',CLOUDOPT(33,2,6,1,6)
 !!  print*,'OKcloud21',CLOUDOPT(33,2,6,2,1)
 !!  print*,'OKcloud22',CLOUDOPT(33,3,6,2,1)
 !!  print*,'OKcloud23',CLOUDOPT(33,2,6,2,2)
 !!  print*,'OKcloud24',CLOUDOPT(33,2,6,2,3)
 !!  print*,'OKcloud25',CLOUDOPT(33,2,6,2,4)
 !!  print*,'OKcloud26',CLOUDOPT(33,2,6,2,5)
 !!  print*,'OKcloud27',CLOUDOPT(33,2,6,2,6)
 !!  print*,'OKcloud31',CLOUDOPT(33,2,6,3,1)
 !!  print*,'OKcloud32',CLOUDOPT(33,3,6,3,1)
 !!  print*,'OKcloud33',CLOUDOPT(33,2,6,3,2)
 !!  print*,'OKcloud34',CLOUDOPT(33,2,6,3,3)
 !!  print*,'OKcloud35',CLOUDOPT(33,2,6,3,4)
 !!  print*,'OKcloud36',CLOUDOPT(33,2,6,3,5)
 !!  print*,'OKcloud37',CLOUDOPT(33,2,6,3,6)

   DEALLOCATE(VX)

   CLOSE(fid)
   
  END SUBROUTINE LOAD_CLOUD_OPTICS

!
 
 SUBROUTINE CLOUD_OPTOUT(FREQUENCY,TC,IWP,DE,TAU,ALBEDO,GG)

  REAL(4), PARAMETER :: TC_MIN = 180.0, TC_MAX = 270.0
  REAL(4), PARAMETER :: IWP_MIN = 0.001, IWP_MAX = 2.0
  REAL(4), PARAMETER :: DE_MIN0 = 0.001,DE_MIN = 0.05, DE_MAX = 2.0
  REAL(4), PARAMETER :: DTC = 2.0, DIWP = 0.04, DDE = 0.05, DDE0 = 0.01
  INTEGER(2)  :: ic,id,iw,ich,ip
  INTEGER(2)  :: ich_index,ic_index,ide_index,iw_index
  REAL(4)     :: FREQUENCY,TC,IWP,DE,TAU,ALBEDO,GG
  REAL(4), DIMENSION(nch) :: freq
  REAL(4), DIMENSION(npara) :: you
  data freq/23.8,31.4,50.3,89.0,150.0,183./

! LOCATE
  ich_index = 1
  DO ich = 1, nch
     if (abs(FREQUENCY-freq(ich)) <= 1.0 ) ich_index = ich
  ENDDO

  ic_index =  INT((TC-TC_MIN)/DTC) + 1
  if (TC <= TC_MIN) ic_index = 1
  if (TC >= TC_MAX) ic_index = mtc

  iw_index =  INT(IWP/DIWP) + 1
  if (IWP <= IWP_MIN) iw_index = 1
  if (IWP >= IWP_MAX) iw_index = mrwp
  
! DE: 0.001,0.01,0.02,0.03,0.04,0.05~2.0 BY AN INCREASEMENT OF 0.05

  IF (DE < DE_MIN) THEN
      IF (DE <= DE_MIN0) THEN
          ide_index = 1
      ELSE
          ide_index = INT(DE/DDE0) + 1  
      ENDIF
  ELSE
    ide_index = INT(DE/DDE) + 5 
  ENDIF
  if (DE >= DE_MAX ) ide_index = mde

   DO ip = 1, npara

      CALL TWOD_INTERPOLATION(TC,IWP,DE,TC_MIN,IWP_MIN,DE_MIN,DTC,DIWP,DDE,DDE0,DE_MIN0,  &
                              ic_index,ide_index,iw_index,ip,ich_index,      &
                              you(ip))
   ENDDO
   TAU    = you(3)
   ALBEDO = you(1)
   GG     = you(2)
!   print*,'INTER:',TAU,ALBEDO,GG
 
  END SUBROUTINE CLOUD_OPTOUT

  SUBROUTINE TWOD_INTERPOLATION(TC,IWP,DE,TC_MIN,IWP_MIN,DE_MIN,DTC,DIWP,DDE,DDE0,DE_MIN0,  &
                                ic_index,ide_index,iw_index,npara_in,nch_in,   &
                                you)

  REAL(4), PARAMETER :: min_distance = 0.001
  INTEGER(2) :: ic_index,ide_index,iw_index,npara_in,nch_in
  INTEGER(2) :: ic,ntime
  REAL*4 :: TC,IWP,DE,TC_MIN,IWP_MIN,DE_MIN,DTC,DIWP,DDE0,DDE,DE_MIN0
  REAL*4 :: x1,x2,y1,y2,ylow,yhi,you
  REAL*4, ALLOCATABLE, DIMENSION(:) :: Copt_tc

! DETERMINE VARIABLE NUMBER OF TEMPERATURE DEPENDENCY
  
   ntime  = 1
   if (ic_index+1 <= mtc) ntime = 2
   ALLOCATE(Copt_tc(ntime))

   do ic = 1, ntime

! x-direction (iwp)

         if (iw_index <=2) then 
             x1 = IWP_MIN
             x2 = iw_index*DIWP
         else
             x1 = (iw_index-1) * DIWP
             x2 = iw_index* DIWP
         endif
         y1 = CLOUDOPT(ic_index+(ic-1),iw_index,ide_index,npara_in,nch_in)
         y2 = y1
         if (iw_index+1 <= mrwp ) &
         y2 = CLOUDOPT(ic_index+(ic-1),iw_index+1,ide_index,npara_in,nch_in)

         if (abs(x2-x1) <=min_distance) then
             ylow = y1
         else
             ylow = y1 + (y2-y1)*(IWP-x1)/(x2-x1)
         endif
        
         yhi = ylow
         if (ide_index+1 <= mde) then

            y1 = CLOUDOPT(ic_index+(ic-1),iw_index,ide_index+1,npara_in,nch_in)
            y2 = y1
            if (iw_index+1 <= mrwp ) &
            y2 = CLOUDOPT(ic_index+(ic-1),iw_index+1,ide_index+1,npara_in,nch_in)

            if (abs(x2-x1) <=min_distance) then
                yhi = y1
            else
                yhi = y1 + (y2-y1)*(IWP-x1)/(x2-x1)
            endif
         endif
! y-direction
         if (ide_index <= 5 ) then
             if (ide_index == 1) then
                 x1 = DE_MIN0
                 x2 = 0.01
             else
                 x1 = (ide_index-1)*DDE0
                 x2 = ide_index*DDE0
             endif
         else
            x1 = (ide_index-5)*DDE   
            x2 = x1
            if (ide_index+1 <= mde) x2 = (ide_index+1.0-5.0)*DDE
         endif    
         y1 = ylow
         y2 = yhi
         if (abs(x2-x1) <=min_distance) then
             Copt_tc(ic) = y1
         else

             Copt_tc(ic)  =  y1 + (y2-y1)*(DE-x1)/(x2-x1)
         endif
   enddo  

! interpolation in temperature direction

     x1 = TC_MIN + (ic_index-1.)*DTC
     x2 = x1
     if (ic_index+1 <=  mtc) x2 = TC_MIN + ic_index*DTC
     y1 = Copt_tc(1)
     y2 = Copt_tc(2)
     if (abs(x2-x1) <=min_distance) then
         you = y1
     else
         you = y1 + (y2-y1)*(TC-x1)/(x2-x1)
     endif

 !    print *,'you=',you,y1,y2
RETURN


  END SUBROUTINE TWOD_INTERPOLATION

!------------------------------------------------------------------------------
!S+
! NAME:
!       Get_Lun
!
! PURPOSE:
!       Function to obtain a free logical unit number for file access
!
! CALLING SEQUENCE:
!       Lun = Get_Lun()
!
! FUNCTION RESULT:
!       Lun:          Logical unit number that may be used for file access.
!                     If Lun > 0 it can be used as a logical unit number to open
!                                and access a file.
!                        Lun < 0 a non-existant logical unit number was reached
!                                during the search.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!!S-
!------------------------------------------------------------------------------
  FUNCTION Get_Lun() RESULT( Lun )

    ! -----------------
    ! Type declarations
    ! -----------------

    INTEGER(2) :: Lun

    Logical(2) :: Existence, Is_Open


    ! ------------------------------
    ! Initialise logical unit number
    ! ------------------------------

    Lun = 9


    ! ------------------------------
    ! Start open loop for Lun Search
    ! ------------------------------

    Lun_Search: DO

      ! -- Increment logical unit number
      Lun = Lun + 1

      ! -- If file unit does not exist, set to -1 and exit

      INQUIRE( UNIT = Lun, EXIST = Existence )
      IF ( .NOT. Existence ) THEN
        Lun = -1
        EXIT Lun_Search
      END IF
      ! -- If the file is not open, we're done.

      INQUIRE( UNIT = Lun, OPENED = Is_Open )
      IF ( .NOT. Is_Open ) EXIT Lun_Search

    END DO Lun_Search

  END FUNCTION Get_Lun


END MODULE CLOUD_OPT_CAL
