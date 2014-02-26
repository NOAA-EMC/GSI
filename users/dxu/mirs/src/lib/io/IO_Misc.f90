!$Id: IO_Misc.f90 3301 2013-07-01 21:10:51Z kgarrett $
!-----------------------------------------------------------------------------------------------
! Name:         IO_Misc
! 
! Type:         F90 module
!
! Description:
!       This module is dedicated to the I/O of several different files.
!
! Modules needed:
!       - misc
!       - Consts
!       - utils
!
! Subroutines contained:
!       - readGDASanalys
!       - WritePressGridFile
!       - ReadPressGridFile
!       - Out4InterpolTest
!       - ReadHdrCldInfo
!       - ReadCldInfo
!       - Open_Text_File
!
! Data type included:
!       - none
!
! 
! History:
!       2006    S.A. Boukabara IMSG Inc. @ NOAA/NESDIS/ORA 
!
!-----------------------------------------------------------------------------------------------

MODULE IO_Misc
  USE Consts
  USE misc
  USE utils
  USE ErrorHandling
  IMPLICIT NONE
  PRIVATE
  !---Publicly available subroutine
  PUBLIC :: readGDASanalys,WritePressGridFile,ReadPressGridFile,readGFSforcst
  PUBLIC :: Out4InterpolTest,ReadHdrCldInfo,ReadCldInfo,ReadList,ReadList2,Open_Text_File
  !---INTRINSIC functions used in this module
  INTRINSIC :: TRIM,RANDOM_NUMBER,ADJUSTL,INDEX,LEN_TRIM

CONTAINS

!===============================================================
! Name:         ReadList
!
!
! Type:         Subroutine
!
!
! Description:  Reads the content of an ASCII list of filenames.
!               It also gives a corresponding list of file names 
!               whose prefix has been modified according to
!               an argument.
!
!
! Arguments:
!
!      Name                 Type          Description
!      ---------------------------------------------------
!       - iu                 O            Unit number
!       - ListName           I            Name of the list file
!       - rdrFiles           O            List of filenames contained in list
!       - nFiles             O            Number of files found in list
!       - tdrFiles           O            Files with same paths, names, except 
!                                         prefix was modified
!       - pathTDR            O            Path extracted from filenames
!       - pref               I            Prefix to replace the existing one 
!                                         (to be used for tdrFiles)
!
!
! Modules needed:
!       - None
!
!
! History:
!       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
!
!===============================================================

  subroutine ReadList(iu,ListName,rdrFiles,nFiles,tdrFiles,pathTDR,pref)
    !---Input/Output variables
    INTEGER                                 :: iu,nFiles
    CHARACTER(LEN=*)                        :: ListName,pathTDR,pref
    CHARACTER(LEN=*), DIMENSION(:), POINTER :: rdrFiles,tdrFiles
    !---Local variables
    CHARACTER(LEN=1000)                     :: xFile,path
    INTEGER                                 :: iFile,indx,npref,nSiz
    !---Open file and establish the number of files present in the list
    iu=Get_Lun()
    open(iu,file=trim(ListName),status='old',form='formatted')
    nFiles=0
    DO While (.true.)
       read(iu,'(a)',err=100,end=100) xfile
       indx=INDEX(xfile,'/',.true.)
       IF (xfile.eq.'') EXIT
       IF (indx .eq. 0) CALL ErrHandl(ErrorType,Err_AbsolPathNotPresnt,'(in ReadList)') 
       nFiles=nFiles+1
    ENDDO
100 CONTINUE
    close(iu)
    !---Allocate memory and read again the files names
    ALLOCATE(rdrFiles(nFiles),tdrFiles(nFiles))
    open(iu,file=trim(ListName),status='old',form='formatted')
    npref=len_trim(pref)
    DO ifile=1,nFiles
!       read(iu,'(a300)')  xfile
       read(iu,'(a)')  xfile
       rdrFiles(ifile) = trim(adjustl(xfile))
       indx=INDEX(xfile,'/',.true.)
       Path=xfile(1:indx)
       nSiz=len_trim(xfile)-indx-npref
       xfile(1:nSiz)=trim(adjustl(xfile(indx+1+npref:len_trim(xfile))))
       tdrFiles(ifile) = trim(adjustl(pathTDR))//trim(adjustl(pref))//trim(adjustl(xfile(1:nSiz)))
    ENDDO
    close(iu)
    return
  end subroutine ReadList
  
  !-- not replace, just add pref 
  subroutine ReadList2(iu,ListName,rdrFiles,nFiles,tdrFiles,pathTDR,pref,sufix,node_type)
    !---Input/Output variables
    INTEGER                                 :: iu,nFiles
    CHARACTER(LEN=1)                        :: node_type
    CHARACTER(LEN=*)                        :: ListName,pathTDR,pref,sufix
    CHARACTER(LEN=*), DIMENSION(:), POINTER :: rdrFiles,tdrFiles
    !---Local variables
    CHARACTER(LEN=512)                      :: xFile
    INTEGER                                 :: iFile,indx,npref
    !---Open file and establish the number of files present in the list
    iu=Get_Lun()
    open(iu,file=trim(ListName),status='old',form='formatted')
    nFiles=0
    DO While (.true.)
       read(iu,'(a)',err=100,end=100) xfile
       indx=INDEX(xfile,'/',.true.)
       IF (xfile.eq.'') EXIT
       IF (indx .eq. 0) CALL ErrHandl(ErrorType,Err_AbsolPathNotPresnt,'(in ReadList2)') 
       nFiles=nFiles+1
    ENDDO
100 CONTINUE
    close(iu)
    !---Allocate memory and read again the files names
    ALLOCATE(rdrFiles(nFiles),tdrFiles(nFiles))
    open(iu,file=trim(ListName),status='old',form='formatted')
    npref=len_trim(pref)
    DO ifile=1,nFiles
       read(iu,'(a)')  xfile
       rdrFiles(ifile) = trim(adjustl(xfile))
       indx=INDEX(xfile,'/',.true.)
       node_type = trim(adjustl(xfile(indx+10:indx+10)))
      tdrFiles(ifile) = trim(adjustl(pathTDR))//trim(adjustl(pref))// &
                        trim(adjustl(xfile(indx+1:len_trim(xfile)-4)))//trim(adjustl(sufix))
    ENDDO
    close(iu)
    return
  end subroutine ReadList2
  

!===============================================================
! Name:         ReadHdrCldInfo
!
!
! Type:         Subroutine
!
!
! Description:  Reads the header of the cloud Information file
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - FileInput          I              Name of the input file
!       - iu                 O              Unit number
!       - nPrf               O              Number of profiles
!       - nLay               O              Number of layers
!
!
! Modules needed:
!       - None
!
!
! History:
!       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
!
!===============================================================

  SUBROUTINE ReadHdrCldInfo(FileInput,iu,nPrf,nLay)
    CHARACTER(LEN=*) :: FileInput
    INTEGER          :: iu,nPrf,nLay
    iu=get_lun()
    OPEN(iu,file=trim(FileInput),form='unformatted',status='old')
    READ(iu) nPrf,nLay
    RETURN
  END SUBROUTINE ReadHdrCldInfo


!===============================================================
! Name:         ReadCldInfo
!
!
! Type:         Subroutine
!
!
! Description:  Reads the cloud info file content
!
!
! Arguments:
!
!           Name                    Type            Description
!      ---------------------------------------------------
!       - iu                         I             Unit number
!       - nLayCld                    I             # cloudy Layers
!       - pressLayCld                O             Layer-based pressure grid
!       - TempLayCld                 O             Layer-based temperature profile
!       - WvapLayCld                 O             Layer-based humidity profile
!       - Clw                        O             Layer-based cloud amount profile
!       - Rain                       O             Layer-basd rain profile
!       - Ice                        O             Layer-based ice profile
!       - Snow                       O             Layer-based snow profile
!       - Grpl                       O             Layer-based graupel profile
!       - layer_o3                   O             Layer-based ozone profile
!       - SfcPress                   O             Surface pressure
!       - ptop                       O             Top pressure 
!       - grdT                       O             Ground temperature (surface)
!       - TSeaSurf                   O             Sea surface temperature
!       - SfcType                    O             Surface type
!       - SfcWind_s                  O             surface wind speed
!
!
! Modules needed:
!       - random_number
!
!
! History:
!       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
!
!===============================================================

  SUBROUTINE ReadCldInfo(iu,nLayCld,pressLayCld,TempLayCld,&
       WvapLayCld,Clw,Rain,Ice,Snow,Grpl,layer_o3,SfcPress,&
       ptop,grdT,TSeaSurf,SfcType,SfcWind_s)
    !---Input/Output variables
    INTEGER              :: iu,nLayCld,SfcType
    REAL,   DIMENSION(:) :: pressLayCld,TempLayCld,WvapLayCld,Clw
    REAL,   DIMENSION(:) :: Rain,Ice,Snow,Grpl,layer_o3
    REAL                 :: SfcPress,ptop,grdT,TSeaSurf,SfcWind_s,x
    !---Local variables
    INTEGER              :: indxProf,indxI,indxJ,k,LayIndx
    READ(iu) indxProf,indxI,indxJ,grdT,TSeaSurf,SfcPress,ptop
    do k=1,nLayCld
       READ(iu) LayIndx,pressLayCld(k),TempLayCld(k),WvapLayCld(k),&
            CLW(k),Rain(k),Ice(k),Snow(k),Grpl(k)
       layer_o3(k)=0.
    enddo
    SfcType=OC_TYP  !Quick fix (later on should be coming from MM5 itself)
    call random_number(x)
    SfcWind_s = x *20.
    RETURN
  END SUBROUTINE ReadCldInfo

!===============================================================
! Name:         readGDASanalys
!
!
! Type:         Subroutine
!
!
! Description:  Reads GDAS analyses contents
!
!
! Arguments:
!
!      Name                 Type           Description
!      ---------------------------------------------------
!       - NWPFile            I             GDAS file name
!       - Arr                O             Array containing all parameters
!       - nlat               I             Number of latitude grids
!       - nlon               I             Number of longitude grids
!       - nparam             I             Number of parameters within Arr
!
!
! Modules needed:
!       - None
!
!
! History:
!       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
!
!===============================================================

  SUBROUTINE readGDASanalys(NWPFile,Arr,nlat,nlon,nparam)
    CHARACTER(LEN=*)                  :: NWPFile
    REAL,            DIMENSION(:,:,:) :: Arr
    INTEGER                           :: iu,nlat,nlon,nparam,ilat,iParam,irec
    REAL,            DIMENSION(nlon)  :: xdummy
    iu=get_lun()
    OPEN(iu,FILE=NWPFile,FORM='UNFORMATTED',ACCESS='DIRECT',RECL=nlon*4)
    irec=0
    DO iparam=1,nparam
       DO ilat=1,nlat
          irec=irec+1
          read(iu,rec=irec) xdummy
          Arr(nlat-ilat+1,:,iparam)=xdummy(:)
       ENDDO
    ENDDO
    CLOSE(iu)
    RETURN
  END SUBROUTINE readGDASanalys

!===============================================================
! Name:         readGFSforcst
!
!
! Type:         Subroutine
!
!
! Description:  Reads GFS forecast
!
!
! Arguments:
!
!      Name                 Type           Description
!      ---------------------------------------------------
!       - NWPFile            I             GDAS file name
!       - Arr                O             Array containing all parameters
!       - nlat               I             Number of latitude grids
!       - nlon               I             Number of longitude grids
!       - nparam             I             Number of parameters within Arr
!
!
! Modules needed:
!       - None
!
!
! History:
!       07-09-2009      Wanchun Chen      To read GFS forecast data
!
!===============================================================

  SUBROUTINE readGFSforcst(NWPFile,Arr,nlat,nlon,nparam)
    CHARACTER(LEN=*)                  :: NWPFile
    INTEGER                           :: iu,nlat,nlon,nparam,irec
    REAL, DIMENSION(nlat,nlon)        :: xdummy
    REAL, DIMENSION(:,:,:)            :: Arr
    iu=get_lun()
    OPEN(iu,FILE=NWPFile,FORM='UNFORMATTED',ACCESS='DIRECT',RECL=nlon*nlat*4)
    DO irec=1,nparam
        read(iu,rec=irec) xdummy
        Arr(:,:,irec) = xdummy(:,:)
    ENDDO
    CLOSE(iu)
    RETURN
  END SUBROUTINE readGFSforcst


!===============================================================
! Name:         WritePressGridFile
!
!
! Type:         Subroutine
!
!
! Description:  Writes out the pressure grid into a specific fmt
!
!
! Arguments:
!
!      Name                 Type          Description
!      ---------------------------------------------------
!       - FileOut            I            File name of the output
!       - nlev               I            Number of levels
!       - press_lev          I            Levels-based pressure grid
!
!
! Modules needed:
!       - None
!
!
! History:
!       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
!
!===============================================================

  SUBROUTINE WritePressGridFile(FileOut,nlev,press_lev)
    CHARACTER(LEN=*)               :: FileOut
    INTEGER                        :: iu,nlev,i
    REAL,             DIMENSION(:) :: press_lev
    iu=get_lun()
    OPEN(iu,file=trim(FileOut),form='formatted',status='new')
    write(iu,'(i8)') nlev
    do i=1,nlev
       write(iu,'(i8,4x,f8.3)') i,press_lev(i)
    enddo
    close(iu)
    RETURN
  END SUBROUTINE WritePressGridFile


!===============================================================
! Name:         ReadPressGridFile
!
!
! Type:         Subroutine
!
!
! Description:  Reads the pressure grid info from the pressure-grid-file
!
!
! Arguments:
!
!      Name                 Type           Description
!      ---------------------------------------------------
!       - FileIn             I             Name of the pressure-grid file
!       - nlev               O             Number of levels
!       - nlay               O             Number of layers
!       - press_lev          O             Level-based Pressure grid
!       - press_lay          O             Layer-based Pressure grid
!
!
! Modules needed:
!       - None
!
!
! History:
!       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
!
!===============================================================

  SUBROUTINE ReadPressGridFile(FileIn,nlev,nlay,press_lev,press_lay)
    CHARACTER(LEN=*)                       :: FileIn
    INTEGER                                :: iu,nlev,nlay,i,i0
    REAL,            DIMENSION(:), POINTER :: press_lev,press_lay
    iu=get_lun()
    OPEN(iu,file=trim(FileIn),form='formatted',status='old')
    !---Read Level information
    read(iu,'(i8)') nlev
    ALLOCATE(press_lev(nlev))
    do i=1,nlev
       read(iu,'(i8,4x,f8.3)') i0,press_lev(i)
    enddo
    !---Compute Layer Information
    nLay=nLev-1
    ALLOCATE(press_lay(nlay))
    do i=1,nLay
       press_lay(i)    = (press_lev(i)+press_lev(i+1))/2.
    enddo
    close(iu)
    RETURN
  END SUBROUTINE ReadPressGridFile


!===============================================================
! Name:         Out4InterpolTest
!
!
! Type:         Subroutine
!
!
! Description:  Outputs the original and interpolated profiles
!               (at two different pressure grids). This will 
!               the purpose of checking the validity of the 
!               interpolation process.
!
!
! Arguments:
!
!           Name                    Type            Description
!      ---------------------------------------------------
!       - iu                         I           Unit number
!       - nPrf                       I           Number of profiles
!       - iprof                      I           Profile processed in this call
!       - nLayCld                    I           Number of layers (before Interp)
!       - nLay                       I           Number of layers (after interpl)
!       - pressLayCld                I           Pressure grid (before interp)
!       - layer_p                    I           Pressure grid (after interp)
!       - TempLayCld                 I           Temperature before interp
!       - layer_t                    I           Temperature after interp
!       - WvapLayCld                 I           Humidity before interp
!       - layer_w                    I           Humidity after interp
!       - clw                        I           Cloud before interp
!       - layer_clw                  I           Cloud after interp
!       - rain                       I           Rain before interp
!       - layer_rain                 I           Rain amount after interp
!       - snow                       I           Snow before interp
!       - layer_snow                 I           Snow amount after interp
!       - ice                        I           Ice before interp
!       - layer_ice                  I           Ice after interp
!       - Grpl                       I           Graupel before interp
!       - layer_gh                   I           Graupel amount after interp
!
!
! Modules needed:
!       - None
!
!
! History:
!       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
!
!===============================================================

  SUBROUTINE Out4InterpolTest(iu,nPrf,iprof,nLayCld,nLay,pressLayCld,layer_p,&
       TempLayCld,layer_t,WvapLayCld,layer_w,clw,layer_clw,rain,    &
       layer_rain,snow,layer_snow,ice,layer_ice,Grpl,layer_gh)
    INTEGER               :: iu,nPrf,iprof,nLayCld,nLay
    REAL,    DIMENSION(:) :: pressLayCld,layer_p,TempLayCld,layer_t,&
         WvapLayCld,layer_w,clw,layer_clw,rain, &
         layer_rain,snow,layer_snow,ice,layer_ice,&
         Grpl,layer_gh
    IF (iprof.eq.1) write(iu) nPrf
    write(iu) iprof,nLayCld,nLay
    write(iu) pressLayCld(1:nLayCld)
    write(iu) layer_p(1:nLay)
    write(iu) TempLayCld(1:nLayCld)
    write(iu) layer_t(1:nLay)
    write(iu) WvapLayCld(1:nLayCld)
    write(iu) layer_w(1:nLay)
    write(iu) clw(1:nLayCld)
    write(iu) layer_clw(1:nLay)
    write(iu) rain(1:nLayCld)
    write(iu) layer_rain(1:nLay)
    write(iu) snow(1:nLayCld)
    write(iu) layer_snow(1:nLay)
    write(iu) ice(1:nLayCld)
    write(iu) layer_ice(1:nLay)
    write(iu) Grpl(1:nLayCld)
    write(iu) layer_gh(1:nLay)
    RETURN
  END SUBROUTINE Out4InterpolTest




!===============================================================
! Name:         Open_Text_File
!
!
! Type:         Function
!
!
! Description:  Reads the NOAA88 dataset.  Copied from CRTMp 
!               version of Binary_File_Utility.f90 to this
!               module, solely for the MIRS utility 
!               preProcessGeophData in mirs_utilities/src/covmtrx_gen     
!
!
! Arguments:
!
!           Name                    Type            Description
!      ---------------------------------------------------
!       - Filename                   I           Name of File to open
!       - FileID                     I           LUN of file
!
!=================================================================

  FUNCTION Open_Text_File( Filename,     &  ! Input
       FileID,       &  ! Output
       For_Output,   &  ! Optional input
       No_Check,     &  ! Optional input
       Message_Log ) &  ! Error messaging
       RESULT( Error_Status )
    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS -- #
    !#--------------------------------------------------------------------------#
    ! ---------
    ! Arguments
    ! ---------
    ! -- Input
    CHARACTER( * ),           INTENT( IN )  :: Filename
    ! -- Output
    INTEGER,                  INTENT( OUT ) :: FileID
    ! -- Optional input
    INTEGER,        OPTIONAL, INTENT( IN )  :: For_Output
    INTEGER,        OPTIONAL, INTENT( IN )  :: No_Check
    ! -- Error messaging
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log
    ! ---------------
    ! Function result
    ! ---------------
    INTEGER :: Error_Status
    ! ----------------
    ! Local parameters
    ! ----------------
    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Open_Text_File'
    ! ---------------
    ! Local variables
    ! ---------------
    CHARACTER( 256 ) :: Message
    LOGICAL :: File_Check
    LOGICAL :: File_Input
    INTEGER :: IO_Status
    CHARACTER( 7 ) :: File_Status
    CHARACTER( 5 ) :: File_Action
    !#--------------------------------------------------------------------------#
    !#                   -- SET SUCCESSFUL RETURN STATUS -- #
    !#--------------------------------------------------------------------------#
    Error_Status = 1
    ! ------------------------------
    ! Is file to be read or written?
    ! ------------------------------
    ! -- Default action is to READ file
    File_Input = .TRUE.
    ! -- ...unless the For_Output keyword is set
    IF ( PRESENT( For_Output ) ) THEN
      IF ( For_Output == 99 ) THEN
       File_Input = .FALSE.
        File_Check = .FALSE.
      END IF
    END IF
    !#--------------------------------------------------------------------------#
    !#                      -- CHECK DATA FILE EXISTENCE -- #
    !#--------------------------------------------------------------------------#
    IF ( File_Input ) THEN

      ! -- Set OPEN keywords for READING
      File_Status = 'OLD'
      File_Action = 'READ'
    ELSE

      ! -- Set OPEN keywords for WRITING
      File_Status = 'REPLACE'
      File_Action = 'WRITE'
    END IF
    !#--------------------------------------------------------------------------#
    !#                        -- OPEN THE DATA FILE -- #
    !#--------------------------------------------------------------------------#
    ! ----------------------
    ! Get a free unit number
    ! ----------------------
    FileID = Get_Lun()
    IF ( FileID < 0 ) THEN
      Error_Status = 1

      RETURN
    END IF
    ! -------------
    ! Open the file
    ! -------------
    OPEN( FileID, FILE   = TRIM( Filename ), &
                  STATUS = TRIM( File_Status ), &
                  ACTION = TRIM( File_Action ), &
                  ACCESS = 'SEQUENTIAL', &
                  FORM   = 'FORMATTED', &
                  IOSTAT = IO_Status )
    IF ( IO_Status /= 0 ) THEN
      Error_Status = 1
      WRITE( Message, '( "Error opening ", a, ". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status

      RETURN
    END IF
  END FUNCTION Open_Text_File



END MODULE IO_Misc
