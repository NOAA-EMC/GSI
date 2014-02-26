MODULE IO_Misc_LE
USE misc
USE utils


IMPLICIT NONE
PRIVATE
!---Publicly available subroutine
PUBLIC :: readECMWFanalys

CONTAINS

!===========================================================================
!
! History:
!       03-27-2008      Flavio Iturbide-Sanchez, IMSG Inc @ NOAA/NESDIS/STAR
!
!===========================================================================

  SUBROUTINE readECMWFanalys(sfcECMWFile,atmECMWFile,ECMWFieldsR,nparam,iflag)
   CHARACTER(LEN=*)                  :: sfcECMWFile,atmECMWFile
   INTEGER                           :: iu,iflag,status
   INTEGER                           :: nparam,nREC
   INTEGER                           :: mxi,myi,nxi,nyi,nx,ny
   INTEGER                           :: nxp,nyp,nlev
   REAL                              :: fieldadd,CF
   REAL                              :: sfctype
   REAL                              :: HPO, HP1        
   REAL,  DIMENSION(92)              :: ECMWFA=&
        (/0.000000,2.000040,3.980832,7.387186,12.908319,21.413612,33.952858,51.746601, & 
        76.167656,108.715561,150.986023,204.637451,271.356506,352.824493,450.685791, &
        566.519226,701.813354,857.945801,1036.166504,1237.585449,1463.163940,1713.709595, & 
        1989.874390,2292.155518,2620.898438,2976.302246,3358.425781,3767.196045,4202.416504, & 
        4663.776367,5150.859863,5663.156250,6199.839355,6759.727051,7341.469727,7942.926270, & 
        8564.624023,9208.305664,9873.560547,10558.881836,11262.484375,11982.662109,12713.897461, & 
        13453.225586,14192.009766,14922.685547,15638.053711,16329.560547,16990.623047,17613.281250, & 
        18191.029297,18716.968750,19184.544922,19587.513672,19919.796875,20175.394531,20348.916016, & 
        20434.158203,20426.218750,20319.011719,20107.031250,19785.357422,19348.775391,18798.822266, & 
        18141.296875,17385.595703,16544.585938,15633.566406,14665.645508,13653.219727,12608.383789, & 
        11543.166992,10471.310547,9405.222656,8356.252930,7335.164551,6353.920898,5422.802734, & 
        4550.215820,3743.464355,3010.146973,2356.202637,1784.854614,1297.656128,895.193542, & 
        576.314148,336.772369,162.043427,54.208336,6.575628,0.003160,0.000000/)
   REAL,  DIMENSION(92)              :: ECMWFB=&
        (/0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,0.000000, &
         0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,0.000000, & 
         0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,0.000000, &
         0.000000,0.000000,0.000000,0.000000,0.000000,0.000014,0.000055,0.000131,0.000279,0.000548, &
         0.001000,0.001701,0.002765,0.004267,0.006322,0.009035,0.012508,0.016860,0.022189,0.028610, &
         0.036227,0.045146,0.055474,0.067316,0.080777,0.095964,0.112979,0.131935,0.152934,0.176091, &
         0.201520,0.229315,0.259554,0.291993,0.326329,0.362203,0.399205,0.436906,0.475016,0.513280, &
         0.551458,0.589317,0.626559,0.662934,0.698224,0.732224,0.764679,0.795385,0.824185,0.850950, &
         0.875518,0.897767,0.917651,0.935157,0.950274,0.963007,0.973466,0.982238,0.989153,0.994204, &
         0.997630,1.000000/)
   REAL,  DIMENSION(1440,721)            :: Pressure     
   REAL,  DIMENSION(1440,721)            :: ECMWFields   
   REAL,  DIMENSION(:,:,:)               :: ECMWFieldsR  
  

!=============================================================
!Reading the surface parameters  
!=============================================================

 IF (iflag==1) THEN
    iu=get_lun()
    !Opening the file containing the surface parameters
    OPEN (UNIT=iu, FILE=sfcECMWFile, ACCESS= 'DIRECT', STATUS='OLD',& 
           ACTION='READ', FORM='UNFORMATTED', RECL=1440*721*4, IOSTAT=status)
    DO nREC=1,nparam
       READ(iu, IOSTAT=status, REC=nREC ) ECMWFields(:,:) 

       IF (nREC .eq. 2) THEN 
          CF=0.01
       ELSE  
          CF=1.0
       ENDIF

       !Regriding surface parameters from 1440x721 to 360x181 points
       DO myi=0,(720/4)-1
          DO mxi=0,(1440/4)-1  
             fieldadd=0
             DO nyi= 1,4
                DO nxi= 1,4         
                   fieldadd=ECMWFields(nxi+4*mxi,nyi+4*myi) + fieldadd
                ENDDO
             ENDDO
             ECMWFieldsR(myi+1,mxi+1,nREC)= CF*fieldadd/16                     
          ENDDO
       ENDDO

     
       DO mxi=0,(1440/4)-1   
          fieldadd=0
          DO nxi= 1,4
             fieldadd=ECMWFields(nxi+4*mxi,721) + fieldadd
          ENDDO
          ECMWFieldsR(181,mxi+1,nREC)= CF*fieldadd/4 
       ENDDO
    
    ENDDO
    CLOSE(iu) !Closing the file containing the surface parameters


!--Defining the surface type
    DO ny=1,181
       DO nx=1,360
          sfctype=ECMWFieldsR(ny,nx,1)
          IF ( sfctype >= 0.0 .AND. sfctype <= 0.05 ) THEN
             ECMWFieldsR(ny,nx,1)= 0.0
          ELSE IF (sfctype >= 0.95 .AND. sfctype <= 1.0 ) THEN
             ECMWFieldsR(ny,nx,1)= 2.0
          ELSE 
             ECMWFieldsR(ny,nx,1)= 6.0
          ENDIF
       ENDDO
    ENDDO
     
!=============================================================
!Reading the atmospheric parameters  
!=============================================================
 
 ELSE IF (iflag==2) THEN

    iu=get_lun()
    !Openin the file containing the atmospheric parameters
    OPEN (UNIT=iu, FILE=atmECMWFile, ACCESS= 'DIRECT', STATUS='OLD',& 
         ACTION='READ', FORM='UNFORMATTED', RECL=1440*721*4, IOSTAT=status)

    DO nREC=1,nparam-91
       READ(iu, IOSTAT=status, REC=nREC ) ECMWFields(:,:)
       IF (nREC <= 92) THEN 
          CF=1
       ELSE 
          CF=1000
       ENDIF


       !Regriding atmosheric parameters from 1440x721 to 360x181 points
       DO myi=0,(720/4)-1
          DO mxi=0,(1440/4)-1  
             fieldadd=0
             DO nyi= 1,4
                DO nxi= 1,4         
                   fieldadd=ECMWFields(nxi+4*mxi,nyi+4*myi) + fieldadd
                ENDDO
             ENDDO
             ECMWFieldsR(myi+1,mxi+1,nREC)= CF*fieldadd/16           
          ENDDO
       ENDDO

       DO mxi=0,(1440/4)-1   
          fieldadd=0
          DO nxi= 1,4
             fieldadd=ECMWFields(nxi+4*mxi,721) + fieldadd
          ENDDO
          ECMWFieldsR(181,mxi+1,nREC)= CF*fieldadd/4
       ENDDO
    ENDDO
    CLOSE(iu) !Closing the file containing the atmospheric parameters

    !--Defining the surface type
    DO ny=1,181
       DO nx=1,360
          sfctype=ECMWFieldsR(ny,nx,1)
          IF ( sfctype >= 0.0 .AND. sfctype <= 0.05 ) THEN
             ECMWFieldsR(ny,nx,1)= 0.0
          ELSE IF (sfctype >= 0.95 .AND. sfctype <= 1.0 ) THEN
             ECMWFieldsR(ny,nx,1)= 2.0
          ELSE 
             ECMWFieldsR(ny,nx,1)= 6.0
          ENDIF
       ENDDO
    ENDDO
 
!--------------------------------------------------------------------------------
!  Calculating the Pressure Field using the surface pressure and the ECMWF model 
!  level definition
!--------------------------------------------------------------------------------
    iu=get_lun()
    !Opening the file with the surface parameters
    OPEN (UNIT=iu, FILE=sfcECMWFile, ACCESS= 'DIRECT', STATUS='OLD',& 
         ACTION='READ', FORM='UNFORMATTED', RECL=1440*721*4, IOSTAT=status)
    !The second grid layer of the sfcECMWF file  must contain the surface pressure
    READ(iu, IOSTAT=status, REC=2 ) ECMWFields(:,:)
    CLOSE(iu) !Closing the file containing the surface parameters

    !Calculation of the Pressure Field in mbars
    DO nlev=1, 91   
       DO nyp=1, 721
          DO nxp=1, 1440
             HPO=ECMWFA(nlev) + ECMWFB(nlev)*ECMWFields(nxp,nyp)
             HP1=ECMWFA(nlev+1) + ECMWFB(nlev+1)*ECMWFields(nxp,nyp)
             Pressure(nxp,nyp)= 0.01 * ( HPO + HP1) * 0.5
          ENDDO
       ENDDO
       !Regrid the Pressure Field from 1440x721 to 360x181 points
       DO myi=0,(720/4)-1
          DO mxi=0,(1440/4)-1  
             fieldadd=0
             DO nyi= 1,4
                DO nxi= 1,4         
                   fieldadd=Pressure(nxi+4*mxi,nyi+4*myi) + fieldadd
                ENDDO
             ENDDO
             ECMWFieldsR(myi+1,mxi+1,nlev+456)= fieldadd/16       
          ENDDO
       ENDDO
       DO mxi=0,(1440/4)-1   
          fieldadd=0
          DO nxi= 1,4
             fieldadd=Pressure(nxi+4*mxi,721) + fieldadd
          ENDDO
          ECMWFieldsR(181,mxi+1,nlev+456)= fieldadd/4
       ENDDO
    ENDDO

!--------------------------------------------------------------------------------------
!  Converting Specific Humidity to Mixing Ratio in g/kg
!--------------------------------------------------------------------------------------      
!    DO nlev=1,91      
!       DO ny=1,181
!          DO nx=1,360
!             ECMWFieldsR(ny,nx,nlev+92) = ECMWFieldsR(ny,nx,nlev+92)/( 1 - 0.001*ECMWFieldsR(ny,nx,nlev+92) )
!          ENDDO
!       ENDDO
!    ENDDO
       
         
 ELSE
    print*,'Neither surface nor atmospheric field parameter has been read'

 ENDIF



END SUBROUTINE readECMWFanalys



END MODULE IO_Misc_LE
