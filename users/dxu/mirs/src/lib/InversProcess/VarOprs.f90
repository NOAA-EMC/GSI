!$Id: VarOprs.f90 1631 2008-11-06 19:07:36Z wchen $
!-----------------------------------------------------------------------------------------------
! Name:         VarOprs
! 
! Type:         F90 module
!
! Description:
!       This module is dedicated to performing the different operations
!       related to the variational retrieval.
!
! Modules needed:
!       - MathFcts
!
! Subroutines contained:
!       - Reset2ZeroJacob
!       - ComputeDeltaX
!       - ComputeGeoX
!       - CompContribFcts
!       - CompContribFcts_dbl
!
! Data type included:
!       - none
!
! 
! History:
!       2006     S.A. Boukabara IMSG Inc. @ NOAA/NESDIS/ORA 
!
!-----------------------------------------------------------------------------------------------

MODULE VarOprs
  USE MathFcts
  IMPLICIT NONE
  PRIVATE
  !---Publicly available subroutine
  PUBLIC :: ComputeDeltaX,SolEstimat,transfGeo2EOF,transfEOF2Geo
  PUBLIC :: Adjust4NonLinearity,ComputeGeoX,CompContribFcts
  PUBLIC :: CompAvgKern,CompCovSol,Reset2ZeroJacob,CombineCovMatr
  !---INTRINSIC functions used in this module
  INTRINSIC :: SIZE,MAXVAL,TRANSPOSE,MATMUL,SELECTED_REAL_KIND,DBLE,REAL

CONTAINS


!===============================================================
! Name:         Reset2ZeroJacob
!
!
! Type:         Subroutine
!
!
! Description:  Resets to zero the Jacobians, depending on how 
!               the parameters were selected to be treated.
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - K                  I/O            Jacobians
!       - nEDRselec          I              # EDRs selected
!       - EDR_cntrlRetr      I              EDRs control flags vector
!       - ParamIndx          I              EDRs index vector (within K)
!       - ParamLength        I              EDRs lengths vector (within K)
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

  SUBROUTINE Reset2ZeroJacob(K,nEDRselec,EDR_cntrlRetr,ParamIndx,ParamLength)
    REAL,    DIMENSION(:,:) :: K
    INTEGER, DIMENSION(:)   :: EDR_cntrlRetr,ParamIndx,ParamLength
    INTEGER                 :: nEDRselec
    !---Local variables
    INTEGER :: iEDR,iG,nG
    DO iEDR=1,nEDRselec
       iG=ParamIndx(iEDR)
       nG=ParamLength(iEDR)
       IF ((EDR_cntrlRetr(iEDR) .eq. 2 .or. EDR_cntrlRetr(iEDR) .eq. 3)) THEN
          K(:,iG:iG+nG-1) = 0.
       ENDIF
    ENDDO
    RETURN
  END SUBROUTINE Reset2ZeroJacob


!===============================================================
! Name:         ComputeDeltaX
!
!
! Type:         Subroutine
!
!
! Description: Computes the departure of the state vector from 
!              background vector.
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - Xg                I               State vector
!       - Xb                I               Background vector
!       - DX                O               Departure vector
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

  SUBROUTINE ComputeDeltaX(Xg,Xb,DX)
    REAL,   DIMENSION(:) :: Xg,Xb,DX
    DX = Xg - Xb
    RETURN
  END SUBROUTINE ComputeDeltaX


!===============================================================
! Name:         ComputeGeoX
!
!
! Type:         Subroutine
!
!
! Description:  Computes the geophysical state vector given the 
!               departure and background vectors.
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - DX                 I              Departure vector
!       - Xb                 I              Backgrounf vector                      
!       - Xg                 O              State vector
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

  SUBROUTINE ComputeGeoX(DX,Xb,Xg)
    REAL, DIMENSION(:)   :: DX,Xb,Xg
    Xg = Xb + DX
    RETURN
  END SUBROUTINE ComputeGeoX


!===============================================================
! Name:         CompContribFcts
!
!
! Type:         Subroutine
!
!
! Description:  Computes the contribution function matrix.
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - np                I              Number of parameters
!       - nc                I              Number of channels
!       - Sa                I              Covariance matrix (np,np)
!       - K                 I              Jacobians matrix (nc,np)
!       - Serr              I              Error matrix (nc,nc)
!       - G                 O              Contribution Fct Matrix (np,nc)
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

  SUBROUTINE CompContribFcts(np,nc,Sa,K,Serr,G)
    !---Input/Output variables
    INTEGER                :: np,nc
    REAL, DIMENSION(:,:)   :: Sa,K,Serr,G
    !---Local variables
    REAL, DIMENSION(nc,nc) :: Q
    
    Q(1:nc,1:nc) = MATMUL(K,MATMUL(Sa,TRANSPOSE(K)))
    Q(1:nc,1:nc) = Serr(1:nc,1:nc) + Q(1:nc,1:nc)
    Q(1:nc,1:nc) = MATINV_sgl(Q(1:nc,1:nc))
    G(1:np,1:nc) = MATMUL(Sa,MATMUL(TRANSPOSE(K),Q))
    RETURN
  END SUBROUTINE CompContribFcts
  

!===============================================================
! Name:         CompContribFcts_dbl
!
!
! Type:         Subroutine
!
!
! Description:  Same as CompContribFcts but in double precision.
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - np                I              Number of parameters
!       - nc                I              Number of channels
!       - Sa                I              Covariance matrix (np,np)
!       - K                 I              Jacobians matrix (nc,np)
!       - Serr              I              Error matrix (nc,nc)
!       - G                 O              Contribution Fct Matrix (np,nc)
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

  SUBROUTINE CompContribFcts_dbl(np,nc,Sa,K,Serr,G)
    !---Input/Output variables
    INTEGER                :: np,nc
    REAL, DIMENSION(:,:)   :: Sa,K,Serr,G
    !---Local variables
    REAL(SELECTED_REAL_KIND(15) ), DIMENSION(nc,nc) :: Q
    
    Q(1:nc,1:nc) = MATMUL(dble(K),MATMUL(dble(Sa),TRANSPOSE(dble(K))))
    Q(1:nc,1:nc) = dble(Serr(1:nc,1:nc)) + Q(1:nc,1:nc)
    Q(1:nc,1:nc) = MATINV_dbl(Q(1:nc,1:nc))
    G(1:np,1:nc) = REAL(MATMUL(dble(Sa),MATMUL(TRANSPOSE(dble(K)),Q)))
    RETURN
  END SUBROUTINE CompContribFcts_dbl
  
!===============================================================
! Name:          CombineCovMatr
!
!
! Type:         Function
!
!
! Description:  Performs combination of covariance matrices.
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - np                 I              Number of parameters
!       - Sa                 I              Covariance matrix
!       - S                  I              2nd covariance matrix
!       - CombineCovMatr     O              Resulting covar matrix
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

  FUNCTION CombineCovMatr(np,Sa,S)
    !---Input/Output variables
    INTEGER                :: np
    REAL, DIMENSION(:,:)   :: Sa,S
    REAL, DIMENSION(np,np) :: CombineCovMatr
    !---Local variables
    REAL(SELECTED_REAL_KIND(15) ), DIMENSION(np,np) :: Q,Q1,Q2
    Q1(1:np,1:np)             = MATINV_dbl(dble(Sa))
    Q2(1:np,1:np)             = MATINV_dbl(dble(S))
    Q(1:np,1:np)              = Q1+Q2
    CombineCovMatr(1:np,1:np) = REAL(MATINV_dbl(Q(1:np,1:np)))
    RETURN
  END FUNCTION CombineCovMatr
  

!===============================================================
! Name:         CompAvgKern
!
!
! Type:         Subroutine
!
!
! Description:  Computes average Kernel matrix.
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - np                 I              Number of parameters
!       - nc                 I              Number of channels
!       - G                  I              Contrib. Fct matrix 
!       - K                  I              Jacobian matrix
!       - A                  O              Aver. Kernel matrix
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

  SUBROUTINE CompAvgKern(np,nc,G,K,A)
    !---Input/Output variables
    INTEGER                 :: np,nc
    REAL,    DIMENSION(:,:) :: G,K,A
    A=MATMUL(G,K)
    RETURN
  END SUBROUTINE CompAvgKern

!===============================================================
! Name:         CompCovSol
!
!
! Type:         Subroutine
!
!
! Description:  Computes the resulting uncertainty matrix (Rodgers 1976).
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - np                 I              Number of parameters
!       - Sa                 I              Covariance matrix
!       - A                  I              Average Kernel matrix
!       - S                  O              resulting uncertainty matrix
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

  SUBROUTINE CompCovSol(np,Sa,A,S)
    !---Input/Output variables
    INTEGER                :: np
    REAL,   DIMENSION(:,:) :: Sa,A,S
    S=Sa-MATMUL(A,Sa)
    RETURN
  END SUBROUTINE CompCovSol


!===============================================================
! Name:         SolEstimat
!
!
! Type:         Subroutine
!
!
! Description:  Computes the optimal solution (Rodgers 1976).
!
!
! Arguments:
!
!         Name                     Type           Description
!      ---------------------------------------------------
!       - nchan                     I             Number of channels
!       - nch                       I             # channels effectively used
!       - nG                        I             # Geophysical parameters
!       - nR                        I             # params used in Retrieval
!       - DXtilda                   I             Bkgd-Departure vector projected in EOF space
!       - Ktilda                    I             Jacobians matrx projected in EOF space
!       - Ystar                     I             Simulated Radiances Effectively used
!       - YmStar                    I             Measured Radiances Effctively used
!       - SeStar                    I             Reduced error matrix (instrument)
!       - FeStar                    I             Reduced error matrix (model)
!       - Lambda                    I             EOF-reduced covariance matrix (Bkgd)
!       - DXtildaNew                O             Updated EOF-reduced Bkd-departure (solution)
!
!
! Modules needed:
!       - CompContribFcts_dbl
!
!
! History:
!       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
!
!===============================================================

  SUBROUTINE SolEstimat(nchan,nch,nG,nR,DXtilda,Ktilda,Ystar,YmStar,SeStar,&
       FeStar,Lambda,DXtildaNew)
    !---Input/Output variables
    REAL,    DIMENSION(:,:)     :: Ktilda,SeStar,FeStar,Lambda
    REAL,    DIMENSION(:)       :: DXtilda,Ystar,YmStar,DXtildaNew
    INTEGER                     :: nchan,nch,nG,nR
    !---Local variables
    REAL,    DIMENSION(nR,nch)  :: G
    REAL,    DIMENSION(nch)     :: B
    !call CompContribFcts(nR,nch,Lambda,Ktilda,SeStar+FeStar,G)
    call CompContribFcts_dbl(nR,nch,Lambda,Ktilda,SeStar+FeStar,G)
    B=(YmStar-Ystar+matmul(Ktilda,DXtilda))
    DXtildaNew = matmul(G,B)
    RETURN
  END SUBROUTINE SolEstimat

  
!===============================================================
! Name:         transfGeo2EOF
!
!
! Type:         Subroutine
!
!
! Description:  Projects the Jacobian and Bkgd-departure vector
!               into the EOF space.
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - Ustar              I              Transformation matrx
!       - Kstar              I              Jacobian for effective channels
!       - DX                 I              Bakgd-departure vector
!       - Ktilda             O              EOF-projected Jacobians
!       - DXtilda            O              EOF-projected bkgd-departure
!       - nG                 I              Number of geophysical params
!       - nR                 I              Number of retrieval items
!       - nch                I              Number of channels effectively used
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

  SUBROUTINE transfGeo2EOF(Ustar,Kstar,DX,Ktilda,DXtilda,nG,nR,nch)
    REAL, DIMENSION(:,:)  :: Ustar,Kstar,Ktilda
    REAL, DIMENSION(:)    :: DX,DXtilda
    INTEGER               :: nG,nR,nch
    Ktilda(1:nch,1:nR)  = matmul(Kstar(1:nch,1:nG),Ustar(1:nG,1:nR))
    DXtilda(1:nR)       = matmul(transpose(Ustar(1:nG,1:nR)),DX(1:nG))
    RETURN
  END SUBROUTINE transfGeo2EOF

              
!===============================================================
! Name:         transfEOF2Geo
!
!
! Type:         Subroutine
!
!
! Description:  Projects back from the EOF space into the 
!               geophysical space.
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - Ustar              I             Transformation matrix
!       - DXtildaNew         I             Vector of Bkgd-departure (EOF space)
!       - DX                 O             Geoph-space Bkgd-departure vector
!       - nG                 I             # Geophy params 
!       - nR                 I             # retrieval items (#EOFs)
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

  SUBROUTINE transfEOF2Geo(Ustar,DXtildaNew,DX,nG,nR)
    REAL,   DIMENSION(:,:) :: Ustar
    REAL,   DIMENSION(:)   :: DX,DXtildaNew
    INTEGER                :: nG,nR,i,j
    !DX(1:nG)=matmul(Ustar(1:nG,1:nR),DXtildaNew(1:nR))
    DO i=1,nG
       DX(i)=0.
       DO j=1,nR
          DX(i)=DX(i)+Ustar(i,j)*DXtildaNew(j)
       ENDDO
    ENDDO
    RETURN
  END SUBROUTINE transfEOF2Geo

!===============================================================
! Name:         Adjust4NonLinearity
!
!
! Type:         Subroutine
!
!
! Description:  Adjusts the instrument error covariance matrix
!               ad-hocly to accomodate non-linearities.
!
!
! Arguments:
!
!      Name                 Type           Description
!      ---------------------------------------------------
!       - Se                I/O            Error cov matrx
!       - dY2                I             Square of radiance residuals
!       - alpha              I             1st Tuning factor
!       - beta               I             2nd Tuning factor
!       - Kstar              I             Jacobians
!       - nch                I             # Channels effectively used
!       - nGselec            I             # Geoph params selected
!       - Sa                 I             Covaraince matrix
!       - iter               I             Iteration number
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

  SUBROUTINE Adjust4NonLinearity(Se,dY2,alpha,beta,Kstar,nch,nGselec,Sa,iter)
    !---Input/Output variables
    REAL, DIMENSION(:,:) :: Se,Sa,Kstar
    REAL, DIMENSION(:)   :: dY2
    REAL                 :: alpha,beta
    INTEGER              :: nch,nGselec,iter
    !---Local variables
    INTEGER              :: ichan
    REAL                 :: d
    
    ChanLoop: DO ichan=1,SIZE(DY2)
       d=dY2(ichan)
       Se(ichan,ichan)=maxval((/Se(ichan,ichan),(d)/(alpha+beta*iter)/))
    ENDDO ChanLoop
    RETURN
  END SUBROUTINE Adjust4NonLinearity




END MODULE VarOprs
