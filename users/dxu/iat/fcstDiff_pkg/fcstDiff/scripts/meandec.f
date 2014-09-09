        dimension tssti(192,94)
        dimension ssti(192,94)
        dimension sst(144,73)
c
        call assign('assign -Nieee -Ff77 u:11')
        open(11,file='/ptmp2/wd23sm/runkan2/prate.9712',
     *  form='unformatted')
        call assign('assign -Nieee -Ff77 u:51')
        open(51,file='/ptmp2/wd23sm/runkan2/prate.9712.ll',
     *  form='unformatted')
c
        nt=0
        do ihr=1056,1812,12
        read(11) ssti
        nt=nt+1
        print *,nt,ihr
        do j=1,94
        do i=1,192
        tssti(i,j)=tssti(i,j)+ssti(i,j)
        enddo
        enddo
        enddo
c
        fnt=1.0/float(nt)
        do j=1,94
        do i=1,192
        tssti(i,j)=tssti(i,j)*fnt
        enddo
        enddo
c
        call gau2ll(tssti,192,94,sst,144,73)
        write(51) sst
c
        stop
        end
      SUBROUTINE GAULAT(GAUL,K)                                         
C                                                                       
      DIMENSION A(500)                                                  
      DIMENSION GAUL(1)                                                 
C                                                                       
      ESP=1.E-14                                                        
      C=(1.E0-(2.E0/3.14159265358979E0)**2)*0.25E0                      
      FK=K                                                              
      KK=K/2                                                            
      CALL BSSLZ1(A,KK)                                                 
      DO 30 IS=1,KK                                                     
      XZ=COS(A(IS)/SQRT((FK+0.5E0)**2+C))                               
      ITER=0                                                            
   10 PKM2=1.E0                                                         
      PKM1=XZ                                                           
      ITER=ITER+1                                                       
      IF(ITER.GT.10) GO TO 70                                           
      DO 20 N=2,K                                                       
      FN=N                                                              
      PK=((2.E0*FN-1.E0)*XZ*PKM1-(FN-1.E0)*PKM2)/FN                     
      PKM2=PKM1                                                         
   20 PKM1=PK                                                           
      PKM1=PKM2                                                         
      PKMRK=(FK*(PKM1-XZ*PK))/(1.E0-XZ**2)                              
      SP=PK/PKMRK                                                       
      XZ=XZ-SP                                                          
      AVSP=ABS(SP)                                                      
      IF(AVSP.GT.ESP) GO TO 10                                          
      A(IS)=XZ                                                          
   30 CONTINUE                                                          
      IF(K.EQ.KK*2) GO TO 50                                            
      A(KK+1)=0.E0                                                      
      PK=2.E0/FK**2                                                     
      DO 40 N=2,K,2                                                     
      FN=N                                                              
   40 PK=PK*FN**2/(FN-1.E0)**2                                          
   50 CONTINUE                                                          
      DO 60 N=1,KK                                                      
      L=K+1-N                                                           
      A(L)=-A(N)                                                        
   60 CONTINUE                                                          
C                                                                       
      RADI=180./(4.*ATAN(1.))                                           
      DO 211 N=1,K                                                      
      GAUL(N)=90.-ACOS(A(N))*RADI                                           
  211 CONTINUE                                                          
C
C     PRINT *,'GAUSSIAN LAT (DEG) FOR JMAX=',K 
C     PRINT *,(GAUL(N),N=1,K)                                           
C                                                                       
      RETURN                                                            
   70 WRITE(6,6000)                                                     
 6000 FORMAT(//5X,14HERROR IN GAUAW//)                                  
      STOP                                                              
      END                                                               
      SUBROUTINE BSSLZ1(BES,N)                                          
C                                                                       
      DIMENSION BES(N)                                                  
      DIMENSION BZ(50)                                                  
C                                                                       
      DATA PI/3.14159265358979E0/                                       
      DATA BZ         / 2.4048255577E0, 5.5200781103E0,                 
     $  8.6537279129E0,11.7915344391E0,14.9309177086E0,18.0710639679E0, 
     $ 21.2116366299E0,24.3524715308E0,27.4934791320E0,30.6346064684E0, 
     $ 33.7758202136E0,36.9170983537E0,40.0584257646E0,43.1997917132E0, 
     $ 46.3411883717E0,49.4826098974E0,52.6240518411E0,55.7655107550E0, 
     $ 58.9069839261E0,62.0484691902E0,65.1899648002E0,68.3314693299E0, 
     $ 71.4729816036E0,74.6145006437E0,77.7560256304E0,80.8975558711E0, 
     $ 84.0390907769E0,87.1806298436E0,90.3221726372E0,93.4637187819E0, 
     $ 96.6052679510E0,99.7468198587E0,102.888374254E0,106.029930916E0, 
     $ 109.171489649E0,112.313050280E0,115.454612653E0,118.596176630E0, 
     $ 121.737742088E0,124.879308913E0,128.020877005E0,131.162446275E0, 
     $ 134.304016638E0,137.445588020E0,140.587160352E0,143.728733573E0, 
     $ 146.870307625E0,150.011882457E0,153.153458019E0,156.295034268E0/ 
      NN=N                                                              
      IF(N.LE.50) GO TO 12                                              
      BES(50)=BZ(50)                                                    
      DO 5 J=51,N                                                       
    5 BES(J)=BES(J-1)+PI                                                
      NN=49                                                             
   12 DO 15 J=1,NN                                                      
   15 BES(J)=BZ(J)                                                      
      RETURN                                                            
      END                                                               
      SUBROUTINE GAU2LL(GAUIN,IMXIN,JMXIN,REGOUT,IMXOUT,JMXOUT)
C
      SAVE
C                                                                       
C  INTERPOLATION FROM LAT/LON GRID TO OTHER LAT/LON GRID                
C                                                                       
      DIMENSION GAUIN (IMXIN,JMXIN)                                   
C                                                                       
      DIMENSION REGOUT(IMXOUT,JMXOUT)                                     
      DIMENSION GAUL(500),REGL(500)                                     
      DIMENSION IINDX1(1000)                                            
      DIMENSION IINDX2(1000)                                            
      DIMENSION JINDX1(500)                                             
      DIMENSION JINDX2(500)                                             
      DIMENSION DDX(1000)                                               
      DIMENSION DDY(500)                                                
C                                                                       
      DATA IFP/0/                                                       
C                                                                       
      IF(IFP.NE.0) GO TO 111                                            
      IFP=1                                                             
C                                                                       
      CALL GAULAT(GAUL,JMXIN)                                           
C                                                                       
      DPHI=180./FLOAT(JMXOUT-1)                                         
      DO 20 J=1,JMXOUT                                                  
      REGL(J)=90.-FLOAT(J-1)*DPHI                                       
   20 CONTINUE                                                          
C                                                                       
      DXIN =360./FLOAT(IMXIN )                                          
      DXOUT=360./FLOAT(IMXOUT)                                          
C                                                                       
      DO 30 I=1,IMXOUT                                                  
      ALAMD=FLOAT(I-1)*DXOUT                                            
      I1=ALAMD/DXIN+1.001                                               
      IINDX1(I)=I1                                                      
      I2=I1+1                                                           
      IF(I2.GT.IMXIN) I2=1                                              
      IINDX2(I)=I2                                                      
      DDX(I)=(ALAMD-FLOAT(I1-1)*DXIN)/DXIN                              
   30 CONTINUE                                                          
C                                                                       
      J2=1                                                              
      DO 40 J=1,JMXOUT                                                  
      APHI=REGL(J)                                                      
      DO 50 JJ=1,JMXIN                                                  
      IF(APHI.LT.GAUL(JJ)) GO TO 50                                     
      J2=JJ                                                             
      GO TO 42                                                          
   50 CONTINUE                                                          
   42 CONTINUE                                                          
      IF(J2.GT.2) GO TO 43                                              
      J1=1                                                              
      J2=2                                                              
      GO TO 44                                                          
   43 CONTINUE                                                          
      IF(J2.LE.JMXIN) GO TO 45                                          
      J1=JMXIN-1                                                        
      J2=JMXIN                                                          
      GO TO 44                                                          
   45 CONTINUE                                                          
      J1=J2-1                                                           
   44 CONTINUE                                                          
      JINDX1(J)=J1                                                      
      JINDX2(J)=J2                                                      
      DDY(J)=(APHI-GAUL(J1))/(GAUL(J2)-GAUL(J1))                        
   40 CONTINUE                                                          
C                                                                       
  111 CONTINUE                                                          
C                                                                       
C     WRITE(LUPTR,*) 'IINDX1'                                                  
C     WRITE(LUPTR,*) (IINDX1(N),N=1,IMXOUT)                                    
C     WRITE(LUPTR,*) 'IINDX2'                                                  
C     WRITE(LUPTR,*) (IINDX2(N),N=1,IMXOUT)                                    
C     WRITE(LUPTR,*) 'JINDX1'                                                  
C     WRITE(LUPTR,*) (JINDX1(N),N=1,JMXOUT)                                    
C     WRITE(LUPTR,*) 'JINDX2'                                                  
C     WRITE(LUPTR,*) (JINDX2(N),N=1,JMXOUT)                                    
C     WRITE(LUPTR,*) 'DDY'                                                     
C     WRITE(LUPTR,*) (DDY(N),N=1,JMXOUT)                                       
C     WRITE(LUPTR,*) 'DDX'                                                     
C     WRITE(LUPTR,*) (DDX(N),N=1,JMXOUT)                                       
C                                                                       
      DO 60 J=1,JMXOUT                                                  
      Y=DDY(J)                                                          
      J1=JINDX1(J)                                                      
      J2=JINDX2(J)                                                      
      DO 60 I=1,IMXOUT                                                  
      X=DDX(I)                                                          
      I1=IINDX1(I)                                                      
      I2=IINDX2(I)                                                      
      REGOUT(I,J)=(1.-X)*(1.-Y)*GAUIN(I1,J1)+(1.-Y)*X*GAUIN(I2,J1)+     
     1           (1.-X)*Y*GAUIN(I1,J2)+X*Y*GAUIN(I2,J2)                 
   60 CONTINUE                                                          
C                                                                       
      SUM1=0.                                                           
      SUM2=0.                                                           
      DO 70 I=1,IMXIN                                                   
      SUM1=SUM1+GAUIN(I,1)                                              
      SUM2=SUM2+GAUIN(I,JMXIN)                                          
   70 CONTINUE                                                          
      SUM1=SUM1/FLOAT(IMXIN)                                            
      SUM2=SUM2/FLOAT(IMXIN)                                            
C                                                                       
      DO 80 I=1,IMXOUT                                                  
      REGOUT(I,     1)=SUM1                                             
      REGOUT(I,JMXOUT)=SUM2                                             
   80 CONTINUE                                                          
C                                                                       
      RETURN                                                            
      END                                                               
