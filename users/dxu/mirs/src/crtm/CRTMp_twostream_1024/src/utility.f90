!
    MODULE utility 
!
    PRIVATE
    PUBLIC ::  Compute_Planck,Planck,Planck_K,Bright,Bright_K
    PUBLIC ::  matinv
    PUBLIC :: RelHum_to_mixingratio,Mixingratio_to_RelHum,Saturate_Humidity
    PUBLIC :: cal_water,cal_hydrometeor,read_topography,day_month
    PUBLIC :: compute_height,ADIAB
    PUBLIC :: flip_r4,flip_i4,flip_i2
!
  CONTAINS
!
  subroutine Compute_Planck(Nch,use_channel,Nl,n_bottom, &
    wavelength,channel_index,T,Ts,Plancka)
!-----------------------------------------------------------------
! Computes Planck radiance B at wavenumber V and temperature Temp
!   INPUTS:
!      WAVELENGTH   wavelength in micrometer
!      T, Ts       temperature in Kelvin
!   OUTPUT:
!     Plancka      radiance in Watt/m**2/sr/micrometer
!-----------------------------------------------------------------
  Implicit None
  REAL( SELECTED_REAL_KIND(15) ) :: V,Temp,Ts
  INTEGER :: k,i,Nch,Nl,use_channel,n_bottom,channel_index(Nch)
  REAL( SELECTED_REAL_KIND(15) ) ::  Plancka(Nl+1,Nch)
  REAL( SELECTED_REAL_KIND(15) ) ::  T(Nl),wavelength(Nch)
      DO 101 k=1,use_channel
      V=wavelength(channel_index(k))
        DO i=1,n_bottom
        Temp=T(i)
        Plancka(i,k)=(1.1911E8)/V**5/(EXP(14388.0/(V*TEMP))-1.0) 
        ENDDO
        Temp=Ts
        Plancka(n_bottom+1,k)=(1.1911E8)/V**5/(EXP(14388.0/(V*TEMP))-1.0)
 101  CONTINUE
End subroutine Compute_Planck
!
!
  subroutine Planck(wavelength,T,Plancka)
!-----------------------------------------------------------------
! Computes Planck radiance B at wavenumber V and temperature Temp
!   INPUTS:
!      WAVELENGTH   wavelength in micrometer
!      T,           temperature in Kelvin
!   OUTPUT:
!     Plancka      radiance in Watt/m**2/sr/micrometer
!-----------------------------------------------------------------
  Implicit None
  REAL( SELECTED_REAL_KIND(15) ) :: T,wavelength,Plancka
  REAL( SELECTED_REAL_KIND(15) ) ::  C1,C2
!  DATA C1/1.1905E8/,C2/14385.0/       ! NOAA
  DATA C1/1.1911E8/,C2/14388.0/
  SAVE C1,C2
      Plancka=C1/wavelength**5/(EXP(C2/(wavelength*T))-1.0) 
!      Plancka=(1.1911E8)/wavelength**5/(EXP(14388.0/(wavelength*T))-1.0) 
  RETURN
  End subroutine Planck
!
!
  subroutine Planck_K(wavelength,T,T_K,Plancka,B_K)
!-----------------------------------------------------------------
! Computes Planck radiance B at wavenumber V and temperature Temp
!   INPUTS:
!      WAVELENGTH   wavelength in micrometer
!      T,           temperature in Kelvin
!      B_K
!     Plancka      radiance in Watt/m**2/sr/micrometer
!   OUTPUT:
!      T_K
!-----------------------------------------------------------------
  Implicit None
  REAL( SELECTED_REAL_KIND(15) ) :: T,wavelength,Plancka,T_K,B_K
      T_K=Plancka*14388.0*EXP(14388.0/(wavelength*T))/  &
      (wavelength*T**2*(EXP(14388.0/(wavelength*T))-1.0))*B_K
  RETURN
  End subroutine Planck_K
!
  subroutine Bright(wavelength,RAD,BTB)
!-----------------------------------------------------------------
! Computes Brightness Temperature
!   INPUTS:
!      WAVELENGTH   wavelength in micrometer
!      radiance     Watt/m**2/sr/micrometer
!   OUTPUT:
!     BTB     Brightness temperatures  in Kelvin
!-----------------------------------------------------------------
      Implicit None
      REAL( SELECTED_REAL_KIND(15) ) ::  wavelength,RAD,threshold,BTB
      data threshold/1.E-30/
      save threshold
      BTB=0.0
        if(RAD.GT.threshold) then
      BTB=1.4388E4/(wavelength*LOG(1.0+ 1.1911E8/(RAD*wavelength**5)))
        endif
      RETURN
      End subroutine Bright
!
  subroutine Bright_K(wavelength,RAD,RAD_K,TB_K)
!-----------------------------------------------------------------
! Computes Brightness Temperature
!   INPUTS:
!      WAVELENGTH   wavelength in micrometer
!      radiance     Watt/m**2/sr/micrometer
!      TB_K
!   OUTPUT:
!     RAD_K     Brightness temperatures  in Kelvin
!-----------------------------------------------------------------
      Implicit None
      REAL( SELECTED_REAL_KIND(15) ) :: wavelength,RAD,RAD_K,TB_K,threshold
      data threshold/1.E-30/
      save threshold
        if(RAD.GT.threshold) then
      RAD_K=1.4388E4/wavelength/  &
      (LOG(1.0+ 1.1911E8/(RAD*wavelength**5)))**2*  &
      1.1911E8/(1.1911E8+RAD*wavelength**5)/RAD*TB_K
!!      1.1911E8/(1.1911E8+RAD*wavelength**5)*TB_K
        endif
      RETURN
      End subroutine Bright_K
!
      FUNCTION matinv(A)
! Invert matrix by Gauss method
! --------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER :: n,n1
      REAL( SELECTED_REAL_KIND(15) ), intent(in),dimension(:,:) :: a
      REAL( SELECTED_REAL_KIND(15) ), dimension(size(a,1),size(a,2)) :: b
      REAL( SELECTED_REAL_KIND(15) ), dimension(size(a,1),size(a,2)) :: matinv
      REAL( SELECTED_REAL_KIND(15) ), dimension(size(a,1)) :: temp
! - - - Local Variables - - -
      REAL( SELECTED_REAL_KIND(15) ) :: c, d
      INTEGER :: i, j, k, m, imax(1), ipvt(size(a,1))
! - - - - - - - - - - - - - -
      b = a
      n=size(a,1)
      matinv=a
      ipvt = (/ (i, i = 1, n) /)
! Go into loop- b, the inverse, is initialized equal to a above
      DO k = 1,n
! Find the largest value and position of that value
         imax = MAXLOC(ABS(b(k:n,k)))
         m = k-1+imax(1)
!   sigular matrix check
        if(ABS(b(m,k)).LE.(1.0D-40)) then
!        if(ABS(b(m,k)).LE.(1.E-40)) then
!        print *,'  sigular matrix '
!        STOP
        matinv(1,1)=-99999999.0
         return 
        ENDIF
! get the row beneath the current row if the current row will
! not compute
         IF (m .ne. k) THEN
            ipvt( (/m,k/) ) = ipvt( (/k,m/) )
            b((/m,k/),:) = b((/k,m/),:)
         END IF
! d is a coefficient - brings the pivot value to one and then is applied
! to the rest of the row
         d = 1/b(k,k)
         temp = b(:,k)
         DO j = 1, n
            c = b(k,j)*d
            b(:,j) = b(:,j)-temp*c
            b(k,j) = c
         END DO
         b(:,k) = temp*(-d)
         b(k,k) = d
      END DO
      matinv(:,ipvt) = b
      END FUNCTION matinv
!
       Function Saturate_Humidity(T)
! --------------------------------------------------------
!  INPUT:  T  temperature in Kelvin
! OUTPUT   Saturate_Humidity  in Pa
! --------------------------------------------------------
       IMPLICIT NONE
       REAL( SELECTED_REAL_KIND(15) ) :: T,Saturate_Humidity
       Saturate_Humidity=611.0*10**(7.5*(T-273.15 )/(T-35.85))
       END Function Saturate_Humidity
!
      Function RelHum_to_mixingratio(Rel,T,P)
! --------------------------------------------------------
!  INPUT:  T  temperature in Kelvin, P  hPa, Rel  0 - 1
! OUTPUT   RelHum_to_mixingratio   kg/kg 
! --------------------------------------------------------
       IMPLICIT NONE
       REAL( SELECTED_REAL_KIND(15) ) :: T,P,Rel,RelHum_to_mixingratio
       RelHum_to_mixingratio=2.16685*287.04/(P*100.0)*  &
       Saturate_Humidity(T)*Rel/1000.0
      END Function RelHum_to_mixingratio
!
      Function Mixingratio_to_RelHum(mixingratio,T,P)
! --------------------------------------------------------
!  INPUT:  T  temperature in Kelvin, P  hPa, mixingratio kg/kg 
! OUTPUT   Mixingratio_to_RelHum     relative humidity  (0 - 1) 
! --------------------------------------------------------
       IMPLICIT NONE
       REAL( SELECTED_REAL_KIND(15) ) :: T,P,mixingratio,Mixingratio_to_RelHum

       Mixingratio_to_RelHum=mixingratio*(P*100.0)*  &
       1000.0/(2.16685*287.04)/Saturate_Humidity(T)
      END Function Mixingratio_to_RelHum 
!
       subroutine cal_water(P,h2o,water)
! ------------------------------------------------------
!   compute column water content
!    INPUT:   P hPa (level value),  H2O  g/kg (layer value)
!    OUTPUT:   water   kg/m**2 (column value)
! ------------------------------------------------------
       IMPLICIT NONE
       REAL( SELECTED_REAL_KIND(15) ) :: water,dP
       REAL( SELECTED_REAL_KIND(15) ), DIMENSION(:) :: P,h2o
       INTEGER i,nl
!
       nl=SIZE(P)
       water=0.0
        DO i=1,nl
         if(i.eq.1) then
         dP = P(1)
         else
         dP = P(i) - P(i-1)
         endif
         water=water+h2o(i)*dP
       ENDDO 
 913    water=water/10.0/9.8
        return
      END subroutine cal_water
!
       subroutine cal_hydrometeor(nl,h2o,water)
! ------------------------------------------------------
!   compute column water content
!    INPUT:    H2O  kg/m**2 
!    OUTPUT:   water   kg/m**2
! ------------------------------------------------------
       IMPLICIT NONE
       REAL( SELECTED_REAL_KIND(15) ) :: water,dP
       REAL( SELECTED_REAL_KIND(15) ), DIMENSION(:) :: h2o
       INTEGER i,nl
!
       water=0.0
       DO i=1,nl
       if(h2o(i).gt.0.0) water=water+h2o(i)
       ENDDO 
        return
      END subroutine cal_hydrometeor
!
      subroutine read_topography(alat,alon,alt,stype,scover)
! -------------------------------------------------------------------
!     get surface type, primary coverage, and altitude
!     based on latitude and longitude.
! -------------------------------------------------------------------
!
      implicit none
      REAL( SELECTED_REAL_KIND(15) ) :: alat,alon,alon1,alt,scover
      integer i,stype,init_topo,index_lat,index_lon
      character(LEN=1) ::  I1,I3
      INTEGER( SELECTED_INT_KIND(4) ) :: I2
!      integer(KIND=2) ::  I2
      data init_topo/0/
      SAVE init_topo
!
      if(init_topo.eq.0) then
   open(22,file='../data/CRTM_and_1dvar/topography.bin_sgi',form='unformatted', &
      access='direct',RECL=4)
      init_topo=1
      endif
!
      if(alon.gt.180.0) then
      alon1=alon-360.0
      else
      alon1=alon
      endif
      index_lat=(90.0-alat)*6+1
      index_lon=(180+alon1)*6+1
      if(index_lat.gt.1080) index_lat=1080
      if(index_lon.gt.2160) index_lon=2160
      i=(index_lat-1)*2160+index_lon
      read(22,rec=i) I1,I3,I2
      alt=I2*1.0
      scover=ICHAR(I3)*0.01
      stype=ICHAR(I1)
      return
      end subroutine read_topography
!
      subroutine day_month(year,month,day,jul_day)
      IMPLICIT NONE
      INTEGER year,month,day,jul_day,sum1,i
      INTEGER day1(12),day2(12)
      DATA day1/31,28,31,30,31,30,31,31,30,31,30,31/
      DATA day2/31,29,31,30,31,30,31,31,30,31,30,31/
      SAVE day1,day2
!
       sum1=0
         DO i=1,12
       if(MOD(year,4).eq.0) then
         sum1=sum1+day2(i)
       else
         sum1=sum1+day1(i)
       endif
         if(jul_day.lt.sum1) go to 900
         ENDDO
!
 900    month=i
        if(MOD(year,4).eq.0) then
         day=jul_day-(sum1-day2(i))
        else
         day=jul_day-(sum1-day1(i))
        endif
        return
        END subroutine day_month
!
       subroutine compute_height(N,P,T,H2O,Ps,Z)
       IMPLICIT NONE
       INTEGER N,i,k
       REAL( SELECTED_REAL_KIND(15) ) :: T(N),H2O(N),P(N),Z(N),ZZ(100),Ps
       REAL( SELECTED_REAL_KIND(15) ) :: PP,TT,dP,dz
!
       DO i=1,N
       ZZ(i)=0.0
       Z(i)=0.0
       ENDDO

       zz(1)=0.0
       do i=1,N-1
        if(P(i+1).le.Ps) then
       PP=(P(i+1)+P(i))/2.0
       TT=(T(i+1)+T(i))/2.0
       dP=P(i+1)-P(i)
       dz=dP/PP*287.0*TT/9.8
!  convert meter to km
!       dz=dz/1000.0
       ZZ(i+1)=ZZ(i)+dz
        else
       PP=(Ps+P(i))/2.0
       TT=(T(i+1)+T(i))/2.0
       dP=Ps-P(i)
       dz=dP/PP*287.0*TT/9.8
!  convert meter to km
!       dz=dz/1000.0
       ZZ(i+1)=ZZ(i)+dz
       go to 801
        endif
       enddo
 801   continue
       K=i+1
       if(K.gt.N) K=N
       DO i=1,K
       Z(i)=ZZ(K)-ZZ(i)
       enddo
       return
       end subroutine compute_height
!
      subroutine ADIAB(NLAY,T,P,Z,RH,Wc,WIC,Wre,awc,aic,relsat,PWV)
! --------------------------------------------------------------------
!  adibatic calculation
! INPUTS:
!    T:         temperature (K)
!    P:         pressure (hPa)
!    Z:         height (km)
!   RH:         relative humidity  (0 - 100)
!  WIC:         ice water content profile  (g/m**3)
!  WRE:         liquid water content (g/m**3)
!   WC:         liquid water content (g/m**3)
!  PWV:         water vapor profile  (g/m**3)
!
! awc:          total liquid water content
! aic:          total ice water content
! relsat:       threshold value for liquid
! --------------------------------------------------------------------
      REAL( SELECTED_REAL_KIND(15) ) :: LWC,L,deck,relsat
      INTEGER :: nlay,nl,i,j
      REAL( SELECTED_REAL_KIND(15) ) :: T(NLAY),P(NLAY),Z(NLAY),RH(Nlay),PWV(NLAY)
      REAL( SELECTED_REAL_KIND(15) ) :: wc(nlay),wic(nlay),wre(nlay)
      REAL( SELECTED_REAL_KIND(15) ) :: G, CP, rel, z00, P10, R, aic, awc, WS, RWV, g1
      data G/9.80616/,CP/1005.0/
      SAVE G, CP
      nl=nlay-1
      rel=rh(nlay)*0.01
      PWV(nlay)=ABHUM(T(nlay),Rel)
      LWC=0.
      z00=z(nl+1)
      DO 101 J=NL,1,-1
      rel=rh(j)*0.01
      PWV(J)=ABHUM(T(J),Rel)

      wc(j)=0.
      wic(j)=0.
      wre(j)=0.
      if(z(j).lt.0.) then
      z00=z(j+1)
      go to 101
      end if
      if(rh(j).le.relsat) then
      LWC=0.
      z00=z(j+1)
      else
      L=VAPHET(T(j))
      p10=p(j)*100.
      rel=rh(j)*0.01
      R=DENSIty(rel,T(J),P10)
      RWV=ABHUM(T(J),Rel)
      WS=RWV/(R-RWV)
      LWC=LWC+R*CP/L*(G/CP-PSEUDO(T(j),WS))*(Z(J)-Z(J+1))*1.E+6

      if(t(j).le.253.16) then
      g1=4.*dexp(-0.0002443*(253.16-t(j))**2.455)
      wic(j)=dexp(-7.6+g1)
      end if
      end if
      if(lwc.gt.0.0000001) then
      deck=(z(j)-z00)*1000.
      if(deck.gt.5000.) deck=5000.
!      wc(j)=LWC*(-0.14479*dlog(deck)+1.239387)
      wc(j)=LWC*(-0.14479*log(deck)+1.239387)
      end if
 101  CONTINUE
      awc=0.
      aic=0.
      do 102 i=1,nl
      awc=awc+wc(i)*(z(i)-z(i+1))
      aic=aic+wic(i)*(z(i)-z(i+1))
 102  continue
      RETURN
      END subroutine ADIAB
!
      FUNCTION ABHUM(T,RH)
!      implicit real*8(a-h,o-z)
      REAL( SELECTED_REAL_KIND(15) ) :: MW, R,ES,T,RH,ABHUM
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!
!     This function calculates the absolute humidity in kg/m**3 from 
!     air temperature and relative humidity by a formula taken from
!     LILJEQUIST :
!
!              A = MW * ES * RH / ( R * T )
!
!     Parameter :
!     (Input )
!               T   :   temperature   [ K ]
!               RH  :   rel. humidity [ 1 ]
!
!     (Output)
!               A   :   absolute humidity [ kg/m**3 ]
!
!     Constants :
!               MW  :   molecular weight ( =18.016 [kg/kmol] )
!               R   :   gas constant   ( =8314.3 [J/kmol K] )
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      MW = 18.016
      R  = 8314.3
!
!   Calculate saturation vapor pressure
!
     ES = ESTET ( T )
!
!   Calculate absolute humidity
!
     ABHUM = (MW * ES * RH) / (8314.3 * T )
      RETURN
      END FUNCTION ABHUM
!
      FUNCTION ESTET(T)
      REAL( SELECTED_REAL_KIND(15) ) :: T, ESTET
!   Calculate saturation vapor pressure
!
      ESTET = 611.0 * 10.0** (7.5 * ( T - 273.15 ) / ( T - 35.85 ))
      RETURN
      END FUNCTION ESTET 
!
      FUNCTION TVIRT(RH,T,P)
      REAL( SELECTED_REAL_KIND(15) ) :: T, ES, RH, P, TVIRT
!   Calculate saturation vapor pressure by TETENS formula [ - ESTET ]
!   Calculate virtuel temperature
      ES = ESTET ( T )
      TVIRT = T * ( 1. + 0.00378 * ( RH * ES / P)  )
      RETURN
      END FUNCTION TVIRT
!
      FUNCTION DENSITY(RH,T,P)
      REAL( SELECTED_REAL_KIND(15) ) :: RH,T,P,TV,DENSITY
!   Calculate virtual temperature
!   Calculate density
!
       TV = TVIRT(RH,T,P)
       DENSITY = P  / ( TV * 287.04 )
       RETURN
       END FUNCTION DENSITY
!
!**********************************************************************
!                                                                     *
!   Pseudoadiabatic lapse rate                                        *
!                                                                     *
!   Input: T   [K]  thermodynamic temperature                         *
!          W   [1]  mixing ratio of saturation                        *
!                                                                     *
!   Output: PSEUDO [K/m] pseudoadiabatic lapse rate                   *
!                                                                     *
!   Constants: G   [m/s2]     : constant of acceleration              *
!              CP  [J/(kg K)] : specific heat cap. at const. press.   *
!              RL  [J/(kg K)] : gas constant of dry air               *
!              RW [J/(kg K)] : gas constant of water vapor           *
!                                                                     *
!   Source: Rogers and Yau, 1989: A Short Course in Cloud Physics     *
!           (III.Ed.), Pergamon Press, 293p. Page 32                  *
!                                                                     *
!**********************************************************************

       double precision FUNCTION PSEUDO(T,WS)
       REAL( SELECTED_REAL_KIND(15) ) :: T,L,RL,RW,G,CP,WS
       DATA RL/287.05/,RW/461.5/,G/9.80616/,CP/1005.0/
       SAVE RL,RW,G,CP
!   Compute specific humidity of vaporisation
!   Compute pseudo-adiabatic temperature gradient
      L=VAPHET(T)
      PSEUDO=(G/CP) * (1+(L*WS/RL/T)) / (1+(WS*L**2/CP/RW/T**2))
      RETURN
  END FUNCTION PSEUDO
!**********************************************************************
!   Compute specific heat of vaporisation                             *
!                                                                     *
!   Input  : T      [K]      thermodynamic temperature                *
!                                                                     *
!   Output : VAPHET [J/kg]   specific heat of vaporisation            *
!                                                                     *
!   Source: Liljequist, G.H. und K. Cehak, 1984: Allgemeine           *
!           Meteorologie (III.Ed.). Vieweg, 396p. Page 95             *
!                                                                     *
!**********************************************************************

      DOUBLE PRECISION FUNCTION VAPHET(T)
      REAL( SELECTED_REAL_KIND(15) ) :: T
      VAPHET=(2500.8-2.372*(T-273.15)) * 1000.0
      Return
      END FUNCTION VAPHET
!
      subroutine flip_r4(x)
! -----------------------------------------------
!   swith bates for lunix
! -----------------------------------------------
       character ch
       character(len=4) :: a
       real x,y
       equivalence(y,a)
       y=x
       ch=a(4:4)
       a(4:4)=a(1:1)
       a(1:1)=ch
       ch=a(2:2)
       a(2:2)=a(3:3)
       a(3:3)=ch
       x=y
       return
       end  subroutine flip_r4
!
      subroutine flip_i4(x)
! -----------------------------------------------
!   swith bates for lunix
! -----------------------------------------------
       character ch
       character(len=4) ::  a
       integer x,y
       equivalence(y,a)
       y=x
       ch=a(4:4)
       a(4:4)=a(1:1)
       a(1:1)=ch
       ch=a(2:2)
       a(2:2)=a(3:3)
       a(3:3)=ch
       x=y
       return
       end subroutine flip_i4
!
      subroutine flip_i2(x)
! -----------------------------------------------
!   swith bates for lunix
! -----------------------------------------------
!       character ch
!       character(len=2) ::  a
       INTEGER(KIND=1) ::  a(2),ch
       INTEGER(KIND=2) :: x,y
       equivalence(y,a)
       y=x
       ch=a(2)
       a(2)=a(1)
       a(1)=ch
       x=y
       return
       end subroutine flip_i2
!
!
 END MODULE utility 
