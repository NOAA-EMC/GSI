;$Id: meteorFcts_sub.pro 2208 2010-06-18 22:28:17Z wchen $
;---------------------------------------------------------------------------------
; Summary of all subroutines related to meteorological functions and
; how to integrate the vertical distributions of water vapor and other
; hydrometeors.
;
; S.A. Boukabara IMSG Inc. @ NOAA/NESDIS 2005-2006
;
;---------------------------------------------------------------------------------
;===============================================================
; Name:		ComputeTPW
;
;
; Type:		IDL Subroutine
;
;
; Description:  Computes the total precipitable water (TPW).
;
;
; Arguments:
;
;      Name		    Type	    Description
;      ---------------------------------------------------
;	- P                  I              Level pressure grid in mb
;	- Psurf              I              Surface pressure in mb
;	- h2o                I              Layer water vapor in g/kg
;	- water              O              Total precipitable water (mm)
;
;
; Subroutines needed:
;       - None
;
;
; History:
;       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
;
;===============================================================

PRO ComputeTPW,P,Psurf,h2o,water  
    nl=n_elements(P)
    water=0.0
    FOR i=1,nl DO BEGIN
        if(i lt nl) then begin
            if(P(i) le psurf) then begin
                dP=P(i)-P(i-1)
                water=water+(h2o(i-1)+h2o(i))*0.5*dP
            endif  else begin
                water=water+(h2o(i-1)+h2o(i))*0.5*(psurf-P(i-1))
                BREAK 
            endelse
        endif else begin
            water=water+h2o(i-1)*0.5*(psurf-P(i-1))
        endelse
    ENDFOR
    water=water/10.0/9.8
    return
END

;===============================================================
; Name:		ColumIntegr_LayW
;
;
; Type:		IDL Subroutine
;
;
; Description:  Integrates the vertical profile of water vapor
;
;
; Arguments:
;
;      Name		    Type	    Description
;      ---------------------------------------------------
;	- nLay               I              Number of layers
;	- P                  I              Level pressure grid in mb
;	- Psurf              I              Surface pressure in mb
;	- h2o                I              Layer water vapor in g/kg
;	- water              O              Total precipitable water (mm)
;
;
; Subroutines needed:
;       - None
;
;
; History:
;       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
;
;===============================================================

PRO ColumIntegr_LayW,nLay,P,Psurf,h2o,water  
    water=0.0
    FOR i=1L,nlay DO BEGIN
        if(i lt nlay) then begin
            if(P(i) le psurf) then begin
                dP=P(i)-P(i-1)
                water=water+(h2o(i-1))*dP
            endif  else begin
                water=water+(h2o(i-1))*0.5*(psurf-P(i-1))
                BREAK 
            endelse
        endif else begin
            water=water+h2o(i-1)*0.5*(psurf-P(i-1))
        endelse
    ENDFOR
    water=water/10.0/9.8
    return
END

;===============================================================
; Name:		ColumIntegr
;
;
; Type:		IDL Subroutine
;
;
; Description:  vertically integrates the hydrometeors profiles
;
;
; Arguments:
;
;      Name		    Type	    Description
;      ---------------------------------------------------
;	- nLay               I              Number of layers
;	- P                  I              Level pressure grid in mb
;	- Psurf              I              Surface pressure in mb
;	- hydr               I              Hydrometeor profile
;	- integ              O              Total integrated amount (mm)
;
;
; Subroutines needed:
;       - None
;
;
; History:
;       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
;
;===============================================================

PRO ColumIntegr,nLay,P,Psurf,hydr,integ  
    integ=0.0
    FOR i=0L,nlay-1 DO BEGIN
        if(P(i) le psurf and hydr(i) ge 0.0 and P(i) gt 0) then begin
            integ=integ+(hydr(i))
        endif  
    ENDFOR
    return
END


;===============================================================
; Name:		Saturate_Humidity
;
;
; Type:		IDL Function
;
;
; Description:  Computes the saturated humidity
;
;
; Arguments:
;
;      Name		    Type	    Description
;      ---------------------------------------------------
;	- T                  I          temperature in Kelvin
;       - Saturate_Humidity  O          Saturate_Humidity  in Pa
;
;
; Subroutines needed:
;       - None
;
;
; History:
;       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
;
;===============================================================

Function Saturate_Humidity,T 
  Saturate_Humidity=611.0*10^(7.5*(T-273.15 )/(T-35.85))
  RETURN, Saturate_Humidity
END 


;===============================================================
; Name:		Mixingratio_to_RelHum
;
;
; Type:		IDL Function
;
;
; Description:  Converts mixing ratio to relative humidity
;
;
; Arguments:
;
;      Name		       Type	  Description
;      ---------------------------------------------------
;	- mixingratio           I          mixingratio kg/kg
;	- T                     I          temperature in Kelvin
;	- P                     I          pressure hPa
;       - Mixingratio_to_RelHum O          relative humidity  (0 - 1)
;
;
; Subroutines needed:
;       - None
;
;
; History:
;       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
;
;===============================================================

Function Mixingratio_to_RelHum,mixingratio,T,P
  Mixingratio_to_RelHum=mixingratio*(P*100.0)*  $
    1000.0/(2.16685*287.04)/Saturate_Humidity(T)
  RETURN, Mixingratio_to_RelHum
END 


;===============================================================
; Name:		RelHum_to_Mixingratio
;
;
; Type:		IDL Function
;
;
; Description:  Converts relative humidity to mixing ratio
;
;
; Arguments:
;
;      Name		       Type	    Description
;      ---------------------------------------------------
;	- RelHum                 I        relative humidity [0 1]
;	- T                      I        temperature in Kelvin
;	- P                      I        pressure in HPa
;       - RelHum_to_Mixingratio  O        mixing ratio (kg/kg)
;
;
; Subroutines needed:
;       - None
;
;
; History:
;       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
;
;===============================================================

Function RelHum_to_Mixingratio,RelHum,T,P
  RelHum_to_Mixingratio=2.16685*287.04/(P*100.0)*Saturate_Humidity(T)*RelHum/1000.0
  RETURN, RelHum_to_Mixingratio
END 


