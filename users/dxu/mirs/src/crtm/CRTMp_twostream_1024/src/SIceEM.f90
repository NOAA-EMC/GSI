!
!!!  Subroutine  SIceEM(theta,frequency,depth,ts,tba,tbb,esv,esh)
  Subroutine  SIceEM(theta,frequency,depth,ts,tba,tbb,em_vector)
!---------------------------------------------------------------------------!
!
! Programer: 
!           Banghua Yan
!
! Function:
!           Simulate sea ice microwave emssivity 
!
! Input Variables:
! 
!       theta            ----  local zenith angle in radian	 
!       frequency        ----  frequency in GHz	
!		ts               ----  surface temperature (K)		 (GDAS)
!		depth            ----  scatter medium depth (mm)  (not used here)						 (GDAS)
!
!		tba[1] ~ tba[4]  ----  brightness temperature at four AMSU-A window channels:
!                              tba[1] : 23.8 GHz
!                              tba[2] : 31.4 GHz
!                              tba[3] : 50.3 GHz
!                              tba[4] : 89 GHz
!		tbb[1] ~ tbb[2]  ----  brightness temperature at two AMSU-B window channels:
!                              tbb[1] : 89 GHz
!                              tbb[2] : 150 GHz
!
!       When tba[ ] or tbb[ ] = -999.9, it means a missing value (no available data)
!
!
!	Output variables:
!
!		em_vector        ----  esv, esh
!
!       sea ice_type (to be determined)
!
!  Code history:
!
!       beta version: Nov., 2003
!---------------------------------------------------------------------------       
   implicit none
   real:: pi   
   integer      :: nch,nwcha,nwchb,nwch,nalg   
   Parameter(nwcha = 4, nwchb = 2, nwch = 5,nalg = 7)
   REAL( SELECTED_REAL_KIND(15) ) ::  depth
   real    :: theta,frequency,ts
   real    :: em_vector(2),esv,esh
   real    :: tb(nwch),tba(nwcha),tbb(nwchb)
   logical :: INDATA(nalg),AMSUAB,AMSUA,AMSUB,ABTs,ATs,BTs,MODL
   integer :: seaice_type,input_type,i,ich,np,k
   Equivalence(INDATA(1), ABTs)
   Equivalence(INDATA(2), ATs)
   Equivalence(INDATA(3), AMSUAB)
   Equivalence(INDATA(4), AMSUA)
   Equivalence(INDATA(5), BTs)
   Equivalence(INDATA(6), AMSUB)
   Equivalence(INDATA(7), MODL)
!  Initialization 
   pi = acos(-1.0)
   em_vector(1) = 0.85
   em_vector(2) = 0.82
   seaice_type  = -999
   input_type = -999
   do k = 1, nalg
      INDATA(k) = .TRUE.
   enddo
!                                           Read AMSU & Ts data and set available option
!                                                   Get five AMSU-A/B window measurements
   tb(1) = tba(1)
   tb(2) = tba(2)
   tb(3) = tba(3)
   tb(4) = tba(4)
   tb(5) = tbb(2)
!                                                                    Check available data 
     if((ts .le. 100.0) .or. (ts .ge. 320.0) ) then
      ABTs = .false.
      ATs  = .false.
      BTs  = .false.
      MODL = .false.
     endif
     do i=1,nwcha
        if((tba(i) .le. 100.0) .or. (tba(i) .ge. 320.0) ) then
          ABTs   = .false.
        ATs    = .false.
        AMSUAB = .false.
        AMSUA  = .false.
        exit
        endif
     enddo
     do i=1,nwchb
        if((tbb(i) .le. 100.0) .or. (tbb(i) .ge. 320.0) ) then
        ABTs  = .false.
        AMSUAB = .false.
        BTs  = .false.
        AMSUB  = .false.
        exit
        endif
     enddo
      if((depth .lt. 0.0) .or. (depth .ge. 3000.0)) MODL = .false.
      if((frequency .ge. 80.) .and. (BTs)) then
        ATs = .false.
        AMSUAB = .false.
       endif 

!                                Check input type and call a specific Option/subroutine
   
   DO np = 1, nalg
      if (INDATA(np)) then
     input_type = np
         exit
  endif
   ENDDO

   GET_option: SELECT CASE (input_type)
   CASE (1)     
!        call siem_ABTs(theta,frequency,tb,ts,seaice_type,em_vector)
   CASE (2)     
        call siem_ATs(theta,frequency,tba,ts,seaice_type,em_vector)
   CASE (3)      
!        call siem_AB(theta,frequency,tb,seaice_type,em_vector)
   CASE (4)
!        call siem_amsua(theta,frequency,tba,seaice_type,em_vector)
   CASE(5)
    call siem_BTs(theta,frequency,tbb,ts,seaice_type,em_vector)
   CASE(6)
!        call siem_amsub(theta,frequency,tbb,seaice_type,em_vector)
   CASE(7)
!        call siem_default(theta,frequency,depth,ts,seaice_type,em_vector)
   END SELECT GET_option
   if (em_vector(1) .gt. 1.0) em_vector(1) = 1.0
   if (em_vector(2) .gt. 1.0) em_vector(2) = 1.0
   if (em_vector(1) .lt. 0.6) em_vector(1) = 0.6
   if (em_vector(2) .lt. 0.6) em_vector(2) = 0.6
   esv = em_vector(1)
   esh = em_vector(2)

!  Liu
    em_vector(1)=esh
    em_vector(2)=esv

   END Subroutine SIceEM


subroutine siem_ATs(theta,frequency,tba,ts,seaice_type,em_vector)

!---------------------------------------------------------------------------------------!
! Function:
!
!         Calculate the emissivity discriminators and interpolate/extrapolate
!  emissivity at required frequency with respect to secenery AMSUA & Ts
!
!   Input variables:
!
!
!       frequency        ----  frequency in GHz
!
!       theta            ----  local zenith angle in radian
!
!       ts               ----  surface temperature
!
!	    tba[1] ~ tba[4]  ----  brightness temperature at five AMSU-A window channels:
!                              tba[1] : 23.8 GHz
!                              tba[2] : 31.4 GHz
!                              tba[3] : 50.3 GHz
!                              tba[4] : 89   GHz
!
!
!   Important internal variables/parameters:
!
!
!		coe23           ---- fitting coefficients to estimate discriminator at 23.8 GHz
!
!		coe31           ---- fitting coefficients to estimate discriminator at 31.4 GHz
!
!		coe50           ---- fitting coefficients to estimate discriminator at 50.3 GHz
!
!		coe89           ---- fitting coefficients to estimate discriminator at 89   GHz
!
!		coe150          ---- fitting coefficients to estimate discriminator at 150  GHz
!
!
!	Output variables:
!
!		em_vector[1] and [2]  ----  emissivity at two polarizations. 
!                              set esv = esh here and will be updated
!       
!       seaice_type        ----  	 ?????
!		
!---------------------------------------------------------------------------------!

implicit none
integer,parameter:: nch =10,nwch = 5,ncoe = 10
real    :: tba(*),theta
real    :: em_vector(*),emissivity,ts,frequency,discriminator(nwch)
integer :: seaice_type,i,k,ich,nvalid_ch
!real*8  :: coe23(0:ncoe),coe31(0:ncoe),coe50(0:ncoe),coe89(0:ncoe),coe150(0:ncoe)
REAL( SELECTED_REAL_KIND(15) ), DIMENSION(0:ncoe) :: coe23,coe31,coe50,coe89,coe150
REAL( SELECTED_REAL_KIND(15) ), DIMENSION(nch*(ncoe+1)) :: coe
!real*8  :: coe(nch*(ncoe+1))
Equivalence (coe(1),coe23)
Equivalence (coe(21),coe31)
Equivalence (coe(41),coe50)
Equivalence (coe(61),coe89)
Equivalence (coe(81),coe150)
!                           Fitting Coefficients at 23.8 GHz: Using Tb1, Tb2 and Ts
data coe23/ -2.491836e+000,  4.644502e-002, -9.593168e-005, -4.220644e-002,  &
             9.882050e-005,  2.723754e-002, -6.187501e-005, -5.724581e-004,  &
    4.815041e-007, -1.420312e-002, -2.362082e-003/
!                           Fitting Coefficients at 31.4 GHz: Using Tb1, Tb2 and Ts

data coe31/ -2.552060e+000,  3.461913e-002, -7.818796e-005, -3.094014e-002,  &
             8.221620e-005,  2.821830e-002, -6.392461e-005, -5.208531e-004,  &
     6.478691e-007, -1.825029e-002, -2.388536e-003/

!                      Fitting Coefficients at 50.3 GHz: Using Tb1, Tb2, Tb3 and Ts
data coe50/-3.217067e+000,  6.485219e-002, -1.476230e-004, -4.902203e-002,  &
             1.189236e-004,  1.388731e-002, -2.288052e-005,  8.612061e-003,  &
    -1.866321e-005,  1.091455e-002, -4.066406e-003/

!                          Fitting Coefficients at 89 GHz: Using Tb1 ~ Tb4 and Ts
data coe89/  -1.843540e+000,  4.573410e-002, -1.017161e-004, -4.500178e-002,  &
              1.038772e-004,  1.701683e-002, -4.467814e-005,  8.785107e-003,  &
 -7.595831e-006,  5.735541e-003, -2.768894e-003/
 !                        Fitting Coefficients at 150 GHz: Using Tb1 ~ Tb4 and Ts
 data coe150/ -4.480585e+000,  1.059987e-001, -2.272918e-004, -1.029932e-001,  &
               2.228334e-004,  4.358730e-002, -1.062647e-004,  2.966921e-003,  &  
   1.159196e-005,  5.150734e-002, -3.152954e-003/

save coe23,coe31,coe50,coe89,coe150
!                       Calculate emissivity discriminators at five AMSU window channels
DO ich = 1, nwch
   discriminator(ich) = coe(1+(ich-1)*20)
   nvalid_ch = 4 
   DO i=1,nvalid_ch
      discriminator(ich) = discriminator(ich) + coe((ich-1)*20 + 2*i)*tba(i) +               &
            coe((ich-1)*20 + 2*i+1)*tba(i)*tba(i)
   ENDDO

   discriminator(ich) = discriminator(ich) +                                                &
    coe( (ich-1)*20 + (nvalid_ch+1)*2 )*cos(theta)   +               &
    coe( (ich-1)*20 + (nvalid_ch+1)*2 + 1 )*ts
ENDDO
  
call siem_interpolate(frequency,discriminator,emissivity,seaice_type)

em_vector(1) = emissivity
em_vector(2) = emissivity
1000 format(10(f4.2,1x))

end subroutine siem_ATs



subroutine siem_BTs(theta,frequency,tbb,ts,seaice_type,em_vector)

!---------------------------------------------------------------------------------------!
! Function:
!
!         Calculate the emissivity discriminators and interpolate/extrapolate
!  emissivity at required frequency with respect to secenery BTs 
!
!   Input variables:
!
!
!       frequency        ----  frequency in GHz
!
!       theta            ----  local zenith angle (not used here)
!
!       ts               ----  surface temperature in degree
!
!	    tbb[1] ~ tbb[2]  ----  brightness temperature at five AMSU-B window channels:
!                              tbb[1] : 89  GHz
!                              tbb[2] : 150 GHz
!
!
!   Important internal variables/parameters:
!
!		coe31           ---- fitting coefficients to estimate discriminator at 31.4 GHz
!
!		coe89           ---- fitting coefficients to estimate discriminator at 89   GHz
!
!		coe150          ---- fitting coefficients to estimate discriminator at 150  GHz
!
!
!	Output variables:
!
!		em_vector(1) and (2)  ----  emissivity at two polarizations. 
!                              set esv = esh here and will be updated
!       
!       seaice_type        ----  snow type (reference [2])		
!                              1 : Wet Snow        
!                              2 : Grass_after_Snow           
!                              3 : RS_Snow (A)
!                              4 : Powder Snow
!                              5 : RS_Snow (B)
!                              6 : RS_Snow (C)
!                              7 : RS_Snow (D)
!                              8 : Thin Crust Snow
!                              9 : RS_Snow (E)
!                              10: Bottom Crust Snow (A)
!                              11: Shallow Snow
!                              12: Deep Snow
!                              13: Crust Snow
!                              14: Medium Snow
!                              15: Bottom Crust Snow (B)
!                              16: Thick Crust Snow
!---------------------------------------------------------------------------------!
   
implicit none
integer,parameter:: nch =10,nwch = 5,ncoe = 6
real    :: tbb(*),theta
real    :: em_vector(*),emissivity,ts,frequency,discriminator(nwch)
integer :: seaice_type,i,k,ich,nvalid_ch
!real*8  :: coe23(0:ncoe),coe31(0:ncoe),coe50(0:ncoe),coe89(0:ncoe),coe150(0:ncoe)
!real*8  :: coe(nch*(ncoe+1))
REAL( SELECTED_REAL_KIND(15) ), DIMENSION(0:ncoe) :: coe23,coe31,coe50,coe89,coe150
REAL( SELECTED_REAL_KIND(15) ), DIMENSION(nch*(ncoe+1)) :: coe
Equivalence (coe(1),coe23)
Equivalence (coe(11),coe31)
Equivalence (coe(21),coe50)
Equivalence (coe(31),coe89)
Equivalence (coe(41),coe150)
!                                  Fitting Coefficients at 31.4 GHz

data coe23/ 2.239429e+000, -2.153967e-002,  5.785736e-005,  1.366728e-002,    &

           -3.749251e-005, -5.128486e-002, -2.184161e-003/

data coe31/ 1.768085e+000, -1.643430e-002,  4.850989e-005,  1.288753e-002,   &

           -3.628051e-005, -4.751277e-002, -2.580649e-003/

data coe50/ 8.910227e-001,  6.170706e-003, -3.772921e-006, -4.146567e-004,   &

           -2.208121e-006, -3.163193e-002, -3.863217e-003/

data coe89/ 2.131304e-001,  1.093012e-002, -1.416395e-005, -3.076028e-005,   &

           -5.209114e-007, -5.899909e-003, -4.221833e-003/


data coe150/ 6.853691e-001,  1.991818e-002, -4.789746e-005, -7.289032e-003,   &

             2.989625e-005,  8.349298e-003, -7.136807e-003/



save coe23,coe31,coe50,coe89,coe150


!                        Calculate emissivity discriminators at five AMSU window channels

DO ich = 1, nwch
   discriminator(ich) = coe(1+(ich-1)*10)
   nvalid_ch = 2 
   DO i=1,nvalid_ch
      discriminator(ich) = discriminator(ich) + coe((ich-1)*10 + 2*i)*tbb(i) +               &
            coe((ich-1)*10 + 2*i+1)*tbb(i)*tbb(i)
   ENDDO

   discriminator(ich) = discriminator(ich) +                                                &
    coe( (ich-1)*10 + (nvalid_ch+1)*2 )*cos(theta)  +               &
    coe( (ich-1)*10 + (nvalid_ch+1)*2 + 1 )*ts
ENDDO

call siem_interpolate(frequency,discriminator,emissivity,seaice_type)
em_vector(1) = emissivity
em_vector(2) = emissivity
end subroutine siem_BTs

subroutine siem_interpolate(frequency,discriminator,emissivity,seaice_type)
!----------------------------------------------------------------------------------------!
!
! Function:
!
!        (1) Find one snow emissivity spectrum to mimic the emission property of the
! realistic snow condition using a set of discrminators	
!        (2) Interpolate/extrapolate emissivity at a required frequency 
!
! Input variables:
!
!       frequency             ---- frequency in GHz
!       discriminators        ---- emissivity discriminators at five AMSU-A & B window
!                                  channels 
!            discriminator[1]   :  emissivity discriminator at 23.8 GHz
!            discriminator[2]   :  emissivity discriminator at 31.4 GHz
!            discriminator[3]   :  emissivity discriminator at 50.3 GHz
!            discriminator[4]   :  emissivity discriminator at 89   GHz
!            discriminator[5]   :  emissivity discriminator at 150  GHz
!
!       Note: discriminator(1) and discriminator(3) are missing value in 
!            'AMSU-B & Ts','AMUS-B' and 'MODL' options., which are defined to as -999.9, 
!       
!
! Internal variables:
!
!
! Output variables:
!
!		em_vector[1] and [2]  ----  emissivity at two polarizations. 
!       seaice_type             ----  snow type (reference [2])		
!----------------------------------------------------------------------------------------!
implicit none
integer,parameter:: ncand = 16,nch =5
integer:: ich,ichmin,ichmax,i,j,k,s,seaice_type
real   :: dem,demmin0
real   :: em(ncand,nch)
real   :: frequency,freq(nch),emissivity,discriminator(*)
real   :: cor_factor,adjust_check,kratio, bconst
data  freq/23.8, 31.4, 50.3,89.0, 150./

!                                        Estimate sea ice emissivity at a required frequency
 
  seaice_type = -999   ! temporal assumption

  do i = 2, nch
     if(frequency .lt. freq(1))   exit
 if(frequency .ge. freq(nch)) exit
 if(frequency .lt. freq(i)) then
    emissivity = discriminator(i-1) + (discriminator(i) - discriminator(i-1))*(frequency - freq(i-1))    &
         /(freq(i) - freq(i-1))
  exit
 endif
  enddo
  
  if(frequency .lt. freq(1))    emissivity = discriminator(1)

  !                                Assume emissivity = constant at frequencies >= 150 GHz	

  if (frequency .ge. freq(nch)) emissivity = discriminator(nch)

 end subroutine siem_interpolate 


