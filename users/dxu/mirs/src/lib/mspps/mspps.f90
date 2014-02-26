!----------------------------------------------------------------------
!
!   NOAA/NESDIS/ORA AMSU Cloud and Precipitation Algorithms
!   
!----------------------------------------------------------------------
!   The codes are created by AMSU CPA Group
!
!   Personnels
!
!     Fuzhong Weng and Limin Zhao (Lead Scientists)
!     Ralph Ferrao, Norman Grody
!
!     Copyright @ 2001
!----------------------------------------------------------------------
!
!   The Algorithm retrieves cloud and precipitation products based on
!   on AMSU measurements from either NOAA-15 or NOAA-16. The products
!   includes: total precipitable water, cloud liquid water, cloud ice
!   water path, ice particle effective diameters and surface rain rate.
!   Ancillary data such as surface temperature and wind speed are also 
!   needed in the current version. These data can be obtained from NCEP 
!   GDAS data.
!
!   Version
!
!     v0: Released on 10/25/01
!
!         No screening procedures are provided in this version. However, 
!         placeholders are set for snow and sea ice flages.  Cloud and
!         precipitation products can be retrieved when AMSU 
!         measurements are available. Surface temperature and wind speed
!         are needed in determining cloud base brightness temperature.
!
!----------------------------------------------------------------------
!   The program modification history 
!    
!      Code Created -------- 10/8/01 
!
!----------------------------------------------------------------------
!
!   Benchmark results:
!   
!   Satellite: NOAA-15
!   Node:      Ascending
! 
!   Case 1.
!     Clear Atmospheric Conditions (Ocean)
!
!     Inputs 
!       Landseatag = 1
!       TA23 = 208, TA31 = 175, TA50 = 239, TA89 = 252, TA150 = 286 (K)
!       TA176 = 274 (K), theta = 32 (degree), SST = 300 (K)  Wind = 0.0 (m/s)
!
!     Outputs 
!       V = 49.77 (kg/m**2)  L = 0.0 (kg/m**2) 
!       IWP = 0.0 (kg/m**2) De = 0.0 (mm) RR = 0.0 (mm/h)
!
!   Case 2.
!     Cloudy Atmospheric Conditions (no precipitating) (Land)
!
!     Inputs 
!
!       Landseatag = 1
!       TA23 = 286, TA31 = 285, TA50 = 284, TA89 = 281 TA150 = 279 (K)
!       TA176 = 263 (K) theta = 4.0 (degree),SST = 304 (K)  Wind = 6.0 (m/s)
!
!     Outputs 
!       V = 0 (kg/m**2)  L = 0.0 (kg/m**2) 
!       IWP = 0.06 (kg/m**2) De = 0.4 (mm) RR = 0.0 (mm/h)
!
!   Case 3.
!     Cloudy Atmospheric Conditions (no precipitating) (Ocean)
!
!     Inputs:
!       Landseatag = 0
!        TA23 = 201, TA31 = 182, TA50 = 239, TA89 = 260, TA150 = 284 (K)
!        TA176 = 277 (K)  theta = 9.0 (degree), SST = 299 (K), Wind = 8.0 (m/s)
! 
!     Outputs:
!        V = 45.03 (kg/m**2), L = 0.37 ((kg/m**2)
!        IWP = 0.0 (kg/m**2) De = 0.0 (mm) RR = 0.0 (mm/h)
!
!   Case 4.
!     Cloudy Atmospheric Conditions (precipitating) (Land)
!
!     Inputs 
!       Landseatag = 1
!       TA23 = 273, TA31 = 271, TA50 = 258, TA89 = 194, TA150 = 167 (K)
!       TA176 = 161 (K) theta = 33 (degree), SST = 293(K)  
!
!     Outputs 
!       V = 0 (kg/m**2)  L = 0.0 (kg/m**2) 
!       IWP = 1.46 (kg/m**2) De = 1.50 (mm) RR = 17.3 (mm/hr)
!
!   Case 5.
!      Cloudy Atmospheric Conditions (precipitating) (Ocean)
!      
!      Inputs
!        Landseatag = 0
!        TA23 = 241, TA31 =  221, TA50 = 251, TA89 = 210, TA150 = 188 (K)
!        TA176 = 175 (K), theta = 29 (degree), SST = 301 (K), Wind = 9.0
!
!      Outputs
!        V = 70.19 (kg/m**2)  L =0.94 (kg/m**2)
!        IWP = 1.48 (kg/m**2) De = 1.78 (mm), RR = 17.42 (mm/hr)
! 
!---------------------------------------------------------------------
       Program main
!----------------------------------------------------------------------
!
!   Called Subroutines:
!
!     benchmark_data   ---- Read in the benchmark data provided
!     AMSU_algorithm   ---- The maindriver for the algorithm
!     amsu_aymmetry    ---- AMSU asymmetry correction
!     vapor_liquid     ---- AMSU TPW and cloud liquid water
!     ice_water_path   ---- AMSU cloud ice water path and size
!     rain_rate        ---- AMSU surface rainfall rate
!     tb_base_ocean    ---- estimating 89 and 150 GHz Tbs at ice cloud
!                           base over ocean
!     tb__base_land    ---- estimating 89 and 150 GHz Tbs at ice cloud
!                           base over land
!     OceanEM          ---- computing ocean surface emissivity
!
!---------------------------------------------------------------------
!      Inputs
!
!       sat_id char*3 ------ Satellite, e.g, N15 (n15) or N16 (n16)
!       node   char*2 ------ (ascending = 'as', descending = 'ds') 
!       ta     real*4 ------ AMSU antenna temperature at 23, 31, 50.3
!                            89, 150 and 176 GHz
!      theta   real*4 ------ AMSU local zenith angle in degree 
!       Wind   real*4 ------ surface wind (m/s)
!        Ts    real*4 ------ surface temperature (K)
!    landeatag int*1  ------ land sea tag (1 = land, 0 = ocean)
!    snowflag  int*1  ------ snow flag (1 = snow, 0 = no snow)
!    siceflag  int*1  ------ sea ice flag (1 = sea ice, 0 = no sea ice)
!
!      Outputs
!
!        V     real*4 ------ AMSU total precipitable water (mm)
!        L     real*4 ------ AMSU cloud liquid water path (mm)
!       IWP    real*4 ------ AMSU cloud ice water path (mm)
!        De    real*4 ------ AMSU cloud ice effective diameter (mm)
!        RR    real*4 ------ AMSU surface rain rate (mm/h)
!
!-----------------------------------------------------------------
       implicit none
       parameter(screen_flag=-999)
       character sat_id*3,node*2
       character filename*20
       integer*1 landseatag
       real*4    ta(6), tb(6), theta, wind, ts, tl
       real*4     l, v, iwp, de, rr

       filename='Rev.log'      
       open(9, file = filename, status='unknown')
 
       call benchmark_data(sat_id, node, ta, theta, ts, wind, landseatag, &
                           snowflag, siceflag)
       print *, 'done reading'
       if(snowflag.eq.1.or.siceflag.eq.1)then
          iwp = screen_flag
          de  = screen_flag
          rr  = screen_flag
          v   = screen_flag
          l   = screen_flag
       else
          call AMSU_algorithm(sat_id, node, ta, theta,landseatag, &
                               wind, ts, v, l, iwp, de, rr)
       endif
       write(9,*)'The retrieved results'
       write(9,*)'TPW=',v, 'CLW=', l
       write(9,*)'IWP=',iwp, 'De=', de, 'Rain rate = ', rr

       stop
       END
       
!---------------------------------------------------------------------

       Subroutine AMSU_algorithm(sat_id, node ,ta, theta, landseatag, &
                                 wind, ts,v, l, iwp, de, rr)
!-----------------------------------------------------------------
!      Inputs
!
!       sat_id char*3 ------ Satellite, e.g, N15 (n15) or N16 (n16)
!       node   char*2 ------ (ascending = 'as', descending = 'ds') 
!       ta     real*4 ------ AMSU antenna temp.
!      theta   real*4 ------ AMSU local zenith angle in radiance 
!       Wind   real*4 ------ surface wind (m/s)
!        Ts    real*4 ------ surface temperature (K)
!    landeatag int*1  ------ land sea tag (1 = land, 0 = ocean)
!        
!      Outputs 
!
!        V     real*4 ------ AMSU total precipitable water (mm)
!        L     real*4 ------ AMSU cloud liquid water path (mm)
!       IWP    real*4 ------ AMSU cloud ice water path (mm)
!        De    real*4 ------ AMSU cloud ice effective diameter (mm)
!        RR    real*4 ------ AMSU surface rain rate (mm/h)
!          
!-----------------------------------------------------------------
      implicit none
      character sat_id*3,node*2
      integer*1 landseatag
      real*4	ta(6), tb(6), theta, wind, ts, tl
      real*4	 l, v, iwp, de, rr

      call amsu_asymmetry(landseatag, sat_id, node, ta, tb, theta)

      tl = 285.0
      if(tl.lt.273.15) then
         tl = 0.0
      else
         tl = tl - 273.15
      endif
      call vapor_liquid(landseatag, tb, wind, ts, tl, theta, v, l)
      tl=2.0
      call ice_water_path(landseatag, tb, ts, wind, v, l, tl, &
        		  theta, iwp, de)
      call rain_rate(landseatag, tb, l, iwp, de, rr)

      return
      END

!-----------------------------------------------------------------------------

      subroutine amsu_asymmetry(landseatag, sat_id, node, ta, tb, theta)

!-----------------------------------------------------------------------------
!    Function:
!       An asymmetry correction is performed for AMSU-A low frequencies
!       channel measurements over ocean.
!
!    Reference:
!       Weng al at. 2000: Effects of AMSU cross-scan asymmetry of brightness
!          temperatures on retrieval of atmospheric and surface parameters. 
!          Microwave Radimetry and Remote Sening of the Earth's surface and
!          Atmosphere, 255-262.
!
!-----------------------------------------------------------------------------
!
!      Inputs
!
!       sat_id char*3 ---- Satellite, e.g, N15 (n15) or N16 (n16)
!       node   char*2 ---- (ascending = 'as', descending = 'ds') 
!       theta  real*4 ---- local zenith angle in degree
!        ta    real*4 ---- AMSU antenna temperatures (K)
!    landeatag int*1  ---- land sea tag (1 = land, 0 = ocean)
!
!      Outputs
!
!        tb    real*4 ---- Asymmetry corrected brightness temperatures (K)
!         
!----------------------------------------------------------------------------
      implicit none
      character sat_id*3,node*2
      integer*1 landseatag
      real*4 ta(6),tb(6),theta
      real*4 a23(6),a31(6),a50(6),a89(6)
      real*4 a23_as_n15(6), a31_as_n15(6), a50_as_n15(6), a89_as_n15(6)
      real*4 a23_ds_n15(6), a31_ds_n15(6), a50_ds_n15(6), a89_ds_n15(6)
      real*4 a23_as_n16(6), a31_as_n16(6), a50_as_n16(6), a89_as_n16(6)
      real*4 a23_ds_n16(6), a31_ds_n16(6), a50_ds_n16(6), a89_ds_n16(6)

      data a23_as_n15/-3.94106,72.91610, 15.0711, 0.00003, 0.0295778, 0.000210/
      data a31_as_n15/-12.0020,75.04110,20.2908, 0.01286, 0.0171646, 0.001108/
      data a50_as_n15/13.21010,75.35760,35.1155,-1.32094,-0.0950990,-0.001078/
      data a89_as_n15/-3.30494,-9.55244,20.5939, 2.96786, 0.0011953,-0.000476/
      data a23_ds_n15/-2.66477,-57.16550,-0.5828, 0.00000, 0.0187898,-0.000240/
      data a31_ds_n15/-6.58248,68.16420,16.5054, 0.00130, 0.0070707, 0.000457/
      data a50_ds_n15/18.93700,86.15060,40.3028,-1.92800,-0.1108850,-0.001418/
      data a89_ds_n15/2.550630,33.43680,13.5250,-0.12007, 0.0191702, 0.000019/
      data a23_as_n16/-1.99975,61.35390,5.10721, 0.00000, 0.0098383,-0.000019/
      data a31_as_n16/-5.48215,71.60910,13.9380, 0.00001,-0.0109019, 0.000189/
      data a50_as_n16/14.97760,92.20820,44.5338,-1.75598,-0.0768734,-0.000735/
      data a89_as_n16/-14.2038,-7.28960,41.8596,13.99010,-0.0189460,-0.002202/
      data a23_ds_n16/-1.51766,57.65410, 4.77877,0.00000,-0.0043410, 0.000040/
      data a31_ds_n16/-3.79544,65.49440,10.5331, 0.00000,-0.0203534, 0.000060/
      data a50_ds_n16/11.14810,74.29960,36.5321,-1.40925,-0.0726450,-0.000740/
      data a89_ds_n16/52.42600,-99.10280,44.1729,-4.23217, 0.2939530,-0.003514/

      if(landseatag.eq.0) then 
         if(sat_id .eq. 'n15'.or.sat_id .eq. 'N15') then
            if(node .eq. 'as') then 
	       do i = 1, 6 
		  a23(i) = a23_as_n15(i)
		  a31(i) = a31_as_n15(i)
		  a50(i) = a50_as_n15(i)
		  a89(i) = a89_as_n15(i)
               enddo
            else 
 	       do i = 1, 6 
		  a23(i) = a23_ds_n15(i)
		  a31(i) = a31_ds_n15(i)
		  a50(i) = a50_ds_n15(i)
		  a89(i) = a89_ds_n15(i)
              enddo
            endif
         else if(sat_id .eq. 'n16'.or.sat_id .eq. 'N16') then 
            if(node .eq. 'as') then 
	       do i = 1, 6 
         	  a23(i) = a23_as_n16(i)
		  a31(i) = a31_as_n16(i)
		  a50(i) = a50_as_n16(i)
		  a89(i) = a89_as_n16(i)
               enddo
            else 
 	       do i = 1, 6 
		  a23(i) = a23_ds_n16(i)
		  a31(i) = a31_ds_n16(i)
		  a50(i) = a50_ds_n16(i)
		  a89(i) = a89_ds_n16(i)
               enddo
            endif
         else
            print*, 'Invalide satellite ID.'
            stop
         endif
      
         dtb23 = a23(1)*exp(-.5*((theta-a23(2))/a23(3))**2) + & 
                     a23(4) + (a23(5) + a23(6)*theta)*theta
         dtb31 = a31(1)*exp(-.5*((theta-a31(2))/a31(3))**2) + &
                     a31(4) + (a31(5) + a31(6)*theta)*theta
         dtb50 = a50(1)*exp(-.5*((theta-a50(2))/a50(3))**2) + &
                     a50(4) + (a50(5) + a50(6)*theta)*theta
         dtb89 = a89(1)*exp(-.5*((theta-a89(2))/a89(3))**2) + &
                     a89(4) + (a89(5) + a89(6)*theta)*theta
         tb(1)=  ta(1) + dtb23
         tb(2) = ta(2) + dtb31
         tb(3) = ta(3) + dtb50
         tb(4) = ta(4)
         tb(5) = ta(5)
         tb(6) = ta(6) 
      else
       do ic = 1, 6
          tb(ic) = ta(ic)
       enddo 
      endif

      return
      END

!-------------------------------------------------------------------------
      subroutine vapor_liquid(landseatag, tb, wind, ts, tl, & 
                             theta, v_val, l_val)

!------------------------------------------------------------------------
!    Function:
!       Retrieving cloud liquid water and total precipitable water from
!       AMSU-A measurements at 23 and 31 GHzs over oceans.
!
!    Reference:
!       Weng and Zhao, 2001: AMSU cloud and precipitation algorithm. 
!            Submitted to Radio Science.
!
!-------------------------------------------------------------------------
!
!      Inputs
!       tb     real*4 ------ AMSU corrected brightness temperatures (K)         
!       wind   real*4 ------ Surface wind (m/s)
!       ts     real*4 ------ Surface temperature (K)
!       tl     real*4 ------ Cloud layer temperature (K) 
!       theta  real*4 ------ local zenith angle in degree
!        ta    real*4 ------ AMSU antenna temperatures (K)
!    landeatag int*1  ------ land sea tag (1 = land, 0 = ocean)
!
!      Outputs
!        v_val  real*4 ----- AMSU total precipitable water (mm)
!        l_val  real*4 ----- AMSU cloud liquid water path (mm)
!
!---------------------------------------------------------------------
      implicit none
      parameter(salinity=35.5, satheight=833.4,earthrad=6374.0) 
      parameter(coeA=0.968, coeB=-1.878)
      real*4    kv23, kv31, kl23, kl31
      real*4    tb(6), wind, ts, tl, theta, theta_rad, v_val, l_val
      real*4    em23hv(2), em31hv(2), tauo23, tauo31 
      real*4    sinthetas, costhetas, mu, tb23, tb31 
      real*4    em23, em31, em89, a0, a1, a2, b0, b1, b2
      integer*1 landseatag
 
      kv23=4.80423e-3
      kv31=1.93241e-3

      pi=4.0*atan(1.0) 

      tb23 = tb(1) 
      tb31 = tb(2)
      theta_rad=theta*pi/180.0
      mu = cos(theta_rad) 
      if(landseatag .eq. 0.and. ts .gt. 271.0)then
         sinthetas = sin(theta_rad)* earthrad/(earthrad + satheight) 
         sinthetas = sinthetas**2 
         costhetas = 1.0 - sinthetas 
         call OceanEM(wind, abs(theta), ts, salinity, 23.8, em23hv) 
         em23 = costhetas*em23hv(2) + sinthetas*em23hv(1) 
         call OceanEM(wind, abs(theta), ts, salinity, 31.4, em31hv) 
         em31 = costhetas*em31hv(2) + sinthetas*em31hv(1) 

         tauo23 = 3.21410e-2 - 6.31860e-5*ts 
         tauo31 = 5.34214e-2 - 1.04835e-4*ts 

         kl23 = 1.18203e-1 - 3.48761e-3*tl + 5.01301e-5*tl*tl 
         kl31 = 1.98774e-1 - 5.45692e-3*tl + 7.18339e-5*tl*tl 

         b0 = .5*kl23/(kv23*kl31 - kv31*kl23) 
         b1 =  kl31/kl23 
         b2 = - 2.0*(tauo31 - b1*tauo23)/mu + (1.0 - b1)*log(ts) + & 
               log(1.0 - em31) - b1*log(1.0 - em23) 

         a0 = -.5*kv23/(kv23*kl31 - kv31*kl23) 
         a1 = kv31/kv23
         a2 = -2.0*(tauo31 -a1*tauo23)/mu + (1.0 - a1)*log(ts) + & 
              log(1.0 - em31) - a1*log(1.0 - em23) 

         v_val = coeA*(mu*b0*(log(ts - tb31) - b1*log(ts - tb23) - b2))+coeB 
         l_val = mu*a0*(log(ts - tb31) - a1*log(ts - tb23) - a2) 

         if (l_val .gt. 2.0) then
             l_val = 2.0 
             v_val = 70.0
         endif

         if(l_val.lt.0)l_val=0.0 
         if(v_val.lt.0)v_val=0.0 
      else
         l_val = 0.0
         v_val = 0.0       
      endif
      return 
      END
!-------------------------------------------------------------------------

      subroutine ice_water_path(landseatag, tb, ts, wind, v, l, &
                tl, theta, iwp_val, de_val )
!
!--------------------------------------------------------------------------
!    Function:
!       Retrieving cloud ice water path and ice particle effective diameters
!       from AMSU-B measurements at 89 and 150 GHz.
!
!    References:
!        Zhao and Weng (2002): Retrieval of ice cloud parameters using the
!           advanced microwave sounding unit(AMSU). J. App. Meteor.(In press).
!
!        Weng and Grody(2000): Retrieval of ice cloud parameters using a
!           microwave imaging radiometer. J. Atmos. Sci., 57, 1069-1081.
!
!--------------------------------------------------------------------------
!
!      Inputs
!       tb     real*4 ------ AMSU corrected brightness temperatures (K)
!       wind   real*4 ------ Surface wind (m/s)
!       ts     real*4 ------ Surface temperature (K)
!       tl     real*4 ------ Ice cloud layer temperature (C)
!       theta  real*4 ------ local zenith angle in degree
!        ta    real*4 ------ AMSU antenna temperatures (K)
!        v     real*4 ----- AMSU total precipitable water (mm)
!        l     real*4 ----- AMSU cloud liquid water path (mm)
!    landeatag int*1  ------ land sea tag (1 = land, 0 = ocean)
!
!      Outputs
!        iwp_val    real*4 ----- AMSU ice water path (kg/m^2)
!        de_val     real*4 ----- AMSU ice particle effective diameter (mm)
!
!-----------------------------------------------------------------------------
      implicit none
      parameter (bulk_volume_density = 920.0)
      integer*1  landseatag
      real*4     tb(6), ts, wind, v, l, tl, theta,theta_rad, iwp_val, de_val
      real*4     tb89, tb150, tb23, tb31, tb50, &
                 tb89_base, tb150_base, tbs(5) 
      real*4     omega89, omega150, omegan, ratio, &
                 dee, iwpp, ts_thrd, mu 
      tb23  = tb(1) 
      tb31  = tb(2)
      tb50  = tb(3) 
      tb89  = tb(4) 
      tb150 = tb(5) 
      tb176 = tb(6)
      pi = 4.0*atan(1.0)
      theta_rad=theta*pi/180.0
      mu = cos(theta_rad) 
      if(landseatag .eq. 0.0) then
         call tb_base_ocean(ts, wind, v, l, tl, theta, tb89_base, tb150_base)
         ts_thrd = 271 
      else 
         call tb_base_land(tb23, tb31, tb89_base, tb150_base) 
         ts_thrd = 269 
      endif	
      omega89 = (tb89_base - tb89)/tb89 
      omega150 = (tb150_base - tb150)/tb150 
      iwpp=0.0
      dee=0.0
      if(omega89.gt.0.01.and.omega150.gt.0.02.and.tb176.lt.265)then 

        ratio = omega89/omega150 

        if((ratio .gt. 0.15) .and. (ratio .le. 0.9)) then
            dee = -0.24843 + ratio*(3.86726 - 4.70782*ratio & 
                + 4.67150*ratio**2) 
            if(omega89.lt.0.15.or.omega150.lt.0.18)dee = 0.4
	    if(dee.le.2.0.and.dee.gt.0)then 
               omegan = exp(-1.74663 + 1.90711*log(dee) & 
                            -0.730292*(log(dee))**2)
            else if(dee.gt.2.0)then
               omegan = exp(-1.58571 + 1.52223*log(dee) &
                            -0.524367*log(dee)**2)
            endif
            if(omega89.lt.0.15.or.omega150.lt.0.18)omegan = 0.15
            if(omegan.gt.0) & 
              iwpp = omega89 * dee * 1.0e-3 * mu * bulk_volume_density/omegan 
            if(iwpp .gt. 2) iwpp = 2.0                     
            if(iwpp .lt. 0) iwpp = 0.0 
        endif
      endif
      iwp_val=iwpp
      de_val=dee		
	
      return             
      END
      
!------------------------------------------------------------------------------

      subroutine rain_rate(landseatag, tb, aclw, biwp, bde, brr)
!
!------------------------------------------------------------------------------
!    Function:
!       Retrieving surface rain rate from cloud ice water path and ice particle
!       effective diameters derived from AMSU-B measurements at 89 and 150 GHz.
!
!    References:
!        Zhao al at. (2001): A physically_based algorithm to derive surface
!           rainfall rate using AMSU-B measurements. 11th Conf. on satellite
!           meteorology and oceangrahy, 371-374.
!-----------------------------------------------------------------------------
!
!      Inputs
!       tb     real*4 ------ AMSU corrected brightness temperatures (K)
!       aclw   real*4 ------ AMSU cloud liquid water path (mm)
!       biwp   real*4 ------ AMSU ice water path (kg/m^2)
!       bde    real*4 ------ AMSU ice particle effective diameter (mm)
!    landeatag int*1  ------ land sea tag (1 = land, 0 = ocean)
!
!      Outputs
!       brr    real*4 ------ AMSU rainfall rate (mm)
!        
!---------------------------------------------------------------------
      implicit none
      integer*1   landseatag
      real*4      tb(6)
      real*4      b89, b150, b176; 
      real*4      aclw, biwp, bde, brr; 

      data A0/0.321717/
      data A1/16.5043/
      data A2/-3.3419/

      if (biwp .lt. 0) then
          brr = 0.0  
      else
          b89 = tb(4) 
          b150 = tb(5)
          b176 = tb(6)
          if(landseatag.eq.0) then
             if(aclw.gt. 0.2.and.biwp.ge.0.05.and.bde.gt.0.4)then 
	        brr = A0 + A1 * biwp + A2 * biwp**2
             else
                brr = 0.0
             endif
          else 
             if(biwp.ge.0.05.and.bde.gt.0.4.and.b89-b150.gt.3)then
	        brr = A0 + A1 * biwp + A2 * biwp**2
             else
                brr = 0.0
             endif
          endif
      endif 
      if(brr .gt. 30.0) brr=30.0
      if(brr .lt. 0) brr=0.0
      return
      END
      
!-------------------------------------------------------------------------

      subroutine tb_base_land(tb23, tb31, tb89_base_val, tb150_base_val)
!
!--------------------------------------------------------------------------
!    Function:
!       Estmating brightness temperatures of ice clouds at 89 and 150 GHz
!       using AMSU-A measurements at 23 and 31 GHz.
!
!    References:
!        Zhao and Weng (2002): Retrieval of ice cloud parameters using the
!           advanced microwave sounding unit(AMSU). J. App. Meteor.(In press).
!
!-------------------------------------------------------------------------
!
!     Inputs
!       tb23     real*4 ------ AMSU 23 GHz measurement (K)
!       tb31     real*4 ------ AMSU 23 GHz measurement (K)
!
!     Outputs
!       tb89_base_val   real*4 ------ cloud base temperature at 89 GHz (K)
!       tb150_base_val  real*4 ------ cloud base temperature at 89 GHz (K)
!
!-------------------------------------------------------------------------
      implicit none
      real*4    tb23, tb31, tb89_base_val, tb150_base_val     

      tb89_base_val  = 17.88 + 1.61*tb23 - 0.67*tb31 
      tb150_base_val = 33.78 + 1.69*tb23 - 0.80*tb31 

      return
      END
      
!-------------------------------------------------------------------------

      subroutine tb_base_ocean( ts, wind, v, l, tl, theta, pred89, pred150)
!
!--------------------------------------------------------------------------
!    Function:
!       Estmating brightness temperatures of ice clouds at 89 and 150 GHz
!       using surface emissivity and AMSU-A TPW, CLW.
!
!    References:
!        Zhao and Weng (2002): Retrieval of ice cloud parameters using the
!           advanced microwave sounding unit(AMSU). J. App. Meteor.(In press).
!
!
!-------------------------------------------------------------------------
!
!     Inputs
!       tb     real*4 ------ AMSU corrected brightness temperatures (K)
!       wind   real*4 ------ Surface wind (m/s)
!       ts     real*4 ------ Surface temperature (K)
!       tl     real*4 ------ Cloud layer temperature (K)
!       theta  real*4 ------ local zenith angle in degree
!        v     real*4 ----- AMSU total precipitable water (mm)
!        l     real*4 ----- AMSU cloud liquid water path (mm)
!
!    landeatag int*1  ------ land sea tag (1 = land, 0 = ocean)
!
!     Outputs
!       tbs(5)   real*4 ------ cloud base temperatures (K)
!
!-------------------------------------------------------------------------
      implicit none
      parameter(salinity =  35.5, satheight = 833.4, earthrad = 6374.0)

      real*4     ts, wind, v, l, tl,  theta, theta_rad 
      real*4     emhv(2), ems89, ems150, sinthetas, costhetas, mu
      real*4     a0_89, a0_150, av_89, av_150
      real*4     al_89, bl_89, cl_89, al_150, bl_150, cl_150

      data  a0_89/0.108333/b0_89/-0.000221042/
      data  a0_150/0.030698/b0_150/-0.000071433/
      data  av_89/0.0115839/av_150/0.029519/ 
      data  al_89/1.0349e+0/al_150/1.7129e+0/
      data  bl_89/-9.7151e-3/bl_150/5.1329e-3/
      data  cl_89/-6.5914e-5/cl_150/-2.2475e-4/
      
      pi=4.0*atan(1.0)
      theta_rad=theta*pi/180.0
      mu = cos(theta_rad)
      sinthetas = sin(theta_rad)* earthrad/(earthrad + satheight) 
      sinthetas = sinthetas**2 
      costhetas = 1.0 - sinthetas 
      call 	OceanEM(wind, abs(theta), ts, salinity, 89.0, emhv) 
      ems89 = costhetas*emhv(2) + sinthetas*emhv(1) 
      call    OceanEM(wind, abs(theta), ts, salinity, 150.0, emhv)
      ems150 = costhetas*emhv(2) + sinthetas*emhv(1)
      print*, 'Check emis', wind, ems89, ems150
      tauo  =  a0_89 + b0_89 * ts
      tauv  =  av_89*v
      taul  = (al_89 + (bl_89 + cl_89 * tl)*tl)*l
      xi    =  exp(-(tauo  +  tauv  +  taul)/mu)         
      pred89 = ts*(1-(1-ems89)*xi*xi)

      tauo  =  a0_150 + b0_150 * ts
      tauv  =  av_150*v
      taul  = (al_150 + (bl_150 + cl_150 * tl)*tl)*l
      xi    =  exp(-(tauo  +  tauv  +  taul)/mu)
      pred150 = ts*(1-(1-ems150)*xi*xi)

 10   continue

      return
      END
!-------------------------------------------------------------------------

    subroutine OceanEM(wind,degre,t,s,frequency, emhv)
!
!-------------------------------------------------------------------------
!
!       References:
!
!       For sea water dielectric constant:
!
!       Klein and Swift, (1977) " emissivity for calm water"  IEEE Trans.
!       Antennas Propag., 25, 104-111
!
!       For foam reflectivity 
!
!       Stogryn, A. 1972, A study of radiometric emission from a rough sea 
!          surface, NASA contractor report NASA, CR-2088.
!      
!       For a rough surface without foam coverage
!
!       Holinger, J. P., 1971, Passive microwave measurements of sea surface
!          roughness,  IEEE Trans on Geosci. Elec., GE-9, 165-169.
!
!       For foam fraction model:
! 
!       Wilheit T. T., 1979, A model for the microwave emissvity of the 
!          ocean's surface as a function of wind speed, IEEE Trans and 
!          Goesci. Elect., GE-17, 244-249.
!-------------------------------------------------------------------------
!
!       Inputs
!
!         degre    :  incident angle (degree)
!         t        : temperature (K)
!         s        : sea water salinity (1/thousand)
!         frequency: (GHz)
!         wind     : wind speed (m/s)
!
!       Output variables
!     
!         rh       :  surface reflectance in horizontally polarized state for specular
!         rv       :
!         eh       :  surface emitance in .... for surface with and roughtness
!         ev       :
!
!       Internal variables 
!
!         foam     : foam fraction
!         g,tr     : emperical functions for wind induced 
!                    changes in reflection coefficient
!         f        : frequency in Hz
!
!-------------------------------------------------------------------
      implicit none
      real*4 wind, t, s, frequency, emhv(2) 
      complex mu, eps, aid1,aid2,aid3,cang,refwat
      
      mu = cmplx (1.0,0.0)

!     complex dielectric properties of saline water

      f = frequency*1.0e9
      t1 = t - 273.15
      call water_diel(t1, s, f, eps)
      
!     complex refractive index of saline water (not used)

      refwat = csqrt(eps)
      pi = 4.0*atan(1.0)
      cang = cmplx(degre*pi/180.0,0.)
      aid1 = csqrt(mu*eps-csin(cang)**2)
      aid2 = mu*ccos(cang)-aid1
      aid3 = mu*ccos(cang)+aid1
      rh = (cabs(aid2/aid3))**2
      aid2 = eps*ccos(cang)-aid1
      aid3 = eps*ccos(cang)+aid1 
      rv = (cabs(aid2/aid3))**2
      if(wind.lt.7.0) then
         foam=0.0
      else
         foam=0.006*(1.0-exp(-f*1.0e-9/7.5))*(wind-7.0)
      endif 

!     correction for wind induced foam free sea surface

      if(foam.lt.0.) foam=0. 
      if(foam.gt.1.) foam=1.  

!     emperical functions for wind induced reflection changes for hp

      g = 1.0 - 1.748e-3*degre - 7.336e-5*degre**2 + & 
         1.044e-7*degre**3
      tr = wind*(1.15e-1+3.8e-5*degre**2)*sqrt(f*1.0e-9)
      rfoam = 1.0-(208.0+1.29e-9*f)/t*g 
      rclear = rh - tr/t
      eh =1.0- (1.0-foam)*rclear-foam*rfoam

!     emperical functions for wind induced reflection changes for vp

      g  = 1.0 - 9.946e-4*degre + 3.218e-5*degre**2  & 
          -1.187e-6*degre**3+7.e-20*degre**10
      tr = wind*(1.17e-1-2.09e-3*exp(7.32e-2*degre)) & 
          *sqrt(f*1.0e-9)
      rfoam = 1.0-(208.0+1.29e-9*f)/t*g 
      rclear = rv - tr/t
      ev = 1.0-(1.0-foam)*rclear-foam*rfoam

      if(eh.gt.1.0) eh=1.0	 
      if(eh.lt.0.0) eh=0.0	 
      if(ev.gt.1.0) ev=1.0	 
      if(ev.lt.0.0) ev=0.0	 
      emhv(1)=eh
      emhv(2)=ev

      return
      END
!----------------------------------------------------------------------------

    subroutine water_diel(t, s, f, esw)

!----------------------------------------------------------------------------
!   Calculate the DIELECTRIC CONSTANT OR SALINE WATER
!
!     Klein and Swift emissivity for calm water (1977) IEEE Trans.
!     Antennas Propag., 25, 104-111
!
!     UNITS:
!     TEMPERATURE (T) IS IN DEGREES CELSIUS
!     SALINITY (S) PARTS PER THOUSAND
!     FREQUENCY (F) Hz
!----------------------------------------------------------------------------
      implicit none
      complex esw

      ESWI = 4.9

      ESWO = 87.134-1.949E-1*T-1.276E-2*T*T+2.491E-4*T**3
      A = 1.0+1.613E-5*T*S-3.656E-3*S+3.210E-5*S*S-4.232E-7*S**3
      ESW = ESWO*A

      TSWO = 1.1109E-10-3.824E-12*T+6.938E-14*T**2-5.096E-16*T**3
      B = 1.0+2.282E-5*T*S-7.638E-4*S-7.760E-6*S**2+1.105E-8*S**3
      TSW = TSWO*B

      EPSP = ESWI +(ESW-ESWI)/(1.0+(F*TSW)**2)

      EO = 8.854E-12
      PI = 4.0 * ATAN(1.0)
      ESWO = 87.134-1.949E-1*T-1.276E-2*T*T+2.491E-4*T**3
      A = 1.0+1.613E-5*T*S-3.656E-3*S+3.210E-5*S*S-4.232E-7*S**3
      ESW = ESWO*A
      TSWO = 1.1109E-10-3.824E-12*T+6.938E-14*T**2-5.096E-16*T**3
      B = 1.0+2.282E-5*T*S-7.638E-4*S-7.760E-6*S**2+1.105E-8*S**3
      TSW = TSWO*B
      SSWO = S*(0.18252-1.4619E-3*S+2.093E-5*S**2-1.282E-7*S**3)
      D = 25.0-T
      FI = D*(2.033E-2 + 1.266E-4*D + 2.464E-6*D**2 -  &  
            S*(1.849E-5-2.551E-7*D+2.551E-8*D*D))
      SSW = SSWO*EXP(-FI)
      EPSPP = TSW*F*(ESW-ESWI)/(1.0+(TSW*F)**2)
      EPSPP = EPSPP + SSW/(2.0*PI*EO*F)
    
      esw = cmplx(EPSP, -EPSPP)

      return
      END

!----------------------------------------------------------------------------

      subroutine benchmark_data(sat_id, node, ta, theta, ts, wind, &
                                 landseatag,snowflag, siceflag)

!----------------------------------------------------------------------------
!     OUTPUTS 
!
!       sat_id char*3 ------ Satellite, e.g, N15 (n15) or N16 (n16)
!       node   char*2 ------ (ascending = 'as', descending = 'ds')
!       ta     real*4 ------ AMSU antenna temp.
!      theta   real*4 ------ AMSU local zenith angle in radiance
!       Wind   real*4 ------ surface wind (m/s)
!        Ts    real*4 ------ surface temperature (K)
!    landeatag int*1  ------ land sea tag (1 = land, 0 = ocean)
!
!--------------------------------------------------------------------------
      implicit none
      character sat_id*3,node*2
      integer*1 landseatag
      real*4	ta(6), theta, wind, ts
      write(9,*)'Input data profile:'
      read(5,'(A)')sat_id
      write(9,*)'Satellite ID:  ',sat_id
      read(5,'(A)')node
      write(9,*)'Orbital mode:  ', node
      read(5,*)landseatag
      write(9,*)'Surface type:  ', landseatag
      read(5,*)snowflag
      write(9,*)'Snow Flag:  ', snowflag
      read(5,*)siceflag
      write(9,*)'Sea ice Flag:  ', siceflag
      read(5,*)ta(1), ta(2), ta(3), ta(4), ta(5), ta(6)
      write(9,*)'AMSU antenna temperatures (23, 31, 50, 89, 150 and 176 GHz):' 
      write(9,*)ta
      ta(1)=ta(1)
      ta(2)=ta(2)
      read(5,*)theta
      write(9,*)'Local zenith angle (degree):', theta
      read(5,*)ts
      write(9,*)'Surface temperature (K): ', ts
      read(5,*)wind
      write(9,*)'Surface Wind Speed (m/s): ', wind
      return 
      END
!-------------------------------------------------------------------------------
!
!      A simple readme file
!      
!      This program can be used to generate cloud and precipitation products
!      at each (lat, lon) location where AMSU measurements are available. For
!      testing the code on your mechine, you can use the data from benchmark 
!      cases. The example of the input data file is as follows:
!       
!
!      N15
!      as
!      1
!      0
!      0
!      273.0 271.0 258.0 194.0 167.0 161.0
!      33.0
!      293.0
!
!      A log file that contains both the input profile and retrieved results
!      is generated. The log file is 'Rev.log'.
!-------------------------------------------------------------------------------
