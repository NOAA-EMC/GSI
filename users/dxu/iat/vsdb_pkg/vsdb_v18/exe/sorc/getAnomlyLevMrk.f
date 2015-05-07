           subroutine getAnomlyLevMrk(k5,k6, anomly_mrk, plevel,
     +        numvarbl,numlevel,anomlylev)

           INCLUDE 'parm.inc'

           integer k5(mxvrbl),k6(mxvrbl), plevel(maxlvl), 
     +       anomlylev(mxvrbl,maxlvl), anomly_mrk(mxvrbl)
           integer numvarbl,numlevel

           !step 1 set anomlylev markers:
           do n = 1, numvarbl
                                                                                                                      
            if(anomly_mrk(n).eq.1) then
                                                                                                                      
             if(k5(n).eq.7.and.k6(n).eq.100) then    !Geographic Hight has anomly computation
              do ilv = 1, numlevel
               if(plevel(ilv).eq.1000.or.plevel(ilv).eq.500.or.
     +            plevel(ilv).eq.700.or.plevel(ilv).eq.250 )      !1000, 700 500, 250 mb has HGT climate anomly
     +          anomlylev(n,ilv)=1
              end do
             end if
                                                                                                                      
             if((k5(n).eq.11.and.k6(n).eq.100) .or.             !Temperature has anomly computation
     +          (k5(n).eq.32.and.k6(n).eq.100) .or.             !Wind speed has anomly computation
     +          (k5(n).eq.33.and.k6(n).eq.100) .or.             !U wind has anomly computation
     +          (k5(n).eq.34.and.k6(n).eq.100) ) then           !V wind has anomly computation
              do ilv = 1, numlevel
               if(plevel(ilv).eq.850.or.plevel(ilv).eq.500.or.
     +            plevel(ilv).eq.250 )                          !850 500, 250 mb has T, U, W climate anomly
     +          anomlylev(n,ilv)=1
              end do
             end if
                                                                                                                      
             if(k5(n).eq.2.and.k6(n).eq.102) anomlylev(n,1) = 1    !Sfc pressure has climate anomly
                                                                                                                      
             if((k5(n).eq.11.and.k6(n).eq.105) .or.             !2m T
     +          (k5(n).eq.15.and.k6(n).eq.105) .or.             !2m Tmax
     +          (k5(n).eq.16.and.k6(n).eq.105) .or.             !2m Tmin
     +          (k5(n).eq.32.and.k6(n).eq.105) .or.             !10m wind speed
     +          (k5(n).eq.33.and.k6(n).eq.105) .or.             !10m U-wind
     +          (k5(n).eq.34.and.k6(n).eq.105) )                !10m V-wind
     +        anomlylev(n,1) = 1
                                                                                                                      
            end if
           end do

           return    
           end

