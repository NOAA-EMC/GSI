c        real fcst, obsv, threshold
c        character *10 symbol
c        character*1 updown
c        real F, O, H
c        symbol = 'FHO>'
c        fcst = 2.20
c        obsv = 1.01
c        threshold = 1.5
c        updown='+'
c        F = getTND_FO(fcst,updown, threshold, symbol) 
c        O = getTND_FO(obsv,updown, threshold, symbol) 
c        H = getTND_Hit(fcst, obsv, updown, threshold, symbol)
c        write(*,*) fcst, obsv,  symbol,threshold, updown
c        write(*,*) "FHO=",F,O,H
c        stop
c        end 

c
c   This function is to get F(fcst <  or = or > threshold) and 
c   O (obsv < or = or > threshold) values for FHO vsdb record
c   Author: Binbin Zhou
c           Mar, 2005

 	function getFO(x,threshold,symbol)
        real getFO
        character(len=10), intent(in) :: symbol
        real,              intent(in) :: x, threshold

        getFO = 0.0
        if (trim(symbol).eq.'FHO>'.or.
     +      trim(symbol).eq.'AFHO>') then
          if ( x .ge. threshold) then
            getFO = 1.0
          end if
        else if(trim(symbol).eq.'FHO<'.or.
     +          trim(symbol).eq.'AFHO<') then
          if ( x .le. threshold) then
            getFO = 1.0
          end if
        else if(trim(symbol).eq.'FHO='.or.
     +          trim(symbol).eq.'AFHO=') then
          if ( x .eq. threshold) then
            getFO = 1.0
          end if
        end if

        return
        end
c
c   This function is to get H (hit: both fcst and obsv > or < threshold)  
c   value for FHO vsdb record
c   Author: Binbin Zhou
c           Mar, 2005

        function getHit(fcst, obsv, threshold,symbol)
        real getHit
        real, intent(in) :: fcst, obsv, threshold
        character(len=10), intent(in) :: symbol
     
        getHit = 0.0
        if (getFO(fcst, threshold, symbol).eq.1. 
     +  .and. getFO(obsv, threshold, symbol).eq.1.)
     +    getHit = 1.0

        return
        end
   
        function getTND_FO(x,updown,threshold,symbol)       
        real getTND_FO
        character(len=10), intent(in) :: symbol
        real,              intent(in) :: x, threshold
        character(len=1),  intent(in) :: updown        
                                                                                                                                              
        getTND_FO = 0.0
        if ((trim(symbol).eq.'FHO>'.or.trim(symbol).eq.'AFHO>') 
     +      .and. updown.eq.'-') then
          if ( x .le. -threshold) then
            getTND_FO = 1.0
          else
            getTND_FO = 0.0
          end if
        else if((trim(symbol).eq.'FHO>'.or.trim(symbol).eq.'AFHO>')
     +          .and. updown.eq.'+') then
          if ( x .ge. threshold) then
            getTND_FO = 1.0
          else
            getTND_FO = 0.0
          end if
        else if((trim(symbol).eq.'FHO<'.or.trim(symbol).eq.'AFHO<')
     +          .and. updown.eq.'-') then
          if ( x .ge. -threshold) then
            getTND_FO = 1.0
          else
            getTND_FO = 0.0
          end if
        else if((trim(symbol).eq.'FHO<'.or.trim(symbol).eq.'AFHO<')
     +          .and. updown.eq.'+') then
          if ( x .le. threshold) then
            getTND_FO = 1.0
          else
            getTND_FO = 0.0
          end if
        end if
                                                                                                                                                                
        return
        end
 
        function getTND_Hit(fcst, obsv, updown, threshold,symbol)
        real getHit
        real, intent(in) :: fcst, obsv, threshold
        character(len=10), intent(in) :: symbol
        character(len=1),  intent(in) :: updown                                                                                                                                                        
        getTND_Hit = 0.0
        if (getTND_FO(fcst, updown, threshold, symbol).eq.1.
     +  .and. getTND_FO(obsv, updown, threshold, symbol).eq.1.)
     +    getTND_Hit = 1.0
                                                                                                                                                                
        return
        end

