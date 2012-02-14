module scaninfo
  implicit none
  integer msat,mstep
  parameter (msat=1000,mstep=100)

  integer,dimension(msat):: nstep
  real,dimension(msat):: start,step
contains
  subroutine initscan

! Set starting angle, stepsize, and number of scan positions for each satellite

    start(58)  = 0.0;      step(58)  = 1.0;        nstep(58) =90  ! goes 8 sounder
    start(60)  = 0.0;      step(60)  = 1.0;        nstep(60) =90  ! goes 10 sounder
    start(62)  = 0.0;      step(62)  = 1.0;        nstep(62) =90  ! goes 12 sounder
    start(258) = 0.0;      step(258) = 1.0;        nstep(258)=90  ! goes 8 imager
    start(260) = 0.0;      step(260) = 1.0;        nstep(260)=90  ! goes 10 imager
    start(262) = 0.0;      step(262) = 1.0;        nstep(262)=90  ! goes 12 imager
    
    start(14)  = -49.5;    step(14)  = 1.8;        nstep(14) =56  ! noaa-14 hirs2
    start(15)  = -49.5;    step(15)  = 1.8;        nstep(15) =56  ! noaa-15 hirs3
    start(16)  = -49.5;    step(16)  = 1.8;        nstep(16) =56  ! noaa-16 hirs3
    start(17)  = -49.5;    step(17)  = 1.8;        nstep(17) =56  ! noaa-17 hirs3
    start(18)  = -49.5;    step(18)  = 1.8;        nstep(18) =56  ! noaa-18 hirs4
    
    start(214) = -47.37;   step(214) = 9.474;      nstep(214)=11  ! noaa-14 msu
    
    start(315) = -48.33;   step(315) = 3. + 1./3.; nstep(315)=30  ! noaa-15 amsua
    start(316) = -48.33;   step(316) = 3. + 1./3.; nstep(316)=30  ! noaa-16 amsua
    start(317) = -48.33;   step(317) = 3. + 1./3.; nstep(317)=30  ! noaa-17 amsua
    start(318) = -48.33;   step(318) = 3. + 1./3.; nstep(318)=30  ! noaa-18 amsua
    
    start(415) = -48.95;   step(415) = 1.1;        nstep(415)=90  ! noaa-15 amsub
    start(416) = -48.95;   step(416) = 1.1;        nstep(416)=90  ! noaa-16 amsub
    start(417) = -48.95;   step(417) = 1.1;        nstep(417)=90  ! noaa-17 amsub
    start(418) = -48.95;   step(418) = 1.1;        nstep(418)=90  ! noaa-18 mhs 
    
    start(49)  = -48.95;   step(49)  = 1.1;        nstep(49) =90  ! aqua airs
    start(349) = -48.33;   step(349) = 3. + 1./3.; nstep(349)=30  ! aqua eos_amsua
    start(449) = -48.95;   step(449) = 1.1;        nstep(449)=90  ! aqua hsb
    return
  end subroutine initscan
end module scaninfo
