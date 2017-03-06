!-----------------------------------------------------------------------------
!  maingrads_sfctime
!
!    This program reads the conventional data and converts it into a GrADS
!    data file.  The data is a profile type having multiple levels.
!
!-----------------------------------------------------------------------------

   implicit none

   real(4),dimension(11) :: ptime11 
   real(4),dimension(7) :: ptime7
   character(10) :: fileo,stype,timecard 
   character(3) :: intype
   character(2) :: subtype
   integer nreal,nreal_m2,nreal_m4,iscater,igrads,isubtype 
   integer nobs,lstype
   integer n_time7,n_time11,itype

   namelist /input/intype,stype,itype,nreal,iscater,igrads,timecard,isubtype,subtype

   data n_time11 / 11 /
   data n_time7 / 7 /
   data ptime11 / -2.5,-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5 /
   data ptime7 / -3.0,-2.0,-1.0,0.0,1.0,2.0,3.0 /

   read(5,input)
   write(6,*)' User input below'
   write(6,input)

   lstype=len_trim(stype) 

   call read_conv2grads(intype,stype,itype,nreal,nobs,isubtype,subtype)

   !------------------------------------------------------------------------
   !  here's what's going on with nreal_m2:  
   !  
   !  The read_conv2grads routine reads all input fields from the intended
   !  obs (nreals) but only writes fields 3:nreal to the temporary file.
   !  So we need to send grads_lev nreal_m2 (minus 2). 
   !    
   !  NOTE:  grads_sfctime.f90 is more confused than the other programs.  The
   !  grads_sfctime.f90 file uses both nreal (now renamed as nreal_m2) and 
   !  nreal2 (now renamed nreal_m4).  Not sure it's using nreal_m4
   !  correctly, but for the moment I'm going to send in nreal_m4 for 
   !  nreal2 and assume it's all good.  Seems like it should really only use
   !  nreal-2 but what do I know?!
   !
   nreal_m2 = nreal -2
   nreal_m4 = nreal -4

   if( trim(timecard) == 'time11') then
      call grads_sfctime(stype,lstype,nobs,nreal_m2,nreal_m4,n_time11,ptime11,iscater,igrads,isubtype,subtype) 
   endif

   if( trim(timecard) == 'time7') then 
      call grads_sfctime(stype,lstype,nobs,nreal_m2,nreal_m4,n_time7,ptime7,iscater,igrads,isubtype,subtype) 
   endif

   stop
end
