!-----------------------------------------------------------------------------
!  maingrads_sfc
!
!    This program reads the conventional data and converts it into GrADS
!    data files.  The data is a profile type having multiple levels.
!
!-----------------------------------------------------------------------------

   implicit none

   real(4),dimension(21) :: pmand 
   character(10) :: fileo,stype 
   character(3) :: intype
   character(2) :: subtype
   integer nreal,nreal_m2,iscater,igrads,isubtype 
   integer n_alllev,n_acft,n_lowlev,n_upair,nobs,lstype
   integer n_mand,itype

   namelist /input/intype,stype,itype,nreal,iscater,igrads,subtype,isubtype

   data n_mand / 21 /
   data pmand /1000.,925.,850.,700.,500.,400.,300.,250.,200.,150.,100.,&
               70.,50.,30.,20.,10.,7.,5.,3.,2.,1./

   read(5,input)
   write(6,*)' User input below'
   write(6,input)

   lstype=len_trim(stype) 

   call read_conv2grads( intype,stype,itype,nreal,nobs,isubtype,subtype )
 
   !------------------------------------------------------------------------
   !  here's what's going on with nreal_m2:  
   !  
   !  The read_conv2grads routine reads all input fields from the intended
   !  obs (nreals) but only writes fields 3:nreal to the temporary file.
   !  So we need to send grads_lev nreal_m2 (minus 2). 
   !    
   nreal_m2 = nreal -2 
   call grads_sfc(stype,lstype,nobs,nreal_m2,iscater,igrads,isubtype,subtype) 

   stop
end
