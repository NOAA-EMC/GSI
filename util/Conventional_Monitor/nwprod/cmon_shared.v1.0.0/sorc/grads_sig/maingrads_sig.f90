!-----------------------------------------------------------------------------
!  maingrads_sig
!
!    This program reads the conventional data and converts it into a GrADS
!    data file.  The data is from a vertical profile having multiple levels.
!
!-----------------------------------------------------------------------------

   implicit none

   real(4),dimension(46) :: psig 
   character(10) :: fileo,stype 
   character(3) :: intype
   character(2) :: subtype
   integer nreal,nreal_m2,iscater,igrads,isubtype 
   integer n_alllev,n_acft,n_lowlev,n_upair,nobs,lstype
   integer itype,n_sig

   namelist /input/intype,stype,itype,nreal,iscater,igrads,subtype,isubtype

   data n_sig / 46 /
!----------------------------------------------------------------
!  the psig levels are used to match the level in the pgb files
   data psig /997.,992.,985.,978.,970.,960.,950.,938.,&
             925.,911.,895.,877.,850.,837.,814.,789.,762.,&
             733.,700.,671.,638.,600.,570.,534.,500.,463.,&
             428.,400.,361.,329.,300.,271.,250.,219.,200.,& 
             175.,156.,138.,122.,100.,95.,83.,73.,64.,55.,48. /

   read(5,input)
   write(6,*)' User input: '
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
   nreal_m2 = nreal -2
   call grads_sig(stype,lstype,nobs,nreal_m2,n_sig,psig,iscater,igrads,isubtype,subtype) 

   stop
end
