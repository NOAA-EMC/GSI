!  the program is a driver to read the data and convert into grads format, 
!  the data type is profile type which has multilevel 

   implicit none

   real(4),dimension(13) :: pmand 
   character(10) :: fileo,stype 
   character(3) :: intype
   character(2) :: subtype
   integer nreal,nreal_m2,iscater,igrads,isubtype,itype
   integer n_alllev,n_acft,n_lowlev,n_upair,nobs,lstype


   namelist /input/intype,stype,itype,nreal,iscater,igrads,subtype,isubtype
   integer n_mand

   data n_mand / 13 /
   data pmand /1000.,925.,850.,700.,500.,400.,300.,250.,200.,150.,100.,&
              70.,50./



   read(5,input)
   write(6,*)' User input below'
   write(6,input)

   print *, 'intype   = ', intype
   print *, 'stype    = ', stype
   print *, 'itype    = ', itype
   print *, 'nreal    = ', nreal
   print *, 'iscater  = ', iscater
   print *, 'igrads   = ', igrads 
   print *, 'subtype  = ', subtype
   print *, 'isubtype = ', isubtype

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
 
   call grads_mandlev(stype,lstype,nobs,nreal_m2,n_mand,pmand,iscater,igrads,isubtype,subtype) 

    stop
    end
