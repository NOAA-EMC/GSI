!  the program is a driver to read the data and convert into grads format, 
!  the data type is profile type which has multilevel 

   implicit none

   real(4),dimension(46) :: psig 
   character(10) :: fileo,stype 
   character(3) :: intype
   character(2) :: subtype
   integer nreal,nreal2,iscater,igrads,isubtype 
   integer n_alllev,n_acft,n_lowlev,n_upair,nobs,lstype
   integer itype,n_sig

   namelist /input/intype,stype,itype,nreal,nreal2,iscater,igrads,subtype,isubtype

  data n_sig / 46 /
!  the level try to match the level in the pgb files
  data psig /997.,992.,985.,978.,970.,960.,950.,938.,&
             925.,911.,895.,877.,850.,837.,814.,789.,762.,&
             733.,700.,671.,638.,600.,570.,534.,500.,463.,&
             428.,400.,361.,329.,300.,271.,250.,219.,200.,& 
             175.,156.,138.,122.,100.,95.,83.,73.,64.,55.,48. /

    read(5,input)
    write(6,*)' User input below'
    write(6,input)

    lstype=len_trim(stype) 

    call read_conv2grads(intype,stype,itype,nreal,nreal2,nobs,isubtype,subtype)
 
    call grads_sig(stype,lstype,nobs,nreal,nreal2,n_sig,psig,iscater,igrads,isubtype,subtype) 

    stop
    end
