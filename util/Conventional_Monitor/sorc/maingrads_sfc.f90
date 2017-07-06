!  the program is a driver to read the data and convert into grads format, 
!  the data type is profile type which has multilevel 

   implicit none

   real(4),dimension(21) :: pmand 
   character(10) :: fileo,stype 
   character(3) :: intype
   character(2) :: subtype
   integer nreal,nreal2,iscater,igrads,isubtype 
   integer n_alllev,n_acft,n_lowlev,n_upair,nobs,lstype
   integer n_mand,itype

   namelist /input/intype,stype,itype,nreal,nreal2,iscater,igrads,subtype,isubtype

  data n_mand / 21 /
  data pmand /1000.,925.,850.,700.,500.,400.,300.,250.,200.,150.,100.,&
              70.,50.,30.,20.,10.,7.,5.,3.,2.,1./



    read(5,input)
    write(6,*)' User input below'
    write(6,input)

    lstype=len_trim(stype) 

    call read_conv2grads(intype,stype,itype,nreal,nreal2,nobs,isubtype,subtype)
 
    call grads_sfc(stype,lstype,nobs,nreal,nreal2,iscater,igrads,isubtype,subtype) 

    stop
    end
