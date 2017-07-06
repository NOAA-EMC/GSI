!  the program is a driver to read the data and convert into grads format, 
!  the data type is profile type which has multilevel 

   implicit none

   real(4),dimension(13) :: pmand 
   character(10) :: fileo,stype 
   character(3) :: intype
   character(2) :: subtype
   integer nreal,nreal2,iscater,igrads,isubtype,itype
   integer n_alllev,n_acft,n_lowlev,n_upair,nobs,lstype


   namelist /input/intype,stype,itype,nreal,nreal2,iscater,igrads,subtype,isubtype
   integer n_mand

  data n_mand / 13 /
  data pmand /1000.,925.,850.,700.,500.,400.,300.,250.,200.,150.,100.,&
              70.,50./



    read(5,input)
    write(6,*)' User input below'
    write(6,input)

    lstype=len_trim(stype) 

    call read_conv2grads(intype,stype,itype,nreal,nreal2,nobs,isubtype,subtype)
 
    call grads_mandlev(stype,lstype,nobs,nreal,nreal2,n_mand,pmand,iscater,igrads,isubtype,subtype) 

    stop
    end
