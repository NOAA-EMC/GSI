!  the program is a driver to read the data and convert into grads format, 
!  the data type is profile type which has multilevel 

   implicit none

   real(4),dimension(11) :: ptime11 
   real(4),dimension(7) :: ptime7
   character(10) :: fileo,stype,timecard 
   character(3) :: intype
   character(2) :: subtype
   integer nreal,nreal2,iscater,igrads,isubtype 
   integer n_alllev,n_acft,n_lowlev,n_upair,nobs,lstype
   integer n_time7,n_time11,itype

   namelist /input/intype,stype,itype,nreal,nreal2,iscater,igrads,timecard,isubtype,subtype

  data n_time11 / 11 /
  data n_time7 / 7 /
  data ptime11 / -2.5,-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5 /
  data ptime7 / -3.0,-2.0,-1.0,0.0,1.0,2.0,3.0 /



    read(5,input)
    write(6,*)' User input below'
    write(6,input)

    lstype=len_trim(stype) 

    call read_conv2grads(intype,stype,itype,nreal,nreal2,nobs,isubtype,subtype)
    if( trim(timecard) == 'time11') & 
    call grads_sfctime(stype,lstype,nobs,nreal,nreal2,n_time11,ptime11,iscater,igrads,isubtype,subtype) 
    if( trim(timecard) == 'time7') & 
    call grads_sfctime(stype,lstype,nobs,nreal,nreal2,n_time7,ptime7,iscater,igrads,isubtype,subtype) 

    stop
    end
