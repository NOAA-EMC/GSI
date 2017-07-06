!  the program is a driver to read the data and convert into grads format, 
!  the data type is profile type which has multilevel 
!  the hint is the vertical level thickness, for example, 925 presents
!  from 925-hint to 925+hint 
 
   implicit none

   integer itype
   real(4),dimension(3) :: plowlev 
   real(4),dimension(6) :: pacft 
   real(4),dimension(4) :: pupair
   real(4),dimension(10) :: palllev
   character(10) :: levcard,fileo,stype 
   character(3) :: intype
   character(2) :: subtype
   integer nreal,nreal2,iscater,igrads 
   integer n_alllev,n_acft,n_lowlev,n_upair,nobs,lstype,intv,isubtype
   real hint


   namelist /input/intype,stype,itype,nreal,nreal2,iscater,igrads,levcard,intv,subtype,isubtype
  data pacft /700.,600.,500.,400.,300.,100./
  data palllev /950.,850.,700.,600.,500.,400.,300.,250.,200.,100./
  data plowlev /950.,850.,700./
  data pupair /300.,250.,200.,100./
  data n_alllev / 10 /
  data n_acft / 6 /
  data n_lowlev / 3 /
  data n_upair / 4 /



    read(5,input)
    write(6,*)' User input below'
    write(6,input)

    lstype=len_trim(stype) 
    hint=real(intv)

    call read_conv2grads(intype,stype,itype,nreal,nreal2,nobs,isubtype,subtype)
 
    if(trim(levcard) == 'alllev' ) call grads_lev(stype,lstype,nobs,nreal,nreal2,n_alllev,&
                               palllev,iscater,igrads,levcard,hint,isubtype,subtype)
    if(trim(levcard) == 'acft' ) call grads_lev(stype,lstype,nobs,nreal,nreal2,n_acft,&
                               pacft,iscater,igrads,levcard,hint,isubtype,subtype)
    if(trim(levcard) == 'lowlev' ) call grads_lev(stype,lstype,nobs,nreal,nreal2,n_lowlev,&
                               plowlev,iscater,igrads,levcard,hint,isubtype,subtype)
    if(trim(levcard) == 'upair' ) call grads_lev(stype,lstype,nobs,nreal,nreal2,n_upair,&
                               pupair,iscater,igrads,levcard,hint,isubtype,subtype)
     

    stop
    end
