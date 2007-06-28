program readsig
  use sigio_module
  implicit none
  integer narg,iargc
  integer(sigio_intkind),parameter:: lusig=11,luggg=51,luctl=52
  integer(sigio_intkind):: irets
  character(255) cfsig,cfggg,cfctl,cidrt,cimax,cjmax
  integer ncfsig,ncfggg,ncfctl,ncidrt,ncimax,ncjmax,k
  integer iret,idrt,imax,jmax,nsig,n
  type(sigio_head):: head
  type(sigio_data):: data

  narg=iargc()
  if(narg.lt.1) then
     if(narg.ne.0) call errmsg('read_sig: 1 argument required')
     call errexit(1)
  endif

  call getarg(narg,cfsig)
  ncfsig=len_trim(cfsig)
  call sigio_srohdc(lusig,cfsig(1:ncfsig),head,data,irets)
  if(irets.ne.0) then
     call errmsg('ss2gg: error opening file '//cfsig(1:ncfsig))
     call errexit(2)
  endif

  write(6,*)'read file:  ',cfsig(1:ncfsig)
  write(6,*)'clabsig:  ',head%clabsig
  write(6,*)'fhour  :  ',head%fhour
  write(6,*)'idate  :  ',head%idate
  write(6,*)'jcap   :  ',head%jcap
  write(6,*)'levs   :  ',head%levs
  write(6,*)'itrun  :  ',head%itrun
  write(6,*)'iorder :  ',head%iorder
  write(6,*)'irealf :  ',head%irealf
  write(6,*)'igen   :  ',head%igen
  write(6,*)'latf   :  ',head%latf
  write(6,*)'lonf   :  ',head%lonf
  write(6,*)'latb   :  ',head%latb
  write(6,*)'lonb   :  ',head%lonb
  write(6,*)'latr   :  ',head%latr
  write(6,*)'lonr   :  ',head%lonr
  write(6,*)'ntrac  :  ',head%ntrac
  write(6,*)'icen2  :  ',head%icen2
  write(6,*)'iens   :  ',head%iens
  write(6,*)'idpp   :  ',head%idpp
  write(6,*)'idsl   :  ',head%idsl
  write(6,*)'idvc   :  ',head%idvc
  write(6,*)'idvm   :  ',head%idvm
  write(6,*)'idvt   :  ',head%idvt
  write(6,*)'idrun  :  ',head%idrun
  write(6,*)'idusr  :  ',head%idusr
  write(6,*)'pdryini:  ',head%pdryini
  write(6,*)'ncldt  :  ',head%ncldt
  write(6,*)'ixgr   :  ',head%ixgr
  write(6,*)'ivs    :  ',head%ivs
  write(6,*)'nxgr   :  ',head%nxgr
  write(6,*)'nxss   :  ',head%nxss
  write(6,*)'nhead  :  ',head%nhead
  write(6,*)'ndata  :  ',head%ndata
  write(6,*)'lhead  :  ',head%lhead
  write(6,*)'ldata  :  ',head%ldata

  do k=1,100
     write(6,*)'k,si,sl,ak,bk=',k,head%si(k),head%sl(k),head%ak(k),head%bk(k)
  end do
  k=101
  write(6,*)'k,si,ak,bk=',k,head%si(k),head%ak(k),head%bk(k)

  do k=1,head%levs+1
     write(6,*)'k,vcoord=',k,(head%vcoord(k,n),n=1,head%nvcoord)
  end do

  do k=1,head%ntrac+5
     write(6,*)'k,cfvars=',k,head%cfvars(k)
  end do


!     clabsig           Character(sigio_lhead1) ON85 label
!                       (obsolescent)
!     fhour             Real(sigio_realkind) forecast hour
!     idate             Integer(sigio_intkind)(4) initial date
!                       (hour, month, day, 4-digit year)
!     si                Real(sigio_realkind)(101) sigma interfaces
!                       (obsolescent)
!     sl                Real(sigio_realkind)(100) sigma levels
!                       (obsolescent)
!     ak                Real(sigio_realkind)(101) hybrid interface a
!                       (obsolescent)
!     bk                Real(sigio_realkind)(101) hybrid interface b
!                       (obsolescent)
!     jcap              Integer(sigio_intkind) spectral truncation
!     levs              Integer(sigio_intkind) number of levels
!     itrun             Integer(sigio_intkind) truncation flag
!                       (=1 for triangular)
!     iorder            Integer(sigio_intkind) coefficient order flag
!                       (=2 for ibm order)
!     irealf            Integer(sigio_intkind) floating point flag
!                       (=1 for 4-byte ieee, =2 for 8-byte ieee)
!     igen              Integer(sigio_intkind) model generating flag
!     latf              Integer(sigio_intkind) latitudes in dynamics
!                       (=(jcap+1)*3/2)
!     lonf              Integer(sigio_intkind) longitudes in dynamics
!                       (>=(jcap+1)*3 appropriate for fft)
!     latb              Integer(sigio_intkind) latitudes in physics
!     lonb              Integer(sigio_intkind) longitudes in physics
!     latr              Integer(sigio_intkind) latitudes in radiation
!     lonr              Integer(sigio_intkind) longitudes in radiation
!     ntrac             Integer(sigio_intkind) number of tracers
!     icen2             Integer(sigio_intkind) subcenter id
!     iens              Integer(sigio_intkind)(2) ensemble ids
!     idpp              Integer(sigio_intkind) processing id
!     idsl              Integer(sigio_intkind) semi-lagrangian id
!     idvc              Integer(sigio_intkind) vertical coordinate id
!                       (=1 for sigma, =2 for ec-hybrid, =3 for ncep hybrid)
!     idvm              Integer(sigio_intkind) mass variable id
!     idvt              Integer(sigio_intkind) tracer variable id
!     idrun             Integer(sigio_intkind) run id
!     idusr             Integer(sigio_intkind) user-defined id
!     pdryini           Real(sigio_realkind) global mean dry air pressure (kPa)
!                       (obsolescent)
!     ncldt             Integer(sigio_intkind) number of cloud types
!     ixgr              Integer(sigio_intkind) extra grid field id
!                       (=0 for none, =1 for zhao1, =2 for zhao2,
!                        =3 for ferrier)
!     ivs               Integer(sigio_intkind) version number
!     nvcoord           Integer(sigio_intkind) number of vcoord profiles
!  The following variables should be allocated with sigio_alhead:
!     vcoord            Real(sigio_realkind)((levs+1),nvcoord) vcoord profiles
!     cfvars            Character(8)(5+ntrac) field variable names
!  The following variables should not be modified by the user:
!     nxgr              Integer(sigio_intkind) number of extra grid fields
!     nxss              Integer(sigio_intkind) number of extra scalars
!     nhead             Integer(sigio_intkind) number of header records
!     ndata             Integer(sigio_intkind) number of data records
!     lhead             Integer(sigio_intkind)(nhead) header record lengths
!     ldata             Integer(sigio_intkind)(ndata) data record lengths



end program readsig
