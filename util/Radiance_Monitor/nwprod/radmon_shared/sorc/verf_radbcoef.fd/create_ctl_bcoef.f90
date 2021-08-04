subroutine create_ctl_bcoef(ntype,ftype,n_chan,iyy,imm,idd,ihh,idhh,&
     incr,ctl_file,lunctl,rmiss,satname,satype,dplat,&
     nu_chan,use,ratio,frequency,wavenumbr,little_endian)

  implicit none

  integer, intent(in)                        :: ntype,n_chan,iyy,imm,idd,ihh,idhh
  character(10),dimension(ntype),intent(in)  :: ftype
  integer, intent(in)                        :: lunctl,incr,little_endian
  character(40),intent(in)                   :: ctl_file
  real, intent(in)                           :: rmiss
  character(20),intent(in)                   :: satname
  character(10),intent(in)                   :: satype,dplat
  integer,dimension(n_chan),intent(in)       :: nu_chan
  real,dimension(n_chan),intent(in)          :: ratio,use,frequency,wavenumbr


  character(3),dimension(12):: mon
  character(3):: clatmin,clatmax
  character(4):: clonmin,clonmax
  character(13) stringd
  character(40) grad_file
  character(80) string

  integer iuse,j,i
  integer iyy2,imm2,idd2,ihh2,ntime
  integer,dimension(8):: ida,jda

  real wavelength
  real,dimension(5):: fha

  data stringd / '.%y4%m2%d2%h2' /
  data mon / 'jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', &
       'aug', 'sep', 'oct', 'nov', 'dec' /

!**************************************************************************
! Create date for tdef based on given date and hour offset
  fha=0.0; ida=0; jda=0; ntime=0
  iyy2=iyy; imm2=imm; idd2=idd; ihh2=ihh
  if (idhh/=0) then
     ntime = abs(idhh)/incr
     fha(2)=idhh
     ida(1)=iyy
     ida(2)=imm
     ida(3)=idd
     ida(4)=0
     ida(5)=ihh
     call w3movdat(fha,ida,jda)
     iyy2=jda(1)
     imm2=jda(2)
     idd2=jda(3)
     ihh2=jda(5)
  endif
  ntime=ntime+1

! Open unit to GrADS control file
  open(lunctl,file=ctl_file,form='formatted')


! Write header information
  grad_file = trim(satname) // stringd // '.ieee_d'
  write(lunctl,100) grad_file
  if ( little_endian == 1 ) then
     write(lunctl,112) 
  else
     write(lunctl,110) 
  endif
  write(lunctl,120) rmiss
  write(lunctl,130) adjustl(satype),dplat,n_chan
  write(lunctl,132)
  do i=1,n_chan
     iuse = nint(use(i))
     wavelength = 10000./wavenumbr(i)
     write(lunctl,134) i,nu_chan(i),iuse,ratio(i),wavelength,frequency(i)
  end do

  write(lunctl,210)
  write(lunctl,211)
  write(lunctl,212)
  write(lunctl,213)
  write(lunctl,214)
  write(lunctl,215)
  write(lunctl,216)
  write(lunctl,217)
  write(lunctl,218)
  write(lunctl,219)
  write(lunctl,220)
  write(lunctl,221)
  write(lunctl,222)
  write(lunctl,223)
  write(lunctl,224)
  write(lunctl,210)

  write(lunctl,140) n_chan
  write(lunctl,150) 
  write(lunctl,160) 
  write(lunctl,170) ntime,ihh2,idd2,mon(imm2),iyy2,incr
  write(lunctl,180) ntype

100 format('dset ^',a40)
110 format('options template big_endian sequential')
112 format('options template little_endian sequential')
120 format('undef ',f5.0)
130 format('title ',a10,1x,a10,1x,i4)
132 format('*XDEF is channel number')
134 format('*  x= ',i4,', channel= ',i4,' , iuse= ',i2,' , ratio= ',f8.3,&
         ' , wlth= ',f9.2,' , freq= ',f9.2)
136 format('* ',a70)
140 format('xdef ',i4,' linear 1.0 1.0')
150 format('ydef 1 linear 1.0 1.0')
160 format('zdef 1 linear 1.0 1.0')
170 format('tdef ',i4,' linear ',i2.2,'Z',i2.2,a3,i4.4,' ',i2.2'hr')
180 format('vars ',i7)

210 format('*')
211 format('* Explanation of data terms:')
212 format('*   penalty    penalty value')
213 format('*   mean       global offset (mean)')
214 format('*   atmpath    not used when adp_anglebc=.true. and newpc4pred=.true.')
215 format('*   clw        cloud liquid water term')
216 format('*   lapse2     temperature lapse rate**2')
217 format('*   lapse      temperature lapse rate')
218 format('*   cos_ssmis  cosine term for SSMIS')
219 format('*   sin_ssmis  sine term for SSMIS')
220 format('*   emiss      emissivity sensitivity term')
221 format('*   4ordang    4th order angle term')
222 format('*   3ordang    3rd order angle term')
223 format('*   2ordang    2nd order angle term')
224 format('*   1ordang    1st order angle term')


! Write data portion of GraDS control file  
  do i=1,ntype
     string = trim(ftype(i))
     write(lunctl,190) adjustl(string),trim(ftype(i))
190  format(a10,' 0 0 ',a10)
  end do

! Write trminating "endvars" record
  write(lunctl,200) 
200 format('endvars')


! Close ctl file
  close(lunctl)

! Return
  return
end subroutine create_ctl_bcoef
