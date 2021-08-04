subroutine create_ctl_time(ntype,ftype,n_chan,iyy,imm,idd,ihh,idhh,&
     incr,ctl_file,lunctl,rmiss,satname,satype,dplat,nregion,&
     region,rlonmin,rlonmax,rlatmin,rlatmax,nu_chan,use,error,&
     frequency,wavenumbr,little_endian)

  implicit none

  integer,intent(in)                          :: ntype
  character(10),dimension(ntype),intent(in)   :: ftype
  integer,intent(in)                          :: n_chan,lunctl,iyy,imm,idd,ihh,idhh,incr
  character(40),intent(in)                    :: ctl_file
  real,intent(in)                             :: rmiss
  character(20),intent(in)                    :: satname
  character(10),intent(in)                    :: satype,dplat
  integer,intent(in)                          :: nregion,little_endian
  character(40),dimension(nregion),intent(in) :: region
  real,dimension(nregion),intent(in)          :: rlonmin,rlonmax,rlatmin,rlatmax
  integer,dimension(n_chan),intent(in)        :: nu_chan
  real,dimension(n_chan),intent(in)           :: error,use,frequency,wavenumbr


  character(2) cword
  character(3),dimension(12):: mon
  character(3):: clatmin,clatmax
  character(4):: clonmin,clonmax
  character(13) stringd
  character(40) grad_file
  character(80) string
  character(80),dimension(nregion):: stringr

  integer iuse
  integer j,i
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

!*******************************************************************
!  Construct the region strings if this is for a global source,
!     which is defined as nregion > 1.
!
  if (nregion > 1) then
     do i=1,nregion
        if (rlatmin(i)>0.) then
           write(clatmin,10) int(rlatmin(i))
        else
           write(clatmin,20) abs(int(rlatmin(i)))
        endif
        if (rlatmax(i)>0.) then
           write(clatmax,10) int(rlatmax(i))
        else
           write(clatmax,20) abs(int(rlatmax(i)))
        endif
        if (rlonmin(i)>0.) then
           write(clonmin,30) int(rlonmin(i))
        else
           write(clonmin,40) abs(int(rlonmin(i)))
        endif
        if (rlonmax(i)>0.) then
           write(clonmax,30) int(rlonmax(i))
        else
           write(clonmax,40) abs(int(rlonmax(i)))
        endif
        stringr(i) = trim(region(i)) // ' (' // &
             trim(clonmin) // '-' // trim(clonmax) // ', ' // &
             trim(clatmin) // '-' // trim(clatmax) // ')'
     end do
  endif
10 format(i2,'N')
20 format(i2,'S')
30 format(i3,'E')
40 format(i3,'W')

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
     write(lunctl,134) i,nu_chan(i),iuse,error(i),wavelength,frequency(i)
  end do
  write(lunctl,136)

  if (nregion > 1) then
     do i=1,nregion
        write(cword,'(i2)') i
        string = '*  region=' // cword // ' ' // trim(stringr(i))
        write(lunctl,138) string
     end do
  endif

  write(lunctl,140) n_chan
  write(lunctl,150) nregion
  write(lunctl,160) 
  write(lunctl,170) ntime,ihh2,idd2,mon(imm2),iyy2,incr
  write(lunctl,180) ntype

100 format('dset ^',a40)
110 format('options template big_endian sequential')
112 format('options template little_endian sequential')
120 format('undef ',f5.0)
130 format('title ',a10,1x,a10,1x,i4)
132 format('*XDEF is channel number')
134 format('*  x= ',i4,', channel= ',i4,' , iuse= ',i2,' , error= ',f8.3,&
         ' , wlth= ',f9.2,' , freq= ',f9.2)
136 format('*YDEF is geographic region')
138 format(a80)
140 format('xdef ',i4,' linear 1.0 1.0')
150 format('ydef ',i2,' linear 1.0 1.0')
160 format('zdef 1 linear 1.0 1.0')
170 format('tdef ',i4,' linear ',i2.2,'Z',i2.2,a3,i4.4,' ',i2.2,'hr')
180 format('vars ',i7)

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
end subroutine create_ctl_time
