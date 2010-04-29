module gradsmod
!$$$   module documentation block
! This module contains routines and information useful for plotting grads maps
!   of the guess and analysis on the analysis grid.
!
! Subroutines Included:
!   sub init_grads - initialize internal variables prior to opening output
!                      files for grads output
!   sub open_grads - open output files and initialize for writing grads output
!   sub out_grads  - write next output grads record
!   sub close_grads- close grads output files
!
! Variable Definitions:
!   def make_maps - if = .true., then generate grads output of guess and
!                            analysis on analysis grid
!$$$ end documentation block

  implicit none

  logical make_maps
  character(80) label_dat(20)
  integer unit_dat(20)
  integer num_open

contains

  subroutine init_grads

    integer i

    make_maps=.false.
    do i=1,20
     label_dat(i)='xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
     unit_dat(i)=0
    end do
    num_open=0

  end subroutine init_grads

  subroutine open_grads(label,nlon,nlat,nsig)

    character(*) label
    integer nlon,nlat,nsig
    integer i,ithis,k,ntime
    real(4) undef
    character(1) blank
    character(80) datdes(29)
    integer unit_des
    character(80) label_des
    real(4) startx,starty,startp,xinc,yinc,pinc

    blank=' '
    undef=-9.99e33

    ithis=num_open+1

!    create names of grads control and data files

    write(label_des,'(a,".ctl")')trim(label)
    write(label_dat(ithis),'(a,".dat")')trim(label)

!    find unused unit number

    unit_des=60
    unit_dat(ithis)=60+ithis

!    initialize counters for this set of output fields

    startx=1. ; xinc=1.
    starty=1. ; yinc=1.
    startp=1. ; pinc=1.
    ntime=1
    do i=1,29
     write(datdes(i),'(80a1)')(blank,k=1,80)
    end do
    write(datdes(1),'("DSET ",a)')trim(label_dat(ithis))
    write(datdes(2),'("options big_endian sequential")')
    write(datdes(3),'("TITLE ",a)')trim(label)
    write(datdes(4),'("UNDEF ",e11.2)')undef
    write(datdes(5),'("XDEF ",i5," LINEAR ",f7.2,f7.2)')nlon,startx,xinc
    write(datdes(6),'("YDEF ",i5," LINEAR ",f7.2,f7.2)')nlat,starty,yinc
    write(datdes(7),'("ZDEF ",i5," LINEAR ",f7.2,f7.2)')nsig,startp,pinc
    write(datdes(8),'("TDEF ",i5," LINEAR 0Z23may1992 24hr")')ntime
    write(datdes(9),'("VARS 19")')
    write(datdes(10),'("mub      ",i5," 99 mub      ")')1
    write(datdes(11),'("mu       ",i5," 99 mu       ")')1
    write(datdes(12),'("phb      ",i5," 99 phb      ")')1
    write(datdes(13),'("t        ",i5," 99 t        ")')nsig
    write(datdes(14),'("qvapor   ",i5," 99 qvapor   ")')nsig
    write(datdes(15),'("u        ",i5," 99 u        ")')nsig
    write(datdes(16),'("v        ",i5," 99 v        ")')nsig
    write(datdes(17),'("landmask ",i5," 99 landmask ")')1
    write(datdes(18),'("xice     ",i5," 99 xice     ")')1
    write(datdes(19),'("sst      ",i5," 99 sst      ")')1
    write(datdes(20),'("vgtyp    ",i5," 99 vgtyp    ")')1
    write(datdes(21),'("sltyp    ",i5," 99 sltyp    ")')1
    write(datdes(22),'("vegfra   ",i5," 99 vegfra   ")')1
    write(datdes(23),'("snow     ",i5," 99 snow     ")')1
    write(datdes(24),'("u10      ",i5," 99 u10      ")')1
    write(datdes(25),'("v10      ",i5," 99 v10      ")')1
    write(datdes(26),'("smois    ",i5," 99 smois    ")')1
    write(datdes(27),'("tslb     ",i5," 99 tslb     ")')1
    write(datdes(28),'("tsk      ",i5," 99 tsk      ")')1
    write(datdes(29),'("ENDVARS")')

!   write out datdes

    open(unit_des,file=label_des,form='formatted')
    rewind unit_des
    write(unit_des,'(a80)')datdes
    close(unit_des)

!   open dat file

    open(unit_dat(ithis),file=label_dat(ithis),form='unformatted')
    rewind unit_dat(ithis)

!    advance num_open

    num_open=num_open+1

  end subroutine open_grads

  subroutine out_grads(tempa,label,nlon,nlat)

    integer nlon,nlat
    real(4) tempa(nlon,nlat)
    character(*) label
    character(80) label_test
    integer i,ithis


!  find output unit number

    write(label_test,'(a,".dat")')trim(label)
    ithis=0
    if(num_open.gt.0) then
     do i=1,num_open
      if(trim(label_dat(i)).eq.trim(label_test)) then
       ithis=i
       exit
      end if
     end do
    end if
    if(ithis.gt.0) write(unit_dat(ithis))tempa

  end subroutine out_grads

  subroutine close_grads(label)

    character(*) label
    character(80) label_test
    integer i,ithis

!   close dat file

    write(label_test,'(a,".dat")')trim(label)
    ithis=0
    if(num_open.gt.0) then
     do i=1,num_open
      if(trim(label_dat(i)).eq.trim(label_test)) then
       ithis=i
       exit
      end if
     end do
    end if
    if(ithis.gt.0) then
     close(unit_dat(ithis))
     label_dat(ithis)='xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
    end if

  end subroutine close_grads

end module gradsmod

program reg_gsi_grads

!   create grads files from regional mode input/output of unified gsi

use gradsmod
implicit none

integer regional_time(6)
integer nlon_regional,nlat_regional,nsig
real(4) pt
real(4),allocatable:: znu(:),znw(:),dx(:,:),dy(:,:),mapfac_m(:,:),xlat(:,:),xlong(:,:)
real(4) rdx,rdy
character(120) input_file,outname
real(4),allocatable::temp1(:,:),temp1u(:,:),temp1v(:,:),tempb(:,:),temp3(:,:,:)
integer,allocatable::itemp1(:,:)
integer im,jm,igtypeh,igtypeu,igtypev,i,j,k
integer iyear,imonth,iday,ihour,iminute,isecond
integer jyear,jmonth,jday,jhour,jminute,jsecond
integer nlon,nlat,nsoil

igtypeh=1
igtypeu=2
igtypev=3
call init_grads

!   get file name from input argument

call getarg(1,input_file)
call getarg(2,outname)

call retrieve_binary_mass_constants(input_file, &
                      iyear,imonth,iday,ihour,iminute,isecond, &
                      jyear,jmonth,jday,jhour,jminute,jsecond, &
                      nlon,nlat,nsig,nsoil,pt)
im=nlon
jm=nlat
allocate(znu(nsig),znw(nsig+1),dx(im,jm),dy(im,jm),mapfac_m(im,jm),xlat(im,jm),xlong(im,jm))
call retrieve_binary_mass_field('ZNU',znu,input_file,nsig)
call retrieve_binary_mass_field('ZNW',znw,input_file,nsig)
call retrieve_binary_mass_field('RDX',rdx,input_file,1)
call retrieve_binary_mass_field('RDY',rdy,input_file,1)
call retrieve_binary_mass_field('MAPFAC_M',mapfac_m,input_file,1)
call retrieve_binary_mass_field('XLAT',xlat,input_file,1)
call retrieve_binary_mass_field('XLONG',xlong,input_file,1)

do j=1,jm
 do i=1,im
  dx(i,j)=1./(mapfac_m(i,j)*rdx)
  dy(i,j)=1./(mapfac_m(i,j)*rdy)
 end do
end do

call open_grads(outname,im,jm,nsig)

allocate(temp1(im,jm),temp1u(im+1,jm),temp1v(im,jm+1),tempb(im,jm),itemp1(im,jm))

!                    mub
call retrieve_binary_mass_field('MUB',temp1,input_file,im*jm)
call fill_mass_grid2t(temp1,im,jm,tempb,igtypeh)
call out_grads(tempb,outname,im,jm)

!                    mu
call retrieve_binary_mass_field('MU',temp1,input_file,im*jm)
call fill_mass_grid2t(temp1,im,jm,tempb,igtypeh)
call out_grads(tempb,outname,im,jm)

!                    phb
allocate(temp3(im,nsig+1,jm))
call retrieve_binary_mass_field('PHB',temp3,input_file,im*jm*nsig+1)
do j=1,jm
 do i=1,im
  temp1(i,j)=temp3(i,1,j)
 end do
end do
call fill_mass_grid2t(temp1,im,jm,tempb,igtypeh)
call out_grads(tempb,outname,im,jm)

deallocate(temp3)
allocate(temp3(im,nsig,jm))
!                      T
call retrieve_binary_mass_field('T',temp3,input_file,im*jm*nsig)
do k=1,nsig
 do j=1,jm
  do i=1,im
   temp1(i,j)=temp3(i,k,j)
  end do
 end do
 call fill_mass_grid2t(temp1,im,jm,tempb,igtypeh)
 call out_grads(tempb,outname,im,jm)
end do

!                      qvapor
call retrieve_binary_mass_field('QVAPOR',temp3,input_file,im*jm*nsig)
do k=1,nsig
 do j=1,jm
  do i=1,im
   temp1(i,j)=temp3(i,k,j)
  end do
 end do
 call fill_mass_grid2t(temp1,im,jm,tempb,igtypeh)
 call out_grads(tempb,outname,im,jm)
end do
deallocate(temp3)
allocate(temp3(im+1,nsig,jm))

!                      u
call retrieve_binary_mass_field('U',temp3,input_file,(im+1)*jm*nsig)
do k=1,nsig
 do j=1,jm
  do i=1,im+1
   temp1u(i,j)=temp3(i,k,j)
  end do
 end do
 call fill_mass_grid2u(temp1u,im,jm,tempb,igtypeu)
 call out_grads(tempb,outname,im,jm)
end do
deallocate(temp3)
allocate(temp3(im,nsig,jm+1))

!                      v
call retrieve_binary_mass_field('V',temp3,input_file,im*(jm+1)*nsig)
do k=1,nsig
 do j=1,jm+1
  do i=1,im
   temp1v(i,j)=temp3(i,k,j)
  end do
 end do
 call fill_mass_grid2v(temp1v,im,jm,tempb,igtypev)
 call out_grads(tempb,outname,im,jm)
end do
deallocate(temp3)

!                      landmask
call retrieve_binary_mass_field('LANDMASK',temp1,input_file,im*jm)
call fill_mass_grid2t(temp1,im,jm,tempb,igtypeh)
call out_grads(tempb,outname,im,jm)

!                      xice
call retrieve_binary_mass_field('XICE',temp1,input_file,im*jm)
call fill_mass_grid2t(temp1,im,jm,tempb,igtypeh)
call out_grads(tempb,outname,im,jm)

!                      sst
call retrieve_binary_mass_field('SST',temp1,input_file,im*jm)
call fill_mass_grid2t(temp1,im,jm,tempb,igtypeh)
call out_grads(tempb,outname,im,jm)

!                      ivgtyp
call retrieve_binary_mass_ifield('IVGTYP',itemp1,input_file,im*jm)
do j=1,jm
 do i=1,im
  temp1(i,j)=itemp1(i,j)
 end do
end do
call fill_mass_grid2t(temp1,im,jm,tempb,igtypeh)
call out_grads(tempb,outname,im,jm)

!                      isltyp
call retrieve_binary_mass_ifield('ISLTYP',itemp1,input_file,im*jm)
do j=1,jm
 do i=1,im
  temp1(i,j)=itemp1(i,j)
 end do
end do
call fill_mass_grid2t(temp1,im,jm,tempb,igtypeh)
call out_grads(tempb,outname,im,jm)

!                      vegfra
call retrieve_binary_mass_field('VEGFRA',temp1,input_file,im*jm)
call fill_mass_grid2t(temp1,im,jm,tempb,igtypeh)
call out_grads(tempb,outname,im,jm)

!                      snow
call retrieve_binary_mass_field('SNOW',temp1,input_file,im*jm)
call fill_mass_grid2t(temp1,im,jm,tempb,igtypeh)
call out_grads(tempb,outname,im,jm)

!                      u10
call retrieve_binary_mass_field('U10',temp1,input_file,im*jm)
call fill_mass_grid2t(temp1,im,jm,tempb,igtypeh)
call out_grads(tempb,outname,im,jm)

!                      v10
call retrieve_binary_mass_field('V10',temp1,input_file,im*jm)
call fill_mass_grid2t(temp1,im,jm,tempb,igtypeh)
call out_grads(tempb,outname,im,jm)

allocate(temp3(im,nsoil,jm))
!                      smois
call retrieve_binary_mass_field('SMOIS',temp3,input_file,im*jm*nsoil)
do j=1,jm
 do i=1,im
  temp1(i,j)=temp3(i,1,j)
 end do
end do
call fill_mass_grid2t(temp1,im,jm,tempb,igtypeh)
call out_grads(tempb,outname,im,jm)

!                      tslb
call retrieve_binary_mass_field('TSLB',temp3,input_file,im*jm*nsoil)
do j=1,jm
 do i=1,im
  temp1(i,j)=temp3(i,1,j)
 end do
end do
call fill_mass_grid2t(temp1,im,jm,tempb,igtypeh)
call out_grads(tempb,outname,im,jm)
deallocate(temp3)

!                      tsk
call retrieve_binary_mass_field('TSK',temp1,input_file,im*jm)
call fill_mass_grid2t(temp1,im,jm,tempb,igtypeh)
call out_grads(tempb,outname,im,jm)

call close_grads(outname)

end program reg_gsi_grads

subroutine fill_mass_gridg(gin,nx,ny,b,igtype)

!  convert input staggered grid to output filled grid

!   --> gin:   input staggered grid
!   --> nx,ny: input grid dimensions
!  <--  b:     output filled grid
!   --> igtype: =1, then (1,1) on staggered grid is at corner of grid
!               =2, then (1,1) is staggered

implicit none

integer nx,ny,igtype
real(4) gin(nx,ny),b(2*nx-1,ny)

integer i,im,ip,j,jm,jp
real(4) fill,test

fill=.95*huge(fill) ; test=.95*fill
do j=1,ny
 do i=1,2*nx-1
  b(i,j)=fill
 end do
end do

!  first transfer all staggered points to appropriate
!   points on filled output grid

if(igtype.eq.1) then
 do j=1,ny,2
  do i=1,nx
   b(2*i-1,j)=gin(i,j)
  end do
 end do
 do j=2,ny,2
  do i=1,nx-1
   b(2*i,j)=gin(i,j)
  end do
 end do
else
 do j=1,ny,2
  do i=1,nx-1
   b(2*i,j)=gin(i,j)
  end do
 end do
 do j=2,ny,2
  do i=1,nx
   b(2*i-1,j)=gin(i,j)
  end do
 end do
end if

!  now fill in holes

! top and bottom rows:

do j=1,ny,ny-1
 do i=1,2*nx-1
  if(b(i,j).gt.test) then
   ip=i+1 ; if(ip.gt.2*nx-1) ip=i-1
   im=i-1 ; if(im.lt.1) im=i+1
   b(i,j)=.5*(b(im,j)+b(ip,j))
  end if
 end do
end do

! left and right rows:

do j=1,ny
 jp=j+1 ; if(jp.gt.ny) jp=j-1
 jm=j-1 ; if(jm.lt.1) jm=j+1
 do i=1,2*nx-1,2*nx-2
  if(b(i,j).gt.test) b(i,j)=.5*(b(i,jm)+b(i,jp))
 end do
end do

! interior points

do j=1,ny
 jp=j+1 ; if(jp.gt.ny) jp=j-1
 jm=j-1 ; if(jm.lt.1) jm=j+1
 do i=1,2*nx-1
  if(b(i,j).gt.test) then
   ip=i+1 ; if(ip.gt.2*nx-1) ip=i-1
   im=i-1 ; if(im.lt.1) im=i+1
   b(i,j)=.25*(b(ip,j)+b(im,j)+b(i,jp)+b(i,jm))
  end if
 end do
end do

end subroutine fill_mass_gridg

subroutine retrieve_binary_mass_constants(wrf_ges_filename, &
                      iyear,imonth,iday,ihour,iminute,isecond, &
                      jyear,jmonth,jday,jhour,jminute,jsecond, &
                      nlon,nlat,nsig,nsoil,pt)

  implicit none

  character(*),intent(in):: wrf_ges_filename
  integer,intent(out):: iyear,imonth,iday,ihour,iminute,isecond  !  start time
  integer,intent(out):: jyear,jmonth,jday,jhour,jminute,jsecond  !  forecast time
  integer,intent(out):: nlon,nlat,nsig,nsoil
  real(4),intent(out):: pt

  integer,parameter:: int_field       =       530
  integer,parameter:: int_dom_ti_char =       220

  integer in_unit,status,status_hdr,code
  integer hdrbuf(512)

  integer itypesize,rtypesize,typesize
  integer hdrbufsize
  integer inttypesize,realtypesize
  integer datahandle
  character(132) datestr,varname
  real(4) dummy_field(1)
  integer fieldtype,comm,iocomm
  integer domaindesc
  character(132) memoryorder,stagger,dimnames(3)
  integer domainstart(3),domainend(3)
  integer memorystart(3),memoryend(3)
  integer patchstart(3),patchend(3)
  integer irec
  character(128) element,strdata,dumstr
  integer loccode,loccount

  call wrf_sizeof_integer(itypesize)
  call wrf_sizeof_real(rtypesize)
  in_unit=10
  open(in_unit,file=trim(wrf_ges_filename),form='unformatted')
  write(6,*)' retrieve_binary_mass_constants: in_unit=',in_unit
  inttypesize=itypesize
  realtypesize=rtypesize

! Check for valid input file
  read(in_unit,iostat=status_hdr)hdrbuf
  if(status_hdr /= 0) then
     write(6,*)'retrieve_binary_mass_constants:  problem with wrf_ges_filename = ',&
          trim(wrf_ges_filename),', Status = ',status_hdr
     stop
  endif

!   start with 1st record, which has various constants

!                   y,m,d,h,m,s
  rewind in_unit
  do
     read(in_unit,iostat=status_hdr)hdrbuf
     if(status_hdr /= 0) exit
     if(hdrbuf(2) == int_dom_ti_char) then
        call int_get_ti_header_char(hdrbuf,hdrbufsize,inttypesize, &
             datahandle,element,dumstr,strdata,loccode)
        if(trim(element) == 'START_DATE') then
           read(strdata,'(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2)') &
                iyear,imonth,iday,ihour,iminute,isecond
           write(6,*)' retrieve_binary_mass_constants: START_DATE =',&
                iyear,imonth,iday,ihour,iminute,isecond
           exit
        end if
     end if
  end do

!                  nlon, nlat, nsig
  rewind in_unit
  do
     read(in_unit,iostat=status_hdr)hdrbuf
     if(status_hdr /= 0) exit
     code=hdrbuf(2)
     if(code == int_field) then
        call int_get_write_field_header(hdrbuf,hdrbufsize,inttypesize,typesize, &
             datahandle,datestr,varname,dummy_field,fieldtype,comm,iocomm, &
             domaindesc,memoryorder,stagger,dimnames, &
             domainstart,domainend,memorystart,memoryend,patchstart,patchend)
        if(trim(varname) == 'T') then
           nlon=domainend(1)
           nlat=domainend(3)
           nsig=domainend(2)
           read(datestr,'(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2)') &
                jyear,jmonth,jday,jhour,jminute,jsecond
           write(6,*)' retrieve_binary_mass_constants: iy,m,d,h,m,s=',&
                iyear,imonth,iday,ihour,iminute,isecond
           write(6,*)' retrieve_binary_mass_constants: nlon,lat,sig=',&
                nlon,nlat,nsig
           exit
        end if
        read(in_unit,iostat=status)
        if(status /= 0) exit
     end if
  end do

!                  pt
  rewind in_unit
  do
     read(in_unit,iostat=status_hdr)hdrbuf
     if(status_hdr /= 0) exit
     code=hdrbuf(2)
     if(code == int_field) then
        call int_get_write_field_header(hdrbuf,hdrbufsize,inttypesize,typesize, &
             datahandle,datestr,varname,dummy_field,fieldtype,comm,iocomm, &
             domaindesc,memoryorder,stagger,dimnames, &
             domainstart,domainend,memorystart,memoryend,patchstart,patchend)
        if(trim(varname) == 'P_TOP') then
           read(in_unit,iostat=status)pt
           write(6,*)' retrieve_binary_mass_constants: pt=',pt
           exit
        end if
        read(in_unit,iostat=status)
        if(status /= 0) exit
     end if
  end do

!                   SMOIS
  rewind in_unit
  do
     read(in_unit,iostat=status_hdr)hdrbuf
     if(status_hdr /= 0) exit
     code=hdrbuf(2)
     if(code == int_field) then
        call int_get_write_field_header(hdrbuf,hdrbufsize,inttypesize,typesize, &
             datahandle,datestr,varname,dummy_field,fieldtype,comm,iocomm, &
             domaindesc,memoryorder,stagger,dimnames, &
             domainstart,domainend,memorystart,memoryend,patchstart,patchend)
        if(trim(varname) == 'SMOIS') then
           nsoil=domainend(2)
           write(6,*)' convert_binary_mass: nsoil=',nsoil
           exit
        end if
        read(in_unit,iostat=status)
        if(status /= 0) exit
     end if
  end do

  close(in_unit)

end subroutine retrieve_binary_mass_constants

subroutine retrieve_binary_mass_field(desired_varname,outbuf,wrf_ges_filename,lenbuf)

  implicit none

  character(*),intent(in)::desired_varname
  integer,intent(in):: lenbuf
  character(*),intent(in)::wrf_ges_filename
  real(4),intent(out)::outbuf(lenbuf)

  integer,parameter:: int_field       =       530
  integer,parameter:: int_dom_ti_char =       220

  integer in_unit,status,status_hdr,code
  integer hdrbuf(512)

  integer itypesize,rtypesize,typesize
  integer hdrbufsize
  integer inttypesize,realtypesize
  integer datahandle
  character(132) datestr,varname
  real(4) dummy_field(1)
  integer fieldtype,comm,iocomm
  integer domaindesc
  character(132) memoryorder,stagger,dimnames(3)
  integer domainstart(3),domainend(3)
  integer memorystart(3),memoryend(3)
  integer patchstart(3),patchend(3)
  integer irec
  character(128) element,strdata,dumstr
  integer loccode,loccount

  call wrf_sizeof_integer(itypesize)
  call wrf_sizeof_real(rtypesize)
  in_unit=10
  open(in_unit,file=trim(wrf_ges_filename),form='unformatted')
  write(6,*)' retrieve_binary_mass_field: in_unit=',in_unit
  inttypesize=itypesize
  realtypesize=rtypesize

  rewind in_unit
  do
     read(in_unit,iostat=status_hdr)hdrbuf
     if(status_hdr /= 0) exit
     code=hdrbuf(2)
     if(code == int_field) then
        call int_get_write_field_header(hdrbuf,hdrbufsize,inttypesize,typesize, &
             datahandle,datestr,varname,dummy_field,fieldtype,comm,iocomm, &
             domaindesc,memoryorder,stagger,dimnames, &
             domainstart,domainend,memorystart,memoryend,patchstart,patchend)
        if(trim(varname) == trim(desired_varname)) then
           read(in_unit,iostat=status)outbuf
           exit
        end if
        read(in_unit,iostat=status)
        if(status /= 0) exit
     end if
  end do

  close(in_unit)

end subroutine retrieve_binary_mass_field

subroutine retrieve_binary_mass_ifield(desired_varname,outbuf,wrf_ges_filename,lenbuf)

  implicit none

  character(*),intent(in)::desired_varname
  integer,intent(in):: lenbuf
  character(*),intent(in)::wrf_ges_filename
  integer(4),intent(out)::outbuf(lenbuf)

  integer,parameter:: int_field       =       530
  integer,parameter:: int_dom_ti_char =       220

  integer in_unit,status,status_hdr,code
  integer hdrbuf(512)

  integer itypesize,rtypesize,typesize
  integer hdrbufsize
  integer inttypesize,realtypesize
  integer datahandle
  character(132) datestr,varname
  real(4) dummy_field(1)
  integer fieldtype,comm,iocomm
  integer domaindesc
  character(132) memoryorder,stagger,dimnames(3)
  integer domainstart(3),domainend(3)
  integer memorystart(3),memoryend(3)
  integer patchstart(3),patchend(3)
  integer irec
  character(128) element,strdata,dumstr
  integer loccode,loccount

  call wrf_sizeof_integer(itypesize)
  call wrf_sizeof_real(rtypesize)
  in_unit=10
  open(in_unit,file=trim(wrf_ges_filename),form='unformatted')
  write(6,*)' retrieve_binary_mass_ifield: in_unit=',in_unit
  inttypesize=itypesize
  realtypesize=rtypesize

  rewind in_unit
  do
     read(in_unit,iostat=status_hdr)hdrbuf
     if(status_hdr /= 0) exit
     code=hdrbuf(2)
     if(code == int_field) then
        call int_get_write_field_header(hdrbuf,hdrbufsize,inttypesize,typesize, &
             datahandle,datestr,varname,dummy_field,fieldtype,comm,iocomm, &
             domaindesc,memoryorder,stagger,dimnames, &
             domainstart,domainend,memorystart,memoryend,patchstart,patchend)
        if(trim(varname) == trim(desired_varname)) then
           read(in_unit,iostat=status)outbuf
           exit
        end if
        read(in_unit,iostat=status)
        if(status /= 0) exit
     end if
  end do

  close(in_unit)

end subroutine retrieve_binary_mass_ifield
subroutine mpi_abort

  stop

end subroutine mpi_abort

subroutine fill_mass_grid2t(gin,nx,ny,b)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    fill_mass_grid2t    move wrf mass core c-grid to a-grid
!   prgmmr: parrish          org: np22                date: 2004-06-22
!
! abstract: creates an unstaggered A grid from the staggered C grid used 
!           by the wrf mass core.  This is for mass points, for which the
!           output grid is a direct copy of the input grid.  The final 
!           output grid is rearranged for transfer to subdomains.
!
! program history log:
!   2004-07-15  parrish
!
!   input argument list:
!     gin      - input C grid field over entire horizontal domain
!     nx,ny    - input grid dimensions
!
!   output argument list
!     gout     - output A-grid  (reorganized for distibution to local domains)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  implicit none

  integer nx,ny
  real(4) gin(nx,ny)
  
  real(4) b(nx,ny)
  integer i,j

!---------------------------mass grids--just copy
    do j=1,ny
       do i=1,nx
          b(i,j)=gin(i,j)
       end do
    end do

end subroutine fill_mass_grid2t

subroutine fill_mass_grid2u(gin,nx,ny,b)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    fill_mass_grid2u   move wrf mass core c-grid to a-grid
!   prgmmr: parrish          org: np22                date: 2004-06-22
!
! abstract: creates an unstaggered A grid from the staggered C grid used 
!           by the wrf mass core.  This interpolates u-grid points in x 
!           to the location of mass grid points.
!
! program history log:
!   2004-07-15  parrish
!
!   input argument list:
!     gin      - input C grid field over entire horizontal domain
!     nx,ny    - input grid dimensions
!
!            INDEXING FOR C-GRID (u-points relative to h-points
!
!
!       ^    3         u  h  u  h  u  h  u  h  u
!       |
!           
!
!       y    2         u  h  u  h  u  h  u  h  u
!
!          
!
!        h,u 1         u  h  u  h  u  h  u  h  u 
!
!                                       (note extra u point in x direction)
!
!                u     1     2     3
!                h        1     2     3
!
!                           x -->
!
!   output argument list
!     gout     - output A-grid  (reorganized for distibution to local domains)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  implicit none

  integer nx,ny
  real(4) gin(nx+1,ny)
  
  real(4) b(nx,ny)
  integer i,ip,j

  do j=1,ny
     do i=1,nx
        ip=i+1
        b(i,j)=.5*(gin(i,j)+gin(ip,j))
     end do
  end do
  
end subroutine fill_mass_grid2u

subroutine fill_mass_grid2v(gin,nx,ny,b)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    fill_mass_grid2    move wrf mass core c-grid to a-grid
!   prgmmr: parrish          org: np22                date: 2004-06-22
!
! abstract: creates an unstaggered A grid from the staggered C grid used 
!           by the wrf mass core.  For the mass points, the output grid 
!           is a direct copy of the input grid.  The u-grid (v-grid) is interpolated
!           in the x-direction (y-direction) to the mass grid points.
!
! program history log:
!   2004-07-15  parrish
!
!   input argument list:
!     gin      - input C grid field over entire horizontal domain
!     nx,ny    - input grid dimensions
!
!            INDEXING FOR C-GRID
!
!              4          v     v     v     v
!
!       ^    3            h     h     h     h
!       |
!              3          v     v     v     v
!
!       y    2            h     h     h     h
!
!              2          v     v     v     v
!
!        h   1            h     h     h     h
!
!         v    1          v     v     v     v
!
!                h,v      1     2     3
!                          (note extra v point in y direction)
!
!                           x -->
!
!   output argument list
!     gout     - output A-grid  (reorganized for distibution to local domains)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  implicit none

  integer nx,ny
  real(4) gin(nx,ny+1)
  
  real(4) b(nx,ny)
  integer i,j,jp

  do j=1,ny
     jp=j+1
     do i=1,nx
        b(i,j)=.5*(gin(i,j)+gin(i,jp))
     end do
  end do
  
end subroutine fill_mass_grid2v

SUBROUTINE int_get_ti_header_char( hdrbuf, hdrbufsize, itypesize, &
                              DataHandle, Element, VarName, Data, code )
!<DESCRIPTION>
!<PRE>
! Same as int_gen_ti_header_char except that Data is read from 
! the file.  
!</PRE>
!</DESCRIPTION>
  IMPLICIT NONE
!!  INCLUDE 'intio_tags.h'
  INTEGER, INTENT(INOUT)       ::  hdrbuf(*)
  INTEGER, INTENT(OUT)         ::  hdrbufsize
  INTEGER, INTENT(IN)          ::  itypesize
  CHARACTER*(*), INTENT(INOUT) ::  Element, Data, VarName
  INTEGER, INTENT(OUT)         ::  DataHandle, code
!Local
  INTEGER i, n, DummyCount, typesize
  CHARACTER * 132  dummyData
  logical, external :: debug_foo
!
  CALL int_get_ti_header_c ( hdrbuf, hdrbufsize, n, itypesize, typesize, &
                           DataHandle, dummyData, DummyCount, code )
  i = n/itypesize+1 ;
  CALL int_unpack_string ( Element, hdrbuf( i ), n ) ; i = i + n
  CALL int_unpack_string ( Data   , hdrbuf( i ), n ) ; i = i + n
  CALL int_unpack_string ( VarName  , hdrbuf( i ), n ) ; i = i + n
  hdrbufsize = hdrbuf(1)

  RETURN
END SUBROUTINE int_get_ti_header_char
SUBROUTINE int_get_write_field_header ( hdrbuf, hdrbufsize, itypesize, ftypesize, &
                                        DataHandle , DateStr , VarName , Dummy , FieldType , Comm , IOComm,  &
                                        DomainDesc , MemoryOrder , Stagger , DimNames ,              &
                                        DomainStart , DomainEnd ,                                    &
                                        MemoryStart , MemoryEnd ,                                    &
                                        PatchStart , PatchEnd )
!<DESCRIPTION>
!<PRE>
! See documentation block in int_gen_write_field_header() for 
! a description of a "write field" header.  
!</PRE>
!</DESCRIPTION>
  IMPLICIT NONE

!!  INCLUDE 'intio_tags.h'
  integer,parameter:: int_field       =       530
  INTEGER,       INTENT(INOUT)  ::  hdrbuf(*)
  INTEGER,       INTENT(OUT)    ::  hdrbufsize
  INTEGER,       INTENT(INOUT)  ::  itypesize, ftypesize
  INTEGER ,      INTENT(OUT)    :: DataHandle
  CHARACTER*(*), INTENT(INOUT)  :: DateStr
  CHARACTER*(*), INTENT(INOUT)  :: VarName
  REAL, DIMENSION(*)            :: Dummy
  INTEGER                                       :: FieldType
  INTEGER                                       :: Comm
  INTEGER                                       :: IOComm
  INTEGER                                       :: DomainDesc
  CHARACTER*(*)                                 :: MemoryOrder
  CHARACTER*(*)                                 :: Stagger
  CHARACTER*(*) , dimension (*)                 :: DimNames
  INTEGER ,dimension(*)                         :: DomainStart, DomainEnd
  INTEGER ,dimension(*)                         :: MemoryStart, MemoryEnd
  INTEGER ,dimension(*)                         :: PatchStart,  PatchEnd
!Local
  CHARACTER*132 mess
  INTEGER i, n

  hdrbufsize = hdrbuf(1)
  IF ( hdrbuf(2) .NE. int_field ) THEN
    write(mess,*)'int_get_write_field_header: hdrbuf(2) ne int_field ',hdrbuf(2),int_field
    CALL wrf_error_fatal3 ( "module_internal_header_util.b" , 220 ,  mess )
  ENDIF
  ftypesize = hdrbuf(3)

   i = 4
   DataHandle = hdrbuf(i)     ; i = i+1
  call int_unpack_string( DateStr, hdrbuf(i), n )     ; i = i+n
  call int_unpack_string( VarName, hdrbuf(i), n )     ; i = i+n
   FieldType = hdrbuf(i)      ; i = i+1
  call int_unpack_string( MemoryOrder, hdrbuf(i), n ) ; i = i+n
  call int_unpack_string( Stagger, hdrbuf(i), n )     ; i = i+n
  call int_unpack_string( DimNames(1), hdrbuf(i), n ) ; i = i+n
  call int_unpack_string( DimNames(2), hdrbuf(i), n ) ; i = i+n
  call int_unpack_string( DimNames(3), hdrbuf(i), n ) ; i = i+n
   DomainStart(1) = hdrbuf(i)    ; i = i+1
   DomainStart(2) = hdrbuf(i)    ; i = i+1
   DomainStart(3) = hdrbuf(i)    ; i = i+1
   DomainEnd(1) = hdrbuf(i)       ; i = i+1
   DomainEnd(2) = hdrbuf(i)       ; i = i+1
   DomainEnd(3) = hdrbuf(i)       ; i = i+1
   PatchStart(1) = hdrbuf(i)     ; i = i+1
   PatchStart(2) = hdrbuf(i)     ; i = i+1
   PatchStart(3) = hdrbuf(i)     ; i = i+1
   PatchEnd(1) = hdrbuf(i)       ; i = i+1
   PatchEnd(2) = hdrbuf(i)       ; i = i+1
   PatchEnd(3) = hdrbuf(i)       ; i = i+1
   DomainDesc = hdrbuf(i)       ; i = i+1

  RETURN
END SUBROUTINE int_get_write_field_header
SUBROUTINE int_unpack_string ( str, buf, n )
  IMPLICIT NONE
!<DESCRIPTION>
!<PRE>
! This routine is used to extract a string from a sequence of integers.  
! The first integer is the string length.  
!</PRE>
!</DESCRIPTION>
  CHARACTER*(*), INTENT(OUT)        :: str
  INTEGER, INTENT(OUT)              :: n       ! on return, N is the number of ints copied from buf
  INTEGER, INTENT(IN), DIMENSION(*) :: buf
!Local
  INTEGER i
  INTEGER strlen

  strlen = buf(1)
  str = ""
  DO i = 1, strlen
    str(i:i) = char(buf(i+1))
  ENDDO
  n = strlen + 1
END SUBROUTINE int_unpack_string

SUBROUTINE wrf_sizeof_integer( retval )
  integer(4),parameter:: i_kind=4
  IMPLICIT NONE
  INTEGER retval
! IWORDSIZE is defined by CPP
  retval = i_kind
  RETURN
END SUBROUTINE wrf_sizeof_integer

SUBROUTINE wrf_sizeof_real( retval )
  integer(4),parameter:: r_kind=4
  IMPLICIT NONE
  INTEGER retval
! RWORDSIZE is defined by CPP
  retval = r_kind
  RETURN
END SUBROUTINE wrf_sizeof_real

!WRF:DRIVER_LAYER:UTIL
!

MODULE module_wrf_error
  INTEGER           :: wrf_debug_level = 0
  CHARACTER*256     :: wrf_err_message
CONTAINS

  LOGICAL FUNCTION wrf_at_debug_level ( level )
    IMPLICIT NONE
    INTEGER , INTENT(IN) :: level
    wrf_at_debug_level = ( level .LE. wrf_debug_level )
    RETURN
  END FUNCTION wrf_at_debug_level

  SUBROUTINE init_module_wrf_error
  END SUBROUTINE init_module_wrf_error

END MODULE module_wrf_error

  SUBROUTINE set_wrf_debug_level ( level )
    USE module_wrf_error
    IMPLICIT NONE
    INTEGER , INTENT(IN) :: level
    wrf_debug_level = level
    RETURN
  END SUBROUTINE set_wrf_debug_level

  SUBROUTINE get_wrf_debug_level ( level )
    USE module_wrf_error
    IMPLICIT NONE
    INTEGER , INTENT(OUT) :: level
    level = wrf_debug_level
    RETURN
  END SUBROUTINE get_wrf_debug_level


SUBROUTINE wrf_debug( level , str )
  USE module_wrf_error
  IMPLICIT NONE
  CHARACTER*(*) str
  INTEGER , INTENT (IN) :: level
  INTEGER               :: debug_level
  CHARACTER (LEN=256) :: time_str
  CHARACTER (LEN=256) :: grid_str
  CHARACTER (LEN=512) :: out_str
  CALL get_wrf_debug_level( debug_level )
  IF ( level .LE. debug_level ) THEN
    ! old behavior
      CALL wrf_message( str )
  ENDIF
  RETURN
END SUBROUTINE wrf_debug

SUBROUTINE wrf_message( str )
  USE module_wrf_error
  IMPLICIT NONE
  CHARACTER*(*) str
  write(0,*) TRIM(str)
  print*, TRIM(str)
!TBH:  Calls to logwrite cut off str in ESMF 2.2.0.
!TBH:  Restore this call once this ESMF bug is fixed.
!TBH#ifdef USE_LOGERR
!TBH  IF ( WRFU_IsInitialized() ) THEN
!TBH    CALL WRFU_LogWrite( TRIM(str), WRFU_LOG_INFO )
!TBH  ENDIF
!TBH#endif
END SUBROUTINE wrf_message

! intentionally write to stderr only
SUBROUTINE wrf_message2( str )
  USE module_wrf_error
  IMPLICIT NONE
  CHARACTER*(*) str
  write(0,*) str
!TBH:  Calls to logwrite cut off str in ESMF 2.2.0.
!TBH:  Restore this call once this ESMF bug is fixed.
!TBH#ifdef USE_LOGERR
!TBH  IF ( WRFU_IsInitialized() ) THEN
!TBH    CALL WRFU_LogWrite( str, WRFU_LOG_INFO )
!TBH  ENDIF
!TBH#endif
END SUBROUTINE wrf_message2

SUBROUTINE wrf_error_fatal3( file_str, line, str )
  USE module_wrf_error
  IMPLICIT NONE
  CHARACTER*(*) file_str
  INTEGER , INTENT (IN) :: line  ! only print file and line if line > 0
  CHARACTER*(*) str
  CHARACTER*256 :: line_str

  write(line_str,'(i6)') line
  CALL wrf_message( '-------------- FATAL CALLED ---------------' )
  ! only print file and line if line is positive
  IF ( line > 0 ) THEN
    CALL wrf_message( 'FATAL CALLED FROM FILE:  '//file_str//'  LINE:  '//TRIM(line_str) )
  ENDIF
  CALL wrf_message( str )
  CALL wrf_message( '-------------------------------------------' )
! CALL wrf_abort
  stop
END SUBROUTINE wrf_error_fatal3

SUBROUTINE wrf_error_fatal( str )
  USE module_wrf_error
  IMPLICIT NONE
  CHARACTER*(*) str
  CALL wrf_error_fatal3 ( ' ', 0, str )
END SUBROUTINE wrf_error_fatal

! Check to see if expected value == actual value
! If not, print message and exit.  
SUBROUTINE wrf_check_error( expected, actual, str, file_str, line )
  USE module_wrf_error
  IMPLICIT NONE
  INTEGER , INTENT (IN) :: expected
  INTEGER , INTENT (IN) :: actual
  CHARACTER*(*) str
  CHARACTER*(*) file_str
  INTEGER , INTENT (IN) :: line
  CHARACTER (LEN=512)   :: rc_str
  CHARACTER (LEN=512)   :: str_with_rc

  IF ( expected .ne. actual ) THEN
    WRITE (rc_str,*) '  Routine returned error code = ',actual
    str_with_rc = TRIM(str // rc_str)
    CALL wrf_error_fatal3 ( file_str, line, str_with_rc )
  ENDIF
END SUBROUTINE wrf_check_error
