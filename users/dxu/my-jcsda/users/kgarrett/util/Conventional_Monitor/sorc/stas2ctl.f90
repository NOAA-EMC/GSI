!  the program is creat the control file 

 subroutine creatstas_ctl(dtype,itype,ituse,nt,nc,nlev,nregion,nvar,&
                              region,rlatmin,rlatmax,rlonmin,rlonmax,isubtype)
    implicit none

    integer nregion,nt,nlev,nc,i,icc,nvar
    integer,dimension(nt):: itype,ituse,isubtype
    character(7) dtype
    character(20) fileo
    character(40),dimension(nregion) :: region
    real,dimension(nregion) :: rlatmin,rlatmax,rlonmin,rlonmax
    character(3) :: clatmin,clatmax
    character(4) :: clonmin,clonmax
    character(80) string
    character(2) cword
    character(80),dimension(nregion):: stringr
    real rmiss

   nc=nc

    rmiss=-999.0
  
    if(nvar /=18) then
      print *,'wrong variable number'
      return
     endif

    fileo=trim(dtype)//'_stas.ctl'
    

    open(21,file=fileo,form='formatted')

   do i=1, nregion

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
10 format(i2,'N')
20 format(i2,'S')
30 format(i3,'E')
40 format(i3,'W')

    write(21,100)
    write(21,110)
    write(21,120) rmiss
    write(21,130) 
    write(21,140) 
    write(21,150) 
    write(21,160)  
    do i=1,nc
     write(21,141) dtype,i,itype(i),isubtype(i),ituse(i)
    enddo
icc=nc+1
    write(21,142) dtype,icc 
    write(21,143) icc
    write(21,151) nregion 
    do i=1,nregion
     write(cword,'(i2)') i
     string = '*  region= ' // cword // ' ' // trim(stringr(i))
     write(21,152) string
  end do

    write(21,161) nlev 
    write(21,170)  
    write(21,180) nvar 
    write(21,181) nlev
    write(21,182) nlev
    write(21,183) nlev
    write(21,184) nlev
    write(21,185) nlev
    write(21,186) nlev
    write(21,187) nlev
    write(21,188) nlev
    write(21,189) nlev
    write(21,190) nlev
    write(21,191) nlev
    write(21,192) nlev
    write(21,193) nlev
    write(21,194) nlev
    write(21,195) nlev
    write(21,196) nlev
    write(21,197) nlev
    write(21,198) nlev
    
    write(21,200)



100 format('dset ')
110 format('options big_endian sequential template')
120 format('undef ',f5.0)
130 format('title  conventional stats  jiter')
140 format('*XDEF is data type')
150 format('*YDEF is region')
141 format('* ', a7,'x= ',i4,' dtype= ',i6,' subtype= ',i6,' iuse= ',i2) 
142 format('* ',a7,'x= ',i4,' dtype=    all    ','subtype=   0   iuse=   1 ') 
160 format('*ZDEF is vertical level')
143 format('xdef ',i3,' linear 1.0 1.0')
151 format('ydef ',i3,' linear 1.0 1.0')
152 format(a80)
161 format('zdef ',i3,' linear 1.0 1.0')
170 format('tdef 1 linear 00z14dec2001 1hr ')
180 format('vars ',i7)
181 format('count1     ', i3,'  0 assimilated obs no. ,0: all')
182 format('count_vqc1     ', i3,'  0  obs no. rejected by vqc for assimilated data, 0: all')
183 format('bias1      ', i3,'  0  bias (obs-ges) for assimilated data')
184 format('rms1       ', i3,'  0  rms  for assimilated data')
185 format('rat1       ', i3,'  0  penalty for assimilated data')
186 format('qcrat1     ', i3,'  0  qc penalty for assimilated data')
187 format('count2     ', i3,'  0  rejected obs no,0: all')
188 format('count_vqc2     ', i3,'  0  obs no. rejected by vqc for rejected data,0: all')
189 format('bias2      ', i3,'  0  bias(obs-ges) for rejected data')
190 format('rms2       ', i3,'  0  rms  for rejected data')
191 format('rat2       ', i3,'  0  penalty for rejected data')
192 format('qcrat2     ', i3,'  0  qc penalty for rejected data')
193 format('count3     ', i3,'  0   obs no. for monitored data,0: all')
194 format('count_vqc3     ', i3,'  0 obs no. rejected by vqc for monitored data,0: all')
195 format('bias3      ', i3,'  0  bias(obs-ges) for monitored data')
196 format('rms3       ', i3,'  0  rms for monitored data')
197 format('rat3       ', i3,'  0  penalty for monitored data')
198 format('qcrat3     ', i3,'  0  qc penalty for monitored data')
200 format('endvars')

close(21)

return
end
