! the program read humidity files
  subroutine read_pw(nreal,mtype,fname,fileo,gtross,rlev)

     implicit none

  real(4),allocatable,dimension(:,:)  :: rdiag
  real(4),dimension(3,3000000) :: rpress
  integer,dimension(3) :: ncount,ncount_vqc,ncount_gros

  character*200 fname
  character*50 fileo
  character*15 mtype 

  real*4 tiny,huge,real
  real rlev,rgtross,gtross,weight,ddf

  integer nobs,nreal,ntotal,ngross,nreal_in,nlev
  integer ilat,ilon,ipres,itime,iqc,iuse,imuse,iweight,ierr,ierr2,ierr3,iobs,iogs
  integer i,ndup,ioges,igos

  real(4) :: bmiss,vqclmt,vqclmte

  data bmiss/-999.0/ 


 

!  print *,'nreal=',nreal

  ncount=0
  rpress=bmiss
  ncount_vqc=0
  ncount_gros=0
  tiny=1.0e-6
  huge=1.0e6


     ntotal=0
     open(unit=11,file=fname,form='unformatted')
     rewind(11)
     read(11) nobs,nreal_in
!     print *, 'nobs=',nobs
     if (nreal /= nreal_in) then
      print *,'nreal_in,nreal ',nreal_in,nreal
      stop
     endif 

    allocate(rdiag(nreal,nobs))
    read(11) rdiag


   ilat=1                           !  lat
    ilon=2                          !  lon
    ipres=4                         !  pressure
    itime=6                         !  relative time
    iqc=7                           !  qc flag
    iuse=9                          !  original data usage flag
    imuse=10                        !  data usage flag in the analsis
    iweight=11                      !  variational relative weight
    ierr=12                        !  original error from bufr error table
    ierr2=13                        !  error from read_bufr
    ierr3=14                        !  error from final adjusted
    iobs=15                         !  obs
    iogs=16                         !  obs-ges
! check weather data have duplicate

  call hash(rdiag,nobs,nreal,ilat,ilon,ipres,itime,iweight,ndup)

 do  i=1,nobs
      if( rdiag(iweight,i) >= 0.0 .and. rdiag(imuse,i) >0.0 ) then
           ncount(1)=ncount(1)+1
           rpress(1,ncount(1))=rdiag(iogs,i)*rdiag(ierr,i)
           weight=rdiag(iweight,i)
           if(weight <1.0) then
              ncount_vqc(1)=ncount_vqc(1)+1 
              vqclmt=vqclmt+abs(rdiag(ioges,i))*rdiag(ierr,i)
           endif
           if(rdiag(iqc,i) >=4.0) then
             ncount(2)=ncount(2)+1
             if(weight <1.0) then
               ncount_vqc(2)=ncount_vqc(2)+1 
             endif
             rpress(2,ncount(2))=rdiag(iogs,i)*rdiag(ierr,i)
           endif
      else if(rdiag(iweight,i) >= 0.0 .and. rdiag(imuse,i) <0.0) then
           if(rdiag(iqc,i) <=7.0) then
              ncount_gros(1)=ncount_gros(1)+1
              if(rdiag(iqc,i) >3.0 .and. rdiag(iqc,i) <=7.0) then
                 ncount_gros(2)=ncount_gros(2)+1
              endif
            else if(rdiag(iqc,i) >=8.0 ) then  
                if(rdiag(ierr,i) >tiny ) then
                   ddf=abs(rdiag(igos,i))*rdiag(ierr,i) 
                   if(ddf <gtross) then
                      ncount(3)=ncount(3)+1
                      rpress(3,ncount(3))=rdiag(iogs,i)*rdiag(ierr,i)
                   else
                      ncount_gros(3)=ncount_gros(3)+1
                   endif
                 else
                   ncount(3)=ncount(3)+1
                   rpress(3,ncount(3))=rdiag(iogs,i)
                 endif
            endif
      endif
  enddo
           

  deallocate(rdiag)


  

   if(ncount_vqc(1) >0) then
      vqclmt=vqclmt/ncount_vqc(1)
    endif

!  calculate the the areacd usr2
    rgtross=-gtross

    nlev=nint((gtross-rgtross)/rlev) 
    
    nlev=nlev
    print *,nlev
    print *, 'rmax,rmin ',gtross,rgtross
    print *, 'ncount_gros,',ncount_gros(1),ncount_gros(2),ncount_gros(3) 
    print *, 'vqc-limit,vqc-limite ',vqclmt,vqclmte
    
    

   call hist(mtype,rpress,3,3000000,ncount,rgtross,gtross,rlev,fileo,ncount_vqc,ncount_gros)   


  return 
  end
