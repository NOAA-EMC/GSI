!This program is run by run_extract.sh and extracts data from the fit files
!Kristen Bathmann
!2018
program extract
implicit none
integer*4:: fn,ihr, nhr, tim, ntimes
integer*4:: ivar, ireg, ilev,i
integer*4:: nlev_raob2,reclen
character*6:: filet
character*10:: filen
integer, parameter:: ninstr=4
!1-acar
!2-acft
!3-raob
!4-surf
integer, parameter:: nreg_acar=1
integer, parameter:: nsub_acar=1
integer, parameter:: nlev_acar=3
integer, parameter:: nvar_acar=4

integer, parameter:: nreg_acft=7
integer, parameter:: nsub_acft=1
integer, parameter:: nlev_acft=3
integer, parameter:: nvar_acft=4

integer, parameter:: nreg_raob=7
integer, parameter:: nsub_raob=1
integer, parameter:: nlev_raob=21
integer, parameter:: nvar_raob=5

!integer, parameter:: nreg_sfc=7
!integer, parameter:: nsub_sfc=2
!integer, parameter:: nlev_sfc=1

integer, parameter:: nreg_surf=7
integer, parameter:: nsub_surf=2
integer, parameter:: nlev_surf=1
integer, parameter:: nvar_surf=5
real*4:: bias,rms,bias1,rms1
real*4, dimension(:,:,:,:), allocatable:: fits_acar_ctl, fits_acft_ctl, fits_raob_ctl, fits_surf_ctl
real*4, dimension(:,:,:,:), allocatable:: fits_acar_exp, fits_acft_exp,fits_raob_exp, fits_surf_exp
real*4, dimension(:,:,:,:,:), allocatable::Facar, Facft, Fraob, Fsurf

read(5,*) ntimes, nhr
allocate(fits_acar_ctl(nvar_acar,9,nlev_acar,nreg_acar))
allocate(fits_acft_ctl(nvar_acft,9,nlev_acft,nreg_acft))
allocate(fits_raob_ctl(nvar_raob,9,nlev_raob,nreg_raob))
allocate(fits_surf_ctl(nvar_surf,9,nsub_surf,nreg_surf))
allocate(fits_acar_exp(nvar_acar,9,nlev_acar,nreg_acar))
allocate(fits_acft_exp(nvar_acft,9,nlev_acft,nreg_acft))
allocate(fits_raob_exp(nvar_raob,9,nlev_raob,nreg_raob))
allocate(fits_surf_exp(nvar_surf,9,nsub_surf,nreg_surf))
allocate(Facar(11,nvar_acar,nlev_acar,nreg_acar,nhr)) 
allocate(Facft(11,nvar_acft,nlev_acft,nreg_acft,nhr))
allocate(Fraob(11,nvar_raob,nlev_raob,nreg_raob,nhr))
allocate(Fsurf(11,nvar_surf,nsub_surf,nreg_surf,nhr))
Facar=0.0
Facft=0.0
Fraob=0.0
Fsurf=0.0
fn=0
do ihr=1,nhr
   do tim=1,(ntimes/nhr)
      fn=fn+1
      filet='cacar_'
      call get_filename(fn,filet,filen)
      call read_fits(filen,nreg_acar,nsub_acar,nlev_acar,nvar_acar,fits_acar_ctl)
      filet='eacar_'
      call get_filename(fn,filet,filen)
      call read_fits(filen,nreg_acar,nsub_acar,nlev_acar,nvar_acar,fits_acar_exp)
      do ivar=1,nvar_acar
         do ireg=1,nreg_acar
            do ilev=1,nlev_acar
               if (ivar.ne.3) then
                  bias1=fits_acar_exp(ivar,2,ilev,ireg)-fits_acar_exp(ivar,3,ilev,ireg)
                  rms1=fits_acar_exp(ivar,5,ilev,ireg)+fits_acar_exp(ivar,6,ilev,ireg)&
                      -2*fits_acar_exp(ivar,4,ilev,ireg)
                  rms1=sqrt(rms1)
                  Facar(1,ivar,ilev,ireg,ihr)=Facar(1,ivar,ilev,ireg,ihr)+bias1
                  Facar(2,ivar,ilev,ireg,ihr)=Facar(2,ivar,ilev,ireg,ihr)+rms1
                  Facar(6,ivar,ilev,ireg,ihr)=Facar(6,ivar,ilev,ireg,ihr)+bias1
                  Facar(7,ivar,ilev,ireg,ihr)=Facar(7,ivar,ilev,ireg,ihr)+rms1
                  Facar(10,ivar,ilev,ireg,ihr)=Facar(10,ivar,ilev,ireg,ihr)+fits_acar_exp(ivar,1,ilev,ireg)
                  Facar(11,ivar,ilev,ireg,ihr)=Facar(11,ivar,ilev,ireg,ihr)+fits_acar_ctl(ivar,1,ilev,ireg)
                  bias=fits_acar_ctl(ivar,2,ilev,ireg)-fits_acar_ctl(ivar,3,ilev,ireg)
                  rms=fits_acar_ctl(ivar,5,ilev,ireg)+fits_acar_ctl(ivar,6,ilev,ireg)&
                      -2*fits_acar_ctl(ivar,4,ilev,ireg)
                  rms=sqrt(rms)
                  Facar(1,ivar,ilev,ireg,ihr)=Facar(1,ivar,ilev,ireg,ihr)-bias
                  Facar(2,ivar,ilev,ireg,ihr)=Facar(2,ivar,ilev,ireg,ihr)-rms
                  Facar(8,ivar,ilev,ireg,ihr)=Facar(8,ivar,ilev,ireg,ihr)+bias
                  Facar(9,ivar,ilev,ireg,ihr)=Facar(9,ivar,ilev,ireg,ihr)+rms
                  Facar(3,ivar,ilev,ireg,ihr)=Facar(3,ivar,ilev,ireg,ihr)+((bias1-bias)*(bias1-bias))
                  Facar(4,ivar,ilev,ireg,ihr)=Facar(4,ivar,ilev,ireg,ihr)+((rms1-rms)*(rms1-rms))
               else
                  bias1=fits_acar_exp(ivar,9,ilev,ireg)
                  rms1=fits_acar_exp(ivar,7,ilev,ireg)+fits_acar_exp(ivar,8,ilev,ireg)&
                      - 2*fits_acar_exp(ivar,6,ilev,ireg)
                  rms1=sqrt(rms1)
                  Facar(1,ivar,ilev,ireg,ihr)=Facar(1,ivar,ilev,ireg,ihr)+bias1
                  Facar(2,ivar,ilev,ireg,ihr)=Facar(2,ivar,ilev,ireg,ihr)+rms1
                  Facar(6,ivar,ilev,ireg,ihr)=Facar(6,ivar,ilev,ireg,ihr)+bias1
                  Facar(7,ivar,ilev,ireg,ihr)=Facar(7,ivar,ilev,ireg,ihr)+rms1
                  Facar(10,ivar,ilev,ireg,ihr)=Facar(10,ivar,ilev,ireg,ihr)+fits_acar_exp(ivar,1,ilev,ireg)
                  Facar(11,ivar,ilev,ireg,ihr)=Facar(11,ivar,ilev,ireg,ihr)+fits_acar_ctl(ivar,1,ilev,ireg)
                  bias=fits_acar_ctl(ivar,9,ilev,ireg)
                  rms=fits_acar_ctl(ivar,7,ilev,ireg)+fits_acar_ctl(ivar,8,ilev,ireg)&
                      - 2*fits_acar_ctl(ivar,6,ilev,ireg)
                  rms=sqrt(rms)
                  Facar(1,ivar,ilev,ireg,ihr)=Facar(1,ivar,ilev,ireg,ihr)-bias
                  Facar(2,ivar,ilev,ireg,ihr)=Facar(2,ivar,ilev,ireg,ihr)-rms
                  Facar(8,ivar,ilev,ireg,ihr)=Facar(8,ivar,ilev,ireg,ihr)+bias
                  Facar(9,ivar,ilev,ireg,ihr)=Facar(9,ivar,ilev,ireg,ihr)+rms
                  Facar(3,ivar,ilev,ireg,ihr)=Facar(3,ivar,ilev,ireg,ihr)+((bias1-bias)*(bias1-bias))
                  Facar(4,ivar,ilev,ireg,ihr)=Facar(4,ivar,ilev,ireg,ihr)+((rms1-rms)*(rms1-rms))
               end if
            end do
         end do
      end do

      filet='cacft_'
      call get_filename(fn,filet,filen)
      call read_fits(filen,nreg_acft,nsub_acft,nlev_acft,nvar_acft,fits_acft_ctl)
      filet='eacft_'
      call get_filename(fn,filet,filen)
      call read_fits(filen,nreg_acft,nsub_acft,nlev_acft,nvar_acft,fits_acft_exp)
      do ivar=1,nvar_acft
         do ireg=1,nreg_acft
            do ilev=1,nlev_acft
               if (ivar.ne.3) then
                  bias1=fits_acft_exp(ivar,2,ilev,ireg)-fits_acft_exp(ivar,3,ilev,ireg)
                  rms1=fits_acft_exp(ivar,5,ilev,ireg)+fits_acft_exp(ivar,6,ilev,ireg)&
                      -2*fits_acft_exp(ivar,4,ilev,ireg)
                  rms1=sqrt(rms1)
                  Facft(1,ivar,ilev,ireg,ihr)=Facft(1,ivar,ilev,ireg,ihr)+bias1
                  Facft(2,ivar,ilev,ireg,ihr)=Facft(2,ivar,ilev,ireg,ihr)+rms1
                  Facft(6,ivar,ilev,ireg,ihr)=Facft(6,ivar,ilev,ireg,ihr)+bias1
                  Facft(7,ivar,ilev,ireg,ihr)=Facft(7,ivar,ilev,ireg,ihr)+rms1
                  Facft(10,ivar,ilev,ireg,ihr)=Facft(10,ivar,ilev,ireg,ihr)+fits_acft_exp(ivar,1,ilev,ireg)
                  Facft(11,ivar,ilev,ireg,ihr)=Facft(11,ivar,ilev,ireg,ihr)+fits_acft_ctl(ivar,1,ilev,ireg)
                  bias=fits_acft_ctl(ivar,2,ilev,ireg)-fits_acft_ctl(ivar,3,ilev,ireg)
                  rms=fits_acft_ctl(ivar,5,ilev,ireg)+fits_acft_ctl(ivar,6,ilev,ireg)&
                      -2*fits_acft_ctl(ivar,4,ilev,ireg)
                  rms=sqrt(rms)
                  Facft(1,ivar,ilev,ireg,ihr)=Facft(1,ivar,ilev,ireg,ihr)-bias
                  Facft(2,ivar,ilev,ireg,ihr)=Facft(2,ivar,ilev,ireg,ihr)-rms
                  Facft(8,ivar,ilev,ireg,ihr)=Facft(8,ivar,ilev,ireg,ihr)+bias
                  Facft(9,ivar,ilev,ireg,ihr)=Facft(9,ivar,ilev,ireg,ihr)+rms
                  Facft(3,ivar,ilev,ireg,ihr)=Facft(3,ivar,ilev,ireg,ihr)+((bias1-bias)*(bias1-bias))
                  Facft(4,ivar,ilev,ireg,ihr)=Facft(4,ivar,ilev,ireg,ihr)+((rms1-rms)*(rms1-rms))
               else
                  bias1=fits_acft_exp(ivar,9,ilev,ireg)
                  rms1=fits_acft_exp(ivar,7,ilev,ireg)+fits_acft_exp(ivar,8,ilev,ireg)&
                      - 2*fits_acft_exp(ivar,6,ilev,ireg)
                  rms1=sqrt(rms1)
                  Facft(1,ivar,ilev,ireg,ihr)=Facft(1,ivar,ilev,ireg,ihr)+bias1
                  Facft(2,ivar,ilev,ireg,ihr)=Facft(2,ivar,ilev,ireg,ihr)+rms1
                  Facft(6,ivar,ilev,ireg,ihr)=Facft(6,ivar,ilev,ireg,ihr)+bias1
                  Facft(7,ivar,ilev,ireg,ihr)=Facft(7,ivar,ilev,ireg,ihr)+rms1
                  Facft(10,ivar,ilev,ireg,ihr)=Facft(10,ivar,ilev,ireg,ihr)+fits_acft_exp(ivar,1,ilev,ireg)
                  Facft(11,ivar,ilev,ireg,ihr)=Facft(11,ivar,ilev,ireg,ihr)+fits_acft_ctl(ivar,1,ilev,ireg)
                  bias=fits_acft_ctl(ivar,9,ilev,ireg)
                  rms=fits_acft_ctl(ivar,7,ilev,ireg)+fits_acft_ctl(ivar,8,ilev,ireg)&
                      - 2*fits_acft_ctl(ivar,6,ilev,ireg)
                  rms=sqrt(rms)
                  Facft(1,ivar,ilev,ireg,ihr)=Facft(1,ivar,ilev,ireg,ihr)-bias
                  Facft(2,ivar,ilev,ireg,ihr)=Facft(2,ivar,ilev,ireg,ihr)-rms
                  Facft(8,ivar,ilev,ireg,ihr)=Facft(8,ivar,ilev,ireg,ihr)+bias
                  Facft(9,ivar,ilev,ireg,ihr)=Facft(9,ivar,ilev,ireg,ihr)+rms
                  Facft(3,ivar,ilev,ireg,ihr)=Facft(3,ivar,ilev,ireg,ihr)+((bias1-bias)*(bias1-bias))
                  Facft(4,ivar,ilev,ireg,ihr)=Facft(4,ivar,ilev,ireg,ihr)+((rms1-rms)*(rms1-rms))

               end if
            end do
         end do
      end do

      filet='craob_'
      call get_filename(fn,filet,filen)
      call read_fits(filen,nreg_raob,nsub_raob,nlev_raob,nvar_raob,fits_raob_ctl)
!fits_raob_ctl(:,1,:,:) should be counts
      filet='eraob_'
      call get_filename(fn,filet,filen)
      call read_fits(filen,nreg_raob,nsub_raob,nlev_raob,nvar_raob,fits_raob_exp)
      do ivar=1,nvar_raob
         nlev_raob2=nlev_raob
         if (ivar.eq.5) nlev_raob2=1
         do ireg=1,nreg_raob
            do ilev=1,nlev_raob2
               if (ivar.ne.3) then
                  bias1=fits_raob_exp(ivar,2,ilev,ireg)-fits_raob_exp(ivar,3,ilev,ireg)
                  rms1=fits_raob_exp(ivar,5,ilev,ireg)+fits_raob_exp(ivar,6,ilev,ireg)-2*fits_raob_exp(ivar,4,ilev,ireg)
                  rms1=sqrt(rms1)
                  Fraob(1,ivar,ilev,ireg,ihr)=Fraob(1,ivar,ilev,ireg,ihr)+bias1
                  Fraob(2,ivar,ilev,ireg,ihr)=Fraob(2,ivar,ilev,ireg,ihr)+rms1
                  Fraob(6,ivar,ilev,ireg,ihr)=Fraob(6,ivar,ilev,ireg,ihr)+bias1
                  Fraob(7,ivar,ilev,ireg,ihr)=Fraob(7,ivar,ilev,ireg,ihr)+rms1
                  Fraob(10,ivar,ilev,ireg,ihr)=Fraob(10,ivar,ilev,ireg,ihr)+fits_raob_exp(ivar,1,ilev,ireg)
                  Fraob(11,ivar,ilev,ireg,ihr)=Fraob(11,ivar,ilev,ireg,ihr)+fits_raob_ctl(ivar,1,ilev,ireg)
                  bias=fits_raob_ctl(ivar,2,ilev,ireg)-fits_raob_ctl(ivar,3,ilev,ireg)
                  rms=fits_raob_ctl(ivar,5,ilev,ireg)+fits_raob_ctl(ivar,6,ilev,ireg)-2*fits_raob_ctl(ivar,4,ilev,ireg)
                  rms=sqrt(rms)
                  Fraob(1,ivar,ilev,ireg,ihr)=Fraob(1,ivar,ilev,ireg,ihr)-bias
                  Fraob(2,ivar,ilev,ireg,ihr)=Fraob(2,ivar,ilev,ireg,ihr)-rms
                  Fraob(8,ivar,ilev,ireg,ihr)=Fraob(8,ivar,ilev,ireg,ihr)+bias
                  Fraob(9,ivar,ilev,ireg,ihr)=Fraob(9,ivar,ilev,ireg,ihr)+rms
                  Fraob(3,ivar,ilev,ireg,ihr)=Fraob(3,ivar,ilev,ireg,ihr)+((bias1-bias)*(bias1-bias))
                  Fraob(4,ivar,ilev,ireg,ihr)=Fraob(4,ivar,ilev,ireg,ihr)+((rms1-rms)*(rms1-rms))

               else
                  bias1=fits_raob_exp(ivar,9,ilev,ireg)
                  rms1=fits_raob_exp(ivar,7,ilev,ireg)+fits_raob_exp(ivar,8,ilev,ireg) &
                      - 2*fits_raob_exp(ivar,6,ilev,ireg)
                  rms1=sqrt(rms1)
                  Fraob(1,ivar,ilev,ireg,ihr)=Fraob(1,ivar,ilev,ireg,ihr)+bias1
                  Fraob(2,ivar,ilev,ireg,ihr)=Fraob(2,ivar,ilev,ireg,ihr)+rms1
                  Fraob(6,ivar,ilev,ireg,ihr)=Fraob(6,ivar,ilev,ireg,ihr)+bias1
                  Fraob(7,ivar,ilev,ireg,ihr)=Fraob(7,ivar,ilev,ireg,ihr)+rms1
                  Fraob(10,ivar,ilev,ireg,ihr)=Fraob(10,ivar,ilev,ireg,ihr)+fits_raob_exp(ivar,1,ilev,ireg)
                  Fraob(11,ivar,ilev,ireg,ihr)=Fraob(11,ivar,ilev,ireg,ihr)+fits_raob_ctl(ivar,1,ilev,ireg)
                  bias=fits_raob_ctl(ivar,9,ilev,ireg)
                  rms=fits_raob_ctl(ivar,7,ilev,ireg)+fits_raob_ctl(ivar,8,ilev,ireg) &
                      - 2*fits_raob_ctl(ivar,6,ilev,ireg)
                  rms=sqrt(rms)
                  Fraob(1,ivar,ilev,ireg,ihr)=Fraob(1,ivar,ilev,ireg,ihr)-bias
                  Fraob(2,ivar,ilev,ireg,ihr)=Fraob(2,ivar,ilev,ireg,ihr)-rms
                  Fraob(8,ivar,ilev,ireg,ihr)=Fraob(8,ivar,ilev,ireg,ihr)+bias
                  Fraob(9,ivar,ilev,ireg,ihr)=Fraob(9,ivar,ilev,ireg,ihr)+rms
                  Fraob(3,ivar,ilev,ireg,ihr)=Fraob(3,ivar,ilev,ireg,ihr)+((bias1-bias)*(bias1-bias))
                  Fraob(4,ivar,ilev,ireg,ihr)=Fraob(4,ivar,ilev,ireg,ihr)+((rms1-rms)*(rms1-rms))

               end if
            end do
         end do
      end do
      filet='csurf_'
      call get_filename(fn,filet,filen)
      call read_fits(filen,nreg_surf,nsub_surf,nlev_surf,nvar_surf,fits_surf_ctl)
      filet='esurf_'
      call get_filename(fn,filet,filen)
      call read_fits(filen,nreg_surf,nsub_surf,nlev_surf,nvar_surf,fits_surf_exp)
      do ivar=1,nvar_surf
         do ireg=1,nreg_surf
            do ilev=1,nsub_surf
               if (ivar.ne.3) then
                  bias1=fits_surf_exp(ivar,2,ilev,ireg)-fits_surf_exp(ivar,3,ilev,ireg)
                  rms1=fits_surf_exp(ivar,5,ilev,ireg)+fits_surf_exp(ivar,6,ilev,ireg)&
                      -2*fits_surf_exp(ivar,4,ilev,ireg)
                  rms1=sqrt(rms1)
                  Fsurf(1,ivar,ilev,ireg,ihr)=Fsurf(1,ivar,ilev,ireg,ihr)+bias1
                  Fsurf(2,ivar,ilev,ireg,ihr)=Fsurf(2,ivar,ilev,ireg,ihr)+rms1
                  Fsurf(6,ivar,ilev,ireg,ihr)=Fsurf(6,ivar,ilev,ireg,ihr)+bias1
                  Fsurf(7,ivar,ilev,ireg,ihr)=Fsurf(7,ivar,ilev,ireg,ihr)+rms1
                  Fsurf(10,ivar,ilev,ireg,ihr)=Fsurf(10,ivar,ilev,ireg,ihr)+fits_surf_exp(ivar,1,ilev,ireg)
                  Fsurf(11,ivar,ilev,ireg,ihr)=Fsurf(11,ivar,ilev,ireg,ihr)+fits_surf_ctl(ivar,1,ilev,ireg)
                  bias=fits_surf_ctl(ivar,2,ilev,ireg)-fits_surf_ctl(ivar,3,ilev,ireg)
                  rms=fits_surf_ctl(ivar,5,ilev,ireg)+fits_surf_ctl(ivar,6,ilev,ireg)&
                      -2*fits_surf_ctl(ivar,6,ilev,ireg)
                  rms=sqrt(rms)
                  Fsurf(1,ivar,ilev,ireg,ihr)=Fsurf(1,ivar,ilev,ireg,ihr)-bias
                  Fsurf(2,ivar,ilev,ireg,ihr)=Fsurf(2,ivar,ilev,ireg,ihr)-rms
                  Fsurf(8,ivar,ilev,ireg,ihr)=Fsurf(8,ivar,ilev,ireg,ihr)+bias
                  Fsurf(9,ivar,ilev,ireg,ihr)=Fsurf(9,ivar,ilev,ireg,ihr)+rms
                  Fsurf(3,ivar,ilev,ireg,ihr)=Fsurf(3,ivar,ilev,ireg,ihr)+((bias1-bias)*(bias1-bias))
                  Fsurf(4,ivar,ilev,ireg,ihr)=Fsurf(4,ivar,ilev,ireg,ihr)+((rms1-rms)*(rms1-rms))
               else
                  bias1=fits_surf_exp(ivar,9,ilev,ireg)
                  rms1=fits_surf_exp(ivar,7,ilev,ireg)+fits_surf_exp(ivar,8,ilev,ireg)&
                      - 2*fits_surf_exp(ivar,4,ilev,ireg)
                  rms1=sqrt(rms1)
                  Fsurf(1,ivar,ilev,ireg,ihr)=Fsurf(1,ivar,ilev,ireg,ihr)+bias1
                  Fsurf(2,ivar,ilev,ireg,ihr)=Fsurf(2,ivar,ilev,ireg,ihr)+rms1
                  Fsurf(6,ivar,ilev,ireg,ihr)=Fsurf(6,ivar,ilev,ireg,ihr)+bias1
                  Fsurf(7,ivar,ilev,ireg,ihr)=Fsurf(7,ivar,ilev,ireg,ihr)+rms1
                  Fsurf(10,ivar,ilev,ireg,ihr)=Fsurf(10,ivar,ilev,ireg,ihr)+fits_surf_exp(ivar,1,ilev,ireg)
                  Fsurf(11,ivar,ilev,ireg,ihr)=Fsurf(11,ivar,ilev,ireg,ihr)+fits_surf_ctl(ivar,1,ilev,ireg)
                  bias=fits_surf_ctl(ivar,9,ilev,ireg)
                  rms=fits_surf_ctl(ivar,7,ilev,ireg)+fits_surf_ctl(ivar,8,ilev,ireg)&
                      - 2*fits_surf_ctl(ivar,6,ilev,ireg)
                  rms=sqrt(rms)
                  Fsurf(1,ivar,ilev,ireg,ihr)=Fsurf(1,ivar,ilev,ireg,ihr)+bias
                  Fsurf(2,ivar,ilev,ireg,ihr)=Fsurf(2,ivar,ilev,ireg,ihr)+rms
                  Fsurf(8,ivar,ilev,ireg,ihr)=Fsurf(8,ivar,ilev,ireg,ihr)+bias
                  Fsurf(9,ivar,ilev,ireg,ihr)=Fsurf(9,ivar,ilev,ireg,ihr)+rms
                  Fsurf(3,ivar,ilev,ireg,ihr)=Fsurf(3,ivar,ilev,ireg,ihr)+((bias1-bias)*(bias1-bias))
                  Fsurf(4,ivar,ilev,ireg,ihr)=Fsurf(4,ivar,ilev,ireg,ihr)+((rms1-rms)*(rms1-rms))
               end if
            end do
         end do
      end do
   end do
end do
do ihr=1,nhr
   do ivar=1,nvar_acar
      do ireg=1,nreg_acar
         do ilev=1,nlev_acar
            do i=1,2
               Facar(i+2,ivar,ilev,ireg,ihr)=(Facar(i+2,ivar,ilev,ireg,ihr)- &
                  ((Facar(i,ivar,ilev,ireg,ihr)**2)/(ntimes/nhr)))/((ntimes/nhr)-1)
               Facar(i+2,ivar,ilev,ireg,ihr)=sqrt(Facar(i+2,ivar,ilev,ireg,ihr))
               Facar(i,ivar,ilev,ireg,ihr)=Facar(i,ivar,ilev,ireg,ihr)/(ntimes/nhr)
            end do
            do i=6,9
               Facar(i,ivar,ilev,ireg,ihr)=Facar(i,ivar,ilev,ireg,ihr)/(ntimes/nhr)
            end do
            Facar(5,ivar,ilev,ireg,ihr)=(ntimes/nhr)
         end do
      end do
   end do
   do ivar=1,nvar_acft
      do ireg=1,nreg_acft
         do ilev=1,nlev_acft
            do i=1,2
               Facft(i+2,ivar,ilev,ireg,ihr)=(Facft(i+2,ivar,ilev,ireg,ihr)- &
                  ((Facft(i,ivar,ilev,ireg,ihr)**2)/(ntimes/nhr)))/((ntimes/nhr)-1)
               Facft(i+2,ivar,ilev,ireg,ihr)=sqrt(Facft(i+2,ivar,ilev,ireg,ihr))
               Facft(i,ivar,ilev,ireg,ihr)=Facft(i,ivar,ilev,ireg,ihr)/(ntimes/nhr)
            end do
            do i=6,9
               Facft(i,ivar,ilev,ireg,ihr)=Facft(i,ivar,ilev,ireg,ihr)/(ntimes/nhr)
            end do
            Facft(5,ivar,ilev,ireg,ihr)=(ntimes/nhr)
         end do
      end do
   end do
   do ivar=1,nvar_raob
      nlev_raob2=nlev_raob
      if (ivar.eq.5) nlev_raob2=1
      do ireg=1,nreg_raob
         do ilev=1,nlev_raob2
            do i=1,2
               Fraob(i+2,ivar,ilev,ireg,ihr)=Fraob(i+2,ivar,ilev,ireg,ihr)-((Fraob(i,ivar,ilev,ireg,ihr)*Fraob(i,ivar,ilev,ireg,ihr))/(ntimes/nhr))
               Fraob(i+2,ivar,ilev,ireg,ihr)=(Fraob(i+2,ivar,ilev,ireg,ihr)/((ntimes/nhr)-1))
               Fraob(i+2,ivar,ilev,ireg,ihr)=sqrt(Fraob(i+2,ivar,ilev,ireg,ihr))
               Fraob(i,ivar,ilev,ireg,ihr)=Fraob(i,ivar,ilev,ireg,ihr)/(ntimes/nhr)
            end do
            do i=6,9
               Fraob(i,ivar,ilev,ireg,ihr)=Fraob(i,ivar,ilev,ireg,ihr)/(ntimes/nhr)
            end do
            Fraob(5,ivar,ilev,ireg,ihr)=(ntimes/nhr)
         end do
      end do
   end do

   do ivar=1,nvar_surf
      do ireg=1,nreg_surf
         do ilev=1,nsub_surf
            do i=1,2
               Fsurf(i+2,ivar,ilev,ireg,ihr)=(Fsurf(i+2,ivar,ilev,ireg,ihr)- &
                  ((Fsurf(i,ivar,ilev,ireg,ihr)**2)/(ntimes/nhr)))/((ntimes/nhr)-1)
               Fsurf(i+2,ivar,ilev,ireg,ihr)=sqrt(Fsurf(i+2,ivar,ilev,ireg,ihr))
              Fsurf(i,ivar,ilev,ireg,ihr)=Fsurf(i,ivar,ilev,ireg,ihr)/(ntimes/nhr)
            end do
            do i=6,9
              Fsurf(i,ivar,ilev,ireg,ihr)=Fsurf(i,ivar,ilev,ireg,ihr)/(ntimes/nhr)
            end do
            Fsurf(5,ivar,ilev,ireg,ihr)=(ntimes/nhr)
         end do
      end do
   end do

end do
!KAB 9 to 11
inquire(iolength=reclen) Facar(1,1,1,1,1)
open(48,file='Acar_fits',form='unformatted',access='direct',recl=11*nvar_acar*nlev_acar*nreg_acar*nhr*reclen)
write(48,rec=1) Facar
close(48)
open(49,file='Acft_fits',form='unformatted',access='direct',recl=11*nvar_acft*nlev_acft*nreg_acft*nhr*reclen)
write(49,rec=1) Facft
close(49)
open(50,file='Raob_fits',form='unformatted',access='direct',recl=11*nvar_raob*nlev_raob*nreg_raob*nhr*reclen)
write(50,rec=1) Fraob
close(50)
open(51,file='Surf_fits',form='unformatted',access='direct',recl=11*nvar_surf*nsub_surf*nreg_surf*nhr*reclen)
write(51,rec=1) Fsurf
close(51)

contains
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine read_fits(filen,nreg,nsub,nlev,nvar,fits)
implicit none
character(10), intent(in):: filen
integer, intent(in):: nreg,nsub,nlev,nvar
real, dimension(:,:,:,:), intent(out):: fits
real, dimension(:,:), allocatable:: gdata
real, dimension(:,:,:,:), allocatable:: Fit
integer:: istat, ilev, nstat, coun,nlev2,ib
allocate(gdata(nreg,nsub))
if (nsub.gt.1) then
   allocate(Fit(nvar,9,nsub,nreg))
else
   allocate(Fit(nvar,9,nlev,nreg))
end if
Fit=0.0
open(11,file=trim(filen),form='unformatted',convert='little_endian')
coun=0
!do ib=1,2
do ivar=1,nvar
   nstat=6
   if (ivar.eq.3) nstat=9
   nlev2=nlev
   if ((nlev.eq.21).and.(nvar.eq.5).and.(ivar.eq.5)) nlev2=1
   do istat=1,nstat
      do ilev=1,nlev2
         read(11) gdata
         Fit(ivar,istat,ilev,1:nreg)=gdata(1:nreg,1)
         if (nsub>1)  Fit(ivar,istat,2,1:nreg)=gdata(1:nreg,2)
      end do
   end do
end do
!end do
close(11)
fits=Fit
deallocate(Fit, gdata)
end subroutine read_fits
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine get_filename(T,ext, filename)
implicit none
integer,intent(in):: T
character(6), intent(in)::ext
character(10),intent(out):: filename
real:: tem
integer:: t1i,t2i,t3i,t4i
character(1)::t1,t2,t3,t4
integer, parameter::one=1
tem=T/1000
t1i=floor(tem)
tem=(T-1000*t1i)/100
t2i=floor(tem)
tem=(T-1000*t1i-100*t2i)/10
t3i=floor(tem)
t4i=T-1000*t1i-100*t2i-10*t3i
t1=ACHAR(t1i+48)
t2=ACHAR(t2i+48)
t3=ACHAR(t3i+48)
t4=ACHAR(t4i+48)
filename(1:6)=ext

filename(7:7)=t1
filename(8:8)=t2
filename(9:9)=t3
filename(10:10)=t4
end subroutine get_filename

end program extract
