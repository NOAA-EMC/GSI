  program save_wrf_mass_ensperts
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    get_wrf_mass_ensperts  read arw model ensemble members
  !   prgmmr: Ming Hu          org: GSD                 date: 2018-07-11
  !
  ! abstract: read ensemble members from the arw model in netcdf format, for use
  !           with hybrid ensemble option.  ensemble spread is also written out as
  !           a byproduct for diagnostic purposes.
  !
  !
  ! program history log:
  !   2018-07-11  Ming Hu, initial documentation
  !
  !   input argument list:
  !
  !   output argument list:
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm RS/6000 SP
  !
  !$$$ end documentation block
  
      use kinds, only: r_kind,i_kind,r_single
      use constants, only: init_constants
      use general_read_wrf_mass, only: general_read_wrf_mass_save
      use general_read_wrf_mass, only: general_read_wrf_mass_dim_eta
      use general_read_wrf_mass, only: cal_ensperts
  
      implicit none
      integer :: n_ens
      integer :: initialtime,fcsthh
      character(255) :: constfile
      namelist/setup/ n_ens,initialtime,fcsthh,constfile

      integer(i_kind):: n

      character(255) filename
      logical :: if_exist
! 
! 
      call init_constants(.true.)
!
      n_ens=9
      initialtime=2018052918
      fcsthh=18
      constfile='wrfout_d01_const'

      inquire(file='namelist_enspert', EXIST=if_exist )
      if(if_exist) then
          open(10,file='namelist_enspert',status='old')
            read(10,setup)
          close(10)
          write(*,*) 'Namelist setup are:'
          write(*,setup)
        else
          write(*,*) 'No namelist file exist'
          stop 123
      endif

!
      inquire(file=trim(constfile), EXIST=if_exist) 
      if(if_exist) then
         call general_read_wrf_mass_dim_eta(constfile)
      else
         write(*,*) 'constant file does not exist ',trim(constfile)
         stop 1234
      endif
!
      open(10,file='filelist03',form='formatted',err=30)

  ! LOOP OVER ENSEMBLE MEMBERS 
         do n=1,n_ens
  !
  ! DEFINE INPUT FILE NAME
            read(10,'(a)',err=20,end=20) filename
  ! READ ENEMBLE MEMBERS DATA
            inquire(file=trim(filename), EXIST=if_exist)    
            if(if_exist) then
               write(6,'(a,a)') 'CALL READ_WRF_MASS_ENSPERTS FOR ENS DATA : ',trim(filename)
               call general_read_wrf_mass_save(filename,.false.,n)
            else
               write(*,*) 'ensemble file does not exist ',trim(filename)
               stop 1234
            endif
  
         enddo
  
      close(10)
  ! calculate perturbation and mean
      call cal_ensperts(n_ens,initialtime,fcsthh)

   stop
30 write(6,*) 'get_wrf_mass_ensperts_netcdf: open filelist failed '
   call stop2(555)
20 write(6,*) 'get_wrf_mass_ensperts_netcdf: read WRF-ARW ens failed ',n
   call stop2(555)

end program
