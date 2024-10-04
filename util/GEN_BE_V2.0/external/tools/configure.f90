

module configure

!---------------------------------------------------------------------- 
! Purpose: read a unique namelist to drive all the stages of gen_be
!          control the user choices 
!
! History:
!
! Date     Author & Comment
! -------- ----------------
! dd/mm/yy Gael Descombes
!          Initial version (01/07/2012)
!----------------------------------------------------------------------

implicit none

character (len=32)   :: model, application
character (len=1024) :: file_in
character (len=1024) :: dat_dir
character (len=32)   :: be_method
character (len=10)   :: start_date
character (len=10)   :: end_date
integer :: ne
integer :: interval
logical :: use_mean_ens
logical :: testing_eofs, use_global_eofs
logical :: data_on_levels
integer, dimension(6) :: cut

character (len=3)     :: cne                       ! Ensemble size.
character (len=3)     :: ce                        ! Member index -> character.

!gen_be_cv
integer :: nb_cv
integer :: fft_method
character (len=32) :: masscv
character (len=32) :: balpres
logical :: nobaldiv
integer :: cv_options
integer :: num_passes
integer :: rf_scale
logical :: use_chol_reg

!gen_be_bin
integer :: bin_type
real(kind=8) :: lat_min, lat_max, binwidth_lat, hgt_min, hgt_max, binwidth_hgt
integer :: N_holm_bins,holm_reference

real(8) ::  qrain_th_low = 0.0001
real(8) ::  qrain_th_high = 0.01

!gen_be_lenscale
integer :: stride
integer :: ls_method
integer :: vert_ls_method
character (len=4) :: horizfunct
character (len=6) :: horizvar
character*8       :: uh_method = 'scale   '                 ! Uh_method (power, scale)
integer           :: n_smth_ls                  ! Number of smoothing for
logical           :: use_med_ls
logical           :: use_global_bin

!gen_be spectral
integer :: k_glb
character*20 :: grid_type 
logical :: testing_spectral
logical :: limited_area 
character*10 :: be_area


integer             :: n_smth_sl = 0
real              :: spike_tolerance = 1.5

!!!!!
integer :: io_status
integer, parameter :: nMaxvar = 10
integer, dimension(nMaxvar) :: covar1, covar2, covar3, covar4, covar5 
integer, dimension(nMaxvar) :: covar6, covar7, covar8, covar9, covar10 
integer, dimension(nMaxvar) :: ncovar, nvarce1d, ncovar_row 
integer, dimension(nMaxvar,nMaxvar) :: covar_ID, covar_ID2 
character (len=32), dimension(nMaxvar) :: cv_list, cv_listu, tmp_list,  tmp_list2 
!character (len=64)(nMaxvar) :: list_tmp, tmp_list, tmp_list2
character (len=1024), dimension(nMaxvar) :: filename_list
integer, dimension(nMaxvar) :: varcebin2d_dim_list, varcebin_dim_list, vardim_list, vardim_list2, vardim_list3
integer :: nVar1d, nVar2d, nVar3d, nVar, ii, jj, kk, vv
integer :: Dim1, Dim2, Dim3
!!
integer, parameter    :: nrange = 50               ! Range to search for efficient FFT.
real   , parameter    :: rsmall = 10.0e-30          ! small number to assign with hydrometeor
integer :: ncovar_tot

! dynamical mask
logical :: dyn_mask = .false.
real(kind=8) :: value0

namelist /gen_be_info/ model, &
                   application, &
                   be_method, &
                   ne, &
                   cut, &
                   use_mean_ens, &
                   start_date, &
                   end_date, &
                   interval, &
                   testing_eofs
       namelist /gen_be_cv/ nb_cv, &
                   cv_list, &
                   fft_method, &
                   covar1, &
                   covar2, &
                   covar3, &
                   covar4, &
                   covar5, &
                   covar6, &
                   covar7, &
                   covar8, &
                   covar9, &
                   covar10, &
                   use_chol_reg
      namelist /gen_be_bin/ bin_type, &
                   lat_min, &
                   lat_max, &
                   binwidth_lat, &
                   hgt_min, &
                   hgt_max, &
                   binwidth_hgt
      namelist /gen_be_lenscale/ data_on_levels, &
                   vert_ls_method, &
                   ls_method, &
                   use_med_ls, &
                   horizvar, &
                   horizfunct, &
                   stride, &
                   n_smth_ls, &
                   use_global_bin
 



contains


   subroutine read_varname(varname, vardim_list0)

      character (len=*), intent(in) :: varname
      integer, intent(inout) :: vardim_list0
      integer, parameter :: NVARMAX = 41
      character (len=10), dimension(1:NVARMAX) :: varname_all
      integer, dimension(1:NVARMAX) :: vardim_all
      character (len=10) :: varname_u
      integer :: ii, jj

      varname_all(1) = 'psi'
      varname_all(2) = 'chi'
      varname_all(3) = 'vor'
      varname_all(4) = 'div'
      varname_all(5) = 'u'
      varname_all(6) = 'v'
      varname_all(7) = 't'
      varname_all(8) = 'tv'
      varname_all(9) = 'rh'
      varname_all(10) = 'qs'
      varname_all(11) = 'qcloud'
      varname_all(12) = 'qice'
      varname_all(13) = 'qrain'
      varname_all(14) = 'qsnow'
      varname_all(15) = 'qcwall'
      varname_all(16) = 'qgraup'
      varname_all(17) = 'dust_1'
      varname_all(18) = 'dust_2'
      varname_all(19) = 'dust_3'
      varname_all(20) = 'dust_4'
      varname_all(21) = 'p25'
      varname_all(22) = 'p10'
      varname_all(23) = 'oc1'
      varname_all(24) = 'oc2'
      varname_all(25) = 'bc1'
      varname_all(26) = 'bc2'
      varname_all(27) = 'seas_1'
      varname_all(28) = 'seas_2'
      varname_all(29) = 'seas_3'
      varname_all(30) = 'seas_4'
      varname_all(31) = 'o3'
      varname_all(32) = 'no2'
      varname_all(33) = 'hno3'
      varname_all(34) = 'so2'
      varname_all(35) = 'co'
      varname_all(36) = 'ch4'
      varname_all(37) = 'ho'
      varname_all(38) = 'no'
      varname_all(39) = 'ch4'
      vardim_all(1:39) = 3

      varname_all(40) = 'ps'
      varname_all(41) = 'vis'
      vardim_all(40:41) = 2 

      jj = 0
      !! need to be read in the external ascii file, next dev GD !!
      do ii = 1, NVARMAX
         varname_u = trim(varname_all(ii))//'_u'
         if ((trim(varname_all(ii))==trim(varname)).or.(varname_u==trim(varname)) ) then 
            vardim_list0 = vardim_all(ii)
            write(*,*)'variable ',trim(varname_all(ii)),vardim_list0
            jj = jj + 1
         end if
      end do

      if ( jj .eq.0 ) then
         write(*,*) 'ERROR control variable not define in subroutine read_varname',varname
              stop
      end if

   end subroutine read_varname


   subroutine read_vardim(cv_list,vardim_list,nVar,nVar1d,nVar2d,nVar3d)

      integer , intent(in) :: nVar
      integer , intent(inout):: nVar1d, nVar2d, nVar3d
      character (len=*), dimension(:), intent(in) :: cv_list
      integer :: vardim_list0
      integer, dimension(:), intent(inout) :: vardim_list
      integer :: ii

      nVar1d = 0
      nVar2d = 0
      nVar3d = 0
      vardim_list = -1 
      do ii = 1, nVar
         call read_varname(cv_list(ii), vardim_list(ii))
         if (vardim_list(ii) .eq. 1) then
            nVar1d = nVar1d + 1
         else if (vardim_list(ii) .eq. 2) then 
            nVar2d = nVar2d + 1
         else if (vardim_list(ii) .eq. 3) then
            nVar3d = nVar3d + 1
         end if           
      end do

   end subroutine read_vardim


   subroutine read_namelist(nVar2d, nVar3d)

       implicit none
       integer :: config_restart_time
       integer :: funit
       integer :: nVar2d, nVar3d
       integer :: nVar1d

       funit = 21
       open(funit,file='namelist.input',status='old',form='formatted',iostat = io_status, action='read')
       if ( io_status .ne. 0 ) then
          write(*,*) 'ERROR OPENING namelist.input'
          stop
       end if

       ! Set default values for namelist options
       start_date = '2004030312'
       end_date = '2004033112'
       interval = 24
       be_method = 'NMC'
       ne = 1 
       bin_type = 5  ! 0 = Every pt, 1 = x direction, 2 = latitude, ....
       lat_min = -90.0
       lat_max = 90.0
       binwidth_lat = 10.0
       hgt_min = 0.0
       hgt_max = 20000.0
       binwidth_hgt = 1000.0
       N_holm_bins = 1
       testing_eofs = .true.
       masscv = ""
       balpres = ""
       nobaldiv = .false.
       use_global_eofs = .true.
       data_on_levels = .false.
       dat_dir = '.'
       holm_reference = 0
       cut = 0

       write(6,*) "reading namelist.input"
      
       open(funit,file='namelist.input',status='old',form='formatted',iostat = io_status)
       if ( io_status .ne. 0 ) then
          write(*,*) 'ERROR OPENING namelist.input'
          stop
       end if

       write(*,*) 'after read '

       read(funit,gen_be_info)
       read(funit,gen_be_cv)
       read(funit,gen_be_bin)
       read(funit,gen_be_lenscale)
!       read(funit,gen_be_spectral)
       close(funit)
       file_in=start_date(1:10)
       n_smth_sl = n_smth_ls
  
       call read_vardim(cv_list,vardim_list,nb_cv,nVar1d, nVar2d,nVar3d)
       nVar = nb_cv

       if ( be_method /= "ENS" ) ne = 1 
       write(UNIT=ce,FMT='(i3.3)') 1
       write(UNIT=cne,FMT='(i3.3)') ne

       ! Binning GSI uses latitude-dependent binning (2), but set to domain average (5)
       if( trim(application(1:3)) == "GSI" .or. trim(application(1:3)) == "gsi") then
          !bin_type = 5
          data_on_levels = .true.
          use_global_eofs = .false.
          !write(*,*)'Enforce option bin_type = 5, data_on_levels true for GSI Applications'
          application='GSI'
       else if(  trim(application(1:5)) == "WRFDA" .or. trim(application(1:5)) == "wrfda") then
          !data_on_levels = .false.
          write(*,*)'Enforce data_on_levels false for GSI Applications'
          application='WRFDA'
       else
          write(*,*)'this application is unknown'
       end if
       !/covar_ID2 binning 7 enforce 
       if (bin_type .eq. 7) then
          use_mean_ens = .false.
          write(*,*)'The binning option 7 enforce use_mean_ens = .false.'
       end if

       if (nvar > nMaxvar) then
          write(*,*)'Error the number of control variables is higher than the max possible : ',nMaxvar
          write(*,*)'change the value in configure.f90'
          stop
       end if
      

       ! initialisation of covar
       !covar_ID(1,:) = covar1(:)
       covar_ID(1,:) = -1 
       covar_ID(2,:) = covar2(:)
       covar_ID(3,:) = covar3(:)
       covar_ID(4,:) = covar4(:)
       covar_ID(5,:) = covar5(:)
       covar_ID(6,:) = covar6(:)
       covar_ID(7,:) = covar7(:)
       covar_ID(8,:) = covar8(:)
       covar_ID(9,:) = covar9(:)
       covar_ID(10,:)= covar10(:)

       covar_ID2 = covar_ID

       ! prepare for dimension purpose in alloc section
       do ii=1, nMaxvar
           do jj=1, ii-1
              if (  covar_ID(ii,jj) > 0  ) then
                 covar_ID(ii,jj)  = min0(vardim_list(ii),vardim_list(jj))
                 if ( covar_ID2(ii,jj) .ne. 2 ) then
                 !    covar_ID2(ii,jj) = 2 
                 !else
                     covar_ID2(ii,jj) = covar_ID(ii,jj)
                 end if
              !write(*,*)'',trim(cv_list(ii)),trim(cv_list(jj))
              !write(*,*)'covar_ID(ii,jj)  : ',covar_ID(ii,jj)    
              !write(*,*)'covar_ID2(ii,jj) : ',covar_ID2(ii,jj)    
              !write(*,*)''
              end if
           end do
       end do
       !write(*,*)'covar_ID2 ',covar_ID2(:,1)
       !write(*,*)'covar_ID  ',covar_ID(:,1)

    
       if ( (trim(cv_list(1)).eq.'psi').and.( trim(cv_list(2)).eq.'chi') ) then 
          covar_ID(2,1)  = 1 ! bin3d for psi       
          covar_ID2(2,1) = 1 ! bin3d for psi 
          write(*,*)'Psi and Chi covar_ID = ',covar_ID(2,1)
       !else
       !   write(*,*)'',trim(cv_list(1)),trim(cv_list(2)),covar_ID(2,1)
       end if

       ! indice per ligne to know if the unbalanced part will be calculated 
       ncovar = 0
       ncovar_tot = 0
       do ii=2, nMaxvar
           do jj=1, ii-1
              if (  covar_ID(ii,jj) > 0  ) then
                 ncovar(ii) = ncovar(ii) + 1
                 ncovar_tot = ncovar_tot + 1
              else
                 covar_ID(ii,jj) = -1
                 covar_ID2(ii,jj) = -1
              end if
           end do
       end do
       write(*,*)'ncovar_tot ',ncovar_tot

       ! indice per row to know if there is regcoeff or not
       ncovar_row = 0
       do jj=1,nvar
          do ii=2, nvar
             if (  covar_ID(ii,jj) > 0  ) then
                ncovar_row(jj) = ncovar_row(jj) + 1
             end if
          end do
       end do


       do vv=1, nvar
          if ( ncovar(vv) == 0 ) then
            cv_listu(vv) = trim(cv_list(vv))
          else
            cv_listu(vv) = trim(cv_list(vv))//'_u'
          end if
       end do
       write(*,*)'cv_listu ',cv_listu(:)

       ! dimension of variance that we compute by default
       ! 3d for 3d variable (num_bins2d,Dim3,Dim3)
       ! 1d for 2d variable (num_bins2d)
       do ii=1, nMaxvar
          varcebin2d_dim_list(ii) = vardim_list(ii) 
          if ( ( varcebin2d_dim_list(ii) == 1 ).or.( varcebin2d_dim_list(ii) == 2 ) ) then
              varcebin2d_dim_list(ii) = 1  
          end if
       end do

       ! number of variance 1d that we need to compute
       ! using bins. The variable is 3d, the variance 1d 
       ! example : psi(Dim1,Dim2,Dim3) --> varce_psi(num_bins)
       varcebin_dim_list = 0
       do jj=1, nMaxvar
           do ii=1, nMaxvar
              if (  covar_ID(ii,jj) == 1  ) then
                 varcebin_dim_list = 1
              end if
           end do
       end do


   end subroutine read_namelist


   subroutine write_ascii_file(filename, filename_list, nvar)

       implicit none
       character(len=*), intent(in) :: filename
       character(len=*), dimension(:), intent(in) :: filename_list
       integer, intent(in) :: nvar
       integer :: ll, funit

       funit = 54
       write(*,*)'write ascii file',filename
       open(funit,file=trim(filename),status='unknown',form='formatted')
       write (funit,fmt='(i3)')nvar
       do ll = 1, nvar
          write (funit,fmt='(a30)')trim(filename_list(ll))
       end do
       close(funit)

   end subroutine  write_ascii_file



end module configure
