

module io_input_model

!---------------------------------------------------------------------- 
! Purpose: Handle meteorological fields of different models, convert
!          them in control variable (mainly stage0) 
!
! History:
!
! Date     Author & Comment
! -------- ----------------
! dd/mm/yy Gael Descombes
!          Initial version (01/07/2012)
!----------------------------------------------------------------------

   use variable_types 
   use configure
   use da_gen_be

   contains

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !! Initialize framework
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  
   subroutine da_get_field8( input_file, var, field_dims, dim1, dim2, dim3, k,field)

      implicit none

      character(len=200), intent(in)  :: input_file       ! 1 file nane.
      character(len=12),  intent(in)  :: var              ! Variable to search for.
      integer,            intent(in)  :: field_dims       ! # Dimensions of field.
      integer,            intent(in)  :: dim1             ! Dimension 1 of field.
      integer,            intent(in)  :: dim2             ! Dimension 2 of field.
      integer,            intent(in)  :: dim3             ! Dimension 3 of field.
      integer,            intent(in)  :: k                ! Vertical index.
      real*8,             intent(out) :: field(1:dim1,1:dim2) ! Output field
      real                            :: field_in(1:dim1,1:dim2) ! Output field

      call da_get_field( input_file, var, field_dims, dim1, dim2, dim3, k, field_in )
      field = field_in

   end subroutine da_get_field8

   subroutine init_mesh_stage0(mesh, model, filin)

      implicit none

      type (mesh_type), intent(inout), pointer :: mesh
      character (len=32), intent(in)    :: model
      character (len=1024), intent(in)  :: filin
      character (len=1024)  :: input_file
      character (len=12)    :: var
      real(kind=8) ,  allocatable, dimension(:,:) :: tmp2d
      type (field2DReal) :: mapfac_u, mapfac_v
      type (field1DReal) :: znu

      if ( trim(model) == 'WRF' ) then

          write(*,*)'WRF model' 
          ! allocate the array
          call allocate_field(mesh % mapfac_u,'mapfac_u',mesh%Dim1+1, mesh%Dim2)
          call allocate_field(mesh % mapfac_v,'mapfac_v',mesh%Dim1, mesh%Dim2+1)
          call allocate_field(mesh % znu,'znu',mesh%Dim3)
          ! initialize the array
          ! got warnings when it compiles for the call da_get_field
          write(*,*)'read XLAT' 
          input_file = trim(filin)//'.e001'         
          var = "XLAT"
          call da_get_field8( input_file, var, 2, mesh%Dim1, mesh%Dim2, 1, 1, mesh%lat%array )
          var = "XLONG"
          call da_get_field8( input_file, var, 2, mesh%Dim1, mesh%Dim2, 1, 1, mesh%lon%array )
          var = "MAPFAC_M"
          call da_get_field8( input_file, var, 2, mesh%Dim1, mesh%Dim2, 1, 1, mesh%mapfac_m%array )
          var = "MAPFAC_U"
          call da_get_field8( input_file, var, 2, mesh%Dim1+1, mesh%Dim2, 1, 1, mesh%mapfac_u%array )
          var = "MAPFAC_V"
          call da_get_field8( input_file, var, 2, mesh%Dim1, mesh%Dim2+1, 1, 1, mesh%mapfac_v%array )
          var = "ZNU"
          allocate( tmp2d(mesh%Dim3,1) )
          call da_get_field8(input_file, var, 1, mesh%Dim3, 1, 1, 1,tmp2d )
          mesh%znu%array= tmp2d(:,1)
          deallocate(tmp2d) 
          call da_get_height( input_file, mesh%Dim1, mesh%Dim2, mesh%Dim3, mesh%hgt%array )

      end if

   end subroutine init_mesh_stage0


   subroutine finalize_mesh(mesh, model)

      implicit none

      type (mesh_type), intent(inout), pointer :: mesh
      character (len=32), intent(in), optional :: model

      if (present(model)) then
          !! can be only for WRF, if yes be careful with init_mesh in io_input module
          call deallocate_field(mesh % mapfac_u)
          call deallocate_field(mesh % mapfac_v)
          call deallocate_field(mesh % znu)
      end if

      call deallocate_mesh(mesh)

   end subroutine finalize_mesh


   subroutine subset_mesh_bc( mesh_in, mesh_out, cut, model)

      implicit none

      type (mesh_type), intent(inout) ,pointer :: mesh_in
      type (mesh_type), intent(inout) ,pointer :: mesh_out
      integer , dimension(6) ,intent(in) :: cut
      character (len=32), intent(in)    :: model
      integer :: vv
      integer :: Dim1_bc1, Dim1_bc2, Dim2_bc1, Dim2_bc2, Dim3_bc1, Dim3_bc2
      integer :: Dim1_out, Dim2_out, Dim3_out

      Dim1_bc1 =  1 + cut(1)
      Dim1_bc2 =  mesh_in % Dim1 - cut(2)
      Dim2_bc1 =  1 + cut(3)
      Dim2_bc2 =  mesh_in % Dim2 - cut(4)
      Dim3_bc1 =  1 + cut(5)
      Dim3_bc2 =  mesh_in % Dim3 - cut(6)

      Dim1_out = mesh_in % Dim1 -cut(1) -cut(2)
      Dim2_out = mesh_in % Dim2 -cut(3) -cut(4)
      Dim3_out = mesh_in % Dim3 -cut(5) -cut(6)

      mesh_out % Dim1 = Dim1_out
      mesh_out % Dim2 = Dim2_out
      mesh_out % Dim3 = Dim3_out
      mesh_out % lat % array(1:Dim1_out,1:Dim2_out) = mesh_in % lat % array(Dim1_bc1:Dim1_bc2, Dim2_bc1:Dim2_bc2)
      mesh_out % lon % array(1:Dim1_out,1:Dim2_out) = mesh_in % lon % array(Dim1_bc1:Dim1_bc2, Dim2_bc1:Dim2_bc2)
      mesh_out % mapfac_m % array(1:Dim1_out,1:Dim2_out) = mesh_in % mapfac_m % array(Dim1_bc1:Dim1_bc2, Dim2_bc1:Dim2_bc2)
      mesh_out % hgt % array(1:Dim1_out,1:Dim2_out,1:Dim3_out) = mesh_in % hgt % array(Dim1_bc1:Dim1_bc2, Dim2_bc1:Dim2_bc2, Dim3_bc1:Dim3_bc2)
      mesh_out % ds % scalar = mesh_in % ds % scalar
     
      if ( trim(model) == 'WRF' ) then

         mesh_out % Dim1u = Dim1_out + 1
         mesh_out % Dim2u = Dim2_out
         mesh_out % Dim1v = Dim1_out
         mesh_out % Dim2v = Dim2_out + 1
         mesh_in  % Dim1u = mesh_in % Dim1 + 1
         mesh_in  % Dim2u = mesh_in % Dim2
         mesh_in  % Dim1v = mesh_in % Dim1
         mesh_in  % Dim2v = mesh_in % Dim2 + 1   

         write(*,*)'Allocate specifics WRF in subset_mesh_bc'
         call allocate_field(mesh_out % mapfac_u,'mapfac_u',Dim1_out+1, Dim2_out)
         call allocate_field(mesh_out % mapfac_v,'mapfac_v',Dim1_out,Dim2_out+1)
         call allocate_field(mesh_out % znu ,'znu',Dim3_out)
         mesh_out % mapfac_u % array(1:Dim1_out+1,1:Dim2_out) = mesh_in % mapfac_u % array(Dim1_bc1:Dim1_bc2+1, Dim2_bc1:Dim2_bc2 )
         mesh_out % mapfac_v % array(1:Dim1_out,1:Dim2_out+1) = mesh_in % mapfac_v % array(Dim1_bc1:Dim1_bc2, Dim2_bc1:Dim2_bc2+1 )
         mesh_out % znu % array(1:Dim3_out) = mesh_in % znu % array(Dim3_bc1:Dim3_bc2)
      end if

   end subroutine subset_mesh_bc


   subroutine subset_state_bc( state_in, state_out, mesh_in, cut)

     implicit none

     type (state_type), intent(in) ,pointer :: state_in
     type (state_type), intent(inout) ,pointer :: state_out
     type (mesh_type), intent(in) ,pointer :: mesh_in
     integer , dimension(6) ,intent(in) :: cut
     integer :: vv
     integer :: Dim1_bc1, Dim1_bc2, Dim2_bc1, Dim2_bc2, Dim3_bc1, Dim3_bc2
     integer :: Dim1_out, Dim2_out, Dim3_out

     Dim1_bc1 =  1 + cut(1)
     Dim1_bc2 =  mesh_in % Dim1 - cut(2)
     Dim2_bc1 =  1 + cut(3)
     Dim2_bc2 =  mesh_in % Dim2 - cut(4)
     Dim3_bc1 =  1 + cut(5)
     Dim3_bc2 =  mesh_in % Dim3 - cut(6)

     Dim1_out = mesh_in % Dim1 -cut(1) -cut(2)
     Dim2_out = mesh_in % Dim2 -cut(3) -cut(4)
     Dim3_out = mesh_in % Dim3 -cut(5) -cut(6)

     do vv = 1, state_in%nvar

        if (state_in%num(vv)%IDdim == 2) then
           state_out%num(vv)%field%field2d%array(1:Dim1_out,1:Dim2_out) = &
              state_in%num(vv)%field%field2d%array(Dim1_bc1:Dim1_bc2, Dim2_bc1:Dim2_bc2)
        else if (state_in%num(vv)%IDdim == 3) then
           state_out%num(vv)%field%field3d%array(1:Dim1_out,1:Dim2_out,1:Dim3_out) = &
               state_in%num(vv)%field%field3d%array(Dim1_bc1:Dim1_bc2, Dim2_bc1:Dim2_bc2, Dim3_bc1:Dim3_bc2)
        end if

     end do

   end subroutine subset_state_bc


   subroutine subset_field_bc( field_in, field_out, mesh_in, cut)

     implicit none
     type (field3DReal), intent(in), pointer ::  field_in
     type (field3DReal), intent(inout), pointer ::  field_out
     type (mesh_type), intent(in) ,pointer :: mesh_in
     integer , dimension(6) ,intent(in) :: cut
     integer :: Dim1_bc1, Dim1_bc2, Dim2_bc1, Dim2_bc2, Dim3_bc1, Dim3_bc2
     integer :: Dim1_out, Dim2_out, Dim3_out
  
     Dim1_bc1 =  1 + cut(1)
     Dim1_bc2 =  mesh_in % Dim1 - cut(2)
     Dim2_bc1 =  1 + cut(3)
     Dim2_bc2 =  mesh_in % Dim2 - cut(4)
     Dim3_bc1 =  1 + cut(5)
     Dim3_bc2 =  mesh_in % Dim3 - cut(6)

     Dim1_out = mesh_in % Dim1 -cut(1) -cut(2)
     Dim2_out = mesh_in % Dim2 -cut(3) -cut(4)
     Dim3_out = mesh_in % Dim3 -cut(5) -cut(6)

     field_out%array(1:Dim1_out,1:Dim2_out,1:Dim3_out) = field_in%array(Dim1_bc1:Dim1_bc2, Dim2_bc1:Dim2_bc2, Dim3_bc1:Dim3_bc2)
 

   end subroutine subset_field_bc


   subroutine get_vardim_stage0(model, filin, Dim1, Dim2, Dim3, ds)
  
      implicit none  
      character (len=1024), intent(in)  :: filin 
      character (len=1024)   :: input_file
      character (len=32)     :: model
      character (len=12)     :: var
      real(kind=8), intent(inout)    :: ds    
      real    :: ds0    
      integer, intent(inout) ::  Dim1, Dim2, Dim3
 
      input_file = trim(filin)//'.e001'
      if ( trim(model) == 'WRF') then
         var = "T"
         call da_stage0_initialize(input_file, var, Dim1, Dim2, Dim3, ds0)
         write(*,*)'get_vardim_stage0 Dim1, Dim2, Dim3 : ',Dim1, Dim2, Dim3
         ds = ds0
      end if  

   end subroutine get_vardim_stage0

   subroutine framework_init_stage0(domain)

      implicit none

      type (domain_type), intent(inout), pointer :: domain
      integer :: Dim1, Dim2, Dim3, nVar2d, nVar3d
      !integer :: ii, iVar2d, iVar3d
      real(kind=8)    :: ds    
      nVar2d = 0
      nVar3d = 0

      call read_namelist(nVar2d,nVar3d)

      call get_vardim_stage0(model, file_in, Dim1, Dim2, Dim3, ds)

      nvar = nVar2d + nVar3d
      call allocate_domain(domain, Dim1, Dim2, Dim3, nvar, cv_list, vardim_list)
      domain % model = model
      domain % application = application

      !call init_variable(domain%state, cv_list)
      domain % state % date = start_date(1:10)
      write (*,*)'framework_init_stage0 ',domain % state % date

      call init_mesh_stage0(domain%mesh, model, file_in)
      write (*,*)'after init_mesh'
      domain % mesh % ds % scalar = ds

   end subroutine framework_init_stage0


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! read field, convert to state vector 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine convert_uv2state(domain, input_file, fft_method)

     use da_change_wind_variables
     use da_fft_initialize
     !use da_control, only : num_fft_factors

     implicit none

     type (domain_type), intent(inout), pointer :: domain
     character (len=1024), intent(in) :: input_file
     integer, intent(in) ::  fft_method
     character (len=12)    :: var
     real, allocatable     :: u(:,:), v(:,:)
     real, allocatable     :: vor2d(:,:),div2d (:,:)
     real, allocatable     :: psi2d(:,:),chi2d (:,:)
     integer :: kk, ii, jj, iVar3d
     real :: ds
     integer :: poisson_method, ktest, n1, n2, n1s, n2s
     logical :: test_inverse

     real, allocatable     :: trigs1(:)                 ! FFT trig functions.
     real, allocatable     :: trigs2(:)                 ! FFT trig functions.
     real, allocatable     :: fft_coeffs(:,:)           ! FFT coefficients.
     real, allocatable     :: trigs1s(:)                ! FFT trig functions.
     real, allocatable     :: trigs2s(:)                ! FFT trig functions.
     real, allocatable     :: fft_coeffss(:,:)          ! FFT coefficients.
     real, allocatable     :: mapfac_m(:,:), mapfac_u(:,:), mapfac_v(:,:)    
 
     integer               :: ifax1(1:num_fft_factors)  ! FFT factors.
     integer               :: ifax2(1:num_fft_factors)  ! FFT factors.
     integer               :: ifax1s(1:num_fft_factors) ! FFT factors.
     integer               :: ifax2s(1:num_fft_factors) ! FFT factors.

     integer :: indice_psi, indice_chi, indice_vor, indice_div, indice_u, indice_v

     indice_psi =  get_state_indice(domain%state,"psi")
     indice_chi =  get_state_indice(domain%state,"chi")
     indice_vor =  get_state_indice(domain%state,"vor")
     indice_div =  get_state_indice(domain%state,"div")
     indice_u   =  get_state_indice(domain%state,"u")
     indice_v   =  get_state_indice(domain%state,"v")

     poisson_method = 1
     test_inverse = .false.
     ktest = 1
     !fft_method = 2
     ds = domain%mesh%ds%scalar
     write(*,*)'ds = ',ds

     if ( (indice_psi/=0).or.(indice_chi/=0).or.(indice_vor/=0).or.(indice_div/=0).or.(indice_u/=0).or.(indice_v/=0) ) then

     if (trim(domain%model)=='WRF') then

        !  Initialize FFT coefficients:
        if ( poisson_method == 1 ) then
           call da_fft_initialize1( domain%mesh%Dim1, domain%mesh%Dim2, n1, n2, ifax1, ifax2 )
           call da_fft_initialize1( domain%mesh%Dim1+1, domain%mesh%Dim2+1, n1s, n2s, ifax1s, ifax2s )
           allocate( trigs1(1:3*n1) )
           allocate( trigs2(1:3*n2) )
           allocate( fft_coeffs(1:n1+1,1:n2+1) ) 
           call da_fft_initialize2( n1, n2, ds, trigs1, trigs2, fft_coeffs )
           !call da_fft_initialize2( 2, 2, 3., trigs1, trigs2, fft_coeffs )
           allocate( trigs1s(1:3*n1s) )
           allocate( trigs2s(1:3*n2s) )
           allocate( fft_coeffss(1:n1s+1,1:n2s+1) )
           call da_fft_initialize2( n1s, n2s, ds, trigs1s, trigs2s, fft_coeffss )
        end if
        write(*,*)'n1 n2 n1s n1s',n1, n2, n1s, n2s
        write(*,*)'num_fft_factors ',num_fft_factors
        write(*,*)'fft_method',fft_method
        write(*,*)'trigs1 trigs1s',trigs1(10), trigs1s(10)
        write(*,*)'fft_coeffs fft_coeffss ',fft_coeffs(1,10), fft_coeffss(10,10)
        write(*,*)'ifax1, ifax2 ifax1s, ifax2s ',ifax1(10), ifax2(10), ifax1s(10), ifax2s(10)

        ! allocate temporary arrays, dimensions can depend of the grid model
        allocate( u(domain%mesh%Dim1+1,domain%mesh%Dim2) )
        allocate( v(domain%mesh%Dim1,domain%mesh%Dim2+1) )
        allocate( vor2d(domain%mesh%Dim1+1,domain%mesh%Dim2+1) )
        allocate( div2d(domain%mesh%Dim1,domain%mesh%Dim2) )
        allocate( psi2d(domain%mesh%Dim1+1,domain%mesh%Dim2+1) )
        allocate( chi2d(domain%mesh%Dim1,domain%mesh%Dim2) )
        allocate( mapfac_u(domain%mesh%Dim1+1,domain%mesh%Dim2) )
        allocate( mapfac_v(domain%mesh%Dim1,domain%mesh%Dim2+1) )
        allocate( mapfac_m(domain%mesh%Dim1,domain%mesh%Dim2) )
        mapfac_m(:,:) = domain%mesh%mapfac_m%array(:,:)
        mapfac_u(:,:) = domain%mesh%mapfac_u%array(:,:)
        mapfac_v(:,:) = domain%mesh%mapfac_v%array(:,:)

        do kk = 1, domain%mesh%Dim3
    
           ! load U, V component
           var = "U"
           call da_get_field( input_file, var, 3, domain%mesh%Dim1+1, domain%mesh%Dim2, domain%mesh%Dim3, kk, u )
           var = "V"
           call da_get_field( input_file, var, 3, domain%mesh%Dim1, domain%mesh%Dim2+1, domain%mesh%Dim3, kk, v )

           ! Calculate vor2dticity (in center of mass grid on WRF's Arakawa C-grid):
           if ((indice_psi/=0).or.(indice_chi/=0).or.(indice_vor/=0).or.(indice_div/=0) ) then 
              call da_uv_to_vor_c( domain%mesh%Dim1, domain%mesh%Dim2, ds, &
                 mapfac_m, mapfac_u, mapfac_v, u, v, vor2d )
              ! Calculate div2dergence (at mass pts. on WRF's Arakawa C-grid):
              call da_uv_to_div_c( domain%mesh%Dim1, domain%mesh%Dim2, ds, &
                  mapfac_m, mapfac_u, mapfac_v, u, v, div2d )
              if ( indice_vor/=0 ) then    
                 domain % state  % num(indice_vor) % field % field3d % array(:,:,kk) =  &
                        0.5*( vor2d(1:domain%mesh%Dim1,1:domain%mesh%Dim2) + vor2d(2:domain%mesh%Dim1,2:domain%mesh%Dim2) )
              end if
              if ( indice_div/=0 ) then 
                 domain % state  % num(indice_div) % field % field3d % array(:,:,kk) = div2d 
              end if
           end if

           ! Calculate streamfunction and potential
           ! Assumes vor2d/div2d converted to Del**2 psi/chi):
           if ( (indice_psi/=0).or.(indice_chi/=0) ) then
              if ( poisson_method == 1 ) then
                 call da_del2a_to_a( domain%mesh%Dim1+1, domain%mesh%Dim2+1, n1s, n2s, fft_method, ifax1s, ifax2s, &
                                trigs1s, trigs2s, fft_coeffss, vor2d, psi2d )
                call da_del2a_to_a( domain%mesh%Dim1, domain%mesh%Dim2, n1, n2, fft_method, ifax1, ifax2, &
                               trigs1, trigs2, fft_coeffs, div2d, chi2d )
              else if ( poisson_method == 2 ) then
                  !call da_sor( dim1s, dim2s, ds, vor, psi2d )
                  !call da_sor( dim1, dim2, ds, div, chi2d )
                  stop 'Unsupported poisson_method'
              end if

              !write(*,*)'test inverse'

              !if ( test_inverse .and. kk == ktest .and. member == 1 ) then
              if ( test_inverse .and. kk == ktest ) then
                 write(*,*) 'test_inverse ktest : ',test_inverse,ktest
                 call da_test_inverse( domain%mesh%Dim1, domain%mesh%Dim3, ds, mapfac_m, &
                                    mapfac_u, mapfac_v, &
                                  n1, n2, fft_method, ifax1, ifax2, trigs1, trigs2, fft_coeffs, &
                                  n1s, n2s, ifax1s, ifax2s, trigs1s, trigs2s, fft_coeffss, &
                                  u, v, psi2d, chi2d )
              
              end if
              ! Interpolate psi to mass pts ready for output:
              if ( indice_psi/=0 ) then
                 do jj = 1, domain%mesh%Dim2
                    do ii = 1, domain%mesh%Dim1
                       domain % state  % num(indice_psi) % field % field3d % array(ii,jj,kk) = & 
                           0.25 * ( psi2d(ii,jj) + psi2d(ii+1,jj) + &
                                     psi2d(ii,jj+1) + psi2d(ii+1,jj+1) )
                    end do
                 end do
                 
              end if
              
              ! rizvi fix for chi Southern zero value boundary
              !chi2d(:,1) = chi2d(:,2)
              !chi2d(:,Dim2) = chi2d(:,Dim2-1)
              !chi2d(1,:) = chi2d(2,:)
              !chi2d(Dim1,:) = chi2d(Dim1-1,:)
              !psi2d(:,1) = psi2d(:,2)
              !psi2d(:,Dim2) = psi2d(:,Dim2-1)
              !psi2d(1,:) = psi2d(2,:)
              !psi2d(Dim1,:) = psi2d(Dim1-1,:)
                                  
              if ( indice_chi/=0 ) then 
                 ! write output for chi
                 domain%state%num(indice_chi)%field%field3d%array(:,:,kk) = chi2d(:,:) 
              end if

           end if ! condition psi , chi

           if  (indice_u/=0) then
              domain%state%num(indice_u)%field%field3d%array(:,:,kk) = &
              0.5*(u(1:domain%mesh%Dim1,:) + u(2:domain%mesh%Dim1+1,:))
           end if

           if  (indice_v/=0) then
              domain%state%num(indice_v)%field%field3d%array(:,:,kk) = &
               0.5*(v(:,1:domain%mesh%Dim2) + v(:,2:domain%mesh%Dim2+1))
           end if

!           if ( kk == 10 ) then
!              write (*,*)' vor = ',vor2d(1,1), vor2d(10,10)
!              write (*,*)' div = ',div2d(1,1), div2d(10,10)
!              write (*,*)' psi = ',psi2d(1,1), psi2d(10,10)
!              write (*,*)' chi = ',chi2d(1,1), chi2d(10,10)
!           end if

       end do !! loop on vertical levels 
    
       deallocate( u )
       deallocate( v )
       deallocate( vor2d )
       deallocate( div2d )
       deallocate( psi2d )
       deallocate( chi2d )
       deallocate( mapfac_u )
       deallocate( mapfac_v )
       deallocate( mapfac_m )

       if ( poisson_method == 1 ) then
          deallocate( trigs1 )
          deallocate( trigs2 )
          deallocate( fft_coeffs )
          deallocate( trigs1s )
          deallocate( trigs2s )
          deallocate( fft_coeffss )
       end if

     
     end if ! condition model

    else
       write(*,*)'No variable of control psi or chi or vor or div has been selected'
    end if

  end subroutine convert_uv2state


  subroutine convert_qcond2state(domain, input_file, rsmall)

     implicit none

     type (domain_type), intent(inout), pointer :: domain
     character (len=1024),intent(in) :: input_file
     real,  intent(in)    :: rsmall ! small number to assign with hydrometeor


     character (len=12)   :: varname, var
     real(kind=8), allocatable    :: qcond(:,:)
     !real, allocatable     :: tmp(:,:)
     integer :: kk
     integer :: indice_qcloud, indice_qrain, indice_qice, indice_qsnow

     indice_qcloud = get_state_indice(domain%state,"qcloud")
     indice_qrain = get_state_indice(domain%state,"qrain")
     indice_qice = get_state_indice(domain%state,"qice")
     indice_qsnow = get_state_indice(domain%state,"qsnow")

     if ( (indice_qcloud/=0).or.(indice_qrain/=0).or.(indice_qice/=0).or.(indice_qsnow/=0) ) then
     
     if (trim(domain%model)=='WRF') then

        ! allocate temporary arrays, dimensions can depend of the grid model
        allocate( qcond(domain%mesh%Dim1, domain%mesh%Dim2) )

        do kk = 1, domain%mesh%Dim3
           
           if ( indice_qcloud /= 0 ) then
              var = "QCLOUD"
              call da_get_field8( input_file, var, 3, domain%mesh%Dim1, domain%mesh%Dim2, domain%mesh%Dim3, kk, qcond )
              where(qcond(:,:).lt.0.0)
                  qcond(:,:)=rsmall
              end where
              domain % state % num(indice_qcloud) % field % field3d % array(:,:,kk) = qcond
           end if

           if ( indice_qrain /= 0 ) then
              var = "QRAIN"
              call da_get_field8( input_file, var, 3, domain%mesh%Dim1, domain%mesh%Dim2, domain%mesh%Dim3, kk, qcond )
              where(qcond(:,:).lt.0.0)
                  qcond(:,:)=rsmall
              end where
              domain % state % num(indice_qrain) % field % field3d % array(:,:,kk) = qcond
           end if           
 
           if ( indice_qice /= 0 ) then
              var = "QICE"
              call da_get_field8( input_file, var, 3, domain%mesh%Dim1, domain%mesh%Dim2, domain%mesh%Dim3, kk, qcond )
              where(qcond(:,:).lt.0.0)
                  qcond(:,:)=rsmall
              end where
              domain % state % num(indice_qice) % field % field3d % array(:,:,kk) = qcond
           end if

           if ( indice_qsnow /= 0 ) then
              var = "QSNOW"
              call da_get_field8( input_file, var, 3, domain%mesh%Dim1, domain%mesh%Dim2, domain%mesh%Dim3, kk, qcond )
              where(qcond(:,:).lt.0.0)
                  qcond(:,:)=rsmall
              end where
              domain % state % num(indice_qsnow) % field % field3d % array(:,:,kk) = qcond
           end if

        end do       

        deallocate(qcond)

     end if ! end condition model

     else
       write(*,*)'No variable de control qrain or qsnow or qice or qclouc has been selected'
     end if
    
  end subroutine convert_qcond2state


  subroutine convert_qt2state(domain, input_file)

     implicit none

     type (domain_type), intent(inout), pointer :: domain
     character (len=1024),intent(in) :: input_file
     real, allocatable    :: temp2d(:,:),rh2d(:,:), p2d(:,:)
     character (len=12)   :: var
     integer :: kk, indice_temp, indice_rh, indice_qs, indice_tv

     indice_temp = get_state_indice(domain%state,"t")
     indice_tv = get_state_indice(domain%state,"tv")
     indice_qs = get_state_indice(domain%state,"qs")
     indice_rh = get_state_indice(domain%state,"rh")

     if ( (indice_temp/=0).or.(indice_rh/=0).or.(indice_qs/=0).or.(indice_tv/=0) ) then

     if (trim(domain%model)=="WRF") then
        ! allocate temporary arrays, dimensions can depend of the grid model
        allocate( temp2d(domain%mesh%Dim1,domain%mesh%Dim2) )
        allocate( rh2d(domain%mesh%Dim1,domain%mesh%Dim2) )
        allocate( p2d(domain%mesh%Dim1,domain%mesh%Dim2) )
        temp2d = 0.0
        rh2d   = 0.0
        p2d    = 0.0
        if ( ( indice_temp /= 0 ) .or. ( indice_rh /= 0 ) .or. (indice_qs/=0) ) then
              do kk = 1, domain%mesh%Dim3
                 call da_get_trh( input_file, domain%mesh%Dim1, domain%mesh%Dim2, domain%mesh%Dim3, kk, temp2d, rh2d )
                 if ( indice_temp /= 0 ) then
                    domain % state % num(indice_temp) % field % field3d % array(:,:,kk) = temp2d
                 end if
                 if ( indice_tv /= 0 ) then
                    var = "QVAPOR"
                    call da_get_field( input_file, var, 3,domain%mesh%Dim1,domain%mesh%Dim2, domain%mesh%Dim3, kk, p2d )
                    domain % state % num(indice_tv) % field % field3d %array(:,:,kk) = temp2d*(1 + 1.608*p2d)/(1 + p2d)
                 end if
                 if ( indice_rh /= 0 ) then
                    domain % state % num(indice_rh) % field % field3d % array(:,:,kk) = rh2d  * 0.01 ! to conform to unit of GSI and WRFVAR
                 end if
                 if ( indice_qs /= 0 ) then
                    var = "QVAPOR"
                    call da_get_field( input_file, var, 3, domain%mesh%Dim1, domain%mesh%Dim2, domain%mesh%Dim3, kk, rh2d )
                    domain % state % num(indice_qs) % field % field3d % array(:,:,kk) = rh2d / (1 + rh2d) 
                 end if
              end do
        end if 
        deallocate(temp2d)
        deallocate(rh2d)
        deallocate(p2d)

     end if ! condition model

     else
        write(*,*)'No variable de control qrain or qsnow or qice or qclouc are selected'
     end if

  end subroutine convert_qt2state


  subroutine convert_chem2state(domain, input_file)

     implicit none

     type (domain_type), intent(inout), pointer :: domain
     character (len=1024),intent(in) :: input_file
     real(kind=8), allocatable    :: temp2d(:,:)
     character (len=12), allocatable   :: varname_wrf(:), varname_ind(:)
     integer :: kk, ii, indice, nchem

     nchem = 23 
     allocate(varname_wrf(1:nchem))
     allocate(varname_ind(1:nchem))

     ! if people are using other model then wrf, it will be better to do
     ! a subroutine that convert name_model to name of gen_be (indice of state)
     ! convert_varname_model2state(model,varname_model,varname_indice) 
     ! G. DESCOMBES
     varname_wrf(1)  = "DUST_1"
     varname_wrf(2)  = "DUST_2"
     varname_wrf(3)  = "DUST_3"
     varname_wrf(4)  = "DUST_4"
     varname_wrf(5)  = "DUST_5"
     varname_wrf(6)  = "P25"
     varname_wrf(7)  = "P10"
     varname_wrf(8)  = "OC1"
     varname_wrf(9)  = "OC2"
     varname_wrf(10) = "BC1"
     varname_wrf(11) = "BC2"
     varname_wrf(12) = "SEAS_1"
     varname_wrf(13) = "SEAS_2"
     varname_wrf(14) = "SEAS_3"
     varname_wrf(15) = "SEAS_4"
     varname_wrf(16) = "o3"
     varname_wrf(17) = "no2"
     varname_wrf(18) = "hno3"
     varname_wrf(19) = "no"
     varname_wrf(20) = "co"
     varname_wrf(21) = "ch4"
     varname_wrf(22) = "ho"
     varname_wrf(23) = "so2"
 
     varname_ind(1)  = "dust_1"
     varname_ind(2)  = "dust_2"
     varname_ind(3)  = "dust_3"
     varname_ind(4)  = "dust_4"
     varname_ind(5)  = "dust_5"
     varname_ind(6)  = "p25"
     varname_ind(7)  = "p10"
     varname_ind(8)  = "oc1"
     varname_ind(9)  = "oc2"
     varname_ind(10) = "bc1"
     varname_ind(11) = "bc2"
     varname_ind(12) = "seas_1"
     varname_ind(13) = "seas_2"
     varname_ind(14) = "seas_3"
     varname_ind(15) = "seas_4"
     varname_ind(16) = "o3"
     varname_ind(17) = "no2"
     varname_ind(18) = "hno3"
     varname_ind(19) = "no"
     varname_ind(20) = "co"
     varname_ind(21) = "ch4"
     varname_wrf(22) = "ho"
     varname_wrf(23) = "so2"

     do ii=1, nchem
 
        indice = get_state_indice(domain%state,varname_ind(ii))
        write(*,*)'indice ',indice, trim(varname_ind(ii)), trim(varname_wrf(ii))

        if ( indice/=0) then

           if (trim(domain%model)=="WRF") then
              ! allocate temporary arrays, dimensions can depend of the grid model
              allocate( temp2d(domain%mesh%Dim1,domain%mesh%Dim2) )
              temp2d = 0.0
              do kk = 1, domain%mesh%Dim3
                 call da_get_field8( input_file, varname_wrf(ii), 3, domain%mesh%Dim1, domain%mesh%Dim2, domain%mesh%Dim3, kk, temp2d )
                 domain % state % num(indice) % field % field3d % array(:,:,kk) = temp2d
                 !write(*,*)'temp2d ',temp2d(2,2)
              end do
              deallocate(temp2d)
           end if ! condition model

        else
           write(*,*)'No chemistry variables are selected'
        end if
      end do   
 
     deallocate(varname_wrf)
     deallocate(varname_ind)

  end subroutine convert_chem2state


  subroutine convert_psfc2state(domain,input_file)

     implicit none

     type (domain_type), intent(inout), pointer :: domain
     character (len=1024),intent(in) :: input_file
     character (len=12)   :: var
     integer :: indice

     indice = get_state_indice(domain%state,"ps")

     if (indice /= 0 ) then
        if (trim(domain%model)=="WRF") then
           var = "PSFC"
           call da_get_field8( input_file, var, 2, domain%mesh%Dim1, domain%mesh%Dim2, domain%mesh%Dim3, &
                           1, domain%state%num(indice)%field%field2d%array )
        end if
  
        if ( trim(domain%application(1:5))=="GSI") then
           domain%state%num(indice)%field%field2d%array = domain%state%num(indice)%field%field2d%array * 0.001
        end if

     end if

  end subroutine convert_psfc2state
  

  subroutine compute_cldfra(cld_fra, domain, input_file)

     use da_cloud_fraction, only : cal_cldfra2

     implicit none

     type (field3DReal), intent(inout), pointer :: cld_fra
     type (domain_type), intent(in), pointer :: domain
     character (len=1024), intent(in) :: input_file
     character (len=12)   :: var
     real, allocatable    :: tmp2d(:,:), tmp3d(:,:,:)
     real, allocatable    :: press(:,:,:), qvapor(:,:,:)
     integer :: kk, indice_qcloud, indice_qice, indice_qsnow, indice_temp

     indice_qcloud = get_state_indice(domain%state,"qcloud")
     indice_qice = get_state_indice(domain%state,"qice")
     indice_qsnow = get_state_indice(domain%state,"qsnow")
     indice_temp = get_state_indice(domain%state,"t")

     if (trim(domain%model)=="WRF") then
        
        do kk=1, domain%mesh%Dim3
     
           allocate( tmp2d(domain%mesh%Dim1,domain%mesh%Dim2) )
           allocate( press(domain%mesh%Dim1,domain%mesh%Dim2,domain%mesh%Dim3) )
           allocate( qvapor(domain%mesh%Dim1,domain%mesh%Dim2,domain%mesh%Dim3) )

           !  Needed for cld fraction: pressure, qvapor
           var = "PB"
           call da_get_field( input_file, var, 3, domain%mesh%Dim1, domain%mesh%Dim2, domain%mesh%Dim3, &
                          kk, tmp2d )
           press(:,:,kk) = tmp2d(:,:)
           var = "P"
           call da_get_field( input_file, var, 3, domain%mesh%Dim1, domain%mesh%Dim2, domain%mesh%Dim3, &
                          kk, tmp2d )
           press(:,:,kk) = press(:,:,kk) + tmp2d(:,:)
           var = "QVAPOR"
           call da_get_field( input_file, var, 3, domain%mesh%Dim1, domain%mesh%Dim2, domain%mesh%Dim3, &
                          kk, tmp2d )
           qvapor(:,:,kk) = tmp2d(:,:)

        end do

     end if ! WRF condition

     ! compute cld_fra / WRF kg/kg
 
     allocate( tmp3d(domain%mesh%Dim1,domain%mesh%Dim2,domain%mesh%Dim3) ) 
  !   call cal_cldfra2(CLDFRA=tmp3d, QV=qvapor, &
  !                      QC=domain % state % num(indice_qcloud) % field % field3d % array, &
  !                      QI=domain % state % num(indice_qice) % field % field3d % array, &
  !                      QS=domain % state % num(indice_qsnow) % field % field3d % array, &
  !                      F_QV=.true.,F_QC=.true.,F_QI=.true.,F_QS=.true.,&
  !                      t_phy=domain % state % num(indice_temp) % field % field3d % array, &
  !                      p_phy=press, &
  !                      ids=1,ide=domain%mesh%Dim1,jds=1,jde=domain%mesh%Dim2,kds=1,kde=domain%mesh%Dim3,&
  !                      ims=1,ime=domain%mesh%Dim1,jms=1,jme=domain%mesh%Dim2,kms=1,kme=domain%mesh%Dim3,&
  !                      its=1,ite=domain%mesh%Dim1,jts=1,jte=domain%mesh%Dim2,kts=1,kte=domain%mesh%Dim3)

      cld_fra%array = tmp3d 
      deallocate( tmp3d )
      deallocate( tmp2d )
      deallocate( press )
      deallocate( qvapor )

  end subroutine compute_cldfra


!  subroutine convert_unit()
!
!
!    indice_ps = get_state_indice(domain%state,"ps")
!    indice_qcloud = get_state_indice(domain%state,"qcloud")
!    indice_qrain  = get_state_indice(domain%state,"qrain")
!    indice_qice   = get_state_indice(domain%state,"qice")
!    indice_qsnow  = get_state_indice(domain%state,"qsnow")
!
!    ! surface pressure
!    if ( trim(application(1:5))=="GSI") then ! pressure in hpa
!        if (trim(domain%model)=="WRF") then
!           domain%state%num(indice_ps)%field%field2d%array = domain%state%num(indice_ps)%field%field2d%array * 0.001
!        end if
!    end if
!
!  end subroutine convert_unit

  !===========================================================================
  ! Section Create an object for the first time from scratch
  !===========================================================================
  
   subroutine create_bins(bins, bin_type, lat_min, lat_max, binwidth_lat, &
                     hgt_min, hgt_max, binwidth_hgt, mesh)

      use da_gen_be, only : da_create_bins

      implicit none
      type (bins_type), intent(inout), pointer :: bins
      type (mesh_type), intent(in), pointer :: mesh
      integer, intent(in) :: bin_type
      real(kind=8), intent(inout) :: lat_min, lat_max, binwidth_lat, hgt_min, hgt_max, binwidth_hgt
      integer :: Dim1, Dim2, Dim3, ii, jj, kk, b
      integer :: num_bins, num_bins2d
      integer(kind=4), allocatable :: bin0(:,:,:)                  ! Bin assigned to each 3D point.
      integer(kind=4), allocatable :: bin2d0(:,:)                  ! Bin assigned to each 2D point.
      !real, allocatable :: lat(:,:), hgt(:,:,:)     
 
      Dim1 = mesh%Dim1
      Dim2 = mesh%Dim2
      Dim3 = mesh%Dim3
      write(*,*)'Dim1,Dim2,Dim3,bin_type ',Dim1,Dim2,Dim3,bin_type
      allocate(bin0(Dim1,Dim2,Dim3))
      allocate(bin2d0(Dim1,Dim2))
      !allocate(lat(Dim1,Dim2))
      !allocate(hgt(Dim1,Dim2,Dim3))
      !lat = mesh%lat%array
      !hgt = mesh%hgt%array

      call da_create_bins( Dim1, Dim2, Dim3, bin_type, num_bins, num_bins2d, &
                 bin0, bin2d0, &
                 lat_min, lat_max, binwidth_lat, &
                 hgt_min, hgt_max, binwidth_hgt,mesh%lat%array,mesh%hgt%array)
      write(*,*)'num_bins, num_bins2d ',num_bins, num_bins2d
      call allocate_bins(bins, Dim1, Dim2, Dim3, num_bins, num_bins2d)
      bins%bin_type = bin_type
      bins%bin%array(:,:,:) = bin0(:,:,:)
      bins%bin2d%array(:,:) = bin2d0(:,:)
      bins%bin_type2_param%num(1)%field%scalar = lat_min
      bins%bin_type2_param%num(1)%field%ioinfo%fieldName = 'lat_min'
      bins%bin_type2_param%num(2)%field%scalar = lat_max
      bins%bin_type2_param%num(2)%field%ioinfo%fieldName = 'lat_max'
      bins%bin_type2_param%num(3)%field%scalar = binwidth_lat
      bins%bin_type2_param%num(3)%field%ioinfo%fieldName = 'binwidth_lat'
      bins%bin_type2_param%num(4)%field%scalar = hgt_min
      bins%bin_type2_param%num(4)%field%ioinfo%fieldName = 'hgt_min'
      bins%bin_type2_param%num(5)%field%scalar = hgt_max
      bins%bin_type2_param%num(5)%field%ioinfo%fieldName = 'hgt_max'
      bins%bin_type2_param%num(6)%field%scalar = binwidth_hgt
      bins%bin_type2_param%num(6)%field%ioinfo%fieldName = 'binwidth_hgt'
     !  Compute number of bins (once and forever)
     !  Binning along i,j,k
     do kk = 1, Dim3
       do jj = 1, Dim2
          do ii = 1, Dim1
             b = bins%bin%array(ii,jj,kk)
             bins%bin_pts%array(b) = bins%bin_pts%array(b) + 1
          end do
        end do
     end do
    !   Binning along i,j
     do jj = 1, Dim2
       do ii = 1, Dim1
         b = bins%bin2d%array(ii,jj)
         bins%bin2d_pts%array(b) = bins%bin2d_pts%array(b) + 1
       end do
     end do

    deallocate(bin0)
    deallocate(bin2d0)
    !deallocate(lat)
    !deallocate(hgt)
   
  end subroutine create_bins


  logical function pass_filter(varname, field_val)

      implicit none
      character (len=*), intent(in) :: varname
      real(kind=8), intent(in)      :: field_val 
      real(kind=8) :: limit0 
      
      pass_filter = .true.

      if ((trim(varname)=='qcloud_u').or.(trim(varname)=='qrain_u').or.(trim(varname)=='qice_u').or.(trim(varname)=='qsnow_u') .or. &
         (trim(varname)=='qcloud').or.(trim(varname)=='qrain').or.(trim(varname)=='qice').or.(trim(varname)=='qsnow') ) then
         limit0 = 1e-6 
         if ( field_val < limit0 ) then
            pass_filter = .false.
         end if
      end if
    
      return

   end function pass_filter 

  
   

!============================================================================

end module io_input_model
