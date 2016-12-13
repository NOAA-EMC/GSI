 
module io_output_application

!---------------------------------------------------------------------- 
! Purpose: tools useful for different application GSI, WRFDA 
!
! History:
!
! Date     Author & Comment
! -------- ----------------
! dd/mm/yy Gael Descombes
!          Initial version (01/07/2012)
!----------------------------------------------------------------------

      ! use variable_types
      use configure
      use io_input
      use io_input_model
      use io_output
    
      implicit none

      contains

      !---------------------------------------------------------------------------
      ! read/write regression coefficient binary format
      !---------------------------------------------------------------------------

      subroutine write_matrix_state_binary(ounit, matrix, bins, mesh)

         !use da_tools_serial, only : da_get_unit
         
         implicit none
         type (state_matrix_type), intent(in) :: matrix
         type (mesh_type), intent(in) :: mesh
         type (bins_type), intent(in) :: bins
         integer, intent(in) :: ounit
         integer :: ii, jj
       
         write(*,*)'inside write_matrix_state_binary ounit ',ounit
 
         do jj = 1, matrix%nvar
            do ii = 1, matrix%nvar
               if ( matrix%num2d(ii,jj)%IDdim == 1 ) then
                  write(*,*)matrix%num2d(ii,jj)%field%field1d%ioinfo%fieldName
                  write(ounit) matrix%num2d(ii,jj)%field%field1d%array
               else if ( matrix%num2d(ii,jj)%IDdim == 2 ) then
                  write(*,*)matrix%num2d(ii,jj)%field%field2d%ioinfo%fieldName
                  write(ounit) matrix%num2d(ii,jj)%field%field2d%array
               else if ( matrix%num2d(ii,jj)%IDdim == 3 ) then
                  write(*,*)matrix%num2d(ii,jj)%field%field3d%ioinfo%fieldName
                  write(ounit) matrix%num2d(ii,jj)%field%field3d%array
               end if 
            end do
         end do 

      end subroutine write_matrix_state_binary


      subroutine write_stage2_dat(filename, matrix, bins, mesh)

         use da_tools_serial, only : da_get_unit        

         implicit none
         character(len=*), intent(in)         :: filename
         type (state_matrix_type), intent(in) :: matrix
         type (mesh_type), intent(in) :: mesh
         type (bins_type), intent(in) :: bins
         integer :: ii, jj, ounit

         call da_get_unit(ounit)
         open (ounit, file = filename, form='unformatted')
         write(ounit)mesh%Dim1, mesh%Dim2, mesh%Dim3
         write(ounit)bins%num_bins, bins%num_bins2d
         write(*,*)'write_matrix_state_binary : ',filename
         call write_matrix_state_binary(ounit, matrix, bins, mesh)
         close(ounit)

      end subroutine write_stage2_dat      

     
      subroutine read_matrix_state_binary(iunit, matrix)

         implicit none
         type (state_matrix_type), intent(inout) :: matrix
         integer, intent(in) :: iunit
         integer :: ii, jj
 
         do jj = 1, matrix%nvar
            do ii = 1, matrix%nvar
               if ( matrix%num2d(ii,jj)%IDdim == 1 ) then
                  write(*,*)matrix%num2d(ii,jj)%field%field1d%ioinfo%fieldName
                  read(iunit) matrix%num2d(ii,jj)%field%field1d%array
               else if ( matrix%num2d(ii,jj)%IDdim == 2 ) then
                  write(*,*)matrix%num2d(ii,jj)%field%field2d%ioinfo%fieldName
                  read(iunit) matrix%num2d(ii,jj)%field%field2d%array
               else if ( matrix%num2d(ii,jj)%IDdim == 3 ) then
                  write(*,*)matrix%num2d(ii,jj)%field%field3d%ioinfo%fieldName
                  read(iunit) matrix%num2d(ii,jj)%field%field3d%array
               end if
            end do
         end do
 
      end subroutine read_matrix_state_binary


      subroutine read_stage2_dat(filename, matrix, bins, mesh)

         use da_tools_serial, only : da_get_unit

         implicit none
         character(len=*), intent(in)         :: filename
         type (state_matrix_type), intent(inout) :: matrix
         type (mesh_type), intent(inout) :: mesh
         type (bins_type), intent(inout) :: bins
         integer :: Dim1, Dim2, Dim3, num_bins, num_bins2d
         integer :: ii, jj, iunit

         call da_get_unit(iunit)
         open (iunit, file = filename, form='unformatted')
         read(iunit)mesh%Dim1, mesh%Dim2, mesh%Dim3
         read(iunit)bins%num_bins, bins%num_bins2d
         call read_matrix_state_binary(iunit, matrix)
         close(iunit) 

      end subroutine read_stage2_dat


     !---------------------------------------------------------------------------
     ! write bins binary format
     !---------------------------------------------------------------------------

      subroutine write_bins_binary(filename, bins)

         use da_tools_serial, only : da_get_unit

         implicit none
         character(len=*), intent(in)         :: filename
         type (bins_type), intent(in) :: bins
         integer :: ounit

         call da_get_unit(ounit)
         open (ounit, file = filename, form='unformatted')
            write(6,'(3a,i4,a,i4)') '    Write binary bin data in ',trim(filename),' bin_type=',bins%bin_type,&
                ' bins%num_bins2d=',bins%num_bins2d
            open (ounit, file = filename, form='unformatted')
            write(ounit)bins%bin_type
            write(ounit)bins%bin_type2_param%num(1)%field%scalar,bins%bin_type2_param%num(2)%field%scalar,bins%bin_type2_param%num(3)%field%scalar
            write(ounit)bins%bin_type2_param%num(4)%field%scalar,bins%bin_type2_param%num(5)%field%scalar,bins%bin_type2_param%num(6)%field%scalar
            write(ounit)bins%num_bins, bins%num_bins2d
            write(ounit)bins%bin%array
            write(ounit)bins%bin2d%array
        close(ounit)

      end subroutine write_bins_binary

     !---------------------------------------------------------------
     ! da_readwrite_be_stage2
     !---------------------------------------------------------------

      subroutine da_readwrite_be_stage2_binary(outunit, regcoeff, bins, mesh)
        
         ! ----------------------------------------------------------------------
         ! Purpose: Read and write the dimensions, bin information, and
         !          regression coefficients.
         !-----------------------------------------------------------------------

         implicit none

         integer, intent(in)      :: outunit                    ! Output unit number.
         type (state_matrix_type),  intent(in) :: regcoeff
         type (bins_type), intent(in) :: bins
         type (mesh_type), intent(in) :: mesh
         
         write(*,*)'the dimensions are da_readwrite_be_stage2_binary : ',mesh%Dim1, mesh%Dim2, mesh%Dim3
         !------------------------------------------------------
         ! Write out the dimensions and bin information
         !------------------------------------------------------
         write(*,*) 'Write out the dimensions and bin information'
         write(outunit)Dim1, Dim2, Dim3
         write(outunit)bins%bin_type
         write(outunit)bins%bin_type2_param%num(1)%field%scalar, bins%bin_type2_param%num(2)%field%scalar, bins%bin_type2_param%num(3)%field%scalar
         write(outunit)bins%bin_type2_param%num(3)%field%scalar, bins%bin_type2_param%num(4)%field%scalar, bins%bin_type2_param%num(6)%field%scalar
         write(outunit)bins%num_bins, bins%num_bins2d
         write(outunit)bins%bin%array(1:mesh%Dim1,1:mesh%Dim2,1:mesh%Dim3)
         write(outunit)bins%bin2d%array(1:mesh%Dim1,1:mesh%Dim2)
         !-------------------------------------------------------
         ! Write out the coefficients
         !-------------------------------------------------------
         write(*,*)'write_matrix_state_binary'
         write(*,*)'inside write_matrix_state_binary outunit ',outunit
         call write_matrix_state_binary(outunit, regcoeff, bins, mesh)
         
      end subroutine da_readwrite_be_stage2_binary

!==================================================================== 


     subroutine filter_lenscale(scale_length_out, n_smth_sl, spike_tolerance, nk)

        real,             intent(inout) :: scale_length_out(:)
        integer, intent(in)             :: n_smth_sl                  ! Number ofsmoothing. 0: no smoothig
        real, intent(in)                :: spike_tolerance      ! Threshold fordetecting spikes in data.
        integer, intent(in)             :: nk      ! Threshold fordetecting spikes in data.
        real                            :: mean_scale
        real, allocatable               :: sl_smth(:)
        real, allocatable               :: scale_length(:)
        integer                         :: k,kdum 


        if ((n_smth_sl .gt. 0 ).and.( nk .ne. 1  )) then !smoothing

           allocate(sl_smth(1:nk))
           allocate(scale_length(1:nk))
           sl_smth = 0.0
           scale_length = scale_length_out  
           write(*,*)'scale_length ',scale_length 
           ! Remove spikes in lengthscales (extrapolate if spike detected):
           do k = 2, nk-1
              mean_scale = 0.5 * ( scale_length(k-1) + scale_length(k+1) )
              if ( scale_length(k) > spike_tolerance * mean_scale ) then
                 scale_length(k) = mean_scale
                 write(*,*)'remove spike level ',k
              end if
           end do
           write(*,*)'scale_length ',scale_length
           

           ! Smoothing the scale_length
           sl_smth =  scale_length
           do kdum = 1, n_smth_sl
              write(*,*)'smooth it'
              do k = 2, nk-1
                 sl_smth(k) = scale_length(k) &
                    + 0.25*(scale_length(k-1)+scale_length(k+1)-2.0*scale_length(k))
              end do
              scale_length = sl_smth
              write(*,*)'scale_length ',scale_length(:)
           end do

           scale_length_out = scale_length

           deallocate(sl_smth)
           deallocate(scale_length)

        end if

    end subroutine filter_lenscale



   !subroutine  da_readwrite_be_stage4_field( filename0, outunit, nk, uh_method, n_smth_sl, variable,  scale_length_out, io_opt)
   subroutine  da_readwrite_be_stage4_field( filename0, outunit, nk, uh_method, n_smth_sl, variable,  scale_length_out)

      implicit none
  
      integer, intent(in)      :: outunit                    ! Output unit number.
      integer, intent(in)      :: nk                         ! Number of vertical levels/modes.
      integer, intent(in)      :: n_smth_sl                  ! Number of smoothing. 0: no smoothig
      !integer, intent(in)      :: io_opt                     ! Number of smoothing. 0: no smoothig
      character(len=*), intent(in)  :: uh_method
      character*10, intent(in) :: variable                   ! Variable name.
      !type (fieldnDReal), intent(inout) :: field
      real, allocatable,intent(inout) :: scale_length_out(:)
      character(len=*), intent(in) :: filename0                      ! Input filename.

      character(len=1024)      :: filename                      ! Input filename.
      real, parameter          :: spike_tolerance = 1.5      ! Threshold for detecting spikes in data. 
      character*10             :: cvar                       ! Dummy variable name.
      character*2              :: ck                         ! Loop index -> character.
      integer                  :: k                          ! Loop counter.
      integer                  :: kdum                       ! Dummy vertical index.
      integer                  :: max_wavenumber             ! Smallest scale required (ni/2 - 1).
      real                     :: ml                         ! Gradient (read from file but not used).
      real                     :: mean_scale                 ! Average scale between points.
      logical                  :: first_time                 ! True if first time through loop
      real, allocatable        :: total_power(:)             ! Total Power spectrum.
      real, allocatable        :: scale_length(:)
      real, allocatable        :: sl_smth(:)
      integer :: iunit



      filename=trim(filename0)

      if (trim(uh_method) .eq. 'power') then

      allocate(total_power(0:max_wavenumber))

      do k = 1, nk
         write(ck,'(i2)')k
         if (k < 10) ck = '0'//ck(2:2)
         filename = trim(variable)//'/'//trim(variable)
         filename = trim(filename)//'.'//ck//'.spectrum'

         call da_get_unit(iunit)
         open (iunit, file = filename, form='unformatted')
         read(iunit)cvar
         if (trim(cvar) /=  trim(variable)) then
           call da_error("da_readwrite_be_stage4.inc",47, &
             (/"Variable name inconsistency"/))
         end if

         read(iunit)max_wavenumber, kdum
         if (kdum /= k) then
           call da_error("da_readwrite_be_stage4.inc",53, &
             (/"Inconsistent vertical label"/))
         end if

         read(iunit)total_power(:)
         close(iunit)
         call da_free_unit(iunit)

         write(outunit)variable
         write(outunit)max_wavenumber, k
         write(outunit) .false. ! preserve file format
         write(outunit)total_power(:)

         !field%field1d%array = total_power 
         scale_length_out = total_power 
         deallocate(total_power)

      end do

!      else if (trim(uh_method) == 'scale') then
      else 

      allocate(sl_smth(1:nk))
      allocate(scale_length(1:nk))

      call da_get_unit(iunit)
      open (iunit, file = filename)
      write(*,*)'GD read ',filename
      do k=1, nk
         read(iunit,'(a,2e20.8)') ck, ml, scale_length(k)
         write(*,*)'ck, ml, scale_length(k) ',ck, ml, scale_length(k)

         ! If missing value is encountered, use the value from the last
         ! mode or level (YRG, 06/30/2005):
         !if ( k .ge. 1 ) then
         !write(*,*)'coucou gael ',k
         if (ml == 0.0 .or. scale_length(k) == 0.0) &
         !    scale_length(k) = scale_length(k-1)
              scale_length(k) = 0.1

      end do

      if ((n_smth_sl .gt. 0 ).and.( nk .ne. 1  )) then !smoothing

      ! Remove spikes in lengthscales (extrapolate if spike detected):
      do k = 2, nk-1
         write(*,*)'remove spike'
         mean_scale = 0.5 * ( scale_length(k-1) + scale_length(k+1) )
         if ( scale_length(k) > spike_tolerance * mean_scale ) then
            scale_length(k) = mean_scale
         end if
      end do

      ! Smoothing the scale_length
      sl_smth =  scale_length
      do kdum = 1, n_smth_sl
         write(*,*)'smooth it'
         do k = 2, nk-1
            sl_smth(k) = scale_length(k) &
               + 0.25*(scale_length(k-1)+scale_length(k+1)-2.0*scale_length(k))
         end do
         scale_length = sl_smth
      end do

      end if

      write(outunit) variable
      write(outunit) scale_length

      close(iunit)
      call da_free_unit(iunit)
      deallocate(sl_smth)

      if ( nk.eq.1 ) then
         !field%field0d%scalar = scale_length(1)
          scale_length_out(1) =  scale_length(1)
      else
         !field%field1d%array = scale_length
         scale_length_out = scale_length
      end if
      deallocate(scale_length)

   end if

   end subroutine da_readwrite_be_stage4_field

  



!subroutine da_readwrite_benc(fielname)

!   implicit none  

!   type (io_output_object) :: output_obj
!   type (state_type) :: state
!   character (len=*), intent(in):: filename
!   type (bins_type):: bins
!   type (mesh_type):: mesh
!   integer :: vv, nferr 
!   real :: var      
          
!   include 'netcdf.inc' 
        

!   output_obj % filename = trim(filename)
!   nferr = nf_create(trim(output_obj % filename), ior(NF_CLOBBER,NF_64BIT_OFFSET), output_obj%wr_ncid)


   ! create a file even if is existing
   !ncid = NCCRE(filename, NCCLOB, RCODE)

   ! open just to read bin.data
   !ncid = ncopen("bin.data", NC_NOWRITE)
   !call init_bins(filin, bins)

   ! open just to read regcoeff
   !ncid = ncopen("regcoeff.nc", NC_NOWRITE)
   !call read_matrix_state(filename, matrix)

   ! open length scale

   ! open eigen vector and eigen value
   

!end subroutine da_readwrite_benc

!====================================================================
!    GSI stage 1 and stage 2
!==================================================================== 

subroutine get_basis(nz,rlenzsig_psi,zsig,bigxkm,vng,p8,ierror)

!   compute vertical function basis used to compute functional form
!        of t-psi and psfc-psi coupling constants
! input nx,ny,nz
! input rlenzsig_psi: vertical length of psi in zsig unit
! input zsig: R/G*log(sigl)
! output bigxkm: sum vng(i,k)*vng(i,m)
! output vng: basis function; normal decay btw 2 leyers
! output p8: sqrt(X(k,k)) 

  implicit none

  integer, intent(in) :: nz
  real(kind=8), intent(out)   :: bigxkm(1:nz,1:nz),vng(1:nz,1:nz),p8(1:nz)
  real(kind=8), intent(in)    :: rlenzsig_psi(1:nz),zsig(1:nz)
  integer, intent(inout) :: ierror
  integer             :: i,k,kz,m,n
  real(kind=8),allocatable    :: biga8(:,:)
  real(kind=8)                :: eps8,arg8
!
allocate(biga8(1:nz,1:nz) )
do n=1,nz
 do i=1,nz
  arg8=.5_8*((zsig(i)-zsig(n))/rlenzsig_psi(n))**2
  vng(i,n)=0._8
  if(arg8.lt.75._8) then
   vng(i,n)=exp(-arg8)
  end if
 end do
end do

!  compute bigxkm

do m=1,nz
 do k=1,nz
  bigxkm(k,m)=0._8
  do kz=1,nz
   bigxkm(k,m)=bigxkm(k,m)+vng(kz,k)*vng(kz,m)
  end do
 end do
end do

eps8=.0001_8
do m=1,nz
 p8(m)=sqrt(bigxkm(m,m))
end do
do m=1,nz
 do k=1,nz
  biga8(k,m)=bigxkm(k,m)/(p8(k)*p8(m))
  if(k.eq.m) biga8(k,m)=eps8+biga8(k,m)
 end do
end do
do m=1,nz
 do k=1,nz
  bigxkm(k,m)=biga8(k,m)*p8(k)*p8(m)
 end do
end do
call choldc(bigxkm,nz,p8,ierror)

deallocate(biga8)

return
end subroutine get_basis


subroutine choldc(a,n,p,ierror)

!  form cholesky factors of symmetric matrix a

  integer, intent(in)   :: n
  real(kind=8), intent(inout)   ::  a(n,n),p(n)
  integer, intent(out)  :: ierror

  integer              :: i,j,k
  real                 :: sum
!

  ierror=0
  do i=1,n
   do j=i,n
    sum=a(i,j)
    do k=i-1,1,-1
     sum=sum-a(i,k)*a(j,k)
    end do
    if(i.eq.j) then
     if(sum.le.0._8) then
      ierror=1
      return
     end if
     p(i)=sqrt(sum)
    else
     a(j,i)=sum/p(i)
    end if
   end do
  end do

return
end subroutine choldc


subroutine cholsl(a,n,p,b,x)

  !   solve A*x = b, where a has been cholseky factored.
  implicit none
  integer, intent(in)  :: n
  real(kind=8), intent(inout)  ::  x(n)
  real(kind=8), intent(in)     ::  a(n,n),p(n),b(n)

  integer              :: i,j,k
  real                 :: sum

  do i=1,n
   sum=b(i)
   do k=i-1,1,-1
    sum=sum-a(i,k)*x(k)
   end do
   x(i)=sum/p(i)
  end do
  do i=n,1,-1
   sum=x(i)
   do k=i+1,n
    sum=sum-a(k,i)*x(k)
   end do
   x(i)=sum/p(i)
  end do

  return
end subroutine cholsl

subroutine cholsl_check(a,n,b,x)

  !   check cholesky solution of Ax = b, by computing Ax and comparing to b

  implicit none
  integer, intent(in)  :: n
  real(kind=8), intent(in)  ::  x(n)
  real(kind=8), intent(in)     ::  a(n,n),b(n)

  integer              :: j,k
  real(kind=8)                 :: sum, errormax, bmax

  errormax=0._8
  bmax=0._8
  do k=1,n
   sum=0.
   do j=1,n
    sum=sum+a(k,j)*x(j)
   end do
   errormax=max(abs(sum-b(k)),errormax)
   bmax=max(abs(b(k)),bmax)
  end do
  return
end subroutine cholsl_check

subroutine var_tilde(nx,ny,nz,sf,func)
  implicit none

  integer, intent(in)  :: nx, ny, nz
  real(kind=8), intent(inout)  :: sf(1:nx,1:ny,1:nz)
  real(kind=8), intent(in)     :: func(1:nz,1:nz)

  real                 :: sft(1:nx,1:ny,1:nz)
  integer              :: i,j,k, n

   sft=0.
   do n=1,nz
    do k=1,nz
     do j=1,ny
      do i=1,nx
         sft(i,j,k)=sft(i,j,k)+func(n,k)*sf(i,j,n)
      end do
     end do
    end do
   end do
   sf=sft
  return
end subroutine var_tilde


!subroutine vert_corr(nx,ny,nz,sigl, tlflt, &
!              vng,bigxkm,p8,rlen,ierror)

subroutine vert_corr(nx,ny,nz,sigl,tlflt,vng,ierror) 

   integer, intent(in)       :: nx,ny,nz !,ncat
   !integer, intent(in)       :: count(1:ncat)
   !integer, intent(in)       :: nlat(1:nx,1:ny)

!   real, intent(inout)       :: bigxkm(1:nz,1:nz),vng(1:nz,1:nz)
   real(kind=8)       :: bigxkm(1:nz,1:nz)
   real(kind=8) :: vng(1:nz,1:nz)
   
   real(kind=8), intent(inout)       :: tlflt(1:nz,1:nz)
   real(kind=8), intent(in)          :: sigl(1:nz)
   real(kind=8)       :: rlen(1:nz),p8(1:nz)
   real(kind=8)                      :: pdfl(1:nz),zsig(1:nz)
   integer, intent(inout)    :: ierror
   integer :: l, k, k2, lp, lm
   
   ierror = 0
   do l=1,nz
      pdfl(l)=1./sqrt(tlflt(l,l))
   enddo
  write(*,*)'sqrt variance ',pdfl

   do k=1,nz
     do l=1,nz
       !write(*,*)'tlflt k l ',k,l,tlflt(l,k)
       tlflt(l,k)=tlflt(l,k)*pdfl(k)*pdfl(l)
       write(*,*)'tlflt k l ',k,l,tlflt(l,k)
     enddo
    enddo
   
   write(*,*)'normalized sqrt variance ',tlflt(1,1)

!   do k=1,nz
!      do l=1,2
!         k2 = max(k+l-2,1)
!         tlflt(l,k)=tlflt(l,k)*pdfl(k2)*pdfl(k)
!      end do
!   end do

!   do l=1,nz
!      lp=l+1
!      lm=l-1
!        if(lm==0)lm=2
!        if(lp==nz+1)lp=nz-1
!        rlen(l)=sqrt( 1./( abs(2.-tlflt(lm,l)-tlflt(lp,l)) ))
!   end do

   do l=1,nz
      lp=l+1
      lm=l-1
        if(lm==0)lm=2
        if(lp==nz+1)lp=nz-1
        write(*,*)'tlflt(lm,l) ',tlflt(lm,l)
        write(*,*)'tlflt(lp,l) ',tlflt(lp,l)
        rlen(l)=sqrt( 1./( abs(2.-tlflt(lm,l)-tlflt(lp,l)) ))
   end do

   write(*,*)'rlen(l)=sqrt( 1./( abs(2.-tlflt(lm,l)-tlflt(lp,l)) )) ',rlen

   !!!!!!convert from grid units to zsig units !!!!!!!!!!!
   do k=1,nz
      zsig(k)=-log(sigl(k))
   end do
   write(*,*)'zsig ',zsig
   write(*,*)'sigl ',sigl

   do l=1,nz
      lp=min(nz,l+1)
      lm=max(1,l-1)
      rlen(l)=rlen(l)*(zsig(lp)-zsig(lm))/float(lp-lm)
   end do
   write(*,*)'rlen(l)',1/rlen

   call get_basis(nz,rlen,zsig,bigxkm,vng,p8,ierror)

  return
 end subroutine vert_corr


subroutine chol_factor(biga8_bin, p88_bin, varce_tilde, nz, num_bins2d, ierror)

       implicit none

       real(kind=8), intent(out)   :: biga8_bin(1:nz, 1:nz, 1:num_bins2d)
       real(kind=8), intent(out)   :: p88_bin(1:nz, 1:num_bins2d)
       real(kind=8), intent(in)    :: varce_tilde(1:nz, 1:nz, 1:num_bins2d)
       integer, intent(in) :: nz, num_bins2d
       integer, intent(inout) :: ierror
       real(kind=8) :: biga8(1:nz, 1:nz)
       real(kind=8) :: p88(1:nz)
       integer :: j, l, k

       ierror = 0
       do j=1, num_bins2d

          biga8=0.

          do k=1,nz
            do l=1,nz
               biga8(l,k)=varce_tilde(l,k,j)
            end do
          end do
          do k=1,nz
             p88(k)=sqrt(varce_tilde(k,k,j))
             biga8(k,k)=biga8(k,k)*1.001_8
             write(*,*)'p88 ',p88(k)
          end do

          ! After choldc
          ! a) biga8-array is chol-factor of <psi_coeff,psi_coeff>
          ! b) p88-array diagonal for chol-factor of <psi_coeff,psi_coeff>
          write(6,*)' calling choldc'
          write(*,*)'biga8 ',biga8
          write(*,*)'p88 ',p88
          call choldc(biga8,nz,p88,ierror)
          biga8_bin(:,:,j) = biga8(:,:)
          p88_bin(:,j) = p88(:)

     end do

end subroutine chol_factor


subroutine chol_func(mesh, bins, tlflt, varce_tilde, func, biga8_bin, p88_bin)

    implicit none

    type (mesh_type), intent(in) :: mesh
    type (bins_type), intent(in) :: bins
    real(kind=8), intent(inout)  :: tlflt(mesh%Dim3, mesh%Dim3) 
    real(kind=8), intent(out)    :: func(mesh%Dim3, mesh%Dim3)
    real(kind=8), intent(in)     :: varce_tilde(mesh%Dim3, mesh%Dim3,bins%num_bins2d)    
    real(kind=8), intent(out)    :: biga8_bin(mesh%Dim3, mesh%Dim3,bins%num_bins2d)    
    real(kind=8), intent(out)    :: p88_bin(mesh%Dim3, mesh%Dim3,bins%num_bins2d)    
    integer :: ierror 
    !real, allocatable :: tmp1d(:)

    ierror = 0
    !allocate (tmp1d(1:mesh%Dim3))
    !tmp1d = mesh%znu%array
    call vert_corr(mesh%Dim1, mesh%Dim2, mesh%Dim3, mesh%znu%array, &
                  tlflt, func, ierror)
    !deallocate(tmp1d)
    if ( ierror .ne. 0 ) then
       write(6,*)'Error in vert_corr subroutine'
    end if
    write(*,*)'func ',func
    write(*,*)'varce_tilde ',varce_tilde
    write(*,*)'bins%num_bins2d ',bins%num_bins2d 

    ierror = 0
    call chol_factor(biga8_bin, p88_bin, varce_tilde, mesh%Dim3, bins%num_bins2d, ierror)
    if ( ierror .ne. 0 ) then
       write(6,*)'Error in chol_factor subroutine'
    end if
     
end subroutine chol_func


subroutine chol_reg_coeff2d(reg_coeff2d, covarce, biga8_bin, p88_bin, func, nz, num_bins2d)

     implicit none

     real(kind=8), intent(out)   :: reg_coeff2d(1:nz, 1:num_bins2d)
     real(kind=8), intent(in)    :: covarce(1:nz, 1:num_bins2d)
     real(kind=8), intent(in)    :: biga8_bin(1:nz, 1:nz, 1:num_bins2d)
     real(kind=8), intent(in)    :: p88_bin(1:nz, 1:num_bins2d)
     integer, intent(in) :: nz, num_bins2d
     real(kind=8), intent(in)    :: func(1:nz,1:nz)
     real(kind=8)    :: biga8(1:nz, 1:nz)
     real(kind=8)    :: p88(1:nz)
     real(kind=8)    :: x8(1:nz), b8(1:nz)
     integer :: j, k, n

     reg_coeff2d = 0.0
     write(*,*)'func inside reg2d ',func
     write(*,*)'covarce ',covarce 

     do j=1, num_bins2d

        do k=1,nz
           b8(k)=covarce(k,j)
        end do
        write(*,*)'b8 ',b8

        ! Solves          biag8        * x8 = b8
        !        <psi_coeff,psi_coeff> * x8 = <psi_coef,ps>  No lat variation
        ! After solving x8-array holds reg_psi_coeff_ps coeffs
        write(6,*)' calling cholsl'
        p88(:) = p88_bin(:,j)
        biga8(:,:) = biga8_bin(:,:,j)        
        call cholsl(biga8,nz,p88,b8,x8)
        write(*,*)'x8',x8

        ! wnew-array holds reg_psi_coeff_ps * basis of vertical error covar <psi,psi>
        ! Thus effectively the actual reg coeff of <psi,ps>
        ! Note: Same array is filled for all lats (ncat)
        do k=1,nz
           do n=1,nz
              reg_coeff2d(k,j) = reg_coeff2d(k,j)+x8(n)*func(k,n)
           end do
        end do

     end do
     write(*,*)'leaving chol_reg_coeff2d'

end subroutine chol_reg_coeff2d


subroutine chol_reg_coeff3d(reg_coeff3d, covarce, biga8_bin, p88_bin, func, nz, num_bins2d)

     implicit none

     real(kind=8), intent(out)   :: reg_coeff3d(1:nz, 1:nz, 1:num_bins2d)
     real(kind=8), intent(in)    :: covarce(1:nz, 1:nz, 1:num_bins2d)
     real(kind=8), intent(in)   :: biga8_bin(1:nz, 1:nz, 1:num_bins2d)
     real(kind=8), intent(in)   :: p88_bin(1:nz, 1:num_bins2d)
     integer, intent(in) :: nz, num_bins2d
     real(kind=8), intent(in)    :: func(1:nz,1:nz)
     real(kind=8) :: x8(1:nz), b8(1:nz)
     real(kind=8) :: atilde(1:nz,1:nz)
     integer :: j, kz, kp, k, n
     real(kind=8) :: biga8(1:nz, 1:nz)
     real(kind=8) :: p88(1:nz)

    reg_coeff3d = 0.0

     do j = 1, num_bins2d

        do kz=1,nz
          do k=1,nz
             b8(k)=covarce(kz,k,j)
          end do
          write(6,*)' calling cholsl'
          biga8(:,:) = biga8_bin(:,:,j)
          p88(:) = p88_bin(:,j)
          call cholsl(biga8,nz,p88,b8,x8)
          do n=1,nz
              atilde(n,kz)=x8(n)
          end do
        end do

        do k=1,nz
          do kp=1,nz
             reg_coeff3d(k,kp,j)=0.
             do n=1,nz
                reg_coeff3d(k,kp,j)=reg_coeff3d(k,kp,j)+atilde(n,k)*func(kp,n)
             end do
          end do
        end do

    end do

end subroutine chol_reg_coeff3d



!=================================================================== 

end module io_output_application
