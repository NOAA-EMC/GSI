program cov_calc
!This program computes a covariance matrix 
!based on either Desroziers' method or
!the Hollingsworth-Lonnberg method
!Kristen Bathmann
!5-2015

use kinds, only:             r_kind, i_kind
use matrix_tools
use obs_tools
use pairs
use constants, only:         zero_int,zero,one,two,sixty,threesixty, &
                             small
use RadDiag_IO, only:        RadDiag_Hdr_type, &
                             RadDiag_Data_type, &
                             RadDiag_ReadMode, &
                             RadDiag_WriteMode, &
                             RadDiag_AppendMode, &
                             RadDiag_OpenFile, &
                             RadDiag_CloseFile, &
                             RadDiag_Hdr_ReadFile, &
                             RadDiag_Data_ReadFile
use Message_Handler, only:   success, warning, failure, eof, &
                             program_message, display_message
use RadDiag_Define, only:    RadDiag_Data_Destroy, &
                             RadDiag_Hdr_Destroy


implicit none
!program info
character(*), parameter:: program_name='Compute_Covariance'

!loop counters
integer:: i,j, r, c, jj,dd,dis
integer(i_kind):: div
integer:: tim                                            !time step
integer:: n_pair                                         !number of pairs made for one analysis obs at one time step
integer:: ntimes                                         !number of time steps to process
integer:: nc

!file variables
character(5):: ges_stub, anl_stub
character(9):: gesfile, anlfile
character(256):: cov_file                                !name of outputted covariance file
character(256):: wave_file                               !name of outputted file containing channel wavenumbers
character(256):: err_file                                !name of outputted file containing assumed obs errors
character(256):: corr_file                               !name of outputted correlation file
character(256):: instr
integer:: Error_Status, gesid, anlid
integer, parameter:: dsize=4500                          !cap size on the number of omg's that can be stored at each time step
integer:: read_status, leninstr
integer:: lencov, lencorr, lenwave, lenerr
integer(i_kind):: reclen
logical:: out_wave                                       !option to output channel wavenumbers
logical:: out_err                                        !option to output assigned obs errors
logical:: out_corr                                       !option to output correlation matrix

!Diag data
integer:: no_chn                                         !number of instrument channels available
type(RadDiag_Hdr_type):: RadDiag_Hdr                     !header info about the diag data
type(RadDiag_Data_type):: RadDiag_Data                   !diag data
real(r_kind), dimension(:,:), allocatable:: ges          !background omg data for three files
real(r_kind),dimension(:),allocatable:: anl              !analysis omg for one file
integer, dimension(:,:), allocatable:: gesuse            !specifies whether a particular background omg should be used
integer, dimension(:), allocatable:: anluse              !specifies whether a particular analysis omg should be used
real(r_kind), dimension(:), allocatable:: chaninfo       !wavenumbers of assimilated channels
real(r_kind), dimension(:), allocatable:: errout         !assumed obs errors of assimilated channels
integer(i_kind):: nch_active                             !number of assimilated channels for this instrument
integer(i_kind),dimension(:),allocatable:: indR          !indices of the assimlated channels
integer:: ng                                             !the number of background omg's for three time steps

!FOV choice
integer:: Surface_Type, Cloud_Type
integer, parameter:: Sea=1
integer, parameter:: Land =2
integer, parameter:: Snow=3
integer, parameter:: Mixed=4
integer, parameter:: Ice=5
integer, parameter:: Snow_and_Ice=6
integer, parameter:: Clear_FOV=1
integer, parameter:: Clear_Channel=2
real(r_kind), parameter:: clear_threshold=0.01_r_kind     !if using clear sky data, do not use if above this threshold
real(r_kind), parameter:: sea_threshold=0.99_r_kind       !if using sea data, do not use if below this threshold
real(r_kind), parameter:: lower_sea_threshold=0.9_r_kind  !if using mixed data, do not use if above this threshold
real(r_kind), parameter:: lower_land_threshold=0.9_r_kind !if using mixed data, do not use if above this threshold
real(r_kind), parameter:: lower_ice_threshold=0.9_r_kind  !if using mixed data, do not use if above this threshold
real(r_kind), parameter:: lower_snow_threshold=0.9_r_kind !if using mixed data, do not use if above this threshold
real(r_kind), parameter:: land_threshold=0.99_r_kind      !if using land data, do not use if above this threshold
real(r_kind), parameter:: ice_threshold=0.95_r_kind       !if using ice data, do not use if below this threshold
real(r_kind), parameter:: snow_threshold=0.99_r_kind      !if using snow data, do not use if below this threshold
real(r_kind):: satang

!Data times
real(r_kind):: time_min                                  !time of obs, relative to time of corresponding diag file
real(r_kind),dimension(:), allocatable:: ges_times       !times of background obs, relative to time of first diag file
real(r_kind):: anl_time                                  !time of analysis obs, relative ot time of first diag file

!Data locations
real(r_kind), dimension(:,:), allocatable::gesloc        !locations (lat,lon) of background obs
real(r_kind), dimension(2):: anlloc                      !location (lat,lon) of analysis obs
integer:: num_bin,num_bins
real(r_kind):: bin_size, timeth
real(r_kind),dimension(:),allocatable:: bin_dist 
real(r_kind)::bin_center                                 !bin center, km, used for Hollingworth Lonnberg method

!Covariance Definition
integer, parameter:: hl_method=1
integer, parameter:: desroziers=2
integer, parameter:: full_chan=1
integer:: cov_method, chan_choice
integer,dimension(:), allocatable:: obs_pairs, n_pair_hl
integer,dimension(:,:), allocatable:: obs_pairs_hl
real(r_kind), dimension(:,:), allocatable:: Rcov         !the covariance matrix
real(r_kind), dimension(:,:), allocatable:: Rcorr        !the correlation matrix
real(r_kind), dimension(:,:), allocatable:: anl_ave      !average value of oma
real(r_kind), dimension(:,:), allocatable:: ges_ave      !average value of omb
integer(i_kind), dimension(:,:), allocatable:: divider   !divider(r,c) gives the total number of ges omgs used to compute Rcov(r,c)
real(r_kind):: cov_sum, anl_sum, ges_sum1,ges_sum2, ges_sum
real(r_kind):: val, divreal
real(r_kind), dimension(:,:,:), allocatable:: Rcovbig
real(r_kind), dimension(:,:,:), allocatable:: ges_avebig1,ges_avebig2
real(r_kind), dimension(:,:,:), allocatable:: divbig

!Matrix conditioning
real(r_kind),dimension(:), allocatable:: eigs            !Eigenvalue array (if reconditioning)
real(r_kind),dimension(:,:), allocatable:: eigv          !Eigenvectors (if reconditioning)
real(r_kind), dimension(:,:), allocatable:: Rout
real(r_kind):: kreq, mx, mn
integer:: rec_method
real(r_kind), parameter:: errt=0.0001_r_kind

read(5,*) ntimes, Surface_Type, Cloud_Type, satang, instr, out_wave, out_err,  &
   out_corr, kreq, rec_method, cov_method, chan_choice, timeth, bin_size, bin_center
if (cov_method==desroziers) then
   allocate(bin_dist(1))
   bin_dist(1)=bin_size
else
   num_bin=3
   num_bins=num_bin
   allocate(bin_dist(num_bin))
   bin_dist(1)=small
   bin_dist(2)=bin_center-(bin_size/2)
   bin_dist(3)=bin_center+(bin_size/2)
end if
leninstr=len_trim(instr)
lencov=len_trim('Rcov_')
cov_file(1:lencov)='Rcov_'
cov_file(lencov+1:lencov+leninstr)=instr
lencorr=len_trim('Rcorr_')
corr_file(1:lencorr)='Rcorr_'
corr_file(lencorr+1:leninstr+lencorr)=instr
lenwave=len_trim('wave_')
wave_file(1:lenwave)='wave_'
wave_file(lenwave+1:lenwave+leninstr)=instr
lenerr=len_trim('err_')
err_file(1:lenerr)='err_'
err_file(lenerr+1:leninstr+lenerr)=instr

ges_stub(1:5)='dges_'
anl_stub(1:5)='danl_'
allocate(gesloc(dsize,2))
allocate(ges_times(dsize))
do tim=1,ntimes
   call get_filename(tim,anl_stub,anlfile)
   !we read in one analysis diag file at each time step.
   !ges diag data is overwritten when no longer needed
   call get_filename(tim,ges_stub,gesfile)
   !opening ges diag file
   Error_Status=RadDiag_OpenFile(trim(gesfile),gesid)
   if (Error_Status /= success ) then
      call display_message(program_name,'Error opening '//trim(gesfile),failure)
      stop
   end if
   !read ges header
   Error_Status=RadDiag_Hdr_ReadFile(gesid,RadDiag_Hdr)
   if (Error_Status /= success ) then
      call display_message(program_name,'Error reading ges header',failure)
      stop
   end if

   !allocate
   if ((tim==1)) then
      no_chn=RadDiag_Hdr%Scalar%nchan
      nch_active=0
      i=0
      if ((chan_choice==full_chan)) then
         nch_active=no_chn
      else
         do j=1,no_chn
            !only want to use actively assimilated channels
            if (RadDiag_Hdr%Channel(j)%iuse.gt.zero) then
               nch_active=nch_active+1
            end if
         end do
      end if
      !indicies of the actively assimilated channels, needed
      !by the GSI
      allocate(indR(nch_active))
      i=0
      do j=1,no_chn
         if (chan_choice==full_chan) then
            indR(j)=j
         else if (RadDiag_Hdr%Channel(j)%iuse.gt.zero) then
            i=i+1
            indR(i)=j
         end if
      end do
      allocate(ges(dsize,nch_active))
      allocate(gesuse(dsize,nch_active))
      allocate(Rcov(nch_active,nch_active))
      allocate(divider(nch_active,nch_active))
      allocate(ges_ave(nch_active,nch_active))
      allocate(chaninfo(nch_active),errout(nch_active))
      allocate(obs_pairs(dsize))
      if (cov_method==desroziers) then
         allocate(anl(nch_active),anluse(nch_active))
         allocate(anl_ave(nch_active,nch_active))
         anl_ave=zero
      else if (cov_method==hl_method) then
         allocate(Rcovbig(nch_active,nch_active,num_bins))
         allocate(ges_avebig1(nch_active,nch_active,num_bin))
         allocate(ges_avebig2(nch_active,nch_active,num_bin))
         allocate(divbig(nch_active,nch_active,num_bins))
         allocate(n_pair_hl(num_bins), obs_pairs_hl(dsize,num_bins))
         Rcovbig=zero
         divbig=zero_int
         ges_avebig1=zero
         ges_avebig2=zero
      end if
      if (out_corr) then 
         allocate(Rcorr(nch_active,nch_active))
         Rcorr=zero
      end if
      if (kreq>zero) then
         allocate(eigs(nch_active),eigv(nch_active,nch_active))
         allocate(Rout(nch_active,nch_active))
      end if
      do r=1,nch_active
         chaninfo(r)=RadDiag_Hdr%Channel(indR(r))%wave
         errout(r)=RadDiag_Hdr%Channel(indR(r))%varch
      end do               
      Rcov=zero
      divider=zero_int
      ges_ave=zero
   end if !tim=1
   ng=0
   ges_read_loop: do 
      read_status=RadDiag_Data_ReadFile(gesid,RadDiag_Hdr,RadDiag_Data)
      select case (read_status)
      case(eof)
         exit ges_read_loop
      case(failure)
         call display_message(program_name, 'Error reading ges data', warning)
         exit ges_read_loop
      case default
         !do nothing
      end select
      !if doesnt meet criteria, dont save, cycle
      if ((Surface_Type==Sea).and.(RadDiag_Data%Scalar%Water_Frac<sea_threshold)) &
         cycle ges_read_loop
      if ((Surface_Type==Land).and.(RadDiag_Data%Scalar%Land_Frac<land_threshold)) & 
         cycle ges_read_loop
      if ((Surface_Type==Snow_And_Ice).and.((RadDiag_Data%Scalar%Snow_Frac<snow_threshold).and. &
         (RadDiag_Data%Scalar%Ice_Frac<ice_threshold))) &
         cycle ges_read_loop
      if ((Surface_Type==Snow).and.(RadDiag_Data%Scalar%Snow_Frac<snow_threshold)) &
         cycle ges_read_loop
      if ((Surface_Type==Ice).and.(RadDiag_Data%Scalar%Ice_Frac<ice_threshold)) &
         cycle ges_read_loop
      if ((Surface_Type==Mixed).and.(RadDiag_Data%Scalar%Water_Frac>=lower_sea_threshold)) &
         cycle ges_read_loop
      if ((Surface_Type==Mixed).and.(RadDiag_Data%Scalar%Land_Frac>=lower_land_threshold)) &
         cycle ges_read_loop             
      if ((Surface_Type==Mixed).and.(RadDiag_Data%Scalar%Ice_Frac>=lower_ice_threshold)) &
         cycle ges_read_loop
      if ((Surface_Type==Mixed).and.(RadDiag_Data%Scalar%Snow_Frac>=lower_snow_threshold)) &
         cycle ges_read_loop
      if ((Cloud_Type==Clear_FOV).and.(RadDiag_Data%Scalar%qcdiag1>clear_threshold)) &
         cycle ges_read_loop
      if (abs(RadDiag_Data%Scalar%satzen_ang)>satang) cycle ges_read_loop
      nc=0
      ng=ng+1
      if (ng>dsize) then
         ng=dsize
         cycle ges_read_loop
      end if
      ges_channel_loop: do jj=1,nch_active
         j=indR(jj)
         if (((abs(RadDiag_Data%Channel(j)%qcmark)<one)).and. &
            (abs(RadDiag_Data%Channel(j)%errinv)>errt)) then 
            ges(ng,jj)=RadDiag_Data%Channel(j)%omgbc
            gesuse(ng,jj)=1
            nc=nc+1
         else
            ges(ng,jj)=zero
            gesuse(ng,jj)=0
         end if
      end do ges_channel_loop
      if (nc<1) then
         cycle ges_read_loop
         ng=ng-1
      end if
      time_min=RadDiag_Data%Scalar%obstime
      ges_times(ng)=(time_min*sixty)+(threesixty*(tim-1))
      gesloc(ng,1)=RadDiag_Data%Scalar%lat
      gesloc(ng,2)=RadDiag_Data%Scalar%lon
   end do ges_read_loop
   close(gesid)
   if (cov_method==desroziers) then
      !read anl data
      Error_Status=RadDiag_OpenFile(trim(anlfile),anlid)
      if (Error_Status /= success ) then
         call display_message(program_name,'Error opening'//trim(anlfile),failure)
         stop
      end if
      !read anl header
      Error_Status=RadDiag_Hdr_ReadFile(anlid,RadDiag_Hdr)
      if (Error_Status /= success ) then
         call display_message(program_name,'Error reading anl header',failure)
         stop
      end if
      anl_read_loop: do
         read_status=RadDiag_Data_ReadFile(anlid,RadDiag_Hdr,RadDiag_Data)
         select case (read_status)
         case(eof)
            exit anl_read_loop
         case(failure)
            call display_message(program_name, 'Error reading anl data', warning)
            exit anl_read_loop
         case default
            !do nothing
         end select
         !if doesnt meet criteria, cycle 
         if ((Surface_Type==Sea).and.(RadDiag_Data%Scalar%Water_Frac<sea_threshold)) &
            cycle anl_read_loop
         if ((Surface_Type==Land).and.(RadDiag_Data%Scalar%Land_Frac<land_threshold)) &
            cycle anl_read_loop
         if ((Surface_Type==Snow_and_Ice).and.((RadDiag_Data%Scalar%Snow_Frac<snow_threshold).and. &
            (RadDiag_Data%Scalar%Ice_Frac<ice_threshold))) &
            cycle anl_read_loop
         if ((Surface_Type==Snow).and.(RadDiag_Data%Scalar%Snow_Frac<snow_threshold)) &
            cycle anl_read_loop
         if ((Surface_Type==Ice).and.(RadDiag_Data%Scalar%Ice_Frac<ice_threshold)) &
            cycle anl_read_loop
         if ((Surface_Type==Mixed).and.(RadDiag_Data%Scalar%Water_Frac>=lower_sea_threshold)) &
            cycle anl_read_loop
         if ((Surface_Type==Mixed).and.(RadDiag_Data%Scalar%Land_Frac>=lower_land_threshold)) &
            cycle anl_read_loop
         if ((Surface_Type==Mixed).and.(RadDiag_Data%Scalar%Ice_Frac>=lower_ice_threshold)) &
            cycle anl_read_loop
         if ((Surface_Type==Mixed).and.(RadDiag_Data%Scalar%Snow_Frac>=lower_snow_threshold)) &
            cycle anl_read_loop
         if ((Cloud_Type==Clear_FOV).and.(RadDiag_Data%Scalar%qcdiag1>clear_threshold)) &
            cycle anl_read_loop
         if (abs(RadDiag_Data%Scalar%satzen_ang)>satang) cycle anl_read_loop
         nc=0
         anl_channel_loop: do jj=1,nch_active
            j=indR(jj)
            if (((abs(RadDiag_Data%Channel(j)%qcmark)<one)).and.&
               (abs(RadDiag_Data%Channel(j)%errinv)>errt)) then 
               anl(jj)=RadDiag_Data%Channel(j)%omgbc
               anluse(jj)=1
               nc=nc+1
            else
               anl(jj)=zero
               anluse(jj)=0
            end if
         end do anl_channel_loop
         if (nc<one) cycle anl_read_loop
         time_min=RadDiag_Data%Scalar%obstime
         anl_time=(time_min*sixty)+(threesixty*(tim-1))
         anlloc(1)=RadDiag_Data%Scalar%lat
         anlloc(2)=RadDiag_Data%Scalar%lon
         n_pair=zero
         obs_pairs=zero
         !find all possible pairs for this one oma
         !cycle through current ges file to find all matches
         call make_pairs(gesloc(:,:),anlloc,ges_times(:),anl_time,ng, &
              bin_dist(1),timeth,obs_pairs,n_pair)
         if (n_pair>zero) then
!$omp parallel do private(r,c,j,cov_sum,div,anl_sum,ges_sum)
            do c=1,nch_active
               do r=1,nch_active
                  cov_sum=zero
                  div=0
                  anl_sum=zero
                  ges_sum=zero
                  do j=1,n_pair
                     if ((anluse(r)>zero).and.(gesuse(obs_pairs(j),c)>zero)) then
                        cov_sum=cov_sum+(anl(r)*ges(obs_pairs(j),c))
                        anl_sum=anl_sum+anl(r)
                        ges_sum=ges_sum+ges(obs_pairs(j),c)
                        div=div+1
                     end if  
                  end do
                  Rcov(r,c)=Rcov(r,c)+cov_sum
                  anl_ave(r,c)=anl_ave(r,c)+anl_sum
                  ges_ave(r,c)=ges_ave(r,c)+ges_sum
                  divider(r,c)=divider(r,c)+div
               end do !r=1,nch_active
            end do  !c=1,nch_active
!$omp end parallel do
         end if  !npair>zero 
      end do anl_read_loop
      close(anlid)
   else if (cov_method==hl_method) then  !end of cov_method=desroziers
      do dd=1,ng
         obs_pairs=zero
         n_pair_hl=zero
         call make_pairs_hl(gesloc(:,:),gesloc(dd,:),ges_times(:), &
              ges_times(dd),ng,bin_dist,timeth, num_bin, obs_pairs_hl,n_pair_hl)
         do dis=1,num_bins
            if (n_pair_hl(dis)>zero) then
!$omp parallel do private(r,c,j,cov_sum,div,ges_sum1,ges_sum2)
               do c=1,nch_active
                  do r=1,nch_active
                     cov_sum=zero
                     div=0
                     ges_sum1=zero
                     ges_sum2=zero
                     do j=1,n_pair_hl(dis)
                        if ((gesuse(dd,r)>zero).and.(gesuse(obs_pairs_hl(j,dis),c)>zero)) then
                           cov_sum=cov_sum+(ges(dd,r)*ges(obs_pairs_hl(j,dis),c))
                           ges_sum1=ges_sum1+ges(obs_pairs_hl(j,dis),c)
                           ges_sum2=ges_sum2+ges(dd,r)
                           div=div+1
                        end if
                     end do
                     Rcovbig(r,c,dis)=Rcovbig(r,c,dis)+cov_sum
                     divbig(r,c,dis)=divbig(r,c,dis)+div
                     ges_avebig1(r,c,dis)=ges_avebig1(r,c,dis)+ges_sum1
                     ges_avebig2(r,c,dis)=ges_avebig2(r,c,dis)+ges_sum2
                  end do !r=1,nch_active
               end do !c=1,nch_active
!$omp end parallel do
            end if  !n_pair>0
         end do !dis=1,num_bin
      end do !dd=1,ng
   end if   !cov_method=hl_method
end do !tim=1,ntimes
!covariance calculation
if (cov_method==desroziers) then
!$omp parallel do private(r,c,divreal)
   do c=1,nch_active
      do r=1,nch_active
         if (divider(r,c)>zero) then
            divreal=real(divider(r,c),r_kind)
            !the second term here subtracts the biases
            Rcov(r,c)=(Rcov(r,c)/divreal)-((anl_ave(r,c)/divreal)*(ges_ave(r,c)/divreal))
         else if (r==c) then 
            !if there is no data passing qc for this channel, set Rcov to the
            !orignal obs error
            Rcov(r,c)=errout(r)**2
         end if
      end do
   end do
!$omp end parallel do
else if (cov_method==hl_method) then
!$omp parallel do private(r,c,dis,divreal)
   do c=1,nch_active
      do r=1,nch_active
         do dis=1,num_bins
            if (divbig(r,c,dis)>zero) then
               divreal=real(divbig(r,c,dis),r_kind)
               !the second term here subtracts the biases
               Rcovbig(r,c,dis)=(Rcovbig(r,c,dis)/divreal)-((ges_avebig1(r,c,dis)/divreal)*(ges_avebig2(r,c,dis)/divreal))
            end if
          end do
          Rcov(r,c)=Rcovbig(r,c,1)-Rcovbig(r,c,2)
          if ((r==c).and.(abs(Rcov(r,c))<=small)) Rcov(r,c)=errout(r)**2
      end do
   end do      
!$omp end parallel do
end if
Rcov=(Rcov+TRANSPOSE(Rcov))/two

if (kreq>zero) then
   call eigdecomp(Rcov,nch_active,eigs,eigv)
   mx=0
   mn=1000
   do r=1,nch_active
      if (eigs(r) > mx) mx=eigs(r)
      if ((eigs(r) < mn).and.(eigs(r)>=0)) mn=eigs(r)
      if (eigs(r)<0) print *, 'Negative eigenvalue, before reconditioning:',  eigs(r)
   end do
   print *, 'Original condition number: ', mx/mn
   call recondition(eigv,eigs,nch_active,kreq,Rout,rec_method)
   Rcov=Rout
end if
Rcov=(Rcov+TRANSPOSE(Rcov))/two
if (kreq>zero) then
   call eigdecomp(Rcov,nch_active,eigs,eigv)
   mx=0
   mn=1000
   do r=1,nch_active
      if (eigs(r) > mx) mx=eigs(r)
      if ((eigs(r) < mn).and.(eigs(r)>=0)) mn=eigs(r)
      if (eigs(r)<0) print *, 'Negative eigenvalue after reconditioning:',  eigs(r)
   end do
   print *, 'New condition number: ', mx/mn
end if
if (out_corr) then
   do c=1,nch_active
      do r=1,nch_active
        if (divider(r,c)>zero) then
            val=Rcov(r,r)*Rcov(c,c)
            val=sqrt(abs(val))
            if (val>errt) then
               Rcorr(r,c)=Rcov(r,c)/val
            else
               Rcorr(r,c)=one
            end if
        else if (r==c) then
            Rcorr(r,c)=one
        end if
      end do
   end do
   Rcorr=(Rcorr+TRANSPOSE(Rcorr))/two
end if
call RadDiag_Hdr_Destroy(RadDiag_Hdr)
call RadDiag_Data_Destroy(RadDiag_Data)
deallocate(ges_times,gesloc,ges,gesuse, ges_ave,bin_dist)
if (cov_method==desroziers) then
   deallocate(anl, anluse, anl_ave)
else if (cov_method==hl_method) then
   deallocate(Rcovbig,divbig,ges_avebig1,ges_avebig2)
   deallocate(n_pair_hl, obs_pairs_hl)
end if
!output
inquire(iolength=reclen) Rcov(1,1)
open(26,file=trim(cov_file),form='unformatted')
write(26) nch_active, reclen
write(26) indR
write(26) Rcov
close(26)

if (out_wave) then
   open(28,file=trim(wave_file),form='unformatted',access='direct',recl=nch_active*reclen)
   write(28,rec=1) chaninfo
   close(28)
end if
if (out_err) then
   open(29,file=trim(err_file),form='unformatted',access='direct',recl=nch_active*reclen)
   write(29,rec=1) errout
   close(29)
end if
if (out_corr) then 
   open(25,file=trim(corr_file),form='unformatted',access='direct',recl=nch_active*nch_active*reclen)
   write(25,rec=1) Rcorr
   close(25)
end if

deallocate(Rcov,chaninfo,errout)
deallocate(indR)
deallocate(divider)
deallocate(obs_pairs)
if (out_corr) then
   deallocate(Rcorr)
end if
if (kreq>zero) deallocate(Rout,eigv, eigs)
end program cov_calc
