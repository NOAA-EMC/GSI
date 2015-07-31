program cov_calc
!This program computes a covariance matrix based on Desroziers' method
!Kristen Bathmann
!5-2015

use kinds, only:             r_kind, i_kind
use obs_tools
use pairs
use RadDiag_IO, only:        RadDiag_Hdr_type, &
                             RadDiag_Data_type, &
                             RadDiag_ReadMode, &
                             RadDiag_WriteMode, &
                             RadDiag_AppendMode, &
                             RadDiag_OpenFile, &
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
integer:: j, r, c, jj
integer:: i, i1, i2, ii
integer:: ptimes, gtim
integer(i_kind):: div
integer:: tim                                           !time step
integer:: n_pair                                        !number of pairs made for one analysis obs at one time step
integer:: ntimes                                        !number of time steps to process
integer:: nc, ncc

!files
character(5):: ges_stub, anl_stub
character(9):: gesfile, anlfile
character(256):: cov_file                               !name of outputted covariance file
character(256):: wave_file                              !name of outputted file containing channel wavenumbers
character(256):: err_file                               !name of outputted file containing assumed obs errors
character(256):: corr_file                              !name of outputted correlation file
character(256):: instr
integer:: Error_Status, gesid, anlid
integer, parameter:: dsize=4500                         !cap size on the number of omg's that can be stored at each time step
integer:: gcmod, gsize
integer:: gwhile, gblock
integer:: read_status, leninstr
integer:: lencov, lencorr, lenwave, lenerr
integer(i_kind):: reclen
logical:: out_wave                                      !option to output channel wavenumbers
logical:: out_err                                       !option to output assigned obs errors
logical:: out_corr                                      !option to output correlation matrix
!Diag data
integer:: no_chn                                        !number of instrument channels available
type(RadDiag_Hdr_type):: RadDiag_Hdr                    !header info about the diag data
type(RadDiag_Data_type):: RadDiag_Data                  !diag data
real(r_kind), dimension(:,:,:), allocatable:: ges       !background omg data for three files
real(r_kind),dimension(:),allocatable:: anl             !analysis omg for one file
integer, dimension(:,:,:), allocatable:: gesuse         !specifies whether a particular background omg should be used
integer, dimension(:), allocatable:: anluse             !specifies whether a particular analysis omg should be used
real(r_kind), dimension(:), allocatable:: chaninfo      !wavenumbers of assimilated channels
real(r_kind), dimension(:), allocatable:: errout        !assumed obs errors of assimilated channels
integer(i_kind):: nch_active                            !number of assimilated channels for this instrument
integer(i_kind),dimension(:),allocatable:: indR         !indices of the assimlated channels
integer, dimension(3):: ng                              !the number of background omg's for three time steps

!FOV choice
integer:: Surface_Type, Cloud_Type
integer, parameter:: All_Surfaces=0
integer, parameter:: Sea=1
integer, parameter:: Land =2
integer, parameter:: Ice=3
integer, parameter:: Snow=4
integer, parameter:: Clear_FOV=1
integer, parameter:: Cloud_FOV=3
integer, parameter:: All_Cloud=0
integer, parameter:: Clear_Channel=2
integer, parameter:: Cloud_Channel=3
real(r_kind), parameter:: clear_threshold=0.01_r_kind   !if using clear sky data, do not use if above this threshold
real(r_kind), parameter:: cloud_threshold=0.25_r_kind   !if using cloudy data, do not use if below this threshold
real(r_kind), parameter:: sea_threshold=0.99_r_kind     !if using sea data, do not use if below this threshold
real(r_kind), parameter:: land_threshold=0.99_r_kind    !if using land data, do not use if below this threshold
real(r_kind), parameter:: ice_threshold=0.99_r_kind     !if using ice data, do not use if below this threshold
real(r_kind), parameter:: snow_threshold=0.99_r_kind    !if using snow data, do not use if below this threshold
real(r_kind):: satang

!constants
real(r_kind), parameter:: one=1.0_r_kind                
real(r_kind), parameter:: zero=0.0_r_kind               
integer,parameter:: three=3
real(r_kind), parameter:: sixty=60.0_r_kind
real(r_kind), parameter:: threesixty=360.0_r_kind
real(r_kind), parameter:: two=2.0_r_kind

!Data times
real(r_kind):: time_min                                 !time of obs, relative to time of corresponding diag file
real(r_kind),dimension(:,:), allocatable:: ges_times    !times of background obs, relative to time of first diag file
real(r_kind):: anl_time                                 !time of analysis obs, relative ot time of first diag file

!Data locations
real(r_kind), dimension(:,:,:), allocatable::gesloc     !locations (lat,lon) of background obs
real(r_kind), dimension(2):: anlloc                     !location (lat,lon) of analysis obs

!Covariance Definition
integer,dimension(:), allocatable:: obs_pairs
real(r_kind), dimension(:,:), allocatable:: Rcov      !the covariance matrix
real(r_kind), dimension(:,:), allocatable:: Rcorr     !the correlation matrix
real(r_kind), dimension(:,:), allocatable:: anl_ave   !average value of oma
real(r_kind), dimension(:,:), allocatable::  ges_ave  !average value of omb
integer(i_kind), dimension(:,:), allocatable:: divider  !divider(r,c) gives the total number of ges omgs used to compute Rcov(r,c)
real(r_kind):: cov_sum, anl_sum, ges_sum
real(r_kind):: val

read(5,*) ntimes, Surface_Type, Cloud_Type, satang, instr, out_wave, out_err, out_corr
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
gsize=three
if (ntimes<=three) gsize=ntimes
allocate(gesloc(dsize,2,gsize))
allocate(ges_times(dsize,gsize))
do tim=1,ntimes
   call get_filename(tim,anl_stub,anlfile)
   gcmod=mod(tim,3)
   gblock=gcmod+1
   gwhile=0
   if ((tim==ntimes).and.(tim>1)) gwhile=1
   gtim=1
   gblock=1
   if (tim>1) gtim=tim+1
   ncc=0
   !we read in one analysis diag file at each time step.
   !at time step tim, we need data from the ges diag files of time tim-1, tim and tim+1.
   !rather than reading in three ges diag files each time step,
   !we will read in the first two ges diag files at the first time step, 
   !read in the third file at the second time step,
   !read in the tim+1th file at time step tim
   !at the last time step, we do not read in any ges diag files

   !variable gblock indicates the position of the various ges arrays
   !into which data from ges diag file we are currenly reading is put
   !ges diag data is overwritten when no longer needed

   do while (gwhile==0)
      !this while loop only takes effect during the first time step
      !it reads in the first and second ges diag file
      !if ntimes=1 then this while loop goes thru one iteration only
      call get_filename(gtim,ges_stub,gesfile)
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
      if ((tim==1).and.(ncc==0)) then
         !ncc counts the interations in this while loop
         !only want to do this part once, hence require ncc=0 here
         no_chn=RadDiag_Hdr%Scalar%nchan
         nch_active=0
         do j=1,no_chn
            !only want to use actively assimilated channels
            if (RadDiag_Hdr%Channel(j)%iuse.gt.zero) then
               nch_active=nch_active+1
            end if
         end do

         !indicies of the actively assimilated channels, needed
         !by the GSI
         allocate(indR(nch_active))
         i=0
         do j=1,no_chn
            if (RadDiag_Hdr%Channel(j)%iuse.gt.zero) then
               i=i+1
               indR(i)=j
            end if
         end do
         allocate(ges(dsize,nch_active,gsize),anl(nch_active))
         allocate(gesuse(dsize,nch_active,gsize), anluse(nch_active))
         allocate(Rcov(nch_active,nch_active),Rcorr(nch_active,nch_active))
         allocate(divider(nch_active,nch_active))
         allocate(anl_ave(nch_active,nch_active),ges_ave(nch_active,nch_active))
         allocate(chaninfo(nch_active),errout(nch_active))
         do r=1,nch_active
            chaninfo(r)=RadDiag_Hdr%Channel(indR(r))%wave
            errout(r)=RadDiag_Hdr%Channel(indR(r))%varch
         end do
         allocate(obs_pairs(dsize))               
         Rcov=zero
         Rcorr=zero
         divider=zero
         anl_ave=zero
         ges_ave=zero
      end if !tim=1, ncc=0
      ng(gblock)=0
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
          if ((Surface_Type==Ice).and.(RadDiag_Data%Scalar%Ice_Frac<ice_threshold)) &
            cycle ges_read_loop
          if ((Surface_Type==Snow).and.(RadDiag_Data%Scalar%Snow_Frac<snow_threshold)) &
            cycle ges_read_loop
          if ((Cloud_Type==Clear_FOV).and.(RadDiag_Data%Scalar%qcdiag1>clear_threshold)) &
            cycle ges_read_loop
          if ((Cloud_Type==Cloud_FOV).and.(RadDiag_Data%Scalar%qcdiag1<cloud_threshold)) &
            cycle ges_read_loop
          if (abs(RadDiag_Data%Scalar%satzen_ang)>satang) cycle ges_read_loop
          nc=0
          ng(gblock)=ng(gblock)+1
          if (ng(gblock)>dsize) then
             ng(gblock)=dsize
             cycle ges_read_loop
          end if
          ges_channel_loop: do jj=1,nch_active
             j=indR(jj)
             if (((Cloud_Type>All_Cloud).and.&
             (abs(RadDiag_Data%Channel(j)%qcmark)<one)).and. &
             (abs(RadDiag_Data%Channel(j)%errinv)>zero)) then 
                ges(ng(gblock),jj,gblock)=RadDiag_Data%Channel(j)%omgbc
                gesuse(ng(gblock),jj,gblock)=1
                nc=nc+1
             else
                ges(ng(gblock),jj,gblock)=zero
                gesuse(ng(gblock),jj,gblock)=0
             end if
          end do ges_channel_loop
          if (nc<1) then
             cycle ges_read_loop
             ng(gblock)=ng(gblock)-1
          end if
          time_min=RadDiag_Data%Scalar%obstime
          ges_times(ng(gblock),gblock)=(time_min*sixty)+(threesixty*(gtim-1))
          gesloc(ng(gblock),1,gblock)=RadDiag_Data%Scalar%lat
          gesloc(ng(gblock),2,gblock)=RadDiag_Data%Scalar%lon
      end do ges_read_loop
      if ((tim==1).and.(gblock==1).and.(ntimes>1)) then 
         gtim=2
         gblock=2
      else 
         gwhile=1
      end if
      ncc=ncc+1
   end do !while
   !read anl stuff
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
      if ((Surface_Type==Ice).and.(RadDiag_Data%Scalar%Ice_Frac<ice_threshold)) &
          cycle anl_read_loop
      if ((Surface_Type==Snow).and.(RadDiag_Data%Scalar%Snow_Frac<snow_threshold)) &
          cycle anl_read_loop
      if ((Cloud_Type==Clear_FOV).and.(RadDiag_Data%Scalar%qcdiag1>clear_threshold)) &
          cycle anl_read_loop
      if ((Cloud_Type==Cloud_FOV).and.(RadDiag_Data%Scalar%qcdiag1<cloud_threshold)) & 
          cycle anl_read_loop
      if (abs(RadDiag_Data%Scalar%satzen_ang)>satang) cycle anl_read_loop
      nc=0
      anl_channel_loop: do jj=1,nch_active
         j=indR(jj)
         if (((Cloud_Type>All_Cloud).and.&
         (abs(RadDiag_Data%Channel(j)%qcmark)<one)).and.&
         (abs(RadDiag_Data%Channel(j)%errinv)>zero)) then 
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
      !ptimes is the number of ges diag files to use with the current
      !anl diag file to compute the statistics.  
      !if ntimes=1 then only need data from one ges diag file
      !if tim=1 then need the current ges diag data and the data from tim+1
      !if tim=ntimes then need the current ges diag data and data from tim-1
      !otherwise need data from three files (current, tim-1 and tim+1 data)
      if (ntimes==1) then 
         ptimes=1
      else if ((tim==1).or.(tim==ntimes)) then 
         ptimes=2
      else
         ptimes=3
      end if
      if ((tim==ntimes).and.(ntimes>2)) then
         gtim=tim-1
         gcmod=mod(gtim,3)
         i1=gcmod+1
         gtim=tim-2
         gcmod=mod(gtim,3)
         i2=gcmod+1
      end if
      do ii=1,ptimes
         i=ii
         if ((tim==ntimes).and.(ntimes>2)) then
            i=i2
            if (ii==1) i=i1
         end if
         !find all possible pairs for this one oma
         !cycle through preceding, concurrent, and proceding diag
         !ges files to find all matches
         call make_pairs(gesloc(:,:,i),anlloc,ges_times(:,i),anl_time,ng(i),obs_pairs,n_pair)
         if (n_pair>zero) then
            do r=1,nch_active
               do c=1,nch_active
                  cov_sum=zero
                  div=zero
                  anl_sum=zero
                  ges_sum=zero
                  do j=1,n_pair
                     if ((anluse(r)>zero).and.(gesuse(obs_pairs(j),c,i)>zero)) then
                        cov_sum=cov_sum+(anl(r)*ges(obs_pairs(j),c,i))
                        anl_sum=anl_sum+anl(r)
                        ges_sum=ges_sum+ges(obs_pairs(j),c,i) 
                        div=div+1
                     end if  
                  end do
                  Rcov(r,c)=Rcov(r,c)+cov_sum
                  anl_ave(r,c)=anl_ave(r,c)+anl_sum
                  ges_ave(r,c)=ges_ave(r,c)+ges_sum
                  divider(r,c)=divider(r,c)+div
               end do
            end do
          end if  
      end do !ii=1,ptimes
   end do anl_read_loop
end do !ntimes
call RadDiag_Hdr_Destroy(RadDiag_Hdr)
call RadDiag_Data_Destroy(RadDiag_Data)
deallocate(ges_times,gesloc,ges,anl)
deallocate(gesuse,anluse)
!covariance calculation
do r=1,nch_active
   do c=1,nch_active
      if (divider(r,c)>zero) then
         !the second term here subtracts the biases
         Rcov(r,c)=(Rcov(r,c)/divider(r,c))-((anl_ave(r,c)*ges_ave(r,c))/(divider(r,c)**2))
      else if (r==c) then 
         !if there is no data passing qc for this channel, set Rcov to the
         !orignal obs error
         Rcov(r,c)=errout(r)**2
      end if
   end do
end do
do r=1,nch_active
   do c=1,nch_active
     if (divider(r,c)>zero) then
         val=Rcov(r,r)*Rcov(c,c)
         val=sqrt(val)
         Rcorr(r,c)=Rcov(r,c)/val
      else if (r==c) then
         Rcorr(r,c)=one
      end if
   end do
end do
!make covariance matrix symmetric
Rcov=Rcov+TRANSPOSE(Rcov)
Rcorr=Rcorr+TRANSPOSE(Rcorr)
Rcov=Rcov/two
Rcorr=Rcorr/two

!output
inquire(iolength=reclen) Rcov(1,1)
open(26,file=trim(cov_file),form='unformatted')
write(26) nch_active
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
deallocate(indR,Rcorr)
deallocate(divider)
deallocate(anl_ave, ges_ave)
deallocate(obs_pairs)
end program cov_calc
