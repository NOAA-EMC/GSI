program cov_calc
!This program computes a covariance matrix 
!based on either Desroziers' method or
!the Hollingsworth-Lonnberg method
!Kristen Bathmann
!5-2015

use ckinds, only:        r_kind, i_kind
use matrix_tools
use obs_tools
use pairs
use cconstants, only:    zero_int,zero,one,two,five, &
                         small, sixty, threesixty
use readsatobs


implicit none
!program info
character(*), parameter:: program_name='Compute_Covariance'

!loop counters
integer(i_kind):: j,r, c, dd,dis,ka
integer(i_kind):: tim                                    !time step
integer(i_kind):: n_pair                                 !number of pairs made for one analysis obs at one time step
integer(i_kind):: ntimes                                 !number of time steps to process

!file variables
character(5):: ges_stub, anl_stub
character(9):: gesfile, anlfile
character(256):: cov_file                                !name of outputted covariance file
character(256):: wave_file                               !name of outputted file containing channel wavenumbers
character(256):: err_file                                !name of outputted file containing assumed obs errors
character(256):: corr_file                               !name of outputted correlation file
character(256):: instr
integer(i_kind), parameter:: dsize=4500                  !cap size on the number of omg's that can be stored at each time step
integer(i_kind):: lencov, lencorr, lenwave, lenerr
integer(i_kind):: reclen, leninstr
logical:: out_wave                                       !option to output channel wavenumbers
logical:: out_err                                        !option to output assigned obs errors
logical:: out_corr                                       !option to output correlation matrix

!Diag data
type(RadData):: Radges,Radanl                            !actual data from the radstats
integer(i_kind):: netcdf_in
logical:: netcdf
integer(i_kind):: ng, na                                 !the number of omg's 
real(r_kind):: anl_time                                  !time of analysis obs, relative ot time of first diag file
real(r_kind), dimension(2):: anlloc                      !location (lat,lon) of analysis obs
integer(i_kind):: num_bin,num_bins
real(r_kind):: bin_size, timeth
real(r_kind),dimension(:),allocatable:: bin_dist 
real(r_kind)::bin_center                                 !bin center, km, used for Hollingworth Lonnberg method

!Covariance Definition
integer(i_kind), parameter:: hl_method=1
integer(i_kind), parameter:: desroziers=2
integer(i_kind):: cov_method, chan_choice,Surface_Type,Cloud_Type
real(r_kind):: satang
integer(i_kind),dimension(:), allocatable:: n_pair_hl
integer(i_kind),dimension(:), allocatable:: obs_pairs
integer(i_kind),dimension(:,:), allocatable:: obs_pairs_hl
real(r_kind), dimension(:,:), allocatable:: Rcov         !the covariance matrix
real(r_kind), dimension(:,:), allocatable:: Rcorr        !the correlation matrix
real(r_kind), dimension(:,:), allocatable:: anl_ave      !average value of oma
real(r_kind), dimension(:,:), allocatable:: ges_ave      !average value of omb
integer(i_kind), dimension(:,:), allocatable:: divider   !divider(r,c) gives the total number of ges omgs used to compute Rcov(r,c)
real(r_kind):: cov_sum, ges_sum1,ges_sum2
real(r_kind):: ges_sum,anl_sum
real(r_kind):: val, divreal
integer(i_kind):: div
real(r_kind), dimension(:,:,:), allocatable:: Rcovbig
real(r_kind), dimension(:,:,:), allocatable:: ges_avebig1,ges_avebig2
real(r_kind), dimension(:,:,:), allocatable:: divbig

!Matrix conditioning
real(r_kind),dimension(:), allocatable:: eigs            !Eigenvalue array (if reconditioning)
real(r_kind),dimension(:,:), allocatable:: eigv          !Eigenvectors (if reconditioning)
real(r_kind), dimension(:,:), allocatable:: Rout
real(r_kind):: kreq, mx, mn
integer(i_kind):: rec_method
real(r_kind), parameter:: errt=0.0001_r_kind

read(5,*) ntimes, Surface_Type, Cloud_Type, satang, instr, out_wave, out_err,  &
   out_corr, kreq, rec_method, cov_method, chan_choice, timeth, bin_size, &
   bin_center, netcdf_in
netcdf=.false.
if (netcdf_in>0) netcdf=.true.
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
cov_file=''
corr_file=''
wave_file=''
err_file=''
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
tim=1
call get_filename(tim,ges_stub,gesfile)  
call get_chaninfo(trim(gesfile),netcdf,chan_choice)
allocate(Radges%omg(dsize,nch_active))
allocate(Radges%latlon(dsize,2),Radges%timeobs(dsize))
allocate(Rcov(nch_active,nch_active))
allocate(divider(nch_active,nch_active))
allocate(ges_ave(nch_active,nch_active))
if (bin_size<five) then
   allocate(obs_pairs(1))
else
   allocate(obs_pairs(dsize))
end if
if (cov_method==desroziers) then
   allocate(Radanl%latlon(dsize,2),Radanl%timeobs(dsize))
   allocate(Radanl%omg(dsize,nch_active))
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
Rcov=zero
divider=zero_int
ges_ave=zero
!loop over the files
do tim=1,ntimes
   call get_filename(tim,anl_stub,anlfile)
   call get_filename(tim,ges_stub,gesfile)
   Radges%omg=zero
   Radges%latlon=zero
   Radges%timeobs=zero
   call get_satobs_data(gesfile,netcdf,dsize,Surface_type,Cloud_Type,satang,Radges,ng)
   do ka=1,ng
      Radges%timeobs(ka)=(Radges%timeobs(ka)*sixty)+(threesixty*(tim-1))
   end do
   if (cov_method==desroziers) then
      !read anl data
      Radanl%omg=zero
      Radanl%latlon=zero
      Radanl%timeobs=zero
      call get_satobs_data(anlfile,netcdf,dsize,Surface_type,Cloud_Type,satang,Radanl,na)
      n_pair=zero_int
      obs_pairs=zero_int
      do ka=1,na
         anlloc(1)=Radanl%latlon(ka,1)
         anlloc(2)=Radanl%latlon(ka,2)
         anl_time=(Radanl%timeobs(ka)*sixty)+(threesixty*(tim-1))
         call make_pairs(Radges%latlon(:,:),anlloc,Radges%timeobs(:),anl_time,ng, &
              bin_dist(1),timeth,obs_pairs,n_pair)
         if (n_pair>zero) then
!$omp parallel do private(r,c,cov_sum,div,anl_sum,ges_sum,j) 
            do c=1,nch_active
               do r=1,nch_active
                  cov_sum=zero
                  div=0
                  anl_sum=zero
                  ges_sum=zero
                  do j=1,n_pair
                     if ((abs(Radanl%omg(ka,r))>zero).and.(abs(Radges%omg(obs_pairs(j),c))>zero)) then
                        cov_sum=cov_sum+(Radanl%omg(ka,r)*Radges%omg(obs_pairs(j),c))
                        anl_sum=anl_sum+Radanl%omg(ka,r)
                        ges_sum=ges_sum+Radges%omg(obs_pairs(j),c)
                        div=div+1
                     endif
                  end do
                  Rcov(r,c)=Rcov(r,c)+cov_sum
                  anl_ave(r,c)=anl_ave(r,c)+anl_sum
                  ges_ave(r,c)=ges_ave(r,c)+ges_sum
                  divider(r,c)=divider(r,c)+div
               end do !r=1,nch_active
            end do  !c=1,nch_active
!$omp end parallel do
         end if  !npair>zero 
      end do !ka
   else if (cov_method==hl_method) then  !end of cov_method=desroziers
      do dd=1,ng
         obs_pairs_hl=zero_int
         n_pair_hl=zero_int
         call make_pairs_hl(Radges%latlon(:,:),Radges%latlon(dd,:),Radges%timeobs(:), &
              Radges%timeobs(dd),ng,bin_dist,timeth, num_bin, obs_pairs_hl,n_pair_hl)
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
                        if ((abs(Radges%omg(dd,r))>zero).and. &
                           (abs(Radges%omg(obs_pairs_hl(j,dis),c))>zero)) then
                           cov_sum=cov_sum+(Radges%omg(dd,r)*Radges%omg(obs_pairs_hl(j,dis),c))
                           ges_sum1=ges_sum1+Radges%omg(obs_pairs_hl(j,dis),c)
                           ges_sum2=ges_sum2+Radges%omg(dd,r) 
                           div=div+1
                        endif
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
   do dis=1,num_bins
!$omp parallel do private(r,c,divreal)
      do c=1,nch_active
         do r=1,nch_active
            if (divbig(r,c,dis)>zero) then
               divreal=real(divbig(r,c,dis),r_kind)
               !the second term here subtracts the biases
               Rcovbig(r,c,dis)=(Rcovbig(r,c,dis)/divreal)-((ges_avebig1(r,c,dis)/divreal)*(ges_avebig2(r,c,dis)/divreal))
            end if
            if (dis==num_bins) then
               Rcov(r,c)=Rcovbig(r,c,1)-Rcovbig(r,c,2)
               if ((r==c).and.(abs(Rcov(r,c))<=small)) Rcov(r,c)=errout(r)**2
            end if
         end do 
      end do    
!$omp end parallel do
end do

end if
Rcov=(Rcov+TRANSPOSE(Rcov))/two
if (kreq>zero) then
   eigs=0
   eigv=0
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
deallocate(ges_ave,bin_dist,obs_pairs)
if (cov_method==desroziers) then
   deallocate(anl_ave)
else if (cov_method==hl_method) then
   deallocate(Rcovbig,divbig,ges_avebig1,ges_avebig2)
   deallocate(n_pair_hl, obs_pairs_hl)
end if

!output
reclen=kind(Rcov(1,1))
open(26,file=trim(cov_file),form='unformatted')
write(26) nch_active, nctot, reclen
write(26) indR
write(26) Rcov
close(26)

if (out_wave) then
   open(28,file=trim(wave_file),form='unformatted',access='direct',recl=nch_active)
   write(28,rec=1) chaninfo
   close(28)
end if
if (out_err) then
   open(29,file=trim(err_file),form='unformatted',access='direct',recl=nch_active)
   write(29,rec=1) errout
   close(29)
end if
if (out_corr) then 
   open(25,file=trim(corr_file),form='unformatted',access='direct',recl=nch_active*nch_active)
   write(25,rec=1) Rcorr
   close(25)
end if

deallocate(Rcov,chaninfo,errout)
deallocate(indR)
deallocate(divider)
if (out_corr) then
   deallocate(Rcorr)
end if
if (kreq>zero) deallocate(Rout,eigv, eigs)
end program cov_calc
