subroutine anbkerror_reg(gradx,grady)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    anbkerror_reg apply anisotropic background error covariance  
!   prgmmr: parrish          org: np22                date: 2005-02-03
!
! abstract: apply regional anisotropic background error.
!
! program history log:
!   2005-02-08  parrish
!   2005-04-29  parrish - replace coarse2fine with fgrid2agrid;
!                         remove ansmoothrf_reg_d
!   2006-11-30  todling - add fpsproj as arg to (t)balance routine(s)
!   2008-10-10  derber - add strong constraint to background error
!   2008-12-29  todling - update interface to strong_bk/bk_ad
!   2009-04-13  derber - move strong_bk into balance
!
!   input argument list:
!     gradx    - input field  
!
!   output
!     grady    - background structure * gradx 
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  use kinds, only: r_kind,i_kind
  use gridmod, only: lat2,lon2
  use jfunc, only: nsclen,npclen
  use balmod, only: balance,tbalance
  use berror, only: varprd,fpsproj
  use constants, only: zero
  use control_vectors
  use gsi_4dvar, only: nsubwin
  implicit none

! Declare passed variables
type(control_vector),intent(inout):: gradx
type(control_vector),intent(inout):: grady

! Declare local variables
  integer(i_kind) i,j,ii
  real(r_kind),dimension(lat2,lon2):: sst,slndt,sicet

! Put things in grady first since operations change input variables
  grady=gradx

! Zero arrays for land, ocean, ice skin (surface) temperature.
  do j=1,lon2
     do i=1,lat2
        slndt(i,j)=zero
        sst(i,j)  =zero
        sicet(i,j)=zero
     end do
  end do

! Loop on control steps
  do ii=1,nsubwin

!   Transpose of balance equation
    call tbalance(grady%step(ii)%t ,grady%step(ii)%p , &
                  grady%step(ii)%st,grady%step(ii)%vp,fpsproj)

!   Apply variances, as well as vertical & horizontal parts of background error
    call anbkgcov_reg(grady%step(ii)%st,grady%step(ii)%vp,grady%step(ii)%t, &
                      grady%step(ii)%p ,grady%step(ii)%rh,grady%step(ii)%oz, &
                      grady%step(ii)%sst,sst,slndt,sicet,grady%step(ii)%cw)

!   Balance equation
    call balance(grady%step(ii)%t ,grady%step(ii)%p ,&
                 grady%step(ii)%st,grady%step(ii)%vp,fpsproj)

end do

! Take care of background error for bias correction terms
  if(nsclen>0)then
    do i=1,nsclen
      grady%predr(i)=grady%predr(i)*varprd(i)
    end do
  end if
  if(npclen>0)then
    do i=1,npclen
      grady%predp(i)=grady%predp(i)*varprd(nsclen+i)
    end do
  end if

end subroutine anbkerror_reg


subroutine anbkgcov_reg(st,vp,t,p,q,oz,skint,sst,slndt,sicet,cwmr)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    anbkgcov    apply anisotropic background error covar
!   prgmmr: parrish        org: np22                date: 2005-02-14
!
! abstract: apply regional anisotropic background error covariance
!
! program history log:
!   2005-02-14  parrish
!
!   input argument list:
!     t        - t on subdomain
!     p        - p surface pressure on subdomain
!     q        - q on subdomain
!     oz       - ozone on subdomain
!     skint    - skin temperature on subdomain
!     sst      - sea surface temperature on subdomain
!     slndt    - land surface temperature on subdomain
!     sicet    - ice surface temperature on subdomain
!     cwmr     - cloud water mixing ratio on subdomain
!     st       - streamfunction on subdomain
!     vp       - velocity potential on subdomain
!
!   output argument list:
!                 all after smoothing, combining scales
!     t        - t on subdomain
!     p        - p surface pressure on subdomain
!     q        - q on subdomain
!     oz       - ozone on subdomain
!     skint    - skin temperature on subdomain
!     sst      - sea surface temperature on subdomain
!     slndt    - land surface temperature on subdomain
!     sicet    - ice surface temperature on subdomain
!     cwmr     - cloud water mixing ratio on subdomain
!     st       - streamfunction on subdomain
!     vp       - velocity potential on subdomain
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!$$$
  use kinds, only: r_kind,i_kind
  use gridmod, only: lat2,lon2,nlat,nlon,nsig,nsig1o
  implicit none

! Passed Variables
  real(r_kind),dimension(lat2,lon2),intent(inout):: p,skint,sst,slndt,sicet
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout):: t,q,cwmr,oz,st,vp

! Local Variables
  integer(i_kind) iflg
  real(r_kind),dimension(nlat,nlon,nsig1o):: hwork


! break up skin temp into components
  call anbkgvar_reg(skint,sst,slndt,sicet,0)

! Convert from subdomain to full horizontal field distributed among processors
  iflg=1
  call sub2grid(hwork,t,p,q,oz,sst,slndt,sicet,cwmr,st,vp,iflg)

! Apply horizontal smoother for number of horizontal scales
  call ansmoothrf_reg(hwork)

! Put back onto subdomains
  call grid2sub(hwork,t,p,q,oz,sst,slndt,sicet,cwmr,st,vp)

! combine sst,sldnt, and sicet into skin temperature field
  call anbkgvar_reg(skint,sst,slndt,sicet,1)

end subroutine anbkgcov_reg


subroutine anbkgvar_reg(skint,sst,slndt,sicet,iflg)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    anbkgvar_reg      manipulate skin temp
!   prgmmr: parrish          org: np22                date: 1990-10-06
!
! abstract: manipulate skin temp <--> sst,sfc temp, and ice temp fields
!
! program history log:
!   2005-01-22  parrish
!   2008-06-05  safford - rm unused uses
!
!   input argument list:
!     skint    - skin temperature grid values
!     sst      - sst grid values
!     slndt    - land surface temperature grid values
!     sicet    - snow/ice covered surface temperature grid values
!     iflg     - flag for skin temperature manipulation
!                0: skint --> sst,slndt,sicet
!                1: sst,slndt,sicet --> skint
!
!   output argument list:
!     skint    - skin temperature grid values
!     sst      - sst grid values
!     slndt    - land surface temperature grid values
!     sicet    - snow/ice covered surface temperature grid values
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use gridmod, only: lat2,lon2
  use guess_grids, only: isli2
  implicit none

! Declare passed variables
  integer(i_kind),intent(in):: iflg
  real(r_kind),dimension(lat2,lon2),intent(inout):: skint,sst,slndt,sicet

! Declare local variables
  integer(i_kind) i,j

       do j=1,lon2
         do i=1,lat2
           if(iflg == 0) then
! Break skin temperature into components
!          If land point
             if(isli2(i,j) == 1) then
               slndt(i,j)=skint(i,j)
!          If ice
             else if(isli2(i,j) == 2) then
               sicet(i,j)=skint(i,j)
!          Else treat as a water point
             else
               sst(i,j)=skint(i,j)
             end if

           else if (iflg.eq.1) then
! Combine sst,slndt, and sicet into skin temperature field
!          Land point, load land sfc t into skint
             if(isli2(i,j) == 1) then
               skint(i,j)=slndt(i,j)
!          Ice, load ice temp into skint
             else if(isli2(i,j) == 2) then
               skint(i,j)=sicet(i,j)
!          Treat as a water point, load sst into skint
             else
               skint(i,j)=sst(i,j)
             end if
           end if
         end do
       end do

  return
end subroutine anbkgvar_reg


subroutine ansmoothrf_reg(work)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    ansmoothrf_reg  anisotropic rf for regional mode
!   prgmmr: parrish          org: np22                date: 2005-02-14
!
! abstract: apply anisotropic rf for regional mode
!
! program history log:
!   2005-02-14  parrish
!   2009-01-02  todling - get mype from mpimod directly
!
!   input argument list:
!     work     - fields to be smoothed
!
!   output argument list:
!     work     - smoothed fields
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!$$$
  use mpimod, only: mype
  use kinds, only: r_kind,i_kind,r_single
  use anberror, only: ids,ide,jds,jde,kds,kde,ips,ipe,jps,jpe,kps,kpe, &
           ims,ime,jms,jme,kms,kme,filter_all,ngauss
  use mpimod, only: npe
  use constants, only: zero
  use gridmod, only: nlat,nlon
  use fgrid2agrid_mod, only: fgrid2agrid,tfgrid2agrid
  use raflib, only: raf4_ad,raf4
  implicit none

! Declare passed variables
  real(r_kind),dimension(nlat,nlon,kms:kme),intent(inout):: work

! Declare local variables
  integer(i_kind) i,igauss,j,k
  real(r_kind),dimension(ims:ime,jms:jme,kms:kme):: worka
  real(r_single),dimension(ngauss,ims:ime,jms:jme,kms:kme):: workb


!  adjoint of coarse to fine grid
  do k=kms,kme
    call tfgrid2agrid(work(1,1,k),worka(ims,jms,k))

  end do

!  transfer coarse grid fields to ngauss copies
  do k=kms,kme
    do j=jms,jme
      do i=ims,ime
        do igauss=1,ngauss
          workb(igauss,i,j,k)=worka(i,j,k)
        end do
      end do
    end do
  end do

!   apply recursive filter

  call raf4(workb,filter_all,ngauss, &
       ids,ide,jds,jde,kds,kde,ips,ipe,jps,jpe,kps,kpe,ims,ime,jms,jme,kms,kme,mype,npe)
  call raf4_ad(workb,filter_all,ngauss, &
       ids,ide,jds,jde,kds,kde,ips,ipe,jps,jpe,kps,kpe,ims,ime,jms,jme,kms,kme,mype,npe)

!  add together ngauss copies
  do k=kms,kme
    do j=jms,jme
      do i=ims,ime
        worka(i,j,k)=zero
        do igauss=1,ngauss
          worka(i,j,k)=worka(i,j,k)+workb(igauss,i,j,k)
        end do
      end do
    end do
  end do

!  coarse to fine grid
  do k=kms,kme
    call fgrid2agrid(worka(ims,jms,k),work(1,1,k))
  end do


end subroutine ansmoothrf_reg
