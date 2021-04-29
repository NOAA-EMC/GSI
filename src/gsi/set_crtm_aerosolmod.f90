module set_crtm_aerosolmod
!$$$ module documentation block
!           .      .    .                                       .
! module:   set_crtm_aerosolmod
!  prgmmr: todling          org: gmao                date: 2011-06-01
!
! abstract: module providing interface to set-crtm-aerosol procedures
!
! program history log:
!   2011-06-01  todling
!   2011-09-20  hclin   - separate na and na_crtm for p25 handling
!   2019-03-21  martin - replaced blank subroutine here with that previously
!                        in stub_set_crtm_aerosol.f90;
!                        also moved eff rad for dust to size function
!
! subroutines included:
!   sub Set_CRTM_Aerosol_
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

implicit none

private

public Set_CRTM_Aerosol

contains
    subroutine Set_CRTM_Aerosol ( km, na, na_crtm, aero_name, aero_conc, rh, aerosol,  aero_wc)
!  subroutine Set_CRTM_Aerosol ( km, na, na_crtm, aero_name, aero_conc, rh, aerosol, aero_conc_wk, aero_wc)
! USES:

    use kinds, only: i_kind,r_kind
    use constants, only: tiny_r_kind
    use CRTM_Aerosol_Define, only: CRTM_Aerosol_type
    use mpeu_util, only: getindex
    !use crtm_module, only: SULFATE_AEROSOL,BLACK_CARBON_AEROSOL,ORGANIC_CARBON_AEROSOL,&
    !    DUST_AEROSOL,SEASALT_SSAM_AEROSOL,SEASALT_SSCM1_AEROSOL,SEASALT_SSCM2_AEROSOL,SEASALT_SSCM3_AEROSOL

    use chemmod, only: naero_cmaq_fv3,aeronames_cmaq_fv3,imodes_cmaq_fv3
    use chemmod, only: laod_crtm_cmaq,crtm_aerosol_model,iaod_crtm_cmaq,visindx_cmaq_fv3,visindx_large_cmaq_fv3,humfac,humfac_large,humfac_ss
    use chemmod, only: aemolwt_cmaq_fv3,raod_radius_mean_scale,raod_radius_std_scale
    use chemmod, only: iaod_recs_cmaq,visindx_recs_fv3,humfac_recs,humfac_recs_ss
    use crtm_module, only: SULFATE_AEROSOL,BLACK_CARBON_AEROSOL,ORGANIC_CARBON_AEROSOL,&
        DUST_AEROSOL,SEASALT_SSAM_AEROSOL,SEASALT_SSCM1_AEROSOL,SEASALT_SSCM2_AEROSOL,SEASALT_SSCM3_AEROSOL

    implicit none

 !ARGUMENTS:
!Aerosol types  (CRTM CMAQ):
!1. Dust
!2. Soot
!3. Water soluble
!4. Sulfate
!5. Sea salt
!6. Water
!7. Insoluble
!8. dust-like

  INTEGER,  PARAMETER ::        INVALID_AEROSOL = 0
  INTEGER,  PARAMETER ::      DUST_CMAQ_AEROSOL = 1
  INTEGER,  PARAMETER ::           SOOT_AEROSOL = 2
  INTEGER,  PARAMETER ::  WATER_SOLUBLE_AEROSOL = 3
  INTEGER,  PARAMETER ::   SULFATE_CMAQ_AEROSOL = 4
  INTEGER,  PARAMETER ::        SEASALT_AEROSOL = 5
  INTEGER,  PARAMETER ::          WATER_AEROSOL = 6
  INTEGER,  PARAMETER ::      INSOLUBLE_AEROSOL = 7
  INTEGER,  PARAMETER ::      DUST_LIKE_AEROSOL = 8


    integer(i_kind) , intent(in)    :: km                ! number of levels
    integer(i_kind) , intent(in)    :: na                ! number of aerosols
    integer(i_kind) , intent(in)    :: na_crtm           ! number of aerosols seen by CRTM
    character(len=*), intent(in)    :: aero_name(na)     ! [na]    GOCART aerosol names
    real(r_kind),     intent(inout) :: aero_conc(km,na)  ! [km,na] aerosol concentration (Kg/m2)
    real(r_kind),     intent(in)    :: rh(km)            ! [km]    relative humidity [0,1]
!Hongli Wang
!    real(r_kind),     intent(in)    :: aero_conc_wk(km,na)
    real(r_kind),     intent(inout) :: aero_wc(km,na) 
!    real(r_kind)                     ::  aero_wc(km,na) 
    type(CRTM_Aerosol_type), intent(inout) :: aerosol(na_crtm)! [na]   CRTM Aerosol object
!                            diam(nm)       exp(2.5*ln(sig)**2)
! 25 aothri                  15.00000       2.021654   0.030 
! 26 aothrj                  80.00000       3.323879   0.26  
! 31 asoil                   600.0000       4.731123   2.8  
!Reff
!  0.1000E-02  0.1000E-01  0.2500E-01  0.5000E-01  0.7500E-01  0.1000      0.2000    
!  0.3000      0.4000      0.5000      0.6000      0.7000      0.8000    
!  0.9000       1.000       1.100       1.200       1.300       1.400    
!   1.500       1.600       1.700       1.800       1.900       2.000    
!   2.250       2.500       2.750       3.000       3.500       4.000    
!   4.500       5.000       6.000       7.500
!Rsig
!   1.050       1.100       1.200       1.300       1.400       1.500    
!   1.600       1.700       1.800       1.900       2.000       2.100    
!   2.200       2.300       2.400       2.500

    Real,    Parameter :: def_diam( 3 )   = (/ 15.0E-3, 80.0E-3, 600.0E-3 /) !um for CRTM 
    Real,    Parameter :: def_sigma_g( 3 ) = (/ 1.70, 2.0, 2.2 /)
    Real,    Parameter :: crtm_cmaq_max_wc(8) = (/3750,6000,5600,4400,4400,6000,4800,3750/)
 
!    Real(r_kind)       :: total_mass_so4(km), total_mass_no3(km), total_mass_om(km) 
!    integer(i_kind) :: anh4_idx,aso4_idx,ano3_idx

    integer(i_kind) :: i, k, irh

    integer(i_kind) :: indx_dust1, indx_dust2, indx_dust3, indx_dust4,indx_dust5
    integer(i_kind) :: indx_bc1, indx_oc1

    indx_bc1=-1; indx_oc1=-1; indx_dust1=-1; indx_dust2=-1
    indx_dust3=-1; indx_dust4=-1; indx_dust5=-1;
    
!   total_mass_so4 = 0.0_r_kind
!   total_mass_no3 = 0.0_r_kind
!   total_mass_om  = 0.0_r_kind

!    aso4_idx=1; anh4_idx=10 ; ano3_idx = 4

    write(6,*)"Set_CRTM_Aerosol: na and km= ",na,na_crtm,km
    write(6,*)"Set_CRTM_Aerosol: rh(1:km)= ",rh(1),rh(km)

    do i = 1, na_crtm
       write(6,*)"set_crtm_aerosolmod_conc= ",trim(aero_name(i))," ",sum(aero_conc(:,i))
       if (laod_crtm_cmaq) then
       ! assign aerosol type
       if (crtm_aerosol_model .eq."CMAQ")then
       select case ( trim(aero_name(i)) )
          !10
          case ('aalj','acaj','afej','akj','amgj','amnj','asij','atij')
             aerosol(i)%type  = DUST_CMAQ_AEROSOL
          case ('asoil','acors')
             aerosol(i)%type  = DUST_LIKE_AEROSOL
          !2
          case ('aeci','aecj')
             aerosol(i)%type  = SOOT_AEROSOL
          !9
          case ('alvoo1i','alvoo2i','anh4i','anh4j','ano3i','ano3j','aso4k','asvoo1i','asvoo2i')
             aerosol(i)%type  = WATER_SOLUBLE_AEROSOL
          !5
          case ('aso4i','aso4j','acli','anh4k','ano3k')
             aerosol(i)%type  = SULFATE_CMAQ_AEROSOL 
          !5
          case ('aclj','aclk','anai','anaj','aseacat')
             aerosol(i)%type  = SEASALT_AEROSOL
          !14
          case ('aivpo1j','alvpo1i','alvpo1j','aothri','aothrj','asvpo1i','asvpo1j','asvpo2i','asvpo2j','asvpo3j','atol1j','axyl1j','axyl2j','axyl3j')
             aerosol(i)%type  = INSOLUBLE_AEROSOL 
          case default
             aerosol(i)%type  = INVALID_AEROSOL 
       end select
       end if
       else !!! assign cmaq aerosal to crtm_gocart 
!Tang, Y., Pagowski, M., Chai, T., Pan, L., Lee, P., Baker, B., Kumar, R., Delle
!Monache, L., Tong, D., and Kim, H.-C.: A case study of aerosol data
!assimilation with the Community Multi-scale Air Quality Model over the
!contiguous United States using 3D-Var and optimal interpolation methods,
!Geosci. Model Dev., 10, 4743–4758, https://doi.org/10.5194/gmd-10-4743-2017,2017.
       if (crtm_aerosol_model .eq."GOCART" .or. crtm_aerosol_model .eq."CRTM")then
          select case ( trim(aero_name(i)) )
          !10
          case ('aalj','acaj','afej','akj','amgj','amnj','asij','asoil','atij','acors')
             aerosol(i)%type  = DUST_AEROSOL
          !2
          case ('aeci','aecj')
             aerosol(i)%type  = BLACK_CARBON_AEROSOL 
          !10
          case ('aso4i','aso4j','acli','anh4k','ano3k','anh4i','anh4j','ano3i','ano3j','aso4k')
             aerosol(i)%type  = SULFATE_AEROSOL
          !3
          case ('aclj','anai','anaj','aclk','aseacat')
             aerosol(i)%type  = SEASALT_SSAM_AEROSOL
          !2
          !case ('aclk','aseacat')
          !   aerosol(i)%type  = SEASALT_SSCM1_AEROSOL
          !18
          case ('alvoo1i','alvoo2i','asvoo1i','asvoo2i','aivpo1j','alvpo1i','alvpo1j','aothri','aothrj','asvpo1i','asvpo1j','asvpo2i','asvpo2j','asvpo3j','atol1j','axyl1j','axyl2j','axyl3j')
             aerosol(i)%type  = ORGANIC_CARBON_AEROSOL 
          case default
             aerosol(i)%type  = INVALID_AEROSOL
         end select
       end if

! Aerosol_Type_Name =
!1  "Dust",
!2  "Sea salt",
!3  Organic carbon hydrophobic
!4  Organic carbon hydrophilic
!5  Black carbon hydrophobic
!6  Black carbon hydrophilic
!7  "Sulfate",
!8  "Nitrate" ;

       if (crtm_aerosol_model .eq."GOCART-GEOS5")then
          select case ( trim(aero_name(i)) )
          !10 DUST
          case ('aalj','acaj','afej','akj','amgj','amnj','asij','asoil','atij','acors')
             aerosol(i)%type  = 1
          !5
          case ('aclj','anai','anaj','aclk','aseacat')
             aerosol(i)%type  = 2
          !2
          case ('aeci','aecj')
             aerosol(i)%type  = 6
          !4
          case ('aso4i','aso4j','acli','aso4k')
             aerosol(i)%type  = 7
          !6
          case ('anh4i','anh4j','anh4k','ano3i','ano3j','ano3k')
             aerosol(i)%type  = 8
          !18
          case ('alvoo1i','alvoo2i','asvoo1i','asvoo2i')
             aerosol(i)%type  = 4
          case ('aivpo1j','alvpo1i','alvpo1j','aothri','aothrj','asvpo1i','asvpo1j','asvpo2i','asvpo2j','asvpo3j','atol1j','axyl1j','axyl2j','axyl3j')
             aerosol(i)%type  = 3
          case default
             aerosol(i)%type  = 1 !INVALID_AEROSOL
          end select
          !print*,"GOCART-GEOS5: ",aerosol(i)%type, trim(aero_name(i))
       end if ! GOCART-GEOS5

       end if ! laod_crtm_cmaq
       write(6,*)"set_crtm_aerosolmod: ",crtm_aerosol_model,aerosol(i)%type, trim(aero_name(i))
       !write(6,*)"set_crtm_aerosolmod_i= ",i,aero_name(i)," ",def_diam(imodes_cmaq_fv3(i)),exp(2.5*(log(def_sigma_g(imodes_cmaq_fv3(i))))**2)

       ! crtm aerosol structure
       do k = 1, km
          irh = int( 100.0 * rh(k)  ) ! truncate relative humidity to nearest
          irh = max( 1, min( 99, irh ) ) ! set bounds
          if (laod_crtm_cmaq) then
            if(iaod_crtm_cmaq.eq.1)then
            ! ke in CMAQ LUTs are 1.0
            !select case ( trim(aero_name(i)) )
            !case ('aso4i','aso4j','aso4k','ano3i','ano3j','ano3k','anh4i','anh4j','anh4k')
            !   aerosol(i)%concentration(k) = max(tiny_r_kind,1000.0*aero_conc(k,i)*visindx_recs_fv3(i)*humfac_recs(irh))
            !   aero_wc(k,i)=1000.0*visindx_recs_fv3(i)*humfac_recs(irh)
            !case ('acli','aclj','aclk','anai','anaj','aseacat')
            !   aerosol(i)%concentration(k) = max(tiny_r_kind,1000.0*aero_conc(k,i)*visindx_recs_fv3(i)*humfac_recs_ss(irh)) 
            !   aero_wc(k,i)=1000.0*visindx_recs_fv3(i)*humfac_recs_ss(irh)
            !case default
            !   aerosol(i)%concentration(k) = max(tiny_r_kind,1000.0*aero_conc(k,i)*visindx_recs_fv3(i))
            !   aero_wc(k,i)=1000.0*visindx_recs_fv3(i)
            ! end select
            !elseif(iaod_crtm_cmaq.eq.2)then
            ! ke in CMAQ LUTs are 1000.0*visindx_recs_fv3 
            select case ( trim(aero_name(i)) )
            case ('aso4i','aso4j','aso4k','ano3i','ano3j','ano3k','anh4i','anh4j','anh4k')
               aerosol(i)%concentration(k) = max(tiny_r_kind,aero_conc(k,i)*humfac_recs(irh))
               aero_wc(k,i)=humfac_recs(irh)
            case ('acli','aclj','aclk','anai','anaj','aseacat')
               aerosol(i)%concentration(k) = max(tiny_r_kind,aero_conc(k,i)*humfac_recs_ss(irh))
               aero_wc(k,i)=humfac_recs_ss(irh)
            case default
               aerosol(i)%concentration(k) = max(tiny_r_kind,aero_conc(k,i))
               aero_wc(k,i)=1.0
             end select
            else 
            aerosol(i)%concentration(k) = max(tiny_r_kind, aero_conc(k,i))
            aero_wc(k,i)=1.0
            end if
          else
          aerosol(i)%concentration(k) = max(tiny_r_kind, aero_conc(k,i))
          end if
          !if(laod_crtm_cmaq)then
          ! calculate effective radius; diam to radius (0.5)
          ! raod_radius_mean_scale,raod_radius_std_scale
          aerosol(i)%effective_radius(k) = raod_radius_mean_scale*0.5*def_diam(i)*exp(2.5*(log(def_sigma_g(imodes_cmaq_fv3(i))))**2) 
          !   = GOCART_Aerosol_size(i, aerosol(i)%type, rh(k))
          aerosol(i)%effective_variance(k) = raod_radius_std_scale*def_sigma_g(imodes_cmaq_fv3(i)) 
          !else 
          ! calculate effective radius
          !aerosol(i)%effective_radius(k) &
          !   = GOCART_Aerosol_size(i, aerosol(i)%type, rh(k))
    
          !end if
!       if (crtm_aerosol_model .eq."GOCART-GEOS5")then
!  0.635884556972458, 1.32442296705467, 2.30121373305749, 4.16720351690789, 
!    7.6707125528765,
!  0.0784111415376474, 0.265607522123851, 1.07211256968108, 2.55155445329703, 
!    7.33948958941014,
!  0.0876579733244398, 0.0876579733244398, _, _, _,
!  0.0392103203239458, 0.0392103203239458, _, _, _,
!  0.156644503590542, 0.599092004449631, _, _, _,
!  0.15599965763613, 2.09998847822135, 7.74998170527397, _, _ ;

!          aerosol(i)%effective_radius(k) = 0.635884556972458
!       end if

          !irh = int( 100.0 * rh(k)  ) ! truncate relative humidity to nearest integer
          !irh = max( 1, min( 99, irh ) ) ! set bounds
!          write(6,*)"setup_crtm= ",trim(aero_name(i))
!       if (iaod_recs_cmaq .eq. 1) then
!       select case ( trim(aero_name(i)) )
!          case ('aso4i','aso4j','aso4k','ano3i','ano3j','ano3k','anh4i','anh4j','anh4k')
!              aero_wc(k,i)=visindx_recs_fv3(i)*humfac_recs(irh)
!          case ('acli','aclj','aclk','anai','anaj','aseacat')
!              aero_wc(k,i)=visindx_recs_fv3(i)*humfac_recs_ss(irh)
!          case default
!              aero_wc(k,i) = visindx_recs_fv3(i) 
!       end select
!       end if 
          !v2 
!       if (iaod_recs_cmaq .eq. 2) then
!       select case ( trim(aero_name(i)) )
!          case ('aso4i','aso4j','aso4k')
!               if (total_mass_so4(k) .lt. 20.0)  then  ! if total mass less than 20ug/m3, split to 2 modes
!                  aero_wc(k,i)=(humfac(irh)*visindx_cmaq_fv3(i)*19.0 + humfac_large(irh)*visindx_large_cmaq_fv3(i))/20.0 
!               else                               ! else  all mass goes to
!                  aero_wc(k,i)= humfac_large(irh)*visindx_large_cmaq_fv3(i)  
!               end if
!               aero_wc(k,i)= aero_wc(k,i)*(aemolwt_cmaq_fv3(anh4_idx)*2+aemolwt_cmaq_fv3(aso4_idx))/aemolwt_cmaq_fv3(aso4_idx)
!               write(6,*)"total_mass_so4= ",k,total_mass_so4(k),aero_wc(k,i)
!
!          case ('ano3i','ano3j','ano3k')
!               if (total_mass_no3(k) .lt. 20.0)  then  
!                  aero_wc(k,i)=(humfac(irh)*visindx_cmaq_fv3(i)*19.0 + humfac_large(irh)*visindx_large_cmaq_fv3(i))/20.0
!               else                               ! else  all mass goes to
!                  aero_wc(k,i)= humfac_large(irh)*visindx_large_cmaq_fv3(i)
!               end if
!               aero_wc(k,i)=aero_wc(k,i)*(aemolwt_cmaq_fv3(anh4_idx)+aemolwt_cmaq_fv3(aso4_idx))/aemolwt_cmaq_fv3(aso4_idx)
!               write(6,*)"total_mass_no3= ",k,total_mass_no3(k),aero_wc(k,i)
!          case ('aivpo1j','alvpo1i','alvpo1j','aothri','aothrj','asvpo1i','asvpo1j','asvpo2i','asvpo2j','asvpo3j','atol1j','axyl1j','axyl2j','axyl3j')
!               if (total_mass_om(k) .lt. 20.0)  then
!                  aero_wc(k,i)=(visindx_cmaq_fv3(i)*19.0 +visindx_large_cmaq_fv3(i))/20.0
!               else                              
!                  aero_wc(k,i)= visindx_large_cmaq_fv3(i)
!               end if
!               write(6,*)"total_mass_om= ",k,total_mass_om(k),aero_wc(k,i)
!          case ('acli','aclj','aclk','anai','anaj','aseacat')
!              aero_wc(k,i)=visindx_cmaq_fv3(i)*humfac_ss(irh)
!              write(6,*)"seasalt= ",k,aero_wc(k,i)
!          case default
!              aero_wc(k,i) = visindx_cmaq_fv3(i) 
!              write(6,*)"default= ",k,aero_wc(k,i)
!       end select
!       end if ! iaod_recs_cmaq =2
!       if (iaod_recs_cmaq .eq. 3) then 
!       select case ( trim(aero_name(i)) )
!          !10
!          case ('aalj','acaj','afej','akj','amgj','amnj','asij','asoil','atij','acors')
!             aero_wc(k,i) = crtm_cmaq_max_wc(1)*0.001
!          !2
!          case ('aeci','aecj')
!             aero_wc(k,i) = crtm_cmaq_max_wc(2)*0.001
!          !9
!          case ('alvoo1i','alvoo2i','anh4i','anh4j','ano3i','ano3j','aso4k','asvoo1i','asvoo2i')
!             aero_wc(k,i) = crtm_cmaq_max_wc(3)*0.001
!          !5
!          case ('aso4i','aso4j','acli','anh4k','ano3k')
!             aero_wc(k,i) = crtm_cmaq_max_wc(4)*0.001
!          !5
!          case ('aclj','aclk','anai','anaj','aseacat')
!            aero_wc(k,i) = crtm_cmaq_max_wc(5)*0.001
!          !14
!          case ('aivpo1j','alvpo1i','alvpo1j','aothri','aothrj','asvpo1i','asvpo1j','asvpo2i','asvpo2j','asvpo3j','atol1j','axyl1j','axyl2j','axyl3j')
!            aero_wc(k,i) = crtm_cmaq_max_wc(7)*0.001
!          case default
!            aero_wc(k,i) = 0.0
!       end select
!       end if !iaod_recs_cmaq =3

       enddo

    enddo  ! na

    contains

    function GOCART_Aerosol_size( kk, itype,  & ! Input
                                         eh ) & ! Input in 0-1
                             result( R_eff  )   ! in micrometer
    use crtm_aerosolcoeff, only: AeroC
    implicit none
!
!   modified from a function provided by Quanhua Liu
!

    integer(i_kind) ,intent(in) :: kk, itype
    real(r_kind)    ,intent(in) :: eh

    integer(i_kind) :: j1,j2,k
    real(r_kind)    :: h1
    real(r_kind)    :: R_eff

    if ( itype==DUST_AEROSOL ) then
       if (kk==indx_dust1) then
            R_eff = 0.55_r_kind
       else if (kk==indx_dust2) then
            R_eff = 1.4_r_kind
       else if (kk==indx_dust3) then
            R_eff = 2.4_r_kind
       else if (kk==indx_dust4) then
            R_eff = 4.5_r_kind
       else if (kk==indx_dust5) then
            R_eff = 8.0_r_kind
       end if
       return
    else if ( itype==BLACK_CARBON_AEROSOL .and. kk==indx_bc1 ) then
       R_eff = AeroC%Reff(1,itype )
       return
    else if ( itype==ORGANIC_CARBON_AEROSOL .and. kk==indx_oc1 ) then
       R_eff = AeroC%Reff(1,itype )
       return
    endif

    j2 = 0
    if ( eh < AeroC%RH(1) ) then
       j1 = 1
    else if ( eh > AeroC%RH(AeroC%n_RH) ) then
       j1 = AeroC%n_RH
    else
       do k = 1, AeroC%n_RH-1
          if ( eh <= AeroC%RH(k+1) .and. eh > AeroC%RH(k) ) then
             j1 = k
             j2 = k+1
             h1 = (eh-AeroC%RH(k))/(AeroC%RH(k+1)-AeroC%RH(k))
             exit
          endif
       enddo
    endif

    if ( j2 == 0 ) then
       R_eff = AeroC%Reff(j1,itype )
    else
       R_eff = (1.0_r_kind-h1)*AeroC%Reff(j1,itype ) + h1*AeroC%Reff(j2,itype )
    endif

    return
    end function GOCART_Aerosol_size


  end subroutine Set_CRTM_Aerosol

  subroutine Set_CRTM_GOCART_Aerosol ( km, na, na_crtm, aero_name, aero_conc, rh, aerosol)
  
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    Set_CRTM_Aerosol
!   prgmmr: hclin          org: ncar/mmm                date: 2011-09-20
!
! abstract: Set the CRTM Aerosol object given GOCART aerosol properties.
!
!
! program history log:
! 2011-02-23  da Silva - Initial version, FORTRAN-77 interface for GSI.
! 2011-08-01  Lueken   - Replaced F90 with f90 (no machine logic)
! 2011-09-20  HCLin    - Coded based on the WRFCHEM implementation of GOCART.
! 2013-11-17 Todling   - Brought HCLin implementation into stub - it live
!                        outside GSI, but to not break DTC usage it's placed
!                        here temporarily. 
! 2019-03-21 Martin    - Moved aerosol eff radius for dust to function GOCART_Aerosol_size
!
!   input argument list:
!     km        : number of CRTM levels
!     na        : number of aerosols
!     na_crtm   : number of aerosols seen by CRTM
!     aero_name : GOCART aerosol names
!     aero_conc : aerosol concentration (Kg/m2)
!     rh        : relative humidity [0,1]
!     aerosol   : CRTM Aerosol object
!
!   output argument list:
!     aero_conc : aerosol concentration (Kg/m2)
!     aerosol   : CRTM Aerosol object
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

! USES:
  
    use kinds, only: i_kind,r_kind
    use constants, only: tiny_r_kind
    use CRTM_Aerosol_Define, only: CRTM_Aerosol_type
    use mpeu_util, only: getindex
    use crtm_module, only: SULFATE_AEROSOL,BLACK_CARBON_AEROSOL,ORGANIC_CARBON_AEROSOL,&
        DUST_AEROSOL,SEASALT_SSAM_AEROSOL,SEASALT_SSCM1_AEROSOL,SEASALT_SSCM2_AEROSOL,SEASALT_SSCM3_AEROSOL
  
    implicit none
  
! !ARGUMENTS:
  
    integer(i_kind) , intent(in)    :: km                ! number of levels
    integer(i_kind) , intent(in)    :: na                ! number of aerosols
    integer(i_kind) , intent(in)    :: na_crtm           ! number of aerosols seen by CRTM
    character(len=*), intent(in)    :: aero_name(na)     ! [na]    GOCART aerosol names
    real(r_kind),     intent(inout) :: aero_conc(km,na)  ! [km,na] aerosol concentration (Kg/m2)
    real(r_kind),     intent(in)    :: rh(km)            ! [km]    relative humidity [0,1]
  
    type(CRTM_Aerosol_type), intent(inout) :: aerosol(na_crtm)! [na]   CRTM Aerosol object
  
    integer(i_kind) :: i, k
    integer(i_kind) :: indx_p25, indx_dust1, indx_dust2, indx_dust3, indx_dust4, indx_dust5
    integer(i_kind) :: indx_bc1, indx_oc1
  
    indx_bc1=-1; indx_oc1=-1; indx_dust1=-1; indx_dust2=-1
    indx_dust3=-1; indx_dust4=-1; indx_dust5=-1; indx_p25=-1
  
    indx_p25   = getindex(aero_name,'p25')
    indx_dust1 = getindex(aero_name,'dust1')
    indx_dust2 = getindex(aero_name,'dust2')
    indx_dust3 = getindex(aero_name,'dust3')
    indx_dust4 = getindex(aero_name,'dust4')
    indx_dust5 = getindex(aero_name,'dust5')
    indx_bc1   = getindex(aero_name,'bc1')
    indx_oc1   = getindex(aero_name,'oc1')
  
    do i = 1, na
  
       if ( trim(aero_name(i)) == 'p25' ) cycle
  
       ! assign aerosol type
       select case ( trim(aero_name(i)) )
          case ('sulf')
             aerosol(i)%type  = SULFATE_AEROSOL
          case ('bc1','bc2')
             aerosol(i)%type  = BLACK_CARBON_AEROSOL
          case ('oc1','oc2')
             aerosol(i)%type  = ORGANIC_CARBON_AEROSOL
          case ('dust1','dust2','dust3','dust4','dust5')
             aerosol(i)%type  = DUST_AEROSOL
          case ('seas1')
             aerosol(i)%type  = SEASALT_SSAM_AEROSOL
          case ('seas2')
             aerosol(i)%type  = SEASALT_SSCM1_AEROSOL
          case ('seas3')
             aerosol(i)%type  = SEASALT_SSCM2_AEROSOL
          case ('seas4')
             aerosol(i)%type  = SEASALT_SSCM3_AEROSOL
       end select
  
       if ( indx_p25 > 0 ) then
          ! partition p25 to dust1 and dust2
          if ( i == indx_dust1 ) then
             aero_conc(:,i) = aero_conc(:,i)+ 0.78_r_kind*aero_conc(:,indx_p25)
          endif
          if ( i == indx_dust2 ) then
             aero_conc(:,i) = aero_conc(:,i)+ 0.22_r_kind*aero_conc(:,indx_p25)
          endif
       endif
  
       ! crtm aerosol structure
       do k = 1, km
          aerosol(i)%concentration(k) = max(tiny_r_kind, aero_conc(k,i))
          ! calculate effective radius
          aerosol(i)%effective_radius(k) &
             = GOCART_Aerosol_size(i, aerosol(i)%type, rh(k))
       enddo
  
    enddo  ! na
  
    contains
  
    function GOCART_Aerosol_size( kk, itype,  & ! Input
                                         eh ) & ! Input in 0-1
                             result( R_eff  )   ! in micrometer
    use crtm_aerosolcoeff, only: AeroC
    implicit none
!
!   modified from a function provided by Quanhua Liu
!
    integer(i_kind) ,intent(in) :: kk, itype
    real(r_kind)    ,intent(in) :: eh
  
    integer(i_kind) :: j1,j2,k
    real(r_kind)    :: h1
    real(r_kind)    :: R_eff
  
    if ( itype==DUST_AEROSOL ) then
       if (kk==indx_dust1) then
            R_eff = 0.55_r_kind
       else if (kk==indx_dust2) then
            R_eff = 1.4_r_kind
       else if (kk==indx_dust3) then
            R_eff = 2.4_r_kind
       else if (kk==indx_dust4) then
            R_eff = 4.5_r_kind
       else if (kk==indx_dust5) then
            R_eff = 8.0_r_kind
       end if
       return
    else if ( itype==BLACK_CARBON_AEROSOL .and. kk==indx_bc1 ) then
       R_eff = AeroC%Reff(1,itype )
       return
    else if ( itype==ORGANIC_CARBON_AEROSOL .and. kk==indx_oc1 ) then
       R_eff = AeroC%Reff(1,itype )
       return
    endif
  
    j2 = 0
    if ( eh < AeroC%RH(1) ) then
       j1 = 1
    else if ( eh > AeroC%RH(AeroC%n_RH) ) then
       j1 = AeroC%n_RH
    else
       do k = 1, AeroC%n_RH-1
          if ( eh <= AeroC%RH(k+1) .and. eh > AeroC%RH(k) ) then
             j1 = k
             j2 = k+1
             h1 = (eh-AeroC%RH(k))/(AeroC%RH(k+1)-AeroC%RH(k))
             exit
          endif
       enddo
    endif
  
    if ( j2 == 0 ) then
       R_eff = AeroC%Reff(j1,itype )
    else
       R_eff = (1.0_r_kind-h1)*AeroC%Reff(j1,itype ) + h1*AeroC%Reff(j2,itype )
    endif
  
    return
    end function GOCART_Aerosol_size
  
  end subroutine Set_CRTM_GOCART_Aerosol

end module set_crtm_aerosolmod
