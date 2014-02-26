! -----------------------------------------------------------------------------
!
!   2004-10-26  Quanhua Liu, Yong Han, Paul van delst, and Fuzhong Weng
!   Two-stream solution for forward and adjoint 
! -----------------------------------------------------------------------------

MODULE RT_Twostream

  USE Type_Kinds, ONLY : fp_kind
  USE CRTM_Parameters
  USE RT_Twostream_Variable

  IMPLICIT NONE

  ! Default visibility
  Private

  ! Visibale subprograms
  Public :: RT_Solution
  Public :: RT_Solution_TL
  Public :: RT_Solution_AD

  INTEGER, PARAMETER  :: SPECULAR_SURFACE = 1

   !==========================================================
   !  ------------------------------- level 1  Top atmosphere
   !       layer 1
   !  ------------------------------- level 2
   !               .
   !               .
   !               .
   !  ------------------------------- level n_level-1
   !       layer n_layer(=n_level-1)
   !  ------------------------------- level n_level, surface
   !==========================================================


CONTAINS

  subroutine RT_Solution(stype, u, alb_lay, g_lay, opt_lay, &
                         Plancka_lay, Plancks, cosmic_background, &
                         emissivity, radiance, downWelling_radiance,&
                         upWelling_radiance)


    integer :: i
    integer,       intent(in)  :: stype
    real(fp_kind), intent(in)  :: u                                                                    
    real(fp_kind), intent(in)  :: g_lay(:), alb_lay(:), opt_lay(:)
    real(fp_kind), intent(in)  :: Plancka_lay(:)
    real(fp_kind), intent(in)  :: Plancks
    real(fp_kind), intent(in)  :: cosmic_background,emissivity
    real(fp_kind), intent(out) :: radiance 
    real(fp_kind),optional, intent(out) :: downWelling_radiance,upWelling_radiance

    !--- local
!    real(fp_kind), dimension(SIZE(opt_lay)+1) :: upstream_lev, downstream_lev                                      
    !--- compute up and down streams
    upstream_lev( : ) = ZERO
    downstream_lev( : ) = ZERO

    ! --- compute two-stream radiance
    !
    call Twostream_Solution( Plancka_lay, Plancks, alb_lay, g_lay, &
                             opt_lay, cosmic_background, emissivity)


    !--- compute radiance at a given zenith angle  ( u=cos(theta) ) using
    !--- path radiance and stream radiances for multiple scattering
    call RT_Path( stype, u, alb_lay, g_lay, opt_lay, &  
                  Plancka_lay, Plancks, &
                  cosmic_background, emissivity, radiance,&
                  downWelling_radiance,upWelling_radiance)

  end subroutine RT_Solution

!
  subroutine RT_Path( stype, & ! INPUT  Lambertian, specular, or BRDF surface         
                          u, & ! INPUT  cosine of local viewing angle                 
                    alb_lay, & ! INPUT  scattering albedo                             
                      g_lay, & ! INPUT  asymmetry factor                              
                    opt_lay, & ! INPUT  optical depth                                 
                Plancka_lay, & ! INPUT  Planck radiance (skin)         
                    Plancks, & ! INPUT  Planck radiance (skin)
          cosmic_background, & ! INPUT  cosmic background radiance                    
                 emissivity, & ! INPUT  surface emissivity                            
                   radiance, & ! OUTPUT TOA radiance
                   downWelling_radiance,upWelling_radiance) 

    integer                    :: stype
    real(fp_kind), intent(in)  :: u                                                                    
    real(fp_kind), intent(in)  :: g_lay(:), alb_lay(:), opt_lay(:)
    real(fp_kind), intent(in)  :: Plancka_lay(:)
    real(fp_kind), intent(in)  :: Plancks
    real(fp_kind), intent(in)  :: cosmic_background,emissivity
    real(fp_kind), intent(out) :: radiance                                       
    real(fp_kind),optional, intent(out) :: downWelling_radiance,upWelling_radiance

    !--- local
    real(fp_kind), dimension(SIZE(opt_lay))   :: trans
    real(fp_kind) :: em, layer_em, layer_upstream, layer_downstream, &
                     term, scat_into   
    integer :: ilay, ilev1, ilev2, n_layer, n_level

    n_layer = SIZE(opt_lay)   
    n_level = n_layer + 1     
    up_r_lev = ZERO
    down_r_lev = ZERO

    trans(:) = exp(-opt_lay(:)/u)
 
    !--- Downwelling radiation
  
    down_r_lev(1) = cosmic_background
    
    DO ilay = 1, n_layer

      ilev1 = ilay
      ilev2 = ilev1 + 1
      em = ONE - trans(ilay)              
      layer_em = em * Plancka_lay(ilay) 

      !--------------------------------------------------------------
      !  Test
      !--------------------------------------------------------------
      !down_r_lev(ilev2) = down_r_lev(ilev1)*trans(ilay) + layer_em
      !IF ()
      down_r_lev(ilev2) = down_r_lev(ilev1)*trans(ilay) + layer_em
                 
      IF(alb_lay(ilay) > Smallvalue) then
        !--- scattering contribution into the propagation direction
      layer_upstream = (upstream_lev(ilev1) + upstream_lev(ilev2))/TWO
      layer_downstream = (downstream_lev(ilev1) + downstream_lev(ilev2))/TWO
        term = (ONE-g_lay(ilay))/TWO*layer_upstream + &
               (ONE+g_lay(ilay))/TWO*layer_downstream
        scat_into = em*(alb_lay(ilay)/TWO)*term
                
        down_r_lev(ilev2) = down_r_lev(ilev2) - alb_lay(ilay)*layer_em   &
                        + scat_into
      ENDIF
      
    ENDDO

    IF (present(downWelling_radiance)) downWelling_radiance=down_r_lev(n_layer+1)

    !--- Upwelling radiation
    
    if(stype == SPECULAR_SURFACE) then
       up_r_lev(n_level) = emissivity*Plancks + &
                       (ONE-emissivity)*down_r_lev(n_level)
    else
       up_r_lev(n_level) = emissivity*Plancks + &
                       (ONE-emissivity)*downstream_lev(n_level)
    endif

    IF (present(upWelling_radiance)) upWelling_radiance=0.

    DO ilay = n_layer, 1, -1
      ilev1 = ilay         
      ilev2 = ilev1 + 1    
      em = ONE - trans(ilay)                         
      layer_em = em*Plancka_lay(ilay) 
      
      up_r_lev(ilev1) = up_r_lev(ilev2)*trans(ilay)+layer_em       
                          
      IF (present(upWelling_radiance)) upWelling_radiance=upWelling_radiance*trans(ilay)+layer_em

      IF(alb_lay(ilay) > Smallvalue) then
         layer_upstream = (upstream_lev(ilev1) + upstream_lev(ilev2))/TWO
         layer_downstream = (downstream_lev(ilev1) + downstream_lev(ilev2))/TWO
         
         term = (ONE+g_lay(ilay))/TWO*layer_upstream + &
              (ONE-g_lay(ilay))/TWO*layer_downstream
         scat_into = em*(alb_lay(ilay)/TWO)*term
         up_r_lev(ilev1) = up_r_lev(ilev1)-alb_lay(ilay)*layer_em + scat_into

         IF (present(upWelling_radiance)) upWelling_radiance = upWelling_radiance-alb_lay(ilay)*layer_em + scat_into


      ENDIF
 
    ENDDO

    radiance=up_r_lev(1)
 
  END subroutine RT_Path

! -------------------------------------------------------------------------
!     Double-Adding two stream model
! -------------------------------------------------------------------------

  subroutine Twostream_Solution( &
                 Plancka_lay, & ! INPUT  layer Planck radiance (atmosphere)
                     Plancks, & ! INPUT  Planck radiance (skin)                            
                     alb_lay, & ! INPUT  layer scattering albedo            
                       g_lay, & ! INPUT  layer asymmetry factor
                     opt_lay, & ! INPUT  layer optical depth
           cosmic_background, & ! INPUT  cosmic background radiance 
                  emissivity)   ! INPUT  surface emissivity
                                                                                       
    real (fp_kind), intent(in)  :: Plancka_lay(:)
    real (fp_kind), intent(in)  :: Plancks
    real (fp_kind), intent(in)  :: alb_lay(:), g_lay(:), opt_lay(:)
    real (fp_kind), intent(in)  :: cosmic_background, emissivity

    !--- local variables
    real (fp_kind) :: ac, coef, tem, gp,gm
    real (fp_kind) :: exp1, exp2, denom
    integer        :: n_layer, ilay, ilev
                                                                                 
    n_layer = SIZE(opt_lay)

    !---------------------------------------------------------------	                         
    ! UPWARD ADDING LOOP STARTS FROM BOTTOM OF ATMOSPHERE.
    !---------------------------------------------------------------
    
    !--- initialize the composite upward stream and reflectance arrays	       
    eupc(n_layer+1)     = emissivity*Plancks       
    rupc(n_layer+1)     = ONE - emissivity 


    DO ilay = n_layer, 1, -1
   
      !--- Two stream solution of the layer:
      !--- layer transmittance and reflectance
      
      ac=sqrt((ONE - alb_lay(ilay))*(ONE - alb_lay(ilay)*g_lay(ilay)))
      gp=alb_lay(ilay)*(ONE+g_lay(ilay))
      gm=alb_lay(ilay)*(ONE-g_lay(ilay))
      
      if(ac == ZERO) then        !   conservation  
        tran_lay(ilay)=TWO/(TWO+Diffuse_Factor*opt_lay(ilay)*(TWO-gp))
        ref_lay(ilay)=ONE-tran_lay(ilay)                          
        eup_lay(ilay)=ZERO                                
        edn_lay(ilay)=ZERO                                
      else
        exp1 = exp(-Diffuse_Factor*ac*opt_lay(ilay))
        exp2 = exp1 * exp1
        Tem=ONE/(TWO*ac*(ONE+exp2)+(ONE-exp2)*(TWO-gp))
 
        !--- layer transmittance and reflectance
        tran_lay(ilay)=TWO*TWO*ac*Tem*exp1
        ref_lay(ilay)=Tem*(ONE-exp2)*gm

        !--- layer up and down-ward emission
        eup_lay(ilay)=(ONE-ref_lay(ilay)-tran_lay(ilay))*Plancka_lay(ilay)
        edn_lay(ilay)=eup_lay(ilay)
      endif

      !--- compute composite up-stream and reflectance from surface to the
      !--- layer considered. At this stage, this up-stream does not include     
      !--- the downward stream from all other layers           
      !--- above the current layer which is reflected upward by the layers     
      !--- below.            
      
      denom = ONE-rupc(ilay+1)*ref_lay(ilay)
      coef=tran_lay(ilay)/denom
      eupc(ilay)=eup_lay(ilay) &           ! up emission of the layer 
             + coef*(eupc(ilay+1)  &       ! up stream from all the layers below the layer
             + rupc(ilay+1)*edn_lay(ilay)) ! down emission of the layer reflected upward       
      rupc(ilay)=ref_lay(ilay)+coef*rupc(ilay+1)*tran_lay(ilay) 
    ENDDO

    !---------------------------------------------------------------	                         
    ! DOWNWARD ADDING LOOP STARTS FROM TOP OF ATMOSPHERE.
    !---------------------------------------------------------------

    !--- initialization.  The 0 level is used for computation convenience.
    ednc(0)=cosmic_background  
    rdnc(0)=ZERO                          

    !--- up and down streams at top of atmosphere (level 1)
    upstream_lev(1)=eupc(1)                        
    downstream_lev(1)=cosmic_background              

    DO ilay=1,n_layer
     
      ilev = ilay + 1  
 
      !--- compute composite downward stream and reflectance from TOA to the
      !--- layer considered. At this stage, this downward stream does not include     
      !--- the upward stream from all other layers below the current layer        
      !--- which is reflected downward by the layers above.
      
      coef=tran_lay(ilay)/(ONE-rdnc(ilay-1)*ref_lay(ilay))                                         
      ednc(ilay)=edn_lay(ilay) &           ! down emission of current layer
             + coef*(ednc(ilay-1) &        ! down stream from all the layers above the layer
             + rdnc(ilay-1)*eup_lay(ilay)) ! up emission of the layer reflected downward        
      rdnc(ilay)=ref_lay(ilay)+coef*rdnc(ilay-1)*tran_lay(ilay)     
          
      !--- compute up and down streams at boundary between two layers. 
      !--- The reflected componenet is added in.
      
      coef=ONE/(ONE-rupc(ilay+1)*rdnc(ilay))
      downstream_lev(ilev)=coef*( &
             ednc(ilay) &             ! composite downward stream
           + rdnc(ilay)*eupc(ilay+1)) ! composite upward stream reflected downward
      upstream_lev(ilev)=coef*( &
             eupc(ilay+1) &           ! composite upward stream
           + rupc(ilay+1)*ednc(ilay)) ! composite downward stream reflected upward
    ENDDO
      
  END SUBROUTINE Twostream_Solution

!
  subroutine RT_Solution_TL(stype, u, alb_lay, g_lay, opt_lay, &
                         Plancka_lay, Plancks, cosmic_background, &
                         emissivity, &
                         ! --- Tangent-liner input
                         alb_lay_TL, g_lay_TL, opt_lay_TL, &
                         Plancka_lay_TL, Plancks_TL, &
                         emissivity_TL, &
                         ! --- Tangent-liner output
                         radiance_TL)

    integer,       intent(in)  :: stype
    real(fp_kind), intent(in)  :: u                                                                    
    real(fp_kind), intent(in)  :: g_lay(:), alb_lay(:), opt_lay(:)
    real(fp_kind), intent(in)  :: Plancka_lay(:)
    real(fp_kind), intent(in)  :: Plancks
    real(fp_kind), intent(in)  :: cosmic_background,emissivity
    
    real(fp_kind), intent(in)  :: g_lay_TL(:), alb_lay_TL(:), opt_lay_TL(:)
    real(fp_kind), intent(in)  :: Plancka_lay_TL(:)
    real(fp_kind), intent(in)  :: Plancks_TL
    real(fp_kind), intent(in)  :: emissivity_TL
    
    real(fp_kind), intent(out) :: radiance_TL 

    !--- local
    real(fp_kind), dimension(SIZE(opt_lay)+1) :: upstream_lev_TL, downstream_lev_TL

    !--- compute up and down streams

    call Twostream_Solution_TL(Plancka_lay, Plancks, alb_lay, g_lay, &
                               opt_lay, cosmic_background, emissivity,  &   
                               Plancka_lay_TL, Plancks_TL, alb_lay_TL, g_lay_TL, &
                               opt_lay_TL, emissivity_TL,  &
                               upstream_lev_TL, downstream_lev_TL)

    !--- compute radiance at a given zenith angle  ( u=cos(theta) ) using
    !--- path radiance and stream radiances for multiple scattering

    call RT_Path_TL(stype, u, alb_lay, g_lay, opt_lay, &
                    Plancka_lay, Plancks, &
!                    upstream_lev, downstream_lev, Plancka_lay, Plancks, &
                    cosmic_background, emissivity, &                   
                    alb_lay_TL, g_lay_TL, opt_lay_TL, &
                    upstream_lev_TL, downstream_lev_TL, Plancka_lay_TL, &
                    Plancks_TL, emissivity_TL, radiance_TL)
                                                                  
  end subroutine RT_Solution_TL

!
  subroutine RT_Path_TL( stype, & ! INPUT  Lambertian, specular, or BRDF surface         
                          u, & ! INPUT  cosine of local viewing angle                 
                    alb_lay, & ! INPUT  scattering albedo                             
                      g_lay, & ! INPUT  asymmetry factor                              
                    opt_lay, & ! INPUT  optical depth                                 
                Plancka_lay, & ! INPUT  Planck radiance (skin)         
                    Plancks, & ! INPUT  Planck radiance (skin)
          cosmic_background, & ! INPUT  cosmic background radiance                    
                 emissivity, & ! INPUT  surface emissivity  
                 
                ! --- Tangent-liner input               
                 alb_lay_TL, &                          
                   g_lay_TL, &                          
                 opt_lay_TL, &                          
            upstream_lev_TL, &                          
          downstream_lev_TL, &                          
             Plancka_lay_TL, &                          
                 Plancks_TL, &                          
              emissivity_TL, &                          
                                                        
                ! --- Tangent-liner output              
                radiance_TL)    ! OUTPUT TOA radiance    

    integer                    :: stype
    real(fp_kind), intent(in)  :: u                                                                    
    real(fp_kind), intent(in)  :: g_lay(:), alb_lay(:), opt_lay(:)
    real(fp_kind), intent(in)  :: Plancka_lay(:)
    real(fp_kind), intent(in)  :: Plancks
    real(fp_kind), intent(in)  :: cosmic_background,emissivity
    real(fp_kind), intent(in)  :: g_lay_TL(:), alb_lay_TL(:), opt_lay_TL(:)
    real(fp_kind), intent(in)  :: upstream_lev_TL(:), downstream_lev_TL(:)           
    real(fp_kind), intent(in)  :: Plancka_lay_TL(:)
    real(fp_kind), intent(in)  :: Plancks_TL
    real(fp_kind), intent(in)  :: emissivity_TL

    real(fp_kind), intent(out) :: radiance_TL
      
    !--- local
    real(fp_kind), dimension(SIZE(opt_lay)+1) :: down_r_lev_TL, up_r_lev_TL 
    real(fp_kind), dimension(SIZE(opt_lay))   :: trans, trans_TL
    real(fp_kind) :: em, layer_em, layer_upstream, layer_downstream, &
                     term, scat_into, &
                     em_TL, layer_em_TL, layer_upstream_TL, layer_downstream_TL, &
                     term_TL, scat_into_TL
    integer :: ilay, ilev1, ilev2, n_layer, n_level

    n_layer = SIZE(opt_lay)   
    n_level = n_layer + 1     

    trans(:) = exp(-opt_lay(:)/u)

    trans_TL(:) = -trans(:)*opt_lay_TL(:)/u
      
    !--- Downwelling radiation
  
    down_r_lev(1) = cosmic_background

    down_r_lev_TL(1) = ZERO
    
    DO ilay = 1, n_layer

      ilev1 = ilay
      ilev2 = ilev1 + 1

      em = ONE - trans(ilay)              
      layer_em = em * Plancka_lay(ilay) 
      
      down_r_lev(ilev2) = down_r_lev(ilev1)*trans(ilay) + layer_em
      em_TL = -trans_TL(ilay)
      layer_em_TL = em_TL * Plancka_lay(ilay) + em * Plancka_lay_TL(ilay)
      down_r_lev_TL(ilev2) = down_r_lev_TL(ilev1)*trans(ilay) + &
                         down_r_lev(ilev1)*trans_TL(ilay) + layer_em_TL
      IF(alb_lay(ilay) > Smallvalue) then
     
        !--- scattering contribution into the propagation direction
      layer_upstream = (upstream_lev(ilev1) + upstream_lev(ilev2))/TWO
      layer_downstream = (downstream_lev(ilev1) + downstream_lev(ilev2))/TWO
      layer_upstream_TL = (upstream_lev_TL(ilev1) + upstream_lev_TL(ilev2))/TWO
      layer_downstream_TL = (downstream_lev_TL(ilev1) + downstream_lev_TL(ilev2))/TWO

        term = (ONE-g_lay(ilay))/TWO*layer_upstream + &
               (ONE+g_lay(ilay))/TWO*layer_downstream
        scat_into = em*(alb_lay(ilay)/TWO)*term

        down_r_lev(ilev2) = down_r_lev(ilev2) - alb_lay(ilay)*layer_em   &
                           + scat_into
        !--- scattering contribution into the propagation direction
        term_TL = -g_lay_TL(ilay)/TWO*layer_upstream + &
                  (ONE-g_lay(ilay))/TWO*layer_upstream_TL + &
                  g_lay_TL(ilay)/TWO*layer_downstream + &
                  (ONE+g_lay(ilay))/TWO*layer_downstream_TL

        scat_into_TL = em_TL*(alb_lay(ilay)/TWO)*term + &
                       em*(alb_lay_TL(ilay)/TWO)*term + &
                       em*(alb_lay(ilay)/TWO)*term_TL                             
        down_r_lev_TL(ilev2) = down_r_lev_TL(ilev2) - alb_lay_TL(ilay)*layer_em -  &
                               alb_lay(ilay)*layer_em_TL + scat_into_TL
      ENDIF
      
    ENDDO
 
    !--- Upwelling radiation
    
    if(stype == SPECULAR_SURFACE) then
    
       up_r_lev(n_level) = emissivity*Plancks + &
                           (ONE-emissivity)*down_r_lev(n_level)
       up_r_lev_TL(n_level) = emissivity_TL*Plancks + emissivity*Plancks_TL &
                             -emissivity_TL*down_r_lev(n_level) &
                             +(ONE-emissivity)*down_r_lev_TL(n_level)
    else
       up_r_lev(n_level) = emissivity*Plancks + &
                           (ONE-emissivity)*downstream_lev(n_level)
       up_r_lev_TL(n_level) = emissivity_TL*Plancks + emissivity*Plancks_TL &
                          -emissivity_TL*downstream_lev(n_level) &
                          +(ONE-emissivity)*downstream_lev_TL(n_level)
    endif
    
    DO ilay = n_layer, 1, -1
      ilev1 = ilay         
      ilev2 = ilev1 + 1    
      em = ONE - trans(ilay)                         
      layer_em = em*Plancka_lay(ilay) 
      up_r_lev(ilev1) = up_r_lev(ilev2)*trans(ilay)+layer_em       
      em_TL = -trans_TL(ilay)
      layer_em_TL = em_TL * Plancka_lay(ilay) + em * Plancka_lay_TL(ilay)
      up_r_lev_TL(ilev1) = up_r_lev_TL(ilev2)*trans(ilay) + &
                           up_r_lev(ilev2)*trans_TL(ilay) + layer_em_TL       

      IF(alb_lay(ilay) > Smallvalue) then

      layer_upstream = (upstream_lev(ilev1) + upstream_lev(ilev2))/TWO
      layer_downstream = (downstream_lev(ilev1) + downstream_lev(ilev2))/TWO
      layer_upstream_TL = (upstream_lev_TL(ilev1) + upstream_lev_TL(ilev2))/TWO
      layer_downstream_TL = (downstream_lev_TL(ilev1) + downstream_lev_TL(ilev2))/TWO
        term = (ONE+g_lay(ilay))/TWO*layer_upstream + &
               (ONE-g_lay(ilay))/TWO*layer_downstream
        scat_into = em*(alb_lay(ilay)/TWO)*term
        up_r_lev(ilev1) = up_r_lev(ilev1)-alb_lay(ilay)*layer_em  &
                        + scat_into
        term_TL = g_lay_TL(ilay)/TWO*layer_upstream + &
                  (ONE+g_lay(ilay))/TWO*layer_upstream_TL - &
                  g_lay_TL(ilay)/TWO*layer_downstream + &
                  (ONE-g_lay(ilay))/TWO*layer_downstream_TL
        scat_into_TL = em_TL*(alb_lay(ilay)/TWO)*term + &
                       em*(alb_lay_TL(ilay)/TWO)*term + &
                       em*(alb_lay(ilay)/TWO)*term_TL
        up_r_lev_TL(ilev1) = up_r_lev_TL(ilev1) - alb_lay_TL(ilay)*layer_em -  &
                             alb_lay(ilay)*layer_em_TL + scat_into_TL

      ENDIF
      
    ENDDO
    
    radiance_TL = up_r_lev_TL(1)
 
  END subroutine RT_Path_TL

! -------------------------------------------------------------------------
!     Double-Adding two stream TL model
! -------------------------------------------------------------------------

  subroutine Twostream_Solution_TL( &
                 Plancka_lay, & ! INPUT  layer Planck radiance (atmosphere)
                     Plancks, & ! INPUT  Planck radiance (skin)                            
                     alb_lay, & ! INPUT  layer scattering albedo            
                       g_lay, & ! INPUT  layer asymmetry factor
                     opt_lay, & ! INPUT  layer optical depth
           cosmic_background, & ! INPUT  cosmic background radiance 
                  emissivity, & ! INPUT  surface emissivity

                 ! --- Tangent-liner input
                 Plancka_lay_TL, & 
                     Plancks_TL, &                 
                     alb_lay_TL, &  
                       g_lay_TL, & 
                     opt_lay_TL, & 
                  emissivity_TL, & 
                  
                 ! -- Tangent-liner output
                 upstream_lev_TL, &                           
               downstream_lev_TL  ) 
                                                                                       
    real (fp_kind), intent(in)  :: Plancka_lay(:)
    real (fp_kind), intent(in)  :: Plancks
    real (fp_kind), intent(in)  :: alb_lay(:), g_lay(:), opt_lay(:)
    real (fp_kind), intent(in)  :: cosmic_background, emissivity

    real (fp_kind), intent(in)  :: Plancka_lay_TL(:)
    real (fp_kind), intent(in)  :: Plancks_TL
    real (fp_kind), intent(in)  :: alb_lay_TL(:), g_lay_TL(:), opt_lay_TL(:)                                     
    real (fp_kind), intent(in)  :: emissivity_TL
    
    real (fp_kind), intent(out) :: upstream_lev_TL(:), downstream_lev_TL(:)

    !--- local variables
    real (fp_kind), dimension(SIZE(opt_lay)) :: eup_lay_TL, edn_lay_TL, ref_lay_TL,tran_lay_TL
    real (fp_kind), dimension(SIZE(opt_lay)+1) :: eupc_TL, rupc_TL
    real (fp_kind), dimension(0:SIZE(opt_lay)) ::  ednc_TL, rdnc_TL
    real (fp_kind) :: ac, coef, tem, gp, gm, &
                      ac_TL, coef_TL, tem_TL, gp_TL, gm_TL
    real (fp_kind) :: exp1, exp2, denom, exp1_TL, exp2_TL, denom_TL
    integer        :: n_layer, ilay, ilev
                                                                                 
    n_layer = SIZE(opt_lay)

    !---------------------------------------------------------------	                         
    ! UPWARD ADDING LOOP STARTS FROM BOTTOM OF ATMOSPHERE.
    !---------------------------------------------------------------
    
    !--- initialize the composite upward stream and reflectance arrays	       
    eupc(n_layer+1)     = emissivity*Plancks       
    rupc(n_layer+1)     = ONE - emissivity 

    eupc_TL(n_layer+1)     = emissivity_TL*Plancks + emissivity*Plancks_TL        
    rupc_TL(n_layer+1)     = -emissivity_TL 

    DO ilay = n_layer, 1, -1
   
      !--- Two stream solution of the layer:
      !--- layer transmittance and reflectance
      
      ac=sqrt((ONE - alb_lay(ilay))*(ONE - alb_lay(ilay)*g_lay(ilay)))
      gp=alb_lay(ilay)*(ONE+g_lay(ilay))
      gm=alb_lay(ilay)*(ONE-g_lay(ilay))

      !--- ac_TL will computed in the IF block.
      
      gp_TL=alb_lay_TL(ilay)*(ONE+g_lay(ilay)) + alb_lay(ilay)*g_lay_TL(ilay)
      gm_TL=alb_lay_TL(ilay)*(ONE-g_lay(ilay)) - alb_lay(ilay)*g_lay_TL(ilay)
      
      if(ac == ZERO) then        !   conservation  
      
        tran_lay(ilay)=TWO/(TWO+Diffuse_Factor*opt_lay(ilay)*(TWO-gp))
        ref_lay(ilay)=ONE-tran_lay(ilay)                          

        eup_lay(ilay)=ZERO                                
        edn_lay(ilay)=ZERO 
        
        tran_lay_TL(ilay) = -(tran_lay(ilay)*tran_lay(ilay)/TWO)* &
               (Diffuse_Factor*opt_lay_TL(ilay)*(TWO-gp)-Diffuse_Factor*opt_lay(ilay)*gp_TL)

        ref_lay_TL(ilay) = -tran_lay_TL(ilay)

        eup_lay_TL(ilay)=ZERO                                
        edn_lay_TL(ilay)=ZERO 
        
      else

        ac_TL=-( alb_lay_TL(ilay)*(ONE - alb_lay(ilay)*g_lay(ilay)) &
               +(ONE-alb_lay(ilay))*alb_lay_TL(ilay)*g_lay(ilay) &
               +(ONE-alb_lay(ilay))*alb_lay(ilay)*g_lay_TL(ilay) ) / (TWO*ac)

        exp1 = exp(-Diffuse_Factor*ac*opt_lay(ilay))
        exp2 = exp1 * exp1

        Tem=ONE/(TWO*ac*(ONE+exp2)+(ONE-exp2)*(TWO-gp))
        
        !--- layer transmittance and reflectance
        tran_lay(ilay)=TWO*TWO*ac*Tem*exp1
        ref_lay(ilay)=Tem*(ONE-exp2)*gm

        !--- layer up and down-ward emission
        eup_lay(ilay)=(ONE-ref_lay(ilay)-tran_lay(ilay))*Plancka_lay(ilay)
        edn_lay(ilay)=eup_lay(ilay)
                    
        exp1_TL = -Diffuse_Factor * exp1 * (ac_TL*opt_lay(ilay) + ac*opt_lay_TL(ilay)) 
        exp2_TL = TWO * exp1 * exp1_TL
        
        Tem_TL = -Tem*Tem*(TWO*(ONE+exp2)*ac_TL + (TWO*ac-TWO+gp)*exp2_TL - &
                          (ONE-exp2)*gp_TL)
        
        tran_lay_TL(ilay)=TWO*TWO*(ac_TL*Tem*exp1+ac*Tem_TL*exp1+ac*Tem*exp1_TL)
        ref_lay_TL(ilay)=Tem_TL*(ONE-exp2)*gm-Tem*exp2_TL*gm+ &
                         Tem*(ONE-exp2)*gm_TL

        eup_lay_TL(ilay)=-(ref_lay_TL(ilay)+tran_lay_TL(ilay))*Plancka_lay(ilay) + &
                          (ONE-ref_lay(ilay)-tran_lay(ilay))*Plancka_lay_TL(ilay)
        edn_lay_TL(ilay)=eup_lay_TL(ilay)
        
      endif

      !--- compute composite up-stream and reflectance from surface to the
      !--- layer considered. At this stage, this up-stream does not include     
      !--- the downward stream from all other layers           
      !--- above the current layer which is reflected upward by the layers     
      !--- below.            
      
      denom = ONE-rupc(ilay+1)*ref_lay(ilay)
      coef=tran_lay(ilay)/denom

      eupc(ilay)=eup_lay(ilay) &           ! up emission of the layer 
         + coef*(eupc(ilay+1)  &       ! up stream from all the layers below the layer
         + rupc(ilay+1)*edn_lay(ilay)) ! down emission of the layer reflected upward       
      rupc(ilay)=ref_lay(ilay)+coef*rupc(ilay+1)*tran_lay(ilay) 

      denom_TL = -rupc_TL(ilay+1)*ref_lay(ilay)-rupc(ilay+1)*ref_lay_TL(ilay)
      coef_TL = tran_lay_TL(ilay)/denom - (tran_lay(ilay)/(denom*denom))*denom_TL
        
      eupc_TL(ilay) = eup_lay_TL(ilay) + &
                     coef_TL*(eupc(ilay+1) + rupc(ilay+1)*edn_lay(ilay)) + &
                     coef*(eupc_TL(ilay+1) + rupc_TL(ilay+1)*edn_lay(ilay) + &
                     rupc(ilay+1)*edn_lay_TL(ilay))
      rupc_TL(ilay)=ref_lay_TL(ilay) + coef_TL*rupc(ilay+1)*tran_lay(ilay) + &
                    coef*(rupc_TL(ilay+1)*tran_lay(ilay) + &
                          rupc(ilay+1)*tran_lay_TL(ilay))
    ENDDO

    !---------------------------------------------------------------	                         
    ! DOWNWARD ADDING LOOP STARTS FROM TOP OF ATMOSPHERE.
    !---------------------------------------------------------------

    !--- initialization.  The 0 level is used for computation convenience.
    ednc(0)=cosmic_background  
    rdnc(0)=ZERO                          

    ednc_TL(0)=ZERO  
    rdnc_TL(0)=ZERO                          

    !--- up and down streams at top of atmosphere (level 1)
    upstream_lev_TL(1)=eupc_TL(1)                        
    downstream_lev_TL(1)=ZERO
    
    DO ilay=1,n_layer
     
      ilev = ilay + 1  
 
      !--- compute composite downward stream and reflectance from TOA to the
      !--- layer considered. At this stage, this downward stream does not include     
      !--- the upward stream from all other layers below the current layer        
      !--- which is reflected downward by the layers above.

      denom = ONE-rdnc(ilay-1)*ref_lay(ilay)     
      coef = tran_lay(ilay)/denom
      ednc(ilay)=edn_lay(ilay) &           ! down emission of current layer
          + coef*(ednc(ilay-1) &        ! down stream from all the layers above the layer
             + rdnc(ilay-1)*eup_lay(ilay)) ! up emission of the layer reflected downward        
      rdnc(ilay)=ref_lay(ilay)+coef*rdnc(ilay-1)*tran_lay(ilay)     

      denom_TL = -rdnc_TL(ilay-1)*ref_lay(ilay) - rdnc(ilay-1)*ref_lay_TL(ilay)
      coef_TL = tran_lay_TL(ilay)/denom - (tran_lay(ilay)/(denom*denom))*denom_TL 
      ednc_TL(ilay)=edn_lay_TL(ilay) + &
      coef_TL*(ednc(ilay-1)+ rdnc(ilay-1)*eup_lay(ilay)) + & 
      coef*(ednc_TL(ilay-1)+ rdnc_TL(ilay-1)*eup_lay(ilay) + &
                                     rdnc(ilay-1)*eup_lay_TL(ilay))
      rdnc_TL(ilay)=ref_lay_TL(ilay) + coef_TL*rdnc(ilay-1)*tran_lay(ilay) + &
                    coef*(rdnc_TL(ilay-1)*tran_lay(ilay) + &
                          rdnc(ilay-1)*tran_lay_TL(ilay))
                          
      !--- compute up and down streams at boundary between two layers. 
      !--- The reflected componenet is added in.
      
      coef=ONE/(ONE-rupc(ilay+1)*rdnc(ilay))

      coef_TL=coef*coef*(rupc_TL(ilay+1)*rdnc(ilay)+rupc(ilay+1)*rdnc_TL(ilay))
      downstream_lev_TL(ilev)=coef_TL*(ednc(ilay)+rdnc(ilay)*eupc(ilay+1)) + &
                        coef*(ednc_TL(ilay)+rdnc_TL(ilay)*eupc(ilay+1) + &
                                                 rdnc(ilay)*eupc_TL(ilay+1))
      upstream_lev_TL(ilev)=coef_TL*(eupc(ilay+1)+rupc(ilay+1)*ednc(ilay)) + &
                        coef*(eupc_TL(ilay+1)+rupc_TL(ilay+1)*ednc(ilay) + &
                                              rupc(ilay+1)*ednc_TL(ilay))
    ENDDO

    END subroutine Twostream_Solution_TL

! 
   subroutine RT_Solution_AD(stype, u, alb_lay, g_lay, opt_lay, &
                         Plancka_lay, Plancks, cosmic_background, &
                         emissivity, &
                         
                         !--- Adjoint input
                         radiance_AD, &
                         
                         ! --- Adjoint in/output
                         alb_lay_AD, g_lay_AD, opt_lay_AD, &
                         Plancka_lay_AD, Plancks_AD, &
                         emissivity_AD)

    integer,       intent(in)  :: stype
    real(fp_kind), intent(in)  :: u                                                                    
    real(fp_kind), dimension( : ), intent(in)  :: g_lay, alb_lay, opt_lay
    real(fp_kind), dimension( : ), intent(in)  :: Plancka_lay
    real(fp_kind), intent(in)  :: Plancks
    real(fp_kind), intent(in)  :: cosmic_background,emissivity
    
    real(fp_kind), intent(in)  :: radiance_AD 

    real(fp_kind), dimension( : ), intent(inout)  :: g_lay_AD, alb_lay_AD, opt_lay_AD
    real(fp_kind), dimension( : ), intent(inout)  :: Plancka_lay_AD
    real(fp_kind), intent(inout)  :: Plancks_AD
    real(fp_kind), intent(inout)  :: emissivity_AD

    !--- local
    real(fp_kind), dimension(SIZE(opt_lay)+1) :: upstream_lev_AD, downstream_lev_AD
   
    integer :: i
 
    !--- compute radiance at a given zenith angle  ( u=cos(theta) ) using
    !--- path radiance and stream radiances for multiple scattering

    downstream_lev_AD = ZERO
    upstream_lev_AD = ZERO

    opt_lay_AD = ZERO
    alb_lay_AD = ZERO
    g_lay_AD = ZERO

    !---- compute adjoint part in path radiance 
    call RT_Path_AD(stype, u, alb_lay, g_lay, opt_lay, & 
                     Plancka_lay, Plancks, &
                    cosmic_background, emissivity, & 
                    radiance_AD, &                  
                    alb_lay_AD, g_lay_AD, opt_lay_AD, &    
                    upstream_lev_AD, downstream_lev_AD, Plancka_lay_AD, &
                    Plancks_AD, emissivity_AD)

    !---- compute adjoint in two-steam part 
    call Twostream_Solution_AD(Plancka_lay, Plancks, alb_lay, g_lay, &
                               opt_lay, cosmic_background, emissivity,  &   
                               upstream_lev_AD, downstream_lev_AD, &
                               Plancka_lay_AD, Plancks_AD, alb_lay_AD, g_lay_AD, &
                               opt_lay_AD, emissivity_AD)

  END SUBROUTINE RT_Solution_AD 

!
  subroutine RT_Path_AD(stype, & ! INPUT  Lambertian, specular, or BRDF surface         
                          u, & ! INPUT  cosine of local viewing angle                 
                    alb_lay, & ! INPUT  scattering albedo                             
                      g_lay, & ! INPUT  asymmetry factor                              
                    opt_lay, & ! INPUT  optical depth                                 
                Plancka_lay, & ! INPUT  Planck radiance (skin)         
                    Plancks, & ! INPUT  Planck radiance (skin)
          cosmic_background, & ! INPUT  cosmic background radiance                    
                 emissivity, & ! INPUT  surface emissivity  
                 
                ! --- Ajoint input               
                radiance_AD, &
                
                ! --- Ajoint in/output
                 alb_lay_AD, &                          
                   g_lay_AD, &                          
                 opt_lay_AD, &                          
            upstream_lev_AD, &                          
          downstream_lev_AD, &                          
             Plancka_lay_AD, &                          
                 Plancks_AD, &                          
              emissivity_AD)

    integer                    :: stype
    real(fp_kind), intent(in)  :: u 
    real(fp_kind), intent(in)  :: g_lay(:), alb_lay(:), opt_lay(:)
!    real(fp_kind), intent(in)  :: upstream_lev(:), downstream_lev(:)           
    real(fp_kind), intent(in)  :: Plancka_lay(:)
    real(fp_kind), intent(in)  :: Plancks
    real(fp_kind), intent(in)  :: cosmic_background,emissivity
    real(fp_kind), intent(in)  :: radiance_AD
    
    real(fp_kind), intent(inout)  :: g_lay_AD(:), alb_lay_AD(:), opt_lay_AD(:)
    real(fp_kind), intent(inout)  :: upstream_lev_AD(:), downstream_lev_AD(:)           
    real(fp_kind), intent(inout)  :: Plancka_lay_AD(:)
    real(fp_kind), intent(inout)  :: Plancks_AD
    real(fp_kind), intent(inout)  :: emissivity_AD

      
    !--- local
    real(fp_kind), dimension(SIZE(opt_lay)+1) :: down_r_lev_AD, up_r_lev_AD 
    real(fp_kind), dimension(SIZE(opt_lay))   :: trans, trans_AD
    real(fp_kind) :: em, layer_em, layer_upstream, layer_downstream, &
                     term, scat_into, &
                     em_AD, layer_em_AD, layer_upstream_AD, layer_downstream_AD, &
                     term_AD, scat_into_AD
    integer :: ilay, ilev1, ilev2, n_layer, n_level
  
    up_r_lev_AD = ZERO
    down_r_lev_AD = ZERO
    trans_AD = ZERO

      layer_em_AD = ZERO
      em_AD = ZERO
      term_AD = ZERO
      scat_into_AD = ZERO
      layer_downstream_AD = ZERO
      layer_upstream_AD = ZERO

    up_r_lev_AD(1) = radiance_AD
    n_layer = SIZE(opt_lay)
    n_level = n_layer + 1
    trans(:) = exp(-opt_lay(:)/u)

    DO ilay = 1, n_layer
      ilev1 = ilay
      ilev2 = ilev1 + 1
      em = ONE - trans(ilay)
      em_AD = ZERO 
      layer_em_AD = ZERO
      layer_em = em*Plancka_lay(ilay)
       
      IF(alb_lay(ilay) > Smallvalue) then
      layer_upstream = (upstream_lev(ilev1) + upstream_lev(ilev2))/TWO
      layer_downstream = (downstream_lev(ilev1) + downstream_lev(ilev2))/TWO

        term = (ONE+g_lay(ilay))/TWO*layer_upstream + &
               (ONE-g_lay(ilay))/TWO*layer_downstream
        scat_into = em*(alb_lay(ilay)/TWO)*term

      scat_into_AD = up_r_lev_AD(ilev1)
      layer_em_AD = -alb_lay(ilay)*up_r_lev_AD(ilev1)
      alb_lay_AD(ilay) = alb_lay_AD(ilay)-up_r_lev_AD(ilev1)*layer_em
      term_AD = em*(alb_lay(ilay)/TWO)*scat_into_AD

      alb_lay_AD(ilay) = alb_lay_AD(ilay)+em*scat_into_AD/TWO*term
      em_AD = scat_into_AD*(alb_lay(ilay)/TWO)*term

      layer_downstream_AD = (ONE-g_lay(ilay))/TWO*term_AD
      g_lay_AD(ilay) = -term_AD/TWO*layer_downstream
      layer_upstream_AD = (ONE+g_lay(ilay))/TWO*term_AD
      g_lay_AD(ilay) = g_lay_AD(ilay) +term_AD/TWO*layer_upstream

      upstream_lev_AD(ilev1) = upstream_lev_AD(ilev1) + layer_upstream_AD/TWO
      upstream_lev_AD(ilev2) = upstream_lev_AD(ilev2) + layer_upstream_AD/TWO
      downstream_lev_AD(ilev1) = downstream_lev_AD(ilev1) + layer_downstream_AD/TWO
      downstream_lev_AD(ilev2) = downstream_lev_AD(ilev2) + layer_downstream_AD/TWO

      ENDIF

      layer_em_AD = layer_em_AD + up_r_lev_AD(ilev1)
      trans_AD(ilay) = trans_AD(ilay) + up_r_lev(ilev2)* up_r_lev_AD(ilev1)
      up_r_lev_AD(ilev2) = up_r_lev_AD(ilev2)+up_r_lev_AD(ilev1) *trans(ilay)

!
      Plancka_lay_AD(ilay) = Plancka_lay_AD(ilay) + em*layer_em_AD
      em_AD = em_AD + layer_em_AD * Plancka_lay(ilay)
      trans_AD(ilay) = trans_AD(ilay) - em_AD
! test
      up_r_lev_AD(ilev1) = ZERO
      em_AD = ZERO

    ENDDO

   !--- Upwelling radiation
    if(stype == SPECULAR_SURFACE) then
       down_r_lev_AD(n_level)=down_r_lev_AD(n_level)+(ONE-emissivity)*up_r_lev_AD(n_level)
       emissivity_AD=emissivity_AD-up_r_lev_AD(n_level)*down_r_lev(n_level)
       Plancks_AD=Plancks_AD+emissivity*up_r_lev_AD(n_level)
       emissivity_AD=emissivity_AD+up_r_lev_AD(n_level)*Plancks

      up_r_lev_AD(n_level) = ZERO
    else
       downstream_lev_AD(n_level)=downstream_lev_AD(n_level)+(ONE-emissivity)*up_r_lev_AD(n_level)
       emissivity_AD=emissivity_AD-up_r_lev_AD(n_level)*downstream_lev(n_level)
       Plancks_AD=Plancks_AD+emissivity*up_r_lev_AD(n_level)
       emissivity_AD=emissivity_AD+up_r_lev_AD(n_level)*Plancks

       up_r_lev_AD(n_level)=ZERO
    endif

    DO ilay = n_layer, 1, -1
      ilev1 = ilay
      ilev2 = ilev1 + 1
      em_AD = ZERO 
      layer_em_AD = ZERO

      em = ONE - trans(ilay)
      layer_em = em * Plancka_lay(ilay)

      IF(alb_lay(ilay) > Smallvalue) then
        !--- scattering contribution into the propagation direction
      layer_upstream = (upstream_lev(ilev1) + upstream_lev(ilev2))/TWO
      layer_downstream = (downstream_lev(ilev1) + downstream_lev(ilev2))/TWO
        term = (ONE-g_lay(ilay))/TWO*layer_upstream + &
               (ONE+g_lay(ilay))/TWO*layer_downstream
        scat_into = em*(alb_lay(ilay)/TWO)*term
        !--- scattering contribution into the propagation direction
        scat_into_AD = down_r_lev_AD(ilev2)
        layer_em_AD = -alb_lay(ilay)*down_r_lev_AD(ilev2)
        alb_lay_AD(ilay) = alb_lay_AD(ilay)-down_r_lev_AD(ilev2)*layer_em

!  test
!        down_r_lev_AD(ilev2) = ZERO

        term_AD = em*(alb_lay(ilay)/TWO)*scat_into_AD
        alb_lay_AD(ilay) = alb_lay_AD(ilay)+em*(scat_into_AD)/TWO*term
        em_AD = scat_into_AD*(alb_lay(ilay)/TWO)*term

!  test
        scat_into_AD = ZERO

        layer_downstream_AD=(ONE+g_lay(ilay))/TWO*term_AD
        g_lay_AD(ilay)=g_lay_AD(ilay)+term_AD/TWO*layer_downstream
        layer_upstream_AD=(ONE-g_lay(ilay))/TWO*term_AD
        g_lay_AD(ilay)=g_lay_AD(ilay)-term_AD/TWO*layer_upstream

!  test
        term_AD = ZERO

      upstream_lev_AD(ilev1) = upstream_lev_AD(ilev1) + layer_upstream_AD/TWO
      upstream_lev_AD(ilev2) = upstream_lev_AD(ilev2) + layer_upstream_AD/TWO
      downstream_lev_AD(ilev1) = downstream_lev_AD(ilev1) + layer_downstream_AD/TWO
      downstream_lev_AD(ilev2) = downstream_lev_AD(ilev2) + layer_downstream_AD/TWO

!  test
      layer_upstream_AD = ZERO
      layer_downstream_AD = ZERO

      ENDIF

!!      layer_em_AD=down_r_lev_AD(ilev2)
      layer_em_AD=layer_em_AD+down_r_lev_AD(ilev2)
      trans_AD(ilay)=trans_AD(ilay)+down_r_lev(ilev1)*down_r_lev_AD(ilev2)
      down_r_lev_AD(ilev1)=down_r_lev_AD(ilev1)+down_r_lev_AD(ilev2)*trans(ilay)
! test
!       down_r_lev_AD(ilev2) = ZERO
      
      Plancka_lay_AD(ilay)=Plancka_lay_AD(ilay)+em * layer_em_AD
      em_AD = em_AD + layer_em_AD* Plancka_lay(ilay)

      trans_AD(ilay)=trans_AD(ilay)-em_AD

    ENDDO

    down_r_lev_AD(1)=ZERO
    opt_lay_AD(:) = opt_lay_AD(:)-trans(:)*trans_AD(:)/u

    !--- Downwelling radiation
 
 END subroutine RT_Path_AD  
        
   subroutine Twostream_Solution_AD( &
                Plancka_lay, & ! INPUT  Planck radiance (skin)         
                    Plancks, & ! INPUT  Planck radiance (skin)
                    alb_lay, & ! INPUT  scattering albedo                             
                      g_lay, & ! INPUT  asymmetry factor                              
                    opt_lay, & ! INPUT  optical depth                                 
          cosmic_background, & ! INPUT  cosmic background radiance                    
                 emissivity, & ! INPUT  surface emissivity  
            upstream_lev_AD, &                          
          downstream_lev_AD, &                          
             Plancka_lay_AD, &                          
                 Plancks_AD, &                          
                 alb_lay_AD, &                          
                   g_lay_AD, &                          
                 opt_lay_AD, &                          
              emissivity_AD)

    real(fp_kind), intent(in)  :: Plancks,cosmic_background,emissivity
    real(fp_kind), dimension( : ), intent(in)  :: g_lay, alb_lay, opt_lay
    real(fp_kind), dimension( : ), intent(in)  :: Plancka_lay
    real(fp_kind), dimension( : ), intent(inout)  :: g_lay_AD, alb_lay_AD, opt_lay_AD
    real(fp_kind), dimension( : ), intent(inout)  :: upstream_lev_AD, downstream_lev_AD 
    real(fp_kind), dimension( : ), intent(inout)  :: Plancka_lay_AD
    real(fp_kind), intent(inout)  :: Plancks_AD
    real(fp_kind), intent(inout)  :: emissivity_AD

    real (fp_kind), dimension(SIZE(opt_lay)) :: eup_lay_AD, edn_lay_AD, ref_lay_AD,tran_lay_AD
    real (fp_kind), dimension(SIZE(opt_lay)+1) :: eupc_AD, rupc_AD
    real (fp_kind), dimension(0:SIZE(opt_lay)) :: ednc_AD, rdnc_AD
    real (fp_kind) :: ac, coef, tem, gp, gm, &
                      ac_AD, coef_AD, tem_AD, gp_AD, gm_AD
    real (fp_kind) :: exp1, exp2, denom, exp1_AD, exp2_AD, denom_AD
    integer        :: n_layer, ilay, ilev
     !--- initializaing

    n_layer = SIZE(opt_lay)
      tran_lay_AD = ZERO
      ref_lay_AD = ZERO
      eup_lay_AD = ZERO
      edn_lay_AD = ZERO
      ednc_AD = ZERO
      rupc_AD = ZERO
      eupc_AD = ZERO
      rdnc_AD = ZERO

      ac_AD = ZERO
      coef_AD = ZERO
      tem_AD = ZERO
      gp_AD = ZERO
      gm_AD = ZERO
      exp1_AD = ZERO
      exp2_AD = ZERO
      denom_AD = ZERO
        
   DO ilay=n_layer, 1, -1
      ilev = ilay + 1
      !--- compute composite downward stream and reflectance from TOA to the
      !--- layer considered. At this stage, this downward stream does not include
      !--- the upward stream from all other layers below the current layer
      !--- which is reflected downward by the layers above.
      denom = ONE-rdnc(ilay-1)*ref_lay(ilay)
      coef = tran_lay(ilay)/denom

      !--- compute up and down streams at boundary between two layers.
      !--- The reflected componenet is added in.
      coef=ONE/(ONE-rupc(ilay+1)*rdnc(ilay))
      ednc_AD(ilay)=ednc_AD(ilay)+coef*rupc(ilay+1)*upstream_lev_AD(ilev)
      rupc_AD(ilay+1)=rupc_AD(ilay+1)+coef*upstream_lev_AD(ilev)*ednc(ilay)
      eupc_AD(ilay+1)=eupc_AD(ilay+1)+coef*upstream_lev_AD(ilev)
      coef_AD = upstream_lev_AD(ilev)*(eupc(ilay+1)+rupc(ilay+1)*ednc(ilay))

      eupc_AD(ilay+1)=eupc_AD(ilay+1)+coef*rdnc(ilay)*downstream_lev_AD(ilev)
      rdnc_AD(ilay)= rdnc_AD(ilay)+coef*downstream_lev_AD(ilev)*eupc(ilay+1)
      ednc_AD(ilay)=ednc_AD(ilay)+coef*downstream_lev_AD(ilev)
      coef_AD = coef_AD + downstream_lev_AD(ilev) * (ednc(ilay)+rdnc(ilay)*eupc(ilay+1))

      rdnc_AD(ilay)=rdnc_AD(ilay)+coef*coef*rupc(ilay+1)*coef_AD
      rupc_AD(ilay+1)=rupc_AD(ilay+1)+coef*coef*coef_AD*rdnc(ilay)

      coef_AD = ZERO

      !--- compute composite downward stream and reflectance from TOA to the
      !--- layer considered. At this stage, this downward stream does not include
      !--- the upward stream from all other layers below the current layer
      !--- which is reflected downward by the layers above.
                                                                                                              
      denom = ONE-rdnc(ilay-1)*ref_lay(ilay)
      coef = tran_lay(ilay)/denom

      tran_lay_AD(ilay)=tran_lay_AD(ilay)+coef*rdnc(ilay-1)*rdnc_AD(ilay)
      rdnc_AD(ilay-1)=rdnc_AD(ilay-1)+coef*rdnc_AD(ilay)*tran_lay(ilay)
      coef_AD = rdnc_AD(ilay)*rdnc(ilay-1)*tran_lay(ilay)
      ref_lay_AD(ilay)= ref_lay_AD(ilay)+rdnc_AD(ilay)

      eup_lay_AD(ilay)=eup_lay_AD(ilay)+coef*rdnc(ilay-1)*ednc_AD(ilay)
      rdnc_AD(ilay-1)=rdnc_AD(ilay-1)+coef*ednc_AD(ilay)*eup_lay(ilay)
      ednc_AD(ilay-1)=ednc_AD(ilay-1)+coef*ednc_AD(ilay)
      coef_AD = coef_AD +ednc_AD(ilay)*(ednc(ilay-1)+ rdnc(ilay-1)*eup_lay(ilay))
      edn_lay_AD(ilay)=edn_lay_AD(ilay)+ednc_AD(ilay)

       denom_AD = - (tran_lay(ilay)/(denom*denom))*coef_AD
       tran_lay_AD(ilay)=tran_lay_AD(ilay)+coef_AD/denom

       ref_lay_AD(ilay)=ref_lay_AD(ilay)-rdnc(ilay-1)*denom_AD
       rdnc_AD(ilay-1)=rdnc_AD(ilay-1)-denom_AD*ref_lay(ilay) 
 
    ENDDO

    !---------------------------------------------------------------
    ! DOWNWARD ADDING LOOP STARTS FROM TOP OF ATMOSPHERE.
    !---------------------------------------------------------------
                                                                                                              
    !--- initialization.  The 0 level is used for computation convenience.
    ednc_AD(0)=ZERO
    rdnc_AD(0)=ZERO

    downstream_lev_AD(1) = ZERO
    eupc_AD(1) = upstream_lev_AD(1)

    DO ilay = 1, n_layer
      !--- Two stream solution of the layer:
      !--- layer transmittance and reflectance
      ac=sqrt((ONE - alb_lay(ilay))*(ONE - alb_lay(ilay)*g_lay(ilay)))
      gp=alb_lay(ilay)*(ONE+g_lay(ilay))
      gm=alb_lay(ilay)*(ONE-g_lay(ilay))

      !--- compute composite up-stream and reflectance from surface to the
      !--- layer considered. At this stage, this up-stream does not include
      !--- the downward stream from all other layers
      !--- above the current layer which is reflected upward by the layers
      !--- below.
      denom = ONE-rupc(ilay+1)*ref_lay(ilay)
      coef=tran_lay(ilay)/denom

      tran_lay_AD(ilay)=tran_lay_AD(ilay)+coef*rupc(ilay+1)*rupc_AD(ilay)
      rupc_AD(ilay+1)=rupc_AD(ilay+1)+coef*rupc_AD(ilay)*tran_lay(ilay)
      coef_AD=rupc_AD(ilay)*rupc(ilay+1)*tran_lay(ilay)
      ref_lay_AD(ilay)=ref_lay_AD(ilay)+rupc_AD(ilay)

      edn_lay_AD(ilay)=edn_lay_AD(ilay)+coef*rupc(ilay+1)*eupc_AD(ilay)
      rupc_AD(ilay+1)=rupc_AD(ilay+1)+coef*eupc_AD(ilay)*edn_lay(ilay)
      eupc_AD(ilay+1)=eupc_AD(ilay+1)+coef*eupc_AD(ilay)
      coef_AD=coef_AD+eupc_AD(ilay)*(eupc(ilay+1) + rupc(ilay+1)*edn_lay(ilay))

      eup_lay_AD(ilay)= eup_lay_AD(ilay)+eupc_AD(ilay)
      denom_AD=- (tran_lay(ilay)/(denom*denom))*coef_AD
      tran_lay_AD(ilay)=tran_lay_AD(ilay)+coef_AD/denom

      ref_lay_AD(ilay)=ref_lay_AD(ilay)-rupc(ilay+1)*denom_AD
      rupc_AD(ilay+1)=rupc_AD(ilay+1)-denom_AD*ref_lay(ilay)

      if(ac == ZERO) then        !   conservation case

        edn_lay_AD(ilay)=ZERO
        eup_lay_AD(ilay)=ZERO
        tran_lay_AD(ilay)=tran_lay_AD(ilay)-ref_lay_AD(ilay)
        gp_AD=(tran_lay(ilay)*tran_lay(ilay)/TWO)* &
              Diffuse_Factor*opt_lay(ilay)*tran_lay_AD(ilay)
        opt_lay_AD(ilay)=opt_lay_AD(ilay)-(tran_lay(ilay)*tran_lay(ilay)/TWO)* &
                         Diffuse_Factor*tran_lay_AD(ilay)*(TWO-gp)
      else

        exp1 = exp(-Diffuse_Factor*ac*opt_lay(ilay))
        exp2 = exp1 * exp1
        Tem=ONE/(TWO*ac*(ONE+exp2)+(ONE-exp2)*(TWO-gp))

        exp1_AD = ZERO
        exp2_AD = ZERO
        Tem_AD = ZERO

        !--- layer transmittance and reflectance
        !--- layer up and down-ward emission
        eup_lay_AD(ilay)=eup_lay_AD(ilay)+edn_lay_AD(ilay)

        Plancka_lay_AD(ilay)=Plancka_lay_AD(ilay)+(ONE-ref_lay(ilay)-tran_lay(ilay))*eup_lay_AD(ilay)
        ref_lay_AD(ilay)=ref_lay_AD(ilay)-eup_lay_AD(ilay)*Plancka_lay(ilay)
        tran_lay_AD(ilay)=tran_lay_AD(ilay)-eup_lay_AD(ilay)*Plancka_lay(ilay)

        gm_AD=gm_AD+Tem*(ONE-exp2)*ref_lay_AD(ilay)
        exp2_AD=exp2_AD-Tem*ref_lay_AD(ilay)*gm
        Tem_AD=Tem_AD+ref_lay_AD(ilay)*(ONE-exp2)*gm

        exp1_AD=exp1_AD+TWO*TWO*ac*Tem*tran_lay_AD(ilay)
        Tem_AD=Tem_AD+TWO*TWO*ac*tran_lay_AD(ilay)*exp1
        ac_AD=ac_AD+TWO*TWO*tran_lay_AD(ilay)*Tem*exp1

        gp_AD=gp_AD+Tem*Tem*(ONE-exp2)*Tem_AD
        exp2_AD= exp2_AD + Tem*Tem*(TWO-gp)*Tem_AD
        exp2_AD= exp2_AD-Tem*Tem*TWO*ac*Tem_AD
        ac_AD=ac_AD-Tem*Tem*TWO*(ONE+exp2)*Tem_AD

        exp1_AD=exp1_AD+TWO * exp1 * exp2_AD

        opt_lay_AD(ilay)=opt_lay_AD(ilay)-Diffuse_Factor * exp1 *ac*exp1_AD
        ac_AD=ac_AD-Diffuse_Factor * exp1 *exp1_AD*opt_lay(ilay)

      endif

      g_lay_AD(ilay)=g_lay_AD(ilay)- alb_lay(ilay)*gm_AD
      alb_lay_AD(ilay)=alb_lay_AD(ilay)+gm_AD*(ONE-g_lay(ilay))

      g_lay_AD(ilay)=g_lay_AD(ilay) +  alb_lay(ilay)*gp_AD
      alb_lay_AD(ilay)=alb_lay_AD(ilay)+gp_AD*(ONE+g_lay(ilay))

      alb_lay_AD(ilay)=alb_lay_AD(ilay)-ac_AD*(ONE - alb_lay(ilay)*g_lay(ilay))/ (TWO*ac)
      alb_lay_AD(ilay)=alb_lay_AD(ilay)-(ONE-alb_lay(ilay))*ac_AD*g_lay(ilay)/ (TWO*ac)
      g_lay_AD(ilay)=g_lay_AD(ilay)-(ONE-alb_lay(ilay))*alb_lay(ilay)*ac_AD/ (TWO*ac)

      ac_AD = ZERO
      gp_AD = ZERO
      gm_AD = ZERO
    ENDDO

     emissivity_AD = emissivity_AD - rupc_AD(n_layer+1)
     emissivity_AD = emissivity_AD + eupc_AD(n_layer+1) * Plancks
     Plancks_AD = Plancks_AD + emissivity * eupc_AD(n_layer+1)
   END subroutine Twostream_Solution_AD 


END MODULE RT_Twostream
