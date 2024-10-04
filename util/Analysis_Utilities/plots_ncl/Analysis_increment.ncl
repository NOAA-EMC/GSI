
load "$NCARG_ROOT/lib/ncarg/nclex/gsun/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

begin

  cdf_analysis = addfile("./wrf_inout.cdf","r")
  cdf_bk = addfile("arw/wrfinput_d01.cdf","r")
  Ta = cdf_analysis->T(0,:,:,:)
  Tb = cdf_bk->T(0,:,:,:)
  DT = Ta - Tb
  delete(Ta)
  delete(Tb)
  Ta = cdf_analysis->U(0,:,:,:)
  Tb = cdf_bk->U(0,:,:,:)
  DU = Ta - Tb
  delete(Ta)
  delete(Tb)
  Ta = cdf_analysis->V(0,:,:,:)
  Tb = cdf_bk->V(0,:,:,:)
  DV = Ta - Tb
  delete(Ta)
  delete(Tb)
  Ta = cdf_analysis->QVAPOR(0,:,:,:)
  Tb = cdf_bk->QVAPOR(0,:,:,:)
  DQ = Ta - Tb
  delete(Ta)
  delete(Tb)
  DQ = DQ * 1000.0
  landmask = cdf_bk->LANDMASK(0,:,:)

  dsizes = getfiledimsizes(cdf_bk)
  nx = dsizes(2)
  ny = dsizes(3)
  nz = dsizes(4)
  lat=cdf_bk->XLAT(0,:,:)
  lon=cdf_bk->XLONG(0,:,:)
  lat_ll  = lat(0,0)
  lat_ur  = lat(ny-1,nx-1)
  lon_ll  = lon(0,0)
  lon_ur  = lon(ny-1,nx-1)

  f2dv = new ((/nz,nx/), typeof(DT))
  f2dh = new ((/ny,nx/), typeof(DT))

  rmax = -9999.0
  do k=0, nz-1
  do j=0, ny-1
  do i=0, nx-1
    if (rmax .le. DT(k,j,i)) then
       rmax=DT(k,j,i)
       imax=i
       jmax=j
       kmax=k
    end if
  end do
  end do
  end do

  print(rmax)
  print(imax)
  print(jmax)
  print(kmax)

  kmax=15

  titles = new(4,string)
  titles(0)="T inc, XY"
  titles(1)="U inc, XY"
  titles(2)="V inc, XY"
  titles(3)="Q inc, XY"

  plot = new(4,graphic)

  xwks = gsn_open_wks("pdf","GSI_prepbufr_arw_15")
;  xwks = gsn_open_wks("x11","gsun01n")
  gsn_define_colormap(xwks,"cosam12")

  resources                  =  True              ; plot mods desired
  resources@gsnDraw              = False              ; Do not draw plot
  resources@gsnFrame             = False              ; Do not advance frame

  resources@cnMonoLineColor    = False
  resources@cnFillOn    = False
;  resources@cnFillOn    = True
  resources@gsnContourNegLineDashPattern = 1         ; negtive line use dash

; map
  resources@sfXArray         = lon
  resources@sfYArray         = lat

  if ( cdf_bk@MAP_PROJ .eq. 1 ) then
         mapproj = "LambertConformal"
         truelat1 = cdf_bk@TRUELAT1
         truelat2 = cdf_bk@TRUELAT2
         clon     = cdf_bk@STAND_LON
  end if
  if ( cdf_bk@MAP_PROJ .eq. 2 ) then
         mapproj = "Stereographic"
         truelat1 = cdf_bk@TRUELAT1
         truelat2 = cdf_bk@TRUELAT2
         clon     = cdf_bk@CEN_LON
         clat     = cdf_bk@CEN_LAT
  end if
  if ( cdf_bk@MAP_PROJ .eq. 3 ) then
         mapproj = "Mercator"
  end if

   resources@mpProjection            = mapproj          ; choose projection
 if ( mapproj .eq. "LambertConformal" ) then
      resources@mpLambertParallel1F     = truelat1      ; two parallels
      resources@mpLambertParallel2F     = truelat2
      resources@mpLambertMeridianF      = clon          ; central meridian
 end if
 if ( mapproj .eq. "Stereographic" ) then
      resources@mpCenterLatF         = clat
      resources@mpCenterLonF         = clon
 end if

  resources@mpLimitMode             = "Corners"
  resources@mpLeftCornerLatF        = lat_ll
  resources@mpLeftCornerLonF        = lon_ll
  resources@mpRightCornerLatF       = lat_ur
  resources@mpRightCornerLonF       = lon_ur

  resources@pmTickMarkDisplayMode = "Always"
  resources@mpFillOn              =  False             ; turn off map fill
  resources@mpOutlineDrawOrder    = "PostDraw"         ; continent outline last
  resources@mpOutlineBoundarySets = "GeophysicalAndUSStates" ; state boundaries
  resources@mpGridAndLimbOn = False                  ; turn off lat/lon lines 
  resources@mpPerimOn = True
  resources@pmLabelBarDisplayMode = "Always" ; Turn on label bar.
  resources@lbPerimOn             = False    ; Turn off perimeter on label bar.


;  resources@cnLevelSpacingF = 0.2
  plot(0) = gsn_contour_map(xwks, DT(kmax,:,:), resources)
  do j=0, ny-1
  do i=0, nx-1
      f2dh(j,i) = (DU(kmax,j,i) + DU(kmax,j,i+1))/2.0
  end do
  end do
;  resources@cnLevelSpacingF = 1.0
  plot(1) = gsn_contour_map(xwks, f2dh, resources)
  do j=0, ny-1
  do i=0, nx-1
      f2dh(j,i) = (DV(kmax,j,i) + DV(kmax,j+1,i))/2.0
  end do
  end do
;  resources@cnLevelSpacingF = 1.0
  plot(2) = gsn_contour_map(xwks, f2dh, resources)
;  resources@cnLevelSpacingF = 0.1
  plot(3) = gsn_contour_map(xwks, DQ(kmax,:,:), resources)

  resourcesP                    =  True              ; plot mods desired
  resourcesP@gsnPanelFigureStrings = titles
  resourcesP@gsnPanelFigureStringsFontHeightF = 0.01 
  resourcesP@amJust = "TopLeft"     
  resourcesP@gsnPanelFigureStringsPerimOn = False  
  gsn_panel(xwks,plot,(/2,2/),resourcesP)

  delete(plot)
  delete(resourcesP)

; plot landmask to make sure the map info is correct
  resources@gsnDraw              = True
  resources@gsnFrame             = True
  resources@cnLevelSpacingF = 1
  resources@cnFillOn = True
  resources@cnLineLabelsOn = False
  resources@tiMainString = " LANDMASK"
  plotm=gsn_contour_map(xwks, landmask(:,:), resources)

  delete(resources)
end
