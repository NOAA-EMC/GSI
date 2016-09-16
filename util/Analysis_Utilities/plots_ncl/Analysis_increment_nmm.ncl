load "$NCARG_ROOT/lib/ncarg/nclex/gsun/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "./fill_nmm_grid2.ncl"

begin

  cdf_analysis = addfile("./wrf_inout.cdf","r")
  cdf_bk = addfile("nmm/wrfinput_d01.cdf","r")
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
  Ta = cdf_analysis->Q(0,:,:,:)
  Tb = cdf_bk->Q(0,:,:,:)
  DQ = Ta - Tb
  delete(Ta)
  delete(Tb)
  DQ = DQ * 1000.0
  lon = cdf_bk->GLON(0,:,:)     ; lon in radiances
  lat = cdf_bk->GLAT(0,:,:)     ; lat in radiances
  landmask=cdf_bk->LANDMASK(0,:,:) ; land mask

; map information
   clon     = cdf_bk@CEN_LON
   clat     = cdf_bk@CEN_LAT
   print(clon)
   print(clat)

  dsizes = getfiledimsizes(cdf_bk)
  nx = dsizes(2)
  ny = dsizes(3)
  nz = dsizes(4)

  f3d  = new ((/nz,ny-4,2*nx-5/), typeof(DT))
  lona = new ((/ny-4,2*nx-5/), typeof(lon))
  lata = new ((/ny-4,2*nx-5/), typeof(lat))
  lma = new ((/ny-4,2*nx-5/), typeof(landmask))
  
  fill_nmm_grid2(lon(:,:),nx,ny,lona(:,:),1)
  fill_nmm_grid2(lat(:,:),nx,ny,lata(:,:),1)
  fill_nmm_grid2(landmask(:,:),nx,ny,lma(:,:),1)
  lona = lona * 180.0 /3.1415926
  lata = lata * 180.0 /3.1415926

  lata_ll=lata(0,0)
  lata_ur=lata(ny-5,2*nx-6)
  lona_ll=lona(0,0)
  lona_ur=lona(ny-5,2*nx-6)

  do k=0, nz-1
    fill_nmm_grid2(DT(k,:,:),nx,ny,f3d(k,:,:),1)
  end do

  rmax = -9999.0
  do k=0, nz-1
  do j=0, ny-1-4
  do i=0, 2*nx-1-1-4
    if (rmax .le. f3d(k,j,i)) then
       rmax=f3d(k,j,i)
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

  xwks = gsn_open_wks("pdf","Analysis_prepbufr_nmm")
;  xwks = gsn_open_wks("x11","gsun01n")
  gsn_define_colormap(xwks,"cosam12")

  resources                  =  True              ; plot mods desired
  resources@gsnDraw              = False              ; Do not draw plot
  resources@gsnFrame             = False              ; Do not advance frame

  resources@cnMonoLineColor    = False
;  resources@cnFillOn    = True
  resources@cnFillOn    = False
;  resources@gsnContourNegLineDashPattern = 1

; map
  resources@sfXArray         = lona
  resources@sfYArray         = lata
 
  resources@mpCenterLatF         = clat
  resources@mpCenterLonF         = clon

  resources@mpLimitMode        = "Corners"          ; choose range of map
  resources@mpLeftCornerLatF      = lata_ll
  resources@mpLeftCornerLonF      = lona_ll
  resources@mpRightCornerLatF     = lata_ur
  resources@mpRightCornerLonF     = lona_ur

  resources@pmTickMarkDisplayMode = "Always"
  resources@mpFillOn              =  False             ; turn off map fill
  resources@mpOutlineDrawOrder    = "PostDraw"         ; continent outline last
  resources@mpOutlineBoundarySets = "GeophysicalAndUSStates" ; state boundaries
  resources@mpGridAndLimbOn = False                  ; turn off lat/lon lines 
  resources@mpPerimOn = True
  resources@pmLabelBarDisplayMode = "Always" ; Turn on label bar.
  resources@lbPerimOn             = False    ; Turn off perimeter on label bar.

;  resources@cnLevelSpacingF = 0.5
  plot(0) = gsn_contour_map(xwks, f3d(kmax,:,:), resources)
  do k=0, nz-1
    fill_nmm_grid2(DU(k,:,:),nx,ny,f3d(k,:,:),2)
  end do
;  resources@cnLevelSpacingF = 1.0
  plot(1) = gsn_contour_map(xwks, f3d(kmax,:,:), resources)
  do k=0, nz-1
    fill_nmm_grid2(DV(k,:,:),nx,ny,f3d(k,:,:),2)
  end do
;  resources@cnLevelSpacingF = 1.0
  plot(2) = gsn_contour_map(xwks, f3d(kmax,::,:), resources)
  do k=0, nz-1
    fill_nmm_grid2(DQ(k,:,:),nx,ny,f3d(k,:,:),1)
  end do
;  resources@cnLevelSpacingF = 1.0
  plot(3) = gsn_contour_map(xwks, f3d(kmax,:,:), resources)

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
  plotm=gsn_contour_map(xwks, lma(:,:), resources)
  delete(resources)
end

