load "$NCARG_ROOT/lib/ncarg/nclex/gsun/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

begin

  cdf_analysis = addfile("run/gsiprd_2011032212_arw_singleobs/wrf_inout.cdf","r")
  cdf_bk = addfile("2011032212/wrfinput_d01_ARW_2011-03-22_12.cdf","r")
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

  nx=dimsizes(DT(1,1,:))
  ny=dimsizes(DT(1,:,1))
  nz=dimsizes(DT(:,1,1))

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

  titles = new(8,string)
  titles(0)="T inc, XY"
  titles(1)="T inc, XZ "
  titles(2)="U inc, XY"
  titles(3)="U inc, XZ"
  titles(4)="V inc, XY"
  titles(5)="V inc, XZ"
  titles(6)="Q inc, XY"
  titles(7)="Q inc, XZ"

  plot = new(8,graphic)

  xwks = gsn_open_wks("pdf","GSI_singleObse_T_arw")
;  xwks = gsn_open_wks("x11","gsun01n")
  gsn_define_colormap(xwks,"cosam12")

  resources                  =  True              ; plot mods desired
  resources@gsnDraw              = False              ; Do not draw plot
  resources@gsnFrame             = False              ; Do not advance frame

  resources@cnMonoLineColor    = False
  resources@gsnContourNegLineDashPattern = 1         ; negtive line use dash

  plot(0) = gsn_contour(xwks, DT(kmax,:,:), resources)
  plot(1) = gsn_contour(xwks, DT(:,jmax,:), resources)
  do j=0, ny-1
  do i=0, nx-1
      f2dh(j,i) = (DU(kmax,j,i) + DU(kmax,j,i+1))/2.0
  end do
  end do
  plot(2) = gsn_contour(xwks, f2dh, resources)
  plot(3) = gsn_contour(xwks, DU(:,jmax,:), resources)
  do j=0, ny-1
  do i=0, nx-1
      f2dh(j,i) = (DV(kmax,j,i) + DV(kmax,j+1,i))/2.0
  end do
  end do
  plot(4) = gsn_contour(xwks, f2dh, resources)
  plot(5) = gsn_contour(xwks, DV(:,jmax,:), resources)
  plot(6) = gsn_contour(xwks, DQ(kmax,:,:), resources)
  plot(7) = gsn_contour(xwks, DQ(:,jmax,:), resources)

  resourcesP                    =  True              ; plot mods desired
  resourcesP@gsnPanelFigureStrings = titles
  resourcesP@gsnPanelFigureStringsFontHeightF = 0.006 
  resourcesP@amJust = "TopLeft"     
  resourcesP@gsnPanelFigureStringsPerimOn = False  
  gsn_panel(xwks,plot,(/4,2/),resourcesP)

  delete(plot)
  delete(resources)
  delete(resourcesP)
end
