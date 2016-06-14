load "$NCARG_ROOT/lib/ncarg/nclex/gsun/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

begin

  nxny = fbinrecread("compref.bin",0,(/2/),"integer")
  nx=nxny(0)
  ny=nxny(1)
  print(ny)
  print(nx)

  mydata = fbinrecread("compref.bin",1,(/ny,nx/),"float")

  xwks = gsn_open_wks("pdf","compsiteRef")
  gsn_define_colormap(xwks,"cosam12")
 
  resources                    =  True              ; plot mods desired
  resources@cnMonoLineColor    = False
  resources@tiMainString       ="Composite Reflectivity"

  resources@tfDoNDCOverlay           = False               ; don't transform
  resources@cnFillOn                 = True               ; turn on color
  resources@cnLineLabelsOn      = False
  resources@cnLinesOn                = False              ; turn off contour lines
  resources@cnConstFLabelPerimOn      = False               ; 
  
  resources@gsnMaximize       =   True
  resources@pmLabelBarDisplayMode = "Always" ; Turn on label bar.
  resources@lbPerimOn             = False    ; Turn off perimeter on label bar.

  resources@cnFillColors         = (/-1,2,3,4,5,6,7,8,9,10,11,12/)
  resources@cnFillDrawOrder       = "PreDraw"         ; make sure fill map on top
;
  resources@cnLevelSelectionMode = "ExplicitLevels"     ;set explicit contour levels
  resources@cnLevels    = (/-15.0,-5.0,0.,10.,15.,20.,25.,30.,35.,40.,50./)	; set levels

  plot = gsn_contour(xwks, mydata, resources)

  delete(plot)
  delete(mydata)
  delete(resources)
end

