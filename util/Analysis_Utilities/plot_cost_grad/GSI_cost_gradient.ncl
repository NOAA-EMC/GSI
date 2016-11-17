load "$NCARG_ROOT/lib/ncarg/nclex/gsun/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

begin

;  nloop1=10
;  nloop2=10
  nloop1=stringtoint(systemfunc("cat cost_gradient.txt  | grep '^1' | tail -1 | awk '{print $2}' "))   ; iterations for 1st outer loop
  nloop2=stringtoint(systemfunc("cat cost_gradient.txt  | grep '^2' | tail -1 | awk '{print $2}' "))  ; iterations for 2nd outer loop

  print(nloop1)
  print(nloop2)

  obfile    =  "./cost_gradient.txt"
  data      =  asciiread(obfile, -1, "string")    ; -1 means read all rows.
  delim     =  " ";

  step     = stringtofloat(str_get_field(data,2,  delim));   inner iterations
  cost     = stringtofloat(str_get_field(data,3,  delim));
  gradient = stringtofloat(str_get_field(data,4,  delim));

  print(step(nloop1+1:nloop1+nloop2+1))
  titles = new(4,string)
  titles(0)="Cost outer 1"
  titles(1)="Gradient outer 1"
  titles(2)="Cost outer 2"
  titles(3)="Gradient outer 2"

  plot = new(4,graphic)

  xwks = gsn_open_wks("pdf","GSI_cost_gradient")
;  xwks = gsn_open_wks("x11","gsun01n")
  gsn_define_colormap(xwks,"cosam12")

  resources                  =  True              ; plot mods desired
  resources@gsnDraw              = False              ; Do not draw plot
  resources@gsnFrame             = False              ; Do not advance frame

  resources@xyMarkLineModes   = "Markers"                ; choose which have markers
  resources@xyMarkerColor = "black"

  resources@xyMarkLineModes   = "Lines"                ; choose which have markers
  plot(0) = gsn_xy(xwks, step(0:nloop1), cost(0:nloop1), resources)
  plot(1) = gsn_xy(xwks, step(0:nloop1), gradient(0:nloop1), resources)
  plot(2) = gsn_xy(xwks, step(nloop1+1:nloop1+nloop2+1), cost(nloop1+1:nloop1+nloop2+1), resources)
  plot(3) = gsn_xy(xwks, step(nloop1+1:nloop1+nloop2+1), gradient(nloop1+1:nloop1+nloop2+1), resources)

  resourcesP                    =  True              ; plot mods desired
  resourcesP@gsnPanelFigureStrings = titles
  resourcesP@gsnPanelFigureStringsFontHeightF = 0.02
  resourcesP@amJust = "TopRight"
  resourcesP@gsnPanelFigureStringsPerimOn = False
  gsn_panel(xwks,plot,(/2,2/),resourcesP)

  delete(plot)
  delete(resources)
  delete(resourcesP)
end
