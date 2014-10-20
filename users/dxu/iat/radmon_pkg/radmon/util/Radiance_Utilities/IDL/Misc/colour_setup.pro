PRO COLOUR_SETUP

COMMON Colours,black, white, grey, blue, green, red, cyan, magenta, $
 yellow, orange, cherry, aqua, lime, purple, navy, lightgrey, default

if (!d.name eq 'X') then begin
  device,get_decomposed=current_decomposed
endif else begin
  current_decomposed=0
endelse
 
if (!d.name ne 'X' or current_decomposed eq 0) then begin
  num_colours=!d.n_colors-1	

  r1= 255*[0, 1, 1, 0, 0, 0, 1, 1]
  g1= 255*[0, 1, 0, 1, 0, 1, 1, 0]
  b1= 255*[0, 1, 0, 0, 1, 1, 0, 1]

  r=fltarr(256)
  g=fltarr(256)
  b=fltarr(256)
  for i=0,35 do begin
    r(1+i*7:i*7+7)=r1(1:7)
    g(1+i*7:i*7+7)=g1(1:7)
    b(1+i*7:i*7+7)=b1(1:7)
  endfor

  r(8:12)=[255,125,80,127,191]
  g(8:12)=[85,0,255,127,191]
  b(8:12)=[0,255,80,127,191]

  if (!d.n_colors le 256) then begin
    r(!d.n_colors-1)=255
    g(!d.n_colors-1)=255
    b(!d.n_colors-1)=255
  endif else begin
    r(255)=255
    g(255)=255
    b(255)=255
  endelse

  TVLCT, r,g,b

  black     =  0
  grey      = 11
  lightgrey = 12
  white     =  1

  blue      =  4
  green     =  3
  red       =  2

  cyan      =  5
  magenta   =  7
  yellow    =  6

  orange    =  8
  purple    =  9
  lime      = 10

  if (!d.name eq 'X') then begin
    default = white
  endif else begin
    default = black
  endelse

endif else begin

  black   = '000000'xl
  white   = 'ffffff'xl
  grey    = '808080'xl

  blue    = 'ff0000'xl
  green   = '00ff00'xl
  red     = '0000ff'xl

  cyan    = 'ffff00'xl
  magenta = 'ff00ff'xl
  yellow  = '00ffff'xl

  orange  = '0080ff'xl
  cherry  = '8000ff'xl
  aqua    = '80ff00'xl
  lime    = '00ff80'xl
  purple  = 'ff0080'xl
  navy    = 'ff8000'xl

  default = white

endelse

RETURN
END





