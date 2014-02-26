;***************************************************************
;
; This is only needed if GFS(avn) data has endian problem.
; You may need to run the code before using avn data.
;
;***************************************************************

pro swap_endian_avn, file

;print, file

tmp=fltarr(181,360)

openr, 1, file, /swap_endian
readu, 1, tmp
close, 1

openw,  2, file
writeu, 2, tmp
close,  2

end
