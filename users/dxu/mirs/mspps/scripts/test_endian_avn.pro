;***************************************************************
;
; This is to test whether if AVN is endian correct
; if tmp has weird values(too small or too large), then you need
; call swap_endian_avn.pro to swap endian.
;
;***************************************************************

pro test_endian_avn, file

tmp=fltarr(181,360)

openr, 1, file
readu, 1, tmp
close, 1

print, tmp

end
