
pro set_filled_circle

a=findgen(17) * (!PI*2/16.)
usersym, cos(a), sin(a), /fill, thick=!p.thick

end

