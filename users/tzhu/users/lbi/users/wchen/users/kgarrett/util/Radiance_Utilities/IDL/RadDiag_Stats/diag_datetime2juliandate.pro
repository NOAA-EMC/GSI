FUNCTION Diag_DateTime2JulianDate, DateTime

 Month = (DateTime MOD 1000000L)/10000L
 Day   = (DateTime MOD 10000L)/100L
 Hour  = (DateTime MOD 100L)
 Year  = DateTime/1000000L

 RETURN, JULDAY( Month, Day, Year, Hour )

END ; FUNCTION Diag_DateTime2JulianDate
