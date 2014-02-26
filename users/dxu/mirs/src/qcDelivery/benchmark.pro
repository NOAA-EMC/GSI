@../../setup/paths_idl.pro


PRINT, 'Input Satellite ID: 1->N18,  2->Metop-A,  3->F16, 4->N19, 5->F18, 6->NPP, 7->Metop-B'
read,answer

i=1
;PRINT, 'Input sub orbit number ( 1,2,..., 9, 10, ...,)'
;read,i,format='(i)'
;indx = strtrim(string(i),2)
;if ( i lt 10 ) then indx='0'+indx

;---N18
IF (answer eq 1) then begin
    ;---Benchmark
    file1='../../data/BenchmarkData/EDR_SX.NN.D06032.S0122.E0317.B0361819.WI.LR_001'
    ;---Generated
    file2='../../data/TestbedData/Outputs/edr/n18_amsua_mhs/EDR_SX.NN.D06032.S0122.E0317.B0361819.WI.LR_001'
    ;---Title
    tit='N18'
ENDIF

;----Metop-A
IF (answer eq 2) then begin
    ;---Benchmark
    file1='../../data/BenchmarkData/EDR_SX.M2.D06305.S0203.E0347.B0017677.SV.LR_001'
    ;---Generated
    file2='../../data/TestbedData/Outputs/edr/metopA_amsua_mhs/EDR_SX.M2.D06305.S0203.E0347.B0017677.SV.LR_001'
    ;---Title
    tit='Metop-A'
ENDIF

;----F16
IF (answer eq 3) then begin
   ;---Benchmark
   file1='../../data/BenchmarkData/EDR_SN.SA.D06032.S0426.E0604.B1181617.NS.CALIB.UAS_001'
   ;---Generated
   file2='../../data/TestbedData/Outputs/edr/f16_ssmis/EDR_SN.SA.D06032.S0426.E0604.B1181617.NS.CALIB.UAS_001'
   ;---Title
   tit='F16'
ENDIF

;----N19
IF (answer eq 4) then begin
   ;---Benchmark
   file1='../../data/BenchmarkData/EDR_SX.NP.D10152.S1810.E2005.B0677274.GC.LR_001'
   ;---Generated
   file2='../../data/TestbedData/Outputs/edr/n19_amsua_mhs/EDR_SX.NP.D10152.S1810.E2005.B0677274.GC.LR_001'
   ;---Title
   tit='N19'
ENDIF

;----F18
IF (answer eq 5) then begin
   ;---Benchmark
   file1='../../data/BenchmarkData/EDR_SN.SC.D10151.S0434.E0611.B0317071.NS.UAS_001'
   ;---Generated
   file2='../../data/TestbedData/Outputs/edr/f18_ssmis/EDR_SN.SC.D10151.S0434.E0611.B0317071.NS.UAS_001'
   ;---Title
   tit='F18'
ENDIF

;----NPP
IF (answer eq 6) then begin
   ;---Benchmark
   file1='../../data/BenchmarkData/EDR_TMS_npp_d20120415_t1235310_e1236026_b02414_c20120415190332243321_noaa_ops.h5.HR.ORB'
   ;---Generated
   file2='../../data/TestbedData/Outputs/edr/npp_atms/EDR_TMS_npp_d20120415_t1235310_e1236026_b02414_c20120415190332243321_noaa_ops.h5.HR.ORB'
   ;---Title
   tit='NPP'
ENDIF

;----Metop-B
IF (answer eq 7) then begin
    ;---Benchmark
    file1='../../data/BenchmarkData/EDR_SX.M1.D12351.S0133.E0316.B0126970.SV.HR_001'
    ;---Generated
    file2='../../data/TestbedData/Outputs/edr/metopB_amsua_mhs/EDR_SX.M1.D12351.S0133.E0316.B0126970.SV.HR_001'
    ;---Title
    tit='Metop-B'
ENDIF


print,' Reading file:',strcompress(file1)
ReadScene,file1, nprofiles1,nlay1,nlev1,nchan1,nAbsorb1,nParmCLW1,nParmRain1,nParmSnow1, $
            nParmIce1,nParmGrpl1,AbsorbID1,CentrFrq1,Polarty1,iH2o1,iO31,ProfIndx1,Pres_lay1, $
            Pres_lev1,Temp_lay1,Abso_lay1,clw1,rain1,snow1,ice1,graupel1,Angle1,Emiss1,Refl1,   $
            WindSp1,Tskin1,SfcPress1,SfcTyp1,deltaT1,Scene1,Lat1,Lon1,windU1,windV1,RelAziAngle1,SolZenAngle1,snowdepth1

print,' Reading file:',strcompress(file2)
ReadScene,file2, nprofiles2,nlay2,nlev2,nchan2,nAbsorb2,nParmCLW2,nParmRain2,nParmSnow2, $
            nParmIce2,nParmGrpl2,AbsorbID2,CentrFrq2,Polarty2,iH2o2,iO32,ProfIndx2,Pres_lay2, $
            Pres_lev2,Temp_lay2,Abso_lay2,clw2,rain2,snow2,ice2,graupel2,Angle2,Emiss2,Refl2,   $
            WindSp2,Tskin2,SfcPress2,SfcTyp2,deltaT2,Scene2,Lat2,Lon2,windU2,windV2,RelAziAngle2,SolZenAngle2,snowdepth2

;---Filter
chiSqThresh     = 5.
chiSqEquality   = 2.
qcConditions    = (Scene1.qc(*,0) eq 0 and Scene2.qc(*,0) eq 0)
ChiSqConditions = Scene1.ChiSq le chiSqThresh and Scene2.ChiSq le chiSqThresh and abs(Scene1.ChiSq-Scene2.ChiSq) le chiSqEquality
ind             = where (qcConditions and ChiSqConditions)

;---Chi-Square
xmin=min([min(Scene1.ChiSq(ind)),min(Scene2.ChiSq(ind))])
xmax=max([max(Scene2.ChiSq(ind)),max(Scene2.ChiSq(ind))])
PlotScatt_png,Scene1.ChiSq(ind),Scene2.ChiSq(ind),'ChiSquare -benchmark-','ChiSquare -generated-',$
 tit,1,xmin,xmax,xmin,xmax,1.2,3, tit + '_chisq.png'

;---Latitude
xmin=min([min(Scene1.lat(ind)),min(Scene2.lat(ind))])
xmax=max([max(Scene2.lat(ind)),max(Scene2.lat(ind))])
PlotScatt_png,Scene1.lat(ind),Scene2.lat(ind),'Latitude (deg) -benchmark-','Latitude (deg) -generated-',$
 tit,1,xmin,xmax,xmin,xmax,1.2,3, tit + '_lat.png'

;---Longitude
xmin=min([min(Scene1.lon(ind)),min(Scene2.lon(ind))])
xmax=max([max(Scene2.lon(ind)),max(Scene2.lon(ind))])
PlotScatt_png,Scene1.lon(ind),Scene2.lon(ind),'Longitude (deg) -benchmark-','Longitude (deg) -generated-',$
 tit,1,xmin,xmax,xmin,xmax,1.2,3, tit + '_lon.png'

;---Tskin
xmin=min([min(tskin1(ind)),min(tskin2(ind))])
xmax=max([max(tskin1(ind)),max(tskin2(ind))])
PlotScatt_png,tskin1(ind),tskin2(ind),'Skin Temperature (K) -benchmark-','Skin Temperature (K) -generated-',$
 tit,1,xmin,xmax,xmin,xmax,1.2,3, tit + '_tskin.png'

;---TPW
xmin=min([min(Scene1.tpwvec(ind)),min(Scene2.tpwvec(ind))])
xmax=max([max(Scene1.tpwvec(ind)),max(Scene2.tpwvec(ind))])
PlotScatt_png,Scene1.tpwvec(ind),Scene2.tpwvec(ind),'TPW (mm) -benchmark-','TPW(mm) -generated-',$
 tit,1,xmin,xmax,xmin,xmax,1.2,3, tit + '_tpw.png'

;---CLW
xmin=min([min(Scene1.clwvec(ind)),min(Scene2.clwvec(ind))])
xmax=max([max(Scene1.clwvec(ind)),max(Scene2.clwvec(ind))])
PlotScatt_png,Scene1.clwvec(ind),Scene2.clwvec(ind),'CLW (mm) -benchmark-','CLW(mm) -generated-',$
 tit,1,xmin,xmax,xmin,xmax,1.2,3, tit + '_clw.png'

;---Emiss
ichan=0
xmin=min([min(Scene1.emissvec(ind,ichan)),min(Scene2.emissvec(ind,ichan))])
xmax=max([max(Scene1.emissvec(ind,ichan)),max(Scene2.emissvec(ind,ichan))])
PlotScatt_png,Scene1.emissvec(ind,ichan),Scene2.emissvec(ind,ichan),$
 'Emiss -benchmark- @ Freq'+string(Scene1.cfreq(ichan),'(f6.1)')+'GHz',$
 'Emiss -generated- @ Freq'+string(Scene1.cfreq(ichan),'(f6.1)')+'GHz',tit,1,xmin,xmax,xmin,xmax,1.2,3, tit + '_em.png'

;---Temperature
iLay=80
xmin=min([min(Scene1.TempLayvec(ind,iLay)),min(Scene2.TempLayvec(ind,iLay))])
xmax=max([max(Scene1.TempLayvec(ind,iLay)),max(Scene2.TempLayvec(ind,iLay))])
PlotScatt_png,Scene1.TempLayvec(ind,iLay),Scene2.TempLayvec(ind,iLay),'Temperature [K] -benchmark- @ P'+$
 string(Scene1.presLayVec(0,iLay),'(f6.1)')+'mb',$
 'Temperature [K] -generated- @ P'+string(Scene1.presLayVec(0,iLay),'(f6.1)')+'mb',tit,1,xmin,xmax,xmin,xmax,1.2,3, tit + '_temp.png'

;---Humidity
iLay=80
xmin=min([min(Scene1.AbsorbLayVec(ind,iLay)),min(Scene2.AbsorbLayVec(ind,iLay))])
xmax=max([max(Scene1.AbsorbLayVec(ind,iLay)),max(Scene2.AbsorbLayVec(ind,iLay))])
PlotScatt_png,Scene1.AbsorbLayVec(ind,iLay),Scene2.AbsorbLayVec(ind,iLay),'Humidity [g/Kg] -benchmark- @ P'+$
 string(Scene1.presLayVec(0,iLay),'(f6.1)')+'mb',$
 'Humidity [g/Kg] -generated- @ P'+string(Scene1.presLayVec(0,iLay),'(f6.1)')+'mb',tit,1,xmin,xmax,xmin,xmax,1.2,3, tit + '_hm.png'



end
