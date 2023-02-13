PRO SAR_GPS3D_TO_LOS, file, ofile
  IF N_PARAMS() LT 2 THEN BEGIN
    file='D:\data\taiwan\TimeSeriesReleased1993.01.01_2017.11.30\neu\TTUN.COR.neu'
    ofile='D:\data\taiwan\TimeSeriesReleased1993.01.01_2017.11.30\neu.los\TTUN.COR.neu'
    
    file='D:\ICD\projects\CEA.2017.Tibet.GNSS_InSAR\2018sep\result\pos.neu\xmx2\resid\seasonal.frost.deoff\XMX2.icd.rapid_igs08.pos.neu'
    ofile='D:\ICD\projects\CEA.2017.Tibet.GNSS_InSAR\2018sep\result\pos.neu\xmx2\resid\seasonal.frost.deoff.los.t41\XMX2.icd.rapid_igs08.pos.neu'
    ofile='D:\ICD\projects\CEA.2017.Tibet.GNSS_InSAR\2018sep\result\pos.neu\xmx2\resid\seasonal.frost.deoff.los.t150\XMX2.icd.rapid_igs08.pos.neu'
  ENDIF
  
  READ_SIO, file, data=data
  
  odata=data
  FOR di=0, N_ELEMENTS(data[0,*])-1 DO BEGIN
    ;stop
    ;odata[3:5,di]=sar_enu2los(data[[4,3,5],di],theta=44*!dpi/180d0, alpha=(-13+360d0)*!dpi/180d0) ;t41
    odata[3:5,di]=sar_enu2los(data[[4,3,5],di],theta=38*!dpi/180d0, alpha=(193d0)*!dpi/180d0) ;t150
  ENDFOR
  
  WRITE_SIO, ofile, data=odata
END