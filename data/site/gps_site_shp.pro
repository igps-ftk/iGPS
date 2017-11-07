;+
; NAME:
;		GPS_SITE_SHP
; PURPOSE:
;		CREATE POINT SHAPEFILE.
; CALLING SEQUENCE:
;		SHP_XYZ_NEW,OFNAME, DATA, XI=XI, YI=YI, CAPS=CAPS
;
;+++
;-
PRO GPS_SITE_SHP, SFILE,OFILE,LFILE=LFILE
  IF N_PARAMS() LT 2 THEN BEGIN
    SFILE='cmonoc.sit'
    OFILE='cmonoc.shp'
    
    SFILE='D:\phd\expt\gpsf\proctab\rerun.lutai\map\sit\lut1_lutai.sit'
    OFILE='D:\phd\expt\gpsf\proctab\rerun.lutai\map\sit\lut1_lutai_loc.shp'
    LFILE='D:\phd\expt\gpsf\cgps\conf\lfile.lut1.2011355_approx.net'
    
    sfile='E:\gpse\eq.yaan.2013apr20\gps\lutai\solut\s1\tables\lutai-yaan.sit'
    ofile='E:\gpse\eq.yaan.2013apr20\gps\lutai\solut\s1\tables\lutai-yaan-shp.shp'
    lfile='E:\gpse\eq.yaan.2013apr20\gps\lutai\comb.yaan\regional\tables\lfile.yaan.net'
    
    sfile='D:\ICD\projects\DirectorFund\Application.2012\Field\2014oct\result\nqxm.sit'
    ofile='D:\ICD\projects\DirectorFund\Application.2012\Field\2014oct\result\lfile.nqxm.shp'
    lfile='D:\ICD\projects\DirectorFund\Application.2012\Field\2014oct\result\lfile.nqxm.net'
    
    sfile='D:\ICD\projects\DirectorFund\Application.2012\Field\2015July\results\ls27.sit'
    ofile='D:\ICD\projects\DirectorFund\Application.2012\Field\2015July\results\ls27.shp'
    lfile='D:\ICD\projects\DirectorFund\Application.2012\Field\2015July\results\ts2\lgn.net'
    
    sfile='D:\gpse\rerun.lutai\comb\trnsLTCM2\gsoln\pos-appr.sit'
    ofile='D:\gpse\rerun.lutai\comb\trnsLTCM2\gsoln\pos-appr.shp'
    lfile='D:\gpse\rerun.lutai\comb\trnsLTCM2\gsoln\pos-appr.net'
    
    sfile='D:\gpse\北京GPS水汽\bjcors.sit'
    ofile='D:\gpse\bjcors.shp'
    
    sfile='D:\data\cmonoc\timeseries\fromIS\ftp.cgps.ac.cn\products\position.20161023\gamit\290.sit'
    ofile='d:\gpse\lutai.shp'
    
    
    sfile='D:\gpse\rerun.lutai\comb\trnsLTCM2\gsoln\pos-appr-noLGN.sit'
    ofile='D:\gpse\rerun.lutai\comb\trnsLTCM2\gsoln\pos-appr-noLGN.shp'
    
    sfile='D:\ICD\projects\2015nov.lys.yunnan\DATA\30S24h\rnx\sites.sit'
    ofile='D:\ICD\projects\2015nov.lys.yunnan\DATA\30S24h\rnx\sites.shp'
    lfile='D:\ICD\projects\2015nov.lys.yunnan\DATA\30S24h\rnx\site.latlonht.net'
    
    PRINT,'Usage: gps_site_shp, site_file, out_name[, lfile=lfile]'
    ;RETURN
  ENDIF
  
 IF N_ELEMENTS(LFILE) EQ 0 THEN LFILE=GET_DEFAULT_QOCA_NETWORK_FILE()
  
  RDSIT,SFILE,SITES=SITES
  NS=N_ELEMENTS(SITES)
  IF NS LE 0 THEN BEGIN
    PRINT,'[GPS_SITE_SHP]NO SITES!'
    RETURN
  ENDIF
  READ_NET,LFILE, SITE=SITES,LLH=LLHS
  LLS=LLHS[0:1,*]
;  FOR SI=0,NS-1 DO BEGIN
;    SITE=SITES[SI]
;    READ_NET,LFILE, SITE=SITE,LLH=LLH
;    LLS[*,SI]=LLH[0:1]
;    PRINT,SITE,LLS[*,SI]
;  ENDFOR
  ;RETURN
  ;stop
  SHP_POINT,OFILE, SITES,XI=REFORM(LLS[0,*]), YI=REFORM(LLS[1,*]), CAPS='sname'
END
