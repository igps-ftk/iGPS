PRO  WRITE_VEL_PROFILE, ofile $
    , odata $
    , sites=sites $
    , fa_xys=fa_xys  $
    , pf_xys=pf_xys  $
    , fa_pf_xy=fa_pf_xy $
    , odata_2nd=odata_2nd $
    , src=src $
    , headers=headers
    
  PROG=(STRSPLIT(LAST(SCOPE_TRACEBACK()),/EXTRACT))[0]
  
  IF N_PARAMS()LT 2 THEN BEGIN
    ;test only
    ofile='D:\gsar\interseismic\004-d-m5-0476_0481_0486_0491_0496-jiali8\f123\sbas.4.0.0001.9999.20170311.20230831.060.0410.01.___\pg.fa_ganzi\profile_001_vel.psxy'
    PRINT,ofile
    odata=DBLARR(21,3)
    odata=RANDOMU(5L,21,5)
  ENDIF
  
  
  IF STRTRIM(ofile,2) EQ '' THEN BEGIN
    PRINT,'['+PROG+']ERROR: no output file given ('+ofile+')!!'
    RETURN
  ENDIF
  
  IF N_ELEMENTS(headers) EQ 0 THEN headers=''
  
  IF N_ELEMENTS(pf_xys) EQ 0 THEN BEGIN
    pf_xys=[[-9999,-9999],[-9999,-9999]]
  ENDIF
  
  IF N_ELEMENTS(fa_xys) EQ 0 THEN BEGIN
    fa_xys=[[-9999,-9999],[-9999,-9999]]
  ENDIF
  
  IF N_ELEMENTS(fa_pf_xy) EQ 0 THEN BEGIN
    fa_pf_xy=[-9999,-9999]
  ENDIF
  
  IF N_ELEMENTS(sites) EQ 0 THEN BEGIN
    sites='p'+STRTRIM(INDGEN(N_ELEMENTS(odata[0,*]))+1,2)
  ENDIF
  
  ;    labels='site     p_long    p_lati     p_dist    v_along  ve_along     v_tang   ve_tang       v_up     ve_up       long      lati dist_to_fault          ve         vn     ve_sig     vn_sig      vlos_d vlos_d_sig     vlos_a vlos_a_sig'
  ;    labels='01_site          02_p_long        03_p_lati        04_p_dist        05_v_along       06_ve_along      07_v_tang        08_ve_tang      09_v_up          10_ve_up         11_long          12_lati          13_dist_to_fault  14_vlos 15_vlos_sig   16_ve          17_vn            18_ve_sig       19_vn_sig 20_Cen 21_Ceu 22_Cnu'
  ;    labels=STRSPLIT(labels,/extra)
  ;  inds=INDGEN(N_ELEMENTS(tmp))+1
  ;  labels=STRING(inds,format='(i02)')+'_'+tmp
  labels_description=[ ['01site         ', 'name of location'] $
    ,['02pLon       ', 'longitude of point projected onto the profile line (deg)'] $
    ,['03pLat       ', 'latitude of point projected onto the profile line (deg)'] $
    ,['04pDist       ', 'distance from location to profile (km)'] $
    ,['05VNor      ', 'velocity along the profile (normal to the fault trace); positive-north (mm/yr)'] $
  ,['06VeNor     ', 'velocity uncertainty along the profile (mm/yr)'] $
    ,['07VPar       ', 'velocity tangent to the profile (parallel to the fault trace); positive-90deg-clockwise from v_along direction (mm/yr)'] $
  ,['08VePar      ', 'velocity uncertainty tangent to the profile (mm/yr)'] $
    ,['09VUp         ', 'vertical velocity; positive-up (mm/yr)'] $
  ,['10VeUp        ', 'vertical velocity uncertainty'] $
    ,['11lon         ', 'longitude of location (deg)'] $
    ,['12lat         ', 'latitude of location (deg)'] $
    ,['13distFa', 'distance from location to fault; positive-east (km)'] $
  ,['14VLOS       ', 'InSAR LOS velocity (mm/yr)'] $
    ,['15VeLOS   ', 'InSAR velocity uncertainty (mm/yr)'] $
    ,['16VE           ', 'east velocity of location; positive-east (mm/yr)'] $
  ,['17VN           ', 'north velocity of location; positive-north (mm/yr)'] $
  ,['18VEe       ', 'east velocity uncertainty (mm/yr)'] $
    ,['19VNe       ', 'north velocity uncertainty (mm/yr)'] $
    ,['20CEN       ', 'correlation coefficient between east and north'] $
    ,['21CEU       ', 'correlation coefficient between east and up'] $
    ,['22CNU       ', 'correlation coefficient between north and up'] $
    ,['23distFa2    ', 'distance from location to 2nd fault; positive-east (km)'] $
  ,['24StrkDif       ', 'angle of strikes of two faults (degree)'] $
    ]
    
  labels_description[0,*]=STRTRIM(REFORM(labels_description[0,*]),2)
  ;stop
  
  OPENW,fid,ofile,/get_lun
  WRITE_SYS_INFO,fid,prog=prog,user=user
  IF N_ELEMENTS(src) GT 0 THEN BEGIN
    PRINTF,FID,'*   SRC: '+SRC,FORMAT='(A)'
  ENDIF
  IF headers[0] NE '' THEN PRINTF,fid,headers,format='(a)'
  ;output profile vertex
  IF pf_xys[0,0] NE -9999d0 THEN BEGIN
    PRINTF,fid,pf_xys[*,0],format='("# PSXY_PROFILE",2(1x,f20.8))'
    PRINTF,fid,pf_xys[*,1],format='("# PSXY_PROFILE",2(1x,f20.8))'
  ENDIF
  IF fa_pf_xy[0,0] NE -9999d0 THEN BEGIN
    PRINTF,fid,fa_pf_xy,format='("# PSXY_FAULT_PROFILE_INTERSECT",2(1x,f20.8))'
  ENDIF
  IF fa_xys[0,0] NE -9999d0 THEN BEGIN
    FOR j=0ull,N_ELEMENTS(fa_xys[0,*])-1 DO BEGIN
      PRINTF,fid,fa_xys[*,j],format='("# PSXY_FAULT_TRACE",2(1x,f20.8))'
    ENDFOR
  ENDIF
  PRINTF,fid,'*',format='(a)'
  ;output stations  help, labels
  FOR i=0ull,N_ELEMENTS(labels_description[0,*])-1 DO BEGIN
    PRINTF,fid,labels_description[*,i],format='("*",1x,a-16,1x,":",1x,a)'
  ENDFOR
  PRINTF,fid,'*',format='(a)'
  
  IF N_ELEMENTS(odata_2nd) LT 2 THEN BEGIN  ;if no 2nd fault
    PRINTF,fid,labels_description[0,0:21],  $
      format='("*",a9,(1x,a8,1x,a7),1x,a7,3(1x,a8,1x,a7),(1x,a8,1x,a7),1x,a9, (1x,a8,1x,a7),2(1x,a8),4(1x,a7),1x,a6)'
    ;printf,fid,'*',format='(a)'
    FOR j=0ull, N_ELEMENTS(odata[0,*])-1 DO BEGIN
      PRINTF,fid,sites[j],odata[*,j],$
        format='(1x,a9, (1x,f8.3,1x,f7.3), 1x,f7.2, 3(1x,f8.2,1x,f7.2),(1x,f8.2,1x,f7.2), 1x,f9.3,(1x,f8.2,1x,f7.2),2(1x,f8.2),4(1x,f7.2),1x,f6.3)'
    ENDFOR
  ENDIF ELSE BEGIN ; if with 2nd fault
    PRINTF,fid,labels_description[0,*],  $
      format='("*",a9,(1x,a8,1x,a7),1x,a7,3(1x,a8,1x,a7),(1x,a8,1x,a7),1x,a9, (1x,a8,1x,a7),2(1x,a8),4(1x,a7),1x,a6,20(1x,a9))'
    ;printf,fid,'*',format='(a)'
    ;stop
    FOR j=0ull, N_ELEMENTS(odata[0,*])-1 DO BEGIN
      PRINTF,fid,sites[j],odata[*,j],odata_2nd[*,j],  $
        format='(1x,a9, (1x,f8.3,1x,f7.3), 1x,f7.2, 3(1x,f8.2,1x,f7.2),(1x,f8.2,1x,f7.2), 1x,f9.3,(1x,f8.2,1x,f7.2),2(1x,f8.2),4(1x,f7.2),1x,f6.3,20(1x,f9.3))'
    ENDFOR
  ENDELSE
  
  FREE_LUN,fid
;stop
END