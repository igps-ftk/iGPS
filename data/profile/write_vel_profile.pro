PRO  WRITE_VEL_PROFILE, ofile $
    , odata $
    , sites=sites $
    , fa_xys=fa_xys  $
    , pf_xys=pf_xys  $
    , fa_pf_xy=fa_pf_xy $
    , headers=headers
    
  PROG=(STRSPLIT(LAST(SCOPE_TRACEBACK()),/EXTRACT))[0]
  
  IF N_PARAMS()LT 2 THEN BEGIN
    ;test only
    ofile='D:\gsar\interseismic\004-d-m5-0476_0481_0486_0491_0496-jiali8\f123\sbas.4.0.0001.9999.20170311.20230831.060.0410.01.___\pg.fa_ganzi\profile_001_vel.psxy'
    print,ofile
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
    ]
    
  labels_description[0,*]=STRTRIM(REFORM(labels_description[0,*]),2)
  ;stop
  
  OPENW,fid,ofile,/get_lun
  WRITE_SYS_INFO,fid,prog=prog,user=user
  PRINTF,fid,headers,format='(a)'
  ;output profile vertex
  IF pf_xys[0,0] NE -9999d0 THEN BEGIN
    PRINTF,fid,pf_xys[*,0],format='("# PSXY_PROFILE",2(1x,f20.8))'
    PRINTF,fid,pf_xys[*,1],format='("# PSXY_PROFILE",2(1x,f20.8))'
  ENDIF
  IF fa_pf_xy[0,0] NE -9999d0 THEN BEGIN
    PRINTF,fid,fa_pf_xy,format='("# PSXY_FAULT_PROFILE_INTERSECT",2(1x,f20.8))'
  ENDIF
  IF fa_xys[0,0] NE -9999d0 THEN BEGIN
    FOR j=0,N_ELEMENTS(fa_xys[0,*])-1 DO BEGIN
      PRINTF,fid,fa_xys[*,j],format='("# PSXY_FAULT_TRACE",2(1x,f20.8))'
    ENDFOR
  ENDIF
  PRINTF,fid,'*',format='(a)'
  ;output stations  help, labels
  FOR i=0,N_ELEMENTS(labels_description[0,*])-1 DO BEGIN
    PRINTF,fid,labels_description[*,i],format='("*",1x,a-16,1x,":",1x,a)'
  ENDFOR
  PRINTF,fid,'*',format='(a)'
  
  PRINTF,fid,labels_description[0,*],  $
    format='("*",a9,(1x,a8,1x,a7),1x,a7,3(1x,a8,1x,a7),(1x,a8,1x,a7),1x,a9, (1x,a8,1x,a7),2(1x,a8),4(1x,a7),1x,a6)'
    
  ;printf,fid,'*',format='(a)'
  FOR j=0, N_ELEMENTS(odata[0,*])-1 DO BEGIN
    ;convert gps velocity to insar los direction
    ;descending
    ;    enu_j=[odata[4,2,j], 0d0]
    ;    enu_sig_j=[odata[5,3,j], 0d0]
    ;    vlos_des=sar_enu2los(enu_j, alpha=(193)*!dpi/180d0 )
    ;    vlos_asc=sar_enu2los(enu_j, alpha=(-13+360d0)*!dpi/180d0 )
    ;    vlos_sig_des=sar_enu2los(enu_sig_j, alpha=193*!dpi/180d0 )
    ;    vlos_sig_asc=sar_enu2los(enu_sig_j, alpha=(-13+360d0)*!dpi/180d0 )
  
    ;        PRINTF,fid,sites[pos[ind[j]]],p_lls[*,pos[ind[j]]],dists[pos[ind[j]]],vel_along_all[ind[j]],$
    ;      vele_along_all[ind[j]],vel_tang_all[ind[j]],vele_tang_all[ind[j]], $
    ;      vel_up_all[ind[j]],vele_up_all[ind[j]], lls[*,pos[ind[j]]], $
    ;      dists_fault[pos[ind[j]]], $
    ;      vels[[4,2,5,3],pos[ind[j]]], $
    ;      vlos_des,vlos_sig_des,vlos_asc,vlos_sig_asc,$
    ;      format='(1x,a4,1x,2f10.3,1x,f10.2,1x,2f10.2,1x,2f10.2,1x,2f10.2,1x,2f10.3,1x,f13.6,1x,4(1x,f10.3),1x,4(1x,f10.3))'
  
    PRINTF,fid,sites[j],odata[*,j],$
      format='(1x,a9, (1x,f8.3,1x,f7.3), 1x,f7.2, 3(1x,f8.2,1x,f7.2),(1x,f8.2,1x,f7.2), 1x,f9.3,(1x,f8.2,1x,f7.2),2(1x,f8.2),4(1x,f7.2),1x,f6.3)'
  ENDFOR
  FREE_LUN,fid
;stop
END