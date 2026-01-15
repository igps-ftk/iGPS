FUNCTION SAR_ENU2LOS, enu, theta=theta, alpha=alpha
  ;
  ;theta - satellite looking angle
  IF N_ELEMENTS(theta) EQ 0 THEN BEGIN
    theta=36d0*!dpi/180d0
  ENDIF
  ;
  IF N_ELEMENTS(alpha) EQ 0 THEN BEGIN
    alpha=193d0*!dpi/180d0  ; -167+360 for descending orbit
    alpha=(-13+360d0)*!dpi/180d0 ; for ascending orbit
  ENDIF
  
  dlos=enu[2]*COS(theta)-SIN(theta)*( enu[1]*COS(alpha-3*!dpi/2)+enu[0]*SIN(alpha-3*!dpi/2) )
  dlos2=(enu[1]*sin(alpha)-enu[0]*cos(alpha))*sin(theta)+enu[2]*cos(theta)
  ;print,dlos,dlos2
;  print,'input:',dlos,dlos2,enu, alpha,theta,format='(a,f,3(1x,f16.12),2(1x,f))'
;  print,'result:',cos(theta),-SIN(theta), COS(alpha-3*!dpi/2),SIN(alpha-3*!dpi/2),-SIN(theta)*COS(alpha-3*!dpi/2),-SIN(theta)*COS(alpha-3*!dpi/2)
  ;stop
  RETURN, dlos2
END

PRO SAR_ENU2LOS
;  enu=[-0.00495327,  0.01635940,  -0.03314708d0]
;  dlos=sar_enu2los(enu)
;    PRINT,enu,dlos
;  
;  enu=[ 0.088429600d0,0.096325300d0,         0.033124700d0]
;  dlos=sar_enu2los(enu)
;  PRINT,enu,dlos
;  
;  enu=[ 0.0823646,0.0945073    ,   0.0355227]
;  enu=[0.0692956  ,0.0590283    ,  0.0214207]
;  dlos1=sar_enu2los(enu)
;  PRINT,enu,dlos1
;  
;  enu=[ 0.0818366,0.0796533   ,    0.0337487]
;  enu=[ 0.0514836 ,0.0523783 ,     0.0274257]
;  
;  enu=[ 0.012 ,0.0 ,     0.0]
;  
  enu=[5d0, 0, 2 ]  ;East motion + uplift
  enu=[5d0, 0, -3.5 ]  ;East motion + subsidence
  
  enu=[0,0,15d0]
  
  enu_u=[0,0,1d0]
  enu_n=[0,1,0d0]
  enu_e=[1,0,0d0]
  
;  
;  ;dlos2=sar_enu2los(enu)
;  ;PRINT,enu,dlos2
;  
  npt=91
  theta_min=23d0
  theta_min=30d0
  theta_max=46d0
  theta_min=0d0
  theta_max=90d0
  
  npt=17
  theta_min=30d0
  theta_max=46d0
  
  theta_step=(1d0*theta_max-theta_min)/(npt-1)
  thetas=theta_min+theta_step*indgen(npt)
  loses_e_a=dblarr(npt)
  loses_e_d=dblarr(npt)
  loses_n_a=dblarr(npt)
  loses_n_d=dblarr(npt)
  loses_u_a=dblarr(npt)
  loses_u_d=dblarr(npt)
  for ti=0,npt-1 do begin
    theta_i=thetas[ti]
    loses_e_a[ti]=sar_enu2los(enu_e,theta=theta_i*!dpi/180d0, alpha=(-13+360d0)*!dpi/180d0) ; for ascending orbit
    loses_e_d[ti]=sar_enu2los(enu_e,theta=theta_i*!dpi/180d0, alpha=(193d0)*!dpi/180d0) ; for descending orbit
    loses_n_a[ti]=sar_enu2los(enu_n,theta=theta_i*!dpi/180d0, alpha=(-13+360d0)*!dpi/180d0) ; for ascending orbit
    loses_n_d[ti]=sar_enu2los(enu_n,theta=theta_i*!dpi/180d0, alpha=(193d0)*!dpi/180d0) ; for descending orbit
    loses_u_a[ti]=sar_enu2los(enu_u,theta=theta_i*!dpi/180d0, alpha=(-13+360d0)*!dpi/180d0) ; for ascending orbit
    loses_u_d[ti]=sar_enu2los(enu_u,theta=theta_i*!dpi/180d0, alpha=(193d0)*!dpi/180d0) ; for descending orbit
  endfor
  
  window,1
  plot ,thetas*!dpi/180,loses_e_a,background='ffffff'x,color='0'x,psym=0 $
    ,xrange=[0,360]*!dpi/180  $
    ,yrange=[-1,1],/nodata
    
  xs=indgen(360)
  ys=sin(xs*!dpi/180)
  ys2=cos(xs*!dpi/180)
  oplot,xs*!dpi/180,ys,color='ffff00'x,psym=1
  oplot,xs*!dpi/180,-1*ys,color='ffff00'x,psym=2
  oplot,xs*!dpi/180,1*ys2,color='ffff00'x,psym=3
  oplot,xs*!dpi/180,-1*ys2,color='ffff00'x,psym=4
  
  oplot,thetas*!dpi/180,loses_e_a,color='000000'x
  oplot,thetas*!dpi/180,loses_e_d,color='0000ff'x
  ;
  oplot,thetas*!dpi/180,loses_n_a,color='00ffff'x
  oplot,thetas*!dpi/180,loses_n_d,color='00ff00'x
  ;
  oplot,thetas*!dpi/180,loses_u_a,color='ff00ff'x,thick=2
  oplot,thetas*!dpi/180,loses_u_d,color='ff0000'x
  
;  window,2
;  plot ,thetas,1/loses_e_a,background='ffffff'x,color='0'x,xrange=[20,60],yrange=[-10,10],psym=0
;  oplot,thetas,1/loses_e_d,color='0000ff'x
;  ;
;  oplot,thetas,1/loses_n_a,color='00ffff'x
;  oplot,thetas,1/loses_n_d,color='00ff00'x
;  ;
;  oplot,thetas,1/loses_u_a,color='ff00ff'x,thick=2
;  oplot,thetas,1/loses_u_d,color='ff0000'x
  ;stop
;  
  ofile='D:\Papers\review.insar.active.fault\figure\sar.los.sensitivity\3d.vs.los2b.txt'
  openw,fid,ofile,/get_lun
  write_sys_info,fid,prog='SAR_ENU2LOS'
  printf,fid,'thetas','loses_e_a','loses_e_d','loses_n_a','loses_n_d','loses_u_a','loses_u_d',  $
  format='("*",a,6(1x,a))'
  for ti=0,npt-1 do begin
    printf,fid,thetas[ti],loses_e_a[ti],loses_e_d[ti],loses_n_a[ti],loses_n_d[ti],loses_u_a[ti],loses_u_d[ti],  $
    format='(1x,f,6(1x,f))'
  endfor
  free_lun,fid
  return
;  
;  
;  print,sar_enu2los(enu_e,theta=37*!dpi/180d0, alpha=(-13+360d0)*!dpi/180d0) ; for ascending orbit  
;  print,sar_enu2los(enu_e,theta=37*!dpi/180d0, alpha=(193d0)*!dpi/180d0) ; for ascending orbit
  print,sar_enu2los(enu_n,theta=37*!dpi/180d0, alpha=(-13+360d0)*!dpi/180d0) ; for ascending orbit  
  print,sar_enu2los(enu_n,theta=37*!dpi/180d0, alpha=(193d0)*!dpi/180d0) ; for ascending orbit
;  print,sar_enu2los(enu_u,theta=37*!dpi/180d0, alpha=(-13+360d0)*!dpi/180d0) ; for ascending orbit  
;  print,sar_enu2los(enu_u,theta=37*!dpi/180d0, alpha=(193d0)*!dpi/180d0) ; for ascending orbit
  
  ;stop
;  print,dlos1-dlos2
;  
;;
;  enu=[ 20.0,0,0]
  enu=[0.006954d0,-0.002111 , -0.035434]
  dlos=sar_enu2los(enu,theta=38*!dpi/180d0, alpha=(193d0)*!dpi/180d0)
  PRINT,enu,dlos


;vn=enu[1]
;ve=enu[0]
;vlos=gps_vel2los(vn,ve,theta=34d0*!dpi/180d0,alpha=(-13d0)*!dpi/180d0)
;print,'vlos:', vlos
  
END