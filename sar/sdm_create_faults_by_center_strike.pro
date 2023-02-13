PRO SDM_CREATE_FAULTS_BY_CENTER_STRIKE

  PROG=(STRSPLIT(LAST(SCOPE_TRACEBACK()),/EXTRACT))[0]
  
  ;given the epicenter and strike of the fault, output fault traces
  ;
  ofile='D:\Papers\paper.Xiangyang-Buruo\sdm\nodal_strike_61.5\param\faults.txt'
  ;ofile='D:\Papers\paper.Xiangyang-Buruo\sdm\nodal_strike_182\param\faults.txt'
  
  ;epicenter
  x0= 86.238058d0
  y0= 34.374268d0
  
  ;fault strike
  strike=61.5 ;in degree
  ;strike=182 ;in degree
  ;
  theta=!dpi/2-strike*!dpi/180d0
  ;stop
  
  ;length of fault
  length_fault=0.5 ;in degrees
  
  ;spacing between neighboring faults
  spacing_fault=0.03 ;in degrees
  n_fault=11 ;
  
  
  ;  shifts=INDGEN(20)*.02d0-.3
  ;  nx=N_ELEMENTS(shifts)
  ;  PRINT,shifts
  ;
  ;  xmin=MIN((x1+x2)/2+shifts,max=xmax)
  
  WINDOW,1
  PLOT,[x0],[y0],color='0'x,background='ffffff'x,psym=-4,/ynozero,  $
    yrange=[y0-length_fault/2,y0+length_fault/2],/iso,  $
    xrange=[x0-n_fault/2*spacing_fault,x0+n_fault/2*spacing_fault]
    
    
  rate=TAN(theta)  ;
  x1=x0+length_fault*.5d0*COS(theta)
  y1=y0+length_fault*.5d0*SIN(theta)
  x2=x0-length_fault*.5d0*COS(theta)
  y2=y0-length_fault*.5d0*SIN(theta)
  
  OPLOT,[x2,x1],[y2,y1], color='000ff0'x,psym=-2,linestyle=2
  ;oplot,[x0,x1],[y0,y1], color='0000ff'x,psym=-2
  
  
  theta2=theta-!dpi/2
  
  fa_xys=[-9999d0]
  FOR i=0, n_fault-1 DO BEGIN
    x0i=x0+(i-n_fault/2)*spacing_fault*COS(theta2)
    y0i=y0+(i-n_fault/2)*spacing_fault*SIN(theta2)
    ;x4=x0+spacing_fault*cos(theta2)
    ;y4=y0+spacing_fault*sin(theta2)
    OPLOT,[x0i],[y0i], color='ff0000'x,psym=5
    
    x1i=x0i+length_fault*.5d0*COS(theta)
    y1i=y0i+length_fault*.5d0*SIN(theta)
    x2i=x0i-length_fault*.5d0*COS(theta)
    y2i=y0i-length_fault*.5d0*SIN(theta)
    OPLOT,[x2i,x1i],[y2i,y1i], color='000ff0'x,psym=-2,linestyle=2
    
    c1=[x1i,y1i]
    d1=[x2i,y2i]
    
    IF fa_xys[0] EQ -9999d0 THEN BEGIN
      fa_xys=[c1,d1]
    ENDIF ELSE BEGIN
      fa_xys=[[fa_xys], [c1,d1]]
    ENDELSE
    
  ENDFOR
  
  
  OPENW,fid,ofile,/get_lun
  WRITE_SYS_INFO,fid,prog=prog,src=''
  PRINTF,fid,strike, format='("* strike=",f,1x," (degrees)")'
  PRINTF,fid,length_fault, format='("* length of fault=",f,1x," (degrees)")'
  PRINTF,fid,spacing_fault, format='("* spacing of faults=",f,1x," (degrees)")'
  PRINTF,fid,x0,y0, format='("* center =(",f,1x,",",1x,f,") (degrees)")'
  PRINTF,fid,'#','fa_id','lon_1','lat_1','lon_2','lat_2',format='(a1,1x,a5,1x,2(1x,a20),1x,2(1x,a20))'
  FOR i=0,N_ELEMENTS(fa_xys[0,*])-1 DO BEGIN
    PRINTF,fid,i+1,fa_xys[[0,1,2,3],i],format='(1x,1x,i05,1x,2(1x,f20.8),1x,2(1x,f20.8))'
  ENDFOR
  printf,fid,'Now reverse the strike ...', format='("*",1x,a)'
  PRINTF,fid,'#','fa_id','lon_2','lat_2','lon_1','lat_1',format='(a1,1x,a5,1x,2(1x,a20),1x,2(1x,a20))'
  FOR i=0,N_ELEMENTS(fa_xys[0,*])-1 DO BEGIN
    PRINTF,fid,i+1+N_ELEMENTS(fa_xys[0,*]),fa_xys[[2,3,0,1],i],format='(1x,1x,i05,1x,2(1x,f20.8),1x,2(1x,f20.8))'
  ENDFOR
  ;
  printf,fid,'gmt2kml -Fl -W1p,white faults.psxy > faults.kml',format='("* ",a)'
  FREE_LUN,fid
  
  
  ofile=desuffix(ofile)+'.psxy'
  OPENW,fid,ofile,/get_lun
  ;WRITE_SYS_INFO,fid,prog=prog,src=''
  ;PRINTF,fid,'#','fa_id','lon_1','lat_1','lon_2','lat_2',format='(a1,1x,a5,1x,2(1x,a20),1x,2(1x,a20))'
  FOR i=0,N_ELEMENTS(fa_xys[0,*])-1 DO BEGIN
    PRINTF,fid,i+1,fa_xys[*,i],format='(">",2x,i05,%"\n",1x,2(1x,f20.8),%"\n",1x,2(1x,f20.8))'
  ;PRINTF,fid,i+1,fa_xys[*,i],format='(%"> %05d\n%f %f\n%f %f")'
  ENDFOR
  FREE_LUN,fid
  
  
  
;  OPENW,fid,desuffix(ofile)+'_'+'.txt',/get_lun
;
;  WRITE_SYS_INFO,fid,prog=prog,src=''
;  PRINTF,fid,'#','fa_id','lon_1','lat_1','lon_2','lat_2',format='(a1,1x,a5,1x,2(1x,a20),1x,2(1x,a20))'
;  FOR i=0,N_ELEMENTS(fa_xys[0,*])-1 DO BEGIN
;    PRINTF,fid,i+1,fa_xys[*,i],format='(1x,1x,i05,1x,2(1x,f20.8),1x,2(1x,f20.8))'
;  ENDFOR
;  FREE_LUN,fid
  
END