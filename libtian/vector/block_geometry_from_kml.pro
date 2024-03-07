PRO BLOCK_GEOMETRY_FROM_KML, file,  $
  np=count, $
  pxys=opxys, $
  names=names, $
  bcs=pc_xys
  
  IF N_ELEMENTS(file) EQ 0 THEN BEGIN
    file='D:\gpse\eq.sc08\block\defnode\jiali20240223\vector2\block_fault_model.txt'
    file='D:\gpse\eq.sc08\block\defnode\g219\vector\blocks_western_tibet.psxy'
  ENDIF
  
  if n_elements(dist_max) eq 0 then dist_max=5d3
  
  ext=getfilesuffix(file)
  ofile=desuffix(file)+'_corrected.'+ext
  ;ofile_bc=file+'.center'
  
  READ_PSXY,   $
    file,   $ ;input file
    region=regions,   $ ;x,y coorinates of each polygons (pointer type)
    nps=nps, $  ;number of point pairs for each polygon
    count=count,  $ ;number of polygons
    igpsmode=igpsmode,  $ ;whether skip comments lines (i.e., non-blank first column lines)
    names=names   ;region names (if exist)
    
  HELP, regions, nps, count,igpsmode,names
  opxys=regions
  ;
  ;firstly, connect adjacent fault vertices
  FOR i=0,count-1 DO BEGIN
    name=names[i]
    poly_type=STRMID(name,3,2)
    ;IF poly_type EQ 'bl' THEN CONTINUE ;in the 1st round, only move adjacent fault vertices (fa_)
    PRINT,'testing '+name
    pxy1=(*opxys[i])[*,0]
    pxy2=(*opxys[i])[*,nps[i]-1]
    ;
    FOR j=0,count-1 DO BEGIN
      IF i EQ j THEN CONTINUE ;skip itself
      ;IF STRMID(names[j],3,2) EQ 'bl' THEN CONTINUE
      FOR k=0,nps[j]-1 DO BEGIN
        fxy1=(*opxys[j])[*,k]
        pf_dist=MAP_2POINTS(pxy1[0],pxy1[1],fxy1[0],fxy1[1],/meter)
        IF pf_dist LE dist_max THEN BEGIN
          (*opxys[i])[*,0]=fxy1
          PRINT,'starting point corrected:',i,j,k
        ENDIF
        pf_dist=MAP_2POINTS(pxy2[0],pxy2[1],fxy1[0],fxy1[1],/meter)
        IF pf_dist LE dist_max THEN BEGIN
          (*opxys[i])[*,nps[i]-1]=fxy1
          PRINT,'ending point corrected:',i,j,k
        ENDIF
        
      ENDFOR
      
    ENDFOR
    
  ;stop
  ENDFOR
  ;STOP
  
  ;secondly, connect boundary vertices to fault
;  pc_xys=DBLARR(2,count)
  FOR i=0,count-1 DO BEGIN
    name=names[i]
    poly_type=STRMID(name,3,2)
    IF poly_type EQ 'fa' THEN CONTINUE ;only move boundary vertices (ba_)
    PRINT,'testing '+name
    pxy1=(*opxys[i])[*,0]
    pxy2=(*opxys[i])[*,nps[i]-1]
    ;
    FOR j=0,count-1 DO BEGIN
      IF i EQ j THEN CONTINUE ;skip itself
      IF STRMID(names[j],3,2) EQ 'bl' THEN CONTINUE
      FOR k=0,nps[j]-1 DO BEGIN
        fxy1=(*opxys[j])[*,k]
        pf_dist=MAP_2POINTS(pxy1[0],pxy1[1],fxy1[0],fxy1[1],/meter)
        IF pf_dist LE dist_max THEN BEGIN
          (*opxys[i])[*,0]=fxy1
          PRINT,'starting point corrected:',i,j,k
        ENDIF
        pf_dist=MAP_2POINTS(pxy2[0],pxy2[1],fxy1[0],fxy1[1],/meter)
        IF pf_dist LE dist_max THEN BEGIN
          (*opxys[i])[*,nps[i]-1]=fxy1
          PRINT,'ending point corrected:',i,j,k
        ENDIF
      ENDFOR
      
    ENDFOR
    
    pi_xys=(*opxys[i])
    ;pc_xys[*,i]=POLYGON_CENTER(pi_xys)
    
  ;stop
  ENDFOR
  ;stop
  
  OPENW,fid,ofile,/get_lun
  FOR i=0, count-1 DO BEGIN
    PRINTF,fid,names[i],format='("> ",a)'
    FOR j=0,nps[i]-1 DO BEGIN
      PRINTF,fid,(*opxys[i])[*,j],format='(2(1x,f))'
    ENDFOR
  ENDFOR
  FREE_LUN,fid
  
;  OPENW,fid,ofile_bc,/get_lun
;  FOR i=0, count-1 DO BEGIN
;    name=names[i]
;    poly_type=STRMID(name,3,2)
;    IF poly_type EQ 'fa' THEN CONTINUE ;only move boundary vertices (ba_)
;    PRINTF,fid,STRMID(names[i],6,4),format='("> -L",a)'
;    PRINTF,fid,pc_xys[*,i],format='(2(1x,f))'
;  ENDFOR
;  FREE_LUN,fid
  
  FOR i=0,count-1 DO PTR_FREE,regions[i]
  
END