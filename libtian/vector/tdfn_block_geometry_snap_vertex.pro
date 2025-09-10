PRO TDFN_BLOCK_GEOMETRY_SNAP_VERTEX, file,  $
    np=count, $
    pxys=opxys, $
    names=names, $
    bcs=pc_xys
    
  IF N_ELEMENTS(file) EQ 0 THEN BEGIN
    file='D:\gpse\eq.sc08\block\defnode\jiali_20241202\model\block_fault_model.txt
    file='D:\gpse\eq.sc08\block\defnode\jiali_20250705\model\jiali_20250705e.psxy'
  ENDIF
  
  IF N_ELEMENTS(dist_max) EQ 0 THEN dist_max=10d3
  
  ext=getfilesuffix(file)
  ofile=desuffix(file)+'_corrected.'+ext
  ofile_bc=file+'.center'
  
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
  print,'1). connect adjacent fault vertices ...'
  FOR i=0,count-1 DO BEGIN ;for each fault
    name=names[i]
    poly_type=STRMID(name,3,2)
    IF poly_type ne 'fa' THEN CONTINUE ;in the 1st round, only move adjacent fault vertices (fa_)
    PRINT,'testing '+name
    
    ;for each vertex
    FOR pi=0, nps[i]-1 DO BEGIN
      pxy1=(*opxys[i])[*,pi]
      ;
      FOR j=i+1,count-1 DO BEGIN ;loop for the 2nd fault
        if i eq j then continue ;skip itself
        IF STRMID(names[j],3,2) ne 'fa' THEN CONTINUE
        FOR k=0,nps[j]-1 DO BEGIN ;loop for each vertex in the 2nd fault
          fxy1=(*opxys[j])[*,k]
          pf_dist=MAP_2POINTS(pxy1[0],pxy1[1],fxy1[0],fxy1[1],/meter)
          IF pf_dist LE dist_max THEN BEGIN
            (*opxys[j])[*,k]=pxy1
            ;PRINT,'dists:',pf_dist, dist_max
            PRINT,'  vertex ',pi,' on fault ',i,' has been assigned to ',k,' on fault ',j
          ENDIF
          
        ENDFOR
        
      ENDFOR
    ENDFOR ;end-of-loop-for-each-vertex-in-1st-fault
    
  ;stop
  ENDFOR ;end-of-loop-for-1st-fault
  ;STOP
  
  print,'2). connect boundary vertices to fault ...'
  pc_xys=DBLARR(2,count)
  FOR i=0,count-1 DO BEGIN
    name=names[i]
    poly_type=STRMID(name,3,2)
    IF poly_type ne 'bl' THEN CONTINUE ;only move boundary vertices (ba_)
    PRINT,'testing '+name
    
    ;for each vertex
    FOR pi=0, nps[i]-1 DO BEGIN
      pxy1=(*opxys[i])[*,pi]
      ;
      FOR j=0,count-1 DO BEGIN
        if i eq j then continue
        IF STRMID(names[j],3,2) ne 'fa' and STRMID(names[j],3,2) ne 'bl' THEN CONTINUE
        FOR k=0,nps[j]-1 DO BEGIN
          fxy1=(*opxys[j])[*,k]
          pf_dist=MAP_2POINTS(pxy1[0],pxy1[1],fxy1[0],fxy1[1],/meter)
          IF pf_dist LE dist_max THEN BEGIN
            (*opxys[i])[*,pi]=fxy1
            PRINT,'vertex ',k,' on boundary ',j,' has been assigned to ',pi,' on fault ',i
          ENDIF
        ENDFOR
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