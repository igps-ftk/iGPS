PRO SAR_MULTI_LOOK_SOL_LOS_TO_3D, loss, thetas, alphas,   $
    disps_3d
    
  IF N_PARAMS() LT 3 THEN BEGIN
    file='C:\Papers.data\sse.mila\multi-look\test.txt'
    file='C:\Papers.data\sse.mila\multi-look\xy113344.1'
    
    ofile=file+'.3d2'
    
    READ_COLS, file, data=data
    
    odata=DBLARR(5,N_ELEMENTS(data[0,*]))
    
    FOR pi=0,N_ELEMENTS(data[0,*])-1 DO BEGIN
      lon=data[0,pi]
      lat=data[1,pi]
      ;
      thetas=data[[3,6,9],pi]*!dpi/180d0
      alphas=data[[2,5,8],pi]*!dpi/180d0-3*!dpi/2
      loss=data[[4,7,10],pi]
      ;HELP, data, thetas, alphas, loss
      SAR_MULTI_LOOK_SOL_LOS_TO_3D, loss, thetas, alphas,   $
        disps_3d
      RETURN
    ENDFOR
  ENDIF
  
  n_look=N_ELEMENTS(thetas)
  ;rotation matrix
  IF n_look EQ 3 THEN BEGIN
    rmat=[ [COS(thetas[0]), -1d0*SIN(thetas[0])*COS(alphas[0]), -1d0*SIN(thetas[0])*SIN(alphas[0]) ],  $
      [COS(thetas[1]), -1d0*SIN(thetas[1])*COS(alphas[1]), -1d0*SIN(thetas[1])*SIN(alphas[1]) ],  $
      [COS(thetas[2]), -1d0*SIN(thetas[2])*COS(alphas[2]), -1d0*SIN(thetas[2])*SIN(alphas[2]) ] ]
  ENDIF ELSE BEGIN
    IF n_look EQ 4 THEN BEGIN
      rmat=[ [COS(thetas[0]), -1d0*SIN(thetas[0])*COS(alphas[0]), -1d0*SIN(thetas[0])*SIN(alphas[0]) ],  $
        [COS(thetas[1]), -1d0*SIN(thetas[1])*COS(alphas[1]), -1d0*SIN(thetas[1])*SIN(alphas[1]) ],  $
        [COS(thetas[2]), -1d0*SIN(thetas[2])*COS(alphas[2]), -1d0*SIN(thetas[2])*SIN(alphas[2]) ],  $
        [COS(thetas[3]), -1d0*SIN(thetas[3])*COS(alphas[3]), -1d0*SIN(thetas[3])*SIN(alphas[3]) ] ]
    ENDIF ELSE BEGIN
      STOP
    ENDELSE
  ENDELSE
;  ;stop
;  ;get the invert of the rotation matrix
;  ;rmat=TRANSPOSE(rmat)
;  rmat_inv=LA_INVERT(rmat,/double,status=status)
;  ;rmat_inv=transpose(rmat_inv)
;  disps_3d=rmat_inv##loss
;  
;  ;d3=transpose(rmat_inv)##TRANSPOSE(loss)
;  los2=rmat##REFORM(disps_3d)
;  
;  aludc=rmat
;  LA_LUDC,aludc,index
;  b=loss
;  x=LA_LUSOL(aludc,index,b)
;  PRINT,'x:',x
  
  aludc=rmat
  b=loss
  disps_3d=LA_LEAST_SQUARES(aludc,b,rank=rank,status=status,residual=resid,/double)
  PRINT,'x2:',disps_3d
  ;STOP
END