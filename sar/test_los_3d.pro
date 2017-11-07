PRO TEST_LOS_3D
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
    
    ;rotation matrix
    rmat=[ [COS(thetas[0]), -1d0*SIN(thetas[0])*COS(alphas[0]), -1d0*SIN(thetas[0])*SIN(alphas[0]) ],  $
      [COS(thetas[1]), -1d0*SIN(thetas[1])*COS(alphas[1]), -1d0*SIN(thetas[1])*SIN(alphas[1]) ],  $
      [COS(thetas[2]), -1d0*SIN(thetas[2])*COS(alphas[2]), -1d0*SIN(thetas[2])*SIN(alphas[2]) ] ]
     ;stop
    ;get the invert of the rotation matrix
    ;rmat=TRANSPOSE(rmat)
    rmat_inv=LA_INVERT(rmat,/double,status=status)
    ;rmat_inv=transpose(rmat_inv)
    d3=rmat_inv##loss
    
    ;d3=transpose(rmat_inv)##TRANSPOSE(loss)
    los2=rmat##REFORM(d3)
    
    aludc=rmat
    la_ludc,aludc,index
    b=loss
    x=la_lusol(aludc,index,b)
    print,'x:',x
    
    aludc=rmat
    b=loss
    x2=la_least_squares(aludc,b,rank=rank,status=status,residual=resid,/double)
    print,'x2:',x2
    
    odata[*,pi]=[lon,lat,REFORM(d3)]
    STOP
  ENDFOR
  
  OPENW,fid,ofile,/get_lun
  ;WRITE_SYS_INFO,fid,prog='',src=file
  FOR pi=0,N_ELEMENTS(data[0,*])-1 DO BEGIN
    PRINTF,fid,odata[*,pi],format='(1x,2(1x,f12.7),1x,3(1x,f10.5))'
  ENDFOR
  FREE_LUN,fid
END