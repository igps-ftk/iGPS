PRO PROFILE_STEP_FIT
  path='D:\ICD\Eighth\2021\20210331.linyi_dsm\dsm\p\'
  opath='D:\ICD\Eighth\2021\20210331.linyi_dsm\dsm\pf'
  
  files=FILE_SEARCH(path+PATH_SEP()+'profile_???_vel_dem.txt', count=nf)
  IF nf LE 0 THEN RETURN
  
  FOR fi=0,nf-1 DO BEGIN
    file=files[fi]
    
    ;file='D:\ICD\Eighth\2021\20210331.linyi_dsm\dsm\p\profile_039_vel_dem.txt'
    
    lines=read_txt(file,comment='~ ')
    HELP, lines
    
    data=DOUBLE(str_lines2arr(lines))
    HELP, lines, data
    PRINT,lines[0:3]
    
    x=REFORM(data[0,*])
    y=REFORM(data[2,*])
    
    WINDOW,1
    
    npt=N_ELEMENTS(x)
    step_x=10
    xmin=x[0]
    
    nstep=npt/step_x
    rmses=DBLARR(nstep)
    rmses[*]=9999d0
    ;for offi=step_x, npt-step_x, step_x do begin
    FOR si=1, nstep-2 DO BEGIN
      off_x=x[si*step_x]
      ;stop
      offset=[-9999d0,-9999, off_x]
      psdecay=[-9999d0,-9999, off_x]
      ratechange=[-9999d0,-9999, off_x]
      ;TS_MODEL, x, y, offset=offset,yfit=yfit, ratechange=ratechange
      TS_MODEL, x, y, offset=offset,yfit=yfit, psdecay=psdecay
      rmse=rms(y-yfit)
      rmses[si]=rmse
    ;print,offset
    ;  PLOT,x,y,background='ffffff'x,color='0'x,/ynozero,title=STRING(rmse,format='("Residual RMS:",1x,F10.3)')
    ;  OPLOT,x,yfit,color='0000ff'x
    ;  STOP
    ENDFOR
    tmp=MIN(rmses,pos1)
    ;stop
    
    off_x=x[pos1*step_x]
    offset=[-9999d0,-9999, off_x]
    psdecay=offset
    TS_MODEL, x, y, offset=offset,yfit=yfit, psdecay=psdecay
    WINDOW,1
    PLOT,x,y,background='ffffff'x,color='0'x,/ynozero,title=STRING(rmse,format='("Residual RMS:",1x,F10.3)')
    XYOUTS,MEAN(x),MEAN(y),'DEM offset:'+STRING(offset[0:1],'(F8.3,"+/-",1x,F8.3,"m")'),color='0'x
    OPLOT,x,yfit,color='0000ff'x
    
    ofile=opath+PATH_SEP()+GETFILENAME(file)
    OPENW,fid,ofile,/get_lun
    WRITE_SYS_INFO, fid, prog=prog, user=user
    PRINTF,fid,offset,data[0:1,pos1*step_x],format='("*",6(1x,f))'
    PRINTF,fid,'map_x','map_y','obs','model',format='("*",2(1x,a20),1x,a10,1x,a10)'
    FOR i=0, N_ELEMENTS(x)-1 DO BEGIN
      PRINTF,fid,x[i],data[1,i],y[i],yfit[i],format='(1x,2(1x,f20.5),1x,f10.3,1x,f10.3)'
    ENDFOR
    FREE_LUN,fid
    jfile=desuffix(ofile)+'.jpg'
    WRITE_JPEG,jfile,TVRD(true=1),true=1,quality=100
    
  ENDFOR
  
END