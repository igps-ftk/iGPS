PRO COULOMB3_DISP_ENU2LOS, file, ofile

  PROG=(STRSPLIT(LAST(SCOPE_TRACEBACK()),/EXTRACT))[0]
  
  IF N_ELEMENTS(file) EQ 0 THEN BEGIN
  
    ;file='D:\Papers\paper.Xiangyang-Buruo\coulomb3\20160812\Displacement_16km.cou'
    ;
    path='D:\Papers\paper.Xiangyang-Buruo\coulomb3\20160812\'
    files=FILE_SEARCH(path+PATH_SEP()+'Displacement_1*km.cou',count=nf)
    
    files='F:\coulomb34\output_files\Displacement.cou'
    nf=1
    
    FOR fi=0,nf-1 DO BEGIN
      file=files[fi]
      vfile=file+'.los'
      COULOMB3_DISP_ENU2LOS, file, vfile
      continue ;;;
      ;;;
      ;
      ffile='D:\Papers\paper.Xiangyang-Buruo\coulomb3\20160812\Cross_section.dat.psxy'
      opath=path+PATH_SEP()+desuffix(GETFILENAME(file))+path_sep()+'p'
      ;PRINT,opath
      ;CONTINUE
      SAR_LOS_PROFILES_XYV, vfile=vfile, $  ;velocity file (in varied formats)
        opath=opath, $   ;output path
        ffile=ffile
      ;RETURN
    ENDFOR
    
    RETURN
    
  ENDIF
  
  PRINT,'['+prog+']Input:'+file
  PRINT,'['+prog+']Output:'+ofile
  
  READ_COLS,file,data=data,skip=3
  ;HELP,data
  npt=N_ELEMENTS(data[0,*])
  odata=DBLARR(4,npt)
  FOR i=0ull, npt-1 DO BEGIN
    odata[*,i]=[data[0:2,i], sar_enu2los(data[3:5,i])]
  ENDFOR
  
  OPENW,fid,ofile,/get_lun
  WRITE_SYS_INFO, fid, prog=prog,src=[file]
  PRINTF,fid,'X_km','Y_km','LOS_m','Z_km',format='("*",4(1x,a20))'
  FOR i=0ull,npt-1 DO BEGIN
    PRINTF,fid,odata[[0,1,3,2],i],format='(1x,4(1x,f20.10))'
  ENDFOR
  FREE_LUN,fid
  PRINT,'['+prog+']Normal end.'
;stop
END