 PRO SAR_S1_OVERLAPPING_CHECK_ASSIGN_IDS_SKIPPED, ids_skipped, pos
   IF ids_skipped[0] EQ -1 THEN BEGIN ;;
     ids_skipped=pos
   ENDIF ELSE BEGIN
     ids_skipped=[ids_skipped, pos]
   ENDELSE
   
 END
 
 PRO SAR_S1_OVERLAPPING_CHECK3, file, ofile
   IF N_ELEMENTS(file) EQ 0 THEN BEGIN
     file='D:\gsar\interseismic\t136-d-m_tianshan4M3\input.lst'
   ENDIF
   
   PROG=(STRSPLIT(LAST(SCOPE_TRACEBACK()),/EXTRACT))[0]
   
   IF N_ELEMENTS(ofile) EQ 0 THEN BEGIN
     ofile=file+'.ok'
   ENDIF
   
   lines=read_txt(file,comment='~ ')
   HELP, lines
   
   ;number of scenes
   ns=N_ELEMENTS(lines)
   scene_ids=STRARR(ns)
   ymdhms=INTARR(6,ns)
   dyrs=DBLARR(ns)
   ymds=STRARR(ns)
   sods=DBLARR(ns)
   oks=INTARR(ns)
   timestrs=STRARR(ns)
   
   ;convert time
   FOR i=0, ns-1 DO BEGIN
     line=lines[i]
     line_p=STRSPLIT(line,/extract)
     scene_ids[i]=line_p[0]
     tmp=STRSPLIT(line_p[0],'_',/extract)
     timestr=tmp[4]
     timestrs[i]=timestr
     year=STRMID(timestr,0,4)
     mon=STRMID(timestr,4,2)
     day=STRMID(timestr,6,2)
     hh=STRMID(timestr,9,2)
     mm=STRMID(timestr,11,2)
     ss=STRMID(timestr,13,2)
     ymds[i]=STRING(year,mon,day,format='(3a)')
     sods[i]=hh*3600d0+mm*60d0+ss
     ;print,line
     ;print,year,'-',mon,'-',day,'T',hh,':',mm,':',ss
     DOY,year,mon,day,hh,mm+ss/60d0,dyear=dyr,sectag=sectag
     dyrs[i]=dyr
   ENDFOR
   HELP,dyrs
   ;sort the times
   ;;ind=SORT(dyrs)
   ind=SORT(timestrs)
   dyrs=dyrs[ind]
   scene_ids=scene_ids[ind]
   ymds=ymds[ind]
   lines=lines[ind]
   sods=sods[ind]
   
   ;method 1). get the unique year-mon-day
   ;ymd_ids=ymds[UNIQ(ymds)]
   ;
   ;method 2). get the unique acquisitions
   tmp_days=(dyrs[1:*]-dyrs[0:N_ELEMENTS(dyrs)-2])
   ids_acquition=WHERE(tmp_days LT 1d0)
   ;
   ;method 3). get unique dates of acquisitions
   is_out=INTARR(N_ELEMENTS(dyrs))
   is_uniq=INTARR(N_ELEMENTS(dyrs))
   FOR di=0, N_ELEMENTS(dyrs)-1 DO BEGIN
     IF is_out[di] EQ 1 THEN CONTINUE
     tmp=WHERE(ABS(dyrs-dyrs[di])*365.25d0 LT 1 AND is_out EQ 0)
     is_uniq[tmp[0]]=1
     is_out[tmp]=1
   ;stop
   ENDFOR
   ids_acquition=WHERE(is_uniq EQ 1)
   ;help,ids_acquition,is_uniq
   ;stop
   
   ;remove adundant acquisitions
   is_out=INTARR(N_ELEMENTS(dyrs))
   FOR di=0, N_ELEMENTS(dyrs)-1 DO BEGIN
     IF is_out[di] EQ 1 THEN CONTINUE
     tmp=WHERE(ABS(dyrs-dyrs[di])*365.25d0 LT 1 AND is_out EQ 0 AND ABS(sods-sods[di]) LT 6)
     ;stop
     IF N_ELEMENTS(tmp) GT 1 THEN BEGIN
       is_out[tmp[1:*]]=1
     ENDIF
   ;stop
   ENDFOR
   ;HELP,is_out
   pos=WHERE(is_out EQ 1)
   ;HELP, pos
   ;print,lines[pos]
   ;STOP
   ids_skipped=[-1]
   IF pos[0] NE -1 THEN BEGIN
     SAR_S1_OVERLAPPING_CHECK_ASSIGN_IDS_SKIPPED, ids_skipped, pos
   ENDIF
   
   
   ;loop for each acquisition date
   ;
   FOR i=0, N_ELEMENTS(ids_acquition)-1 DO BEGIN
     dyr_i=dyrs[ids_acquition[i]]
     name_scene=(STRSPLIT(lines[ids_acquition[i]],/extract))[0]
     PRINT,'['+PROG+']INFO:searching pairing scenes for '+name_scene+' ...'
     
     pos=WHERE( ABS(dyrs-dyr_i)*365.25d0 LT 1 AND is_out NE 1 )
     IF N_ELEMENTS(pos) LT 3 THEN BEGIN  ;if the number
       PRINT,'['+PROG+']WARNING: no enough scenes for '+name_scene+'!'
       SAR_S1_OVERLAPPING_CHECK_ASSIGN_IDS_SKIPPED, ids_skipped, pos
       CONTINUE
     ENDIF
     
     tmp1=ymds[pos]
     tmp2=dyrs[pos]
     tmp3=sods[pos]
     ;stop
     
     IF N_ELEMENTS(pos) EQ 3 THEN BEGIN  ;if only two scenes found, check whether they are identidal.
       gap_sec=ABS(tmp3[1]-tmp3[0])
       gap_sec2=ABS(tmp3[2]-tmp3[1])
       IF gap_sec LT 10 || gap_sec2 LT 10 THEN BEGIN
         PRINT, '['+PROG+']WARNING: two scenes are identical!'
         PRINT, '['+PROG+']WARNING: '+lines[pos[0]]
         PRINT, '['+PROG+']WARNING: '+lines[pos[1]]
         PRINT, '['+PROG+']WARNING: '+lines[pos[2]]
         
         SAR_S1_OVERLAPPING_CHECK_ASSIGN_IDS_SKIPPED, ids_skipped, pos
         ;
         CONTINUE  ;skip the identical scenes
       ;
       ENDIF ELSE BEGIN  ;if not identical, then OK
         oks[pos]=1
       ENDELSE
     ENDIF ;end-of-two-scenes
     
     IF N_ELEMENTS(pos) GT 3 THEN BEGIN
       SAR_S1_OVERLAPPING_CHECK_ASSIGN_IDS_SKIPPED, ids_skipped, pos
       PRINT,lines[pos]
       PRINT,dyrs[pos],format='(10(1x,f20.8))'
       ;stop
       CONTINUE
       STOP
     ENDIF
     
   ENDFOR
   
   pos=WHERE(oks EQ 1)
   olines=lines[pos]
   
   OPENW,fid,ofile,/get_lun
   PRINTF,fid,olines,format='(1x,a)'
   IF ids_skipped[0] NE -1 THEN BEGIN ;;
     PRINTF,fid,lines[ids_skipped],format='("*",a)'
   ENDIF
   FREE_LUN,fid
 ;stop
 END