;+
;calculate cmc time series for one file
;-
FUNCTION CMC_FILE,   $
    fi, $ ;file id to process
    files=files, $  ;file names, string, [nsit]
    dataa=dataa, $  ;input data matrix for files, double, [6, ndays, nsit]
    indsa=indsa, $  ;epoch index for each sites, integer, [ndays, nsit]
    sites=sites, $  ;site names, string, [nsit]
    llhs=llhs,  $   ;geographical coordinates (longitude, latitude, height) for sites, double, [3, nsit]
    corrs=corrs,  $ ;correlation matrix, double, [nsit, nsit, 3]
    blen=blen, $    ;baseline length in angular degrees, double, [nsit, nsit]
    ntau=ntau,nw=nw,taus=taus,ws=ws,  $ ;gridding parameters
    dmin=dmin, $    ;distance threshold (dmin) in angular degrees; to use sites far away to derive cmc, set dmin to a positive value (e.g. 3.5)
    nmin=nmin, $    ;minimum number of sites required to derive cmc corrections
    opath_cmc_raw=opath_cmc_raw,  $
    opath_flt_raw=opath_flt_raw,  $
    opath_flt_smoothed=opath_flt_smoothed,  $
    opath_cmc_smoothed=opath_cmc_smoothed,  $
    neustr=neustr,  $
    xrange=xrange, $  ;in decimal years
    yranges=yranges, $  ;in mm
    dates=dates, $
    fmtstr=fmtstr, $
    corr_file=corr_file, path=path, $
    sf=sf, $
    preview=preview, $
    title_neu=title_neu
    
  IF N_ELEMENTS(sf) EQ 0 THEN sf=1d3
  IF N_ELEMENTS(nmin) EQ 0 THEN nmin=3  ;
  IF N_ELEMENTS(preview) EQ 0 THEN preview=0
  IF N_ELEMENTS(debug_grid) EQ 0 THEN debug_grid=0
  
  ;tmp=dialog_message(strtrim(max(corrs),2))
  ;return,1
  ndays=N_ELEMENTS(dataa[0,*,0])
  file=files[fi]
  inds_fi=WHERE(indsa[*,fi] EQ 1)
  cmc_fi=DBLARR(3,ndays)
  nsits_fi=INTARR(3,ndays)
  
  site=STRUPCASE(STRMID(GETFILENAME(file),0,4))
  
  ;print, '[cmc_cal_optimal_ps]calculating cmc for '+site+' []...'
  ;stop
  
  header=''
  ind_s=WHERE(STRUPCASE(site) EQ STRUPCASE(sites))
  IF ind_s[0] EQ -1 THEN BEGIN
    ;print, '[cmc_cal_optimal_ps]no correlation relationship for '+site+'; skipped!'
    RETURN,0
  ENDIF
  cdi=ind_s[0]
  ;;
  
  ;get the geographic coordinates for current site
  sind=WHERE(STRUPCASE(sites) EQ STRUPCASE(site))
  llh_cur=REFORM(llhs[*,sind])
  
  sblen=REFORM(blen[cdi,*])
  scorr=REFORM(corrs[cdi,*,*])
  
  ;be sure to remove itself from calculation of weight
  pos=WHERE(sblen GT dmin)  ;do not use "ge" operator, because when dmin=0, we want to exclude the current site itself.
  sblen_=sblen[pos]
  scorr_=scorr[pos,*]
  ssites_=sites[pos]
  sllhs_=llhs[*,pos]
  
  d_ij=sblen_
  
  
  used_weights=REPLICATE(PTR_NEW(),3)
  used_llhs=REPLICATE(PTR_NEW(),3)
  used_sites=REPLICATE(PTR_NEW(),3)
  
  ;loop for each component
  FOR neui=0,2 DO BEGIN
  ;FOR neui=2,2 DO BEGIN
    ;stop
    c_ij=REFORM(scorr_[*,neui])
    resid_unf=REFORM(dataa[neui,inds_fi,fi])
    
    flt_rmss=DBLARR(ntau,nw)
    nsit_used=INTARR(ntau,nw)
    ;loop for tau
    ;print,'gridding...'
    FOR taui=0,ntau-1 DO BEGIN
      tau=taus[taui]
      ;loop for w
      ;print,'gridding... tau:',taui;,wi
      ;stop
      FOR wi=0,nw-1 DO BEGIN
        w=ws[wi]
        ;stop
        w_ij=c_ij*EXP(-1d0*(d_ij^2)/(tau^2))
        ;i1=where(abs(w_ij) ge 1d-6)
        ;help, i1
        ;if i1[0] ne -1 then stop
        tmpind=SORT(w_ij)
        w_ij_s=w_ij[tmpind]
        ;          stop
        FOR n=nmin-1,N_ELEMENTS(d_ij)-1 DO BEGIN
          sum_w_ij=TOTAL(w_ij_s[N_ELEMENTS(d_ij)-1-n:*])
          IF sum_w_ij GE w THEN BEGIN
            BREAK
          ENDIF
          IF n EQ N_ELEMENTS(d_ij)-1 THEN BEGIN
            GOTO,next_w
          ENDIF
        ENDFOR
        n=n-1
        
        ind=tmpind[N_ELEMENTS(d_ij)-1-n:*]
        ;stop
        csites=ssites_[ind]
        cblen=sblen_[ind]
        ccorr=scorr_[ind]
        c_w_ij=w_ij_s[N_ELEMENTS(d_ij)-1-n:*]
        ;get geographic coordinates for cmc sites
        llh_cmcs=sllhs_[*,ind]
        ssigma=REFORM(dataa[neui+3,inds_fi,pos[ind]])
        ts=REFORM(dataa[neui,inds_fi,pos[ind]])
        
        
        ;stop
        IF N_ELEMENTS(c_w_ij) GE nmin THEN BEGIN
          CMC_FORMULA,  $
            ts=ts,  $
            ssigma=ssigma, $
            weights=c_w_ij, $
            cmc=cmc,  $
            nsits=nsits
          ;
          flt_rmss[taui,wi]=rms(resid_unf-cmc)
          nsit_used[taui,wi]=N_ELEMENTS(c_w_ij)
        ENDIF ELSE BEGIN
          flt_rmss[taui,wi]=rms(resid_unf)
        ENDELSE
        
        
        
        IF preview && debug_grid THEN BEGIN
          ;stop
          oldwin=!d.WINDOW
          WINDOW,/free,/pixmap,xsize=1000,ysize=600
          DEVICE,decomposed=1
          win=!d.WINDOW
          !p.MULTI=[0,3,1]
          
          ofile=opath_cmc_raw+PATH_SEP()+desuffix(GETFILENAME(file))+'_map_W'+STRING(ws[wi],taus[taui],format='(i02,"_tau",i02)')+'.jpg'
          
          FOR neui_tw=0,2 DO BEGIN
            ;stop
            lon=REFORM(llh_cmcs[0,*])
            lat=REFORM(llh_cmcs[1,*])
            xx=llh_cur[0]
            yy=llh_cur[1]
            ;csites=*used_sites[neui_tw]
            nstn=N_ELEMENTS(csites)
            
            PLOT,lon,lat,background='ffffff'x,color='0'x,psym=2,charsize=1,$
              ;xrange=[80,130],yrange=[15,55],$
              ;xstyle=1,ystyle=1,$
              /ynozero,$
              xtitle='longitude',$
              ytitle='latitude',$
              title='map for '+site+' '+neustr[neui_tw]
            OPLOT,[xx,xx],[yy,yy],color='ff0000'x,psym=2,symsize=3
            wazi=(c_w_ij)*1d0 ;only north
            FOR i=0,nstn-1 DO XYOUTS,lon[i],lat[i],STRING(wazi[i],format='(f8.5)'),/data,color='0'x,font=1,size=1.5
            FOR i=0,nstn-1 DO XYOUTS,lon[i],lat[i]-.02,csites[i],/data,color='0'x,alignment=1,font=1,size=1.5
            XYOUTS,xx,yy-.02,site,/data,color='ff0000'x,alignment=1,font=1,size=2
            
          ;stop
            
          ENDFOR
          !p.MULTI=0
          WRITE_JPEG,ofile,TVRD(true=1),/true,quality=100
          WSET,oldwin
          WDELETE,win
          
        ENDIF
        
        ;stop
        next_w:
      ;;
      ENDFOR  ;end-of-loop-w
    ;
    ENDFOR  ;end-of-loop-tau
    ;;
    
    ;print,'gridding...done!!!'
    tmp=WHERE(flt_rmss EQ 0)
    IF tmp[0] NE -1 THEN BEGIN
      flt_rmss[tmp]=!values.D_NAN
    ;flt_rmss[tmp]=255
    ;flt_rmss[tmp]=max(flt_rmss)+.01
    ENDIF
    ;window,1,xsize=ntau*10,ysize=nw*10
    ;device,decomposed=0
    ;loadct,10
    ;tv,make_array(ntau*10,nw*10,/byte,value=250)
    ;tvscl,congrid(flt_rmss,ntau*10,nw*10),/nan
    ;device,decomposed=1
    ;
    ;if all NaN,
    ind_not_nan=where(finite(flt_rmss) eq 1)
    if ind_not_nan[0] eq -1 then begin   ;cannot derive CMC for all (tau, w) combinations.
      print,'[]ERROR: no CMC solution for component '+neustr[neui]
      return,0
    endif
    ;
    tmp=MIN(flt_rmss,indmin,/nan)
    taui= indmin MOD ntau
    wi= indmin / ntau
    ;plots,taui*10,wi*10,psym=2,color='00ffff'x
    ;xyouts,taui*10,wi*10,'psym=2',color='00ffff'x,/data
    ofile_tau_w=opath_cmc_raw+PATH_SEP()+desuffix(GETFILENAME(file))+'_tau_w_grid_'+neustr[neui]+'.jpg'
    ;      plot_tau_w_grid,ntau=ntau,nw=nw,img=flt_rmss,sf=10, $
    ;        ofile=ofile_tau_w,site=site,taui=[taui],wi=[wi]
    
    ;stop
    ;final solution (best; minimum residual rms)
    tau=taus[taui]
    w=ws[wi]
    ;print,'tau:',tau,'  w:',w
    ;printf,fido_param,tau,w,format='()'
    ;    printf,fido_param,site,neustr[neui],tau,w,format='(1x,a4,1x,a3,1x,i9,1x,i9)'
    ;    flush,fido_param
    w_ij=c_ij*EXP(-1d0*(d_ij^2)/(tau^2))
    tmpind=SORT(w_ij)
    w_ij_s=w_ij[tmpind]
    FOR n=nmin-1,N_ELEMENTS(d_ij)-1 DO BEGIN
      sum_w_ij=TOTAL(w_ij_s[N_ELEMENTS(d_ij)-1-n:*])
      IF sum_w_ij GE w THEN BEGIN
        BREAK
      ENDIF
    ENDFOR
    n=n-1
    
    ind=tmpind[N_ELEMENTS(d_ij)-1-n:*]
    ;stop
    csites=ssites_[ind]
    cblen=sblen_[ind]
    ccorr=scorr_[ind]
    c_w_ij=w_ij_s[N_ELEMENTS(d_ij)-1-n:*]
    ;get geographic coordinates for cmc sites
    llh_cmcs=sllhs_[*,ind]
    ;
    ;use only epochs for current site
    ;ssigma=reform(dataa[ind_err[neui],inds_fi,pos[ind]])
    ;ts=reform(dataa[ind_neu[neui],inds_fi,pos[ind]])
    ;use all epochs
    ssigma=REFORM(dataa[neui+3,*,pos[ind]])
    ts=REFORM(dataa[neui,*,pos[ind]])
    
    ;    resid_unf=reform(dataa[ind_neu[neui],inds_fi,fi])
    
    ;stop
    
;    pos_nozero=WHERE(ABS(c_w_ij) GE 1d-6)
;    ;print,c_w_ij
;    ;help, pos_nozero
;    IF N_ELEMENTS(pos_nozero) NE N_ELEMENTS(c_w_ij) THEN BEGIN
;      ts=ts[pos_nozero]
;      ssigma=ssgima[pos_nozero]
;      c_w_ij=c_w_ij[pos_nozero]
;    ENDIF
    
    CMC_FORMULA,  $
      ts=ts,  $
      ssigma=ssigma, $
      weights=c_w_ij, $
      cmc=cmc,  $
      nsits=nsits
      
    ;tmp=dialog_message(strjoin(strtrim(max(dataa),2),','))
    ;return,0
      
    ;    resid_flt=resid_unf-cmc[inds_fi]
      
    cmc_fi[neui,*]=cmc
    nsits_fi[neui,*]=nsits
    
    
    used_weights[neui]=PTR_NEW(c_w_ij)
    used_llhs[neui]=PTR_NEW(llh_cmcs)
    used_sites[neui]=PTR_NEW(csites)
    
    IF header[0] EQ '' THEN BEGIN
      header='#'+neustr[neui]+'[site-'+STRING(N_ELEMENTS(csites),format='(i4)')+']: '+STRJOIN(csites,',')
    ENDIF ELSE BEGIN
      header=[header, '#'+neustr[neui]+'[site-'+STRING(N_ELEMENTS(csites),format='(i4)')+']: '+STRJOIN(csites,',')]
    ENDELSE
    fmt='('+STRTRIM(N_ELEMENTS(csites))+'f5.2)'
    header=[header,'#'+neustr[neui]+'[corr-'+STRING(N_ELEMENTS(csites),format='(i4)')+']:'+STRING(ccorr,format=fmt)]
    fmt='('+STRTRIM(N_ELEMENTS(csites))+'f5.1)'
    header=[header,'#'+neustr[neui]+'[dist-'+STRING(N_ELEMENTS(csites),format='(i4)')+']:'+STRING(cblen,format=fmt)]
    fmt='('+STRTRIM(N_ELEMENTS(csites))+'f5.2)'
    header=[header,'#'+neustr[neui]+'[wegt-'+STRING(N_ELEMENTS(csites),format='(i4)')+']:'+STRING(c_w_ij,format=fmt)]
    header=[header,'#'+neustr[neui]+'[parm-    ] tau='+STRING(tau,format='(i5)')+' ; w='+STRING(w,format='(i5)') ]
    
    IF DEBUG_GRID EQ 1 THEN BEGIN
      FOR TAUI=0, NTAU-1 DO BEGIN
        FOR WI=0, NW-1 DO BEGIN
          HEADER=[HEADER,'#'+STRING(TAUS[TAUI],WS[WI],FLT_RMSS[TAUI,WI],NSIT_USED[TAUI,WI], $
            FORMAT='(I5,1X,I5,1X,E,1X,I5)') ]
        ENDFOR
      ENDFOR
    ENDIF
    
  ;stop
  ENDFOR  ;end-of-loop-component
  
  
  ;output cmc series
  ofile=opath_cmc_raw+PATH_SEP()+desuffix(GETFILENAME(file))+'cmc.neu'
  nsites=REFORM(nsits_fi[*,*])
  ind=WHERE(nsites[0,*] NE 0 OR nsites[1,*] NE 0 OR nsites[2,*] NE 0)
  IF ind[0] EQ -1 THEN BEGIN
    ;print,'[cmc_cal_optimal_ps]no cmc data derived.'
    ;tmp=dialog_message("output "+strtrim(fi,2)+' '+ofile)
    RETURN,0
  ENDIF
  ;cmc=cmc[*,ind]
  odata=DBLARR(9,N_ELEMENTS(cmc_fi[0,ind]))
  odata[0:2,*]=dates[0:2,ind]
  odata[3:5,*]=cmc_fi[*,ind]
  odata[6:8,*]=nsits_fi[*,ind]
  pos1=WHERE(odata[3,*] EQ 0) & IF pos1[0] NE - 1 THEN odata[3,pos1]=!values.D_NAN
  pos2=WHERE(odata[4,*] EQ 0) & IF pos2[0] NE - 1 THEN odata[4,pos2]=!values.D_NAN
  pos3=WHERE(odata[5,*] EQ 0) & IF pos3[0] NE - 1 THEN odata[5,pos3]=!values.D_NAN
  ;help,odata
  ;stop
  ;only save cmc series
  WRITE_SIO, ofile, data=odata, headers=[header,title_neu],user=user,prog=prog,$
    src=[path,corr_file], fmt=fmtstr, force_fmt=0
  IF preview THEN BEGIN
    NEU_SMOOTH, files=ofile, opath=opath_cmc_smoothed, xrange=xrange, yrange=yranges,sf=sf, verbose=0
  ENDIF
  ;print, yranges
  
  ;stop
  ;output filtered series
  ind2=WHERE(nsites[0,*] NE 0 AND nsites[1,*] NE 0 AND nsites[2,*] NE 0)
  ind=set_intersect(ind2, inds_fi)
  IF ind[0] EQ -1 THEN BEGIN
    ;print,'[cmc_cal_optimal_ps]no cmc data derived.'
    ;tmp=dialog_message("output "+strtrim(fi,2)+' '+ofile)
    RETURN,0
  ENDIF
  odata=DBLARR(9,N_ELEMENTS(cmc_fi[0,ind]))
  odata[0:2,*]=dates[0:2,ind]
  odata[3:5,*]=dataa[0:2,ind,fi]-cmc_fi[*,ind]
  odata[6:8,*]=dataa[3:5,ind,fi]
  ;  pos1=WHERE(odata[3,*] EQ 0) & IF pos1[0] NE - 1 THEN odata[3,pos1]=!values.D_NAN
  ;  pos2=WHERE(odata[4,*] EQ 0) & IF pos2[0] NE - 1 THEN odata[4,pos2]=!values.D_NAN
  ;  pos3=WHERE(odata[5,*] EQ 0) & IF pos3[0] NE - 1 THEN odata[5,pos3]=!values.D_NAN
  ;  IF pos1[0] NE - 1 THEN odata[3,pos1]=!values.D_NAN
  ;  IF pos2[0] NE - 1 THEN odata[4,pos2]=!values.D_NAN
  ;  IF pos3[0] NE - 1 THEN odata[5,pos3]=!values.D_NAN
  ;ind2=set_intersect(ind,inds_fi)
  ;odata=odata[ *, [inds_fi] ]
  ;ind=where(odata[3,*] ne 0 or odata[4,*] ne 0 or odata[5,*] ne 0)
  ;if ind[0] eq -1 then begin
  ;  print,'[cmc_cal_optimal_ps]no cmc data derived.'
  ;  return,0
  ;endif
  ;odata=odata[*, [ind] ]
  ofile=opath_flt_raw+PATH_SEP()+desuffix(GETFILENAME(file))+'flt.neu'
  WRITE_SIO, ofile, data=odata, headers=[header],user=user,prog=prog,$
    src=[path,corr_file]
  IF preview THEN BEGIN
    NEU_SMOOTH, files=ofile, opath=opath_flt_smoothed, xrange=xrange, yrange=yranges, sf=sf, verbose=0
  ENDIF
  
  IF preview THEN BEGIN
    ;stop
    oldwin=!d.WINDOW
    WINDOW,/free,/pixmap,xsize=1000,ysize=600
    DEVICE,decomposed=1
    win=!d.WINDOW
    !p.MULTI=[0,3,1]
    
    ofile=opath_cmc_raw+PATH_SEP()+desuffix(GETFILENAME(file))+'_map.jpg'
    
    FOR neui=0,2 DO BEGIN
      ;stop
      lon=REFORM((*used_llhs[neui])[0,*])
      lat=REFORM((*used_llhs[neui])[1,*])
      xx=llh_cur[0]
      yy=llh_cur[1]
      csites=*used_sites[neui]
      nstn=N_ELEMENTS(csites)
      
      PLOT,lon,lat,background='ffffff'x,color='0'x,psym=2,charsize=1,$
        ;xrange=[80,130],yrange=[15,55],$
        ;xstyle=1,ystyle=1,$
        /ynozero,$
        xtitle='longitude',$
        ytitle='latitude',$
        title='map for '+site+' '+neustr[neui]
      OPLOT,[xx,xx],[yy,yy],color='ff0000'x,psym=2,symsize=3
      wazi=(*used_weights[neui])*1d0 ;only north
      FOR i=0,nstn-1 DO XYOUTS,lon[i],lat[i],STRING(wazi[i],format='(f5.2)'),/data,color='0'x,font=1,size=1.5
      FOR i=0,nstn-1 DO XYOUTS,lon[i],lat[i]-.02,csites[i],/data,color='0'x,alignment=1,font=1,size=1.5
      XYOUTS,xx,yy-.02,site,/data,color='ff0000'x,alignment=1,font=1,size=2
      
    ;stop
      
    ENDFOR
    !p.MULTI=0
    WRITE_JPEG,ofile,TVRD(true=1),/true,quality=100
    WSET,oldwin
    WDELETE,win
    
  ENDIF
  
  FOR I=0,N_ELEMENTS(used_weights)-1 DO IF PTR_VALID(used_weights[I]) THEN PTR_FREE,used_weights[I]
  FOR I=0,N_ELEMENTS(used_llhs)-1 DO IF PTR_VALID(used_llhs[I]) THEN PTR_FREE,used_llhs[I]
  FOR I=0,N_ELEMENTS(used_sites)-1 DO IF PTR_VALID(used_sites[I]) THEN PTR_FREE,used_sites[I]
  
  
  RETURN,1
END