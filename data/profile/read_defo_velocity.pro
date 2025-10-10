PRO READ_DEFO_VELOCITY, file,   $
    data=data,  $
    sites=sites,  $
    lls=lls,  $
    vels=vels,  $
    fmt=inputfmt, $
    dummy=dummy
    
  ;input formats:
  ;inputfmt=3 xyz
  ; x y z
  ;inputfmt=4 xyze
  ; x y z z_sig
  ;nputfmt=81 psvelo
  ;lon lat Ve Vn Se Sn Cen Site
  ;nputfmt=82
  ;Site Long  Lat Vn  Sn  Ve  Se  Cne
  ;H095  102.232 27.8745 -10.8 1.3 10.5  1.3 0.0064
  ;nputfmt=83
  ;  Long.    Lat.      Ve       Vn     Se     Sn    Cne  Sta.      Source
  ; 74.336  39.842   0.170   13.790  0.560  0.503  0.000  I089  This_study
  ; 78.680  29.848  10.840   32.656  1.550  1.450 -0.002  LAN2  Kreemer et al. [2014] from Banerjee et al. [2008]
  ;nputfmt=83 psvelo+up
  ;lon lat Ve Vn Se Sn Cen Site Vu
  ;         76.22383        43.75322     -4.62    -17.39      0.00      0.00   0.00 7gis_SAR      0.55
  ;nputfmt=101
  ;* Site   Longitude  Latitude   Ve   dVe    Vn   dVn   Cen      Vu  dVu  (mm/yr)
  ;1375_GPS  105.8150   33.3400  -0.47 0.80  -1.66 0.70 -0.043   0.00 9.00
  ;inputfmt=102
  ;   long      lat       Ve      dVe       Vn      dVn       Vu      dVn    Tau_h   Tau_v
  ;   86.000   26.000  10.5821   0.9956  22.6099   0.8203   0.0000   7.4123  54.0000  54.0000
  ;inputfmt=111
  ;   long      lat       Ve      dVe       Vn      dVn       Vu      dVn     Cen      Ceu      Cnu
  ;   87.650   39.400  -9.3569   0.4007  -3.4889   0.4172   0.7089   3.8793  -0.0002   0.0328   0.0066
  ;inputfmt=112 ; CMM4 (Shen)
  ; site lat lon Ve dVe Vn dVn Cen n_epoch time_span epoch_avg [other]
  ;CHAF_GPS 34.3006 -119.3310 -29.13 0.36 28.23 0.32 0.020 5 8.6 1991.5 I034
  ;inputfmt=121
  ;*Station   Longitude   Latitude Ve_init Ve_incr    Ve     dVe   Vn_init Vn_incr    Vn     dVn   Cen
  ; 1375_GPS  105.8150   33.3400   0.000   0.000   7.300   0.800   0.000   0.000  -1.800   0.700  -0.0430
  ;inputfmt=121
  ;* Station Longitude  Latitude    Ve_init    Ve_incr         Ve        dVe    Vn_init    Vn_incr         Vn        dVn      Cen
  ; 1375_GPS  105.8150   33.3400      7.300      7.772     -0.472      0.800     -1.800     -0.142     -1.658      0.700  -0.0430
  ;inputfmt=131
  ;*   Long.       Lat.         E & N Rate      E & N Adj.      E & N +-   RHO        H Rate   H adj.    +-  SITE
  ;*  (deg)      (deg)           (mm/yr)       (mm/yr)       (mm/yr)                 (mm/yr)
  ;   92.78000   26.61800     1.46   15.57    1.46   15.57    0.20    0.30  0.031      0.00    0.00    3.00 TZPR_GPS 
  ;   90.68400   27.18300     1.41   12.84    1.41   12.84    0.90    0.80 -0.120     -0.00   -0.00    3.00 ZHEM_GPS 
     
  
  ;output data array (data)
  ;   0  1   2   3  4   5  6   7  8    9  10  11   12
  ;[ lon lat Ve dVe Vn dVn Vu dVu Cen Ceu Cnu Los dLos ]
    
  PROG=(STRSPLIT(LAST(SCOPE_TRACEBACK()),/EXTRACT))[0]
  
  
  IF N_PARAMS() LT 1 THEN BEGIN
    file=FILEPATH('velh.cmm4.psvelo',root_dir=!igps_root,subdirectory=['tables'])
    file=FILEPATH('wang_shen_2019JB018774_Table.S4S5.qocamap',root_dir=!igps_root,subdirectory=['tables'])
    file=FILEPATH('velh.cmm4',root_dir=!igps_root,subdirectory=['tables'])
    file=FILEPATH('velu.cmm4',root_dir=!igps_root,subdirectory=['tables'])
    file='D:\gsar\interseismic\100-a-m3-0114_0119_0124-karakul_lake\f123.1\sbas.3.0.0460.9999.20150925.20200326.116.0341.01.___\vel_mask_ll3.xyz'
    file='D:\gsar\gic3dv\kunlun\asc_des\gps_prd'
    file='D:\gsar\gic3dv\kunlun\asc_des\gic3dv.out'
    file='D:\gsar\gic3dv\tianshan\asc_des\insar_los_2_3d.psvelou'
    
    file='D:\tmp\FICORO_GNSS-main\FICORO_GNSS-main\results\rotation_steps\wang_shen_2020\ninh_wang_zhangling.vel'
    inputfmt=131
    
    file='\\10.4.134.30\root\g9n\gsar\gic3dv\saf\2019ea001036_dataverse_files\v_gps_h.dat'
    inputfmt=101
    
  ;inputfmt=121
  ENDIF
  
  IF N_ELEMENTS(vi) EQ 0 THEN vi=11 ;xyz is los data
  vei=vi+1
  
  
  IF N_ELEMENTS(inputfmt) EQ 0 || inputfmt EQ -1 THEN BEGIN
    ;determine file type from filename extension
    fname_ext=getfilesuffix(file)
    CASE fname_ext OF
      'xyz': BEGIN
        inputfmt=3
      END
      'xyze': BEGIN
        inputfmt=4
      END
      'psvelo': BEGIN
        inputfmt=81
      END
      'psvelou': BEGIN
        inputfmt=84
      END
      'gic': BEGIN
        ;* Site   Longitude  Latitude   Ve   dVe    Vn   dVn   Cen      Vu  dVu  (mm/yr)
        ;1375_GPS  105.8150   33.3400  -0.47 0.80  -1.66 0.70 -0.043   0.00 9.00
        inputfmt=101
      END
      'qmap': BEGIN
        ;*Station   Longitude   Latitude Ve_init Ve_incr    Ve     dVe   Vn_init Vn_incr    Vn     dVn   Cen
        ; 1375_GPS  105.8150   33.3400   0.000   0.000   7.300   0.800   0.000   0.000  -1.800   0.700  -0.0430
        inputfmt=121
      END
      'qocamap': BEGIN
        ;*Station   Longitude   Latitude Ve_init Ve_incr    Ve     dVe   Vn_init Vn_incr    Vn     dVn   Cen
        ; 1375_GPS  105.8150   33.3400   0.000   0.000   7.300   0.800   0.000   0.000  -1.800   0.700  -0.0430
        inputfmt=121
      END
      ELSE: BEGIN
        PRINT,'cannot determine file type from extension', fname_ext
      END
    ENDCASE
  ENDIF
  
  
  ;determine file type from filename
  IF N_ELEMENTS(inputfmt) EQ 0 || inputfmt EQ -1 THEN BEGIN
    ;determine file type from filename extension
    fname=GETFILENAME(file)
    CASE fname OF
      'gic3dv.out': BEGIN
        ;   long      lat       Ve      dVe       Vn      dVn       Vu      dVn     Cen      Ceu      Cnu
        ;   87.650   39.400  -9.3569   0.4007  -3.4889   0.4172   0.7089   3.8793  -0.0002   0.0328   0.0066
        inputfmt=111
      END
      'gps_prd': BEGIN
        ;   long      lat       Ve      dVe       Vn      dVn       Vu      dVn    Tau_h   Tau_v
        ;   86.000   26.000  10.5821   0.9956  22.6099   0.8203   0.0000   7.4123  54.0000  54.0000
        inputfmt=102
      END
      'resf': BEGIN
        ;* Station Longitude  Latitude    Ve_init    Ve_incr         Ve        dVe    Vn_init    Vn_incr         Vn        dVn      Cen
        ; 1375_GPS  105.8150   33.3400      7.300      7.772     -0.472      0.800     -1.800     -0.142     -1.658      0.700  -0.0430
        inputfmt=121
      END
      'velh.cmm4': BEGIN
        ; site lat lon Ve dVe Vn dVn Cen n_epoch time_span epoch_avg [other]
        ;CHAF_GPS 34.3006 -119.3310 -29.13 0.36 28.23 0.32 0.020 5 8.6 1991.5 I034
        inputfmt=112
      END
      'velu.cmm4': BEGIN
        ; site lat lon Vu dVu Ceu Cnu
        ;33JD_GPS  35.9429 -120.4647   2.16 1.52  0.037 -0.012    8 10.5 1996.1
        inputfmt=113
      END
      'xxx': BEGIN
        inputfmt=000
      END
      ELSE: BEGIN
        PRINT,'cannot determine file type from extension', fname_ext
      END
    ENDCASE
  ENDIF
  
  ;guess file type from content
  IF N_ELEMENTS(inputfmt) EQ 0 || inputfmt EQ -1 THEN BEGIN
    ;STOP
    lines=read_txt(file,N=5)
    lines_p=str_lines2arr(lines)
    ncol=N_ELEMENTS(lines_p[*,0])
    IF ncol LT 3 THEN BEGIN
      PRINT,'['+prog+']ERROR: less then 3 columns in velocity file ('+file+')!!'
      RETURN
    ENDIF
    IF ncol EQ 3 THEN BEGIN
      inputfmt=3 ;x y z
    ENDIF
    IF ncol EQ 4 THEN BEGIN
      inputfmt=4 ;x y z z_sig
    ENDIF
    IF ncol EQ 8 THEN BEGIN
      col1st=REFORM(lines_p[0,*])
      
      tmp1=DOUBLE(lines_p[0,4])
      IF !error_state NE 0 THEN BEGIN
        ;Site Long  Lat Vn  Sn  Ve  Se  Cne
        ;H095  102.232 27.8745 -10.8 1.3 10.5  1.3 0.0064
        inputfmt=81
      ENDIF ELSE BEGIN
        ;read psvelo velocity field
        ;lon lat Ve Vn Se Sn Cen Site
        inputfmt=82
      ENDELSE
    ENDIF
  ;STOP
    
    
  ENDIF
  
  PRINT,'inputfmt:',inputfmt
  ;output data array (data)
  ;   0  1   2   3  4   5  6   7  8    9  10  11   12
  ;[ lon lat Ve dVe Vn dVn Vu dVu Cen Ceu Cnu Los dLos ]
  CASE inputfmt OF
    3: BEGIN
      ; X          Y         Z
      ; 89.892319  30.813284  2.033756
      READ_COLS,file,DATA=lines_pd
      ;lines=read_txt(file)
      ;lines_pd=DOUBLE(str_lines2arr(lines))
      NSIT=N_ELEMENTS(lines_pd[0,*])
      SITES=STRING(LINDGEN(NSIT)+1,FORMAT='(I)')
      data=DBLARR(13, nsit)
      data[[2,4,6,11],*]=!values.d_nan
      data[0:1,*]=lines_pd[0:1,*]
      data[vi,*]=REFORM(lines_pd[2,*])
    END
    4: BEGIN
      ;X          Y         Z     Z_sig
      ;89.892319  30.813284  2.033756      2.838231
      READ_COLS,file,DATA=lines_pd
      NSIT=N_ELEMENTS(lines_pd[0,*])
      SITES=STRING(LINDGEN(NSIT)+1,FORMAT='(I)')
      data=DBLARR(13, nsit)
      data[[2,4,6,11],*]=!values.d_nan
      data[0:1,*]=lines_pd[0:1,*]
      data[[vi,vei],*]=lines_pd[2:3,*]
    END
    81: BEGIN  ;
      ;read psvelo velocity field
      ;lon lat Ve Vn Se Sn Cen Site
      READ_PSVELO, file,   $
        sites=sites,  $
        lls=lls,  $
        vels=vels,  $
        nsit=nsit
      data=DBLARR(13, nsit)
      data[[2,4,6,11],*]=!values.d_nan
      data[[0,1,2,3,4,5,8],*]=vels
    END
    82: BEGIN
      ;Site Long  Lat Vn  Sn  Ve  Se  Cne
      ;H095  102.232 27.8745 -10.8 1.3 10.5  1.3 0.0064
      READ_GNSS_VELH_SLLNE, file,   $
        sites=sites,  $
        lls=lls,  $
        vels=vels,  $
        nsit=nsit
      data=DBLARR(13, nsit)
      data[[2,4,6,11],*]=!values.d_nan
      data[[0,1,2,3,4,5,8],*]=vels
    END
    83: BEGIN  ;
      ;  Long.    Lat.      Ve       Vn     Se     Sn    Cne  Sta.      Source
      ; 74.336  39.842   0.170   13.790  0.560  0.503  0.000  I089  This_study
      ; 78.680  29.848  10.840   32.656  1.550  1.450 -0.002  LAN2  Kreemer et al. [2014] from Banerjee et al. [2008]
      READ_PSVELO_EXT, file,   $
        sites=sites,  $
        lls=lls,  $
        vels=vels,  $
        nsit=nsit
      data=DBLARR(13, nsit)
      data[[2,4,6,11],*]=!values.d_nan
      data[[0,1,2,3,4,5,8],*]=vels
    END 
  ;output data array (data)
  ;   0  1   2   3  4   5  6   7  8    9  10  11   12
  ;[ lon lat Ve dVe Vn dVn Vu dVu Cen Ceu Cnu Los dLos ]
    84: BEGIN  ;
      ;read psvelo+up velocity field
      ;lon lat Ve Vn Se Sn Cen Site Vu
      READ_cols_ascii, file,   $
        data=lines_p
      NSIT=N_ELEMENTS(lines_p[0,*])
        sites=reform(lines_p[7,*])        
      data=DBLARR(13, nsit)
      data[[2,4,6,11],*]=!values.d_nan
      data[[0,1,2,3,4,5,6,8],*]=double(lines_p[[0,1,2,4,3,5,8,6],*])
    END
    85: BEGIN  ;
      ;read psvelo+up velocity field
      ;lon lat Ve Vn Se Sn Cen Site Vu Su
      READ_cols_ascii, file,   $
        data=lines_p
      NSIT=N_ELEMENTS(lines_p[0,*])
        sites=reform(lines_p[7,*])        
      data=DBLARR(13, nsit)
      data[[2,4,6,11],*]=!values.d_nan
      data[[0,1,2,3,4,5,6,7,8],*]=double(lines_p[[0,1,2,4,3,5,8,9,6],*])
    END
    101: BEGIN
      ;* Site   Longitude  Latitude   Ve   dVe    Vn   dVn   Cen      Vu  dVu  (mm/yr)
      ;P544_CGP -119.7380   35.7313 -10.63 0.10  11.31 0.10  0.000  -4.70 0.20
      READ_COLS_ascii, file, data=lines_p, skip=1
      NSIT=N_ELEMENTS(lines_p[0,*])
        sites=reform(lines_p[0,*])    
      data=DBLARR(13, nsit)
      data[[2,4,6,11],*]=!values.d_nan
      data[[0,1,2,3,4,5,6,7,8],*]=DOUBLE(lines_p[[1,2,3,4,5,6,8,9,7],*])
    END
  ;output data array (data)
  ;   0  1   2   3  4   5  6   7  8    9  10  11   12
  ;[ lon lat Ve dVe Vn dVn Vu dVu Cen Ceu Cnu Los dLos ]
    102: BEGIN
      ;   long      lat       Ve      dVe       Vn      dVn       Vu      dVn    Tau_h   Tau_v
      ;   86.000   26.000  10.5821   0.9956  22.6099   0.8203   0.0000   7.4123  54.0000  54.0000
      READ_COLS, file, data=lines_pd, skip=1
      NSIT=N_ELEMENTS(lines_pd[0,*])
      SITES=STRING(LINDGEN(NSIT)+1,FORMAT='(I)')
      data=DBLARR(13, nsit)
      data[[2,4,6,11],*]=!values.d_nan
      data[[0,1,2,3,4,5,6,7],*]=DOUBLE(lines_pd[[0,1,2,3,4,5,6,7],*])
    END
    111: BEGIN
      ;   long      lat       Ve      dVe       Vn      dVn       Vu      dVn     Cen      Ceu      Cnu
      ;   87.650   39.400  -9.3569   0.4007  -3.4889   0.4172   0.7089   3.8793  -0.0002   0.0328   0.0066
      READ_COLS, file, data=lines_pd, skip=1
      NSIT=N_ELEMENTS(lines_pd[0,*])
      SITES=STRING(LINDGEN(NSIT)+1,FORMAT='(I)')
      data=DBLARR(13, nsit)
      data[[2,4,6,11],*]=!values.d_nan
      data[[0,1,2,3,4,5,6,7,8,9,10],*]=DOUBLE(lines_pd[[0,1,2,3,4,5,6,7,8,9,10],*])
    END
    112: BEGIN
      READ_GNSS_VELH_CMM4, file, $
        sites=sites,  $
        lls=lls,  $
        vels=vels,  $
        nsit=nsit
      data=DBLARR(13, nsit)
      data[[2,4,6,11],*]=!values.d_nan
      data[[0,1,2,3,4,5,8],*]=vels
    END
  ;output data array (data)
  ;   0  1   2   3  4   5  6   7  8    9  10  11   12
  ;[ lon lat Ve dVe Vn dVn Vu dVu Cen Ceu Cnu Los dLos ]
    113: BEGIN
      READ_GNSS_VELU_CMM4, file, $
        sites=sites,  $
        lls=lls,  $
        vels=vels,  $
        nsit=nsit
      data=DBLARR(13, nsit)
      data[[2,4,6,11],*]=!values.d_nan
      data[[0,1,6,7,9,10],*]=vels
    END
    121: BEGIN  ;
      ;read qoca map velocity field
      ;*Station   Longitude   Latitude Ve_init Ve_incr    Ve     dVe   Vn_init Vn_incr    Vn     dVn   Cen
      ; ARTU_GPS   58.5583   56.4278     0.0     0.0    24.9     0.0     0.0     0.0     6.1     0.0   0.0000
      READ_GNSS_VELH_QOCA_MAP, file,   $
        sites=sites,  $
        lls=lls,  $
        vels=vels,  $
        nsit=nsit
      data=DBLARR(13, nsit)
      data[[2,4,6,11],*]=!values.d_nan
      data[[0,1,2,3,4,5,8],*]=vels
    END
    131: BEGIN  ;
      ;read gamit/globk velocity field
      ;*   Long.       Lat.         E & N Rate      E & N Adj.      E & N +-   RHO        H Rate   H adj.    +-  SITE
      ;*  (deg)      (deg)           (mm/yr)       (mm/yr)       (mm/yr)                 (mm/yr)
      ;   92.78000   26.61800     1.46   15.57    1.46   15.57    0.20    0.30  0.031      0.00    0.00    3.00 TZPR_GPS 
        READ_GNSS_GAMIT_GLOBK_VEL, file,   $
        sites=sites,  $
        lls=lls,  $
        vels=vels,  $
        nsit=nsit
      data=DBLARR(13, nsit)
      data[[2,4,6,11],*]=!values.d_nan
      data[0:1,*]=lls
      data[2:8,*]=vels[0:6,*]
    END
  ;output data array (data)
  ;   0  1   2   3  4   5  6   7  8    9  10  11   12
  ;[ lon lat Ve dVe Vn dVn Vu dVu Cen Ceu Cnu Los dLos ]
    ELSE: BEGIN
      PRINT,'['+prog+']ERROR: invalid input velocity format!!'
      RETURN
    END
  ENDCASE
  
  IF N_PARAMS() LT 1 THEN BEGIN
    HELP,file,lls,vels,veles,nsit,sites, data
    PRINT,data[*,0:3],format='(13(1x,f10.5))'
    PRINT,sites[0:3]
  ENDIF
  
END