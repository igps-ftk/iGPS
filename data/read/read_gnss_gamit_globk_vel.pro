;
PRO READ_GNSS_GAMIT_GLOBK_VEL, file,   $
    sites=sites2,  $
    lls=lls,  $
    vels=vels,  $
    nsit=nsit
    
  PROG=(STRSPLIT(LAST(SCOPE_TRACEBACK()),/EXTRACT))[0]
  
  IF N_PARAMS() LT 1 THEN BEGIN
    file=FILEPATH('wang_shen_2019JB018774_Table.S4S5.qocamap',root_dir=!igps_root,subdirectory=['tables'])
    file='D:\tmp\FICORO_GNSS-main\FICORO_GNSS-main\results\rotation_steps\wang_shen_2020\ninh_wang_zhangling.vel'
    
  ENDIF
  
  ;read gamit/glock velocity field
  ;   0         1           2      3        4         5       6      7       8        9      10    11
  ;*   Long.       Lat.         E & N Rate      E & N Adj.      E & N +-   RHO        H Rate   H adj.    +-  SITE
  ;*  (deg)      (deg)           (mm/yr)       (mm/yr)       (mm/yr)                 (mm/yr)
  ;   92.78000   26.61800     1.46   15.57    1.46   15.57    0.20    0.30  0.031      0.00    0.00    3.00 TZPR_GPS
  ;stop
  lines=read_txt(file)
  pos=WHERE(strmids(lines,0,1) EQ ' ')
  lines1=lines[pos]
  lines1=str_lines2arr(lines1)
  ;sites=strmids(lines1[0,*],0,4)
  sites=REFORM(lines1[12,*])
  sites2=strmids(sites,0,8)
  lls=DOUBLE(lines1[0:1,*])
  
  ;output data array (data)
  ;   0  1   2   3  4   5  6   7  8    9  10  
  ;[  Ve dVe Vn dVn Vu dVu Cen Ceu Cnu Los dLos ]
  vels=dblarr(11,n_elements(sites))
  vels[[0,1,2,3,4,5,6],*]=DOUBLE(lines1[[2,6,3,7,9,11,8],*])
  ;stop
  nsit=N_ELEMENTS(sites)
  
  IF N_PARAMS() LT 1 THEN BEGIN
    HELP,file,sites,lls,vels,nsit
  ENDIF
END