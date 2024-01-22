;http://scec.ess.ucla.edu/~zshen/cmm4/cmm4.html
;Southern California Crustal Motion Map Version 4.0
;by Z.-K. Shen, R. W. King, D. C. Agnew, M. Wang, T. A. Herring, D. Dong, and P. Fang
;
;The format of these files is:
;Column 1: Site name and type. The names are 4-character codes; the type is either _GPS (continuous or survey-mode GPS), or _VLB (fixed or mobile VLBI); vertical velocities cannot be reliably estimated from the trilateration data. If a point was used by more than one technique, the name will appear more than once; for example, site PINY was observed both by mobile VLBI and by survey-mode GPS.
;Column 2: Site latitude.
;Column 3: Site longitude, positive for East.
;Column 4: Vertical velocity (in a local East-North-Up framework), in mm/yr, positive up. Because of possible systematic errors, the zero level should not be assumed to be zero velocity relative to the geoid, the ellipsoid, or sea level.
;Column 5: Standard error of the vertical velocity.
;Column 6: Correlation between the East and vertical velocities.
;Column 7: Correlation between the North and vertical velocities.
;Column 8: Number of epochs used in estimate. For most of the time covered, GPS data were combined into monthly files; after large earthquakes, a smaller temporal spacing was used. (See the file epochplot.pdf). The number of epochs is therefore not equivalent to the number of days of occupation, but should give a useful sense of how many data went into the final velocity estimate. For the EDM and VLBI data, the number of epochs corresponds more closely to the number of occupations.
;Column 9: The time span, in years, from the first to the last epoch.
;Column 10: The average of the epoch times.

PRO READ_GNSS_VELU_CMM4, file, $
    sites=sites,  $
    lls=lls,  $
    vels=vels,  $
    nsit=nsit
    
  PROG=(STRSPLIT(LAST(SCOPE_TRACEBACK()),/EXTRACT))[0]
  
  IF N_PARAMS() LT 1 THEN BEGIN
    file=FILEPATH('velu.cmm4',root_dir=!igps_root,subdirectory=['tables'])
  ENDIF
  
  lines=read_txt(file)
  lines1=lines[0:*]
  lines1=str_lines2arr(lines1)
  sites=REFORM(lines1[0,*])
  lls=DOUBLE(lines1[[2,1],*])
  vels=DOUBLE(lines1[[2,1,3,4,5,6],*])  ;stop
  nsit=N_ELEMENTS(sites)
  
  IF N_PARAMS() LT 1 THEN BEGIN
    HELP,file,sites,lls,vels,nsit,lines,lines1
  ENDIF
END