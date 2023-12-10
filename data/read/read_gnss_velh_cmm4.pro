;Southern California Crustal Motion Map Version 4.0
;by Z.-K. Shen, R. W. King, D. C. Agnew, M. Wang, T. A. Herring, D. Dong, and P. Fang
;
;The format of these files is:
;Column 1: Site name and type. The names are 4-character codes;
;  the type is either _GPS (continuous or survey-mode GPS), _EDM
;  (US Geological Survey trilateration) or _VLB (fixed or mobile VLBI).
;  If a point was used by more than one technique, the name will appear
;  more than once; for example, site PINY was observed both by mobile
;  VLBI and by survey-mode GPS, and REDH served first as a trilateration
;  point and than as a point for survey-mode GPS.
;Column 2: Site latitude.
;Column 3: Site longitude, positive for East.
;Column 4: East velocity (in a local East-North-Up framework), relative to the North American plate, in mm/yr.
;Column 5: Standard error of the East velocity.
;Column 6: North velocity (in a local East-North-Up framework), relative to the North American plate, in mm/yr.
;Column 7: Standard error of the North velocity.
;Column 8: Correlation between the East and North velocities.
;Column 9: Number of epochs used in estimate. For most of the time covered,
;  GPS data were combined into monthly files; after large earthquakes, a
;  smaller temporal spacing was used. (See the file epochplot.pdf). The number
;  of epochs is therefore not equivalent to the number of days of occupation,
;  but should give a useful sense of how many data went into the final velocity
;  estimate. For the EDM and VLBI data, the number of epochs corresponds more
;  closely to the number of occupations.
;Column 10: The time span, in years, from the first to the last epoch.
;Column 11: The average of the epoch times.
;
;After column 11, there may be additional entries to indicate ties between sites.
;These ties are assigned a letter I if the tie was "hard", forcing the velocities
;  at tied sites to be identical, or a letter s if the tie was "soft", allowing
;  some variation. For example,
;CHAF_GPS 34.3006 -119.3310 -29.13 0.36 28.23 0.32 0.020 5 8.6 1991.5 I034
;SOLI_GPS 34.2983 -119.3427 -29.13 0.36 28.24 0.32 0.020 12 16.2 1993.6 I034
;  means that these two nearby sites have been forced to have the same horizontal velocity, and
;REDH_EDM 35.6050 -120.2606 -22.16 0.47 26.04 0.40 0.096 56 19.0 1980.6 s055
;REDH_GPS 35.6050 -120.2606 -23.23 0.40 26.11 0.38 -0.008 15 11.5 1995.2 s055
;  that these two sites, which are the same mark observed with different methods
;  (largely used over different times) have been constrained to have similar
;  velocities. These ties have been used both to tie the VLBI and GPS reference
;  frames, and to set the rigid-body rotation for the trilateration networks.

PRO READ_GNSS_VELH_CMM4, file, $
    sites=sites,  $
    lls=lls,  $
    vels=vels,  $
    nsit=nsit
    
  PROG=(STRSPLIT(LAST(SCOPE_TRACEBACK()),/EXTRACT))[0]
  
  IF N_PARAMS() LT 1 THEN BEGIN
    file=FILEPATH('velh.cmm4',root_dir=!igps_root,subdirectory=['tables'])
  ENDIF
  
  lines=read_txt(file)
  lines1=lines[0:*]
  lines1=str_lines2arr(lines1)
  sites=REFORM(lines1[0,*])
  lls=DOUBLE(lines1[1:2,*])
  vels=DOUBLE(lines1[1:*,*])  ;stop
  nsit=N_ELEMENTS(sites)
  
  IF N_PARAMS() LT 1 THEN BEGIN
    HELP,file,sites,lls,vels,nsit
  ENDIF
END