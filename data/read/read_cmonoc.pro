;;BJFS_GPS    4409209.3843   0.0025    9939262.1403   0.0033     87.4305   0.0065   -0.0333  -0.1349  -0.0606   1999.1630
;;BJFS_GPS    4409209.3834   0.0023    9939262.1422   0.0039     87.4358   0.0063   -0.0253  -0.1297  -0.0750   1999.1658
;;BJFS_GPS    4409209.3821   0.0023    9939262.1425   0.0034     87.4381   0.0069   -0.0301  -0.1045  -0.1028   1999.1685
;;BJFS_GPS    4409209.3848   0.0030    9939262.1417   0.0051     87.4413   0.0092   -0.0072  -0.1233  -0.0093   1999.1712
;;BJFS_GPS    4409209.3843   0.0025    9939262.1470   0.0033     87.4310   0.0066   -0.0488  -0.1063  -0.0775   1999.1740

;MODIFICATIONS:
;
;
;
;BUGS:
;  AUG-18-2008 TIAN
;    A BUG IN CONVERTED CMONOC TIME SERIES WAS FOUND.
;      2000.9959 2000 365  4409209.35640  9939262.19190       87.43110   0.00200   0.00300   0.00480
;    X 2000.9986 2000   1  4409209.35550  9939262.19120       87.43590   0.00200   0.00280   0.00490
;      2001.0014 2001   1  4409209.36010  9939262.19220       87.43160   0.00180   0.00260   0.00470
;

pro read_cmonoc, file, $
                  data = data
  ;;
  if n_params() lt 1 then begin
      file = 'BJFS.list'
      if file_test(file) ne 1 then file=dialog_pickfile()
      if file eq '' then return
  endif
  ;;
  ;;
  ;;num_rows=txt_lines(file, num_cols=num_cols)
  ;;help, num_rows, num_cols
  read_cols_ascii, file, data=dataStr, comm='#'

  ;;
  sz=size(dataStr,/dimensions)
  data=dblarr(7,sz[1])
  data[0,*]=double(dataStr[10,*])
  data[1,*]=double(dataStr[1,*])
  data[2,*]=double(dataStr[3,*])
  data[3,*]=double(dataStr[5,*])
  data[4,*]=double(dataStr[2,*])
  data[5,*]=double(dataStr[4,*])
  data[6,*]=double(dataStr[6,*])

  ;;help, data
end