PRO READ_taiwan_cor, FILE, DATA=DATA
  IF N_PARAMS() LT 1 THEN BEGIN
    FILE='D:\data\taiwan\TimeSeriesReleased1993.01.01_2017.11.30\8118.COR'
  ENDIF
  READ_COLS, file, data=data
END