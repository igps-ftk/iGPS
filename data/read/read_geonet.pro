pro read_geonet, filen, data=data
  if n_params() lt 1 then begin
    filen='H:\mirror_ftp\ftp.geonet.org.nz\gps\solutions\regional-filtered\AHTI_n.plt'
  endif
  
  read_cols,filen,skip=1,data=datan
  filee=strrep(filen,'_n','_e')
  read_cols,filee,skip=1,data=datae
  fileu=strrep(filen,'_n','_u')
  read_cols,fileu,skip=1,data=datau
  ;help,filee
  ;print,filee
  ;help,datan,datae,datau
  ndays=n_elements(datan[0,*])
  data=dblarr(6,ndays)
  ;The first column is epoch seconds - the number of seconds since January 1, 1970, 00:00:00 GMT.
  doy,1970,1,1,jd=jd
  jds=dblarr(ndays)
  ;stop
  for i=0,ndays-1 do begin
    jdi=jd+datan[0,i]/86400d0
    doy,jdi,dyear=dyr,day_of_year=doyr
    data[0:2,i]=[dyr,fix(dyr),doyr]
  endfor
  data[3,*]=datan[1,*]
  data[4,*]=datae[1,*]
  data[5,*]=datau[1,*]
  ;stop
  if n_params() lt 1 then begin
    help,data
  endif
  
end