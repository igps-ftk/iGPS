PRO SAR_LOS_PROFILE_FIT, pfile,   $
    theta_in=theta_in, $
    fts_in=fts_in, $
    distmax=distmax, $
    distmin=distmin,  $
    overwrite=is_overwrite, $
    dummy=dummy
    
  PROG=(STRSPLIT(LAST(SCOPE_TRACEBACK()),/EXTRACT))[0]
  
  IF N_PARAMS() LT 1 THEN BEGIN
    ;path='D:\gsar\asc\m_kangding1\asc_F2\SBAS6post\p'
    ;    ;    path='D:\gsar\asc\m_dayi1\p'
    ;    ;    path='D:\gsar\des\m_gyaringco2_yzs2\des_F1\SBAS6\p'
    ;    ;    path='D:\gsar\des\m_gulu1_dangxiong2\f123.1\SBAS7\p.g'
    ;    ;    pid=26
    ;    ;    ;pid=25
    ;    ;    theta_in=-1d0
    ;    ;;    path='D:\gsar\asc\m_dongqiao_dangxiong\f123.1\SBAS6\p.g'
    ;    ;;    pid=27
    ;    ;;    pid=26
    ;    ;;    theta_in=1.5d0
    ;    ;;    path='D:\ICD\projects\nsfc\2019\figure\rate.profile.gulu\des1\p'
    ;    ;
    ;    ;;
    ;    ;    path='D:\gsar\des\m_gulu1_dangxiong2\f123.1\SBAS7\p.b'
    ;    ;    pid=8
    ;    ;    theta_in=-.5d0
    ;    ;
    ;    ;  ;jiali2_nujiang4, across jiali fault
    ;    ;    path='D:\gsar\des\m_jiali2_nujiang4\f123.1\SBAS4\p.b'
    ;    ;    pid=14
    ;    ;    theta_in=-.5d0
    ;    ;
    ;    ;
    ;    path='D:\gsar\asc\m_altyntagh11\f123.1\SBAS3\p'
    ;    ;pid=16
    ;    theta_in=1
    ;    ;fts_in=-10
    ;    fts_in=10
    ;
    ;    ;    path='D:\gsar\des\m_altyntagh_\des_F3\SBAS5b\p'
    ;    ;    pid=11
    ;    ;    fts_in=5
    ;    ;
    ;    path='D:\gsar\des\m_kangding2\des_F2\SBAS5f\p'
    ;    theta_in=1.5
    ;    fts_in=-5
    ;
    ;    path='D:\gsar\asc\m_dongqiao_sewa2\f123.1\SBAS6\p'
    ;    theta_in=0
    ;    fts_in=10
    ;
    ;    path='D:\gsar\asc\m_woniuhu1\f123.1\SBAS7\p'
    ;    theta_in=1.5
    ;    fts_in=-85
    ;
    ;    path='D:\gsar\asc\m_woniuhu1\f123.1\SBAS7\p'
    ;    theta_in=.5
    ;    fts_in=-0
    ;    fid_in=10
    ;    distmax=80 ;maximum distance used, in km; specify a large value to use all points
    ;    distmin=-20
  
    ;    path='D:\gsar\des\m_eastkunlun2\f123.1\SBAS3\p'
    ;    path='D:\gsar\des\m_eastkunlun2M3\f123.1\SBAS5.long\p'
    ;    path='D:\gsar\des\m_eastkunlun2M3\f123.1\SBAS9.long\p'
    ;    ;theta_in=1
    ;    fts_in=0
    ;    ;fid_in=[41, 37, 29, 26]
    ;    ;fid_in=38
    ;    ;fid_in=29
    ;    distmax=150 ;maximum distance used, in km; specify a large value to use all points
    ;    distmin=-150
    ;
    ;    path='D:\gsar\des\p106-f471-f481-d-m_eastkunlun2M3\f123.1\SBAS14.long\p'
    ;    theta_in=0.5
    ;    fts_in=0
    ;    fid_in=[59]
    ;    distmax=1500 ;maximum distance used, in km; specify a large value to use all points
    ;    distmin=-200
  
    ;atf
    ;    path='\\192.168.11.68\tianyf\tmp\t41-f115-f125-a-m_altyntagh_M3\f123.1\SBAS1.b500-\p'
    ;    theta_in=1.4
    ;    fts_in=0
    ;    ;fid_in=[51]
    ;    distmax=200 ;maximum distance used, in km; specify a large value to use all points
    ;    distmin=-200
    ;    path='D:\gsar\interseismic\t92-d-m_altyntagh_M3\f123.1\sbas5.b350-.with_gacos_add\p.fa_atf'
    ;;    theta_in=1.4
    ;;    theta_in=0
    ;    ;fts_in=0
    ;    fid_in=[8]
    ;    distmax=80 ;maximum distance used, in km; specify a large value to use all points
    ;    distmin=-80
    ;  ;
    ;
    ;
    ;    path='D:\gsar\des\m_eastkunlun8M3\f123.1\SBAS2.long\p'
    ;    distmax=100 ;maximum distance used, in km; specify a large value to use all points
    ;    distmin=-180
    ;    fid_in=[23]
  
    ;    path='D:\gsar\des\t135-d-m_honghe4\f123.1\SBAS5.all\p'
    ;    path='D:\gsar\asc\t99-a-m_honghe5\f123.1\SBAS4.long\p'
    ;    distmax=1100 ;maximum distance used, in km; specify a large value to use all points
    ;    distmin=-1180
    ;    fid_in=[58]
  
    ;    path='D:\gsar\des\m_longriba2_kangding2\f123.1\SBAS3\p'
    ;    fts_in=0
    ;    fid_in=14
    ;    theta_in=1.
  
    ;
    ;    ;profile EF in figure 3
    ;    theta_in=0
    ;    fts_in=0
    ;    fid_in=3
    ;    distmax=120
    ;
    ;    path='D:\gsar\des\m_ganzi2\f123.1\SBAS3\p'
    ;    theta_in=1.75
    ;    fts_in=0
    ;    fid_in=10
  
    ;    ;kunlun fault
    ;    path='C:\gsar\des\m_eastkunlun6\f123.1\SBAS3\p'
    ;    theta_in=2
    ;    fts_in=-45
    ;    fid_in=0
    ;
    ;    ;
    ;    path='D:\gsar\des\m_eastkunlun4\f123.1\SBAS3\p'
    ;    theta_in=2.5
    ;    fts_in=-50
    ;    fid_in=23
    ;
    ;    path='D:\gsar\des\m_eastkunlun4_10\f123.1\SBAS5.long.nodetrend\p'
    ;    theta_in=0
    ;    fts_in=-70
    ;    fid_in=74
    ;    distmax=1500 ;maximum distance used, in km; specify a large value to use all points
    ;    distmin=-1500
    ;
    ;    path='D:\gsar\asc\m_gozhaco1\f123.1\SBAS4\p'
    ;    theta_in=3
    ;    fts_in=-5
    ;    fid_in=25
    ;
    ;    path='D:\gsar\asc\m_woniuhu3\f123.1\SBAS8.long\p'
    ;    theta_in=3
    ;    ;path='D:\gsar\des\m_woniuhu2b\f123.1\SBAS4.long\p'
    ;    ;theta_in=0
    ;    fts_in=-105
    ;    fid_in=32
  
    ;    path='D:\gsar\des\m_eastkunlun2M3\f123.1\SBAS5.long\p'
    ;    theta_in=1
    ;    fts_in=0
    ;    fid_in=40
  
    ;    path='D:\gsar\interseismic\t135-f463-f473-d-m3-haiyuan_gulang\f123\SBAS6\p.fa_haiyuan'
    ;    theta_in=1
    ;    fts_in=30
    ;    fid_in=57
    ;    distmin=-150
  
    ;    path='D:\gsar\interseismic\t150-f485-f495-d-m_dangxiong2_yzs3_gulu1\f123\SBAS4\p.fa_bengco'
    ;    theta_in=-.6
    ;    fts_in=0
    ;    fid_in=14
    ;    distmin=-150
  
    ;    path='D:\gsar\interseismic\150-d-m3-04862072_049116E8_049647FA-dangxiong2_yzs3_gulu1\f123\sbas.4.0.0700.9999.20141027.20200820.128.1753.01.x2\p.fa_bengco'
    ;    path='D:\gsar\interseismic\150-d-m3-04862072_049116E8_049647FA-dangxiong2_yzs3_gulu1\f123\sbas.4.0.0700.9999.20141027.20200820.128.1753.01.x2\p.fa_jiali'
    ;        theta_in=-0.7
    ;    fts_in=-80
    ;    fid_in=indgen(1)+61
    ;    distmin=-360
    ;    distmax=180
  
    ;    path='D:\gsar\interseismic\077-d-m3-04875F6E_04928DD6_0497869F-jiali2_nujiang4_cona1c\f123\SBAS6.x2\p.fa_jiali'
    ;         theta_in=-0.5
    ;    fts_in=-120
    ;    fid_in=indgen(1)+56
    ;    distmin=-360
    ;    distmax=180
  
    ;path='D:\gsar\interseismic\026-a-m2-0074_0079-honghe3M3\f123\sbas.4.0.0121.9999.20170313.20201222.112.1271.01.___\p.fa_xiaojiang_ext'
    ;         theta_in=2.0
    ;    fts_in=0
    ;    fid_in=77
    ;    distmin=-360
    ;    distmax=180
  
  
    ;    path='D:\gsar\interseismic\135-d-m3-0502_0507_0511-honghe\f123\sbas.3.0.0900.9999.20141026.20200527.078.0350.01.___\p.fa_puer'
    ;    ;theta_in=2.0
    ;    fts_in=-10
    ;    fid_in=10
    ;    distmin=-100
    ;    distmax=180
  
    ;    path='D:\gsar\interseismic\135-d-m3-0507_0511_0516-honghe4\f123\sbas.4.0.0001.9999.20141026.20201229.090.0294.01.___\p.fa_jingu20141007Ms6.6'
    ;    ;theta_in=2.0
    ;    fts_in=0
    ;    fid_in=5
    ;    distmin=-100
    ;    distmax=80
  
    ;    path='D:\gsar\interseismic\135-d-m3-0502_0507_0511-honghe\f123\sbas.3.0.0900.9999.20141026.20200527.078.0350.01.___\p.fa_redriver'
    ;    path='D:\gsar\interseismic\062-d-m3-0507_0512_0517-honghe2M3\f123\sbas.3.0.0500.9999.20141009.20200404.117.1013.01.32x4\p.fa_redriver'
    ;    ;theta_in=2.0
    ;    fts_in=100
    ;    fid_in=26
    ;    distmin=-1000
    ;    distmax=1400
  
    ;    path='D:\gsar\interseismic\062-d-m3-0507_0512_0517-honghe2M3\f123\sbas.3.0.0500.9999.20141009.20200404.117.1013.01.32x4\p.fa_xiaojiang_ext'
    ;        ;theta_in=2.0
    ;    fts_in=0
    ;    fid_in=57
    ;    distmin=-100
    ;    distmax=100
  
    ;    path='D:\gsar\interseismic\099-a-m3-1245_1250_1255-honghe\f123\sbas.4.0.0001.9999.20170318.20201227.112.0448.01.___\p.fa_xiaojiang_ext'
    ;    theta_in=1.0
    ;    fts_in=-50
    ;    fid_in=93
    ;    distmin=-1300
    ;    distmax=1003
  
    ;   path='D:\gsar\interseismic\143-a-m3-0089E456_00955439_0100C232-jiali_nujiang_yzs4\f123\sbas.4.0.0900.9999.20141015.20200808.139.1800.01.___\p.fa_jiali'
    ;         theta_in=0.3
    ;    fts_in=-120
    ;    fid_in=indgen(1)+56
    ;    distmin=-360
    ;    distmax=120
  
    ;    path='D:\gsar\interseismic\085-a-m3-0109_0114_0119-woniuhu1M3\f123\sbas.4.0.0367.9999.20141116.20210212.153.1011.01.___\p.fa_atf'
    ;    ;theta_in=0.0
    ;    fts_in=-0
    ;    fid_in=INDGEN(1)+1
    ;    distmin=-360
    ;    distmax=1200
  
    ;    path='\\10.4.134.30\root\g16n\gsar\136-d-m3-0471_0476_0481-aksaichin4M3\f123\sbas.4.0.0001.9999.20151115.20200305.091.0505.01.___\p.fa_jss'
    ;        ;theta_in=0.0
    ;    fts_in=-0
    ;    fid_in=INDGEN(1)+35
    ;    distmin=-300
    ;    distmax=1200
  
    ;    path='\\gpsac11\root\g11j\D\gsar\interseismic\062-d-m4-0507_0512_0517_0522-honghe2M3\f123\sbas.4.0.0367.9999.20141009.20210704.135.1428.01.___\p.fa_redriver'
    ;    path='D:\gsar\interseismic\062-d-m4-0507_0512_0517_0522-honghe2M3\f123\sbas.4.0.0367.9999.20141009.20210704.135.1428.01.___\p.fa_redriver'
    ;    theta_in=-0.0
    ;    fts_in=-20
    ;    fid_in=INDGEN(1)+27
    ;    distmin=-1200
    ;    distmax=80
  
    ;    path='D:\gsar\interseismic\041-a-m4-0109_0114_0119_0124-altyntagh_M3\f123\sbas.4.0.0367.9999.20141020.20210901.166.1299.01.___\p.fa_atf'
    ;    theta_in=1.0
    ;    fts_in=10
    ;    fid_in=INDGEN(1)+51
    ;    distmin=-120
    ;    distmax=200
  
    ;    path='D:\gsar\interseismic\026-a-m4-0117_0122_0127_0132-qilian\f123\sbas.4.0.0181.9999.20141019.20210208.140.0679.01.___\p.fa_haiyuan'
    ;    theta_in=0.0
    ;    fts_in=0
    ;    fid_in=INDGEN(1)+14
    ;    distmin=-1200
    ;    distmax=2000
  
    ;    path='D:\gsar\interseismic\026-a-m4-0058_0063_0068_0073-dianbianfu\f123\sbas.4.0.0367.9999.20170301.20210714.132.0727.01.___\p.fa_xiaojiang_ext'
    ;     theta_in=0.7
    ;    fts_in=-60
    ;    fid_in=INDGEN(1)+72
    ;    distmin=-1200
    ;    distmax=2000
  
  
    ;    path='D:\gsar\interseismic\135-d-m4-0472_0477_0482_0487-eastkunlun12M3\f123\sbas.4.0.0900.9999.20141026.20211013.114.0246.01.___\p.fa_eklf'
    ;         theta_in=0.5
    ;    fts_in=-0
    ;    fid_in=INDGEN(1)+24
    ;    distmin=-200
    ;    distmax=2000
  
    ;    path='D:\gsar\interseismic\004-d-m4-0466_0471_0476_0481-eastkunlun\f123\sbas.4.0.0367.9999.20150801.20210501.125.1391.01.___\p.fa_eklf'
    ;            theta_in=1
    ;    fts_in=-80
    ;    fid_in=INDGEN(1)+66
    ;    distmin=-270
    ;    distmax=2000
  
    ;    path='D:\gsar\interseismic\172-a-m4-0117_0122_0127_0132-alyntagh\f123\sbas.4.0.0001.9999.20150708.20210206.088.0072.01.___\p.fa_atf'
    ;    theta_in=1.1
    ;    fts_in=10
    ;    fid_in=INDGEN(1)+113
    ;    distmin=-1270
    ;    distmax=2000
  
    ;    path='D:\gsar\interseismic\019-d-m4-0456_0461_0466_0471-altyntagh_M3\f123\sbas.4.0.0001.9999.20141031.20210415.053.0739.01.___\p.fa_atf'
    ;        theta_in=0
    ;    fts_in=-0
    ;    fid_in=INDGEN(1)+41
    ;    distmin=-120
    ;    distmax=120
  
  
    ;    path='D:\gsar\interseismic\135-d-m3-0486_0491_0497-kangding2_honghe6\f123\sbas.4.0.0700.9999.20141026.20210416.103.0686.01.___\p.fa_xsh'
    ;    path='D:\gsar\interseismic\135-d-m3-0486_0491_0497-kangding2_honghe6\f123\sbas.4.0.0700.9999.20150106.20210416.101.0929.01.___\p.fa_xsh'
    ;    path='D:\gsar\interseismic\026-a-m4-0087_0092_0097_0102-kangding1M3\f123\sbas.4.0.0367.9999.20150123.20210527.157.1388.01.___\p.fa_xsh'
    ;    ;profile 86
    ;    theta_in=0.60
    ;    ;fts_in=-10
    ;    fid_in=INDGEN(1)+86
    ;    distmin=-1200
    ;    distmax=800
    ;    ;profile 96
    ;    theta_in=1.0
    ;    fts_in=-10
    ;    fid_in=INDGEN(1)+96
    ;    distmin=-120
    ;    distmax=800
    ;    ;profile 96 (T26)
    ;    theta_in=-1.0
    ;    fts_in=-10
    ;    fid_in=INDGEN(1)+96
    ;    distmin=-150
    ;    distmax=80
    ;    ;;profile 86 (T26)
    ;    theta_in=-0.60
    ;    ;fts_in=-10
    ;    fid_in=INDGEN(1)+86
    ;    distmin=-1200
    ;    distmax=800
  
    ;    path='D:\gsar\interseismic\026-a-m4-0117_0122_0127_0132-qilian\f123\sbas.4.0.0367.9999.20141019.20210208.140.1329.01.___\p.fa_haiyuan'
    ;            theta_in=0
    ;    fts_in=-0
    ;    fid_in=INDGEN(1)+15
    ;    distmin=-1200
    ;    distmax=120
  
    ;    path='\\10.4.134.30\root\g15e\gsar\172-a-m4-0117_0122_0127_0132-alyntagh\f123\sbas.4.0.0001.9999.20150708.20210407.137.1008.01.___\p.fa_atf'
    ;    theta_in=1.0
    ;    fts_in=5
    ;    fid_in=INDGEN(1)+108
    ;    distmin=-1200
    ;    distmax=120
  
    ;    path='D:\gsar\interseismic\100-a-m3-0119_0124_0129-karakul_lake_north\f123\sbas.4.0.0367.9999.20150925.20200513.120.0592.01.x2\p.fa_main_pamir_thrust'
    ;        theta_in=0.0
    ;    fts_in=5
    ;    fid_in=INDGEN(1)+28
    ;    distmin=-120
    ;    distmax=80
  
    ;    path='D:\gsar\interseismic\137-a-m3-0109_0114_0119-saf\f123\sbas.4.0.0001.9999.20150401.20210216.097.0505.01.___\p.fa_saf1'
    ;            theta_in=1.9
    ;    fts_in=0
    ;    fid_in=INDGEN(1)+74
    ;    distmin=-1200
    ;    distmax=800
  
    ;    path='\\192.168.11.68\root\g8n\gsar\gic3dv\saf\2019ea001036_dataverse_files.a120\sbas.4.0.0001.9999.20150327.20190704.068.0071.01.___\p.fa_saf1'
    ;                theta_in=0.5
    ;    fts_in=0
    ;    fid_in=INDGEN(1)+84
    ;    distmin=-1200
    ;    distmax=150
  
    ;    path='D:\gsar\interseismic\137-a-m3-0109_0114_0119-saf\f123\sbas.4.0.0001.9999.20150401.20210216.097.0505.01.___\p.fa_saf1'
    ;    theta_in=1
    ;    fts_in=0
    ;    fid_in=INDGEN(1)+71
    ;    distmin=-1200
    ;    distmax=1500
  
    ;    path='D:\gsar\interseismic\128-a-m3-0115_0120_0125-haiyuan\f123\sbas.4.9.0367.9999.20141014.20201006.136.1451.01.___\p.fa_haiyuan'
    ;        theta_in=-.5
    ;    fts_in=0
    ;    fid_in=INDGEN(1)+36
    ;    distmin=-1200
    ;    distmax=100
  
    ;    path='D:\gsar\interseismic\033-d-m4-0458_0463_0468_0473-qinghai_lake_haiyuan\f123\sbas.4.0.0367.9999.20141031.20210421.147.1280.01.___\p.fa_haiyuan'
    ;            theta_in=0.5
    ;    fts_in=0
    ;    fid_in=INDGEN(1)+36
    ;    distmin=-90
    ;    distmax=150
  
    ;path='\\10.4.35.86\root\g7d\gsar\070-a-m4-0115_0120_0125_0130-altyntagh\f123\sbas.4.0.0001.9999.20141022.20210424.147.0331.01.___\p.fa_atf_ext'
    ;            theta_in=1.
    ;    fts_in=0
    ;    fid_in=INDGEN(1)+101
    ;    distmin=-900
    ;    distmax=150
  
    ;path='D:\gsar\interseismic\150-d-m4-0482_0487_0492_0497-dangxiong2_yzs3_gulu1\f123\sbas.4.0.0367.9999.20141027.20210417.148.1399.01.___\p.fa_jiali'
    ;                theta_in=-.5
    ;    fts_in=-80
    ;    fid_in=INDGEN(1)+62
    ;    distmin=-900
    ;    distmax=150
  
    ;    path='D:\gsar\interseismic\114-a-m4-0110_0115_0120_0125-altyntagh2\f123\sbas.4.0.0367.9999.20141106.20210403.146.1300.01.___\p.fa_atf_ext'
    ;    theta_in=0.25
    ;    fts_in=5
    ;    fid_in=INDGEN(1)+49
    ;    distmin=-100
    ;    distmax=250
  
;    path='D:\gsar\interseismic\085-a-m4-0104_0109_0114_0119-woniuhu1M3\f123\sbas.4.0.0367.9999.20141116.20220303.182.1300.01.___\p.fa_atf_ext'
;    theta_in=1
;    fts_in=5
;    fid_in=INDGEN(1)+1
;    distmin=-100
;    distmax=250
    
    path='D:\gsar\interseismic\026-a-m4-0087_0092_0097_0102-kangding1M3\f123\sbas.4.0.0367.9999.20150123.20210527.157.1388.01.___\p.fa_xsh_b'
        ;theta_in=1
    fts_in=0
    fid_in=INDGEN(1)+14
    distmin=-80
    distmax=80
    
    
    pfiles=FILE_SEARCH(path+PATH_SEP()+'profile_*_vel.psxy', count=nf)
    ;pfiles=FILE_SEARCH(path+PATH_SEP()+'*rot.txt', count=nf)
    
    fnames=GETFILENAME(pfiles)
    tmp=strsplits(fnames,'_',/extract)
    ids=FIX(REFORM(tmp[1,*]))
    
    ;stop
    fids=ids
    IF N_ELEMENTS(fid_in) NE 0 THEN BEGIN
      fids=fid_in
    ENDIF
    ;    fid1=first(ids)
    ;    fid2=last(ids)
    ;;    IF N_ELEMENTS(fid_in) EQ 1 THEN BEGIN
    ;      fid1=fid_in
    ;      fid2=fid_in
    ;    ENDIF
    ;    IF N_ELEMENTS(fid_in) GT 1 THEN BEGIN
    ;      fid1=first(fid_in)
    ;      fid2=last(fid_in)
    ;    ENDIF
    ;
    ;
    ;    ;FOR fi=22, nf-1 DO BEGIN
    ;    ;FOR fi=15,15 DO BEGIN
    ;FOR fid=fid1,fid2 DO BEGIN
    FOR i=0,N_ELEMENTS(fids)-1 DO BEGIN
      ;FOR fi=pid,pid DO BEGIN
      fi=WHERE(ids EQ fids[i])
      pfile=pfiles[fi]
      PRINT,pfile
      SAR_LOS_PROFILE_FIT, pfile, theta_in=theta_in, fts_in=fts_in, distmax=distmax, distmin=distmin,overwrite=1
    ;RETURN
    ENDFOR
    RETURN
  ;
  ENDIF
  
  IF N_ELEMENTS(is_overwrite) EQ 0 THEN is_overwrite=0
  
  ;output rotated profile
  ofile_rot=desuffix(pfile)+'_rot.txt'
  ofile_rot2=desuffix(pfile)+'_rot2.txt'
  ;output modeled profile
  ofile=desuffix(pfile)+'_mdl.txt'
  ofile=desuffix(pfile)+'_mdl_los.txt'
  ofile_mdl_raw=desuffix(pfile)+'_mdl_raw.txt'
  ;output residual (rot2 - mdl)
  ofile_resid=desuffix(pfile)+'_resid_los.txt'
  
  ;stop
  IF FILE_TEST(ofile,/regular) EQ 1 && is_overwrite NE 1 THEN BEGIN
    PRINT,'['+prog+']WARNING: output file already exist ('+pfile+')! Skipped.'
    RETURN
  ENDIF
  ;
  ;distance thresholds
  IF N_ELEMENTS(distmax) EQ 0 THEN distmax=5000 ;maximum distance used, in km; specify a large value to use all points
  IF N_ELEMENTS(distmin) EQ 0 THEN distmin=-2200 ;minimum distance used, in km
  ;distmin=-90
  ;stop
  
  IF N_ELEMENTS(sig_max) EQ 0 THEN sig_max=1.6 ;
  ;
  ;GRID SEARCH PARAMETERS
  ;A) ROTATION OF AXIS
  theta_max=2  ;degree
  theta_step=.25  ;degree
  ;stop
  thetas=(FINDGEN((theta_max*2/theta_step)+1)*theta_step-theta_max)
  ;thetas=thetas*2
  ;thetas=-1
  ;thetas=1.5
  ;thetas=0
  IF N_ELEMENTS(theta_in) NE 0 THEN thetas=theta_in
  ;
  ntheta=N_ELEMENTS(thetas)
  PRINT,'rotation of axis (degree):', thetas
  thetas=thetas*!dpi/(-180d0)
  ;STOP
  ;
  ;A) FAR-FIELD INTERSEISMIC FAULT SLIP RATES
  nfs=121
  fss=INDGEN(400)/10d0-20 ;fault-slip-s, [-20,20] with step of 0.1 mm/a
  fss=1d0*(INDGEN(nfs)/1d0-nfs/2)
  ;fss=1*(INDGEN(21)/1d0-10)
  ;  fss=fss*3
  ;fss=fss*.1d0
  fss=fss*.2d0
  ;fss=(indgen(40)-90)*.1
  ;fss=4d0
  nfs=N_ELEMENTS(fss)
  PRINT,'far-field slips:',fss
  ;stop
  
  ;B) ANOTHER TYPE OF GRID SEARCH TO CORRECT THE POSSIBLE SHIFT OF FAULT TRACE
  nfts=21
  nfts=9
  ;nfts=3
  sf_fts=1
  ftss=INDGEN(nfts)*sf_fts-(nfts/ 2)*sf_fts
  ftss=ftss*5
  ;ftss=ftss*10
  ;ftss=ftss+5
  ;ftss=-60
  ;ftss=-4d0
  ;ftss=0
  IF N_ELEMENTS(fts_in) NE 0 THEN ftss=fts_in
  nfts=N_ELEMENTS(ftss)
  PRINT,'fault trace shifts:',ftss
  ;stop
  ;
  ;C) MEANS (demean the velocity profile)
  ;the xm (mean of x) ranges between the means of profile segment on the two sides of fault trace (xm1, xm2)
  nxm=17
  ;(see below)
  ;
  ;D) LOCKING DEPTH
  lds=INDGEN(25)*2+1d0  ;locking-depth-s, [1, 50] with step of 1 km
  ;lds=INDGEN(30)*2+1d0
  ;  lds=INDGEN(10)*5+1d0
  ;lds=INDGEN(10)*2+.001d0
  ;lds=[0.001,0.002]
  ;lds=lds*.2
  ;lds=[6d0,6.001d0]
  ;not working !!;  lds=6
  ;lds=[10,20]
  nld=N_ELEMENTS(lds)
  PRINT,'locking depths:',lds
  ;STOP
  ;
  
  ;Read velocity profile
  lines=read_txt(pfile)
  pos=WHERE(strmids(lines,0,1) NE ' ')
  IF pos[0] NE -1 THEN BEGIN
    header_lines=lines[pos]
  ENDIF ELSE BEGIN
    header_lines=''
  ENDELSE
  ;stop
  pxy3=DOUBLE((STRSPLIT(grepi(lines,'PSXY_FAULT_PROFILE_INTERSECT'), /extract))[2:3])
  tmp=grepi(lines,'PSXY_PROFILE')
  tmp=DOUBLE((str_lines2arr(tmp))[2:3,*])
  pxy1=REFORM(tmp[*,0])
  pxy2=REFORM(tmp[*,1])
  ;stop
  ;
  lines2=grepi(lines,'^ ')
  lines3=str_lines2arr(lines2)
  
  dists=DOUBLE(REFORM(lines3[10,*]))  ;distances to fault trace
  p_dists=DOUBLE(REFORM(lines3[3,*]))  ;distances to profile
  
  ;  ;use insar los
  vels_los=DOUBLE(REFORM(lines3[11,*]))
  veles_los=DOUBLE(REFORM(lines3[12,*]))
  ;stop
  ;gps strike-slip rate
  ;  vels_los=DOUBLE(REFORM(lines3[6,*]))
  ;  veles_los=DOUBLE(REFORM(lines3[7,*]))
  
  IF TOTAL(veles_los) GT 0 THEN BEGIN ;screen out worse rate estimates
    pos=WHERE(dists GE distmin AND dists LE distmax AND veles_los LT sig_max)
  ENDIF ELSE BEGIN  ;no check for rate uncertainity
    pos1=WHERE(dists LT -40)
    pos2=WHERE(dists GT 10)
    IF pos1[0] NE -1 AND pos2[0] NE -1 THEN BEGIN
      PRINT,MEAN(vels_los[pos1])-MEAN(vels_los[pos2]),'mm/a'
    ENDIF
    ;stop
    pos=WHERE(dists GE distmin AND dists LE distmax AND vels_los GT -131d0)
    
  ;pos=WHERE(dists lt distmin or (dists gE 10 and dists le 400) and  vels_los gt -1000.51d0)
    
  ENDELSE
  ;pos=WHERE((dists GE 0 AND dists LE distmax) or (dists le -30))
  IF N_ELEMENTS(pos) LE 3 THEN BEGIN
    PRINT,'['+prog+']WARNING: not enough number of data!'
    RETURN
  ENDIF
  
  ;"station" locations (longitude, latitude)
  lls=DOUBLE(lines3[1:2,pos])
  lls_site=DOUBLE(lines3[8:9,pos])
  ;distances
  d=dists[pos]
  
  ;PRINT,';for strike-slip component'
  x0_raw=vels_los[pos]
  
  ;x0_raw=smooth(x0_raw,25)
  
  
  WINDOW,0,xsize=1600,ysize=699,/pixmap
  !p.MULTI=[0,2,3]
  ;PLOT,d,x0_raw,background='ffffff'x,color='0'x,psym=1,/nodata, $
  PLOT,dists,vels_los,background='ffffff'x,color='0'x,psym=1,/nodata, $
    title='Raw Profile',  $
    xtitle='Distance from Given Fault Trace (km)',  $
    ytitle='Far-field LOS Velocity (mm/yr)';,/iso
  OPLOT,d,x0_raw,color='ffaaee'x,psym=1
  OPLOT,[-1d3,1d3],[0,0],linestyle=2,color='0'x,thick=2
  OPLOT,[0,0],[-1d3,1d3],linestyle=2,color='0'x,thick=2
  ;ERRPLOT,d,x0_raw-xe,x0_raw+xe,color='aaaa00'x
  ;HELP, lines2
  ;stop
  ;
  ;tmp=LINFIT(d,x0_raw,/double)
  ;x0=x0_raw-tmp[0]-tmp[1]*d
  x0=x0_raw
  ;oPLOT,d,x0+mean(x0_raw),color='aa9922'x,psym=1
  ;STOP
  ind1=WHERE(d-MEAN(ftss) LT 0)
  ind2=WHERE(d-MEAN(ftss) GT 0)
  IF ind1[0] EQ -1 OR ind2[0] EQ -1 THEN RETURN
  
  xe=veles_los[pos]
  pd=p_dists[pos]
  
  ;stop
  
  rchi2s=DBLARR(nfts,nxm,ntheta,nfs,nld)
  rchi2s[*]=9999
  xms_all=DBLARR(nxm,ntheta)
  ;stop
  FOR thetai=0,ntheta-1 DO BEGIN ;loop for rotation of axis
    theta=thetas[thetai]
    ;theta=0
    ;theta=-2*!dpi/180
    ;x=replicate(0d0,n_elements(d))
    d1=d*COS(theta)-x0*SIN(theta)
    x1=d*SIN(theta)+x0*COS(theta)
    
    ;    ;                    PLOT,d,x0,background='ffffff'x,color='0'x,psym=1,/nodata,/iso
    ;    ;                    OPLOT,d,x0,color='ffaaee'x,psym=1
    ;    OPLOT,d1,x1,color='0000ff'x,psym=4
    ;    ;                jfile=desuffix(ofile)+STRING(theta*180/!dpi,format='("-",f06.2)')+'.jpg'
    ;    ;                WRITE_JPEG,jfile,TVRD(true=1),true=1,quality=100
    ;    ;                x=d
    ;    ;                y=replicate(0d0,n_elements(d))
    ;    ;                x1=x*COS(theta)-y*SIN(theta)
    ;    ;                y1=x*SIN(theta)+y*COS(theta)
    ;    ;                OPLOT,x,y,color='ffaaee'x,psym=1
    ;    ;                OPLOT,x1,y1,color='0000ff'x,psym=4
    ;
    ;STOP
    xm1=MAX(x1,min=xm2)
    xms=INDGEN(nxm)*ABS(xm2-xm1)/nxm+MIN([xm1,xm2])
    ;xms=MEAN(x1)
    ;nxm=1
    xms_all[*,thetai]=xms
    PRINT,'velocities means for rotation angle '+STRTRIM(theta*180/!dpi,2)+':',xms
    ;stop
    
    FOR ftsi=0, nfts-1 DO BEGIN ;loop for fault trace shift
      fts=ftss[ftsi]
      
      FOR xmi=0, nxm-1 DO BEGIN ;loop for mean of velocity profile
      
        xm=xms[xmi]
        x=x1-xm
        
        FOR fsi=0, nfs-1 DO BEGIN ;loop for far-field slip rate
          fs=fss[fsi]
          FOR ldi=0, nld-1 DO BEGIN ;loop for locking depth
            ld=lds[ldi]
            xp=(fs/!dpi)*ATAN((d1-fts)/ld) ;v(y)=Vmax/pi*atan(y/D)
            ;            OPLOT,d,xp+xm, color='00ffff'x
            ;            ;chi2=TOTAL( ((x-xp)^2/xp) )
            ;            ;chi2=TOTAL( (x-xp)^2/(stddev(xp))^2 )
            ;            ;chi2=TOTAL( (x-xp)^2/xe^2 )
            ;            ;rchi2=chi2/(N_ELEMENTS(x)-2-1)
            rchi2=SQRT(TOTAL((x-xp)^2)/(N_ELEMENTS(x)))
            rchi2s[ftsi,xmi,thetai,fsi,ldi]=rchi2
          ;PRINT,rchi2,fs,ld
          ;stop
          ENDFOR
        ENDFOR
      ENDFOR  ;end-of-loop-axis-rotation
      PRINT,pfile
    ;STOP
    ENDFOR
  ENDFOR
  
  tmp=MIN(ABS(rchi2s-0),ind)
  PRINT,'minimum rchi2:', tmp
  ind2=ARRAY_INDICES(rchi2s,ind)
  ;STOP
  ;coli=(ind MOD nfs)
  ;rowi=ind/nfs
  ;PRINT,rchi2s[ind2[0],ind2[1],ind2[2],ind2[3]],tmp
  ;;d2=[-1d0*INDGEN(ABS(distmin)),INDGEN(distmax)+1]
  d2=d1
  ;d2=FINDGEN(1000)-500d0
  d2=d2[SORT(d2)]
  d2=d2[UNIQ(d2)]
  x2=xms_all[ind2[1],ind2[2]]+(fss[ind2[3]]/!dpi)*ATAN((d2-ftss[ind2[0]])/lds[ind2[4]])
  
  theta=thetas[ind2[2]]
  d1=d*COS(theta)-x0*SIN(theta)
  x1=d*SIN(theta)+x0*COS(theta)
  
  ;vels_los2=vels_los-xm
  ;
  d_all=dists*COS(theta)-vels_los*SIN(theta)
  x_all=dists*SIN(theta)+vels_los*COS(theta)
  
  fts_final=ftss[ind2[0]]
  GEO_LINE_XY_FAR_DIST, pxy1, pxy2, pxy3,fts_final,oxy=poxy
  ;  OPLOT,[poxy[0]],[poxy[1]],color='0000ff'x,psym=5
  ;stop
  
  ;rotate the fitted curve to the raw data coordinate system
  ;OPLOT,d2,x2,color='0000ff'x,psym=1
  out_fts=ftss[ind2[0]]
  ;  'de-mean of velocity:'
  out_xm=xms_all[ind2[1],ind2[2]]
  ;  'angle of axis rotation (deg):'
  out_theta=thetas[ind2[2]]
  
  d3=d2;-out_fts
  x3=x2;-out_xm
  ;first, rotate back
  d3_rot=d3*COS(-1*out_theta)-x3*SIN(-1*out_theta)
  x3_rot=d3*SIN(-1*out_theta)+x3*COS(-1*out_theta)
  ;second, add back the distance shift (fault-trace shift)
  d3_rot_fts=d3_rot;+out_fts
  ;third, add back meman of velocities
  x3_rot_xm=x3_rot;+out_xm
  OPLOT,d3_rot_fts,x3_rot_xm,color='0000ff'x,psym=0
  ;
  ;rotated y-axis
  a1=[0,0]+out_fts
  b1=[-3,3]+out_xm
  a2=a1*COS(-1*out_theta)-b1*SIN(-1*out_theta)
  b2=a1*SIN(-1*out_theta)+b1*COS(-1*out_theta)
  d3_y_axis_x=a2;+out_fts
  d3_y_axis_y=b2;+out_xm
  OPLOT,d3_y_axis_x,d3_y_axis_y,color='0'x,linestyle=1,thick=1
  ;rotated x-axis
  a1=[-100,100]+out_fts
  b1=[0,0]+out_xm
  a2=a1*COS(-1*out_theta)-b1*SIN(-1*out_theta)
  b2=a1*SIN(-1*out_theta)+b1*COS(-1*out_theta)
  d3_x_axis_x=a2;+out_fts
  d3_x_axis_y=b2;+out_xm
  OPLOT,d3_x_axis_x,d3_x_axis_y,color='0'x,linestyle=1
  ;
  ;stop
  OPENW,fid,ofile_mdl_raw,/get_lun
  WRITE_SYS_INFO,fid,prog=prog,src=[pfile],user=user
  IF header_lines[0] NE '' THEN PRINTF,fid,header_lines,format='(a)'
  PRINTF,fid,'> Y-axis',format='("# PSXY_AXIS_MDL_RAW",2(1x,a))'
  PRINTF,fid,d3_x_axis_x[0],d3_x_axis_y[0],format='("# PSXY_AXIS_MDL_RAW",2(1x,f))'
  PRINTF,fid,d3_x_axis_x[1],d3_x_axis_y[1],format='("# PSXY_AXIS_MDL_RAW",2(1x,f))'
  PRINTF,fid,'> X-axis',format='("# PSXY_AXIS_MDL_RAW",2(1x,a))'
  PRINTF,fid,d3_y_axis_x[0],d3_y_axis_y[0],format='("# PSXY_AXIS_MDL_RAW",2(1x,f))'
  PRINTF,fid,d3_y_axis_x[1],d3_y_axis_y[1],format='("# PSXY_AXIS_MDL_RAW",2(1x,f))'
  PRINTF,fid,'far-field strike-slip rates:',fss[ind2[3]],'    locking depth:',lds[ind2[4]],  $
    format='("*",1x,a,1x,f8.2,1x,a,1x,f8.2)'
  PRINTF,fid,'fault trace shift (km):',ftss[ind2[0]],format='("*",1x,a,1x,f)'
  ;PRINTF,fid,'shifted fault trace (deg):',poxy,format='("*",1x,a,1x,2(1x,f12.6))'
  PRINTF,fid,'de-mean of velocity:',xms_all[ind2[1],ind2[2]],format='("*",1x,a,1x,f)'
  PRINTF,fid,'angle of axis rotation (deg):',thetas[ind2[2]]*180/!dpi,format='("*",1x,a,1x,f)'
  FOR i=0, N_ELEMENTS(d2)-1 DO BEGIN
    PRINTF,fid, d3_rot_fts[i], x3_rot_xm[i],  format='(1x,f10.2,1x,f15.7,1x,2(1x,f12.6))'
  ENDFOR
  FREE_LUN,fid
  ;
  
  
  ;STOP
  
  PLOT,d1,x1,color='0'x,psym=1,background='ffffff'x, $
    title='Best-fit Profile', $
    xrange=[-300,200],  $
    xtitle='Distance from Given Fault Trace (km)',  $
    ytitle='Far-field LOS Velocity (mm/yr)',/nodata
  OPLOT,d1,x1,color='aaaaaa'x,psym=1
  
  OPLOT,d2,x2, color='0000ff'x,psym=-4,thick=1
  OPLOT,[-1d3,1d3],[0,0],linestyle=2,color='0'x,thick=2
  OPLOT,[0,0],[-1d3,1d3],linestyle=2,color='0'x,thick=2
  
  OPLOT,[-1d3,1d3],[xms_all[ind2[1],ind2[2]],xms_all[ind2[1],ind2[2]]],linestyle=2,color='00ff00'x,thick=2
  OPLOT,[ftss[ind2[0]],ftss[ind2[0]]],[-1d3,1d3],linestyle=2,color='00ff00'x,thick=2
  ;
  XYOUTS,0,4,STRING('far-field slip rates:',fss[ind2[3]],'    locking depth:',lds[ind2[4]],format='(a,f7.2,a,f5.1)'),color='0'x,alignment=.5
  XYOUTS,0,2,STRING('angle of axis rotation (deg):',thetas[ind2[2]]*180/!dpi,format='(a,f,a,f)'),color='0'x,alignment=.5
  XYOUTS,0,-2,STRING('de-mean of velocity:',xms_all[ind2[1],ind2[2]],format='(a,f7.2,a,f)'),color='0'x,alignment=.5
  XYOUTS,0,-4,STRING('fault trace shift:',ftss[ind2[0]],format='(a,f5.1,a,f)'),color='0'x,alignment=.5
  ;stop
  ;
  PRINT,'far-field slip rates:',fss[ind2[3]],'    locking depth:',lds[ind2[4]]
  PRINT,'angle of axis rotation (deg):',thetas[ind2[2]]*180/!dpi
  ;PRINT,'de-mean of velocity:',xms[ind2[1]]
  PRINT,'de-mean of velocity:',xms_all[ind2[1],ind2[2]]
  PRINT,'fault trace shift:',ftss[ind2[0]]
  
  ;get the lon&lat of these distances
  lons2=DBLARR(N_ELEMENTS(d2))
  lats2=DBLARR(N_ELEMENTS(d2))
  lines4=grepi(lines,'PSXY_PROFILE')
  ;stop
  a1=DOUBLE((STRSPLIT(lines4[0],/extract))[2:3])
  b1=DOUBLE((STRSPLIT(lines4[1],/extract))[2:3])
  rate_p=(b1[1]-a1[1])/(b1[0]-a1[0])
  FOR i=0, N_ELEMENTS(d2)-1 DO BEGIN
  ;stop
  ENDFOR
  
  
  ;  WINDOW,3
  ;  !p.MULTI=-1
  ;  PLOT,[pxy1[0],pxy2[0]],[pxy1[1],pxy2[1]],psym=-2,background='ffffff'x,color='0'x,/ynozero
  ;  XYOUTS,pxy1[0],pxy1[1],'xy1',color='0'x
  ;  XYOUTS,pxy2[0],pxy2[1],'xy2',color='0'x
  ;  OPLOT,[pxy3[0]],[pxy3[1]],color='ff0000'x,psym=4
  
  
  
  OPENW,fid,ofile,/get_lun
  WRITE_SYS_INFO,fid,prog=prog,src=[pfile],user=user
  IF header_lines[0] NE '' THEN PRINTF,fid,header_lines,format='(a)'
  PRINTF,fid,'far-field strike-slip rates:',fss[ind2[3]],'    locking depth:',lds[ind2[4]],  $
    format='("*",1x,a,1x,f8.2,1x,a,1x,f8.2)'
  PRINTF,fid,'fault trace shift (km):',ftss[ind2[0]],format='("*",1x,a,1x,f)'
  PRINTF,fid,'shifted fault trace (deg):',poxy,format='("*",1x,a,1x,2(1x,f12.6))'
  PRINTF,fid,'de-mean of velocity:',xms_all[ind2[1],ind2[2]],format='("*",1x,a,1x,f)'
  PRINTF,fid,'angle of axis rotation (deg):',thetas[ind2[2]]*180/!dpi,format='("*",1x,a,1x,f)'
  FOR i=0, N_ELEMENTS(d2)-1 DO BEGIN
    PRINTF,fid, d2[i], x2[i], lons2[i], lats2[i], format='(1x,f10.2,1x,f15.7,1x,2(1x,f12.6))'
  ENDFOR
  FREE_LUN,fid
  ;
  PLOT,d1,x1,color='0'x,psym=1,background='ffffff'x, $
    title='Rotated Fitted Profile', $
    xtitle='Distance from Given Fault Trace (km)',  $
    ytitle='Far-field LOS Velocity (mm/yr)',/nodata
  OPLOT,d2,x2,color='aaaaaa'x,psym=1
  ;write rotated profile
  OPENW,fid,ofile_rot,/get_lun
  WRITE_SYS_INFO,fid,prog=prog,src=[pfile],user=user
  IF header_lines[0] NE '' THEN PRINTF,fid,header_lines,format='(a)'
  PRINTF,fid,'far-field strike-slip rates:',fss[ind2[3]],'    locking depth:',lds[ind2[4]],  $
    format='("*",1x,a,1x,f,1x,a,1x,f)'
  PRINTF,fid,'fault trace shift (km):',ftss[ind2[0]],format='("*",1x,a,1x,f)'
  PRINTF,fid,'de-mean of velocity:',xms_all[ind2[1],ind2[2]],format='("*",1x,a,1x,f)'
  PRINTF,fid,'angle of axis rotation (deg):',thetas[ind2[2]]*180/!dpi,format='("*",1x,a,1x,f)'
  FOR i=0ull, N_ELEMENTS(d1)-1 DO BEGIN
    PRINTF,fid, d1[i], x1[i],   $
      pd[i], xe[i], $
      format='(1x,f10.2,1x,f15.7,1x,2(1x,f12.6))'
  ENDFOR
  FREE_LUN,fid
  
  
  PLOT,d1,x1,color='0'x,psym=1,background='ffffff'x, $
    title='Rotate Profile (all)', $
    xtitle='Distance from Given Fault Trace (km)',  $
    ytitle='Far-field LOS Velocity (mm/yr)',/nodata
  OPLOT,d_all,x_all,color='aaaaaa'x,psym=1
  ;write rotated profile (all data points)
  OPENW,fid,ofile_rot2,/get_lun
  WRITE_SYS_INFO,fid,prog=prog,src=[pfile],user=user
  IF header_lines[0] NE '' THEN PRINTF,fid,header_lines,format='(a)'
  PRINTF,fid,'far-field strike-slip rates:',fss[ind2[3]],'    locking depth:',lds[ind2[4]],  $
    format='("*",1x,a,1x,f,1x,a,1x,f)'
  PRINTF,fid,'fault trace shift (km):',ftss[ind2[0]],format='("*",1x,a,1x,f)'
  PRINTF,fid,'de-mean of velocity:',xms_all[ind2[1],ind2[2]],format='("*",1x,a,1x,f)'
  PRINTF,fid,'angle of axis rotation (deg):',thetas[ind2[2]]*180/!dpi,format='("*",1x,a,1x,f)'
  FOR i=0ull, N_ELEMENTS(d_all)-1 DO BEGIN
    PRINTF,fid, d_all[i], x_all[i],   $
      p_dists[i], veles_los[i], $
      format='(1x,f10.2,1x,f15.7,1x,2(1x,f12.6))'
  ENDFOR
  FREE_LUN,fid
  
  
  PLOT,d1,x1,color='0'x,psym=1,background='ffffff'x, $
    title='Residual Profile (used)', $
    xtitle='Distance from Given Fault Trace (km)',  $
    ytitle='Far-field LOS Velocity (mm/yr)',/nodata
  OPLOT,d2,x1-x2,color='aaaaaa'x,psym=1
  ;write residual
  OPENW,fid,ofile_resid,/get_lun
  WRITE_SYS_INFO,fid,prog=prog,src=[pfile],user=user
  IF header_lines[0] NE '' THEN PRINTF,fid,header_lines,format='(a)'
  PRINTF,fid,'far-field strike-slip rates:',fss[ind2[3]],'    locking depth:',lds[ind2[4]],  $
    format='("*",1x,a,1x,f,1x,a,1x,f)'
  PRINTF,fid,'fault trace shift (km):',ftss[ind2[0]],format='("*",1x,a,1x,f)'
  PRINTF,fid,'de-mean of velocity:',xms_all[ind2[1],ind2[2]],format='("*",1x,a,1x,f)'
  PRINTF,fid,'angle of axis rotation (deg):',thetas[ind2[2]]*180/!dpi,format='("*",1x,a,1x,f)'
  FOR i=0ull, N_ELEMENTS(d1)-1 DO BEGIN
    PRINTF,fid, d1[i], x1[i]-x2[i],   $
      pd[i], xe[i], $
      lls_site[*,i], $
      format='(1x,f10.2,1x,f15.7,1x,2(1x,f12.6),1x,2(1x,f12.6))'
  ENDFOR
  FREE_LUN,fid
  
  jfile=desuffix(ofile)+'.jpg'
  WRITE_JPEG, jfile, TVRD(true=1), true=1, quality=100
;return
;STOP
END