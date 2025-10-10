PRO INSAR_LOS_2_3D_BY_GPSN_XYZ, paths
  ;decompose the insar los displacements (one acending and one descending) into 3d components
  ;using north component from GNSS


  PROG=(STRSPLIT(LAST(SCOPE_TRACEBACK()),/EXTRACT))[0]
  
  
  IF N_PARAMS() LT 1 THEN BEGIN
    ;Kunlun fatul zone
    ;test d077+a070+a172
    ;    paths=[ $
    ;      'D:\gsar\interseismic\048-d-m6-0462_0467_0472_0477_0482-0487-kunlun\f123\sbas.4.0.0001.9999.20141101.20230401.054.0733.01.___', $
    ;      'D:\gsar\interseismic\150-d-m6-0462_0467_0472_0475_0482_0487-kunlun\f123\sbas.4.0.0001.9999.20141027.20230218.060.0800.01.___', $
    ;      'D:\gsar\interseismic\077-d-m7-0458_0463_0468_0473_0478_0483_0488-kunlun\f123\sbas.4.0.0001.9999.20141103.20230309.050.0670.01.___',  $
    ;      'D:\gsar\interseismic\004-d-m6-0461_0466_0471_0476_0481_0486-eastkunlun\f123\sbas.4.0.0001.9999.20141029.20210513.077.0626.01.___', $
    ;      'D:\gsar\interseismic\106-d-m7-0460_0465_0470_0475_0480_0485_0490-eastkunlun2M\f123\sbas.4.0.0001.9999.20150808.20210520.074.0396.01.___',  $
    ;      'D:\gsar\interseismic\033-d-m7-0463_0468_0473_0478_0483_0487_0493-kunlun\f123\sbas.4.0.0001.9999.20141031.20210515.079.0345.01.___',  $
    ;      'D:\gsar\interseismic\135-d-m6-0471_0476_0481_0486_0491_0496-longriba2_kangding2\f123\sbas.4.0.0001.9999.20141026.20230301.071.0790.01.___',  $
    ;      ;
    ;      'D:\gsar\interseismic\041-a-m6-0099_0104_0109_0114_0119_0124-altyntagh_kunlun\f123\sbas.4.0.0001.9999.20150124.20220920.053.0599.01.___', $
    ;      'D:\gsar\interseismic\143-a-m6-0100_0105_0110_0115_0120_0125-kunlun\f123\sbas.4.0.0367.9999.20141015.20230302.058.0631.01.___', $
    ;      'D:\gsar\interseismic\070-a-m6-0100_0105_0110_0115_0120_0125-eastkunlun5M3\f123\sbas.4.0.0001.9999.20141022.20230225.056.0770.01.___',  $
    ;      'D:\gsar\interseismic\172-a-m6-0096_0101_0106_0111_0116_0121-eastkunlun7M3\f123\sbas.4.0.0001.9999.20141017.20210513.143.0757.01.___',  $
    ;      'D:\gsar\interseismic\099-a-m6-0096_0101_0106_0112_0117_0122-eastkunlun3M3\f123\sbas.4.0.0001.9999.20141012.20210520.156.0620.01.___',  $
    ;      'D:\gsar\interseismic\026-a-m7-0092_0097_0102_0107_0112_0117_0122-eastkunlun1M3\f123\sbas.4.0.0001.9999.20141019.20210515.137.0650.01.___', $
    ;      'D:\gsar\interseismic\128-a-m4-0100_0105_0110_0115-eastkunlun9M3\f123\sbas.4.0.0367.9999.20141014.20220809.194.1300.01.___'   $
    ;      ]
    ;
    ;
    ;    ;  ;interpolated GNSS 3d velocities
    ;    file_gnss='C:\tmp\gic3dv\kunlun\asc_des\gps_prd'
    ;
    ;    ofile='D:\gsar\interseismic\070-a-m6-0100_0105_0110_0115_0120_0125-eastkunlun5M3\f123\sbas.4.0.0001.9999.20141022.20230225.056.0770.01.___\insar_los_2_3d_all.psvelo'
    ;    ofile='D:\gsar\interseismic\070-a-m6-0100_0105_0110_0115_0120_0125-eastkunlun5M3\f123\sbas.4.0.0001.9999.20141022.20230225.056.0770.01.___\insar_los_2_3d_all_2km.psvelo'
    ;    ofile='D:\gsar\interseismic\070-a-m6-0100_0105_0110_0115_0120_0125-eastkunlun5M3\f123\sbas.4.0.0001.9999.20141022.20230225.056.0770.01.___\insar_los_2_3d_all_1km.psvelo'
  
    ;    ;kunlun east
    ;      file_gnss='D:\gsar\gic3dv\jiali\asc_des\gps_prd'
    ;      file_list='D:\gsar\gic3dv\kunlun\asc_des\sbas_list_20241213.txt'
    ;      ofile='D:\gsar\gic3dv\kunlun\asc_des\insar_los_2_3d-20241213.psvelo'
    ;      paths=read_txt(file_list,comment='~ ')
    ;      paths=STRTRIM(paths,2)
    ;
  
    ;    file_list='D:\gsar\gic3dv\kunlun\asc_des\sbas_list_20241219_xsh.txt'
    ;    file_gnss='C:\GMT_pub\gps\wang.min.jgr2020\rotinv.sichuan\resf.psvelo.gps_prd'
    ;    ofile='D:\gsar\gic3dv\kunlun\asc_des\insar_los_2_3d-20241219_xsh.psvelo'
    ;    paths=read_txt(file_list,comment='~ ')
    ;    paths=STRTRIM(paths,2)
  
    ;    ;honghe / redriver
    ;    file_gnss='C:\GMT_pub\gps\wang.min.jgr2020\rotinv.sichuan\resf.psvelo.gps_prd'
    ;    ;      file_gnss='D:\gsar\gic3dv\chuandian2\asc_des\gps_prd'
    ;    file_list='D:\gsar\gic3dv\honghe\asc_des\sbas_list.txt'
    ;    ofile='D:\gsar\gic3dv\honghe\asc_des\insar_los_2_3d-20241216_zeroN.psvelo'
    ;    paths=read_txt(file_list,comment='~ ')
    ;    paths=STRTRIM(paths,2)
  
  
    ;jiali fault
    ;    paths=['D:\gsar\interseismic\070-a-m4-0090_0095_0100_0105-jiali\f123\sbas.4.0.0001.9999.20170103.20220501.146.1192.01.___',  $
    ;      'D:\gsar\interseismic\077-d-m4-0483_0488_0493_0498-jiali2_nujiang4_cona1\f123\sbas.4.0.0367.9999.20141103.20221121.174.1300.01.___', $
    ;      'D:\gsar\interseismic\143-a-m4-0089_0095_0100_0105-jiali_nujiang_yzs4\f123\sbas.4.0.0367.9999.20170108.20210815.138.0699.01.___']
  
    ;    file_list='D:\gsar\gic3dv\jiali\asc_des\sbas.dir.list.txt'
    ;    file_list='D:\gsar\gic3dv\jiali\asc_des\sbas.dir.list.linzhi.txt'
    ;    paths=read_txt(file_list,comment='~ ')
    ;    paths=STRTRIM(paths,2)
    ;    ;paths=paths[0]
    ;    HELP, paths
    ;    ;stop
    ;
    ;    file_gnss='D:\gsar\gic3dv\jiali\asc_des\gps_prd'
    ;    ofile='D:\gsar\gic3dv\jiali\asc_des\insar_los_2_3d-20240221test.psvelo'
    ;    ofile='D:\gsar\gic3dv\jiali\asc_des\insar_los_2_3d-20240221.psvelo'
    ;
    ;    file_gnss='D:\gsar\gic3dv\kunlun\asc_des\gps_prd'
    ;    ofile='D:\gsar\gic3dv\jiali\asc_des\insar_los_2_3d-20240822_linzhi.psvelo'
    ;
    ;
    ;jiali fault
    ;        paths=[ $
    ;        ;'D:\gsar\interseismic\004-d-m4-0481_0486_0491_0496-jiali8M3\f123\sbas.4.0.0367.9999.20141029.20221116.168.1323.01.___', $
    ;        ;'D:\gsar\interseismic\077-d-m4-0483_0488_0493_0498-jiali2_nujiang4_cona1\f123\sbas.4.0.0367.9999.20141103.20221121.174.1300.01.___', $
    ;        'D:\gsar\interseismic\150-d-m4-0482_0487_0492_0497-dangxiong2_yzs3_gulu1\f123\sbas.4.0.0367.9999.20141027.20210417.148.1399.01.___', $
    ;
    ;          'D:\gsar\interseismic\070-a-m4-0090_0095_0100_0105-jiali\f123\sbas.4.0.0001.9999.20170103.20220501.146.1192.01.___',  $
    ;          'D:\gsar\interseismic\143-a-m4-0089_0095_0100_0105-jiali_nujiang_yzs4\f123\sbas.4.0.0367.9999.20170108.20210815.138.0699.01.___']
    ;
    ;  paths=['D:\gsar\interseismic\004-d-m4-0486_0491_0496_0501-jiali8\f123\sbas.4.0.0367.9999.20180105.20210712.085.0931.01.___',$
    ;  'D:\gsar\interseismic\077-d-m5-0480_0485_0490_0495_0500-jiali\f123\sbas.4.0.0367.9999.20170316.20230201.031.0287.01.___', $
    ;  'D:\gsar\interseismic\150-d-m6-0482_0486_0491_0496_0501_506-mht\f123\sbas.4.0.0367.9999.20170225.20231109.103.0507.01.roiDT', $
    ;
    ;  'D:\gsar\interseismic\172-a-m6-0077_0082_0087_0092_0097_0102-jiali5_chayu3\f123\sbas.4.0.0367.9999.20170510.20230316.049.0468.01.___',  $
    ;  'D:\gsar\interseismic\070-a-m6-0080_0085_0090_0095_0100_0105-jiali\f123\sbas.4.0.0001.9999.20170103.20240208.096.0679.01.___',  $
    ;  'D:\gsar\interseismic\143-a-m4-0089_0095_0100_0105-jiali_nujiang_yzs4\f123\sbas.4.0.0367.9999.20141015.20210815.170.1299.01.___', $
    ;  'D:\gsar\interseismic\143-a-m3-0086_0091_0096-jiali_yzs4_mht\f123\sbas.4.0.0367.9999.20170225.20200504.096.2050.01.x2', $
    ;  'D:\gsar\interseismic\041-a-m6-0078_0083_0088_0094_0099_0104-lhasa\f123\sbas.4.0.0367.9999.20170314.20240206.103.0538.01.___']
  
    ;  paths=['D:\gsar\interseismic\077-d-m4-0483_0488_0493_0498-jiali2_nujiang4_cona1\f123\sbas.4.0.0181.9999.20141103.20221121.174.1209.01.___',$
    ;  'D:\gsar\interseismic\070-a-m4-0085_0090_0095_0100-jiali3\f123\sbas.4.0.0700.9999.20170103.20210530.106.0560.01.___', $
    ;  'D:\gsar\interseismic\143-a-m4-0089_0095_0100_0105-jiali_nujiang_yzs4\f123\sbas.4.0.0001.9999.20141015.20230525.222.1100.01.___'$
    ;  ]
    ;;    file_gnss='D:\gsar\gic3dv\kunlun\asc_des\gps_prd'
    ;;    ofile='D:\gsar\gic3dv\jiali\asc_des\insar_los_2_3d-20240911b_linzhi.psvelo'
    ;    file_gnss='D:\gsar\gic3dv\jiali\asc_des.linzhi_voronoi\asc_des.linzhi\gps_prd'
    ;    ofile='D:\gsar\gic3dv\jiali\asc_des.linzhi_voronoi\asc_des.linzhi\insar_los_2_3d-20240912b_linzhi.psvelo'
    ;
    ;    ;no gps vertical
    ;    paths=['D:\gsar\interseismic\077-d-m4-0483_0488_0493_0498-jiali2_nujiang4_cona1\f123\sbas.4.0.0181.9999.20141103.20221121.174.1209.01.___no_U',$
    ;      'D:\gsar\interseismic\070-a-m4-0085_0090_0095_0100-jiali3\f123\sbas.4.0.0700.9999.20170103.20210530.106.0560.01.___no_U', $
    ;      'D:\gsar\interseismic\143-a-m4-0089_0095_0100_0105-jiali_nujiang_yzs4\f123\sbas.4.0.0001.9999.20141015.20230525.222.1100.01.___no_U'$
    ;      ]
    ;    file_gnss='D:\gsar\gic3dv\jiali\asc_des.linzhi_voronoi\asc_des.linzhi\gps_prd'
    ;    ofile='D:\gsar\gic3dv\jiali\asc_des.linzhi_voronoi\asc_des.linzhi\insar_los_2_3d-20240919_linzhi_no_U.psvelo'
  
    ;    ;no gps vertical + larger area (t150+yzs)
    ;    paths=['D:\gsar\interseismic\077-d-m4-0483_0488_0493_0498-jiali2_nujiang4_cona1\f123\sbas.4.0.0181.9999.20141103.20221121.174.1209.01.___no_U',$
    ;      'D:\gsar\interseismic\070-a-m4-0085_0090_0095_0100-jiali3\f123\sbas.4.0.0700.9999.20170103.20210530.106.0560.01.___no_U', $
    ;      'D:\gsar\interseismic\143-a-m4-0089_0095_0100_0105-jiali_nujiang_yzs4\f123\sbas.4.0.0001.9999.20141015.20230525.222.1100.01.___no_U'$
    ;      ]
    ;    ;    ;file_list='D:\gsar\gic3dv\jiali\asc_des.linzhi_voronoi\asc_des.linzhi\sbas_list_no_up_20241015_more_area.txt'
    ;        file_list='D:\gsar\gic3dv\jiali\asc_des.linzhi_voronoi\asc_des.linzhi\sbas_list_no_up_20241105_gulu.txt'
    ;    ;    ;         file_list='D:\gsar\gic3dv\jiali\asc_des.linzhi_voronoi\asc_des.linzhi\sbas_list_no_up_20241105_gulu-d048.txt'
    ;    ;
    ;    ;
    ;        file_gnss='D:\gsar\gic3dv\jiali\asc_des.linzhi_voronoi\asc_des.linzhi\gps_prd'
    ;;        ofile='D:\gsar\gic3dv\jiali\asc_des.linzhi_voronoi\asc_des.linzhi\insar_los_2_3d-20241105_gulu_no_U.psvelo'
    ;        ofile='D:\gsar\gic3dv\jiali\asc_des.linzhi_voronoi\asc_des.linzhi\insar_los_2_3d-20241105_gulu_no_U_zeroN.psvelo'
    ;    ;    ;        ofile='D:\gsar\gic3dv\jiali\asc_des.linzhi_voronoi\asc_des.linzhi\insar_los_2_3d-20241105_gulu_no_U-d048.psvelo'
    ;
    ;    ;    ;reference InSAR velocity maps to raw gnss velocity  (not interpoated grid field)
    ;    ;    file_list='D:\gsar\gic3dv\jiali\asc_des.linzhi_voronoi\asc_des.linzhi\sbas_list_no_up_20241105_gulu-gnss-raw-network.txt'
    ;    ;    ofile='D:\gsar\gic3dv\jiali\asc_des.linzhi_voronoi\asc_des.linzhi\insar_los_2_3d-20241116_gulu_no_U-gnss-raw-network.psvelo'
    ;        paths=read_txt(file_list,comment='~ ')
    ;        paths=STRTRIM(paths,2)
  
  
    ;    ;around yigong
    ;    paths=['D:\gsar\interseismic\041-a-m5-0085_0090_0095_0100_0105-jiali\f123\sbas.4.0.0367.9999.20170314.20241015.224.0622.01.___',  $
    ;      'D:\gsar\interseismic\070-a-m5-0085_0090_0095_0100_0105-jiali\f123\sbas.4.0.0367.9999.20170103.20230414.172.0581.01.___',  $
    ;      'D:\gsar\interseismic\004-d-m3-0486_0491_0496-jiali8M3\f123\sbas.4.0.0001.9999.20141029.20241024.221.0710.01.___',  $
    ;      'D:\gsar\interseismic\004-d-m5-0476_0481_0486_0491_0496-jiali8\f123\sbas.4.0.0001.9999.20170311.20241105.190.0379.01.___']
    ;    file_gnss='D:\gsar\gic3dv\jiali\asc_des.linzhi_voronoi\asc_des.linzhi\gps_prd'
    ;    ofile='D:\gsar\gic3dv\jiali\asc_des.linzhi_voronoi\asc_des.linzhi\insar_los_2_3d-20241125_yigong.psvelo'
    ;
    ;
    ;    ;yadong-gulu
    ;    paths=[ $
    ;      'D:\gsar\interseismic\099-a-m5-0076_0081_0086_0092_0097-chuanzang\f123\sbas.4.0.0001.9999.20141012.20241007.201.0557.01.___', $
    ;      'D:\gsar\interseismic\172-a-m4-0082_0087_0092_0097-jiali5_chayu3\f123\sbas.4.0.0367.9999.20170110.20210805.133.1289.01.___', $
    ;      'D:\gsar\interseismic\070-a-m5-0085_0090_0095_0100_0105-jiali\f123\sbas.4.0.0367.9999.20170103.20230414.172.0581.01.___', $
    ;      'D:\gsar\interseismic\143-a-m5-0084_0089_0095_0100_0105-jiali\f123\sbas.4.0.0001.9999.20170225.20241010.227.0850.01.___\',  $
    ;      'D:\gsar\interseismic\041-a-m5-0085_0090_0095_0100_0105-jiali\f123\sbas.4.0.0367.9999.20170314.20241015.214.0536.01.cln',  $
    ;      'D:\gsar\interseismic\114-a-m4-0090_0095_0100_0105-gyaringco_ranwu_yadong_gulu2\f123\sbas.4.0.0367.9999.20160112.20210322.137.1187.01.___',  $
    ;      'D:\gsar\interseismic\114-a-m4-0095_0100_0105_0110-gyaringco_shuanghu1_wulan5\f123\sbas.4.0.0367.9999.20141106.20241020.256.0623.01.___', $
    ;      'D:\gsar\interseismic\012-a-m3-0088_0093_0098-dingjie2\f123\sbas.4.0.0001.9999.20150603.20200507.121.0707.01.___',  $
    ;      'D:\gsar\interseismic\012-a-m3-0093_0098_0103-gyaringco\f123\sbas.4.0.0367.9999.20141018.20210219.153.1571.01.___', $
    ;      'D:\gsar\interseismic\012-a-m3-0099_0104_0109-riganpei2_gyaringco4_xiangyang_boruo1\f123\sbas.4.0.0367.9999.20141018.20210315.152.1470.01.___', $
    ;
    ;      'D:\gsar\interseismic\033-d-m5-0483_0488_0493_0498_0503-chuanzang_batangM3\f123\sbas.4.0.0367.9999.20150710.20240217.114.0538.01.___', $
    ;      'D:\gsar\interseismic\106-d-m5-0481_0486_0491_0496_0502-xsh_jiali\f123\sbas.4.0.0001.9999.20170222.20240727.190.0631.01.___', $
    ;      'D:\gsar\interseismic\004-d-m5-0476_0481_0486_0491_0496-jiali8\f123\sbas.4.0.0001.9999.20170311.20241105.192.0553.01.___', $
    ;      'D:\gsar\interseismic\077-d-m5-0480_0485_0490_0495_0500-jiali\f123\sbas.4.0.0367.9999.20170316.20240806.190.0720.01.___', $
    ;      'D:\gsar\interseismic\150-d-m6-0482_0486_0491_0496_0501_506-mht\f123\sbas.4.0.0001.9999.20170225.20231109.103.0797.01.roiDT', $
    ;      'D:\gsar\interseismic\048-d-m5-0478_0483_0488_0493_0498-sewa3_wulan1_gyaringco3_ranwu2_iys\f123\sbas.4.0.0001.9999.20150113.20230131.065.0698.01.___',  $
    ;      'D:\gsar\interseismic\121-d-m6-0462_0467_0472_0477_0482_0487-atf_kunlun\f123\sbas.4.0.0001.9999.20141026.20230325.048.0732.01.___', $
    ;      'D:\gsar\interseismic\121-d-m4-0482_0486_0492_0497-gyaringco2_riganpei3_yzs2\f123\sbas.4.0.0367.9999.20141026.20211106.162.1300.01.___']
    ;    file_gnss='D:\gsar\gic3dv\jiali\asc_des.linzhi_voronoi\asc_des.linzhi\gps_prd'
    ;;    file_gnss='D:\gsar\gic3dv\kunlun\asc_des\gps_prd'
    ;    ofile='D:\gsar\gic3dv\jiali\asc_des.linzhi_voronoi\asc_des.linzhi\insar_los_2_3d-20241201_ydgl.psvelo'
    ;
  
    ;  ;jiali east
    ;      file_gnss='D:\gsar\gic3dv\jiali\asc_des\gps_prd'
    ;;    file_gnss='D:\gsar\gic3dv\kunlun\asc_des\gps_prd'
    ;;      file_gnss='D:\gsar\gic3dv\jiali\asc_des.linzhi_voronoi\asc_des.linzhi\gps_prd'
    ;      file_list='D:\gsar\gic3dv\jiali\asc_des.linzhi_voronoi\asc_des.linzhi\sbas_list_no_up_20241213.txt'
    ;      ofile='D:\gsar\gic3dv\jiali\asc_des.linzhi_voronoi\asc_des.linzhi\insar_los_2_3d-20241213.psvelo'
    ;      paths=read_txt(file_list,comment='~ ')
    ;      paths=STRTRIM(paths,2)
    ;
    ;;    ; jiali with densified NINH network; no GPS vertical
    ;    file_list='D:\gsar\gic3dv\jiali\interp\asc_des\sbas_list_20250604b.txt'
    ;    paths=read_txt(file_list,comment='~ ')
    ;    paths=STRTRIM(paths,2)
    ;    file_gnss='D:\gsar\gic3dv\jiali\interp\asc_des\gps_prd'
    ;    ofile='D:\gsar\gic3dv\jiali\interp\asc_des\insar_3d-20250604_noGPSup.psvelo'
    ;    ofile='D:\gsar\gic3dv\jiali\interp\asc_des\insar_3d-20250604_withGPSup.psvelo'
    ;    ofile='D:\gsar\gic3dv\jiali\interp\asc_des\insar_3d-20250604_withGPSupExt.psvelo'
  
    ;    ;with LGN network (3D)
    ;    file_list='D:\gsar\gic3dv\jiali\interp\asc_des2\sbas_list_20250604Ext.txt'
    ;    paths=read_txt(file_list,comment='~ ')
    ;    paths=STRTRIM(paths,2)
    ;    file_gnss='D:\gsar\gic3dv\jiali\interp\asc_des2\gps_prd'
    ;    ofile='D:\gsar\gic3dv\jiali\interp\asc_des2\insar_3d-20250628_withGPSupExt.psvelo'
  
    ;    ; jiali East with densified NINH network; with GPS vertical
    ;    file_list='D:\gsar\gic3dv\jiali\interp\asc_des\sbas_list_20250620_jialiEast.txt'
    ;    paths=read_txt(file_list,comment='~ ')
    ;    paths=STRTRIM(paths,2)
    ;    file_gnss='D:\gsar\gic3dv\jiali\interp\asc_des\gps_prd'
    ;    ofile='D:\gsar\gic3dv\jiali\interp\asc_des\insar_3d-20250620_jialiEast.psvelo'
  
    ;        ; re-processsed 143+77 (validate the results: seems good)
    ;        file_list='D:\gsar\gic3dv\jiali\interp\asc_des2\sbas_list_20250807.txt'
    ;        paths=read_txt(file_list,comment='~ ')
    ;        paths=STRTRIM(paths,2)
    ;        file_gnss='D:\gsar\gic3dv\jiali\interp\asc_des2\gps_prd'
    ;        ofile='D:\gsar\gic3dv\jiali\interp\asc_des2\insar_3d-20250807.psvelo'
;    ;
;    ;
;    file_list='D:\gsar\gic3dv\jiali\interp\asc_des\sbas_list_20250903.txt'
;    paths=read_txt(file_list,comment='~ ')
;    paths=STRTRIM(paths,2)
;    file_gnss='C:\GMT_pub\gps\wang.min.jgr2020\rotinv.eastern_lhasa_block\resf.psvelo'
;    fmt_gnss='psvelo'
;    ofile='D:\gsar\gic3dv\jiali\interp\asc_des\insar_3d-20250903.psvelo'
    
    ;
    ;
    ;shuanghu
    ;;        file_gnss='D:\gsar\gic3dv\jiali\asc_des\gps_prd'
    ;        ;file_gnss='D:\gsar\gic3dv\jiali\asc_des.linzhi_voronoi\asc_des.linzhi\gps_prd'
    ;        file_gnss='C:\GMT_pub\gps\wang.min.jgr2020\rotinv.kunlun2\resf.psvelo'
    ;        fmt_gnss='psvelo'
    ;        file_list='D:\gsar\gic3dv\shuanghu\asc_des\sbas_list_20250601.txt'
    ;        ofile='D:\gsar\gic3dv\shuanghu\asc_des\insar_los_2_3d-20250601.psvelo'
    ;        paths=read_txt(file_list,comment='~ ')
    ;        paths=STRTRIM(paths,2)
    ;
    
    ;    ;jiali+bengco+gyaringco
    ;    file_gnss='D:\gsar\gic3dv\jiali\asc_des.linzhi_voronoi\asc_des.linzhi\gps_prd'
    ;    file_list='D:\gsar\gic3dv\jiali\asc_des.linzhi_voronoi\asc_des.linzhi\sbas_list_2025026.txt'
    ;    paths=read_txt(file_list,comment='~ ')
    ;    paths=STRTRIM(paths,2)
    ;    ofile='D:\gsar\gic3dv\jiali\asc_des.linzhi_voronoi\asc_des.linzhi\insar_3d-20250526.psvelo'
    ;
    ;
    ;    ;iys sse
    ;    paths=['D:\gsar\interseismic\012-a-m6-0078_0083_0088_0093_0098_0103-mht\f123\sbas.4.0.0367.9999.20150603.20230317.060.0597.01.___',  $
    ;      'D:\gsar\interseismic\085-a-m6-0083_0088_0093_0098_0104_0109-mht\f123\sbas.4.0.0001.9999.20150726.20231105.106.0535.01.___', $
    ;      'D:\gsar\interseismic\121-d-m4-0486_0492_0497_0502-dingjie_gyaringco2\f123\sbas.4.0.0001.9999.20150506.20231108.217.0639.01.___']
    ;
    ;      file_gnss='C:\tmp\gic3dv\jiali\asc_des\gps_prd'
    ;      ofile='C:\tmp\gic3dv\iys\asc_des\insar_los_2_3d.psvelo'
    
    ;paths=['D:\gsar\interseismic\012-a-m4-0083_0088_0093_0098-iys\f123\sbas.4.0.0001.9999.20150603.20231112.220.1059.01.___', $
    ;'D:\gsar\interseismic\085-a-m3-0083_0088_0093-mht\f123\sbas.4.0.0367.9999.20150608.20210320.146.1700.01.___', $
    ;'D:\gsar\interseismic\121-d-m3-0491_0496_0501-dingjie_mht\f123\sbas.4.0.0367.9999.20150611.20181228.072.0261.01.___']
    ;
    ;      file_gnss='D:\gsar\gic3dv\jiali\asc_des\gps_prd'
    ;      ofile='D:\gsar\gic3dv\iys\asc_des\insar_los_2_3d_20240120a.psvelo'
    ;
    ;    paths=['D:\gsar\interseismic\121-d-m4-0486_0492_0497_0502-dingjie_gyaringco2\f123\sbas.4.0.0001.9999.20150530.20231108.216.1102.01.___', $
    ;      'D:\gsar\interseismic\012-a-m4-0083_0088_0093_0098-iys\f123\sbas.4.0.0367.9999.20150603.20231112.220.0706.01.___']
    ;    file_gnss='D:\gsar\gic3dv\jiali\asc_des\gps_prd'
    ;    ofile='D:\gsar\gic3dv\yzs\asc_des\insar_los_2_3d_20240427.psvelo'
    
    ;      file_list='D:\gsar\gic3dv\yzs\asc_des\sbas.list.y15-19.txt'
    ;      paths=read_txt(file_list,comment='~ ')
    ;      paths=STRTRIM(paths,2)
    ;      ;paths=paths[0]
    ;      HELP, paths
    ;      file_gnss='D:\gsar\gic3dv\jiali\asc_des\gps_prd'
    ;      ofile='D:\gsar\gic3dv\yzs\asc_des\insar_los_2_3d_20240516_y2015-2019.psvelo'
    
    ;    file_list='D:\gsar\gic3dv\yzs\asc_des\sbas.list.y15-19-gacos.txt'
    ;    paths=read_txt(file_list,comment='~ ')
    ;    paths=STRTRIM(paths,2)
    ;    ;paths=paths[0]
    ;    HELP, paths
    ;    file_gnss='D:\gsar\gic3dv\jiali\asc_des\gps_prd'
    ;    ofile='D:\gsar\gic3dv\yzs\asc_des\insar_los_2_3d_20240801_y2015-2019-gacos.psvelo'
    ;
    ;    file_list='D:\gsar\gic3dv\yzs\asc_des\sbas.list.20250505.txt'
    ;    paths=read_txt(file_list,comment='~ ')
    ;    paths=STRTRIM(paths,2)
    ;    ;paths=paths[0]
    ;    HELP, paths
    ;    file_gnss='C:\GMT_pub\gps\wang.min.jgr2020\rotinv.eastern_lhasa_block\resf.psvelo'
    ;    fmt_gnss='psvelo'
    ;    ofile='D:\gsar\gic3dv\yzs\asc_des\insar_3d_20250505.psvelo'
    ;
;    file_list='D:\gsar\gic3dv\yzs\asc_des\sbas.list.20250721.txt'
;    file_list='D:\gsar\gic3dv\yzs\asc_des\sbas.list.20250808.txt'
;    file_list='D:\gsar\gic3dv\yzs\asc_des\sbas.list.20250809.txt'
;    ;    file_list='D:\gsar\gic3dv\yzs\asc_des\sbas.list.20250808pre.txt'
;    paths=read_txt(file_list,comment='~ ')
;    paths=STRTRIM(paths,2)
;    ;paths=paths[0]
;    HELP, paths
;    file_gnss='C:\GMT_pub\gps\wang.min.jgr2020\rotinv.eastern_lhasa_block\resf.psvelo'
;    fmt_gnss='psvelo'
;    ofile='D:\gsar\gic3dv\yzs\asc_des\insar_3d_20250809.psvelo'
;  ;    ofile='D:\gsar\gic3dv\yzs\asc_des\insar_3d_20250808pre.psvelo'
;  ;    dlon=9d-3
;  ;
  ;
  ;
  ;      ;tianshan
  ;          paths=[ $
  ;          ;'D:\gsar\interseismic\056-a-m3-0128_0133_0138-tianshan\f123\sbas.4.0.0366.9999.20151004.20210129.078.1656.01.___',  $
  ;          'D:\gsar\interseismic\056-a-m3-0123_0128_0133-tianshan1M3\f123.1\sbas.3.0.0400.9999.20141114.20191230.102.0329.01.___', $
  ;          'D:\gsar\interseismic\034-d-m8-0448_0452_0456_0461_0466_0471_0476_0481-tianshan\f123\sbas.4.0.0367.9999.20151015.20221026.056.0435.01.___']
  ;          ;'D:\gsar\interseismic\034-d-m3-0448_0452_0457-tianshan\f123\sbas.4.0.0366.9999.20141020.20200310.094.0792.01.___',  $
  ;          ;'D:\gsar\interseismic\136-d-m5-0454_0459_0464_0471_0476-tarim\f123\sbas.4.0.0001.9999.20141027.20211219.050.0659.01.___']
  ;          ;'D:\gsar\interseismic\136-d-m3-0449_0454_0459-tianshan4M3\f123.1\SBAS4.500-']
    
  ;          paths=['D:\gsar\interseismic\034-d-m4-0448_0452_0457_0462-tianshan\f123\sbas.4.0.0001.9999.20151015.20240125.139.0800.01.___',  $
  ;          'D:\gsar\interseismic\136-d-m4-0449_0454_0459_0464-tianshan4M3\f123\sbas.4.0.0001.9999.20150928.20211219.132.0586.01.___', $
  ;          'D:\gsar\interseismic\056-a-m4-0123_0128_0133_0138-tianshan\f123\sbas.4.0.0001.9999.20151004.20240126.223.0703.01.___', $
  ;          'D:\gsar\interseismic\129-a-m3-0124_0129_0134-kashi\f123.1\sbas.3.0.0400.9999.20151009.20200409.113.0342.01.___']
  ;
  ;
  ;          file_gnss='D:\gsar\gic3dv\tianshan\asc_des\gps_prd'
  ;          ofile='D:\gsar\gic3dv\tianshan\asc_des\insar_3d_20240315b.psvelou'
    
  ;      file_list='D:\gsar\gic3dv\tianshan\asc_des\sbas.list.20250806.txt'
  ;    paths=read_txt(file_list,comment='~ ')
  ;    paths=STRTRIM(paths,2)
  ;    ;paths=paths[0]
  ;    HELP, paths
  ;    ;file_gnss='C:\GMT_pub\gps\wang.min.jgr2020\rotinv.kunlun2\resf.psvelo'
  ;    file_gnss='C:\GMT_pub\gps\wang.min.jgr2020\rotinv.tarim3\resf.psvelo'
  ;;    file_gnss='C:\GMT_pub\gps\wang.min.jgr2020\rotinv.chuandian2\resf.psvelo'
  ;;    file_gnss='C:\GMT_pub\gps\wang.min.jgr2020\rotinv.litang2\resf.psvelo'
  ;;    file_gnss='C:\GMT_pub\gps\wang.min.jgr2020\rotinv.aba2\resf.psvelo'
  ;    fmt_gnss='psvelo'
  ;    ofile='D:\gsar\gic3dv\tianshan\asc_des\insar_3d_20250806.psvelo'
  ;    dlon=9d-3
    
    
  ;
  ;      ;menyuan
  ;      paths=[ $
  ;      ;'D:\gsar\interseismic\033-d-m4-0458_0463_0468_0473-qinghai_lake_haiyuan\f123\sbas.4.0.0367.9999.20141031.20210421.147.1280.01.___',  $
  ;      ;'D:\gsar\interseismic\004-d-m6-0461_0466_0471_0476_0481_0486-eastkunlun\f123\sbas.4.0.0367.9999.20141029.20210513.077.0653.01.___', $
  ;;      'D:\gsar\interseismic\106-d-m7-0460_0465_0470_0475_0480_0485_0490-eastkunlun2M\f123\sbas.4.0.0001.9999.20150808.20210520.074.0396.01.___',  $
  ;      'D:\gsar\interseismic\135-d-m7-0451_0456_0461_0467_0472_0477-0482-haiyuan_gulang\f123\sbas.4.0.0001.9999.20150130.20221020.049.0596.01.___',  $
  ;      ;'D:\gsar\interseismic\062-d-m6-0447_0452_0457_0462_0467_0472-haiyuan4M3\f123\sbas.4.0.0001.9999.20150206.20210517.053.0561.01.___', $
  ;      'D:\gsar\interseismic\062-d-m3-0462_0467_0472-haiyuan4M3\f123\sbas.4.0.0367.9999.20150206.20210423.144.1350.01.___',  $
  ;      'D:\gsar\interseismic\055-a-m3-0112_0117_0122-haiyuan1M3\f123\sbas.4.0.0367.9999.20141021.20210423.122.1215.01.___', $
  ;      'D:\gsar\interseismic\157-a-m4-1018_0113_0118_0123-haiyuan\f123\sbas.4.0.0367.9999.20141016.20210406.144.1437.01.___', $
  ;;      'D:\gsar\interseismic\099-a-m4-0117_0122_0126_0131-atf\f123\sbas.4.0.0367.9999.20141012.20210402.152.1300.01.___', $
  ;;      'D:\gsar\interseismic\026-a-m7-0092_0097_0102_0107_0112_0117_0122-eastkunlun1M3\f123\sbas.4.0.0001.9999.20141019.20210515.137.0650.01.___', $
  ;      'D:\gsar\interseismic\128-a-m6-0100_0105_0110_0115_0120_0125-eastkunlun9M3\f123\sbas.4.0.0001.9999.20141014.20230406.060.0800.01.___', $
  ;      'D:\gsar\interseismic\128-a-m3-0115_0120_0125-haiyuan\f123\sbas.4.9.0367.9999.20141014.20201006.136.1451.01.___']
  ;;      'D:\gsar\interseismic\026-a-m4-0117_0122_0127_0132-qilian\f123\sbas.4.0.0367.9999.20141019.20210208.140.1329.01.___']
  ;
  ;    paths=['D:\gsar\interseismic\004-d-m4-0455_0461_0466_0471-atf\f123\sbas.4.0.0001.9999.20141029.20250317.112.0150.01.___', $
  ;  ;    'D:\gsar\interseismic\099-a-m4-0117_0122_0126_0131-atf\f123\sbas.4.0.0367.9999.20141012.20210402.152.1300.01.___']
  ;;      file_list='D:\gsar\gic3dv\hyf\asc_des\sbas_list_20250420.txt'
  ;      file_list='D:\gsar\gic3dv\hyf\asc_des\sbas_list_20250420b.txt'
  ;        paths=read_txt(file_list,comment='~ ')
  ;        paths=STRTRIM(paths,2)
  ;
  ;          file_gnss='D:\gsar\gic3dv\kunlun\asc_des\gps_prd'
  ;  ;          ofile='C:\tmp\gic3dv\hyf\asc_des\insar_los_2_3d_1920.psvelo'
  ;;            ofile='D:\gsar\gic3dv\hyf\asc_des\insar_3d_20250425.psvelo'
  ;            ofile='D:\gsar\gic3dv\hyf\asc_des\insar_3d_20250420b.psvelo'
  ;
  ;    ;2022 menyuan EQ, preslip
  ;    paths=[ $
  ;      'D:\gsar\interseismic\033-d-m4-0458_0463_0468_0473-qinghai_lake_haiyuan\f123\sbas.4.0.0001.9999.20141031.20211229.167.0644.01.___',  $
  ;      'D:\gsar\interseismic\106-d-m4-0460_0465_0470_0475-qinghai_lake4M3\f123\sbas.4.0.0001.9999.20141117.20210520.151.0800.01.___',  $
  ;      ;'D:\gsar\interseismic\135-d-m4-0461_0467_0472_0477-haiyuan_gulang\f123\sbas.4.0.0001.9999.20141026.20211212.144.0277.01.___',  $
  ;
  ;      'D:\gsar\interseismic\128-a-m4-0110_0115_0120_0125-haiyuan\f123\sbas.4.0.0001.9999.20141014.20211224.174.0564.01.___', $
  ;      'D:\gsar\interseismic\026-a-m4-0117_0122_0127_0132-qilian\f123\sbas.4.0.0367.9999.20141019.20210208.140.1329.01.___']
  ;
  ;    file_gnss='D:\gsar\gic3dv\kunlun\asc_des\gps_prd'
  ;    ofile='D:\gsar\gic3dv\hyf\asc_des\insar_3d_20240812.psvelo'
  ;
  ;        ;lajishan
  ;    paths=[ $
  ;       'D:\gsar\interseismic\135-d-m4-0461_0467_0472_0477-haiyuan_gulang\f123\sbas.4.0.0001.9999.20141026.20220105.148.0451.01.___',  $
  ;
  ;      'D:\gsar\interseismic\128-a-m4-0110_0115_0120_0125-haiyuan\f123\sbas.4.0.0001.9999.20141014.20211224.174.0564.01.___']
  ;
  ;    file_gnss='D:\gsar\gic3dv\kunlun\asc_des\gps_prd'
  ;    ofile='D:\gsar\gic3dv\hyf\asc_des\insar_3d_20240815_lajishan.psvelo'
    
  ;        ;lajishan
  ;    paths=[ $
  ;       'D:\gsar\interseismic\033-d-m4-0463_0468_0473_0478-lajishan\F1\sbas.4.0.0367.9999.20141031.20240803.220.0314.01.___',  $
  ;
  ;      'D:\gsar\interseismic\128-a-m4-0110_0115_0120_0125-haiyuan\f123\sbas.4.0.0001.9999.20141014.20211224.174.0564.01.___']
  ;
  ;    file_gnss='D:\gsar\gic3dv\kunlun\asc_des\gps_prd'
  ;    ofile='D:\gsar\gic3dv\hyf\asc_des\insar_3d_20240820_lajishan.psvelo'
  ;
  ;
  ;    ;      ;pishan
  ;    paths=[ $
  ;      'D:\gsar\interseismic\136-d-m5-0471_0476_0481_0486_0491-aksaichin\f123\sbas.4.0.0001.9999.20150928.20230829.115.0800.01.___',  $
  ;      'D:\gsar\interseismic\034-d-m5-0461_0466_0471_0476_0481-tibet\f123\sbas.4.0.0001.9999.20151015.20221026.056.0800.01.___', $
  ;      'D:\gsar\interseismic\063-d-m5-0462_0467_0472_0478_0483-aksaichin2_karakoram\f123\sbas.4.0.0001.9999.20141010.20230707.109.0787.01.___',  $
  ;      'D:\gsar\interseismic\056-a-m5-0098_0103_0108_0113_0118-tibet\f123\sbas.4.0.0001.9999.20141114.20230811.082.0800.01.___',  $ ;contain coseismic
  ;      ;'D:\gsar\interseismic\056-a-m3-0114_0119_0124-karakax\f123.1\sbas.3.0.0700.9999.20150724.20200615.116.0333.01.post', $
  ;      'D:\gsar\interseismic\129-a-m3-0114_0119_0124-karakax\f123\sbas.4.0.0367.9999.20151009.20210404.143.1255.01.___',  $
  ;      'D:\gsar\interseismic\055-a-m3-0112_0117_0122-haiyuan1M3\f123\sbas.4.0.0367.9999.20141021.20210423.122.1215.01.___']
  ;
  ;    file_gnss='D:\gsar\gic3dv\pishan\asc_des\gps_prd'
  ;    ofile='D:\gsar\gic3dv\pishan\asc_des\insar_3d.psvelo'
  ;
  ;    ;      ;western Tibet
  ;    file_list='D:\gsar\gic3dv\g219\asc_des\sbas.list.txt'
  ;    paths=read_txt(file_list,comment='~ ')
  ;    paths=STRTRIM(paths,2)
  ;    ;paths=paths[0]
  ;    HELP, paths
  ;    file_gnss='D:\gsar\gic3dv\g219\asc_des\gps_prd'
  ;    ofile='D:\gsar\gic3dv\g219\asc_des\insar_3d.psvelo'
  ;
  ;
  ;
    
  ;;mht
  ;paths=['D:\gsar\interseismic\012-a-m3-0078_0083_0088-mht\f123\sbas.4.0.0367.9999.20160105.20210219.133.1700.01.___',  $
  ;'D:\gsar\interseismic\121-d-m3-0491_0496_0501-dingjie_mht\f123\sbas.4.0.0367.9999.20160313.20210323.133.1702.01.___', $
  ;'D:\gsar\interseismic\048-d-m3-0494_0499_0504-mht\f123\sbas.4.0.0900.9999.20150606.20210210.124.0563.01.___']
  ;
  ;    file_gnss='C:\GMT_pub\gps\wang.min.jgr2020\rotinv.indian_plate\resf.psvelo'
  ;    ofile='D:\gsar\gic3dv\mht\asc_des\insar_3d.psvelo'
  ;
  ;  ;Gyaring Co fault
  ;  paths=[$
  ;    '', $
  ;    '', $
  ;    '', $
  ;    '']
  ;
  ;    file_gnss='C:\tmp\gic3dv\pishan\asc_des\gps_prd'
  ;      ofile='C:\tmp\gic3dv\pishan\asc_des\insar_los_2_3d.psvelo'
    
  ;    ;atf D019 track
  ;    paths=['D:\gsar\interseismic\114-a-m6-0100_0105_0110_0115_0120_0125-altyntagh2\f123\sbas.4.0.0001.9999.20141106.20231107.120.0546.01.___',  $
  ;      'D:\gsar\interseismic\019-d-m4-0461_0466_0471_0476-altyntagh_M3\f123\sbas.4.0.0001.9999.20141031.20240229.155.0238.01.___', $
  ;      'D:\gsar\interseismic\019-d-m5-0461_0466_0471_0476_0481-atf\f123\sbas.4.0.0001.9999.20141031.20230330.057.0323.01.___', $
  ;      'D:\gsar\interseismic\121-d-m6-0462_0467_0472_0477_0482_0487-atf_kunlun\f123\sbas.4.0.0367.9999.20141026.20230325.048.0459.01.___', $
  ;      'D:\gsar\interseismic\048-d-m4-0457_0462_0467_0472-qaidagasikule2__altyntagh\f123\sbas.4.0.0367.9999.20141101.20211112.160.1300.01.___',$
  ;      'D:\gsar\interseismic\048-d-m6-0462_0467_0472_0477_0482-0487-kunlun\f123\sbas.4.0.0001.9999.20141101.20230401.054.0103.01.___',$
  ;      'D:\gsar\interseismic\041-a-m6-0099_0104_0109_0114_0119_0124-altyntagh_kunlun\f123\sbas.4.0.0001.9999.20141020.20230223.060.0101.01.___', $
  ;      'D:\gsar\interseismic\143-a-m4-0115_0120_0125_0130-altyntagh_M3\f123\sbas.4.0.0367.9999.20141015.20210417.155.1300.01.___', $
  ;      'D:\gsar\interseismic\143-a-m4-0105_0110_0115_0120-kunlun5_wulan4_nujiang5\f123\sbas.4.0.0900.9999.20141015.20210228.154.0739.01.___',$
  ;      'D:\gsar\interseismic\092-d-m5-0460_0465_0470_0475_0480-altyntagh_M3\f123\sbas.4.0.0001.9999.20141012.20221030.054.0800.01.___',  $
  ;      'D:\gsar\interseismic\165-d-m6-0467_0472_0477_0482_0487_0492-woniuhu4M3\f123\sbas.4.0.0367.9999.20141029.20230208.063.0258.01.___',$
  ;      ;'D:\gsar\interseismic\085-a-m6-0098_0104_0109_0114_0119_0124-atf\f123\sbas.4.0.0001.9999.20150515.20230415.057.0380.01.___',  $
  ;      'D:\gsar\interseismic\085-a-m4-0088_0093_0098_0103-gaize_yzs5\f123\sbas.4.0.0367.9999.20150515.20211127.164.1300.01.___',$
  ;      'D:\gsar\interseismic\085-a-m4-0104_0109_0114_0119-woniuhu1M3\f123\sbas.4.0.0367.9999.20141116.20220303.182.1300.01.___',$
  ;      'D:\gsar\interseismic\012-a-m6-0104_0109_0114_0119_0124_0129-altyntagh\f123\sbas.4.0.0367.9999.20141229.20230410.057.0557.01.roi_detrend']
  ;
    
  ;;    file_list='\\10.4.134.30\root\g11j\D\gsar\gic3dv\atf.d019\asc_des.3rd-order\sbas.list.txt'
  ;    file_list='D:\gsar\gic3dv\atf.d019\validation.dataset\3d.deformation.field\sbas.list.txt'
  ;    paths=read_txt(file_list,comment='~ ')
  ;    paths=STRTRIM(paths,2)
  ;    file_gnss='D:\gsar\gic3dv\atf.d019\validation.dataset\gnss_velocity_field\gps_prd'
  ;;    ofile='D:\gsar\gic3dv\atf.d019\asc_des.3rd-order\insar_3d_20250306.psvelou'
  ;    ofile='D:\gsar\gic3dv\atf.d019\validation.dataset\3d.deformation.field\insar_3d.psvelou'
  ;
    
    
  ;
  ;    ;tangshan postseismic
  ;    paths=['D:\gsar\interseismic\149-d-m4-0453_0458_0464_0469-beijing4\f123\sbas.4.0.0001.9999.20150612.20211207.137.0218.01.___',$
  ;      'D:\gsar\interseismic\069-a-m4-0119_0124_0129_0134-jing_jin_shandong\f123\sbas.4.0.0001.9999.20150701.20231116.139.0131.01.___']
  ;
  ;    file_gnss='D:\gsar\gic3dv\tangshan\asc_des\gps_prd'
  ;    ofile='D:\gsar\gic3dv\tangshan\asc_des\insar_3d.psvelou'
    
    
  ;    ;hami
  ;    paths=['D:\gsar\interseismic\099-a-m3-1310_1315_1320-hami\f123\sbas.4.0.0001.9999.20170318.20240808.051.0195.01.___',$
  ;    'D:\gsar\interseismic\077-d-m3-0449_0455_0460-hami\f123\sbas.4.0.0001.9999.20150207.20240619.078.0303.01.___']
  ;    ;
  ;    paths=[ $
  ;      'D:\gsar\interseismic\106-d-m3-0450_0455_0460-hami\f123\sbas.4.0.0001.9999.20170318.20240727.079.0518.01.___',  $
  ;      'D:\gsar\interseismic\106-d-m4-0460_0465_0470_0475-qinghai_lake4M3\f123\sbas.4.0.0001.9999.20141117.20210520.151.0399.01.___',  $
  ;      'D:\gsar\interseismic\106-d-m5-0470_0475_0480_0485_0490-eastkunlun2M\f123\sbas.4.0.0001.9999.20141024.20210520.055.0800.01.___', $
  ;
  ;      'D:\gsar\interseismic\004-d-m3-0450_0456_0461-hami\f123\sbas.4.0.0367.9999.20170311.20240825.151.0585.01.___', $
  ;      'D:\gsar\interseismic\004-d-m4-0466_0471_0476_0481-eastkunlun\f123\sbas.4.0.0367.9999.20150614.20210513.132.0623.01.___', $
  ;      'D:\gsar\interseismic\004-d-m6-0461_0466_0471_0476_0481_0486-eastkunlun\f123\sbas.4.0.0001.9999.20141029.20210513.077.0360.01.___', $
  ;
  ;      'D:\gsar\interseismic\077-d-m3-0439_0444_0449-hami\f123\sbas.4.9.0367.9999.20150207.20210325.096.0409.01.___', $
  ;      'D:\gsar\interseismic\077-d-m3-0449_0455_0460-hami\f123\sbas.4.0.0367.9999.20150207.20240818.175.0618.01.___',  $
  ;      'D:\gsar\interseismic\077-d-m4-0453_0458_0463_0468-altyntagh\f123\sbas.4.0.0700.9999.20141103.20210424.125.0903.01.___',  $
  ;      'D:\gsar\interseismic\077-d-m7-0458_0463_0468_0473_0478_0483_0488-kunlun\f123\sbas.4.0.0001.9999.20141103.20230309.050.0577.01.___', $
  ;
  ;      'D:\gsar\interseismic\150-d-m3-0434_0439_0444-hami\f123\sbas.4.0.0001.9999.20150131.20211008.129.0571.01.___',  $
  ;      ;'D:\gsar\interseismic\150-d-m3-0440_0445_0450-hami\f123\sbas.4.0.0001.9999.20150131.20210330.101.0237.01.___',$
  ;      'D:\gsar\interseismic\150-d-m4-0445_0450_0455_0460-hami\f123\sbas.4.0.0001.9999.20150131.20240624.140.0751.01.___', $
  ;      ;'D:\gsar\interseismic\150-d-m5-0445_0450_0455_0460_0465-hami\f123\sbas.4.0.0001.9999.20150131.20240811.062.0797.01.___', $
  ;      'D:\gsar\interseismic\150-d-m4-0457_0462_0467_0472-altyntagh_M3\f123\sbas.4.0.0700.9999.20141027.20210417.144.0994.01.___', $
  ;      'D:\gsar\interseismic\150-d-m6-0462_0467_0472_0475_0482_0487-kunlun\f123\sbas.4.0.0001.9999.20141027.20230218.060.0640.01.___', $
  ;
  ;         ;'D:\gsar\interseismic\048-d-m3-0435_0440_0445-hami\f123\sbas.4.0.0001.9999.20160729.20200807.080.0580.01.___',  $
  ;      ;    'D:\gsar\interseismic\048-d-m4-0445_449_0455_0459-hami\f123\sbas.4.0.0001.9999.20150205.20240816.191.0260.01.___', $
  ;      'D:\gsar\interseismic\048-d-m4-0445_449_0455_0459-hami\f123\sbas.4.0.0001.9999.20150205.20240921.194.0233.01.___', $
  ;      'D:\gsar\interseismic\048-d-m4-0457_0462_0467_0472-qaidagasikule2__altyntagh\f123\sbas.4.0.0700.9999.20141101.20211112.160.1032.01.___',  $
  ;      'D:\gsar\interseismic\048-d-m6-0462_0467_0472_0477_0482-0487-kunlun\f123\sbas.4.0.0367.9999.20141101.20230401.054.0609.01.___', $
  ;
  ;      'D:\gsar\interseismic\121-d-m4-0444_0449_0454_0459-tianshan\f123\sbas.4.0.0001.9999.20141026.20210410.116.0579.01.___', $
  ;      'D:\gsar\interseismic\121-d-m3-0451_0456_0462-altyntagh_M3\f123\sbas.4.0.0367.9999.20141026.20190620.097.1361.01.___', $
  ;      'D:\gsar\interseismic\121-d-m4-0467_0472_0477_0482-manyi_manyi2_altyntagh5\f123\sbas.4.0.0700.9999.20141026.20211106.166.1049.01.___', $
  ;      'D:\gsar\interseismic\121-d-m6-0462_0467_0472_0477_0482_0487-atf_kunlun\f123\sbas.4.0.0001.9999.20141026.20230325.048.0732.01.___', $
  ;
  ;
  ;      'D:\gsar\interseismic\026-a-m3-0122_0127_0132-hami\f123\sbas.4.0.0367.9999.20170313.20240827.171.0541.01.___', $
  ;      'D:\gsar\interseismic\026-a-m4-0117_0122_0127_0132-qilian\f123\sbas.4.0.0700.9999.20141019.20210208.140.0888.01.___', $
  ;      'D:\gsar\interseismic\026-a-m5-0112_0117_0122_0127_0132-qilian\f123\sbas.4.0.0367.9999.20141019.20231219.072.0614.01.___', $
  ;
  ;      'D:\gsar\interseismic\099-a-m3-1310_1315_1320-hami\f123\sbas.4.0.0001.9999.20170318.20240808.165.0799.01.___',  $
  ;      'D:\gsar\interseismic\099-a-m4-1305_1310_1315_1320-hami\f123\sbas.4.0.0001.9999.20170318.20240913.166.0800.01.___',  $
  ;      'D:\gsar\interseismic\099-a-m4-0117_0122_0126_0131-atf\f123\sbas.4.0.0367.9999.20141012.20231212.180.0317.01.___',  $
  ;      'D:\gsar\interseismic\099-a-m6-0096_0101_0106_0112_0117_0122-eastkunlun3M3\f123\sbas.4.0.0367.9999.20141012.20210520.156.0337.01.___', $
  ;
  ;      ;    'D:\gsar\interseismic\172-a-m5-1307_1312_1317_1322_1327-hami\f123\sbas.4.0.0700.9999.20170404.20240801.073.0551.01.___', $
  ;      'D:\gsar\interseismic\172-a-m4-1312_1317_1322_1327-hami\f123\sbas.4.0.0001.9999.20170404.20240906.191.0792.01.___', $
  ;      'D:\gsar\interseismic\172-a-m4-0117_0122_0127_0132-alyntagh\f123\sbas.4.0.0001.9999.20150708.20231229.197.0734.01.___', $
  ;      'D:\gsar\interseismic\172-a-m6-0096_0101_0106_0111_0116_0121-eastkunlun7M3\f123\sbas.4.0.0001.9999.20141204.20210513.130.0623.01.___', $
  ;
  ;      'D:\gsar\interseismic\070-a-m4-1312_1317_1322_1327-hami\f123\sbas.4.0.0367.9999.20170316.20240830.194.1033.01.___', $
  ;      'D:\gsar\interseismic\070-a-m4-0115_0120_0125_0130-altyntagh\f123\sbas.4.0.0700.9999.20141022.20210424.149.0964.01.___', $
  ;      'D:\gsar\interseismic\070-a-m6-0100_0105_0110_0115_0120_0125-eastkunlun5M3\f123\sbas.4.0.0001.9999.20141022.20230225.056.0522.01.___', $
  ;
  ;      ;'D:\gsar\interseismic\143-a-m4-0115_0120_0125_0130-altyntagh_M3\f123\sbas.4.0.0367.9999.20141015.20210417.155.1300.01.___', $
  ;      'D:\gsar\interseismic\143-a-m3--0145_0151_0156-altay\f123\sbas.4.0.0001.9999.20141015.20240904.099.0800.01.___',  $
  ;      'D:\gsar\interseismic\143-a-m4-0115_0120_0125_0130-altyntagh_M3\f123\sbas.4.0.0700.9999.20141015.20210417.155.0970.01.___', $
  ;      'D:\gsar\interseismic\143-a-m6-0100_0105_0110_0115_0120_0125-kunlun\f123\sbas.4.0.0001.9999.20141015.20230302.058.0572.01.___',  $
  ;
  ;      'D:\gsar\interseismic\041-a-m3-0145_0150_0155-altay\f123\sbas.4.0.0001.9999.20141020.20240909.131.0761.01.___', $
  ;      'D:\gsar\interseismic\041-a-m5-0125_0130_0135_0140_0145-tarim\f123\sbas.4.0.0001.9999.20141020.20240828.150.0538.01.___', $
  ;      'D:\gsar\interseismic\041-a-m6-0099_0104_0109_0114_0119_0124-altyntagh_kunlun\f123\sbas.4.0.0001.9999.20141020.20230307.062.0303.01.___', $
  ;
  ;      'D:\gsar\interseismic\114-a-m4-0090_0095_0100_0105-gyaringco_ranwu_yadong_gulu2\f123\sbas.4.0.0700.9999.20141106.20210322.150.0974.01.___', $
  ;      'D:\gsar\interseismic\114-a-m6-0100_0105_0110_0115_0120_0125-altyntagh2\f123\sbas.4.0.0001.9999.20150306.20231107.115.0705.01.___',  $
  ;      'D:\gsar\interseismic\114-a-m4-0110_0115_0120_0125-altyntagh2\f123\sbas.4.0.0700.9999.20141106.20210403.146.0904.01.___' $
  ;        ]
  ;    file_gnss='D:\gsar\gic3dv\hami\asc_des.tarim\gps_prd'
  ;    ofile='D:\gsar\gic3dv\hami\asc_des\insar_3d_20241006.psvelou'
  ;
  ;    paths=['D:\gsar\interseismic\033-d-m5-0483_0488_0493_0498_0503-chuanzang_batangM3\f123\sbas.4.0.0367.9999.20150710.20240217.114.0538.01.___',  $
  ;      'D:\gsar\interseismic\099-a-m4-0087_0092_0097_0102-chuanzang_batang3M3\f123\sbas.4.0.0367.9999.20141012.20240210.191.0378.01.___']
  ;    file_gnss='D:\gsar\gic3dv\kunlun\asc_des\gps_prd'
  ;    ofile='D:\gsar\gic3dv\jss\asc_des\insar_3d_20240924.psvelou'
  ;
  ;    ;    ;altay
  ;    paths=['D:\gsar\interseismic\019-d-m3-0429_0434_0439-altay\f123\sbas.4.0.0001.9999.20150616.20210310.094.0455.01.___',  $
  ;      'D:\gsar\interseismic\143-a-m3--0145_0151_0156-altay\f123\sbas.4.0.0001.9999.20141015.20240904.099.0800.01.___' ,  $
  ;      'D:\gsar\interseismic\092-d-m3-0425_0430_0435-altay\f123\sbas.4.0.0001.9999.20170324.20211216.066.0772.01.___']
  ;    file_gnss='D:\gsar\gic3dv\hami\asc_des.tarim\gps_prd'
  ;    ofile='D:\gsar\gic3dv\altay\asc_des\insar_3d_20240928.psvelou'
  ;
  ;    ;    ;gozha co
  ;    file_list='D:\gsar\gic3dv\gozhaco\asc_des\sbas.list.txt'
  ;    paths=read_txt(file_list,comment='~ ')
  ;    paths=STRTRIM(paths,2)
  ;    ;    file_gnss='C:\GMT_pub\gps\wang.min.jgr2020\rotinv.western_tibet\resf.psvelo'
  ;    ;    fmt_gnss='psvelo'
  ;    file_gnss='D:\gsar\gic3dv\g219\asc_des\gps_prd'
  ;    ofile='D:\gsar\gic3dv\gozhaco\asc_des\insar_3d_20250204.psvelou'
  ;
  ;  ;
  ;
  ;  ;    ;karakoram
  ;      file_list='D:\gsar\gic3dv\karakoram\asc_des\sbas.list.txt'
  ;      file_list='D:\gsar\gic3dv\karakoram\asc_des\sbas.list.20250525.txt'
  ;      file_list='D:\gsar\gic3dv\karakoram\asc_des\sbas.list.20250606.txt'
  ;      paths=read_txt(file_list,comment='~ ')
  ;      paths=STRTRIM(paths,2)
  ;      ;        paths=['D:\gsar\interseismic\129-a-m5-0089_0094_0099_0104_109-mht\f123\sbas.4.0.0001.9999.20150530.20250407.135.0202.01.___' ,  $
  ;      ;        'D:\gsar\interseismic\056-a-m4-0088_0093_0098_0103-karakoram1_mht\f123\sbas.4.0.0367.9999.20150501.20240219.218.0314.01.___', $
  ;      ;        ;'D:\gsar\interseismic\165-d-m5-0478_0483_0488_0494_0499-mht\f123\sbas.4.0.0367.9999.20150509.20231018.127.0440.01.___', $
  ;      ;        ;'D:\gsar\interseismic\063-d-m5-0478_0483_0487_0492_0497-mht\f123\sbas.4.0.0001.9999.20160917.20230917.091.0800.01.___', $
  ;      ;        'D:\gsar\interseismic\063-d-m3-0483_0487_0492-mht\f123\sbas.4.0.0367.9999.20141022.20220206.149.1620.01.___', $
  ;      ;        'D:\gsar\interseismic\136-d-m5-0476_0481_0486_0491_0496-mht\f123\sbas.4.0.0367.9999.20150928.20231016.116.0468.01.___' ]
  ;      ;    ;    ;    file_gnss='D:\gsar\gic3dv\karakoram\asc_des\gps_prd'
  ;      ;    ;;    ofile='D:\gsar\gic3dv\karakoram\asc_des\insar_3d_20250213.psvelou'
  ;      ;    ;    file_gnss='C:\GMT_pub\gps\wang.min.jgr2020\rotinv.indian_plate\resf.psvelo'
  ;      ;    ;    fmt_gnss='psvelo'
  ;      ;    ;    ofile='D:\gsar\gic3dv\karakoram\asc_des.india\insar_3d_20250213.psvelou'
  ;      ;    ;
  ;      file_gnss='C:\GMT_pub\gps\wang.min.jgr2020\rotinv.western_tibet\resf.psvelo'
  ;      fmt_gnss='psvelo'
  ;      ofile='D:\gsar\gic3dv\karakoram\asc_des\insar_3d_20250606_western_tibet.psvelou'
    
    
  ;    ; gcf with densified NINH network + interpolation; with GPS vertical
  ;    file_list='D:\gsar\gic3dv\gcf\asc_des\sbas_list_20250613.txt'
  ;    paths=read_txt(file_list,comment='~ ')
  ;    paths=STRTRIM(paths,2)
  ;    file_gnss='D:\gsar\gic3dv\jiali\interp\asc_des\gps_prd'
  ;    ofile='D:\gsar\gic3dv\gcf\asc_des\insar_3d-20250613.psvelo'
    
    
    
    
  ;    ;validation of GCF's motion at its eastern terminus
  ;  ;    ;d048+a114
  ;  ;    ;horizontal velocity field (psvelo) relative to eastern Lhasa Block
  ;       file_list='D:\gsar\gic3dv\gcf\asc_des\sbas_list_20250617_bcf.txt'
  ;       file_list='D:\gsar\gic3dv\gcf\asc_des\sbas_list_20250626_bcf.txt'
  ;       file_list='D:\gsar\gic3dv\gcf\asc_des\sbas_list_20250629_bcf.txt'
  ;       file_list='D:\gsar\gic3dv\gcf\asc_des\sbas_list_20250703_bcf.txt'
  ;      paths=read_txt(file_list,comment='~ ')
  ;      paths=STRTRIM(paths,2)
  ;;      file_gnss='C:\GMT_pub\gps\wang.min.jgr2020\rotinv.eastern_lhasa_block\resf.psvelo'
  ;;      file_gnss='C:\GMT_pub\gps\wang.min.jgr2020\rotinv.western_tibet\resf.psvelo'
  ;      ;file_gnss='C:\GMT_pub\gps\wang.min.jgr2020\rotinv.indian_plate\resf.psvelo'
  ;      file_gnss='C:\GMT_pub\gps\wang.min.jgr2020\rotinv.kunlun2\resf.psvelo'
  ;      fmt_gnss='psvelo'
  ;      ;file_gnss='D:\gsar\gic3dv\jiali\interp\asc_des2\gps_prd'
  ;      ;file_gnss='D:\gsar\gic3dv\jiali\interp\asc_des2\gps_prd2'
  ;      ofile='D:\gsar\gic3dv\gcf\asc_des\insar_3d-20250629_bcf.psvelo'
  ;      ofile='D:\gsar\gic3dv\gcf\asc_des\insar_3d-20250630_bcf2.psvelo' ;no GPS up
  ;      ofile='D:\gsar\gic3dv\gcf\asc_des\insar_3d-20250703_bcf.psvelo' ;no GPS up; relative to Indian plate
  ;
  ;    ;    ;d062+a128+...
  ;    ;    longmenshan dayi gap
  ;    file_list='D:\gsar\gic3dv\dayi\asc_des\sbas_list_20250707.txt'
  ;    paths=read_txt(file_list,comment='~ ')
  ;    paths=STRTRIM(paths,2)
  ;    file_gnss='C:\GMT_pub\gps\wang.min.jgr2020\rotinv.kunlun2\resf.psvelo'
  ;    file_gnss='C:\GMT_pub\gps\wang.min.jgr2020\rotinv.sichuan\resf.psvelo'
  ;    fmt_gnss='psvelo'
  ;    ofile='D:\gsar\gic3dv\dayi\asc_des\insar_3d-20250707.psvelo'
    
  ;  ;woniuhu
  ;    file_list='D:\gsar\gic3dv\woniuhu\asc_des\sbas.list.20250719.txt'
  ;    paths=read_txt(file_list,comment='~ ')
  ;    paths=STRTRIM(paths,2)
  ;    ;paths=paths[0]
  ;    HELP, paths
  ;    file_gnss='C:\GMT_pub\gps\wang.min.jgr2020\rotinv.tarim3\resf.psvelo'
  ;    fmt_gnss='psvelo'
  ;    ofile='D:\gsar\gic3dv\woniuhu\asc_des\insar_3d_20250719.psvelo'
  ;    dlon=9d-3
    
  ;     file_list='D:\gsar\gic3dv\xsh\asc_des\sbas.list.20250815.txt'
  ;    paths=read_txt(file_list,comment='~ ')
  ;    paths=STRTRIM(paths,2)
  ;    ;paths=paths[0]
  ;    HELP, paths
  ;    ;file_gnss='C:\GMT_pub\gps\wang.min.jgr2020\rotinv.kunlun2\resf.psvelo'
  ;    file_gnss='C:\GMT_pub\gps\wang.min.jgr2020\rotinv.sichuan\resf.psvelo'
  ;;    file_gnss='C:\GMT_pub\gps\wang.min.jgr2020\rotinv.chuandian2\resf.psvelo'
  ;;    file_gnss='C:\GMT_pub\gps\wang.min.jgr2020\rotinv.litang2\resf.psvelo'
  ;;    file_gnss='C:\GMT_pub\gps\wang.min.jgr2020\rotinv.aba2\resf.psvelo'
  ;    fmt_gnss='psvelo'
  ;    ofile='D:\gsar\gic3dv\xsh\asc_des\insar_3d_20250815_Nzero.psvelo'
  ;;    dlon=9d-3
  
;  ;2021 maduo postseismic  
;    ;
;    file_list='D:\gsar\gic3dv\maduo\asc_des\sbas_list_20250929.txt'
;    paths=read_txt(file_list,comment='~ ')
;    paths=STRTRIM(paths,2)
;    file_gnss='C:\GMT_pub\gps\wang.min.jgr2020\rotinv.kunlun2\resf.psvelo'
;    fmt_gnss='psvelo'
;    ofile='D:\gsar\gic3dv\maduo\asc_des\insar3d_20251004.psvelou'
;    
  ;2022 menyuan postseismic  
    file_list='D:\gsar\gic3dv\menyuan\asc_des\sbas_list_20251010.txt'
    paths=read_txt(file_list,comment='~ ')
    paths=STRTRIM(paths,2)
    file_gnss='C:\GMT_pub\gps\wang.min.jgr2020\rotinv.kunlun2\resf.psvelo'
    fmt_gnss='psvelo'
    ofile='D:\gsar\gic3dv\menyuan\asc_des\insar3d_20251010.psvelou'
    
  ENDIF
  
  IF N_ELEMENTS(fmt_gnss) EQ 0 THEN fmt_gnss='gps_prd'
  
  IF N_ELEMENTS(dlon) EQ 0 THEN BEGIN
    dlon=.009d0
    dlon=9d-3
    ;    dlon=9d-2
    ;    dlon=5d-2
    dlon=2d-2
  ENDIF
  dlat=dlon
  
  ;first, read in all data
  ntrack=N_ELEMENTS(paths)
  pdata=REPLICATE(PTR_NEW(),ntrack)
  plook=REPLICATE(PTR_NEW(),ntrack)
  rects=DBLARR(4,ntrack)
  
  
  FOR ti=0, ntrack-1 DO BEGIN
    path=paths[ti]
    PRINT,'['+PROG+']INFO:reading data '+STRTRIM(ti+1,2)+"/"+STRTRIM(ntrack,2)+' in '+path+' ...'
    ;break
    file_vel=path+PATH_SEP()+'vel_mask_ll_gnss3.xyze'
    file_look=path+PATH_SEP()+'look_mask_ll3.xyz'
    READ_COLS, file_vel, data=insar_vel
    READ_COLS, file_look, data=insar_look
    n_col_look=N_ELEMENTS(insar_look[*,0])
    IF n_col_look NE 8 THEN STOP
    HELP, insar_look,insar_vel
    pdata[ti]=PTR_NEW(insar_vel)
    plook[ti]=PTR_NEW(insar_look)
    xmin1=MIN(insar_vel[0,*], max=xmax1)
    ymin1=MIN(insar_vel[1,*], max=ymax1)
    rects[*,ti]=[xmin1,xmax1,ymin1,ymax1]
  ENDFOR
  xmin=MIN(rects[0,*])
  xmax=MAX(rects[1,*])
  ymin=MIN(rects[2,*])
  ymax=MAX(rects[3,*])
  ;
  ;;  ;test only
  ;  xmin=80
  ;  xmax=100
  ;  ymin=26
  ;  ymax=37
  PRINT,'rect:',xmin,xmax,ymin,ymax
  ;stop
  
  
  ;first, resample all data to the same grid
  ;get grid coordinates
  nx=CEIL((xmax-xmin)/dlon)
  ny=CEIL((ymax-ymin)/dlat)
  xs=INDGEN(nx)*dlon+xmin
  ys=INDGEN(ny)*dlat+ymin
  
  
  PRINT,'reading gnss velocities in '+fmt_gnss+' format ...'
  CASE fmt_gnss OF
    'gps_prd': BEGIN
      READ_COLS, file_gnss, data=gnss_vel, skip=1
      x=REFORM(gnss_vel[0,*])
      y=REFORM(gnss_vel[1,*])
      z=REFORM(gnss_vel[4,*]) ;gic3dv gps_prd format
    END
    'psvelo': BEGIN
      READ_COLS, file_gnss, data=gnss_vel
      x=REFORM(gnss_vel[0,*])
      y=REFORM(gnss_vel[1,*])
      z=REFORM(gnss_vel[3,*]) ;psvelo format
    END
    ELSE: BEGIN
      PRINT,'['+prog+']ERROR: invalid GNSS format ('+fmt_gnss+')!!'
      RETURN
    END
  ENDCASE
  
  GRID_INPUT, x, y, z, x1,y1,z1, /degree,  epsilon = 0.5
  QHULL, x1, y1, tri, /delaunay;, /sphere
  gnss_vel_n = GRIDDATA( x1, y1, z1,  $
    /grid, xout = xs, yout = ys, $
    method = 'NaturalNeighbor', $
    triangles=tri, $
    ;SEARCH_ELLIPSE = 3*dlon,  $
    ;MIN_POINTS =2, $
    /degree, $
    missing = !values.D_NAN)
  ;
  ;zeroN, Zero North, no GNSS north constraint
;  gnss_vel_n[*]=0d0
  
  ;  window,1,ysize=1000
  ;  tvscl,gnss_vel_n
  ;
  ;  ;stop
  ;
  ;  ;Output NetCDF file
  ;  ofile_grd_n=desuffix(ofile)+'_north_interp.grd'
  ;  id = NCDF_CREATE(ofile_grd_n, /CLOBBER)
  ;  ; Fill the file with default values:
  ;  NCDF_CONTROL, id, /FILL
  ;  xid = NCDF_DIMDEF(id, 'lon', nx)    ; Make dimensions.
  ;  yid = NCDF_DIMDEF(id, 'lat', ny)    ; Make dimensions.
  ;  zid = NCDF_DIMDEF(id, 'z', nx*ny)
  ;  ; Define variables:
  ;  aid = NCDF_VARDEF(id, 'lon', [xid], /double)
  ;  bid = NCDF_VARDEF(id, 'lat', [yid], /double)
  ;  vid = NCDF_VARDEF(id, 'Vn_interp', [xid,yid], /double)
  ;  NCDF_ATTPUT, id, aid, 'units', 'degree_east'
  ;  NCDF_ATTPUT, id, aid, 'long_name', 'Longitude'
  ;  NCDF_ATTPUT, id, bid, 'units', 'degree_north'
  ;  NCDF_ATTPUT, id, bid, 'long_name', 'Latitude'
  ;  NCDF_ATTPUT, id, vid, 'units', 'mm/a'
  ;  NCDF_ATTPUT, id, vid, 'long_name', 'North Velocity Interpolated'
  ;  NCDF_ATTPUT, id, /GLOBAL, 'Title', 'Vn_interp'
  ;  ; Put file in data mode:
  ;  NCDF_CONTROL, id, /ENDEF
  ;  ; Input data:
  ;  NCDF_VARPUT, id, aid, xs
  ;  NCDF_VARPUT, id, bid, ys
  ;  NCDF_VARPUT, id, vid, gnss_vel_n
  ;  NCDF_CLOSE, id ; Close the NetCDF file.
  ;  help, xs,ys,nx,ny,gnss_vel_n
  ;  return
  
  PRINT,'regridding insar data ...'
  insar_vel_all=DBLARR(nx,ny,ntrack)
  insar_vel_all[*]=!values.D_NAN
  insar_look_all=insar_vel_all
  insar_azimuth_all=insar_vel_all
  
  FOR ti=0, ntrack-1 DO BEGIN
    x=REFORM((*pdata[ti])[0,*])
    y=REFORM((*pdata[ti])[1,*])
    z=REFORM((*pdata[ti])[2,*])
    look=REFORM((*plook[ti])[6,*])
    azimuth=REFORM((*plook[ti])[7,*])
    
    FOR i=0ull,N_ELEMENTS(x)-1 DO BEGIN
      IF x[i] LT xmin OR x[i] GT xmax OR y[i] LE ymin OR y[i] GT ymax THEN CONTINUE
      indi=ROUND( (x[i]-xmin)/dlon ) < nx-1
      indi=indi > 0
      indj=ROUND( (y[i]-ymin)/dlat ) < ny-1
      indj=indj > 0
      IF FINITE(insar_vel_all[indi,indj,ti]) EQ 0 THEN BEGIN
        insar_vel_all[indi,indj,ti]=z[i]
        insar_look_all[indi,indj,ti]=look[i]
        insar_azimuth_all[indi,indj,ti]=azimuth[i]
      ;print,'found',indi,indj,z[ti],insar_vel_all[indi,indj,ti]
      ENDIF ELSE BEGIN
        insar_vel_all[indi,indj,ti]=MEAN([ insar_vel_all[indi,indj,ti],z[i] ])
        insar_look_all[indi,indj,ti]=MEAN([ insar_look_all[indi,indj,ti],look[i] ])
        insar_azimuth_all[indi,indj,ti]=MEAN([ insar_azimuth_all[indi,indj,ti],azimuth[i] ])
      ;print,'averaging points',indi,indi,insar_vel_all[indi,indj,ti],z[i],insar_vel_all[indi,indj,ti]
      ENDELSE
    ENDFOR
  ;WINDOW,1
  ;TVSCL,reform(insar_vel_all[*,*,ti]),/nan
  ;STOP
  ENDFOR
  ;STOP
  
  
  out_ns=DBLARR(nx,ny)
  out_ns[*]=!values.D_NAN
  out_es=out_ns
  out_us=out_ns
  
  np10=FIX(nx/10)
  
  PRINT,'decomposing velocities ...'
  FOR ii=0ull,nx-1 DO BEGIN ;loop for each pixel
    PRINT,ii+1,' /',nx
    IF (ii+1 MOD np10) EQ 0 THEN BEGIN
      PRINT,'['+PROG+']INFO: '+STRING((ii+1d0)/nx*100,format='(f6.2,"%")')
    ENDIF
    
    FOR jj=0ull, ny-1 DO BEGIN
      ind1=WHERE(FINITE(insar_azimuth_all[ii,jj,*]) EQ 1)
      IF N_ELEMENTS(ind1) LT 2 THEN CONTINUE  ;less than two views
      ind2=WHERE(insar_azimuth_all[ii,jj,ind1] GT 0)
      IF ind2[0] EQ -1 THEN CONTINUE  ;no ascending data
      ind3=WHERE(insar_azimuth_all[ii,jj,ind1] LT 0)
      IF ind3[0] EQ -1 THEN CONTINUE  ;no descending data
      
      ;;;if n_elements(ind1) lt 3 then continue ;debug-only
      
      LOS_2_3D_BY_GPSN, insar_vel_all[ii,jj,ind1],  $
        insar_look_all[ii,jj,ind1], $
        insar_azimuth_all[ii,jj,ind1],  $
        gnss_vel_n[ii,jj],  $
        east=east,  $
        north=north,  $
        up=up
        
      ;STOP
        
      out_ns[ii,jj]=north
      out_es[ii,jj]=east
      out_us[ii,jj]=up
    ENDFOR
    
    
  ENDFOR
  
  
  
  ;Output NetCDF file
  ofile_grd_n=desuffix(ofile)+'_north.grd'
  id = NCDF_CREATE(ofile_grd_n, /CLOBBER)
  ; Fill the file with default values:
  NCDF_CONTROL, id, /FILL
  xid = NCDF_DIMDEF(id, 'lon', nx)    ; Make dimensions.
  yid = NCDF_DIMDEF(id, 'lat', ny)    ; Make dimensions.
  zid = NCDF_DIMDEF(id, 'z', nx*ny)
  ; Define variables:
  aid = NCDF_VARDEF(id, 'lon', [xid], /double)
  bid = NCDF_VARDEF(id, 'lat', [yid], /double)
  vid = NCDF_VARDEF(id, 'Vn', [xid,yid], /double)
  NCDF_ATTPUT, id, aid, 'units', 'degree_east'
  NCDF_ATTPUT, id, aid, 'long_name', 'Longitude'
  NCDF_ATTPUT, id, bid, 'units', 'degree_north'
  NCDF_ATTPUT, id, bid, 'long_name', 'Latitude'
  NCDF_ATTPUT, id, vid, 'units', 'mm/a'
  NCDF_ATTPUT, id, vid, 'long_name', 'North Velocity'
  NCDF_ATTPUT, id, /GLOBAL, 'Title', 'Vn'
  ; Put file in data mode:
  NCDF_CONTROL, id, /ENDEF
  ; Input data:
  NCDF_VARPUT, id, aid, xs
  NCDF_VARPUT, id, bid, ys
  NCDF_VARPUT, id, vid, out_ns
  NCDF_CLOSE, id ; Close the NetCDF file.
  
  ;Output NetCDF file
  ofile_grd_n=desuffix(ofile)+'_east.grd'
  id = NCDF_CREATE(ofile_grd_n, /CLOBBER)
  ; Fill the file with default values:
  NCDF_CONTROL, id, /FILL
  xid = NCDF_DIMDEF(id, 'lon', nx)    ; Make dimensions.
  yid = NCDF_DIMDEF(id, 'lat', ny)    ; Make dimensions.
  zid = NCDF_DIMDEF(id, 'z', nx*ny)
  ; Define variables:
  aid = NCDF_VARDEF(id, 'lon', [xid], /double)
  bid = NCDF_VARDEF(id, 'lat', [yid], /double)
  vid = NCDF_VARDEF(id, 'Ve', [xid,yid], /double)
  NCDF_ATTPUT, id, aid, 'units', 'degree_east'
  NCDF_ATTPUT, id, aid, 'long_name', 'Longitude'
  NCDF_ATTPUT, id, bid, 'units', 'degree_north'
  NCDF_ATTPUT, id, bid, 'long_name', 'Latitude'
  NCDF_ATTPUT, id, vid, 'units', 'mm/a'
  NCDF_ATTPUT, id, vid, 'long_name', 'East Velocity'
  NCDF_ATTPUT, id, /GLOBAL, 'Title', 'Ve'
  ; Put file in data mode:
  NCDF_CONTROL, id, /ENDEF
  ; Input data:
  NCDF_VARPUT, id, aid, xs
  NCDF_VARPUT, id, bid, ys
  NCDF_VARPUT, id, vid, out_es
  NCDF_CLOSE, id ; Close the NetCDF file.
  
  
  ;Output NetCDF file
  ofile_grd_n=desuffix(ofile)+'_up.grd'
  id = NCDF_CREATE(ofile_grd_n, /CLOBBER)
  ; Fill the file with default values:
  NCDF_CONTROL, id, /FILL
  xid = NCDF_DIMDEF(id, 'lon', nx)    ; Make dimensions.
  yid = NCDF_DIMDEF(id, 'lat', ny)    ; Make dimensions.
  zid = NCDF_DIMDEF(id, 'z', nx*ny)
  ; Define variables:
  aid = NCDF_VARDEF(id, 'lon', [xid], /double)
  bid = NCDF_VARDEF(id, 'lat', [yid], /double)
  vid = NCDF_VARDEF(id, 'Vu', [xid,yid], /double)
  NCDF_ATTPUT, id, aid, 'units', 'degree_east'
  NCDF_ATTPUT, id, aid, 'long_name', 'Longitude'
  NCDF_ATTPUT, id, bid, 'units', 'degree_north'
  NCDF_ATTPUT, id, bid, 'long_name', 'Latitude'
  NCDF_ATTPUT, id, vid, 'units', 'mm/a'
  NCDF_ATTPUT, id, vid, 'long_name', 'Up Velocity'
  NCDF_ATTPUT, id, /GLOBAL, 'Title', 'Vu'
  ; Put file in data mode:
  NCDF_CONTROL, id, /ENDEF
  ; Input data:
  NCDF_VARPUT, id, aid, xs
  NCDF_VARPUT, id, bid, ys
  NCDF_VARPUT, id, vid, out_us
  NCDF_CLOSE, id ; Close the NetCDF file.
  
  
  PRINT,'writing output file ...'
  OPENW, fid, ofile, /get_lun
  ;  WRITE_SYS_INFO, fid, prog=prog, src=[file_asc, file_des]
  ;  PRINTF,fid,'lon','lat','east','north','sig_east','sig_north', 'corr_en','site',  $
  ;    format='("*",2(1x,a15),4(1x,a9),1(1x,a6),(1x,a8))'
  FOR ii=0,nx-1 DO BEGIN
    FOR jj=0,ny-1 DO BEGIN
      IF FINITE(out_es[ii,jj]) EQ 0 THEN CONTINUE
      INIT_GNSS_SITE_NAME, ii*ny+jj, site_name=site_name
      PRINTF,fid,xs[ii],ys[jj],out_es[ii,jj],out_ns[ii,jj],0,0,0,site_name, out_us[ii,jj],  $
        format='(1x,2(1x,f15.5),4(1x,f9.2),1(1x,f6.2),(1x,a4,"_SAR"),1(1x,f9.2))'
    ENDFOR
  ENDFOR
  FREE_LUN, fid
  
  ;  OPENW, fid, ofile2, /get_lun
  ;  WRITE_SYS_INFO, fid, prog=prog, src=[file_asc, file_des]
  ;  PRINTF,fid,'lon','lat','east','up','sig_east','sig_up', 'corr_en','site',  $
  ;    format='("*",2(1x,a15),4(1x,a9),1(1x,a6),(1x,a8))'
  ;  FOR ia=0ull, N_ELEMENTS(pos)-1 DO BEGIN
  ;    posi=pos[ia]
  ;    INIT_GNSS_SITE_NAME, ia, site_name=site_name
  ;    PRINTF,fid,lon_as[posi],lat_as[posi],out_es_3d[posi],out_us_3d[posi],0,0,0,site_name,  $
  ;      format='(1x,2(1x,f15.5),4(1x,f9.2),1(1x,f6.2),(1x,a4,"_SAR"))'
  ;  ENDFOR
  ;  FREE_LUN, fid
  ;;  OPENW, fid, ofile3, /get_lun
  ;;  WRITE_SYS_INFO, fid, prog=prog, src=[file_asc, file_des]
  ;;  PRINTF,fid,'lon','lat','up','sig_up','site',  $
  ;;    format='("*",2(1x,a15),2(1x,a9),(1x,a8))'
  ;;  FOR ia=0ull, N_ELEMENTS(pos)-1 DO BEGIN
  ;;    posi=pos[ia]
  ;;    INIT_GNSS_SITE_NAME, ia, site_name=site_name
  ;;    PRINTF,fid,lon_as[posi],lat_as[posi],out_us_3d[posi],0,site_name,  $
  ;;      format='(1x,2(1x,f15.5),2(1x,f9.2),(1x,a4,"_SAR"))'
  ;;  ENDFOR
  ;;  FREE_LUN, fid
  ;;STOP
  ;;STOP
  
  PRINT,'['+prog+']Normal end.'
  
END