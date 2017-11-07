

PRO SAR_S1_MANIFEST_OVERLAPPING, path=path, ofile=ofile, target=target, xys=xys, names=names,  $
    onames=onames, perc_min=perc_min, files=files
    
  PROG=(STRSPLIT(LAST(SCOPE_TRACEBACK()),/EXTRACT))[0]
  
  IF N_ELEMENTS(xys) EQ 0 THEN BEGIN
    path='C:\Downloads\esa.data\S1'
    ;path='C:\Downloads\esa.data\S1\tmp'
    ;    path='\\gpsac4\root\g4d\esa.data\S1'
    ;    path='\\gpsac5\root\h2\esa.data\S1'
    ;
    ;path='C:\Downloads\esa.data\manifest.tibet\MANIFEST'
    
    
    ;    target='S1A_IW_SLC__1SSV_20160922T235330_20160922T235358_013172_014F0A_E1C4.manifest.safe'
    ;    target='S1A_IW_SLC__1SDV_20151027T115741_20151027T115808_008338_00BC3D_AF24.manifest.safe'
    ;    target='S1A_IW_SLC__1SSV_20150612'
    ;    target='S1A_IW_SLC__1SSV_20141020T115800_20141020T115827_002913_0034D7_89AC'
    ;    ;gyaring co
    ;    target='S1A_IW_SLC__1SSV_20161213'
    ;    ;u1
    ;    target='S1A_IW_SLC__1SSV_20150612'
    ;    opath='D:\gsar\u1'
    ;    ;dangxiong section of gulu rift
    ;    target='S1A_IW_SLC__1SSV_20141020T115735_20141020T115803_002913_0034D7_A1DD'
    ;    target='S1A_IW_SLC__1SSV_20160530T115739_20160530T115807_011488_01182B_78BE.manifest.safe'
    ;    opath='D:\gsar\asc\dangxiong'
    ;    perc_min=.9d0
    ;  ;beijing
    ;  target='S1A_IW_SLC__1SSV_20141008T222018_20141008T222048_002744_003146_40B4.manifest.safe'
    ;  opath='D:\gsar\bj'
    ;  ;kunlun
    ;  target='S1A_IW_SLC__1SSV_20141103T234348_20141103T234415_003124_003966_7536.manifest.safe'
    ;  opath='D:\gsar\kunlun'
    ;;  ;u1b
    ;;  target='S1A_IW_SLC__1SSV_20150217T115758_20150217T115824_004663_005C14_4687.manifest.safe'
    ;;  opath='D:\gsar\anduo'
    ;;  ;bengco
    ;;  target='S1A_IW_SLC__1SSV_20150512T115759_20150512T115827_005888_007951_509A.manifest.safe'
    ;;  target='S1A_IW_SLC__1SSV_20160224T115758_20160224T115825_010088_00EDD0_5441.manifest.safe'
    ;;  opath='D:\gsar\bengco'
    ;;  ;anduo-sewa
    ;;  target='S1A_IW_SLC__1SSV_20160903T115834_20160903T115901_012888_0145C7_7832.manifest.safe'
    ;    target='S1A_IW_SLC__1SSV_20151016T000120_20151016T000147_008170_00B7AC_4A9B.manifest.safe'
    ;    target='S1A_IW_SLC__1SSV_20160401T000112_20160401T000140_010620_00FCF1_019C'
    ;    opath='D:\gsar\des\sewa1'
    ;    ;;  target='S1A_IW_SLC__1SSV_20160131T115823_20160131T115850_009738_00E39A_B9FC.manifest.safe'
    ;    ;;  target='S1A_IW_SLC__1SSV_20141113T115825_20141113T115852_003263_003C61_1833.manifest.safe'
    ;    ;;  opath='D:\gsar\sewa2'
    ;    target='S1A_IW_SLC__1SSV_20150308T114948_20150308T115015_004940_0062C6_7BAC.manifest.safe'
    ;    opath='D:\gsar\asc\nujiang'
    ;;
    target='S1A_IW_SLC__1SSV_20141126T000105_20141126T000131_003445_004074_13F0.manifest.safe'
    opath='D:\gsar\des\sewa3'
    ;;
    target='S1A_IW_SLC__1SDV_20141022T234555_20141022T234624_002949_003598_FA46.manifest.safe'
    opath='D:\gsar\des\cona1'
    ;perc_min=.85d0
    ;;;
    ;;  target='S1A_IW_SLC__1SSV_20141027T235250_20141027T235317_003022_003721_39DA.manifest.safe'
    ;;  opath='D:\gsar\nujiang2'
    ;;
    ;        target='S1A_IW_SLC__1SSV_20161002T120628_20161002T120655_013311_015371_1887'
    ;            target='S1A_IW_SLC__1SSV_20151125T120620_20151125T120647_008761_00C7B8_AA01.manifest.safe'
    ;    ;        perc_min=.99
    ;            opath='D:\gsar\asc\gyaringco'
    ;   target='S1A_IW_SLC__1SSV_20141213T000942_20141213T001009_003693_004625_C38A.manifest.safe'
    ;   opath='D:\gsar\des\gyaringco2'
    ;target='S1A_IW_SLC__1SSV_20141022T114253_20141022T114320_002942_003578_671D.manifest.safe'
    ;    target='S1A_IW_SLC__1SSV_20141022T114253_20141022T114320_002942_003578_671D.manifest'
    ;    opath='D:\gsar\asc\qaidam1'
    target='S1A_IW_SLC__1SSV_20141113T115706_20141113T115736_003263_003C61_4990.manifest.safe'
    opath='D:\gsar\asc\yzs1'
    ;
    ;target='S1A_IW_SLC__1SSV_20141213T001006_20141213T001033_003693_004625_245D.manifest.safe'
    ;opath='D:\gsar\yzs2'
    ;target='S1A_IW_SLC__1SSV_20141009T000016_20141009T000042_002745_00314A_0DD6.manifest.safe'
    ;opath='D:\gsar\kunlun2'
    ;    target='S1A_IW_SLC__1SSV_20141214T235159_20141214T235226_003722_0046D6_653A.manifest.safe'
    ;    opath='D:\gsar\des\kunlun3'
    ;    target='S1A_IW_SLC__1SSV_20160805T235328_20160805T235356_012472_0137E8_4141.manifest.safe'
    ;    target='S1A_IW_SLC__1SSV_20141027T235315_20141027T235343_003022_003721_2072'
    ;    target='S1A_IW_SLC__1SSV_20141027T235315_20141027T235343_003022_003721_2072.m'
    ;    opath='D:\gsar\des\gulu1'
    ;    ;    perc_min=.906d
    ;        perc_min=.906d
    ;    ;
    ;    target='S1A_IW_SLC__1SSV_20141008T235925_20141008T235953_002745_00314A_7557.manifest.safe'
    ;    opath='D:\gsar\altyntagh'
    ;
    ;    target='S1A_IW_SLC__1SSV_20160922T235330_20160922T235358_013172_014F0A_E1C4'
    ;    opath='D:\gsar\bengco.des'
    ;        target='S1A_IW_SLC__1SSV_20150907T121458_20150907T121525_007609_00A89D_EE8D.manifest.safe'
    ;        opath='D:\gsar\asc\riganpei.asc'
    ;        target='S1A_IW_SLC__1SSV_20150122T121439_20150122T121506_004284_005368_B815.manifest.safe'
    ;        opath='D:\gsar\asc\riganpei2.asc'
    ;    perc_min=.68
    ;perc_min=.86
    ;    target='S1A_IW_SLC__1SSV_20141027T235250_20141027T235317_003022_003721_39DA.manifest.safe'
    ;    opath='D:\gsar\zaduo.des'
    ;    ;the same as nujiang2; removed.
    ;    ;
    ;    target='S1A_IW_SLC__1SSV_20141009T000130_20141009T000158_002745_00314A_EC39.manifest.safe'
    ;    opath='D:\gsar\gyaringco3'
    ;    ;
    ;    target='S1A_IW_SLC__1SSV_20141106T120546_20141106T120614_003161_003A27_B103.manifest.safe'
    ;    opath='D:\gsar\asc\ranwu'
    ;    ;
    ;      target='S1A_IW_SLC__1SSV_20141102T000040_20141102T000107_003095_0038C0_C1EB.manifest.safe'
    ;      opath='D:\gsar\des\wulan1'
    ;    ;
    ;    target='S1A_IW_SLC__1SSV_20141207T115849_20141207T115916_003613_00445F_3EA7.manifest.safe'
    ;    opath='D:\gsar\asc\wulan2'
    ;    ;
    ;    ;    target='S1A_IW_SLC__1SSV_20141106T120751_20141106T120819_003161_003A27_D6AA.manifest.safe'
    ;    target='S1A_IW_SLC__1SSV_20141106T120751_20141106T120819_003161_003A27_D6AA.manifest'
    ;    opath='D:\gsar\asc\altyntagh2'
    ;    target='S1A_IW_SLC__1SSV_20141116T122359_20141116T122426_003307_003D4D_1FFA.manifest.safe'
    ;    opath='D:\gsar\asc\altyntagh3'
    ;    perc_min=.63d0
    ;    target='S1A_IW_SLC__1SSV_20141015T115155_20141015T115223_002840_00333F_7C09.manifest.safe'
    ;    opath='D:\gsar\asc\altyntagh4'
    ;    ;
    ;    target='S1A_IW_SLC__1SSV_20141009T000156_20141009T000223_002745_00314A_8793.manifest.safe'
    ;    opath='D:\gsar\des\ranwu2'
    ;    ;
    ;    target='S1A_IW_SLC__1SSV_20141027T235340_20141027T235407_003022_003721_16E8.manifest.safe'
    ;    target='S1A_IW_SLC__1SSV_20160525T235350_20160525T235417_011422_011607_6789.manifest.safe'
    ;    opath='D:\gsar\des\dangxiong2'
    ;
    ;    target='S1A_IW_SLC__1SSV_20141116T122359_20141116T122426_003307_003D4D_1FFA.manifest.safe'
    ;    target='S1A_IW_SLC__1SSV_20150515T122348_20150515T122416_005932_007A3F_8190'
    ;    opath='D:\gsar\asc\altyntagh3'
    
    ;    target='S1A_IW_SLC__1SDV_20150131T235302_20150131T235330_004422_005687_0AAC.manifest.safe'
    ;    opath='d:\tmp'
    ;
    ;    target='S1A_IW_SLC__1SSV_20141027T235225_20141027T235252_003022_003721_49D7.manifest.safe'
    ;    opath='D:\gsar\des\wulan2'
    ;
    ;    target='S1A_IW_SLC__1SSV_20141026T000803_20141026T000831_002993_003684_3EF0.manifest.safe'
    ;    opath='D:\gsar\des\altyntagh5'
    
    ;    target='S1A_IW_SLC__1SSV_20141020T115850_20141020T115917_002913_0034D7_8C59.manifest.safe'
    ;    opath='D:\gsar\asc\wulan3'
    ;    target='S1A_IW_SLC__1SSV_20141020T115915_20141020T115942_002913_0034D7_D701.manifest.safe'
    ;    opath='D:\gsar\asc\kunlun4'
    ;    target='S1A_IW_SLC__1SSV_20141015T115105_20141015T115132_002840_00333F_B384.manifest.safe'
    ;    opath='D:\gsar\asc\kunlun5'
    ;    target='S1A_IW_SLC__1SSV_20141015T115040_20141015T115107_002840_00333F_F77F.manifest.safe'
    ;    opath='D:\gsar\asc\wulan4'
    
    ;    target='S1A_IW_SLC__1SSV_20150517T120659_20150517T120726_005961_007AE4_8D7E.SAFE'
    ;    opath='D:\gsar\asc\wulan5'
    ;
    ;    target='S1A_IW_SLC__1SSV_20141007T232010_20141007T232037_002730_0030F8_81AA.manifest.safe'
    ;    target='S1B_IW_SLC__1SSV_20161225T231953_20161225T232021_003559_00616C_77E3.manifest.safe'
    ;    opath='D:\gsar\des\xianshuihe2'
    ;perc_min=.8d0
    ;
    ;    target='S1A_IW_SLC__1SSV_20141029T003310_20141029T003337_003037_00377D_2E06.manifest.safe'
    ;    opath='D:\gsar\des\altyntagh6'
    
    ;target='S1A_IW_SLC__1SSV_20141007T001622_20141007T001649_002716_0030A9_8424.manifest.safe'
    ;opath='D:\gsar\des\altyntagh7'
    ;perc_min=.7d0
    
    ;    target='S1A_IW_SLC__1SSV_20141103T234438_20141103T234505_003124_003966_1553.manifest.safe'
    ;    opath='D:\gsar\des\nujiang3'
    ;        target='S1A_IW_SLC__1SSV_20141103T234503_20141103T234531_003124_003966_5F6E.manifest.safe'
    ;        target='S1A_IW_SLC__1SSV_20141103T234503_20141103T234531_003124_003966_5F6E.manifest'
    ;        opath='D:\gsar\des\nujiang4'
    ;            target='S1A_IW_SLC__1SSV_20141103T234529_20141103T234556_003124_003966_8DD6.manifest.safe'
    ;            opath='D:\gsar\des\jiali2'
    ;  ;          perc_min=.89
    ;
    ;    target='S1A_IW_SLC__1SSV_20141015T114925_20141015T114953_002840_00333F_5439.manifest.safe'
    ;    target='S1A_IW_SLC__1SSV_20141015T114925_20141015T114953_002840_00333F_5439.manifest'
    ;    target='S1A_IW_SLC__1SSV_20151127T114925_20151127T114953_008790_00C890_59A9'
    ;    target='S1A_IW_SLC__1SSV_20170201T114931_20170201T114959_015090_018AAC_3FC3'
    ;    ;target='S1A_IW_SLC__1SSV_20160129T121336_20160129T121403_009709_00E2C4_06E8.manifest.safe'
    ;    opath='D:\gsar\asc\jiali'
    ;perc_min=.8
    
    ;    target='S1A_IW_SLC__1SDV_20170414T115007_20170414T115034_016140_01AA8B_8621.manifest.safe'
    ;    opath='D:\gsar\asc\anduo1'
    ;
    ;      target='S1A_IW_SLC__1SSV_20141106T120637_20141106T120704_003161_003A27_140E.manifest.safe'
    ;      opath='D:\gsar\asc\shuanghu1'
    ;
    ;    target='S1A_IW_SLC__1SSV_20141205T121416_20141205T121443_003584_0043B4_3D43.manifest.safe'
    ;      target='S1A_IW_SLC__1SSV_20160808T121436_20160808T121503_012509_013919_D126.manifest.safe'
    ;      opath='D:\gsar\asc\gyaringco4'
    ;      perc_min=.68d0
    ;    opath='D:\gsar\asc\riganpei5'
    ;
    target='S1A_IW_SLC__1SSV_20141027T235405_20141027T235436_003022_003721_47FA.manifest.safe'
    opath='D:\gsar\des\yzs3'
    
    ;    target='S1A_IW_SLC__1SSV_20150106T000916_20150106T000943_004043_004E0B_04B1.manifest.safe'
    ;    opath='D:\gsar\des\riganpei3'
    ;
    ;    target='S1B_IW_SLC__1SSV_20161014T001656_20161014T001723_002495_00435C_62C3.manifest.safe'
    ;    opath='D:\gsar\des\riganpei4'
    ;
    ;    target='S1A_IW_SLC__1SDV_20150308T235425_20150308T235455_004947_0062FA_37C3.manifest.safe'
    ;    opath='D:\gsar\des\mht1'
    ;
    ;    target='S1A_IW_SLC__1SSV_20141015T115015_20141015T115042_002840_00333F_974A.manifest.safe'
    ;    opath='D:\gsar\asc\nujiang5'
    ;
    ;      target='S1A_IW_SLC__1SSV_20141015T114857_20141015T114927_002840_00333F_E456.manifest.safe'
    ;      opath='D:\gsar\asc\yzs4'
    
    ;    target='S1A_IW_SLC__1SSV_20141106T120702_20141106T120729_003161_003A27_8EAB.manifest.safe'
    ;    opath='D:\gsar\asc\wulan5'
    ;    target='S1A_IW_SLC__1SSV_20141106T120726_20141106T120753_003161_003A27_B36B.manifest.safe'
    ;    opath='D:\gsar\asc\wulan6'
    ;    target='S1A_IW_SLC__1SSV_20141012T112725_20141012T112753_002796_003261_2867.manifest.safe'
    ;    opath='D:\gsar\asc\altyntagh8'
    ;    ;perc_min=.65
    ;    target='S1A_IW_SLC__1SSV_20141103T234258_20141103T234326_003124_003966_FCAD.manifest.safe'
    ;    opath='D:\gsar\des\altyntagh7'
    ;    target='S1A_IW_SLC__1SSV_20141029T233446_20141029T233514_003051_0037C8_AB8A.manifest.safe'
    ;    opath='D:\gsar\des\altyntagh9'
    ;    ;perc_min=.69
    ;    ;      target='S1A_IW_SLC__1SDV_20150204T232019_20150204T232046_004480_0057EB_ABC6.manifest.safe'
    ;    target='S1B_IW_SLC__1SSV_20170118T231951_20170118T232019_003909_006BC1_2808.manifest.safe'
    ;    ;        target='S1A_IW_SLC__1SSV_20141007T232010_20141007T232037_002730_0030F8_81AA'
    ;    target='S1A_IW_SLC__1SSV_20150710T232032_20150710T232059_006755_0090F7_D4AE'
    ;    opath='D:\gsar\des\xianshuihe4'
    ;    target='S1A_IW_SLC__1SSV_20161020T125805_20161020T125832_013574_015BBB_CAD7.manifest.safe'
    ;    opath='D:\gsar\asc\muji'
    ;    target='S1A_IW_SLC__1SSV_20141026T000853_20141026T000920_002993_003684_F4E4.manifest.safe'
    ;    opath='D:\gsar\des\manyi'
    ;    target='S1A_IW_SLC__1SSV_20141229T121349_20141229T121417_003934_004BA3_D50A.manifest.safe'
    ;    target='S1A_IW_SLC__1SSV_20161230T121411_20161230T121438_014609_017BF7_DF65.manifest.safe'
    ;    opath='D:\gsar\asc\tangrayumco'
    ;    target='S1A_IW_SLC__1SSV_20141018T121417_20141018T121444_002884_00343C_E210.manifest.safe'
    ;    target='S1A_IW_SLC__1SDV_20170204T121434_20170204T121501_015134_018C01_A67A'
    ;    opath='D:\gsar\asc\riganpei5'
    ;    target='S1A_IW_SLC__1SSV_20141026T000828_20141026T000855_002993_003684_FDE5.manifest.safe'
    ;    opath='D:\gsar\des\manyi2'
    ;    target='S1A_IW_SLC__1SSV_20141007T001801_20141007T001828_002716_0030A9_8C24.manifest.safe'
    ;    target='S1A_IW_SLC__1SSV_20160306T001753_20160306T001820_010241_00F21D_AA34'
    ;    opath='D:\gsar\des\riganpei6'
    ;        target='S1A_IW_SLC__1SSV_20150527T122210_20150527T122237_006107_007EC5_1830.manifest.safe
    ;        opath='D:\gsar\asc\yzs5'
    ;    target='S1A_IW_SLC__1SDV_20150303T234619_20150303T234647_004874_006130_15E1.manifest.safe'
    ;    opath='D:\gsar\des\mht2'
    ;    perc_min=.6
    
    ;    target='S1A_IW_SLC__1SSV_20141009T000221_20141009T000257_002745_00314A_61CD.manifest.safe'
    ;    opath='D:\gsar\des\yadong-gulu'
    ;    perc_min=.7
    ;    target='S1A_IW_SLC__1SSV_20141026T001033_20141026T001103_002993_003684_DFCC.manifest.safe'
    ;    target='S1A_IW_SLC__1SSV_20150506T001032_20150506T001059_005793_007722_31DA.manifest.safe'
    ;    opath='D:\gsar\des\shenza-dingjie'
    ;    perc_min=.7
    ;
    ;  target='S1A_IW_SLC__1SSV_20141020T115707_20141020T115737_002913_0034D7_C5FE.manifest.safe'
    ;  opath='D:\gsar\asc\yadong-gulu2'
    ;    target='S1A_IW_SLC__1SSV_20141106T120519_20141106T120548_003161_003A27_82D4.manifest.safe'
    ;    opath='D:\gsar\asc\yadong-gulu2'
    ;    target='S1A_IW_SLC__1SDV_20150206T000218_20150206T000246_004495_00583C_7261.manifest.safe'
    ;    opath='D:\gsar\des\yadong-gulu3'
    ;    target='S1B_IW_SLC__1SSV_20161014T001746_20161014T001813_002495_00435C_C03E.manifest.safe'
    ;    opath='D:\gsar\des\tangrayumco2'
    ;    target='S1A_IW_SLC__1SSV_20141018T121351_20141018T121419_002884_00343C_219D.manifest.safe'
    ;    target='S1A_IW_SLC__1SSV_20150522T121332_20150522T121400_006034_007CA2_A637.manifest.safe'
    ;    opath='D:\gsar\asc\shenza-dingjie2'
    ;    target='S1A_IW_SLC__1SSV_20161114T115810_20161114T115837_013938_016715_4191.manifest.safe'
    ;    opath='D:\gsar\asc\dongqiao'
    
    ;        target='S1A_IW_SLC__1SSV_20141008T222018_20141008T222048_002744_003146_40B4.manifest.safe'
    ;        opath='D:\gsar\des\bj'
    ;        perc_min=.69
    ;  target='S1A_IW_SLC__1SDV_20170218T115756_20170218T115823_015338_019269_933E.manifest.safe'
    ;  target='S1A_IW_SLC__1SSV_20161028T115001_20161028T115027_013690_015F4C_67E8.manifest.safe'
    ;  target='S1A_IW_SLC__1SSV_20170125T115807_20170125T115834_014988_018790_FF00.manifest.safe'
    ;  opath='D:\gsar\asc\nqxm'
    ;  target='S1A_IW_SLC__1SSV_20141010T004218_20141010T004245_002760_00319D_86CC.manifest.safe'
    ;  opath='D:\gsar\des\karakoram'
    ;  target='S1A_IW_SLC__1SSV_20151008T120601_20151008T120629_008061_00B4AC_7586.manifest.safe'
    ;  opath='D:\gsar\asc\gyaringco5'
    ;    target='S1A_IW_SLC__1SSV_20141115T114113_20141115T114141_003292_003D01_B813.manifest.safe'
    ;    opath='D:\gsar\asc\jiali3'
    ;    perc_min=.8
    ;    ;target='S1A_IW_SLC__1SSV_20141029T233717_20141029T233744_003051_0037C8_DE72.manifest.safe'
    ;    target='S1B_IW_SLC__1SSV_20161129T233639_20161129T233706_003180_005694_9CA3.manifest.safe'
    ;    opath='D:\gsar\des\jiali4'
    ;    perc_min=.67
    
    ;    target='S1A_IW_SLC__1SSV_20141029T113243_20141029T113305_003044_0037A9_DE23.manifest.safe'
    ;    opath='D:\gsar\asc\jiali5'
    ;    target='S1A_IW_SLC__1SSV_20141029T233651_20141029T233719_003051_0037C8_36C9.manifest.safe'
    ;        perc_min=.7
    ;    ;target='S1B_IW_SLC__1SSV_20161129T233613_20161129T233641_003180_005694_1AE7.manifest.safe'
    ;    opath='D:\gsar\des\nujiang8'
    ;    ;target='S1A_IW_SLC__1SSV_20141024T232913_20141024T232940_002978_00362F_8191.manifest.safe'
    ;    target='S1B_IW_SLC__1SSV_20161218T232853_20161218T232918_003457_005E81_7FF3.manifest.safe'
    ;    opath='D:\gsar\des\jiali6'
    ;    target='S1B_IW_SLC__1SSV_20161105T233704_20161105T233729_002830_004CB1_9C7C.manifest.safe'
    ;    opath='D:\gsar\des\apalong2'
    ;;    target='S1A_IW_SLC__1SSV_20151204T114044_20151204T114110_008892_00CB7E_7129.manifest.safe'
    ;    target='S1A_IW_SLC__1SSV_20150327T114043_20150327T114113_005217_00696A_915B.manifest.safe'
    ;    opath='D:\gsar\asc\apalong1'
    ;    target='S1A_IW_SLC__1SSV_20141103T234413_20141103T234440_003124_003966_AF66.manifest.safe'
    ;    opath='D:\gsar\des\wulan8'
    ;      target='S1A_IW_SLC__1SSV_20170105T112524_20170105T112552_014696_017E97_6F2A.manifest.safe'
    ;      opath='D:\gsar\asc\xianshuihe1'
    ;    target='S1B_IW_SLC__1SSV_20170213T230532_20170213T230600_004288_007717_9B27.manifest.safe'
    ;    opath='D:\gsar\des\honghe2'
    ;    perc_min=.75
    ;    target='S1A_IW_SLC__1SDV_20170217T111529_20170217T111556_015323_0191EB_2AC4.manifest.safe'
    ;    opath='D:\gsar\asc\xiaojiang1'
    ;    perc_min=.75
    ;    target='S1A_IW_SLC__1SSV_20141102T230509_20141102T230536_003109_00390D_E953.manifest.safe'
    ;    opath='D:\gsar\des\xiaojiang2'
    ;    perc_min=.75
    ;    target='S1A_IW_SLC__1SDV_20170426T115007_20170426T115034_016315_01AFE6_4E3B.manifest.safe'
    ;    target='S1A_IW_SLC__1SDV_20170501T115815_20170501T115842_016388_01B21F_78BF.manifest.safe'
    ;    opath='C:\Downloads\esa.data\S1\tmp'
    
    ;    target='S1A_IW_SLC__1SSV_20150725T114100_20150725T114130_006967_009718_FD83'
    ;    opath='D:\gsar\co\jiali.20150718'
    ;    target='S1A_IW_SLC__1SSV_20150801T233714_20150801T233742_007076_009A12_5B65'
    ;    opath='D:\gsar\des\co.jiali.20150718.2'
    ;    target='S1B_IW_SLC__1SSV_20161017T234411_20161017T234438_002553_004502_69EA'
    ;    opath='D:\gsar\des\co.zaduo.20161017.2'
    ;    target='S1A_IW_SLC__1SSV_20160905T114210_20160905T114238_012917_0146B9_5576'
    ;    opath='D:\gsar\asc\co.zaduo.20161017.1'
    ;    target='S1A_IW_SLC__1SSV_20161121T115000_20161121T115027_014040_016A28_F8F7'
    ;    opath='D:\gsar\asc\co.nujiang.20161204'
    ;    target='S1A_IW_SLC__1SSV_20160126T235253_20160126T235320_009672_00E1B8_33A3'
    ;    opath='D:\gsar\des\co.nujiang.20160113.2'
    ;    target='S1A_IW_SLC__1SSV_20160515T233649_20160515T233717_011276_01113E_2EDD'
    ;    opath='D:\gsar\des\co.nujiang.20160511.2'
    ;    target='S1A_IW_SLC__1SSV_20150412T001030_20150412T001101_005443_006EFD_467B.manifest.safe'
    ;    opath='D:\gsar\des\co.stds.20150425.2'
    ;    target='S1B_IW_SLC__1SSV_20161218T002411_20161218T002438_003443_005E1F_5DFB.manifest.safe'
    ;    opath='D:\gsar\des\co.altyntagh.20161220.2'
    ;    target='S1A_IW_SLC__1SSV_20160113T111020_20160113T111047_009475_00DBF0_0EAB'
    ;    opath='D:\gsar\asc\co.menyuan.20160121.1'
    ;    ;    target='S1A_IW_SLC__1SSV_20160118T231849_20160118T231915_009555_00DE41_A125.manifest.safe'
    ;    ;    opath='D:\gsar\des\co.menyuan.20160121.2'
    ;    target='S1A_IW_SLC__1SDV_20170211T120943_20170211T121010_015236_018F46_3679.manifest.safe'
    ;    opath='D:\gsar\asc\co.hutubi.20161208.1'
    ;    target='S1B_IW_SLC__1SSV_20161218T002206_20161218T002233_003443_005E1F_FEF2.manifest.safe'
    ;    opath='D:\gsar\des\co.hutubi.20161208.2'
    ;;    target='S1A_IW_SLC__1SSV_20160512T111022_20160512T111049_011225_010F96_A73B.manifest.safe'
    ;;    target='S1A_IW_SLC__1SSV_20141014T111011_20141014T111038_002825_0032F3_6EC5.manifest.safe'
    ;;    opath='D:\gsar\asc\lenglongling1'
    ;;    perc_min=.8
    ;  ;    target='S1B_IW_SLC__1SSV_20170118T231812_20170118T231839_003909_006BC1_7703.manifest.safe'
    ;  ;    opath='D:\gsar\des\lenglongling2'
    ;      target='S1A_IW_SLC__1SSV_20150523T001851_20150523T001919_006041_007CD6_B48D.manifest.safe'
    ;      opath='D:\gsar\des\co.nepal.20150425.2'
    ;      target='S1A_IW_SLC__1SSV_20150503T122143_20150503T122211_005757_007649_F885.manifest.safe'
    ;      opath='D:\gsar\asc\co.nepal.20150425.1'
    ;    target='S1A_IW_SLC__1SDV_20170513T005742_20170513T005809_016556_01B73B_C4FD'
    ;    opath='D:\gsar\des\co.karakorum.20170511.2'
    ;    target='S1A_IW_SLC__1SDV_20170512T125713_20170512T125741_016549_01B6F8_3435'
    ;    opath='D:\gsar\asc\co.karakorum.20170511.1'
    
    ;    target='S1A_IW_SLC__1SSV_20141026T231346_20141026T231413_003007_0036CE_2CD2.manifest.safe'
    ;    ;target='S1A_IW_SLC__1SSV_20150530T231353_20150530T231421_006157_008014_3915.manifest.safe'
    ;    opath='D:\gsar\des\dali2'
    ;    perc_min=.77d0
    
    target='S1A_IW_SLC__1SDV_20170726T234558_20170726T234625_017649_01D8B8_9538.manifest.safe'
    TARGET='S1A_IW_SLC__1SDV_20150207T234529_20150207T234556_004524_0058E5_3C5F.manifest.safe'
    TARGET='S1A_IW_SLC__1SDV_20150207T234529_20150207T234556_004524_0058E5_3C5F.manifest'
    opath='D:\gsar\des\mila2'
    perc_min=.46d0
;    target='S1A_IW_SLC__1SSV_20170201T114931_20170201T114959_015090_018AAC_3FC3'
;    opath='d:\gsar\asc\mila1'
;    ;perc_min=.65d0
;    target='S1A_IW_SLC__1SDV_20170719T114922_20170719T114949_017540_01D556_CA96'
;    
;    target='S1A_IW_SLC__1SDV_20170702T234557_20170702T234624_017299_01CE12_49AD'
;    opath='c:\tmp\'
;    
;    
;    target='S1A_IW_SLC__1SDV_20170302T115731_20170302T115758_015513_0197BA_CFD1.manifest.safe'
;    opath='D:\gsar\asc\mila3'
;    perc_min=.55d0
;    
;    target='S1A_IW_SLC__1SSV_20150624T235347_20150624T235415_006522_008AA6_980C'
;    opath='D:\gsar\des\mila4'
    
  ENDIF
  
  IF N_ELEMENTS(target) EQ 0 THEN BEGIN
    PRINT,'ERROR: no target specified!!'
    RETURN
  ENDIF
  
  IF N_ELEMENTS(perc_min) EQ 0 THEN perc_min=.83
  ;stop
  ofile=opath+PATH_SEP()+'overlapping.'+target+'.txt'
  
  IF N_ELEMENTS(xys) EQ 0 THEN BEGIN  ;if no xys/names inputs, then read files
    IF N_ELEMENTS(files) EQ 0 THEN BEGIN
      files=FILE_SEARCH(path+PATH_SEP()+'*.manifest.safe', count=nf)
      ;files=FILE_SEARCH(path+PATH_SEP()+'manifest.safe-*', count=nf)
      IF nf LE 0 THEN BEGIN
        RETURN
      ENDIF
      
    ENDIF ELSE BEGIN
      nf=N_ELEMENTS(files)
    ENDELSE
    ;
    ;stop
    
    obtnames=['ascending','descending']
    
    xys_all=DBLARR(2,4,nf)
    names_all=STRARR(nf)
    obtyps_all=INTARR(nf)
    
    lbls_all=STRTRIM(INDGEN(4)+1,2)
    
    oldwin=!d.WINDOW
    WINDOW,/free,xsize=1800,ysize=600
    DEVICE, decomposed=1
    !p.MULTI=[0,3,1]
    ;stop
    PLOT,[0],[0],background='ffffff'x,color='0'x,psym=-2,/iso,/yno,$
      xrange=[83,101],yrange=[28,42],$
      title='Footprints of All Scenes',$
      xtitle='Longitude',ytitle='Latitude',charsize=1.2
      
    FOR fi=0, nf-1 DO BEGIN
      file=files[fi]
      pos=STRPOS(file,'E456')
      ;if pos[0] ne -1 then stop
      names_all[fi]=GETFILENAME(file)
      ;
      lines=read_txt(file)
      line=grepi(lines,'coordinates')
      ; <gml:coordinates>32.437653,89.348289 32.836613,91.993134 31.158100,92.323769 30.756710,89.728622</gml:coordinates>
      line=STRTRIM(line,2)
      line_p1=STRSPLIT(line,'>',/extract)
      line_p2=STRSPLIT(line_p1[1],'<',/extract)
      tmp=line_p2[0]
      tmp_p=STRSPLIT(tmp,/extract)
      
      xys_i=DBLARR(2,4)
      FOR pi=0,3 DO BEGIN
        tmp_xy=STRSPLIT(tmp_p[pi],',',/extract)
        xys_i[*,pi]=DOUBLE(tmp_xy[[1,0]])
      ENDFOR
      xys_all[*,*,fi]=xys_i
      ;PRINT,xys_i
      ;stop
      
      ;descending/ascending
      line=grepi(lines,'pass')
      line1=strrep(line,'/','')
      line2=STRSPLIT(line1,'<s1:pass>',/extract)
      CASE line2[1] OF
        'ASCENDING': obtyps_all[fi]=0
        'DESCENDING': obtyps_all[fi]=1
      ENDCASE
      
      OPLOT,xys_i[0,[0,1,2,3,0]],xys_i[1,[0,1,2,3,0]],color='00ffaa'x
    ;XYOUTS,xys_i[0,*],xys_i[1,*],lbls,color='ff0000'x,charsize=2
    ;ofile=file+'.jpg'
    ;WRITE_JPEG,ofile,TVRD(true=1),true=1,quality=100
    ;STOP
      
    ENDFOR
    
  ENDIF
  ;//read-files-end
  tmp=grepi(names_all,target)
  IF tmp[0] EQ '' || N_ELEMENTS(pos) GT 1 THEN STOP
  target=tmp
  IF N_ELEMENTS(target) GT 1 THEN BEGIN
    PRINT,'[WARNING]More than one records matched! The first one used.'
    target=target[0]
  ENDIF
  ind_fi=REFORM(WHERE(names_all EQ target[0]))
  PRINT,'target scene:',target
  
  ;STOP
  
  pos=WHERE(obtyps_all EQ (obtyps_all[ind_fi])[0] )
  IF pos[0] EQ -1 THEN RETURN
  ;
  ;stop
  ;pos=pos[0:590]  ;only for test
  xys=xys_all[*,*,pos]
  names=names_all[pos]
  ind_fi=REFORM(WHERE(names EQ target[0]))
  ;
  np=N_ELEMENTS(pos)
  
  
  ;stop
  IF ind_fi[0] EQ -1 THEN RETURN
  
  
  ;STOP
  percs=DBLARR(np,np)
  
  ;FOR fi=0, np-1 DO BEGIN
  FOR fi=ind_fi[0],ind_fi[0] DO BEGIN
    xys_fi=REFORM(xys[*,*,fi])
    xmin_fi=MIN(xys_fi[0,*],max=xmax_fi)
    ymin_fi=MIN(xys_fi[1,*],max=ymax_fi)
    ;
    FOR fj=0, np-1 DO BEGIN
      xys_fj=REFORM(xys[*,*,fj])
      xmin_fj=MIN(xys_fj[0,*],max=xmax_fj)
      ymin_fj=MIN(xys_fj[1,*],max=ymax_fj)
      ;stop
      IF xmin_fj GT xmax_fi || xmax_fj LT xmin_fi || ymin_fj GT ymax_fi || ymax_fj LT ymin_fj THEN BEGIN
        tmp=0d0
      ENDIF ELSE BEGIN
        tmp=POLYGON_OVERLAY(xys_fi,xys_fj)
      ENDELSE
      PRINT,np,fi,fj,tmp
      percs[fi,fj]=tmp
      IF tmp EQ 0 THEN CONTINUE
      
    ;      WINDOW,0,xsize=900,ysize=900
    ;      PLOT,xys_fi[0,[0,1,2,3,0]],xys_fi[1,[0,1,2,3,0]],background='ffffff'x,color='0'x,psym=-2,/iso,/yno,xrange=[85,100],yrange=[25,35]
    ;      OPLOT,xys_fj[0,[0,1,2,3,0]],xys_fj[1,[0,1,2,3,0]],color='0'x,psym=-2
      
      
    ;STOP
    ENDFOR
  ENDFOR
  
  
  pos=WHERE(percs[ind_fi,*] GE perc_min)
  IF pos[0] EQ -1 THEN BEGIN
    RETURN
  ENDIF
  onames=names[pos]
  ;WINDOW,0,xsize=900,ysize=900
  xys_fi=REFORM(xys[*,*,ind_fi[0]])
  PLOT,xys_fi[0,[0,1,2,3,0]],xys_fi[1,[0,1,2,3,0]],background='ffffff'x,  $
    title='Footprints of Matching Scenes',$
    xtitle='Longitude',ytitle='Latitude',charsize=1.2,$
    color='000000'x,/iso,/yno,/nodata   ;,xrange=[85,100],yrange=[25,35]
  OPLOT,xys_fi[0,[0,1,2,3,0]],xys_fi[1,[0,1,2,3,0]], $
    color='0000ff'x,psym=-4,symsize=4,thick=3
  dx=(MAX(xys_fi[0,*])-MIN(xys_fi[0,*]))/N_ELEMENTS(pos)
  ;stop
  ;DEVICE,decomposed=0
  FOR i=0,N_ELEMENTS(pos)-1 DO BEGIN
    xys_fj=REFORM(xys[*,*,pos[i]]);
    ;POLYFILL,xys_fj[0,[0,1,2,3,0]],xys_fj[1,[0,1,2,3,0]],color=i*256
    ;OPLOT,xys_fj[0,[0,1,2,3,0]],xys_fj[1,[0,1,2,3,0]],color='ff0000'x,psym=-3,linestyle=2,thick=2
    OPLOT,xys_fj[0,[0,1,2,3,0]],xys_fj[1,[0,1,2,3,0]],color=i*(!d.N_COLORS/N_ELEMENTS(pos)),psym=-3,linestyle=2,thick=1
    y1=MAX(xys_fj[1,*],indmax1)
    tmp=xys_fj
    tmp[1,indmax1]=-9999
    y2=MAX(tmp[1,*],indmax2)
    xy1=xys_fj[*,indmax1]
    xy2=xys_fj[*,indmax2]
    IF xy1[0] GT xy2[0] THEN BEGIN
      tmp=xy2
      xy2=xy1
      xy1=tmp
    ENDIF
    xi=xy1[0]+dx*i
    rate=(xy1[1]-xy2[1])/(xy1[0]-xy2[0])
    b=xy1[1]-rate*xy1[0]
    yi=xi*rate+b
    XYOUTS,xi,yi, $
      STRTRIM(i+1,2),color=i*(!d.N_COLORS/N_ELEMENTS(pos))
  ENDFOR
  ;DEVICE,decomposed=1
  ;STOP
  
  OPENW,fid,ofile,/get_lun
  LLS_KML=REPLICATE(PTR_NEW(),N_ELEMENTS(POS))
  NAMES_KML=NAMES[POS]
  FOR i=0,N_ELEMENTS(pos)-1 DO BEGIN
    PRINTF,fid,names[pos[i]],percs[ind_fi,pos[i]],format='(1x,a,1x,f)'
    LLS_KML[I]=PTR_NEW(REFORM(XYS[*,*,POS[I]]))
  ENDFOR
  FOR fi=0,np-1 DO BEGIN
    PRINTF,fid,names[fi],percs[ind_fi,fi],format='("#",a,1x,f)'
  ENDFOR
  FREE_LUN,fid
  
  hp=HISTOGRAM(percs[ind_fi,*],binsize=.02,locations=hx)
  ;WINDOW,1
  pos=WHERE(hp GT 0)
  PLOT,[hx[1:*],1.1],[hp[1:*],0],background='ffffff'x,color='000000'x,psym=-2, $
    xtitle='Percentage of Overlapping (%)',  $
    ytitle='Count (#)', $
    title=target,/nodata,charsize=1.5
  IF pos[0] NE -1 THEN BEGIN
    OPLOT,hx[pos],hp[pos],color='ff0000'x,psym=-4
  ENDIF
  
  jfile=desuffix(ofile)+'.jpg'
  PRINT,jfile
  ;STOP
  WRITE_JPEG,jfile,TVRD(true=1),true=1,quality=100
  newwin=!d.WINDOW
  WSET,oldwin
  
  OFILE=DESUFFIX(OFILE)+'.kml'
  KML_POLYLINE, OFILE, names=names_KML, LLS=LLS_KML, IS_FINALIZE=1, IS_FREE_PTR=0
  OFILE=DESUFFIX(OFILE)+'.psxy'
  PSXY_POLYLINE, OFILE, names=names_KML, LLS=LLS_KML, IS_FINALIZE=1
  
;WDELETE,newwin
;STOP
  
END