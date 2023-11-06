clear
clc

PROG='sar_los_profile_fits';
ver='20220802';
user='tianyf';
run_at='hostname';
run_on=datestr(now);

dmin=-200;
dmax=200;
fts_min=-15;
fts_max=15;

d_exclude=[9999,-9999];

cmt='';

is_show_fig='off';
% is_show_fig='on';

%bengco
% path='D:\gsar\interseismic\150-d-m3-0487_0492_0497-dangxiong2_yzs3_gulu1\f123\sbas.4.0.0700.9999.20141027.20200820.128.1753.01.x2\p.fa_bengco';
% 
% %eklf
% % path='Z:\g7c\gsar\D\gsar\interseismic\004-d-m4-0466_0471_0476_0481-eastkunlun\f123\sbas.4.0.0367.9999.20150801.20210501.125.1391.01.___\p.fa_Kunlun_Fault';
% % path='Z:\g7c\gsar\D\gsar\interseismic\026-a-m4-0102_0107_0112_0117-eastkunlun1M3\f123\sbas.4.0.0367.9999.20141019.20210527.142.1350.01.___\p.fa_Kunlun_Fault';
% % path='Z:\g7c\gsar\D\gsar\interseismic\033-d-m3-0473_0478_0483-eastkunlun8M3\f123\sbas.4.0.0367.9999.20141031.20210503.148.1299.01.___\p.fa_Kunlun_Fault';
% % 
% % path='Z:\g7c\gsar\D\gsar\interseismic\070-a-m3-0105_0110_0115-eastkunlun5M3\f123\sbas.4.0.0367.9999.20141022.20210211.146.0908.01.___\p.fa_Kunlun_Fault';
% % path='Z:\g7c\gsar\D\gsar\interseismic\077-d-m3-0468_0473_0478-eastkunlun6M3\f123\sbas.4.0.0367.9999.20141103.20210506.149.1127.01.___\p.fa_Kunlun_Fault';
% % %
% % path='Z:\g7c\gsar\D\gsar\interseismic\099-a-m3-0106_0112_0117-eastkunlun3M3\f123\sbas.4.0.0367.9999.20141012.20210508.155.1512.01.___\p.fa_Kunlun_Fault';
% % % path='Z:\g7c\gsar\D\gsar\interseismic\106-d-m4-0470_0475_0480_0485-eastkunlun2M\f123\sbas.4.0.0367.9999.20150808.20210520.147.1355.01.___\p.fa_Kunlun_Fault';
% % % path='Z:\g7c\gsar\D\gsar\interseismic\128-a-m3-0105_0110_0115-eastkunlun9M3\f123\sbas.4.0.0367.9999.20141014.20210428.156.1524.01.___\p.fa_Kunlun_Fault';
% % % %
% % % path='Z:\g7c\gsar\D\gsar\interseismic\135-d-m4-0472_0477_0482_0487-eastkunlun12M3\f123\sbas.4.0.0367.9999.20141026.20211013.124.1299.01.___\p.fa_Kunlun_Fault';
% % % path='Z:\g7c\gsar\D\gsar\interseismic\172-a-m4-0101_0106_0111_0116-eastkunlun7M3\f123\sbas.4.0.0367.9999.20141017.20210525.150.1399.01.___\p.fa_Kunlun_Fault';
% % % path='Z:\g7c\gsar\D\gsar\interseismic\172-a-m4-0106_0111_0116_0121-eastkunlun7M3\f123\sbas.4.0.0367.9999.20141017.20211109.158.1300.01.___\p.fa_Kunlun_Fault';
% 
% paths={ ...
%   %'Z:\g7c\gsar\D\gsar\interseismic\004-d-m4-0466_0471_0476_0481-eastkunlun\f123\sbas.4.0.0367.9999.20150801.20210501.125.1391.01.___\p.fa_Kunlun_Fault';...
%   %'Z:\g7c\gsar\D\gsar\interseismic\026-a-m4-0102_0107_0112_0117-eastkunlun1M3\f123\sbas.4.0.0367.9999.20141019.20210527.142.1350.01.___\p.fa_Kunlun_Fault';...
%   %'Z:\g7c\gsar\D\gsar\interseismic\033-d-m3-0473_0478_0483-eastkunlun8M3\f123\sbas.4.0.0367.9999.20141031.20210503.148.1299.01.___\p.fa_Kunlun_Fault';...
%   
%   %'Z:\g7c\gsar\D\gsar\interseismic\070-a-m3-0105_0110_0115-eastkunlun5M3\f123\sbas.4.0.0367.9999.20141022.20210211.146.0908.01.___\p.fa_Kunlun_Fault';...
%   %'Z:\g7c\gsar\D\gsar\interseismic\077-d-m3-0468_0473_0478-eastkunlun6M3\f123\sbas.4.0.0367.9999.20141103.20210506.149.1127.01.___\p.fa_Kunlun_Fault';...
%   %
%   % 'Z:\g7c\gsar\D\gsar\interseismic\099-a-m3-0106_0112_0117-eastkunlun3M3\f123\sbas.4.0.0367.9999.20141012.20210508.155.1512.01.___\p.fa_Kunlun_Fault';...
%   'Z:\g7c\gsar\D\gsar\interseismic\106-d-m4-0470_0475_0480_0485-eastkunlun2M\f123\sbas.4.0.0367.9999.20150808.20210520.147.1355.01.___\p.fa_Kunlun_Fault';...
%   % 'Z:\g7c\gsar\D\gsar\interseismic\128-a-m3-0105_0110_0115-eastkunlun9M3\f123\sbas.4.0.0367.9999.20141014.20210428.156.1524.01.___\p.fa_Kunlun_Fault';...
%   % %
%   % 'Z:\g7c\gsar\D\gsar\interseismic\135-d-m4-0472_0477_0482_0487-eastkunlun12M3\f123\sbas.4.0.0367.9999.20141026.20211013.124.1299.01.___\p.fa_Kunlun_Fault';...
%   % 'Z:\g7c\gsar\D\gsar\interseismic\172-a-m4-0101_0106_0111_0116-eastkunlun7M3\f123\sbas.4.0.0367.9999.20141017.20210525.150.1399.01.___\p.fa_Kunlun_Fault';...
%   % 'Z:\g7c\gsar\D\gsar\interseismic\172-a-m4-0106_0111_0116_0121-eastkunlun7M3\f123\sbas.4.0.0367.9999.20141017.20211109.158.1300.01.___\p.fa_Kunlun_Fault';...
%   ''};
% %paths={'D:\gsar\interseismic\026-a-m4-0097_0102_0107_0112-kangding_eastkunlun\f123\sbas.4.0.0367.9999.20141019.20210924.152.1296.01.___\p.fa_Kunlun_Fault'};
% %paths={'D:\gsar\interseismic\135-d-m4-0472_0477_0482_0487-eastkunlun12M3\f123\sbas.4.0.0367.9999.20141026.20211013.124.1299.01.___\p.fa_Kunlun_Fault'};
% paths={'D:\gsar\interseismic\106-d-m4-0470_0475_0480_0485-eastkunlun2M\f123\sbas.4.0.0367.9999.20150808.20210520.147.1355.01.___\p.fa_Kunlun_Fault'};
% paths={'D:\gsar\interseismic\099-a-m3-0106_0112_0117-eastkunlun3M3\f123\sbas.4.0.0367.9999.20141012.20210508.155.1512.01.___\p.fa_Kunlun_Fault'};
% paths={'D:\gsar\interseismic\026-a-m4-0102_0107_0112_0117-eastkunlun1M3\f123\sbas.4.0.0367.9999.20141019.20210527.142.1350.01.___\p.fa_Kunlun_Fault'};
% paths={'D:\gsar\interseismic\004-d-m4-0466_0471_0476_0481-eastkunlun\f123\sbas.4.0.0367.9999.20150801.20210501.125.1391.01.___\p.fa_Kunlun_Fault'};
% paths={'D:\gsar\interseismic\172-a-m4-0106_0111_0116_0121-eastkunlun7M3\f123\sbas.4.0.0367.9999.20141017.20211109.158.1300.01.___\p.fa_Kunlun_Fault'};
% paths={'D:\gsar\interseismic\070-a-m3-0105_0110_0115-eastkunlun5M3\f123\sbas.4.0.0367.9999.20141022.20210211.146.0908.01.___\p.fa_Kunlun_Fault'};
% 
% paths={'Z:\g7c\gsar\D\gsar\interseismic\135-d-m3-0472_0477_0482-eastkunlun12M3\f123\sbas.4.0.0367.9999.20150130.20210510.107.1496.01.___\p.fa_Kunlun_Fault'};
% paths={'Z:\g7c\gsar\D\gsar\interseismic\135-d-m4-0472_0477_0482_0487-eastkunlun12M3\f123\sbas.4.0.0181.9999.20170113.20211013.099.1091.01.___\p.fa_Kunlun_Fault'; ...
%   'Z:\g7c\gsar\D\gsar\interseismic\135-d-m4-0472_0477_0482_0487-eastkunlun12M3\f123\sbas.4.0.0367.9999.20170113.20211013.099.0788.01.___\p.fa_Kunlun_Fault'; ...
%   'Z:\g7c\gsar\D\gsar\interseismic\135-d-m4-0472_0477_0482_0487-eastkunlun12M3\f123\sbas.4.0.0900.9999.20141026.20211013.124.0800.01.___\p.fa_Kunlun_Fault';...
%   ''};
% paths={'Z:\g7c\gsar\D\gsar\interseismic\135-d-m4-0472_0477_0482_0487-eastkunlun12M3\f123\sbas.4.0.0367.9999.20141026.20210510.112.1148.01.___\p.fa_Kunlun_Fault'};
% paths={'Z:\g7c\gsar\D\gsar\interseismic\172-a-m4-0106_0111_0116_0121-eastkunlun7M3\f123\sbas.4.0.0367.9999.20141017.20210513.148.1316.01.___\p.fa_Kunlun_Fault'};
% paths={'D:\gsar\interseismic\172-a-m4-0106_0111_0116_0121-eastkunlun7M3\f123\sbas.4.0.0367.9999.20141017.20210513.148.1316.01.___\p.fa_Kunlun_Fault'};
% paths={'D:\gsar\interseismic\070-a-m3-0105_0110_0115-eastkunlun5M3\f123\sbas.4.0.0367.9999.20141022.20210211.146.0908.01.___\p.fa_Kunlun_Fault'};
% paths={'D:\gsar\interseismic\077-d-m3-0468_0473_0478-eastkunlun6M3\f123\sbas.4.0.0367.9999.20141103.20210506.149.1127.01.___\p.fa_Kunlun_Fault'};
% paths={'D:\gsar\interseismic\026-a-m4-0097_0102_0107_0112-kangding_eastkunlun\f123\sbas.4.0.0367.9999.20141019.20210515.141.1281.01.___\p.fa_Kunlun_Fault'};
% paths={'D:\gsar\interseismic\026-a-m4-0102_0107_0112_0117-eastkunlun1M3\f123\sbas.4.0.0367.9999.20170124.20210527.111.0786.01.___\p.fa_Kunlun_Fault'};
% %
% paths={'D:\gsar\interseismic\150-d-m4-0482_0487_0492_0497-dangxiong2_yzs3_gulu1\f123\sbas.4.0.0367.9999.20141027.20210417.148.1399.01.___\p.fa_bengco_jiali_ext2'};
% paths={'D:\gsar\interseismic\041-a-m4-0088_0094_0099_0104-dangxiong_yzs1_dongqiao_sewa2\f123\sbas.4.0.0367.9999.20141020.20210808.167.1288.01.___\p.fa_bengco_jiali_ext2'};
% paths={'D:\gsar\interseismic\041-a-m4-0088_0094_0099_0104-dangxiong_yzs1_dongqiao_sewa2\f123\sbas.4.0.0367.9999.20141020.20210808.167.1288.01.___\p.fa_bengco_jiali_ext2'};
% paths={'D:\gsar\interseismic\150-d-m4-0457_0462_0467_0472-altyntagh_M3\f123\sbas.4.0.0367.9999.20141027.20210417.144.1300.01.___\p.fa_atf_ext'};
% paths={'D:\gsar\interseismic\041-a-m4-0088_0094_0099_0104-dangxiong_yzs1_dongqiao_sewa2\f123\sbas.4.0.0367.9999.20141020.20210808.167.1288.01.___\p.fa_bengco_jiali_ext2'};
% % paths={'D:\gsar\interseismic\048-d-m4-0478_0483_0488_0493-sewa3_wulan1_gyaringco3_ranwu2\f123\sbas.4.0.0367.9999.20141102.20210505.141.1399.01.___\p.fa_bengco_jiali_ext2'};
% paths={'D:\gsar\interseismic\048-d-m3-0482_0487_0492-sewa3_gyaringco3_ranwu2\f123\sbas.4.0.0700.9999.20141102.20201001.125.1936.01.___\p.fa_gcf'};
% paths={'D:\gsar\interseismic\121-d-m3-0482_0486_0492-gyaringco2_riganpei3_yzs2\f123\sbas.4.0.0367.9999.20141026.20210323.143.1457.01.___\p.fa_gcf'};
% paths={'D:\gsar\interseismic\114-a-m4-0090_0095_0100_0105-gyaringco_ranwu_yadong_gulu2\f123\sbas.4.0.0367.9999.20150117.20210322.148.1299.01.___\p.fa_gcf'};
% paths={'\\gpsac11\root\g11j\D\gsar\interseismic\150-d-m4-0457_0462_0467_0472-altyntagh_M3\f123\sbas.4.0.0367.9999.20141027.20210417.144.1300.01.___\p.fa_atf_ext'};
% paths={'\\gpsac11\root\g11j\D\gsar\interseismic\150-d-m4-0457_0462_0467_0472-altyntagh_M3\f123\sbas.4.0.0001.0120.20141027.20210417.144.1131.01.___\p.fa_atf_ext'};
% paths={'\\gpsac11\root\g11j\D\gsar\interseismic\121-d-m3-0456_0462_0467-altyntagh_M3\f123\sbas.3.9.0367.9999.20141026.20170425.031.0197.01.ADD\p.fa_atf_ext'};
% paths={'\\gpsac11\root\g11j\D\gsar\interseismic\121-d-m3-0456_0462_0467-altyntagh_M3\f123\sbas.4.0.0367.9999.20141026.20170425.031.0197.01.___\p.fa_atf_ext'};
% paths={'\\gpsac11\root\g11j\D\gsar\interseismic\121-d-m3-0462_0467_0472-altyntagh_M3\f123\sbas.4.0.0001.9999.20141026.20221008.037.0666.01.___\p.fa_atf_ext'};
% paths={'\\gpsac11\root\g11j\D\gsar\interseismic\121-d-m3-0462_0467_0472-altyntagh_M3\f123\sbas.4.0.0001.9999.20141026.20221008.037.0089.01.___\p.fa_atf_ext'};
% paths={'D:\gsar\interseismic\041-a-m4-0109_0114_0119_0124-altyntagh_M3\f123\sbas.4.0.0367.9999.20141020.20210901.166.1299.01.___\p.fa_atf'};
% paths={'D:\gsar\interseismic\055-a-m3-0112_0117_0122-haiyuan1M3\f123\sbas.4.0.0367.9999.20141021.20210423.122.1215.01.___\p.fa_haiyuan'};
% paths={'D:\gsar\interseismic\033-d-m4-0458_0463_0468_0473-qinghai_lake_haiyuan\f123\sbas.4.0.0367.9999.20141031.20210421.147.1280.01.___\p.fa_haiyuan'};
% paths={'D:\gsar\interseismic\114-a-m3-0115_0120_0125-altyntagh2_wulan6M3\f123\sbas.4.0.0367.9999.20141106.20200115.107.1324.01.___\p.fa_atf'};
% paths={'D:\gsar\interseismic\070-a-m3-0105_0110_0115-eastkunlun5M3\f123\sbas.4.0.0367.9999.20141022.20210211.146.0908.01.___\p.fa_Kunlun_Fault'};
% paths={'D:\gsar\interseismic\150-d-m3-0467_0472_0477-kunlun3_wulan2_qaidam4\f123.1\sbas.3.0.0500.9999.20141027.20191001.102.0323.01.___\p.fa_eklf'};
% paths={'D:\gsar\interseismic\085-a-m4-0104_0109_0114_0119-woniuhu1M3\f123\sbas.4.0.0367.9999.20141116.20220303.182.1300.01.___\p.fa_atf_ext'};
% paths={'D:\gsar\interseismic\158-a-m4-0103_0108_0113_0118-gozhaco\f123\sbas.4.0.0073.9999.20141016.20220507.188.0621.01.___\p.fa_xiaoerkule1'};
% paths={'D:\gsar\interseismic\033-d-m4-0493_0498_0503_0508-ehs\f123\sbas.4.0.0001.9999.20141031.20230117.119.0278.01.___\p.fa_deqin_zhongdian_daju_eq2013cluster'};
% paths={'D:\gsar\interseismic\014-a-m2-0115_0120-turkey20230206\f123\sbas.4.0.0001.9999.20141006.20230128.005.0010.01.___\p.fa_east_anatolia'};
% paths={'D:\gsar\interseismic\014-a-m2-0115_0120-turkey20230206\f123\sbas.4.0.0001.9999.20141006.20230128.019.0138.01.___\p.fa_east_anatolia'};
% paths={'D:\gsar\interseismic\014-a-m3-0110_0115_0120-turkey20230206\f123\sbas.4.0.0001.9999.20141018.20230128.021.0100.01.___\p.fa_east_anatolia'};
% paths={'D:\gsar\interseismic\021-d-m4-0455_0460_0465_070-turkey\f123\sbas.4.0.0001.9999.20141019.20220122.029.0051.01.___\p.fa_east_anatolia'};
% paths={'D:\gsar\interseismic\165-d-m3-0467_0472_0477-woniuhu4M3\f123\sbas.4.0.0367.9999.20141029.20210507.128.1546.01.___\p.fa_xiaoerkule1'};
% paths={'D:\gsar\interseismic\085-a-m4-0104_0109_0114_0119-woniuhu1M3\f123\sbas.4.0.0367.9999.20141116.20220303.182.1300.01.___\p.fa_xiaoerkule1'};
% paths={'D:\gsar\interseismic\085-a-m3-0109_0114_0119-woniuhu1M3\f123\sbas.4.0.0367.9999.20141116.20210212.153.1522.01.___\p.fa_atf_ext'};
% paths={'D:\gsar\interseismic\165-d-m6-0467_0472_0477_0482_0487_0492-woniuhu4M3\f123\sbas.4.0.0001.9999.20141029.20230208.063.0534.01.___\p.fa_xiaoerkule1'};
% paths={'D:\gsar\interseismic\158-a-m6-0093_0098_0103_0108_0113_0118-gozhaco\f123\sbas.4.0.0001.9999.20141016.20230219.059.0206.01.___\p.fa_gozhaco_woniuhu_ext2'};
% paths={'D:\gsar\interseismic\158-a-m6-0093_0098_0103_0108_0113_0118-gozhaco\f123\sbas.4.0.0001.9999.20141016.20230219.059.0206.01.___\p.fa_xiaoerkule1'};
% paths={'D:\gsar\interseismic\026-a-m1-0097-xsh\f123\sbas.3.0.0001.0735.20170301.20220323.027.0212.01.___\p.fa_xsh_b_gnss'};
% paths={'\\10.4.35.84\root\g9h\gsar\026-a-m1-0097-xsh\f123\sbas.3.4.0001.9999.20170301.20220323.027.0212.01.___\p.fa_xsh'};
% paths={'\\10.4.134.30\root\g13a\gsar\012-a-m7-0088_0093_0098_0103_0108_0113_0118-tibet\f123\sbas.4.0.0001.9999.20150627.20230317.056.0085.01.___\p.fa_atf_ext'};
% paths={'D:\gsar\interseismic\012-a-m7-0088_0093_0098_0103_0108_0113_0118-tibet\f123\sbas.4.9.0001.9999.20150627.20220930.053.0393.01.___\p.fa_atf_9r'};
% paths={'D:\gsar\interseismic\135-d-m7-0451_0456_0461_0467_0472_0477-0482-haiyuan_gulang\f123\sbas.4.0.0001.9999.20150130.20221020.049.0596.01.___\p.fa_Kunlun_Fault'};
% paths={'D:\gsar\interseismic\106-d-m6-0460_0465_0470_0475_0480_0485-eastkunlun2M\f123\sbas.4.0.0001.9999.20150808.20210520.038.0207.01.___\p.fa_Kunlun_Fault'};
% paths={'\\10.4.134.30\root\g9h\gsar\026-a-m1-0097-xsh\f123\sbas.3.4.0001.9999.20170301.20220323.027.0212.01.___\p.fa_xsh_b_gnss'};
% paths={'Z:\g11j\D\gsar\interseismic\077-d-m7-0458_0463_0468_0473_0478_0483_0488-kunlun\f123\sbas.4.0.0001.9999.20141103.20230309.050.0670.01.___\p.fa_Kunlun_Fault'};
% ptn='039'
% dmin=-350;
% dmax=200;
% d_exclude=[-160,180];
% paths={'Z:\g11j\D\gsar\interseismic\150-d-m6-0462_0467_0472_0475_0482_0487-kunlun\f123\sbas.4.0.0001.9999.20141027.20230218.060.0800.01.___\p.fa_Kunlun_Fault'};
% ptn='019'
% dmin=-320;
% dmax=320;
% d_exclude=[-160,160];
% paths={'Z:\g11j\D\gsar\interseismic\077-d-m7-0458_0463_0468_0473_0478_0483_0488-kunlun\f123\sbas.4.0.0001.9999.20141103.20230309.050.0670.01.___\p.fa_Kunlun_Fault'};
% paths={'Z:\g11j\D\gsar\interseismic\106-d-m6-0460_0465_0470_0475_0480_0485-eastkunlun2M\f123\sbas.4.0.0001.9999.20150808.20210520.038.0703.01.___\p.fa_Kunlun_Fault'};
% paths={'Z:\g11j\D\gsar\interseismic\033-d-m7-0463_0468_0473_0478_0483_0487_0493-kunlun\f123\sbas.4.0.0001.9999.20141031.20210515.039.0231.01.___\p.fa_Kunlun_Fault'};
% paths={'Z:\g11j\D\gsar\interseismic\135-d-m6-0471_0476_0481_0486_0491_0496-longriba2_kangding2\f123\sbas.4.0.0001.9999.20141026.20230301.071.0397.01.___\p.fa_Kunlun_Fault'};
% ptn='112'
% dmin=-200;
% dmax=200;
% paths={'Z:\g11j\D\gsar\interseismic\172-a-m8-0096_0101_0106_0111_0116_0121_0126_0131-eastkunlun7M3\f123\profile\p.fa_Kunlun_Fault'};
% paths={'Z:\g11j\D\gsar\interseismic\172-a-m8-0096_0101_0106_0111_0116_0121_0126_0131-eastkunlun7M3\f123\profile_des\p.fa_Kunlun_Fault'};
% paths={'Z:\g11j\D\gsar\interseismic\033-d-m7-0463_0468_0473_0478_0483_0487_0493-kunlun\f123\sbas.4.0.0001.9999.20141031.20230330.055.0556.01.___\p.fa_Kunlun_Fault'};
% paths={'Z:\g11j\D\gsar\interseismic\048-d-m6-0462_0467_0472_0477_0482-0487-kunlun\f123\sbas.4.0.0001.9999.20141101.20230401.054.0733.01.___\p.fa_Kunlun_Fault'};
% ptn='001';
% dmin=-300;
% dmax=1600;
% d_exclude=[-50,160];
% paths={'Z:\g11j\D\gsar\interseismic\106-d-m4-0470_0475_0480_0485-eastkunlun2M\f123\sbas.4.0.0367.9999.20150808.20210520.147.1355.01.___\p.fa_Kunlun_Fault_gnss'};
% paths={'Z:\g11j\D\gsar\interseismic\033-d-m4-0473_0478_0483_0487-eastkunlun8M3\f123\sbas.4.0.0367.9999.20141031.20210503.148.1299.01.___\p.fa_Kunlun_Fault_merge_gnss'};
% % paths={'Z:\g11j\D\gsar\interseismic\033-d-m7-0463_0468_0473_0478_0483_0487_0493-kunlun\f123\sbas.4.0.0001.9999.20141031.20210515.039.0231.01.___\p.fa_Kunlun_Fault'};
% paths={'Z:\g11j\D\gsar\interseismic\004-d-m6-0461_0466_0471_0476_0481_0486-eastkunlun\f123\sbas.4.0.0001.9999.20141029.20210501.062.0100.01.___\p.fa_Kunlun_Fault'};
% ptn='066'
% dmin=-100;
% dmax=200;
%d_exclude=[-160,150];
% % paths={'Z:\g11j\D\gsar\interseismic\033-d-m4-0473_0478_0483_0487-eastkunlun8M3\f123\sbas.4.0.0367.9999.20141031.20210503.148.1299.01.___\p.fa_Kunlun_Fault'};
% % paths={'Z:\g11j\D\gsar\interseismic\033-d-m4-0473_0478_0483_0487-eastkunlun8M3\f123\sbas.4.0.0367.9999.20141031.20210503.148.1299.01.___\p.fa_Kunlun_Fault_gnss'};
% paths={'Z:\g11j\D\gsar\interseismic\106-d-m6-0460_0465_0470_0475_0480_0485-eastkunlun2M\f123\sbas.4.0.0001.9999.20150808.20210520.038.0703.01.___\p.fa_Kunlun_Fault'};
% ptn='086'
% dmin=-100;
% dmax=200;
% d_exclude=[-160,150];


%xianshuihe fault
% path='D:\gsar\interseismic\135-d-m4-0476_0481_0486_0491-longriba2_kangding2\f123\sbas.4.0.0367.9999.20150106.20210522.101.1359.01.___\p.fa_xsh_b\';

%honghe fault
% path='Z:\g16h\gsar\062-d-m4-0507_0512_0517_0522-honghe2M3\f123\sbas.4.0.0001.9999.20141009.20220629.165.0732.01.___\p.fa_redriver';

% %jss-d121
% paths={'Z:\g11j\D\gsar\interseismic\121-d-m6-0462_0467_0472_0477_0482_0487-atf_kunlun\f123\sbas.4.0.0001.9999.20141026.20230325.048.0732.01.___\p.fa_jss'};
% ptn='136'
% ptn='13*'
% dmin=-300;
% % dmin=-240;
% dmax=300;
% d_exclude=[-100,150];

% %klf-d077
% paths={'Z:\g11j\D\gsar\interseismic\077-d-m7-0458_0463_0468_0473_0478_0483_0488-kunlun\f123\sbas.4.0.0001.9999.20141103.20230309.050.0670.01.___\p.fa_Kunlun_Fault'};
% ptn='046*'
% dmin=-360;
% dmax=170;
% d_exclude=[-240,10];

% %klf-d004
% paths={'Z:\g11j\D\gsar\interseismic\004-d-m6-0461_0466_0471_0476_0481_0486-eastkunlun\f123\sbas.4.0.0001.9999.20141029.20210513.077.0626.01.___\p.fa_Kunlun_Fault'};
% ptn='0*'
% dmin=-360;
% dmax=170;
% d_exclude=[-240,10];

% %klf-033
% paths={'Z:\g11j\D\gsar\interseismic\033-d-m7-0463_0468_0473_0478_0483_0487_0493-kunlun\f123\sbas.4.0.0001.9999.20141031.20210515.079.0345.01.___\p.fa_Kunlun_Fault'};
% ptn='*';
% dmin=-330;
% % dmin=-240;
% dmax=160;
% d_exclude=[-320,60];

% %klf-041
% paths={'Z:\g11j\D\gsar\interseismic\041-a-m6-0099_0104_0109_0114_0119_0124-altyntagh_kunlun\f123\sbas.4.0.0001.9999.20160107.20220920.050.0533.01.___\p.fa_Kunlun_Fault'};
% ptn='00*';
% dmin=-480;
% % dmin=-240;
% dmax=160;
% d_exclude=[-390,0];

% %klf-a128
% paths={'Z:\g11j\D\gsar\interseismic\128-a-m6-0085_0090_0095_0100_0105_0110-dayi1M3\f123\sbas.4.0.0367.9999.20150118.20230124.049.0400.01.___\p.fa_Kunlun_Fault'};
% ptn='114*';
% dmin=-480;
% % dmin=-240;
% dmax=120;
% d_exclude=[-90,100];

% %klf-a128 
% paths={'Z:\g11j\D\gsar\interseismic\128-a-m6-0100_0105_0110_0115_0120_0125-eastkunlun9M3\f123\sbas.4.0.0001.9999.20170107.20230406.051.0584.01.___\p.fa_Kunlun_Fault'};
% ptn='110*';
% dmin=-480;
% % dmin=-240;
% dmax=120;
% d_exclude=[-90,100];


%
% paths={'Z:\g11j\D\gsar\interseismic\106-d-m4-0470_0475_0480_0485-eastkunlun2M\f123\sbas.4.0.0001.9999.20150808.20210520.141.1300.01.___\p.fa_Kunlun_Fault'};
% ptn='*';
% dmin=-120;
% dmax=120;
% d_exclude=[-9,9];


% %farLock-d004
% paths={'Z:\g11j\D\gsar\interseismic\004-d-m6-0461_0466_0471_0476_0481_0486-eastkunlun\f123\sbas.4.0.0001.9999.20141029.20210513.077.0626.01.___\p.fa_Kunlun_Fault'};
% ptn='0*'
% dmin=-360;
% dmax=270;
% % d_exclude=[-240,10];

% paths={'Z:\g13b\proc_gmtsar\interseismic\062-d-m4-0507_0512_0517_0522-honghe2M3\f123\sbas.4.0.0367.9999.20141009.20220629.165.1393.01.___\p.fa_redriver'};
% ptn='04*'
% dmin=-200;
% dmax=80;

paths={'Z:\g11j\D\gsar\interseismic\056-a-m7-0088_0093_0098_0103_0108_0113_0118-tibet\comb\p.fa_karakoram'};
paths={'Z:\g11j\D\gsar\interseismic\063-d-m5-0462_0467_0472_0478_0483-aksaichin2_karakoram\comb\p.fa_karakoram'};
ptn='036*'
dmin=-100;
dmax=100;


% cmt='far'
% cmt='klfCreep'
%cmt='farLock'

npath=size(paths,1);

% dmin=-120;
% dmax=160;
% fts_min=-100;
% fts_max=25;
% fts_min=-35;
% fts_max=35;
% fts_min=-15;
% fts_max=15;
fts_min=-3;
fts_max=3;
is_show_fig='on';
%is_show_fig='off';


dmin_fts=dmin%+fts_min;
dmax_fts=dmax%+fts_max;

for pi=1:npath
  path=paths{pi};
  if isempty(path)
    continue
  end
  disp(path);
  
  files=dir([path,filesep,'profile_',ptn,'*_vel.psxy']);
  
  if ( strcmp(cmt , '') == 1 )
    opath=path
  else
    opath=[path,filesep,cmt]
    if (isdir(opath) == 0)
      mkdir (opath);
    end
  end
  
  nf=size(files,1);
  for fi=1:nf
    % file='D:\gsar\interseismic\077-d-m3-0468_0473_0478-eastkunlun6M3\f123\sbas.4.0.0367.9999.20141103.20210506.149.1127.01.___\p.fa_eklf\profile_103_vel.psxy'
    % % file='D:\Papers\gyaringco\figure\profiles.gps\pg\profile_003_vel2';
    % file='D:\gsar\interseismic\150-d-m3-0487_0492_0497-dangxiong2_yzs3_gulu1\f123\sbas.4.0.0700.9999.20141027.20200820.128.1753.01.x2\p.fa_bengco\profile_010_vel.psxy';
    % % file='D:\gsar\interseismic\092-d-m3-0465_0470_0475-altyntagh_M3\f123.1\sbas.3.0.0350.9999.20141012.20180729.051.0339.01.roiref\p.fa_atf\profile_025_vel.psxy';
    % % file='D:\gsar\interseismic\092-d-m3-0465_0470_0475-altyntagh_M3\f123.1\sbas.3.0.0350.9999.20141012.20180729.051.0339.01.___\p.fa_atf\profile_025_vel.psxy'
    
    file=[path,filesep,files(fi).name];
    [pathstr, name, ext] = fileparts(file);
    
    str=sprintf('[%s]INFO: %d/%d processing %s ...',PROG,fi,nf,name);
    disp(str);
    
    
    ofile=[opath,filesep,name,'_mdl.txt'];
    ofile_rot=[opath,filesep,name,'_rot.txt'] ;
    jfile=[opath,filesep,name,'_mdl.jpg'];
    disp(['output to:',ofile]);
    % return
    
    fid = fopen(file);
    
    tline = fgetl(fid);
    nl=0;
    while ischar(tline)
      if strcmp(tline(1),' ') ~= 0
        
        %disp(tline);
        nl=nl+1;
        if nl == 1
          dlines=sscanf(tline,'%f');
        else
          dlines(:,nl)=sscanf(tline,'%f');
        end
        %break
      end
      tline = fgetl(fid);
      %pause
    end
    
    fclose(fid);
    
    x1=dlines(11,:);
    y1=dlines(12,:);
    min(y1);
    max(y1);
    
    if d_exclude(1) ~= 9999
      ind3=find(x1<d_exclude(1) | x1>d_exclude(2));
      if isempty(ind3)
        disp('[]WARNING: no data nearby fault.');
        continue
      end
      
    x3=x1(ind3);
    y3=y1(ind3);
    else
      x3=x1;
      y3=y1;
    end
    
    ind1=find(x3>=dmin_fts & x3<=dmax_fts);
    if isempty(ind1)
      disp('[]WARNING: no data nearby fault.');
      continue
    end
    x2=x3(ind1);
    y2=y3(ind1);
    
    ind2=find(x2<fts_min);
    if isempty(ind2)
      disp('[]WARNING: no data in the left side of fault.');
      continue
    end
    ind2=find(x2>fts_max);
    if isempty(ind2)
      disp('[]WARNING: no data in the right side of fault.');
      continue
    end
  
    
    % ir=random('Normal',0,1,1,390);
    % ind2=fix((ir/max(abs(ir))+1)*(size(x2,2)-2)/2)+1;
    % x=x2(ind2);
    % y=y2(ind2);
    x=x2;
    y=y2;
    % x=x+10;
    %y=y+2.6;
    %plot(x,y,'s');
    ymin=min(y);
    ymax=max(y);
    
    
    clear data model options
    data.xdata=x;
    data.ydata=y;
    h1=figure(1);
    set(h1,'Visible',is_show_fig);
    clf
    set(gcf, 'Position', [100 100 900 650]);
    subplot(2,1,1);
    plot(x1,y1,'r.');
    hold on;
    plot(data.xdata,data.ydata,'.');
    xlim([-400 400]);ylim([-6 6]);  xlabel('x [km]'); ylabel('y [mm/yr]');
    
    
    % fun_2d_screw_dislocation = @(x,param) param(1)/pi*atan((x-param(4))/param(2))+param(3);
    ssfun    = @(param,data) sum((data.ydata-fun_2d_screw_dislocation(data.xdata,param)).^2);
    
    [tmin,ssmin]=fminsearch(ssfun,[3;4;(ymin+ymax)/2;0;.01],[],data);
    n = length(data.xdata);
    p = 2;
    mse = ssmin/(n-p); % estimate for the error variance
    
    J = [data.xdata./(tmin(2)+data.xdata), ...
      -tmin(1).*data.xdata./(tmin(2)+data.xdata).^2];
    % tcov = inv(J'*J)*mse;
    %tcov = (J'*J) \ mse
    % tcov = pinv(J'*J)*mse;
    
    params = {
      {'sr', 0, -20, 20}
%       {'ld', 10, .1, 50}
%       {'ld', 10, .1, 25}
      {'ld', 4, 0, 10}
%       {'ld', 10, .1, 15}
%       {'ld', .1, .01, .2}
%       {'ld', 9, 8.9, 9.1}
      {'yshift', (ymin+ymax)/2, ymin,ymax}
      %     {'fts', 0, -30, 30}
%           {'fts', 0, -1, 1}
      {'fts', 0, fts_min, fts_max}
      
      {'rot', 0.001, -pi/3, pi/3}
%       {'rot', 0.000011, 0.000010, 0.000012}
      };
    
    model.ssfun  = ssfun;
    model.sigma2 = mse; % (initial) error variance from residuals of the lsq fit
    
    model.N = length(data.ydata);  % total number of observations
    model.S20 = model.sigma2;      % prior mean for sigma2
    model.N0  = 4;                 % prior a
    
    options.nsimu = 5000;
    options.updatesigma = 1;
    % options.qcov = tcov; % covariance from the initial fit
    options.verbosity = 1;
    options.waitbar = 1;
    %   options.MaxFunEvals = 10000;
    
    [res,chain,s2chain] = mcmcrun(model,data,params,options);
    
      figure(2); clf
      mcmcplot(chain,[],res,'chainpanel');
    
      figure(3); clf
      mcmcplot(chain,[],res,'pairs');
    
      figure(4); clf
      mcmcplot(sqrt(s2chain),[],[],'hist')
      title('Error std posterior')
    
      % add prior distribution to the plot, if it was informative
      if res.N0>0
        xl = xlim; xx = linspace(xl(1),xl(2));
        hold on
        plot(xx,invchi1pf(xx,res.N0,sqrt(res.S20)));
        hold off;
        legend('posterior','prior');
      end
    %
    
    out_params=mean(chain);
    out_stds=std(chain);
    %out_sigs=0.9945*out_stds/sqrt(size(chain,1))
    %out_sigs=0.9945*out_stds/sqrt(20);
    out_sigs=0.9945*out_stds/sqrt(1);
    %   out_sigs=1.96*out_stds;
    
    %   xo = linspace(-300,300)';
    xo=x1';
    yo=fun_2d_screw_dislocation(xo,mean(chain));
    %figure(1)
    set(0,'CurrentFigure',h1);
    subplot(2,1,1);
    hold on
    plot(xo,fun_2d_screw_dislocation(xo,mean(chain)),'-k')
    disp(['model result: ',num2str(mean(chain))]);
    hold off
    legend({'raw','used','model'},'Location','BestOutside');
    
    h5=figure(5);
    set(h5,'Visible',is_show_fig);
    clf
    out = mcmcpred(res,chain,[],xo,'fun_2d_screw_dislocation');
    %   out = mcmcpred(res,chain,s2chain,xo,'fun_2d_screw_dislocation');
    mcmcpredplot(out);
    hold on
    plot(data.xdata,data.ydata,'.'); % add data points to the plot
    xlabel('x [km]'); ylabel('y [mm/yr]');
    hold off
    title('Predictive envelopes of the model')
    
    %figure(1);
    set(0,'CurrentFigure',h1);
    subplot(2,1,2);
    xor=xo*cos(-1*out_params(5))-yo*sin(-1*out_params(5));
    yor=xo*sin(-1*out_params(5))+yo*cos(-1*out_params(5));
    x1r=x1*cos(-1*out_params(5))-y1*sin(-1*out_params(5));
    y1r=x1*sin(-1*out_params(5))+y1*cos(-1*out_params(5));
    plot(x1r,y1r,'.');
    hold on;
    plot(xor,yor,'-k');
    hold off;
    legend({'data','model'},'Location','BestOutside')
    xlim([-400 400]); xlabel('x [km]'); ylabel('y [mm/yr]');
    
    np = size(out.predlims{1}{1},1);
    nn = (np+1)/2; % median
    np = nn-1;
    nbatch = length(out.predlims);
    plimi = out.predlims{1};
    yo_lowers=plimi{1}(1,:)';
    yo_uppers=plimi{1}(2*nn-1,:)';
    yo_means=plimi{1}(nn,:)';
    %
    %   yo_lowers=out.obslims{1}{1}(1,:)';
    %   yo_uppers=out.obslims{1}{1}(3,:)';
    %
    h6=figure(6);
    set(h6,'Visible',is_show_fig);
    clf
    dimc = [0.8,0.8,0.8];
    fillyy(xo,yo_lowers,yo_uppers,dimc);
    hold on;
    plot(xo,yo_lowers,'r');
    plot(xo,yo_uppers,'r');
    plot(xo,plimi{1}(nn,:),'-k');
    hold off;
    
    
    yor_lowers=xo*sin(-1*out_params(5))+yo_lowers*cos(-1*out_params(5));
    yor_uppers=xo*sin(-1*out_params(5))+yo_uppers*cos(-1*out_params(5));
    yor_means=xo*sin(-1*out_params(5))+yo_means*cos(-1*out_params(5));
    h7=figure(7);
    set(h7,'Visible',is_show_fig);
    clf
    dimc = [0.8,0.8,0.8];
    fillyy(xor,yor_lowers,yor_uppers,dimc);
    hold on;
    plot(xor,yor_lowers,'r');
    plot(xor,yor_uppers,'r');
    plot(xor,yor_means,'-k');
    hold off;
    
    
    set(0,'CurrentFigure',h1);
    subplot(2,1,2);
    hold on;
    fillyy(xor,yor_lowers,yor_uppers,dimc);
    plot(xor,yor_lowers,'r');
    plot(xor,yor_uppers,'r');
    
    
    subplot(2,1,1);
    hold on;
    tmpstr1=sprintf('slip rate: %7.2f +/- %7.2f\n', out_params(1), out_sigs(1));
    tmpstr2=sprintf('locking depth: %7.2f +/- %7.2f\n', out_params(2), out_sigs(2));
    tmpstr3=sprintf('fault trace shift (km): %7.2f +/- %7.2f\n', out_params(4), out_sigs(4));
    tmpstr4=sprintf('rotation (deg): %9.6f +/- %9.6f\n', out_params(5), out_sigs(5));
    tmpstr5=sprintf('velocity offset: %7.2f +/- %7.2f\n', out_params(3), out_sigs(3));
    text(-380,4,tmpstr1);
    text(-380,3,tmpstr2);
    text(-380,2,tmpstr3);
    text(-380,1,tmpstr4);
    text(-380,0,tmpstr5);
    
    
    
    fido=fopen(ofile,'w');
    fprintf(fido,'* SRC: %s\n', file);
    fprintf(fido,'*PROG: %s\n', PROG);
    fprintf(fido,'* ver: %s\n', ver);
    fprintf(fido,'*user: %s\n', user);
    fprintf(fido,'*run@: %s\n', run_at);
    fprintf(fido,'*  on: %s\n', run_on);
    fprintf(fido,'*model paramters:\n');
    fprintf(fido,'* far-field strike-slip rates: %f +/- %f\n', out_params(1), out_sigs(1));
    fprintf(fido,'* locking depth: %f +/- %f\n', out_params(2), out_sigs(2));
    fprintf(fido,'* fault trace shift (km): %f +/- %f\n', out_params(4), out_sigs(4));
    fprintf(fido,'* angle of axis rotation (deg): %f +/- %f\n', out_params(5), out_sigs(5));
    fprintf(fido,'* velocity offset: %f +/- %f\n', out_params(3), out_sigs(3));
    fprintf(fido,'*\n');
    fprintf(fido,'*%15s %15s %15s %15s %15s\n',  'dist_to_fault',      'prediction',      'horiz_pred','pred_lower','pred_upper');
    for i=1:size(xo,1)
      fprintf(fido,' %15.6f %15.6f %15.6f %15.6f %15.6f\n',xo(i),yo(i),0,yo_lowers(i),yo_uppers(i));
    end
    fclose(fido);
    
    
    fido=fopen(ofile_rot,'w');
    fprintf(fido,'* SRC: %s\n', file);
    fprintf(fido,'*PROG: %s\n', PROG);
    fprintf(fido,'* ver: %s\n', ver);
    fprintf(fido,'*user: %s\n', user);
    fprintf(fido,'*run@: %s\n', run_at);
    fprintf(fido,'*  on: %s\n', run_on);
    fprintf(fido,'*model paramters:\n');
    fprintf(fido,'* far-field strike-slip rates: %f +/- %f\n', out_params(1), out_sigs(1));
    fprintf(fido,'* locking depth: %f +/- %f\n', out_params(2), out_sigs(2));
    fprintf(fido,'* fault trace shift (km): %f +/- %f\n', out_params(4), out_sigs(4));
    fprintf(fido,'* angle of axis rotation (deg): %f +/- %f\n', out_params(5), out_sigs(5));
    fprintf(fido,'* velocity offset: %f +/- %f\n', out_params(3), out_sigs(3));
    fprintf(fido,'*\n');
    fprintf(fido,'*%15s %15s %15s %15s %15s\n',  'dist_to_fault',      'vLos_rotated',      'vLos_modeled','pred_lower','pred_upper');
    for i=1:size(xor,1)
      fprintf(fido,' %15.6f %15.6f %15.6f %15.6f %15.6f\n',xor(i),y1r(i),yor(i),yor_lowers(i),yor_uppers(i));
    end
    fclose(fido);
    
    
    %figure(1);
    %   set(0,'CurrentFigure',h1);
    %   frame=getframe(gcf);
    %   im=frame2im(frame);
    %   imwrite(im,jfile,'jpg');
    saveas(h1,jfile);
    
    % return
    
  end
end

disp(['[',PROG,']INFO: Normal end.']);