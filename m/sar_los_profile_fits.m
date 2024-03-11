clear
clc

PROG='sar_los_profile_fits';
ver='20240103';
% user=getenv('User');
[s,user]=unix('whoami');
[s,run_at]=system('hostname');
run_on=datestr(now);
disp(['[',PROG,'(',ver,')]INFO: run by ',user]);

%x data range used (km)
dmin=-200;  %200 km south/west of the fault
dmax=200;   %200 km north/east of the fault

% default fault trace shift range
fts_min=-15;    %15 km south/west of the fault
fts_max=15;     %15 km north/east of the fault

% middle part to exclude
d_exclude=[9999,9999]; %no exclusion
%d_exclude=[-6,9]; %exclude data between 6 km south/west of the fault and 9 km north/east of the fault

% addition comment added to the output path
cmt='';

% whether how figure plots
is_show_fig='off';
% is_show_fig='on';

% Which columns in profile files should be used?
%distance index
di=13; %for new format
%*   01site   02pLon  03pLat 04pDist   05VNor 06VeNor   07VPar 08VePar    09VUp  10VeUp    11lon   12lat  13distFa   14VLOS 15VeLOS     16VE     17VN   18VEe   19VNe   20CEN   21CEU  22CNU
%di=11;  %for old velocity format
%*            site     p_long    p_lati     p_dist    v_along  ve_along     v_tang   ve_tang       long      lati dist_to_fault       v_los     ve_los

%velocity index
% vi=5; % * 05VNor           : velocity along the profile (normal to the fault trace); positive-north (mm/yr)
vi=7;  % * 07VPar           : velocity tangent to the profile (parallel to the fault trace); positive-90deg-clockwise from v_along direction (mm/yr)
% vi=9; % * 09VUp            : vertical velocity; positive-up (mm/yr)
% vi=14;% * 14VLOS           : InSAR LOS velocity (mm/yr)
% vi=16;% * 16VE             : east velocity of location; positive-east (mm/yr)
% vi=17;% * 17VN             : north velocity of location; positive-north (mm/yr)

%method to estimate profile tilt
tilt_correction=1; %rotation of coordinate system
tilt_correction=2; %linear trend (Segall, 2010)

%MCMC constraints
nsimu=5000; %number of simulations
% npar=5 ;


% paths={'C:\tmp\gic3dv\kunlun\asc_des\sbas.4.0.0367.9999.20150808.20210520.147.1355.01.___\p.fa_kunlun_Fault_gic3dv_out_horizontal_twofaults'};
% ptn='016*'
% dmin=-400;
% dmax=200;
% ptn='103*'
% dmin=-600;
% dmax=-200;

% paths={'/g17b/gsar/D/gsar/interseismic/070-a-m6-0100_0105_0110_0115_0120_0125-eastkunlun5M3/f123/sbas.4.0.0001.9999.20141022.20230225.056.0770.01.___/p.fa_Kunlun_Fault_dec3d_byN_all_2km'};
% paths={'D:\gsar\interseismic\070-a-m6-0100_0105_0110_0115_0120_0125-eastkunlun5M3\f123\sbas.4.0.0001.9999.20141022.20230225.056.0770.01.___\p.fa_Kunlun_Fault_dec3d_byN_all_2km_toGYXSH'};
%
% ptn='031*'
% dmin=-300;
% dmax=200;
% dmax=150;
% cmt='farLock'
%
% paths={'D:\gsar\interseismic\070-a-m6-0100_0105_0110_0115_0120_0125-eastkunlun5M3\f123\sbas.4.0.0001.9999.20141022.20230225.056.0770.01.___\p.fa_Kunlun_Fault_dec3d_byN_all_2km'};
% ptn='039*';
% dmin=-300;
% dmax=300;
% 
% paths={'D:\gsar\interseismic\077-d-m3-0468_0473_0478-eastkunlun6M3\f123\sbas.4.0.0367.9999.20141103.20210506.149.1127.01.___\p.fa_Kunlun_Fault'};
% 
% paths={'D:\gsar\interseismic\070-a-m6-0100_0105_0110_0115_0120_0125-eastkunlun5M3\f123\asc_des\sbas.4.0.0367.9999.20150808.20210520.147.1355.01.___\p.fa_kunlun_Fault_gic3dv_out_horizontal'};
% % paths={'D:\gsar\interseismic\070-a-m6-0100_0105_0110_0115_0120_0125-eastkunlun5M3\f123\asc_des\sbas.4.0.0367.9999.20150808.20210520.147.1355.01.___\p.fa_ganzi_yushu_xianshuihe_gic3dv_out_horizontal_a1'};
% % % paths={'D:\Papers\kunlun\figure\gps.profile\pg.klf.zhang.pz03'};
% % paths={'D:\gsar\interseismic\070-a-m6-0100_0105_0110_0115_0120_0125-eastkunlun5M3\f123\sbas.4.0.0001.9999.20141022.20230225.056.0770.01.___\p.fa_ganzi_xianshuihe_dec3d_byN_all_2km'};
% paths={'D:\gsar\interseismic\070-a-m6-0100_0105_0110_0115_0120_0125-eastkunlun5M3\f123\sbas.4.0.0001.9999.20141022.20230225.056.0770.01.___\p.fa_wudaoliang_changshagongma_dec3d_byN_all_1km'};
% ptn='029*';
% % di=11;
% % vi=12;
% % ptn='*';
% dmin=-90;
% dmax=90;
% tilt_correction=2;

% paths={'\\10.4.134.30\root\g6h\gsar\gic3dv\iys\asc_des\sbas.4.0.0001.9999.20150506.20231108.217.0639.01.___\p.fa_iys_insar3d'};
% paths={'D:\gsar\interseismic\121-d-m4-0486_0492_0497_0502-dingjie_gyaringco2\f123\sbas.4.0.0001.9999.20150506.20231108.217.0639.01.___\p.fa_iys'};
% ptn='167*';
% vi=14;
% % di=11;
% % vi=12;
% % ptn='*';
% dmin=-50;
% dmax=60;
% tilt_correction=2;
% 
% %menyuan
% paths={'D:\gsar\interseismic\033-d-m4-0458_0463_0468_0473-qinghai_lake_haiyuan\f123\sbas.4.0.0367.9999.20141031.20210421.147.1280.01.___\p.fa_haiyuan'};
% % paths={'D:\gsar\interseismic\128-a-m3-0115_0120_0125-haiyuan\f123\sbas.4.9.0367.9999.20141014.20201006.136.1451.01.___\p.fa_haiyuan'};
% paths={'/g17b/gsar/D/gsar/interseismic/033-d-m4-0458_0463_0468_0473-qinghai_lake_haiyuan/f123/sbas.4.0.0367.9999.20141031.20210421.147.1280.01.___/p.fa_haiyuan'; ...
%     '/g17b/gsar/D/gsar/interseismic/128-a-m3-0115_0120_0125-haiyuan/f123/sbas.4.9.0367.9999.20141014.20201006.136.1451.01.___/p.fa_haiyuan'; ...
%     '/g17b/gsar/D/gsar/interseismic/026-a-m4-0117_0122_0127_0132-qilian/f123/sbas.4.0.0367.9999.20141019.20210208.140.1329.01.___/p.fa_haiyuan'};
% paths={'C:\tmp\gic3dv\hyf\asc_des\p.fa_haiyuan_insar3d2'};
% paths={'D:\gsar\interseismic\062-d-m6-0447_0452_0457_0462_0467_0472-haiyuan4M3\f123\sbas.4.0.0001.9999.20150206.20210517.053.0561.01.___\p.fa_haiyuan_insar3d_1920'};
% % paths={'D:\gsar\interseismic\055-a-m3-0112_0117_0122-haiyuan1M3\f123\sbas.4.0.0367.9999.20141021.20210423.122.1215.01.___\p.fa_haiyuan'};
% 
% vi=7; %fault-parallel
% ptn='061*';
% % vi=14;
% % di=11;
% % vi=12;
% % ptn='*';
% dmin=-200;
% dmax=200;
% % dmin=-100;
% % dmax=200;
% 
% d_exclude=[-120,20];
% tilt_correction=2;

%
% paths={'D:\gsar\interseismic\048-d-m5-0478_0483_0488_0493_0498-sewa3_wulan1_gyaringco3_ranwu2_iys\f123\sbas.4.0.0001.9999.20150113.20230131.065.0698.01.___\p.fa_bengco'};
% paths={'D:\gsar\interseismic\048-d-m5-0478_0483_0488_0493_0498-sewa3_wulan1_gyaringco3_ranwu2_iys\f123\sbas.4.0.0001.9999.20150113.20230131.065.0698.01.___\p.fa_gyaringco'};
% % paths={'D:\gsar\interseismic\121-d-m6-0462_0467_0472_0477_0482_0487-atf_kunlun\f123\sbas.4.0.0001.9999.20141026.20230325.048.0732.01.___\p.fa_gcf'};
% ptn='011*'
% dmin=-1200;
% dmax=80;

% % ptn='102*'
% dmin=-200;
% dmin=-30;
% dmax=200;
%
% ptn='029*'
% % ptn='102*'
% dmin=-300;
% % dmin=-30;
% dmax=300;

% d_exclude=[-1,400];

% 
% paths={'/g17c/gsar/gic3dv/tianshan/asc_des/figure/p.fa_maidan_shayilamu_3d'};
% ptn='018';
% dmin=-200;
% dmax=200;
% 
% paths={'D:\gsar\interseismic\026-a-m5-0112_0117_0122_0127_0132-qilian\f123\sbas.4.0.0001.9999.20141019.20231219.072.0605.01.___\p.fa_haiyuan'};
% ptn='014';
% dmin=-200;
% dmax=200;
% vi=14;
% 
% paths={'D:\gsar\interseismic\077-d-m7-0475_0480_0485_0490_0495_0500_0505-jiali\f123\asc_des\p.fa_jiali_i3d'};
% paths={'D:\gsar\interseismic\077-d-m7-0475_0480_0485_0490_0495_0500_0505-jiali\f123\asc_des\p.fa_bengco_jiali_ext2_i3d'};
% % paths={'D:\gsar\interseismic\077-d-m7-0475_0480_0485_0490_0495_0500_0505-jiali\f123\asc_des\p.fa_yadong_gulu_appro_i3d'};
% % paths={'D:\gsar\interseismic\077-d-m7-0475_0480_0485_0490_0495_0500_0505-jiali\f123\asc_des\p.fa_sangri_cuona_east_ext_i3d'};
% ptn='1*';
% dmin=-200;
% dmax=200;
% vi=7;

paths={'D:\gsar\gic3dv\g219\asc_des\profiles\p.fa_gozhaco'};
% paths={'D:\gsar\gic3dv\g219\asc_des\profiles\p.fa_tianshen_daban\'};
% paths={'D:\gsar\gic3dv\g219\asc_des\profiles\p.fa_longmuco_dulishihu'};
% paths={'D:\gsar\gic3dv\pishan\asc_des\profiles\p.fa_gozhaco'};
paths={'/g17b/gsar/D/gsar/gic3dv/g219/asc_des/profiles/p.fa_xiaoerkule1'};
paths={'/g17b/gsar/D/gsar/gic3dv/g219/asc_des/profiles/p.fa_jieze_caka'};
paths={'/g17b/gsar/D/gsar/gic3dv/g219/asc_des/profiles/p.fa_longmuco_to_atf'};
paths={'D:\gsar\gic3dv\mht\asc_des\profiles\p.fa_mbt'};
paths={'D:\gsar\interseismic\041-a-m4-0109_0114_0119_0124-altyntagh_M3\f123\sbas.4.0.0367.9999.20141020.20210901.166.1299.01.___\p.fa_atf'};
ptn='066';
dmin=-100;
dmax=150;
vi=7;
vi=14;
nsimu=30000;

cmt='parallel_farLock';
cmt='parallel_nearLock';
% cmt='up';
% cmt='normal';


% cmt='farCreep'
% cmt='klfCreep'
% cmt='farLock'

npath=size(paths,1);

% dmin=-120;
% dmax=160;
%
% fts_min=-350;
% fts_max=-250;
% fts_min=180;
% fts_max=220;
% fts_min=-100;
% fts_max=100;
% fts_min=-55;
% fts_max=55;
% fts_min=-35;
% fts_max=35;
fts_min=-15;
fts_max=15;
% fts_min=-3;
% fts_max=3;
is_show_fig='on';
%is_show_fig='off';


dmin_fts=dmin;  %+fts_min;
dmax_fts=dmax;  %+fts_max;

switch tilt_correction
    case 1
        func_name='fun_2d_screw_dislocation';
    case 2
        func_name='fun_2d_screw_dislocation_trend';
    otherwise
        disp('[]ERROR: invalid option',tile_correction,'!');
end

for ii=1:npath
    path=paths{ii};
    if isempty(path)
        continue
    end
    disp(path);
%     continue
    
    files=dir([path,filesep,'profile_',ptn,'*_vel.psxy']);
    
    if ( strcmp(cmt , '') == 1 )
        opath=path;
    else
        opath=[path,filesep,cmt]
        if (isdir(opath) == 0)
            mkdir (opath);
        end
    end    
    
    nf=size(files,1);
    
    for fi=1:nf        
        
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
                dlines(:,nl)=sscanf(tline,'%*s %f %f %f %f %f       %f %f %f %f %f       %f %f %f %f %f       %f %f %f %f %f       %f %f %f %f %f ');
                %break
            end
            tline = fgetl(fid);
            %pause
        end
        fclose(fid);
        
        x1=dlines(di-1,1:nl);  %di 13-distance to fault column
        y1=dlines(vi-1,1:nl);  %vi 7-fault-parallel (default)
        %     y1=dlines(14-1,1:nl);
        
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
        
        
        params = {
            {'sr', 0, -20, 20}
%             {'ld', 5, .1, 10}
            %       {'ld', 10, .1, 25}
%                   {'ld', 1, 0, 50}
%                   {'ld', 1, 0, 20}
                  {'ld', 10, .1, 15}
                  % {'ld', .1, .01, .2}
            %       {'ld', 9, 8.9, 9.1}
            %     {'fts', 0, -30, 30}
            %           {'fts', 0, -1, 1}
            {'fts', (fts_min+fts_max)/2, fts_min, fts_max}
            {'yshift', (ymin+ymax)/2, ymin,ymax}
            
            {'rot', 0.001, -pi/3, pi/3}
                  % {'rot', 0.000011, 0.000010, 0.000012}
            };
        npar=size(params,1);
        
        clear data model options
        data.xdata=x;
        data.ydata=y;
        h1=figure(1);
        set(h1,'Visible',is_show_fig);
        clf
        set(gcf, 'Position', [100 100 1200 800]);
        subplot(2,1,1);
        plot(x1,y1,'r.');
        hold on;
        plot(data.xdata,data.ydata,'.');
        xlim([-600 600]);ylim([-20 20]);  xlabel('x [km]'); ylabel('y [mm/yr]');
        
        switch tilt_correction
            case 1
                ssfun    = @(param,data) sum((data.ydata-fun_2d_screw_dislocation(data.xdata,param)).^2);
            case 2
                ssfun    = @(param,data) sum((data.ydata-fun_2d_screw_dislocation_trend(data.xdata,param)).^2);
        end
        
        [tmin,ssmin]=fminsearch(ssfun,[3;4;0;(ymin+ymax)/2;.01],[],data);
        n = length(data.xdata);
        p =  size(params,1);
        mse = ssmin/(n-p); % estimate for the error variance         
        model.ssfun  = ssfun;
        model.sigma2 = mse; % (initial) error variance from residuals of the lsq fit        
        model.N = length(data.ydata);  % total number of observations
        model.S20 = model.sigma2;      % prior mean for sigma2
        model.N0  = 4;                 % prior a        
        options.nsimu = nsimu;
        options.updatesigma = 1;
%         options.method  = 'dram';
% options.nsimu   = 10000;
% options.qcov    = eye(npar)*1.5; % [initial] proposal covariaance

%         options.qcov     = eye(npar)/npar*2.4^2.;
% options.qcov     = eye(npar)*.001;
%         options.qcov     = eye(npar)/npar*2.4^2.;
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
        out_stat=chainstats(chain,res);
        out_sigs=out_stat(:,3);
        
        %   xo = linspace(-300,300)';
        xo=x1';
        switch tilt_correction
            case 1
                yo=fun_2d_screw_dislocation(xo,mean(chain));
                out = mcmcpred(res,chain,[],xo,'fun_2d_screw_dislocation');
                
                np = size(out.predlims{1}{1},1);
                nn = (np+1)/2; % median
                np = nn-1;
                nbatch = length(out.predlims);
                plimi = out.predlims{1};
                yo_lowers=plimi{1}(1,:)';
                yo_uppers=plimi{1}(2*nn-1,:)';
                yo_means=plimi{1}(nn,:)';                
                
                %rotate -theta (theta clockwisely)
                xor=xo*cos(-1*out_params(5))-yo*sin(-1*out_params(5));
                yor=xo*sin(-1*out_params(5))+yo*cos(-1*out_params(5));
                x1r=x1*cos(-1*out_params(5))-y1*sin(-1*out_params(5));
                y1r=x1*sin(-1*out_params(5))+y1*cos(-1*out_params(5));
                yor_lowers=xo*sin(-1*out_params(5))+yo_lowers*cos(-1*out_params(5));
                yor_uppers=xo*sin(-1*out_params(5))+yo_uppers*cos(-1*out_params(5));
                yor_means=xo*sin(-1*out_params(5))+yo_means*cos(-1*out_params(5));
                
                title_str=sprintf('%5s %20s %20s %20s %20s %20s\n','fault', 'slip rate(mm/yr)','locking depth(km)','trace shift(km)','velocity shift','rotation angle(deg)');
                result_str=sprintf('%5d          %5.1f\\pm%-5.1f          %5.1f\\pm%-5.1f          %5.1f\\pm%-5.1f          %5.1f\\pm%-5.1f          %5.1f\\pm%-5.1f\n',1, out_params(1), out_sigs(1), out_params(2), out_sigs(2), out_params(3), out_sigs(3), out_params(4), out_sigs(4), rad2deg(out_params(5)), rad2deg(out_sigs(5)));
                
                out_tilt_str=sprintf('* angle of axis rotation (deg): %f +/- %f', rad2deg(out_params(5)), rad2deg(out_sigs(5)) );
            case 2
                yo=fun_2d_screw_dislocation_trend(xo,mean(chain));
                out = mcmcpred(res,chain,[],xo,'fun_2d_screw_dislocation_trend');
                
                np = size(out.predlims{1}{1},1);
                nn = (np+1)/2; % median
                np = nn-1;
                nbatch = length(out.predlims);
                plimi = out.predlims{1};
                yo_lowers=plimi{1}(1,:)';
                yo_uppers=plimi{1}(2*nn-1,:)';
                yo_means=plimi{1}(nn,:)';
                
                %remove linear trend (B*x)
                xor=xo;
                y_tilt=out_params(5)*(xo-out_params(3));
                yor=yo-y_tilt;
                x1r=x1;
                y1r=y1-out_params(5)*(x1-out_params(3));
                yor_lowers=yo_lowers-y_tilt;
                yor_uppers=yo_uppers-y_tilt;
                yor_means=yo_uppers-y_tilt;
                
                title_str=sprintf('%5s %20s %20s %20s %20s %20s\n','fault', 'slip rate(mm/yr)','locking depth(km)','trace shift(km)','velocity shift','linear trend(deg)');
                result_str=sprintf('%5d          %5.1f\\pm%-5.1f          %5.1f\\pm%-5.1f          %5.1f\\pm%-5.1f          %5.1f\\pm%-5.1f          %5.1f\\pm%-5.1f\n',1, out_params(1), out_sigs(1), out_params(2), out_sigs(2), out_params(3), out_sigs(3), out_params(4), out_sigs(4),  rad2deg(atan(out_params(5))), rad2deg(atan(out_sigs(5))));
                    
                out_tilt_str=sprintf('* angle of profile trend (deg): %f +/- %f', rad2deg(atan(out_params(5))), rad2deg(atan(out_sigs(5))) );
            
        end
        %figure(1)
        set(0,'CurrentFigure',h1);
        subplot(2,1,1);
        hold on        
        plot(xo,yo,'-k')
        disp(['model result: ',num2str(mean(chain))]);
        hold off
        legend({'raw','used','model'},'Location','southeast');
        
        h5=figure(5);
        set(h5,'Visible',is_show_fig);
        clf
        %   out = mcmcpred(res,chain,s2chain,xo,'fun_2d_screw_dislocation');
        mcmcpredplot(out);
        hold on
        plot(data.xdata,data.ydata,'.'); % add data points to the plot
        xlabel('x [km]'); ylabel('y [mm/yr]');
        hold off
        title('Predictive envelopes of the model')        

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
        
        %figure(1);
        set(0,'CurrentFigure',h1);
        subplot(2,1,2);
        plot(x1r,y1r,'.');
        hold on;
        plot(xor,yor,'-k');
        legend({'data','model'},'Location','southeast')
        xlim([-600 600]); ylim([-20 20]); xlabel('x [km]'); ylabel('y [mm/yr]');   
        fillyy(xor,yor_lowers,yor_uppers,dimc);
        plot(xor,yor_lowers,'r');
        plot(xor,yor_uppers,'r');
        plot([out_params(3),out_params(3)],[-100,100],'--');
        plot([-100,100],[out_params(4),out_params(4)],'--');
        hold off;       
        %2nd subplot
        subplot(2,1,1);
        hold on;
%         title_str=sprintf('%5s %20s %20s %20s %20s %20s\n','fault', 'slip rate(mm/yr)','locking depth(km)','trace shift(km)','velocity shift','rotation angle(rad)');
        text(-580,17,title_str);
%         result_str=sprintf('%5d          %5.1f\\pm%-5.1f          %5.1f\\pm%-5.1f          %5.1f\\pm%-5.1f          %5.1f\\pm%-5.1f          %5.1f\\pm%-5.1f\n',1, out_params(1), out_sigs(1), out_params(2), out_sigs(2), out_params(3), out_sigs(3), out_params(4), out_sigs(4), rad2deg(out_params(5)), rad2deg(out_sigs(5)));
        text(-580,14,result_str);
        plot([out_params(3),out_params(3)],[-100,100],'--');
        plot([-100,100],[out_params(4),out_params(4)],'--');
        
        %write output file (modeled)
        fido=fopen(ofile,'w');
        fprintf(fido,'* SRC: %s\n', file);
        fprintf(fido,'*PROG: %s\n', PROG);
        fprintf(fido,'* ver: %s\n', ver);
        fprintf(fido,'*user: %s', user);
        fprintf(fido,'*run@: %s', run_at);
        fprintf(fido,'*  on: %s\n', run_on);
        fprintf(fido,'*model paramters:\n');
        fprintf(fido,'* far-field strike-slip rates: %f +/- %f\n', out_params(1), out_sigs(1));
        fprintf(fido,'* locking depth: %f +/- %f\n', out_params(2), out_sigs(2));
        fprintf(fido,'* fault trace shift (km): %f +/- %f\n', out_params(3), out_sigs(3));
        fprintf(fido,'* velocity offset: %f +/- %f\n', out_params(4), out_sigs(4));
%         fprintf(fido,'* angle of axis rotation (rad): %f +/- %f\n', out_params(5), out_sigs(5));
        fprintf(fido,'%s', out_tilt_str);
        fprintf(fido,'*\n');
        fprintf(fido,'*%15s %15s %15s %15s %15s\n',  'dist_to_fault',      'prediction',      'horiz_pred','pred_lower','pred_upper');
        for i=1:size(xo,1)
            fprintf(fido,' %15.6f %15.6f %15.6f %15.6f %15.6f\n',xo(i),yo(i),0,yo_lowers(i),yo_uppers(i));
        end
        fclose(fido);
        
        
        %write output file (modeled: tilt-corrected)
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
        fprintf(fido,'* fault trace shift (km): %f +/- %f\n', out_params(3), out_sigs(3));
        fprintf(fido,'* velocity offset: %f +/- %f\n', out_params(4), out_sigs(4));
%         fprintf(fido,'* angle of axis rotation (rad): %f +/- %f\n', out_params(5), out_sigs(5));
        fprintf(fido,'%s', out_tilt_str);
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