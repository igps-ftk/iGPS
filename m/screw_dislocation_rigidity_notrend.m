clear
clc

PROG='screw_dislocation_rigidity_notrend';

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


%MCMC constraints
nsimu=5000; %number of simulations
% npar=5 ;



paths={'D:\gsar\gic3dv\atf.d019\asc_des\profiles\p.fa_atf_ext'};
% paths={'Z:\g11j\D\gsar\gic3dv\atf.d019\asc_des\profiles\p.fa_atf_ext'};
% paths={'/g11j/D/gsar/gic3dv/atf.d019/asc_des/profiles/p.fa_atf_ext/'};
ptn='060'
% ptn='102*'
dmin=-200;
% dmin=-30;
dmax=200;
cmt='ridigity2'
cmt='ridigity_fixTrace3km_200km'
nsimu=30000; 


% dmin=-250;
% dmax=260;
% fts_min=-35;
% fts_max=135;
% fts_min=-35;
% fts_max=35;
% fts_min=-15;
% fts_max=15;
% fts_min=-1;
% fts_max=5;
fts_min=-3;
fts_max=3;
is_show_fig='on';


npath=size(paths,1);


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
    file=[path,filesep,files(fi).name];
%     file='D:\gsar\interseismic\070-a-m6-0100_0105_0110_0115_0120_0125-eastkunlun5M3\f123\sbas.4.0.0001.9999.20141022.20230225.056.0770.01.___\p.fa_Kunlun_Fault_dec3d_byN_all_2km\klfPostseismic\test\profile_019_vel_mdl.txt';
    
        
    [pathstr, name, ext] = fileparts(file);
    
    str=sprintf('[%s]INFO: %d/%d processing %s ...',PROG,fi,nf,name);
    disp(str);
    
    
    ofile=[opath,filesep,name,'_mdl.txt'];
%     ofile_rot=[opath,filesep,name,'_rot.txt'] ;
    jfile=[opath,filesep,name,'_mdl.jpg'];
    jfile_sampling=[opath,filesep,name,'_mdl_sampling.jpg'];
    jfile_denspanel=[opath,filesep,name,'_mdl_denspanel.jpg'];
    disp(['output to:',ofile]);
    % return
    
    fid = fopen(file);
    tline = fgetl(fid);
    nl=0;
    clear x* y* chain* dlines* ind* tmp*;
    while ischar(tline)
      if isempty(tline) ~= 1 && strcmp(tline(1),' ') ~= 0        
        %disp(tline);
        nl=nl+1;
dlines(:,nl)=sscanf(tline,'%*s %f %f %f %f %f       %f %f %f %f %f       %f %f %f %f %f       %f %f %f %f %f       %f %f %f %f %f ');
% dlines(:,nl)=sscanf(tline,'%f %f %f %f %f %f       %f %f %f %f %f       %f %f %f %f %f       %f %f %f %f %f       %f %f %f %f %f ');
        
        
        %break
      end
      tline = fgetl(fid);
      %pause
    end
    
    fclose(fid);
    
    x1=dlines(13-1,:);
    y1=dlines(7-1,:);
%     x1=dlines(1,:);
%     y1=dlines(6,:);
    min(y1);
    max(y1);
    
    ind1=find(x1>=dmin_fts & x1<=dmax_fts);
    if isempty(ind1)
      disp('[]WARNING: no data nearby fault.');
      continue
    end
    x2=x1(ind1);
    y2=y1(ind1);
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
    xlim([-400 400]);ylim([-26 26]);  xlabel('x [km]'); ylabel('y [mm/yr]');
    
    
    % fun_2d_screw_dislocation_rigidity_notrend = @(x,param) param(1)/pi*atan((x-param(4))/param(2))+param(3);
    ssfun    = @(param,data) sum((data.ydata-fun_2d_screw_dislocation_rigidity_notrend(data.xdata,param)).^2);
    
     options1 = optimset('MaxFunEvals',6000);
%     [tmin,ssmin]=fminsearch(ssfun,[3;4;(ymin+ymax)/2;0;.01;0;0;5],[],data);
    [tmin,ssmin]=fminsearch(ssfun,[3;4;(ymin+ymax)/2;0;.01;1;20;60],options1,data);
    n = length(data.xdata);
    p = 5;
    p = 8;
    mse = ssmin/(n-p); % estimate for the error variance
    
    J = [data.xdata./(tmin(2)+data.xdata), ...
      -tmin(1).*data.xdata./(tmin(2)+data.xdata).^2];
%     tcov = inv(J'*J)*mse;
    %tcov = (J'*J) \ mse
%     tcov = pinv(J'*J)*mse;
    
    params = {
      {'sr', -10, -30, 30}
%       {'sr', -16, -17,-15}
      {'ld', 10, .1, 90}
%       {'ld', 10, .1, 50}
%       {'ld', 34, 33, 35}
%       {'ld', 10, .1, 20}
%       {'ld', 9, 8.9, 9.1}
%           {'fts', -13, -14, -12}    
          
      %     {'fts', 0, -30, 30}
      {'fts', 0, fts_min, fts_max}
%       {'yshift', 5.8, 5.7,5.9} 
      {'yshift', (ymin+ymax)/2, ymin,ymax}
%       {'yshift', (ymin+ymax)/2, ymin,ymax} 
      
%       {'rot', 0.001, -pi/3, pi/3}
%       {'rot', 0.000011, 0.000010, 0.000012}

       % rigidity 
      {'K', .5, 0, 1}
      };
    
    model.ssfun  = ssfun;
    model.sigma2 = mse; % (initial) error variance from residuals of the lsq fit
    
    model.N = length(data.ydata);  % total number of observations
    model.S20 = model.sigma2;      % prior mean for sigma2
    model.N0  = 4;                 % prior a
    
    options.nsimu = nsimu;
    options.updatesigma = 0;
%     options.qcov = tcov; % covariance from the initial fit
    options.verbosity = 1;
    options.waitbar = 1;
    %   options.MaxFunEvals = 10000;
    
        options.nsimu   = 3000;
%         options.nsimu   = nsimu;
        [res,chain,s2chain] = mcmcrun(model,data,params,options);
        options.nsimu   = nsimu;
        [res,chain,s2chain] = mcmcrun(model,data,params,options, res);
        
    
      h2=figure(2); clf
      mcmcplot(chain,[],res,'chainpanel');
    
      h3=figure(3); clf
      mcmcplot(chain,[],res,'pairs');
    
      h4=figure(4); clf
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
    out_stat=chainstats(chain,res);
    out_stds=out_stat(:,2);
    alpha=0.05; % 95% confidence level
%     alpha=0.1; % 90% confidence level
    alpha=0.01; % 99% confidence level
    for pi=1:size(out_params,2)
      lower_bounds(pi)=norminv(alpha/2,out_params(pi), out_stds(pi));
      upper_bounds(pi)=norminv(1-alpha/2,out_params(pi), out_stds(pi));
    end
    %         lower_bounds
    %         out_params
    %         upper_bounds
    out_sigs=out_params-lower_bounds;
    out_sig2=(upper_bounds-lower_bounds)/2;
    out_sig3=-out_params+upper_bounds;
    
    
%     %   xo = linspace(-300,300)';
    xo=x1';
    yo=fun_2d_screw_dislocation_rigidity_notrend(xo,mean(chain));
    yres=y1'-yo;
%     
%        xo=x1;
%     yo=fun_2d_screw_dislocation_rigidity_notrend(xo,mean(chain));
%     yres=y1-yo;
    
%     xo=xo';
%     yo=yo';
%     yres=yres';
    
    %figure(1)
    set(0,'CurrentFigure',h1);
    subplot(2,1,1);
    hold on
    plot(xo,fun_2d_screw_dislocation_rigidity_notrend(xo,mean(chain)),'-k')
    disp(['model result: ',num2str(mean(chain))]);
    hold off
    legend({'raw','used','model'},'Location','BestOutside');
    
    h5=figure(5);
    set(h5,'Visible',is_show_fig);
    clf
%     out = mcmcpred(res,chain,[],xo,'fun_2d_screw_dislocation_rigidity_notrend');
      out = mcmcpred(res,chain,s2chain,xo,'fun_2d_screw_dislocation_rigidity_notrend');
    mcmcpredplot(out);
    hold on
    plot(data.xdata,data.ydata,'.'); % add data points to the plot
    xlabel('x [km]'); ylabel('y [mm/yr]');
    hold off
    title('Predictive envelopes of the model')
    
    %figure(1);
    set(0,'CurrentFigure',h1);
    subplot(2,1,2);
%     xor=xo*cos(-1*0)-yo*sin(-1*0);
%     yor=xo*sin(-1*0)+yo*cos(-1*0);
%     x1r=x1*cos(-1*0)-y1*sin(-1*0);
%     y1r=x1*sin(-1*0)+y1*cos(-1*0);
%     plot(x1r,y1r,'.');
    plot(x1,y1,'.');
    hold on;
%     plot(xor,yor,'-k');
    plot(xo,yo,'-k');
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
%     fillyy(xo,yo_lowers,yo_uppers,dimc);
    hold on;
    plot(xo,yo_lowers,'r');
    plot(xo,yo_uppers,'r');
    plot(xo,plimi{1}(nn,:),'-k');
    hold off;
    
    
%     yor_lowers=xo*sin(-1*0)+yo_lowers*cos(-1*0);
%     yor_uppers=xo*sin(-1*0)+yo_uppers*cos(-1*0);
%     yor_means=xo*sin(-1*0)+yo_means*cos(-1*0);
    h7=figure(7);
    set(h7,'Visible',is_show_fig);
    clf
    dimc = [0.8,0.8,0.8];
    fillyy(xo,yo_lowers,yo_uppers,dimc);
    hold on;
    plot(xo,yo_lowers,'r');
    plot(xo,yo_uppers,'r');
    plot(xo,yo_means,'-k');
    hold off;
%     
    
    set(0,'CurrentFigure',h1);
    subplot(2,1,2);
    hold on;
    fillyy(xo,yo_lowers,yo_uppers,dimc);
    plot(xo,yo_lowers,'r');
    plot(xo,yo_uppers,'r');
    
    
    subplot(2,1,1);
    hold on;
    tmpstr1=sprintf('slip rate: %7.2f +/- %7.2f\n', out_params(1), out_sigs(1));
    tmpstr2=sprintf('locking depth: %7.2f +/- %7.2f\n', out_params(2), out_sigs(2));
    tmpstr3=sprintf('fault trace shift (km): %7.2f +/- %7.2f\n', out_params(3), out_sigs(3));
    tmpstr4=sprintf('velocity offset: %7.2f +/- %7.2f\n', out_params(4), out_sigs(4));
    tmpstr5=sprintf('rigidity (K): %7.2f +/- %7.2f\n', out_params(5), out_sigs(5));
    
    sf1=1;
    sf1=5;
    text(-380,4*sf1,tmpstr1);
    text(-380,3*sf1,tmpstr2);
    text(-380,2*sf1,tmpstr3);
    text(-380,1*sf1,tmpstr4);
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
    fprintf(fido,'* fault trace shift (km): %f +/- %f\n', out_params(3), out_sigs(3));
    fprintf(fido,'* velocity offset: %f +/- %f\n', out_params(4), out_sigs(4));
    fprintf(fido,'* rigidity(K): %f +/- %f\n', out_params(5), out_sigs(5));
    fprintf(fido,'*\n');
    fprintf(fido,'*%15s %15s %15s %15s %15s\n',  'dist_to_fault',      'prediction',      'horiz_pred','pred_lower','pred_upper');
    for i=1:size(xo,1)
      fprintf(fido,' %15.6f %15.6f %15.6f %15.6f %15.6f %15.6f\n',xo(i),yo(i),0,yo_lowers(i),yo_uppers(i), yres(i));
    end
    fclose(fido);
    
    
%     fido=fopen(ofile_rot,'w');
%     fprintf(fido,'* SRC: %s\n', file);
%     fprintf(fido,'*PROG: %s\n', PROG);
%     fprintf(fido,'* ver: %s\n', ver);
%     fprintf(fido,'*user: %s\n', user);
%     fprintf(fido,'*run@: %s\n', run_at);
%     fprintf(fido,'*  on: %s\n', run_on);
%     fprintf(fido,'*model paramters:\n');
%     fprintf(fido,'* far-field strike-slip rates: %f +/- %f\n', out_params(1), out_sigs(1));
%     fprintf(fido,'* locking depth: %f +/- %f\n', out_params(2), out_sigs(2));
%     fprintf(fido,'* fault trace shift (km): %f +/- %f\n', out_params(3), out_sigs(3));
%     fprintf(fido,'* velocity offset: %f +/- %f\n', out_params(4), out_sigs(4));
%     fprintf(fido,'* rigidity(K): %f +/- %f\n', out_params(5), out_sigs(5));
%     fprintf(fido,'*\n');
%     fprintf(fido,'*%15s %15s %15s %15s %15s\n',  'dist_to_fault',      'vLos_rotated',      'vLos_modeled','pred_lower','pred_upper');
%     for i=1:size(xor,1)
%       fprintf(fido,' %15.6f %15.6f %15.6f %15.6f %15.6f %15.f\n',xor(i),y1r(i),yor(i),yor_lowers(i),yor_uppers(i),yres(i));
%     end
%     fclose(fido);
    
    
    %figure(1);
    %   set(0,'CurrentFigure',h1);
    %   frame=getframe(gcf);
    %   im=frame2im(frame);
    %   imwrite(im,jfile,'jpg');
    saveas(h1,jfile);
        saveas(h3,jfile_sampling);
        saveas(h7,jfile_denspanel);
    
    % return
    
  end
end

disp(['[',PROG,']INFO: Normal end.']);