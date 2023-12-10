%%
% <html><a href="../index.html">MCMC toolbox</a> » <a href="../examples.html">Examples</a> » Algae</html>

%% Algae example
%
% The example uses functions 
% <algaesys.html |algaesys|>, 
% <algaefun.html |algaefun|> and 
% <algaess.html |algaess|>.
%
% We study the following system
%
% <<algaeweb.png>>
%

%%
% This is a simplified lake algae dynamics model. We consider
% phytoplankton _A_, zooplankton _Z_ and nutrition _P_
% (eg. phosphorus) available for _A_ in the water. The system is
% affected by the water outflow/inflow _Q_, incoming phosphorus load
% _Pin_ and temperature _T_. It is described as a simple
% predator - pray dynamics between _A_ and _Z_. The growth of _A_ is
% limited by the availability of _P_ and it depends on the water
% temperature _T_. The inflow/outflow _Q_ affects both _A_ and _P_,
% but not _Z_. We use the following equations:

%%
% <html>
% dA/dt = (&mu; - &rho;<sub>a</sub> - Q/V - &alpha;Z) A<br>
% dZ/dt = &alpha;ZA-&rho;<sub>z</sub> Z<br>
% dP/dt = -Q/V (P-P<sub>in</sub>) +
%              (&rho;<sub>a</sub>-&mu;)A+&rho;<sub>z</sub>Z
% </html>
% 
% where the growth rate µ depends on both temperature
% _T_ and phosphorus _P_
%
% <html>
% &mu; = &mu;<sup>max</sup>&theta;<sup>(T-20)</sup>P/(k+P).
% </html>


%%
% The data set is stored in |algaedata.mat|. First we load and plot
% the data.
clear model data params options
% load algaedata.mat
file='D:\gsar\interseismic\077-d-m3-0468_0473_0478-eastkunlun6M3\f123\sbas.4.0.0367.9999.20141103.20210506.149.1127.01.___\p.fa_eklf\profile_103_vel.psxy'
file='D:\Papers\gyaringco\figure\profiles.gps\pg\profile_003_vel2';
file='D:\gsar\interseismic\150-d-m3-0487_0492_0497-dangxiong2_yzs3_gulu1\f123\sbas.4.0.0700.9999.20141027.20200820.128.1753.01.x2\p.fa_bengco\profile_010_vel.psxy';
file='D:\gsar\interseismic\092-d-m3-0465_0470_0475-altyntagh_M3\f123.1\sbas.3.0.0350.9999.20141012.20180729.051.0339.01.roiref\p.fa_atf\profile_025_vel.psxy';

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
min(y1)
max(y1)

ind=find(x1>-80); 
x2=x1(ind);
y2=y1(ind);
ir=random('Normal',0,1,1,10);
ind2=fix((ir/max(abs(ir))+1)*(size(x2,2)-2)/2)+1;
x=x2(ind2);
y=y2(ind2);
% x=x2;
% y=y2;
x=x+10;
%y=y+2.6;
plot(x,y,'s');
ymin=min(y)
ymax=max(y)
data.xdata=x;
data.ydata=y;

figure(1); clf
for i =1:3
  subplot(2,3,i)
  plot(data.xdata(:,1),data.xdata(:,i+1),'-k');
%   title(data.xlabels(i+1)); xlim([1,120])
end
subplot(2,1,2)
plot(data.ydata(:,1),data.ydata(:,2:end),'o-');
title('model state variable observations');
% legend(data.ylabels(2:end),'Location','best');
xlabel('days');


%%
% The model sum of squares in file <algaess.html |algaess.m|> is
% given in the model structure.
model.ssfun = @fsr;

%%
% All parameters are constrained to be positive. The initial
% concentrations are also unknown and are treated as extra parameters.
params = {
    {'sr', 0.5,  -10, 10}
    {'ld',  10, 0.1, 50}
    {'yshift',  (ymin+ymax)/2, ymin,ymax}
    };

%%
% We assume having at least some prior information on the
% repeatability of the observation and assign rather non informational
% prior for the residual variances of the observed states. The default
% prior distribution is sigma2 ~ invchisq(S20,N0), the inverse chi
% squared distribution (see for example Gelman et al.). The 3
% components (_A_, _Z_, _P_) all have separate variances.
model.S20 = [1 1 2];
model.N0  = [4 4 4];

%%
% First generate an initial chain.
options.nsimu = 1000;
[results, chain, s2chain]= mcmcrun(model,data,params,options);
%%
% Then re-run starting from the results of the previous run,
% this will take couple of minutes.
% options.nsimu = 5000;
% [results, chain, s2chain] = mcmcrun(model,data,params,options, results);

%%
% Chain plots should reveal that the chain has converged and we can
% use the results for estimation and predictive inference.
figure(2); clf
mcmcplot(chain,[],results,'pairs');
figure(3); clf
mcmcplot(chain,[],results,'denspanel',2);

%%
% Function |chainstats| calculates mean ans std from the chain and
% estimates the Monte Carlo error of the estimates. Number |tau| is
% the integrated autocorrelation time and |geweke| is a simple test
% for a null hypothesis that the chain has converged.
chainstats(chain,results)

%%
% In order to use the |mcmcpred| function we need
% function |modelfun| with input arguments given as
% |modelfun(xdata,theta)|. We construct this as an anonymous function.

modelfun = @(d,th) algaefun(d(:,1),th,th(7:9),d);

%%
% We sample 500 parameter realizations from |chain| and |s2chain|
% and calculate the predictive plots.
nsample = 500;
out = mcmcpred(results,chain,s2chain,data.xdata,modelfun,nsample);
figure(4); clf
mcmcpredplot(out);
% add the 'y' observations to the plot
hold on
for i=1:3
  subplot(3,1,i)
  hold on
  plot(data.ydata(:,1),data.ydata(:,i+1),'s'); 
  ylabel(''); title(data.ylabels(i+1));
  hold off
end
xlabel('days');
