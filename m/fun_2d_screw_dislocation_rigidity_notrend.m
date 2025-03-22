function v=fun_2d_screw_dislocation_rigidity_notrend(x1, param)
% 2-D dislocation model function
% interseismic:
%             S          x
%  vi=2(1-K) --- arctan(---)  when x<0
%             pi         D
%
%         S          x
%  vi=2K --- arctan(---)      when x>0
%         pi         D
%

% param
S=param(1);
D=param(2);
xshift=param(3);
vshift=param(4);
K=param(5);

ind1=find(x1<xshift);
ind2=find(x1>=xshift);

v1 = 2* (1-K)* S/pi*atan( (x1(ind1)-xshift)/D );
v2 = 2* K* S/pi*atan( (x1(ind2)-xshift)/D );

if isrow(x1) 
  v12=[v1,v2];
else
  v12=[v1;v2];
end

v=v12 + vshift;    