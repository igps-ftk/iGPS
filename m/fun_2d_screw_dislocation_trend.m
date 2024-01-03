function y=fun_2d_screw_dislocation_trend(x1, param)
% 2-D dislocation model function

% params = {
%       {'sr', 0, -30, 30}                          %S, slip rate (mm/yr)
%       {'ld', 5, .1, 10}                           %D, locking depath (km)
%       {'fts', 00, fts_min, fts_max}               %fts, fault trace shift (km)
%       {'vs', (ymin+ymax)/2, ymin,ymax}            %vs, velocity shift (mm/yr)
%       {'rot', 0.000011, 0.000010, 0.000012}};     %rotation of profile(rad)
 
%dislocation theory (Savage and Burford, 1973):
%     S          x
%  v=--- arctan(---)
%     pi         D
%
%  to account for fault trace inaccuracy (fts) and velocity offset (vs):
%     S        x-fts
%  v=--- arctan(---) + vs
%     pi         D
% 
% y=  slip_rate/pi*atan(x/locking_depth)+yshift;
% 
% 
%use extra linear trend (B) to account for the tilting of profile (Segall, 2010)
%theoretical profile:                  real case:
%-----------                            -----_____          |
%           \                                      |        v
%            ----------                   ^         -----
%                                         |              -----
%
%     S        x-fts
%  v=--- arctan(---) + vs +B*(x-fts)
%     pi         D

y= param(1)/pi*atan((x1-param(3))/param(2))+param(4)+param(5)*(x1-param(3));

    