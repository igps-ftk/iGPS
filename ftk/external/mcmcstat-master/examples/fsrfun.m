function y=fsrfun(x, param)
% algae model function


% y=  slip_rate/pi*atan(x/locking_depth)+yshift;
y= param(1)/pi*atan((x-param(4))/param(2))+param(3);
