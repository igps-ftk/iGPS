function ss = fsr(theta,data)
% algae sum-of-squares function


ydata  = data.ydata;
xdata  = data.xdata;


ymodel = fsrfun(xdata,theta(1),theta(2),theta(3));
ss = sum((ymodel - ydata).^2);
