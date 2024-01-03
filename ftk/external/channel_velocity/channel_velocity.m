function [vfinal] = channel_velocity(xfinal, slipRate, t, h, D, H, mu, eta, T)
% This function calculates the velocity at points x at time t from the 3-layer
% viscoelastic earthquake cycle model.
%
% Reference: 
%
% vfinal = ChannelVelocity(xfinal, slipRate, t, h, D, H, mu, eta, T)
%
% Arguments:
%   xfinal      : x values at which to evaluate solution, must be within 500 km of the fault
%   slipRate    : slip rate [m/yr]
%   t           : time since last earthquake [years]
%   H           : thickness of elastic upper crust [km]
%   D           : locking depth of the fault [km]
%   h           : channel thickness of viscoelastic layer [km]
%   mu          : shear modulus [Pa]
%   eta         : dynamic viscocity [Pa s]
%   T           : length of earthquake cycle [yrs]
%
% Returned variables:
%   v           : velocity [mm/yr]
% 
% Example:
% xfinal = linspace(-500, 500, 1000);
% vfinal = ChannelVelocity(xfinal, 1, 10, 20, 20, 20, 3e10, 1e18, 100);
% plot(xfinal, vfinal);

% Check bounds on x-coordinates
if (max(xfinal) > 1000 || min(xfinal) < -1000)
   error('x inputs must be between -1000 km and 1000 km of the fault trace');
end

% Dimensionalize and calculate necessary parameters
siay                  = 365.25*24*60*60; % seconds in a year
N                     = 2^12; % number of sampling intervals in space
x                     = linspace(-15000, 15000, N) ; % calculation inverval
si                    = 2*max(x)/N; % sampling interval
kVec                  = 2*pi*linspace(-5e-1/si, 5e-1/si, N); % vector of wavenumbers
T                     = T*siay; % earthquake recurrence interval (in seconds)
t                     = t*siay; % earthquake recurrence interval (in seconds)
slipRate              = slipRate/siay; % mm/yr
slip                  = slipRate*T; % earthquake slip (m)
xoffset               = (x(2)-x(1))/2;

% Channel model calculation
t_R                   = 2*eta/mu;
B                     = 1/t_R; 
vkc                   = zeros(N, 1);

% Loop over wavenumbers
for ii=1:N
   k                  = kVec(ii);
   K_1                = sinh(h*abs(k));
   K_2                = sinh(D*abs(k));
   K_3                = cosh(H*abs(k));
   K_4                = sinh(H*abs(k));
   K_5                = cosh(h*abs(k));
   K_6                = 2*(K_3*K_5+K_4*K_5+2*K_1*K_4);
   K_7                = 2*(K_3*K_5+K_1*K_3+K_4*K_5+K_1*K_4);
   K_8                = (K_1+K_5)*(K_3+K_4);
   K_9                = (-4*K_1^2*K_3*K_4+K_3^2*K_5^2+2*K_3*K_4*K_5^2+K_4^2*K_5^2)^(1/2);
   K_10               = (K_1*K_3 )-(K_1*K_4);
   K_11               = (K_9/K_8-K_6/K_7);
   K_12               = (K_9/K_8+K_6/K_7);
   spinup_fact1       = exp(-B*T*K_11)/(exp(-B*T*K_11)-1);
   spinup_fact2       = exp(B*T*K_12)/(exp(B*T*K_12)-1);
   
   % In limit that the exponentials blow up
   if isnan(spinup_fact1)
       spinup_fact1   = 1;
   end
   if isnan(spinup_fact2)
       spinup_fact2   = 1;
   end
   
   C_1                = (-2*B*K_1*K_2*k*1i)*(K_3-K_4)/(K_9*K_8*k^2)*spinup_fact1*(K_9+K_10);                
   C_2                = (-2*B*K_1*K_2*k*1i)*(K_3-K_4)/(K_9*K_8*k^2)*spinup_fact2*(K_9-K_10);                
   B_1                = -B*K_11;
   B_2                = B*K_12;
   
   % Inverse fourier transform back to space domain
   vkc(ii)            = slip*(C_1*exp(-B_1*t) + C_2*exp(-B_2*t));          
end
vc                    = real(fftshift(ifft(fftshift(double(vkc)/si))));
v                     = T/slip*vc;

% Correct for Nyquist frequency shift    
x                     = x-xoffset;

% Interpolate onto specified x-coordinates
% vfinal                = spline(x, v, xfinal);
% whos x v xfinal
pos=find(isnan(xfinal) == 1);
if isempty(pos) ~= 1 
    pos
end
vfinal                = spline(x, v, xfinal)*slipRate*siay*1000;

