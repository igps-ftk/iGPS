function xyz2neu,x,y,z,lat,lon
  R=dblarr(3,3)
  R=[[-1d0*sin(lat)*cos(lon), -1d0*sin(lat)*sin(lon), cos(lat)], $
     [-1d0*sin(lon), cos(lon), 0],$
     [cos(lat)*cos(lon), cos(lat)*sin(lon), sin(lat) ] ]
  n=R[0,0]*x+R[1,0]*y+R[2,0]*z   
  e=R[0,1]*x+R[1,1]*y+R[2,1]*z   
  u=R[0,2]*x+R[1,2]*y+R[2,2]*z
 
 return,[n,e,u]
end

pro xyz2neu
  x=-2148744.2893d0  
  y=4426641.2357d0  
  z=4044655.8795d0
  lat=39.60860063d0*!dpi/180  
  lon=115.89248750d0*!dpi/180
  print,xyz2neu(x,y,z,lat,lon)
end