#!/usr/bin/python

import sys
import numpy as np
import matplotlib.pyplot as plt
#from scipy.optimize import leastsq
from scipy import optimize
#from matplotlib.dates import DateFormatter
import matplotlib.ticker as ticker

def f_1(x,A,B):
  return A*x+B

print(len(sys.argv))

fname="0001.neu"
if len(sys.argv) >= 2:
  fname=sys.argv[1]

print("input file:",fname)

f = open(fname, "r")
#lines = text_file.readlines()
#lines = text_file.read().split(' ')
#print (lines)
#print (len(lines))
#text_file.close()
line=f.readline()
data_list=[]
while line:
  num=list(map(float,line.split()))
  data_list.append(num)
  line=f.readline()
f.close
data_array=np.array(data_list)

#print(data_array)
print( len(data_array) )

plt.figure(figsize=(10,6))
x0=data_array[:,0]
y0=data_array[:,5]

#print(x0)
#print ( len(x0))
#print (y0)
#print ( len(y0))

plt.scatter(x0,y0,color="red",label="InSAR Positions",linewidth=2)

A1,B1=optimize.curve_fit(f_1,x0,y0)[0]
y1=A1*x0+B1
plt.plot(x0,y1,color="blue",label="Fitted Line",linewidth=2)

#plt.plot(x0,y1)
#plt.plot(x0,y0,'o')

plt.title(fname)
plt.xlabel("T(yr)")
plt.ylabel("InSAR LOS Displacement (mm)")
plt.legend(loc='upper right')

print(A1)
#myFmt=DateFormatter("%Y-%b")
plt.gca().xaxis.set_major_formatter(ticker.FormatStrFormatter('%4.2f'))

ratestr="average rate: %7.2f mm/yr" % A1

x2=np.mean(plt.xlim())
ylim=plt.ylim()
y2=ylim[1]-(ylim[1]-ylim[0])/5
print('x2:',x2,' y2:',y2)
plt.text(x2,y2,ratestr,horizontalalignment="center")

ofile="0001.png"
ofile=fname.replace(".neu",".png")
print("output to:",ofile)
plt.savefig(ofile)

print(B1)
print(np.mean(plt.xlim()))
print(np.mean(plt.ylim()))

#plt.show()
