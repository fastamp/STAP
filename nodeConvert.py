import os
from matplotlib import lines

from pyparsing import line

fabq=open("ckb_24.inp")
finp=open("ckb_24.nod",'wt')
lines=fabq.readlines()
i=-1
for line in lines:
    if(line[0]=='*'):
        i=-1
    if(i==0):
        line.replace('\n',',')
        temp=line.split(',')
        n=int(temp[0])
        x=float(temp[1])
        y=float(temp[2])
        str="%d,%d,%d,%d,%f,%f,%f\n" %(n,0,0,1,x,y,0.0)
        finp.write(str)
    if(line.find('*Node')==0):
        i=0