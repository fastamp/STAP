import os
from matplotlib import lines

from pyparsing import line

fabq=open("ckb_16.inp")
finp=open("ckb_16.nod",'wt')
lines=fabq.readlines()
i=-1
for line in lines:
    if(line[0]=='*'):
        i=-1
    if(line.find('*Node')==0):
        i=0
    if(i==0):
        
