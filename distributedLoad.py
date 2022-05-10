#�Զ����ɷֲ��ڵ�������
from matplotlib.pyplot import plot
import numpy as np

nump=25                          #�ʷֵĽڵ���
x=np.linspace(44,60,nump)
ngs=2
forc=np.zeros(nump)
gsp=[-0.5773502691896,0.5773502691896]
gsw=[1.0,1.0]
n=[0.0,0.0]
q=1.0                           #�ֲ���ֵ��С
nume=nump-1                     #��Ԫ��
for i in range(nume):
    x1=x[i]
    x2=x[i+1]
    for j in range(ngs):
        n[0]=0.5*(1.0-gsp[j])   #��Ԫ�ͺ���
        n[1]=0.5*(1.0+gsp[j])
        J=0.5*(x2-x1)           #�ſɱ�
        JI=1.0/J                #�ſɱȵ���
        forc[i]=forc[i]+gsw[j]*n[0]*q*J
        forc[i+1]=forc[i+1]+gsw[j]*n[1]*q*J

for i in range(nump):
    print(forc[i])