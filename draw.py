import matplotlib.pyplot as plt
import numpy as np
def draw_beam_bendinga():
     a=np.linspace(0,4,5)
     quad=[0.242424,0.127371,0.845805E-01,0.702731E-01,0.596923E-01];
     wilson=[1.0,0.831050E+00,0.853002,0.908078E+00,0.944757E+00];
     srJ=[1.0,0.215894E+00,0.139765E+00,0.149358E+00,0.176619E+00];
     srJT=[1.0,0.617358E+00,0.533728E+00,0.527120E+00,0.504373E+00];
     srN5=[1.0,0.998983E+00,0.984084E+00,0.940840E+00,0.866059E+00]
     srN4=[1.0,0.998979E+00,0.984020E+00,0.940560E+00,0.865032E+00]
     plt.plot(a,quad,'--or',linewidth=3,label='Quad4')
     plt.plot(a,wilson,'--+k',linewidth=3,label='Wilson')
     plt.plot(a,srJT,'--sg',linewidth=3,label='$SR-J^T$')
     plt.plot(a,srJ,'--xm',linewidth=3,label='SR-J')
     plt.plot(a,srN4,'-->b',linewidth=3,label='SR-N_5-para')
     plt.plot(a,srN5,'--^c',linewidth=3,label='SR-N_4-para')
     plt.legend(loc='upper right')
     plt.xlim(0.0,4.0)
     plt.ylim(0.0,1.1)
     plt.grid()
     plt.show()

def draw_cook_beam():
     a=np.linspace(4,8,2)
     quad=[0.425570E+01,0.518228E+01]
     wilson=[0.493482E+01,0.562243E+01]
     plt.plot(a,quad,'--or',linewidth=3,label='Quad4')
     plt.plot(a,wilson,'--+k',linewidth=3,label='Wilson')
     plt.legend(loc='upper right')
     plt.xlim(4.0,8.0)
     #plt.ylim(0.0,60.0)
     plt.grid()
     plt.show()

draw_cook_beam()
print('finished!')