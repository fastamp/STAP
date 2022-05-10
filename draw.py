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
     plt.plot(a,srN5,'-->b',linewidth=3,label='SR-N_5-para')
     plt.plot(a,srN4,'--^c',linewidth=3,label='SR-N_4-para')
     plt.legend(loc='upper right')
     plt.xlim(0.0,4.0)
     plt.ylim(0.0,1.1)
     plt.grid()
     plt.show()

def draw_cook_beam():
     a=[2,4,8,16,60]
     quad=[0.166513E+02,0.260082E+02,0.321510E+02,0.346954E+02,0.363195E+02]
     wilson=[0.334923E+02,0.345455E+02,0.352873E+02,0.357158E+02,0.364641E+02]
     srN4=[0.366758E+02,0.351954E+02,0.354582E+02,0.357706E+02,0.364729E+02]
     plt.plot(a,quad,'--or',linewidth=3,label='Quad4')
     plt.plot(a,wilson,'--+k',linewidth=3,label='Wilson')
     plt.plot(a,srN4,'--^c',linewidth=3,label='SR-N_4-para')
     plt.legend(loc='upper right')
     plt.xlim(0,60.0)
     #plt.ylim(0.0,60.0)
     plt.grid()
     plt.show()

draw_cook_beam()
print('finished!')