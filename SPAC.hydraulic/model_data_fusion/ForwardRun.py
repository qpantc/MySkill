import numpy as np
import pandas as pd
import warnings; warnings.simplefilter("ignore")
from model_data_fusion.src.myFun import SoilRoot, InitialState, f_PM_hydraulics

# Site-specific input data and parameters
inpath = '/Users/quan/projects/MySkill/model_data_fusion/Input/'
sitename = 'US-Me2'
soil_texture = 2; root_type = 4
root_depth = 2; canopy_height = 33; tower_height = 47 # in meters
nobsinaday = 48 # number of observations per day
warmup = 60 # warmup period, days

# df = pd.read_csv(sitename+'.csv')
df = pd.read_csv(inpath+sitename+'.csv')
SRparas = SoilRoot(soil_texture,root_type,root_depth,canopy_height,tower_height,24*3600/nobsinaday,1,0)
Init = InitialState(-0.05,-0.1,-0.2,-0.1,df['SOILM'][0],df['SOILM'][0])


# Parameters estimated using MCMC, one sample
gpmax = 3900 # maximum xylem conductance, 10^-9 m/s/MPa
p50 = -5.86 # psi_50, MPa
aa = 4.53 # shape parameter of vulnerability curve, unitless
lww = 6255 # marginal water use efficiency under a well-watered condition # umol/mol
b0 = -1.28 # sensitivity of MWUE to leaf water potential, /MPa
soil_b = 0.62 # calibration coefficient for soil hydraulic parameter, unitless
boundary_cond = 0.253 # boundary condition of the second soil layer, in volumnetric water content
theta = [gpmax,p50,aa,lww,b0,0,soil_b,boundary_cond] # in the same order of outputs from AMIS.py and AMIS_PT.py
#%%


ET,PSIL = f_PM_hydraulics(df,SRparas,theta)

#%%
import matplotlib.pyplot as plt
from model_data_fusion.src.myFun import dailyAvg

plt.figure()
plt.plot(dailyAvg(df['ET'][warmup*nobsinaday:],nobsinaday*15),'-r',label='observed')
plt.plot(dailyAvg(ET[warmup*nobsinaday:]*1e3,nobsinaday*15),'-k',label='modeled')
plt.xlabel('Time')
plt.ylabel('ET (mmol/m2/s)')
plt.legend()

plt.figure()
plt.plot(dailyAvg(PSIL[warmup*nobsinaday:],nobsinaday*15),'-k')
plt.xlabel('Time')
plt.ylabel('Leaf water potential (MPa)')

plt.figure()
plt.plot(PSIL[warmup*nobsinaday:])
