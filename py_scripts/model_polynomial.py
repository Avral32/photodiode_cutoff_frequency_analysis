import os
import re
import sys
from pathlib import Path
import numpy as np
import pandas as pd
import errno
import matplotlib.pyplot as plt
from scipy.optimize import curve_fit
import seaborn as sns

from timeit import default_timer as timer

abs_dir = os.path.dirname(os.path.abspath(__file__))
base, sub = os.path.split(abs_dir)
dirname = os.path.join(base, "spectra")

print(os.getcwd())

files = []
oprts = []
ro = []
part = []
mess = []

for f in os.listdir(dirname):
    if f.endswith('.CSV') and f[-9:-8] not in ["1", "2", "3", "4"]:
        #print(f)
        files.append(f)
        file_split = f.split("_")
        oprts.append([file_split[0], file_split[1], "".join(re.findall(r"\d+",file_split[2])), file_split[3][:3]])
        # convert a list to a str ' "".join(list)'
# define a function to find the cutoff frequency

def model(w, u, b1, b2,b3, a1, a2, a3):
    """The transfer function of the low pass filter with up to second order """
    #w = 2*np.pi*w
    w = w/1000
    ze = np.exp(-1j*w)
    H = u + (b1 + b2*ze + b3*ze**2 ) / (a1 + a2*ze**2)
    HdB = 20*np.log10(abs(H))
    
    return HdB   

freq_min = 8
freq_max = 100

def cutoff(dframe, fmin = freq_min, fmax = freq_max):
    """Find the -3dB cutoff frequency!"""
    
    min_idx = abs(dframe.iloc[:, 0] - fmin).values.argmin()
    max_idx = abs(dframe.iloc[:,0] - fmax).values.argmin()
    media = data.iloc[min_idx:max_idx, 1].median()
    # find the index of the vlaue for media
    idx = abs(data.iloc[:,1] - media).values.argmin()
    # index of cutoff frequency 
    #for first oder filter 20 dB/decade-3dB roll-off, for second oder -40 dB/decade wiht -6 dB roll-off
    idx_cutoff = abs(3 - abs(data.iloc[min_idx:-1,1]-media)).values.argmin()
    
    idx_cutoff = idx_cutoff + min_idx 
    cutoff_freq = data.iloc[idx_cutoff,0]
    print("Cutoff frequency: %.3f MHz" %cutoff_freq)

    return[media, cutoff_freq]


# fit parameters
u = 20*np.log10(abs(.1))
b1 = 1
b2 = 2
b3 =1
a1 = 5
a2 = -0.5
a3 =1
pars = (u, b1, b2, b3, a1, a2, a3)


start_time = timer()
em = []  
rslts = []    
for file, oprts in zip(files, oprts):
        #print(file)
        data = pd.read_csv(os.path.join(dirname, file),  delimiter = ";", decimal = ",",
        engine='python', skiprows = 3,  skipfooter=11,  header = None, usecols = [0,1])
        data.columns =['freq_Hz',  'ampl_dBm']
        data = data.astype("float64").round(5)
        # devide the first col to 1e6 to convert MHz
        data.iloc[:, 0] = data.iloc[:, 0]/1e6      
        df_new = data.iloc[8:600,:]
        #print(df_new)
        x1 = df_new.iloc[:,0]
        y1 = df_new.iloc[:,1]
        
        
        #bound = ([-np.inf, -np.inf], [np.inf, np.inf])
        rslt, err = curve_fit(model, x1, y1, pars)
        rslt = rslt.round(1)
        #print(rslt)
        fitted = pd.DataFrame(model(x1, *rslt))
        freq =pd.DataFrame(x1) 
        fitted_data = pd.concat([freq, fitted], axis=1, ignore_index = True, keys=["red", "blue"])
        #print(file)
        cut_off = cutoff(fitted_data) 
        rslts.append(cut_off[1])
        em.append(oprts)
       
  
MessDetail = pd.DataFrame(em, columns = ["Operators", "Runoders", "Part", "Mess"])
rslts = pd.DataFrame(rslts, columns = ["Cutoff_MHz"])
#rslts.groupby("Operators", "Runoders", "Part").mean()
cutoff = pd.concat([MessDetail,rslts], axis=1, ignore_index=False).round(2)
cutoff['Runoders'] = cutoff['Runoders'].str.extract(r'(\d+)', expand=True).astype(float)

cutoff.to_csv(os.path.join(base, "output/cutoff_polynomial_fit.csv"), sep = ";", index = False, decimal = ",")

end_time = timer()
print('\nThe evaluation is done in {:.0f} s.'\
            .format(end_time - start_time))

print(cutoff)
#print(vishnu)      
