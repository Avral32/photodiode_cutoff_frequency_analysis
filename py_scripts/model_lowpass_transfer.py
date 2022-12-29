# Fit the response funtion of photodiodes using second order low pass transfer funtion under the assuption that the two low pass filters
# are decoupled each other, os we can separate the second order function into two separate first order functions.

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
import time

from timeit import default_timer as timer

#dirname = os.path.dirname(__file__)  
#filename = os.path.join(dirname, "spectra")

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

def lowpass(w, w_0, a):
    """The transfer function of the low pass filter with up to second order """
    # f_c is the cutoff frequency of the filter
    
    w = w*2*np.pi
    w_0 = 2*w_0*np.pi
    H =  a*1.0 / (1 + 1j*w / w_0) / (1 + 1j*w / w_0)
    HdB =  20*np.log10(abs(H))
    return HdB     
  
# fit parameters
w_0 = 400
a = 2
pars = (w_0, a)

em = []  
rslts = [] 
t = 0   
for file, oprts in zip(files, oprts):
        
        start_time = timer()
        data = pd.read_csv(os.path.join(dirname, file),  delimiter = ";", decimal = ",",
        engine='python', skiprows = 3,  skipfooter=11,  header = None, usecols = [0,1])
        data.columns =['freq_Hz',  'ampl_dBm']
        data = data.astype("float64").round(5)
        # devide the first col to 1e6 to convert MHz
        data.iloc[:, 0] = data.iloc[:, 0]     
        df_new = data.iloc[15:500,:]
        #print(df_new)
        x1 = df_new.iloc[:,0]
        y1 = df_new.iloc[:,1]
        
        rslt, err = curve_fit(lowpass, x1, y1, pars)
        rslt = rslt.round(1)[0]/1e6  # MHz
       
        rs = abs(rslt.round(1))
        print(rs, "MHz")
        rslts.append(rs)
        em.append(oprts)
       
        end_time = timer()
        delta_t = end_time - start_time
        t = delta_t + t
        print('\nProcessing time sums up {:.0f} s.'\
            .format(t))
        #time.sleep(.1)
MessDetail = pd.DataFrame(em, columns = ["Operators", "Runoders", "Part", "Mess"])
rslts = pd.DataFrame(rslts, columns = ["Cutoff_MHz"])
#rslts.groupby("Operators", "Runoders", "Part").mean()
cutoff = pd.concat([MessDetail,rslts], axis=1, ignore_index=False).round(2)
cutoff['Runoders'] = cutoff['Runoders'].str.extract(r'(\d+)', expand=True).astype(float)

cutoff.to_csv(os.path.join(base, "output/decoupled_lowpass_2nd_order.csv"), sep = ";", index = False, decimal = ",")

print(cutoff)

