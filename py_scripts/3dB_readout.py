import os
import re
import sys
from pathlib import Path
import numpy as np
import pandas as pd
import errno
import matplotlib.pyplot as plt

#relative_dir = os.path.dirname(__file__)  # relative path return an empty string
#filename = os.path.join(dirname, "spectra")

abs_dir = os.path.dirname(os.path.abspath(__file__))
base, sub = os.path.split(abs_dir)
#rejoin the base and a new sub again
dirname = os.path.join(base, "spectra")
#print(os.getcwd())

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
    idx_cutoff = abs(3 - abs(data.iloc[min_idx:-1,1]-media)).values.argmin()
    
    idx_cutoff = idx_cutoff + min_idx 
    cutoff_freq = data.iloc[idx_cutoff,0]
    #print("Cutoff frequency: %.3f MHz" %cutoff_freq)

    return cutoff_freq


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
        
        freq = cutoff(data, fmin = freq_min, fmax = freq_max).round(1)
        rslts.append(freq)
        em.append(oprts)
              
  
MessDetail = pd.DataFrame(em, columns = ["Operators", "Runoders", "Part", "Mess"])
rslts = pd.DataFrame(rslts, columns = ["Cutoff_MHz"])
#rslts.groupby("Operators", "Runoders", "Part").mean()
cutoff = pd.concat([MessDetail,rslts], axis=1, ignore_index=False).round(2)
cutoff['Runoders'] = cutoff['Runoders'].str.extract(r'(\d+)', expand=True).astype(float)
cutoff["risetime_ps"] = (0.35/cutoff["Cutoff_MHz"]*1e6).round(1)
# take the average over the mess
cutoff = cutoff.groupby(["Operators",  "Runoders",  "Part"], as_index = False)[["Cutoff_MHz","risetime_ps"]].mean()

cutoff.to_csv(os.path.join(base, "output/3dB_readout.csv"), sep = ";", index = False, decimal = ",")
print("\n******** Output ***********")
print(cutoff)      


        