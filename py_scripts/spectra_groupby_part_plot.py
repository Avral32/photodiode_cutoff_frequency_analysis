import os
import re
import sys
from pathlib import Path
import numpy as np
import pandas as pd
import seaborn as sns
import errno
import matplotlib.pyplot as plt

abs_dir = os.path.dirname(os.path.abspath(__file__))
base, sub = os.path.split(abs_dir)
#rejoin the base and a new sub again
dirname = os.path.join(base, "spectra_plot_test")

files = []
oprts = []
ro = []
part = []
mess = []

for f in os.listdir(dirname):
    if f.endswith('.CSV'):
        files.append(f)
        file_split = f.split("_")
        oprts.append([file_split[0], file_split[1], "".join(re.findall(r"\d+",file_split[2])), file_split[3][:3]])


em = []
rslt = []

arr_empty = np.empty(shape = [0, 3]) # shape = [1999, 3]
for file, oprts in zip(files, oprts): 
        #print(file)
        data = pd.read_csv(os.path.join(dirname, file),  delimiter = ";", decimal = ",",
        engine='python', skiprows = 3,  skipfooter=11,  header = None, usecols = [0,1])
        data.columns =['freq_Hz',  'ampl_dBm']
        data = data.astype("float64").round(2)
        # devide the first col to 1e6 to convert MHz
        data.iloc[:, 0] = data.iloc[:, 0]/1e6
        # add ro to the data to merge later
        ro = []
        for i in range(len(data)):
            ro.append(file.split("_")[1])
            
        ro = pd.DataFrame(ro, columns = ["ro"])
        oprt = pd.DataFrame([oprts], columns = ["Operators", "ro", "Part", "Mess"])
        data = pd.concat([data, ro], axis = 1, ignore_index = True) 
        # append the data as column wide using numpy
        arr = np.array(data)
        arr_empty = np.append(arr_empty, arr, axis = 0)
        em.append(oprts)
        
#print(arr_empty, arr_empty.shape) 
detail = pd.DataFrame(em, columns = ["operators", "ro", "part", "mess"])
rsl = pd.DataFrame(arr_empty, columns = ["freq", "ampl", "ro"])
merged = rsl.merge(detail, how = "left", on = "ro") #.astype(float)

df = merged

df['ro'] = df['ro'].str.extract(r'(\d+)', expand=True).astype(float)
# change the data types
df[['ampl', "mess", 'freq', 'part']] = df[['ampl', "mess", 'freq', 'part']].astype(float)

#vishnu = df.loc[df['part'].isin([5,6])]
# include only the vishnu parts
vishnu = df.loc[df['part'] != (1,2,3,4)]
print(vishnu)

sns.lineplot(data = df, x = "freq", y = "ampl", hue = "part").plot()
plt.xscale("log")
plt.show()
        