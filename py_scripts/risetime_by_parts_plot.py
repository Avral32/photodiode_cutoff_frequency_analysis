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



abs_dir = os.path.dirname(os.path.abspath(__file__))
base, sub = os.path.split(abs_dir)
dirname = os.path.join(base, "output/decoupled_lowpass_2nd.csv")

data = data = pd.read_csv(os.path.join(dirname), delimiter = ";", decimal = ",",
        engine='python')

data["Cutoff_MHz"] = data["Cutoff_MHz"].abs()
print(data)
df = data.groupby(["Operators",  "Runoders",  "Part"], as_index = False)["Cutoff_MHz"].mean()
print()
df[['risetime_ps']] = 0.35*1e6 / df[["Cutoff_MHz"]] #cutoff_freq*1e6,"ps" 

df.to_csv(os.path.join(base, "output/decoupled lowpass-2nd groupby mess.csv"), sep = ";", index = False, decimal = ",")


print(df)

plt.figure(figsize=[8,5.5])
sns.scatterplot(data= df, x = "Part", y = "risetime_ps", hue = "Operators", s= 100)
plt.ylim([700, 1250])
plt.xlabel('Parts', fontsize = 17)
plt.ylabel('Risetime [ps]', fontsize = 17)
plt.xticks(fontsize=16), plt.yticks(fontsize=16)
plt.tight_layout(pad = 0.4, w_pad=0.5, h_pad=1.0)

plt.savefig("operators_1.jpg")
plt.show()
