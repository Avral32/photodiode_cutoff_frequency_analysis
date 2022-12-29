import os
import sys
from pathlib import Path
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt 


def stepfunc(t, n):
    """define step response function of nth order low pass filters"""
    
    tau = 1/(2*np.pi*1)
    term = 1
    last = 1
    
    if n == 1: 
        step = 1 - np.exp(-t/tau)
    
    if n > 1:
        for i in range(2, n+1):
            
            last = last*t/tau/(n-1)
            term = term + last
            
    step = 1 - np.exp(-t/tau)*term
    return  step
    
t = np.linspace(.1, 2, 100)

y1 = stepfunc(t, 1)
y2 = stepfunc(t, 2)


plt.figure(figsize=[8,6])
plt.plot(t, y1, "r--", label = "1st order")
plt.plot(t, y2, "k--", label = "2nd order")
plt.xlabel('Time [s]', fontsize = 18)
plt.title("Step response of $n^{th}$ order low pass filter", size = 18)
plt.xticks(fontsize=16), plt.yticks(fontsize=16)
plt.ylabel('Amplitude [Norm.]', fontsize = 18)
plt.text(.5, 0.5, s =  r"$ y(t) \, = \, 1 - e^-{t/\tau} \sum_{r=0}^{n-1} \, \frac{1}{r!}(\frac{t}{\tau})^r$", size = 20)
plt.ylim([0, 1.1])
plt.legend()
#plt.savefig("step response of nth order LPF.jpg")
plt.show()