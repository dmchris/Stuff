# -*- coding: utf-8 -*-
"""
Spyder Editor

This temporary script file is located here:
C:\Users\USER\.spyder2\.temp.py
"""

import random
import math
import matplotlib
''' Q1 Fizz & Buzz '''
for x in range(1,101):
    if x%3==0 and x%5==0: 
        print "FB"
    elif x%3==0:
        print "F"
    elif x%5==0:
        print "B"
    else: 
        print x
        
''' Q2 '''
X = list()
Y = list()
for i in range(0,10000):
    X.insert(i,random.uniform(0,2*math.pi))
    Y.append(random.uniform(0,1))
U=list()
V=list()
for i in range(0,10000):
    U.append(Y[i]*math.cos(X[i]))
    V.append(Y[i]*math.sin(X[i]))
matplotlib.pyplot.scatter(U,V)
R=list()
for i in range(0,10000):
    R.append(math.sqrt(X[i]**2+Y[i]**2))
matplotlib.pyplot.scatter(range(0,10000),R)

''' Q3 '''
Str = "Hello, my name is Bob. I am a statistician. I like statistics very much."
n = len(Str)
for i in range(0,n):
    f = open('out_%02d.txt' % i,'w')
    f.write(Str[i])
f.close()
'''Now recover the string by reading all the files'''
S = ''
for i in range(0,n):
    f=open('out_%02d.txt' % i, 'r')
    S = S+f.read()
    f.close()
S

''' Q6 '''
''' a '''
rho = 0.8
def Simu(n):
    y = list()
    y.append(0)
    e = list()
    e.append(0)    
    for i in range(0,n):
        e.append(random.normalvariate(0,1))
        y.append(rho*y[i]+e[i+1])
    return y
X = Simu(1000)

''' b '''
TOT = list()
for i in range(0,200):
    TOT.append(Simu(1000))

''' c Mean for each time point '''
M = list()
for i in range(0,1000):
    sum = 0
    for j in range(0,200):
        sum = sum + TOT[j][i]
    M.append(sum/200)
matplotlib.pyplot.scatter(range(0,1000),M)

''' d '''
V = list()
for i in range(0,1000):
    ss = 0
    for j in range(0,200):
        ss = ss + TOT[j][i]**2
    V.append((ss-200*(M[i]**2))/199)
matplotlib.pyplot.scatter(range(0,1000),V)

''' e '''
Mc = list()
for j in range(0,200):
    sumc = 0
    for i in range(0,1000):
        sumc = sumc + TOT[j][i]
    Mc.append(sumc/1000)
matplotlib.pyplot.scatter(range(0,200),Mc)

''' f '''
Vc = list()
for j in range(0,200):
    ssc = 0
    for i in range(0,1000):
        ssc = ssc + TOT[j][i]**2
    Vc.append((ssc-1000*(Mc[j]**2))/999)
matplotlib.pyplot.scatter(range(0,200),Vc)