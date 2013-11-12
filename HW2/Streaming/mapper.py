#!/usr/bin/python
"""
Created on Fri Nov 08 15:12:11 2013

Map-Reduce Program for STA250 Homework 2 Question 2
Fall 2013

@author: Hao Ji
"""
import sys

''' Map function takes in the paired observation and returns a map element 
    with key being the lower bound combination of x and y, and value 1'''
for line in sys.stdin:
    line = line.strip()
    x,y = line.split('\t',1)
    if float(x)>0:
        x = str(int(float(x)/0.1)*0.1)
    else:
        x = str(int(float(x)/0.1-1)*0.1)
    if float(y)>0:
        y = str(int(float(y)/0.1)*0.1)
    else:
        y = str(int(float(y)/0.1-1)*0.1)
    print x+'_'+y+'\t'+'1'
''' mapresult is a list of strings containing the output of mapper function '''
