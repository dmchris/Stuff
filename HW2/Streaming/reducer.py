#!/usr/bin/python

"""
Created on Fri Nov 08 15:17:54 2013

Map-Reduce Program for STA250 Homework 2 Question 2
Fall 2013

@author: Hao Ji
"""
import sys

''' Reduce function counts the frequency for each 2-d grid combination'''
current_word = None
word = None
current_count = 0

for line in sys.stdin:
    line = line.strip()
    key, count = line.split('\t', 1)
    try:
        count = int(count)
    except ValueError:
        continue
    if current_word == key:
        current_count += count
    else:
        if current_word:
            x, y = current_word.split('_')
            # write result to STDOUT
            print x+','+str(float(x)+0.1)+','+y+','+str(float(y)+0.1)+','+str(current_count)                
        current_count = count
        current_word = key
         
if current_word == key:
    x, y = current_word.split('_')
    print x+','+str(float(x)+0.1)+','+y+','+str(float(y)+0.1)+','+str(current_count)
