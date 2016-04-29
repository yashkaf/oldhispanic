# -*- coding: utf-8 -*-
"""
Created on Thu Apr 28 15:49:45 2016

@author: yfalkovich
"""
import pandas as pn
ng=2
na=3
nr=4
ns=2

def popgen(ngenders,nages,nraces,nsubcat):
    gen=[]
    age=[]
    rac=[]
    for i in range(ngenders):
        gen=gen+[i+1]*nages*nraces*nsubcat
    for i in range(ngenders):
        for j in range(nages):
            age=age+[j+1]*nraces*nsubcat
    for i in range(ngenders):
        for j in range(nages):
            for k in range(nraces):
                rac=rac+[k+1]*nsubcat
    d=pn.DataFrame.from_dict({'Gender':gen,'Age':age,'Race':rac})
    return d
    
population=popgen(ng,na,nr,ns)
print population