#!/usr/bin/python

import json
import requests
import time

auth="?auth=0232BqcaQdAREtz6KHAt3AUaxcZQVpSKqgyW3A9cvpsH1H"
urlBase="http://icfpc2013.cloudapp.net/"
maxInt=2**64
zero='0'*16
one='0'*15 + '1'

def getproblems():
    r = requests.get(urlBase + "myproblems"+auth)
    return r.json()

def status():
    r = requests.get(urlBase + "status"+auth)
    return r.json()

def train(size=None,ops=None):
    requestDict = {}
    if size is not None:
        requestDict['size']=size
    if ops is not None:
        requestDict['ops']=ops
    r=requests.post(urlBase + "train" + auth,data=json.dumps(requestDict))
    return r.json()

def solveSize3(progHash,ops):
    op = uOpDict[ops[0]]
    guess = "9e9e9e9e9e9e9e9e"
    prog = '(lambda (x) (%s %s))'
    evalDict={
            'id':progHash,
            'arguments':[guess]
            }
    r=requests.post(urlBase+"eval"+auth,data=json.dumps(evalDict))
    j=r.json()
    if j['status']!='ok':
        print evalDict
        print j
        raise Exception
    ans=hex2int(j['outputs'][0])
    if ans==op(0):
        val=0
    if ans==op(1):
        val=1
    if ans==op(hex2int(guess)):
        val='x'
    prog = prog % (ops[0],val)
    guessDict={
            'id':progHash,
            'program':prog,
            }
    r=requests.post(urlBase+"guess"+auth,data=json.dumps(guessDict))
    j=r.json()
    if j['status']=='win':
        print "won! %s" % prog
        return
    else:
        print "***FAIL***"
        print evalDict
        print guessDict
        print j
        raise Exception

        

def int2hex(x):
    return hex(x)[2:].rjust(16,'0')

def hex2int(x):
    return int(x,16)
    
def shr1(x):
    return x>>1

def shr4(x):
    return x>>4

def shr16(x):
    return x>>16

def shl1(x):
    return (x<<1) % maxInt

def not64(x):
    return maxInt-1-x

uOpDict = {
        "shr1" : shr1,
        "shr4" : shr4,
        "shr16" : shr16,
        "shl1" : shl1,
        "not" : not64,
        }

