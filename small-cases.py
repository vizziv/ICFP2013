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

def unsolved(size=None):
    return [i for i in getproblems() if ('solved' not in i or not i['solved']) and ('timeLeft' not in i or i['timeLeft']>0) and (size is None or i['size']==size)]

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

def solveSize4OneUnary(progHash,ops):
    op = uOpDict[ops[0]]
    guess = "9e9e9e9e9e9e9e9e"
    prog = '(lambda (x) (%s (%s %s)))'
    ans=evalHash(progHash,[guess])[0]
    val={op(op(0)):0, op(op(1)):1, op(op(hex2int(guess))):'x'}[ans]
    prog = prog % (ops[0],ops[0],val)
    guessProg(progHash,prog)

def solveSize4TwoUnary(progHash,ops):
    op0=uOpDict[ops[0]]
    op1=uOpDict[ops[1]]
    guesses = ["9e9e9e9e9e9e9e9e","abcabcabcabcabca","1234567890123456"]
    prog = '(lambda (x) (%s (%s %s)))'
    anss=tuple(evalHash(progHash,guesses))
    val = { (f1(f2(v[0])),f1(f2(v[1])),f1(f2(v[2]))):(f1s,f2s,v[3]) for (f1,f1s,f2,f2s) in [(op0,ops[0],op1,ops[1]),(op1,ops[1],op0,ops[0])] for v in [(0,0,0,0),(1,1,1,1),(hex2int(guesses[0]),hex2int(guesses[1]),hex2int(guesses[2]),'x')] }[anss]
    prog = prog % val
    guessProg(progHash,prog)
    
def solveSize4Binary(progHash,ops):
    op = binOpDict[ops[0]]
    guesses = map(int2hex,xrange(16))
    prog = '(lambda (x) (%s %s %s))'
    anss=tuple(evalHash(progHash,guesses))
    possibilities = [0,1,'x']
    val = {
            tuple(op(hex2int(g) if v=='x' else v,hex2int(g) if w=='x' else w) for g in guesses)
            :
            (v,w)
            for v in possibilities for w in possibilities
            }[anss]
    prog = prog % (ops[0],val[0],val[1])
    guessProg(progHash,prog)

def fTrees(fs,size):
    if fs:
        if size==1:
            return [('x',[],lambda x: x[0])]
        elif size<=0:
            return []
        else:
            fTs=[]
            for f in fs:
                if f in uOpDict:
                    function=uOpDict[f]
                    for fT in fTrees(fs,size-1):
                        fTs.append(('(%s %s)' % (f,fT[0]),fT[1]+[f],compose(function,fT[2])))
                elif f in binOpDict:
                    if size>=2:
                        function = binOpDict[f]
                        for i in range(1,(size+1)//2):
                            for fT1 in fTrees(fs,i):
                                argsNeeded = 1+len([j for j in fT1[1] if j in binOpDict])
                                for fT2 in fTrees(fs,size-1-i):
                                    fTs.append(('(%s %s %s)' % (f,fT1[0],fT2[0]),fT1[1]+fT2[1]+[f],compose2List(function,fT1[2],fT2[2],argsNeeded)))
                else:
                    raise NotImplementedError
        return fTs

def fullFTrees(fs,size):
    return [(1+len([j for j in fT[1] if j in binOpDict]),fT[0],fT[2]) for fT in fTrees(fs,size) if len(fs)==len(set(fT[1]))]


def guessProg(progHash,prog):
    print "guessing %s for %s..." % (prog,progHash)
    guessDict={
            'id':progHash,
            'program':prog,
            }
    r=requests.post(urlBase+"guess"+auth,data=json.dumps(guessDict))
    j=r.json()
    if j['status']=='win':
        print "won!"
        return
    else:
        print "***FAIL***"
        print guessDict
        print j
        raise Exception

def evalHash(progHash,guesses):
    evalDict={
            'id':progHash,
            'arguments':guesses
            }
    r=requests.post(urlBase+"eval"+auth,data=json.dumps(evalDict))
    j=r.json()
    if j['status']!='ok':
        print evalDict
        print j
        raise Exception
    return map(hex2int,j['outputs'])

def compose(*fs):
    if len(fs)==0:
        return lambda x: x
    elif len(fs)==1:
        return fs[0]
    elif len(fs)==2:
        return lambda x: fs[0](compose(*fs[1:])(x))

def compose2List(f1,f2a,f2b,cutoff):
    return lambda l: f1(f2a(l[:cutoff]),f2b(l[cutoff:]))
        
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

def bitand(x,y):
    return x & y

def bitor(x,y):
    return x | y

def bitxor(x,y):
    return x ^ y

def plus(x,y):
    return (x + y) % maxInt

uOpDict = {
        "shr1" : shr1,
        "shr4" : shr4,
        "shr16" : shr16,
        "shl1" : shl1,
        "not" : not64,
        }

binOpDict = {
        "and" : bitand,
        "or" : bitor,
        "xor" : bitxor,
        "plus" : plus,
        }

