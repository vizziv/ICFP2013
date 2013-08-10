#!/usr/bin/python

import itertools
import json
import regex as re
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

def unsolved(size=None,pred=None):
    '''Returns problems of size `size` (None returns all) and matching predicate pred (e.g. nofolds)'''
    return [i for i in getproblems() if ('solved' not in i or not i['solved']) and ('timeLeft' not in i or i['timeLeft']>0) and (size is None or i['size']==size) and (pred is None or pred(i))]

def nofolds(p):
    return all(o not in p['operators'] for o in ['fold','tfold'])

def easy(p):
    return all(o not in p['operators'] for o in ['fold','tfold','if0'])

def status():
    r = requests.get(urlBase + "status"+auth)
    return r.json()

def train(size=None,ops=None):
    '''Grab a training problem with size `size` and operations `ops`.'''
    requestDict = {}
    if size is not None:
        requestDict['size']=size
    if ops is not None:
        requestDict['ops']=ops
    r=requests.post(urlBase + "train" + auth,data=json.dumps(requestDict))
    return r.json()

def solveSmallEasy(progDesc):
    print "solving program %s" % progDesc['id']
    possibleProgs = progs(progDesc['operators'],progDesc['size'])
    for i in xrange(10):
        guesses=map(int2hex,xrange((3**35)*i-(2**i+1)*128,(3**35)*i+(2**i+1)*128,2**i+1))
        print "making guesses [%s,...,%s] ... " % (guesses[0],guesses[-1])
        anss = evalHash(progDesc['id'],guesses)
        possibleProgs = [prog for prog in possibleProgs
                if all(prog[1](hex2int(guess))==ans for (guess, ans) in zip(guesses,anss))]
        if len(possibleProgs)==0:
            print "no programs are possible!"
            raise Exception
        if len(possibleProgs)<=4**i:
            possibleProgs2=possibleProgs[:]
            possibleProgStrings = [x[0] for x in possibleProgs]
            for prog in possibleProgs2:
                if prog[0] in possibleProgStrings:
                    mism = guessProg(progDesc['id'],prog[0],exception=False)
                    if mism is None:
                        return
                    else:
                        possibleProgs = [prog for prog in possibleProgs if prog[1](hex2int(mism[0]))==hex2int(mism[1])]
                        possibleProgStrings = [x[0] for x in possibleProgs]
                        time.sleep(5)
        print "possibilities:\n"+"\n".join([p[0] for p in possibleProgs])
    print "couldn't determine the program!"
    raise Exception

def fTrees(fs,size):
    if fs:
        if size==1:
            return [((None,1,'%s'),[])]
        elif size<=0:
            return []
        else:
            fTs=[]
            for f in fs:
                if f in unOpDict:
                    function=unOpDict[f]
                    for fT in fTrees(fs,size-1):
                        fTs.append((canonicalize((f,fT[0][1],[fT[0]])),fT[1]+[f]))
                elif f in binOpDict:
                    if size>=3:
                        function = binOpDict[f]
                        for i in range(1,(size+1)//2):
                            for fT1 in fTrees(fs,i):
                                argsNeeded = 1+sum(map(arity,fT1[1]))
                                for fT2 in fTrees(fs,size-1-i):
                                    fTs.append((canonicalize((f,fT1[0][1]+fT2[0][1],[fT1[0],fT2[0]])),fT1[1]+fT2[1]+[f]))
                elif f=="if0":
                    if size>=4:
                        function = if0
                        for i in range(1,size-2):
                            for fT1 in fTrees(fs,i):
                                for j in range(1,size-1-i):
                                    for fT2 in fTrees(fs,j):
                                        k=size-1-i-j
                                        for fT3 in fTrees(fs,k):
                                            fTs.append((canonicalize((f,fT1[0][1]+fT2[0][1]+fT3[0][1],[fT1[0],fT2[0],fT3[0]])),fT1[1]+fT2[1]+fT3[1]+[f]))


                else:
                    raise NotImplementedError, f
        return nub(fTs,0)


def fullFTrees(fs,size):
    return [fT[0] for fT in fTrees(fs,size) if len(fs)==len(set(fT[1]))]

def progs(fs,size):
    primitives = [0,1,'x']
    progs = []
    for fT in fullFTrees(fs,size-1):
        for args in itertools.product(primitives,repeat=fT[1]):
            subbed = canonicalize(subInExpr(fT,args),subbed=True)
            formattedProg =("(lambda (x) "+stringify(subbed)+")")
            progs.append((formattedProg,lambdify(subbed),subbed))
    return nub(progs,0)

def stringify(expr):
    f,n,args = expr
    if f==None:
        if args==maxInt-1:
            return "(not 0)"
        elif args==maxInt-2:
            return "(not 1)"
        elif args==maxInt-3:
            return "(not (shl1 1))"
        elif args==2:
            return "(shl1 1)"
        elif args==4:
            return "(shl1 (shl1 1))"
        else:
            return str(args)
    else:
        return "("+f+" "+" ".join(map(stringify,args))+")"

def lambdify(expr):
    f,n,args = expr
    if f==None:
        if args=='x':
            return lambda x: x
        else:
            return lambda x: args
    elif f in unOpDict:
        return lambda x: unOpDict[f](lambdify(args[0])(x))
    elif f in binOpDict:
        return lambda x: binOpDict[f](lambdify(args[0])(x),lambdify(args[1])(x))
    elif f=="if0":
        return lambda x: if0(lambdify(args[0])(x),lambdify(args[1])(x),lambdify(args[2])(x))
    else:
        raise NotImplementedError

def canonicalize(expr,subbed=False):
    f,n,args = expr
    if 'sort' in dir(args):
        args=[canonicalize(a,subbed) for a in args]
        if f in unOpDict:
            if isint(args[0][2]):
                return (None,1,unOpDict[f](args[0][2]))
            elif f=='not' and args[0][0]=='not':
                return args[0][2][0]
            elif f=='not' and args[0][0] in unOpDict:
                return (args[0][0],n,[(f,args[0][1],args[0][2])])
            elif f=='shl16' and args[0][0] in ['shl1','shl4']:
                return (args[0][0],n,[(f,args[0][1],args[0][2])])
            elif f=='shl4' and args[0][0]=='shl1':
                return (args[0][0],n,[(f,args[0][1],args[0][2])])
        elif f in binOpDict:
            if isint(args[0][2]) and isint(args[1][2]):
                return (None,1,binOpDict[f](args[0][2],args[1][2]))
            elif args[0][2]==0:
                if f=="and":
                    return (None,0,0)
                else:
                    return args[1]
            elif args[1][2]==0:
                if f=="and":
                    return (None,0,0)
                else:
                    return args[0]
            elif subbed and args[0]==args[1]:
                if f=="xor":
                    return (None,0,0)
                elif f=="plus":
                    return ("shl1",args[0][1],[args[0]])
                else:
                    return args[0]
            args.sort()
        elif f=="if0":
            if args[0][2]==0:
                return args[1]
            elif isint(args[0][2]):
                return args[2]
            elif subbed and args[1]==args[2]:
                return args[1]
            elif subbed and args[0][2]==args[1][2] and args[2][2]==0:
                return (None,0,0)
            elif subbed and args[0][2]==args[2][2] and args[1][2]==0:
                return (None,1,'x')
    return (expr[0],expr[1],args)

def nub(l,i):
    return {str(j[i]):j for j in l}.values()

def isint(x):
    return type(x) in map(type,[0,0L])

def subInExpr(expr,args):
    f, n, aas = expr
    if f==None:
        if aas in [0,1]:
            return expr
        else:
            return (f,n,args[0])
    else:
        newArgs = []
        consumed=0
        for aa in aas:
            newArgs.append(subInExpr(aa,args[consumed:consumed+aa[1]]))
            consumed+=aa[1]
        return (f,n,newArgs)
            

def guessProg(progHash,prog,exception=True):
    '''Make a guess as to what the program with id progHash is.  prog should be a string (e.g. "(lambda (x) x)").  If exception is True (default), raise Exception if the guess is wrong; otherwise, simply return the program's input and output that proves it.'''
    print "guessing %s for %s..." % (prog,progHash),
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
        try:
            print guessDict['program'],j['values']
        except:
            print guessDict, j
        if exception:
            raise Exception
        else:
            return j['values'][:2]

def evalHash(progHash,guesses):
    '''Evaluate a program with id progHash on guesses.  guesses should be a list of strings (e.g. ["0000000000000000","1234567890123456"])'''
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

def compose3List(f1,f2a,f2b,f2c,cutoff1,cutoff2):
    return lambda l: f1(f2a(l[:cutoff1]),f2b(l[cutoff1:cutoff2]),f2c(l[cutoff2:]))
        
def int2hex(x):
    if x<0 or x>maxInt
        x=x % maxInt
    return hex(x).lstrip('0x').rstrip('L').rjust(16,'0')

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

def if0(x,y,z):
    return y if x==0 else z

unOpDict = {
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
