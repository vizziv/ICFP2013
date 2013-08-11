from small_cases import *
from bv import *

exps_by_size = {}

'''size is an int meaning program size
ops is a list of strings representing operations,
not including fold/tfold
only works on size 8 right now'''
def gen_tfold_prog(size, ops):
    #it's size-5 because program is 1, x is 1, 0 is 1, and fold is 2
    exps_by_size.clear()
    return [Program('x', Fold(Variable('x'), Constant(0), exp, Variable('x'), \
                              Variable('y')))
            for exp in gen_nofold_exp(size-5, ops, ['x', 'y'])]

def gen_nofold_exp(size, ops, all_vars):
    if exps_by_size.get(size) != None:
        return exps_by_size.get(size, id)
    if size<1:
        return []
    if size==1:
        exps_by_size[size] = [Variable(var) for var in all_vars] + \
                                   [Constant(0), Constant(1)]
        return exps_by_size[size]
    else:
        exps = []
        for op in ops:
            if op in unops:
                exps_by_size[size-1] = exps1 = gen_nofold_exp(size-1, ops, all_vars)
                exps += [unops[op](exp) for exp in exps1]
            elif op in binops:
                for i in xrange(1, size-1):
                    exps_by_size[i] = exps1 = gen_nofold_exp(i, ops, all_vars)
                    exps_by_size[size-i-1] = exps2 = \
                                             gen_nofold_exp(size-i-1, ops, all_vars)
                    exps += [binops[op](exp1, exp2) for exp1 in exps1 \
                             for exp2 in exps2]
            elif op=="if0":
                raise NotImplementedError
        return exps

def gen_tfold_8(size, ops, all_vars):
    if size!= 3: raise Exception
    exps = []
    for op in ops:
        if op in unops:
            exps += [unops[op](e) for e in gen_size_2(ops, all_vars)]
        elif op in binops:
            size1 = gen_size_1(all_vars)
            for a in size1:
                for b in size1:
                    exps.append(binops[op](a, b))
        elif op == 'tfold':
            pass
        else:
            raise Exception
    return exps

def gen_size_1(all_vars):
    return [Variable(var) for var in all_vars] + [Constant(0), Constant(1)]

def gen_size_2(ops, all_vars):
    exps = []
    for op in ops:
        if op in unops:
            exps += [unops[op](e) for e in gen_size_1(all_vars)]
    return exps

'''mostly copied from small_cases.solveSmallEasy()'''
def solve_small_tfold(progDesc):
    print "solving program %s" % progDesc['id']
    possibleProgs = gen_tfold_prog(progDesc['size'],progDesc['operators'])
    for i in xrange(10):
        guesses=map(int2hex,xrange((3**35)*i-(2**i+1)*128,(3**35)*i+(2**i+1)*128,2**i+1))
        print "making guesses [%s,...,%s] ... " % (guesses[0],guesses[-1])
        anss = evalHash(progDesc['id'],guesses)
        #print zip(guesses, anss)
        #for p in possibleProgs:
        #    print p
##        poss = []
##        for prog in possibleProgs:
##            if all(prog.run(hex2int(guess))==ans for (guess, ans) in zip(guesses, anss)):
##                poss.append(prog)
        possibleProgs = [prog for prog in possibleProgs
                if all(prog.run(hex2int(guess))==ans for (guess, ans) in zip(guesses,anss))]
        if len(possibleProgs)==0:
            print "no programs are possible!"
            raise Exception
        if len(possibleProgs)<=4**i:
            possibleProgs2=possibleProgs[:]
            possibleProgStrings = [str(x) for x in possibleProgs]
            for prog in possibleProgs2:
                if str(prog) in possibleProgStrings:
                    mism = guessProg(progDesc['id'],str(prog),exception=False)
                    if mism is None:
                        return
                    else:
                        possibleProgs = [prog for prog in possibleProgs if prog.run(hex2int(mism[0]))==hex2int(mism[1])]
                        possibleProgStrings = [str(prog) for x in possibleProgs]
                        time.sleep(100)
        print "possibilities:\n"+"\n".join([str(prog) for p in possibleProgs])
    print "couldn't determine the program!"
    raise Exception

unops = {
        "shr1" : Shr1,
        "shr4" : Shr4,
        "shr16" : Shr16,
        "shl1" : Shl1,
        "not" : Not,
        }

binops = {
        "and" : And,
        "or" : Or,
        "xor" : Xor,
        "plus" : Plus,
        }
