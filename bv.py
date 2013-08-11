maxInt=2**64

class Program:

    #var is a string indicating the variable
    #exp is an expression
    def __init__(self, var, exp):
        self.var = Variable(var)
        self.exp = exp
        self.size = exp.size + 1

    def __str__(self):
        return "(lambda ("+str(self.var)+") "+str(self.exp)+")"

    def run(self, value):
        return self.exp.run()({self.var.value: value})


#This exists mostly to organize my brain
#and maybe because I am thinking in Java for some reason
#(oops)
#maybe something useful will go in here
#when someone starts trying to generate programs
class Expression:

    def __init__(self):
        self.size = 0
        raise NotImplementedError

    def __str__(self):
        raise NotImplementedError

    def run(self):
        raise NotImplementedError

class Constant(Expression):

    #value is 0 or 1
    def __init__(self, value):
        self.value = value
        self.size = 1

    def __str__(self):
        return str(self.value)

    def run(self):
        def go(args):
            return self.value
        return go

    def equals(self, other):
        if not isinstance(other, Constant):
            return False
        else:
            return self.value==other.value

class Variable(Expression):

    #value is a string
    def __init__(self, value):
        self.value = value
        self.size = 1

    def __str__(self):
        return self.value

    def run(self):
        def go(args):
            #will cause KeyError if not defined
            #haven't fully thought thorough, but might be fine?
            return args[self.value]
        return go

    def equals(self, other):
        if not isinstance(other, Variable):
            return False
        else:
            return self.value==other.value

class If(Expression):

    def __init__(self, exp0, exp1, exp2):
        self.exp0=exp0
        self.exp1=exp1
        self.exp2=exp2
        self.size = 1 + exp0.size + exp1.size + exp2.size

    def __str__(self):
        return "(if0 "+str(self.exp0)+" "+str(self.exp1)+" "+str(self.exp2)+")"

    def run(self):
        #assign arg to exp0 and return exp1 or exp2's run()
        def go(args):
            if self.exp0.run()(args)==0:
                return self.exp1.run()(args)
            else:
                return self.exp2.run()(args)
        return go

    def equals(self, other):
        if not isinstance(other, If):
            return False
        else:
            return self.exp0.equals(other.exp0) \
                   and self.exp1.equals(other.exp1) \
                   and self.exp2.equals(other.exp2)

class Fold(Expression):

    def __init__(self, exp0, exp1, exp2, var0, var1):
        self.exp0=exp0
        self.exp1=exp1
        self.exp2=exp2
        self.var0=var0
        self.var1=var1
        self.size = 2 + exp0.size + exp1.size + exp2.size

    def __str__(self):
        return "(fold "+str(self.exp0)+" "+str(self.exp1)+" (lambda (" \
               +str(self.var0)+" "+str(self.var1)+") "+str(self.exp2)+"))"

    def run(self):
        def go(args):
            acc = self.exp1.run()(args)
            e0 = self.exp0.run()(args)
            for i in range(0, 8):
                newargs = args.copy()
                newargs[self.var0.value] = (e0 >> i*8) & 0xFF
                newargs[self.var1.value] = acc
                acc = self.exp2.run()(newargs)
            return acc
        return go

    #lazy because this shouldn't matter
    def equals(self, other):
        return False
            

class Op1(Expression):

    def __init__(self, exp):
        self.exp=exp
        self.size = 1 + exp.size

class Op2(Expression):

    def __init__(self, exp0, exp1):
        self.exp0=exp0
        self.exp1=exp1
        self.size = 1 + exp0.size + exp1.size

class Not(Op1):

    def __str__(self):
        return "(not "+str(self.exp)+")"

    def run(self):
        def go(args):
            return maxInt - 1 - self.exp.run()(args)
        return go

    def equals(self, other):
        if not isinstance(other, Not):
            return False
        else:
            return self.exp.equals(other.exp)

class Shl1(Op1):

    def __str__(self):
        return "(shl1 "+str(self.exp)+")"

    def run(self):
        def go(args):
            return (self.exp.run()(args) << 1) % maxInt
        return go

    def equals(self, other):
        if not isinstance(other, Op1):
            return False
        elif isinstance(self.exp, Constant):
            return self.exp.value==0 and \
                   (isinstance(other, Shr1) or isinstance(other, Shr4) \
                    or isinstance(other, Shr16))
        else:
            return isinstance(other, Shl1) and self.exp.equals(other.exp)

class Shr1(Op1):

    def __str__(self):
        return "(shr1 "+str(self.exp)+")"

    def run(self):
        def go(args):
            return self.exp.run()(args) >> 1
        return go

    def equals(self, other):
        if not isinstance(other, Op1):
            return False
        elif isinstance(self.exp, Constant):
            return self.exp.value==0 and \
                   (isinstance(other, Shl1) or isinstance(other, Shr4) \
                    or isinstance(other, Shr16))
        else:
            return isinstance(other, Shr1) and self.exp.equals(other.exp)

class Shr4(Op1):

    def __str__(self):
        return "(shr4 "+str(self.exp)+")"

    def run(self):
        def go(args):
            return self.exp.run()(args) >> 4
        return go

    def equals(self, other):
        if not isinstance(other, Op1):
            return False
        elif isinstance(self.exp, Constant):
            return self.exp.value==0 and \
                   (isinstance(other, Shr1) or isinstance(other, Shl1) \
                    or isinstance(other, Shr16))
        else:
            return isinstance(other, Shr4) and self.exp.equals(other.exp)

class Shr16(Op1):

    def __str__(self):
        return "(shr16 "+str(self.exp)+")"

    def run(self):
        def go(args):
            return self.exp.run()(args) >> 16
        return go

    def equals(self, other):
        if not isinstance(other, Op1):
            return False
        elif isinstance(self.exp, Constant):
            return self.exp.value==0 and \
                   (isinstance(other, Shr1) or isinstance(other, Shr4) \
                    or isinstance(other, Shl1))
        else:
            return isinstance(other, Shr16) and self.exp.equals(other.exp)

class And(Op2):

    def __str__(self):
        return "(and "+str(self.exp0)+" "+str(self.exp1)+")"

    def run(self):
        def go(args):
            return self.exp0.run()(args) & self.exp1.run()(args)
        return go

    def equals(self, other):
        return isinstance(other, And) and self.exp0.equals(other.exp0) \
               and self.exp1.equals(other.exp1)

class Or(Op2):

    def __str__(self):
        return "(or "+str(self.exp0)+" "+str(self.exp1)+")"

    def run(self):
        def go(args):
            return self.exp0.run()(args) | self.exp1.run()(args)
        return go

    def equals(self, other):
        return isinstance(other, Or) and self.exp0.equals(other.exp0) \
               and self.exp1.equals(other.exp1)

class Xor(Op2):

    def __str__(self):
        return "(xor "+str(self.exp0)+" "+str(self.exp1)+")"

    def run(self):
        def go(args):
            return self.exp0.run()(args) ^ self.exp1.run()(args)
        return go

    def equals(self, other):
        return isinstance(other, Xor) and self.exp0.equals(other.exp0) \
               and self.exp1.equals(other.exp1)

class Plus(Op2):

    def __str__(self):
        return "(plus "+str(self.exp0)+" "+str(self.exp1)+")"

    def run(self):
        def go(args):
            return (self.exp0.run()(args) + self.exp1.run()(args)) % maxInt
        return go

    def equals(self, other):
        return isinstance(other, Plus) and self.exp0.equals(other.exp0) \
               and self.exp1.equals(other.exp1)
    
