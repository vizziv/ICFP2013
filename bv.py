class Program:

    #var is a string indicating the variable
    #exp is an expression
    def __init__(self, var, exp):
        self.var = Variable(var)
        self.exp = exp
        self.size = exp.size + 1

    def __str__(self):
        return "(lambda("+str(self.var)+")"+str(self.exp)+")"

    def run(self, value):
        return self.exp.run()(value)


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
        def go(*args):
            return self.value
        return go

class Variable(Expression):

    #value is a string
    def __init__(self, value):
        self.value = value
        self.size = 1

    def __str__(self):
        return self.value

    def run(self):
        def go(arg):
            return arg
        return go

class If(Expression):

    def __init__(self, exp0, exp1, exp2):
        self.exp0=exp0
        self.exp1=exp1
        self.exp2=exp2
        self.size = 1 + exp0.size + exp1.size + exp2.size

    def __str__(self):
        return "(if0"+str(self.exp0)+str(self.exp1)+str(self.exp2)+")"

    def run(self):
        #assign arg to exp0 and return exp1 or exp2's run()
        def go(arg):
            if self.exp0.run()(arg)==0:
                return self.exp1.run()
            else:
                return self.exp2.run()
        return go

class Fold(Expression):

    def __init__(self, exp0, exp1, exp2, var0, var1):
        self.exp0=exp0
        self.exp1=exp1
        self.exp2=exp2
        self.var0=var0
        self.var1=var1
        self.size = 2 + exp0.size + exp1.size + exp2.size

    def __str__(self):
        return "(fold"+str(self.exp0)+str(self.exp1)+"(lambda(" \
               +str(self.var0)+" "+str(self.var1)+")"+str(self.exp2)+"))"

    #I know this doesn't work
    #but I'm sleepy and confused
    def run(self):
        def go(arg0, arg1, arg2):
            acc = self.exp1.run()(arg1)
            e0 = self.exp0.run(arg0)
            for i in range(0, 8):
                acc = self.exp2.run()((e0 >> i*8) & 0xFF, acc)
            return acc
            

class Op1(Expression):

    def __init__(self, exp):
        self.exp=exp
        self.size = 1 + exp.size

class Op2(Expression):

    def __init__(self, exp0, exp1):
        self.exp0=exp0
        self.exp1=exp1
        self.size = 1 + exp1.size + exp2.size

class Not(Op1):

    def __str__(self):
        return "(not"+str(self.exp)+")"

    def run(self):
        def go(arg):
            return ~self.exp.run()(arg)
        return go

class Shl1(Op1):

    def __str__(self):
        return "(shl1"+str(self.exp)+")"

    def run(self):
        def go(arg):
            return self.exp.run()(arg) << 1
        return go

class Shr1(Op1):

    def __str__(self):
        return "(shr1"+str(self.exp)+")"

    def run(self):
        def go(arg):
            return self.exp.run()(arg) >> 1
        return go

class Shr4(Op1):

    def __str__(self):
        return "(shr4"+str(self.exp)+")"

    def run(self):
        def go(arg):
            return self.exp.run()(arg) >> 4
        return go

class Shr16(Op1):

    def __str__(self):
        return "(shr16"+str(self.exp)+")"

    def run(self):
        def go(arg):
            return self.exp.run()(arg) >> 16
        return go

class And(Op2):

    def __str__(self):
        return "(and"+str(self.exp0)+str(self.exp1)+")"

    def run(self):
        def go(arg0, arg1):
            return self.exp0.run()(arg0) & self.exp1.run()(arg1)
        return go

class Or(Op2):

    def __str__(self):
        return "(or"+str(self.exp0)+str(self.exp1)+")"

    def run(self):
        def go():
            return self.exp0.run()(arg0) | self.exp1.run()(arg1)
        return go

class Xor(Op2):

    def __str__(self):
        return "(xor"+str(self.exp0)+str(self.exp1)+")"

    def run(self):
        def go():
            return self.exp0.run()(arg0) ^ self.exp1.run()(arg1)
        return go

class Plus(Op2):

    def __str__(self):
        return "(plus"+str(self.exp0)+str(self.exp1)+")"

    def run(self):
        def go():
            return self.exp0.run()(arg0) + self.exp1.run()(arg1)
        return go
    
