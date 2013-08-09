class Program:

    #var is a string indicating the variable
    #exp is an expression
    def __init__(self, var, exp):
        self.var = var
        self.exp = exp
        self.size = exp.size + 1

    def __str__(self):
        return "(lambda("+self.var+")"+str(self.exp)+")"

    def run(self, value):
        return self.exp.run()((self.var, value))


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
        return (lambda x: self.value)

class Variable(Expression):

    #value is a string
    def __init__(self, value):
        self.value = value
        self.size = 1

    def __str__(self):
        return self.value

    def run(self):
        return (lambda x: x)

class If(Expression):

    def __init__(self, exp1, exp2, exp3):
        self.exp1=exp1
        self.exp2=exp2
        self.exp3=exp3
        self.size = 1 + exp1.size + exp2.size + exp3.size

    def __str__(self):
        return "(if0"+str(self.exp1)+str(self.exp2)+str(self.exp3)+")"

    def run(self):
        def go():
            if self.exp1.run()==0:
                return self.exp2.run()
            else:
                return self.exp3.run()
        return go

class Fold(Expression):

class Op1(Expression):

class Op2(Expression):

class Not(Op1):

class Shl1(Op1):

class Shr1(Op1):

class Shr4(Op1):

class Shr16(Op1):

class And(Op2):

class Or(Op2):

class Xor(Op2):

class Plus(Op2):
    
