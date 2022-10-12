# Silly proof-of-concept register VM.

def compile_binary_op(op, ast):
    result = []
    for x in compile(ast[1]):
        result.append(x)
    result.append(PUSH_REG)
    result.append(RES)
    for x in compile(ast[2]):
        result.append(x)
    result.append(ASSIGN_REG_REG)
    result.append(Y)
    result.append(RES)
    result.append(POP)
    result.append(X)
    result.append(op)
    return result

def compile(ast):
    result = []

    if ast[0] == 'CONST':
        result.append(ASSIGN_REG_LIT)
        result.append(RES)
        result.append(ast[1])
    elif ast[0] == 'ADD':
        result += compile_binary_op(ADD, ast)
    elif ast[0] == 'SUB':
        result += compile_binary_op(SUB, ast)
    elif ast[0] == 'MUL':
        result += compile_binary_op(MUL, ast)
    elif ast[0] == 'DIV':
        result += compile_binary_op(DIV, ast)
    elif ast[0] == 'RETURN':
        result.append(RETURN)
    else:
        raise Exception('Cannot compile unknown AST node: {}'.format(ast[0]))

    return result

# opcodes
ASSIGN_REG_LIT = 0x0
ASSIGN_REG_REG = 0x1
ADD = 0x2
SUB = 0x3
MUL = 0x4
DIV = 0x5
SWAP = 0x6
RETURN = 0x7
PUSH_REG = 0x8
POP = 0x9

# register indices
X = 0x0
Y = 0x1
RES = 0x2

registers = [0x0] * 8
stack = []

def reg_name(i):
    if i == X: return 'x'
    if i == Y: return 'y'
    if i == RES: return 'res'

def print_instructions(xs):
    i = 0

    while i < len(xs):
        if xs[i] == ASSIGN_REG_LIT:
            # print('ASSIGN_REG_LIT {} {}'.format(reg_name(xs[i + 1]), xs[i + 2]))
            print('{} <- {}'.format(reg_name(xs[i + 1]), xs[i + 2]))
            i += 3
        elif xs[i] == ASSIGN_REG_REG:
            # print('ASSIGN_REG_REG {} {}'.format(reg_name(xs[i + 1]), reg_name(xs[i + 2])))
            print('{} <- ${}'.format(reg_name(xs[i + 1]), reg_name(xs[i + 2])))
            i += 3
        elif xs[i] == ADD:
            print('add')
            i += 1
        elif xs[i] == SUB:
            print('sub')
            i += 1
        elif xs[i] == MUL:
            print('mul')
            i += 1
        elif xs[i] == DIV:
            print('div')
            i += 1
        elif xs[i] == PUSH_REG:
            print('push ${}'.format(reg_name(xs[i + 1])))
            i += 2
        elif xs[i] == POP:
            print('{} <- pop'.format(reg_name(xs[i + 1])))
            i += 2
        else:
            raise Exception('Cannot print instruction: {}'.format(xs[i]))

def eval(instructions):
    print_instructions(instructions)
    ip = 0
    cont = True
    while ip < len(instructions):
        if instructions[ip] == ASSIGN_REG_LIT:
            r = instructions[ip + 1]
            x = instructions[ip + 2]
            registers[r] = x
            ip += 3
        elif instructions[ip] == ASSIGN_REG_REG:
            r_dst = instructions[ip + 1]
            r_src = instructions[ip + 2]
            registers[r_dst] = registers[r_src]
            ip += 3
        elif instructions[ip] == ADD:
            registers[RES] = registers[X] + registers[Y]
            ip += 1
        elif instructions[ip] == MUL:
            registers[RES] = registers[X] * registers[Y]
            ip += 1
        elif instructions[ip] == SUB:
            registers[RES] = registers[X] - registers[Y]
            ip += 1
        elif instructions[ip] == MUL:
            registers[RES] = registers[X] * registers[Y]
            ip += 1
        elif instructions[ip] == DIV:
            registers[RES] = registers[X] / registers[Y]
            ip += 1
        elif instructions[ip] == SWAP:
            r1 = instructions[ip + 1]
            r2 = instructions[ip + 2]
            registers[r1], registers[r2] = registers[r2], registers[r1]
            ip += 3
        elif instructions[ip] == RETURN:
            ip += 1
            cont = False
            return registers[RES]
        elif instructions[ip] == PUSH_REG:
            src = instructions[ip + 1]
            stack.append(registers[src])
            ip += 2
        elif instructions[ip] == POP:
            dst = instructions[ip + 1]
            registers[dst] = stack.pop()
            ip += 2
        else:
            raise Exception('Cannot eval instruction: {}'.format(instructions[ip]))
    return registers[RES]

def main():
    ast = ['ADD',
           ['MUL',
            ['MUL', ['CONST', 2], ['CONST', 3]],
            ['DIV', ['CONST', 5], ['CONST', 5]]],
           ['ADD',
            ['SUB', ['CONST', 10], ['CONST', 1]],
            ['MUL', ['CONST', 2], ['CONST', 2]]]]

    print('result: {}'.format(eval(compile(ast))))

main()
