'''
  This generator will generate our whole instructionset, without it we would be lost.
  The instrctionset contains several thousen different instructions.
  
  I would not want to be one to write all these manually! ;-)

  Some of these operators may need some manual fixes afterwards
'''
import re
from collections import namedtuple

Instr = namedtuple('Instr', 'token instruction evalfmt narg format compat1 compat2')
Type = namedtuple('Type', 'long short')

newline = '\n' #python will fix this

is_signed   = lambda x: x.startswith('i')
is_unsigned = lambda x: x.startswith('u')
is_float    = lambda x: x.startswith('f')
is_ordinal  = lambda x: is_signed(x) or is_unsigned(x)

#order matters
notype = 'xtNone';
type_names = [
  Type('xtInt8',   'i8'),   #0
  Type('xtUInt8',  'u8'),   #1
  Type('xtInt16',  'i16'),  #2
  Type('xtUInt16', 'u16'),  #3
  Type('xtInt32',  'i32'),  #4
  Type('xtUInt32', 'u32'),  #5
  Type('xtInt64',  'i64'),  #6
  Type('xtUInt64', 'u64'),  #7
  Type('xtSingle', 'f32'),  #8
  Type('xtDouble', 'f64')   #9
]

type_names_all = type_names + [
  Type('xtBoolean','bool'), #10
  Type('xtNone',   'none')  #11
]

ptr = lambda x: 'P'+x[2:]

boolTest   = lambda x,y: (x == y)
exactOnly  = lambda x,y: x == y
canAssign  = lambda x,y: (is_ordinal(x) and is_ordinal(y)) or (is_float(x))
canBitwise = lambda x,y: is_ordinal(x) and is_ordinal(y)

fmtAsgn    = lambda fmtstr, instr, left, right, result: fmtstr % (instr, left, right)
fmtBinLeft = lambda fmtstr, instr, left, right, result: fmtstr % (instr, left, left, right)
fmtUnary   = lambda fmtstr, instr, left, right, result: fmtstr % (instr, result, left)
fmtBinary  = lambda fmtstr, instr, left, right, result: fmtstr % (instr, result, left, right)
fmtJumpIf  = lambda fmtstr, instr, left, right, result: fmtstr % (instr, left)
fmtJumpCmp = lambda fmtstr, instr, left, right, result: fmtstr % (instr, left, right)

instructionset = [
  # jump instructions (relative) :
  Instr('op_AND','JIZ', '%s: if %s(op.arg1)^ = 0 then pc += PtrInt(op.arg2);',   2, fmtJumpIf, boolTest, None), 
  Instr('op_OR', 'JNZ', '%s: if %s(op.arg1)^ <> 0 then pc += PtrInt(op.arg2);',  2, fmtJumpIf, boolTest, None),
  
  # pointer :
  Instr('op_Deref',   'DREF',  '%s: %s(op.arg1)^ := %s(Pointer(op.arg2)^)^;',      1, fmtAsgn, exactOnly, None),
  Instr(None,         'MOVP',  '%s: %s(Pointer(op.arg1)^)^ := %s(op.arg2)^;',      2, fmtAsgn, canAssign, None),
  
  # assignment instructions : 
  Instr('op_Asgn',    'MOV',   '%s: %s(op.arg1)^ := %s(op.arg2)^;',                2, fmtAsgn, canAssign, None),
  
  Instr('op_AsgnAdd', 'LADD',  '%s: %s(op.arg1)^ := %s(op.arg1)^ + %s(op.arg2)^;',         2, fmtBinLeft,  canAssign, None), 
  Instr('op_AsgnSub', 'LSUB',  '%s: %s(op.arg1)^ := %s(op.arg1)^ - %s(op.arg2)^;',         2, fmtBinLeft,  canAssign, None), 
  Instr('op_AsgnMul', 'LMUL',  '%s: %s(op.arg1)^ := %s(op.arg1)^ * %s(op.arg2)^;',         2, fmtBinLeft,  canAssign, None), 
  Instr('op_AsgnDiv', 'LDIV',  '%s: %s(op.arg1)^ := Divide(%s(op.arg1)^, %s(op.arg2)^);',  2, fmtBinLeft,  canAssign, None), 
  Instr('op_AsgnMod', 'LMOD',  '%s: %s(op.arg1)^ := Modulo(%s(op.arg1)^, %s(op.arg2)^);',  2, fmtBinLeft,  canAssign, None),
  Instr('op_AsgnBND', 'LBND',  '%s: %s(op.arg1)^ := %s(op.arg1)^ and %s(op.arg2)^;',       2, fmtBinLeft,  canBitwise, canAssign), 
  Instr('op_AsgnBOR', 'LBOR',  '%s: %s(op.arg1)^ := %s(op.arg1)^ or %s(op.arg2)^;',        2, fmtBinLeft,  canBitwise, canAssign), 
  Instr('op_AsgnXOR', 'LXOR',  '%s: %s(op.arg1)^ := %s(op.arg1)^ xor %s(op.arg2)^;',       2, fmtBinLeft,  canBitwise, canAssign), 
  Instr('op_AsgnSHL', 'LSHL',  '%s: %s(op.arg1)^ := %s(op.arg1)^ shl %s(op.arg2)^;',       2, fmtBinLeft,  canBitwise, canAssign), 
  Instr('op_AsgnSHR', 'LSHR',  '%s: %s(op.arg1)^ := %s(op.arg1)^ shr %s(op.arg2)^;',       2, fmtBinLeft,  canBitwise, canAssign), 
  
  # unary :
  Instr('op_UnarySub', 'USUB',  '%s: %s(op.arg2)^ := -%s(op.arg1)^;',  1, fmtUnary,   None, None), 
  
  # binary :
  Instr('op_Add', 'ADD',  '%s: %s(op.arg3)^ := %s(op.arg1)^ + %s(op.arg2)^;',          2, fmtBinary,  None, None), 
  Instr('op_Sub', 'SUB',  '%s: %s(op.arg3)^ := %s(op.arg1)^ - %s(op.arg2)^;',          2, fmtBinary,  None, None), 
  Instr('op_Mul', 'MUL',  '%s: %s(op.arg3)^ := %s(op.arg1)^ * %s(op.arg2)^;',          2, fmtBinary,  None, None), 
  Instr('op_Div', 'DIV',  '%s: %s(op.arg3)^ := Divide(%s(op.arg1)^, %s(op.arg2)^);',   2, fmtBinary,  None, None), 
  Instr('op_Mod', 'MOD',  '%s: %s(op.arg3)^ := Modulo(%s(op.arg1)^, %s(op.arg2)^);',   2, fmtBinary,  None, None), 
  Instr('op_Pow', 'POW',  '%s: %s(op.arg3)^ := Power(%s(op.arg1)^, %s(op.arg2)^);',         2, fmtBinary,  None, None), 
  
  Instr('op_EQ',  'EQ',   '%s: %s(op.arg3)^ := %s(op.arg1)^ = %s(op.arg2)^;',   2, fmtBinary,  None, None), 
  Instr('op_GTE', 'GTE',  '%s: %s(op.arg3)^ := %s(op.arg1)^ >= %s(op.arg2)^;',  2, fmtBinary,  None, None), 
  Instr('op_GT',  'GT',   '%s: %s(op.arg3)^ := %s(op.arg1)^ > %s(op.arg2)^;',   2, fmtBinary,  None, None), 
  Instr('op_LT',  'LT',   '%s: %s(op.arg3)^ := %s(op.arg1)^ < %s(op.arg2)^;',   2, fmtBinary,  None, None), 
  Instr('op_LTE', 'LTE',  '%s: %s(op.arg3)^ := %s(op.arg1)^ <= %s(op.arg2)^;',  2, fmtBinary,  None, None), 
  Instr('op_NEQ', 'NEQ',  '%s: %s(op.arg3)^ := %s(op.arg1)^ <> %s(op.arg2)^;',  2, fmtBinary,  None, None), 
  
  Instr('op_BND', 'BND',  '%s: %s(op.arg3)^ := %s(op.arg1)^ and %s(op.arg2)^;',     2, fmtBinary, canBitwise, None), 
  Instr('op_BOR', 'BOR',  '%s: %s(op.arg3)^ := %s(op.arg1)^ or %s(op.arg2)^;',      2, fmtBinary, canBitwise, None), 
  Instr('op_SHL', 'SHL',  '%s: %s(op.arg3)^ := %s(op.arg1)^ shl %s(op.arg2)^;',     2, fmtBinary, canBitwise, None), 
  Instr('op_SHR', 'SHR',  '%s: %s(op.arg3)^ := %s(op.arg1)^ shr %s(op.arg2)^;',     2, fmtBinary, canBitwise, None), 
  Instr('op_XOR', 'XOR',  '%s: %s(op.arg3)^ := %s(op.arg1)^ xor %s(op.arg2)^;',     2, fmtBinary, canBitwise, None),
  Instr('op_SAR', 'SAR',  '%s: %s(op.arg3)^ := Sar(%s(op.arg1)^, %s(op.arg2)^);',   2, fmtBinary, canBitwise, None), 
]

bool_instr = ('EQ', 'GT', 'GTE', 'LT', 'LTE', 'NEQ', 'JIZ','JNZ')

# ---------------------------------------------------------------------------
def nameindex(test, idx, arr=type_names):
    for i,n in enumerate(arr):
      if n[idx] == test:
        return i
    
    raise ValueError("Can't find %s at %d" % (test, idx))

# ---------------------------------------------------------------------------
# the minimum rule is to prevent unwanted overflow
def minimum_rule(typ):
    getmax = lambda x,y: max(nameindex(x, 1), nameindex(y, 1))
    if is_signed(typ):   return type_names[getmax(typ, 'i32')][1]
    if is_unsigned(typ): return type_names[getmax(typ, 'u32')][1]


# Compute the result type of an operation
# The arthimetic rule used here goes something like this:
#
# 1: If both operands are the same type, the result keeps the type, but promotes to the 
#      type of the operands which is largest. The minimum rule applies here as well.
#
# 2: If any operand is real, the reuslt is real. The size of the result is the size of 
#      the largest operand
#
# 3: The result of signed and unsigned operands are signed, but promoted to the next tier over the largest, then
#      the minimum rule is applied here as well.
#
# 4: If you can't handle this.. then this is why we have to support explicit casting! ;)

def restype(op, x,y):
    if op in bool_instr: return 'bool'
    idx1 = nameindex(x, 1)
    idx2 = nameindex(y, 1)

    # case #1 of our rule (same type)
    if (is_signed(x)   and is_signed(y)) or \
       (is_unsigned(x) and is_unsigned(y)):
      return minimum_rule(type_names[max(idx1,idx2)][1])

    # case #2 of our rule (any is float)
    if (is_float(x) and (y in ['u64','i64'])) or \
       (is_float(y) and (x in ['u64','i64'])):
      return 'f64'

    if is_float(x) or is_float(y):
      return type_names[max(idx1,idx2)][1]

    # case 3 of our rule (uint-int operands)
    if x.endswith('64') or y.endswith('64'): return 'i64' #no higher signed type
    
    # handle operands smaller than 64bit:
    if is_unsigned(x): return minimum_rule(type_names[max(idx1+1, idx2)][1])
    if is_unsigned(y): return minimum_rule(type_names[max(idx2+1, idx1)][1])
  

# ---------------------------------------------------------------------------
# print the shit out of some lookup array
def print_op2instruct(instr):
    if instr.token is None:
      return ''

    result = newline
    result += ('// '+'-'*76) + newline
    result += ('// %s operators' % instr.instruction) + newline

    op2instruct_arr1 = 'op2instruct[%s][%-8s][%s]'
    op2instruct_arr2 = 'op2instruct[%s][%s][%-8s]'

    for i, left in enumerate(type_names):
      print_newline = False;
      for j, right in enumerate(type_names):
        if ((instr.compat1 is not None) and (not instr.compat1(left.short, right.short))) or \
           ((instr.compat2 is not None) and (not instr.compat2(left.short, right.short))):
          continue;
        
        if (instr.narg == 1):
          arr = op2instruct_arr1 % (instr.token, left.long, notype)
          res = '%s_%s'          % (instr.instruction, left.short)
        elif (instr.narg == 2) and (left.short == right.short):
          arr = op2instruct_arr2 % (instr.token, left.long, right.long)
          res = '%s_%s'          % (instr.instruction, left.short)
        elif instr.narg == 2:
          arr = op2instruct_arr2 % (instr.token, left.long, right.long)
          res = '%s_%s_%s'       % (instr.instruction, left.short, right.short)

        result += ('%s := %s;' % (arr, res)) + newline
        if instr.narg == 1: 
          break
        print_newline = True;

      if print_newline: result += newline

    return result


# ---------------------------------------------------------------------------
# print every freaking possible evaluation case
def print_evalcode(instr):
    resops = newline
    resops += ('// '+'-'*76) + newline
    resops += ('// %s instructions' % instr.instruction) + newline

    result = newline
    result += ('// ' + '-'*76) + newline
    result += ('// %s instructions' % instr.instruction) + newline

    # special cases
    if instr.instruction == 'DREF':
      result += 'DREF   : Move(PByte(Pointer(op.arg2)^)^, PByte(op.arg1)^, PtrUInt(op.arg3));' + newline
      resops += 'DREF, ';

    # ...
    for i, left in enumerate(type_names):
      print_newline = False;
      for j, right in enumerate(type_names):
        if ((instr.compat1 is not None) and (not instr.compat1(left.short, right.short))) or \
           ((instr.compat2 is not None) and (not instr.compat2(left.short, right.short))):
          continue;
        
        if instr.narg == 1:
          case = '%s_%s' % (instr.instruction, left.short)
        elif instr.narg == 2 and left.short == right.short:
          case = '%s_%s' % (instr.instruction, left.short)
        elif instr.narg == 2: 
          case = '%s_%s_%s' % (instr.instruction, left.short, right.short)

        restyp = restype(instr.instruction, left.short, right.short)
        restyp = type_names_all[nameindex(restyp, 1, type_names_all)].long
        resops += case + ', '

        result += (instr.format(instr.evalfmt, case, ptr(left.long), ptr(right.long), ptr(restyp))) + newline
        if instr.narg == 1: 
          break
        print_newline = True;

      if print_newline: 
        resops += newline
        result += newline
    return result, resops + newline

# ---------------------------------------------------------------------------
# print the shit out of evalres lookup array
def print_evalres(instr):
    if instr.token is None:
      return ''

    result = newline
    result += '// ' + '-'*76
    result += newline
    result += '// %s instructions' % instr.instruction
    result += newline

    evalResArr1 = 'evalResArr[%s][%-8s][%s]'
    evalResArr2 = 'evalResArr[%s][%s][%-8s]'

    for i, left in enumerate(type_names):
      print_newline = False;
      for j, right in enumerate(type_names):
        if ((instr.compat1 is not None) and (not instr.compat1(left.short, right.short))) or \
           ((instr.compat2 is not None) and (not instr.compat2(left.short, right.short))):
          continue;
        
        restyp = type_names_all[nameindex(restype(instr.instruction, left.short, right.short), 1, type_names_all)]

        if instr.narg == 1:
          arr = evalResArr1 % (instr.token, left.long, notype)
        elif instr.narg == 2: 
          arr = evalResArr2 % (instr.token, left.long, right.long)
        res = ' := %s;' % (restyp.long)

        result += arr + res + newline
        if instr.narg == 1: 
          break
        print_newline = True;

      if print_newline: result += newline
    return result


# this will create both the op2instruct lookup as well as the actual instructions.
def do_op2instruct(instructionset):
    result  = 'op2instruct[op_Addr][xtNone][xtNone] := ADDROF;' + newline
    for instr in instructionset:
      result += print_op2instruct(instr);
    return result


def do_evalcode(instructionset):
    result = 'ADDROF: PPointer(op.arg2)^ := op.arg1;' + newline
    opcodes = 'ADDROF, ' + newline

    for instr in instructionset:
      table, opcode = print_evalcode(instr)
      result  += table
      opcodes += opcode

    return result, opcodes


def do_evalres(instructionset):
    result = 'evalResArr[op_Addr][xtNone][xtNone] := xtPointer;' + newline
    for instr in instructionset:
      result += print_evalres(instr)
    return result


def main():
    op2ins = do_op2instruct(instructionset)
    cases, opcodes = do_evalcode(instructionset)
    restyp = do_evalres(instructionset)
    
    with open('op2instr.inc', 'w') as file: 
      file.write(header)
      file.write(op2ins)

    with open('opcodes.inc', 'w') as file:
      file.write(header)
      file.write(opcodes)
    
    with open('evalcode.inc', 'w') as file: 
      file.write(header)
      file.write(cases)

    with open('evalres.inc', 'w') as file:
      file.write(header)
      file.write(restyp)


if __name__ == '__main__':
    import datetime, time as t

    now = datetime.datetime.now()
    time = str(now.replace(microsecond=0))

    header  = '//' + '-'*76 + newline
    header += '// This file was autogenerated by generator.py (%s) %s' % (time,  newline)
    header += '// Note that this file should not be modified manually, it may be overwritten' + newline
    header += newline
	
    start = t.time()
    main();
    print('Generated in %.3f ms' % ((t.time() - start) * 1000))