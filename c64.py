from enum import Enum
from functools import partial

class Opcode(Enum):
    pass

def sanitize_value(value, word_length):
    if not isinstance(value, int):
        value = int(value)
    value = value % 2**word_length
    return value

# binary codec decimal: translates an 8-bit value into a 2-digit decimal number
def bcd(value):
    ones = value & 0b00001111
    tens = (value & 0b11110000) >> 4
    if ones >= 10 or tens >= 10:
        raise ValueError
    else:
        return (tens*10) + ones

class RAM(object):
    # initial_contents must be a list of words, and each word must fit into word_size. **This is not checked!**
    def __init__(self, word_length, size, initial_contents=None):
        self.word_length = word_length
        self._contents = [0x0000 for _ in range(size)]
        if initial_contents:
            assert len(initial_contents) <= len(self._contents)
            self._contents[:len(initial_contents)] = initial_contents

    @property
    def size(self):
        return len(self._contents)

    def get(self, pos):
        return self._contents[pos]

    def set(self, pos, value):
        if pos >= self.size or pos < 0:
            raise ValueError('Position out of memory bounds')
        value = sanitize_value(value, self.word_length)
        self._contents[pos] = value

class Register(object):
    def __init__(self, word_length, value=0):
        self.word_length = word_length
        self._value = value

    # why isn't this a descriptor? Primarily to facilitate composite registers like PC, SP and status
    def get(self):
        return self._value

    def set(self, value):
        self._value = sanitize_value(value, self.word_length)

class ReadOnlyRegister(Register):
    def set(self, value):
        if self._value != sanitize_value(value, self.word_length):
            raise NotImplementedError
        else:
            pass # Silently do nothing if no bits would change anyways

class CompositeRegister(Register):
    def __init__(self, components):
        # components should be an ordered list, *from most to least significant bit*, of smaller registers.
        self.components = components
        word_length = 0
        for component in self.components:
            word_length += component.word_length
        self.word_length = word_length

    @property
    def _value(self):
        result = 0
        for component in self.components:
            result = (result << component.word_length) + component.get()
        return result

    @_value.setter
    def _value(self, value):
        value = sanitize_value(value, self.word_length)
        pos = 0 # position counter from most to least significant bit
        for component in self.components:
            shift_by = self.word_length - pos - component.word_length
            component.set(value >> shift_by)
            pos += component.word_length

class RegisterBank(object):
    # includes PC and other utility banks as registers

    _regs = (('A', 8), ('Y', 8), ('X', 8), ('PCH', 8), ('PCL', 8), ('S', 8),
            ('N', 1), ('V', 1), ('B', 1), ('D', 1), ('I', 1), ('Z', 1), ('C', 1))

    _composite_regs = (('PC', ['PCH', 'PCL']),
                      ('SP', [ReadOnlyRegister(1, value=1), 'S']),
                      ('P', ['N', 'V', ReadOnlyRegister(1), 'B', 'D', 'I', 'Z', 'C']))

    def __init__(self, values=None):
        regs = {}
        for reg, bits in self._regs:
            regs[reg] = Register(bits)
        for reg, description in self._composite_regs:
            # dereference all components in description
            components = [item if isinstance(item, Register) else regs[item] for item in description]
            regs[reg] = CompositeRegister(components)
        # set all regs at once, with an _ prepended
        for name, reg in regs.items():
            setattr(self, '_' + name, reg)
        if values:
            # Behavior if a value is simultaneously set for a composite register and its components is undefined
            for name, value in values.items():
                setattr(self, name, value)

    # If the attribute isn't preceeded by a "_", treat it as a register but fake it being a normal value
    def __getattr__(self, attr):
        if not attr.startswith('_'):
            return getattr(self, '_' + attr).get()
        else:
            return self.__getattribute__(attr)

    # If the attribute isn't preceeded by a "_", treat it as a register but fake it being a normal value
    def __setattr__(self, attr, value):
        if not attr.startswith('_'):
            return getattr(self, '_' + attr).set(value)
        else:
            return object.__setattr__(self, attr, value)

class CPU():
    def __init__(self, initial_registers=None, initial_ram=None):
        # initial_registers must be a RegisterBank or a dict of values for RegisterBank()
        # initial_ram must be a RAM object or a list of contents

        if isinstance(initial_registers, RegisterBank):
            self.reg = initial_registers
        else:
            self.reg = RegisterBank(initial_registers)

        if isinstance(initial_ram, RAM):
            self.ram = initial_ram
        else:
            self.ram = RAM(word_length=8, size=0xffff, initial_contents=initial_ram)

    def next_word(self):
        word = self.ram.get(self.reg.PC)
        self.reg.PC += 1
        return word

    def ADC(self, value):
        """Add value to A with carry

        Reads flags: D, C
        Writes flags: N, V, Z, C
        """
        result = self.reg.A + value + self.reg.C
        # set overflow
        self.reg.V = 1 if self.reg.A >> 7 != result >> 7 else 0
        self.reg.N = self.reg.A >> 7
        self.reg.Z = result == 0
        # binary codec decimal stuff... ick. I have no idea why the above is
        # executed even if the decimal flag is set, btw
        if self.reg.D:
            result = bcd(self.reg.A) + bcd(value) + self.reg.C
            self.reg.C = result > 99
        else:
            self.reg.C = result > 0xff
        self.reg.A = result

    def AND(self, value):
        """Bitwise AND A with value

        Writes flags: N, Z
        """
        self.reg.A = self.reg.A & value
        self.reg.Z = self.reg.A == 0
        self.reg.N = self.reg.A >> 7

    def ASL(self, value):
        """Arithmetic shift left

        Note: An Arithmetic shift normally preserves the Most Significant Bit (MSb) or "Sign bit" of the source value.
        ASL does NOT do this on the 6502.
        The 6502 places a copy of the sign from the result of a Logical Shift Left into the sigN Flag (P.N)

        Writes flags: N, Z, C

        Returns the result
        """
        self.reg.C = value >> 7
        result = (value << 1) & 0xff
        self.reg.N = result >> 7
        self.reg.Z = result == 0
        return result

    def BCC(self, value):
        """Branch (add value to PC) iff reg.C is 0

        Value in this case should be an actual signed Python integer! ("relative" addressing)

        Reads flags: C
        """
        if not self.reg.C:
            self.reg.PC += value

    def BCS(self, value):
        """Branch (add value to PC) iff reg.C is 1

        Value in this case should be an actual signed Python integer! ("relative" addressing)

        Reads flags: C
        """
        if self.reg.C:
            self.reg.PC += value


    def BEQ(self, value):
        """Branch (add value to PC) iff reg.Z is 1

        Value in this case should be an actual signed Python integer! ("relative" addressing)

        Reads flags: Z
        """
        if self.reg.Z:
            self.reg.PC += value

##################

"""
    def pop_addr(self):
        address = self.reg.sp
        self.reg.sp += 1
        return address

    def push_addr(self):
        self.reg.sp -= 1
        return self.reg.sp

    def peek_addr(self):
        return self.reg.sp

    def needs_next_word(self, operand):
        return (0x10 <= operand <= 0x17) or operand in [0x1e, 0x1f]

    # This has side effects (it can increment PC or affect SP)
    def address_for_operand(self, operand):
        if self.needs_next_word(operand):
            self.cycle += 1
        try:
            return self.operands[operand](operand)
        except KeyError:
            return None

    # This has no side effects except in the case of code=0x1f, in which case PC++
    def get_by_address(self, address, code=None):
        if address is None and code is not None:
            if code == 0x1f:
                return self.next_word()
            elif 0x20 <= code <= 0x3f:
                return code - 0x20
        elif isinstance(address, int):
            return self.ram.get(address)
        else:
            return self.reg[address]

    def set_by_address(self, address, value):
        if isinstance(address, int):
            self.ram.set(address, value)
        else:
            self.reg[address] = value

    def get_by_code(self, code, return_addr=False):
        addr = self.address_for_operand(code)
        value = self.get_by_address(addr, code=code)
        return value if not return_addr else (value, addr)

    def set_by_code(self, code, value):
        addr = self.address_for_operand(code)
        if addr:
            self.set_by_address(addr, value)
        else:
            pass

    def step(self):
        word = self.next_word()
        b, a, o = decompile_word(word)

        opcode = Opcode(o)
        if opcode == Opcode.NONBASIC:
            opcode = NonBasicOpcode(a)
            a, b = b, None

        a_val, addr = self.get_by_code(a, return_addr=True)
        operation = getattr(self, opcode.name)
        if not isinstance(opcode, NonBasicOpcode):
            b_val = self.get_by_code(b)
            operation(a_val, b_val, addr)
        else:
            operation(a_val, addr)

    def SET(self, a, b, addr):
        self.cycle += 1
        self.set_by_address(addr, b)

    def ADD(self, a, b, addr):
        self.cycle += 2
        value = a + b
        self.set_by_address(addr, value)
        self.reg.o = 0 if value < 2**16 else 0x0001

    def SUB(self, a, b, addr):
        self.cycle += 2
        value = a - b
        self.set_by_address(addr, value)
        self.reg.o = 0 if value >= 0 else 0xffff

    def MUL(self, a, b, addr):
        self.cycle += 2
        self.set_by_address(addr, a*b)
        self.reg.o = ((a*b)>>16)&0xffff

    def DIV(self, a, b, addr):
        self.cycle += 3
        try:
            self.set_by_address(addr, a // b)
            self.reg.o = ((a<<16)//b)&0xffff
        except ZeroDivisionError:
            self.set_by_address(addr, 0)

    def MOD(self, a, b, addr):
        self.cycle += 3
        try:
            self.set_by_address(addr, a % b)
        except ZeroDivisionError:
            self.set_by_address(addr, 0)

    def SHL(self, a, b, addr):
        self.cycle += 2
        self.set_by_address(addr, a<<b)
        self.reg.o = ((a<<b)>>16)&0xffff

    def SHR(self, a, b, addr):
        self.cycle += 2
        self.set_by_address(addr, a>>b)
        self.reg.o = ((a<<16)>>b)&0xffff

    def AND(self, a, b, addr):
        self.cycle += 1
        self.set_by_address(addr, a & b)

    def BOR(self, a, b, addr):
        self.cycle += 1
        self.set_by_address(addr, a | b)

    def XOR(self, a, b, addr):
        self.cycle += 1
        self.set_by_address(addr, a ^ b)

    def IFE(self, a, b, addr):
        self.cycle += 2
        if a == b:
            pass
        else:
            self.skip_next_and_cycle()

    def IFN(self, a, b, addr):
        self.cycle += 2
        if a != b:
            pass
        else:
            self.skip_next_and_cycle()

    def IFG(self, a, b, addr):
        self.cycle += 2
        if a > b:
            pass
        else:
            self.skip_next_and_cycle()

    def IFB(self, a, b, addr):
        self.cycle += 2
        if a & b != 0:
            pass
        else:
            self.skip_next_and_cycle()

    def skip_next_and_cycle(self):
        word = self.next_word() # this increments PC!
        b, a, o = decompile_word(word)
        if self.needs_next_word(a):
            self.next_word()
        if self.needs_next_word(b):
            self.next_word()
        self.cycle += 1

    def JSR(self, a, addr):
        self.cycle += 2
        addr = self.push_addr()
        self.set_by_address(addr, self.reg.pc)
        self.reg.pc = a
"""