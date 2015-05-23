from enum import Enum
from functools import partial

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
            raise NotImplementedError("Can't write %s to read-only register bit; "
                                      "are you trying to write an invalid value to e.g. SP?" % value)
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
            self.ram = RAM(word_length=8, size=0x10000, initial_contents=initial_ram)

        # These need to be bound to self, so they're defined in the constructor
        self.ADDRESSING_MODES = {
            None: lambda: None,
            "A": lambda: self.reg.A, # contents of A
            "IMM": lambda: self.next_word(), # next word as literal
            "REL": lambda: self.next_word() - 128, #  next word as signed integer
            "ABS": lambda: self.ram.get(self.next_two()), # get next 16 bits as address
            "ZP": lambda: self.ram.get(self.next_word()), # get next 8 bits as address for zeroth memory page
            "ABS_X": lambda: self.ram.get((self.next_two() + self.reg.X) & 0xffff), # as with ABS but add X to address
            "ABS_Y": lambda: self.ram.get((self.next_two() + self.reg.Y) & 0xffff), # as with ABS but add Y to address
            "ZP_X": lambda: self.ram.get((self.next_word() + self.reg.X) & 0xff), # as with ZP but add X to address
            "ZP_Y": lambda: self.ram.get((self.next_word() + self.reg.Y) & 0xff), # as with ZP but add Y to address
            "IND": self.addr_indirect, # Use next word as zero-page addr and the following byte from that zero page
                                            # as another 8 bits, and combine the two into a 16-bit address
            "IND_X": lambda: self.addr_indirect(add_to_index=self.reg.X), # As with the above, but add X to the ZP
            "IND_Y": lambda: self.addr_indirect(add_to_address=self.reg.Y), # As with the above, but add Y to the
                                                                            # resulting address (not the same as IND_X!)
        }

        self.OPCODES = {
            0x00: (self.BRK, None),
            0x01: (self.ORA, "IND_X"),
            0x02: None,
            0x03: None, #(self.SLO, "IND_X"),
            0x04: (self.NOP, "ZP"),
            0x05: (self.ORA, "ZP"),
            0x06: (self.ASL, "ZP"),
            0x07: None, #(self.SLO, "ZP"),
            0x08: (self.PHP, None),
            0x09: (self.ORA, "IMM"),
            0x0a: (self.ASL, None),
            0x0b: None, #(self.ANC, "IMM"),
            0x0c: (self.NOP, "ABS"),
            0x0d: (self.ORA, "ABS"),
            0x0e: (self.ASL, "ABS"),
            0x0f: None, #(self.SLO, "ABS"),

            0x10: (self.BPL, "REL"),
            0x11: (self.ORA, "IND_Y"),
            0x12: None,
            0x13: None, #(self.SLO, "IND_Y"),
            0x14: (self.NOP, "ZP_X"),
            0x15: (self.ORA, "ZP_X"),
            0x16: (self.ASL, "ZP_X"),
            0x17: None, #(self.SLO, "ZP_X"),
            0x18: (self.CLC, None),
            0x19: (self.ORA, "ABS_Y"),
            0x1a: (self.NOP, None),
            0x1b: None, #(self.SLO, "ABS_Y"),
            0x1c: (self.NOP, "ABS_X"),
            0x1d: (self.ORA, "ABS_X"),
            0x1e: (self.ASL, "ABS_X"),
            0x1f: None, #(self.SLO, "ABS_X"),

            0x20: (self.JSR, "ABS"),
            0x21: (self.AND, "IND_X"),
            0x22: None,
            0x23: (self.RLA, "IND_X"),
            0x24: (self.BIT, "ZP"),
            0x25: (self.AND, "ZP"),
            0x26: (self.ROL, "ZP"),
            0x27: (self.RLA, "ZP"),
            0x28: (self.PLP, None),
            0x29: (self.AND, "IMM"),
            0x2a: (self.ROL, None),
            0x2b: (self.ANC, "IMM"),
            0x2c: (self.BIT, "ABS"),
            0x2d: (self.AND, "ABS"),
            0x2e: (self.ROL, "ABS"),
            0x2f: (self.RLA, "ABS"),

            0x30: (self.BMI, "REL"),
            0x31: (self.AND, "IND_Y"),
            0x32: None,
            0x33: (self.RLA, "IND_Y"),
            0x34: (self.NOP, "ZP_X"),
            0x35: (self.AND, "ZP_X"),
            0x36: (self.ROL, "ZP_X"),
            0x37: (self.RLA, "ZP_X"),
            0x38: (self.SEC, None),
            0x39: (self.AND, "ABS_Y"),
            0x3a: (self.NOP, None),
            0x3b: (self.RLA, "ABS_Y"),
            0x3c: (self.NOP, "ABS_X"),
            0x3d: (self.AND, "ABS_X"),
            0x3e: (self.ROL, "ABS_X"),
            0x3f: (self.RLA, "ABS_X"),

            0x40: (self.RTI, None),
            0x41: (self.EOR, "IND_X"),
            0x42: None,
            0x43: (self.SRE, "IND_X"),
            0x44: (self.NOP, "ZP"),
            0x45: (self.EOR, "ZP"),
            0x46: (self.LSR, "ZP"),
            0x47: (self.SRE, "ZP"),
            0x48: (self.PHA, None),
            0x49: (self.EOR, "IMM"),
            0x4a: (self.LSR, None),
            0x4b: (self.ALR, "IMM"),
            0x4c: (self.JMP, "ABS"),
            0x4d: (self.EOR, "ABS"),
            0x4e: (self.LSR, "ABS"),
            0x4f: (self.SRE, "ABS"),

            0x50: (self.BVC, "REL"),
            0x51: (self.EOR, "IND_Y"),
            0x52: None,
            0x53: (self.SRE, "IND_Y"),
            0x54: (self.NOP, "ZP_X"),
            0x55: (self.EOR, "ZP_X"),
            0x56: (self.LSR, "ZP_X"),
            0x57: (self.SRE, "ZP_X"),
            0x58: (self.CLI, None),
            0x59: (self.EOR, "ABS_Y"),
            0x5a: (self.NOP, None),
            0x5b: (self.SRE, "ABS_Y"),
            0x5c: (self.NOP, "ABS_X"),
            0x5d: (self.EOR, "ABS_X"),
            0x5e: (self.LSR, "ABS_X"),
            0x5f: (self.SRE, "ABS_X"),

            0x60: (self.RTS, None),
            0x61: (self.ADC, "IND_X"),
            0x62: None,
            0x63: (self.RRA, "IND_X"),
            0x64: (self.NOP, "ZP"),
            0x65: (self.ADC, "ZP"),
            0x66: (self.ROR, "ZP"),
            0x67: (self.RRA, "ZP"),
            0x68: (self.PLA, None),
            0x69: (self.ADC, "IMM"),
            0x6a: (self.ROR, None),
            0x6b: (self.ARR, "IMM"),
            0x6c: (self.JMP, "IND"),
            0x6d: (self.ADC, "ABS"),
            0x6e: (self.ROR, "ABS"),
            0x6f: (self.RRA, "ABS"),

            0x70: (self.BVS, "REL"),
            0x71: (self.ADC, "IND_Y"),
            0x72: None,
            0x73: (self.RRA, "IND_Y"),
            0x74: (self.NOP, "ZP_X"),
            0x75: (self.ADC, "ZP_X"),
            0x76: (self.ROR, "ZP_X"),
            0x77: (self.RRA, "ZP_X"),
            0x78: (self.SEI, None),
            0x79: (self.ADC, "ABS_Y"),
            0x7a: (self.NOP, None),
            0x7b: (self.RRA, "ABS_Y"),
            0x7c: (self.NOP, "ABS_X"),
            0x7d: (self.ADC, "ABS_X"),
            0x7e: (self.ROR, "ABS_X"),
            0x7f: (self.RRA, "ABS_X"),

            0x80: (self.NOP, "IMM"),
            0x81: (self.STA, "IND_X"),
            0x82: (self.NOP, "IMM"),
            0x83: (self.SAX, "IND_X"),
            0x84: (self.STY, "ZP"),
            0x85: (self.STA, "ZP"),
            0x86: (self.STX, "ZP"),
            0x87: (self.SAX, "ZP"),
            0x88: (self.DEY, None),
            0x89: (self.NOP, "IMM"),
            0x8a: (self.TXA, None),
            0x8b: (self.XAA, "IMM"),
            0x8c: (self.STY, "ABS"),
            0x8d: (self.STA, "ABS"),
            0x8e: (self.STX, "ABS"),
            0x8f: (self.SAX, "ABS"),

            0x90: (self.BCC, "REL"),
            0x91: (self.STA, "IND_Y"),
            0x92: None,
            0x93: (self.AHX, "IND_Y"),
            0x94: (self.STY, "ZP_X"),
            0x95: (self.STA, "ZP_X"),
            0x96: (self.STX, "ZP_Y"),
            0x97: (self.SAX, "ZP_Y"),
            0x98: (self.TYA, None),
            0x99: (self.STA, "ABS_Y"),
            0x9a: (self.TXS, None),
            0x9b: (self.TAS, "ABS_Y"),
            0x9c: (self.SHY, "ABS_X"),
            0x9d: (self.STA, "ABS_X"),
            0x9e: (self.SHX, "ABS_X"),
            0x9f: (self.AHX, "ABS_X"),

            0xa0: (self.LDY, "IMM"),
            0xa1: (self.LDA, "IND_X"),
            0xa2: (self.LDX, "IMM"),
            0xa3: (self.LAX, "IND_X"),
            0xa4: (self.LDY, "ZP"),
            0xa5: (self.LDA, "ZP"),
            0xa6: (self.LDX, "ZP"),
            0xa7: (self.LAX, "ZP"),
            0xa8: (self.TAY, None),
            0xa9: (self.LDA, "IMM"),
            0xaa: (self.TAX, None),
            0xab: (self.LAX, "IMM"),
            0xac: (self.LDY, "ABS"),
            0xad: (self.LDA, "ABS"),
            0xae: (self.LDX, "ABS"),
            0xaf: (self.LAX, "ABS"),

            0xb0: (self.BCS, "REL"),
            0xb1: (self.LDA, "IND_Y"),
            0xb2: None,
            0xb3: (self.LAX, "IND_Y"),
            0xb4: (self.LDY, "ZP_X"),
            0xb5: (self.LDA, "ZP_X"),
            0xb6: (self.LDX, "ZP_X"),
            0xb7: (self.LAX, "ZP_X"),
            0xb8: (self.CLV, None),
            0xb9: (self.LDA, "ABS_Y"),
            0xba: (self.TSX, None),
            0xbb: (self.LAS, "ABS_Y"),
            0xbc: (self.LDY, "ABS_X"),
            0xbd: (self.LDA, "ABS_X"),
            0xbe: (self.LDX, "ABS_X"),
            0xbf: (self.LAX, "ABS_X"),

            0xc0: (self.CPY, "IMM"),
            0xc1: (self.CMP, "IND_X"),
            0xc2: (self.NOP, "IMM"),
            0xc3: (self.DCP, "IND_X"),
            0xc4: (self.CPY, "ZP"),
            0xc5: (self.CMP, "ZP"),
            0xc6: (self.DEC, "ZP"),
            0xc7: (self.DCP, "ZP"),
            0xc8: (self.INY, None),
            0xc9: (self.CMP, "IMM"),
            0xca: (self.DEX, None),
            0xcb: (self.AXS, "IMM"),
            0xcc: (self.CPY, "ABS"),
            0xcd: (self.CMP, "ABS"),
            0xce: (self.DEC, "ABS"),
            0xcf: (self.DCP, "ABS"),

            0xd0: (self.BNE, "REL"),
            0xd1: (self.CMP, "IND_Y"),
            0xd2: None,
            0xd3: (self.DCP, "IND_Y"),
            0xd4: (self.NOP, "ZP_X"),
            0xd5: (self.CMP, "ZP_X"),
            0xd6: (self.DEC, "ZP_X"),
            0xd7: (self.DCP, "ZP_X"),
            0xd8: (self.CLD, None),
            0xd9: (self.CMP, "ABS_Y"),
            0xda: (self.NOP, None),
            0xdb: (self.DCP, "ABS_Y"),
            0xdc: (self.NOP, "ANS_X"),
            0xdd: (self.CMP, "ANS_X"),
            0xde: (self.DEC, "ANS_X"),
            0xdf: (self.DCP, "ANS_X"),

            0xe0: (self.CPX, "IMM"),
            0xe1: (self.SBC, "IND_X"),
            0xe2: (self.NOP, "IMM"),
            0xe3: (self.ISC, "IND_X"),
            0xe4: (self.CPX, "ZP"),
            0xe5: (self.SBC, "ZP"),
            0xe6: (self.INC, "ZP"),
            0xe7: (self.ISC, "ZP"),
            0xe8: (self.INX, None),
            0xe9: (self.SBC, "IMM"),
            0xea: (self.NOP, None),
            0xeb: (self.SBC, "IMM"),
            0xec: (self.CPX, "ABS"),
            0xed: (self.SBC, "ABS"),
            0xee: (self.INC, "ABS"),
            0xef: (self.ISC, "ABS"),

            0xf0: (self.BEQ, "REL"),
            0xf1: (self.SBC, "IND_Y"),
            0xf2: None,
            0xf3: (self.ISC, "IND_Y"),
            0xf4: (self.NOP, "ZP_X"),
            0xf5: (self.SBC, "ZP_X"),
            0xf6: (self.INC, "ZP_X"),
            0xf7: (self.ISC, "ZP_X"),
            0xf8: (self.SED, None),
            0xf9: (self.SBC, "ABS_Y"),
            0xfa: (self.NOP, None),
            0xfb: (self.ISC, "ABS_Y"),
            0xfc: (self.NOP, "ABS_X"),
            0xfd: (self.SBC, "ABS_X"),
            0xfe: (self.INC, "ABS_X"),
            0xff: (self.ISC, "ABS_X"),
        }

    def step(self):
        # Get the opcode via next_word()
        opcode = self.next_word()

        # Before we look for extra info like immediate values or memory addresses,
        # we need to look up the opcode to get its addressing type.

        

    def next_word(self):
        word = self.ram.get(self.reg.PC)
        self.reg.PC += 1
        return word

    def next_two(self): # like next_word except gets two words as a 16-bit value
        return (self.next_word() << 8) | self.next_word()

    def push(self, value):
        self.ram.set(self.reg.SP, value)
        self.reg.SP -= 1

    def pop(self):
        self.reg.SP += 1
        return self.ram.get(self.reg.SP)

    # Addressing modes
    def addr_indirect(self, add_to_index=0, add_to_address=0):
        zp = (self.next_word + add_to_index) & 0xff
        address = (((self.ram.get(zp) << 8) | (self.ram.get(zp+1))) + add_to_address) & 0xffff
        return self.ram.get(address)

    # Instructions
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
        This instruction would be better named as SLS (logical Shift Left and update Sign)

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

    def BIT(self, value):
        """Test bits in A with M

        Has no return value and updates no "real" registers; only affects flags

        Writes flags: N, V, Z
        """
        result = self.reg.A & value
        self.reg.N = result >> 7
        self.reg.V = result >> 6 & 1
        self.reg.Z = result == 0

    def BMI(self, value):
        """Branch (add value to PC) iff reg.N is 1

        Value in this case should be an actual signed Python integer! ("relative" addressing)

        Reads flags: N
        """
        if self.reg.N:
            self.reg.PC += value

    def BNE(self, value):
        """Branch (add value to PC) iff reg.Z is 0

        Value in this case should be an actual signed Python integer! ("relative" addressing)

        Reads flags: Z
        """
        if not self.reg.Z:
            self.reg.PC += value

    def BPL(self, value):
        """Branch (add value to PC) iff reg.N is 0

        Value in this case should be an actual signed Python integer! ("relative" addressing)

        Reads flags: N
        """
        if not self.reg.N:
            self.reg.PC += value

    def BRK(self, *_):
        """Simulates an Interrupt REquest (IRQ)

        The copy of reg.B in the CPU is not changed (remains 0)
        The copy of reg.B which is pushed to the stack is changed (set to 1)

        Simulates an interrupt in all but one respect:
        The return address that is placed on the stack is incremented by 1
        This means that the byte following a BRK command will not be executed upon return.
        The most straightforward solution here is simply to place a NOP after every BRK.
        """
        # first, increment PC. Awkwardly, this overshoots the instruction after BRK, but that's a c64 bug and not an emulator bug
        self.reg.PC += 1
        # now write the PC to the stack in two steps
        self.push(self.reg.PCH)
        self.push(self.reg.PCL)
        # write processor flags to the stack
        self.push(self.reg.P | 0b00010000) # this logical OR forces the B flag set on write to stack only
        # set PC from the 16 bytes at 0xfffe and 0xffff
        self.reg.PC = self.ram.get(0xffff) | (self.ram.get(0xfffe) << 8)

    def BVC(self, value):
        """Branch (add value to PC) iff reg.V is 0

        Value in this case should be an actual signed Python integer! ("relative" addressing)

        Reads flags: V
        """
        if not self.reg.V:
            self.reg.PC += value

    def BVS(self, value):
        """Branch (add value to PC) iff reg.V is 1

        Value in this case should be an actual signed Python integer! ("relative" addressing)

        Reads flags: V
        """
        if self.reg.V:
            self.reg.PC += value

    def CLC(self, *_):
        """Clear carry flag
        
        Writes flags: C
        """
        self.reg.C = 0

    def CLD(self, *_):
        """Clear decimal flag
        
        Writes flags: D
        """
        self.reg.D = 0

    def CLI(self, *_):
        """Clear interrupt (disable) flag
        
        Writes flags: I
        """
        self.reg.I = 0

    def CLV(self, *_):
        """Clear overflow flag
        
        Writes flags: V
        """
        self.reg.V = 0

    def _compare(self, value, target):
        """Generic implementation of CMP, CPX, CPY"""
        result = getattr(self.reg, target) - value
        self.reg.N = result >> 7
        self.reg.C = getattr(self.reg, target) >= value
        self.reg.Z = result == 0

    def CMP(self, value):
        """Compare A with value

        Usually followed by a conditional branch

        Writes flags: N, C, Z
        """
        self._compare(value, 'A')

    def CPX(self, value):
        """Compare X with value

        Usually followed by a conditional branch

        Writes flags: N, C, Z
        """
        self._compare(value, 'X')

    def CPY(self, value):
        """Compare Y with value

        Usually followed by a conditional branch

        Writes flags: N, C, Z
        """
        self._compare(value, 'Y')

    def DEC(self, value):
        """Decrement value by one and return

        Writes flags: N, Z
        """
        result = (value - 1) & 0xff
        self.reg.N = result >> 7
        self.reg.Z = result == 0
        return result

    def _dec_or_inc_register(self, target, increment=True):
        """Generic implementation of DEX, DEY, INX, INY"""
        result = (getattr(self.reg, target) + (1 if increment else -1)) & 0xff
        self.reg.Z = result == 0
        self.reg.N = result >> 7
        setattr(self.reg, target, result)

    def DEX(self, *_):
        """Decrement X by one

        Writes flags: Z, N
        """
        self._dec_or_inc_register('X', increment=False)

    def DEY(self, *_):
        """Decrement Y by one

        Writes flags: Z, N
        """
        self._dec_or_inc_register('Y', increment=False)

    def EOR(self, value):
        """Bitwise eXclusive OR between A and value -- sets A

        Known as XOR on other platforms

        Writes flags: N, Z
        """
        result = self.reg.A ^ value
        self.reg.N = result >> 7
        self.reg.Z = result == 0
        self.reg.A = result

    def INC(self, value):
        """Increment value by one and return

        Writes flags: N, Z
        """
        result = (value + 1) & 0xff
        self.reg.N = result >> 7
        self.reg.Z = result == 0
        return result

    def INX(self, *_):
        """Increment X by one

        Writes flags: Z, N
        """
        self._dec_or_inc_register('X', increment=True)

    def INY(self, *_):
        """Decrement Y by one

        Writes flags: Z, N
        """
        self._dec_or_inc_register('Y', increment=True)

    def JMP(self, value):
        """GOTO address (set PC)"""
        self.reg.PC = value

    def JSR(self, value):
        """Jump to subroutine

        Push PC-1 (the location of the JSR call itself) to the stack, then set PC
        """
        self.reg.PC -= 1 # no problem mutating state here since it's about to be overwritten
        self.push(self.reg.PCH)
        self.push(self.reg.PCL)
        self.reg.PC = value

    def _load(self, target, value):
        """Generic impl of LDA, LDX, LDY"""
        setattr(self.reg, target, value)
        self.reg.N = value >> 7
        self.reg.Z = value == 0

    def LDA(self, value):
        """Load A with value
        
        Writes flags: N, Z
        """
        self._load('A', value)

    def LDX(self, value):
        """Load X with value
        
        Writes flags: N, Z
        """
        self._load('X', value)

    def LDY(self, value):
        """Load Y with value
        
        Writes flags: N, Z
        """
        self._load('Y', value)

    def LSR(self, value):
        """Logical shift right

        Writes flags: N, C, Z
        """
        self.reg.N = 0
        self.reg.C = value & 0b1
        result = value >> 1 & 0b01111111 # logical AND to cover case where > 8-bit value is specified
        self.reg.Z = result == 0
        return result

    def NOP(self, *_):
        """Do nothing"""
        pass

    def ORA(self, value):
        """Bitwise OR between A and value -- sets A

        Writes flags: N, Z
        """
        result = self.reg.A | value
        self.reg.N = result >> 7
        self.reg.Z = result == 0
        self.reg.A = result

    def PHA(self, *_):
        """Pushes A onto the stack.
        
        Be sure to pop it before returning from a subroutine!
        """
        self.push(self.reg.A)

    def PHP(self, *_):
        """Pushes P onto the stack.
        
        Be sure to pop it before returning from a subroutine!
        """
        self.push(self.reg.P)

    def PLA(self, *_):
        """Pops from the stack to A.
        
        Writes flags: N, Z
        """
        result = self.pop()
        self.reg.N = result >> 7
        self.reg.Z = result == 0
        self.reg.A = result

    def PLP(self, *_):
        """Pops from the stack to P."""
        self.reg.P = self.pop()

    def ROL(self, value):
        """Rotate bits left, with carry.

        Writes flags: C, Z, N
        """
        carry = value << 7
        result = ((value << 1) & 0b11111110) | self.reg.C
        self.reg.C = carry
        self.reg.N = result >> 7
        self.reg.Z = result == 0
        return result

    def ROR(self, value):
        """Rotate bits right, with carry.

        Writes flags: C, Z, N
        """
        carry = value & 0b1
        result = ((value >> 1) & 0b01111111) | (0b10000000 if self.reg.C else 0)
        self.reg.C = carry
        self.reg.N = result >> 7
        self.reg.Z = result == 0
        return result

    def RTI(self, value):
        """Return from interrupt
        
        Unlike RTS, this restores P from the stack and does NOT increment PC
        """
        self.reg.P = self.pop()
        self.reg.PCL = self.pop()
        self.reg.PCH = self.pop()

    def RTS(self, value):
        """Return from subroutine
        
        Increments PC in the process
        """
        self.reg.PCL = self.pop()
        self.reg.PCH = self.pop()
        self.reg.PC += 1

    def SBC(self, value):
        """Subtract value from A with borrow.

        There is no "subtract without carry" instruction.
        The result depends on the C flag, so when performing single precision math,
        or just before the first operation of multi precision math, the C flag must
        be set with SEC before this operation.

        The D flag changes the behavior of this operation.

        Reads flags: C
        Writes flags: D, V, C, N, Z
        """
        if self.reg.D:
            result = bcd(self.reg.A) - bdc(value) - 1 if not self.reg.C else 0
            self.reg.V = result > 99 or result < 0
        else:
            result = self.reg.A - value - 1 if not self.reg.C else 0
            self.reg.V = result > 127 or result < 128
        self.reg.C = result >= 0
        self.reg.N = result >> 7
        self.reg.Z = result == 0
        self.reg.A = result

    def SEC(self, value):
        """Set carry flag.

        Writes flag: C
        """
        self.reg.C = 1

    def SED(self, value):
        """Set decimal flag.

        Writes flag: D
        """
        self.reg.D = 1

    def SEI(self, value):
        """Set interrupt (disable) flag.

        Writes flag: I
        """
        self.reg.I = 1

    def STA(self, *_):
        """return A"""
        return self.reg.A

    def STX(self, *_):
        """return X"""
        return self.reg.X

    def STY(self, *_):
        """return Y"""
        return self.reg.Y

    def TAX(self, *_):
        """Transfer A to X"""
        self.reg.X = self.reg.A
        self.reg.N = self.reg.X << 7
        self.reg.Z = self.reg.X == 0

    def TAY(self, *_):
        """Transfer A to Y"""
        self.reg.Y = self.reg.A
        self.reg.N = self.reg.Y << 7
        self.reg.Z = self.reg.Y == 0

    def TSX(self, *_):
        """Transfer S(P) to X

        This is the only way to retrieve the stack pointer."""
        self.reg.X = self.reg.S
        self.reg.N = self.reg.X << 7
        self.reg.Z = self.reg.X == 0

    def TXA(self, *_):
        """Transfer X to A"""
        self.reg.A = self.reg.X
        self.reg.N = self.reg.A << 7
        self.reg.Z = self.reg.A == 0

    def TXS(self, *_):
        """Transfer X to SP

        This is the only way to set the stack pointer to an arbitrary value."""
        self.reg.S = self.reg.X
        self.reg.N = self.reg.S << 7
        self.reg.Z = self.reg.S == 0

    def TYA(self, *_):
        """Transfer Y to A"""
        self.reg.A = self.reg.Y
        self.reg.N = self.reg.A << 7
        self.reg.Z = self.reg.A == 0
