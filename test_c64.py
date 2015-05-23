import c64
import pytest

@pytest.fixture
def cpu():
    return c64.CPU()

def test_register():
    reg = c64.Register(word_length=8, value=0xf0)
    assert reg.get() == 0xf0
    reg.set(0x0f)
    assert reg.get() == 0x0f
    reg.set(0x4040)
    assert reg.get() == 0x40
    reg.set(-1)
    assert reg.get() == 0xff

def test_readonly_register():
    ro_reg = c64.ReadOnlyRegister(word_length=1, value=1)
    assert ro_reg.get() == 1
    ro_reg.set(1) # this shouldn't fail
    assert ro_reg.get() == 1
    with pytest.raises(NotImplementedError):
        ro_reg.set(0)
    assert ro_reg.get() == 1

def test_composite_register():
    ro_reg = c64.ReadOnlyRegister(word_length=1, value=1)
    reg_a = c64.Register(word_length=8, value=0x0f)
    reg_b = c64.Register(word_length=8, value=0xf0)
    creg = c64.CompositeRegister([ro_reg, reg_a, reg_b])
    assert creg.get() == (ro_reg.get() << 16) + (reg_a.get() << 8) + reg_b.get()
    creg.set(0x10000)
    assert creg.get() == 0x10000
    with pytest.raises(NotImplementedError):
        creg.set(0x00000)
    creg.set(0x1abcd)
    assert creg.get() == 0x1abcd
    assert ro_reg.get() == 1
    assert reg_a.get() == 0xab
    assert reg_b.get() == 0xcd
    with pytest.raises(NotImplementedError):
        creg.set(0xfff0ffff)
    creg.set(0xffffffff)
    assert creg.get() == 0x1ffff
    assert ro_reg.get() == 1
    assert reg_a.get() == 0xff
    assert reg_b.get() == 0xff

def test_blank_register_bank():
    regbank = c64.RegisterBank()
    for name, _ in regbank._regs:
        assert getattr(regbank, name) == 0
    assert regbank.SP == 0x100
    assert regbank.P == 0
    assert regbank.PC == 0
    regbank.PC = 0x1234
    assert regbank.PC == 0x1234
    assert regbank.PCH == 0x12
    assert regbank.PCL == 0x34
    with pytest.raises(NotImplementedError):
        regbank.SP = 0
    assert regbank.SP == 0x100
    regbank.SP = 0x1ab
    assert regbank.SP == 0x1ab
    assert regbank.S == 0xab
    for name, word_length in regbank._regs:
        assert getattr(regbank, '_' + name).word_length == word_length
        setattr(regbank, name, int('1'*(word_length+1), base=2))
        assert getattr(regbank, name) == int('1'*(word_length), base=2)
    with pytest.raises(AttributeError):
        regbank._NOT_A_REAL_REGISTER
    with pytest.raises(AttributeError):
        regbank.NOT_A_REAL_REGISTER

def test_default_value_register_bank():
    with pytest.raises(AttributeError):
        regbank = c64.RegisterBank({'NOT_A_REAL_REGISTER': 0xff})
    with pytest.raises(NotImplementedError):
        regbank = c64.RegisterBank({'SP': 0})
    regbank = c64.RegisterBank({'A': 0xff, 'Y': 0xffff, 'X': 0x12,
                                'N': 1, 'V': 0,
                                'PCH': 0xff, 'SP': 0x123})
    assert regbank.A == 0xff
    assert regbank.Y == 0xff
    assert regbank.X == 0x12
    assert regbank.N == 1
    assert regbank.P == 0b10000000
    assert regbank.PCH == 0xff
    assert regbank.PC == 0xff00
    assert regbank.S == 0x23
    assert regbank.SP == 0x123

def test_blank_ram():
    ram = c64.RAM(8, 0xffff)
    assert ram.word_length == 8
    assert ram.size == 0xffff
    ram.set(0x1000, 0xfa)
    assert ram.get(0x1000) == 0xfa
    ram.set(0x2000, 0xfffb)
    assert ram.get(0x2000) == 0xfb
    with pytest.raises(ValueError):
        ram.set(0x1ffff, 1)

def test_initial_contents_ram():
    ram = c64.RAM(8, 0xffff, initial_contents=[0xff, 0x01, 0x02, 0x03])
    assert ram.word_length == 8
    assert ram.size == 0xffff
    assert ram.get(0) == 0xff
    assert ram.get(1) == 0x01
    assert ram.get(2) == 0x02
    assert ram.get(3) == 0x03
    ram.set(3, 0x06)
    assert ram.get(3) == 0x06

def test_cpu_init():
    cpu = c64.CPU()
    assert cpu.reg.PC == 0
    assert cpu.reg.SP == 0x100
    assert cpu.ram.size == 0xffff
    assert cpu.ram.word_length == 8
    assert cpu.ram.get(0) == 0

def test_cpu_init_with_defaults():
    cpu = c64.CPU(initial_registers={'PC': 0x1234}, initial_ram=[0xff, 0x02])
    assert cpu.reg.PC == 0x1234
    assert cpu.reg.SP == 0x100
    assert cpu.ram.size == 0xffff
    assert cpu.ram.word_length == 8
    assert cpu.ram.get(0) == 0xff
    assert cpu.ram.get(1) == 0x02

def test_cpu_init_with_initialized_defaults():
    ram = c64.RAM(8, 0xffff, initial_contents=[0xff, 0x01, 0x02, 0x03])
    regbank = c64.RegisterBank({'A': 0xff, 'Y': 0xffff, 'X': 0x12,
                                'N': 1, 'V': 0,
                                'PCH': 0xff, 'SP': 0x123})
    cpu = c64.CPU(initial_registers=regbank, initial_ram=ram)
    assert cpu.reg.PC == 0xff00
    assert cpu.reg.SP == 0x123
    assert cpu.ram.size == 0xffff
    assert cpu.ram.word_length == 8
    assert cpu.ram.get(0) == 0xff
    assert cpu.ram.get(1) == 0x01
    assert cpu.ram.get(2) == 0x02
    assert cpu.ram.get(3) == 0x03

def test_next_word():
    cpu = c64.CPU(initial_ram=[0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0xff])
    cpu.reg.PC = 0x0002
    assert cpu.next_word() == 0x02
    assert cpu.next_word() == 0x03
    assert cpu.next_word() == 0x04
    assert cpu.next_word() == 0x05
    assert cpu.next_word() == 0xff
    assert cpu.reg.PC == 0x0007
    cpu.reg.PC = 0x0000
    assert cpu.next_word() == 0x00
    assert cpu.next_word() == 0x01
    assert cpu.next_word() == 0x02
    assert cpu.reg.PC == 0x0003

def test_bcd():
    assert c64.bcd(0x43) == 43
    assert c64.bcd(0x12) == 12
    with pytest.raises(ValueError):
        c64.bcd(0xfa)

# TODO: refactor ADC, AND, ASL to use parametrization if appropriate

def test_ADC(cpu):
    cpu.reg.A = 0x02
    cpu.ADC(0x04)
    assert cpu.reg.A == 0x06
    assert cpu.reg.P == 0b00000000

def test_ADC_with_carry_and_sign(cpu):
    cpu.reg.C = 1
    cpu.reg.A = 0xf2
    cpu.ADC(0x04)
    assert cpu.reg.A == 0xf7
    assert cpu.reg.N == 0x01
    assert cpu.reg.P == 0b10000000

def test_ADC_overflow(cpu):
    cpu.reg.A = 0xf2
    cpu.ADC(0x14)
    assert cpu.reg.A == 0x06
    assert cpu.reg.V == 0x01
    assert cpu.reg.C == 0x01
    assert cpu.reg.N == 0x01
    assert cpu.reg.P == 0b11000001

def test_ADC_zero(cpu):
    cpu.ADC(0x00)
    assert cpu.reg.A == 0x00
    assert cpu.reg.Z == 0x01
    assert cpu.reg.P == 0b00000010

def test_AND_with_sign(cpu):
    cpu.reg.A = 0b10111111
    cpu.AND(0b11001111)
    assert cpu.reg.A == 0b10001111
    assert cpu.reg.N == 0x01
    assert cpu.reg.P == 0b10000000

def test_AND_zero(cpu):
    cpu.AND(0x12)
    assert cpu.reg.A == 0x00
    assert cpu.reg.Z == 0x01
    assert cpu.reg.P == 0b00000010

def test_ASL(cpu):
    assert cpu.ASL(0b11111110) == 0b11111100
    assert cpu.reg.C == 1
    assert cpu.reg.N == 1
    assert cpu.reg.P == 0b10000001

def test_ASL_zero(cpu):
    assert cpu.ASL(0b10000000) == 0b00000000
    assert cpu.reg.Z == 1
    assert cpu.reg.C == 1
    assert cpu.reg.P == 0b00000011

@pytest.mark.parametrize("pc, c, value, exp_pc, exp_p", [
    (0x0002, 1, 2, 0x0002, 0b00000001),
    (0x0002, 0, 2, 0x0004, 0b00000000),
    (0x0004, 0, -2, 0x0002, 0b00000000),
])
def test_BCC(cpu, pc, c, value, exp_pc, exp_p):
    cpu.reg.PC = pc
    cpu.reg.C = c
    cpu.BCC(value)
    assert cpu.reg.PC == exp_pc
    assert cpu.reg.P == exp_p

@pytest.mark.parametrize("pc, c, value, exp_pc, exp_p", [
    (0x0002, 0, 2, 0x0002, 0b00000000),
    (0x0002, 1, 2, 0x0004, 0b00000001),
    (0x0004, 1, -2, 0x0002, 0b00000001),
])
def test_BCS(cpu, pc, c, value, exp_pc, exp_p):
    cpu.reg.PC = pc
    cpu.reg.C = c
    cpu.BCS(value)
    assert cpu.reg.PC == exp_pc
    assert cpu.reg.P == exp_p

@pytest.mark.parametrize("pc, z, value, exp_pc, exp_p", [
    (0x0002, 0, 2, 0x0002, 0b00000000),
    (0x0002, 1, 2, 0x0004, 0b00000010),
    (0x0004, 1, -2, 0x0002, 0b00000010),
])
def test_BEQ(cpu, pc, z, value, exp_pc, exp_p):
    cpu.reg.PC = pc
    cpu.reg.Z = z
    cpu.BEQ(value)
    assert cpu.reg.PC == exp_pc
    assert cpu.reg.P == exp_p

@pytest.mark.parametrize("a, value, exp_p", [
    (0b00000000, 0b11111111, 0b00000010),
    (0b11111111, 0b11111111, 0b11000000),
    (0b10000000, 0b11111111, 0b10000000),
    (0b01000000, 0b11111111, 0b01000000),
])
def test_BIT(cpu, a, value, exp_p):
    cpu.reg.A = a
    cpu.BIT(value)
    assert cpu.reg.P == exp_p
    assert cpu.reg.A == a # hasn't changed

@pytest.mark.parametrize("pc, n, value, exp_pc, exp_p", [
    (0x0002, 0, 2, 0x0002, 0b00000000),
    (0x0002, 1, 2, 0x0004, 0b10000000),
    (0x0004, 1, -2, 0x0002, 0b10000000),
])
def test_BMI(cpu, pc, n, value, exp_pc, exp_p):
    cpu.reg.PC = pc
    cpu.reg.N = n
    cpu.BMI(value)
    assert cpu.reg.PC == exp_pc
    assert cpu.reg.P == exp_p

@pytest.mark.parametrize("pc, z, value, exp_pc, exp_p", [
    (0x0002, 1, 2, 0x0002, 0b00000010),
    (0x0002, 0, 2, 0x0004, 0b00000000),
    (0x0004, 0, -2, 0x0002, 0b00000000),
])
def test_BNE(cpu, pc, z, value, exp_pc, exp_p):
    cpu.reg.PC = pc
    cpu.reg.Z = z
    cpu.BNE(value)
    assert cpu.reg.PC == exp_pc
    assert cpu.reg.P == exp_p

@pytest.mark.parametrize("pc, n, value, exp_pc, exp_p", [
    (0x0002, 1, 2, 0x0002, 0b10000000),
    (0x0002, 0, 2, 0x0004, 0b00000000),
    (0x0004, 0, -2, 0x0002, 0b00000000),
])
def test_BPL(cpu, pc, n, value, exp_pc, exp_p):
    cpu.reg.PC = pc
    cpu.reg.N = n
    cpu.BPL(value)
    assert cpu.reg.PC == exp_pc
    assert cpu.reg.P == exp_p

# BRK NYI

@pytest.mark.parametrize("pc, v, value, exp_pc, exp_p", [
    (0x0002, 1, 2, 0x0002, 0b01000000),
    (0x0002, 0, 2, 0x0004, 0b00000000),
    (0x0004, 0, -2, 0x0002, 0b00000000),
])
def test_BVC(cpu, pc, v, value, exp_pc, exp_p):
    cpu.reg.PC = pc
    cpu.reg.V = v
    cpu.BVC(value)
    assert cpu.reg.PC == exp_pc
    assert cpu.reg.P == exp_p

@pytest.mark.parametrize("pc, v, value, exp_pc, exp_p", [
    (0x0002, 0, 2, 0x0002, 0b00000000),
    (0x0002, 1, 2, 0x0004, 0b01000000),
    (0x0004, 1, -2, 0x0002, 0b01000000),
])
def test_BVS(cpu, pc, v, value, exp_pc, exp_p):
    cpu.reg.PC = pc
    cpu.reg.V = v
    cpu.BVS(value)
    assert cpu.reg.PC == exp_pc
    assert cpu.reg.P == exp_p

@pytest.mark.parametrize("flag", ['C', 'D', 'I', 'V'])
def test_clear_commands(cpu, flag):
    getattr(cpu, "CL" + flag)()
    assert cpu.reg.P == 0b00000000
    setattr(cpu.reg, flag, 1)
    getattr(cpu, "CL" + flag)()
    assert cpu.reg.P == 0b00000000

@pytest.mark.parametrize("command, target", [
    ('CMP', 'A'),
    ('CPY', 'Y'),
    ('CPX', 'X'),
])
@pytest.mark.parametrize("state, value, exp_p", [
    (0x00, 0x00, 0b00000011),
    (0x40, 0x20, 0b00000001),
    (0x20, 0x40, 0b10000000),
    (0xff, 0xff, 0b00000011),
])
def test_compare_commands(cpu, command, target, state, value, exp_p):
    setattr(cpu.reg, target, state)
    getattr(cpu, command)(value)
    assert cpu.reg.P == exp_p

@pytest.mark.parametrize("command, value, exp_p", [
    ('DEC', 0x01, 0b00000010),
    ('DEC', 0xfb, 0b10000000),
    ('DEC', 0x02, 0b00000000),
    ('DEC', 0x00, 0b10000000),
    ('INC', 0xff, 0b00000010),
    ('INC', 0xfb, 0b10000000),
    ('INC', 0x02, 0b00000000),
])
def test_DEC_INC(cpu, command, value, exp_p):
    expected_value = (value + (1 if command == "INC" else -1)) & 0xff
    assert getattr(cpu, command)(value) == expected_value
    assert cpu.reg.P == exp_p

@pytest.mark.parametrize("command, value, exp_p", [
    ('DEX', 0x02, 0b00000000),
    ('DEY', 0x02, 0b00000000),
    ('DEX', 0x01, 0b00000010),
    ('DEY', 0x01, 0b00000010),
    ('DEX', 0xfb, 0b10000000),
    ('DEY', 0xfb, 0b10000000),
    ('DEX', 0x00, 0b10000000),
    ('DEY', 0x00, 0b10000000),
    ('INX', 0x02, 0b00000000),
    ('INY', 0x02, 0b00000000),
    ('INX', 0xff, 0b00000010),
    ('INY', 0xff, 0b00000010),
    ('INX', 0xfb, 0b10000000),
    ('INY', 0xfb, 0b10000000),
])
def test_DEX_DEY_INX_INY(cpu, command, value, exp_p):
    setattr(cpu.reg, command[2], value)
    getattr(cpu, command)()
    assert getattr(cpu.reg, command[2]) == (value + (1 if command.startswith("IN") else -1)) & 0xff
    assert cpu.reg.P == exp_p

def test_EOR(cpu):
    raise Exception("You still need to write this one")
