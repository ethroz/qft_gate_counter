from fractions import Fraction
from dataclasses import dataclass

@dataclass(frozen=True)
class Gate():
    symbol:str
    wires:tuple[int]
    exp:Fraction
    controls:tuple[int]
    
    @classmethod
    def create(self, symbol:str, wires:int|tuple[int] = (), exp:Fraction = None, controls:int|tuple[int] = ()):
        wires = wires if isinstance(wires, tuple) else (wires,)
        exp = exp if exp else Fraction(1, 1)
        controls = controls if isinstance(controls, tuple) else (controls,)
        return Gate(symbol=symbol, wires=wires, exp=exp, controls=controls)

    def is_controlled(self)->bool:
        return len(self.controls) > 0

    def has_exp(self)->bool:
        return self.exp != 1

    def is_z_rotation(self)->bool:
        return self.symbol in ['Z', 'S', 'T']

    def is_x_rotation(self)->bool:
        return self.symbol == 'X'

    def is_h(self)->bool:
        return self.symbol == 'H' and not self.is_controlled() and not self.has_exp()

    def is_s(self)->bool:
        return self.symbol == 'S' and not self.is_controlled() and not self.has_exp()

    def is_t(self)->bool:
        return self.symbol == 'T' and not self.is_controlled() and not self.has_exp()

    def is_cnot(self)->bool:
        return self.symbol == 'X' and len(self.controls) == 1 and not self.has_exp()
    
    def is_h_fast(self)->bool:
        return self.symbol == 'H'

    def is_s_fast(self)->bool:
        return self.symbol == 'S'

    def is_t_fast(self)->bool:
        return self.symbol == 'T'

    def is_cnot_fast(self)->bool:
        return self.symbol == 'X'

    def is_clifford_t(self)->bool:
        return not self.has_exp() and ((not self.is_controlled() and (self.is_h_fast() or self.is_s_fast() or self.is_t_fast())) or (self.is_cnot_fast() and len(self.controls) == 1))

    def is_toffoli(self)->bool:
        return self.symbol == 'X' and len(self.controls) == 2 and not self.has_exp()

    def __str__(self)->str:
        res = ''
        for control in self.controls:
            res += f'C[{control}]'
        res += self.symbol
        if self.wires:
            res += '[' + ','.join(map(str, self.wires)) + ']'
        if self.has_exp():
            res += f'^{self.exp}'
        return res

    def __repr__(self)->str:
        return str(self)