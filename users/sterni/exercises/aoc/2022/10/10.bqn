⟨SplitOn,ReadDec⟩ ← •Import "../../lib.bqn"
# Instead of implementing the VM described in the problem, translate the
# program to instructions with equivalent timing for a similar VM that
# only needs 1 cycle for every instruction.
is ← ∾{"noop": <"noop"; 𝕩: (<"noop")∾<ReadDec⌾(1⊸⊑) (<' ') SplitOn 𝕩}¨ •FLines "input"

Op ← {x 𝕊 "noop": x;x 𝕊 "addx"‿i: x+i}
Draw ← {𝕊 c‿x‿pic: pic∨(↕240)((c-1)⊸=∘⊣∧∊)(⌊⌾(÷⟜40)c)+¯1+x+↕3}
_vm ← {
  is _self s: (⊑s)≥≠is? s;
  is _self prev‿sum‿x‿pic:
  cycle ← prev+1
  is _self ⟨
    cycle,
    sum+x×cycle×⊑cycle∊20‿60‿100‿140‿180‿220,
    x Op (¯1+cycle)⊑is,
    Draw cycle‿x‿pic
  ⟩
}

·‿sum‿·‿pic ← is _vm 1‿0‿1‿(240⥊0)

•Out "day10.1: "∾•Fmt sum
•Out "day10.2:"
•Show ".#" ⊏˜ ∘‿40⥊pic
