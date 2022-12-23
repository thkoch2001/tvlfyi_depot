⟨ImportBqnLibs, IsAsciiNum, ReadDec⟩ ← •Import "../../lib.bqn"
⟨ReplaceAll, Split⟩ ← ImportBqnLibs "strings.bqn"

i ← ": "⊸Split¨ •FLines "input"
ReplaceInts ← {
  𝕊 𝕩: 𝕊´ 2↑(¬∘(∧´IsAsciiNum∘⊑∘⌽)¨⊔⊢) 𝕩;
  # TODO: Efficient replace on tokens
  is 𝕊 es: (((•Fmt⍟(0⊸≠•Type))¨⌾(1⊸⊑) <˘⍉>is)⊸ReplaceAll⌾(1⊸⊑))¨ es
}

c ← 0
CanEval ← (IsAsciiNum∨∊⟜"+-/* ")
Eval ← {
  a‿s‿b ← " " Split 𝕩
  f ← ⊑+‿-‿×‿÷⊏˜"+-*/"⊐s
  a F○ReadDec b
}
EvalExprs ← {
  p‿e ← 2↑((∧´CanEval∘⊑∘⌽)¨⊔⊢) 𝕩
  ev ← (Eval⌾(⊑⌽))¨ e
  c +↩1
  (⊑(⊑¨ev)∊˜<"root")◶⟨EvalExprs∘(ReplaceInts⟜p),1⊸⊑⊑⟩ ev
}

•Show EvalExprs ReplaceInts i
