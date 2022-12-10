⟨ReadDec, SplitOn, IsAsciiNum⟩ ← •Import "../../lib.bqn"
rs‿rc ← (<"") SplitOn •FLines "../05/input"

stacks ← {
  count ← '0'-˜⊑⌽' ' (≠/⊢) ⊑⌽rs
  ' ' (≠/⊢)¨<˘ (count×4) ((»∘(0⊸=)∘(4⊸|)∘↕⊣)/↑) ⍉> (-1)↓rs
}

cmds ← {0‿1‿1-˜ ReadDec¨ ((∧´IsAsciiNum)¨/⊢) (<' ') SplitOn 𝕩}¨ rc

_ApplyCmd ← {
  s Fn _self c‿f‿t :
  m‿k ← 2↑ c ((≤⟜(↕≠))⊔⊢) f⊑s
  (Fn m)⊸∾⌾(t⊸⊑) k˙⌾(f⊸⊑) s
}

•Out "day05.1: "∾⊑¨stacks ⌽_ApplyCmd˜´ ⌽ cmds
•Out "day05.2: "∾⊑¨stacks ⊢_ApplyCmd˜´ ⌽ cmds
