⟨ReadDec, SplitOn, IsAsciiNum⟩ ← •Import "../../lib.bqn"
rs‿rc ← (<"") SplitOn •FLines "../05/input"

stacks ← {
  count ← '0'-˜⊑⌽' ' (≠/⊢) ⊑⌽rs
  spec ← 0<(-1)↓(-1)⌽∾(∾⟜0‿0‿0)¨1+↕count
  ' ' (≠/⊢)¨⥊⋈˘⍉>spec⊸/¨(-1)↓rs
}

cmds ← {0‿1‿1-˜ ReadDec¨ ((∧´IsAsciiNum)¨/⊢) (<' ') SplitOn 𝕩}¨ rc

ModestTake ← ↑⍟(<⟜≠)
_ApplyCmd ← {
  c‿f‿t ← 𝕩
  m ← 𝔽 c ModestTake f⊑𝕨
  (m⊸∾)⌾(t⊸⊑) (c⊸↓)⌾(f⊸⊑)𝕨
}

•Out "day05.1: "∾⊑¨stacks ⌽_ApplyCmd˜´ ⌽ cmds
•Out "day05.2: "∾⊑¨stacks ⊢_ApplyCmd˜´ ⌽ cmds
