⟨SplitOn, ReadDec⟩ ← •Import "../../lib.bqn"

Sections ← {
  a‿b ← ReadDec¨ (<'-') SplitOn 𝕩
  ↕⌾(-⟜a) 1+b
}
i ← ∘‿2⥊Sections¨ ∾(<',') SplitOn¨ •FLines "input"
Is ← ∊´∘((⍋≠¨)⊏⊢)

•Out "day04.1: "∾•Fmt +´(∧´Is)˘ i
•Out "day04.2: "∾•Fmt +´(∨´Is)˘ i
