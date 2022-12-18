lib ← •Import "../../lib.bqn"

i ← (lib.ReadDec¨(<',')⊸lib.SplitOn)¨ •FLines "input"
dim ← 1+⌈´i
cubes ← i∊˜↕dim

views ← ⟨0‿1‿2, 1‿2‿0, 2‿0‿1⟩
Exposed ← {(6×+´⥊𝕩)-2×+´views{+´⥊(∧˝˘)2↕𝕨⍉𝕩}¨<𝕩}
Interior ← {(¬𝕩)∧´views{((lib.Xor`∘((∊∧⊢)∨»∘(∊⌾⌽∧⊢)))⎉1)⌾(𝕨⊸⍉)𝕩}¨<𝕩}
Displace ← {⌈´(⥊⊢‿⌽⋈⌜views){F‿a 𝕊 𝕩:((-∘¬∘(»((0⊸=⊣)∧>)⊢)⌈⊢)⎉1)⌾(F a⊸⍉)𝕩}¨<𝕩}
Exterior ← (⊢-○Exposed ¯1⊸=∘(Displace lib._fix)∘(-∘Interior+⊢))

•Out "day18.1: "∾•Fmt Exposed cubes
•Out "day18.2: "∾•Fmt Exterior cubes
