lib ← •Import "../../lib.bqn"

F ← ¬∘('-'⊸=∨lib.IsAsciiNum)
i ← ⌽˘˘∘‿2‿2⥊lib.ReadDec¨>(0⊸<⟜≠¨/⊢)∘((F ¯1˙⍟⊣¨(+`F))⊔⊢)¨ •FLines "input"

ssp ← 4000000

sds ← (⊏˘∾˘(+´˘(|(-˝))˘)) i

# _fix is needed to deal with e.g. ⟨0‿15, 5‿8, 12‿23⟩
MergeRanges ← ((⊑∾⊑∘⌽)∘∧∘∾)¨∘(+`∘((<∞‿∞)⊸»{<´1‿2⊏𝕨∾𝕩}¨⊢)⊔⊢) lib._fix

Range ← {cky 𝕊 y‿x‿d: x+¯1‿1×d-|cky-y}
RangesY ← {<˘∧𝕩(⊣Range˘({cky 𝕊 y‿·‿d: d≥|y-cky}˘/⊢))sds}
OutRangeY ← {(1<≠)◶⟨0˙,𝕩⊸+∘(ssp⊸×⟜(+⟜1))∘(1⊸⊑)∘∾⟩ MergeRanges ssp⌊0⌈RangesY 𝕩}

•Out "day15.1: "∾•Fmt +´-˜´¨MergeRanges RangesY 2÷˜ssp
•Out "day15.2: "∾•Fmt +´OutRangeY¨↕ssp
