⟨SplitOn,ReadDec⟩ ← •Import "../../lib.bqn"
i ← ReadDec⌾(1⊸⊑)¨ (<' ')⊸SplitOn¨ •FLines "input"

UnitDelta ← (⊢÷(|+0⊸=))
ExpandStep ← {
  𝕊 "L"‿l: 𝕊 (-l)‿0;
  𝕊 "R"‿r: 𝕊 r‿0;
  𝕊 "U"‿u: 𝕊 0‿u;
  𝕊 "D"‿d: 𝕊 0‿(-d);
  𝕊 delta: ((⌈´|)⥊<∘UnitDelta) delta
}

Step ← {knots 𝕊 delta: {h 𝕊 t: (UnitDelta h-t) +⍟(1<⌈´|h-t) t}` (delta⊸+)⌾⊑ knots}
Visited ← {+´0=⊒(¯1⊸⊑)¨(<𝕨⥊<0‿0) Step` ∾ExpandStep¨ 𝕩}

•Out "day09.1: "∾•Fmt  2 Visited i
•Out "day09.2: "∾•Fmt 10 Visited i
