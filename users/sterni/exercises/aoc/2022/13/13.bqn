lib ← •Import "../../lib.bqn"
str ← lib.ImportBqnLibs "strings.bqn"
i ← >⟨"[","]"⟩‿⟨"⟨","⟩"⟩⊸(•BQN str.ReplaceAll)¨¨0((⟨⟩⊸≡¨¯1˙⍟⊣¨(+`(=⟜≠)¨))⊔⊢)•FLines "input"

Ord ← {
  i1 𝕊 i2: 1‿1≡•Type¨ i1‿i2? ¯1‿1‿0⊑˜i1(=+≤)i2;
  i1 𝕊 l2: 1‿0≡•Type¨ i1‿l2? l2 Ord˜ ⋈i1;
  l1 𝕊 i2: 0‿1≡•Type¨ l1‿i2? l1 Ord ⋈i2;
  l1 𝕊 l2: 0‿0≡•Type¨ l1‿l2?
  ⊑1↑0(≠/⊢)l1 Ord¨○((l1⌈○≠l2)⊸(↑⌾(+⟜1))) l2
}

•Out "day13.1: "∾•Fmt +´1+/(1⊸=Ord´)˘i
•Out "day13.2: "∾•Fmt ×´1‿2++´˘¯1=⟨⟨2⟩⟩‿⟨⟨6⟩⟩Ord⌜⥊i
