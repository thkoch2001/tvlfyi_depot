⟨ImportBqnLibs,_fix⟩ ← •Import "../../lib.bqn"
⟨ReplaceAll⟩ ← ImportBqnLibs "strings.bqn"
i ← >•FLines "input"

elevation ← 'a'-˜⟨"S","E"⟩‿⟨"a","z"⟩ ReplaceAll⌾⥊ i
starts ← (⊏⟜∞‿0)¨⟨'S'=i,0=elevation⟩
end ← 'E'=i

Step ← {
  𝕊 steps:
  Go ← {𝕏⁼((⊢∾¨↕∘≢)(≤⟜(∞⊸»˘∘+⟜1))˜𝕏elevation)⊑>((⥊⟜∞)∘≢⊸⋈)˜∞⊸»˘1+𝕏steps}
  steps⌊´Go¨⟨⊢,⌽˘,⍉,⍉⌽⟩
}
Shortest ← {⊑end/⊸⊏○⥊Step _fix 𝕩}

•Out¨ "day12.1: "‿"day12.2: "∾¨ •Fmt∘Shortest¨ starts
