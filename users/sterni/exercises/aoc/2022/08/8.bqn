i ← >'0'-˜•FLines "input"
Visible ← {
  _vis ← {(⌈`∘(¯1⊸»˘⌾⍉)<⊢)⌾𝕏 𝕗}
  ∨´𝕩 _vis¨ ⟨⊢,⌽,⍉,⌽⍉⟩
}

•Out "day08.1: "∾•Fmt +´⥊Visible i

ViewingDistances ← {
  DirView ← {≠1(»⟜(∧`(⊑𝕩)⊸>)/⊢) 1↓𝕩}
  _spliceDir ← {! =´≢𝕗 ⋄ 𝕏⁼(⊢↓(⊏⟜(𝕏𝕗))∘⊣)´¨ ⋈⌜˜↕≠𝕗}
  ×´ DirView¨¨ 𝕩 _spliceDir¨ ⟨⊢, ⌽˘, ⍉, ⌽˘⍉⟩
}

•Out "day08.2: "∾•Fmt ⌈´⥊ViewingDistances i
