⟨ReadDec⟩ ← •Import "../../lib.bqn"
enc ← ReadDec¨ •FLines "input"

CoordSum ← +´∘(1000‿2000‿3000⊸((⊢≠⊸|+⟜(⊑∘(/=⟜0)∘⊢))⊏⊢))
Mix ← {
  M ← {m 𝕊 i:
    l ← ≠m
    i {n ← (l-1)|(𝕩⊑m)+⊑/𝕩=𝕨 ⋄ (n⊸↑(∾⟜𝕩)⊸∾n⊸↓) 𝕩(≠/⊢)𝕨}˜´ ⌽↕l
  }
  CoordSum ((⊢M⍟𝕨↕∘≠)⊏⊢) 𝕩
}
•Out "day20.1: "∾•Fmt 1 Mix enc
•Out "day20.2: "∾•Fmt 10 Mix 811589153×enc
