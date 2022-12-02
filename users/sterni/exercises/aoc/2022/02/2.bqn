lib ← •Import •path∾"/../../lib.bqn"
i ← 3|"ABCXYZ"⊸⊐¨ ' ' ⊑¨∘lib.SplitOn¨ •FLines •path∾"/input"
S1 ← {1+𝕩+3×3|1+𝕩-𝕨}
S2 ← {𝕨 S1 3|𝕨+𝕩-1}

•Out "day02.1: "∾•Fmt +´S1´¨i
•Out "day02.2: "∾•Fmt +´S2´¨i
