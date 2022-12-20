jets ← '>'= "<>" (∊˜/⊢) •FChars "input"
pieces ← >¨⟨1‿1‿1‿1⟩‿⟨0‿1‿0,1‿1‿1,0‿1‿0⟩‿⟨0‿0‿1,0‿0‿1,1‿1‿1⟩‿⟨⋈1,⋈1,⋈1,⋈1⟩‿⟨1‿1,1‿1⟩
w ← 7
initial ← 0‿w⥊0

# Warning: mutated global!
ji ← 0
_try ← {(⊢ 𝕩˙⍟(≠○(+´∘⥊∘∨⟜𝕨)) 𝔽) 𝕩}
Fall ← {
  pushed ← 𝕨 ((ji⊑jets)◶«‿»)˘ _try 𝕩
  ji ↩ (≠jets)|ji+1
  fallen ← 𝕨 » _try pushed
  𝕨 𝕊⍟(pushed≢fallen) fallen
}
Height ← ≠∘(∨´˘/⊢)
ThrowPiece ← {
  piece ← 𝕩 (|˜⟜≠⊑⊢) pieces
  chamber ← (((3+≠piece)⊸+∘⊑∘(1⊸↑)∘⌽∘(1⊸+)∘/∨´˘)↑⊢)⌾⌽𝕨
  falling ← (≠chamber)↑(»⍟2 w⊸↑)˘piece
  chamber (⊣∨Fall) falling
}

•Out "day17.1: "∾•Fmt Height initial ThrowPiece˜´ ⌽↕2022

# https://mlochbaum.github.io/BQN/doc/control.html#while
While ← {𝕩{𝔽⍟𝔾∘𝔽_𝕣_𝔾∘𝔽⍟𝔾𝕩}𝕨@}´
{
  target ← 1000000000000
  ji ↩ 0 ⋄ i ← 0 ⋄ res ← @

  chamber ← initial
  cycles ← ⟨≠pieces,≠jets⟩⥊<⟨⟩

  While {𝕤⋄res=@}‿{𝕤
    chamber ↩ chamber ThrowPiece i
    i +↩ 1

    t ← i|˜≠pieces
    cycles ↩ {
      new ← 𝕩∾<i⋈Height chamber
      res ↩ {𝕊 𝕩:
        ⟨pl,hl⟩‿· ← chk ← ¯2↑new
        pd‿hd ← -´⌽chk
        @˙⍟(0≠pd|target-pl) hl+hd×pd÷˜target-pl
      }⍟(1<≠new) @
      new
    }⌾(t‿ji⊸⊑) cycles
  }

  •Out "day17.2: "∾•Fmt res
}
