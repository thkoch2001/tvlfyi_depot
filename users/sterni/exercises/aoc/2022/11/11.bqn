# needs export BQNLIBS=/path/to/mlochbaum/bqn-libs
⟨ReadDec,ImportBqnLibs⟩ ← •Import "../../lib.bqn"
⟨Split⟩ ← ImportBqnLibs "strings.bqn"
MakeOp ← {
  𝕊 a‿"+"‿b: 𝕊 a‿+‿b;
  𝕊 a‿"*"‿b: 𝕊 a‿×‿b;
  𝕊 a‿op‿b:
  is‿xs ← (<"old") (≡¨⊔⊢) a‿b
  {op´ (𝕩⋆≠xs) ∾ReadDec¨ is}
}
ParseMonkey ← {
  ·‿items‿op‿if‿then‿else:
  {
    initial ⇐ ReadDec¨ ", " Split 18↓items
    op ⇐ MakeOp " " Split 19↓op
    if ⇐ ReadDec 21↓if
    then ⇐ ReadDec 29↓then
    else ⇐ ReadDec 30↓else
  }
}
monkeys ← ParseMonkey¨ 1↓' '((+`(≠⟜⊑)¨)⊔⊢)0(≠⟜≠¨/⊢)•FLines "input"
items ← {𝕩.initial}¨ monkeys
lim ← ×´{𝕩.if}¨ monkeys

Sim ← {
  div 𝕊 len:
  Turn ← {
    items 𝕊 turnidx:
    i ← (≠monkeys)|turnidx
    m ← i⊑monkeys

    worry ← lim|⌊div÷˜ m.Op¨ i⊑items
    else‿then ← 2↑0 (=⟜(m.if⊸|)⊔⊢) worry

    ⟨then, else⟩⊸(∾˜¨)⌾(m.then‿m.else⊸⊏) ⟨⟩˙⌾(i⊸⊑) items
  }
  ×´2↑∨+˝(<items) ((≠⊑)⊸(>((↕⊣)=|)¨)×(≠¨˘)∘>∘(⊣»Turn`)) ↕len×≠items
}

•Out "day11.1: "∾•Fmt 3 Sim 20
•Out "day11.2: "∾•Fmt 1 Sim 10000
