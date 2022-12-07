lib ← •Import "../../lib.bqn"
cmds ← 1↓ '$' ((+`= ⟜(⊑¨))⊔⊢) •FLines "input"
paths ← (<⟨⟩) {
  𝕨 𝕊 "$ ls": 𝕨;
  𝕨 𝕊 "$ cd /": ⟨⟩;
  𝕨 𝕊 "$ cd ..": (-1)↓𝕨;
  𝕨 𝕊 𝕩: 𝕨∾<5↓𝕩 # "$ cd …"
}` ⊑¨cmds
ParseLs ← {
  dirs‿files ← 2↑((lib.IsAsciiNum∘⊑∘⊑)¨⊔⊢) ((<' ')⊸lib.SplitOn)¨ 1↓𝕩
  (1⊑¨dirs)⋈(lib.ReadDec 0⊸⊑)¨files
}
dirlists ← ParseLs⌾(1⊸⊑)¨⥊⋈˘(("$ cd"⊸≢⟜(4⊸↑)∘⊑¨)∘(1⊸⊏)˘/⊢) (⍒≠¨paths)⊏⍉paths≍cmds
DirSize ← {⊑𝕨 (⊑∘(1⊸⊑¨∘⊣⊐⊢)⊑⊣) <𝕩}
DirName ← ∾'/'⊸∾¨
dirsizes ← ⊑¨ ⟨⟩ {
  szs 𝕊 ⟨dir, subdirs‿files⟩:
  Canon ← DirName dir⊸∾⟜⋈
  sz ← +´files∾szs⊸DirSize∘Canon¨ subdirs
  szs∾<sz⋈DirName dir
}˜´ ⌽dirlists

•Out "day07.1: "∾•Fmt +´ 100000 (≥/⊢) dirsizes
•Out "day07.2: "∾•Fmt (30000000-70000000-⌈´dirsizes) ⌊´∘(≤/⊢) dirsizes
