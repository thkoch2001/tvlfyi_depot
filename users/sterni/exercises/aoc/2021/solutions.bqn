#!/usr/bin/env BQN

#
# Utilities
#

IsAsciiNum ← ('0'⊸≤∧≤⟜'9')

ReadInt ← (10⊸×+⊣)´∘⌽-⟜'0' # stolen from leah2

ReadInput ← {•file.Lines ∾ •path‿"/input/day"‿(•Fmt 𝕩)}

#
# 2021-12-01
#

# part 1

day1ExampleData ← 199‿200‿208‿210‿200‿207‿240‿269‿260‿263
day1Input ← ReadInt¨ReadInput 1

# NB: Because distance from the ground is never smaller than zero, it's
# no problem that nudge inserts a zero at the end of the right list
PositiveDeltaCount ← +´∘(⊢<«)+˝˘∘↕

! 7 = 1 PositiveDeltaCount day1ExampleData

•Out "Day 1.1: "∾•Fmt 1 PositiveDeltaCount day1Input

# part 2

! 5 = 3 PositiveDeltaCount day1ExampleData

•Out "Day 1.2: "∾•Fmt 3 PositiveDeltaCount day1Input

#
# 2021-12-02
#

# part 1

day2ExampleData ← ⟨
  "forward 5",
  "down 5",
  "forward 8",
  "up 3",
  "down 8",
  "forward 2",
⟩

day2Input ← ReadInput 2

ParseSubmarineCommand ← (((↕2)⊸((((-1)⊸⋆)∘(2⊸|))×(=⟜(⌊∘(÷⟜2))))∘("duf"⊸⊐)∘⊑)×ReadInt∘(IsAsciiNum/⊢))

SubmarineDestProduct ← {×´+´ParseSubmarineCommand¨𝕩}

! 150 = SubmarineDestProduct day2ExampleData

•Out "Day 2.1: "∾•Fmt SubmarineDestProduct day2Input

# part 2

SubmarineAimedDestProduct ← {
  ×´+´((×´)∘(1⊸↓)≍(1⊸⊑))¨ (<0‿0‿0) (⊢∾((⊑∘⌽⊣)+(⊑⊢)))` ParseSubmarineCommand¨𝕩
}

! 900 = SubmarineAimedDestProduct day2ExampleData

•Out "Day 2.2: "∾•Fmt SubmarineAimedDestProduct day2Input
