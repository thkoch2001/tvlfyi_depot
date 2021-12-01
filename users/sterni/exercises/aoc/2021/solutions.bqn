#
# Utilities
#

ReadInput ← {
  # TODO(sterni): parse numbers w/o arbitrary code execution :)
  •BQN¨•file.Lines ∾ •path‿"/input/day"‿(•Fmt 𝕩)
}

#
# 2021-12-01
#

# part 1

exampleData ← 199‿200‿208‿210‿200‿207‿240‿269‿260‿263

# 0..n → 1..n - 0..n
ListDeltas ← 1⊸(↓-(-⊸↓))
PositiveDeltaCount ← +´∘(0⊸<)∘ListDeltas

! 7 = PositiveDeltaCount exampleData

•Out "Day 1.1: "∾•Fmt PositiveDeltaCount ReadInput 1

# part 2

# sum the deltas in a moving window
PositiveWindowDeltaCount ← +´∘(0⊸<)∘(+˝˘)∘(↕⟜ListDeltas)

! 5 = 3 PositiveWindowDeltaCount exampleData

•Out "Day 1.2: "∾•Fmt 3 PositiveWindowDeltaCount ReadInput 1
