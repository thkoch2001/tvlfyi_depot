#!/usr/bin/env BQN

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

day1ExampleData ← 199‿200‿208‿210‿200‿207‿240‿269‿260‿263

ListDeltas ← 1⊸(↓-(-⊸↓))∘(+˝˘)∘↕
PositiveDeltaCount ← +´∘(0⊸<)∘ListDeltas

! 7 = 1 PositiveDeltaCount day1ExampleData

•Out "Day 1.1: "∾•Fmt 1 PositiveDeltaCount ReadInput 1

# part 2

! 5 = 3 PositiveDeltaCount day1ExampleData

•Out "Day 1.2: "∾•Fmt 3 PositiveDeltaCount ReadInput 1
