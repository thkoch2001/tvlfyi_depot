#!/usr/bin/env BQN

#
# Utilities
#

IsAsciiNum ← ('0'⊸≤∧≤⟜'9')

ReadInt ← {(𝕨⊸×+⊣)´∘⌽-⟜'0'𝕩} # stolen from leah2
ReadDec ← 10⊸ReadInt

ReadInput ← {•file.Lines ∾ •path‿"/input/day"‿(•Fmt 𝕩)}

SplitOn ← ((⊢ (-1˙)⍟⊣¨ +`)∘=⊔⊢)

_fix ← {𝕩 𝕊∘⊢⍟≢ 𝔽 𝕩}

#
# 2021-12-01
#

# part 1

day1ExampleInput ← 199‿200‿208‿210‿200‿207‿240‿269‿260‿263
day1Input ← ReadDec¨ReadInput 1

# NB: Because distance from the ground is never smaller than zero, it's
# no problem that nudge inserts a zero at the end of the right list
PositiveDeltaCount ← +´∘(⊢<«)+˝˘∘↕

! 7 = 1 PositiveDeltaCount day1ExampleInput

•Out "Day 1.1: "∾•Fmt 1 PositiveDeltaCount day1Input

# part 2

! 5 = 3 PositiveDeltaCount day1ExampleInput

•Out "Day 1.2: "∾•Fmt 3 PositiveDeltaCount day1Input

#
# 2021-12-02
#

# part 1

day2ExampleInput ← ⟨
  "forward 5",
  "down 5",
  "forward 8",
  "up 3",
  "down 8",
  "forward 2",
⟩

day2Input ← ReadInput 2

ParseSubmarineCommand ← (((↕2)⊸((((-1)⊸⋆)∘(2⊸|))×(=⟜(⌊∘(÷⟜2))))∘("duf"⊸⊐)∘⊑)×ReadDec∘(IsAsciiNum/⊢))

SubmarineDestProduct ← {×´+´ParseSubmarineCommand¨𝕩}

! 150 = SubmarineDestProduct day2ExampleInput

•Out "Day 2.1: "∾•Fmt SubmarineDestProduct day2Input

# part 2

SubmarineAimedDestProduct ← {
  ×´+´((×´)∘(1⊸↓)≍(1⊸⊑))¨ (<0‿0‿0) (⊢∾((⊑∘⌽⊣)+(⊑⊢)))` ParseSubmarineCommand¨𝕩
}

! 900 = SubmarineAimedDestProduct day2ExampleInput

•Out "Day 2.2: "∾•Fmt SubmarineAimedDestProduct day2Input

#
# 2021-12-03
#

BinTable ← '0'-˜>

day3ExampleInput ← BinTable ⟨
  "00100",
  "11110",
  "10110",
  "10111",
  "10101",
  "01111",
  "00111",
  "11100",
  "10000",
  "11001",
  "00010",
  "01010",
⟩

day3Input ← BinTable ReadInput 3

DeBinList ← ((2⊸×)+⊣)´⌽
_tableAggr ← {((÷⟜2)∘(/⟜⥊)´∘⌽∘≢𝔽(+˝))𝕩}
GammaRate ← < _tableAggr

! 22 = DeBinList GammaRate day3ExampleInput
! 9  = DeBinList ¬GammaRate day3ExampleInput

•Out "Day 3.1: "∾•Fmt (¬×○DeBinList⊢) GammaRate day3Input

_lifeSupportRating ← {
  # Need to rename the arguments, otherwise the ternary expr becomes a function
  bitPos ← 𝕨
  Cmp ← 𝔽

  crit ← Cmp _tableAggr 𝕩
  matchPos ← bitPos ⊑˘ crit ((⥊˜⟜≢)=⊢) 𝕩
  match ← matchPos/𝕩
  {1=≠match?⊏match;(bitPos+1) Cmp _lifeSupportRating match}
}

OxygenGeneratorRating ← DeBinList 0 ≤_lifeSupportRating ⊢
CO2ScrubberRating ← DebinList 0 >_lifeSupportRating ⊢

! 23 = OxygenGeneratorRating day3ExampleInput
! 10 = CO2ScrubberRating day3ExampleInput

•Out "Day 3.2: "∾•Fmt (OxygenGeneratorRating×CO2ScrubberRating) day3Input

#
# 2021-12-07
#

# part 1

day6ExampleInput ← ⟨16,1,2,0,4,2,7,1,2,14⟩
day6Input ← ReadDec¨ ','  SplitOn ⊑ReadInput 6

PossiblePositions ← (⌊´+⟜(↕1⊸+)⌈´)
FuelConsumption ← +˝∘|∘(-⌜)
_lowestFuelPossible ← {⌊´∘(𝔽⟜PossiblePositions)˜ 𝕩}

! 37 = FuelConsumption _lowestFuelPossible day6ExampleInput

•Out "Day 7.1: "∾•Fmt FuelConsumption _lowestFuelPossible day6Input

# part 2

TriNum ← 1⊸+×÷⟜2

FuelConsumption2 ← +˝∘(TriNum¨)∘|∘(-⌜)

! 168 = FuelConsumption2 _lowestFuelPossible day6ExampleInput

•Out "Day 7.2: "∾•Fmt FuelConsumption2 _lowestFuelPossible day6Input

#
# 2021-12-09
#

# part 1

ParseHeightMap ← ((≠≍(≠⊑))⥊∾)∘-⟜'0'

day9ExampleInput ← ParseHeightMap ⟨
  "2199943210",
  "3987894921",
  "9856789892",
  "8767896789",
  "9899965678"
⟩
day9Input ← ParseHeightMap ReadInput 9

Rotate ← (⍉⌽)∘⊢⍟⊣ # counter clockwise
LowPoints ← {∧´𝕩⊸(⊣<((-⊢) Rotate ∞⊸»˘∘Rotate˜))¨ ↕4}

RiskLevelSum ← (+´⥊)∘(1⊸+×LowPoints)

! 15 = RiskLevelSum day9ExampleInput

•Out "Day 9.1: "∾•Fmt RiskLevelSum day9Input

# part 2

NumberBasins ← ((1⊸+⊒⌾⥊)×⊢)∘LowPoints
Basins ← {𝕩⊸((<⟜9⊣)∧(«⌈»⌈«˘⌈»˘⌈⊢)∘⊢) _fix NumberBasins 𝕩}
LargestBasinsProduct ← {×´ 3↑ ∨ 1↓ ≠¨ ⊔⥊Basins 𝕩}

! 1134 = LargestBasinsProduct day9ExampleInput

•Out "Day 9.2: "∾•Fmt LargestBasinsProduct day9Input

#
# 2021-12-13
#

SplitFoldingInstructions ← ("fold along"⊸(⊣≡≠⊸↑)¨⊔⊢)∘(0⊸(≠⟜≠¨/⊢))
day13ExampleInput ← SplitFoldingInstructions ⟨
  "6,10",
  "0,14",
  "9,10",
  "0,3",
  "10,4",
  "4,11",
  "6,0",
  "6,12",
  "4,1",
  "0,13",
  "10,12",
  "3,4",
  "3,0",
  "8,4",
  "1,10",
  "2,14",
  "8,10",
  "9,0",
  "",
  "fold along y=7",
  "fold along x=5",
⟩
day13Input ← SplitFoldingInstructions ReadInput 13

ParseDots ← ReadDec¨∘(','⊸SplitOn)¨
ParseFolds ← (⊑∘'y'⊸∊≍ReadDec∘(IsAsciiNum/⊢))¨
day13ExampleDots ← ParseDots ⊑ day13ExampleInput

# part 1

# 𝕨=0 => x, 𝕨=1 => y
# 𝕩 is coordinate to fold around
# 𝕗 is input dot list (see ParseDots)
_Fold ← {⍷∘((𝕩⊸(((2⊸×⊣)-⊢)⌊⊢)∘⊑≍1⊸⊑)¨⌾(⌽¨⍟𝕨)) 𝕗}

! 17 = ≠ 1 day13ExampleDots _Fold 7

day13Dots ← ParseDots ⊑ day13Input
day13Folds ← ParseFolds 1 ⊑ day13Input

•Out "Day 13.1: "∾•Fmt ≠ (day13Dots _Fold)´ ⊑day13Folds

# part 2

PerformAllFolds ← {𝕩 {(𝕨 _Fold)´𝕩}˜´ ⌽𝕨}
DotMatrix ← {
  ⟨width, height⟩ ← 1+⌈˝∘‿2⥊∾𝕩
  {𝕩? '█';' '}¨ height‿width⥊≠¨⊔((⊣+(width⊸×)∘⊢)´)¨ 𝕩
}

•Out "Day 13.2:"
•Out •Fmt DotMatrix day13Folds PerformAllFolds day13Dots
