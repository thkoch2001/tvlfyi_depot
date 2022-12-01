#!/usr/bin/env BQN

#
# Utilities
#

⟨IsAsciiNum,ReadInt,ReadDec,SplitOn,_fix⟩ ← •Import •path∾"/../lib.bqn"

ReadInput ← {•file.Lines ∾ •path‿"/input/day"‿(•Fmt 𝕩)}

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
# 2021-12-04
#

day4Numbers ← ReadDec¨ ',' SplitOn ⊑ReadInput 4
day4Boards ← ReadDec¨>˘(' '⊸SplitOn¨)> (<⟨⟩) SplitOn 2↓ReadInput 4

BoardWins ← {C ← ∨´∘(∧´˘) ⋄ (C∨C∘⍉)𝕩}

_CallNumber ← {(𝕗∊⥊𝕩) (∨⍟(¬∘BoardWins∘⊢))˘ 𝕨}

BoardWinScores ← {
  𝕩 (0⊸</×) (⊢-») (+´)∘(BoardWins˘/(+´⥊)˘∘(𝕨⊸×⟜¬))¨ (<0⥊˜≢𝕨) (𝕨 _CallNumber)`𝕩
}

day4WinScores ← day4Boards BoardWinScores day4Numbers

•Out "Day 4.1: "∾•Fmt ⊑day4WinScores
•Out "Day 4.2: "∾•Fmt ⊑⌽day4WinScores

#
# 2021-12-06
#

day6ExampleInput ← ⟨3,4,3,1,2⟩
day6Input ← ReadDec¨ ',' SplitOn ⊑ReadInput 6

LanternfishPopulation ← {+´ (1⊸⌽+(⊑×((6⊸=)∘↕∘≠)))⍟𝕨 9↑≠¨⊔ 𝕩}

! 26 = 18 LanternfishPopulation day6ExampleInput
! 5934 = 80 LanternfishPopulation day6ExampleInput

•Out "Day 6.1: "∾•Fmt 80 LanternfishPopulation day6Input
•Out "Day 6.2: "∾•Fmt 256 LanternfishPopulation day6Input

#
# 2021-12-07
#

# part 1

day7ExampleInput ← ⟨16,1,2,0,4,2,7,1,2,14⟩
day7Input ← ReadDec¨ ','  SplitOn ⊑ReadInput 7

PossiblePositions ← (⌊´+⟜(↕1⊸+)⌈´)
FuelConsumption ← +˝∘|∘(-⌜)
_lowestFuelPossible ← {⌊´∘(𝔽⟜PossiblePositions)˜ 𝕩}

! 37 = FuelConsumption _lowestFuelPossible day7ExampleInput

•Out "Day 7.1: "∾•Fmt FuelConsumption _lowestFuelPossible day7Input

# part 2

TriNum ← 1⊸+×÷⟜2

FuelConsumption2 ← +˝∘(TriNum¨)∘|∘(-⌜)

! 168 = FuelConsumption2 _lowestFuelPossible day7ExampleInput

•Out "Day 7.2: "∾•Fmt FuelConsumption2 _lowestFuelPossible day7Input

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
# 2021-12-10
#

day10ExampleInput ← ⟨
  "[({(<(())[]>[[{[]{<()<>>",
  "[(()[<>])]({[<{<<[]>>(",
  "{([(<{}[<>[]}>{[]{[(<()>",
  "(((({<>}<{<{<>}{[]{[]{}",
  "[[<[([]))<([[{}[[()]]]",
  "[{[{({}]{}}([{[{{{}}([]",
  "{<[[]]>}<{[{[{[]{()[[[]",
  "[<(<(<(<{}))><([]([]()",
  "<{([([[(<>()){}]>(<<{{",
  "<{([{{}}[<[[[<>{}]]]>[]]",
⟩
day10Input ← ReadInput 10

# part 1

opp ← "([{<"
clp ← ")]}>"
SwapParen ← (opp∾⌽clp)⊸((⊑⊐)⊑(⌽⊣))

ParenStacks ← ((<⟨⟩)⊸(((⊑∊)⟜clp⊢)◶(∾˜⟜SwapParen)‿(1⊸↓⊣)`))
LegalParens ← ((1⊸↑)¨∘»∘ParenStacks ((∊⟜opp⊢)∨(≡⟜⋈)¨) ⊢)

_ScoreFor_ ← {𝕗⊸(𝕘⊸⊐⊏⊣) 𝕩}

SyntaxScore ← +´∘(0‿3‿57‿1197‿25137 _ScoreFor_ (" "∾clp))∘∾∘(1⊸↑∘(¬∘LegalParens/⊢)¨)

! 26397 = SyntaxScore day10ExampleInput
•Out "Day 10.1: "∾•Fmt SyntaxScore day10Input

# part 2

AutocompleteScore ← {
  Score ← (5⊸×⊸+)˜´∘⌽∘((1+↕4) _ScoreFor_ clp)
  # TODO(sterni): we compute ParenStacks twice here
  ((⌊÷⟜2)∘≠⊑⊢) ∧ Score∘(⊑⌽)∘ParenStacks¨ (∧´∘LegalParens¨/⊢) 𝕩
}

! 288957 = AutocompleteScore day10ExampleInput
•Out "Day 10.2: "∾•Fmt AutocompleteScore day10Input

#
# 2021-12-11
#

day11Input ← '0'-˜> ReadInput 11
day11ExampleInput ← >⟨
  ⟨5,4,8,3,1,4,3,2,2,3,⟩,
  ⟨2,7,4,5,8,5,4,7,1,1,⟩,
  ⟨5,2,6,4,5,5,6,1,7,3,⟩,
  ⟨6,1,4,1,3,3,6,1,4,6,⟩,
  ⟨6,3,5,7,3,8,5,4,7,8,⟩,
  ⟨4,1,6,7,5,2,4,6,4,5,⟩,
  ⟨2,1,7,6,8,4,1,7,2,1,⟩,
  ⟨6,8,8,2,8,8,1,1,3,4,⟩,
  ⟨4,8,4,6,8,4,8,5,5,4,⟩,
  ⟨5,2,8,3,7,5,1,5,2,6,⟩,
⟩

# part 1

OctopusFlash ← {
  ((⥊⟜0)∘≢𝕊⊢) 𝕩;
  flashing ← (¬𝕨)∧9<𝕩
  energy ← ((«˘»)+(»˘«)+(»˘»)+(«˘«)+(»˘)+(«˘)+«+») flashing
  ((𝕨∨flashing)⊸𝕊)⍟(0<+´⥊flashing) energy+𝕩
}

OctopusStep ← ((9⊸≥)×⊢)∘OctopusFlash∘(1⊸+)
OctopusFlashCount ← {+´⥊0=>(OctopusStep⊣)`(1+𝕨)⥊<𝕩}

! 1656 = 100 OctopusFlashCount day11ExampleInput
•Out "Day 11.1: "∾•Fmt 100 OctopusFlashCount day11Input

# part 2

_iterCountUntil_ ← {
  0 𝕊 𝕩;
  𝔾◶⟨((𝕨+1)⊸𝕊)∘𝔽, 𝕨˙⟩ 𝕩
}

OctopusAllFlashing ← OctopusStep _iterCountUntil_ (∧´∘⥊∘(0⊸=))

! 195 = OctopusAllFlashing day11ExampleInput

•Out "Day 11.2: "∾•Fmt OctopusAllFlashing day11Input

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

#
# 2021-12-14
#

day14Polymer ← ⊑ReadInput 14
day14Mapping ← 2↓ReadInput 14

lp ← (2⊸↑)¨ day14Mapping
le ← ⍷∾lp

# returns array as long as 𝕨 detailing how many times the element
# at any given index occurs in 𝕩.
Counts ← ((≠⊣)↑(/⁼)∘⊐)

deltaPairs ← {
  addedPairs ← ((-1)⊸⊑¨day14Mapping) (⌽⌾(0⊸⊑))∘(∾¨)¨ lp
  removedPairs ← ⋈¨ (2⊸↑)¨ lp
  addedPairs (-○(lp⊸Counts))¨ removedPairs
}

pairCount ← lp Counts ⥊∘(⋈˘) 2↕day14Polymer

PairInsert ← {𝕩 +´ 𝕩×¨deltaPairs}

pairElementCount ← (le⊸Counts)¨lp

ElementRarityDiff ← {
 ((-1)⊸⊑-⊑)∧ ⌈2÷˜ +´ pairElementCount×PairInsert⍟𝕩 pairCount
}

•Out "Day 14.1: "∾•Fmt ElementRarityDiff 10
•Out "Day 14.2: "∾•Fmt ElementRarityDiff 40

#
# 2021-12-15
#

day15ExampleInput ← >⟨
  1‿1‿6‿3‿7‿5‿1‿7‿4‿2
  1‿3‿8‿1‿3‿7‿3‿6‿7‿2
  2‿1‿3‿6‿5‿1‿1‿3‿2‿8
  3‿6‿9‿4‿9‿3‿1‿5‿6‿9
  7‿4‿6‿3‿4‿1‿7‿1‿1‿1
  1‿3‿1‿9‿1‿2‿8‿1‿3‿7
  1‿3‿5‿9‿9‿1‿2‿4‿2‿1
  3‿1‿2‿5‿4‿2‿1‿6‿3‿9
  1‿2‿9‿3‿1‿3‿8‿5‿2‿1
  2‿3‿1‿1‿9‿4‿4‿5‿8‿1
⟩
day15Input ← '0'-˜ ((≠⋈≠∘⊑)⥊∾)ReadInput 15

LowestRiskLevel ← {
  start ← 0˙⌾⊑ (⥊⟜∞) ≢𝕩
  ir ← (1⊑≢𝕩)⥊∞
  Step ← {𝕩 ⌊ 𝕨 + (ir⊸«⌊ir⊸»⌊∞⊸«˘⌊∞⊸»˘) 𝕩}
  ⊑⌽⥊ 𝕩⊸Step _fix start
}

! 40 = LowestRiskLevel day15ExampleInput

•Out "Day 15.1: "∾•Fmt LowestRiskLevel day15Input

FiveByFiveMap ← {(9⊸|)⌾(-⟜1) ∾(<𝕩)+ +⌜˜↕5}

! 315 = LowestRiskLevel FiveByFiveMap day15ExampleInput

•Out "Day 15.2: "∾•Fmt LowestRiskLevel FiveByFiveMap day15Input

#
# 2021-12-20
#

ParsePic ← (⋈⟜0)∘('#'⊸=)∘>

day20ExampleAlgo ← '#'="..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#"
day20ExamplePic ← ParsePic ⟨"#..#.", "#....", "##..#", "..#..", "..###"⟩
day20Input ← ReadInput 20
day20Algo ← '#'=⊑day20Input
day20Pic ← ParsePic 2↓day20Input

GrowAxis ← {(⊢ (-1)⊸⌽∘∾ (⥊⟜𝕨)∘(2˙⌾⊑)∘≢) 𝕩}
Grow ← {𝕨 GrowAxis 𝕨 GrowAxis˘ 𝕩}

Enhance ← {
  inf ← 1⊑𝕩
  npic ← ((⊑⟜𝕨)∘DebinList∘⥊)˘˘ 3‿3↕ (inf⊸Grow)⍟2 ⊑𝕩
  ninf ← 𝕨⊑˜511×inf
  npic⋈ninf
}
_EnhancedPixelCount ← {+´⥊⊑ (𝕨⊸Enhance)⍟𝕗 𝕩}

! 35 = day20ExampleAlgo 2 _EnhancedPixelCount day20ExamplePic
! 3351 = day20ExampleAlgo 50 _EnhancedPixelCount day20ExamplePic

•Out "Day 20.1: "∾•Fmt day20Algo 2 _EnhancedPixelCount day20Pic
•Out "Day 20.2: "∾•Fmt day20algo 50 _EnhancedPixelCount day20Pic

#
# 2021-12-25
#

day25Input ← ".>v" ⊐ > ReadInput 25
day25ExampleInput ← ".>v"⊐∘‿10⥊"v...>>.vv>.vv>>.vv..>>.>v>...v>>v>>.>.v.v>v.vv.v..>.>>..v....vv..>.>v.v.v..>>v.v....v..v.>"

Xor ← (¬⊸∧∨∧⟜¬)
MoveHerd ← {(𝕩∧𝕩≠𝕨)+𝕨× (𝕨=𝕩) (Xor⟜(1⊸⌽)∨⊢) (0=𝕩)∧(-1)⌽𝕨=𝕩}

_fixCount ← {
  1 𝕊 𝕩;
  𝕩 ≡◶⟨(𝕨+1)⊸𝕊, 𝕨˙⟩ 𝔽 𝕩
}

MoveAllHerds ← (2⊸MoveHerd)∘(1⊸MoveHerd˘)

! 58 = MoveAllHerds _fixCount day25ExampleInput
•Out "Day 25.1: "∾•Fmt MoveAllHerds _fixCount day25Input
