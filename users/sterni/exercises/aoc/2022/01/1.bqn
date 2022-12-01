lib ← •Import •path∾"/../../lib.bqn"
input ← lib.ReadDec¨¨ (<"") lib.SplitOn •FLines •path∾"/input"

cpe ← +´¨ input

•Out "day 01.1: "∾•Fmt ⌈´cpe
•Out "day 01.2: "∾•Fmt +´3↑∨cpe
