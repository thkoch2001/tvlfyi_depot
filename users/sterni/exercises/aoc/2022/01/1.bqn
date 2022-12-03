lib ← •Import •path∾"/../../lib.bqn"
input ← lib.ReadDec¨¨ (<"") lib.SplitOn •FLines •path∾"/input"

a‿·‿b ← +`3↑∨+´¨ input

•Out "day01.1: "∾•Fmt a
•Out "day01.2: "∾•Fmt b
