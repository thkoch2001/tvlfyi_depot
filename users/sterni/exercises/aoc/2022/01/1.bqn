lib ← •Import •path∾"/../../lib.bqn"
input ← lib.ReadDec¨¨ (<"") lib.SplitOn •FLines •path∾"/input"

a‿·‿b ← +`3↑∨+´¨ input

•Out "day 01.1: "∾•Fmt a
•Out "day 01.2: "∾•Fmt b
