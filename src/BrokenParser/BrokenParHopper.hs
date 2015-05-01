{-# OPTIONS_GHC -w #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module Parser.ParHopper where
import Parser.AbsHopper
import Parser.LexHopper
import Utils.ErrM
import Control.Applicative(Applicative(..))

-- parser produced by Happy Version 1.19.4

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 (String)
	| HappyAbsSyn5 (Char)
	| HappyAbsSyn6 (Integer)
	| HappyAbsSyn7 (Double)
	| HappyAbsSyn8 (IdVar)
	| HappyAbsSyn9 (IdCon)
	| HappyAbsSyn10 (IdOpr)
	| HappyAbsSyn11 (Module)
	| HappyAbsSyn12 (Export)
	| HappyAbsSyn13 ([Export])
	| HappyAbsSyn14 ([Def])
	| HappyAbsSyn15 (Def)
	| HappyAbsSyn16 (Cons)
	| HappyAbsSyn17 ([Cons])
	| HappyAbsSyn18 (Par)
	| HappyAbsSyn19 ([Par])
	| HappyAbsSyn20 (Arg)
	| HappyAbsSyn21 ([Arg])
	| HappyAbsSyn22 (Type)
	| HappyAbsSyn23 ([Type])
	| HappyAbsSyn24 (Exp)
	| HappyAbsSyn27 (Cla)
	| HappyAbsSyn28 ([Cla])
	| HappyAbsSyn29 (Pat)
	| HappyAbsSyn30 ([Pat])

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47,
 action_48,
 action_49,
 action_50,
 action_51,
 action_52,
 action_53,
 action_54,
 action_55,
 action_56,
 action_57,
 action_58,
 action_59,
 action_60,
 action_61,
 action_62,
 action_63,
 action_64,
 action_65,
 action_66,
 action_67,
 action_68,
 action_69,
 action_70,
 action_71,
 action_72,
 action_73,
 action_74,
 action_75,
 action_76,
 action_77,
 action_78,
 action_79,
 action_80,
 action_81,
 action_82,
 action_83,
 action_84,
 action_85,
 action_86,
 action_87,
 action_88,
 action_89,
 action_90,
 action_91,
 action_92,
 action_93,
 action_94,
 action_95,
 action_96,
 action_97,
 action_98,
 action_99,
 action_100,
 action_101,
 action_102,
 action_103,
 action_104,
 action_105,
 action_106,
 action_107,
 action_108,
 action_109,
 action_110 :: () => Int -> ({-HappyReduction (Err) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Err) HappyAbsSyn)

happyReduce_1,
 happyReduce_2,
 happyReduce_3,
 happyReduce_4,
 happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20,
 happyReduce_21,
 happyReduce_22,
 happyReduce_23,
 happyReduce_24,
 happyReduce_25,
 happyReduce_26,
 happyReduce_27,
 happyReduce_28,
 happyReduce_29,
 happyReduce_30,
 happyReduce_31,
 happyReduce_32,
 happyReduce_33,
 happyReduce_34,
 happyReduce_35,
 happyReduce_36,
 happyReduce_37,
 happyReduce_38,
 happyReduce_39,
 happyReduce_40,
 happyReduce_41,
 happyReduce_42,
 happyReduce_43,
 happyReduce_44,
 happyReduce_45,
 happyReduce_46,
 happyReduce_47,
 happyReduce_48,
 happyReduce_49,
 happyReduce_50,
 happyReduce_51,
 happyReduce_52,
 happyReduce_53,
 happyReduce_54,
 happyReduce_55,
 happyReduce_56,
 happyReduce_57,
 happyReduce_58,
 happyReduce_59,
 happyReduce_60,
 happyReduce_61,
 happyReduce_62,
 happyReduce_63,
 happyReduce_64,
 happyReduce_65 :: () => ({-HappyReduction (Err) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Err) HappyAbsSyn)

action_0 (44) = happyShift action_4
action_0 (11) = happyGoto action_3
action_0 _ = happyFail

action_1 (51) = happyShift action_2
action_1 _ = happyFail

action_2 _ = happyReduce_1

action_3 (58) = happyAccept
action_3 _ = happyFail

action_4 (56) = happyShift action_6
action_4 (9) = happyGoto action_5
action_4 _ = happyFail

action_5 (31) = happyShift action_7
action_5 _ = happyFail

action_6 _ = happyReduce_6

action_7 (55) = happyShift action_11
action_7 (8) = happyGoto action_8
action_7 (12) = happyGoto action_9
action_7 (13) = happyGoto action_10
action_7 _ = happyReduce_10

action_8 _ = happyReduce_9

action_9 (33) = happyShift action_13
action_9 _ = happyReduce_11

action_10 (32) = happyShift action_12
action_10 _ = happyFail

action_11 _ = happyReduce_5

action_12 (47) = happyShift action_15
action_12 _ = happyFail

action_13 (55) = happyShift action_11
action_13 (8) = happyGoto action_8
action_13 (12) = happyGoto action_9
action_13 (13) = happyGoto action_14
action_13 _ = happyReduce_10

action_14 _ = happyReduce_12

action_15 (36) = happyShift action_16
action_15 _ = happyFail

action_16 (41) = happyShift action_20
action_16 (55) = happyShift action_11
action_16 (8) = happyGoto action_17
action_16 (14) = happyGoto action_18
action_16 (15) = happyGoto action_19
action_16 _ = happyReduce_13

action_17 (35) = happyShift action_24
action_17 (21) = happyGoto action_23
action_17 _ = happyReduce_32

action_18 _ = happyReduce_8

action_19 (36) = happyShift action_22
action_19 _ = happyReduce_14

action_20 (56) = happyShift action_6
action_20 (9) = happyGoto action_21
action_20 _ = happyFail

action_21 (37) = happyShift action_43
action_21 _ = happyFail

action_22 (41) = happyShift action_20
action_22 (55) = happyShift action_11
action_22 (8) = happyGoto action_17
action_22 (14) = happyGoto action_42
action_22 (15) = happyGoto action_19
action_22 _ = happyReduce_13

action_23 (37) = happyShift action_37
action_23 (39) = happyShift action_38
action_23 (51) = happyShift action_2
action_23 (52) = happyShift action_39
action_23 (53) = happyShift action_40
action_23 (54) = happyShift action_41
action_23 (55) = happyShift action_11
action_23 (56) = happyShift action_6
action_23 (4) = happyGoto action_30
action_23 (5) = happyGoto action_31
action_23 (6) = happyGoto action_32
action_23 (7) = happyGoto action_33
action_23 (8) = happyGoto action_34
action_23 (9) = happyGoto action_35
action_23 (20) = happyGoto action_36
action_23 _ = happyFail

action_24 (31) = happyShift action_29
action_24 (55) = happyShift action_11
action_24 (56) = happyShift action_6
action_24 (8) = happyGoto action_25
action_24 (9) = happyGoto action_26
action_24 (22) = happyGoto action_27
action_24 (23) = happyGoto action_28
action_24 _ = happyFail

action_25 _ = happyReduce_35

action_26 _ = happyReduce_34

action_27 (34) = happyShift action_47
action_27 _ = happyReduce_37

action_28 _ = happyReduce_16

action_29 (31) = happyShift action_29
action_29 (55) = happyShift action_11
action_29 (56) = happyShift action_6
action_29 (8) = happyGoto action_25
action_29 (9) = happyGoto action_26
action_29 (22) = happyGoto action_46
action_29 _ = happyFail

action_30 _ = happyReduce_28

action_31 _ = happyReduce_29

action_32 _ = happyReduce_30

action_33 _ = happyReduce_31

action_34 _ = happyReduce_26

action_35 _ = happyReduce_25

action_36 _ = happyReduce_33

action_37 (48) = happyShift action_45
action_37 _ = happyFail

action_38 _ = happyReduce_27

action_39 _ = happyReduce_2

action_40 _ = happyReduce_3

action_41 _ = happyReduce_4

action_42 _ = happyReduce_15

action_43 (48) = happyShift action_44
action_43 _ = happyFail

action_44 (56) = happyShift action_6
action_44 (9) = happyGoto action_63
action_44 (16) = happyGoto action_64
action_44 (17) = happyGoto action_65
action_44 _ = happyFail

action_45 (31) = happyShift action_59
action_45 (38) = happyShift action_60
action_45 (40) = happyShift action_61
action_45 (43) = happyShift action_62
action_45 (51) = happyShift action_2
action_45 (52) = happyShift action_39
action_45 (53) = happyShift action_40
action_45 (54) = happyShift action_41
action_45 (55) = happyShift action_11
action_45 (56) = happyShift action_6
action_45 (4) = happyGoto action_50
action_45 (5) = happyGoto action_51
action_45 (6) = happyGoto action_52
action_45 (7) = happyGoto action_53
action_45 (8) = happyGoto action_54
action_45 (9) = happyGoto action_55
action_45 (24) = happyGoto action_56
action_45 (25) = happyGoto action_57
action_45 (26) = happyGoto action_58
action_45 _ = happyFail

action_46 (34) = happyShift action_49
action_46 _ = happyFail

action_47 (31) = happyShift action_29
action_47 (55) = happyShift action_11
action_47 (56) = happyShift action_6
action_47 (8) = happyGoto action_25
action_47 (9) = happyGoto action_26
action_47 (22) = happyGoto action_27
action_47 (23) = happyGoto action_48
action_47 _ = happyFail

action_48 _ = happyReduce_38

action_49 (31) = happyShift action_29
action_49 (55) = happyShift action_11
action_49 (56) = happyShift action_6
action_49 (8) = happyGoto action_25
action_49 (9) = happyGoto action_26
action_49 (22) = happyGoto action_27
action_49 (23) = happyGoto action_86
action_49 _ = happyFail

action_50 _ = happyReduce_42

action_51 _ = happyReduce_43

action_52 _ = happyReduce_44

action_53 _ = happyReduce_45

action_54 _ = happyReduce_39

action_55 _ = happyReduce_40

action_56 _ = happyReduce_51

action_57 (31) = happyShift action_59
action_57 (51) = happyShift action_2
action_57 (52) = happyShift action_39
action_57 (53) = happyShift action_40
action_57 (54) = happyShift action_41
action_57 (55) = happyShift action_11
action_57 (56) = happyShift action_6
action_57 (57) = happyShift action_82
action_57 (4) = happyGoto action_50
action_57 (5) = happyGoto action_51
action_57 (6) = happyGoto action_52
action_57 (7) = happyGoto action_53
action_57 (8) = happyGoto action_54
action_57 (9) = happyGoto action_55
action_57 (10) = happyGoto action_84
action_57 (24) = happyGoto action_85
action_57 _ = happyReduce_53

action_58 (50) = happyShift action_83
action_58 _ = happyFail

action_59 (31) = happyShift action_59
action_59 (38) = happyShift action_60
action_59 (40) = happyShift action_61
action_59 (43) = happyShift action_62
action_59 (51) = happyShift action_2
action_59 (52) = happyShift action_39
action_59 (53) = happyShift action_40
action_59 (54) = happyShift action_41
action_59 (55) = happyShift action_11
action_59 (56) = happyShift action_6
action_59 (57) = happyShift action_82
action_59 (4) = happyGoto action_50
action_59 (5) = happyGoto action_51
action_59 (6) = happyGoto action_52
action_59 (7) = happyGoto action_53
action_59 (8) = happyGoto action_54
action_59 (9) = happyGoto action_55
action_59 (10) = happyGoto action_80
action_59 (24) = happyGoto action_56
action_59 (25) = happyGoto action_57
action_59 (26) = happyGoto action_81
action_59 _ = happyFail

action_60 (39) = happyShift action_79
action_60 (51) = happyShift action_2
action_60 (52) = happyShift action_39
action_60 (53) = happyShift action_40
action_60 (54) = happyShift action_41
action_60 (55) = happyShift action_11
action_60 (56) = happyShift action_6
action_60 (4) = happyGoto action_71
action_60 (5) = happyGoto action_72
action_60 (6) = happyGoto action_73
action_60 (7) = happyGoto action_74
action_60 (8) = happyGoto action_75
action_60 (9) = happyGoto action_76
action_60 (29) = happyGoto action_77
action_60 (30) = happyGoto action_78
action_60 _ = happyFail

action_61 (31) = happyShift action_59
action_61 (40) = happyShift action_61
action_61 (43) = happyShift action_62
action_61 (51) = happyShift action_2
action_61 (52) = happyShift action_39
action_61 (53) = happyShift action_40
action_61 (54) = happyShift action_41
action_61 (55) = happyShift action_11
action_61 (56) = happyShift action_6
action_61 (4) = happyGoto action_50
action_61 (5) = happyGoto action_51
action_61 (6) = happyGoto action_52
action_61 (7) = happyGoto action_53
action_61 (8) = happyGoto action_54
action_61 (9) = happyGoto action_55
action_61 (24) = happyGoto action_56
action_61 (25) = happyGoto action_70
action_61 _ = happyFail

action_62 (31) = happyShift action_59
action_62 (40) = happyShift action_61
action_62 (43) = happyShift action_62
action_62 (51) = happyShift action_2
action_62 (52) = happyShift action_39
action_62 (53) = happyShift action_40
action_62 (54) = happyShift action_41
action_62 (55) = happyShift action_11
action_62 (56) = happyShift action_6
action_62 (4) = happyGoto action_50
action_62 (5) = happyGoto action_51
action_62 (6) = happyGoto action_52
action_62 (7) = happyGoto action_53
action_62 (8) = happyGoto action_54
action_62 (9) = happyGoto action_55
action_62 (24) = happyGoto action_56
action_62 (25) = happyGoto action_69
action_62 _ = happyFail

action_63 (19) = happyGoto action_68
action_63 _ = happyReduce_23

action_64 (49) = happyShift action_67
action_64 _ = happyReduce_20

action_65 (50) = happyShift action_66
action_65 _ = happyFail

action_66 _ = happyReduce_18

action_67 (56) = happyShift action_6
action_67 (9) = happyGoto action_63
action_67 (16) = happyGoto action_64
action_67 (17) = happyGoto action_97
action_67 _ = happyFail

action_68 (56) = happyShift action_6
action_68 (9) = happyGoto action_95
action_68 (18) = happyGoto action_96
action_68 _ = happyReduce_19

action_69 (31) = happyShift action_59
action_69 (46) = happyShift action_94
action_69 (51) = happyShift action_2
action_69 (52) = happyShift action_39
action_69 (53) = happyShift action_40
action_69 (54) = happyShift action_41
action_69 (55) = happyShift action_11
action_69 (56) = happyShift action_6
action_69 (57) = happyShift action_82
action_69 (4) = happyGoto action_50
action_69 (5) = happyGoto action_51
action_69 (6) = happyGoto action_52
action_69 (7) = happyGoto action_53
action_69 (8) = happyGoto action_54
action_69 (9) = happyGoto action_55
action_69 (10) = happyGoto action_84
action_69 (24) = happyGoto action_85
action_69 _ = happyFail

action_70 (31) = happyShift action_59
action_70 (45) = happyShift action_93
action_70 (51) = happyShift action_2
action_70 (52) = happyShift action_39
action_70 (53) = happyShift action_40
action_70 (54) = happyShift action_41
action_70 (55) = happyShift action_11
action_70 (56) = happyShift action_6
action_70 (57) = happyShift action_82
action_70 (4) = happyGoto action_50
action_70 (5) = happyGoto action_51
action_70 (6) = happyGoto action_52
action_70 (7) = happyGoto action_53
action_70 (8) = happyGoto action_54
action_70 (9) = happyGoto action_55
action_70 (10) = happyGoto action_84
action_70 (24) = happyGoto action_85
action_70 _ = happyFail

action_71 _ = happyReduce_60

action_72 _ = happyReduce_61

action_73 _ = happyReduce_62

action_74 _ = happyReduce_63

action_75 _ = happyReduce_58

action_76 _ = happyReduce_57

action_77 (39) = happyShift action_79
action_77 (51) = happyShift action_2
action_77 (52) = happyShift action_39
action_77 (53) = happyShift action_40
action_77 (54) = happyShift action_41
action_77 (55) = happyShift action_11
action_77 (56) = happyShift action_6
action_77 (4) = happyGoto action_71
action_77 (5) = happyGoto action_72
action_77 (6) = happyGoto action_73
action_77 (7) = happyGoto action_74
action_77 (8) = happyGoto action_75
action_77 (9) = happyGoto action_76
action_77 (29) = happyGoto action_77
action_77 (30) = happyGoto action_92
action_77 _ = happyReduce_64

action_78 (34) = happyShift action_91
action_78 _ = happyFail

action_79 _ = happyReduce_59

action_80 (32) = happyShift action_90
action_80 _ = happyFail

action_81 (32) = happyShift action_89
action_81 _ = happyFail

action_82 _ = happyReduce_7

action_83 _ = happyReduce_17

action_84 (31) = happyShift action_59
action_84 (51) = happyShift action_2
action_84 (52) = happyShift action_39
action_84 (53) = happyShift action_40
action_84 (54) = happyShift action_41
action_84 (55) = happyShift action_11
action_84 (56) = happyShift action_6
action_84 (4) = happyGoto action_50
action_84 (5) = happyGoto action_51
action_84 (6) = happyGoto action_52
action_84 (7) = happyGoto action_53
action_84 (8) = happyGoto action_54
action_84 (9) = happyGoto action_55
action_84 (24) = happyGoto action_88
action_84 _ = happyFail

action_85 _ = happyReduce_48

action_86 (32) = happyShift action_87
action_86 _ = happyFail

action_87 _ = happyReduce_36

action_88 _ = happyReduce_47

action_89 _ = happyReduce_46

action_90 _ = happyReduce_41

action_91 (31) = happyShift action_59
action_91 (38) = happyShift action_60
action_91 (40) = happyShift action_61
action_91 (43) = happyShift action_62
action_91 (51) = happyShift action_2
action_91 (52) = happyShift action_39
action_91 (53) = happyShift action_40
action_91 (54) = happyShift action_41
action_91 (55) = happyShift action_11
action_91 (56) = happyShift action_6
action_91 (4) = happyGoto action_50
action_91 (5) = happyGoto action_51
action_91 (6) = happyGoto action_52
action_91 (7) = happyGoto action_53
action_91 (8) = happyGoto action_54
action_91 (9) = happyGoto action_55
action_91 (24) = happyGoto action_56
action_91 (25) = happyGoto action_57
action_91 (26) = happyGoto action_100
action_91 _ = happyFail

action_92 _ = happyReduce_65

action_93 (48) = happyShift action_99
action_93 _ = happyFail

action_94 (31) = happyShift action_59
action_94 (51) = happyShift action_2
action_94 (52) = happyShift action_39
action_94 (53) = happyShift action_40
action_94 (54) = happyShift action_41
action_94 (55) = happyShift action_11
action_94 (56) = happyShift action_6
action_94 (4) = happyGoto action_50
action_94 (5) = happyGoto action_51
action_94 (6) = happyGoto action_52
action_94 (7) = happyGoto action_53
action_94 (8) = happyGoto action_54
action_94 (9) = happyGoto action_55
action_94 (24) = happyGoto action_98
action_94 _ = happyFail

action_95 _ = happyReduce_22

action_96 _ = happyReduce_24

action_97 _ = happyReduce_21

action_98 (42) = happyShift action_104
action_98 _ = happyFail

action_99 (39) = happyShift action_79
action_99 (51) = happyShift action_2
action_99 (52) = happyShift action_39
action_99 (53) = happyShift action_40
action_99 (54) = happyShift action_41
action_99 (55) = happyShift action_11
action_99 (56) = happyShift action_6
action_99 (4) = happyGoto action_71
action_99 (5) = happyGoto action_72
action_99 (6) = happyGoto action_73
action_99 (7) = happyGoto action_74
action_99 (8) = happyGoto action_75
action_99 (9) = happyGoto action_76
action_99 (27) = happyGoto action_101
action_99 (28) = happyGoto action_102
action_99 (29) = happyGoto action_103
action_99 _ = happyFail

action_100 _ = happyReduce_52

action_101 (36) = happyShift action_108
action_101 _ = happyReduce_55

action_102 (50) = happyShift action_107
action_102 _ = happyFail

action_103 (34) = happyShift action_106
action_103 _ = happyFail

action_104 (31) = happyShift action_59
action_104 (51) = happyShift action_2
action_104 (52) = happyShift action_39
action_104 (53) = happyShift action_40
action_104 (54) = happyShift action_41
action_104 (55) = happyShift action_11
action_104 (56) = happyShift action_6
action_104 (4) = happyGoto action_50
action_104 (5) = happyGoto action_51
action_104 (6) = happyGoto action_52
action_104 (7) = happyGoto action_53
action_104 (8) = happyGoto action_54
action_104 (9) = happyGoto action_55
action_104 (24) = happyGoto action_105
action_104 _ = happyFail

action_105 _ = happyReduce_50

action_106 (31) = happyShift action_59
action_106 (38) = happyShift action_60
action_106 (40) = happyShift action_61
action_106 (43) = happyShift action_62
action_106 (51) = happyShift action_2
action_106 (52) = happyShift action_39
action_106 (53) = happyShift action_40
action_106 (54) = happyShift action_41
action_106 (55) = happyShift action_11
action_106 (56) = happyShift action_6
action_106 (4) = happyGoto action_50
action_106 (5) = happyGoto action_51
action_106 (6) = happyGoto action_52
action_106 (7) = happyGoto action_53
action_106 (8) = happyGoto action_54
action_106 (9) = happyGoto action_55
action_106 (24) = happyGoto action_56
action_106 (25) = happyGoto action_57
action_106 (26) = happyGoto action_110
action_106 _ = happyFail

action_107 _ = happyReduce_49

action_108 (39) = happyShift action_79
action_108 (51) = happyShift action_2
action_108 (52) = happyShift action_39
action_108 (53) = happyShift action_40
action_108 (54) = happyShift action_41
action_108 (55) = happyShift action_11
action_108 (56) = happyShift action_6
action_108 (4) = happyGoto action_71
action_108 (5) = happyGoto action_72
action_108 (6) = happyGoto action_73
action_108 (7) = happyGoto action_74
action_108 (8) = happyGoto action_75
action_108 (9) = happyGoto action_76
action_108 (27) = happyGoto action_101
action_108 (28) = happyGoto action_109
action_108 (29) = happyGoto action_103
action_108 _ = happyFail

action_109 _ = happyReduce_56

action_110 _ = happyReduce_54

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyTerminal (PT _ (TL happy_var_1)))
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  5 happyReduction_2
happyReduction_2 (HappyTerminal (PT _ (TC happy_var_1)))
	 =  HappyAbsSyn5
		 ((read ( happy_var_1)) :: Char
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  6 happyReduction_3
happyReduction_3 (HappyTerminal (PT _ (TI happy_var_1)))
	 =  HappyAbsSyn6
		 ((read ( happy_var_1)) :: Integer
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  7 happyReduction_4
happyReduction_4 (HappyTerminal (PT _ (TD happy_var_1)))
	 =  HappyAbsSyn7
		 ((read ( happy_var_1)) :: Double
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  8 happyReduction_5
happyReduction_5 (HappyTerminal (PT _ (T_IdVar happy_var_1)))
	 =  HappyAbsSyn8
		 (IdVar (happy_var_1)
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_1  9 happyReduction_6
happyReduction_6 (HappyTerminal (PT _ (T_IdCon happy_var_1)))
	 =  HappyAbsSyn9
		 (IdCon (happy_var_1)
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_1  10 happyReduction_7
happyReduction_7 (HappyTerminal (PT _ (T_IdOpr happy_var_1)))
	 =  HappyAbsSyn10
		 (IdOpr (happy_var_1)
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happyReduce 8 11 happyReduction_8
happyReduction_8 ((HappyAbsSyn14  happy_var_8) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (MModule happy_var_2 happy_var_4 happy_var_8
	) `HappyStk` happyRest

happyReduce_9 = happySpecReduce_1  12 happyReduction_9
happyReduction_9 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn12
		 (MExport happy_var_1
	)
happyReduction_9 _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_0  13 happyReduction_10
happyReduction_10  =  HappyAbsSyn13
		 ([]
	)

happyReduce_11 = happySpecReduce_1  13 happyReduction_11
happyReduction_11 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn13
		 ((:[]) happy_var_1
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  13 happyReduction_12
happyReduction_12 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn13
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_0  14 happyReduction_13
happyReduction_13  =  HappyAbsSyn14
		 ([]
	)

happyReduce_14 = happySpecReduce_1  14 happyReduction_14
happyReduction_14 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn14
		 ((:[]) happy_var_1
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  14 happyReduction_15
happyReduction_15 (HappyAbsSyn14  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn14
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  15 happyReduction_16
happyReduction_16 (HappyAbsSyn23  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn15
		 (DSig happy_var_1 happy_var_3
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happyReduce 6 15 happyReduction_17
happyReduction_17 (_ `HappyStk`
	(HappyAbsSyn24  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn21  happy_var_2) `HappyStk`
	(HappyAbsSyn8  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (DFun happy_var_1 (reverse happy_var_2) happy_var_5
	) `HappyStk` happyRest

happyReduce_18 = happyReduce 6 15 happyReduction_18
happyReduction_18 (_ `HappyStk`
	(HappyAbsSyn17  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (DDat happy_var_2 happy_var_5
	) `HappyStk` happyRest

happyReduce_19 = happySpecReduce_2  16 happyReduction_19
happyReduction_19 (HappyAbsSyn19  happy_var_2)
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn16
		 (FCon happy_var_1 (reverse happy_var_2)
	)
happyReduction_19 _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  17 happyReduction_20
happyReduction_20 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn17
		 ((:[]) happy_var_1
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  17 happyReduction_21
happyReduction_21 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn17
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1  18 happyReduction_22
happyReduction_22 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn18
		 (GCon happy_var_1
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_0  19 happyReduction_23
happyReduction_23  =  HappyAbsSyn19
		 ([]
	)

happyReduce_24 = happySpecReduce_2  19 happyReduction_24
happyReduction_24 (HappyAbsSyn18  happy_var_2)
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (flip (:) happy_var_1 happy_var_2
	)
happyReduction_24 _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  20 happyReduction_25
happyReduction_25 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn20
		 (ACon happy_var_1
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  20 happyReduction_26
happyReduction_26 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn20
		 (AVar happy_var_1
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  20 happyReduction_27
happyReduction_27 _
	 =  HappyAbsSyn20
		 (AWild
	)

happyReduce_28 = happySpecReduce_1  20 happyReduction_28
happyReduction_28 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn20
		 (AString happy_var_1
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_1  20 happyReduction_29
happyReduction_29 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn20
		 (AChar happy_var_1
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  20 happyReduction_30
happyReduction_30 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn20
		 (AInteger happy_var_1
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  20 happyReduction_31
happyReduction_31 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn20
		 (ADouble happy_var_1
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_0  21 happyReduction_32
happyReduction_32  =  HappyAbsSyn21
		 ([]
	)

happyReduce_33 = happySpecReduce_2  21 happyReduction_33
happyReduction_33 (HappyAbsSyn20  happy_var_2)
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 (flip (:) happy_var_1 happy_var_2
	)
happyReduction_33 _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_1  22 happyReduction_34
happyReduction_34 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn22
		 (TName happy_var_1
	)
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_1  22 happyReduction_35
happyReduction_35 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn22
		 (TVar happy_var_1
	)
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happyReduce 5 22 happyReduction_36
happyReduction_36 (_ `HappyStk`
	(HappyAbsSyn23  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn22  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn22
		 (TFun happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_37 = happySpecReduce_1  23 happyReduction_37
happyReduction_37 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn23
		 ((:[]) happy_var_1
	)
happyReduction_37 _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_3  23 happyReduction_38
happyReduction_38 (HappyAbsSyn23  happy_var_3)
	_
	(HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn23
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_38 _ _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_1  24 happyReduction_39
happyReduction_39 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn24
		 (EVar happy_var_1
	)
happyReduction_39 _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_1  24 happyReduction_40
happyReduction_40 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn24
		 (ECon happy_var_1
	)
happyReduction_40 _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_3  24 happyReduction_41
happyReduction_41 _
	(HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn24
		 (EOpr happy_var_2
	)
happyReduction_41 _ _ _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_1  24 happyReduction_42
happyReduction_42 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn24
		 (EString happy_var_1
	)
happyReduction_42 _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_1  24 happyReduction_43
happyReduction_43 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn24
		 (EChar happy_var_1
	)
happyReduction_43 _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_1  24 happyReduction_44
happyReduction_44 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn24
		 (EInteger happy_var_1
	)
happyReduction_44 _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_1  24 happyReduction_45
happyReduction_45 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn24
		 (EDouble happy_var_1
	)
happyReduction_45 _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_3  24 happyReduction_46
happyReduction_46 _
	(HappyAbsSyn24  happy_var_2)
	_
	 =  HappyAbsSyn24
		 (happy_var_2
	)
happyReduction_46 _ _ _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_3  25 happyReduction_47
happyReduction_47 (HappyAbsSyn24  happy_var_3)
	(HappyAbsSyn10  happy_var_2)
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (EInfix happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_47 _ _ _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_2  25 happyReduction_48
happyReduction_48 (HappyAbsSyn24  happy_var_2)
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (EApp happy_var_1 happy_var_2
	)
happyReduction_48 _ _  = notHappyAtAll 

happyReduce_49 = happyReduce 6 25 happyReduction_49
happyReduction_49 (_ `HappyStk`
	(HappyAbsSyn28  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn24  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn24
		 (ECase happy_var_2 happy_var_5
	) `HappyStk` happyRest

happyReduce_50 = happyReduce 6 25 happyReduction_50
happyReduction_50 ((HappyAbsSyn24  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn24  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn24  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn24
		 (EIf happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_51 = happySpecReduce_1  25 happyReduction_51
happyReduction_51 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (happy_var_1
	)
happyReduction_51 _  = notHappyAtAll 

happyReduce_52 = happyReduce 4 26 happyReduction_52
happyReduction_52 ((HappyAbsSyn24  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn30  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn24
		 (ELambda happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_53 = happySpecReduce_1  26 happyReduction_53
happyReduction_53 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (happy_var_1
	)
happyReduction_53 _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_3  27 happyReduction_54
happyReduction_54 (HappyAbsSyn24  happy_var_3)
	_
	(HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn27
		 (CClause happy_var_1 happy_var_3
	)
happyReduction_54 _ _ _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_1  28 happyReduction_55
happyReduction_55 (HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn28
		 ((:[]) happy_var_1
	)
happyReduction_55 _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_3  28 happyReduction_56
happyReduction_56 (HappyAbsSyn28  happy_var_3)
	_
	(HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn28
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_56 _ _ _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_1  29 happyReduction_57
happyReduction_57 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn29
		 (PCon happy_var_1
	)
happyReduction_57 _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_1  29 happyReduction_58
happyReduction_58 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn29
		 (PVar happy_var_1
	)
happyReduction_58 _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_1  29 happyReduction_59
happyReduction_59 _
	 =  HappyAbsSyn29
		 (PWild
	)

happyReduce_60 = happySpecReduce_1  29 happyReduction_60
happyReduction_60 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn29
		 (PString happy_var_1
	)
happyReduction_60 _  = notHappyAtAll 

happyReduce_61 = happySpecReduce_1  29 happyReduction_61
happyReduction_61 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn29
		 (PChar happy_var_1
	)
happyReduction_61 _  = notHappyAtAll 

happyReduce_62 = happySpecReduce_1  29 happyReduction_62
happyReduction_62 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn29
		 (PInteger happy_var_1
	)
happyReduction_62 _  = notHappyAtAll 

happyReduce_63 = happySpecReduce_1  29 happyReduction_63
happyReduction_63 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn29
		 (PDouble happy_var_1
	)
happyReduction_63 _  = notHappyAtAll 

happyReduce_64 = happySpecReduce_1  30 happyReduction_64
happyReduction_64 (HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn30
		 ((:[]) happy_var_1
	)
happyReduction_64 _  = notHappyAtAll 

happyReduce_65 = happySpecReduce_2  30 happyReduction_65
happyReduction_65 (HappyAbsSyn30  happy_var_2)
	(HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn30
		 ((:) happy_var_1 happy_var_2
	)
happyReduction_65 _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 58 58 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	PT _ (TS _ 1) -> cont 31;
	PT _ (TS _ 2) -> cont 32;
	PT _ (TS _ 3) -> cont 33;
	PT _ (TS _ 4) -> cont 34;
	PT _ (TS _ 5) -> cont 35;
	PT _ (TS _ 6) -> cont 36;
	PT _ (TS _ 7) -> cont 37;
	PT _ (TS _ 8) -> cont 38;
	PT _ (TS _ 9) -> cont 39;
	PT _ (TS _ 10) -> cont 40;
	PT _ (TS _ 11) -> cont 41;
	PT _ (TS _ 12) -> cont 42;
	PT _ (TS _ 13) -> cont 43;
	PT _ (TS _ 14) -> cont 44;
	PT _ (TS _ 15) -> cont 45;
	PT _ (TS _ 16) -> cont 46;
	PT _ (TS _ 17) -> cont 47;
	PT _ (TS _ 18) -> cont 48;
	PT _ (TS _ 19) -> cont 49;
	PT _ (TS _ 20) -> cont 50;
	PT _ (TL happy_dollar_dollar) -> cont 51;
	PT _ (TC happy_dollar_dollar) -> cont 52;
	PT _ (TI happy_dollar_dollar) -> cont 53;
	PT _ (TD happy_dollar_dollar) -> cont 54;
	PT _ (T_IdVar happy_dollar_dollar) -> cont 55;
	PT _ (T_IdCon happy_dollar_dollar) -> cont 56;
	PT _ (T_IdOpr happy_dollar_dollar) -> cont 57;
	_ -> happyError' (tk:tks)
	}

happyError_ 58 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

happyThen :: () => Err a -> (a -> Err b) -> Err b
happyThen = (thenM)
happyReturn :: () => a -> Err a
happyReturn = (returnM)
happyThen1 m k tks = (thenM) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Err a
happyReturn1 = \a tks -> (returnM) a
happyError' :: () => [(Token)] -> Err a
happyError' = happyError

pModule tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn11 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    _ -> " before " ++ unwords (map (id . prToken) (take 4 ts))

myLexer = tokens
{-# LINE 1 "templates\GenericTemplate.hs" #-}
{-# LINE 1 "templates\\GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 1 "templates\\GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 13 "templates\\GenericTemplate.hs" #-}

{-# LINE 46 "templates\\GenericTemplate.hs" #-}








{-# LINE 67 "templates\\GenericTemplate.hs" #-}

{-# LINE 77 "templates\\GenericTemplate.hs" #-}

{-# LINE 86 "templates\\GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 155 "templates\\GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 256 "templates\\GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 322 "templates\\GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
