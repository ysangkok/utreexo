module UnitTests (unitTests) where

import Test.Tasty.HUnit
import Test.Tasty (TestTree, testGroup)

import Lib

tests :: TestTree
tests = testGroup "parentMany/getRootsReverse tests"
  [
    testCase "parentMany(position=0, rise=row=3, forestRows=5)=56" $
     parentMany 0 3 5 @?= 56
  , testCase "parentMany(position=8, rise=row=2, forestRows=5)=50" $
     parentMany 8 2 5 @?= 50
  , testCase "parentMany(position=0, rise=row=4, forestRows=5)=60" $
     parentMany 0 4 5 @?= 60
  , testCase "parentMany(position=16, rise=row=2, forestRows=5)=52" $
     parentMany 16 2 5 @?= 52
  , testCase "parentMany(position=20, rise=row=1, forestRows=5)=42" $
     parentMany 20 1 5 @?= 42
  , testCase "parentMany(position=22, rise=row=0, forestRows=5)=22" $
     parentMany 22 0 5 @?= 22
  , testCase "parentMany(position=0, rise=row=0, forestRows=5)=0" $
     parentMany 0 0 5 @?= 0
  , testCase "getRootsReverse1" $
     getRootsReverse 23 5 @?= ([22, 42, 52, 60], [0, 1, 2, 4])
  , testCase "getRootsReverse2" $
     getRootsReverse 22 5 @?= ([42, 52, 60], [1, 2, 4])
  , testCase "getRootsReverse3" $
     getRootsReverse 21 5 @?= ([20, 52, 60], [0, 2, 4])
  , testCase "getRootsReverse4" $
     getRootsReverse 20 5 @?= ([52, 60], [2, 4])
  , testCase "getRootsReverse4" $
     getRootsReverse 19 5 @?= ([18, 40, 60], [0, 1, 4])
  , testCase "getRootsReverse5" $
     getRootsReverse 18 5 @?= ([40, 60], [1, 4])
  , testCase "getRootsReverse6" $
     getRootsReverse 17 5 @?= ([16, 60], [0, 4])
  , testCase "getRootsReverse7" $
     getRootsReverse 16 5 @?= ([60], [4])
  , testCase "getRootsReverse 15 5" $
     getRootsReverse 15 5 @?= ([14, 38, 50, 56], [0, 1, 2, 3])
  , testCase "getRootsReverse 3 5" $
     getRootsReverse 3 5 @?= ([2, 32], [0, 1])
  , testCase "getRootsReverse 2 5" $
     getRootsReverse 2 5 @?= ([32], [1])
  , testCase "getRootsReverse 1 5" $
     getRootsReverse 1 5 @?= ([0], [0])
  , testCase "getRootsReverse 0 5" $
     getRootsReverse 0 5 @?= ([], [])
  ]

tests2 :: TestTree
tests2 = testGroup "extractTwins tests"
  [
     testCase "  1" $ extractTwins [0, 1, 2, 3, 4, 7, 10, 13, 14, 20, 22, 23] 5 @?= ([32, 33, 43], [4, 7, 10, 13, 14, 20])
   , testCase "  2" $ extractTwins [0, 1, 2, 5, 6, 7, 14, 18, 21, 22, 23, 24, 25] 5 @?= ([32, 35, 43, 44], [2, 5, 14, 18, 21])
   , testCase "  3" $ extractTwins [0, 1, 3, 5, 7, 8, 10, 12, 13] 4 @?= ([16, 22], [3, 5, 7, 8, 10])
   , testCase "  4" $ extractTwins [0] 2 @?= ([], [0])
   , testCase "  5" $ extractTwins [0, 3, 5, 7, 8, 10, 11, 12, 13, 14, 16, 21, 22, 24] 5 @?= ([37, 38], [0, 3, 5, 7, 8, 14, 16, 21, 22, 24])
   , testCase "  6" $ extractTwins [0, 5, 6, 7, 8, 10, 11, 14, 15, 16, 17, 18, 21] 5 @?= ([35, 37, 39, 40], [0, 5, 8, 18, 21])
   , testCase "  7" $ extractTwins [114, 115] 6 @?= ([121], [])
   , testCase "  8" $ extractTwins [121] 6 @?= ([], [121])
   , testCase "  9" $ extractTwins [1, 2, 4, 7, 11, 12, 15, 19] 5 @?= ([], [1, 2, 4, 7, 11, 12, 15, 19])
   , testCase " 10" $ extractTwins [1] 4 @?= ([], [1])
   , testCase " 11" $ extractTwins [16, 18, 20, 21] 4 @?= ([26], [16, 18])
   , testCase " 12" $ extractTwins [16] 4 @?= ([], [16])
   , testCase " 13" $ extractTwins [] 2 @?= ([], [])
   , testCase " 14" $ extractTwins [24] 4 @?= ([], [24])
   , testCase " 15" $ extractTwins [25] 4 @?= ([], [25])
   , testCase " 16" $ extractTwins [32, 33, 35, 38, 42, 43] 5 @?= ([48, 53], [35, 38])
   , testCase " 17" $ extractTwins [32, 34, 35, 41, 42, 43] 5 @?= ([49, 53], [32, 41])
   , testCase " 18" $ extractTwins [33, 35, 37, 38, 39, 42] 5 @?= ([51], [33, 35, 37, 42])
   , testCase " 19" $ extractTwins [33, 35, 38, 41] 5 @?= ([], [33, 35, 38, 41])
   , testCase " 20" $ extractTwins [34, 35, 37, 39, 40, 41] 5 @?= ([49, 52], [37, 39])
   , testCase " 21" $ extractTwins [] 4 @?= ([], [])
   , testCase " 22" $ extractTwins [48, 51, 53] 5 @?= ([], [48, 51, 53])
   , testCase " 23" $ extractTwins [49, 51, 53] 5 @?= ([], [49, 51, 53])
   , testCase " 24" $ extractTwins [49, 51] 5 @?= ([], [49, 51])
   , testCase " 25" $ extractTwins [49, 52, 53] 5 @?= ([58], [49])
   , testCase " 26" $ extractTwins [49] 5 @?= ([], [49])
   , testCase " 27" $ extractTwins [] 5 @?= ([], [])
   , testCase " 28" $ extractTwins [56] 5 @?= ([], [56])
   , testCase " 29" $ extractTwins [57] 5 @?= ([], [57])
   , testCase " 30" $ extractTwins [] 6 @?= ([], [])
   , testCase " 31" $ extractTwins [68, 69, 72, 74, 76, 77, 80, 81] 6 @?= ([98, 102, 104], [72, 74])
   , testCase " 32" $ extractTwins [6, 8, 9, 10, 16, 17, 18, 20, 24, 25, 26, 27, 30, 32, 33, 34, 35, 37] 6 @?= ([68, 72, 76, 77, 80, 81], [6, 10, 18, 20, 30, 37])
   , testCase " 33" $ extractTwins [98, 101, 102] 6 @?= ([], [98, 101, 102])
  ]

tests3 :: TestTree
tests3 = testGroup "swapIfDescendant tests"
  [
     testCase "1" $ swapIfDescendant (56, 56) (52, 50) 3 2 5 @?= 0
   , testCase "2" $ swapIfDescendant (57, 56) (22, 12) 3 0 5 @?= 8
   , testCase "3" $ swapIfDescendant (57, 56) (48, 50) 3 2 5 @?= 2
   , testCase "4" $ swapIfDescendant (48, 48) (22, 4) 2 0 5 @?= 0
   , testCase "5" $ swapIfDescendant (24, 24) (8, 6) 2 0 4 @?= 0
   , testCase "6" $ swapIfDescendant (24, 24) (18, 18) 2 1 4 @?= 0
   , testCase "7" $ swapIfDescendant (18, 18) (8, 6) 1 0 4 @?= 0
   , testCase "9" $ swapIfDescendant (56, 56) (21, 14) 3 0 5 @?= 0
   , testCase "a" $ swapIfDescendant (56, 56) (40, 38) 3 1 5 @?= 0
   , testCase "b" $ swapIfDescendant (56, 56) (51, 50) 3 2 5 @?= 0
   , testCase "c" $ swapIfDescendant (51, 50) (21, 14) 2 0 5 @?= 4
   , testCase "d" $ swapIfDescendant (51, 50) (40, 38) 2 1 5 @?= 2
   , testCase "e" $ swapIfDescendant (40, 36) (21, 4) 1 0 5 @?= 0
   , testCase "f" $ swapIfDescendant (56, 56) (20, 8) 3 0 5 @?= 0
   , testCase "g" $ swapIfDescendant (56, 56) (52, 50) 3 2 5 @?= 0
   , testCase "h" $ swapIfDescendant (120, 120) (101, 100) 4 2 6 @?= 0
   , testCase "i" $ swapIfDescendant (24, 24) (12, 4) 2 0 4 @?= 0
   , testCase "k" $ swapIfDescendant (52, 49) (24, 16) 2 0 5 @?= 20
  ]

tests4 :: TestTree
tests4 = testGroup "swapInRow tests"
  [
      testCase "0" $ swapInRow (51, 49) [Just (27, 14), Just (46, 38), Just (52, 50), Just (56, 56), Nothing] 2 @?= [Just (27, 6), Just (46, 34), Just (52, 50), Just (56, 56), Nothing]
    , testCase "1" $ swapInRow (19, 16) [Just (13, 6), Just (20, 18), Just (24, 24), Nothing] 1 @?= [Just (13, 0), Just (20, 18), Just (24, 24), Nothing]
    , testCase "2" $ swapInRow (114, 113) [Just (32, 20), Nothing, Just (105, 100), Nothing, Just (120, 120), Nothing] 3 @?= [Just (32, 12), Nothing, Just (105, 98), Nothing, Just (120, 120), Nothing]
    , testCase "3" $ swapInRow (99, 97) [Just (32, 12), Nothing, Just (105, 98), Nothing, Just (120, 120), Nothing] 2 @?= [Just (32, 4), Nothing, Just (105, 98), Nothing, Just (120, 120), Nothing]
    , testCase "4" $ swapInRow (114, 113) [Nothing, Just (80, 74), Just (105, 100), Nothing, Just (120, 120), Nothing] 3 @?= [Nothing, Just (80, 70), Just (105, 98), Nothing, Just (120, 120), Nothing]
    , testCase "5" $ swapInRow (39, 37) [Just (26, 14), Just (44, 38), Just (52, 50), Just (56, 56), Nothing] 1 @?= [Just (26, 10), Just (44, 38), Just (52, 50), Just (56, 56), Nothing]
    , testCase "6" $ swapInRow (101, 98) [Nothing, Just (84, 74), Just (102, 100), Nothing, Just (120, 120), Nothing] 2 @?= [Nothing, Just (84, 68), Just (102, 100), Nothing, Just (120, 120), Nothing]
    , testCase "7" $ swapInRow (50, 49) [Nothing, Just (40, 36), Nothing, Just (56, 56), Nothing] 2 @?= [Nothing, Just (40, 34), Nothing, Just (56, 56), Nothing]
    , testCase "8" $ swapInRow (51, 48) [Nothing, Just (45, 38), Just (52, 50), Just (56, 56), Nothing] 2 @?= [Nothing, Just (45, 32), Just (52, 50), Just (56, 56), Nothing]
    , testCase "9" $ swapInRow (50, 49) [Nothing, Just (42, 36), Nothing, Just (56, 56), Nothing] 2 @?= [Nothing, Just (42, 34), Nothing, Just (56, 56), Nothing]
    , testCase "a" $ swapInRow (11, 10) [Just (4, 6), Just (11, 10), Just (12, 12)] 1 @?= [Just (4, 4), Just (11, 10), Just (12, 12)]
    , testCase "b" $ swapInRow (50, 49) [Just (16, 10), Just (43, 36), Nothing, Just (56, 56), Nothing] 2 @?= [Just (16, 6), Just (43, 34), Nothing, Just (56, 56), Nothing]
    , testCase "c" $ swapInRow (57, 56) [Nothing, Nothing, Just (48, 50), Just (57, 56), Nothing] 3 @?= [Nothing, Nothing, Just (48, 48), Just (57, 56), Nothing]
    , testCase "d" $ swapInRow (18, 17) [Just (9, 4), Nothing, Just (24, 24), Nothing] 1 @?= [Just (9, 2), Nothing, Just (24, 24), Nothing]
    , testCase "e" $ swapInRow (101, 97) [Nothing, Just (82, 74), Just (104, 100), Nothing, Just (120, 120), Nothing] 2 @?= [Nothing, Just (82, 66), Just (104, 100), Nothing, Just (120, 120), Nothing]
    , testCase "f" $ swapInRow (51, 50) [Nothing, Just (43, 38), Just (51, 50), Just (56, 56), Nothing] 2 @?= [Nothing, Just (43, 36), Just (51, 50), Just (56, 56), Nothing]
    , testCase "g" $ swapInRow (50, 49) [Just (24, 10), Just (42, 36), Nothing, Just (56, 56), Nothing] 2 @?= [Just (24, 6), Just (42, 34), Nothing, Just (56, 56), Nothing]
    , testCase "h" $ swapInRow (35, 33) [Just (24, 6), Just (42, 34), Nothing, Just (56, 56), Nothing] 1 @?= [Just (24, 2), Just (42, 34), Nothing, Just (56, 56), Nothing]
    , testCase "i" $ swapInRow (41, 39) [Just (27, 18), Just (44, 40), Nothing, Nothing, Nothing] 1 @?= [Just (27, 14), Just (44, 40), Nothing, Nothing, Nothing]
    , testCase "j" $ swapInRow (50, 49) [Nothing, Just (38, 36), Nothing, Just (56, 56), Nothing] 2 @?= [Nothing, Just (38, 34), Nothing, Just (56, 56), Nothing]
    , testCase "k" $ swapInRow (20, 17) [Just (14, 8), Nothing, Nothing, Just (28, 28)] 1 @?= [Just (14, 2), Nothing, Nothing, Just (28, 28)]
    , testCase "l" $ swapInRow (101, 97) [Nothing, Just (84, 74), Just (103, 100), Nothing, Just (120, 120), Nothing] 2 @?= [Nothing, Just (84, 66), Just (103, 100), Nothing, Just (120, 120), Nothing]
    , testCase "m" $ swapInRow (50, 49) [Nothing, Just (42, 36), Nothing, Just (56, 56), Nothing] 2 @?= [Nothing, Just (42, 34), Nothing, Just (56, 56), Nothing]
    , testCase "n" $ swapInRow (50, 49) [Just (17, 10), Just (38, 36), Nothing, Just (56, 56), Nothing] 2 @?= [Just (17, 6), Just (38, 34), Nothing, Just (56, 56), Nothing]
    , testCase "o" $ swapInRow (58, 57) [Just (24, 16), Nothing, Nothing, Nothing, Nothing] 3 @?= [Just (24, 8), Nothing, Nothing, Nothing, Nothing]
    , testCase "p" $ swapInRow (50, 49) [Just (24, 8), Nothing, Nothing, Nothing, Nothing] 2 @?= [Just (24, 4), Nothing, Nothing, Nothing, Nothing]
  ]

tests5 :: TestTree
tests5 = testGroup "swapCollapses tests"
  [

     testCase "0" $ swapCollapses [[(2,1),(9,4),(19,15),(26,25),(31,28),(42,32),(75,59),(84,79),(99,91)],[(135,129),(138,137),(142,141),(151,145),(161,153),(167,163),(176,168)],[(198,197),(212,208)],[(226,225),(232,229)],[],[],[]]
                                  [Nothing,Nothing,Nothing,Just (235,230),Just (240,242),Just (249,248),Nothing] @?=
                                  [Nothing,Nothing,Nothing,Just (235,226),Just (240,240),Just (249,248),Nothing]
   , testCase "1" $ swapCollapses [[(5,2),(11,6),(26,19),(33,31),(36,35),(41,38),(48,42),(53,50),(58,57),(63,61),(71,67),(80,72),(92,85)],[(131,128),(140,136),(145,142),(149,146),(153,151),(156,154),(161,159),(164,163),(170,168),(176,172)],[(196,193),(201,198),(205,202),(209,206),(214,213)],[(227,225),(231,228)],[(242,241)],[],[]]
                                  [Just (99,40),Nothing,Nothing,Just (234,228),Nothing,Just (248,248),Nothing] @?=
                                  [Just (99,8),Nothing,Nothing,Just (234,226),Nothing,Just (248,248),Nothing]
   , testCase "2" $ swapCollapses [[(2,1),(6,4),(18,10),(22,21),(27,24),(35,29),(39,36),(53,49),(60,59),(73,68),(91,74)],[(130,129),(138,134),(142,141),(152,147),(160,156),(165,163)],[(198,193),(206,200),(216,208)],[(232,229)],[(242,241)],[],[]]
                                  [Nothing,Just (175,144),Nothing,Nothing,Nothing,Just (248,248),Nothing] @?=
                                  [Nothing,Just (175,136),Nothing,Nothing,Nothing,Just (248,248),Nothing]
   , testCase "3" $ swapCollapses [[(6,4),(11,9),(14,12),(20,18),(28,25),(32,30),(43,40),(58,46),(65,61),(69,66),(76,75),(88,83),(99,91)],[(132,131),(139,135),(143,141),(152,149),(158,155),(163,160),(167,164),(175,172)],[(195,192),(198,197),(208,204),(212,211)],[(226,225),(230,228)],[(242,241)],[],[]]
                                  [Nothing,Nothing,Just (214,202),Just (233,228),Nothing,Just (248,248),Nothing] @?=
                                  [Nothing,Nothing,Just (214,198),Just (233,226),Nothing,Just (248,248),Nothing]
   , testCase "4" $ swapCollapses [[(35,28),(51,47)],[(152,145)],[(210,204)],[],[],[],[]]
                                  [Just (76,94),Just (167,174),Just (216,214),Just (232,234),Just (245,244),Nothing,Nothing] @?=
                                  [Just (76,76),Just (167,166),Just (216,210),Just (232,232),Just (245,244),Nothing,Nothing]
   , testCase "5" $ swapCollapses [[(8,0),(17,15),(32,24),(47,45),(57,49),(69,67),(82,72),(96,91)],[(133,129),(140,134),(150,142),(161,156),(168,165)],[(195,193),(204,198),(210,207)],[(227,225)],[],[],[]]
                                  [Nothing,Just (173,140),Nothing,Just (231,226),Just (240,240),Nothing,Nothing] @?=
                                  [Nothing,Just (173,132),Nothing,Just (231,226),Just (240,240),Nothing,Nothing]
   , testCase "6" $ swapCollapses [[(10,6),(25,19),(31,26),(48,35),(60,50),(75,71)],[(137,130),(145,140),(153,148),(163,158)],[(198,192),(207,203)],[(229,225)],[],[],[]]
                                  [Just (82,18),Just (175,136),Nothing,Nothing,Just (240,240),Nothing,Nothing] @?=
                                  [Just (82,4),Just (175,136),Nothing,Nothing,Just (240,240),Nothing,Nothing]
   , testCase "7" $ swapCollapses [[(3,0),(9,7),(12,10),(16,15),(21,19),(30,23),(35,32),(46,41),(52,48),(61,59),(69,62),(78,77),(83,80),(86,85),(94,90),(99,96)],[(131,129),(135,132),(139,136),(142,141),(148,145),(155,151),(160,158),(165,162),(168,167),(174,171)],[(194,193),(198,197),(203,201),(209,205),(213,210)],[(226,225),(233,229)],[],[],[]]
                                  [Nothing,Just (176,154),Just (214,204),Nothing,Just (240,242),Just (249,248),Nothing] @?=
                                  [Nothing,Just (176,134),Just (214,194),Nothing,Just (240,240),Just (249,248),Nothing]
   , testCase "8" $ swapCollapses [[(4,3),(21,10),(24,23),(33,26),(38,34),(47,43),(51,48),(58,52),(69,66),(74,73),(82,77),(90,84),(94,93)],[(139,128),(145,140),(150,147),(156,153),(163,159),(174,165)],[(196,193),(202,200),(208,206)],[(228,226)],[],[],[]]
                                  [Nothing,Just (176,158),Just (213,206),Just (233,230),Just (243,242),Just (248,248),Nothing] @?=
                                  [Nothing,Just (176,150),Just (213,202),Just (233,228),Just (243,242),Just (248,248),Nothing]
  ]

tests6 :: TestTree
tests6 = testGroup "rootPosition tests"
  [
      testCase "1" $ rootPosition 5 0 3 @?= 4
    , testCase "2" $ rootPosition 5 1 3 @?= 10
    , testCase "3" $ rootPosition 5 2 3 @?= 12
    , testCase "4" $ rootPosition 29 0 5 @?= 28
    , testCase "5" $ rootPosition 29 1 5 @?= 46
    , testCase "6" $ rootPosition 29 2 5 @?= 54
    , testCase "7" $ rootPosition 29 3 5 @?= 58
    , testCase "8" $ rootPosition 29 4 5 @?= 60
    , testCase "9" $ rootPosition 31 0 5 @?= 30
    , testCase "a" $ rootPosition 31 1 5 @?= 46
    , testCase "b" $ rootPosition 31 2 5 @?= 54
    , testCase "c" $ rootPosition 31 3 5 @?= 58
  ]

tests7 :: TestTree
tests7 = testGroup "makeSwaps tests"
  [
      testCase "0" $ makeSwaps [] False 60 @?= []
    , testCase "1" $ makeSwaps [2, 7, 9, 15, 19, 24, 28, 31, 33, 35, 36, 41] False 42 @?= [(6, 2), (14, 9), (25, 19), (30, 28), (34, 33), (40, 36)]
    , testCase "2" $ makeSwaps [72, 79, 81] False 84 @?= [(78, 72)]
    , testCase "3" $ makeSwaps [97, 99, 104] False 106 @?= [(98, 97)]
    , testCase "4" $ makeSwaps [113, 115] False 116 @?= [(114, 113)]
    , testCase "5" $ makeSwaps [121] False 122 @?= []
    , testCase "6" $ makeSwaps [] False 124 @?= []
    , testCase "7" $ makeSwaps [5, 9, 10, 12, 17, 27] True 28 @?= [(8, 5), (13, 10), (26, 17)]
    , testCase "8" $ makeSwaps [32, 35, 36, 41, 45] False 46 @?= [(34, 32), (40, 36)]
    , testCase "9" $ makeSwaps [49, 51] False 54 @?= [(50, 49)]
    , testCase "a" $ makeSwaps [57] False 58 @?= []
    , testCase "b" $ makeSwaps [] False 60 @?= []
    , testCase "c" $ makeSwaps [2, 7, 9, 11] False 12 @?= [(6, 2), (10, 9)]
  ]

tests8 :: TestTree
tests8 = testGroup "makeCollapse tests"
  [
      testCase "0" $ makeCollapse [0, 4, 9, 12, 25, 26] True 0 29 13 5 @?= Just (28, 12)
    , testCase "1" $ makeCollapse [37, 41, 43, 45] False 1 29 13 5 @?= Nothing
    , testCase "2" $ makeCollapse [49, 51, 52] False 2 29 13 5 @?= Just (53, 50)
    , testCase "3" $ makeCollapse [57] False 3 29 13 5 @?= Just (56, 56)
    , testCase "4" $ makeCollapse [] False 4 29 13 5 @?= Nothing
    , testCase "5" $ makeCollapse [3, 6, 14, 17, 19, 20] True 0 31 17 5 @?= Just (30, 16)
    , testCase "6" $ makeCollapse [35, 37, 40] True 1 31 17 5 @?= Nothing
    , testCase "7" $ makeCollapse [50, 53] False 2 31 17 5 @?= Nothing
    , testCase "8" $ makeCollapse [] False 3 31 17 5 @?= Nothing
    , testCase "9" $ makeCollapse [0, 3, 4] False 0 8 5 3 @?= Just (5, 4)
    , testCase "a" $ makeCollapse [9, 10] False 1 8 5 3 @?= Nothing
    , testCase "b" $ makeCollapse [13] False 2 8 5 3 @?= Just (12, 12)
  ]

tests9 :: TestTree
tests9 = testGroup "makeSwapNextDels tests"
  [
      testCase "0" $ makeSwapNextDels [8, 11, 13, 14, 17, 18, 20] False 5 @?= [37, 39, 41, 42]
    , testCase "1" $ makeSwapNextDels [32, 35, 37, 39, 41, 44] False 5 @?= [49, 51, 54]
    , testCase "2" $ makeSwapNextDels [49, 51, 53] False 5 @?= [57, 58]
    , testCase "3" $ makeSwapNextDels [] False 5 @?= []
    , testCase "4" $ makeSwapNextDels [4, 10, 12, 14, 19, 24, 26, 28] True 5 @?= [37, 39, 44, 46]
    , testCase "5" $ makeSwapNextDels [35, 37, 39, 44] False 5 @?= [50, 54]
    , testCase "6" $ makeSwapNextDels [48, 50] False 5 @?= [57]
    , testCase "7" $ makeSwapNextDels [0, 3, 4] False 3 @?= [9, 10]
    , testCase "8" $ makeSwapNextDels [9, 10] False 3 @?= [13]
    , testCase "9" $ makeSwapNextDels [13] False 3 @?= [14]
    , testCase "a" $ makeSwapNextDels [57] False 5 @?= [60]
    , testCase "b" $ makeSwapNextDels [57] True 5 @?= []
  ]

testsA :: TestTree
testsA = testGroup "remTransPre tests"
  [
      testCase "0" $ remTransPre [1, 2, 7, 11, 12, 13, 14, 15, 16, 17, 20, 22, 23, 24, 25, 27, 28, 37, 40, 41, 42] 43 6 @?= ([[(3, 1), (10, 7), (26, 20), (36, 28)], [(68, 65), (74, 72)], [(103, 101)], [(114, 113)], [], []], [Nothing, Just (83, 74), Just (104, 100), Nothing, Just (120, 120), Nothing])
    , testCase "1" $ remTransPre [] 0 5 @?= (replicate 5 [], [Nothing, Nothing, Nothing, Nothing, Nothing])
    , testCase "2" $ remTransPre [0, 1, 2, 3, 4, 6, 12, 13, 15, 17, 18, 19, 20, 22, 24, 26, 27, 28] 29 5 @?= ([[(7, 4), (16, 15), (23, 20)], [(39, 35)], [(50, 48)], [], []], [Just (25, 10), Just (42, 36), Nothing, Just (56, 56), Nothing])
    , testCase "3" $ remTransPre [] 0 4 @?= (replicate 4 [], replicate 4 Nothing)
    , testCase "4" $ remTransPre [2, 3, 5, 8, 10, 11] 12 4 @?= ([[(9, 5)], [], [], []], [Nothing, Just (16, 18), Just (25, 24), Nothing])
    , testCase "5" $ remTransPre [] 0 5 @?= (replicate 5 [], replicate 5 Nothing)
    , testCase "6" $ remTransPre [0, 1, 3, 4, 6, 8, 9, 11, 12, 13, 14, 15, 18, 19, 22, 25, 26, 27] 28 5 @?= ([[(5, 3), (10, 6), (24, 22)], [(35, 32)], [(53, 49)], [], []], [Nothing, Just (40, 36), Nothing, Just (56, 56), Nothing])
    , testCase "7" $ remTransPre [] 0 5 @?= (replicate 5 [], replicate 5 Nothing)
    , testCase "8" $ remTransPre [1, 3, 5, 8, 9, 10, 12] 18 5 @?= ([[(2, 1), (11, 5)], [(39, 33)], [], [], []], [Just (13, 10), Just (40, 36), Nothing, Just (56, 56), Nothing])
    , testCase "9" $ remTransPre [] 0 3 @?= (replicate 3 [], replicate 3 Nothing)
    , testCase "a" $ remTransPre [0, 2, 3] 5 3 @?= ([[(4, 0)], [], []], [Nothing, Just (8, 8), Nothing])
    , testCase "b" $ remTransPre [] 0 5 @?= (replicate 5 [], replicate 5 Nothing)
    , testCase "c" $ remTransPre [3, 4, 6, 7, 11, 12, 13, 15, 16, 17, 18, 19, 20, 24, 27, 28] 29 5 @?= ([[(5, 3), (14, 11), (25, 20)], [], [(50, 49)], [], []], [Just (26, 12), Nothing, Just (53, 50), Just (56, 56), Nothing])
    , testCase "d" $ remTransPre [] 0 5 @?= (replicate 5 [], replicate 5 Nothing)
    , testCase "e" $ remTransPre [1, 3, 5, 6, 7, 9, 11, 17, 20, 25, 26, 28, 29, 30] 31 5 @?= ([[(2, 1), (8, 5), (16, 11), (24, 20)], [(34, 33), (41, 36)], [(53, 49)], [], []], [Just (27, 16)] ++ replicate 4 Nothing)
    , testCase "f" $ remTransPre [] 0 3 @?= (replicate 3 [], replicate 3 Nothing)
    , testCase "g" $ remTransPre [0, 3, 4] 8 3 @?= ([[(2, 0)], [(11, 9)], []], [Just (5, 4), Nothing, Just (12, 12)])
  ]

testsB :: TestTree
testsB = testGroup "remTrans2 tests"
  [
      testCase "0" $ remTrans2 [3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 17, 19, 20, 21, 22, 23, 26] 29 5 @?= [[(16, 3), (27, 19), (28, 10)], [(41, 38), (44, 36)], [(51, 49)], [], []]
    , testCase "1" $ remTrans2 [0, 2, 3, 4, 5, 11] 12 4 @?= [[(10, 0)], [(19, 17), (20, 18)], [], []]
    , testCase "2" $ remTrans2 [1, 2, 3, 4, 5, 7, 8, 9, 10, 11, 13, 16, 19, 20, 22, 23, 24, 27] 28 5 @?= [[(6, 1), (17, 13), (21, 19), (26, 24)], [(41, 33), (44, 36)], [(51, 49)], [], []]
    , testCase "3" $ remTrans2 [1, 2, 5, 8, 11, 12, 15] 18 5 @?= [[(3, 1), (9, 5), (13, 11), (14, 2)], [(37, 33), (40, 36)], [], [], []]
    , testCase "4" $ remTrans2 [0, 2, 3] 5 3 @?= [[(4, 0)], [], []]
    , testCase "5" $ remTrans2 [0, 1, 5, 7, 8, 9, 10, 12, 14, 16, 17, 20, 21, 25, 26, 27] 29 5 @?= [[(6, 5), (13, 10), (24, 14), (28, 12)], [(34, 32), (39, 36), (43, 40)], [(50, 49), (52, 50)], [], []]
    , testCase "6" $ remTrans2 [2, 3, 4, 5, 9, 11, 13, 18, 19, 20, 21, 22, 23, 25] 31 5 @?= [[(10, 9), (24, 13), (30, 10)], [(35, 33), (40, 37), (46, 44)], [(54, 49)], [], []]
    , testCase "7" $ remTrans2 [3, 6, 7] 8 3 @?= [[], [(10, 9)], []]
  ]

testsD :: TestTree
testsD = testGroup "inForest tests"
  [
      testCase "1" $ inForest 102 33 6 @?= True
    , testCase "2" $ inForest 103 33 6 @?= True
    , testCase "3" $ inForest 96 33 6 @?= True
    , testCase "4" $ inForest 98 33 6 @?= True
    , testCase "19" $ inForest 124 33 6 @?= True
    , testCase "20" $ inForest 126 33 6 @?= False
    , testCase "21" $ inForest 35 29 5 @?= True
    , testCase "31" $ inForest 57 29 5 @?= True
    , testCase "32" $ inForest 59 29 5 @?= False
    , testCase "33" $ inForest 56 29 5 @?= True
    , testCase "37" $ inForest 60 29 5 @?= True
    , testCase "38" $ inForest 62 29 5 @?= False
    , testCase "39" $ inForest 4 4 2 @?= True
    , testCase "40" $ inForest 6 4 2 @?= True
    , testCase "41" $ inForest 32 25 5 @?= True
    , testCase "57" $ inForest 60 25 5 @?= True
    , testCase "58" $ inForest 61 25 5 @?= False
    , testCase "59" $ inForest 62 25 5 @?= False
    , testCase "60" $ inForest 32 25 5 @?= True
    , testCase "75" $ inForest 60 25 5 @?= True
    , testCase "76" $ inForest 62 25 5 @?= False
    , testCase "77" $ inForest 14 8 3 @?= True
    , testCase "78" $ inForest 33 24 5 @?= True
    , testCase "79" $ inForest 37 24 5 @?= True
    , testCase "95" $ inForest 61 24 5 @?= False
    , testCase "96" $ inForest 60 24 5 @?= True
    , testCase "97" $ inForest 62 24 5 @?= False
    , testCase "98" $ inForest 18 11 4 @?= True
    , testCase "99" $ inForest 20 11 4 @?= True
    , testCase "100" $ inForest 25 11 4 @?= True
    , testCase "101" $ inForest 25 11 4 @?= True
    , testCase "102" $ inForest 26 11 4 @?= False
    , testCase "103" $ inForest 28 11 4 @?= True
    , testCase "104" $ inForest 30 11 4 @?= False
    , testCase "105" $ inForest 33 23 5 @?= True
    , testCase "118" $ inForest 57 23 5 @?= True
    , testCase "119" $ inForest 58 23 5 @?= False
    , testCase "120" $ inForest 60 23 5 @?= True
    , testCase "121" $ inForest 60 23 5 @?= True
    , testCase "122" $ inForest 60 23 5 @?= True
    , testCase "123" $ inForest 62 23 5 @?= False
    , testCase "124" $ inForest 17 14 4 @?= True
    , testCase "129" $ inForest 28 14 4 @?= True
    , testCase "130" $ inForest 30 14 4 @?= False
    , testCase "131" $ inForest 65 42 6 @?= True
    , testCase "138" $ inForest 100 42 6 @?= True
    , testCase "139" $ inForest 103 42 6 @?= True
    , testCase "140" $ inForest 101 42 6 @?= True
    , testCase "141" $ inForest 96 42 6 @?= True
    , testCase "142" $ inForest 98 42 6 @?= True
    , testCase "143" $ inForest 100 42 6 @?= True
    , testCase "163" $ inForest 120 42 6 @?= True
    , testCase "164" $ inForest 121 42 6 @?= True
    , testCase "165" $ inForest 122 42 6 @?= False
    , testCase "166" $ inForest 124 42 6 @?= True
    , testCase "171" $ inForest 124 42 6 @?= True
    , testCase "172" $ inForest 126 42 6 @?= False
    , testCase "173" $ inForest 66 35 6 @?= True
    , testCase "194" $ inForest 126 35 6 @?= False
    , testCase "195" $ inForest 33 29 5 @?= True
    , testCase "216" $ inForest 61 29 5 @?= False
    , testCase "217" $ inForest 60 29 5 @?= True
    , testCase "218" $ inForest 60 29 5 @?= True
    , testCase "219" $ inForest 62 29 5 @?= False
    , testCase "223" $ inForest 28 13 4 @?= True
    , testCase "224" $ inForest 30 13 4 @?= False
    , testCase "230" $ inForest 96 38 6 @?= True
    , testCase "231" $ inForest 100 38 6 @?= True
    , testCase "232" $ inForest 103 38 6 @?= True
    , testCase "233" $ inForest 96 38 6 @?= True
    , testCase "234" $ inForest 97 38 6 @?= True
    , testCase "235" $ inForest 98 38 6 @?= True
    , testCase "236" $ inForest 101 38 6 @?= True
    , testCase "237" $ inForest 103 38 6 @?= True
    , testCase "238" $ inForest 112 38 6 @?= True
    , testCase "257" $ inForest 126 38 6 @?= False
    , testCase "265" $ inForest 28 13 4 @?= True
    , testCase "266" $ inForest 30 13 4 @?= False
    , testCase "267" $ inForest 32 29 5 @?= True
    , testCase "283" $ inForest 60 29 5 @?= True
    , testCase "284" $ inForest 61 29 5 @?= False
    , testCase "285" $ inForest 60 29 5 @?= True
    , testCase "286" $ inForest 62 29 5 @?= False
    , testCase "287" $ inForest 32 25 5 @?= True
    , testCase "288" $ inForest 34 25 5 @?= True
    , testCase "289" $ inForest 37 25 5 @?= True
    , testCase "290" $ inForest 40 25 5 @?= True
    , testCase "307" $ inForest 60 25 5 @?= True
    , testCase "308" $ inForest 61 25 5 @?= False
    , testCase "309" $ inForest 62 25 5 @?= False
    , testCase "310" $ inForest 34 29 5 @?= True
    , testCase "311" $ inForest 40 29 5 @?= True
    , testCase "326" $ inForest 60 29 5 @?= True
    , testCase "327" $ inForest 61 29 5 @?= False
    , testCase "328" $ inForest 60 29 5 @?= True
    , testCase "329" $ inForest 61 29 5 @?= False
    , testCase "330" $ inForest 62 29 5 @?= False
    , testCase "331" $ inForest 32 21 5 @?= True
    , testCase "339" $ inForest 60 21 5 @?= True
    , testCase "340" $ inForest 62 21 5 @?= False
    , testCase "341" $ inForest 16 11 4 @?= True
    , testCase "346" $ inForest 24 11 4 @?= True
    , testCase "347" $ inForest 26 11 4 @?= False
    , testCase "348" $ inForest 28 11 4 @?= True
    , testCase "349" $ inForest 28 11 4 @?= True
    , testCase "350" $ inForest 28 11 4 @?= True
    , testCase "351" $ inForest 30 11 4 @?= False
    , testCase "352" $ inForest 16 16 4 @?= True
    , testCase "353" $ inForest 18 16 4 @?= True
    , testCase "378" $ inForest 60 24 5 @?= True
    , testCase "379" $ inForest 60 24 5 @?= True
    , testCase "380" $ inForest 61 24 5 @?= False
    , testCase "381" $ inForest 62 24 5 @?= False
    , testCase "382" $ inForest 69 43 6 @?= True
    , testCase "383" $ inForest 75 43 6 @?= True
    , testCase "410" $ inForest 124 43 6 @?= True
    , testCase "411" $ inForest 126 43 6 @?= False
    , testCase "412" $ inForest 34 29 5 @?= True
    , testCase "432" $ inForest 57 29 5 @?= True
    , testCase "433" $ inForest 60 29 5 @?= True
    , testCase "434" $ inForest 61 29 5 @?= False
    , testCase "435" $ inForest 60 29 5 @?= True
    , testCase "438" $ inForest 62 29 5 @?= False
    , testCase "443" $ inForest 28 12 4 @?= True
    , testCase "444" $ inForest 30 12 4 @?= False
    , testCase "445" $ inForest 32 28 5 @?= True
    , testCase "446" $ inForest 38 28 5 @?= True
    , testCase "464" $ inForest 60 28 5 @?= True
    , testCase "465" $ inForest 61 28 5 @?= False
    , testCase "466" $ inForest 62 28 5 @?= False
    , testCase "467" $ inForest 32 18 5 @?= True
    , testCase "483" $ inForest 60 18 5 @?= True
    , testCase "484" $ inForest 62 18 5 @?= False
    , testCase "485" $ inForest 8 5 3 @?= True
    , testCase "486" $ inForest 12 5 3 @?= True
    , testCase "487" $ inForest 14 5 3 @?= False
    , testCase "488" $ inForest 32 29 5 @?= True
    , testCase "489" $ inForest 37 29 5 @?= True
    , testCase "502" $ inForest 61 29 5 @?= False
    , testCase "503" $ inForest 60 29 5 @?= True
    , testCase "504" $ inForest 60 29 5 @?= True
    , testCase "505" $ inForest 60 29 5 @?= True
    , testCase "506" $ inForest 61 29 5 @?= False
    , testCase "507" $ inForest 62 29 5 @?= False
    , testCase "508" $ inForest 32 31 5 @?= True
    , testCase "524" $ inForest 59 31 5 @?= False
    , testCase "525" $ inForest 56 31 5 @?= True
    , testCase "528" $ inForest 59 31 5 @?= False
    , testCase "529" $ inForest 58 31 5 @?= True
    , testCase "534" $ inForest 61 31 5 @?= False
    , testCase "535" $ inForest 62 31 5 @?= False
    , testCase "looping" $ inForest 1023 431 9 @?= False
  ]

testsE :: TestTree
testsE = testGroup "childMany tests" [
    testCase "1" $ childMany 9 1 3 @?= 2
  , testCase "2" $ childMany 8 1 3 @?= 0
  , testCase "3" $ childMany 20 1 4 @?= 8
  , testCase "4" $ childMany 19 1 4 @?= 6
  , testCase "5" $ childMany 22 1 4 @?= 12
  , testCase "6" $ childMany 21 1 4 @?= 10
  , testCase "7" $ childMany 20 1 4 @?= 8
  , testCase "8" $ childMany 18 1 4 @?= 4
  , testCase "9" $ childMany 20 1 4 @?= 8
  , testCase "10" $ childMany 18 1 4 @?= 4
  , testCase "11" $ childMany 5 2 2 @?= 4
  , testCase "12" $ childMany 7 2 2 @?= 4
 ]

testsF :: TestTree
testsF = testGroup "detectRow tests" [
    testCase "0" $ detectRow 248 7 @?= 5
  , testCase "1" $ detectRow 180 7 @?= 1
  ]

unitTests :: TestTree -> TestTree
unitTests tree =
        testGroup "all tests" [
          tests,  tests2, tests3, tests4, tests5, tests6, tests7, tests8,
          tests9, testsA, testsB, testsD, testsE, testsF, tree
        ]
