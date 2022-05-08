module TortoiseCombinators
       ( andThen 
       , loop 
       , invisibly 
       , retrace 
       , overlay 
       ) where

import Tortoise
------------------
-- start state for retrace as per posts on 
-- https://discourse.cse.unsw.edu.au/21t2/comp3141/t/retrace-confusion/816
-- https://discourse.cse.unsw.edu.au/21t2/comp3141/t/retrace-always-start-from-start-state/769

--------------- \\\\\\\\\\\\-----------------------

-- |Essentially concatenates instructions.
andThen :: Instructions -> Instructions -> Instructions
-- todo: figure out how to curry the value constructors,
-- potential curried functions??

-- pattern matching for all the value constructors 
-- call andThen recursively

-- pen constructors
andThen (PenUp inst1) inst2 = PenUp $ andThen inst1 inst2 
andThen (PenDown inst1) inst2 = PenDown $ andThen inst1 inst2

-- set constructors
andThen (SetStyle lstyle inst1) inst2 = SetStyle lstyle $ andThen inst1 inst2
andThen (SetColour colour inst1) inst2 = SetColour colour $ andThen inst1 inst2

-- drawing constructors
andThen (Move dist inst1) inst2 = Move dist $ andThen inst1 inst2
andThen (Turn ang inst1) inst2 = Turn ang $ andThen inst1 inst2

-- pattern matching for identity input Stop
andThen inst Stop = inst
andThen Stop inst = inst


--------------- \\\\\\\\\\\\-----------------------

-- |Loops instructions n times
loop :: Int -> Instructions -> Instructions
loop n i = 
       if n > 0 
         -- keep calling loop until n == 0
         then andThen i (loop (n-1) i)
         else Stop


--------------- \\\\\\\\\\\\-----------------------

-- |Helper function for invisibly. Returns instruction with correct pennState
invisibly' :: Bool -> Instructions -> Instructions
invisibly' pennState (PenUp inst) = PenUp $ invisibly' False inst
-- call PenUp when PenDown is called
invisibly' pennState (PenDown inst) = PenUp $ invisibly' True inst

-- ensure final state is always accurate
-- pennState is True ==> pen should be down at the end
invisibly' pennState Stop = 
              if pennState == True
                then PenDown Stop
                else Stop

invisibly' pennState (SetStyle lstyle inst) = SetStyle lstyle $ invisibly' pennState inst
invisibly' pennState (SetColour colour inst) = SetColour colour $ invisibly' pennState inst

invisibly' pennState (Move dist inst) = Move dist $ invisibly' pennState inst
invisibly' pennState (Turn ang inst ) = Turn ang $ invisibly' pennState inst

-- pen starts down in the initial state start.
-- |"Invisible" picture drawn. Replicate final state as if actually drawn.
invisibly :: Instructions -> Instructions
invisibly i = PenUp $ invisibly' True i


--------------- \\\\\\\\\\\\-----------------------

-- |To deal with PenUps followed by PenDowns and vice versa 
penCorrect :: Instructions -> Bool -> Instructions
penCorrect revInst pennState = 
  if pennState == True
    then PenDown revInst
    else PenUp revInst

-- final instruction is always PenDown Stop 
-- revInst prepends instructions 
-- |Helper function for retrace, takes extra arguments to keep track of state
retrace' :: Instructions -> LineStyle -> Colour -> Bool -> Instructions -> Instructions

-- retraced instructions
retrace' Stop _ _ _ revInst = revInst

-- retracing pen movements
retrace' (PenDown inst) lstyle colour pennState revInst = 
       retrace' inst lstyle colour True $ penCorrect revInst pennState

retrace' (PenUp inst) lstyle colour pennState revInst = 
       retrace' inst lstyle colour False $ penCorrect revInst pennState 

-- reverse styles and colours
retrace' (SetStyle instStyle inst) lstyle colour pennState revInst = 
       retrace' inst instStyle colour pennState $ SetStyle lstyle revInst

retrace' (SetColour instColour inst) lstyle colour pennState revInst = 
       retrace' inst lstyle instColour pennState $ SetColour colour revInst

-- move and turn instructions flipped
retrace' (Move dist inst) lstyle colour pennState revInst = 
       retrace' inst lstyle  colour pennState $ Move (- dist) revInst
       
retrace' (Turn ang inst) lstyle colour pennState revInst = 
       retrace' inst lstyle colour pennState $ Turn (- ang) revInst

{-
If a set of instructions i goes from state start to state σ 
and produces picture p, 
then the instructions retrace i will go from state σ to start 
and produce the picture reverse p
-}

-- |Retraces the picture to produce same pic with "reversed" instructions
retrace :: Instructions -> Instructions
retrace i = retrace' i (Solid 1) white True $ PenDown Stop


--------------- \\\\\\\\\\\\-----------------------

-- |Overlay of instructions given a list of instructions
overlay :: [Instructions] -> Instructions
-- end of List
overlay [] = Stop
-- compute inst and then get back to inst start and loop
overlay (inst : instructions) = andThen inst $ andThen (invisibly $ retrace inst) (overlay instructions)

--------------- \\\\\\\\\\\\-----------------------

-- ./dist/build/TortoiseTests/TortoiseTests

