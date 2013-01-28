{- CIS194
   HW2
   Boyang Zhang
   zhangb@seas.upenn.edu
-}

{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where
import Log

{- Parses string into correct LogMessage format  -}
parseMessage :: String -> LogMessage
parseMessage s = case (words s) of
  []             -> Unknown ""
  ("I":ts:msg)   -> LogMessage Info (read ts) (unwords msg)
  ("W":ts:msg)   -> LogMessage Warning (read ts) (unwords msg)
  ("E":l:ts:msg) -> LogMessage (Error (read l)) (read ts) (unwords msg)
  list           -> Unknown (unwords list)

{- Creates list of LogMessages from a String  -}
parse :: String -> [LogMessage]
parse l = case (lines l) of
  []     -> []
  (h:tl) -> (parseMessage h):(parse (unlines tl))

{- Inserts into a binary search tree  -}
insert :: LogMessage -> MessageTree -> MessageTree
insert l1@(LogMessage _ ts1 _) (Node left lm2@(LogMessage _ ts2 _) right)
  | ts1 < ts2 = Node (insert l1 left) lm2 right
  | otherwise = Node left lm2 (insert l1 right)
insert l@(LogMessage _ _ _) Leaf        = Node Leaf l Leaf
insert _ mt = mt

{- Constructs a binary search tree from a LogMessage List  -}
build :: [LogMessage] -> MessageTree
build []      = Leaf
build (h:tl)  = buildTree (h:tl) Leaf

{- Helper function for building tree  -}
buildTree :: [LogMessage] -> MessageTree -> MessageTree
buildTree [] mt     = mt
buildTree (h:tl) mt = buildTree tl (insert h mt)

{- In order tree traversal  -}
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf   = []
inOrder (Node left l right) = (inOrder left) ++ [l] ++ (inOrder right)

{- Error message ordering -}
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong lm = case (inOrder (build lm)) of
  ((LogMessage (Error l) _ m):tl) ->
    if l >= 50 then m:(whatWentWrong tl)
    else whatWentWrong tl
  []                    -> []
  (_:tl)                    -> whatWentWrong tl