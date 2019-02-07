recaSeq :: Int -> [Int]
recaSeq 0 = [0]
recaSeq a = 
    let list        = recaSeq (a-1)
        val         = last list
        sub_res     = val - a
    in 
        if (sub_res > 0 && not (sub_res `elem` list)) then list ++ [sub_res]
        else    list ++ [val + a]

recaMan :: Int -> Int
recaMan a = last (recaSeq a)

recaList :: [Int] -> [Int]
recaList a = [recaMan x | x <- a]

