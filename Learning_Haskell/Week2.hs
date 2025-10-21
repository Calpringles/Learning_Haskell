--Week 2 Problems

--Write a function that counts the number of occurrences of an
--element in list
--Q1
popCount:: Eq a => a -> [a] -> Int 
popCount a [] = 0
popCount a (x:xs) = if x == a then 1 + popCount a xs else popCount a xs





--Q2
insertNoDup:: Ord a => a -> [a] -> [a]
insertNoDup a[] = [a]
insertNoDup a (x:xs)
    | a == x = x:xs
    | a < x = a:x:xs
    | otherwise = x: insertNoDup a xs



--Q3 
removeAll:: Eq a => a -> [a] -> [a]
removeAll a [] = []
removeAll a (x:xs)
    | a == x    = removeAll a xs  
    | otherwise = x : removeAll a xs 
{-
--Q4
treeFind2 ::Ord k => k -> KV k v -> Maybe v
treeFind2 = Undefined


treeInsert2 :: Ord k => k -> v -> KV k v -> KV k v
treeInsert2 = Undefined

--Q5
split :: [a] -> ([a],[a])
split = Undefined

merge :: Ord a => [a] -> [a] -> [a]
merge = Undefined

mergeSort :: Ord a => [a] -> [a]
mergeSort = Undefined

--Q6
makeChangeAll:: [Coin] -> [Coin] -> Int -> [[Coin]]
makeChangeAll = Undefined

--Q7
type Row = [String]
type Record = [(String,String)]

lookupField :: String -> Record -> Maybe String
lookupField fieldname record =
  error "lookupField: not implemented"

  rowToRecord :: [String] -> Row -> Maybe Record
rowToRecord header row =
  error "rowToRecord: not implemented"

  rowsToRecords :: [String] -> [Row] -> Maybe [Record]
rowsToRecords header rows =
  error "rowsToRecord: not implemented"

  recordToRow :: [String] -> Record -> Maybe Row
recordToRow header record =
  error "recordToRow: not implemented"

  recordsToRows :: [String] -> [Record] -> Maybe [Row]
recordsToRows header records =
  error "recordsToRows: not implemented"

-}