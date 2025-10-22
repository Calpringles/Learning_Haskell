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




data KV k v
  = Empty
  | Node (KV k v) k v (KV k v)
  deriving (Show, Eq)
--Q4
treeFind2 ::Ord k => k -> KV k v -> Maybe v
treeFind2 targetKey Empty = Nothing
treeFind2 targetKey (Node left key value right)
  | targetKey == key = Just value
  | targetKey < key  = treeFind2 targetKey left  
  | otherwise        = treeFind2 targetKey right 


treeInsert2 :: Ord k => k -> v -> KV k v -> KV k v
treeInsert2 newKey newValue Empty =
 
  Node Empty newKey newValue Empty

treeInsert2 newKey newValue (Node left key value right)
  | newKey == key =
 
    Node left newKey newValue right
  | newKey < key  =

    Node (treeInsert2 newKey newValue left) key value right
  | otherwise =

    Node left key value (treeInsert2 newKey newValue right)


--Q5
split :: [a] -> ([a],[a])
split [] = ([], [])
split [x] = ([x], [])
split (x:y:xs) = let (left, right) = split xs in (x:left, y:right)

--Q6
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x < y     = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs =
  let (left, right) = split xs
  in merge (mergeSort left) (mergeSort right)



--Q7
type Row = [String]
type Record = [(String,String)]

lookupField :: String -> Record -> Maybe String
lookupField fieldname [] = Nothing
lookupField fieldname ((nm, val):record) =
  if nm==fieldname then Just val else lookupField fieldname record
  

rowToRecord :: [String] -> Row -> Maybe Record
rowToRecord [] [] = Just []
rowToRecord(hdr:hdrs) (x:xs) =
  case rowToRecord hdrs xs of
    Just record -> Just ((hdr, x):record)
    Nothing -> Nothing

rowsToRecords :: [String] -> [Row] -> Maybe [Record]
rowsToRecords header [] = Just []
rowsToRecords header (row:rows) =
  case rowsToRecords header rows of
    Nothing -> Nothing
    Just records ->
      case rowToRecord header row of
        Nothing -> Nothing
        Just record ->
          Just (record:records)

recordToRow :: [String] -> Record -> Maybe Row
recordToRow [] record = Just []
recordToRow (f:fs) record =
  case lookupField f record of
    Nothing -> Nothing
    Just val ->
      case recordToRow fs record of
        Nothing -> Nothing
        Just row ->
          Just (val:row)

recordsToRows :: [String] -> [Record] -> Maybe [Row]
recordsToRows header [] = Just []
recordsToRows header (record:records) =
  case recordToRow header record of
    Nothing -> Nothing
    Just row ->
      case recordsToRows header records of
        Nothing -> Nothing
        Just rows ->
          Just (row:rows)