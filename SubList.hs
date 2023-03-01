--Shimon Kriger
--T00381921
--This program's purpose is to provide a function to truncate a list of elements based on the start and end points plugged in to the 
--mySubList function. mySubList relies on a lot of helper functions, as can be seen below.
mySubListL :: (Ord t, Num t) => [a] -> t -> t -> t -> [a] -> [a]
mySubListL list start end counter acc
  | null list = error "empty list"
  | counter < start = mySubListL list start end (counter + 1) acc
  | counter <= end = mySubListL list start end (counter + 1) (accessIndex list counter : acc)
  | otherwise = myReverse acc

mySubList :: (Ord t, Num t) => [a] -> t -> t -> [a]
mySubList list start end =
    mySubListL list start end 0 []
myReverseL :: [a] -> [a] -> [a]
myReverseL list acc=
    case list of
        []-> acc
        (x:xs)-> myReverseL xs (x:acc)

myReverse :: [a] -> [a]
myReverse list =
    myReverseL list []
accessIndexL :: (Ord t, Eq t, Num t) => [a] -> t -> t -> a
accessIndexL list index acc=
    if acc == index
        then myHead list
    else accessIndexL (myTail list) index (acc + 1)

accessIndex :: (Ord t, Num t) => [a] -> t -> a
accessIndex list index=
    accessIndexL list index 0
myHead :: [a] -> a
myHead list =
 case list of
    []-> error "empty list"
    (x:xs)-> x
myTail :: [a] -> [a]
myTail list =
    case list of
        []-> []
        (x:xs)-> xs

main :: IO ()
main = do
    let list = [1,2,3]
    print (mySubList list 0 2)