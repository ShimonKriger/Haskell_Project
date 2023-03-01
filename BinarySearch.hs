--Shimon Kriger
--T00381921
--This program is a Binary Search program that searches for an element in a list, and if the element is present, it returns the index
--of the element in the list.
myReverseL :: [a] -> [a] -> [a]
myReverseL list acc=
    case list of
        []-> acc
        (x:xs)-> myReverseL xs (x:acc)

myReverse :: [a] -> [a]
myReverse list =
    myReverseL list []

myLengthL :: Num int => [a] -> int -> int
myLengthL list acc =
    case list of
        []->acc
        (x:xs)-> myLengthL xs (acc + 1)
myLength :: Num int => [a] -> int
myLength list =
    myLengthL list 0

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


accessIndexL :: (Ord t, Eq t, Num t) => [a] -> t -> t -> a
accessIndexL list index acc=
    if acc == index
        then myHead list
    else accessIndexL (myTail list) index (acc + 1)

accessIndex :: (Ord t, Num t) => [a] -> t -> a
accessIndex list index=
    accessIndexL list index 0


binarySearchL :: (Ord a, Ord int, Num int, Integral int) => [a] -> a -> int -> int -> int
binarySearchL list elem start end
  | start >= end && accessIndex list start /= elem = -1
  | accessIndex list ((end + start)`div`2) == elem = (end + start) `div` 2
  | accessIndex list ((end + start)`div`2) > elem = binarySearchL list elem 0 ((end `div` 2) - 1)
  | accessIndex list ((end + start)`div`2) < elem = binarySearchL list elem ((end `div` 2) + 1) end
  | otherwise = -1


binarySearch :: (Ord a, Integral int) => [a] -> a -> int
binarySearch list elem =
    binarySearchL list elem 0 (myLength list)

main :: IO ()
main = do
    let list = ["aster","crab","crude","funny"]
    print (binarySearch list  "crude")