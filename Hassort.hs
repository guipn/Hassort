module Hassort (
    insertionSort,
    mergeSort,
    quickSort
) where


lesser :: (Ord a) => a -> [a] -> [a]
lesser x = filter (< x)

greaterEq :: (Ord a) => a -> [a] -> [a]
greaterEq x = filter (>= x)




insertionSort :: (Ord a) => [a] -> [a]
insertionSort []   = []
insertionSort list = insert (head list) (insertionSort (tail list))
    where insert x list = (lesser x list) ++ [x] ++ (greaterEq x list)



mergeSort :: (Ord a) => [a] -> [a]
mergeSort []   = []
mergeSort [x]  = [x]
mergeSort list = merge (sort left) (sort right)
                    where left  = take half list
                          right = drop half list
                          half  = length list `div` 2
                          sort  = mergeSort


merge :: (Ord a) => [a] -> [a] -> [a]
merge xs []  = xs
merge [] ys  = ys
merge one @ (x:xs) other @ (y:ys) = if x < y 
                                    then x : merge xs other
                                    else y : merge one ys



quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = (quickSort (lesser x xs)) ++ [x] ++ (quickSort (greaterEq x xs))


