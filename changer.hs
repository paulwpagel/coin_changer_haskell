import PunIt

penny = 1
nickel = 5
dime = 10
quarter = 25
denominations = [quarter, dime, nickel, penny]



change :: [Int] -> Int -> [Int] -> [Int]
change [] amount monies = monies
change (den:rest) amount monies | amount >= den = change ([den] ++ rest )(amount - den) (monies ++ [den])
                                | otherwise = change rest amount monies

main = do 
start
assertEquals (change denominations 1 []) [penny]
assertEquals (change denominations 2 []) [penny, penny]
assertEquals (change denominations 3 []) [penny, penny, penny]
assertEquals (change denominations 4 []) [penny, penny, penny, penny]
assertEquals (change denominations 5 []) [nickel]
assertEquals (change denominations 6 []) [nickel, penny]
assertEquals (change denominations 7 []) [nickel, penny, penny]
assertEquals (change denominations 10 []) [dime]
assertEquals (change denominations 23 []) [dime, dime, penny, penny, penny]
assertEquals (change denominations 25 []) [quarter]
assertEquals (change denominations 99 []) [quarter, quarter, quarter, dime, dime, penny, penny, penny, penny]
end