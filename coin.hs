import PunIt

penny = 1
nickel = 5
dime = 10
quarter = 25
denominations = [quarter, dime, nickel, penny]

change :: [Integer] -> Integer -> [Integer] -> [Integer]
change [] x monies = monies
change (am:ams) x monies | x >= am = change ([am] ++ ams) (x-am) (monies ++ [am])
                         | otherwise = change ams x monies
                          

main = do start
          assert_changed (change denominations 1 []) [penny]
          assert_changed (change denominations 2 []) [penny, penny]
          assert_changed (change denominations 3 []) [penny, penny, penny]
          assert_changed (change denominations 4 []) [penny, penny, penny, penny]
          assert_changed (change denominations 5 []) [nickel]
          assert_changed (change denominations 6 []) [nickel, penny]
          assert_changed (change denominations 14 []) [dime, penny, penny, penny, penny]
          assert_changed (change denominations 20 []) [dime, dime]
          assert_changed (change denominations 25 []) [quarter]
          assert_changed (change denominations 50 []) [quarter, quarter]
          assert_changed (change denominations 99 []) [quarter, quarter, quarter, dime, dime, penny, penny, penny, penny]
          end
