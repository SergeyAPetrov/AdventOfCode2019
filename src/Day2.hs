module Day2 where












-- --solve :: Integral a => [String] -> a
-- solve1 = solve' getFuel

-- solve2 = solve' getFuelTotal

-- solve' f xs = sum $ map (f.read) xs

-- getFuel mass = mass `div` 3 - 2

-- --getFuelTotal :: (RealFrac a,Integral a, Show a) => a -> a
-- getFuelTotal mass =
--     sum masses
--     where
--         massSeq = iterate getFuel mass
--         masses = takeWhile (>0) $ drop 1 massSeq