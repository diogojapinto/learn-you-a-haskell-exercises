{-
 - Lets implement the UNIX echo command
 - The program arguments are simply printed to the standard output.
 - If the first argument is -n, this argument is not printed, and no trailing newline is printed
 -}

import System.Random
  
main = do
    line <- getLine
    let arguments = words line
        firstArgument = head arguments
    if firstArgument == "-n"
    then putStr $ unwords $ tail arguments
    else putStrLn $ unwords arguments
    main


{- Write a lottery number picker
 - This function should take a StdGen instance, and produce a list of six unique numbers between 1 and 49, in numerical order
 -}
lottery :: StdGen -> [Int]
lottery gen = take 6 $ randomRs (1, 49) gen
