import System.Environment (getArgs)

interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input,output] -> interactWith function input output
            _ -> putStrLn "error: exactly two arguments needed"
        
        myFunction = transposeInput

transposeInput input = unlines (transpose (lines input))

transpose lines | all null lines = []
transpose lines = lineT:transpose lines'
  where (lineT,lines') = unzip (map takeFirst lines)

takeFirst "" = (' ',"")
takeFirst (x:xs) = (x,xs)
