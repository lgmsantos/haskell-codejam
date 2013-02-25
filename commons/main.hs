import FileCommons

main = do
    value <- matrixFromFile "input"
    print (value :: [[String]])