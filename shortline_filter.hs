main = interact onlyShortLines

onlyShortLines :: String -> String
onlyShortLines text =
  let allLines = lines text
      shortLines = filter (\l -> length l < 10) allLines
      result = unlines shortLines
  in result
