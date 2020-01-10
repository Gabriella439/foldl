import Test.DocTest

main :: IO ()
main = doctest ["-isrc", "src/Control/Foldl.hs", "src/Control/Scanl.hs"]
