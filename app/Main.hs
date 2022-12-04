module Main (main) where

import OpenSCAD.Vec

f :: forall shape. KnownShape shape => Vec shape -> IO ()
f x = do print x; print $ shape x; putStrLn ""

main :: IO ()
main = do
    let a = vec [[[1, 2, 3], [4, 5, 6], [7, 8, 9]], [[11, 12, 13], [14, 15, 16], [17, 18, 19]]] :: Vec '[2, 3, 3]
        b = vec [[1, 2, 3], [4, 5, 6], [7, 8, 9]] :: Vec '[3, 3]
        c = vec [2, 3, 4] :: Vec '[3]
        d = vec [5, 6, 7] :: Vec '[3]

    f $ (vec [1, 2, 3] :: Vec '[3]) #. (vec [4, 5, 6] :: Vec '[3])
    f $ a #. b
    f $ c #* d
    f $ c #+ d

    print $ (vec [1, 2, 3] :: Vec '[3]) #. (vec [4, 5, 6] :: Vec '[3])
