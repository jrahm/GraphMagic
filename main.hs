{-# LANGUAGE TupleSections #-}
module Main where

import Hw3

import Data.Graph.Inductive
import qualified Data.ByteString.Char8 as BS
import Control.Applicative
import Text.Printf
import Data.List

karateClub' :: IO (Gr Int ())
karateClub' = karateClub

karateClub :: (DynGraph gr) => IO (gr Int ())
karateClub = labeledGraph "karate_labels.txt" "karate_club_edges.txt"

labeledGraph' :: FilePath -> FilePath -> IO (Gr Int ())
labeledGraph' = labeledGraph

labeledGraph :: (DynGraph gr) => FilePath -> FilePath -> IO (gr Int ())
labeledGraph labelTxt edgesTxt = fmap undir $
    let str2node [a,b] = (read a, read b)
        str2node _ = error "Invaild syntax"
        str2edge [a,b] = (read a, read b, ())
        str2edge _ = error "Invaild syntax"
        in

    mkGraph
    <$> (map (str2node . words) . lines <$> readFile labelTxt)
    <*> (map (str2edge . words) . lines <$> readFile edgesTxt)

simpleGraph :: (DynGraph gr) => FilePath -> IO (gr () ())
simpleGraph path = fmap undir $ do
    let str2edge :: [String] -> (Int,Int,())
        str2edge [a,b] = (read a, read b, ())
        str2edge _ = error "Invaild syntax"
    edges' <- map (str2edge . words) . lines <$> readFile path
    let nodes' = nub (map (\(a,_,_)->(a,())) edges')
    return (mkGraph nodes' edges')

main :: IO ()
main = do
    -- jazz <- (simpleGraph "jazz.txt")::IO (Gr () ())
    -- print . snd =<< agglomerateA (\_ a i -> printf "%d %f\n" i a) jazz

    karate <- karateClub' 
    BS.writeFile "karate-social.dot" $ renderDotFile karate

    {- Just because ... -}
    printf "Modularity: %.3f\n" (modularity karate)

    (gr, modular) <- agglomerateA karate $ \g a i -> do
        {- To examine the iteration, we have a monadic function
         - to do it. This is why we need the applicative agglomerate -}
        printf "%d %.3f\n" i a

        {- Render a dot file so we can look back at it -}
        renderDotFile' g ("dots/karate_"++show i++".dot")

    print modular

    BS.writeFile "karate-calulated.dot" $ renderDotFile gr 
