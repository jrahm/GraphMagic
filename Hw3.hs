{-# LANGUAGE TupleSections#-}
module Hw3 where

import Control.Applicative
import Data.Graph.Inductive
import Data.Function
import Data.List
import Data.Ord
import Control.DeepSeq
import Control.Monad

import Debug.Trace
import Control.Parallel.Strategies
import Control.Monad.Writer.Lazy
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
-- import Text.Printf

labels :: (Graph gr, Eq g) => gr g a  -> [g]
labels gr = nub (map snd $ labNodes gr)

{- Function version of if. Why this isn't a thing
 - already, I don't know! -}
if' :: Bool -> a -> a -> a
if' True a _ = a
if' _ _ a = a

{- Return all combinations of elements in a list -}
pairs :: [a] -> [(a,a)]
pairs l = [(a,b) | a <- l, b <- l]

choosePairs :: [a] -> [(a,a)]
choosePairs [] = []
choosePairs as = map (head as,) as ++ choosePairs (tail as)

choosePairsNotEqual :: [a] -> [(a,a)]
choosePairsNotEqual [] = []
choosePairsNotEqual (a:as) = map (a,) as ++ choosePairsNotEqual as

{- Calculates the modularity of a graph -}
modularity :: (Graph gr) => gr Int a -> Double
modularity grph = 
        -- check a value against a maybe. False if the maybe is nothing
    let (==?) n = maybe False (n ==)
        -- Simply divide 2 integers and return a double
        (/!) = (/) `on` fromIntegral
        -- Number of edges. I'm bummed the graph library does not have this
        noEdg = (length . edges) grph
        -- Number of edges inside a group normalized
        edgesInside  grp = length [ edg | edg@(n1, n2) <- edges grph, (grp ==? lab grph n1) && (grp ==? lab grph n2)] /! noEdg
        -- Number of edges not inside, but associated with a group normalized
        edgesOutside  grp = length [ edg | edg@(n1, n2) <- edges grph,
            (grp ==? lab grph n1) && not (grp ==? lab grph n2) ||
            not (grp ==? lab grph n1) && (grp ==? lab grph n2)
            ] /! (2 * noEdg)
        -- The inside of the sum term for modularity
        f g = let inside = edgesInside g
                  outside = edgesOutside g
                  in inside  - (inside + outside)**2
    -- the final sum
    in sum $ map f $ labels grph

-- debugging
showGr :: (Graph gr, Show a) => gr a b -> String
showGr = show . labNodes

data NullMonad a = NullMonad a
instance Functor NullMonad where
    fmap f (NullMonad a) = NullMonad (f a)
instance Applicative NullMonad where
    pure = NullMonad
    (<*>) (NullMonad f) (NullMonad a) = NullMonad (f a)
instance Monad NullMonad where
    (>>) _ b = b
    (>>=) (NullMonad a) f = f a
    return = pure
runNullMonad :: NullMonad a -> a
runNullMonad (NullMonad a) = a

instance NFData (Gr a b) where
    rnf a = a `seq` () 

{- Agglomerate without the monad part -}
agglomerate :: (Show a) => Gr b a -> (Gr Int a, Double)
agglomerate = runNullMonad . flip agglomerateA (\_ _ _ -> NullMonad ())

{- Runs an aglomeration with a monad operating on every merge.
 - This is to be able to run the karate club merge -}
agglomerateA :: (Show a, Applicative m) => Gr b a -> (Gr Int a -> Double -> Int -> m ()) -> m (Gr Int a, Double)
agglomerateA graph f =
    let combine :: (Graph gr, Show a) => gr Int a -> Int -> Int -> gr Int a
        
        {- Take a graph and combines the two labels to return a new graph -}
        combine grph lab1 lab2 = 
            let gr = mkGraph
                 (map (\(nod, llab) -> (nod,) $ if' (llab == lab2) lab1 llab) (labNodes grph))
                 (labEdges grph) in gr
            
        {- Brief rundown:
         -     Has a maximal passed down throught the argumnts.
         -     Take the graph, generate a list of graphs with each pair of labels combined.
         -     Take the maximal across that list.
         -     If that max is greater than the current max, replace the current max
         -       in the recursive call.
         -     Otherwise, keep the original max, but still pass the new graph down
         -}
        agglomerate' maxs'@(_, maxMod) itr gr =
            -- get a list of all the possible graphs after the aglommeration
            let 
                myLabels = labels gr
                {- List of graphs with the pairs combined -}
                graphs' = map (uncurry (combine gr)) $ choosePairsNotEqual myLabels
                graphs = graphs' `using` parList rdeepseq
                {- Calculate and attach the modularities -}
                modulars' = map (\a -> (a,modularity a)) graphs
                modulars = modulars' `using` parList rdeepseq
                {- Select the maximal of the new graphs -}
                curmax@(maxGraph', maxMod') = maximumBy (comparing snd) modulars
                {- Select the best between the current best and the new
                 -  graph -}
                maxs2' = if' (maxMod'>maxMod) (maxGraph', maxMod') maxs'
                in 
                {- Check the base case of only 1 group left (or 0 for good measure) -}
                if' (null myLabels || null (tail myLabels))
                       (maxs' <$ uncurry f maxs' itr) 
                       (uncurry f curmax itr *> agglomerate' maxs2' (itr + 1) maxGraph')

        {- The graph sent in with except each node has been assigned to a
         - unique group -}
        newGraph = mkGraph (zip (nodes graph) [(0::Int)..]) (labEdges graph)
        mods = (newGraph, modularity newGraph)

    -- start the iteration with the new graph
    in (uncurry f mods 0) *> agglomerate' mods 1 newGraph

renderDotFile :: (Graph gr) => gr Int b -> BS.ByteString 
renderDotFile gr = (snd.runWriter) $ do
    let edges' = nub $ map (\(n1,n2,_)-> (max n1 n2, min n1 n2)) $ labEdges gr
    tell (BC.pack "graph {\n")
    tell (BC.pack "graph [splines=false];")
    tell (BC.pack "node [fixedsize=true,width=0.3,height=0.3,fontsize=6];")
    let colors = cycle (pairs [
         "red", "blue", "green", "purple",
         "brown", "pink", "yellow", "grey86", "grey27",
         "red4", "indigo", "DarkRed", "Ivory"])

    forM_ edges' $ \(n1,n2) ->
        tell $ BC.pack ("\t" ++ show n1 ++ " -- " ++ show n2 ++ "\n")
    forM_ (labNodes gr) $ \(n, g) ->
        let (c2, c1) = colors !! g in
        tell $ BC.pack ("\t" ++ show n ++ "[style=\"filled,solid\" fillcolor="++c1++" color=" ++c2++ "]\n")
    tell (BC.pack "}\n")

renderDotFile' :: (Graph gr) => gr Int b -> FilePath -> IO ()
renderDotFile' gr = flip BS.writeFile (renderDotFile gr)


-- clausetsFavoriteGraph, except with a concrete type
clausetsFavoriteGraph' :: Gr Int ()
clausetsFavoriteGraph' = clausetsFavoriteGraph

{- Aaron Clauset's favorite graph -}
clausetsFavoriteGraph :: (Graph gr) => gr Int ()
clausetsFavoriteGraph =
    mkGraph
        [ (0, 0), (1, 0), (2, 0), (3, 1), (4, 1), (5, 1) ]
        [ (0, 1, ()), (0, 2, ()),
          (1, 0, ()), (1, 2, ()),
          (2, 0, ()), (2, 1, ()), (2, 3, ()),
          (3, 2, ()), (3, 4, ()), (3, 5, ()),
          (4, 3, ()), (4, 5, ()),
          (5, 3, ()), (5, 4, ()) ]
