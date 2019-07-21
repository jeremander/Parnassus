{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Parnassus.Search where

import Control.Monad.Identity (runIdentity)
import Control.Monad.Loops (iterateUntilM)
import Data.Foldable (asum, toList)
import Data.List (partition)
import Data.Maybe (fromJust)
import qualified Data.Sequence as S
import Data.Sort (sortOn)

import Parnassus.Utils (safeHead)


-- Priority Queue ("Beam") --

-- performs a binary search to get the index at which we would need to insert an element into a sorted sequence
bisect :: (Ord a) => Int -> (Int -> a) -> a -> Int
bisect len get elt = search (0, len)
    where
        search (i, j)
            | i >= j    = i
            | otherwise = search bounds
            where
                midpt = i + ((j - i) `quot` 2)
                elt' = get midpt
                bounds = if elt <= elt' then (i, midpt) else (midpt + 1, j)

-- data structure that maintains a sorted list of (priority, elt) pairs and imposes a max capacity
data Beam w a = Beam {
    capacity :: Int,
    queue :: S.Seq (w, a)
}
    deriving (Eq, Ord, Show)

empty :: Int -> Beam w a
empty capacity = Beam {capacity = capacity, queue = S.empty}

singleton :: Int -> (w, a) -> Beam w a
singleton capacity pair = Beam {capacity = capacity, queue = S.singleton pair}

popSmallest :: Beam w a -> (Maybe (w, a), Beam w a)
popSmallest beam@(Beam {capacity, queue}) = case smallest of
    S.EmptyL         -> (Nothing, beam)
    (elt S.:< queue') -> (Just elt, Beam {capacity = capacity, queue = queue'})
    where smallest = S.viewl queue

popLargest :: Beam w a -> (Maybe (w, a), Beam w a)
popLargest beam@(Beam {capacity, queue}) = case largest of
    S.EmptyR         -> (Nothing, beam)
    (queue' S.:> elt) -> (Just elt, Beam {capacity = capacity, queue = queue'})
    where largest = S.viewr queue

insert :: (Ord w) => (w, a) -> Beam w a -> Beam w a
insert (pri, elt) beam@(Beam {capacity, queue}) =
    if (capacity == 0) || ((n >= capacity) && (smallestPri >= pri))
        then beam
        else newQueue
    where
        n = S.length queue
        (smallestPri, smallestElt) S.:< queue' = S.viewl queue
        i = bisect (n - 1) (fst . fromJust . (queue' S.!?)) pri
        queue'' = S.insertAt i (pri, elt) (if (n < capacity) then queue else queue')
        newQueue = Beam {capacity = capacity, queue = queue''}

insertMany :: (Ord w) => [(w, a)] -> Beam w a -> Beam w a
insertMany pairs beam = foldr insert beam pairs

fromList :: (Ord w) => Int -> [(w, a)] -> Beam w a
fromList capacity pairs = Beam {capacity = capacity, queue = queue}
    where
        pairs' = reverse $ sortOn fst pairs
        queue = S.fromList $ reverse $ take capacity pairs'


-- Search Algorithms --

type NeighborGen s = s -> [s]
type NeighborGenM m s = s -> m [s]
type FinalStatePredicate s = s -> Bool
type TransitionCostFunc s c = s -> s -> c

-- monadic depth-first search
dfsM :: Monad m => NeighborGenM m s -> FinalStatePredicate s -> s -> m (Maybe [s])
dfsM next found initial = go [initial]
    where
        go path@(cur:rest) =
            if found cur
                then return $ Just path
                else do
                    neighbors <- next cur
                    paths <- sequence $ go . (: path) <$> neighbors
                    return $ case paths of
                        [] -> Nothing
                        otherwise -> asum paths

-- pure depth-first search
dfs :: NeighborGen s -> FinalStatePredicate s -> s -> Maybe [s]
dfs neighborGen found initial = runIdentity $ dfsM (return . neighborGen) found initial

-- beam search (returns all solutions stored, with their costs)
-- attempts to minimize total transition cost from the starting state
-- terminates when every queue entry is in a final state
beamSearchM :: forall c s m . (Num c, Ord c, Monad m) => Int -> NeighborGenM m s -> TransitionCostFunc s c -> FinalStatePredicate s -> s -> m [(c, [s])]
beamSearchM width neighborGen costFunc found initial = results
    where
        -- beam maintains the highest scoring paths (minimize cost = maximize score)
        -- maintain two beams, one for completed states, one for active states
        score :: s -> s -> c
        score x y = -(costFunc x y)
        activeBeam0 :: Beam c [s]
        activeBeam0 = singleton width (0, [initial])
        doneBeam0 :: Beam c [s]
        doneBeam0 = empty width
        step :: (Beam c [s], Beam c [s]) -> m (Beam c [s], Beam c [s])
        step (activeBeam, doneBeam) = do
            let (Just (pathScore, path), activeBeam') = popLargest activeBeam  -- get most promising path in the beam
            let state = head path
            neighbors <- neighborGen state  -- get neighbors of the best state
            let neighborScores = score state <$> neighbors  -- get scores of all transitions
            let pairs = [(pathScore + nbrScore, nbr : path) | (nbr, nbrScore) <- zip neighbors neighborScores]
            -- split paths by whether they have reached terminal states
            let (donePairs, activePairs) = partition (\(_, nbr:_) -> found nbr) pairs
            -- insert new paths into active/done beams
            let activeBeam'' = insertMany activePairs activeBeam'
            let doneBeam'' = insertMany donePairs doneBeam
            return (activeBeam'', doneBeam'')
        -- we are finished when the active beam is empty
        done :: (Beam c [s], Beam c [s]) -> Bool
        done (Beam {queue}, _) = S.length queue == 0
        results = do
            (_, doneBeamFinal) <- iterateUntilM done step (activeBeam0, doneBeam0)
            return $ reverse $ toList $ queue doneBeamFinal

beamSearch :: forall c s . (Num c, Ord c) => Int -> NeighborGen s -> TransitionCostFunc s c -> FinalStatePredicate s -> s -> [(c, [s])]
beamSearch width neighborGen costFunc found initial = runIdentity $ beamSearchM width (return . neighborGen) costFunc found initial

-- a greedy search is just a beam search with a width of 1
greedySearchM :: forall c s m . (Num c, Ord c, Monad m) => NeighborGenM m s -> TransitionCostFunc s c -> FinalStatePredicate s -> s -> m (Maybe (c, [s]))
greedySearchM neighborGen costFunc found initial = safeHead <$> beamSearchM 1 neighborGen costFunc found initial

greedySearch :: forall c s . (Num c, Ord c) => NeighborGen s -> TransitionCostFunc s c -> FinalStatePredicate s -> s -> Maybe (c, [s])
greedySearch neighborGen costFunc found initial = safeHead $ beamSearch 1 neighborGen costFunc found initial
        
