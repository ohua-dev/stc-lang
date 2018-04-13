-- | Original source https://github.com/iu-parfunc/lvars/tree/master/archived_old/fhpc13-lvars/benchmarks

import Data.Set

------------------------
--- The use cases in this file are all taken from the LVars FHPC paper.
------------------------



------------------------
--- This is the purely functional version of the problem.
--- The essential goal as described in the paper is to allow analyze to run as
--- as soon as the first item was found.
------------------------
nbrs :: Graph -> NodeLabel -> Set NodeLabel
-- ’nbrs g n’ is the neighbor nodes of ‘n’ in ‘g’
-- Traverse each level of the graph in parallel,
-- maintaining at each recursive step a set of
-- nodes that have been seen and a set of nodes
-- left to process.

bf_traverse :: Graph -> Set NodeLabel -> Set NodeLabel -> Set NodeLabel
bf_traverse g seen nu =
  if nu == {}
  then seen
  else let seen’ = union seen nu
           allNbr = fold union (map (nbrs g) nu)
           -- allNbr = parFold union (parMap (nbrs g) nu)
           nu’ = difference allNbr seen’
       in bf_traverse g seen’ nu’

runFunctional :: Graph -> NodeLabel -> Set NodeLabel
runFunctional profiles start =
  -- Next we traverse the connected component,
  -- starting with the vertex ‘profile0’:
  let ccmp = bf_traverse profiles {} {start} -- > can not work as a generator because it is a set not a list! (no lazyness possible)
      result = parMap analyze ccmp
  in result

algo :: Graph -> NodeLabel -> OhuaM s (Set NodeLabel)
algo profiles start = do
  ccmp <- liftSf 0 $ bf_traverse profiles {} {start}
  result <- smap (liftSf 1 $ analyze) ccmp
  return result

runOhua1 :: Graph -> NodeLabel -> Set NodeLabel
runOhua1 profiles start = fst $ runOhuaM (algo profiles start) [undefined, undefined]

runOhua2 :: Graph -> NodeLabel -> Set NodeLabel
runOhua2 profiles start = let ccmp = bf_traverse profiles {} {start}
  fst $ runOhuaM (smap (liftSf 0 $ analyze) ccmp) [undefined]

------------
--- In the streams-based version:
--- 1) When the data comes from a stateful function such as in runOhua1 then this function needs to turn into a generator.
---    -> can't do that without a proper notion of a generator!
------------

---------------
--- the solution of the problem of this use case is about data generation.
--- if the result of the bf_traverse function would be a incrementally computable
--- then it could also streamline the found items immediately.
data GenSet a = GenSet [a] Set a

add :: GenSet a -> a -> GenSet a
add (GenSet as sa) item | member item sa = GenSet as sa
add (GenSet as sa) item = GenSet (as ++ [item]) $ Set.insert item sa

head :: GenSet a -> a
head set = undefined
---
---------------

--- maybe it is in fact a problem that can be solved with Ohua when we implement bf_traverse in Ohua then it might become a generator automatically.

prepare :: Graph -> NodeLabel -> [NodeLabel]
prepare profiles start = let gen =
  [x | x <- bf_traverse profiles start ]

--- the question is: how do I define a data generator in Haskell?
--- I need some sort of continuation that stops at some point
--- here is an idea:
--- I make the bf_traverse function stateful instead of recursive.
data TraverseState a = TraverseState
                       Graph            -- the graph
                       (Set NodeLabel)  -- seen
                       (Set NodeLabel)  -- nu
                       [a]              -- found neighbors
bf_traverse :: State (TraverseState NodeLabel) (Maybe NodeLabel)
bf_traverse = do
  (TraverseState g seen nu found) <- get
  if nu == {}
  then return Nothing
  else case found of
    (x:xs) -> put $ TraverseState g seen nu xs >>= return Just x
    [] -> let  seen’ = union seen nu
              allNbr = fold union (map (nbrs g) nu)
                 nu’ = difference allNbr seen’
          in put $ TraverseState g seen' nu' nu' >>= bf_traverse
  where
    next = fold union (map (nbrs graph) nu)

--- now we could define a 'gen' combinator to realize that this thing needs to be called until it is exhausted.
--- but then, what is the difference between this thing and a normal list?!
--- so we could write something like this:
smap analyze $ smap id bf_traverse ~ smap analyze [x | x <- bf_traverse, y <- x*y]
--- composition of generators -> leads to being itself an algo again
--- it feels to me that this later pattern relates to something much more general than our Ohua monad.
bf_traverse1 :: a -> State ...
bf_traverse1 _ =
--- in this particular case, one could even write the program like so:
runOhuaM (smap (liftSf 0 analyze) $ smap (liftSf 1 bf_traverse1) $ (liftSf 2 allNodes)) [undefined, graph, graph]
--- here, reorderMap has type:
reorderMap :: (() -> State (TraverseState a) a) -> [a] -> [a]


--- how about just introducing a new type ...
newtype Generator s b = SF s () (Maybe b)
--- along with a new lift function for it:
liftSf :: Int -> Generator s a -> OhuaM s a
--- but what would that thing do????
--- -> you could technically lift it into the map computation just as a normal stateful function!
--- so it would be better to provide another smap version that works on a generator instead of an array.
--- one can probably generalize that such that arrays implement generators.
smap :: (NFData b, NFData s, Show a) => (a -> OhuaM (GlobalState s) b) -> Generator (GlobalState s) b -> OhuaM (GlobalState s) [b]
smap algo generator = undefined
--- now we could just say that every computation is like so: (generator >>= (\x -> algo x))
