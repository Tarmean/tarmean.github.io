# Abstracting over Branch and Bound

## Introduction

Worlde is a Mastermind-like online game where you guess five-letter words. The game answers which letters are correct, in the wrong position, or flat out wrong. You have 6 guesses to win. This is a great balance because most people win most of the time, but I still feel immensely pleased for suceeding.  
But it creates an interesting meta-puzzle: what is the optimal Wordle strategy? Branch&Bound seems like the perfect approach here. Branch&Bound is an algorithm class I hadn't heard of for the longest time, probably because it is *really* vague. Lets explore what the essence of Branch&Bound is by abstracting over it, in Haskell. Maybe I should mention my final solution ended up being in Rust because it turned out immensely easier to write memory-efficient code in Rust. The Haskell version took 25GB of memory, Rust using bitsets used 50MB. So in my defense, any code-crimes perpetrated here are mostly theoretic.

Most descriptions of Branch&Bound concur on the following steps:

- We have some representation for subproblems
- We can split unsolved subproblems into smaller subproblems
- We can prune some subproblems that won't lead to an optimal solution

Admittedly that's more concrete than 'we use computers and algorithm to find a solution', but not by much. I will focus on what I consider the core which can be found in almost all Branch&Bound implementations:

- We try to give subproblems a minimum and maximum cost
- If we have two subproblems `a` and `b`, when `max_cost(a) < min_cost(b)` then we can prune `b` because we know it won't lead to an optimal solution

Much more concrete, and quite close to alpha-beta-pruning for game trees. For solved subproblems the `max_cost` and actual cost coincide so we don't have to bother writing a `max_cost` heuristic - we can use the smallest finished solution found so far. Many variants use a search strategy other than depth first, use smarter goal expansion, and add further pruning like plane cutting or dominance pruning. Surprisingly many algorithms, including A* or even mixed integer programming, fit in this framework. 

## In Haskell

To abstract over all this in Haskell we will use a monad transformer. We can express alternatives with the `Alternative` typeclass, `a <|> b`. Sequential steps naturally fit as a monad. The monad can keep track of the *slack* we have left - whenever we emit a cost we reduce the slack, once we reach `0` we can prune the current computation:

    newtype Bound s a = Bound { unBound :: StateT s [] a }
        deriving (Monad, Applicative, Alternative, Functor, MonadState s, MonadFail)
    class Monad m => MonadBound s m where
        tellCost :: s -> m ()
    class Slack s where
        dropSlack :: s -> s -> Maybe s
        addSlack :: s -> s -> s

    instance Slack s => MonadBound s (Bound s) where
        tellCost cost = do
            slack <- Bound get
            case dropSlack slack cost of
               Nothing -> empty
               Just slack' -> Bound (put slack')

To keep things interesting I will add another constraint:
If we can split our subgoal into $n$ independent steps $child_1 \ldots child_n$, then we can give a better (i.e. lower) slack value:

    min_costs = children.map(min_cost)
    child_slack[i] = slack - (sum(min_costs) - min_costs[i])

In words, to compute the slack for subgoal `i` we can safely substract the `min_cost` of all other independent subgoals from the current slack in advance. This often gives us a much better estimate! Thankfully Haskell has a typeclass for independent steps (`Applicative`) and a language extension to rewrite Monadic code to Applicative steps (`-XApplicativeDo`). The extension is mostly used for implicit parallelism, but collecting as much `min_cost` information as possible works perfectly fine.  
However, wiring this cost information and monadic flow up sounds complicated. Instead we will build a stack of monad transformers that have narrow purposes.

### Memoization (planning stages)

As a final complication I want to support memoization. This makes the slack computation harder because cost might be context-sensitive. Lets use Wordle as an example. For the following tree our total cost is $1+2+3+2+3+3=14$.


![Wordle tree](wordle.svg)

If we use this cached tree in another position, and this position is at depth `3`, then we must update the cost to $4+5+6+5+6+6=32$! We can work around this by splitting context, cost, and slack into three types:

    newtype WordleSlack = WSlack Int
    instance Slack WordleSlack where
        addSlack = coerce (+)
        dropSlack (WSlack l) (WSlack r)
          | leftover >= 0 = Just (WSlack o)
          | otherwise = Nothing
          where leftover = l - r

    class (Slack s, Ord o) => Cost c o s where
        inContext :: c -> o -> s

    data WordleCost = WCost { totalCost :: Int, nodeCount :: Int }
      deriving Monoid via (Sum Int, Sum Int)
      deriving Ord
    newtype WordleContext = Depth Int

    instance Cost WordleContext WordleCost WordleSlack where
        -- adjust for the cost of shifting `nodeCount` nodes down by `depth`
        inContext (Depth depth) (WCost {totalCost, nodeCount}) = WSlack (totalCost + nodeCount * depth)


Now we can build our next monad transformer. Annoyingly, slack must be global because we want to adjust it whenever we find a new and better solution. Context could live in a `ReaderT` monad, and `Cost` in a `WriterT`, but we will put them in a single `StateT` to simplify things later on. `Stream` is some version of `ListT`-done-right.

    newtype BoundM c o s m a = BoundM { unBoundM :: StateT (c,o) (Stream (StateT s m)) a }

We also need a monad to track the minimum of every `Applicative` branch we can see, and we do this using a fake monad:

    newtype LowerBound o a = LowerBound o
        deriving (Functor)
    instance Monad (LowerBound o) where
        LowerBound l >>= _ = LowerBound l
    instance (Monoid o) => Applicative (LowerBound o) where
        pure _ = LowerBound mempty
        LowerBound l <*> LowerBound r = LowerBound $ l <> r
    instance (Monoid o, Ord o) => Alternative (LowerBound o) where
        empty = LowerBound mempty
        LowerBound l <|> LowerBound r = LowerBound $ l `min` r


We can then combine the monads by running them `Both`:

    data Both m n a = MB { bFirst :: (m a), bSecond :: (n a) }
      deriving Functor
    instance (Monad m, Monad n) => Monad (Both m n) where
        return a = MB (return a) (return a)
        MB m n >>= f = MB (m >>= bFirst . f) (n >>= bSecond . f)
    instance (Applicative m, Applicative n) => Applicative (Both m n) where
        pure a = MB (pure a) (pure a)
        MB m n <*> MB m' n' = MB (m <*> m') (n <*> n')
    instance (Alternative m, Alternative n) => Alternative (Both m n) where
        empty = MB empty empty
        MB m n <|> MB m' n' = MB (m <|> m') (n <|> n')


    newtype BnB c o s m a = BnB { unBnB :: Both (BoundM c o s m) (LowerBound o) a }
      deriving (Functor, Alternative)

Before we run a Monadic bind, we pre-pay the minimum cost for the left hand side. When we reach a `withMinCost` annotation, which gives a heuristic cost for the containing block, we emit this minimum cost so it will be payed in advance. But before entering the block we refund this cost so it can be paid for real this time. During execution we might find that the cost is higher than expected, which either prunes the branch or at least reduces the slack for following steps.

    instance (Monad m, Cost c o s) => Monad (BnB c o s m) where
        return = pure
        l >>= r = let (cost, l') = collectCost l in dropSlack cost *> BnB (unBnB l' >>= unBnB . r)

    withMinCost :: Cost c o s => o -> BnB c o s m a -> BnB c o s m a
    withMinCost o m = liftRight (LowerBound o) *> (liftLeft (increaseSlack o) >> m)


We can then write a rather ugly loop which keeps track of the best solution found so far:

    pickBest :: (Monad m, Cost c o s, Monoid o) => BnB c o s m a -> c -> s -> m (Maybe (a,o))
    pickBest (BnB (MB m0 (LowerBound bound0))) ctx0 slack0 = flip evalStateT initialSlack $ go s0 Nothing $ runStateT (unBoundM m0) (ctx0, mempty)
      where
        initialSlack = reduceSlack slack0 (inContext ctx0 bound)
        go slack acc m = do
           put slack
           step <- runStream m
           case step of
              Done -> pure acc
              Yield (a,(ctx,out)) n -> case acc of
                 Just (_,out') | out' >= out -> go slack acc n
                 _ -> go (inContext ctx out) (Just (a,out)) n

By wrapping the cost in `Maybe` we sidestep problems if we don't have an initial maximum cost. If we want to add additional pruning, like not going past depth 6, we can adjust the cost/slack/context types.



## Memoization (for real this time)


We can add caching with yet another state monad, we only need to produce a cache key for the arguments to the cached function. For Wordle we can do this as 5 `Word32` arguments that encode a bitset. If letter `a` can still occur in position `1`, then the first bit in the first `Word32` is set. 

But the context sensitivity strikes again. If we first compute a solution at depth 4 and later encounter the same arguments at depth 3 then we might find a better solution that is mostly flat but has a single length-three guess chain. On the other hand, if we find an amazingly great solution that has only depth 2 then we should use the cached result. 


I solved this by caching by key and context, and allowing a single call to find solutions for multiple contexts.

    newtype Caching k c o a = Caching { unCaching :: State (M.Map k (M.Map c o)) a }
        deriving (Functor, Applicative, Monad)

By exploiting the polymorphism we can insert `Either MinCost RealCost` in the cache, that way we retain some information even if we prune a call.
