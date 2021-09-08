# Mutating Lenses

Summary: I will be sad about the state of mutable Haskell for a bit. Then we will
figure out how to mix lenses with mutable state.
Also, that the real mutable borrows were the pure values that we made along the way or something.

## Imperative Haskell

Haskell is sometimes referred to as the 'Best Imperative Programming Language'. Jokingly. I assume.  
But it does have a ring of truth to it. Especially for algorithms I quite enjoy writing lens-y, state-y, backtracking-y, pseudo-imperative code. This post started by me writing a naive e-graph implementation based on [egg](https://github.com/egraphs-good/egg/) in Haskell to figure out how it works.

Here is a tiny function which fixes an invariant, namely some lists being normalized and sorted.  
This implementation is nominally pure - a State monad carrying nested maps - to act as a baseline for something more efficient<sup>[1](#footnote1)</sup>.

```haskell
normalizeClassNodes :: M ()
normalizeClassNodes =
    forOfM (egg . #classes . each . #nodes) $ \nodes ->  do
        nodes' <- traverse normalize nodes
        pure $ uniqSorted $ sort nodes'
```


Here is the corresponding Rust, for comparison:

```rust
for class in self.classes.values_mut() {
            class
                .nodes
                .iter_mut()
                .for_each(|n| n.update_children(|id| uf.find_mut(id)));
            class.nodes.sort_unstable();
            class.nodes.dedup();
            ...
```

The Rust code is quite similar, point to the Imperative Haskell proponents. The Rust code, unfortunately, is also orders of magnitudes faster.

## *Fast* Imperative Haskell

Our original Haskell code was pretty much imperative already - how bad could writing a faster version be? Pretty bad, it turns out. Here are the direct challenges

- Vectors can't over-allocate with a capacity for cheap `append`
- Vectors don't come with a sort or dedup function
- Vectors don't have support for virtually nested arrays like `int[10][]`
- The non-`vector` vector libraries are frameworks, usually geared for parallelism
- There exists no hash table in Haskell that doesn't store keys and values as pointers
- Some default Hashable instances are... questionable
- Nested mutable containers are a pain
- ...also pure data in mutable containers is a pain
- ...also mutable containers in pure data are a pain
- ...also interfaces for mutable containers are a pain
- There are, like, 5 incompatible vector variants and it's hard to build abstractions over them
- Abstraction involving generic ADT's tend to optimize badly (nowadays newtypes over unboxed tuples might work?)
- Linear Haskell needs better multiplicity polymorphism (and probably linear constraints) before it can help

I could keep going but I feel like the pattern is clear. Haskell has amazing support for abstractions, and some great ideas like fast streaming and unpacked vectors. But a lot of infrastructure is missing polish and mutable anything lacks support. There are some glimmers of hope like linear types and Backpack, but they aren't ready for prime time yet.

But grumbling over, that won't help us build nice imperative code. For this post let's try to build a reasonably efficient and uniform interface that works across mutable and pure data.

## Mutating Lenses

Let us write some lenses that can mix monadic reads and writes with traditional lenses. We will assume that all mutable state is used in a linear manner, so we don't have to deal with overlapping state, cyclic references, or multi-threading. The most common lenses encode the operation in a polymorphic type:

```haskell
type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s
```

This is basically a continuation - tell me what to do with the field and I will tell you what happens with the container. In the following examples I will inline some newtypes.

Frequently we instantiate the lens to update an `a` within an `s`:
    
```haskell
(a -> a) -> s -> s
```

or to read an `a` within an `s`:


```haskell
(a -> b) -> s -> b
s -> a -- equivalent
```

To allow monadic operations that interop with classic lenses, we will need some way to compose the monadic effects with the `f` parameter. Sorry, `Compose` the monadic effects with the `f` parameter.

```haskell
newtype Compose m n a = Compose (m (n a))

type MLens m s a = forall f. Traversable f => (a -> Compose m f a) -> s -> Compose m f s
```

And here is how we might define a mutating lens:

```haskell
ixM :: (V.MVector v a, PrimMonad m) => Int -> MLens m (v (PrimState m) a) a
ixM i f v = Compose $ do
    -- run in the monad m part
    a <- V.read v i :: m a
    ta <- getCompose (f a) :: m (t a)
    -- the traverse t is what controls updates
    -- during getting it contains no `a`, so traverse is a noop
    tu <- traverse (V.write v i) ta :: m (t ())
    -- fix the return type by injecting our original vector
    -- (fmap is ignored during getting)
    pure (v <$ tu) :: m (t (v (PrimState m) a))
```

This lets us instantiate as

```haskell
(a -> m a) -> s -> m s -- monadic update
(a -> m b) -> s -> m b -- monadic read (and other non-updating effects)
```


This was rushed, so don't worry if not every detail is clear. 
Important is that this gives us a uniform interface, lets us interact with normal lenses, and (assuming linear mutable state) has sane behaviour.

But we return the original vector, and this pseudo update will cascade all the way to the top. This is because Haskell doesn't really understand the concept of a memory location - we only get values. Lenses act on values. If we use lenses, then we cannot distinguish between updating a memory location or a value.

## Mutable, Not Monadic

I think it is worth pausing briefly to make some distinctions. So far, we have built lenses that contain a monad. We have not built monadic lenses. That's because `Compose m n` usually isn't a monad even if both `m` and `n` are - which is the whole reason that monad transformers exist.

Here is an example of what monadic lenses could do, and mutable lenses very much can't:

Imagine we have a container holding an `Int` field `count`, and a map of values. A monadic lens could read this `count` field, read the first `count` elements of a map, and return a list of them. This gives us a compound result - we can read from different lenses, perform control flow, and aggregate the result. This is extremely powerful but makes updating hard.

When updating, the length of the list may have changed. Some elements may have been inserted or deleted. Elements might even be reordered. Updating `count` is easy - we can use the length of the updated list - but the rest may require some sophisticated diffing process. We must be able to project an updated value from the updated compound state for each lens that we used. But often there can be many solutions without a clear best choice. [This blog about monadic profunctors](https://blog.poisson.chat/posts/2017-01-01-monadic-profunctors.html) is a great introduction to one nice approach in Haskell.

Our `Compose m f` lenses can't do any of this. We can do monadic computations, but only in the `m` layer. This means we "only" can do everything traditional lenses can and don't have to deal with the problems monadic lenses bring.



## The problem with lvalues

I recently had an epiphany about lvalues. Beware, this might be like epiphanies about monad-like burritos. Anyway, lvalues are like lazy values. GHC starts by assuming that all values are lazy, that they must be allocated as a function on the heap which is called only when required, and then is updated to return the result immediately once this first invocation is done. This is very slow. Therefore, GHC moves heaven and earth to undo this assumption. It uses strictness analysis, transformations, and a bit of cheating to uncover whenever this laziness is superfluous. Being strict means things can be stored in registers, which is the main ingredient for fast. In some sense using a value lazily changes it into something new because suddenly most optimizations fail and GHC must treat it completely differently.

Lvalues are like that. C compilers assume lvalues must be allocated in memory, and they move heaven and earth to figure out when this can be undone. In a very real way taking the address to a value changes it into something else - if the address may escape then the value must be allocated. This disables a lot of optimizations. Rvalues are things which cannot have a leaked address - like the `3` in `f(3)` - so the compiler doesn't have to worry about maintaining their identity.

But in Haskell values don't have an identity. We don't even have a sensible pointer comparison most of the time. Therefore, GHC doesn't bother optimizing what few lvalues exist - what happens in IORef stays in IORef. This means we should keep small and short-lived things in rvalues if we are interested in performance. If we have a vector with cheap append then it will be occasionally resized, possibly returning a new buffer. I found two packages on Hackage which implement this, both by storing the vector in a mutable reference. This kills GHC's optimizations. If we want fast code, we must use rvalues for small things. So append should return a new vector instead of `IO ()`.  
This causes problems with invalidation and live-times. I will ignore all of them. Hopefully we will be able to encode those invariants with linear haskell at some point but worrying about these bugs is a job for future me. And screw future me - they probably wrote those bugs in the first place.

Anyway, there is something odd about returning a new vector after updates. Let's look at this example:

```rust
foo[0][1].append(3)
```

If we always return a vector after updates, we would have to do something like

```javascript
t1 = foo[0]
t2 = t1[1]
t2_b = t2.append(3)
t1_b = t1([1] := t2_b)
foo_b = foo([0] := t1_b)
```

The second halve is clearly ludicrous. Even if t2 is reallocated, t1 won't be. In some sense accessor-chains in imperative languages are split into two parts - the getter for an lvalue, and usage of that lvalue. But Haskell doesn't have first-class lvalues. This problem also occurs with linear Haskell because we cannot distinguish between ownership, location, and values. Instead, we can use the second-to-last step as a proxy:

```haskell
    t1 <- V.read foo 0
    t2 <- V.read t1 1
    t2' <- V.append t2 3
    V.write t1 1 t2'
```

Note how the index `1` is duplicated, but that we do not have to modify `foo`. Note also that this code is immensely ugly and much harder to both read and write. As a point of comparison, a pure state monad representation *looks* much more imperative.

```haskell
#foo . ix 0 . ix 1 <>= [3]
```

Here the `ix 1` only occurs once. Intuitively we want to split our lens operation into three pieces. The first part is treated as a monadic getter, never updating. The second is treated as a monadic lens, both reading and updating. The final part operates on the inner value, appending `3`.


```haskell
appendOfM (#foo . ix0) (ix 1) 3
```

But, like, without actually having to do all of that. We also might have the occasional lens which *does* need to mutate an inner value - when a HashMap must resize after an insertion, for instance. Ideally lenses would have control over whether their surrounding context should update, but that's exactly the opposite order lenses operate in.

This is related to mutable and non-mutable borrows in rust. Mutable borrows are exactly the ones which can update their surrounding context. Non-mutable borrows are the ones which never update their surrounding context. Weirdly, in this comparison all pure lenses use mutable borrows so make of that what you will. If we look at pure values as top-level locals that completely replace the old version, it doesn't not work?

## Excuse me, could I Borrow a Lens

The main difference from the previous mutating lenses is that we always return an `m (f ())`, with some newtype nonsense on top.


```haskell
type LValLens m s a = forall f. Traversable f => (a -> Compose m f a) -> s -> Compose m f ()
```

That's just a normal mutable lens that returns unit! But we must do some more work to make this compose. If we write our lenses in CPS then `f . g` means that `g` cannot affect the behaviour of `f`. But that is exactly what we need - some way to suppress the update code from `f`. Instead, we use a different function composition `.$` that injects a newtype before passing back to f. 
 
 The outer lens sees the return type `Compose m (Const (f ())) s`, where the `Const` means the outer lens will never see an updated value. During updates we use `f ~ Identity`. After inlining `Compose m (Const (Identity ())) s` is equivalent to `m ()`, skipping the update on the outer part. During reading we get `f ~ Const a`, so we get `Compose m (Const (Const a ())) s`. After inlining this is `m a`, flattening the nested `Const`.

The composition operator is easy to write. If you let GHC complete the type signature, that is.


```haskell
(.$) :: (Functor f, Applicative m)
       =>
          ((a -> Compose m (Const (f ())) b)
         ->(s -> Compose m (Const (f ())) t))
       ->
          ((x -> Compose m f x)
         ->(a -> Compose m f ()))
       -> 
          ((x -> Compose m f x)
         ->(s -> Compose m f ()))
(.$) l r f s = Compose $ fmap getConst $ getCompose $ l (\a -> Compose $ Const <$> getCompose (r f a)) s
```


There is something to be said about definitions that wrangle a single newtype but have a multi-line type signature. I'm just not sure what. Just focus on the fact that it's almost normal lens composition, adding a Const layer before we return to the outer lens.

As a preview for next time, here is a usage example on a 5x3x3 vector which is a newtype on an unboxed vector:


```haskell
someLookup :: PrimMonad m => Slice VU.MVector (PrimState m) '[5,3,3] Int -> m Int
someLookup = viewM (ixM 1 .$ ixM 2 .$ ixM 0)

-- pure data in mutabe state in pure data 
anUpdate :: PrimMonad m => ((Slice VU.MVector (PrimState m) '[5,3,3] (Char, Int)), Bool) -> m ()
anUpdate = overM (_1 .$ ixM 1 .$ ixM 2 .$ ixM 0 . _2) (+1)
```

## Law of the land

Here are some laws I'd deem reasonable

#### Get Put

```haskell
v <- viewM l s
setM l v s
~
pure ()
```

#### Put Put

```haskell
setM l v s
setM l v s
~
setM l v s
```

#### Put Get

```haskell
setM l v s
v' <- viewM l s
assert (v == v')
```

##### Dead Reads

Reads which aren't used should be removable:

```haskell
() <$ viewM l s
~
pure ()
```

This forbids things like the `std::map::operator[]` from C++ which inserts a default value into the map when the key is missing. Good. Use an update and return the new value when this is what's needed.


## Problems

I haven't noticed huge issues, but there are some annoyances.

First, the duplicate lens commands. The implementation for `overM` is different from `over`. If the monad is Coercible the code duplication might be manageable? Either way it's not a huge issue but I'd rather not duplicate all lens operators.
Secondly, we always perform the read step. For `setM` this isn't necessary. For mutable arrays GHC is hopefully smart enough to remove dead reads, though the index checking might not be removable. For complex things like hashmaps we can split out a resolving step which retrieves the correct slot and read/write on that slot. This runs into the lvalue problem, but it's usually representable as an array with offset.

Not really a problem but it's nice to mix mutable lenses with a state monad to hold the top-level values. This state monad could be interpreted as storage for local variables. For things like resizable vectors this prevents us from accidentally reusing an old, now reallocated, version - though linear types would be safer. Adding mutable lens operators that work with state monads adds even more code duplication, though.

## References

I found the answers on [this stackoverflow question](https://stackoverflow.com/questions/18794745/can-i-make-a-lens-with-a-monad-constraint) a great starting point. As-is it doesn't quite allow nested composition and interop with normal lenses, though.
There are many approaches to monadic lenses, like bidirectional transformations or the [following blog post](https://github.com/effectfully-ou/sketches/tree/master/extensible-monadic-lenses). Pretty much no relation, mutable lenses aren't monadic.
There is a [mutable-lens package](https://hackage.haskell.org/package/mutable-lens) but here indexing into a mutable vector isn't a lens.
The [mutable package](https://hackage.haskell.org/package/mutable) has lens-like combinators but lacks interop with lenses.

If there are other relevant approaches, or thoughts on this approach, I'd love to hear about them. Thanks for reading!


## Footnote~~s~~

<a name="footnote1">1</a>: Some people will give me side-eye about the `M ()` type. The algorithm relies on deferring invariants so valid-by-construction types don't work, and indexed types are awkward in general. And extracting small named invariant-repairing functions is good for readability, actually. Thank you for coming to my ~~TED talk~~ tangent.
