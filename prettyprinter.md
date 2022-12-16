# How does Prettyprinter print pretty?

There are many prettyprinter libraries in Haskell, but most are based on Phillip Wadler's paper `A prettier printer`.

To learn Haskell libraries I usually peak at the implementation, but `Prettyprinter` defeated me twice. Partly because printing is rarely the most interesting part of a project. But Wadler's approach also has some action-at-a-distance which makes it tricky to understand the pieces in isolation.
This is a very brief and rough introduction into how Prettyprinter actually works. It is not a guide on how to use it, but after understanding the big ideas it should be easier to read the documentation and source code.

### Example: 

First a small example to understand what Prettyprinter does. This is a Haskell-style let binding:
```haskell
pretty (Let binds body)
    = group ("let" <+> align letBinds <> line <> "in" <+> pretty body)
  where
    letBinds = encloseSep open close sep [pretty k <+> "=" <+> pretty v | (k,v) <- lets]
    open = flatAlt "" "{ "
    close = flatAlt "" " }"
    sep = flatAlt "" "; "

```

Crucially, it can print in two distinct styles:

```haskell
>>> pretty $ Let [("x", 3), ("y", 5)] (Var "x" * Var "y")

-- if it fits into a single line:
let {x = 3; y = 5} in x * y

-- otherwise:
let x = 3
    y = 5
in x * y
```

Fancy! But a bit magical. What do the `group` and `align` functions do exactly?

### The core trick

First let us summarize our goal.
We want alternative layouts: The newline version is narrow but long, the flat version is short but wide. The layouting should pick the shortest version which fits some target width.

This means documents are non-deterministic, in the sense that lists are non-deterministic. Can we use lists?


```Haskell
newtype NondetDoc = ND [Doc]
newline = ND [Text " ", Newline]

instance Semigroup NondetDoc where
    (<>) (ND a) (ND b) = ND $ liftA2 (<>) a b

>>> "foo" <> newline <> "bar" <> newline <> "baz"
ND ["foo bar baz", "foo bar\nbaz", "foo\nbar baz", "foo\nbar\nbaz"]
```

This has two key problems:

- We generate exponentially many candidates and picking the best one is really expensive.
- The choices are un-synchronized. We will cover this in a bit.

The first problem is simple to solve. Using `liftA2` multiplies the lists out, generating all combinations in advance. We have all choices on top, and the document content below that. Instead, we interweave choices and document content by embedding them as a new constructor.

```Haskell
data Doc = Cat [Doc] -- concatentate documents
         | Text String
         | Alts Doc Doc -- alternative documents
         | Line -- Add a newline
instance Semigroup Doc where
    (<>) a b = Cat [a,b]
```

Now layouting to perform a greedy search, rather than the depth-first search of the list monad. When layouting `Union a b`:

- Check whether `a` fits the width. If yes, emit it.
- Otherwise emit `b`.

This is linear rather than exponential. First problem solved!


The second problem is a luxury, but a great one to have. We want to synchronize certain choices. For instance, the following layouts should be impossible:

```Haskell
let x = 3
    y = 5 in x * y

let {x = 3; y = 5}
in x * y
```

Intuitively, we take a sequence of choices like `Cat [Alts a b, Alts c d]` and turn it into a choice of sequences `Alts (Cat [a,c]) (Cat [b,d])`. But this is too synchronized. Either the entire output is one line, or nothing is flat.

We can solve this with two ingredients:

- A scoped synchronization operator `group a = Alts (shortVersion a) (longVersion a)`
- An additional `data Doc = ... | Stop Doc` constructor

```Haskell
data Doc = Cat [Doc] -- concatentate documents
         | Text String
         | Alts Doc Doc -- alternative documents
         | Line -- Add a newline
         | Stop Doc -- Independent group, stop synchronizing

group :: Doc -> Doc
group a = Stop (Alts (shortVersion a) (longVersion a))


longVersion :: Doc -> Doc
longVersion (Stop a) = Stop a
longVersion (Alts long _) = long
longVersion (Cat a) = Cat (map longVersion a)
longVersion a = a

shortVersion :: Doc -> Doc
-- the short version should always be a single line: walk into nested group
shortVersion (Stop a) = shortVersion a
shortVersion (Alts _ flat) = flat
shortVersion (Cat a) = Cat (map shortVersion a)
shortVersion a = a
```
Here is an example of how `group` operates:

```Haskell
newline = Alts (Text " ") Line
instance IsString Doc where
    fromString = Doc
    
test = group $ Cat ["foo", newline, "bar", newline, "foobar"]

>>> test
Stop 
  (Alts 
    (Cat ["foo", " ", "bar", " ", "foobar"])
    (Cat ["foo", Line, "bar", Line, "foobar"]))
```

it is important to note the asymmetry between `shortVersion` and `longVersion`. The `shortVersion` never contains newlines. But in `longVersion` nested groups are independent.

```Haskell
>>> group (Cat ["baz", newline, test])
Stop
  (Alts 
    (Cat ["baz", " ", shortVersion test])
    (Cat ["foo", Line, test])) -- test could be short or long
```

Our approach differes slightly from `Prettyprinter`:
Prettyprinter has no `Stop` constructor. Instead, it has `Union` and `FlatAlt`. `Union a b` is `Stop (Alts a b)`, `FlatAlt` is  `Alts a b`.
Prettyprinter additionally uses a fairly subtle trick: `longVersion = id`. We know `shortVersion` will remove all `FlatAlt`'s. This means all remaining `FlatAlt's` should be long and we can keep them in the document. The layouting step must work around them, reimplementing our `longVersion`.

This is clever but it adds an implicit `longVersion` at the toplevel; without `group` nothing is flattend. Many operators use `group` internally, but it's something to keep in mind.

### Indentation

The final piece to the puzzle is indentation. Docs track a current indentation, and each newline adds that many spaces. You can modify the indent with the `nest offset innerBlock` function.
The `align` function sets the indent to the current column, the `hang` function to the current column+an offset.

### Layouting

Turning a `Doc` into a something we can consume takes two steps

- Use `layoutPretty defaultLayoutOptions` to get a `SimpleDocStream`
- Use a consumer such as `Prettyprinter.Render.Text.renderStrict` to get Text

The first option has other candidates such as `layoutSmart`. The difference is small: When picking a `Union`, `layoutPretty` only checks if the first line of the flat version fits. Meanwhile `layoutSmart` checks if any future lines are too wide, stopping only when the indentation becomes flat. The latter is a heuristic because most syntax constructs end when indentation becomes flat - presumably to guard against non-linear behaviour.

For consumers we have many options, one for each output type. Thankfully this makes the choice simple. Just search the docs for the type you need. There are some external consumers on hackage, supporting fancy formats such as pandoc or terminals. If you want to dive deep you can add consumer specific annotations to documents, allowing for formatting or even onclick handlers.

### The End

As mentioned, this is a pretty rough introduction to `Prettyprinter`'s internals. We haven't discussed any of the actually useful combinators. Thankfully,  [the documentation](https://hackage.haskell.org/package/prettyprinter-1.7.1/docs/Prettyprinter.html) is solid once you have understood grouping and indentation. Feel free to click `view source` if the documentation is unclear, most combinators are thin wrappers around the internals we discussed. Good luck with pretty printing!

To prove my point here are the helpers we used in the example:

```Haskell
(<+>) a b = a <> Char ' ' <> b
-- newline, but in `group` try a space instead
line = FlatAlt Line (Char ' ')

encloseSep l r sep ds = l <> mconcat (intersperse sepWithNL ds) <> line' <> r
  where sepWithNL = line' <> sep -- haskell style leading seperators
line' = FlatAlt Line Empty
```
