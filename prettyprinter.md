# How does Prettyprinter print pretty?

There are many prettyprinter libraries in Haskell, but most are based on Phillip Wadler's paper `A prettier printer`.

To learn Haskell libraries I usually peak at the implementation, but `Prettier` defeated me twice. Partly because printing is rarely the most interesting part of a project. But Wadler's approach also has some action-at-a-distance which is rarely seen in Haskell and it can be difficult to understand the pieces in isolation.
This is a very brief and rough introduction into how Prettier actually works. It is not a guide on how to use it, but after understanding the big ideas it should be easier to read the documentation and source code.

### Example: 

First a small example to understand what Prettier does. This is a Haskell-style let binding:
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
-- >>> pretty $ Let [("x", 3), ("y", 5)] (Var "x" * Var "y")

-- if it fits into a single line:
let {x = 3; y = 5} in x * y

-- otherwise:
let x = 3
    y = 5
in x * y
```

Fancy! But a bit magical. What do the `group` and `align` functions do exactly?

### The core trick

Prettier has a small datatype to describe documents. The trick we just saw uses an internal constructor `data Doc = ... | Union Doc Doc`.  A better name might be `Alt`; it makes the document non-deterministic and layouting a (greedy) search. We try to layout with the first branch if it fits, and fall back onto the second otherwise.

But this does not quite explain everything. Notably, the following layouts are impossible:

```Haskell
let x = 3
    y = 5 in x * y

let {x = 3; y = 5}
in x * y
```


Instead, alternatives in `Prettyprinter` work in two steps. Firstly, we use a new constructor `data Doc = ... | FlatAlt Doc Doc` to express choices. When interpreted this just picks the non-flat branch.

Secondly, the `group` operation tries to flatten a sub-document.

```Haskell
group :: Doc -> Doc
group a = Union (selectFlatVersion a) a

selectFlatVersion :: Doc -> Doc
selectFlatVersion (FlatAlt _ flat) = flat
selectFlatVersion (Union flat _) = flat
selectFlatVersion (Cat a b) = Cat (selectFlatVersion a) (selectFlatVersion b)
selectFlatVersion Empty = Empty
selectFlatVersion ...
```

Maybe it's just me but the mixed-up ordering of `FlatAlt` and `Union` feel needlessly confusing, especially because they have no meaning until some layout step interprets them. But once we understand what the pieces do everything clicks together and the approach seems almost natural. Neat!

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

As mentioned, this is a pretty rough introduction to `Prettyprinter`'s internals. We haven't discussed any of the actually useful combinators. Thankfully, the [the documentation](https://hackage.haskell.org/package/prettyprinter-1.7.1/docs/Prettyprinter.html) is solid once you have understood grouping and indentation. Feel free to click `view source` if the documentation is unclear, most combinators are thin wrappers around the internals we discussed. Good luck with pretty printing!

Here are the helper we used in the example:

```Haskell
(<>) = Cat
(<+>) a b = a <> Char ' ' <> b
line = FlatAlt Line (Char ' ')
encloseSep l r sep ds = l <> mconcat (intersperse sep ds) <> r
```
