`fix-imports` is a small standalone program to manage the import block of a
haskell program.  It will try to add import lines for qualified names
with no corresponding import, remove unused import lines, and keep the
import block sorted, with optional rules for grouping.

Support for unqualified imports is limited to symbols you explicitly configure,
so if you list `System.FilePath ((</>))`, it will add that import when you use
it, or remove when it's no longer used, but it won't go search modules for
unqualified imports.

It doesn't mess with non-managed unqualified imports, so you can still use
unqualified imports, you just have to do it manually.

Since it's a unix-style filter, it should be possible to integrate into any
editor.  There's an example vimrc to bind to a key in vim.

### Usage:

Normally you would integrate it with your editor (see `vimrc` for a vim
example), but for testing, here's an example invocation:

    fix-imports -i src -i test src/A/B/C.hs <src/A/B/C.hs
    [ fixed contents of A/B/C.hs, or an error ]

The `-i` flag is like ghc's `-i` flag, it will add an aditional root to the
module search path.  The example will find modules in both `test/*` and
`src/*`, in addition to the package db.

`fix-imports` will look for `.ghc.environment.*` in the current directory
and use it for pkgs to search.  This is created by cabal v2, but only if
you have `write-ghc-environment-files: always` in `cabal.project`.
Otherwise, it assumes cabal v1 and will use the `ghc-pkg` command to use
the global package db.

If it doesn't seem to see packages you think it should, run with `--debug`
to see what it sees.

I don't use stack, but my understanding is this is enough to get `ghc-pkg`
working:

    export GHC_PACKAGE_PATH=$(stack path --ghc-package-path)
