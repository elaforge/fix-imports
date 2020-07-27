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
`src/*`, in addition to the global package db.

About the global package db, `fix-imports` uses the `ghc-pkg` command to find
packages, so it will see whatever you see if you do `ghc-pkg list`.  If it
doesn't see the right things for your package, say for the new nix-style
builds, you'll have to figure out how to fix that.  As is usual for cabal and
ghc integration, ghc has several overlapping but documented configuration
methods, and cabal is completely undocumented.  The relevant bits for ghc are
GHC_PACKAGE_PATH and perhaps package environments:
https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/packages.html#the-ghc-package-path-environment-variable
Cabal doesn't seem to document how to get the appropriate package path for a
nix-style build.  I don't use cabal so I haven't figured this out yet, but
let me know if you know or figure it out.

I don't use stack either, but my understanding is this is enough to get
`ghc-pkg` working:

    export GHC_PACKAGE_PATH=$(stack path --ghc-package-path)
