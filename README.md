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
