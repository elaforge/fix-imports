1.0.5

- support haskell-src-exts > 1.16

- add 'language' field to .fix-imports, to turn on local extensions

1.0.3 and 1.0.4

- upgrade to haskell-src-exts-1.16

1.0.2

- Fix bug where a qualified import with >1 dot wasn't found.  And don't
mess with Prelude.

1.0.1

- Fix a bunch of bugs: properly recognize unqualified imports as imports,
never import the current module, don't pick up modules with the same suffix
but a different name.

1.0.0

- Change name from FixImports to fix-imports, which is more unixy.

- Change ghc-pkg parsing from String to Text.  It's noticeably faster.

- Add a more flexible system for prioritizing imports.
When there are several possibilities for a module name, they are all given
to a single function to decide.  The config file moved from
fix-imports-priority to .fix-imports and can now specify sort orders for
packages and modules by prefix.

- Make -i includes for non-existent dirs ignored instead of causing an
error.
