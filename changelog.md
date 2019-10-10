- Add --config flag to explicitly set the config file.

### 2.2.0

- fix bugs where pretty printing didn't work right for
leave-space-for-unqualified

- add `format: columns=n` field

- separate qualify-as fields with ; instead of ,

- fix a bug where I didn't allow _ in unqualified import names

- better error reporting

### 2.1.0

- unqualified syntax changed to support multiple imports per module

- add `format: leave-space-for-qualified` and `format: no-group`

- add import-as config option

    E.g. import Data.Text.Lazy as DTL, instead of always having to qualify
    as a suffix, like Lazy, or Text.Lazy.

- various bugs with unqualified imports

#### 2.0.0

- add support for unqualified imports for explicitly configured symbols, via
the `unqualified` field in `.fix-imports`

- significant speed improvement, reuse the loaded pkg index instead of asking
ghc-pkg find-module

- --debug now emits timing metrics

- import-order-{first,last} are exact matches, or are prefix matches if they
have a trailing dot

- prio-module-{high,low} are now exact matches instead of prefix

#### 1.1.0

- Rename import-order to import-order-first, and add import-order-last.

#### 1.0.5

- support haskell-src-exts > 1.16

- add 'language' field to .fix-imports, to turn on local extensions

#### 1.0.3 and 1.0.4

- upgrade to haskell-src-exts-1.16

#### 1.0.2

- Fix bug where a qualified import with >1 dot wasn't found.  And don't
mess with Prelude.

#### 1.0.1

- Fix a bunch of bugs: properly recognize unqualified imports as imports,
never import the current module, don't pick up modules with the same suffix
but a different name.

#### 1.0.0

- Change name from FixImports to fix-imports, which is more unixy.

- Change ghc-pkg parsing from String to Text.  It's noticeably faster.

- Add a more flexible system for prioritizing imports.
When there are several possibilities for a module name, they are all given
to a single function to decide.  The config file moved from
fix-imports-priority to .fix-imports and can now specify sort orders for
packages and modules by prefix.

- Make -i includes for non-existent dirs ignored instead of causing an
error.
