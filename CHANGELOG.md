## Hspec Megaparsec 1.1.0

* Add `HasCallStack` constraint to combinators to improve detection of
  locations where test failures happen.

## Hspec Megaparsec 1.0.0

* To be used with Megaparsec 6.

## Hspec Megaparsec 0.3.1

* Support for Megaparsec 5.2.0.

* Drop support for GHC 7.6.

## Hspec Megaparsec 0.3.0

* Added helpers for parse error construction (useful with `shouldFailWith`):
  `err`, `posI`, `posN`, `utok`, `utoks`, `ulabel`, `ueof`, `etok`, `etoks`,
  `elabel`, `eeof`, `cstm`. Also added an auxiliary type `EC`.

## Hspec Megaparsec 0.2.1

* Refreshed obsolete documentation for `shouldFailWith` and how it reports
  not matching parse errors.

## Hspec Megaparsec 0.2.0

* This version of `hspec-megaparsec` should be used with Megaparsec 5.

## Hspec Megaparsec 0.1.1

* Make it pass tests with Megaparsec 4.4.0 and later.

## Hspec Megaparsec 0.1.0

* Initial release.
