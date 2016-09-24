# Hspec Megaparsec

[![License BSD3](https://img.shields.io/badge/license-BSD3-brightgreen.svg)](http://opensource.org/licenses/BSD-3-Clause)
[![Hackage](https://img.shields.io/hackage/v/hspec-megaparsec.svg?style=flat)](https://hackage.haskell.org/package/hspec-megaparsec)
[![Stackage Nightly](http://stackage.org/package/hspec-megaparsec/badge/nightly)](http://stackage.org/nightly/package/hspec-megaparsec)
[![Stackage LTS](http://stackage.org/package/hspec-megaparsec/badge/lts)](http://stackage.org/lts/package/hspec-megaparsec)
[![Build Status](https://travis-ci.org/mrkkrp/hspec-megaparsec.svg?branch=master)](https://travis-ci.org/mrkkrp/hspec-megaparsec)
[![Coverage Status](https://coveralls.io/repos/mrkkrp/hspec-megaparsec/badge.svg?branch=master&service=github)](https://coveralls.io/github/mrkkrp/hspec-megaparsec?branch=master)

The package is the recommended library for
testing [`Megaparsec`](https://hackage.haskell.org/package/megaparsec)
parsers with with [`Hspec`](https://hackage.haskell.org/package/hspec). As
of Megaparsec 5.1.0, its test suite is re-written with Hspec and this
package with a few ad-hoc helpers.

Consult Haddocks for usage, which should be trivial. Also see test suite of
this package or [Megaparsec test suite](https://github.com/mrkkrp/megaparsec/tree/master/tests).

## License

Copyright © 2016 Mark Karpov

Distributed under BSD 3 clause license.
