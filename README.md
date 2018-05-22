# getit

| Travis CI |
| --- |
| [![Build Status](https://travis-ci.com/benjspriggs/getit.svg?token=bmaxgzYYR9V3NTLzRd1p&branch=master)](https://travis-ci.com/benjspriggs/getit) |

## Get Started
This project uses [`cabal`](https://www.haskell.org/cabal/download.html) as a dependency manager and build tool.

```shell
# install dependencies and build getit
cabal install
# display existing todos - should be empty on first run
cabal exec getit list
# store some samples in our todo list
cabal exec getit store
# display the sample todos
cabal exec getit list
# add a task
cabal exec getit new task "HELLO WORLD" "2018-06-01 12:00:00"
# see our new todo!
cabal exec getit list
```
