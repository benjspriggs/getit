# getit

| Travis CI |
| --- |
| [![Build Status](https://travis-ci.com/benjspriggs/getit.svg?token=bmaxgzYYR9V3NTLzRd1p&branch=master)](https://travis-ci.com/benjspriggs/getit) |

## Get Started
This project uses [`stack`](https://docs.haskellstack.org/en/stable/README/) as a dependency manager and build tool.

```shell
# install dependencies and build getit
stack install
# display existing todos - should be empty on first run
stack exec getit list
# store some samples in our todo list
stack exec getit store
# display the sample todos
stack exec getit list
# add a task
stack exec getit new task "HELLO WORLD" "2018-06-01 12:00:00"
# see our new todo!
stack exec getit list
```
