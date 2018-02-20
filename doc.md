# Documentation

## Manual Installation

```sh
#clone the repo somewhere
git clone git@github.com:jeanCarloMachado/whatnext.git
#add the cloned repo into your path
export PATH="$PATH:/whatnextcloneddirectory"

#initialize the config files

whatnext init
```



## Tired Mode


When you're tired but yet want to do something, use the variable TIRED=1.

```sh
TIRED=1 whatnext
```


Whatnext will reorder the priorities giving the easier one's first.


## Goals

You can configure how much you need to spent with a given subject.
```sh
wn edit-goals
```
```sh
{
    "haskellMastry": {
        "from": "2018-01-01",
        "to": "2018-12-31",
        "minutes": 2000,
        "subject": "haskell"
    },
    "weekOfPython": {
        "from": "2017-12-15",
        "to": "2017-12-22",
        "minutes": 120,
        "subject": "python"
    },
    "readABitOfLisp": {
        "from": "2017-12-14",
        "to": "2017-12-30",
        "minutes": 120,
        "subject": "lisp"
    }
}
```
