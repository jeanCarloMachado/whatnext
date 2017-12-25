# Whatnext

![Status](https://travis-ci.org/jeanCarloMachado/whatnext.svg?branch=master)

Whatnext is the only way I found myself being able to have some control
over my studies.

It's methodology is quite simple. You define the subjects you want to
study, the importance of any one of these subjects and the complexity of
them. Given that whatnext will calculate what's more
important for you to study right now. It will give you an ordered list
of "what to do next". You may follow it or not. Next time you study any
of the subjects you tell whatnext. With this info in hand whatnext will
refine what's more important to do next.

Anytime your requirements change over the subjects you may only change
the configuration file and whatnext will figure out what's more
important next.

The projects rests on some premises:

- Life is too complicated to prepare beforehand a calendar of what to study and follow it accordingly
- Yet we want some control and balance over the subjects we study
- What really matters is that we know what to do next
- What to do next cannot be a "must follow" only an advice
- It's better to study in many little sessions than in few big one's
- We want to study a lot of things and there are subjects more important than others (and their importance change quite often)


## Starting

Create your map of study ``whatnext edit-subjects``

```sh
crypto currencies|99|80|continue reading the book
category theory|70|90|read category theory for programmers
cormen book|65|90|do the exercise of how much you can do with a given complexity (wiki)
kubernetes|79|70|do a tutorial
haskell|45|90|continue reading the book
lisp book|55|90|
hadoop|55|50|continue installing and single node hello world
meditation|80|50|read more papers
math|50|70|master mathematical proophs reading the paper math_proofs 
elm|40|90|do some other tutorials
music theory|43|80|
learning|40|40|
paraiso perdido|33|20|
statistics|80|60|do the second chapter of the thinkstats book
kernel development|30|60|
chemistry|30|70|
clang|20|70|organize wiki, make the tutorial to build a sqlite
databases|15|40|
bad science|10|50|
history|5|20|
productivity|3|30|
climate change|10|10|
dragons book|20|80|
artificial intelligence|40|80|
open source|99|50|
clean code|1|50|
facts and fallacies|70|20|
tese cristiano|80|70|continuar introdução
```

Where the columns are:
1. name of the subject
2. priority of the subject (0 to the least important 100 to the most)
3. the complexity of the subject (0 to the easiest 100 to the hardest)
4. optional description of what to do in the next session

The output of the program will a contextualized list of priorities to
study at the moment. The most important one's will be on the top.

```sh
whatnext
```

![output](https://i.imgur.com/penJYeF.png)

When you study some subject you feed whatnext with this info in order to calculate the next most important item

```sh
$ whatnext done 'tese cristiano' 'read about types of polymorpishms'
#the arguments are: subject studied, description of what was studied
```
Informing whatnext about your studies also gives you a nice history of your progress
```sh
$ whatnext log
```

![log](https://i.imgur.com/vbMa3ji.png)


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

When you get something done your goal status will be updated

```sh
wn status
```

![goalAfter](https://i.imgur.com/fGXj3AX.png)


[Installation and documentation](doc.md)
