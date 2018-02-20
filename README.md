# Whatnext

![BuildStatus](https://travis-ci.org/jeanCarloMachado/whatnext.svg?branch=master)

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

## Installation

If you don't want to install anything there's a web version of wn for [free](https://app.thewhatnext.net/).

### Archlinux

```sh
yaourt -Sa whatnext
whatnext init # to initialize config files

```
## Starting

Create your map of study ``whatnext edit-subjects``

```sh
crypto currencies|99|80|continue reading the book
category theory|70|90|read category theory for programmers
cormen book|65|90|do the exercise of how much you can do with a given complexity (wiki)
kubernetes|79|70|do a tutorial
haskell|45|90|continue reading the book
meditation|80|50|read more papers
math|50|70|master mathematical proophs reading the paper math_proofs 
elm|40|90|do some other tutorials
music theory|43|80|
paraiso perdido|33|20|
kernel development|30|60|
chemistry|30|70|
climate change|10|10|
dragons book|20|80|
open source|99|50|
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

![output](https://i.imgur.com/sbXFpnd.png)

When you study some subject you feed whatnext with this info in order to calculate the next most important item

```sh
whatnext done 'tese cristiano' 'read about types of polymorpishms'
#the arguments are: subject studied, description of what was studied
```
Informing whatnext about your studies also gives you a nice history of your progress
```sh
whatnext log
```

![log](https://i.imgur.com/N2OMkvH.png)



When you get something done your goal status will be updated

```sh
wn status
```

![status](https://i.imgur.com/imsMToT.png)


To get Further examples at a look at the [documentation](doc.md)
