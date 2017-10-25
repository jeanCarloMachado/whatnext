# whatnext

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

The projects premises are:

- Life is too complicated to prepare beforehand a calendar of what to study
- Yet we want to have some control and balance over the subjects we study
- What really matters is that we know what to do next
- What to do next cannot be a "must follow" only a advice
- It's better to study in many little sessions than in few big one's
- We usually want to study a lot of things
- There are subjects more important than others (and their importance change quite often)

## Starting

Create your map of study ~/.scheduler

```sh
git|40|60|create a rebase -i demonstration
crypto currencies|99|80|continue reading the book
category theory|70|90|read category theory for programmers
cormen book|65|90|do the exercise of how much you can do with a given complexity (wiki)
kubernetes|79|70|do a tutorial
haskell|45|90|continue reading the book
lisp book|55|90|
hadoop|55|50|continue installing and single node hello world
python|50|70|refactory notify system
meditation|80|50|read more papers
math|50|70|master mathematical proophs reading the paper math_proofs (next section: quantifiers), study logarithms as well
elm|40|90|do some other tutorials
music theory|43|80|?
learning|40|40|??
paraiso perdido|33|20|
statistics|80|60|do the second chapter of the thinkstats book
kernel development|30|60|??
cqrs|20|50|??
chemistry|30|70|??
clang|20|70|organize wiki, make the tutorial to build a sqlite
databases|15|40|??
bad science|10|50|??
history|5|20|??
productivity|3|30|?
climate change|10|10|?
react|10|30|?
veganism|40|10|?
thebellcurve|70|30|?
dragons book|20|80|?
artificial intelligence|40|80|?
open source|99|50|?
clean code|1|50|read others code and start presentation organize talk topics on the presentation
facts and fallacies|70|20|
tese cristiano|80|70|continuar introdução
```
Where the columns are:
1. name of the subject
2. priority of it for you
3. energy level that you need to study it
4. optional description of what to do in the next session

The output of the program will a contextualized list of priorities to
study at the moment. The most important one's will be on the top.


![output](https://i.imgur.com/KJqNfJC.png)

When you study some subject you feed whatnext with this info in order to calculate the next most important item

```sh
whatnext --done "tese cristiano" 'read about types of polymorpishms'
```

Informing whatnext about your studies also gives you a nice history of your progress

![log](https://i.imgur.com/s7vayZd.png)
