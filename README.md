# whatnext
A tool for scheduling recurrent activities


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

The output of the program will a contextualized list of priorities to study at the moment

https://i.imgur.com/KJqNfJC.png

When you study some subject you feed whatnext with this info in order to calculate the next most important item

```sh
whatnext --done "tese cristiano" 'read about types of polymorpishms'
```

Informing whatnext about your studies also gives you a nice history of your progress

https://i.imgur.com/s7vayZd.png
