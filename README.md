# Jaipur

Game modelling in PureScript for the fast-paced two-player card game, [Jaipur](https://boardgamegeek.com/boardgame/54043/jaipur).

This is primarily an exercise in (a) coding up Euro-style games, and (b) learning PureScript. This extensively uses optics (lenses and traversals) to access and manipulate the game state's data structure. It's highly likely that the data structure and approach is not very idiomatic for PureScript (or Haskell) but I'm still learning, and getting in way over my head with lenses was arguably a rewarding intellectual exercise. 

Big shout to [Brian Marick](https://twitter.com/Marick) for his in-depth [book](https://leanpub.com/lenses) on PureScript lenses. I still don't really grok traversals and PureScript's endless type classes (`Wander`, `Strong`...?) but that's certainly no fault of his. I wish there were more PureScript books out there.

## How to build

Assuming PureScript, `pulp`, and `psc-package` are installed...

```
$ git clone https://github.com/alphajuliet/jaipur.git
$ psc-package install
$ pulp build
$ pulp test
```
