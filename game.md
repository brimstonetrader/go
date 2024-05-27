---
layout: work
type: Project
num: 1
worktitle: Game
due: Monday, March 4
---

For project 1, you will create some kind of game (or other simple
interactive text-based app).  A simple wrapper to do all the I/O has
been provided; you need only provide it with four things:

- A definition of the type `GameState`.  This can be whatever you want
  to keep track of the state of your game.
- A function `initGameState :: [Int] -> GameState` to create an
  initial `GameState`, given an infinite list of randomly chosen
  integers (you can ignore the list of integers if you do not want or
  need any randomness).
- A function `gamePrompt :: GameState -> String` to determine what
  prompt to show the user, depending on the current game state.
- A function `gameStep :: String -> GameState -> (String, Maybe
  GameState)` which performs one step of the game: given the user's
  input and the current game state, it outputs a message to display to
  the user and a next game state (or `Nothing` if the game should be over).

Of course, you can and should decompose the definitions of these
things into many more functions as appropriate.

## Skeleton

Click this link to [download the skeleton code](../csci365-game.tgz)
for the project, and unzip it somewhere appropriate.  The skeleton
contains a basic Haskell package, consisting of:

- `csci365-game.cabal`: a file in the standard Cabal package format
  describing this package.  You should edit the `author` and
  `maintainer` fields to replace them with your own name and email.
    - If you want to use additional libraries from Hackage (for
      example, the `containers` package), you should add them to the
      list under `build-depends`.
- `LICENSE`: the terms of the license under which this package can be
  distributed.  Replace the copyright notice with your own name.  Come
  talk to me if you are curious about using a different license.
- `CHANGELOG.md`: a standard Markdown file intended to contain
  information about versions and releases of the project.  You can
  ignore this for now.
- `game.md`: this file.
- `app/Main.hs`: the I/O wrapper.  You can look at this file if you
  wish but you need not edit it.
- `app/Game.hs`: the file you should edit.  Currently it contains a
  very simple guess-the-number game as a demonstration; you should
  replace it with your own game.

To run your game:

- From the command line, issue the command `cabal run`.
    - If you don't have the `cabal` command, you can install it with
      `ghcup install cabal 3.10.2.1`.
- You may be able to also run it from within your editor/IDE.

## What to turn in

You should turn in a `.zip` or `.tgz` file containing the entire Cabal
package, -i.e.- all the same files that were in the provided project skeleton.

## Specification

- Level 1:
    - Your own name and email appear in the `author` and `maintainer`
      fields in `csci365-game.cabal`, and in the copyright notice in
      `LICENSE`.
    - Your game runs and allows the user to play a game without error.
- Level 2:
    - All requirements for Level 1.
    - Your code conforms to the style guidelines.
    - `app/Game.hs` is at least 100 lines of code (including a
      reasonable number of blank lines and comments).
- Level 3:
    - All requirements for Level 2.
    - `app/Game.hs` is at least 200 lines of code.
    - Your code makes idiomatic use of higher-order recursion patterns
      as appropriate such as `map` and `filter`.
    - Your code makes use of at least one additional package from
      Hackage (-e.g.- `containers`).
