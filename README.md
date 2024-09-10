
---

## Language Implementations

### 1. Smalltalk Implementation (`/war_st`)

- **Language**: Smalltalk
- **Requirements**: You need the Pharo Smalltalk environment to run this version.
- **Instructions**: Detailed instructions for running the Smalltalk version can be found in the [`war_st/README.md`](war_st/README.md) file.
- **Key Files**:
  - `War.st`: The Smalltalk source file containing the game logic.
  - `FINAL Pharo 11.0 - 64bit (stable).zip`: Pharo environment for running the game.

### 2. Elixir Implementation (`/war_ex`)

- **Language**: Elixir
- **Requirements**: You need to have Elixir installed to run this version.
- **Instructions**: Detailed instructions for running the Elixir version can be found in the [`war_ex/README.md`](war_ex/README.md) file.
- **Key Files**:
  - `War.ex`: The main Elixir source file.
  - `mix.exs`: The Mix build configuration file.
  - `Elixir.War.beam`: Compiled Elixir bytecode.

### 3. Haskell Implementation (`/war_hs`)

- **Language**: Haskell
- **Requirements**: You need to have Haskell and Cabal installed to run this version.
- **Instructions**: Detailed instructions for running the Haskell version can be found in the [`war_hs/README.md`](war_hs/README.md) file.
- **Key Files**:
  - `War.hs`: The Haskell source file containing the game logic.
  - `Main.hs`: The main entry point for running the game.
  - `war.cabal`: Cabal configuration file for the Haskell project.
  - `WarTest.hs`: Test file for verifying the game logic.

---

## How to Run Each Version

Each language implementation has its own setup and runtime instructions. Follow the individual README files in each subdirectory for detailed instructions on how to run the game in each specific language:

- [Smalltalk Instructions](war_st/README.md)
- [Elixir Instructions](war_ex/README.md)
- [Haskell Instructions](war_hs/README.md)

---

## Technical Details

### Deck Representation

The deck of cards is represented by a shuffled list of 52 integers, where each integer between 1 and 13 represents the rank of the cards (Ace, 2, 3, ..., King). Each rank appears four times in the deck, representing the four suits (hearts, diamonds, clubs, and spades). 

### Game Logic

- Each round, the players draw one card from their deck. The player with the higher card wins the round and takes both cards.
- If both players draw cards of equal rank, a "war" is initiated. Each player places three cards face down and draws a fourth card to determine the winner.
- The game continues until one player runs out of cards.

---

## Conclusion

This repository demonstrates how the same War card game logic can be implemented in different languages using different programming paradigms. It serves as a showcase of proficiency in **Smalltalk**, **Elixir**, and **Haskell**, and highlights the versatility required to implement the same functionality in various programming languages.

---

### Final Notes

- Make sure to follow the specific language instructions for setup and running the game.
- Each implementation has been designed to pass the required test cases, and no third-party libraries have been used, adhering to the project's constraints.

