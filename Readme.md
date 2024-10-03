# Functional programming course in Vilnius university in Autumn semester 2021

## Functional programming lessons
Code which was written during lectures

To Launch the haskell files you'll need to:
```bash
ghci

:l Lesson1.hs # or some other lesson
```

## FunctionalProgrammingProject
Bomberman game implemented in haskell

### Building

```bash
make build

make runServer
#on separate terminal

make runClient

#testing
make test
```

### Controls
- w - move up
- a - move left
- s - move down
- d - move right
- b - plant bomb

### Surroundings
- b - is a bomb
- i - a player
- o - a ghost
- X - brick (can destroy with a bomb)
- \# - wall
- H - gate

The game ends when player touches the ghost(dies) or reaches the gate.

### Images
![Image](/Images/game.png)

### Running on Windows

You might want to add `--stack-yaml stack-901.yaml` to your stack commands (
e.g. `stack --stack-yaml stack-901.yaml run`) so you would no need to press Enter
after every command keys (`a`, `s`, `d`, `w`).

### Contributors
The functional programming project laboratory assignments were implemented by:
- Arentas Meinorius
- Arnas Stonkus
- Pijus Petkevičius
- Dominykas Višnevskis

