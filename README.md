# EK Programming Language (GLaDOS)

<div align="center">
    <img src="https://static.wikia.nocookie.net/epicpixelbattles/images/5/5f/400px-GLaDOS_P2.png/revision/latest?cb=20191127181958">

![Static Badge](https://img.shields.io/badge/EK-programming_language-blue)

</div>
The goal of this project is to implement a programming language of our own design in Haskell.

## Needed Dependencies ⚙️

The best way to install Haskell, is using [GHCup](https://www.haskell.org/ghcup/).

## Build the Project 🛠️

After installing the necessary dependencies, in order to build the project, use the Makefile. Follow these steps:

- Clone the project:

```sh
git clone git@github.com:EpitechPromo2026/B-FUN-500-PAR-5-2-glados-jeremy.elalouf.git
```

- Go to the project directory:

```sh
cd B-FUN-500-PAR-5-2-glados-jeremy.elalouf.git
```

- Build using Makefile:

```sh
make
```

## How to Use? 🛠️

Run `./glados` to open the REPL.

To use the standard library, run `cd stdlib; ../glados`. Then, you can type expressions:

```sh
glados> import std
glados> 6 * 7
42
```

You can even define functions!

```sh
glados> fn twice _ = 2 * _
glados> twice 3
6
```

For more information about the language, check out [Getting started with EK](https://github.com/EpitechPromo2026/B-FUN-500-PAR-5-2-glados-jeremy.elalouf/wiki/Getting-Started-with-EK).
