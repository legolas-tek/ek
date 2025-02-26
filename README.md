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
git clone git@github.com:legolas-tek/ek.git
```

- Go to the project directory:

```sh
cd ek
```

- Build using Makefile:

```sh
make
```

- To use the project globally, install it:

```sh
make install
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

For more information about the language, check out [Getting started with EK](https://github.com/legolas-tek/ek/wiki/Getting-Started-with-EK).

## Editor Support

If you use Emacs, you can get Syntax Hightlighting using our plugin: [ek-mode](editor/ek-mode)
