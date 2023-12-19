# GLADOS

<div align="center">
    <img src="https://static.wikia.nocookie.net/epicpixelbattles/images/5/5f/400px-GLaDOS_P2.png/revision/latest?cb=20191127181958">

    ![Static Badge](https://img.shields.io/badge/glados-programming_language-blue)

    <p>The goal of this project is to implement a programming language of our own design in Haskell.</p>

</div>

## Needed Dependencies ⚙️

This project will use various dependencies, some are basic, and others are more specific. You can install the specific ones with the following commands:

### To compile, install ghcup

For Linus, macOS:

```sh
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

### To test the code, install lit and llvm

For Ubuntu:

```sh
pip install lit && bash -c "$(wget -O - https://apt.llvm.org/llvm.sh)"
```

For Fedora:

```sh
pip install lit && sudo dnf install llvm"
```

For macOS:

```sh
pip install lit && brew install llvm
```

## Build the Project 🛠️

After installing the necessary dependencies, in order to build the project, use the Makefile. Follow these steps:

- Clone the project:

```sh
git clone git@github.com:EpitechPromo2026/B-CPP-500-PAR-5-1-rtype-jeremy.elalouf.git
```

- Go to the project directory:

```sh
cd B-CPP-500-PAR-5-1-rtype-jeremy.elalouf.git
```

- Build using Makefile:

```sh
make
```

## Test the Project 🛠️

After installing the necessary dependencies, in order to test the project, use the Makefile. Follow this command:

```sh
make tests_run
```
