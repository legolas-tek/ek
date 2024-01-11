# Contributing

For contributing, you need additional dependencies

## Needed Dependencies ⚙️

The best way to install Haskell, is using [GHCup](https://www.haskell.org/ghcup/).

For functional tests, you will need LLVM-lit (lit on pip), and LLVM FileCheck (included with LLVM development packages).

On Fedora:

```sh
sudo dnf install llvm
pip install lit
```

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

- To use the project globally, install it:

```sh
make install
```

- To build unit tests, use:

```sh
make tests_run
```

- To run functional tests, use:

```sh
lit -v ./test/functional
```
