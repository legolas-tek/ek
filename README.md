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
pip install lit && sudo dnf install llvm
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

## How to Use? 🛠️

- run:

```sh
./glados
```

- And then you are gonna have:  
  `glados>`

- And you can use one of there symbols:  
  `#t`: Represents true. It's used in LISP to denote a logically true value.  
  `#f`: Represents false. It's used for denoting a logically false value.  
  `+`: The addition operator. It's used to add multiple numbers together.  
  `(+ 3 4)` would return 7.  
  `*`: The multiplication operator. It's used for multiplying multiple numbers.  
  `(* 2 3)` would return 6.  
  `-`: The subtraction operator. It's used to calculate the difference between two numbers.  
  `(- 10 4)` would return 6.  
  `/`: The division operator. It's used to divide one number by another.  
  `(/ 20 5)` would return 4.  
  `max`: The maximum function. It's used to find the maximum value among a set of numbers.  
  `(max 1 5 3)` would return 5.  
  `min`: The minimum function. It's used to find the minimum value among a set of numbers.  
  `(min 1 5 3)` would return 1.  
  `not`: The logical negation operator. It returns the opposite of the given logical value.  
  `(not #t)` would return `#f` (false).  
  `and`: The logical AND operator. It returns true if all its arguments are true.  
  `(and #t #f)` wound return `#f` (false).  
  `or`: The logical OR operator. It returns true if any of its arguments is true.  
  `(or #t #f)` would return `#t` (true).
  `xor`: The logical XOR (exclusive OR) operator. It returns true if exactly one of its arguments is true.  
  `(xor #t #f)` would return `#t` (true).  
  `eq?`: The equality test function. It checks if two arguments refer to the exact same object.  
  `<`: The less-than comparison operator. It returns true if the first argument is less than the second.  
  `(< 2 5)` would return `#t` (true).  
  `>`: The greater-than comparison operator. It returns true if the first argument is greater than the second.  
  `(> 5 3)` would return `#t` (true),  
  `<=`: The less-than-or-equal-to comparison operator. It returns true if the first argument is less than or equal to the second.  
  `(<= 2 5)` would return `#t` (true),  
  `>=`: The greater-than-or-equal-to comparison operator. It returns true if the first argument is greater than or equal to the second.  
  `(>= 5 2)` would return `#t` (true).  
  `=`: The numeric equality operator. It returns true if two numbers are equal.  
  `(= 3 3)` would return `#t` (true).

- And you can use like this:

```sh
glados> (+ 1 2)
3
```

## Test the Project 🛠️

After installing the necessary dependencies, in order to test the project, use the Makefile. Follow this command:

```sh
make tests_run
```
