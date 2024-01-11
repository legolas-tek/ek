# GRPH Visual Studio Code extension

Write GRPH in VSCode using LSP and this extension.

## Install

Install the extension from source using `npm install`.

To run, it needs the GRPH LSP server installed. Clone the compiler from [here](https://github.com/grph-lang/grph), and build it using `swift build --product LSP` (Swift needs to be installed).

You must then set the VSCode setting `grph.languageServerPath` to `{directory where the GRPH compiler was cloned}/.build/debug/LSP` (append `.exe` to that on Windows)
