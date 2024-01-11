# ek-mode emacs major mode

This simple emacs plugin adds support for EK syntax highlighting.

## Install

Simply add this file to your emacs configuration.

As an example, you can add the following to your `~/.emacs`, after cloning this repository:
```elisp
(setq load-path
    (cons (expand-file-name "~/path/to/glados/editor/ek-mode") load-path))
(require 'ek-mode)
```
