# Kaomel

A snappy kaomoji picker for Emacs.

![screen](screenshot.png)

## Description
`kaomel` is an Emacs package that allows you to easily insert kaomojis into your buffer using Helm. With a collection of almost 1000 kaomojis to choose from, you can quickly find the perfect expression to enhance your messages and documents.

## Features
- Interactive Helm interface for selecting kaomojis.
- Insert selected kaomoji at the current cursor position.
- Copy selected kaomoji to the system clipboard.

## Installation

### Manual Installation
1. Download `kaomel.el` to a directory in your Emacs `load-path`.
2. Add the following lines to your Emacs configuration file:

``` emacs-lisp
(require 'kaomel)
```

### Installation via Straight

```emacs-lisp
(straight-use-package
 '(kaomel :host github :repo "gicrisf/kaomel"
   :files ("*.el")
   :includes (helm)))
```

### Install with use-package

```emacs-lisp
(use-package kaomel
    :ensure t
    :config
    (require 'helm))
```

### Install on Doom Emacs
If you're on Doom Emacs like me, you can install with the `package!` macro:

```emacs-lisp
(package! kaomel :recipe (:host github :repo "gicrisf/kaomel"))
```

## Usage
- To use `kaomel`, simply run `M-x kaomel-insert` or bind it to a key combination of your choice.
- Once the Helm interface opens, you can navigate through the available kaomojis using the arrow keys or by typing search terms.
- Press `RET` on the desired kaomoji to insert it into your buffer at the current cursor position.

Similarly, run `M-x kaomel-to-clipboard` to copy the selected kaomoji to the system clipboard.

## TODOs
- [ ] Easier selection for the most used kaomojis;
- [ ] Better tagging system;
- [ ] Finer helm configuration;
- [ ] More kaomojis!

## License
`kaomel` is licensed under the GPL3 license, see the `LICENSE` file for more information.

## Bug Reporting and Contributions
- Issues, suggestions, and pull requests are welcome on the GitHub repository: https://github.com/gicrisf/kaomel
