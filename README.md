# Kaomel

A snappy kaomoji picker for Emacs.

![screen](screenshot.png)

## Description
The package `kaomel` is an Emacs package that provides easy insertion of kaomojis into your buffer. With a collection of almost 1000 kaomojis to choose from, you can quickly find the perfect expression to enhance your messages and documents. It offers two different commands for copying the kaomojis to the clipboard or inserting them directly into the buffer.

## Features
- [x] Interactive interface for selecting kaomojis.
- [x] Insert selected kaomoji at the current cursor position.
- [x] Copy selected kaomoji to the system clipboard.
- [x] Better tagging system;
- [x] Finer helm configuration;
- [x] Vertico support
- [x] Kana transliteration
- [x] Embedded dataset as vectorized elisp for speed.

## Installation

### Install manually
1. Clone the GitHub repository for the Emacs package using the `git clone` command:
```
git clone https://github.com/gicrisf/kaomel
```

2. Move the cloned package directory into your Emacs' load path, which is typically `~/.emacs.d/`:
```
mv kaomel ~/.emacs.d/
```

3. Open your Emacs configuration file, usually `~/.emacs.d/init.el` or `~/.emacs`:
```
emacs ~/.emacs.d/init.el
```

4. Require the library in your Emacs configuration file. Assuming that the repository has been moved to the path `~/.emacs.d/`:

```elisp
(add-to-list 'load-path "~/.emacs.d/kaomel")
(require 'kaomel)
```

5. Save the configuration file, then restart Emacs, or reload it.

Now, the package should be installed and loaded whenever you start Emacs.

### Install with use-package

If you use `use-package`, you can follow the manual procedure and replace the text in the 4th step with these:

```emacs-lisp
(use-package kaomel
    :load-path "~/.emacs.d/repo")
```

To install the package directly from the GitHub repository, you can simply write:

```elisp
(use-package romkan.el
  :ensure gicrisf/romkan.el)
```

### Install with Straight

```emacs-lisp
(straight-use-package
 '(kaomel :host github :repo "gicrisf/kaomel"
   :files ("*.el")))
```

### Install on Doom Emacs
If you're on Doom Emacs like me, you can install with the `package!` macro:

```emacs-lisp
(package! kaomel :recipe (:host github :repo "gicrisf/kaomel"))
```

## Usage
- To use `kaomel`, simply run `M-x kaomel-insert` or bind it to a key combination of your choice.
- Once the interface opens, you can navigate through the available kaomojis using the arrow keys or by typing search terms.
- Press `RET` on the desired kaomoji to insert it into your buffer at the current cursor position.

Similarly, run `M-x kaomel-to-clipboard` to copy the selected kaomoji to the system clipboard.

By default, the package uses the completing-read interface. This means that it will work with packages like Vertico, as shown in the screenshot. If you have Helm installed, it will use Helm instead. If you have Helm installed but still prefer to use completing-read, you can by simply setting a variable, like shown in the next paragraph.

### Configuration
This chapter provides documentation for several Emacs Lisp options available in the Kaomel package.

Option: `kaomel-avoid-helm`
(type: boolean, default value: nil)

By default, `kaomel` uses `completing-read` for interactive selection. However, if you have Helm installed, it will use Helm instead. If you still want to use `completing-read` even with Helm installed, you can enforce this by setting a specific variable.

```emacs-lisp
(setq kaomel-avoid-helm t)
```

Option: `kaomel-tag-langs`
(type: list, default values: "orig", "hepburn", "en")

This option determines the preferred languages in the displayed tags used by the Kaomel package. It accepts a list of strings representing the language options. By default, the following languages are available to choose from:

- Original (abbreviated as 'orig')
- Hiragana (abbreviated as 'hira')
- Katakana (abbreviated as 'kana')
- English (abbreviated as 'en')
- Italian (abbreviated as 'it')

To set your preferred languages, customize the `kaomel-tag-langs` variable using options provided by Emacs customization interface. Like the others, this option is under the `kaomel` group.

Example usage:

```emacs-lisp
(setq kaomel-tag-langs '("orig" "en"))
```

Option: `kaomel-only-ascii-tags`
(type: boolean, default value: nil)

- If set to t, Kaomel filters out every non-ascii character from tags.
- This option can be useful when you want to restrict tags to contain only ASCII characters.

Options: `kaomel-heavy-trim-tags`
(type: boolean, default value: nil)

- When set to t, Kaomel will show only the first word for every tag token.
- This option is particularly handy when dealing with multiple languages and you want to have shorter tag strings.

Options: `kaomel-tag-val-separator`
(type: string, default value: " ")

- Specifies the separator that is put between the tags and the associated values.
- You can customize this option to change the separator based on your preference.

Options: `kaomel-tag-tag-separator`
(type: string, default value: " ")

- Determines the separator that is placed between a tag and the tag next to it.
- You can modify this option to change the separator according to your needs.

Option: `kaomel-prompt`
(type: string, default value: "Pick a Kaomoji:")
- Specifies the prompt line displayed when prompting the user to pick a Kaomoji.

Example usage:

```emacs-lisp
(setq kaomel-prompt "Select a Kaomoji:")
```

Changing the value of `kaomel-prompt` will update the prompt line in all relevant interactions within the `kaomel` package.

## License
`kaomel` is licensed under the GPL3 license, see the `LICENSE` file for more information.

## Bug Reporting and Contributions
- Issues, suggestions, and pull requests are welcome on the GitHub repository: https://github.com/gicrisf/kaomel
