# deno-ts-mode

A major mode for Deno, based on `typescript-ts-mode`.

## Installation

Requires Emacs 29+ with tree-sitter installed.

```elisp
(package-vc-install "https://git.sr.ht/~mgmarlow/deno-ts-mode")
```

### Installing tree-sitter parsers

`deno-ts-mode` depends on tree-sitter. You need the following parsers:

```elisp
(setq treesit-language-source-alist
      '((typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")))
```

Run `treesit-install-language-grammer` for both `typescript` and `tsx`.

## Eglot example

```elisp
(use-package deno-ts-mode
  :config
  (deno-setup-auto-mode-alist))

(use-package eglot
  :ensure t
  :hook ((deno-ts-mode . eglot-ensure))
  :config
  (deno-setup-eglot))
```
