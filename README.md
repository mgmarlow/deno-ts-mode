# deno-ts-mode

A major mode for Deno, based on `typescript-ts-mode`.

## Installation

Requires Emacs 29+ with tree-sitter installed.

```elisp
(package-vc-install "https://git.sr.ht/~mgmarlow/deno-ts-mode")
```

### Installing tree-sitter parsers

`deno-ts-mode` depends on tree-sitter. You will probably want both the
`typescript` and `tsx` parsers installed:

```elisp
(setq treesit-language-source-alist
      '((typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")))

(mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))
```

### Project detection

When `deno-ts-setup-auto-mode-alist` is used, `deno-ts-mode` will
detect whether the currently visited TypeScript file (`.ts` or `.tsx`)
is a Deno project by looking at its project root for a `deno.json`
file. If successful, that TypeScript file is considered a Deno file
for the purposes of `deno-ts-mode`. Otherwise,
`deno-ts-setup-auto-mode-alist` will fallback to `typescript-ts-mode`.

This all means that the default Deno file detection is based on the
presence of a `deno.json` file in an [Emacs
Project](https://www.gnu.org/software/emacs/manual/html_node/emacs/Projects.html). Generally
speaking, projects are only detectable if they are under version
control.

## Eglot usage example

```elisp
(use-package deno-ts-mode
  :config
  (deno-ts-setup-auto-mode-alist))

(use-package eglot
  :ensure t
  :hook ((deno-ts-mode . eglot-ensure))
  :config
  (deno-ts-setup-eglot))
```
