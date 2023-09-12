# deno-ts-mode

[![MELPA](https://melpa.org/packages/deno-ts-mode-badge.svg)](https://melpa.org/#/deno-ts-mode)

A major mode for Deno, based on `typescript-ts-mode`.

## Features

- Syntax highlighting (based on `typescript-ts-mode`)
- Task automation
- Eglot configuration
- `auto-mode-alist` resolution for ".ts" and ".tsx" files

## Installation

Requires Emacs 29+ with tree-sitter installed.

Available on [MELPA](https://melpa.org/#/deno-ts-mode):

```elisp
(use-package deno-ts-mode
  :ensure t)
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

`deno-ts-mode` will automatically configure `auto-mode-alist` to check
for Deno projects when resolving the ".ts" and ".tsx" file
extensions. Whether or not a project is considered a Deno project is
based on the existence of a [Deno configuration
file](https://deno.land/manual@v1.36.4/getting_started/configuration_file)
at the project root. Both `deno.json` and `deno.jsonc` are considered
valid configuration files.

If a ".ts" or ".tsx" file is not in a project with a Deno
configuration file, `deno-ts-mode-maybe` will fallback to either
`typescript-ts-mode` or `tsx-ts-mode` depending on the extension.

## Task automation

If your project's [deno configuration
file](https://deno.land/manual@v1.36.2/getting_started/configuration_file)
contains tasks you can run them directly from Emacs.

```
M-x deno-ts-run-task
```


## Eglot setup example

```elisp
(use-package deno-ts-mode
  :ensure t)

(use-package eglot
  :ensure t
  :hook ((deno-ts-mode . eglot-ensure)
         (deno-tsx-ts-mode . eglot-ensure)))
```

## Contributing

Please direct bug reports or patches to the [mailing
list](https://lists.sr.ht/~mgmarlow/public-inbox).

## License

Licensed under [GPL-3.0](./COPYING).
