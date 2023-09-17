[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![JCS-ELPA](https://raw.githubusercontent.com/jcs-emacs/badges/master/elpa/v/eask.svg)](https://jcs-emacs.github.io/jcs-elpa/#/eask)
[![MELPA](https://melpa.org/packages/eask-badge.svg)](https://melpa.org/#/eask)
[![MELPA Stable](https://stable.melpa.org/packages/eask-badge.svg)](https://stable.melpa.org/#/eask)
<a href="https://www.gnu.org/software/emacs/download.html"><img align="right" src="etc/badges/emacs.svg" alt="Emacs"></a>

<a href="#"><img align="right" src="https://raw.githubusercontent.com/emacs-eask/cli/master/docs/static/logo.png" width="20%"></a>

# eask
> Core Eask APIs

[![Build](https://github.com/emacs-eask/eask/actions/workflows/build.yml/badge.svg)](https://github.com/emacs-eask/eask/actions/workflows/build.yml)
[![CI](https://github.com/emacs-eask/eask/actions/workflows/test.yml/badge.svg)](https://github.com/emacs-eask/eask/actions/workflows/test.yml)
[![System](https://github.com/emacs-eask/eask/actions/workflows/system.yml/badge.svg)](https://github.com/emacs-eask/eask/actions/workflows/system.yml)

The goal of this package is to provide function signatures so you can
develop Eask with your elisp environment. Few important feature that
this package can do:

1. Provide auto-completion with `capf` (`auto-complete`, `company`, `corfu`, etc)
2. Provide function signatures and display arglist (`eldoc`)

Generally, you would not want to call any of these functions or use any of
these variables from your Emacs editor environment. Unless you are extending
Eask's core functionalities.

## 🔨 Usage

Call the following whenever you need to know Eask's API.

```elisp
(require 'eask-core)
```

Or enable it when the project is a valid Eask project,

```elisp
(add-hook 'emacs-lisp-hook #'eask-api-setup)
```

## 🔧 Customization

### 🧪 Variables

- `eask-api-strict-p` - Set to nil if you want to load Eask API whenever it's possible.

## Contribute

[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg)](http://makeapullrequest.com)
[![Elisp styleguide](https://img.shields.io/badge/elisp-style%20guide-purple)](https://github.com/bbatsov/emacs-lisp-style-guide)
[![Donate on paypal](https://img.shields.io/badge/paypal-donate-1?logo=paypal&color=blue)](https://www.paypal.me/jcs090218)
[![Become a patron](https://img.shields.io/badge/patreon-become%20a%20patron-orange.svg?logo=patreon)](https://www.patreon.com/jcs090218)

If you would like to contribute to this project, you may either
clone and make pull requests to this repository. Or you can
clone the project and establish your own branch of this tool.
Any methods are welcome!
