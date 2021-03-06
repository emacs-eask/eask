<a href="#"><img align="right" src="./docs/static/logo.png" width="20%"></a>

# Eask
> Command-line tool for building and testing Emacs Lisp packages

[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-green.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Emacs Version](https://img.shields.io/badge/Emacs-26.1+-7F5AB6.svg?logo=gnu%20emacs&logoColor=white)](https://www.gnu.org/software/emacs/download.html)
[![Release](https://img.shields.io/github/release/emacs-eask/eask.svg?logo=github)](https://github.com/emacs-eask/eask/releases/latest)

Eask is a command-line tool that helps you build, lint, and test Emacs Lisp
packages. It creates a clean environment to sandbox your elisp code without
influencing your personal configuration. Eask aims to be:

* **Consistent** enough to sandbox across all systems
* **General** enough to have Emacsers often used commands (byte-compile, checkdoc, etc)
* **Robust** enough to provide useful results even in the presence of user errors
* **Dependency-free** so that the tool can be run on any platform

## ๐ Links

* [Documentation](https://emacs-eask.github.io/)
* [Installation](https://emacs-eask.github.io/Getting-Started/Install-Eask/)
* [Command-line interface](https://emacs-eask.github.io/Getting-Started/Commands-and-options/)
* [Examples](https://emacs-eask.github.io/Examples/Real-project-examples/)

## ๐งช Testing

###### Documentation

| Description                            | Done | Status                                                                                                                                            |
|----------------------------------------|------|---------------------------------------------------------------------------------------------------------------------------------------------------|
| Keep the documentation page up to date | โ    | [![Docs](https://github.com/emacs-eask/eask/actions/workflows/docs.yml/badge.svg)](https://github.com/emacs-eask/eask/actions/workflows/docs.yml) |

###### Development

| Description                               | Done | Status                                                                                                                                                     |
|-------------------------------------------|------|------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Compile source and check redefined        | โ    | [![Compile](https://github.com/emacs-eask/eask/actions/workflows/compile.yml/badge.svg)](https://github.com/emacs-eask/eask/actions/workflows/compile.yml) |
| Compatibility check for each Emacs version | โ    | [![Compat](https://github.com/emacs-eask/eask/actions/workflows/compat.yml/badge.svg)](https://github.com/emacs-eask/eask/actions/workflows/compat.yml)    |
| Build executables                         | โ    | [![Build](https://github.com/emacs-eask/eask/actions/workflows/build.yml/badge.svg)](https://github.com/emacs-eask/eask/actions/workflows/build.yml)       |

###### Commands

| Description                                 | Done | Status                                                                                                                                                                                |
|---------------------------------------------|------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Test commands in global (config) mode       | โ    | [![Global](https://github.com/emacs-eask/eask/actions/workflows/global.yml/badge.svg)](https://github.com/emacs-eask/eask/actions/workflows/global.yml)                               |
| Test commands in development (package) mode | โ    | [![Local](https://github.com/emacs-eask/eask/actions/workflows/local.yml/badge.svg)](https://github.com/emacs-eask/eask/actions/workflows/local.yml)                                  |
| Test to install packages                    | โ    | [![Install](https://github.com/emacs-eask/eask/actions/workflows/install.yml/badge.svg)](https://github.com/emacs-eask/eask/actions/workflows/install.yml)                            |
| Test execute commands                       | โ    | [![Exec](https://github.com/emacs-eask/eask/actions/workflows/exec.yml/badge.svg)](https://github.com/emacs-eask/eask/actions/workflows/exec.yml)                                     |
| Test emacs commands                         | โ    | [![Emacs](https://github.com/emacs-eask/eask/actions/workflows/emacs.yml/badge.svg)](https://github.com/emacs-eask/eask/actions/workflows/emacs.yml)                                  |
| Test to search packages                     | โ    | [![Search](https://github.com/emacs-eask/eask/actions/workflows/search.yml/badge.svg)](https://github.com/emacs-eask/eask/actions/workflows/search.yml)                               |
| Test to upgrade and check outdated packages | โ    | [![Outdated_Upgrade](https://github.com/emacs-eask/eask/actions/workflows/outdated_upgrade.yml/badge.svg)](https://github.com/emacs-eask/eask/actions/workflows/outdated_upgrade.yml) |
| `Eask`-file checker                         | โ    | [![Checker](https://github.com/emacs-eask/eask/actions/workflows/checker.yml/badge.svg)](https://github.com/emacs-eask/eask/actions/workflows/checker.yml)                            |

###### Options

| Description          | Done | Status                                                                                                                                                     |
|----------------------|------|------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Test option switches | โ    | [![Options](https://github.com/emacs-eask/eask/actions/workflows/options.yml/badge.svg)](https://github.com/emacs-eask/eask/actions/workflows/options.yml) |

###### Test

| Description               | Done | Status                                                                                                                                                                             |
|---------------------------|------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Test `ert` command        | โ    | [![Test ert](https://github.com/emacs-eask/eask/actions/workflows/test_ert.yml/badge.svg)](https://github.com/emacs-eask/eask/actions/workflows/test_ert.yml)                      |
| Test `ert-runner` command | โ    | [![Test ert-runner](https://github.com/emacs-eask/eask/actions/workflows/test_ert-runner.yml/badge.svg)](https://github.com/emacs-eask/eask/actions/workflows/test_ert-runner.yml) |
| Test `buttercup` command  | โ   | [![Test buttercup](https://github.com/emacs-eask/eask/actions/workflows/test_buttercup.yml/badge.svg)](https://github.com/emacs-eask/eask/actions/workflows/test_buttercup.yml)    |
