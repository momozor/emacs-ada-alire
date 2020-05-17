# ada-alire
An Emacs package which provides interactive helper commands
for `alr` tool from Alire's project.

No need to switch context from your editor to terminal.

## Version

0.2

## Installation
Currently, this package is just a single file and not available in
MELPA. You have to install and load the file manually. 

To load it everytime you run Emacs, simply put `(load-file "/path/to/ada-alire.el")`
inside your `.emacs` file. Next time you launch Emacs, `ada-alire-install-crate` and many others will be available by default.

You need `alr` tool from [alire](https://github.com/alire-project/alire) project
to be installed and available in `PATH` to use this package.

Currently only known to work on Linux.

## Usage

> You must set project path (where alire/ lives in) before running ada-alire
commands!!!

`M-x ada-alire-set-project-path`

## Features

Invoke the commands with `M-x ada-alire-command`

### Build & Run.
- `ada-alire-run`

### Build only.
- `ada-alire-build`

### "Install" crate into your local project.
- `ada-alire-install-crate`

### "Uninstall" crate from your local project.
- `ada-alire-uninstall-crate`

### Clean project directory.
- `ada-alire-clean`

### Print basic info of the local project.
- `ada-alire-info`

### List all installable crates
- `ada-alire-list-installable-crates`

### Letting ada-alire know where to find the local project you want to work on.
- `ada-alire-set-project-path`

### Change session directory to the local project.
- `ada-alire-cd-to-project`

### Print registered local project path known by ada-alire.
- `ada-alire-print-project-path`

## Status
Unstable (use at your own risk)

## License
This software is licensed under the GPL-3.0 or later license. See COPYING
file for more details.
