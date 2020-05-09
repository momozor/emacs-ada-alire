# ada-alire
An Emacs package which provides interactive helper commands
for `alr` tool from Alire's project.

No need to switch context from your editor to terminal.

## Installation
Currently, this package is just a single file and not available in
MELPA. You have to install and load the file manually. 

To load it everytime you run Emacs, simply put `(load-file "/path/to/ada-alire.el")` inside your `.emacs` file. Next time you launch Emacs, `alr-with` and many others will be available by default.

You need `alr` tool from [alire](https://github.com/alire-project/alire) project
to be installed and available in `PATH` to use this package.

Currently only known to work on Linux.

## Usage

> You must set project path (where alire/ lives in) before running ada-alire
commands!!!

`M-x alr-set-project-path`

## Features

Invoke the commands with `M-x alr-command`

### Build & Run.
- `alr-run`

### Build only.
- `alr-build`

### "Install" crate into your local project.
- `alr-with`

### Print basic info of the local project.
- `alr-info`

### List all installable crates (i.e with alr-with)
- `alr-list-installable-crates`

### Letting alire-ada know where to find the local project you want to work on.
- `alr-set-project-path`

### Change session directory to the local project.
- `alr-cd-to-project`

### Print registered local project path known by alire-ada.
- `alr-print-project-path`

## Status
Unstable (use at your own risk)

## License
This software is licensed under the MIT license. See COPYING
file for more details.
