# ada-alire
An Emacs package which provides interactive helper commands
for `alr` tool from Alire's project.

No need to switch context from your editor to terminal.

## Installation
Currently, this package is just a single file and not available in
MELPA. You have to install and load the file manually (and load
the package every time you want the commands to be exposed at Emacs launch).
Simply put `(load-file "/path/to/ada-alire.el")` inside your `.emacs` file.

You need `alr` tool from [alire](https://github.com/alire-project/alire) project
to be installed and available in `PATH` to use this package.

## Features
- `alr-run`
- `alr-build`
- `alr-info`
- `alr-get`
- `alr-with`
- `alr-list-installable-crates`
- `alr-set-project-path`
- `alr-cd-to-project`
- `alr-print-project-path`

## Status
Unstable (use at your own risk)

## License
This software is licensed under the MIT license. See COPYING
file for more details.
