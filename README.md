This package is being merged
============================

I'm working on adding the features of this package to https://github.com/johannes-mueller/devcontainer.el. 
You should use that instead.

devcontainer.el
===============

devcontainer.el adds support for Visual Studio Code-compatible devcontainers
(https://containers.dev/) to GNU Emacs. It provides two interactive commands:

`devcontainer-up' searches up from the current directory for a devcontainer
configuration file, calls the devcontainer CLI to build that container, then
opens the mounted workspace inside the new container using Tramp's Docker/Podman
support.

`devcontainer-down' cleans up all buffers associated with the current container
and deletes that container from the container engine.

Requirements
------------

You need the devcontainer CLI. You can install it on your system or use a
JavaScript package runner like "npx" or "bunx". (I haven't tried this because
they don't work on my system.)

You should have either Docker or Podman on your system, you can choose which by
setting `devcontainer-engine'. By default this is Podman. The location of the
command can be set using `tramp-podman-program' or `tramp-docker-program'.

Installation
------------

You should be able to install this package with `package-vc-install'. In any
case, copying devcontainer.el to your load path and doing (require
'devcontainer) should work.

Licence
-------

At present the text of devcontainer.el is licensed under 0BSD with no
restrictions on usage or modification. IANAL but when loaded into Emacs you are
using it under the terms of Emacs (GPL3.0-or-later).
