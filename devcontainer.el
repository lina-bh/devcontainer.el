;;; devcontainer.el --- Support for Visual Studio Code-compatible devcontainers (https://containers.dev/).  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Lina Bhaile

;; Author: Lina Bhaile <emacs-devel@linabee.uk>
;; Version: 1.0
;; Package-Requires: (tramp cl-lib)
;; Keywords: comm processes tools unix
;; URL: https://github.com/lina-bh/devcontainer.el

;; This file is not part of GNU Emacs.

;; Permission to use, copy, modify, and/or distribute this software for
;; any purpose with or without fee is hereby granted.

;; THE SOFTWARE IS PROVIDED “AS IS” AND THE AUTHOR DISCLAIMS ALL
;; WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES
;; OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE
;; FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY
;; DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN
;; AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT
;; OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

;;; Commentary:

;; This package provides two commands, `devcontainer-up' and `devcontainer-down'
;; which uses the \"devcontainer\" CLI to build containers, and Tramp to connect
;; to the container.

;;; Code:
(require 'cl-lib)
(require 'tramp-container)

(defgroup devcontainer nil "Support for Visual Studio Code-compatible devcontainers (https://containers.dev/).")

(defcustom devcontainer-engine "podman"
  "The container engine to use, one of \"podman\" or \"docker\".

To specify the path or name of the podman/docker executable, customise
`tramp-podman-program' or `tramp-docker-program'."
  :group 'devcontainer
  :type '(choice (const "podman")
                 (const "docker")))

(defcustom devcontainer-dotfiles-repository nil
  "--dotfiles-repository argument to devcontainer CLI.

If non-nil, this argument is passed to devcontainer which should clone
and install your dotfiles inside the container.  See
`devcontainer--up-build-command'."
  :type '(choice (const nil)
                 string)
  :group 'devcontainer)

(defcustom devcontainer-cli '("devcontainer")
  "Initial elements of any devcontainer command arguments.

If you need to specify where your Node.js is, put that before the
absolute path to \"devcontainer\"."
  :type '(list string)
  :group 'devcontainer)

(defun devcontainer--buffer-file-name (&optional buf)
  "Get a file name associated with BUF.  If BUF is nil consider the current buffer."
  (setq buf (or buf (current-buffer)))
  (or (buffer-file-name buf)
      (buffer-local-value 'dired-directory buf)))

(defun devcontainer--local-buffers (dir)
  "Return a list of local buffers whose paths are descended from DIR."
  (let (bufs file-name)
    (dolist (buf (buffer-list))
      (when (and (setq file-name (devcontainer--buffer-file-name buf))
                 (file-in-directory-p file-name dir))
        (push buf bufs)))
    bufs))

(defun devcontainer--remote-buffers (host)
  "Return a list of remote buffers on remote HOST."
  (let (bufs)
    (dolist (buf (tramp-list-remote-buffers))
      (let ((vec (devcontainer--tramp-file-vec buf)))
        (when (and vec
                   (string= host (tramp-file-name-host vec)))
          (push buf bufs))))
    bufs))

(defun devcontainer--save-and-kill-buffers (bufs)
  "Save and kill BUFS."
  (dolist (buf bufs)
    (save-buffer buf)
    (kill-buffer buf)))

(cl-defun devcontainer--find-container-workspace (
                                                  &key host
                                                  &key user
                                                  &key dir
                                                  )
  "Call `dired' to edit the workspace root mounted inside the devcontainer.
HOST is the container ID.
USER is the user inside the container.
DIR is the path to the workspace mount."
  (dired (format "/%s:%s@%s:%s"
                 devcontainer-engine
                 user
                 host
                 dir)))

(defun devcontainer--up-make-stdout-filter (workspace-folder)
  "Make a lambda suitable as a filter for running \"devcontainer up\".

When \"devcontainer up\" returns, parse the JSON object we get from stdout
and call `devcontainer--find-container-workspace' with the extracted fields.
Then, clean up the local buffers in WORKSPACE-FOLDER.

Errors if the object's \"outcome\" field doesn't equal \"success\"."
  (lambda (proc out)
    ;; Plenty of bugs and missing checks here as it is.
    (let ((object (json-parse-string out :object-type 'plist)))
      (if (string-equal "success" (plist-get object :outcome))
          (prog1
              (devcontainer--find-container-workspace
               :user (plist-get object :remoteUser)
               :host (plist-get object :containerId)
               :dir (plist-get object :remoteWorkspaceFolder))
            (devcontainer--save-and-kill-buffers
             (devcontainer--local-buffers workspace-folder)))
        (error "Devcontainer-up did not succeed: %S" object)))))

(defun devcontainer--local-workspace-folder ()
  "Get the local root of the workspace which contains the devcontainer config.

When prefixed, the root directory is read from the minibuffer.

Errors if the given path is remote or \".devcontainer.json\" or
\".devcontainer/devcontainer.json\" cannot be found."
  (let ((path (if current-prefix-arg
                  (read-directory-name "Directory with .devcontainer.json: ")
                (or (devcontainer--buffer-file-name)
                    (error "Buffer must be attached to a file or directory")))))
    (if (file-remote-p path)
        ;; FIXME is this worth doing if its even possible
        ;; can do something like "/ssh:user@host|podman:vscode@ctr/"
        ;; but i can tell it involves pain.
        (error "Will not create devcontainers for remote connections"))
    (expand-file-name
     (or
      (locate-dominating-file path ".devcontainer/devcontainer.json")
      (locate-dominating-file path ".devcontainer.json")
      (error "Could not locate devcontainer.json")))))

(defun devcontainer--docker-path ()
  "Return the path of the Docker-compatible command to call.
If `devcontainer-engine' equals \"docker\", use `tramp-docker-program',
else `tramp-podman-program'."
  (if (string= "docker" devcontainer-engine)
      tramp-docker-program
    tramp-podman-program))

(defun devcontainer--up-build-command (workspace-folder)
  "Assemble a list of arguments to run a devcontainer for WORKSPACE-FOLDER."
  (let ((command (append
                  devcontainer-cli
                  (list
                   "up"
                   (format "--docker-path=%s" (devcontainer--docker-path))
                   (format "--workspace-folder=%s" workspace-folder)))))
    (if devcontainer-dotfiles-repository
        (setq command (append command
                              (list
                               (format "--dotfiles-repository=%s"
                                       devcontainer-dotfiles-repository)))))
    command))

(defun devcontainer--buffer (subcmd label)
  "Return an appropriately named buffer for op SUBCMD with LABEL."
  (let ((buffer (get-buffer-create
                 (format "*devcontainer %s %s*" subcmd label))))
    (with-current-buffer buffer
      ;;   (fundamental-mode)
      ;;   (erase-buffer)
      (run-mode-hooks)
      (goto-char (point-min)))
    buffer))

(defun devcontainer-up ()
  "Bring up a devcontainer."
  (interactive)
  (let* ((workspace-folder (devcontainer--local-workspace-folder))
         (command (devcontainer--up-build-command workspace-folder))
         (buffer (devcontainer--buffer "up" workspace-folder)))
    (display-buffer buffer)
    (make-process
     :name (string-join command " ")
     :buffer nil
     :filter (devcontainer--up-make-stdout-filter workspace-folder)
     :stderr buffer
     :command command
     :connection-type 'pipe)))

(defun devcontainer--tramp-file-vec (&optional buf)
  "Return the `tramp-file-name' object for BUF or the current buffer."
  (and-let* ((name (devcontainer--buffer-file-name buf))
             (ident (file-remote-p name 'identification)))
    (and ident (tramp-dissect-file-name ident))))

(defun devcontainer--container-remove (id)
  "Stop and remove container named by ID."
  (string-trim-right
   (with-output-to-string
     (call-process (devcontainer--docker-path)
                   nil           ; INFILE
                   standard-output
                   nil           ; DISPLAY
                   "rm" "-f" id))))

(defun devcontainer--container-inspect (id &rest json-args)
  "Call `devcontainer-engine' to gather information about the container named ID.
JSON-ARGS is passed to `json-parse-buffer' (which see)."
  (with-temp-buffer
    (unless (= 0 (prog1
                     (call-process (devcontainer--docker-path)
                                   nil
                                   t
                                   nil
                                   "container"
                                   "inspect"
                                   id)
                   (goto-char (point-min))))
      (search-forward "\n")
      (error "%s container inspect %s failed: %s"
             devcontainer-engine
             id
             (string-trim (buffer-substring-no-properties (point) (point-max)))))
    (aref (apply #'json-parse-buffer json-args) 0)))

(defun devcontainer--container-labels-tbl (id)
  "Return labels of container named ID as hash table."
  (thread-last
    (devcontainer--container-inspect id :object-type 'hash-table)
    (gethash "Config")
    (gethash "Labels")))

(defun devcontainer-down ()
  "Save and kill buffers opened inside the devcontainer and remove it."
  (interactive)
  (let* ((vec (devcontainer--tramp-file-vec))
         host)
    (if vec
        (setq host (tramp-file-name-host vec))
      (error "Not inside a devcontainer buffer"))
    (devcontainer--save-and-kill-buffers (devcontainer--remote-buffers host))
    (dired (gethash "devcontainer.local_folder"
                    (devcontainer--container-labels-tbl host)))
    (tramp-cleanup-connection vec)
    (message "%s rm -f %s succeeded"
             devcontainer-engine
             (devcontainer--container-remove host))))

(provide 'devcontainer)

;;; devcontainer.el ends here
