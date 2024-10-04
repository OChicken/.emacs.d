;;; init-package.el --- package management -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'package)
(add-to-list 'package-archives '( "melpa" . "https://melpa.org/packages/") t)
(package-initialize)

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

(defun package-install-init (package)
  "Install PACKAGE if not exists."
  (unless (package-installed-p package)
    (package-install package)))

(package-install-init 'gnu-elpa-keyring-update)

(provide 'init-package)
;;; init-package.el ends here
