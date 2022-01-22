;;; setup package installation
;; credit: https://github.com/bbatsov/prelude
;; credit: https://macowners.club/posts/emacs-config-without-use-package/

(defun user/packages-installed-p (pkg-list)
  "Check if all packages are installed."
  (cl-every #'package-installed-p pkg-list))

(defun user/ensure-pkg (package)
  "Ensure package is installed"
  (unless (package-installed-p package)
    (package-install package)))

(defun user/install-pkgs (pkg-list)
  "Install packages from pkg-list and ensure them"
  (unless (user/packages-installed-p pkg-list)
    (package-refresh-contents)
    ;; install the missing packages
    (mapc #'user/ensure-pkg pkg-list)))

