;;; perl-utils.el --- Perl utilty commands

;; Copyright (C) 2013 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-perl-utils
;; Version: 0.01

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(eval-when-compile
  (require' cl))

(require 'helm)
(require 'compile)
(require 'ansi-color)
(require 'deferred)
(require 'man)

(defgroup perl-utils nil
  "Perl utilities"
  :prefix "perl-utils:"
  :group 'perl)

(defcustom perl-utils:auto-scroll t
  "Enable auto scroll for build buffer"
  :type 'boolean
  :group 'perl-utils)

(defun perl-utils:do-compile (cmd)
  (let ((compilation-scroll-output perl-utils:auto-scroll))
    (compilation-start cmd t (lambda (x) "*perl utils compilation*"))))

(defun perl-utils:package-root-info ()
  (loop with curdir = default-directory
        for file in '("Build.PL" "Makefile.PL")
        when (locate-dominating-file curdir file)
        return (cons file (expand-file-name (file-name-directory it)))))

(defvar perl-utils:build-candidates
  '(("Test Package"  . test)
    ("Build Package" . build)
    ("Install Package" . install)
    ("Initialize package" .init)))

(defun perl-utils:build-command (build-file action)
  (cond ((string= build-file "Build.PL")
         (case action
           (init  "perl Build.PL")
           (build "./Build")
           (test  "./Build && ./Build test")
           (install "./Build install")))
        ((string= build-file "Makefile.PL")
         (case action
           (init "perl Makefile.PL")
           (build "make")
           (test "make && make test")
           (install "make install")))))

(defun perl-utils:build-action (action)
  (let* ((root-info (perl-utils:package-root-info))
         (build-file (car root-info))
         (command (perl-utils:build-command build-file action))
         (default-directory (cdr root-info)))
    (perl-utils:do-compile command)))

(defvar perl-utils:build-source
  '((name . "Build Package")
    (candidates . perl-utils:build-candidates)
    (action . perl-utils:build-action)))

(defvar perl-utils:test-source
  '((name . "Test")
    (init . perl-utils:test-init)
    (candidates-in-buffer)
    (action . perl-utils:test-action)))

(defun perl-utils:test-init ()
  (let* ((root-info (perl-utils:package-root-info))
         (rootdir (cdr root-info)))
    (helm-attrset 'build-file (car root-info) perl-utils:test-source)
    (helm-attrset 'root rootdir perl-utils:test-source)
    (with-current-buffer (helm-candidate-buffer 'global)
      (let ((default-directory rootdir)
            (cmd "find t xt -type f -name '*.t'"))
        (if (zerop (call-process-shell-command cmd nil t))
            (when (string= (buffer-string) "")
              (error "There are no tests in this project!!"))
          (error "Error: 'find' command failed"))))))

(defun perl-utils:test-command (file)
  (let* ((build-file (helm-attr 'build-file perl-utils:test-source))
         (precmd (perl-utils:build-command build-file 'build)))
    (format "%s && prove -bv %s" precmd file)))

(defun perl-utils:test-action (file)
  (let ((default-directory (helm-attr 'root perl-utils:test-source)))
    (perl-utils:do-compile (perl-utils:test-command file))))

;;
;; Document
;;
(defvar perl-utils:installed-modules nil)

(defun perl-utils:setup-installed-module ()
  (when (null perl-utils:installed-modules)
    (deferred:$
      (deferred:process-buffer
        "perl" "-MExtUtils::Installed" "-le" "print for ExtUtils::Installed->new->modules")
      (deferred:nextc it
        (lambda (buf)
          (with-current-buffer buf
            (goto-char (point-min))
            (setq perl-utils:installed-modules
                  (loop with modules = nil
                        while (not (eobp))
                        collect
                        (prog1
                            (buffer-substring-no-properties
                             (line-beginning-position) (line-end-position))
                          (forward-line 1))))
            (kill-buffer (current-buffer))))))))

(defun perl-utils:view-document (module)
  (let ((manual-program "perldoc"))
   (Man-getpage-in-background module)))

(defun perl-utils:view-source-code (module)
  (let ((buffer (format "*perl utils source[%s]*" module)))
    (with-current-buffer (get-buffer-create buffer)
      (setq buffer-read-only nil)
      (erase-buffer)
      (let ((ret (call-process "perldoc" nil t nil "-m" module)))
        (unless (zerop ret)
          (error "Faild: perldoc -m %s" module))
        (goto-char (point-min))
        (cperl-mode)
        (setq buffer-read-only t))
      (display-buffer (current-buffer)))))

(defvar perl-utils:perldoc-source
  '((name . "Perldoc")
    (candidates . perl-utils:installed-modules)
    (action . (("View Document" . perl-utils:view-document)
               ("View Source" . perl-utils:view-source-code)))))

;;;###autoload
(defun perl-utils:perldoc ()
  (interactive)
  (helm :sources '(perl-utils:perldoc-source)
        :buffer "*perl utils*"))

;;;###autoload
(defun perl-utils:setup ()
  (interactive)
  (perl-utils:setup-installed-module))

;;;###autoload
(defun perl-utils ()
  (interactive)
  (helm :sources '(perl-utils:build-source perl-utils:test-source)
        :buffer "*perl utils*"))

(add-hook 'compilation-filter-hook
          (lambda ()
            (ansi-color-apply-on-region (point-min) (point-max))))

(provide 'perl-utils)

;;; perl-utils.el ends here
