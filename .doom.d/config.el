;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;;;

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Piotr Bosak"
      user-mail-address "piotrekb8@gmail.com")
(use-package! all-the-icons)
(setq doom-themes-treemacs-theme "doom-colors")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)
(setq doom-font (font-spec :family "monospace" :size 18 ))
(custom-set-faces '(org-headline-done ((t (:inherit variable-pitch :strike-through t)))))
;;(set-face-attribute 'default nil :font "Fira Code Retina" :height 180)
;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/org/")

(use-package! org-fancy-priorities
  :hook (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '("◼","◼","◼")))



(setq week-mapping '(("Mon" . "monday")
                     ("Tue" . "tuesday")
                     ("Wed" . "wednesday")
                     ("Thu" . "thursday")
                     ("Fri" . "friday")
                     ("Sat" . "saturday")
                     ("Sun" . "sunday")
                     ))


(setq day-of-the-week (cdr (assoc (car (split-string (current-time-string))) week-mapping)))

(global-set-key (kbd "C-; e")
                (lambda () (interactive)
                  (find-file-other-window (concat "/home/piotr/Dropbox/org/routines/" day-of-the-week ".org"))
                  ))

(global-set-key (kbd "C-; s")
                (lambda () (interactive)
                  (find-file-other-window "/home/piotr/Dropbox/org/stuffToProcess.org")
                  ))


;;org babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (scala . t)
   (haskell .t)
   (python . t)))
(setq org-confirm-babel-evaluate nil)

(use-package! org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
(use-package! vterm
  :ensure t)
;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)
(use-package! lsp-metals)

(setq projectile-project-search-path '("~/programming/"))


;;setting breadcrumb

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
(require 'package)

;; Add melpa to your packages repositories
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

;; Install use-package if not already installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; Enable defer and ensure by default for use-package
;; Keep auto-save/backup files separate from source code:  https://github.com/scalameta/metals/issues/1027
(setq use-package-always-defer t
      use-package-always-ensure t
      backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Enable scala-mode for highlighting, indentation and motion commands
;; Enable scala-mode for highlighting, indentation and motion commands
(use-package scala-mode
  :interpreter
  ("scala" . scala-mode)
  :hook (scala-mode . (lambda () (setq prettify-symbols-alist scala-prettify-symbols-alist)
  (prettify-symbols-mode) (yas-reload-all)))
  )
;; Enable sbt mode for executing sbt commands
(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
   ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
   (setq sbt:program-options '("-Dsbt.supershell=false"))
)
;; command log mode

;; Enable nice rendering of diagnostics like compile errors.
(use-package flycheck
  :init (global-flycheck-mode))


(use-package lsp-mode
  ;; Optional - enable lsp-mode automatically in scala files
  :hook  (scala-mode . lsp)
         (lsp-mode . lsp-lens-mode)
         (sh-mode . lsp)
         (lsp-mode . efs/lsp-mode-setup)
         :config
         (setq lsp-prefer-flymake nil)
         (setq lsp-idle-delay 0.1)
         )



(require 'lsp)
(require 'lsp-haskell)
(add-hook 'haskell-mode-hook #'lsp)
(add-hook 'haskell-literate-mode-hook #'lsp)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook #'menu-bar--display-line-numbers-mode-relative)


;; Add metals backend for lsp-mode
(use-package lsp-metals
  :config (setq lsp-metals-treeview-show-when-views-received nil))

;; Enable nice rendering of documentation on hover
(use-package! lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(setq lsp-lens-mode t)


(use-package! lsp-treemacs
  :after lsp)
(require 'treemacs-all-the-icons)
(treemacs-load-theme "all-the-icons")


;; lsp-mode supports snippets, but in order for them to work you need to use yasnippet
;; If you don't want to use snippets set lsp-enable-snippet to nil in your lsp-mode settings
;;   to avoid odd behavior with snippets and indentation
(use-package yasnippet)
(use-package company
  :hook (scala-mode . company-mode)
  :config
  (setq lsp-completion-provider :capf))
;; Add company-lsp backend for metals

;; Use the Debug Adapter Protocol for running tests and debugging
(use-package posframe
  ;; Posframe is a pop-up tool that must be manually installed for dap-mode
  )
(use-package dap-mode
  :hook
  (lsp-mode . dap-mode)
  (lsp-mode . dap-ui-mode)
  )

(use-package dashboard
  :init      ;; tweak dashboard config before loading it
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-banner-logo-title "Welcome to Emacs")
  (setq dashboard-center-content nil) ;; set to 't' for centered content
  (setq dashboard-items '((recents . 5)
                          (agenda . 5 )
                          (bookmarks . 5)
                          (projects . 5)
                          (registers . 5)))
  :config
  (dashboard-setup-startup-hook)
  (dashboard-modify-heading-icons '((recents . "file-text")
			      (bookmarks . "book"))))

(after! org
  (setq org-todo-keywords
        `((sequence  "REPEAT(r)" "TODO(t!/!)" "NEXT(n)" "PROJ(p)"  "WAITING(w@/!)" "SOMEDAY(s)" "|" "DONE(d)" "CANCELLED(c)" "POSTPONED(p@/!)"))))
(after! org
  (setq org-tag-alist '((:startgroup . nil)
                        ("UNIVERSITY" . ?u)
                        ("WORKSTATION" . ?w)
                        ("PROJECT" . ?p)
                        ("CAPGEMINI" . ?c)
                        ("MACIUŚ" .?m)
                        )))
(after! org
  (setq org-archive-location "%s_archive::"))
(setq org-columns-default-format
      "%25ITEM %TODO %3PRIORITY %SCHEDULED")

(defun org-focus-university() "Set focus to university related stuff(including bachelor's work)"
       (interactive)
       (setq org-agenda-files '("~/Dropbox/org/inzynierka.org" "~/Dropbox/org/university.org")))

(defun org-focus-all() "Set focus to all"
       (interactive)
       (setq org-agenda-files '("~/Dropbox/org/" "~/Dropbox/org/stuff/")))
(defun org-focus-programming() "Set focus to programming stuff only"
       (interactive)
       (setq org-agenda-files '("~/Dropbox/org/programmingStuff.org")))

(defun org-focus-touk() "Set focus to capgemini stuff only"
       (interactive)
       (setq org-agenda-files '("~/Dropbox/org/touk/", "~/Dropbox/org/touk/taski/")))
(defun org-focus-private() "Set focus to private stuff"
       (interactive)
       (setq org-agenda-files '("~/Dropbox/org/trening.org" "~/Dropbox/org/linuxStuff.org" "~/Dropbox/org/emacs.org" "~/Dropbox/org/macStuff.org" "~/Dropbox/org/private.org" "~/Dropbox/org/house.org" "~/Dropbox/org/books.org" "~/Dropbox/org/food.org" "~/Dropbox/org/rozrywka.org")))


(use-package org-super-agenda
  :after org-agenda
  :init
  (setq org-super-agenda-groups '((:name "Today"
                                        :time-grid t
                                        :scheduled today)
                                  (:name "Due today"
                                        :deadline today)
                                  (:name "Important"
                                        :priority "A")
                                  (:name "Overdule"
                                        :deadline past)
                                  (:name "Due soon"
                                        :deadline future)
                                  (:name "Big Outcomes"
                                        :tag "bo")))
  :config
  (org-super-agenda-mode)
                    )

;; typescript
(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))
;; enable typescript-tslint checker
;;
;;
(use-package tide
  :ensure t
  :after (rsjx-mode typescript-mode company flycheck)
  :hook (
         (rjsx-mode . setup-tide-mode)
         (typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (typescript-mode . web-mode)
         (before-save . tide-format-before-save)))

(flycheck-add-mode 'typescript-tslint 'web-mode)

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)
(setq lsp-clients-angular-language-server-command
  '("node"
    "/usr/lib/node_modules/@angular/language-server"
    "--ngProbeLocations"
    "/usr/lib/node_modules"
    "--tsProbeLocations"
    "/usr/lib/node_modules"
    "--stdio"))

(defun dw/set-js-indentation ()
  (setq js-indent-level 2)
  (setq evil-shift-width js-indent-level)
  (setq-default tab-width 2))

(use-package js2-mode
  :mode "\\.jsx?\\'"
  :config
  ;; Use js2-mode for Node scripts
  (add-to-list 'magic-mode-alist '("#!/usr/bin/env node" . js2-mode))


  ;; Don't use built-in syntax checking
  (setq js2-mode-show-strict-warnings nil)

  ;; Set up proper indentation in JavaScript and JSON files
  (add-hook 'js2-mode-hook #'dw/set-js-indentation)
  (add-hook 'json-mode-hook #'dw/set-js-indentation))


(use-package apheleia
  :config
  (apheleia-global-mode +1))

(use-package rjsx-mode
  :ensure t
  :mode "\\.jsx?\\'")

(use-package prettier-js
  :after (rjsx-mode)
   :hook ((rjsx-mode . prettier-js-mode)
          (typescript-mode . prettier-js-mode))
  :config
  (setq prettier-js-show-errors nil))

;;web
(use-package web-mode
  :mode "(\\.\\(html?\\|ejs\\|tsx\\|jsx\\)\\'"
  :config
  (setq-default web-mode-code-indent-offset 2)
  (setq-default web-mode-markup-indent-offset 2)
  (setq-default web-mode-attribute-indent-offset 2))

;; 1. Start the server with `httpd-start'
;; 2. Use `impatient-mode' on any buffer
(use-package impatient-mode)

(use-package skewer-mode)
(use-package python-mode
  :hook (python-mode . lsp-deferred))


;; my keybinding
(global-set-key (kbd "C-; d") 'lsp-ui-doc-glance)
(setq lsp-ui-doc-position 'top)
(map! :leader
      :desc "helm search" "r g" #'+helm/project-search)
;;(global-set-key (kbd "SPC m") 'mu4e)
;;
;;



;; ;;MAIL SETUP
;; (setq mu4e-context-policy 'pick-first)
;; (setq mu4e-compose-context-policy nil)
;; (setq message-send-mail-function 'smtpmail-send-it)
;; (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
;; (add-hook 'mu4e-headers-mode-hook (lambda () (display-line-numbers-mode 1)))
;; (require 'mu4e)
;; (require 'smtpmail)
;; (setq user-mail-address "piotrekb8@gmail.com"
;;       user-full-name  "Piotr Bosak"
;;       mu4e-maildir "~/Mail"
;;       mu4e-change-filenames-when-moving t
;;       mu4e-contexts
;;         (list
;;          ;; Work account
;;          (make-mu4e-context
;;           :name "Private"
;;           :match-func
;;             (lambda (msg)
;;               (when msg
;;                 (string-prefix-p "/Private" (mu4e-message-field msg :maildir))))
;;           :vars '((user-mail-address . "piotrekb8@gmail.com")
;;                   (user-full-name    . "piotrekb8")
;;                   (smtpmail-smtp-server  . "smtp.gmail.com")
;;                   (smtpmail-smtp-service . 465)
;;                   (smtpmail-stream-type  . ssl)
;;                   (mu4e-drafts-folder  . "/Private/[Gmail]/Drafts")
;;                   (mu4e-sent-folder  . "/Private/[Gmail]/Sent Mail")
;;                   (mu4e-refile-folder  . "/Private/[Gmail]/All Mail")
;;                   (mu4e-trash-folder  . "/Private/[Gmail]/Trash")))
;;  (make-mu4e-context
;;           :name "Uni"
;;           :match-func
;;             (lambda (msg)
;;               (when msg
;;                 (string-prefix-p "/Uni" (mu4e-message-field msg :maildir))))
;;           :vars '((user-mail-address . "s18692@pjwstk.edu.pl")
;;                   (user-full-name    . "s18692")
;;                   (smtpmail-smtp-server  . "smtp.gmail.com")
;;                   (smtpmail-smtp-service . 465)
;;                   (smtpmail-stream-type  . ssl)
;;                   (mu4e-drafts-folder  . "/Uni/[Gmail]/Drafts")
;;                   (mu4e-sent-folder  . "/Uni/[Gmail]/Sent Mail")
;;                   (mu4e-refile-folder  . "/Uni/[Gmail]/All Mail")
;;                   (mu4e-trash-folder  . "/Uni/[Gmail]/Trash")))
;; (make-mu4e-context
;;           :name "Yahoo"
;;           :match-func
;;             (lambda (msg)
;;               (when msg
;;                 (string-prefix-p "/Yahoo" (mu4e-message-field msg :maildir))))
;;           :vars '((user-mail-address . "piotrbosak@yahoo.com")
;;                   (user-full-name    . "piotrbosak")
;;                   (smtpmail-smtp-server  . "smtp.mail.yahoo.com")
;;                   (smtpmail-smtp-service . 465)
;;                   (smtpmail-stream-type  . ssl)
;;                   (mu4e-drafts-folder  . "/Yahoo/Drafts")
;;                   (mu4e-sent-folder  . "/Yahoo/Sent Mail")
;;                   (mu4e-refile-folder  . "/Yahoo/All Mail")
;;                   (mu4e-trash-folder  . "/Yahoo/Trash")))
;;          )
;;       ;; I have my mbsyncrc in a different folder on my system, to keep it separate from the
;;       ;; mbsyncrc available publicly in my dotfiles. You MUST edit the following line.
;;       ;; Be sure that the following command is: "mbsync -c ~/.config/mu4e/mbsyncrc -a"
;;       mu4e-get-mail-command "mbsync -c ~/.config/mu4e/mbsyncrc -a"
;;       mu4e-update-interval  300
;;       mu4e-main-buffer-hide-personal-addresses t
;;       ;; message-send-mail-function 'smtpmail-send-it
;;       ;; smtpmail-starttls-credentials '(("smtp.1and1.com" 587 nil nil))
;;       mu4e-sent-folder "/Gmail/[Gmail]/Sent Mail"
;;       mu4e-drafts-folder "/Gmail/[Gmail]/Drafts"
;;       mu4e-trash-folder "/Gmail/[Gmail]/Trash"
;;       mu4e-refile-folder "/Gmail/[Gmail]/All Mail"
;;       mu4e-maildir-shortcuts
;;       '(("/Private/Inbox"      . ?i)
;;         ("/Private/[Gmail]/Sent Mail" . ?s)
;;         ("/Private/[Gmail]/Drafts"   . ?d)
;;         ("/Private/[Gmail]/All Mail"   .?a)
;;         ("/Private/[Gmail]/Trash"  . ?t)))
;; (mu4e t)


;;evil collection
;;figure out how to add evil substitute
;;instead of having t as getting before character, have it as evil snipe, and s as substitute

;;dired
(use-package dired
  :ensure nil
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-up-directory
    "l" 'dired-find-file))
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
(require 'dired-x)
(use-package dired-single)
;; (use-package dired-open
;;   :config
;;   (setq dired-open-extensions '(("pdf" . "okular")
;;                                 )))

(use-package pdf-tools
   :pin manual
   :config
   (pdf-tools-install)
   (setq-default pdf-view-display-size 'fit-width)
   (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
   :custom
   (pdf-annot-activate-created-annotations t "automatically annotate highlights"))

(setq TeX-view-program-selection '((output-pdf "PDF Tools"))
      TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
      TeX-source-correlate-start-server t)

(add-hook 'TeX-after-compilation-finished-functions
          #'TeX-revert-document-buffer)


(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-more-map
    "H" 'dired-hide-dotfiles-mode))


(add-hook 'java-mode-hook #'lsp)


(use-package vscode-dark-plus-theme
  :config
  (load-theme 'vscode-dark-plus t))


(setq eshell-aliases-file "~/.doom.d/aliases")
(setq bash-completion-initial-timeout 2)
(setq bash-completion-command-timeout 2)

;;eshell
;;
;;
;;
;;functions

;;end eshell
;;
;;
;;
;;
;;
;;
;;
;;
;;
;;
;;
;;
;;
;;
;;
;;
;;
;;dired
(use-package jetbrains-darcula-theme
  :config
  (load-theme 'jetbrains-darcula t))


(require 'general)

(general-define-key
 :states 'global
 :keymaps 'evil-normal-state-map
 "s" 'evil-substitute)

(defun get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun make-shell (name)
  "Create a shell buffer named NAME."
  (interactive "sName: ")
  (eshell)
  (rename-buffer name))


(defun check-exists-shell ()
  (interactive)
  (let* (
        (dirname (get-string-from-file "/home/piotr/scripts/filePath.txt"))
        (buffer (car (seq-filter (lambda (buff) (string-match-p dirname (buffer-name buff))) (buffer-list))))
                    )
    (if buffer
        (switch-to-buffer buffer)
      (let ((default-directory dirname)) (make-shell (concat "eshell-" dirname))))))

                  ;; (vipin/eshell (get-string-from-file "/home/piotr/scripts/filePath.txt"))))
 (global-set-key (kbd "C-@")
                (lambda () (interactive)                   ;; (let ((default-directory (get-string-from-file "/home/piotr/scripts/filePath.txt"))) (make-shell (concat "eshell-" (get-string-from-file "/home/piotr/scripts/filePath.txt"))))))
                  (check-exists-shell)))

(defun eshell-stop-scroll ()
  (interactive)
  (setenv "eshell-scroll-to-bottom-on-output" nil))

(defun eshell-start-scroll ()
  (interactive)
  (setenv "eshell-scroll-to-bottom-on-output" 'all))

(global-set-key (kbd "C-!")
                (lambda () (interactive)
                  (eww-open-file (concat "~/Desktop/testImports/"
(car(car(cdr(cdr(cl-sort (directory-files-and-attributes "~/Desktop/testImports")
         #'time-less-p
         :key #'(lambda (x) (nth 6 x)))))))
                  ))))

(defun mu-open-in-external-app ()
  "Open the file where point is or the marked files in Dired in external
app. The app is chosen from your OS's preference."
  (interactive)
  (let* ((file-list
          (dired-get-marked-files)))
    (mapc
     (lambda (file-path)
       (let ((process-connection-type nil))
         (start-process "" nil "xdg-open" file-path))) file-list)))
(define-key dired-mode-map (kbd "C-<return>") #'mu-open-in-external-app)

(setq vterm-shell "/Users/pbk/.nix-profile/bin/fish")
