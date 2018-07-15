;;; init-000-core-base.el --- Customize emacs basic functionalities

;; Copyright (C) 2018 Po Tong

;; Author: Po Tong
;; Maintaainer: Po Tong
;; Created: 2018-06-24

;; Keywords: emacs, use-package, configuration

;;; Commentary:

;;; Code:
(require 'use-package)

;; Emacs starts out with a black buffer
(setq inhibit-splash-screen t
      initial-scratch-message nil)

;; Turn dinging off
(setq visible-bell nil)
(setq ring-bell-function 'ignore)

;; Turn off menu bar, tool bar, and scroll bar
(if window-system (scroll-bar-mode -1))
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Backup files setup
(setq
 backup-by-copying t      ; don't clobber symlinks
 backup-directory-alist
 `((".*" . ,temporary-file-directory)) ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)       ; use versioned backups

;; For :diminish
(use-package diminish
  :ensure t)

;; For :delight
(use-package delight
  :ensure t)

;; Load custom theme - Spacemacs-dark
(use-package spacemacs-common
  :ensure spacemacs-theme
  :config (load-theme 'spacemacs-dark t))

;; Customize the modeline
(use-package spaceline-config
  :ensure spaceline
  :config (spaceline-spacemacs-theme))

;; Ivy mode
(use-package ivy
  :ensure t
  :diminish ivy-mode
  :config
  (ivy-mode 1))

;; Projectile setup
(use-package projectile
  :ensure t
  :delight '(:eval (concat " " (projectile-project-name)))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (projectile-mode 1)
  (setq projectile-completion-system 'ivy))

;; ivy UI for projectile
(use-package counsel-projectile
  :ensure t
  :config
  (counsel-projectile-mode))

;; silver search
(use-package ag
  :ensure t
  :config
  (setq ag-highlight-search t)
  (setq ag-reuse-buffers t)
  (add-to-list 'ag-arguments "--word-regexp"))

;; Tree directory using neotree
(use-package neotree
  :ensure t
  :bind (("C-c o" . neotree-toggle)
         :map neotree-mode-map
         ("e" . neotree-enter-hide))
  :init
  (setq neo-show-hidden-files t)
  (setq neo-create-file-auto-open t)
  (setq neo-keymap-style 'concise)
  (setq neo-smart-open t)
  (setq neo-vc-integration '(face))
  (defun neo-open-file-hide (full-path &optional arg)
    "Open a file node and hides tree."
    (neo-global--select-mru-window arg)
    (find-file full-path)
    (neotree-hide))
  (defun neotree-enter-hide (&optional arg)
    "Enters file and hides neotree directly"
    (interactive "P")
    (neo-buffer--execute arg 'neo-open-file-hide 'neo-open-dir)))


(use-package company
  :ensure t
  :init
  (global-company-mode)
  :bind (("<backtab>" . company-complete-common-or-cycle)))

;; Flycheck
(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :init
  (global-flycheck-mode t))

;; Magit
(use-package magit
  :ensure t
  :diminish auto-revert-mode
  :bind ("C-x g" . magit-status))

;; (use-package smartparens-config
;;   :ensure smartparens
;;   :diminish smartparens-mode
;;     :config
;;     (progn
;;       (show-smartparens-global-mode t)))

;; (add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
;; (add-hook 'markdown-mode-hook 'turn-on-smartparens-strict-mode)

;; cmake setup starts here
(use-package cmake-mode
  :ensure t
  :mode "\\CMakeLists.txt\\'")

;; JavaScript setup starts here
(use-package js2-mode
  :ensure t
  :diminish (js2-refactor-mode yas-minor-mode)
  :mode "\\.js\\'"
  :init
  (add-hook 'js2-mode-hook (lambda()
							 (setq js-switch-indent-offset 4))))

(use-package js2-refactor-mode
  :after js2-mode
  :ensure js2-refactor
  :hook js2-mode
  ;; :bind (("C-k" . js2r-kill))
  :init (js2r-add-keybindings-with-prefix "C-c C-r"))

(use-package company-tern
  :after js2-mode
  :diminish (tern-mode company-mode)
  :ensure t
  :init
  (add-hook 'js2-mode-hook (lambda ()
			     (tern-mode)
			     (company-mode)))
  :config
  (add-to-list 'company-backends 'company-tern))

;; JSON setup starts here
(use-package json-mode
  :ensure t
  :mode "\\.json\\'")

;; Maerkdown setup starts here
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc")
  )

(use-package flymd
  :ensure t
  :after markdown-mode
  :config
  (defun my-flymd-browser-function (url)
    (let ((browse-url-browser-function 'browse-url-firefox))
      (browse-url url)))
  (setq flymd-browser-open-function 'my-flymd-browser-function))

;; golang setup starts here
(use-package go-mode
  :ensure t
  :mode "\\.go\\'")

;; php setup starts here
(use-package php-mode
  :ensure t
  :diminish (abbrev-mode)
  :mode "\\.php\\'"
  :init
  (add-hook 'php-mode-hook (lambda()
			     (setq tab-width 4
				   indent-tabs-mode t))))

(use-package company-php
  :ensure t
  :after php-mode
  ;; :diminish company-mode
  :init
  (add-hook 'php-mode-hook 'company-mode)
  :config
  (add-to-list 'company-backends 'company-ac-php-backend))

;; c++ setup starts here
(use-package irony
  :ensure t
  :config
  (use-package company-irony
    :ensure t
    ;; :diminish (company-mode irony-mode)
    :config
    (add-to-list 'company-backends 'company-irony)
    ;; (add-hook 'c++-mode-hook 'company-irony-mode)
    ;; (add-hook 'c-mode-hook 'company-irony-mode)
    ;; (add-hook 'objc-mode-hook 'company-irony-mode)
    )

  (use-package company-irony-c-headers
    :ensure t
    :config
    (add-to-list 'company-backends 'company-irony-c-headers))

  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  (setq-default irony-cdb-compilation-databases '(irony-cdb-libclang
                                                    irony-cdb-clang-complete))
  ;; replace the `completion-at-point' and `complete-symbol' bindings in
  ;; irony-mode's buffers by irony-mode's function
  (defun my-irony-mode-hook ()
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async))
  (add-hook 'irony-mode-hook 'my-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

  ;; (use-package flycheck-irony
  ;;   :ensure t
  ;;   :commands flycheck-irony-setup
  ;;   :init
  ;;   (defun my-c++-flycheck-hook ()
  ;;     (setq flycheck-clang-include-path (list "/home/potong/heros_v_monsters/include")))
  ;;   (add-hook 'c++-mode-hook 'flycheck-irony-setup)
  ;;   (add-hook 'c-mode-hook 'flycheck-irony-setup)
  ;;   ;;(add-hook 'c++-mode-hook 'my-c++-flycheck-hook)
  ;;   )
  (use-package cmake-ide
    :ensure t
    :init
    (use-package semantic/bovine/gcc)
    (setq cmake-ide-flags-c++ (append '("-std=c++11")
  				      (mapcar (lambda (path) (concat "-I" path)) (semantic-gcc-get-include-paths "c++"))))
    (setq cmake-ide-flags-c (append (mapcar (lambda (path) (concat "-I" path)) (semantic-gcc-get-include-paths "c"))))
    (cmake-ide-setup))
  )



;;; init-000-core-base.el ends here
