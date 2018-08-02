;; https://github.com/noctuid/evil-guide
; C-x ESC ESC = repeat-complex-command
; C-h k = describe key
; C-h b = describe key bindings

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; use-package initialization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(setq package-archives '(("melpa" . "http://melpa.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keys
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-eval-after-load 'evil-maps
  (evil-define-key 'normal 'global
    (kbd "<f1>") (lambda () (interactive) (find-file user-init-file))
    (kbd "C-o") 'counsel-fzf
    (kbd "S-C-f") 'counsel-rg
    "/" 'swiper
    (kbd "M-<left>") 'previous-buffer
    (kbd "M-<right>") 'next-buffer)

  (evil-define-key 'normal 'evil-normal-state-map
    "j" 'evil-next-visual-line
    "k" 'evil-previous-visual-line)

  (evil-define-key nil 'global
    (kbd "C->") 'evil-jump-forward
    (kbd "C-<") 'evil-jump-backward))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; common settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(setq package-enable-at-startup nil
      exec-path-from-shell-arguments '("-l") ; https://github.com/syl20bnr/spacemacs/issues/3920
      inhibit-startup-screen t
      initial-scratch-message ""
      ring-bell-function 'ignore
      auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t))
      vc-follow-symlinks t
      x-select-enable-clipboard-manager nil
      coding-system-for-read 'utf-8
      coding-system-for-write 'utf-8
      use-package-always-ensure t)

(global-visual-line-mode 1)
(show-paren-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(global-hl-line-mode 1)
(blink-cursor-mode 0)
(defalias 'yes-or-no-p 'y-or-n-p)

; backup settings
(setq version-control t
      kept-new-versions 10
      kept-old-versions 0
      delete-old-versions t
      backup-by-copying t
      vc-make-backup-files t
      backup-directory-alist '(("" . "~/.emacs.d/backup/per-save")))

(defun force-backup-of-buffer ()
  "Make a special 'per session' backup at the first save of each Emacs session."

  (when (not buffer-backed-up)
    ;; Override the default parameters for per-session backups.
    (let ((backup-directory-alist '(("" . "~/.emacs.d/backup/per-session")))
          (kept-new-versions 3))
      (backup-buffer)))
  ;; Make a "per save" backup on each save.  The first save results in
  ;; both a per-session and a per-save backup, to keep the numbering
  ;; of per-save backups consistent.
  (let ((buffer-backed-up nil))
    (backup-buffer)))

(add-hook 'before-save-hook  'force-backup-of-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; use packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package atom-one-dark-theme :config (load-theme 'atom-one-dark t))

(when (memq window-system '(mac ns x))
  (use-package exec-path-from-shell
    :config
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-env "GOPATH")))

(use-package evil
  :config
  (evil-mode 1)
  (setq evil-search-wrap t
	evil-regexp-search t)
  (when (display-graphic-p)
    (setq evil-emacs-state-cursor '("red" box)
	  evil-normal-state-cursor '("green" box)
	  evil-visual-state-cursor '("orange" box)
	  evil-insert-state-cursor '("red" bar)
	  evil-replace-state-cursor '("red" bar)
	  evil-operator-state-cursor '("red" hollow)))

  (use-package evil-surround :config (global-evil-surround-mode))
  (use-package evil-mc :config (global-evil-mc-mode 1))
  (use-package evil-indent-textobject))


(use-package ivy
  :diminish ivy-mode
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
	ivy-count-format "(%d/%d) "))

(use-package magit)
(use-package counsel)
(use-package htmlize)
(use-package ox-gfm)
(use-package flycheck :init (global-flycheck-mode))

(use-package which-key
  :diminish which-key-mode
  :config
  (add-hook 'after-init-hook 'which-key-mode))

(use-package company
  :bind (:map company-active-map
	 ("C-n" . company-select-next-or-abort)
	 ("C-p" . company-select-previous-or-abort))
  :config
  (global-company-mode))


(use-package lsp-mode
  :config

  (use-package lsp-ui
    :hook (lsp-mode . lsp-ui-mode)
    :config (setq lsp-ui-sideline-ignore-duplicate t))

  (use-package company-lsp :config (push 'company-lsp company-backends)))

(use-package rust-mode
  :config

  (setq rust-format-on-save t)

  (use-package lsp-rust
    :hook (rust-mode . lsp-rust-enable)
    :config (setq lsp-rust-rls-command '("rustup" "run" "nightly" "rls")))

  (use-package cargo
    :bind (:map rust-mode-map
	   ("<f5>" . cargo-process-run)
	   ("<f6>" . cargo-process-test))
    :hook (rust-mode . cargo-minor-mode)))

(use-package go-mode
  :hook (before-save . gofmt-before-save)
  :bind (:map go-mode-map
	 ("C-]" . godef-jump)
	 ("M-[" . previous-error)
	 ("M-]" . next-error)
	 ("<f5>" . go-run)
	 ("<f6>" . go-test-current-file))
  :config
  (setq gofmt-command "goimports"
	compile-command "go build -v && go test -v && go vet")

  (use-package go-guru)
  (use-package go-rename)
  (use-package go-errcheck)
  (use-package go-eldoc :hook (go-mode . go-eldoc-setup))
  (use-package gotest)

  (use-package company-go
    :hook (go-mode . (lambda ()
		       (set (make-local-variable 'company-backends) '(company-go))
		       (company-mode)))))


(provide 'init)
;;; init.el ends here
