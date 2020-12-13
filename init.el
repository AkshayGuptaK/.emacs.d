;; Get rid of extraneous UI
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)

;; Set title of window to current file or buffer name if not a file.
(setq frame-title-format
      '(""(:eval (if (buffer-file-name)
                     (abbreviate-file-name (buffer-file-name))
                   "%b"))))

;; display columns position in modeline
(column-number-mode t)

(defmacro axy/alist-replace (list-var element)
  `(let
       ((replaced-list-var
         (assq-delete-all
          (car ',element) ,list-var)))
     (setq ,list-var
           (add-to-list 'replaced-list-var ',element))))

(defmacro axy/alist-replace-set (list-var element)
  `(setq ,list-var (axy/alist-replace ,list-var ,element)))

;; enable transparent osx titlebar (a la Chrome)
(axy/alist-replace-set default-frame-alist (ns-transparent-titlebar . t))

;; nowrap
(set-default 'truncate-lines t)

;; Set font
(set-face-attribute 'default nil
                    :family "InconsolateG for Powerline"
                    :height 140
                    :weight 'normal
                    :width 'normal)

;; Save all tempfiles in $TMPDIR/emacs$UID/
(defconst emacs-tmp-dir (expand-file-name (format "emacs%d" (user-uid))
                                          temporary-file-directory))
(setq backup-directory-alist
      `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix
      emacs-tmp-dir)

;; Don't let osx swallow Meta key.
(setq mac-pass-command-to-system nil)
(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)

;; bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Specifying :straight t is unnecessary if you set straight-use-package-by-default to a non-nil value.
(setq straight-use-package-by-default t)

(straight-use-package 'use-package)
;; end bootstrap straight.el

(use-package which-key
  :diminish which-key-mode
  :init
  (setq which-key-idle-delay 0.2)
  (which-key-setup-minibuffer)
  (which-key-mode))

(use-package general
  :config
  (general-override-mode)
  (general-define-key
   ;; :states '(insert)
   :keymaps 'override
   "C-`" 'hydra-mainmenu/body))

(use-package hydra
  :config
  (defhydra hydra-mainmenu (:exit t :hint t)
    ("e" eval-last-sexp "evaluate expr")
    ("x" execute-extended-command "exec ext comm")
    ))

(use-package rainbow-delimiters
  :straight (:host github
		   :repo "Fanael/rainbow-delimiters")
  :config (setq show-paren-delay 0)
  (show-paren-mode 1)
  :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package smartparens
  :no-require t
  :diminish smartparens-mode
  :init
  (dolist (hook '(lisp-mode-hook
                  scheme-mode-hook
                  clojure-mode-hook
                  emacs-lisp-mode-hook)))
    ;(add-hook hook #'smartparens-strict-mode)
  :config
  ;; Disable highlights.
  (setq sp-highlight-pair-overlay nil
        sp-highlight-wrap-overlay nil
        sp-highlight-wrap-tag-overlay nil)
  (setq sp-cancel-autoskip-on-backward-movement nil
        sp-autoskip-closing-pair 'always-end
        sp-autoskip-opening-pair t)

  ;(use-package smartparens-config)
  (smartparens-global-mode 1)
  (sp-pair "(" ")" :wrap "M-(")
  (sp-pair "(" ")" :wrap "M-)")
  (sp-pair "[" "]" :wrap "M-[")
  (sp-pair "[" "]" :wrap "M-]")
  (sp-pair "{" "}" :wrap "M-{")
  (sp-pair "{" "}" :wrap "M-}")
  (sp-pair "\"" "\"" :wrap "M-\""))

(use-package projectile
  :commands (projectile-project-p
             projectile-project-root
             projectile-find-file
             projectile-switch-project
             projectile-switch-to-buffer
             projectile-recentf)
  :diminish projectile-mode
  :config
  (setq projectile-enable-caching t)
  (projectile-mode))

(use-package counsel-projectile
  :after projectile
  :init
  (counsel-projectile-mode)
  (defun counsel-projectile-switch-project-action-fzf (project)
    "Call `counsel-fzf' (ie fuzzy find-file)from PROJECT's root."
    (let ((default-directory project)
          (projectile-switch-project-action
           (lambda ()
             (counsel-fzf))))

      (counsel-projectile-switch-project-by-name project))))

(use-package ivy
  :bind (:map ivy-minibuffer-map
              ("M-x" . ivy-dispatching-done))
  :config
  (setq ivy-use-virtual-buffers nil)
  (setq ivy-flx-limit 100)
  (setq ivy-re-builders-alist
        '((counsel-git-log . ivy--regex-plus)
          (swiper . ivy--regex-plus)
          (swiper-multi . ivy--regex-plus)
          (projectile-completing-read . ivy--regex-fuzzy)
          (counsel-fzf . regexp-quote)
          (counsel-rg . ivy--regex-plus)
          (t . ivy--regex-fuzzy)))
  (setq ivy-initial-inputs-alist nil)

  ;; swapping behavior
  (define-key ivy-minibuffer-map (kbd "RET") 'ivy-alt-done)
  (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-done)

  (define-key ivy-minibuffer-map (kbd "<C-return>") 'ivy-immediate-done)

  (setq ivy-count-format "")
  (setq ivy-height 15)

  (setq ivy-do-completion-in-region t) ; this is the default

  (ivy-mode))

(use-package counsel
  :bind (("M-x" . counsel-M-x))
  :commands (counsel-find-file
             counsel-rg
             counsel-git
             counsel-fzf
             counsel-fzf-occur
             counsel-describe-face)
  :init
  (setq projectile-switch-project-action 'counsel-fzf)
  :config
  (ivy-set-prompt 'counsel-fzf (lambda () "> "))
  (setenv "FZF_DEFAULT_COMMAND"
          "(git ls-files --exclude-standard --others --cached ||
        ind . -maxdepth 9 -path \"*/\\.*\" -prune -o -print -o -type l -print |
           sed s/^..//) 2> /dev/null")
  (setq counsel-async-filter-update-time 100000)
  (setq counsel-git-cmd "git ls-files --exclude-standard --full-name --others --cached --")
  (setq counsel-rg-base-command "rg -i --no-heading --line-number --color never %s .")
  (setq counsel-ag-base-command "ag -U --nocolor --nogroup %s -- ."))

(use-package swiper
  :commands (swiper)
  :diminish ivy-mode)

(use-package flycheck
  :if (not (eq system-type 'windows-nt))
  :defer 8
  :diminish flycheck-mode
  :commands (flycheck-mode)
  :config
  (setq flycheck-idle-change-delay 1.2)
  (setq-default flycheck-emacs-lisp-load-path 'inherit)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  ; https://stackoverflow.com/questions/37720869/emacs-how-do-i-set-flycheck-to-python-3
  (custom-set-variables
   '(flycheck-python-flake8-executable "python3")
   '(flycheck-python-pycompile-executable "python3")
   '(flycheck-python-pylint-executable "python3"))
  (global-flycheck-mode))

(use-package company
  :diminish company-mode
  :config
  (company-tng-configure-default)
  (setq company-idle-delay .01)
  (setq company-minimum-prefix-length 1)
  (global-company-mode))

(use-package magit
  :commands (magit-toplevel
             magit-status
             magit-blame
             magit-log
             magit-find-file
             magit-find-file-other-window)
  :config
  (defun +magit-git-submodule-update--init--recursive ()
    "Run $ git submodule update --init --recursive."
    (interactive)
    (magit-run-git-async "submodule" "update" "--init" "--recursive"))

  (setq magit-bury-buffer-function 'magit-mode-quit-window)

  (setq magit-diff-refine-hunk 'all)
  ;(setq magit-diff-arguments '("--no-ext-diff" "--stat" "-U5"))

  ;; Save buffers automatically instead of asking.
  (setq magit-save-repository-buffers 'dontask)

  (setq magit-repository-directories '("~/dev" "~/.emacs.d"))
  (setq magit-refresh-status-buffer nil)

  ;; Add rebase argument to pull
  ;; https://github.com/magit/magit/issues/2597
  (defun +magit-submodule-remove (path &optional leave-in-work-tree)
    "Remove the submodule at PATH.
     https://stackoverflow.com/questions/1260748/how-do-i-remove-a-submodule"
    (interactive
     (list (magit-completing-read "Remove module" (magit-get-submodules)
                                  nil t nil nil (magit-section-when module))))
    (magit-with-toplevel
     ;; 0. mv a/submodule a/submodule_tmp
     (shell-command (format "mv %s %s_tmp" path path))

     ;; 1. git submodule deinit -f -- a/submodule
     (magit-run-git "submodule" "deinit" "-f" "--" path)

     ;; (magit-run-git-async "submodule" "deinit" path)

     ;; 2. rm -rf .git/modules/a/submodule
     (shell-command (format "rm -rf .git/modules/%s" path))

     (if (not leave-in-work-tree)
         ;; 3. git rm -f a/submodule
         (magit-run-git "rm" "-f" path)
       ;; # If you want to leave it in your working tree and have done step 0.
       ;; 3b. git rm --cached a/submodule
       ;; 3b. mv a/submodule_tmp a/submodule
       (magit-run-git "rm" "--cached" path)
       (shell-command-to-string (format "mv %s_tmp %s" path path)))))


  (define-key magit-hunk-section-map (kbd "<return>") 'magit-diff-visit-file-other-window)

  ;; Bind esc
  (define-key transient-map        (kbd "<escape>") 'transient-quit-one)
  (define-key transient-edit-map   (kbd "<escape>") 'transient-quit-one)
  (define-key transient-sticky-map (kbd "<escape>") 'transient-quit-seq))


;; makes handling lisp expressions much, much easier
;; Cheatsheet: http://www.emacswiki.org/emacs/PareditCheatsheet
(use-package paredit)

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package habamax-theme
  :no-require t)

(load "~/.emacs.d/typescript.el")
(load "~/.emacs.d/clojure.el")
(load "~/.emacs.d/java.el")

