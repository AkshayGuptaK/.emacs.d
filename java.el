;; Use LSP for java mode. See github page:
;; https://github.com/emacs-lsp/lsp-java

(use-package lsp-mode
  :hook java-mode)

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-sideline-enable t
	lsp-ui-sideline-show-symbol t
	lsp-ui-sideline-show-hover t
	lsp-ui-sideline-showcode-actions t
	lsp-ui-sideline-update-mode 'point))

(use-package company-lsp
  :after company
  :init
  (add-to-list 'company-backends #'company-lsp)

  :config
  (setq company-lsp-enable-snippet t
	company-lsp-cache-candidates t))

(use-package lsp-java
  :defer 3
  :init  
  (progn
   ;; (require 'lsp-ui-flycheck)
    (require 'lsp-ui-sideline)
    (add-hook 'java-mode-hook #'lsp-java-enable)
    (add-hook 'java-mode-hook #'flycheck-mode)
    (add-hook 'java-mode-hook #'company-mode)
    (add-hook 'java-mode-hook (lambda () (lsp-ui-flycheck-enable t)))
    (add-hook 'java-mode-hook #'lsp-ui-sideline-mode))
  
  :config
  ;; have to add each project individually :(
  (setq lsp-java--workspace-folders
	(list
	 (expand-file-name "~/learning/cs61b"))))
