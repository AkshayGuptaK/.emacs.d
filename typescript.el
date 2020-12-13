(use-package typescript-mode
  ;; npm install -g typescript
  :mode
  ("\\.ts\\'" . typescript-mode)
  ("\\.ts$\\'" . typescript-mode)
  :init
  (add-hook 'typescript-mode-hook
            (lambda ()
              (setq-local typescript-indent-level 2)
                ))
  :config
  (setq typescript-enabled-frameworks '(typescript)))

(use-package tide
  :commands (tide-setup)
  :config
  (setq tide-jump-to-definition-reuse-window nil)
  :init
  (defun +setup-tide-mode ()
    (interactive)
    (when (locate-dominating-file default-directory "tsfmt.json")
      (add-hook 'before-save-hook #'tide-format-before-save nil t))
    ;; Disable linting for Typescript Definition files.
    (when (and (buffer-file-name)
               (string-match-p ".d.ts$" (buffer-file-name)))
      (flycheck-mode -1))
    (tide-setup)
    (tide-hl-identifier-mode +1))
  (add-hook 'typescript-mode-hook #'+setup-tide-mode)

  (add-hook 'web-mode-hook
            (lambda ()
              ;; Set up Tide mode if Typescript.
              (when (string-equal (file-name-extension buffer-file-name) "tsx")
                (setq-local web-mode-enable-auto-quoting nil)
                (when (fboundp 'yas-activate-extra-mode)
                  (yas-activate-extra-mode 'typescript-mode))
                (+setup-tide-mode))))
  :config
  ;; Set up Typescript linting with `web-mode'.
  ;; https://github.com/ananthakumaran/tide/pull/161
  (eval-after-load 'flycheck
    (lambda ()
      (flycheck-add-mode 'typescript-tslint 'web-mode))))

