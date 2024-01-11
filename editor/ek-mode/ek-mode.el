;; ek-mode.el    -*- lexical-binding: t; -*-

(require 'lsp)

;; (defcustom lsp-grph-executable "ek-lsp"
;;   "Path of the EK LSP executable"
;;   :type 'file)

(defvar ek-keywords
  (let* ((ek-keywords '("atom" "struct" "type" "fn" "extern" "lazy" "precedence" "import"))
         (ek-types '("bool" "int" "true" "false" "string" "void" "any" "never"))
         (ek-keywords-regexp (regexp-opt ek-keywords 'words))
         (ek-types-regexp (regexp-opt ek-types 'words)))
    `(("//.*" . font-lock-comment-face)
      (,ek-keywords-regexp . font-lock-keyword-face)
      (,ek-types-regexp . font-lock-type-face)
      ("[0-9]+" . font-lock-constant-face)
      ("[.=/+*!?%<>&|^~$,_\\-]" . font-lock-variable-name-face)
      ("[][(){}]" . font-lock-bracket-face)
      ("[,:]" . font-lock-delimiter-face)
    )))


(define-derived-mode ek-mode prog-mode "EK"
  (setq-local comment-start "//")
  (set (make-local-variable 'font-lock-defaults) '(ek-keywords)))

(add-to-list 'auto-mode-alist (cons "\\.ek\\'" 'ek-mode))

;; (with-eval-after-load 'lsp-mode
;;   (add-to-list 'lsp-language-id-configuration '(ek-mode . "ek"))
;;   (lsp-register-client
;;    (make-lsp-client
;;     :new-connection (lsp-stdio-connection (lambda () lsp-grph-executable))
;;     :activation-fn (lsp-activate-on "ek")
;;     :server-id 'eklsp)))

(provide 'ek-mode)
