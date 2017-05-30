;(setq server-socket-dir (format "/tmp/emacs%d" (user-uid)))
(server-start)
(add-to-list 'auto-mode-alist '("/mutt" . mail-mode))

(turn-on-eldoc-mode)
(prefer-coding-system 'utf-8)
(savehist-mode 1)
(setq visible-bell t)
(setq column-number-mode t)
(setq-default indicate-empty-lines t)
(setq-default show-trailing-whitespace t)
(setq ns-function-modifier 'hyper)  ; make Fn key do Hyper
(setq mac-option-modifier 'super) ; make opt key do Super;
(fset 'yes-or-no-p 'y-or-n-p)

;; helm
;; helm-flyspell
(define-key flyspell-mode-map (kbd "C-;") 'helm-flyspell-correct)
;; helm-flx
(helm-flx-mode +1)
(setq helm-flx-for-helm-find-files t ;; t by default
      helm-flx-for-helm-locate t) ;; nil by default
;; swiper
(global-set-key (kbd "C-s") 'swiper)

;; remove prelude-whitespace
(setq prelude-whitespace nil)
(add-hook 'prog-mode-hook 'whitespace-mode)

;; language/dictionary
(setenv "DICTIONARY" "en_GB-ise-w_accents")
(setq browse-url-browser-function 'eww-browse-url)

(global-set-key (kbd "M-p") (kbd "C-u 1 C-v"))
(global-set-key (kbd "M-n") (kbd "C-u 1 M-v"))

(setq projectile-enable-caching t)
