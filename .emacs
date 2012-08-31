;; ==================== Basic configuration ====================

;; ===== Setup load path =====
(let ((basepath "/Users/markus/Documents/work/github/M-x-my-emacs/emacs.d/plugins/"))

  (add-to-list 'load-path basepath)
  (add-to-list 'load-path (concat basepath "git-emacs/"))
  (add-to-list 'load-path (concat basepath "color-theme-6.6.0/")))

;; Auto elisp compilation cache upon startup
;; (require 'byte-code-cache)
;; (load-library "byte-code-cache")

;; Remove useless toolbar
(tool-bar-mode -1)

;; ===== Color theme setup =====
(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-charcoal-black)))

;; ===== Basic setup =====
;; keeps the buffer in sync with the one that is on disk
(global-auto-revert-mode 1)

;; indentation
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)

;; Better buffer switching
(iswitchb-mode 1)

;; default to better frame titles
(setq frame-title-format (concat  "%b - emacs@" system-name))

;; 80 columns
(require 'whitespace)
;; display only tails of lines longer than 80 columns, tabs and
;; trailing whitespaces
;; (setq whitespace-line-column 80
;;       whitespace-style '(tabs trailing lines-tail))

(setq whitespace-style '(face empty tabs lines-tail trailing))

;; auto enable whitespace mode when entering C/C++ mode
(add-hook 'c-mode-hook (lambda () (whitespace-mode)))
(add-hook 'c++-mode-hook (lambda () (whitespace-mode)))

;; auto enable linum mode when entering C mode
(add-hook 'c-mode-hook (lambda () (linum-mode)))
(add-hook 'c++-mode-hook (lambda () (linum-mode)))

;; ==================== Git setup ====================
(setq git-state-modeline-decoration 'git-state-decoration-large-dot)
(require 'git-emacs)

;; ==================== Backup setup ====================
;; Disable backup files.
(setq make-backup-files nil)

;; Enable versioning with default values (keep five last versions, I think!)
(setq version-control t)

;; ==================== Modeline setup ====================
;; set mode line to show full path of current file
;; Helper function
(defun shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat ".../" output)))
    output))

(setq-default mode-line-format
'("-"
  mode-line-mule-info
  mode-line-modified
  mode-line-frame-identification

 ; directory and buffer/file name
   (:propertize (:eval (shorten-directory default-directory 30))
                face mode-line-folder-face)

   (:propertize "%b"
                face mode-line-filename-face)
   "   "
   mode-line-position
   (vc-mode vc-mode)
   "   "
   mode-line-modes
   (which-func-mode ("" which-func-format "--"))
   (global-mode-string ("--" global-mode-string))
   "-%-"))

;; Show column-number in the mode line
(column-number-mode 1)

;; ===== Misc stuff setup =====
;; Enable compressed files I/O
(auto-compression-mode t)

;; Set a shortcut to enable/disable hl-line-mode
(global-set-key "\C-ch" 'hl-line-mode)
(global-set-key "\C-cl" 'linum-mode)

(load-library "match")
(global-set-key "\C-c)" 'goto-match-paren)

;; igrep
(autoload 'igrep "igrep"
  "*Run `grep` PROGRAM to match REGEX in FILES..." t)
(autoload 'igrep-find "igrep"
  "*Run `grep` via `find`..." t)
(autoload 'igrep-visited-files "igrep"
  "*Run `grep` ... on all visited files." t)
(autoload 'dired-do-igrep "igrep"
  "*Run `grep` on the marked (or next prefix ARG) files." t)
(autoload 'dired-do-igrep-find "igrep"
  "*Run `grep` via `find` on the marked (or next prefix ARG) directories." t)
(autoload 'Buffer-menu-igrep "igrep"
  "*Run `grep` on the files visited in buffers marked with '>'." t)
(autoload 'igrep-insinuate "igrep"
  "Define `grep' aliases for the corresponding `igrep' commands." t)

;; For completeness, you can add these forms as well:
(autoload 'grep "igrep"
  "*Run `grep` PROGRAM to match REGEX in FILES..." t)
(autoload 'egrep "igrep"
  "*Run `egrep`..." t)
(autoload 'fgrep "igrep"
  "*Run `fgrep`..." t)
(autoload 'agrep "igrep"
  "*Run `agrep`..." t)
(autoload 'grep-find "igrep"
  "*Run `grep` via `find`..." t)
(autoload 'egrep-find "igrep"
  "*Run `egrep` via `find`..." t)
(autoload 'fgrep-find "igrep"
  "*Run `fgrep` via `find`..." t)
(autoload 'agrep-find "igrep"
  "*Run `agrep` via `find`..." t)

(global-set-key [(meta r)] 'occur)
(global-set-key [(control x)(v)(e)] 'ediff-revision)

;; automatic format conversion and whitespace cleanup before saving
(defun unix-newline ()
  (set-buffer-file-coding-system 'undecided-unix))

(add-hook 'before-save-hook 'unix-newline)
(add-hook 'before-save-hook 'whitespace-cleanup)

;; ===== Breadcrumb (fast bookmarks management) =====
(require 'breadcrumb)
(global-set-key "\M- "                  'bc-set)            ;; Meta-SPACE for set bookmark
(global-set-key [(meta j)]              'bc-previous)       ;; M-j for jump to previous
(global-set-key [(shift meta j)]        'bc-next)           ;; Shift-M-j for jump to next
(global-set-key [(meta up)]             'bc-local-previous) ;; M-up-arrow for local previous
(global-set-key [(meta down)]           'bc-local-next)     ;; M-down-arrow for local next
(global-set-key [(control c)(j)]        'bc-goto-current)   ;; C-c j for jump to current bookmark
(global-set-key [(control x)(meta j)]   'bc-list)           ;; C-x M-j for the bookmark menu list

;; ===== Windows mode =====
(require 'windows)
(win:startup-with-window)
(define-key ctl-x-map "C" 'see-you-again)
(autoload 'save-current-configuration "revive" "Save status" t)
(autoload 'resume "revive" "Resume Emacs" t)
(autoload 'wipe "revive" "Wipe Emacs" t)

;; And define favorite keys to those functions.  Here is a sample.
(define-key ctl-x-map "S" 'save-current-configuration)
(define-key ctl-x-map "F" 'resume)
(define-key ctl-x-map "K" 'wipe)

;; YASnippet
;; (require 'yasnippet-bundle)

(windmove-default-keybindings 'ctrl)
(require 'comint)
(define-key comint-mode-map [(meta p)]
  'comint-previous-matching-input-from-input)
(define-key comint-mode-map [(meta n)]
  'comint-next-matching-input-from-input)
(define-key comint-mode-map [(control meta n)]
  'comint-next-input)
(define-key comint-mode-map [(control meta p)]
  'comint-previous-input)

;; ===== Shell and sudo'ed find-file =====
(defvar find-file-root-prefix (if (featurep 'xemacs) "/[sudo/root@localhost]" "/sudo:root@localhost:" )
  "*The filename prefix used to open a file with `find-file-root'.")

(defvar find-file-root-history nil
  "History list for files found using `find-file-root'.")

(defvar find-file-root-hook nil
  "Normal hook for functions to run after finding a \"root\" file.")

(defun find-file-root ()
  "*Open a file as the root user.
   Prepends `find-file-root-prefix' to the selected file name so that it
   maybe accessed via the corresponding tramp method."

  (interactive)
  (require 'tramp)
  (let* ( ;; We bind the variable `file-name-history' locally so we can
         ;; use a separate history list for "root" files.
         (file-name-history find-file-root-history)
         (name (or buffer-file-name default-directory))
         (tramp (and (tramp-tramp-file-p name)
                     (tramp-dissect-file-name name)))
         path dir file)

    ;; If called from a "root" file, we need to fix up the path.
    (when tramp
      (setq path (tramp-file-name-localname tramp)
            dir (file-name-directory path)))

    (when (setq file (read-file-name "Find file (UID = 0): " dir path))
      (find-file (concat find-file-root-prefix file))
      ;; If this all succeeded save our new history list.
      (setq find-file-root-history file-name-history)
      ;; allow some user customization
      (run-hooks 'find-file-root-hook))))

(global-set-key [(control x) (control r)]
                'find-file-root)

(global-set-key [(control x) (control t)]
                'shell)

(global-set-key [(meta g)]
                'goto-line)

(global-set-key [(kp-delete)]
                'delete-char)

;; Function keys redefs
;; (global-set-key [f1] 'man)
;; (global-set-key [S-f1] 'start-kbd-macro)
;; (global-set-key [M-f1] 'end-kbd-macro)

(global-set-key [f1] 'man)
;; (global-set-key [S-f1] 'describe-key)

;; (global-set-key [f2] 'comment-region)
;; (global-set-key [S-f2] 'uncomment-region)

;; (global-set-key [f3] 'shell)

;; (global-set-key [f4] 'kill-this-buffer)
;; (global-set-key [S-f4] 'delete-other-windows)

(global-set-key [f5] 'compile)
;; (global-set-key [S-f5] 'start-kbd-macro)
;; (global-set-key [M-f5] 'end-kbd-macro)

(global-set-key [f6] (lambda ()
                       "Switch to scratch buffer"
                       (interactive "*")
                       (switch-to-buffer "*scratch*")))

;; (global-set-key [f8] 'eval-last-sexp)
;; (global-set-key [S-f8] 'load-file)

;; ===== JavaScript (js2 mode) =====
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))

;;; cperl-mode customizations
(add-hook 'cperl-mode-hook
      '(lambda () "Set c-c c-c to comment-region"
         (local-set-key "\C-c\C-c" 'comment-region)
         (setq comment-column 65)))


(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
        "Prevent annoying \"Active processes exist\" query when you quit Emacs."
        (flet ((process-list ())) ad-do-it))

;; ==================== Scala development ====================
;; (require 'scala-mode-auto)
;; (add-hook 'scala-mode-hook
;;           '(lambda ()
;;              (yas/minor-mode-on)
;;              ))

;; ==================== Slime (Lisp interaction) ====================
;; (add-to-list 'load-path "~/.emacs.d/plugins/slime/")  ; your SLIME directory
;; (setq inferior-lisp-program "/opt/local/bin/sbcl") ; your Lisp system
;; (require 'slime)
;; (slime-setup)

;; ;; ==================== Applications ====================

;; ;; ===== Emacs IRC client (erc) =====
;; (require 'erc)
;; (require 'erc-track)
;; (require 'erc-fill)
;; (require 'erc-log)
;; ;; See http://www.emacswiki.org/cgi-bin/wiki/ErcStartupFiles
;; (defmacro ahref-erc-connect (command server port nick)
;;   "Create interactive command `command', for connecting to an IRC
;; server.  The command uses interactive mode if passed an
;; argument."
;;   (fset command
;;         `(lambda (arg)
;;            (interactive "p")
;;            (if (not (= 1 arg))
;;                (call-interactively 'erc-select)
;;              (erc-select :server ,server :port ,port :nick ,nick)))))
;; ; (de-erc-connect erc-ag2 "irc.freenode.net" 6667 "mwolf76")
;; ; (defun erc-cadence ()
;; ;   "Fire up ERC"
;; ;   (interactive)
;; ;   (select-frame (make-frame '((name . "Emacs ERC") (minibuffer . t))))
;; ;   (call-interactively 'erc-ag2))
;; (add-hook 'erc-mode-hook
;;           '(lambda ()
;;              (pcomplete-erc-setup)
;;              (erc-completion-mode 1)))
;; (erc-track-mode t)
;; (erc-fill-mode t)
;; (setq erc-log-insert-log-on-open nil)
;; (setq erc-log-channels t)
;; (setq erc-log-channels-directory "~/.emacs.d/erc/")
;; (setq erc-save-buffer-on-part t)
;; (setq erc-hide-timestamps nil)

;; (defadvice save-buffers-kill-emacs (before save-logs (arg) activate)
;;   (save-some-buffers t (lambda () (when
;;                                       (and (eq major-mode 'erc-mode)
;;                                            (not (null buffer-file-name)))))))

;; (add-hook 'erc-insert-post-hook 'erc-save-buffer-in-logs)

;; (add-hook 'erc-mode-hook
;;           '(lambda () (when (not (featurep 'xemacs))
;;                         (set (make-variable-buffer-local
;;                               'coding-system-for-write) 'emacs-mule))))

;; (load-library "postack")
;; (global-set-key "\C-cm" 'postack-push)
;; (global-set-key "\C-cp" 'postack-pop)

;; autocompletion, take 1:
;; (global-set-key [(meta /)] 'dabbrev-expand)
;; (global-set-key [(control /)] 'hippie-expand)

;; autocompletion, take 2:
;; (when (require 'auto-complete nil t)
;;   (require 'auto-complete-yasnippet)
;;   (require 'auto-complete-semantic)
;;   (require 'auto-complete-css)
;;   (set-face-foreground 'ac-menu-face "wheat")
;;   (set-face-background 'ac-menu-face "darkslategrey")
;;   (set-face-underline 'ac-menu-face "wheat")
;;   (set-face-foreground 'ac-selection-face "white")
;;   (set-face-background 'ac-selection-face "darkolivegreen")
;;   (setq ac-auto-start 2) ; start auto-completion after 4 chars only
;;   (global-set-key "\C-c1" 'auto-complete-mode) ; easy key to toggle AC on/off
;;   (define-key ac-complete-mode-map "\t" 'ac-complete)
;;   (define-key ac-complete-mode-map "\r" nil)
;;   (global-auto-complete-mode t))1

;; which function am I editing?
;; (when (>= emacs-major-version 23)
;;   (add-hook 'python-mode-hook
;;             (lambda ()
;;               (which-function-mode t))))

;; spell check only strings/comments when in Python mode:
;; (add-hook 'python-mode-hook 'flyspell-prog-mode)

;; Get rid of annoying question
(setq kill-buffer-query-functions
      (remove 'process-kill-buffer-query-function
              kill-buffer-query-functions))

;; Lazy yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; ==================== Macros ====================
;; A smart idea from http://blog.deadpansincerity.com/
(defmacro dotfile (filename)
  "Define the function `filename' to edit the dotfile in question"
  (let ((filestr (symbol-name filename)))
    `(progn
       (defun ,(intern filestr) ()
         ,(format "Open %s for editing" filestr)
         (interactive)
         (find-file ,(concat "~/" filestr))))))

(dotfile .emacs)
(dotfile .bashrc)
(dotfile .bash_aliases)
(dotfile .bash_environment)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(fringe-mode 0 nil (fringe))
 '(gitk-program "/opt/local/bin/gitk"))

;; workaround for linum-mode on Mac OS X
(setq linum-format "%d ")
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)
