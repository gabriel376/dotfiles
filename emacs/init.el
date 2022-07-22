;;; Garbage Collector
(setopt gc-cons-threshold (* 32 1024 1024))

;;; Startup
(defun display-startup-echo-area-message ()
  (message ""))

(setopt initial-buffer-choice nil
        initial-scratch-message nil
        inhibit-startup-screen t)

;;; Packages
(setopt package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                           ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                           ("melpa" . "https://melpa.org/packages/"))
        package-selected-packages '()
        package-user-dir (expand-file-name "packages" user-emacs-directory))

(package-install-selected-packages t)
(package-activate-all)

;;; Commands
(defun ignore-command (&optional cmd keys)
  (message "Command %s is disabled "
           (or cmd this-command)))

(setopt disabled-command-function #'ignore-command)

;;; Overwrite
(put 'overwrite-mode 'disabled t)

;;; Narrow
(put 'narrow-to-region 'disabled nil)

;;; Menu Bar
(menu-bar-mode -1)

;;; Tool Bar
(tool-bar-mode -1)

;;; Scroll
(setopt scroll-conservatively 1
        scroll-preserve-screen-position 1)

(scroll-bar-mode -1)

;;; Tooltip
(tooltip-mode -1)

;;; Cursor
(setopt cursor-in-non-selected-windows nil
        visible-cursor nil
        x-stretch-cursor t
        cursor-type 'box)

(blink-cursor-mode -1)

;;; Backup
(setopt make-backup-files nil)

;;; Echo Area
(setopt echo-keystrokes 0.1)

;;; Show Paren
(setopt show-paren-delay 0
        show-paren-context-when-offscreen 'overlay)

(show-paren-mode 1)

;;; Indent
(setopt tab-width 4
        indent-tabs-mode nil)

;;; Columns
(setopt fill-column 80)

;;; Uniquify
(setopt uniquify-buffer-name-style 'forward
        uniquify-trailing-separator-p t)

;;; Font
(custom-theme-set-faces
 'user
 '(default ((t (:font "monospace-11")))))

(setopt x-underline-at-descent-line t)

;;; Theme
(custom-theme-set-faces
 'user
 '(mode-line                  ((t :box (:style flat-button :line-width (0 . 3)) :inherit default)))
 '(mode-line-inactive         ((t :box (:style flat-button :line-width (0 . 3)))))
 '(tab-bar-tab                ((t :box (:style flat-button :line-width (6 . 3)))))
 '(tab-bar-tab-inactive       ((t :box (:style flat-button :line-width (6 . 3)))))
 '(tab-bar-tab-group-current  ((t :box (:style flat-button :line-width (6 . 3)))))
 '(tab-bar-tab-group-inactive ((t :box (:style flat-button :line-width (6 . 3))))))

(setopt modus-themes-headings '((t . (rainbow)))
        modus-themes-mode-line '(borderless)
        modus-themes-org-blocks 'gray-background
        modus-themes-paren-match '(intense))

(load-theme 'modus-vivendi)

;;; Repeat
(setopt repeat-exit-timeout 2
        repeat-keep-prefix nil
        repeat-echo-function #'repeat-echo-mode-line
        repeat-exit-key [escape])

(repeat-mode 1)

;;; Environment Variables
(seq-map (lambda (line)
           (when-let ((index (seq-position line ?=)))
             (setenv (substring line 0 index)
                     (substring line (1+ index)))))
         (thread-last (getenv "SHELL")
                      (format "%s -i -l -c 'env' 2>/dev/null")
                      shell-command-to-string
                      string-lines))

(setopt exec-path
        (append (split-string (getenv "PATH") ":")
                (list exec-directory)))

;;; Buffer
(keymap-set ctl-x-map "k" #'kill-current-buffer)

;;; Window
(setopt window-combination-resize t)

;;; Recenter
(setopt recenter-positions '(top middle bottom))

;;; Window Divider
(setopt window-divider-default-right-width 1)

(window-divider-mode 1)

;;; Tab Bar
(defun tab-bar-tab-name-format-default (tab i)
  (let ((current-p (eq (car tab) 'current-tab)))
    (propertize
     (format " %s " (string-trim (format "%d %s" i (alist-get 'name tab))))
     'face (funcall tab-bar-tab-face-function tab))))

(defun tab-bar-tab-group-format-default (tab i)
  (propertize
   (format " %s " (string-trim (format "%d%s" i (funcall tab-bar-tab-group-function tab))))
   'face 'tab-bar-tab-group-inactive))

(defun tab-bar-tab-group-default (tab)
  (format " %s " (alist-get 'group tab)))

(setopt tab-bar-close-button-show nil
        tab-bar-tab-hints t
        tab-bar-tab-name-function (cl-constantly "")
        tab-bar-tab-post-change-group-functions #'tab-bar-move-tab-to-group
        tab-bar-format '(tab-bar-format-tabs-groups
                         tab-bar-separator
                         tab-bar-format-align-right
                         tab-bar-format-global))

(tab-bar-mode 1)
(tab-bar-history-mode 1)
(tab-bar-change-tab-group "main")

;;; Ruby
(setopt ruby-align-chained-calls t)

;;; Locale
(set-locale-environment "en_US.UTF-8")

;;; Display Time
(setopt display-time-interval 1
        display-time-string-forms '((format-time-string "%a, %d/%b/%Y %H:%M:%S %Z " now)))

(display-time-mode 1)

;;; Kill
(setopt save-interprogram-paste-before-kill t
        kill-do-not-save-duplicates t
        kill-ring-max 128)

;;; Yank
(setopt mouse-yank-at-point t
        yank-menu-max-items 128)

;;; Mark
(setopt shift-select-mode nil)

;;; Search
(setopt case-fold-search nil
        search-whitespace-regexp ".*?"
        isearch-allow-scroll 'unlimited
        isearch-allow-motion t
        isearch-lazy-count t
        lazy-highlight-initial-delay 0
        lazy-count-prefix-format "[%s/%s] ")

;;; Compile
(setopt compilation-scroll-output t)

(add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)

;;; Ediff
(setopt ediff-window-setup-function 'ediff-setup-windows-plain)

;;; Delete Selection
(delete-selection-mode 1)

;;; Minibuffer
(setopt enable-recursive-minibuffers t
        minibuffer-default-prompt-format " [%s]"
        minibuffer-beginning-of-buffer-movement t)

(minibuffer-depth-indicate-mode 1)

;;; Completions
(setopt completion-show-help nil
        completion-show-inline-help nil
        completion-category-defaults nil
        completions-format 'one-column
        completion-styles '(partial-completion))

;;; Icomplete
(setopt icomplete-show-matches-on-no-input t
        icomplete-scroll t
        icomplete-compute-delay 0
        icomplete-matches-format "[%s/%s] ")

(with-eval-after-load 'icomplete
  (keymap-set icomplete-vertical-mode-minibuffer-map "TAB" #'icomplete-force-complete))

(icomplete-vertical-mode 1)

;;; Comments
(setopt comment-empty-lines t)

;;; Fringe
(setopt fringe-mode '(0 . 0))

;;; Help
(setopt help-window-select t
        help-enable-symbol-autoload t)

;;; Zap
(keymap-set global-map "M-z" #'zap-up-to-char)

;;; Fill Column Indicator
(dolist (hook '(message-mode-hook
                org-mode-hook
                prog-mode-hook))
  (add-hook hook #'display-fill-column-indicator-mode))

;;; Line Numbers
(setopt display-line-numbers-width-start t
        display-line-numbers-widen t)

(dolist (hook '(prog-mode-hook
                org-mode-hook))
  (add-hook hook #'display-line-numbers-mode))

;;; Pulse
(defun pulse-current-line ()
  "Pulse current line."
  (interactive)
  (pulse-momentary-highlight-one-line (point) 'region))

;;; Messages
(setopt messages-buffer-max-lines 32768)

;;; Mode Line
(defun mode-line--left ()
  "The left part of `mode-line-format'."
  (concat " "
          (propertize (buffer-name)
                      'face
                      (cond
                       ((and buffer-file-name (buffer-modified-p)) 'error)
                       (buffer-read-only                           'warning)
                       (t                                          'mode-line-buffer-id)))))

(defun mode-line--center ()
  "The center part of `mode-line-format'."
  (concat (substring-no-properties (format-mode-line mode-name))
          (when mode-line-process (format-mode-line mode-line-process))))

(defun mode-line--right ()
  "The right part of `mode-line-format'."
  (string-trim-left
   (concat (seq-reduce
            (lambda (acc elt)
              (or (and (boundp (car elt))
                       (symbol-value (car elt))
                       (concat acc (format-mode-line (cadr elt))))
                  acc))
            minor-mode-alist
            "")
           (when repeat-in-progress " Repeat")
           (when (buffer-narrowed-p) " Narrow")
           " ")))

(setopt mode-line-format '(:eval (let ((left   (mode-line--left))
                                       (center (mode-line--center))
                                       (right  (mode-line--right)))
                                   (concat left
                                           (propertize " "
                                                       'display `((space :align-to (- (+ center (.5 . right-margin))
                                                                                      ,(/ (string-width center) 2.0)
                                                                                      (.5 . left-margin)))))
                                           center
                                           (propertize " "
                                                       'display `((space :align-to (- (+ right right-fringe right-margin)
                                                                                      ,(string-width right)))))
                                           right))))

;; Dabbrev
(defun dabbrev-completing-read ()
  "Completion on current word with `completing-read'."
  (interactive)
  (dabbrev--reset-global-variables)
  (let* ((abbrev (dabbrev--abbrev-at-point))
         (expansions (dabbrev--find-all-expansions abbrev nil)))
    (when (> (length expansions) 1)
      (when-let ((expansion (completing-read "Expand with: " expansions nil t abbrev)))
        (insert (substring expansion (length abbrev)))))))

(keymap-set global-map "C-M-/" #'dabbrev-completing-read)

;;; Hide Show
(setopt hs-allow-nesting t
        hs-isearch-open t)

(dolist (hook '(prog-mode-hook))
  (add-hook hook #'hs-minor-mode))

;;; Save Hist
(setopt savehist-autosave-interval nil
        savehist-additional-variables '(kill-ring
                                        mark-ring
                                        global-mark-ring
                                        search-ring
                                        regexp-search-ring
                                        compile-command
                                        extended-command-history))

(savehist-mode 1)

;;; Project
(defun project-find-regexp-completing-read ()
  (interactive)
  (let ((xref-show-xrefs-function #'xref-show-definitions-completing-read))
    (call-interactively #'project-find-regexp)))

(setopt project-compilation-buffer-name-function #'project-prefixed-buffer-name)

(keymap-set project-prefix-map "G" #'project-find-regexp-completing-read)

;;; Version Control
(defun vc-diff-dwim ()
  (interactive)
  (if (buffer-modified-p)
      (diff-buffer-with-file (current-buffer))
    (call-interactively #'vc-diff)))

(keymap-set vc-prefix-map "=" #'vc-diff-dwim)

;;; Transpose
(defun transpose-up-dwim (arg)
  "Transpose line or region up"
  (interactive "p")
  (transpose--dwim -1 arg))

(defun transpose-down-dwim (arg)
  "Transpose line or region down"
  (interactive "p")
  (transpose--dwim +1 arg))

(defun transpose--dwim (direction arg)
  "Transpose line or region"
  (if (use-region-p)
      (let* ((rbeg (region-beginning))
             (rend (region-end))
             (lbeg (save-excursion (goto-char (min rbeg rend)) (line-beginning-position)))
             (lend (save-excursion (goto-char (max rbeg rend)) (line-end-position)))
             (text (delete-and-extract-region lbeg (1+ lend))))
        (forward-line direction)
        (insert text)
        (backward-char (1+ (- lend rbeg)))
        (set-mark (+ (point) (- rend rbeg)))
        (setq deactivate-mark nil)
        (exchange-point-and-mark))
    (let ((column (current-column)))
      (if (> 0 direction)
          (progn
            (transpose-lines 1)
            (previous-line 2))
        (progn
          (next-line 1)
          (transpose-lines 1)
          (previous-line 1)))
      (move-to-column column))))

(pcase-dolist (`(,key ,cmd) '(("M-<up>" transpose-up-dwim)
                              ("M-<down>" transpose-down-dwim)))
  (keymap-set global-map key cmd))

;;; Pair
(defun edit-pair-dwim (arg)
  "Edit pairs on current sexp or region."
  (interactive "p")
  (let ((open (read-char)))
    (if (equal open (char-after))
        (delete-pair arg)
      (insert-pair arg open (read-char)))))

(setopt delete-pair-blink-delay 0)

;;; Duplicate
(defun duplicate-text-dwim (arg)
  "Duplicate current line or region."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))

;; Case
(defun edit-case-dwim ()
  "Edit current word or region case."
  (interactive)
  (let* ((str (if (use-region-p)
                  (buffer-substring (region-beginning) (region-end))
                (word-at-point)))
         (fn (cond ((string= str (upcase str))   'downcase-dwim)
                   ((string= str (downcase str)) 'capitalize-dwim)
                   (t                            'upcase-dwim))))
    (save-excursion
      (when (not (use-region-p))
        (backward-to-word 1))
      (funcall fn 1)
      (setq deactivate-mark nil))))

;;; Edit char at point
(defun increase-char-at-point (arg)
  "Increase char at point."
  (interactive "p")
  (insert (+ arg (char-after)))
  (delete-char 1)
  (backward-char))

(defun decrease-char-at-point (arg)
  "Decrease char at point."
  (interactive "p")
  (increase-char-at-point (* -1 arg)))

;;; Edit number at point
(defun increase-number-at-point (arg)
  "Increase number at point."
  (interactive "p")
  (skip-chars-backward "-0123456789")
  (when (looking-at "-?[0-9]+")
    (thread-last (match-string 0)
                 string-to-number
                 (+ arg)
                 number-to-string
                 replace-match)))

(defun decrease-number-at-point (arg)
  "Decrease number at point."
  (interactive "p")
  (increase-number-at-point (* -1 arg)))

;;; Checkpoints
(defvar checkpoints--list '())

(defvar checkpoints--index -1)

(defun checkpoints--update ()
  (setq checkpoints--list (seq-filter #'overlay-buffer checkpoints--list)))

(defun checkpoints--goto (index)
  (if-let (overlay (seq-elt checkpoints--list index))
      (progn
        (pop-to-buffer (overlay-buffer overlay))
        (goto-char (overlay-start overlay))
        (message "Checkpoint %s/%s" (1+ index) (length checkpoints--list)))
    (message "No checkpoints")))

(defun checkpoints-toggle (arg)
  (interactive "P")
  (if-let (overlay (seq-find (lambda (overlay)
                               (equal (overlay-get overlay 'category) 'checkpoint))
                             (overlays-at (point))))
      (progn
        (delete-overlay overlay)
        (setq checkpoints--list (remove overlay checkpoints--list))
        (message "Checkpoint removed"))
    (let ((overlay (make-overlay (line-beginning-position) (line-end-position))))
      (overlay-put overlay 'category 'checkpoint)
      (overlay-put overlay 'face 'highlight)
      (overlay-put overlay 'evaporate t)
      (setq checkpoints--list (if (and arg (<= arg (length checkpoints--list)))
                                  (append (seq-subseq checkpoints--list 0 (1- arg))
                                          (list overlay)
                                          (seq-subseq checkpoints--list (1- arg)))
                                (append checkpoints--list (list overlay))))
      (deactivate-mark)
      (message "Checkpoint added"))))

(defun checkpoints-clear ()
  (interactive)
  (dolist (overlay checkpoints--list)
    (delete-overlay overlay))
  (setq checkpoints--list '())
  (message "Checkpoints cleared"))

(defun checkpoints-next ()
  (interactive)
  (checkpoints--update)
  (setq checkpoints--index (1+ checkpoints--index))
  (when (>= checkpoints--index (length checkpoints--list))
    (setq checkpoints--index 0))
  (checkpoints--goto checkpoints--index))

(defun checkpoints-previous ()
  (interactive)
  (checkpoints--update)
  (setq checkpoints--index (1- checkpoints--index))
  (when (< checkpoints--index 0)
    (setq checkpoints--index (1- (length checkpoints--list))))
  (checkpoints--goto checkpoints--index))

(defun checkpoints-select ()
  (interactive)
  (checkpoints--update)
  (if-let (completions (seq-map-indexed (lambda (overlay index)
                                          (with-current-buffer (overlay-buffer overlay)
                                            (format "[%d]  %s  %s"
                                                    (1+ index)
                                                    (buffer-name)
                                                    (buffer-substring (overlay-start overlay) (overlay-end overlay)))))
                                        checkpoints--list))
      (let* ((key (completing-read (format-prompt "Checkpoint" nil)
                                   (lambda (string predicate action)
                                     (if (eq action 'metadata)
                                         `(metadata (cycle-sort-function . ,#'identity))
                                       (complete-with-action action
                                                             (seq-sort 'string< completions)
                                                             string
                                                             predicate)))
                                   nil
                                   t))
             (index (save-match-data
                      (and (string-match "\\[\\([0-9]+\\)\\]" key)
                           (1- (string-to-number (match-string 1 key))))))
             (overlay (seq-elt checkpoints--list index)))
        (setq checkpoints--index index)
        (checkpoints--goto index))
    (message "No checkpoints")))


(defvar-keymap checkpoints-map
  :doc "Keymap for checkpoints commands."
  "RET" #'checkpoints-toggle
  "SPC" #'checkpoints-select
  "<left>" #'checkpoints-previous
  "<right>" #'checkpoints-next
  "<backspace>" #'checkpoints-clear)

(put 'checkpoints-previous 'repeat-map 'checkpoints-map)
(put 'checkpoints-next 'repeat-map 'checkpoints-map)

;;; Bidirectional Display
(setopt bidi-paragraph-direction 'left-to-right
        bidi-inhibit-bpa t)

;;; Xref
(setopt xref-file-name-display 'abs)

(add-hook 'xref-after-update-hook
          (lambda ()
            (setq-local outline-regexp "/")))

;;; Outline
(setopt outline-default-state nil
        outline-minor-mode-cycle t
        outline-minor-mode-use-buttons nil
        outline-minor-mode-cycle-filter 'bolp
        outline-minor-mode-buttons '(("▶" "▼" outline--valid-char-p)))

(dolist (hook '(diff-mode-hook
                prog-mode-hook
                xref-after-update-hook))
  (add-hook hook #'outline-minor-mode))

;;; Shell
(setopt shell-command-prompt-show-cwd t)

;; Org
(setopt org-catch-invisible-edits 'error
        org-M-RET-may-split-line nil
        org-show-context-detail t
        org-outline-path-complete-in-steps nil
        org-fontify-done-headline nil
        org-adapt-indentation t
        org-agenda-files '("~/org/")
        org-agenda-time-leading-zero t
        org-goto-interface 'outline-path-completion
        org-list-allow-alphabetical t
        org-log-done 'time
        org-log-into-drawer t
        org-enforce-todo-dependencies t
        org-enforce-todo-checkbox-dependencies t
        org-hierarchical-todo-statistics nil
        org-checkbox-hierarchical-statistics nil
        org-todo-keywords '((sequence "TODO" "DOING" "WAIT" "|" "DONE" "CANCEL")))

;;; Whitespace
(setopt whitespace-style '(face
                           trailing
                           indentation
                           space-after-tab
                           space-before-tab))

(dolist (hook '(prog-mode-hook
                org-mode-hook))
  (add-hook hook #'whitespace-mode))

;;; Eshell
(require 'eshell)
(require 'em-hist)

(defun eshell-complete-history ()
  "Complete command history."
  (interactive)
  (when-let (command (completing-read "Input history: "
                                      (ring-elements eshell-history-ring)
                                      nil
                                      t))
    (insert command)))

(setopt eshell-banner-message "")

(add-to-list 'eshell-modules-list 'eshell-tramp)

(keymap-set eshell-hist-mode-map "M-r" #'eshell-complete-history)

;;; Application Launcher
(defun launch-application ()
  "Launch an external application."
  (interactive)
  (call-process-shell-command (read-shell-command "Launch application: ")
                              nil 0 nil))

;;; Dired
(setopt dired-listing-switches "-lhA"
        dired-do-revert-buffer t
        dired-auto-revert-buffer t
        dired-kill-when-opening-new-dired-buffer t
        dired-free-space nil)

(put 'dired-find-alternate-file 'disabled nil)

(add-hook 'dired-mode-hook #'dired-hide-details-mode)

;;; Register
(setopt register-preview-delay 0)

;;; Bell
(setopt visible-bell nil
        ring-bell-function nil)

;;; Dialogs
(setopt use-dialog-box nil
        use-file-dialog nil)

;;; Recentf
(setopt recentf-max-saved-items 128
        recentf-show-file-shortcuts-flag nil)

(recentf-mode 1)

;;; Undo
(pcase-dolist (`(,key ,cmd) '(("u" undo)
                              ("U" undo-redo)))
  (keymap-set ctl-x-map key cmd)
  (keymap-set undo-repeat-map key cmd)
  (put cmd 'repeat-map 'undo-repeat-map))

;;; Calendar
(setopt calendar-time-display-form '(24-hours ":" minutes)
        calendar-date-style 'iso
        calendar-week-start-day 1
        calendar-mark-diary-entries-flag t
        calendar-holidays nil
        calendar-latitude -23.58161
        calendar-longitude -46.58127
        calendar-location-name "São Paulo, Brazil")

;;; Buffers
(setopt save-some-buffers-default-predicate #'save-some-buffers-root
        view-read-only t)

;;; Subword
(global-subword-mode 1)

;;; Windmove
(setopt windmove-create-window t
        windmove-wrap-around t)

;;; Cleanup
(defun emacs-cleanup ()
  "Do cleanup by killing buffers, tabs and windows."
  (interactive)
  (tab-bar-close-other-tabs)
  (delete-other-windows)
  (mapc #'kill-buffer (buffer-list)))

;;; Ibuffer
(setopt ibuffer-display-summary nil
        ibuffer-show-empty-filter-groups nil
        ibuffer-default-sorting-mode 'alphabetic)

(add-hook 'ibuffer-mode-hook #'ibuffer-auto-mode)

(keymap-set ctl-x-map "C-b" #'ibuffer)

;;; User Information
(setopt user-full-name "Gabriel"
        user-mail-address "gabriel376@hotmail.com")

;;; Mail
(setopt mail-user-agent 'gnus-user-agent
        message-mail-user-agent t
        message-signature nil
        read-mail-command 'gnus
        send-mail-function 'smtpmail-send-it)

;;; SMTP
(setopt smtpmail-smtp-user "gabriel376@hotmail.com"
        smtpmail-smtp-server "smtp.office365.com"
        smtpmail-smtp-service 587
        smtpmail-stream-type 'starttls)

;;; GNUs
(setopt gnus-select-method '(nnnil "")
        gnus-secondary-select-methods '((nntp   "news.gmane.io")
                                        (nnimap "outlook.office365.com"))
        gnus-message-archive-method '(nnimap "outlook.office365.com")
        gnus-message-archive-group "Sent"
        gnus-use-dribble-file nil
        gnus-user-date-format-alist '((t . "%Y-%m-%d %T"))
        gnus-summary-line-format "%B[%&user-date;] [%f] %s\n"
        gnus-summary-mode-line-format "%G"
        gnus-sum-thread-tree-root "⚪"
        gnus-sum-thread-tree-false-root "⚫"
        gnus-sum-thread-tree-indent " "
        gnus-sum-thread-tree-leaf-with-other "├► "
        gnus-sum-thread-tree-single-leaf "╰► "
        gnus-sum-thread-tree-vertical "│")

;;; Keys
(setopt translate-upper-case-key-bindings nil)

(defvar-keymap user-map
  :doc "Keymap for user commands."

  "a" #'org-agenda
  "c" checkpoints-map
  "d" #'duplicate-text-dwim
  "l" #'launch-application
  "p" #'edit-pair-dwim
  "x" #'edit-case-dwim

  "<escape>" #'emacs-cleanup

  "<left>" #'windmove-left
  "<right>" #'windmove-right
  "<up>" #'windmove-up
  "<down>" #'windmove-down

  "C-z" #'pulse-current-line

  "C-+" #'hs-show-all
  "C-_" #'hs-hide-all
  "C-=" #'hs-show-block
  "C--" #'hs-hide-block

  "C-<left>" #'windmove-display-left
  "C-<right>" #'windmove-display-right
  "C-<up>" #'windmove-display-up
  "C-<down>" #'windmove-display-down

  "M-=" #'increase-number-at-point
  "M--" #'decrease-number-at-point

  "M-<left>" #'windmove-swap-states-left
  "M-<right>" #'windmove-swap-states-right
  "M-<up>" #'windmove-swap-states-up
  "M-<down>" #'windmove-swap-states-down

  "C-M-=" #'increase-char-at-point
  "C-M--" #'decrease-char-at-point

  "C-M-<left>" #'windmove-delete-left
  "C-M-<right>" #'windmove-delete-right
  "C-M-<up>" #'windmove-delete-up
  "C-M-<down>" #'windmove-delete-down)

(put 'duplicate-text-dwim 'repeat-map 'user-map)
(put 'edit-pair-dwim 'repeat-map 'user-map)
(put 'edit-case-dwim 'repeat-map 'user-map)

(put 'windmove-left 'repeat-map 'user-map)
(put 'windmove-right 'repeat-map 'user-map)
(put 'windmove-up 'repeat-map 'user-map)
(put 'windmove-down 'repeat-map 'user-map)

(put 'increase-char-at-point 'repeat-map 'user-map)

(keymap-set global-map "C-z" user-map)
