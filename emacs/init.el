;;; Garbage Collector
(setopt gc-cons-threshold (* 32 1024 1024))

;;; Startup
(defun display-startup-echo-area-message ()
  (message ""))

(setopt initial-buffer-choice nil
        initial-scratch-message nil
        inhibit-startup-screen t)

;;; Packages
(setopt package-archives '(("gnu"    . "https://elpa.gnu.org/packages/")
                           ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                           ("melpa"  . "https://melpa.org/packages/"))
        package-selected-packages '()
        package-user-dir (expand-file-name "packages" user-emacs-directory))

(package-install-selected-packages t)
(package-activate-all)

;;; Keys
(defvar user-prefix-map
  (make-sparse-keymap)
  "Keymap for user commands.")

(setopt translate-upper-case-key-bindings nil)

(dolist (key '("C-z"
               "C-x C-d"))
  (keymap-global-unset key))

(keymap-set global-map "C-z" user-prefix-map)

;;; Dashboard
(defcustom dashboard-buffer "*Dashboard*"
  "The `dashboard' buffer name.")

(defun dashboard--forward-button ()
  (interactive)
  (when (save-excursion
          (re-search-backward (concat "^\\(?:" outline-regexp "\\)") nil t))
    (outline-hide-subtree))
  (forward-button 1 t)
  (outline-show-subtree))

(defun dashboard--backward-button ()
  (interactive)
  (when (save-excursion
          (re-search-forward (concat "^\\(?:" outline-regexp "\\)") nil t))
    (outline-hide-subtree))
  (backward-button 1 t)
  (outline-show-subtree))

(defvar dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "TAB" #'dashboard--forward-button)
    (keymap-set map "<backtab>" #'dashboard--backward-button)
    map)
  "Keymap for `dashboard-mode'.")

(define-derived-mode dashboard-mode
  special-mode "Dashboard"
  "Major mode of `dashboard'."
  (setq-local revert-buffer-function #'dashboard
              outline-regexp "[A-Z]"
              outline-minor-mode-cycle t)
  (outline-minor-mode 1)
  (outline-cycle-buffer))

(defun dashboard (&rest args)
  "Create a dashboard buffer."
  (interactive)
  (when (length= command-line-args 1)
    (let ((inhibit-read-only t)
          (buffer (get-buffer-create dashboard-buffer)))
      (with-current-buffer buffer
        (erase-buffer)
        (progn ; logo
          (let ((image (create-image (expand-file-name "emacs.png" user-emacs-directory)
                                     nil nil :scale 0.15)))
            (insert (propertize " "
                                'display
                                `(space :align-to (+ center (-0.5 . ,image)))))
            (insert-image image)))
        (progn ; header
          (let ((text (propertize "GNU Emacs" 'face 'default)))
            (insert "\n"
                    (propertize " "
                                'display
                                `(space :align-to (+ center (-0.5 . ,(string-width text)))))
                    text)))
        (progn ; system information
          (insert "\n\n" (propertize "System Information:" 'face 'outline-1))
          (insert "\n\t" (format "Emacs Version:	%s (%s %s)" emacs-version emacs-repository-branch emacs-repository-version))
          (insert "\n\t" (format "Emacs PID:		%s" (emacs-pid)))
          (insert "\n\t" "Init File:		")
          (insert-text-button user-init-file
                              'action
                              (lambda (button) (find-file (button-label button))))
          (insert "\n\t" (format "Init Time:		%s seconds" (emacs-init-time "%.2f")))
          (insert "\n\t" (format "Packages:		%s" (length package-activated-list)))
          (insert "\n\t" (format "Uptime:			%s" (emacs-uptime)))
          (insert "\n\t" (format "GCs Done:		%s" gcs-done))
          (insert "\n\t" (format "User Name:		%s" (user-login-name)))
          (insert "\n\t" (format "Host Name:		%s" (system-name)))
          (insert "\n\t" (format "Host Type:		%s" system-type))
          (when-let* ((memory-info (memory-info))
                      (total (/ (float (seq-elt memory-info 0)) (* 1024 1024)))
                      (free  (/ (float (seq-elt memory-info 1)) (* 1024 1024)))
                      (used  (- total free)))
            (insert "\n\t" (format "Memory:			%.1f/%.1f GiB" used total))))
        (progn ; projects
          (require 'project)
          (project--ensure-read-project-list)
          (insert "\n\n" (propertize "Projects:" 'face 'outline-1))
          (dolist (item (seq-map 'car project--list))
            (insert "\n\t")
            (insert-text-button item
                                'action
                                (lambda (button) (project-switch-project (button-label button))))))
        (progn ; recent files
          (require 'recentf)
          (insert "\n\n" (propertize "Recent Files:" 'face 'outline-1))
          (dolist (item recentf-list)
            (insert "\n\t")
            (insert-text-button item
                                'action
                                (lambda (button) (find-file (button-label button))))))
        (dashboard-mode))
      (switch-to-buffer buffer)
      (beginning-of-buffer)
      (search-forward-regexp "^[A-Z]")
      (backward-char)
      buffer)))

(add-hook 'after-init-hook #'dashboard)
(keymap-set user-prefix-map "<return>" #'dashboard)

;;; Commands
(setopt disabled-command-function (lambda (&optional cmd keys)
                                    (message "Command %s disabled "
                                             (or cmd this-command))))

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
 '(default ((t :font "Fira Code Retina-10"))))

(setopt x-underline-at-descent-line t)

;;; Font Lock
(add-hook 'prog-mode-hook
          (lambda ()
            (font-lock-mode 1)
            (setq-local font-lock-keywords nil)))

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
(setopt tab-bar-close-button-show nil
        tab-bar-tab-hints t
        tab-bar-tab-post-change-group-functions #'tab-bar-move-tab-to-group
        tab-bar-format '(tab-bar-format-tabs-groups
                         tab-bar-separator
                         tab-bar-format-align-right
                         tab-bar-format-global))

(tab-bar-mode 1)
(tab-bar-history-mode 1)

;;; Locale
(set-locale-environment "en_US.UTF-8")

;;; Display Time
(setopt display-time-string-forms '((format-time-string "%a %d/%m/%y %H:%M " now)))

(display-time-mode 1)

;;; Kill
(setopt save-interprogram-paste-before-kill t
        kill-do-not-save-duplicates t
        kill-ring-max 100)

;;; Yank
(setopt mouse-yank-at-point t
        yank-menu-max-items 100)

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

(keymap-set user-prefix-map "C-z" #'pulse-current-line)

;;; Messages
(setopt messages-buffer-max-lines 10000)

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
           (when vc-mode (substring-no-properties vc-mode))
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

(pcase-dolist (`(,key ,cmd) '(("C-+" hs-show-all)
                              ("C-_" hs-hide-all)
                              ("C-=" hs-show-block)
                              ("C--" hs-hide-block)))
  (keymap-set user-prefix-map key cmd))

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

(keymap-set user-prefix-map "p" #'edit-pair-dwim)

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

(keymap-set user-prefix-map "d" #'duplicate-text-dwim)

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

(keymap-set user-prefix-map "x" #'edit-case-dwim)

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

(pcase-dolist (`(,key ,cmd) '(("C-M-=" increase-char-at-point)
                              ("C-M--" decrease-char-at-point)))
  (keymap-set user-prefix-map key cmd))

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

(pcase-dolist (`(,key ,cmd) '(("M-=" increase-number-at-point)
                              ("M--" decrease-number-at-point)))
  (keymap-set user-prefix-map key cmd))

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
        outline-minor-mode-highlight t
        outline-minor-mode-use-buttons nil
        outline-minor-mode-buttons '(("▶" "▼" outline--valid-char-p)))

(dolist (hook '(diff-mode-hook
                xref-after-update-hook))
  (add-hook hook #'outline-minor-mode))

;;; Shell
(defun shell-run-cmd-dwim ()
  "Run current line or region as a shell command and insert output in next line."
  (interactive)
  (require 'ansi-color)
  (let* ((start (if (use-region-p) (region-beginning) (line-beginning-position)))
         (end   (if (use-region-p) (region-end) (line-end-position)))
         (cmd   (buffer-substring-no-properties start end)))
    (save-excursion
      (move-end-of-line 1)
      (newline)
      (insert (ansi-color-apply (shell-command-to-string cmd))))))

(setopt shell-command-prompt-show-cwd t)

(keymap-set user-prefix-map "!" #'shell-run-cmd-dwim)

;; Screenshot
(defun take-screenshot ()
  "Take a screenshot of the current Emacs frame and save it to a file."
  (interactive)
  (let* ((template "emacs-screenshot-%Y%m%dT%H%M%S.png")
         (filename (read-file-name "Save as: "
                                   nil
                                   nil
                                   nil
                                   (format-time-string template)))
         (extension (substring filename -3))
         (extension (if (member extension '("png" "pdf" "svg"))
                        extension
                      (completing-read "Type: " '("pdf" "svg" "png") nil t)))
         (data (x-export-frames nil (intern extension))))
    (with-temp-file filename
      (set-buffer-file-coding-system 'raw-text)
      (insert data))
    (kill-new filename)
    (message (concat "Screenshot saved to " filename))))

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

(keymap-set user-prefix-map "a" #'org-agenda)

;;; Whitespace
(setopt whitespace-style '(face
                           trailing
                           indentation
                           space-after-tab
                           space-before-tab))

(dolist (hook '(prog-mode-hook
                org-mode-hook))
  (add-hook hook #'whitespace-mode))

;;; Solar
(defvar sunrise-hook nil
  "Normal hook run at sunrise time.")

(defvar sunset-hook nil
  "Normal hook run at sunset time.")

(defvar solar--sunrise-timer nil
  "Timer for `sunrise-hook'.")

(defvar solar--sunset-timer nil
  "Timer for `sunset-hook'.")

(defun solar--run-hooks ()
  "Run `sunrise-hook' and `sunset-hook'."
  (require 'solar)
  (require 'subr-x)
  (let* ((now (format-time-string "%H:%M" (current-time)))
         (sunrise (thread-last
                    (calendar-current-date)
                    solar-sunrise-sunset
                    car
                    (apply #'solar-time-string)))
         (sunset (thread-last
                   (calendar-current-date)
                   solar-sunrise-sunset
                   cadr
                   (apply #'solar-time-string))))
    (when (timerp solar--sunrise-timer)
      (cancel-timer solar--sunrise-timer)
      (setq solar--sunrise-timer nil))
    (when (timerp solar--sunset-timer)
      (cancel-timer solar--sunset-timer)
      (setq solar--sunset-timer nil))
    (cond
     ((string< now sunrise)
      (progn
        (run-hooks 'sunset-hook)
        (setq solar--sunrise-timer
              (run-at-time sunrise nil #'solar--run-hooks))))
     ((string< now sunset)
      (progn
        (run-hooks 'sunrise-hook)
        (setq solar--sunset-timer
              (run-at-time sunset nil #'solar--run-hooks))))
     (t
      (progn
        (let* ((now (parse-time-string now))
               (sunrise (thread-last
                          (calendar-current-date 1)
                          solar-sunrise-sunset
                          car
                          (apply #'solar-time-string)
                          parse-time-string))
               (time (* 60
                        (- (+ (* 60 (+ 24 (caddr sunrise)))
                              (cadr sunrise))
                           (+ (* 60 (caddr now))
                              (cadr now))))))
          (run-hooks 'sunset-hook)
          (setq solar--sunrise-timer
                (run-at-time time nil #'solar--run-hooks))))))))

(add-hook 'after-init-hook #'solar--run-hooks)

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

(keymap-set user-prefix-map "l" #'launch-application)

;;; Display Table
(defface page-break
  '((t :inherit default))
  "Face for displaying a page break.")

(when (display-graphic-p)
  (unless standard-display-table
    (setq-default standard-display-table (make-display-table)))
  (let* ((glyph (make-glyph-code ?─ 'page-break))
         (entry (vconcat (make-list fill-column glyph))))
    (aset standard-display-table ?\^L entry)))

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
(setopt recentf-max-saved-items 100
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

(pcase-dolist (`(,key ,cmd) '(("<left>" windmove-left)
                              ("<right>" windmove-right)
                              ("<up>" windmove-up)
                              ("<down>" windmove-down)
                              ("C-<left>" windmove-display-left)
                              ("C-<right>" windmove-display-right)
                              ("C-<up>" windmove-display-up)
                              ("C-<down>" windmove-display-down)
                              ("M-<left>" windmove-swap-states-left)
                              ("M-<right>" windmove-swap-states-right)
                              ("M-<up>" windmove-swap-states-up)
                              ("M-<down>" windmove-swap-states-down)
                              ("C-M-<left>" windmove-delete-left)
                              ("C-M-<right>" windmove-delete-right)
                              ("C-M-<up>" windmove-delete-up)
                              ("C-M-<down>" windmove-delete-down)))
  (keymap-set user-prefix-map key cmd))

;;; Cleanup
(defun emacs-cleanup ()
  "Do cleanup by killing buffers, tabs and windows."
  (interactive)
  (tab-bar-close-other-tabs)
  (delete-other-windows)
  (dashboard)
  (mapc #'kill-buffer (cdr (buffer-list))))

(keymap-set user-prefix-map "<escape>" #'emacs-cleanup)

;;; Ibuffer
(defun ibuffer--set-filter-groups ()
  "Set `ibuffer-filter-groups'."
  (setq ibuffer-filter-groups
        (append (thread-last (buffer-list)
                             (mapcar (lambda (buffer)
                                       (with-current-buffer buffer (vc-root-dir))))
                             (delq nil)
                             delete-dups
                             (mapcar (lambda (project)
                                       (let ((name      (file-name-nondirectory (directory-file-name project)))
                                             (directory (expand-file-name project)))
                                         `(,name (directory . ,directory))))))
                '(("Dired" (mode . dired-mode))
                  ("Shell" (or (mode . shell-mode)
                               (mode . eshell-mode)
                               (mode . term-mode)))
                  ("Files" (visiting-file))
                  ("**" (name . "\*.*\*"))))))

(setopt ibuffer-display-summary nil
        ibuffer-show-empty-filter-groups nil
        ibuffer-default-sorting-mode 'alphabetic)

(add-hook 'ibuffer-mode-hook #'ibuffer-auto-mode)
(add-hook 'ibuffer-mode-hook 'ibuffer--set-filter-groups)

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
