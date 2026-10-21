;;; markdown-indent.el --- Dynamic visual indentation for Markdown  -*- lexical-binding: t; -*-

;; Author: OChicken
;; Keywords: markdown, indent, outlines

;;; Commentary:

;; A minor mode providing dynamic virtual indentation for Markdown buffers,
;; modeled after `org-indent-mode'.
;;
;; When `markdown-indent-mode' is active, body text under each heading is
;; visually indented according to the heading's depth, using `line-prefix'
;; and `wrap-prefix' text properties.  The actual buffer content is never
;; modified.
;;
;; Heading lines themselves are also visually indented so that the `#'
;; markers of a heading at level N align with the body text of its parent.
;;
;; Indentation scheme (with `markdown-indent-indentation-per-level' = 2):
;;
;;   # Heading 1          <- 0 spaces prefix
;;   body under h1        <- 2 spaces prefix
;;   ## Heading 2         <- 2 spaces prefix
;;   body under h2        <- 4 spaces prefix
;;   ### Heading 3        <- 4 spaces prefix
;;   body under h3        <- 6 spaces prefix
;;
;; Initialization of existing buffer content happens asynchronously on
;; idle time, as in `org-indent-mode'.

;;; Code:

(require 'cl-lib)

(defgroup markdown-indent nil
  "Options for dynamic virtual indentation in Markdown."
  :tag "Markdown Indent"
  :group 'text)

(defcustom markdown-indent-indentation-per-level 2
  "Number of spaces to indent per heading level."
  :group 'markdown-indent
  :type 'integer)

(defface markdown-indent
  '((t (:inherit (fixed-pitch shadow))))
  "Face for virtual indentation prefix strings.
Make this face blend into the background to render prefixes invisible."
  :group 'markdown-indent)

(defconst markdown-indent--deepest-level 6
  "Maximum heading depth in Markdown (ATX headings h1–h6).")

;;; Internal variables

(defvar-local markdown-indent--text-line-prefixes nil
  "Vector of propertized prefix strings for body text lines, indexed by level.")

(defvar-local markdown-indent--heading-line-prefixes nil
  "Vector of propertized prefix strings for heading lines, indexed by level.")

(defvar-local markdown-indent--initial-marker nil
  "Buffer marker tracking progress of asynchronous initialization.")

(defvar markdown-indent--agent-timer nil
  "Idle timer running the initialization agent.")

(defvar markdown-indent--agentized-buffers nil
  "List of buffers pending asynchronous initialization.")

(defvar markdown-indent--agent-active-delay '(0 2 0)
  "Max time to run agent when its buffer is the current buffer.")

(defvar markdown-indent--agent-passive-delay '(0 0 400000)
  "Max time to run agent when its buffer is not current.")

(defvar markdown-indent--agent-resume-delay '(0 0 100000)
  "Pause before resuming after yielding to other idle processes.")

(defvar markdown-indent--agent-resume-timer nil
  "Timer used to reschedule the agent after yielding.")

;;; Prefix computation

(defun markdown-indent--compute-prefixes ()
  "Precompute prefix strings for all heading levels."
  (setq markdown-indent--heading-line-prefixes
        (make-vector (1+ markdown-indent--deepest-level) nil))
  (setq markdown-indent--text-line-prefixes
        (make-vector (1+ markdown-indent--deepest-level) nil))
  (dotimes (n (1+ markdown-indent--deepest-level))
    ;; Heading at level N aligns with body text of level N-1.
    (let ((head-indent (* (max 0 (1- n)) markdown-indent-indentation-per-level))
          (text-indent (* n markdown-indent-indentation-per-level)))
      (aset markdown-indent--heading-line-prefixes n
            (propertize (make-string head-indent ?\s) 'face 'markdown-indent))
      (aset markdown-indent--text-line-prefixes n
            (propertize (make-string text-indent ?\s) 'face 'markdown-indent)))))

;;; Property helpers

(defun markdown-indent--remove-properties (beg end)
  "Remove `line-prefix' and `wrap-prefix' properties between BEG and END."
  (with-silent-modifications
    (remove-text-properties beg end '(line-prefix nil wrap-prefix nil markdown-indent--state nil))))

(defun markdown-indent--remove-properties-from-string (string)
  "Strip indentation text properties from STRING (for `kill-ring', etc.)."
  (remove-text-properties 0 (length string)
                          '(line-prefix nil wrap-prefix nil markdown-indent--state nil)
                          string)
  string)

;;; Core property-setting logic

;; The parsing state at the start of a line is a cons (LEVEL . FENCE):
;; LEVEL is the heading level in effect, FENCE is the opening fence string
;; (e.g. "```" or "~~~~") when inside a fenced code block, else nil.
;; It is cached on every line as the `markdown-indent--state' property, so
;; that refreshes can start anywhere and stop once the state converges.

(defconst markdown-indent--heading-re "^\\(#+\\)[[:blank:]]"
  "Regexp matching an ATX heading line.")

(defconst markdown-indent--fence-re "^ \\{0,3\\}\\(`\\{3,\\}\\|~\\{3,\\}\\)"
  "Regexp matching the opening of a fenced code block.")

(defconst markdown-indent--fence-close-re
  "^ \\{0,3\\}\\(`\\{3,\\}\\|~\\{3,\\}\\)[[:blank:]\r]*$"
  "Regexp matching the closing of a fenced code block.")

(defun markdown-indent--scan-line (state)
  "Parse the line at point given STATE at its start.
Return (HEADING . NEW-STATE), where HEADING is the heading level of the
line or nil, and NEW-STATE is the state at the start of the next line."
  (let ((level (car state))
        (fence (cdr state)))
    (cond
     ;; Inside a fenced block: only a matching closing fence ends it.
     (fence
      (if (and (looking-at markdown-indent--fence-close-re)
               (string-prefix-p fence (match-string-no-properties 1)))
          (cons nil (cons level nil))
        (cons nil state)))
     ((looking-at markdown-indent--fence-re)
      (cons nil (cons level (match-string-no-properties 1))))
     ((looking-at markdown-indent--heading-re)
      (let ((n (min (length (match-string-no-properties 1))
                    markdown-indent--deepest-level)))
        (cons n (cons n nil))))
     (t (cons nil state)))))

(defun markdown-indent--state-at (pos)
  "Return the parsing state at the start of the line containing POS."
  (save-excursion
    (goto-char pos)
    (forward-line 0)
    (let ((target (point))
          state)
      ;; Walk back to the nearest line with a cached state (or bob) ...
      (while (and (not state) (not (bobp)))
        (forward-line -1)
        (setq state (get-text-property (point) 'markdown-indent--state)))
      (unless state (setq state (cons 0 nil)))
      ;; ... then parse forward to the target line.
      (while (< (point) target)
        (setq state (cdr (markdown-indent--scan-line state)))
        (forward-line 1))
      state)))

(defun markdown-indent--set-line-properties (state level heading-p)
  "Set `line-prefix'/`wrap-prefix' on the current line and advance one line.
STATE is the parsing state at the start of the line, cached on it.
LEVEL is the heading depth that governs indentation.
HEADING-P non-nil means this line is a heading line."
  (let* ((prefix (aref (if heading-p
                            markdown-indent--heading-line-prefixes
                          markdown-indent--text-line-prefixes)
                        level))
         ;; wrap-prefix matches line-prefix so continuation lines align.
         (wrap prefix))
    (add-text-properties (line-beginning-position) (line-beginning-position 2)
                         `(line-prefix ,prefix wrap-prefix ,wrap
                           markdown-indent--state ,state)))
  (forward-line 1))

(defun markdown-indent--add-properties (beg end &optional delay)
  "Add indentation properties from BEG to END.
Past END, keep going until the cached state of a line matches the newly
computed one, so that a changed heading or fence propagates as far as needed.
When DELAY (a time value) is non-nil the process is interruptible: it
throws the tag `interrupt' with the current point when interrupted."
  (save-match-data
    (save-excursion
      (goto-char beg)
      (forward-line 0)
      (let ((state (markdown-indent--state-at (point)))
            (time-limit (and delay (time-add nil delay)))
            (done nil))
        (with-silent-modifications
          (while (and (not done) (not (eobp)))
            (cond
             ;; Past the changed region and state converged (or the rest
             ;; is not initialized yet and left to the agent): stop.
             ((and (> (point) end)
                   (let ((old (get-text-property (point) 'markdown-indent--state)))
                     (or (null old) (equal old state))))
              (setq done t))
             ;; Interruptible: pending user input.
             ((and delay (input-pending-p))
              (throw 'interrupt (point)))
             ;; Interruptible: time slice exhausted.
             ((and delay (time-less-p time-limit nil))
              (setq markdown-indent--agent-resume-timer
                    (run-with-idle-timer
                     (time-add (current-idle-time)
                               markdown-indent--agent-resume-delay)
                     nil #'markdown-indent--initialize-agent))
              (throw 'interrupt (point)))
             ;; Heading line gets the heading prefix of its own level,
             ;; anything else (incl. code blocks) the text prefix.
             (t
              (let* ((res (markdown-indent--scan-line state))
                     (heading (car res)))
                (markdown-indent--set-line-properties
                 state (or heading (car state)) heading)
                (setq state (cdr res)))))))))))

;;; Async initialization agent (mirrors org-indent's agent machinery)

(defun markdown-indent--initialize-agent ()
  "Start or resume asynchronous initialization of agentized buffers."
  (when markdown-indent--agent-resume-timer
    (cancel-timer markdown-indent--agent-resume-timer))
  (setq markdown-indent--agentized-buffers
        (cl-remove-if-not #'buffer-live-p markdown-indent--agentized-buffers))
  (cond
   ;; No more work: shut down agent.
   ((not markdown-indent--agentized-buffers)
    (cancel-timer markdown-indent--agent-timer))
   ;; Current buffer is pending: initialize it with a generous time slice.
   ((memq (current-buffer) markdown-indent--agentized-buffers)
    (markdown-indent--initialize-buffer (current-buffer)
                                        markdown-indent--agent-active-delay))
   ;; Another buffer is pending: initialize it with a short time slice.
   (t
    (markdown-indent--initialize-buffer (car markdown-indent--agentized-buffers)
                                        markdown-indent--agent-passive-delay))))

(defun markdown-indent--initialize-buffer (buffer delay)
  "Asynchronously set indentation properties in BUFFER, yielding after DELAY."
  (with-current-buffer buffer
    (when markdown-indent-mode
      (save-restriction
        (widen)
        (let ((interruptp
               (catch 'interrupt
                 (and markdown-indent--initial-marker
                      (marker-position markdown-indent--initial-marker)
                      (equal (marker-buffer markdown-indent--initial-marker) buffer)
                      (markdown-indent--add-properties
                       markdown-indent--initial-marker (point-max) delay)
                      nil))))
          (move-marker markdown-indent--initial-marker interruptp)
          (unless interruptp
            (setq markdown-indent--agentized-buffers
                  (delq buffer markdown-indent--agentized-buffers))))))))

;;; After-change refresh

(defun markdown-indent--refresh (beg end _len)
  "Refresh indentation properties around the changed region BEG–END.
Called from `after-change-functions'."
  (when markdown-indent-mode
    (save-match-data
      (save-restriction
        (widen)
        ;; `markdown-indent--add-properties' itself extends past END until
        ;; the cached line state converges.
        (markdown-indent--add-properties beg end)))))

;;; Minor mode

;;;###autoload
(define-minor-mode markdown-indent-mode
  "Visually indent Markdown content according to heading structure.

When active, `line-prefix' and `wrap-prefix' text properties are added
to each line so that body text appears indented under its heading.
Buffer content is never changed.

Initial indentation of existing buffer content is performed
asynchronously on idle time."
  :lighter " MdInd"
  :group 'markdown-indent
  (cond
   (markdown-indent-mode
    ;; Turning on.
    (setq-local indent-tabs-mode nil)
    (setq-local markdown-indent--initial-marker (copy-marker 1))
    (markdown-indent--compute-prefixes)
    (add-function :filter-return (local 'filter-buffer-substring-function)
                  #'markdown-indent--remove-properties-from-string)
    (add-hook 'after-change-functions #'markdown-indent--refresh nil t)
    ;; Clear any stale properties before re-indenting.
    (markdown-indent--remove-properties (point-min) (point-max))
    ;; Schedule asynchronous initialization.
    (push (current-buffer) markdown-indent--agentized-buffers)
    (unless (and markdown-indent--agent-timer
                 (not (memq markdown-indent--agent-timer timer-idle-list)))
      (setq markdown-indent--agent-timer
            (run-with-idle-timer 0.2 t #'markdown-indent--initialize-agent))))
   (t
    ;; Turning off.
    (when (markerp markdown-indent--initial-marker)
      (set-marker markdown-indent--initial-marker nil))
    (setq markdown-indent--agentized-buffers
          (delq (current-buffer) markdown-indent--agentized-buffers))
    (remove-function (local 'filter-buffer-substring-function)
                     #'markdown-indent--remove-properties-from-string)
    (remove-hook 'after-change-functions #'markdown-indent--refresh t)
    (save-restriction
      (widen)
      (markdown-indent--remove-properties (point-min) (point-max))))))

(provide 'markdown-indent)
;;; markdown-indent.el ends here
