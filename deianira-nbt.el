;;; nbt.el --- Non-Blocking Threads -*- lexical-binding: t -*-

;; Copyright (C) 2022 Martin Edström

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;; Author:  <meedstrom@teknik.io>
;; Created: 2022-10-30
;; Version: 0.1.0
;; Keywords: tools
;; Homepage: https://github.com/meedstrom/deianira
;; Package-Requires: ((emacs "28.1") (named-timer "0.1"))

;;; Commentary:

;; Ok, say you have a big slow function, and you decide to carve it up into
;; piecemeal functions each with imperceptible latency.  How will you run those
;; functions?  Through a dolist?  No, because dolists can't be paused, so that'd
;; be the same as the big slow function.  You need something smart... something
;; with `run-with-timer' perhaps...
;;
;; This library uses builtin timers to help you run a series of functions.
;;
;; - it weeds out many edge cases for you
;; - it gets out of the way when the user is operating Emacs & should be
;;   unnoticable
;; - it launches the series instantly, not after a second of idle time or so,
;;   because a fraction of a second can be all the time you have before needing
;;   the result of the computation, e.g. on switching to minibuffer and
;;   beginning to type

;; The API consists of `nbt-make' and `nbt-run'.  For usage example, see
;; deianira.el.

;;; Code:

(require 'map)
(require 'cl-lib)
(require 'named-timer) ;; a 70-line library that prevents bugs every day

(defvar nbt-debug t)

(defun nbt-debug-buffer ()
  (let ((bufname (concat (unless dei-debug " ") "*nbt.el debug*")))
    (or (get-buffer bufname)
        (with-current-buffer (get-buffer-create bufname)
          (setq-local truncate-lines t)
          (setq-local buffer-read-only nil)
          (current-buffer)))))

(defun nbt-echo (&rest args)
  "Write a message to the debug buffer.
Arguments same as for `format'."
  (with-current-buffer (nbt-debug-buffer)
    (goto-char (point-min))
    (insert (apply #'format
                   (cons (concat (format-time-string "%T: ")
                                 (car args))
                         (cdr args))))
    (newline)
    ;; caller may have use for it, e.g. like (error (nbt-echo "..."))
    (apply #'format args)))

(defvar nbt-table (make-hash-table))

;;;###autoload
(defun nbt-make (name funs)
  (puthash name `((running . nil)
                  (chain . nil)
                  (last-idle-value . 0)
                  (template . ,funs))
           nbt-table))

;; Macro here b/c when it was a function, (setf (nbt-value ...)) did not work.
;; Something with generalized variables; seems you need to define a setter
;; function named \(setf\ nbt-value\).  Eh, this macro will do.
(defmacro nbt-value (nbt key)
  `(map-elt (map-elt nbt-table ,nbt) ,key))

(defun nbt-set (nbt key val)
  (map-put! (map-elt nbt-table nbt) key val))

;; Extra-convenient accessor for user, see readme
(defmacro nbt-chain (nbt)
  `(cdr (assoc 'chain (gethash ,nbt nbt-table))))

(cl-defun nbt-chomp (name &optional politely per-stage on-abort)
  (if (and per-stage
           (eq 'abort (funcall per-stage)))
      ;; Abort whole chain
      (progn
        (nbt-set name 'running nil)
        (nbt-set name 'chain nil)
        (funcall on-abort))
    (let ((fun (car-safe (nbt-value name 'chain))))
      (condition-case err
          (progn
            ;; In case something called this function twice and it wasn't the
            ;; timer that did it (if the timer ran the function, it won't be
            ;; among the active timers while this body is executing, so the
            ;; error isn't tripped).
            (when (member (named-timer-get name) timer-list)
              (error (nbt-echo "Timer was not cancelled before `nbt-chomp'")))
            (nbt-set name 'running t)
            (nbt-echo "Running: %s" fun)
            (funcall fun) ;; Real work happens here
            ;; If we're here, we know the above funcall exited without error,
            ;; so we can safely pop that step off the queue.
            (pop (nbt-value name 'chain))
            ;; Schedule the next step.  Note to reader: draw a flowchart...
            (when (nbt-value name 'chain)
              (let ((idled-time (or (current-idle-time) 0)))
                (if (or (and politely
                             (time-less-p 1.0 idled-time))
                        (and (not politely)
                             (time-less-p idled-time 1.0)
                             (time-less-p (nbt-value name 'last-idle-value) idled-time)))
                    ;; If user hasn't done any I/O since last chomp, go go go.
                    ;; If being impolite, go even within 1.0 second timeframe
                    ;; until there is user input within that 1.0-second
                    ;; timeframe, in which case give up and be polite.
                    ;;
                    ;; NOTE: By avoiding direct funcall, we avoid running into
                    ;; max-lisp-eval-depth (in case user makes a long chain).
                    (named-timer-run name 0 nil #'nbt-chomp name nil per-stage on-abort)
                  ;; Otherwise give Emacs a moment to respond to user input,
                  ;; and keep waiting as long as input keeps happening.
                  (named-timer-idle-run name 1.0 nil #'nbt-chomp name 'politely per-stage on-abort))
                (nbt-set name 'last-idle-value idled-time))))
        ((error quit)
         (nbt-echo "Chain interrupted because: %s" err)
         (nbt-set name 'running nil)
         (when (eq (car err) 'error)
           (error "Function %s failed: %s" fun (cdr err))))))
    ;; The chain finished, so null running so we know it wasn't due to interruption.
    (unless (nbt-value name 'chain)
      (nbt-set name 'running nil))))

(cl-defun nbt-run (name &key on-interrupt-discovered on-start per-stage on-abort)
  "Run the chain of functions identified by NAME.
The chain must have been previously specified with `nbt-make'.

Run sequentially, but pause for user input to keep Emacs snappy.

If the named chain is already in progress, let it continue.

The optional keyword arguments accept a function or nil.  If any
of these functions (other than ON-ABORT) return the symbol
'abort, the chain will be aborted, its nbt-value name reset, and the
function ON-ABORT called.

These functions are called in the following order:

:on-interrupt-discovered
:on-start
:per-stage
:on-abort"
  (declare (indent defun))
  (map-let (running chain last-idle-value template) (map-elt nbt-table name)
    (if running
        ;; TODO: can we end up in a state where running is t and chain empty?

        ;; Q: can we end up in a state where timer is inactive? A: yes, it's the
        ;; normal case for nbt-chomp because it's often called by a timer, which
        ;; takes itself off the timer-list prior to calling.  However,
        ;; nbt-run is not called by a timer.
        (progn
          (when (not (member (named-timer-get name) timer-list))
            (error (nbt-echo "No timer, but `running' still t")))
          (if chain
              (nbt-echo "Already running chain, letting it continue")
            (error (nbt-echo "Chain empty but `running' still t"))))
      (if (and (not running)
               chain
               (not (equal chain template)))
          ;; Something must have interrupted execution.  Resume.
          (if (or (not on-interrupt-discovered)
                  (and on-interrupt-discovered
                       (not (equal 'abort (funcall on-interrupt-discovered)))))
              ;; The on-interrupt-discovered function returned OK, or we didn't have such a function to run
              (progn
                (nbt-echo "Chain had been interrupted, resuming")
                (nbt-chomp name 'politely per-stage on-abort))
            (named-timer-cancel name)
            (nbt-set name 'running nil)
            (nbt-set name 'chain nil)
            (funcall on-abort))
        (if (or (not on-start)
                (and on-start
                     (not (equal 'abort (funcall on-start)))))
            ;; The on-start function returned OK, or we didn't have such a function to run
            (progn
              (nbt-echo "Launching new chain.  Previous value of chain: %s" chain)
              (named-timer-cancel name)
              (nbt-set name 'chain template)
              (nbt-set name 'last-idle-value 1) ;; so chomp won't wait
              (nbt-chomp name nil per-stage on-abort))
          (named-timer-cancel name)
          (nbt-set name 'running nil)
          (nbt-set name 'chain nil)
          (funcall on-abort))))))

(provide 'nbt)
