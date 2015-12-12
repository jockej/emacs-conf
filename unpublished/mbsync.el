;; -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Joakim Jalap

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; NOTE: I don't actually use the customize interface myself, so this might not
;; work at all.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; These are the customazibles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile
  (require 'cl))

(defcustom mbsync-program "mbsync"
  "The mbsync program. If the value is the string \"mbsync\" the return value of
  (`executable-find' \"mbsync\") will be used. Otherwise, this should be the
  absolute path.")

(defcustom mbsync-rc-file "~/.mbsyncrc"
  "The mbsync rc-file")

(defcustom mbsync-update-interval 180
  "Time between syncs. Setting this too low might cause trouble with multiple
  instances running at the same time, so don't do that...")


(defcustom mbsync-sync-objects nil
  "An alist of objects to sync, associated with their respective hosts.")

(defcustom mbsync-max-fail nil
  "The number of consecutive failed attempts to retrieve mail. After this,
  unregister the timed process. If nil, never give up.")

(defcustom mbsync-top-maildir "~/Maildir"
  "Your 'dir of maildirs'.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro mbsync--update-alist (alist key newval)
  "Update the value associated with 'key' in alist 'alist' with
  the value 'newval', creating it if it doesn't exist."
  nil
  `(progn
     (setq ,alist (delete* ,key ,alist :test 'equal :key 'car))
     (setq ,alist (push (cons ,key ,newval) ,alist))))


(defun mbsync--mbsync-running-p ()
  "A function to check if there is a 'mbsync' process running on
the system, by this same user."
  (let ((processes (list-system-processes))
        (me (user-uid))
        (found))
    (dolist (pid processes)
      (let ((cmd (cdr (assoc 'comm (process-attributes pid)))))
        (when (or (string= cmd "mbsync")
                  (and (not (string= mbsync-program "mbsync"))
                       (string= cmd mbsync-program)))
          (let ((uid (cdr (assoc 'euid (process-attributes pid)))))
            (setq found (= uid me))))))
    found))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Secrets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mbsync--get-secret (channel)
  (let ((machine (cdr (assoc channel mbsync-sync-objects))))
    (unless machine (error "Couldn't find machine for %s" channel))
    (or (mbsync--get-secret-from-file machine)
        (read-passwd (format "Password for %s: " channel)))))


(defun mbsync--get-secret-from-file (machine)
  "Queries `auth-source-search' for the secret for 'machine'." 
  (let ((secret
         (auth-source-search :max 1 :host machine
                             :require '(:secret) :create nil)))
    (when secret
      (funcall (plist-get (car secret) :secret)))))

   
(defvar mbsync--start-times nil
  "An alist of start times. Cells have the form (channel . start-time).")


(defun mbsync--password-filter (proc msg)
  (let ((chan (cadr (process-command proc))))
    (if (not (string= msg (concat "Password (" chan "): ")))
        (progn
          (message "Couldn't find password prompt, killing mbsync")
          (kill-process proc))
      ;; send the password
      (let ((pass (mbsync--get-secret chan)))
        (process-send-string proc (concat pass "\n"))))
    ;; we've done our thing, hand it back to the default filter
    (set-process-filter proc nil)))


(defvar mbsync--consecutive-fails nil
  "An alist where each cell has the form (channel . consecutive-fails).
  Consecutive-fails is the number of times in a row the mbsync process has
  failed.

See also `mbsync-max-fail'.")


(defvar mbsync--inhibit-new nil
  "An alist with cells like (channel . allow-sync). If
  allow-sync is non nil, don't schedule any new syncs.")

(defvar mbsync--timers nil
  "An alist which associates channels with their current timers.

Each cell has the form (channel . timer).")


(defun mbsync--schedule-new (channel)
  "Schedule a new sync for channel."
  (let ((timer (run-at-time mbsync-update-interval nil
                            'mbsync--run-mbsync channel t)))
    (push (cons channel timer) mbsync--timers)))
  

(defconst mbsync--buffer-name "*mbsync*"
  "The buffer of the mbsync processes")   


(defun mbsync--insert-success-msg (cmd time)
  "Insert a message into `mbsync--buffer-name'."
  (with-current-buffer mbsync--buffer-name
    (goto-char (point-max))
    (insert (format "%s: " (current-time-string)))
    (insert (format "'%s %s' done in %f seconds.\n"
                    (car cmd) (cadr cmd) time))
    (goto-char (point-max))))


(defun mbsync--give-up (channel)
  "Give up on channel."
  (with-current-buffer mbsync--buffer-name
    (message "mbsync: %s: Too many consecutive fails! Cancelling." channel)
    (insert (format "Syncing %s has failed more than %s times, giving up\n"
                    channel mbsync-max-fail))
    (mbsync-cancel-sync channel)))


(defun mbsync--sentinel (proc event repeat)
  "A sentinel for mbsync processes. "
  ;; first thing's first: if there is no entry for this chan in
  ;; `mbsync--consecutive-fails' then add one with 0 as the value.
  (let* ((exit-status (process-exit-status proc))
         (cmd (process-command proc))
         (chan (cadr cmd)))
    (cond ((and (string= event "finished\n") (= exit-status 0))
           ;; If we get here I think it means we succeeded, so zero the
           ;; `mbsync--consecutive-fails' entry for this channel
           (progn
             (mbsync--update-alist mbsync--consecutive-fails
                                   chan 0)
             (mbsync--insert-success-msg
              cmd
              (- (float-time) (cdr (assoc chan mbsync--start-times))))
             ;; if repeat, start the next one if it doesn't have a non nil entry
             ;; in `mbsync--inhibit-new'.
             (let ((inhb (assoc chan mbsync--inhibit-new)))
               (when (and repeat (not (cdr inhb)))
                 (mbsync--schedule-new chan)))
             (run-hook-with-args 'mbsync-after-fetch-hook chan)))
          ((string-prefix-p "ex" event)
           ;; mbsync exited abnormally, probably because there is no internet
           ;; connection. Anyway, increment the consecutive fails.
           (let* ((old-fails (cdr (assoc chan mbsync--consecutive-fails)))
                  (new-fails (1+ old-fails)))
             (mbsync--update-alist mbsync--consecutive-fails
                                   chan new-fails)
             (if (and mbsync-max-fail
                      (> new-fails mbsync-max-fail))
                 (mbsync--give-up chan)
               (let ((inhb (assoc chan mbsync--inhibit-new)))
                 (when (and repeat (not (cdr inhb)))
                   (mbsync--schedule-new chan))))))
          ;; I don't know what to do about this yet, just echo
          (t (message "mbsync: %s" event)))))


(defun mbsync--sentinel-repeat (proc event)
  "A sentinel which schedules a new job when the old one finishes."  
  (mbsync--sentinel proc event t))


(defun mbsync--sentinel-single (proc event)
  "A sentinel which does not schedule a new job when the old one finishes."
  (mbsync--sentinel proc event nil))

  
(defun mbsync--run-mbsync (channel &optional repeat)
  "The function that actually runs mbsync. Run for channel.

If 'repeat' is non nil, run with a sentinel which will schedule a
  new job when this one finishes, unless `mbsync--no-new' is non
  nil."
  ;; If this channel has no entry for consecutive fails, make one
  (unless (assoc channel mbsync--consecutive-fails)
    (mbsync--update-alist mbsync--consecutive-fails
                          channel 0))
  ;; remove ourselves from the list of timers
  (setq mbsync--timers (delete* channel mbsync--timers :test 'equal :key 'car))
  (let* ((process-connection-type nil)
         (prog (if (string= mbsync-program "mbsync")
                   (executable-find "mbsync")
                 mbsync-program))
         (proc (start-process (format "mbsync-%s" channel)
                              mbsync--buffer-name
                              prog
                              channel)))
    (set-process-sentinel proc (if repeat 'mbsync--sentinel-repeat
                                 'mbsync--sentinel-single))
    (set-process-filter proc 'mbsync--password-filter))
  (mbsync--update-alist mbsync--start-times channel
                        (float-time)))


(defun mbsync--sync-internal (&optional channel repeat)
  "Run a sync. If 'channel' is given, sync that channel.
Otherwise sync them all. If repeat is non nil, schedule a new
sync to run after `mbsync-update-interval' seconds. Both
'channel' and 'repeat' cannot be nil."
  (cond ((and channel repeat)
          ;; If both channel and repeat are t, that's a paddlin'
          (error "Cannot schedule sync for individual channels"))
         ((not (or channel repeat))
          ;; If we got niether, sync all the channels once
          (dolist (chan mbsync-sync-objects)
            (mbsync--run-mbsync (car chan))))
         ((and channel (not repeat))
          ;; sync the channel once
          (mbsync--run-mbsync channel))
         ;; otherwise run the scheduling, this basically means run with a
         ;; sentinel which schedules a new process once the old one has finished
         (t (dolist (chan mbsync-sync-objects)
              (mbsync--run-mbsync (car chan) t)))))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; This section deals with indexing of the fetched mail.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar mbsync-after-fetch-hook nil
  "This hook is called after each succesfull exit of a mbsync process.
  Each hook is given the argument 'channel'.")


(defun mbsync-notmuch-new (channel)
  (let ((process (start-process (format "notumch-%s" channel)
                                nil (executable-find "notmuch") "new")))
    (set-process-sentinel
     process
     (lambda (proc event)
       (let ((exit-status (process-exit-status proc)))
         (when (and (string= event "finished\n") (= exit-status 0))
           (with-current-buffer mbsync--buffer-name
             (unless (mbsync--mbsync-running-p)
               (insert "notmuch hook run\n")
               (goto-char (point-max))))))))))


(defun mbsync-mu-index (channel)
  (let ((process (start-process (format "mu-%s" channel)
                                nil (executable-find "mu")
                                "index" "-m"
                                (expand-file-name mbsync-top-maildir))))
    (set-process-sentinel
     process
     (lambda (proc event)
       (let ((exit-status (process-exit-status proc)))
         (when (and (string= event "finished\n") (= exit-status 0))
           (with-current-buffer mbsync--buffer-name
             (unless (mbsync--mbsync-running-p)
               (insert "mu hook run\n")
               (goto-char (point-max))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Interactive functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mbsync--prompt-for-channel ()
  "Prompt for a channel."
  (let* ((channels
          (let ((chans))
            (dolist (obj mbsync-sync-objects)
              (push (car obj) chans))
            (nreverse chans)))
          (prompt (format "Channel or group (default %s): "
                        (car channels))))
    (completing-read prompt channels
                                  nil nil nil nil
                                  (car channels))))


;;;###autoload
(defun mbsync-sync-single (&optional arg)
  "Run a single sync. With prefix argument, prompt for a channel
or group to sync."
  (interactive "P")
  (if (not arg) (mbsync--sync-internal)
    (mbsync--sync-internal (mbsync--prompt-for-channel))))


;;;###autoload
(defun mbsync-start-continous-sync ()
  "Sync every `mbsync-update-interval' seconds."
  (interactive)
  (setq mbsync--inhibit-new nil)
  (mbsync--sync-internal nil t))


;;;###autoload
(defun mbsync-cancel-sync (channel)
  "Cancel scheduled sync for 'channel', and inhibit the scheduling of new
syncs."
  (interactive)
  ;; prevent scheduling of new jobs
  (mbsync--update-alist mbsync--inhibit-new channel t)
  ;; cancel timer for job
  (let ((timer (assoc channel mbsync--timers)))
    (when timer (cancel-timer (cdr timer)))))


;;;###autoload
(defun mbsync-cancel-all ()
  "Cancel all scheduled syncs and don't schedule any knew."
  (interactive)
  (dolist (chan mbsync-sync-objects)
    (mbsync-cancel-sync (car chan))))


(provide 'mbsync)
;; mbsync.el ends here. (just kidding, it actually ends here)
