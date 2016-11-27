;;; matlab-mode-server.el --- Server process for MATLAB
;;
;; Copyright © 2016 Anurag Mishra
;;
;; Author: Anurag Mishra
;; URL: https://github.com/anuragm/matlab-mode
;; Version: 1.0.0
;; Keywords: programming, matlab
;; Package-Requires: ((cl-lib "1.0") (dash "2.12"))
;; This file is not part of GNU Emacs.

;;; Commentary:

;; A server process allows us to use MATLAB binary for various purposes.

;;; License:

;; Copyright (c) 2016 Anurag Mishra

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'cl-macs)
(require 'dash)

;; Constants
(defconst matlab--eot ">> "
  "String sent by MATLAB to signal the end of a response.")

;; Customizable options.
(defcustom matlab-server-executable "matlab"
  "Location of the MATLAB executable file."
  :group 'matlab-server)

(defcustom matlab-server-buffer-name "*matlab-server*"
  "Name of the buffer for MATLAB process.  Set <space> as first character to hide this buffer."
  :group 'matlab-server
  :options '"*matlab*")

(defcustom matlab-server-toolbox-location ""
  "Location of the toolbox for MATLAB server."
    :group 'matlab-server)

;; Local variables.
(defvar matlab-process-running nil
  "True if a MATLAB server process is running.")

(defvar matlab-process nil
  "Handle to running MATLAB server.")

(defvar matlab-output-list nil
  "A list containing result returned by evaluation in MATLAB process.")

(defvar matlab-response-complete nil
  "Is t if MATLAB has finished response.")

;; Define a filter function for MATLAB server process. Taken from irony-mode.
;;   We read a response, write it to the temporary buffer, and keep
;;   doing so till we encounter ">> ", the end of MATLAB response. At
;;   the end, join all the string in list and save it in
;;   matlab-output-list.
(defun matlab--server-process-filter (process string)
  "Read from PROCESS the output STRING, and stop when we encounter the EOT."
  (setq matlab-response-complete nil)
  (let ((pbuf (process-buffer process))
        responses)
    ;; append string to process buffer
    (when (buffer-live-p pbuf)
      (with-current-buffer pbuf
        (save-excursion
          (goto-char (process-mark process))
          (insert string)
          (set-marker (process-mark process) (point))
          ;; check if the message is complete based on `matlab--eot'. If so, set
          ;; response complete to true.
          (goto-char (point-min))
          (while (search-forward matlab--eot nil t)
            (let ((response (buffer-substring-no-properties (point-min)
                                                            (point))))
              (delete-region (point-min) (point))
              (setq responses (cons response responses)))
            (setq matlab-response-complete t)
            (goto-char (process-mark process)))))
      (setq responses (delete nil responses))
      ;; Handle all responses.
      (let ((complete-response (apply #'concat (-flatten responses))))
        (setq complete-response (delete "" complete-response))
        (push complete-response matlab-output-list))))
  (setq matlab-output-list (delete "" matlab-output-list)))

  ;; A function to start a server if not running already.
  (defun matlab-start-server()
    "Start a MATLAB server process."
    (interactive)
    (unless matlab-process-running
      (message "Starting MATLAB server...")
      (let ((process-connection-type nil)
            (process-adaptive-read-buffering nil) process)
        (setq matlab-process
              (start-process-shell-command
               "matlab-process"
               matlab-server-buffer-name
               (format "%s -nodesktop"
                       (shell-quote-argument matlab-server-executable)))))
      (set-process-query-on-exit-flag matlab-process nil) ; Don't ask on exit.
      (set-process-filter matlab-process 'matlab--server-process-filter)
      (setq matlab-process-running t)
      (setq matlab-output-list nil)
      (message "MATLAB server started")))

  ;; A function to kill server process if running.
  (defun matlab-kill-server()
    "Kill the MATLAB server, if running."
    (interactive)
    (when (or matlab-process-running (process-live-p matlab-process))
      (kill-buffer (process-buffer matlab-process))
      (delete-process matlab-process))
    (message "Server killed")
    (setq matlab-process-running nil)
    (setq matlab-output-list nil))

  ;;A function to send a command to MATLAB process
  (cl-defun matlab-process-eval (command)
    "Evaluate the given COMMAND with the MATLAB process."
    (unless matlab-process-running
      (return-from matlab-process-eval))
    (setq matlab-response-complete nil)
    (process-send-string matlab-process command))

  ;;A function to get last result.
  (defun matlab-process-last-result ()
    "Return the result in MATLAB buffer from last command."
    (pop matlab-output-list))

  (cl-defun matlab-process-eval-and-return (command)
    "Evaluate the COMMAND with MATLAB process, block, and return the result."
    (unless matlab-process-running
      (message "MATLAB server is not running. Please start it.")
      (return-from matlab-process-eval-and-return))
    (matlab-process-eval command)
    ;;wait till result is back
    (while (not matlab-response-complete)
      (accept-process-output matlab-process))
    (pop matlab-output-list))

  (provide 'matlab-mode-server)
;;; matlab-mode-server.el ends here
