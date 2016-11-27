;;; matlab-mode-server.el --- Server process for MATLAB
;;
;; Copyright © 2016 Anurag Mishra
;;
;; Author: Anurag Mishra
;; URL: https://github.com/anuragm/matlab-mode
;; Version: 1.0.0
;; Keywords: programming, matlab

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

(require 'cl)
(require 's)

;; Constants
(defconst matlab--eot ">> "
  "String sent by MATLAB to signal the end of a response.")

;; Customizable options.
(defcustom matlab-server-executable "matlab"
  "Location of the MATLAB executable file."
  :group 'matlab-server)

(defcustom matlab-server-buffer-name "*matlab-server*"
  "Name of the buffer for MATLAB process.  Set nil to disable process buffer."
  :group 'matlab-server
  :options '"*matlab*")

(defcustom matlab-server-toolbox-location ""
  "Location of the toolbox for MATLAB server.")

;; Local variables.
(defvar matlab-process-running nil
  "True if a MATLAB server process is running.")

(defvar matlab-process nil
  "Handle to running MATLAB server.")

(defvar matlab-output-list nil
  "A list containing result returned by evaluation in MATLAB process")

;; A function to start a server if not running already.
(defun matlab-start-server()
  "Start a MATLAB server process."
  (interactive)
  (unless matlab-process-running
    (message "Starting MATLAB server...")
    (setq matlab-process
          (start-process-shell-command
           "matlab-process"
           matlab-server-buffer-name
           (format "%s -nodesktop"
                   (shell-quote-argument matlab-server-executable))))
    (set-process-query-on-exit-flag matlab-process nil) ; Don't ask on exit.
    (set-process-filter matlab-process 'matlab-process-filter-function)
    (setq matlab-process-running t)
    (message "MATLAB server started")))

;; A function to kill server process if running.
(defun matlab-kill-server()
  "Kill the MATLAB server, if running."
  (interactive)
  (when matlab-process-running
    (delete-process matlab-process)
    (message "Server killed")
  (setq matlab-process-running nil)))

;;A function to send a command to MATLAB process
(defun* matlab-process-eval (command)
  (unless matlab-process-running
    (message "MATLAB server is not running. Please start it.")
    (return-from matlab-process-eval))
  (process-send-string matlab-process command))

;;Define a filter function for MATALB server process
(defun matlab-process-filter-function (process string)
  (push string matlab-output-list))

(provide 'matlab-mode-server)
;;; matlab-mode-server.el ends here
