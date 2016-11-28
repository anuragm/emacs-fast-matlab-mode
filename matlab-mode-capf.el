;;; matlab-mode-capf.el --- Defines a completion function based on MATLAB server.
;;
;; Copyright © 2016 Anurag Mishra
;;
;; Author           : Anurag Mishra
;; URL              : https://github.com/anuragm/matlab-mode
;; Version          : 1.0.0
;; Keywords         : programming, matlab
;; Package-Requires : ((cl-lib "1.0") (dash "2.12"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Defines a completion function based on MATLAB server.

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

(require 'cl-lib)
(require 's)
(require 'matlab-mode-server)

;;Taken from yuhonglin/matlab-mode
(defun matlab--grab-symbol ()
  "Grabs a symbol at point in a MATLAB buffer."
  (let* ((prefix (buffer-substring-no-properties
                  (point)
                  (save-excursion (skip-chars-backward "a-zA-Z0-9._")
                                  (point)))))
    (if (or (= (length prefix) 0)
	    (string= (substring prefix 0 1) ".")
	    (string= (substring prefix 0 1) "/"))
	nil
      prefix)))

(defun matlab--symbol-bound ()
  "Return the bound of MATLAB symbol at point."
  (let* ((endp (point))
         (startp
          (save-excursion
            (skip-chars-backward "a-zA-Z0-9._")
            (point))))
    (cons startp endp)))

(defun matlab--process-completions (substring)
  "Return a list of completion candidates for SUBSTRING from MATLAB process."
  (if substring
      (when matlab-process-running
        ;; If process is running, return results, else return nil
        (progn
          (setq response
                (matlab-process-eval-and-return
                 (format "getCompletions('%s')\n" substring)))
          (s-lines (s-chomp (s-replace matlab--eot "" response)))))
    nil))

(defun matlab-capf ()
  "Completion function for MATLAB using MATLAB server."
  (let ((bounds (matlab--symbol-bound))
        (completions (matlab--process-completions (matlab--grab-symbol))))
    (when (and completions (not (string= (car completions) "")))
      (list (car bounds)
            (cdr bounds)
            completions
            :exclusive 'no))))

(provide 'matlab-mode-capf)
;;; matlab-mode-capf.el ends here
