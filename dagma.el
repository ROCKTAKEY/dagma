;;; dagma.el --- Emacs configure managed as directed acyclic graph  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  ROCKTAKEY

;; Author: ROCKTAKEY <rocktakey@gmail.com>
;; Keywords: tools

;; Version: 0.0.0
;; Package-Requires: ((emacs "24.1"))
;; URL: https://github.com/ROCKTAKEY/dagma

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Emacs configure managed as directed acyclic graph

;;; Code:

(defgroup dagma ()
  "Emacs configure managed as directed acyclic graph"
  :group 'tools
  :prefix "dagma-"
  :link '(url-link "https://github.com/ROCKTAKEY/dagma"))



(defmacro dagma--plist-put (plist prop val)
  "Change value in PLIST of PROP to VAL.
It is same as `plist-put' except it is valid even when PLIST is empty."
  `(if ,plist
       (plist-put ,plist ,prop ,val)
     (setq ,plist (list ,prop ,val))))

(provide 'dagma)
;;; dagma.el ends here
