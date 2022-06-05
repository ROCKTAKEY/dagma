;;; dagma.el --- Emacs configure managed as directed acyclic graph  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  ROCKTAKEY

;; Author: ROCKTAKEY <rocktakey@gmail.com>
;; Keywords: tools

;; Version: 0.0.0
;; Package-Requires: ((emacs "24.1") (cl-lib "0.7"))
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

(require 'cl-lib)

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


;;; Primitive things for node

(cl-defstruct dagma-node
  "Node of graph in `dagma'."
  parents
  children
  plist)

(defcustom dagma-node-default-plist nil
  "Default value of plist on `dagma-node'.
It is used when you make instance of `dagma-node' through `dagma--node-create'."
  :risky t
  :group 'dagma
  :type 'plist)

(cl-defun dagma--node-create (&key parents children plist)
  "Make instance of `dagma-node' with PARENTS, CHILDREN and PLIST.
`dagma-node-default-plist' is used as default value of PLIST."
  (make-dagma-node :parents parents
                   :children children
                   :plist (or plist dagma-node-default-plist)))

(defun dagma--node-plist-get (node prop)
  "Extract a value on PROP from a property list in `dagma-node' NODE."
  (cl-check-type node dagma-node)
  (plist-get (dagma-node-plist node) prop))

(defun dagma--node-plist-put (node prop val)
  "Change value in PLIST of PROP to VAL in NODE."
  (cl-check-type node dagma-node)
  (let ((plist (dagma-node-plist node)))
    (dagma--plist-put plist prop val)
    (setf (dagma-node-plist node) plist)))

(defun dagma--node-parents-add-1 (node key)
  "Add KEY to list as `parents' in `dagma-node' NODE."
  (cl-check-type node dagma-node)
  (cl-check-type key symbol)
  (let ((parents (dagma-node-parents node)))
    (unless (memq key parents)
      (setf (dagma-node-parents node) (cons key parents)))))

(defun dagma--node-parents-remove-1 (node key)
  "Remove KEY from list as `parents' in `dagma-node' NODE."
  (cl-check-type node dagma-node)
  (cl-check-type key symbol)
  (let ((parents (dagma-node-parents node)))
    (setf (dagma-node-parents node) (cl-remove key parents))))

(defun dagma--node-children-add-1 (node key)
  "Add KEY to list as `children' in `dagma-node' NODE."
  (cl-check-type node dagma-node)
  (cl-check-type key symbol)
  (let ((children (dagma-node-children node)))
    (unless (memq key children)
      (setf (dagma-node-children node) (cons key children)))))

(defun dagma--node-children-remove-1 (node key)
  "Remove KEY from list as `children' in `dagma-node' NODE."
  (cl-check-type node dagma-node)
  (cl-check-type key symbol)
  (let ((children (dagma-node-children node)))
    (setf (dagma-node-children node) (cl-remove key children))))


;;; Primitive functions to control graph

(defun dagma--graph-create ()
  "Create `dagma-graph' object."
  (make-hash-table))

(defun dagma--graph-node-get (graph key)
  "Get `dagma-node' node from GRAPH at key KEY."
  (cl-check-type graph hash-table)
  (gethash key graph))

(defun dagma--graph-node-get-ensure (graph key)
  "Get `dagma-node' node from GRAPH at key KEY."
  (cl-check-type graph hash-table)
  (or (dagma--graph-node-get graph key)
      (let ((node (dagma--node-create)))
        (dagma--graph-node-put graph key node))))

(defun dagma--graph-node-put (graph key node)
  "Store `dagma-node' NODE into the node of GRAPH at key KEY."
  (cl-check-type graph hash-table)
  (cl-check-type node dagma-node)
  (puthash key node graph))

(defun dagma--graph-children-get (graph key)
  "Get children on `dagma-node' node in GRAPH at key KEY."
  (dagma-node-children (dagma--graph-node-get-ensure graph key)))

(defun dagma--graph-children-put (graph key val)
  "Store VAL as children on `dagma-node' node in GRAPH at key KEY."
  (let ((node (dagma--graph-node-get-ensure graph key)))
    (setf (dagma-node-children node) val)))

(defun dagma--graph-parents-get (graph key)
  "Get parents on `dagma-node' node in GRAPH at key KEY."
  (dagma-node-parents (dagma--graph-node-get-ensure graph key)))

(defun dagma--graph-parents-put (graph key val)
  "Store VAL as parents on `dagma-node' node in GRAPH at key KEY."
  (let ((node (dagma--graph-node-get-ensure graph key)))
    (setf (dagma-node-parents node) val)))

(defun dagma--graph-children-add-1 (graph key child)
  "Add CHILD to list as `children' in `dagma-node' node in GRAPH at key KEY."
  (dagma--node-children-add-1 (dagma--graph-node-get-ensure graph key) child))

(defun dagma--graph-children-remove-1 (graph key child)
  "Remove CHILD to list as `children' in `dagma-node' node in GRAPH at key KEY."
  (dagma--node-children-remove-1 (dagma--graph-node-get-ensure graph key) child))

(defun dagma--graph-parents-add-1 (graph key parent)
  "Add PARENT to list as `parents' in `dagma-node' node in GRAPH at key KEY."
  (dagma--node-parents-add-1 (dagma--graph-node-get-ensure graph key) parent))

(defun dagma--graph-parents-remove-1 (graph key parent)
  "Remove PARENT to list as `parents' in `dagma-node' node in GRAPH at key KEY."
  (dagma--node-parents-remove-1 (dagma--graph-node-get-ensure graph key) parent))


;;; Primitive functions to control plist in graph

(defun dagma--graph-plist-get (graph key prop)
  "Look up KEY in GRAPH to get `dagma-node' and get plist value on PROP."
  (dagma--node-plist-get (dagma--graph-node-get-ensure graph key) prop))

(defun dagma--graph-plist-put (graph key prop val)
  "Look up KEY in GRAPH to get `dagma-node' and put plist value to VAL on PROP."
  (dagma--node-plist-put (dagma--graph-node-get-ensure graph key) prop val))

(provide 'dagma)
;;; dagma.el ends here
