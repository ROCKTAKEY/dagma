;;; dagma-test.el --- Test for dagma

;; Copyright (C) 2022  ROCKTAKEY

;; Author: ROCKTAKEY <rocktakey@gmail.com>

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

;; Test for dagma

;;; Code:

(require 'ert)

(require 'undercover)
(undercover "*.el"
            (:report-format 'codecov)
            (:report-file "coverage-final.json")
            (:send-report nil))

(require 'dagma)

(ert-deftest dagma--plist-put-empty ()
  (let ((plist nil))
    (dagma--plist-put plist :key 'val)
    (should (eq (plist-get plist :key) 'val))))

(ert-deftest dagma--plist-put-not-empty ()
  (let ((plist '(:key1 val1)))
    (dagma--plist-put plist :key2 'val2)
    (should (eq (plist-get plist :key1) 'val1))
    (should (eq (plist-get plist :key2) 'val2))))



(ert-deftest dagma--node-plist-get/put ()
  (let ((node (dagma--node-create)))
    (should (eq (dagma--node-plist-get node :prop1) nil))
    (dagma--node-plist-put node :prop1 'val1)
    (should (eq (dagma--node-plist-get node :prop1) 'val1))))

(ert-deftest dagma--node-parents-add-1 ()
  (let ((node (dagma--node-create :parents '(key1 key2))))
    (should (memq 'key1 (dagma-node-parents node)))
    (should (memq 'key2 (dagma-node-parents node)))
    (dagma--node-parents-add-1 node 'key3)
    (should (memq 'key1 (dagma-node-parents node)))
    (should (memq 'key2 (dagma-node-parents node)))
    (should (memq 'key3 (dagma-node-parents node)))))

(ert-deftest dagma--node-parents-remove-1 ()
  (let ((node (dagma--node-create :parents '(key1 key2))))
    (should (memq 'key1 (dagma-node-parents node)))
    (should (memq 'key2 (dagma-node-parents node)))
    (dagma--node-parents-remove-1 node 'key3)
    (should (memq 'key1 (dagma-node-parents node)))
    (should (memq 'key2 (dagma-node-parents node)))
    (should-not (memq 'key3 (dagma-node-parents node)))
    (dagma--node-parents-remove-1 node 'key2)
    (should (memq 'key1 (dagma-node-parents node)))
    (should-not (memq 'key2 (dagma-node-parents node)))))

(ert-deftest dagma--node-children-add-1 ()
  (let ((node (dagma--node-create :children '(key1 key2))))
    (should (memq 'key1 (dagma-node-children node)))
    (should (memq 'key2 (dagma-node-children node)))
    (dagma--node-children-add-1 node 'key3)
    (should (memq 'key1 (dagma-node-children node)))
    (should (memq 'key2 (dagma-node-children node)))
    (should (memq 'key3 (dagma-node-children node)))))

(ert-deftest dagma--node-children-remove-1 ()
  (let ((node (dagma--node-create :children '(key1 key2))))
    (should (memq 'key1 (dagma-node-children node)))
    (should (memq 'key2 (dagma-node-children node)))
    (dagma--node-children-remove-1 node 'key3)
    (should (memq 'key1 (dagma-node-children node)))
    (should (memq 'key2 (dagma-node-children node)))
    (should-not (memq 'key3 (dagma-node-children node)))
    (dagma--node-children-remove-1 node 'key2)
    (should (memq 'key1 (dagma-node-children node)))
    (should-not (memq 'key2 (dagma-node-children node)))))

(provide 'dagma-test)
;;; dagma-test.el ends here
