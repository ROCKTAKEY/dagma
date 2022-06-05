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



(ert-deftest dagma--graph-children-get/put ()
  (let ((graph (dagma--graph-create))
        (node (dagma--node-create :children '(child1 child2)
                                  :parents '(parent1 parent2)
                                  :plist '(:prop1 val1 :prop2 val2))))
    (should-not (dagma--graph-children-get graph 'key1))
    (dagma--graph-node-put graph 'key1 node)
    (should (equal (dagma--graph-children-get graph 'key1) '(child1 child2)))))

(ert-deftest dagma--graph-parents-get/put ()
  (let ((graph (dagma--graph-create))
        (node (dagma--node-create :children '(parent1 parent2)
                                  :parents '(parent1 parent2)
                                  :plist '(:prop1 val1 :prop2 val2))))
    (should-not (dagma--graph-parents-get graph 'key1))
    (dagma--graph-node-put graph 'key1 node)
    (should (equal (dagma--graph-parents-get graph 'key1) '(parent1 parent2)))))



(ert-deftest dagma--graph-children-add/remove ()
  (let ((graph (dagma--graph-create))
        (node (dagma--node-create :children '(child1 child2)
                                  :parents '(parent1 parent2)
                                  :plist '(:prop1 val1 :prop2 val2))))
    (dagma--graph-node-put graph 'key1 node)
    (should-not (memq 'child3 (dagma--graph-children-get graph 'key1)))
    (dagma--graph-children-add-1 graph 'key1 'child3)
    (should (memq 'child3 (dagma--graph-children-get graph 'key1)))

    (should-not (memq 'child3 (dagma--graph-children-get graph 'key2)))
    (dagma--graph-children-add-1 graph 'key2 'child3)
    (should (memq 'child3 (dagma--graph-children-get graph 'key2)))

    (dagma--graph-children-remove-1 graph 'key1 'child3)
    (should-not (memq 'child3 (dagma--graph-children-get graph 'key1)))
    (should (memq 'child1 (dagma--graph-children-get graph 'key1)))
    (should (memq 'child2 (dagma--graph-children-get graph 'key1)))

    (dagma--graph-children-remove-1 graph 'key1 'child4)
    (should (memq 'child1 (dagma--graph-children-get graph 'key1)))
    (should (memq 'child2 (dagma--graph-children-get graph 'key1)))))

(ert-deftest dagma--graph-parents-add/remove ()
  (let ((graph (dagma--graph-create))
        (node (dagma--node-create :children '(child1 child2)
                                  :parents '(parent1 parent2)
                                  :plist '(:prop1 val1 :prop2 val2))))
    (dagma--graph-node-put graph 'key1 node)
    (should-not (memq 'parent3 (dagma--graph-parents-get graph 'key1)))
    (dagma--graph-parents-add-1 graph 'key1 'parent3)
    (should (memq 'parent3 (dagma--graph-parents-get graph 'key1)))

    (should-not (memq 'parent3 (dagma--graph-parents-get graph 'key2)))
    (dagma--graph-parents-add-1 graph 'key2 'parent3)
    (should (memq 'parent3 (dagma--graph-parents-get graph 'key2)))

    (dagma--graph-parents-remove-1 graph 'key1 'parent3)
    (should-not (memq 'parent3 (dagma--graph-parents-get graph 'key1)))
    (should (memq 'parent1 (dagma--graph-parents-get graph 'key1)))
    (should (memq 'parent2 (dagma--graph-parents-get graph 'key1)))

    (dagma--graph-parents-remove-1 graph 'key1 'parent4)
    (should (memq 'parent1 (dagma--graph-parents-get graph 'key1)))
    (should (memq 'parent2 (dagma--graph-parents-get graph 'key1)))))

(ert-deftest dagma--graph-children-put ()
  (let ((graph (dagma--graph-create))
        (node (dagma--node-create :children '(child1 child2)
                                  :parents '(parent1 parent2)
                                  :plist '(:prop1 val1 :prop2 val2))))
    (dagma--graph-node-put graph 'key1 node)
    (should-not (memq 'child3 (dagma--graph-children-get graph 'key1)))
    (dagma--graph-children-put graph 'key1 '(child3))
    (should (memq 'child3 (dagma--graph-children-get graph 'key1)))
    (should-not (memq 'child1 (dagma--graph-children-get graph 'key1)))
    (should-not (memq 'child2 (dagma--graph-children-get graph 'key1)))

    (should-not (memq 'child3 (dagma--graph-children-get graph 'key2)))
    (dagma--graph-children-put graph 'key2 '(child3))
    (should (memq 'child3 (dagma--graph-children-get graph 'key2)))
    (should-not (memq 'child1 (dagma--graph-children-get graph 'key2)))
    (should-not (memq 'child2 (dagma--graph-children-get graph 'key2)))))

(ert-deftest dagma--graph-parents-put ()
  (let ((graph (dagma--graph-create))
        (node (dagma--node-create :children '(child1 child2)
                                  :parents '(parent1 parent2)
                                  :plist '(:prop1 val1 :prop2 val2))))
    (dagma--graph-node-put graph 'key1 node)
    (should-not (memq 'parent3 (dagma--graph-parents-get graph 'key1)))
    (dagma--graph-parents-put graph 'key1 '(parent3))
    (should (memq 'parent3 (dagma--graph-parents-get graph 'key1)))
    (should-not (memq 'parent1 (dagma--graph-parents-get graph 'key1)))
    (should-not (memq 'parent2 (dagma--graph-parents-get graph 'key1)))

    (should-not (memq 'parent3 (dagma--graph-parents-get graph 'key2)))
    (dagma--graph-parents-put graph 'key2 '(parent3))
    (should (memq 'parent3 (dagma--graph-parents-get graph 'key2)))
    (should-not (memq 'parent1 (dagma--graph-parents-get graph 'key2)))
    (should-not (memq 'parent2 (dagma--graph-parents-get graph 'key2)))
))



(ert-deftest dagma--graph-plist-get/put ()
  (let ((graph (dagma--graph-create))
        (node (dagma--node-create :children '(child1 child2)
                                  :parents '(parent1 parent2)
                                  :plist '(:prop1 val1 :prop2 val2))))
    (dagma--graph-node-put graph 'key1 node)
    (should (eq (dagma--graph-plist-get graph 'key1 :prop1) 'val1))
    (should (eq (dagma--graph-plist-get graph 'key1 :prop2) 'val2))
    (should-not (eq (dagma--graph-plist-get graph 'key1 :prop3) 'val3))

    (dagma--graph-plist-put graph 'key1 :prop3 'val3)
    (should (eq (dagma--graph-plist-get graph 'key1 :prop1) 'val1))
    (should (eq (dagma--graph-plist-get graph 'key1 :prop2) 'val2))
    (should (eq (dagma--graph-plist-get graph 'key1 :prop3) 'val3))))

(provide 'dagma-test)
;;; dagma-test.el ends here
