;;; helm-tree-sitter-utils.el --- Helm interface for tree-sitter -*- lexical-binding: t -*-

;; Copyright (C) 2021 Giedrius Jonikas <giedriusj1@gmail.com>

;; Author: Giedrius Jonikas <giedriusj1@gmail.com>
;; Version: 0.1.0
;; URL: https://gitlab.com/giedriusj1/helm-tree-sitter

;; Package-Requires: ((emacs "25.1") (helm "3.6.2") (tree-sitter "0.16.1"))

;; This program is free software: you can redistribute it and/or modify
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
;; Simple helm interface to tree-sitter.

;;; Commentary:
;; Utility functions, mainly dealing with strings and copying propertized
;; text from buffers.

;;; Code:

(require 'tsc)

(defun helm-tree-sitter-utils-node-children-to-alist (node)
  (let ((pl '()))
    (dotimes (e (tsc-count-named-children node))
      (let* ((child-node (tsc-get-nth-named-child node e)))
        (setf (alist-get (tsc-node-type child-node) pl) child-node)))
    pl))

(defun helm-tree-sitter-utils-strip-newlines-and-whitespaces (str)
  (unless (stringp str)
    (signal 'wrong-type-argument (list 'stringp str)))

  (replace-regexp-in-string
   "\s" ""
   (replace-regexp-in-string
    "\n" ""
    str)))

;; Copy text from buffer between node-start-byte and node-end-byte.
;; We use this instead of (tsc-node-text node), because this way
;; we can get fontified text.
(defun helm-tree-sitter-utils-get-node-text (node)
  (if (tsc-node-p node)
      (buffer-substring
       (tsc-node-start-position node)
       (tsc-node-end-position node))
    ""))

;; Same as function above, but we'll return nil if no node is
;; provided.
(defun helm-tree-sitter-utils-get-node-text-or-nil (node)
  (when (tsc-node-p node)
    (buffer-substring
     (tsc-node-start-position node)
     (tsc-node-end-position node))))

(defun helm-tree-sitter-utils-append-space-if-not-empty(str)
  (if (not (helm-tree-sitter-utils-empty-string str))
      (concat str " ") str))

(defun helm-tree-sitter-utils-prepend-if-not-empty (str prepend)
  (when (not (= (length str) 0))
    (concat prepend str)))

(defun helm-tree-sitter-utils-strip-newlines (str)
  (replace-regexp-in-string "\n" "" str))

(defun helm-tree-sitter-utils-empty-string (str)
  (when (stringp str)
    (= (length str) 0)))

(provide 'helm-tree-sitter-utils)

;;; helm-tree-sitter-utils.el ends here
