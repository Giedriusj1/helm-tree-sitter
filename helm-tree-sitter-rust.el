;;; helm-tree-sitter-rust.el --- Helm interface for tree-sitter -*- lexical-binding: t -*-

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
;; Provides function for dealing with Rust code

;;; Code:

(defvar helm-tree-sitter-rust-candidate-producer
  '(("use_declaration" . helm-tree-sitter-rust-use-declaration-fn)
    ("struct_item" . helm-tree-sitter-rust-struct-item-fn)
    ("function_item" . helm-tree-sitter-rust-function-definition-fn)
    ("impl_item" . helm-tree-sitter-rust-impl-item-fn)))

(defun helm-tree-sitter-rust-use-declaration-fn (x)
  (unless (helm-tree-sitter-elem-p x)
    (signal 'wrong-type-argument (list 'helm-tree-sitter-elem-p x)))

  (concat
   (propertize "Use / "
               'face 'italic)

   (helm-tree-sitter-utils-get-node-text (helm-tree-sitter-elem-node x ))))

(defun helm-tree-sitter-rust-function-definition-fn (x)
  (unless (helm-tree-sitter-elem-p x)
    (signal 'wrong-type-argument (list 'helm-tree-sitter-elem-p x)))

  (let* ((children-alist (helm-tree-sitter-node-children-to-alist (helm-tree-sitter-elem-node x)))
         (visibility-modifier (helm-tree-sitter-utils-get-node-text (alist-get 'visibility_modifier children-alist)))
         (identifier (helm-tree-sitter-utils-get-node-text (alist-get 'identifier children-alist)))
         (type-identifier (helm-tree-sitter-utils-get-node-text (alist-get 'type_identifier children-alist)))
         (parameters (helm-tree-sitter-utils-get-node-text (alist-get 'parameters children-alist))))

    (concat
     (propertize "Fn / "
                 'face 'italic)

     (concat
      (helm-tree-sitter-utils-append-space-if-not-empty visibility-modifier)
      (helm-tree-sitter-utils-append-space-if-not-empty type-identifier)
      identifier
      parameters

      (helm-tree-sitter-utils-prepend-if-not-empty type-identifier " -> ")))))

(defun helm-tree-sitter-rust-struct-item-fn (x)
  (unless (helm-tree-sitter-elem-p x)
    (signal 'wrong-type-argument (list 'helm-tree-sitter-elem-p x)))

  (let* ((children-alist (helm-tree-sitter-node-children-to-alist (helm-tree-sitter-elem-node x)))
         (identifier (helm-tree-sitter-utils-get-node-text (alist-get 'type_identifier children-alist))))

    (concat
     (propertize "Struct / "
                 'face 'italic)

     identifier)))

(defun helm-tree-sitter-rust-impl-item-fn (x)
  (unless (helm-tree-sitter-elem-p x)
    (signal 'wrong-type-argument (list 'helm-tree-sitter-elem-p x)))

  (let* ((children-alist (helm-tree-sitter-node-children-to-alist (helm-tree-sitter-elem-node x)))
         (identifier (helm-tree-sitter-utils-get-node-text (alist-get 'type_identifier children-alist))))

    (concat
     (propertize "Impl / "
                 'face 'italic)
     identifier)))

(provide 'helm-tree-sitter-rust)

;;; helm-tree-sitter-rust.el ends here
