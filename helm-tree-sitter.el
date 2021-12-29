;;; helm-tree-sitter.el --- Helm interface for tree-sitter -*- lexical-binding: t -*-

;; Copyright (C) 2021 Giedrius Jonikas <giedriusj1@gmail.com>

;; Author: Giedrius Jonikas <giedriusj1@gmail.com>
;; Version: 0.1
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

;; Currently only C/C++, Python and Rust are supported, but adding more
;; languages should be trivial.

;;; Code:

(require 'tree-sitter)
(require 'helm)

(require 'helm-tree-sitter-utilities)

(require 'helm-tree-sitter-c-fns)
(require 'helm-tree-sitter-cpp-fns)
(require 'helm-tree-sitter-python-fns)
(require 'helm-tree-sitter-rust-fns)

;; tree-sitter element. Basically holds everything we care about each of the elements.
(cl-defstruct helm-tree-sitter-elem node node-type node-text start-pos depth)

(setq hts-producer-mode-maps
      '((python-mode . helm-tree-sitter-python-candidate-producer)
        (c++-mode . helm-tree-sitter-cpp-candidate-producer)
        (c-mode . helm-tree-sitter-c-candidate-producer)
        (rust-mode . helm-tree-sitter-rust-candidate-producer)
        (rustic-mode . helm-tree-sitter-rust-candidate-producer)))

;; If tree-sitter-tree is available and we know how to deal with major-mode,
;; we'll use helm-tree-sitter. Otherwise we'll default to helm-imenu
;;;###autoload
(defun helm-tree-sitter-or-imenu ()
  "Helm interface for tree-sitter. If tree-sitter is enabled and we
know how to deal with major mode, we'll use helm-tree-sitter.
Otherwise we'll default to helm-imenu"
  (interactive)

  (if (and tree-sitter-tree
           (symbol-value (assoc-default major-mode hts-producer-mode-maps)))
      (helm-tree-sitter)
    (helm-imenu)))



;;;###autoload
(defun helm-tree-sitter ()
  "Helm interface for tree-sitter"
  (interactive)

  ;; We'll be copying fontified text from the buffer, so we want to
  ;; make sure that it's been properly fontifier before we do anything.
  (if (fboundp 'font-lock-ensure)
      (font-lock-ensure)
    (with-no-warnings
      (font-lock-fontify-buffer)))

  (helm :sources
        (helm-build-sync-source "Tree-sitter"
          :candidates (helm-tree-sitter-elements-to-helm-candidates (helm-tree-sitter-build-node-list (tsc-root-node tree-sitter-tree) 0))
          :action (lambda (x)
                    (goto-char (helm-tree-sitter-elem-start-pos x)))
          :fuzzy-match t)
        :candidate-number-limit 9999
        :buffer "*helm tree-sitter*"))

(defun helm-tree-sitter-get-candidate-producer-for-current-mode ()
  (let* ((our-producer (symbol-value (assoc-default major-mode hts-producer-mode-maps)) ))
    (if our-producer
        our-producer

      ;; TODO: we probably want to signal that the mode is not supported
      nil)))

(defun helm-tree-sitter-elements-to-helm-candidates (elements)
  (remq nil
        (mapcar
         (lambda (node)
           (let* ((my-fn (assoc-default
                          (format "%s" (helm-tree-sitter-elem-node-type node))
                          (helm-tree-sitter-get-candidate-producer-for-current-mode))))
             (when my-fn
               ;; Great, we have a handler for the element node type
               (let ((fun-ret (funcall my-fn node))) ; Let's get the actual text
                 (if fun-ret
                     ;; Each candidate will consist of a list containing (text-string . tree)
                     (cons
                      fun-ret
                      node ; Store the tree too, so additional actions can be performed later
                      )

                   ;; Our handler function can return nil to indicate that the particular case was not worthy of showing.
                   nil )))))
         elements )))

;; Inspect the tree-sitter-tree and build a flat list with all the nodes.
;; This will later be used to build helm candidates.
(defun helm-tree-sitter-build-node-list (node depth)
  (let* (elements '())
    ;; Add the current node
    (add-to-list 'elements
                 (make-helm-tree-sitter-elem
                  :node node
                  :node-type (tsc-node-type node)
                  :node-text (tsc-node-text node)
                  :start-pos (tsc-node-start-position node)
                  :depth depth))

    ;; And now all the child nodes..
    (dotimes (e (tsc-count-named-children node))
      (setq elements (append  elements (helm-tree-sitter-build-node-list (tsc-get-nth-named-child node e) (1+ depth)))))

    elements))


(defun helm-tree-sitter-node-children-to-alist (node)
  (let ((pl '()))
    (dotimes (e (tsc-count-named-children node))
      (let* ((child-node (tsc-get-nth-named-child node e)))
        (setf (alist-get (tsc-node-type child-node) pl) child-node)))
    pl))

(provide 'helm-tree-sitter)

;;; helm-tree-sitter.el ends here