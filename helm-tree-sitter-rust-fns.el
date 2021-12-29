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

   (helm-tree-sitter-get-node-text (helm-tree-sitter-elem-node x ))))

(defun helm-tree-sitter-rust-function-definition-fn (x)
  (unless (helm-tree-sitter-elem-p x)
    (signal 'wrong-type-argument (list 'helm-tree-sitter-elem-p x)))

  (let* ((children-alist (helm-tree-sitter-node-children-to-alist (helm-tree-sitter-elem-node x)))
         (visibility-modifier (helm-tree-sitter-get-node-text (alist-get 'visibility_modifier children-alist)))
         (identifier (helm-tree-sitter-get-node-text (alist-get 'identifier children-alist)))
         (type-identifier (helm-tree-sitter-get-node-text (alist-get 'type_identifier children-alist)))
         (parameters (helm-tree-sitter-get-node-text (alist-get 'parameters children-alist))))

    (concat
     (propertize "Fn / "
                 'face 'italic)

     (concat
      (helm-tree-sitter-append-space-if-not-empty visibility-modifier)
      (helm-tree-sitter-append-space-if-not-empty type-identifier)
      identifier
      parameters

      (helm-tree-sitter-prepend-if-not-empty type-identifier " -> ")))))

(defun helm-tree-sitter-rust-struct-item-fn (x)
  (unless (helm-tree-sitter-elem-p x)
    (signal 'wrong-type-argument (list 'helm-tree-sitter-elem-p x)))

  (let* ((children-alist (helm-tree-sitter-node-children-to-alist (helm-tree-sitter-elem-node x)))
         (identifier (helm-tree-sitter-get-node-text (alist-get 'type_identifier children-alist))))

    (concat
     (propertize "Struct / "
                 'face 'italic)

     identifier)))

(defun helm-tree-sitter-rust-impl-item-fn (x)
  (unless (helm-tree-sitter-elem-p x)
    (signal 'wrong-type-argument (list 'helm-tree-sitter-elem-p x)))

  (let* ((children-alist (helm-tree-sitter-node-children-to-alist (helm-tree-sitter-elem-node x)))
         (identifier (helm-tree-sitter-get-node-text (alist-get 'type_identifier children-alist))))

    (concat
     (propertize "Impl / "
                 'face 'italic)
     identifier)))

(provide 'helm-tree-sitter-rust-fns)
