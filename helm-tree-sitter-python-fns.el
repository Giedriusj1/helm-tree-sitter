(setq helm-tree-sitter-python-candidate-producer
      '(("import_statement"      . helm-tree-sitter-python-import-statement-fn)
        ("import_from_statement" . helm-tree-sitter-python-import-statement-fn)
        ("function_definition"   . helm-tree-sitter-python-function-definition-fn)
        ("class_definition"      . helm-tree-sitter-python-class-definition-fn)))

(defun helm-tree-sitter-python-import-statement-fn (x)
  (unless (helm-tree-sitter-elem-p x)
    (signal 'wrong-type-argument (list 'helm-tree-sitter-elem-p x)))

  (concat
   (propertize "Dependency / "
               'face 'italic)

   (helm-tree-sitter-get-node-text (helm-tree-sitter-elem-node x))))

(defun helm-tree-sitter-python-function-definition-fn (x)
  (unless (helm-tree-sitter-elem-p x)
    (signal 'wrong-type-argument (list 'helm-tree-sitter-elem-p x)))

  (let* ((children-alist (helm-tree-sitter-node-children-to-alist (helm-tree-sitter-elem-node x)))
         (identifier (helm-tree-sitter-get-node-text (alist-get 'identifier children-alist)))
         (parameters (helm-tree-sitter-get-node-text (alist-get 'parameters children-alist))))

    (concat
     (propertize "Function / "
                 'face 'italic)
     (concat
      identifier
      (helm-tree-sitter-strip-newlines-and-whitespaces parameters)))))


(defun helm-tree-sitter-python-class-definition-fn (x)
  (unless (helm-tree-sitter-elem-p x)
    (signal 'wrong-type-argument (list 'helm-tree-sitter-elem-p x)))

  (let* ((children-alist (helm-tree-sitter-node-children-to-alist (helm-tree-sitter-elem-node x)))
         (identifier (helm-tree-sitter-get-node-text (alist-get 'identifier children-alist))))

    (concat
     (propertize "Class / "
                 'face 'italic)
     identifier)))

(provide 'helm-tree-sitter-python-fns)
