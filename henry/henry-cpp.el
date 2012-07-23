(add-hook 'c++-mode-hook '(lambda () (progn (setq c-basic-offset 2)
                                            (setq indent-tabs-mode nil)
                                            (setq tab-width 2)
                                            (semantic-mode)
                                            (c-set-style "my-cc-style")
                                            (my-cedet-hook))))

(add-hook 'c-mode-hook '(lambda () (progn (setq c-basic-offset 2)
                                          (setq indent-tabs-mode nil)
                                          (setq tab-width 2)
                                          (semantic-mode)
                                          (c-set-style "my-cc-style")
                                          (my-cedet-hook))))

;; (semanticdb-enable-gnu-global-databases 'c-mode)
(semanticdb-enable-gnu-global-databases 'c++-mode)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(defconst my-cc-style
'("gnu" (c-offsets-alist . 
												 ((innamespace . [0])
													(arglist-cont-nonempty . 4)
													(access-label . -1)
													(member-init-intro . 4))
												 )))
(c-add-style "my-cc-style" my-cc-style)

(global-set-key [f5] 'compile)

(provide 'henry-cpp)
