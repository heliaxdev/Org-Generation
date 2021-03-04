(in-package :og/org-test)


(defun run-tests ()
  (run! 'haskell-test)
  (run! 'org-directory-test))
