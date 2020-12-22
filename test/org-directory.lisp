(in-package :og/org-test)

(def-suite org-directory-test
  :description "tests various functions on org-directories")

(in-suite org-directory-test)

(defparameter *x*
  (list
   (org-generation/types:make-file-info
    :PATH #P"/home/katya/Documents/Work/repo/Org-Generation/test/with-data/bar.lisp"
    :ALIAS :NONE)
   (org-generation/types:make-org-directory
    :FILE :NONE
    :DIR (list
          (org-generation/types:make-file-info
           :PATH #P"/home/katya/Documents/Work/repo/Org-Generation/test/with-data/with/foo.lisp"
           :ALIAS :NONE))
    :NAME "with")))


(test remove-empty-dirs
  (is (equalp
       (remove-if #'null
                  (mapcar #'og/directory:remove-empty-dirs
                          (og/code-generation::files-and-dirs #p"./test/with-data/"
                                                             (fset:set "lisp"))))
       *x*))
  (is (equalp
       (remove-if #'null
                  (mapcar #'og/directory:remove-empty-dirs
                          (og/code-generation::files-and-dirs #p"./foo/"
                                                             (fset:set "lisp"))))
       nil)))
