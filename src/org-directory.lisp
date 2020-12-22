(defpackage #:org-generation/org-directory
  (:nicknames #:og/directory)
  (:export #:remove-empty-dirs
           #:empty-dir?)
  (:use #:cl
        #:org-generation/types
        #:org-generation/maybe
        #:org-generation/type-signature))

(in-package :org-generation/org-directory)


(sig remove-empty-dirs (-> (or file-info org-directory) (or file-info org-directory null)))
(defun remove-empty-dirs (org-dir)
  (if (file-info-p org-dir)
      org-dir
      (let* ((new-dirs
               (remove-if #'null        ; may make it null but that's fine!
                          (mapcar (lambda (x)
                                    (when x
                                      (remove-empty-dirs x)))
                                  (org-directory-dir org-dir))))
             (new-dir (make-org-directory :file (org-directory-file org-dir)
                                          :name (org-directory-name org-dir)
                                          :dir  new-dirs)))
        (if (empty-dir? new-dir)
            nil                         ; remove the unused dir
            new-dir))))


(sig empty-dir? (-> org-directory Boolean))
(defun empty-dir? (dir)
  (and (nothing? (org-directory-file dir))
     (every #'null (org-directory-dir dir))))
