(in-package :og/org-test)

(defparameter *test-config*
  '(:name "Juvix"
    :dir-before-code ("src" "app" "test")))

(defparameter *test-context*
  (org-generation/haskell:initialize *test-config*))


(def-suite haskell-test
  :description "Tests the og/haskell package")

(in-suite haskell-test)


(test convert
  (is (equalp
       (og/haskell:convert-path
        #P"/home/katyusha/Documents/Work/Repo/juvix/src/Juvix/Visualize/Graph.hs"
        *test-context*)
       "Juvix.Visualize.Graph")))
