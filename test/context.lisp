(in-package :og/org-test)

(defparameter *small-alias-map*
  (og/code-generation::construct-file-alias-map
   (list #P"/home/katyusha/Documents/Work/Repo/juvix/src/Juvix/Visualize/Graph.hs"
         #P"/home/katyusha/Documents/Work/Repo/juvix/src/Juvix/Visualize/Graph.idris")))
