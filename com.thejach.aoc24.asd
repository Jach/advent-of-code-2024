(defsystem "com.thejach.aoc24"
  :author "Jach"
  :license "Public Domain"

  :depends-on ("cl-ppcre" "alexandria" "let-plus" "str")

  :serial t
  :components ((:file "package")
               (:file "puzzle-inputs")
               (:file "main")))
