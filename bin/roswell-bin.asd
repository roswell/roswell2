(defsystem "roswell-bin"
  :class :package-inferred-system
  :depends-on (:roswell-bin/main)
  :build-operation monolithic-concatenate-source-op
  :build-pathname "ros"
  :entry-point "roswell-bin/main:main")
