(lang dune 2.8)
(name yamlt)

(package
 (name yamlt)
 (depends
  jsont
  yaml
 )
)

(package
 (name yamlt_unix)
 (depends
  yamlt
 )
)
