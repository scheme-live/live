(import (scheme base) (live path) (live test))

(test-begin "live/path")

(test-group "path-append handles blank entries"
  (test-equal "" (path-append ""))
  (test-equal "" (path-append "" ""))
  (test-equal "" (path-append "" "" ""))
  (test-equal " " (path-append "" " "))
  (test-equal " /" (path-append " " ""))
  (test-equal " /" (path-append "" " " ""))
  (test-equal " /" (path-append " " "" "")))

(test-group "path-append handles slashes"
  (test-equal "" (path-append ""))
  (test-equal "/bar" (path-append "" "foo/" "/bar"))
  (test-equal "/bar" (path-append "" "/foo/" "/bar"))
  (test-equal "/bar" (path-append "" "/foo//" "/bar"))
  (test-equal "/foo//bar" (path-append "" "/foo//" "bar"))
  (test-equal "/foo/bar" (path-append "" "/foo" "bar"))
  (test-equal "/foo/bar" (path-append "" "/foo/" "bar"))
  (test-equal "/foo/bar" (path-append "" "/foo/" "" "" "" "bar"))
  (test-equal "/bar" (path-append "" "/foo/" "" "" "" "/bar"))
  (test-equal "/foo/bar" (path-append "" "/foo/" "" "" "" "bar"))
  (test-equal "/foo/bar" (path-append "" "/foo" "" "" "" "bar")))

(test-group "path-suffix"
  (test-equal "b" (path-suffix "foo/...a.b"))
  (test-equal "" (path-suffix "foo/...a."))
  (test-equal #f (path-suffix "foo/...a"))
  (test-equal "c" (path-suffix "foo/...a.b.c")))

(test-end)
