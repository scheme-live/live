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

(test-end)
