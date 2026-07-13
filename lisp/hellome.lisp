#!/opt/homebrew/bin/sbcl --script
(require :uiop)
(format t "hello ~a!~&" (uiop:getenv "USER"))