;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Nov 29 04:33:05 2003
;;;; Contains: Load tests for pathnames and logical pathnames

(in-package :cl-test)

(compile-and-load "pathnames-aux.lsp")

(load "pathnames/pathnames.lsp")
(load "pathnames/pathname.lsp")
(load "pathnames/pathnamep.lsp")
(load "pathnames/make-pathname.lsp")
(load "pathnames/pathname-host.lsp")
(load "pathnames/pathname-device.lsp")
(load "pathnames/pathname-directory.lsp")
(load "pathnames/pathname-name.lsp")
(load "pathnames/pathname-type.lsp")
(load "pathnames/pathname-version.lsp")

(load "pathnames/load-logical-pathname-translations.lsp")
(load "pathnames/logical-pathname.lsp")
(load "pathnames/logical-pathname-translations.lsp")
(load "pathnames/translate-logical-pathname.lsp")

(load "pathnames/namestring.lsp")
(load "pathnames/file-namestring.lsp")
(load "pathnames/directory-namestring.lsp")
(load "pathnames/host-namestring.lsp")
(load "pathnames/enough-namestring.lsp")

(load "pathnames/wild-pathname-p.lsp")
(load "pathnames/merge-pathnames.lsp")
(load "pathnames/pathname-match-p.lsp")

(load "pathnames/parse-namestring.lsp")
