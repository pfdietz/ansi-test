;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Dec 12 19:43:17 2004
;;;; Contains: Load environment tests (section 25)

(load "environment/apropos.lsp")
(load "environment/apropos-list.lsp")
(load "environment/describe.lsp")
(load "environment/disassemble.lsp")
(load "environment/environment-functions.lsp")
(load "environment/room.lsp")
(load "environment/time.lsp")
(load "environment/trace.lsp") ;; and untrace
(load "environment/user-homedir-pathname.lsp")

(load "environment/decode-universal-time.lsp")
(load "environment/encode-universal-time.lsp")
(load "environment/get-universal-time.lsp")
(load "environment/sleep.lsp")
(load "environment/get-internal-time.lsp")

(load "environment/documentation.lsp")
#-lispworks (load "environment/inspect.lsp")
(load "environment/dribble.lsp")
(load "environment/ed.lsp")

