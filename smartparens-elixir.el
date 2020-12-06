;;; smartparens-elixir.el --- Configuration for Elixir.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Matúš Goljer

;; Author: Matúš Goljer <matus.goljer@gmail.com>
;; Maintainer: Matúš Goljer <matus.goljer@gmail.com>
;; Version: 0.0.1
;; Created: 15th January 2017
;; Keywords: languages

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'smartparens)

(--each '(elixir-mode)
  (add-to-list 'sp-sexp-suffix (list it 'regexp "")))

(defvar sp--elixir-keywords
  (regexp-opt '("defmodule" "defmacro" "defmacrop" "def" "defp"
                "if" "unless" "case" "cond"
                "with" "for" "receive" "try" "quote"))
  "Regexp that matches opening delimiters.")

(defun sp-elixir-search-def-start (pattern)
  "Return non-nil if the \"do\" keyword is part of definition.

Definitions are the constructions of the form defmodule-do-end,
def-do-end and similar pairs."
  (save-excursion
    (catch 'definition
      (while t
        (let ((line (thing-at-point 'line t)))
          (message "%s" line)
          (cond
           ((string-match-p pattern line)
            (throw 'definition t))
           ;; we terminate the search as early as we find any of
           ;; ordinary keywords
           ((string-match-p sp--elixir-keywords line)
            (throw 'definition nil))
           ;; we've hit the top, no luck
           ((bobp) (throw 'definition nil))))
        (forward-line -1)))))

(defun sp-elixir-skip-do-keyword-p (ms _mb _me)
  (sp-elixir-search-def-start "\\bdo:"))

(defun sp-elixir-skip-def-p (ms _mb _me)
  "Test if \"do\" is part of definition.
MS, MB, ME."
  (when (equal "do" ms)
    (sp-elixir-search-def-start sp--elixir-keywords)))

(defun sp-elixir-do-block-post-handler (_id action _context)
  "Insert \"do\" keyword and indent the new block.
ID, ACTION, CONTEXT."
  (when (eq action 'insert)
    (let ((m (make-marker)))
      (save-excursion
        (forward-word) ;; over the "end"
        (move-marker m (point)))
      (save-excursion (newline))
      (save-excursion (insert " do"))
      (indent-region (line-beginning-position) m)
      (move-marker m nil nil))))

(defun sp-elixir-empty-do-block-post-handler (_id action _context)
  "Insert empty \"do\" keyword and indent the new block.

This is used for receive-do-end expression.
ID, ACTION, CONTEXT."
  (when (eq action 'insert)
    (let ((m (make-marker)))
      (save-excursion
        (forward-word) ;; over the "end"
        (move-marker m (point)))
      (save-excursion
        (forward-line -1)
        (end-of-line)
        (insert " do"))
      (save-excursion (newline))
      (indent-region (line-beginning-position) m)
      (indent-according-to-mode)
      (move-marker m nil nil))))

(sp-with-modes 'elixir-mode
  (sp-local-pair "do" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :skip-match 'sp-elixir-skip-def-p
                 :unless '(sp-in-comment-p sp-in-string-p))
  (sp-local-pair "def" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :post-handlers '(sp-elixir-do-block-post-handler)
                 :skip-match 'sp-elixir-skip-do-keyword-p
                 :unless '(sp-in-comment-p sp-in-string-p))
  (sp-local-pair "defp" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :post-handlers '(sp-elixir-do-block-post-handler)
                 :skip-match 'sp-elixir-skip-do-keyword-p
                 :unless '(sp-in-comment-p sp-in-string-p))
  (sp-local-pair "defmodule" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :post-handlers '(sp-elixir-do-block-post-handler)
                 :skip-match 'sp-elixir-skip-do-keyword-p
                 :unless '(sp-in-comment-p sp-in-string-p))
  (sp-local-pair "fn" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :post-handlers '("| "))
  (sp-local-pair "if" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :post-handlers '(sp-elixir-do-block-post-handler)
                 :skip-match 'sp-elixir-skip-do-keyword-p
                 :unless '(sp-in-comment-p sp-in-string-p))
  (sp-local-pair "unless" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :post-handlers '(sp-elixir-do-block-post-handler)
                 :skip-match 'sp-elixir-skip-do-keyword-p
                 :unless '(sp-in-comment-p sp-in-string-p))
  (sp-local-pair "case" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :post-handlers '(sp-elixir-do-block-post-handler)
                 :unless '(sp-in-comment-p sp-in-string-p))
  (sp-local-pair "receive" "end"
                 :when '(("RET" "<evil-ret>"))
                 :skip-match 'sp-elixir-skip-do-keyword-p
                 :post-handlers '(sp-elixir-empty-do-block-post-handler))
  )

(provide 'smartparens-elixir)
;;; smartparens-elixir.el ends here
