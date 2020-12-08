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

(defvar sp-elixir-keywords
  (regexp-opt '("defmodule" "defmacro" "defmacrop" "def" "defp"
                "if" "unless" "case" "cond"
                "with" "for" "receive" "try" "quote")
              'words)
  "Regexp that matches opening delimiters.")

(defun sp-elixir-search-do-keyword (direction)
  "Search in given DIRECTION if \"do:\" keyword can be found.

DIRECTION is either 1 or -1.

This search terminates early if any of `sp-elixir-keywords' were
found."
  ;; Check starting line out of the loop, so to avoid unwanted
  ;; early termination
  (if (string-match-p "\\bdo:" (thing-at-point 'line t)) t
    (catch 'definition
      (save-excursion
        (while t
          (forward-line direction)
          (let ((line (string-trim-left (thing-at-point 'line t))))
            (cond
             ;; Terminate the search as early as we find any of
             ;; `sp-elixir-keywords'
             ((eq (string-match-p sp-elixir-keywords line) 0)
              (throw 'definition nil))
             ((string-match-p "\\bdo:" line)
              (throw 'definition t))
             ((or (bobp) (eobp)) (throw 'definition nil)))))))))

(defun sp-elixir-search-def-start ()
  "Search for definition start.

Definitions in Elixir can contain any of `sp-elixir-keywords' and
are followed with \"do\" keyword, which may not be on the same
line."
  (save-excursion
    (catch 'definition
      (while t
        (let ((line (string-trim-left (thing-at-point 'line t))))
          (cond ((eq (string-match-p sp-elixir-keywords line) 0)
                 (throw 'definition t))
                ((bobp) (throw 'definition nil))))
        (forward-line -1)))))

(defun sp-elixir-skip-do-keyword-p (ms _mb _me)
  "Test if \"do:\" is part of definition.

MS must not be \"do\" keyword."
  (unless (equal "do" ms)
    (sp-elixir-search-do-keyword 1)))

(defun sp-elixir-skip-def-p (ms _mb _me)
  "Test if \"do\" is part of definition.

MS must be \"do\" keyword.

Also checks for \"do:\" keyword similarly to
`sp-elixir-skip-do-keyword-p' except searches backwards"
  (when (equal "do" ms)
    (or (sp-elixir-search-def-start)
        (sp-elixir-search-do-keyword -1))))

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
