;;; -*- mode: lisp; package: hotentry-maker-mode -*-

;;; hotentry-maker-mode.l --- Hot Entry Maker mode
;;
;; Copyright (c) 2008 MIYAMUKO Katsuyuki.
;;
;; Author: MIYAMUKO Katsuyuki <miyamuko@gmail.com>
;; Version: 0.1
;; Keywords: joke, hotentry, hatena
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:
;;
;; * M-x hotentry-maker でブログタイトルを入力するとホッテントリになりそうなタイトルを
;;   xyzzy のバッファ上に表示します。
;;
;; * F5 を押すとタイトルが更新されます。
;;
;; * 依存ライブラリ
;;   * xml-http-request
;;     http://miyamuko.s56.xrea.com/xyzzy/xml-http-request/intro.htm
;;


;;; Code:

(in-package :lisp)
(require "xml-http-request")

(defpackage :hotentry-maker-mode
  (:use
   :lisp :editor

   :xml-http-request
   ))
(use-package :hotentry-maker-mode :user)

(in-package :hotentry-maker-mode)

(export '(hotentry-maker
          ))

(defparameter *hotentry-maker-uri* "http://pha22.net/hotentry/title/r")
(defparameter *hotentry-maker-buffer-name* "*Hot Entry Maker*")
(defparameter *hotentry-maker-title-variable* 'hotentry-title)

(defun hotentry-maker (title)
  (interactive "sTitle: ")
  (with-output-to-temp-buffer (*hotentry-maker-buffer-name*)
    (make-variable-buffer-local *hotentry-maker-title-variable*)
    (setf hotentry-title title)
    (local-set-key #\F5 'hotentry-maker-reload)
    (multiple-value-bind (phrase-list uri)
        (make-hotentry-title title)
      (format t "(ﾟ∀ﾟ) . ｡o O（今日は~Aについて書く~%" title)
      (format t "~A~%~%" uri)
      (write-hotentry-title phrase-list))))

(defun hotentry-maker-reload ()
  (interactive)
  (let ((buf (find-buffer *hotentry-maker-buffer-name*)))
    (when (eq buf (selected-buffer))
      (with-output-to-buffer (buf (point-max))
        (write-hotentry-title
         (make-hotentry-title (buffer-local-value buf *hotentry-maker-title-variable*)))))))

(defun write-hotentry-title (phrase-list)
  (dolist (phrase phrase-list)
    (format t "~A~%" phrase)))

(defun make-hotentry-title (title)
  (xhr:xhr-get *hotentry-maker-uri*
               :query `(:word ,title)
               :encoding *encoding-utf8n*
               :since :epoch
               :key 'scan-phrase))

(defun scan-phrase (res)
  (values
   (mapcar 'html-unescape
           (scan-inner-text (xhr:xhr-response-text res)
                            "<span class=\"phrase\">" "</span>"))
   (xhr:xhr-requested-uri res)))

(defun scan-inner-text (html start-tag end-tag)
  (let ((start 0)
        (offset (length start-tag))
        s e r)
    (while (and (setf s (string-match start-tag html start))
                (setf e (string-match end-tag html s)))
      (push (substring html (+ s offset) e) r)
      (setf start (1+ e)))
    (nreverse r)))

(defun html-unescape (html)
  (reduce #'(lambda (r pair)
              (substitute-string r (car pair) (string (cdr pair))))
          '(("&lt;" . #\<) ("&gt;" . #\>) ("&quot;" . #\") ("&amp;" . #\&))
          :initial-value html))


(provide "hotentry-maker-mode")

;;; hotentry-maker-mode.l ends here
