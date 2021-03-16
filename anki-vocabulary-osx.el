;;; anki-vocabulary.el --- Help you to create vocabulary cards in Anki

;; Copyright (C) 2021-2021 Hyunggyu Jang <murasakipurplez5@gmail.com>.

;; Author: Hyunggyu Jang <murasakipruplez5@gmail.com>
;; Keywords: lisp, anki, mac
;; Package: anki-vocabulary
;; Version: 1.0
;; Package-Requires: ((emacs "24.4") (osx-dictionary "0.4") (anki-connect "1.0") (s "1.10"))
;; URL: https://github.com/lujun9972/anki-vocabulary.el

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Source code
;;
;; anki-vocabulary's code can be found here:
;;   https://github.com/lujun9972/anki-vocabulary.el

;;; Commentary:

;; anki-vocabulary is a plugin that helps you to create vocabulary cards in Anki
;; First of all, execute =M-x anki-vocabulary-set-ankiconnect= to set the correspondence relation for fields in card.
;; Then,select the sentence and execute =M-x anki-vocabulary=


;;; Code:

(require 's)
(require 'cl-lib)
(require 'subr-x)
(require 'anki-connect)
(require 'osx-dictionary)
(declare-function pdf-view-active-region-text "ext:pdf-view" ())
(declare-function pdf-view-assert-active-region "ext:pdf-view" () t)


(defgroup anki-vocabulary nil
  ""
  :prefix "anki-vocabulary"
  :group 'tools
  :link '(url-link :tag "Github" "https://github.com/lujun9972/anki-vocabulary.el"))

(defcustom anki-vocabulary-deck-name ""
  "Which deck would the word stored."
  :type 'string)

(defcustom anki-vocabulary-model-name ""
  "Specify the model name."
  :type 'string)

(defcustom anki-vocabulary-field-alist nil
  "Specify the corresponding relationship for fields in card."
  :type 'string)

(defcustom anki-vocabulary-audio-fileds nil
  "Specify fields used to store audio."
  :type 'list)

(defcustom anki-vocabulary-before-addnote-functions nil
  "List of hook functions run before add note.

The functions should accept those arguments:
+ expression(单词)
+ sentence(单词所在句子)
+ sentence_clozed(单词所在句子,单词加粗)
+ glossary(单词释义)
+ phonetic(音标)"
  :type 'hook)

(defcustom anki-vocabulary-after-addnote-functions nil
  "List of hook functions run after add note.

The functions should accept those arguments:
+ expression(单词)
+ sentence(单词所在句子)
+ sentence_clozed(单词所在句子,单词加粗)
+ glossary(单词释义)
+ phonetic(音标)"
  :type 'hook)

;;;###autoload
(defun anki-vocabulary-set-ankiconnect ()
  "Set the correspondence relation for fields in card."
  (interactive)
  (let ((deck-names (anki-connect-deck-names))
        (model-names (anki-connect-model-names)))
    (setq anki-vocabulary-deck-name (completing-read "Select the Deck Name:" deck-names))
    (setq anki-vocabulary-model-name (completing-read "Select the Model Name:" model-names))
    (setq anki-vocabulary-field-alist nil)
    (setq anki-vocabulary-audio-fileds nil)
    (let* ((fields (anki-connect-model-field-names anki-vocabulary-model-name))
           (elements '("${expression:单词}" "${glossary:释义}" "${phonetic:音标}" "${sentence:原文例句}" "${sentence_clozed:标粗的原文例句}" "${translation:翻译例句}" "${sound:发声}" "SKIP")))
      (dolist (field fields)
        (let* ((prompt (format "%s" field))
               (element (completing-read prompt elements)))
          (unless (string= element "SKIP")
            (if (equal "${sound:发声}" element)
                (cl-pushnew field anki-vocabulary-audio-fileds)
              (setq elements (remove element elements))
              (add-to-list 'anki-vocabulary-field-alist (cons field element)))))))))

(defun anki-vocabulary--get-normal-text ()
  "Get the text in normal mode."
  (let ((txt (if (region-active-p)
                 (buffer-substring-no-properties (region-beginning) (region-end))
               (or (sentence-at-point)
                   (thing-at-point 'line)))))
    (replace-regexp-in-string "[\r\n]+" " " txt)))

(defun anki-vocabulary--get-pdf-text ()
  "Get the text in pdf mode."
  ;; (if (package-installed-p 'pdf-tools)
  ;;     (require 'pdf-view)
  ;;   (error "`pdf-tools` is required!"))
  (pdf-view-assert-active-region)
  (let* ((txt (pdf-view-active-region-text))
         (txt (string-join txt "\n")))
    (replace-regexp-in-string "[\r\n]+" " " txt)))

(defun anki-vocabulary--get-text ()
  "Get the region text."
  (if (derived-mode-p 'pdf-view-mode)
      (anki-vocabulary--get-pdf-text)
    (anki-vocabulary--get-normal-text)))

(defun anki-vocabulary--select-word-in-string (str &optional default-word)
  "Select word in STR.
Optional argument DEFAULT-WORD specify the default word."
  (let ((words (split-string str "[ \f\t\n\r\v,.:?;\"<>()]+")))
    (completing-read "Pick The Word: " words nil nil default-word)))

(defun anki-vocabulary--get-word ()
  "Get the word at point."
  (unless (derived-mode-p 'pdf-view-mode)
    (word-at-point)))

(defcustom anki-vocabulary-word-searcher #'anki-vocabulary--word-searcher-osx
  "Function used to search word's meaning.

The function should return an alist like
    `((expression . ,expression)
      (glossary . ,glossary)
      (phonetic . ,phonetic))"
  :type 'function)

(defsubst anki-vocabulary--english? (char)
  "Test whether given CHAR is an English character."
  (eq (char-charset char) 'ascii))

(defsubst anki-vocabulary--japanese? (char)
  "Test whether given CHAR is a Japenese character."
  (eq (char-charset char) 'unicode))

(defun anki-vocabulary--word-searcher-osx (word)
  "Search WORD using youdao.

It returns an alist like
    `((expression . ,expression)
      (glossary . ,glossary)
      (phonetic . ,phonetic))"
  (let ((queried (osx-dictionary--search word)))
    (if (string-empty-p queried)
        (user-error "Queried word isn't an entry of OSX-Dictionary"))
    (pcase (aref queried 0)
     ((pred anki-vocabulary--japanese?)
      (anki-vocabulary--parse-japanese-dictionary queried))
     ((pred anki-vocabulary--english?)
      (anki-vocabulary--parse-english-dictionary queried))
     (_
      (user-error "Cannot parse given dictionary output: Unknown language")))))

(defun anki-vocabulary--parse-japanese-dictionary (queried)
  "Parse QUERIED from Japanese dictionary."
  (let ((starter " 【")
        (end "】")
        expression glossary phonetic
        mb me)
    (string-match starter queried)
    (setq mb (match-beginning 0)
          me (match-end 0))
    (setq phonetic (substring queried 0 mb))
    (setq queried (substring queried me))
    (string-match end queried)
    (setq mb (match-beginning 0)
          me (match-end 0))
    (setq expression (substring queried 0 mb))
    (setq glossary (substring queried me))
    `((expression . ,expression)
      (glossary . ,glossary)
      (phonetic . ,phonetic))))

(defun anki-vocabulary--parse-english-dictionary (queried)
  "Parse QUERIED from English dictionary."
  (let ((separator " | ")
        expression glossary phonetic
        mb me)
    (string-match separator queried)
    (setq mb (match-beginning 0)
          me (match-end 0))
    (setq expression (substring queried 0 mb))
    (setq queried (substring queried me))
    (string-match separator queried)
    (setq mb (match-beginning 0)
          me (match-end 0))
    (setq phonetic (substring queried 0 mb))
    (setq glossary (substring queried me))
    `((expression . ,expression)
      (glossary . ,glossary)
      (phonetic . ,phonetic))))

;;;###autoload
(defun anki-vocabulary (&optional sentence word)
  "Translate SENTENCE and WORD, and then create an anki card."
  (interactive)
  (let* ((sentence (or sentence (anki-vocabulary--get-text))) ; 原句
         (word (or word  (anki-vocabulary--select-word-in-string sentence (anki-vocabulary--get-word))))
         (sentence_clozed (replace-regexp-in-string (concat "\\b" (regexp-quote word) "\\b")
                                                  (lambda (word)
                                                    (format "{{c1::%s}}" word))
                                                  sentence)) ; 粗体标记的句子
         (content (funcall anki-vocabulary-word-searcher word))
         (expression (or (cdr (assoc 'expression content))
                         ""))           ; 单词
         (glossary (or (replace-regexp-in-string
                        "\\(\\\\\\\\\\)?[ \t]*\n"
                        "<br>" (cdr (assoc 'glossary content)))
                       ""))
         (phonetic (or (cdr (assoc 'phonetic content))
                       ""))             ; 音标
         (data `((expression:单词 . ,expression)
                 (glossary:释义 . ,glossary)
                 (phonetic:音标 . ,phonetic)
                 (sentence:原文例句 . ,sentence)
                 (sentence_clozed:标粗的原文例句 . ,sentence_clozed)))
         (fileds (mapcar #'car anki-vocabulary-field-alist))
         (elements (mapcar #'cdr anki-vocabulary-field-alist))
         (values (mapcar (lambda (e)
                           (s-format e 'aget data))
                         elements))
         (fields (cl-mapcar #'cons fileds values)))
    (run-hook-with-args 'anki-vocabulary-before-addnote-functions expression sentence sentence_clozed glossary phonetic)
    (anki-connect-add-note anki-vocabulary-deck-name anki-vocabulary-model-name fields)
    (run-hook-with-args 'anki-vocabulary-after-addnote-functions expression sentence sentence_clozed glossary phonetic)))

(provide 'anki-vocabulary-osx)

;;; anki-vocabulary-osx.el ends here
