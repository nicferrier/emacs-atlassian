;;; atlassian.el --- helpful tools for atlassian stuff

;; Copyright (C) 2014  Nic Ferrier

;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
;; Keywords: hypermedia

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(require 'url-parse)
(require 'kv)
(require 'xml-rpc)

(defconst atlassian/auth-tokens (make-hash-table :test 'equal)
  "The authentication tokens to be used hashed against site URL.")

(defvar atlassian/authenicate-username-history nil
  "History of usernames used.")

(defun atlassian/authenticate (username passwrd url)
  "Will collect USERNAME and PASSWRD from the user.

URL is just set to `atlassian-url' so let bind that when you call
this."
  (interactive
   (list
    (read-from-minibuffer
     "username: " nil nil nil 'atlassian/authenicate-username-history)
    (read-passwd "password: ")
    atlassian-url))
  (condition-case err
      (puthash
       url (xml-rpc-method-call url 'confluence2.login username passwrd)
       atlassian/auth-tokens)
    (error (message "whoops! some error: %s" err))))

;; Make an error symbol
(put 'atlassian-session-error 'error-conditions
     '(error atlassian atlassian-session-error))

(defun atlassian/call (url method-symbol &rest parameters)
  "Call the method specified by METHOD-SYMBOL with PARAMETERS.

The authentication token for URL is looked up and if not present
then authentication is done."
  (condition-case err
      (let ((token (or (gethash url atlassian/auth-tokens)
                       (let ((atlassian-url url))
                         (call-interactively 'atlassian/authenticate)))))
        (apply 'xml-rpc-method-call url method-symbol token parameters))
    (error
     (let* ((err-thing (cadr err))
            (err-str (if (stringp err-thing)
                         err-thing
                         "unknown error")))
       (if (string-match "Call login() to open a new session" err-str)
           (progn
             ;; Just guess that the error is a session timeout
             (remhash url atlassian/auth-tokens)
             (apply 'atlassian/call url method-symbol parameters))
           (message "whoops! unknown error! %S %S" err parameters))))))

(defun atlassian/convert-wiki (url page-text)
  "Return the HTML text for the PAGE-TEXT being edited.

PAGE-TEXT is either a string or a buffer."
  (atlassian/call
   url 'confluence2.convertWikiToStorageFormat
   (cond 
     ((stringp page-text) page-text)
     ((bufferp page-text)
      (with-current-buffer page-text
        (buffer-substring-no-properties
         (point-min) (point-max)))))))

(defun atlassian/html-element->wiki (e)
  "Convert a single HTML E to Wiki text."
  (if (not (listp e))
      e
      (case (car e)
        ((h1 h2 h3) (format "%s. %s\n" (symbol-name (car e)) (elt e 2)))
        (p (cond 
             ((listp (cddr e))
              (concat (atlassian/html->wiki (cddr e) t) "\n"))
             ((equal (elt e 2) " ") "")
             (t
              (concat (elt e 2) "\n"))))
        (br "\n")
        (ul (concat (atlassian/html->wiki (cddr e)) "\n"))
        (li (format "* %s" (elt e 2)))
        (a (if (elt e 2)
               (format "[%s|%s]" (elt e 2) (kva 'href (cadr e)))
               (format "[%s]" (kva 'href (cadr e))))))))

(defun atlassian/html->wiki (dom &optional no-delimit)
  "Convert a DOM subtree into Wiki."
  (mapconcat 'atlassian/html-element->wiki dom (if no-delimit "" "\n")))

(defun atlassian/doc->wiki (doc)
  "Convert a confluence DOC to Wiki text.

DOC is a list created by `libxml-parse-xml'."
  (interactive
   (list (let ((sexp (condition-case err 
                         (save-excursion
                           (goto-char (point-min))
                           (read (current-buffer)))
                       (error nil))))
           (if (listp sexp) sexp (error "no document")))))
  (atlassian/html->wiki (cddr (elt doc 2))))


(defvar atlassian/edit-url-history nil
  "The history of the edit page url.")

(defvar atlassian/edit-page-space-history nil
  "The history of the edit page space.")

(defvar atlassian/edit-page-title-history nil
  "The history of the edit page title.")

(defvar atlassian/edit-content nil
  "Buffer local content string")

(defun atlassian/lixbml-parse-string (str)
  (with-temp-buffer
    (insert str)
    (libxml-parse-html-region (point-min) (point-max))))

(defun atlassian/libxml-wiki (buffer &optional debug)
  "`libxml-parse-xml-region' the content in the BUFFER.

The content is stored in `atlassian/edit-content'."
  (interactive (list (current-buffer) t))
  (let ((parsed
         (with-current-buffer buffer
           (let ((content (kva "content" atlassian/edit-content)))
             (atlassian/libxml-parse-string content)))))
    parsed))

(defun atlassian-edit (url page-space page-title)
  "Edit the page in URL with PAGE-SPACE and PAGE-TITLE."
  (interactive
   (list
    (read-from-minibuffer
     "Url: " (car atlassian/edit-url-history)
     nil nil 'atlassian/edit-url-history)
    (read-from-minibuffer
     "Page space: " (car atlassian/edit-page-space-history)
     nil nil 'atlassian/edit-page-space-history)
    (read-from-minibuffer
     "Page title: " (car atlassian/edit-page-title-history)
     nil nil 'atlassian/edit-page-title-history)))
  (let ((content (atlassian/call
                  url 'confluence2.getPage page-space page-title)))
    (with-current-buffer
        (get-buffer-create
         (format "*atlassian-%s-%s*" page-space page-title))
      (erase-buffer)
      (make-variable-buffer-local 'atlassian/edit-content)
      (setq atlassian/edit-content content)
      (insert (or (condition-case err
                      (atlassian/doc->wiki
                       (atlassian/libxml-wiki (current-buffer)))
                    (error nil)) ""))
      (switch-to-buffer-other-window (current-buffer)))))

(defun atlassian/page-url->rpc-url (page-url)
  (let ((url (url-generic-parse-url page-url)))
    (format "%s://%s//rpc/xmlrpc" (url-type url) (url-host url))))

(defconst atlassian/update-keys
  '("space" "title" "content"
    "id" "version")
  "The keys that are required in the store page alist when updating.")

(defun atlassian/store-page (page-buffer)
  "Store the page in PAGE-BUFFER.

PAGE-BUFFER must be a buffer created by `atlassian-edit' so that
it has the necessary meta data stored in the page."
  (interactive (list (current-buffer)))
  (let* ((rpc-url (atlassian/page-url->rpc-url
                   (kva "url" atlassian/edit-content)))
         (wiki-text (with-current-buffer page-buffer
                      (buffer-substring-no-properties
                       (point-min) (point-max))))
         (content (atlassian/convert-wiki rpc-url wiki-text))
         (doc (format
               "<?xml version='1.0'?><html><body>%s</body></html>"
               content)))
    ;; Show the HTML in another buffer
    (with-current-buffer (get-buffer-create "*confluence-edit*")
      (erase-buffer)
      (insert doc)
      (switch-to-buffer-other-window (current-buffer)))
    ;; Functionally replace the content and limit meta data to relevant
    (atlassian/call
     rpc-url 'confluence2.storePage
     (->> atlassian/edit-content
       (--filter (member (car it) atlassian/update-keys))
       (--keep (if (equal (car it) "content")
                   ;;(cons "content" wiki-text)
                   (cons "content" doc)
                   it))))))

(provide 'atlassian)

;;; atlassian.el ends here
