;;; -*- lexical-binding: t -*-
;; Author: Masayuki Ataka <masayuki.ataka@gmail.com>
;;

(require 'url)
(require 'dom)

(defun fortee-make-event-page (url)
  (interactive "sURL: ")
  (let* ((event-props (fortee--fetch-url url))
         (event-proposal (plist-get event-props :proposal))
         (event-schedule (plist-get event-props :schedule))
         (event-author   (plist-get event-props :author))
         (event-twitter  (plist-get event-props :twitter))
         (event-day (format-time-string "[[%Y-%m-%d]]"))
         (frontmatter-type "type:: [[tech talk]]")
         (frontmatter-author "author:: ")
         (frontmatter-published-on (concat "published-on:: " event-day))
         (frontmatter-webpage "webpage:: --")
         (body-presenter "- 発表者情報")
         (body-presenter-author (concat "\t- " event-author))
         (body-presenter-twitter (concat "\t- " event-twitter))
         (body-summary "- # まとめ")
         (body-content "- # 発表内容")
         (body-memo "- # メモ")
         (body-memo-proposal (concat "\t- ## Proposal\n"
                                     (fortee-format-logseq-quote event-proposal 2)))
         (body-neta "- # ネタ"))
    (insert 
     (mapconcat (lambda (str) str)
                `(,frontmatter-type
                  ,frontmatter-author
                  ,frontmatter-published-on
                  ,frontmatter-webpage
                  ,body-presenter
                  ,body-presenter-author
                  ,body-presenter-twitter
                  ,body-summary
                  ,body-content
                  ,body-memo
                  ,body-memo-proposal
                  ,body-neta)
                "\n")
     )))

(defun fortee-format-logseq-quote (str depth)
  (let ((indent (make-string 2 ?\t)))
    (replace-regexp-in-string "^" indent
                              (concat "- #+BEGIN_QUOTE\n"
                                      (replace-regexp-in-string "\n\n+" "\n\n" str)
                                      "\n#+END_QUOTE"))
  ))

;; for debug
; (setq event nil)

(defun fortee--fetch-url (url)
  "Fetch the web page at URL and collect information for event."
  (with-current-buffer (url-retrieve-synchronously url)
    (goto-char (point-min))
    ;; Skip HTTP headers
    (re-search-forward "\n\n")
    (let* ((html (libxml-parse-html-region (point) (point-max)))
           (proposal-div (dom-by-class html "\\`md\\'"))
           (schedule-div (dom-by-class html "\\`schedule"))
           (author-span (dom-child-by-tag (dom-by-class html "\\`speaker") 'span))
           (twitter-a (dom-by-tag (dom-by-class html "\\`speaker") 'a)))
;      (setq event html)
      (if proposal-div
          `(:t ""
               :proposal ,(dom-texts proposal-div "\n")
               :schedule ,(dom-text schedule-div)
               :author   ,(fortee--trim-whitespaces (dom-text author-span))
               :twitter ,(dom-attr twitter-a 'href))
        (message "No div.md found")))))
