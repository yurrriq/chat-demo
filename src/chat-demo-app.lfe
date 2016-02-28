;;; Feel free to use, reuse and abuse the code in this file.

(defmodule chat-demo-app
  (behaviour application)
  ;; API
  (export (start 2) (stop 1)))

;;;===================================================================
;;; API
;;;===================================================================

(defun start (_type _args)
  "Start the application."
  (case (chat-demo-sup:start_link)
    (`#(ok ,pid) `#(ok ,pid))
    (other       `#(error ,other))))

(defun stop (_state)
  "Stop the application."
  'ok)
