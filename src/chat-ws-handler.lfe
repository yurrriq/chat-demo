;;; Copyright (c) 2011-2014 Robert Virding. All rights reserved.
;;; Copyright (c) 2016      Eric Bailey.    All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 2. Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
;;; FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
;;; COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
;;; BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
;;; CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
;;; ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;; POSSIBILITY OF SUCH DAMAGE.

(defmodule chat-ws-handler
  (behaviour elli_handler)
  (behaviour elli_websocket_handler)
  ;; Elli handler callbacks.
  (export (init 2) (handle 2) (handle_event 3))
  ;; Elli websocket handler callbacks.
  (export (websocket_init 2) (websocket_info 3) (websocket_handle 3)
          (websocket_handle_event 3)))

(include-lib "elli/include/elli.hrl")

;; Internal state.
(defrecord state (opts []))

(defmacro log (format args) `(lfe_io:fwrite 'standard_error ,format ,args))

;;;===================================================================
;;; Elli handler callbacks
;;;===================================================================

(defun init (req args)
  (case (elli_request:get_header #"Upgrade" req)
    (#"websocket" (init-ws (elli_request:path req) req args))
    (_            'ignore)))

(defun handle (req args)
  (let ((method (case (elli_request:get_header #"Upgrade" req)
                  (#"websocket" 'websocket)
                  (_            (elli_request:method req)))))
    (handle method (elli_request:path req) req args)))

(defun handle_event (name event-args elli-args)
  (log "Event: ~p ~p ~p\n" `[,name ,event-args ,elli-args]))

(defun init-ws
  (['(#"websocket") _req _args] #(ok handover))
  ([_path           _req _args] 'ignore))

(defun handle
  (['websocket '(#"websocket")  req  args]
   (log "(elli_websocket:upgrade ~p ~p)\n" `[,req ,args])
   (elli_websocket:upgrade req args)
   #(close #""))
  (['GET       '(#"websocket") _req _args] #(200 [] #"Use an upgrade request"))
  ([_method    _path           _req _args] 'ignore))


;;;===================================================================
;;; Elli websocket handler callbacks
;;;===================================================================

(defun websocket_init (req opts)
  (log "(websocket_init ~p ~p)\n" `[,req ,opts])
  (chat-server:new-user) ; A new user
  `#(ok [] ,(make-state opts opts)))

(defun websocket_info
  ([_req `#(chat-server #(message ,msg)) state]
   (log "(websocket_info _req #(chat-server #(message ~p)) _state)\n" `[,msg])
   `#(reply #(text ,(++ "output ! " msg)) ,state))
  ([_req message state]
   (log "(websocket_info _req ~p _state)\n" `[,message])
   `#(ok ,state)))

(defun websocket_handle
  ([_req `#(text ,data) state]
   (log "(websocket_handle _req #(text ~p) ~p)\n" `[,data ,state])
   (case data
     ((binary "msg ! " (msg binary))
      (chat-server:send-message msg)
      `#(ok ,state))
     ((binary "nick ! " (nick binary))
      (chat-server:set-nick nick)
      `#(ok ,state))
     (_
      `#(reply #(text ,(binary "status ! received '" (data binary) "'"))
               ,state))))
  ([_req other state]
   (log "(websocket_handle _req ~p ~p)\n" `[,other ,state])
   `#(ok ,state)))

(defun websocket_handle_event
  (['websocket_open  `(,_ ,_version ,_compress) _] 'ok)
  (['websocket_close `(,_ ,_reason) _] 'ok)
  (['websocket_throw `(,_request ,_exception ,_stacktrace) _] 'ok)
  (['websocket_error `(,_request ,_exception ,_stacktrace) _] 'ok)
  (['websocket_exit  `(,_request ,_exception ,_stacktrace) _] 'ok))
