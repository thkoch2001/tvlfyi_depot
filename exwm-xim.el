<<<<<<< HEAD   (2eafdb feat(grfn/emacs): More ocaml config)
=======
;;; exwm-xim.el --- XIM Module for EXWM  -*- lexical-binding: t -*-

;; Copyright (C) 2019-2024 Free Software Foundation, Inc.

;; Author: Chris Feng <chris.w.feng@gmail.com>

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This module adds XIM support for EXWM and allows sending characters
;; generated by any Emacs's builtin input method (info node `Input Methods')
;; to X windows.

;; This module is essentially an X input method server utilizing Emacs as
;; its backend.  It talks with X windows through the XIM protocol.  The XIM
;; protocol is quite flexible by itself, stating that an implementation can
;; create network connections of various types as well as make use of an
;; existing X connection for communication, and that an IM server may
;; support multiple transport versions, various input styles and several
;; event flow modals, etc.  Here we only make choices that are most popular
;; among other IM servers and more importantly, practical for Emacs to act
;; as an IM server:
;;
;; + Packets are transported on top of an X connection like most IMEs.
;; + Only transport version 0.0 (i.e. only-CM & Property-with-CM) is
;;   supported (same as "IM Server Developers Kit", adopted by most IMEs).
;; + Only support static event flow, on-demand-synchronous method.
;; + Only "root-window" input style is supported.

;; To use this module, first load and enable it as follows:
;;
;;    (require 'exwm-xim)
;;    (exwm-xim-enable)
;;
;; A keybinding for `toggle-input-method' is probably required to turn on &
;; off an input method (default to `default-input-method').  It's bound to
;; 'C-\' by default and can be made reachable when working with X windows:
;;
;;    (push ?\C-\\ exwm-input-prefix-keys)
;;
;; It's also required (and error-prone) to setup environment variables to
;; make applications actually use this input method.  Typically the
;; following lines should be inserted into '~/.xinitrc'.
;;
;;    export XMODIFIERS=@im=exwm-xim
;;    export GTK_IM_MODULE=xim
;;    export QT_IM_MODULE=xim
;;    export CLUTTER_IM_MODULE=xim

;; References:
;; + XIM (http://www.x.org/releases/X11R7.6/doc/libX11/specs/XIM/xim.html)
;; + IMdkit (http://xorg.freedesktop.org/archive/unsupported/lib/IMdkit/)
;; + UIM (https://github.com/uim/uim)

;;; Code:

(require 'cl-lib)

(require 'xcb-keysyms)
(require 'xcb-xim)

(require 'exwm-core)
(require 'exwm-input)

(defconst exwm-xim--locales
  "@locale=\
aa,af,ak,am,an,anp,ar,as,ast,ayc,az,be,bem,ber,bg,bhb,bho,bn,bo,br,brx,bs,byn,\
ca,ce,cmn,crh,cs,csb,cv,cy,da,de,doi,dv,dz,el,en,es,et,eu,fa,ff,fi,fil,fo,fr,\
fur,fy,ga,gd,gez,gl,gu,gv,ha,hak,he,hi,hne,hr,hsb,ht,hu,hy,ia,id,ig,ik,is,it,\
iu,iw,ja,ka,kk,kl,km,kn,ko,kok,ks,ku,kw,ky,lb,lg,li,li,lij,lo,lt,lv,lzh,mag,\
mai,mg,mhr,mi,mk,ml,mn,mni,mr,ms,mt,my,nan,nb,nds,ne,nhn,niu,nl,nn,nr,nso,oc,\
om,or,os,pa,pa,pap,pl,ps,pt,quz,raj,ro,ru,rw,sa,sat,sc,sd,se,shs,si,sid,sk,sl,\
so,sq,sr,ss,st,sv,sw,szl,ta,tcy,te,tg,th,the,ti,tig,tk,tl,tn,tr,ts,tt,ug,uk,\
unm,ur,uz,ve,vi,wa,wae,wal,wo,xh,yi,yo,yue,zh,zu,\
C,no"
  "All supported locales (stolen from glibc).")

(defconst exwm-xim--default-error
  (make-instance 'xim:error
                 :im-id 0
                 :ic-id 0
                 :flag xim:error-flag:invalid-both
                 :error-code xim:error-code:bad-something
                 :length 0
                 :type 0
                 :detail nil)
  "Default error returned to clients.")

(defconst exwm-xim--default-im-attrs
  (list (make-instance 'xim:XIMATTR
                       :id 0
                       :type xim:ATTRIBUTE-VALUE-TYPE:xim-styles
                       :length (length xlib:XNQueryInputStyle)
                       :attribute xlib:XNQueryInputStyle))
  "Default IM attrs returned to clients.")

(defconst exwm-xim--default-ic-attrs
  (list (make-instance 'xim:XICATTR
                       :id 0
                       :type xim:ATTRIBUTE-VALUE-TYPE:long-data
                       :length (length xlib:XNInputStyle)
                       :attribute xlib:XNInputStyle)
        (make-instance 'xim:XICATTR
                       :id 1
                       :type xim:ATTRIBUTE-VALUE-TYPE:window
                       :length (length xlib:XNClientWindow)
                       :attribute xlib:XNClientWindow)
        ;; Required by e.g. xterm.
        (make-instance 'xim:XICATTR
                       :id 2
                       :type xim:ATTRIBUTE-VALUE-TYPE:window
                       :length (length xlib:XNFocusWindow)
                       :attribute xlib:XNFocusWindow))
  "Default IC attrs returned to clients.")

(defconst exwm-xim--default-styles
  (make-instance 'xim:XIMStyles
                 :number nil
                 :styles (list (logior xlib:XIMPreeditNothing
                                       xlib:XIMStatusNothing)))
  "Default styles: root-window, i.e. no preediting or status display support.")

(defconst exwm-xim--default-attributes
  (list (make-instance 'xim:XIMATTRIBUTE
                       :id 0
                       :length nil
                       :value exwm-xim--default-styles))
  "Default IM/IC attributes returned to clients.")

(defvar exwm-xim--conn nil
  "The X connection for initiating other XIM connections.")
(defvar exwm-xim--event-xwin nil
  "X window for initiating new XIM connections.")
(defvar exwm-xim--server-client-plist '(nil nil)
  "Plist mapping server window to [X connection, client window, byte-order].")
(defvar exwm-xim--client-server-plist '(nil nil)
  "Plist mapping client window to server window.")
(defvar exwm-xim--property-index 0 "For generating a unique property name.")
(defvar exwm-xim--im-id 0 "Last IM ID.")
(defvar exwm-xim--ic-id 0 "Last IC ID.")

;; X11 atoms.
(defvar exwm-xim--@server nil)
(defvar exwm-xim--LOCALES nil)
(defvar exwm-xim--TRANSPORT nil)
(defvar exwm-xim--XIM_SERVERS nil)
(defvar exwm-xim--_XIM_PROTOCOL nil)
(defvar exwm-xim--_XIM_XCONNECT nil)

(defvar exwm-xim-buffer-p nil
  "Whether current buffer is used by exwm-xim.")
(make-variable-buffer-local 'exwm-xim-buffer-p)

(defun exwm-xim--on-SelectionRequest (data _synthetic)
  "Handle SelectionRequest events on IMS window.
DATA contains unmarshalled SelectionRequest event data.

Such events would be received when clients query for LOCALES or TRANSPORT."
  (exwm--log)
  (let ((evt (make-instance 'xcb:SelectionRequest))
        value fake-event)
    (xcb:unmarshal evt data)
    (with-slots (time requestor selection target property) evt
      (setq value (cond ((= target exwm-xim--LOCALES)
                         ;; Return supported locales.
                         exwm-xim--locales)
                        ((= target exwm-xim--TRANSPORT)
                         ;; Use XIM over an X connection.
                         "@transport=X/")))
      (when value
        ;; Change the property.
        (xcb:+request exwm-xim--conn
            (make-instance 'xcb:ChangeProperty
                           :mode xcb:PropMode:Replace
                           :window requestor
                           :property property
                           :type target
                           :format 8
                           :data-len (length value)
                           :data value))
        ;; Send a SelectionNotify event.
        (setq fake-event (make-instance 'xcb:SelectionNotify
                                        :time time
                                        :requestor requestor
                                        :selection selection
                                        :target target
                                        :property property))
        (xcb:+request exwm-xim--conn
            (make-instance 'xcb:SendEvent
                           :propagate 0
                           :destination requestor
                           :event-mask xcb:EventMask:NoEvent
                           :event (xcb:marshal fake-event exwm-xim--conn)))
        (xcb:flush exwm-xim--conn)))))

(cl-defun exwm-xim--on-ClientMessage-0 (data _synthetic)
  "Handle ClientMessage event on IMS window (new connection).

Such events would be received when clients request for _XIM_XCONNECT.
A new X connection and server window would be created to communicate with
this client."
  (exwm--log)
  (let ((evt (make-instance 'xcb:ClientMessage))
        conn client-xwin server-xwin)
    (xcb:unmarshal evt data)
    (with-slots (window type data) evt
      (unless (= type exwm-xim--_XIM_XCONNECT)
        ;; Only handle _XIM_XCONNECT.
        (exwm--log "Ignore ClientMessage %s" type)
        (cl-return-from exwm-xim--on-ClientMessage-0))
      (setq client-xwin (elt (slot-value data 'data32) 0)
            ;; Create a new X connection and a new server window.
            conn (xcb:connect)
            server-xwin (xcb:generate-id conn))
      (set-process-query-on-exit-flag (slot-value conn 'process) nil)
      ;; Store this client.
      (plist-put exwm-xim--server-client-plist server-xwin
                 `[,conn ,client-xwin nil])
      (plist-put exwm-xim--client-server-plist client-xwin server-xwin)
      ;; Select DestroyNotify events on this client window.
      (xcb:+request exwm-xim--conn
          (make-instance 'xcb:ChangeWindowAttributes
                         :window client-xwin
                         :value-mask xcb:CW:EventMask
                         :event-mask xcb:EventMask:StructureNotify))
      (xcb:flush exwm-xim--conn)
      ;; Handle ClientMessage events from this new connection.
      (xcb:+event conn 'xcb:ClientMessage #'exwm-xim--on-ClientMessage)
      ;; Create a communication window.
      (xcb:+request conn
          (make-instance 'xcb:CreateWindow
                         :depth 0
                         :wid server-xwin
                         :parent exwm--root
                         :x 0
                         :y 0
                         :width 1
                         :height 1
                         :border-width 0
                         :class xcb:WindowClass:InputOutput
                         :visual 0
                         :value-mask xcb:CW:OverrideRedirect
                         :override-redirect 1))
      (xcb:flush conn)
      ;; Send connection establishment ClientMessage.
      (setf window client-xwin
            (slot-value data 'data32) `(,server-xwin 0 0 0 0))
      (slot-makeunbound data 'data8)
      (slot-makeunbound data 'data16)
      (xcb:+request exwm-xim--conn
          (make-instance 'xcb:SendEvent
                         :propagate 0
                         :destination client-xwin
                         :event-mask xcb:EventMask:NoEvent
                         :event (xcb:marshal evt exwm-xim--conn)))
      (xcb:flush exwm-xim--conn))))

(cl-defun exwm-xim--on-ClientMessage (data _synthetic)
  "Handle ClientMessage event on IMS communication window (request).

Such events would be received when clients request for _XIM_PROTOCOL.
The actual XIM request is in client message data or a property."
  (exwm--log)
  (let ((evt (make-instance 'xcb:ClientMessage))
        conn client-xwin server-xwin)
    (xcb:unmarshal evt data)
    (with-slots (format window type data) evt
      (unless (= type exwm-xim--_XIM_PROTOCOL)
        (exwm--log "Ignore ClientMessage %s" type)
        (cl-return-from exwm-xim--on-ClientMessage))
      (setq server-xwin window
            conn (plist-get exwm-xim--server-client-plist server-xwin)
            client-xwin (elt conn 1)
            conn (elt conn 0))
      (cond ((= format 8)
             ;; Data.
             (exwm-xim--on-request (vconcat (slot-value data 'data8))
                                   conn client-xwin server-xwin))
            ((= format 32)
             ;; Atom.
             (with-slots (data32) data
               (with-slots (value)
                   (xcb:+request-unchecked+reply conn
                       (make-instance 'xcb:GetProperty
                                      :delete 1
                                      :window server-xwin
                                      :property (elt data32 1)
                                      :type xcb:GetPropertyType:Any
                                      :long-offset 0
                                      :long-length (elt data32 0)))
                 (when (> (length value) 0)
                   (exwm-xim--on-request value conn client-xwin
                                         server-xwin)))))))))

(defun exwm-xim--on-request (data conn client-xwin server-xwin)
  "Handle an XIM reuqest."
  (exwm--log)
  (let ((opcode (elt data 0))
        ;; Let-bind `xim:lsb' to make pack/unpack functions work correctly.
        (xim:lsb (elt (plist-get exwm-xim--server-client-plist server-xwin) 2))
        req replies)
    (cond ((= opcode xim:opcode:error)
           (exwm--log "ERROR: %s" data))
          ((= opcode xim:opcode:connect)
           (exwm--log "CONNECT")
           (setq xim:lsb (= (elt data 4) xim:connect-byte-order:lsb-first))
           ;; Store byte-order.
           (setf (elt (plist-get exwm-xim--server-client-plist server-xwin) 2)
                 xim:lsb)
           (setq req (make-instance 'xim:connect))
           (xcb:unmarshal req data)
           (if (and (= (slot-value req 'major-version) 1)
                    (= (slot-value req 'minor-version) 0)
                    ;; Do not support authentication.
                    (= (slot-value req 'number) 0))
               ;; Accept the connection.
               (push (make-instance 'xim:connect-reply) replies)
             ;; Deny it.
             (push exwm-xim--default-error replies)))
          ((memq opcode (list xim:opcode:auth-required
                              xim:opcode:auth-reply
                              xim:opcode:auth-next
                              xim:opcode:auth-ng))
           (exwm--log "AUTH: %d" opcode)
           ;; Deny any attempt to make authentication.
           (push exwm-xim--default-error replies))
          ((= opcode xim:opcode:disconnect)
           (exwm--log "DISCONNECT")
           ;; Gracefully disconnect from the client.
           (exwm-xim--make-request (make-instance 'xim:disconnect-reply)
                                   conn client-xwin)
           ;; Destroy the communication window & connection.
           (xcb:+request conn
               (make-instance 'xcb:DestroyWindow
                              :window server-xwin))
           (xcb:disconnect conn)
           ;; Clean up cache.
           (cl-remf exwm-xim--server-client-plist server-xwin)
           (cl-remf exwm-xim--client-server-plist client-xwin))
          ((= opcode xim:opcode:open)
           (exwm--log "OPEN")
           ;; Note: We make no check here.
           (setq exwm-xim--im-id (if (< exwm-xim--im-id #xffff)
                                     (1+ exwm-xim--im-id)
                                   1))
           (setq replies
                 (list
                  (make-instance 'xim:open-reply
                                 :im-id exwm-xim--im-id
                                 :im-attrs-length nil
                                 :im-attrs exwm-xim--default-im-attrs
                                 :ic-attrs-length nil
                                 :ic-attrs exwm-xim--default-ic-attrs)
                  (make-instance 'xim:set-event-mask
                                 :im-id exwm-xim--im-id
                                 :ic-id 0
                                 ;; Static event flow.
                                 :forward-event-mask xcb:EventMask:KeyPress
                                 ;; on-demand-synchronous method.
                                 :synchronous-event-mask
                                 xcb:EventMask:NoEvent))))
          ((= opcode xim:opcode:close)
           (exwm--log "CLOSE")
           (setq req (make-instance 'xim:close))
           (xcb:unmarshal req data)
           (push (make-instance 'xim:close-reply
                                :im-id (slot-value req 'im-id))
                 replies))
          ((= opcode xim:opcode:trigger-notify)
           (exwm--log "TRIGGER-NOTIFY")
           ;; Only static event flow modal is supported.
           (push exwm-xim--default-error replies))
          ((= opcode xim:opcode:encoding-negotiation)
           (exwm--log "ENCODING-NEGOTIATION")
           (setq req (make-instance 'xim:encoding-negotiation))
           (xcb:unmarshal req data)
           (let ((index (cl-position "COMPOUND_TEXT"
                                     (mapcar (lambda (i) (slot-value i 'name))
                                             (slot-value req 'names))
                                     :test #'equal)))
             (unless index
               ;; Fallback to portable character encoding (a subset of ASCII).
               (setq index -1))
             (push (make-instance 'xim:encoding-negotiation-reply
                                  :im-id (slot-value req 'im-id)
                                  :category
                                  xim:encoding-negotiation-reply-category:name
                                  :index index)
                   replies)))
          ((= opcode xim:opcode:query-extension)
           (exwm--log "QUERY-EXTENSION")
           (setq req (make-instance 'xim:query-extension))
           (xcb:unmarshal req data)
           (push (make-instance 'xim:query-extension-reply
                                :im-id (slot-value req 'im-id)
                                ;; No extension support.
                                :length 0
                                :extensions nil)
                 replies))
          ((= opcode xim:opcode:set-im-values)
           (exwm--log "SET-IM-VALUES")
           ;; There's only one possible input method attribute.
           (setq req (make-instance 'xim:set-im-values))
           (xcb:unmarshal req data)
           (push (make-instance 'xim:set-im-values-reply
                                :im-id (slot-value req 'im-id))
                 replies))
          ((= opcode xim:opcode:get-im-values)
           (exwm--log "GET-IM-VALUES")
           (setq req (make-instance 'xim:get-im-values))
           (let (im-attributes-id)
             (xcb:unmarshal req data)
             (setq im-attributes-id (slot-value req 'im-attributes-id))
             (if (cl-notevery (lambda (i) (= i 0)) im-attributes-id)
                 ;; Only support one IM attributes.
                 (push (make-instance 'xim:error
                                      :im-id (slot-value req 'im-id)
                                      :ic-id 0
                                      :flag xim:error-flag:invalid-ic-id
                                      :error-code xim:error-code:bad-something
                                      :length 0
                                      :type 0
                                      :detail nil)
                       replies)
               (push
                (make-instance 'xim:get-im-values-reply
                               :im-id (slot-value req 'im-id)
                               :length nil
                               :im-attributes exwm-xim--default-attributes)
                replies))))
          ((= opcode xim:opcode:create-ic)
           (exwm--log "CREATE-IC")
           (setq req (make-instance 'xim:create-ic))
           (xcb:unmarshal req data)
           ;; Note: The ic-attributes slot is ignored.
           (setq exwm-xim--ic-id (if (< exwm-xim--ic-id #xffff)
                                     (1+ exwm-xim--ic-id)
                                   1))
           (push (make-instance 'xim:create-ic-reply
                                :im-id (slot-value req 'im-id)
                                :ic-id exwm-xim--ic-id)
                 replies))
          ((= opcode xim:opcode:destroy-ic)
           (exwm--log "DESTROY-IC")
           (setq req (make-instance 'xim:destroy-ic))
           (xcb:unmarshal req data)
           (push (make-instance 'xim:destroy-ic-reply
                                :im-id (slot-value req 'im-id)
                                :ic-id (slot-value req 'ic-id))
                 replies))
          ((= opcode xim:opcode:set-ic-values)
           (exwm--log "SET-IC-VALUES")
           (setq req (make-instance 'xim:set-ic-values))
           (xcb:unmarshal req data)
           ;; We don't distinguish between input contexts.
           (push (make-instance 'xim:set-ic-values-reply
                                :im-id (slot-value req 'im-id)
                                :ic-id (slot-value req 'ic-id))
                 replies))
          ((= opcode xim:opcode:get-ic-values)
           (exwm--log "GET-IC-VALUES")
           (setq req (make-instance 'xim:get-ic-values))
           (xcb:unmarshal req data)
           (push (make-instance 'xim:get-ic-values-reply
                                :im-id (slot-value req 'im-id)
                                :ic-id (slot-value req 'ic-id)
                                :length nil
                                :ic-attributes exwm-xim--default-attributes)
                 replies))
          ((= opcode xim:opcode:set-ic-focus)
           (exwm--log "SET-IC-FOCUS")
           ;; All input contexts are the same.
           )
          ((= opcode xim:opcode:unset-ic-focus)
           (exwm--log "UNSET-IC-FOCUS")
           ;; All input contexts are the same.
           )
          ((= opcode xim:opcode:forward-event)
           (exwm--log "FORWARD-EVENT")
           (setq req (make-instance 'xim:forward-event))
           (xcb:unmarshal req data)
           (exwm-xim--handle-forward-event-request req xim:lsb conn
                                                   client-xwin))
          ((= opcode xim:opcode:sync)
           (exwm--log "SYNC")
           (setq req (make-instance 'xim:sync))
           (xcb:unmarshal req data)
           (push (make-instance 'xim:sync-reply
                                :im-id (slot-value req 'im-id)
                                :ic-id (slot-value req 'ic-id))
                 replies))
          ((= opcode xim:opcode:sync-reply)
           (exwm--log "SYNC-REPLY"))
          ((= opcode xim:opcode:reset-ic)
           (exwm--log "RESET-IC")
           ;; No context-specific data saved.
           (setq req (make-instance 'xim:reset-ic))
           (xcb:unmarshal req data)
           (push (make-instance 'xim:reset-ic-reply
                                :im-id (slot-value req 'im-id)
                                :ic-id (slot-value req 'ic-id)
                                :length 0
                                :string "")
                 replies))
          ((memq opcode (list xim:opcode:str-conversion-reply
                              xim:opcode:preedit-start-reply
                              xim:opcode:preedit-caret-reply))
           (exwm--log "PREEDIT: %d" opcode)
           ;; No preedit support.
           (push exwm-xim--default-error replies))
          (t
           (exwm--log "Bad protocol")
           (push exwm-xim--default-error replies)))
    ;; Actually send the replies.
    (when replies
      (mapc (lambda (reply)
              (exwm-xim--make-request reply conn client-xwin))
            replies)
      (xcb:flush conn))))

(defun exwm-xim--handle-forward-event-request (req lsb conn client-xwin)
  (let ((im-func (with-current-buffer (window-buffer)
                   input-method-function))
        key-event keysym keysyms event result)
    ;; Note: The flag slot is ignored.
    ;; Do conversion in client's byte-order.
    (let ((xcb:lsb lsb))
      (setq key-event (make-instance 'xcb:KeyPress))
      (xcb:unmarshal key-event (slot-value req 'event)))
    (with-slots (detail state) key-event
      (setq keysym (xcb:keysyms:keycode->keysym exwm-xim--conn detail
                                                state))
      (when (/= (car keysym) 0)
        (setq event (xcb:keysyms:keysym->event
                     exwm-xim--conn
                     (car keysym)
                     (logand state (lognot (cdr keysym)))))))
    (while (or (slot-value req 'event) unread-command-events)
      (unless (slot-value req 'event)
        (setq event (pop unread-command-events))
        ;; Handle events in (t . EVENT) format.
        (when (and (consp event)
                   (eq (car event) t))
          (setq event (cdr event))))
      (if (or (not im-func)
              ;; `list' is the default method.
              (eq im-func #'list)
              (not event)
              ;; Select only printable keys.
              (not (integerp event)) (> #x20 event) (< #x7e event))
          ;; Either there is no active input method, or invalid key
          ;; is detected.
          (with-slots ((raw-event event)
                       im-id ic-id serial-number)
              req
            (if raw-event
                (setq event raw-event)
              (setq keysyms (xcb:keysyms:event->keysyms exwm-xim--conn event))
              (with-slots (detail state) key-event
                (setf detail (xcb:keysyms:keysym->keycode exwm-xim--conn
                                                          (caar keysyms))
                      state (cdar keysyms)))
              (setq event (let ((xcb:lsb lsb))
                            (xcb:marshal key-event conn))))
            (when event
              (exwm-xim--make-request
               (make-instance 'xim:forward-event
                              :im-id im-id
                              :ic-id ic-id
                              :flag xim:commit-flag:synchronous
                              :serial-number serial-number
                              :event event)
               conn client-xwin)))
        (when (eq exwm--selected-input-mode 'char-mode)
          ;; Grab keyboard temporarily for char-mode.
          (exwm-input--grab-keyboard))
        (unwind-protect
            (with-temp-buffer
              ;; This variable is used to test whether exwm-xim is enabled.
              ;; Used by e.g. pyim-probe.
              (setq-local exwm-xim-buffer-p t)
              ;; Always show key strokes.
              (let ((input-method-use-echo-area t)
                    (exwm-input-line-mode-passthrough t))
                (setq result (funcall im-func event))
                ;; Clear echo area for the input method.
                (message nil)
                ;; This also works for portable character encoding.
                (setq result
                      (encode-coding-string (concat result)
                                            'compound-text-with-extensions))
                (exwm-xim--make-request
                 (make-instance 'xim:commit-x-lookup-chars
                                :im-id (slot-value req 'im-id)
                                :ic-id (slot-value req 'ic-id)
                                :flag (logior xim:commit-flag:synchronous
                                              xim:commit-flag:x-lookup-chars)
                                :length (length result)
                                :string result)
                 conn client-xwin)))
          (when (eq exwm--selected-input-mode 'char-mode)
            (exwm-input--release-keyboard))))
      (xcb:flush conn)
      (setf event nil
            (slot-value req 'event) nil))))

(defun exwm-xim--make-request (req conn client-xwin)
  "Make an XIM request REQ via connection CONN.

CLIENT-XWIN would receive a ClientMessage event either telling the client
the request data or where to fetch the data."
  (exwm--log)
  (let ((data (xcb:marshal req))
        property format client-message-data client-message)
    (if (<= (length data) 20)
        ;; Send short requests directly with client messages.
        (setq format 8
              ;; Pad to 20 bytes.
              data (append data (make-list (- 20 (length data)) 0))
              client-message-data (make-instance 'xcb:ClientMessageData
                                                 :data8 data))
      ;; Send long requests with properties.
      (setq property (exwm--intern-atom (format "_EXWM_XIM_%x"
                                                exwm-xim--property-index)))
      (cl-incf exwm-xim--property-index)
      (xcb:+request conn
          (make-instance 'xcb:ChangeProperty
                         :mode xcb:PropMode:Append
                         :window client-xwin
                         :property property
                         :type xcb:Atom:STRING
                         :format 8
                         :data-len (length data)
                         :data data))
      ;; Also send a client message to notify the client about this property.
      (setq format 32
            client-message-data (make-instance 'xcb:ClientMessageData
                                               :data32 `(,(length data)
                                                         ,property
                                                         ;; Pad to 20 bytes.
                                                         0 0 0))))
    ;; Send the client message.
    (setq client-message (make-instance 'xcb:ClientMessage
                                        :format format
                                        :window client-xwin
                                        :type exwm-xim--_XIM_PROTOCOL
                                        :data client-message-data))
    (xcb:+request conn
        (make-instance 'xcb:SendEvent
                       :propagate 0
                       :destination client-xwin
                       :event-mask xcb:EventMask:NoEvent
                       :event (xcb:marshal client-message conn)))))

(defun exwm-xim--on-DestroyNotify (data synthetic)
  "Do cleanups on receiving DestroyNotify event.

Such event would be received when the client window is destroyed."
  (exwm--log)
  (unless synthetic
    (let ((evt (make-instance 'xcb:DestroyNotify))
          conn client-xwin server-xwin)
      (xcb:unmarshal evt data)
      (setq client-xwin (slot-value evt 'window)
            server-xwin (plist-get exwm-xim--client-server-plist client-xwin))
      (when server-xwin
        (setq conn (aref (plist-get exwm-xim--server-client-plist server-xwin)
                         0))
        (cl-remf exwm-xim--server-client-plist server-xwin)
        (cl-remf exwm-xim--client-server-plist client-xwin)
        ;; Destroy the communication window & connection.
        (xcb:+request conn
            (make-instance 'xcb:DestroyWindow
                           :window server-xwin))
        (xcb:disconnect conn)))))

(cl-defun exwm-xim--init ()
  "Initialize the XIM module."
  (exwm--log)
  (when exwm-xim--conn
    (cl-return-from exwm-xim--init))
  ;; Initialize atoms.
  (setq exwm-xim--@server (exwm--intern-atom "@server=exwm-xim")
        exwm-xim--LOCALES (exwm--intern-atom "LOCALES")
        exwm-xim--TRANSPORT (exwm--intern-atom "TRANSPORT")
        exwm-xim--XIM_SERVERS (exwm--intern-atom "XIM_SERVERS")
        exwm-xim--_XIM_PROTOCOL (exwm--intern-atom "_XIM_PROTOCOL")
        exwm-xim--_XIM_XCONNECT (exwm--intern-atom "_XIM_XCONNECT"))
  ;; Create a new connection and event window.
  (setq exwm-xim--conn (xcb:connect)
        exwm-xim--event-xwin (xcb:generate-id exwm-xim--conn))
  (set-process-query-on-exit-flag (slot-value exwm-xim--conn 'process) nil)
  ;; Initialize xcb:keysyms module.
  (xcb:keysyms:init exwm-xim--conn)
  ;; Listen to SelectionRequest event for connection establishment.
  (xcb:+event exwm-xim--conn 'xcb:SelectionRequest
              #'exwm-xim--on-SelectionRequest)
  ;; Listen to ClientMessage event on IMS window for new XIM connection.
  (xcb:+event exwm-xim--conn 'xcb:ClientMessage #'exwm-xim--on-ClientMessage-0)
  ;; Listen to DestroyNotify event to do cleanups.
  (xcb:+event exwm-xim--conn 'xcb:DestroyNotify #'exwm-xim--on-DestroyNotify)
  ;; Create the event window.
  (xcb:+request exwm-xim--conn
      (make-instance 'xcb:CreateWindow
                     :depth 0
                     :wid exwm-xim--event-xwin
                     :parent exwm--root
                     :x 0
                     :y 0
                     :width 1
                     :height 1
                     :border-width 0
                     :class xcb:WindowClass:InputOutput
                     :visual 0
                     :value-mask xcb:CW:OverrideRedirect
                     :override-redirect 1))
  ;; Set the selection owner.
  (xcb:+request exwm-xim--conn
      (make-instance 'xcb:SetSelectionOwner
                     :owner exwm-xim--event-xwin
                     :selection exwm-xim--@server
                     :time xcb:Time:CurrentTime))
  ;; Set XIM_SERVERS property on the root window.
  (xcb:+request exwm-xim--conn
      (make-instance 'xcb:ChangeProperty
                     :mode xcb:PropMode:Prepend
                     :window exwm--root
                     :property exwm-xim--XIM_SERVERS
                     :type xcb:Atom:ATOM
                     :format 32
                     :data-len 1
                     :data (funcall (if xcb:lsb
                                        #'xcb:-pack-u4-lsb
                                      #'xcb:-pack-u4)
                                    exwm-xim--@server)))
  (xcb:flush exwm-xim--conn))

(cl-defun exwm-xim--exit ()
  "Exit the XIM module."
  (exwm--log)
  ;; Close IMS communication connections.
  (mapc (lambda (i)
          (when (vectorp i)
            (when (slot-value (elt i 0) 'connected)
              (xcb:disconnect (elt i 0)))))
        exwm-xim--server-client-plist)
  ;; Close the IMS connection.
  (unless (and exwm-xim--conn
               (slot-value exwm-xim--conn 'connected))
    (cl-return-from exwm-xim--exit))
  ;; Remove exwm-xim from XIM_SERVERS.
  (let ((reply (xcb:+request-unchecked+reply exwm-xim--conn
                   (make-instance 'xcb:GetProperty
                                  :delete 1
                                  :window exwm--root
                                  :property exwm-xim--XIM_SERVERS
                                  :type xcb:Atom:ATOM
                                  :long-offset 0
                                  :long-length 1000)))
        unpacked-reply pack unpack)
    (unless reply
      (cl-return-from exwm-xim--exit))
    (setq reply (slot-value reply 'value))
    (unless (> (length reply) 4)
      (cl-return-from exwm-xim--exit))
    (setq reply (vconcat reply)
          pack (if xcb:lsb #'xcb:-pack-u4-lsb #'xcb:-pack-u4)
          unpack (if xcb:lsb #'xcb:-unpack-u4-lsb #'xcb:-unpack-u4))
    (dotimes (i (/ (length reply) 4))
      (push (funcall unpack reply (* i 4)) unpacked-reply))
    (setq unpacked-reply (delq exwm-xim--@server unpacked-reply)
          reply (mapcar pack unpacked-reply))
    (xcb:+request exwm-xim--conn
        (make-instance 'xcb:ChangeProperty
                       :mode xcb:PropMode:Replace
                       :window exwm--root
                       :property exwm-xim--XIM_SERVERS
                       :type xcb:Atom:ATOM
                       :format 32
                       :data-len (length reply)
                       :data reply))
    (xcb:flush exwm-xim--conn))
  (xcb:disconnect exwm-xim--conn)
  (setq exwm-xim--conn nil))

(defun exwm-xim-enable ()
  "Enable XIM support for EXWM."
  (exwm--log)
  (add-hook 'exwm-init-hook #'exwm-xim--init)
  (add-hook 'exwm-exit-hook #'exwm-xim--exit))



(provide 'exwm-xim)

;;; exwm-xim.el ends here
>>>>>>> BRANCH (a6e66f Simplify and improve focus handling (#10))
