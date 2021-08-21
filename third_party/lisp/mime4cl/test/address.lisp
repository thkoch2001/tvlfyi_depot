;;;  address.lisp --- tests for the e-mail address parser

;;;  Copyright (C) 2007, 2009 by Walter C. Pelissero

;;;  Author: Walter C. Pelissero <walter@pelissero.de>
;;;  Project: mime4cl

#+cmu (ext:file-comment "$Module: address.lisp $")

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public License
;;; as published by the Free Software Foundation; either version 2.1
;;; of the License, or (at your option) any later version.
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this library; if not, write to the Free
;;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
;;; 02111-1307 USA

(in-package :mime4cl-tests)

(defun test-parsing (string)
  (format nil "~{~A~^, ~}" (parse-addresses string)))

(deftest address-parse-simple.1
    (test-parsing "foo@bar")
  "foo@bar")

(deftest address-parse-simple.2
    (test-parsing "foo@bar.com")
  "foo@bar.com")

(deftest address-parse-simple.3
    (test-parsing "foo@bar.baz.com")
  "foo@bar.baz.com")

(deftest address-parse-simple.4
    (test-parsing "foo.ooo@bar.baz.com")
  "foo.ooo@bar.baz.com")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest address-parse-simple-commented.1
    (test-parsing "foo@bar (Some Comment)")
  "\"Some Comment\" <foo@bar>")

(deftest address-parse-simple-commented.2
    (test-parsing "foo@bar (Some, Comment)")
  "\"Some, Comment\" <foo@bar>")

(deftest address-parse-simple-commented.3
    (test-parsing "foo@bar (Some Comment (yes, indeed))")
  "\"Some Comment (yes, indeed)\" <foo@bar>")

(deftest address-parse-simple-commented.4
    (test-parsing "foo.bar@host.complicated.domain.net (Some Comment (yes, indeed))")
  "\"Some Comment (yes, indeed)\" <foo.bar@host.complicated.domain.net>")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest address-parse-angle.1
    (test-parsing "<foo@bar.baz.net>")
  "foo@bar.baz.net")

(deftest address-parse-angle.2
    (test-parsing "My far far friend <foo@bar.baz.net>")
  "\"My far far friend\" <foo@bar.baz.net>")

(deftest address-parse-angle.3
    (test-parsing "\"someone, I don't like\" <foo@bar.baz.net>")
  "\"someone, I don't like\" <foo@bar.baz.net>")

(deftest address-parse-angle.4
    (test-parsing "\"this could (be a comment)\" <foo@bar.net>")
  "\"this could (be a comment)\" <foo@bar.net>")

(deftest address-parse-angle.5
    (test-parsing "don't be fooled <foo@bar.net>")
  "\"don't be fooled\" <foo@bar.net>")

(deftest address-parse-angle.6
    (test-parsing "<foo@bar>")
  "foo@bar")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest address-parse-domain-literal.1
    (test-parsing "<foo@[bar]>")
  "foo@[bar]")

(deftest address-parse-domain-literal.2
    (test-parsing "<foo@[bar.net]>")
  "foo@[bar.net]")

(deftest address-parse-domain-literal.3
    (test-parsing "<foo@[10.0.0.2]>")
  "foo@[10.0.0.2]")

(deftest address-parse-domain-literal.4
    (test-parsing "<foo.bar@[10.0.0.2]>")
  "foo.bar@[10.0.0.2]")

(deftest address-parse-domain-literal.5
    (test-parsing "somewhere unkown <foo.bar@[10.0.0.2]>")
  "\"somewhere unkown\" <foo.bar@[10.0.0.2]>")

(deftest address-parse-domain-literal.6
    (test-parsing "\"Some--One\" <foo.bar@[10.0.0.23]>")
  "\"Some--One\" <foo.bar@[10.0.0.23]>")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest address-parse-group.1
    (test-parsing "friends:john@bar.in.soho, jack@pub.round.the.corner, jim@[10.0.1.2];")
  "friends: john@bar.in.soho, jack@pub.round.the.corner, jim@[10.0.1.2];")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest address-parse-mixed.1
    (test-parsing "Foo BAR <foo@bar.com>, \"John, Smith (that one!)\" <john.smith@host.domain.org>, friends:john@bar,jack@pub;, foo.bar.baz@wow.mail.mine, dont.bark@me (Fierce Dog)")
  "\"Foo BAR\" <foo@bar.com>, \"John, Smith (that one!)\" <john.smith@host.domain.org>, friends: john@bar, jack@pub;, foo.bar.baz@wow.mail.mine, \"Fierce Dog\" <dont.bark@me>")
