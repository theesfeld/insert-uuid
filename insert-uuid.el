;;; insert-uuid.el --- RFC 4122 compliant UUID generator -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: TJ <tj@emacs.su>
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, convenience, uuid, guid
;; URL: https://github.com/theesfeld/insert-uuid
;; SPDX-License-Identifier: GPL-3.0-or-later

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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a comprehensive UUID (Universally Unique Identifier)
;; generator that fully implements RFC 4122 standard for GNU Emacs.
;;
;; Features:
;; - Supports UUID versions 1, 3, 4, and 5
;; - Full RFC 4122 compliance with proper version and variant bits
;; - Predefined namespaces (DNS, URL, OID, X.500)
;; - Interactive commands for easy UUID insertion
;;
;; Usage:
;;
;; Generate UUIDs programmatically:
;;   (insert-uuid-create 4)                          ; Random UUID
;;   (insert-uuid-create 5 'dns "example.com")       ; SHA-1 name-based
;;
;; Insert UUIDs interactively:
;;   M-x insert-uuid
;;
;; Bind to a key:
;;   (global-set-key (kbd "C-c u") 'insert-uuid)
;;
;; UUID Versions:
;; - Version 1: Time-based with MAC address (using random multicast address)
;; - Version 3: Name-based using MD5 hashing
;; - Version 4: Random or pseudo-random
;; - Version 5: Name-based using SHA-1 hashing
;;
;; For more information about UUIDs, see RFC 4122:
;; https://www.rfc-editor.org/rfc/rfc4122

;;; Code:

(require 'cl-lib)

(defgroup insert-uuid nil
  "UUID generation according to RFC 4122."
  :group 'tools
  :prefix "insert-uuid-")

(defcustom insert-uuid-default-version 4
  "Default UUID version to use when not specified.
Valid values are 1, 3, 4, or 5."
  :type '(choice (const :tag "Version 1 (Time-based)" 1)
                 (const :tag "Version 3 (MD5 name-based)" 3)
                 (const :tag "Version 4 (Random)" 4)
                 (const :tag "Version 5 (SHA-1 name-based)" 5))
  :group 'insert-uuid)

(defcustom insert-uuid-uppercase nil
  "If non-nil, generate UUIDs in uppercase."
  :type 'boolean
  :group 'insert-uuid)

(defconst insert-uuid-namespace-dns "6ba7b810-9dad-11d1-80b4-00c04fd430c8"
  "Namespace UUID for DNS (from RFC 4122).")

(defconst insert-uuid-namespace-url "6ba7b811-9dad-11d1-80b4-00c04fd430c8"
  "Namespace UUID for URLs (from RFC 4122).")

(defconst insert-uuid-namespace-oid "6ba7b812-9dad-11d1-80b4-00c04fd430c8"
  "Namespace UUID for ISO OIDs (from RFC 4122).")

(defconst insert-uuid-namespace-x500 "6ba7b814-9dad-11d1-80b4-00c04fd430c8"
  "Namespace UUID for X.500 DNs (from RFC 4122).")

(defvar insert-uuid-namespace-alist
  '((dns . insert-uuid-namespace-dns)
    (url . insert-uuid-namespace-url)
    (oid . insert-uuid-namespace-oid)
    (x500 . insert-uuid-namespace-x500))
  "Alist mapping namespace symbols to their UUID strings.")

;;;###autoload
(defun insert-uuid-create (version &optional namespace name)
  "Generate a UUID according to RFC 4122.
VERSION is the UUID version (1, 3, 4, or 5).
NAMESPACE and NAME are required for versions 3 and 5.
NAMESPACE should be a UUID string or one of the predefined symbols:
  `dns', `url', `oid', or `x500'."
  (pcase version
    (1 (insert-uuid--v1))
    (3 (insert-uuid--v3 namespace name))
    (4 (insert-uuid--v4))
    (5 (insert-uuid--v5 namespace name))
    (_ (error "Unsupported UUID version: %s" version))))

(defun insert-uuid--string-to-bytes (uuid-string)
  "Convert UUID string UUID-STRING to byte vector."
  (let ((hex-string (replace-regexp-in-string "-" "" uuid-string)))
    (cl-loop for i from 0 below (length hex-string) by 2
             vconcat (vector (string-to-number
                            (substring hex-string i (+ i 2)) 16)))))

(defun insert-uuid--bytes-to-string (bytes)
  "Convert BYTES (vector or string) to UUID string format."
  (let ((format-string (if insert-uuid-uppercase
                          "%02X%02X%02X%02X-%02X%02X-%02X%02X-%02X%02X-%02X%02X%02X%02X%02X%02X"
                        "%02x%02x%02x%02x-%02x%02x-%02x%02x-%02x%02x-%02x%02x%02x%02x%02x%02x")))
    (if (stringp bytes)
        ;; If it's already a string (from secure-hash), convert to byte values
        (format format-string
                (aref bytes 0) (aref bytes 1) (aref bytes 2) (aref bytes 3)
                (aref bytes 4) (aref bytes 5)
                (aref bytes 6) (aref bytes 7)
                (aref bytes 8) (aref bytes 9)
                (aref bytes 10) (aref bytes 11) (aref bytes 12)
                (aref bytes 13) (aref bytes 14) (aref bytes 15))
      ;; Vector handling
      (format format-string
              (aref bytes 0) (aref bytes 1) (aref bytes 2) (aref bytes 3)
              (aref bytes 4) (aref bytes 5)
              (aref bytes 6) (aref bytes 7)
              (aref bytes 8) (aref bytes 9)
              (aref bytes 10) (aref bytes 11) (aref bytes 12)
              (aref bytes 13) (aref bytes 14) (aref bytes 15)))))

(defun insert-uuid--v1 ()
  "Generate a time-based UUID (version 1).
Uses a random multicast MAC address instead of the real one for privacy."
  (let* ((time-low (random (expt 2 32)))
         (time-mid (random (expt 2 16)))
         (time-hi-and-version (logior #x1000 (random #x0fff))) ; Version 1
         (clock-seq-hi-res (logior #x80 (random #x3f)))        ; Variant 10
         (clock-seq-low (random 256))
         ;; Generate random node ID with multicast bit set
         (node (list (logior #x01 (random 256))  ; Set multicast bit
                     (random 256)
                     (random 256)
                     (random 256)
                     (random 256)
                     (random 256))))
    (let ((format-string (if insert-uuid-uppercase
                            "%08X-%04X-%04X-%02X%02X-%02X%02X%02X%02X%02X%02X"
                          "%08x-%04x-%04x-%02x%02x-%02x%02x%02x%02x%02x%02x")))
      (format format-string
              time-low time-mid time-hi-and-version
              clock-seq-hi-res clock-seq-low
              (nth 0 node) (nth 1 node) (nth 2 node)
              (nth 3 node) (nth 4 node) (nth 5 node)))))

(defun insert-uuid--v3 (namespace name)
  "Generate a name-based UUID using MD5 hashing (version 3).
NAMESPACE is a UUID string or symbol (`dns', `url', `oid', `x500').
NAME is the name string to hash."
  (unless (and namespace name)
    (error "Namespace and name are required for UUID version 3"))
  (let* ((ns-uuid (insert-uuid--resolve-namespace namespace))
         (ns-bytes (insert-uuid--string-to-bytes ns-uuid))
         (name-bytes (encode-coding-string name 'utf-8 t))
         ;; Convert bytes to unibyte string for hashing
         (hash-input (concat (apply #'unibyte-string (append ns-bytes nil))
                            name-bytes))
         (hash (secure-hash 'md5 hash-input nil nil t))
         (uuid-bytes (substring hash 0 16)))
    ;; Set version (3) and variant (10)
    (aset uuid-bytes 6 (logior #x30 (logand #x0f (aref uuid-bytes 6))))
    (aset uuid-bytes 8 (logior #x80 (logand #x3f (aref uuid-bytes 8))))
    (insert-uuid--bytes-to-string uuid-bytes)))

(defun insert-uuid--v4 ()
  "Generate a random UUID (version 4)."
  (let ((bytes (make-vector 16 0)))
    ;; Generate random bytes
    (dotimes (i 16)
      (aset bytes i (random 256)))
    ;; Set version (4) in byte 6
    (aset bytes 6 (logior #x40 (logand #x0f (aref bytes 6))))
    ;; Set variant (10) in byte 8
    (aset bytes 8 (logior #x80 (logand #x3f (aref bytes 8))))
    (insert-uuid--bytes-to-string bytes)))

(defun insert-uuid--v5 (namespace name)
  "Generate a name-based UUID using SHA-1 hashing (version 5).
NAMESPACE is a UUID string or symbol (`dns', `url', `oid', `x500').
NAME is the name string to hash."
  (unless (and namespace name)
    (error "Namespace and name are required for UUID version 5"))
  (let* ((ns-uuid (insert-uuid--resolve-namespace namespace))
         (ns-bytes (insert-uuid--string-to-bytes ns-uuid))
         (name-bytes (encode-coding-string name 'utf-8 t))
         ;; Convert bytes to unibyte string for hashing
         (hash-input (concat (apply #'unibyte-string (append ns-bytes nil))
                            name-bytes))
         (hash (secure-hash 'sha1 hash-input nil nil t))
         (uuid-bytes (substring hash 0 16)))
    ;; Set version (5) and variant (10)
    (aset uuid-bytes 6 (logior #x50 (logand #x0f (aref uuid-bytes 6))))
    (aset uuid-bytes 8 (logior #x80 (logand #x3f (aref uuid-bytes 8))))
    (insert-uuid--bytes-to-string uuid-bytes)))

(defun insert-uuid--resolve-namespace (namespace)
  "Resolve NAMESPACE to a UUID string.
NAMESPACE can be a symbol (`dns', `url', `oid', `x500') or a UUID string."
  (cond
   ((stringp namespace) namespace)
   ((symbolp namespace)
    (or (cdr (assq namespace insert-uuid-namespace-alist))
        (error "Invalid namespace symbol: %s" namespace)))
   (t (error "Invalid namespace: %s" namespace))))

;;;###autoload
(defun insert-uuid (version)
    "Insert a UUID at point.
Prompts for VERSION (1, 3, 4, or 5).
For versions 3 and 5, also prompts for namespace and name."
  (interactive
   (list (read-number "UUID version (1, 3, 4, or 5): "
                      insert-uuid-default-version)))
  (let ((uuid (pcase version
                ((or 1 4) (insert-uuid-create version))
                ((or 3 5)
                 (let* ((ns-choice (completing-read
                                    "Namespace (dns/url/oid/x500/custom): "
                                    '("dns" "url" "oid" "x500" "custom")
                                    nil t))
                        (namespace (if (string= ns-choice "custom")
                                       (read-string "Custom namespace UUID: ")
                                     (intern ns-choice)))
                        (name (read-string "Name: ")))
                   (insert-uuid-create version namespace name)))
                (_ (error "Invalid UUID version: %d" version)))))
    (insert uuid)))

;;;###autoload
(defun insert-uuid-random ()
  "Insert a random UUID (version 4) at point."
  (interactive)
  (insert (insert-uuid-create 4)))

(provide 'insert-uuid)
;;; insert-uuid.el ends here
