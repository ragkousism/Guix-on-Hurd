;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015, 2016 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 David Thompson <davet@gnu.org>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016 Manolis Fragkiskos Ragkousis <manolis837@gmail.com>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (guix build syscalls-tools)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 match)
  #:use-module (ice-9 ftw)
  #:export (sizeof*
            alignof*
            align
            type-size
            struct-alignment
            struct-size
            write-type
            write-types
            read-type
            define-c-struct))

;;;
;;; Packed structures.
;;;

(define-syntax sizeof*
  ;; XXX: This duplicates 'compile-time-value'.
  (syntax-rules (int128 array)
    ((_ int128)
     16)
    ((_ (array type n))
     (* (sizeof* type) n))
    ((_ type)
     (let-syntax ((v (lambda (s)
                       (let ((val (sizeof type)))
                         (syntax-case s ()
                           (_ val))))))
       v))))

(define-syntax alignof*
  ;; XXX: This duplicates 'compile-time-value'.
  (syntax-rules (int128 array)
    ((_ int128)
     16)
    ((_ (array type n))
     (alignof* type))
    ((_ type)
     (let-syntax ((v (lambda (s)
                       (let ((val (alignof type)))
                         (syntax-case s ()
                           (_ val))))))
       v))))

(define-syntax align                             ;as found in (system foreign)
  (syntax-rules (~)
    "Add to OFFSET whatever it takes to get proper alignment for TYPE."
    ((_ offset (type ~ endianness))
     (align offset type))
    ((_ offset type)
     (1+ (logior (1- offset) (1- (alignof* type)))))))

(define-syntax type-size
  (syntax-rules (~)
    ((_ (type ~ order))
     (sizeof* type))
    ((_ type)
     (sizeof* type))))

(define-syntax struct-alignment
  (syntax-rules ()
    "Compute the alignment for the aggregate made of TYPES at OFFSET.  The
result is the alignment of the \"most strictly aligned component\"."
    ((_ offset types ...)
     (max (align offset types) ...))))

(define-syntax struct-size
  (syntax-rules ()
    "Return the size in bytes of the structure made of TYPES."
    ((_ offset (types-processed ...))
     ;; The SysV ABI P.S. says: "Aggregates (structures and arrays) and unions
     ;; assume the alignment of their most strictly aligned component."  As an
     ;; example, a struct such as "int32, int16" has size 8, not 6.
     (1+ (logior (1- offset)
                 (1- (struct-alignment offset types-processed ...)))))
    ((_ offset (types-processed ...) type0 types ...)
     (struct-size (+ (type-size type0) (align offset type0))
                  (type0 types-processed ...)
                  types ...))))

(define-syntax write-type
  (syntax-rules (~ array)
    ((_ bv offset (type ~ order) value)
     (bytevector-uint-set! bv offset value
                           (endianness order) (sizeof* type)))
    ((_ bv offset (array type n) value)
     (let loop ((i 0)
                (value value)
                (o offset))
       (unless (= i n)
         (match value
           ((head . tail)
            (write-type bv o type head)
            (loop (+ 1 i) tail (+ o (sizeof* type))))))))
    ((_ bv offset type value)
     (bytevector-uint-set! bv offset value
                           (native-endianness) (sizeof* type)))))

(define-syntax write-types
  (syntax-rules ()
    ((_ bv offset () ())
     #t)
    ((_ bv offset (type0 types ...) (field0 fields ...))
     (begin
       (write-type bv (align offset type0) type0 field0)
       (write-types bv
                    (+ (align offset type0) (type-size type0))
                    (types ...) (fields ...))))))

(define-syntax read-type
  (syntax-rules (~ array quote *)
    ((_ bv offset '*)
     (make-pointer (bytevector-uint-ref bv offset
                                        (native-endianness)
                                        (sizeof* '*))))
    ((_ bv offset (type ~ order))
     (bytevector-uint-ref bv offset
                          (endianness order) (sizeof* type)))
    ((_ bv offset (array type n))
     (unfold (lambda (i) (= i n))
             (lambda (i)
               (read-type bv (+ offset (* i (sizeof* type))) type))
             1+
             0))
    ((_ bv offset type)
     (bytevector-uint-ref bv offset
                          (native-endianness) (sizeof* type)))))

(define-syntax read-types
  (syntax-rules ()
    ((_ return bv offset () (values ...))
     (return values ...))
    ((_ return bv offset (type0 types ...) (values ...))
     (read-types return
                 bv
                 (+ (align offset type0) (type-size type0))
                 (types ...)
                 (values ... (read-type bv
                                        (align offset type0)
                                        type0))))))

(define-syntax define-c-struct
  (syntax-rules ()
    "Define SIZE as the size in bytes of the C structure made of FIELDS.  READ
as a deserializer and WRITE! as a serializer for the C structure with the
given TYPES.  READ uses WRAP-FIELDS to return its value."
    ((_ name size wrap-fields read write! (fields types) ...)
     (begin
       (define size
         (struct-size 0 () types ...))
       (define (write! bv offset fields ...)
         (write-types bv offset (types ...) (fields ...)))
       (define* (read bv #:optional (offset 0))
         (read-types wrap-fields bv offset (types ...) ()))))))


