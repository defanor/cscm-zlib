;; Copyright (C) 2011 by Joseph Gay
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA
;;
;; Author: Joseph Gay, <gilleylen [at] gmail [dot] com>

;; Bindings for zlib, see http://www.zlib.net for zlib docs

;; zlib: Copyright (C) 1995-2010 Jean-loup Gailly and Mark Adler
;; see http://zlib.net/zlib_license.html

(module zlib

(open-zlib-compressed-input-port
 open-zlib-compressed-output-port)

(import scheme chicken foreign)

(use extras foreigners lolevel miscmacros ports)

#>
#include <zlib.h>
<#

;; if this is set too high, it will segfault
(define chunk #x10000)

(define-foreign-variable Z_OK int)
(define-foreign-variable Z_NULL int)
(define-foreign-variable Z_STREAM_END int)
(define-foreign-variable Z_NO_FLUSH int)
(define-foreign-variable Z_STREAM_ERROR int)
(define-foreign-variable Z_NEED_DICT int)
(define-foreign-variable Z_DATA_ERROR int)
(define-foreign-variable Z_MEM_ERROR int)
(define-foreign-variable Z_FINISH int)
(define-foreign-variable Z_NO_COMPRESSION int)
(define-foreign-variable Z_BEST_SPEED int)
(define-foreign-variable Z_BEST_COMPRESSION int)
(define-foreign-variable Z_DEFAULT_COMPRESSION int)

(define-foreign-record-type (z-stream "z_stream")
  (constructor: make-z-stream)
  (destructor: free-z-stream)
  ((c-pointer char) next_in z-stream-next-in z-stream-next-in-set!)
  (unsigned-integer avail_in z-stream-avail-in z-stream-avail-in-set!)
  (unsigned-long total_in z-stream-total-in)
  ((c-pointer char) next_out z-stream-next-out z-stream-next-out-set!)
  (unsigned-integer avail_out z-stream-avail-out z-stream-avail-out-set!)
  (unsigned-long total_out z-stream-total-out)
  (c-string msg z-stream-msg)
  ((c-pointer (struct "internal_state")) state state)
  (c-pointer zalloc z-alloc z-stream-z-alloc-set!)
  (c-pointer zfree z-free z-stream-z-free-set!)
  (c-pointer opaque z-stream-opaque z-stream-opaque-set!)
  (integer data_type z-stream-data-type)
  (unsigned-long adler z-stream-adler)
  (unsigned-long reserved reserved))

(define inflate-init (foreign-lambda int "inflateInit" z-stream))
(define inflate (foreign-lambda int "inflate" z-stream int))
(define inflate-end (foreign-lambda void "inflateEnd" z-stream))

(define (z-abort type)
  (abort (make-property-condition 'z-error 'type type)))

(define (open-zlib-compressed-input-port port)
  (let ((ret #f)
        (stream (make-z-stream))
        (in #f)
        (out (make-string chunk))
        (bytes-avail "")
        (pos 0)
        (eof? #f))
    (z-stream-z-alloc-set! stream #f)
    (z-stream-z-free-set! stream #f)
    (z-stream-opaque-set! stream #f)
    (z-stream-avail-in-set! stream 0)
    (z-stream-next-in-set! stream #f)
    (set! ret (inflate-init stream))
    (if (not (= Z_OK ret)) (z-abort ret)
        (make-input-port
         (lambda ()
           (when (>= pos (string-length bytes-avail))
             (begin
               (set! pos 0)
               (set! bytes-avail "")
               (set! in (read-string chunk port))
               (z-stream-avail-in-set! stream (string-length in))
               (if (or (= 0 (z-stream-avail-in stream))
                       (= Z_STREAM_END ret))
                   (begin (inflate-end stream) (set! eof? #t))
                   (begin
                     (z-stream-next-in-set! stream (make-weak-locative in))
                     (z-stream-avail-out-set! stream 0)
                     (while (= 0 (z-stream-avail-out stream))
                       (z-stream-avail-out-set! stream chunk)
                       (z-stream-next-out-set! stream (make-weak-locative out))
                       (set! ret (inflate stream Z_NO_FLUSH))
                       (assert (not (= Z_STREAM_ERROR ret)) "state clobbered")
                       (when (or (= Z_NEED_DICT ret)
                                 (= Z_DATA_ERROR ret)
                                 (= Z_MEM_ERROR ret))
                         (set! ret (if (= Z_NEED_DICT ret) Z_DATA_ERROR ret))
                         (inflate-end stream)
                         (z-abort ret))
                       (set! bytes-avail
                             (string-append bytes-avail
                                            (substring out 0 (- chunk (z-stream-avail-out stream))))))))))
           (if eof? #!eof
               (begin 
                 (set! pos (+ 1 pos))
                 (string-ref bytes-avail (- pos 1)))))
         (lambda ()
           (and (char-ready? port) (not eof?)))
         (lambda ()
           (close-input-port port))))))

(define deflate-init (foreign-lambda int "deflateInit" z-stream int))
(define deflate (foreign-lambda int "deflate" z-stream int))
(define deflate-end (foreign-lambda void "deflateEnd" z-stream))

(define (open-zlib-compressed-output-port port)
  (let ((ret #f)
        (stream (make-z-stream))
        (flush Z_NO_FLUSH)
        (out (make-string chunk))
        (collected-in ""))
    (z-stream-z-alloc-set! stream #f)
    (z-stream-z-free-set! stream #f)
    (z-stream-opaque-set! stream #f)
    (set! ret (deflate-init stream Z_DEFAULT_COMPRESSION))
    (define (write-collected)
      (let ((avail-in (string-length collected-in)))
        (z-stream-avail-in-set! stream avail-in)
        (unless (= 0 (string-length collected-in))
          (z-stream-next-in-set! stream (make-weak-locative collected-in)))
        (z-stream-avail-out-set! stream 0)
        (while (= 0 (z-stream-avail-out stream))
          (z-stream-avail-out-set! stream chunk)
          (z-stream-next-out-set! stream (make-locative out))
          (set! ret (deflate stream flush))
          (assert (not (= Z_STREAM_ERROR ret)) "state clobbered")
          (write-string (substring out 0 (- chunk (z-stream-avail-out stream))) #f port))
        (assert (= 0 (z-stream-avail-in stream)) "could not process all input")
        (set! collected-in "")))
    (if (not (= Z_OK ret)) (z-abort ret)
        (make-output-port
         (lambda (in)
           (unless (= flush Z_FINISH)
             (set! collected-in (string-append collected-in in))
             (when (>= (string-length collected-in) chunk)
               (write-collected))))
         (lambda ()
           (unless (= flush Z_FINISH)
             (assert (not (port-closed? port)) "could not finish writing")
             (set! flush Z_FINISH)
             (write-collected)
             (assert (= ret Z_STREAM_END) "could not write stream end")
             (deflate-end stream)))
         (lambda ()
           (flush-output port))))))

)
