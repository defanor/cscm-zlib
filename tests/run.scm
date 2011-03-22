
(import chicken scheme)
(use ports simple-sha1 srfi-1 test zlib)

(define in-data-size #x20000) ; 128K of data

(define in-data (string-unfold null? car cdr (list-tabulate in-data-size (lambda (i) (integer->char (random 255))))))
(define sha1 (string->sha1sum in-data))

(define out-data "")

;; test for idempotence

(test-begin "in memory")

(test-assert "deflate"
             (begin
               (set! out-data
                     (let ((string-port (open-output-string)))
                       (with-output-to-port (open-zlib-compressed-output-port string-port)
                         (lambda ()
                           (write-string in-data)
                           (close-output-port (current-output-port))
                           (get-output-string string-port)))))
               (> (string-length out-data) #x10000)))

(test "inflate" sha1
      (string->sha1sum
       (let ((string-port (open-output-string)))
         (with-input-from-port (open-zlib-compressed-input-port (open-input-string out-data))
           (lambda ()
             (write-string (read-string) #f string-port)
             (close-input-port (current-input-port))
             (get-output-string string-port))))))

(test-end)

(test-exit)
