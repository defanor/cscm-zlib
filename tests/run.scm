;; test for idempotence

(define in-data-size #x40000) ; 256K of data

(import chicken scheme)
(use ports sha1 srfi-1 zlib)

(let* ((in-data (string-unfold null? car cdr (list-tabulate in-data-size (lambda (i) (integer->char (random 255))))))
       (sha1 (sha1-digest in-data))
       (out-data
         (let ((string-port (open-output-string)))
           (with-output-to-port (open-zlib-compressed-output-port string-port)
             (lambda ()
               (write-string in-data)
               (close-output-port (current-output-port))
               (get-output-string string-port))))))
  (unless (string= sha1
                   (sha1-digest
                    (let ((string-port (open-output-string)))
                      (with-input-from-port (open-zlib-compressed-input-port (open-input-string out-data))
                        (lambda ()
                          (write-string (read-string) #f string-port)
                          (close-output-port (current-output-port))
                          (close-input-port (current-input-port))
                          (get-output-string string-port))))))
    (error "compression integrity check failed, sha1 does not match")))
