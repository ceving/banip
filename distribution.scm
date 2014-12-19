#! /usr/bin/csi -s
;;
;; Group hosts by CLASS-C network.
;;
;; Time-stamp: <2014-12-19 17:50:16 szi>
;;
;; This program read a list of IPv4 addresses from stdin and generate
;; a HTML page.  The program generates for each CLASS-C network a
;; headline and lists the host, which are in the network.  The lists
;; are sorted numerically.
;;
;; An example can be found at:
;; http://asshole.ceving.de/distribution.html
;;
;; The code has been tested with Chicken-Scheme 4.7.0 using csi and
;; csc.
;;

(use extras)
(use srfi-13)

#;(define-syntax ?
  (syntax-rules ()
    ((? tag arg)
     (let ((value arg))
       (with-output-to-port (current-error-port)
         (lambda ()
           (display ";;;")
           (if tag
               (begin
                 (display " ")
                 (display tag)
                 (display ":")))
           (display " ")
           (write (quote arg))
           (display " -> ")
           (write value)
           (newline)))
       value))
    ((? arg)
     (? #f arg))))

(define (xml-cat args)
  (lambda ()
    (for-each
     (lambda (arg)
       (if (procedure? arg)
           (arg)
           (display arg)))
     args)))

(define (xml-cat* . args)
  (xml-cat args))

(define (xml-element name)
  (lambda body
    (lambda ()
      (display "<")
      (display name)
      (display ">")
      ((xml-cat body))
      (display "</")
      (display name)
      (display ">"))))

(define-syntax define-xml
  (syntax-rules ()
    ((_ arg ...)
     (begin
       (define arg (xml-element (quote arg)))
       ...))))

(define-xml HTML HEAD TITLE BODY H1 H3 UL LI)

(define (vec3< v1 v2)
  (if (= (vector-ref v1 0)
         (vector-ref v2 0))
      (if (= (vector-ref v1 1)
             (vector-ref v2 1))
          (< (vector-ref v1 2)
             (vector-ref v2 2))
          (< (vector-ref v1 1)
             (vector-ref v2 1)))
      (< (vector-ref v1 0)
         (vector-ref v2 0))))
             
(let ((class-c '()))
  (for-each
   (lambda (address)
     (let ((net (vector (vector-ref address 0)
                        (vector-ref address 1)
                        (vector-ref address 2)))
           (host (vector-ref address 3)))
       (let ((dist (assoc net class-c)))
         (if (pair? dist)
             (set-cdr! dist (cons host (cdr dist)))
             (set! class-c
                   (cons (list net host) class-c))))))
   (map
    (lambda (line)
      (list->vector (map string->number (string-split line "."))))
    (read-lines)))
  (let ((title "Distribution per CLASS-C network"))
    ((HTML
      (HEAD (TITLE title))
      (BODY
       (H1 title)
       (xml-cat
        (map
         (lambda (net)
           (xml-cat*
            (H3 (string-join (map number->string (vector->list (car net)))
                             "."))
            (UL (xml-cat (map LI (sort (cdr net) <))))))
         (sort class-c
               (lambda (a b)
                 (vec3< (car a) (car b)))))))))))
