#lang racket

;; SPDX-License-Identifier: BlueOak-1.0.0

(require data/enumerate/lib
         file/sha1
         threading)

;; Mining for ‘amulets’, short poems whose SHA-256 hash includes a sequence of four or more
;; consecutive 8s. See https://text.bargains/amulet/


;; First some utilities for testing and printing amulets

(define (small-enough? bs)
  (cond
    [(< (bytes-length bs) 65) bs] ;; Amulets must be <= 64 bytes
    [else #f]))

(define (hash-str str)
  (and~> (string->bytes/utf-8 str)
         small-enough?
         sha256-bytes
         bytes->hex-string))

(define (amulet? v)
  (define str
    (cond
      [(string? v) v]
      [(list? v) (string-join v "")]
      [else (format "~a" v)]))
  (match (hash-str str)
    [(regexp #px"8{6,}" amulet) amulet] ;; 'rare' and above only
    [_ #false]))

(define quality-levels
  (hash "8888" "common"
        "88888" "uncommon"
        "888888" "rare"
        "8888888" "epic"
        "88888888" "legendary"
        "888888888" "mythic"))

(define (print-amulet a q [c ""])
  (define core-id (if (eq? c "") "" (format "core ~a:" c)))
  (define rating (hash-ref quality-levels q "?!?!?"))
  (define top-rule-length (apply - 67 (map string-length (list q core-id rating))))
  (displayln
   (format "┌~a[~a~a (~a)]" (make-string top-rule-length #\─) core-id q rating))
  (display (string-join a ""))
  (displayln (format "\n└~a" (make-string 72 #\─))))

;; And now, tools for building things that might be amulets.

;; Given a list of words, we might try modifying them in various ways in order to kick up an amulet:
;; adding bits of punctuation, changing their case, or prefixing them with spaces. Each enumeration
;; is essentially a list of possible modifications.

(define punctuations/e (fin/e "" "," "." "\n" "!" "?"))
(define word-transforms/e (fin/e identity string-upcase string-titlecase))
(define word-spacers/e
  (pam/e (λ (n) (λ (s) (format "~a~a" (make-string n #\space) s)))
         (below/e 5)
         #:contract (-> string? string?)))

;; A word variant is a word transformed by any combination of modifiers. Given a word, return an
;; enumeration of all possible variants of that word using the above enumerations (punctuation, case
;; modifiers, and spacers).
(define (word-variants/e word)
  (pam/e (λ (w p t i) (i (t (format "~a~a" w p))))
         (fin/e word)
         punctuations/e
         word-transforms/e
         word-spacers/e
         #:contract string?))

;; A geode is a list of word-variants, with no word being used more than once. A geodes may or may
;; not contain an amulet. This function returns an enumeration of all possible geodes for a given
;; set of words.
(define (geodes/e words)
  (define word-enums (map word-variants/e words))
  ;; Get a list of all possible arrangements of the word enums.
  ;; Using 'rest' eliminates the first case (an empty list).
  (define possible-word-enum-orders
    (rest (apply append (map permutations (combinations word-enums)))))
  ;; Make each possible arrangement of the word enums into a list enumeration
  ;; and then append them all into one giant enumeration.
  (apply append/e
         (for/list ([word-enum-order (in-list possible-word-enum-orders)])
           (apply list/e word-enum-order))))

;; Take a list of words and just churn through all the possibilities, printing any
;; ammulets found. (With 3 or more words, this could take a LONG time.)
(define (simple-find-amulets word-string)
  (define all-geodes/e (geodes/e (string-split word-string)))
  (for ([geode (in-enum all-geodes/e)])
    (define quality (amulet? geode))
    (cond [quality (print-amulet geode (first quality))])))

;; We can speed things up by using parallel processing.

;; A Racket “place” runs in a separate Racket VM on its own processor or core.
(define (start-amulet-finder word-lst c-id lo hi)
  (let ([p (place ch
                  (define me (place-channel-get ch))
                  (define geo/e (geodes/e (place-channel-get ch)))
                  (define low-bound (place-channel-get ch))
                  (define hi-bound (place-channel-get ch))
                  (for ([n (in-range low-bound hi-bound)])
                    (define g (from-nat geo/e n))
                    (define quality (amulet? (from-nat geo/e n)))
                    (cond [quality (place-channel-put ch (list me g quality))]))
                  (place-channel-put ch me)
                  (exit 0))])
    (place-channel-put p c-id)
    (place-channel-put p word-lst)
    (place-channel-put p lo)
    (place-channel-put p hi)
    (displayln (format "core ~a: start ~a, end ~a" c-id lo hi))
    p))

(define (parallel-find-amulets word-string)
  (define word-lst (string-split word-string))
  (define size (enum-count (geodes/e word-lst)))
  (define per-core (quotient size (processor-count)))

  (displayln (format "Enum space size: ~a" size))
  
  (define cores-running
    (for/list ([c (in-range (processor-count))])
      (define start (* c per-core))
      (define end (if (eq? c (sub1 (processor-count)))
                      size
                      (sub1 (* (add1 c) per-core))))
      (start-amulet-finder word-lst c start end)))

  (define (check-core p)
    (cond
      [p
       (match  (place-channel-get p)
         [(list c a quality) (begin (print-amulet a (first quality) c) p)]
         [_ #false])]
      [else #false]))

  (time
   (void
    (let loop ([cores cores-running])
      (cond [(andmap false? cores) 'finished]
            [else
             (loop
              (for/list ([c (in-list cores)])
                (check-core c)))])))))
