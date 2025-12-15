; ~/~ begin <<docs/day01.md#day01::src/scheme/day01.scm>>[init]
(import (except (rnrs) map filter)
        (std transducers)
        (std combinators)
        (monads monads)
        (parsing parsing))

(define stream-input
    (case-lambda
        ((sink) (let loop ((r (sink)))
            (if (reduced? r)
                (reduced-value r)
                (let ((line (get-line (current-input-port))))
                    (if (eof-object? line)
                        (sink r)
                        (loop (sink r line)))))))
        ((tr sink) (stream-input (tr sink)))))

(define (trace msg value)
    (display msg) (display value) (newline)
    value)

(define (trace-printer sink)
    (case-lambda
        (() (trace "init: " (sink)))
        ((r) (trace "result: " (sink r)))
        ((r x) (display "got: ") (display x) (display " -> ") (trace "" (sink r x)))))

(define print-result
    (case-lambda
        (() '())
        ((r) (display r) r)
        ((r x) x)))

(define instruction
  (seq <parsing>
      (sign <- (choice (parsing-bind (literal "L") (lambda (_) (parsing-return -1)))
                       (parsing-bind (literal "R") (lambda (_) (parsing-return 1)))))
      (amount <- integer)
      (parsing-return (* sign amount))))

(define (parse-instruction s)
    (parse-string instruction s))

(define count (compose (map (const 1)) (scanl +)))

(define main (compose (map parse-instruction)
                      (scanl + 50)
                      (filter (compose zero? (partial (swap mod) 100)))
                      count))

(display "Part 1: ") (stream-input main print-result) (newline)
; ~/~ end
