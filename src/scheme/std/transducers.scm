; ~/~ begin <<docs/day01.md#day01::src/scheme/std/transducers.scm>>[init]
(library (std transducers)
    (export make-reduced reduced? reduced-value map filter tee scanl stream-input)
    (import (except (rnrs) map filter)
            (prefix (only (rnrs) map filter) internal-))

    (define-record-type reduced (fields value))

    (define map
        (case-lambda
            ((f) (lambda (sink)
                (case-lambda
                    ; init
                    (() (sink))
                    ; fini
                    ((r) (sink r))
                    ; step
                    ((r x) (sink r (f x))))))
            ((f . args) (apply internal-map f args))))

    (define filter
        (case-lambda
            ((f) (lambda (sink)
                (case-lambda
                    ; init
                    (() (sink))
                    ; fini
                    ((r) (sink r))
                    ; step
                    ((r x) (if (f x) (sink r x) r)))))
            ((f . args) apply internal-filter f args)))

    (define scanl
        (case-lambda
            ((f init step)
                (case-lambda
                    ; init
                    (() (cons init (step)))
                    ; fini
                    ((r) (step (step (cdr r) (f (car r)))))
                    ; step
                    ((r x) (let ((r1 (f (car r) x)) (r2 (cdr r)))
                               (cons r1 (step r2 r1))))))
            ((f init) (lambda (step) (scanl f init step)))
            ((f)      (lambda (step) (scanl f (f) step)))))

    (define (tee . sinks)
        (case-lambda
            (() (map apply sinks))
            ((rs) rs)
            ((rs x) (map (lambda (sink r) (sink r x)) sinks rs))))

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
)
; ~/~ end
