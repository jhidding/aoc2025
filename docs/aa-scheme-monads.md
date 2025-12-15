# Monads in Scheme

```scheme
;| file: src/scheme/monads/monads.scm
(library (monads monads)
  (export seq make-monad monad? monad-return monad-bind <- ::
          <maybe> maybe-bind maybe-return *nothing* nothing?
          ; <state> state-bind state-return get-state set-state
          ; reader-bind reader-return reader-ask
          ; update-state
          seq-map)

  (import (rnrs (6))
          (monads syntax)
          (monads maybe)
          ; (monads state)
          ; (monads reader)
          (monads support))
)
```

```scheme
;| file: src/scheme/monads/syntax.scm
(library (monads syntax)
  (export seq make-monad monad? monad-return monad-bind <- ::)

  (import (rnrs (6))
          (std receive)
          (utility aux-keyword))

  (define-auxiliary-keywords <- ::)

  (define-record-type monad
    (fields bind return))

  (define-syntax seq
    (syntax-rules (<- ::)
      ;; the last expression in a sequence remains as is.
      ((_ <M> <f>)
       <f>)

      ;; (seq M (a <- expression) ...) expands to a nested
      ;; binding to a function that contains the rest of the
      ;; sequence
      ((_ <M>
          (<formals> ... <- <f>)
          <rest> ...)

       ((monad-bind <M>)
        <f>
        (lambda (<formals> ...)
          (seq <M> <rest> ...))))

      ;; (seq M (a :: expression) ...) expands to a nested
      ;; let binding
      ((_ <M>
          (<formals> ... :: <f>)
          <rest> ...)

       (call-with-values
         (lambda () <f>)
         (lambda (<formals> ...)
           (seq <M> <rest> ...))))

      ;; If the pattern doesn't match the (a <- expr) pattern,
      ;; the outcome of <f> is thrown away, but we still need
      ;; a lambda for bind to work on.
      ((_ <M>
          <f>
          <rest> ...)

       ((monad-bind <M>)
        <f>
        (lambda _
          (seq <M> <rest> ...))))))
)
```

### Maybe

```scheme
;| file: src/scheme/monads/maybe.scm
(library (monads maybe)
  (export nothing? *nothing* maybe-bind maybe-return <maybe>)

  (import (rnrs (6))
          (monads syntax))

  (define-record-type nothing)

  (define *nothing* (make-nothing))

  (define (maybe-bind value f)
    (if (nothing? value)
        value
        (f value)))

  (define maybe-return values)

  (define <maybe> (make-monad maybe-bind maybe-return))
)
```

### Support functions

```scheme
;| file: src/scheme/monads/support.scm
(library (monads support)
  (export seq-map)
  (import (rnrs (6))
          (monads syntax))

  (define (seq-map M f . args)
    (assert (not (null? args)))
    (let loop ((a args)
               (b '()))
      (if (null? (car a))
        ((monad-return M) (reverse b))
        (seq M
          (x <- (apply f (map car a)))
          (loop (map cdr a) (cons x b))))))

  (define (seq-compose M . fs)
    (let ((c2 (lambda (f g) (lambda (x) ((monad-bind M) (g x) f)))))
      (fold-right c2 values fs)))
)
```
