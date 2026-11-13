#lang racket

;; data definitions ;;

; A Process is one of
(struct out [chan val proc] #:transparent)
; writes val to chan and then runs proc
; where
; chan is a Channel
; val is an Any
; proc is a Process
(struct in [chan val->proc] #:transparent)
; reads a value from chan and runs val->proc applied to that value
; where
; chan is a Channel
; val->proc is a (Any -> Process)
(struct with-channel [chan->proc] #:transparent)
; creates a channel and runs chan->proc applied to that channel
; where
; chan->proc is a (Channel -> Process)
(struct branch [proc1 proc2] #:transparent)
; runs proc1 and proc2 concurrently
; where
; proc1 is a Process
; proc2 is a Process
(struct duplicate [proc] #:transparent)
; runs infinite copies of proc concurrently
; where
; proc is a Process
(struct noop [] #:transparent)
; does nothing

; A Channel is a
(struct channel [[values #:mutable]] #:transparent)
; can be read from and written to from processes
; where values is a (listof Any) and the first element is the oldest.

; A ProcessQueue is a (listof Process)
; represents processes running concurrently
; where the first process is the oldest.

;; scheduler ;;

(define current-output-channel (make-parameter #f))
(define current-process-queue (make-parameter #f))

; Process -> (list symbol? (listof Any))
; run the process until it and its children all terminate or are all blocked
(define (run proc [num-steps #f])
  (define output-channel (new-channel))
  (parameterize ([current-output-channel output-channel]
                 [current-process-queue (list proc)])
    (define result-type
      (let loop ([num-steps num-steps])
        (cond
          [(and num-steps (zero? num-steps))
           'timeout]
          [(empty? (current-process-queue))
           'success]
          [(process-queue-all-blocked? (current-process-queue))
           'deadlock]
          [else
           (step!)
           (loop (and num-steps (sub1 num-steps)))])))
    (list result-type (channel-values (current-output-channel)))))

; run one step of computation.
; do a little bit of work in the next unblocked process and push child processes
; onto the queue.
; assumes the program is not blocked.
(define (step!)
  (match (pop-unblocked-process!)
    [(noop)
     (void)]
    [(in chan val->proc)
     (push-process! (val->proc (channel-pop! chan)))]
    [(out chan val proc)
     (channel-push! chan val)
     (push-process! proc)]
    [(with-channel chan->proc)
     (push-process! (chan->proc (new-channel)))]
    [(branch proc1 proc2)
     (push-process! proc1)
     (push-process! proc2)]
    [(duplicate proc)
     (push-process! proc)
     (push-process! (duplicate proc))]))

;; process queue ;;

; {ProcessQueue} -> Boolean
; are all processes definitely blocked?
(define (process-queue-all-blocked? [pq (current-process-queue)])
  (for/and ([proc pq])
    (process-blocked? proc)))

; Process -> Boolean
; is the process definitely blocked?
(define (process-blocked? proc)
  (match proc
    [(in chan _) (channel-empty? chan)]
    [(branch proc1 proc2) (and (process-blocked? proc1) (process-blocked? proc2))]
    [(duplicate p) (process-blocked? p)]
    [_ #f]))

; -> Process
; pops the next unblocked process from the current queue in place.
(define (pop-unblocked-process!)
  (define-values (proc pq) (pop-unblocked-process (current-process-queue)))
  (current-process-queue pq)
  proc)

; ProcessQueue {(listof Process)} -> (values Process ProcessQueue)
; pops the next unblocked process from the queue immutably.
; skipped-rev is an accumulator storing the blocked processes
; at the head of the queue in reverse order.
(define (pop-unblocked-process pq [skipped-rev '()])
  (cond
    [(null? pq) (error 'pop-unblocked-process "cannot pop from empty process queue")]
    [(process-blocked? (first pq)) (pop-unblocked-process (rest pq) (cons (first pq) skipped-rev))]
    [else (values (first pq) (append (reverse skipped-rev) (rest pq)))]))

; Process -> Void
; push a process onto the current queue
(define (push-process! proc)
  (current-process-queue (append (current-process-queue) (list proc))))

;; channels ;;

; -> Channel
; create an empty channel
(define (new-channel) (channel '()))

; Channel -> Boolean
; is the channel empty?
(define (channel-empty? chan)
  (empty? (channel-values chan)))

; Channel Any -> Void
; push a value into the channel
(define (channel-push! chan val)
  (set-channel-values! chan (append (channel-values chan) (list val))))

; Channel -> Any
; pop the next value from the channel
(define (channel-pop! chan)
  (define val (first (channel-values chan)))
  (set-channel-values! chan (rest (channel-values chan)))
  val)

(module+ test
  (require rackunit)
  (check-equal? (run (noop)) '(success ()))
  (check-equal? (run (with-channel (lambda (chan) (noop)))) '(success ()))
  (define simple-in-out-process
    (with-channel
      (lambda (chan)
        (branch (out chan 2 (noop))
                (in chan (lambda (val) (out (current-output-channel) val (noop))))))))
  (check-equal? (run simple-in-out-process) '(success (2)))
  (struct request [response-channel body] #:transparent)
  (define single-round-of-server-process
    (with-channel
      (lambda (server-request-channel)
        (branch (in server-request-channel
                    (lambda (request)
                      (out (request-response-channel request)
                           (add1 (request-body request))
                           (noop))))
                (with-channel
                  (lambda (response-channel)
                    (out server-request-channel
                         (request response-channel 2)
                         (in response-channel
                             (lambda (response)
                               (out (current-output-channel)
                                    response
                                    (noop)))))))))))
  (check-equal? (run single-round-of-server-process) '(success (3)))
  (check-equal? (run (with-channel (lambda (chan) (in chan (lambda (x) (noop))))))
                '(deadlock ()))
  (check-equal? (run (with-channel (lambda (chan) (in chan (lambda (x) (out chan 42 (noop)))))))
                '(deadlock ()))
  (define nats-process
    (with-channel
      (lambda (chan)
        (branch (out chan 0 (noop))
                (duplicate (in chan (lambda (val)
                                      (out (current-output-channel)
                                           val
                                           (out chan (add1 val) (noop))))))))))
  (check-equal? (run nats-process 20) '(timeout (0 1 2)))
  ; this loops
  ; I don't think it's possible to write this in a way that makes it terminate. Furthermore, I can't think of a non-trivial terminating example
  ; using 'duplicate' at all without cheating.
  ; The reason this doesn't terminate is because even when the list is empty, duplicate will still try to read input. It'll noop if it's
  ; empty, but the 'in' will still get duplicated.
  #;(check-equal? (run (with-channel chan (branch (out chan '(1 2 3 4) noop)
                                                  (duplicate (in chan nums (if (null? nums)
                                                                               noop
                                                                               (out out-channel (first nums)
                                                                                    (out chan (rest nums) noop))))))))
                  '(1 2 3 4)))

;; lambda calculus to pi calculus ;;

(struct request [response-channel body] #:transparent)
; represents a request sent to a server
; where
; response-channel is the channel that the response should be sent to
; body is the data of the request

(define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))

(define (eval-expr expr)
  ; wrap with with-channel to delay evaluation of (current-output-channel)
  (define result (eval `(run (with-channel (lambda (bs-channel) ,(compile-expr expr '(current-output-channel))))) ns))
  (match result
    [(list _ (list val)) val]))

; LambdaCalculusExpr symbol? -> PiCalculusExpr
; compiles a lambda calculus expression to a pi calculus expression that writes its value
; to output-channel.
; The process is expected to deadlock if there is a function in the program.
(define (compile-expr expr output-channel)
  ; Here are the big ideas:
  ; - functions are servers. a server expects a request object
  ; - applications are clients
  ; - a function value is represented by its server's input channel
  ; - use CPS
  ;   - every expression has an output channel, its current continuation
  ;   - every expression sends its value to its output channel
  ;   - output-channel is the current continuation for expr
  (match expr
    ['call/cc
     ; let/cc's commented out implementation is simpler.
     ; multi-shot is enabled by duplications in the application rule.
     (define k/cc output-channel)
     `(with-channel
        (lambda (call/cc)
          (branch
           (duplicate
            (in call/cc
                (lambda (req/cc)
                  ; request from the application of call/cc itself
                  ; f takes in k
                  (define f (request-body req/cc))
                  ; the current continuation
                  (define k (request-response-channel req/cc))
                  (with-channel
                    ; k^ is a wrapper around k that user-space functions can call
                    (lambda (k^)
                      (branch
                       (duplicate
                        (in k^
                            (lambda (req/k)
                              ; request from the application of k^, the current continuation
                              ; the value to fill in the hole with
                              (define val (request-body req/k))
                              ; the continuation of applying k (ignored)
                              (define cont (request-response-channel req/k))
                              ; this should abort since we ignore cont
                              ; and the rest is blocked reading it.
                              ; idk if we can do composable continuations directly
                              ; since there is no analog of (cont (k val))
                              (out k val (noop)))))
                       ; f sends its answer to k and has access to k^
                       (out f (request k k^) (noop))))))))
           (out ,k/cc call/cc (noop)))))]
    [(? (negate cons?))
     ; atomic expression like a variable reference
     `(out ,output-channel ,expr (noop))]
    [`(let ([,x ,rhs]) ,body)
     (compile-expr `((lambda (,x) ,body) ,rhs)
                   output-channel)]
    [`(let/cc ,user-k ,body)
     ; the current continuation
     (compile-expr `(call/cc (lambda (,user-k) ,body)) output-channel)
     #;#;
     (define k output-channel)
     `(with-channel
        (lambda (,user-k)
          (branch
           ; a server that writes its request body to k
           (duplicate
            (in ,user-k
                (lambda (request)
                  (let ([val (request-body request)]
                        [cont (request-response-channel request)])
                    ; val is the value passed to k.
                    ; cont the continuation for k being applied.
                    ; this should abort since we ignore cont
                    ; idk if we can do composable continuations directly
                    ; since there is no analog of (cont (k val))
                    (out ,k val (noop))))))
           ,(compile-expr body output-channel))))]
    [`(lambda (,x) ,body)
     ; functions run like a server, waiting for requests and sending responses
     ; they also take in a continuation, which is the response channel included in the request
     (define input-channel (gensym 'input-channel))
     (define request (gensym 'request))
     (define body-output-channel (gensym 'body-output-channel))
     `(with-channel
        (lambda (,input-channel)
          (branch (duplicate
                   (in ,input-channel
                       (lambda (,request)
                         (let ([,x (request-body ,request)]
                               [,body-output-channel (request-response-channel ,request)])
                           ,(compile-expr body body-output-channel)))))
                  ; the lambda is represented by input-channel, and we are writing input-channel itself to output-channel
                  (out ,output-channel ,input-channel (noop)))))]
    [(list ef ex)
     ; function application.
     ; pretty much just the application rewrite rule from CPS.
     ; an application is a client, sending a request to the function's server.
     (define f (gensym 'f))
     (define x (gensym 'x))
     (define f-output-channel (gensym 'f-output-channel))
     (define x-output-channel (gensym 'x-output-channel))
     `(with-channel
        (lambda (,f-output-channel)
          (branch ,(compile-expr ef f-output-channel)
                  ; we duplicate here to support multi-shot continuations.
                  ; otherwise, it is unnecessary.
                  ; expressions can "return twice" if someone re-uses a continuation.
                  (duplicate
                   (in ,f-output-channel
                       (lambda (,f)
                         (with-channel
                           (lambda (,x-output-channel)
                             (branch ,(compile-expr ex x-output-channel)
                                     (duplicate
                                      (in ,x-output-channel
                                          (lambda (,x)
                                            ; send a request with body x to the server f,
                                            ; and tell it to to send its response to the application's output channel
                                            (out ,f (request ,output-channel ,x) (noop))))))))))))))]))

(module+ test
  (check-equal? (eval-expr 1) 1)
  (check-equal? (eval-expr '((lambda (x) x) 1))
                1)
  (check-equal? (eval-expr '(((lambda (x) (lambda (y) x)) 1) 2))
                1)
  (check-equal? (eval-expr '(let ([const (lambda (x) (lambda (y) x))])
                              (let ([return-1 (const 1)])
                                (return-1 2))))
                1)
  ; abort
  (check-equal? (eval-expr '(let ([const (lambda (x) (lambda (y) x))])
                              (let/cc k ((const 1) (k 2)))))
                2)
  ; multi-shot: we resume from the let/cc once with cc and again with that lambda
  (check-equal? (eval-expr '((let ([k (let/cc cc cc)])
                               ; this lambda ends up calling itself
                               (k (lambda (x) (lambda (y) 1))))
                             2))
                1))

; future work:
; - multi argument lambdas, multi-variable let
; - add built-in functions
; - add concurrency operators
