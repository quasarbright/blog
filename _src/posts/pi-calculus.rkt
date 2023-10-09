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

; Process -> Void
; run the process until it and its children all terminate or are all blocked
(define (run proc [num-steps #f])
  (define output-channel (new-channel))
  (parameterize ([current-output-channel output-channel]
                 [current-process-queue (list proc)])
    (let loop ([num-steps num-steps])
      (cond
        [(or (and num-steps (zero? num-steps))
             (empty? (current-process-queue)))
         (void)]
        [(process-queue-all-blocked? (current-process-queue))
         (error 'run "deadlock")]
        [else
         (step!)
         (loop (and num-steps (sub1 num-steps)))]))
    (channel-values (current-output-channel))))

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
  (check-equal? (run (noop)) '())
  (check-equal? (run (with-channel (lambda (chan) (noop)))) '())
  (define simple-in-out-process
    (with-channel
      (lambda (chan)
        (branch (out chan 2 (noop))
                (in chan (lambda (val) (out (current-output-channel) val (noop))))))))
  (check-equal? (run simple-in-out-process) '(2))
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
  (check-equal? (run single-round-of-server-process) '(3))
  (check-exn #rx"deadlock" (thunk (run (with-channel (lambda (chan) (in chan (lambda (x) (noop))))))))
  (check-exn #rx"deadlock" (thunk (run (with-channel (lambda (chan) (in chan (lambda (x) (out chan 42 (noop)))))))))
  (define nats-process
    (with-channel
      (lambda (chan)
        (branch (out chan 0 (noop))
                (duplicate (in chan (lambda (val)
                                      (out (current-output-channel)
                                           val
                                           (out chan (add1 val) (noop))))))))))
  (check-equal? (run nats-process 20) '(0 1 2))
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
