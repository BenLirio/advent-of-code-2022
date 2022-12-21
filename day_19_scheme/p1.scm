; ==================== Utils ====================
; https://stackoverflow.com/questions/16335454/reading-from-file-using-scheme
;works in Chicken, Racket, SISC
;Read a file to a list of chars
(define (file->char_list path)
 (call-with-input-file path
   (lambda (input-port)
     (let loop ((x (read-char input-port)))
       (cond 
        ((eof-object? x) '())
        (#t (begin (cons x (loop (read-char input-port))))))))))

(define drop-last (lambda (lst)
  (reverse (cdr (reverse lst)))))

(define zip (lambda (l1 l2)
  (cond
    ((and (null? l1) (null? l2)) '())
    ((or (null? l1) (null? l2)) #f)
    (else (cons (cons (car l1) (car l2)) (zip (cdr l1) (cdr l2)))))))

(define split (lambda (s c)
  (define idx (string-find-next-char s c))
  (cond 
    ((eq? idx #f) (list s))
    (else (cons (substring s 0 idx) (split (substring s (+ idx 1) (string-length s)) c))))))

(define string->resource-tye (lambda (s)
  (cond
    ((string=? s "ore") 'ore)
    ((string=? s "clay") 'clay)
    ((string=? s "obsidian") 'obsidian)
    ((string=? s "geode") 'geode)
    (else #f))))

(define not-false (lambda (x) (not (eq? x #f))))
(define max (lambda (xs)
  (cond
  ((null? xs) #f)
  ((null? (cdr xs)) (car xs))
  (else (if (> (car xs) (max (cdr xs))) (car xs) (max (cdr xs)))))))
(define sum (lambda (xs)
  (cond
  ((null? xs) 0)
  (else (+ (car xs) (sum (cdr xs)))))))
  
(define seq (lambda (fs) (lambda (x)
  (if (null? fs)
  '()
  (cons ((car fs) x) ((seq (cdr fs)) x))
))))

; ==================== Parse ====================

(define parse-blueprint-idx (lambda (s)
  (string->number (last (split s #\space)))))

(define parse-blueprint-action (lambda (s)
  (define resource-types (filter not-false (map string->resource-tye (split s #\space))))
  (cons
    (car resource-types)
    (zip
      (cdr resource-types)
      (filter not-false (map string->number (split s #\space)))))))

(define parse-blueprint-actions (lambda (s)
  (map parse-blueprint-action (drop-last (split s #\.)))))

(define parse-blueprint (lambda (s)
  (define tmp (split s #\:))
  (cons
    (parse-blueprint-idx (first tmp))
    (parse-blueprint-actions (last tmp)))))

(define parse (lambda (s)
  (map parse-blueprint (split s #\newline))))

; ==================== State ====================
(define robot-of (lambda (x)
  (cond
    ((eq? x 'ore) 'ore-robot)
    ((eq? x 'clay) 'clay-robot)
    ((eq? x 'obsidian) 'obsidian-robot)
    ((eq? x 'geode) 'geode-robot)
    (else #f)
  )))
(define is-robot? (lambda (x)
  (or
    (eq? x 'ore-robot)
    (eq? x 'clay-robot)
    (eq? x 'obsidian-robot)
    (eq? x 'geode-robot))))

(define is-resource? (lambda (x)
  (or
    (eq? x 'ore)
    (eq? x 'clay)
    (eq? x 'obsidian)
    (eq? x 'geode))))
(define zero-state (lambda (x)
  (cond
    ((is-robot? x) 0)
    ((is-resource? x) 0)
    ((eq? x 'min) 5)
    (else #f))))
(define initial-state (lambda (x) (if (eq? x 'ore-robot) 1 (zero-state x))))

(define next-min (lambda (state) (lambda (x)
  (if (eq? x 'min)
  (- (state 'min) 1)
  (state x))
)))

(define can-buy-aux (lambda (cost) (lambda (state)
  (if (null? cost)
  #t
  (and
    (>= (state (car (car cost))) (cdr (car cost)))
    ((can-buy-aux (cdr cost)) state)
  )))))

(define can-buy (lambda (item) (can-buy-aux (cdr item))))

(define add-resource (lambda (resource) (lambda (state) (lambda (x)
  (if (eq? x resource)
  (+ (state x) 1)
  (state x))))))

(define buy-item (lambda (item) (lambda (state) (lambda (x)
  (define resource (car item))
  (define costs (cdr item))
  (define new-state ((add-resource resource) state))
  (define matched-costs (filter (lambda (cost) (eq? (car cost) x)) costs))
  (define delta-cost (sum (map cdr matched-costs)))
  (- (new-state x) delta-cost)
))))

(define buy-items (lambda (items) (lambda (state)
  ((seq (map buy-item items)) state)
)))

(define produce-resource (lambda (resource) (lambda (state)

)))

(define resources '('ore 'clay 'obsidian 'geode))

(define produce-resources (lambda (state)
  (map robot-of resources)
))

(define next-states (lambda (items) (lambda (state)
  (define buyable-items (filter (lambda (item) ((can-buy item) state)) items))
  (cons state ((buy-items buyable-items) state))
)))


; ==================== Solve ====================

(define score-blueprint (lambda (blueprint) (lambda (state)
  (if (eq? (state 'min) 0)
  (state 'geode)
  (max (map (score-blueprint blueprint) ((next-states (cdr blueprint)) (next-min state))))
  ))))

(define solve (lambda (blueprints)
  (map (lambda (f) (f initial-state)) (map score-blueprint blueprints))
))

; ==================== Main ====================

(begin
  (define input (apply string (file->char_list "input_small.txt")))
  (display (solve (parse input)))
  (newline)
  (exit)
)