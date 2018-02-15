; Vanessa Melikian, Vishal Shah, Catherine Tsuei

(load "simpleParser.scm")
; Unary operator??

; takes an arithmetic expression and returns the value
(define MValueInt
  (lambda (expression)
    (cond
      ((number? expression) expression)
      ((eq? '+ (operator expression)) (+ (MValueInt (operand1 expression)) (MValueInt (operand2 expression))))
      ((eq? '- (operator expression)) (- (MValueInt (operand1 expression)) (MValueInt (operand2 expression))))
      ((eq? '* (operator expression)) (* (MValueInt (operand1 expression)) (MValueInt (operand2 expression))))
      ((eq? '/ (operator expression)) (quotient (MValueInt (operand1 expression)) (MValueInt (operand2 expression))))
      ((eq? '% (operator expression)) (remainder (MValueInt (operand1 expression)) (MValueInt (operand2 expression))))
      (else (error 'badop "Undefined operator")))))

; takes a prefix expression and returns the operator
(define operator
  (lambda (e)
    (car e)))

; returns the first operand of an expression
(define operand1 cadr)

; returns the second operand of an expression
(define operand2 caddr)

; TODO: make sure that variables can work here
; takes a comparative expression and returns the boolean result
(define MBoolComparison?
  (lambda (comparison)
    (cond
      ((number? comparison) comparison)
      ((eq? '== (operator comparison)) (eq? (MValueComparison? (operand1 comparison)) (MValueComparison? (operand2 comparison))))
      ((eq? '!= (operator comparison)) (not (eq? (MValueComparison? (operand1 comparison)) (MValueComparison? (operand2 comparison)))))
      ((eq? '< (operator comparison)) (< (MValueComparison? (operand1 comparison)) (MValueComparison? (operand2 comparison))))
      ((eq? '> (operator comparison)) (> (MValueComparison? (operand1 comparison)) (MValueComparison? (operand2 comparison))))
      ((eq? '<= (operator comparison)) (<= (MValueComparison? (operand1 comparison)) (MValueComparison? (operand2 comparison))))
      ((eq? '>= (operator comparison)) (>= (MValueComparison? (operand1 comparison)) (MValueComparison? (operand2 comparison))))
      (else (error 'badop "Undefined operator")))))

; TODO: no numbers allowed, make sure expressions have true/false values
; takes a logical expression and returns the boolean result
(define MBoolLogicOperators?
  (lambda (logicExpression)
    (cond
      ((eq? '#t logicExpression) #t)
      ((eq? '#f logicExpression) #f)
      ((eq? '&& (operator logicExpression)) (and (MBoolLogicOperators? (operand1 logicExpression)) (MBoolLogicOperators? (operand2 logicExpression))))
      ((eq? '|| (operator logicExpression)) (or (MBoolLogicOperators? (operand1 logicExpression)) (MBoolLogicOperators? (operand2 logicExpression))))
      ((eq? '! (operator logicExpression)) (not (MBoolLogicOperators? (operand1 logicExpression))))
      (else (error 'badop "Undefined operator")))))

; takes a conditional expression & current state and returns the new state after evaluation
(define MStateIf
  (lambda (condition then else state)
    (if (MBool condition state) (MStateStatement(then state)) (MStateStatement(else state)))))

; takes a statement list & current state and returns the new state after evaluation
(define MStateStatementList
  (lambda (statementlist state)
    (if (null? statementlist)
        state
        (MStateStatementList (cdr statementlist) (MState (car statementlist) state)))))




(define MState
  (lambda (expression state)
    (cond
      ((eq? '= (operator expression)) ((MAssign (var expression) (value expression) state))))))
      



; binding pairs --> "state"
; variable declaration (var variable) or (var variable value)

; returns the variable of an expression
(define var cadr)

; returns the value of an expression
(define value caddr)

; returns the variable list of the state
(define variableList car)

; returns the value list of the state
(define valueList cdr)
  
; assignment (= variable expression)
(define MAssign
  (lambda (expression state)
    (cond
      ((lookup? (var expression) (variableList state))
       (add (var expression) (value expression) (myRemove (var expression) (variableList state) (valueList state))))
      (else (error 'undeclaredVariable "This variable has not been declared")))))
       
  ;      (Mstate (var expression)
  ;     (eq? '= (operator expression)) ; may not need it based on the = check in MState
  ;        (MBinding (var expression) (value expression) state)
  
; return (return expression) -FIX THIS
  (define return
    (lambda (var state)
      (cond
        ((null? MValue(var state))
          (error 'undef "undeclared variable")
          (MValue(var state))))))
; if (if conditional then-statement optional-else-statement)
; while (while conditional body-statement)
   

  (define MStatement
  (lambda (statement state)
    (if (null? statement)
        state
        (MState(statement state)))))

  ;(define MStatement
   ; (lambda (var '= val state)
    ; ( if (null? statement)
     ;     state
      ;    ((remove (Mvalue(var) state) (add (Mvalue(var) Mvalue(val) state)))))))

(define add
  (lambda (variable value state)
    (list (cons variable (car state)) (cons value (car (cdr state))))))
; (add 'c 'd '((a b) (e f))) -> ((c a b) (d e f))

;lookup
(define lookup?
  (lambda (variable variables)
    (cond
      ((null? (cdr variables)) #f)
      ((equal? variable (car variables)) #t)
      (else (lookup? variable (cdr variables))))))
;(lookup? 'a '(b c d a q w e r))

; value of the variable
(define valueOfVariable
  (lambda (var variables values)
    (if (eq? var (car variables))
             (car values)
             (valueOfVariable var (cdr variables) (cdr values)))))

;remove
(define myRemove
  (lambda (variable variables values)
    (cond
      ((null? variables) (list variables values))
      ((equal? variable (car variables)) (list (cdr variables) (cdr values)))
      (else (list (cons (car variables) (car (myRemove variable (cdr variables) (cdr values)))) (cons (car values) (car (cdr (myRemove variable (cdr variables) (cdr values)))))))))) 


