; Vanessa Melikian, Vishal Shah, Catherine Tsuei

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


