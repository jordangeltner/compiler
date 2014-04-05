#lang racket

(define infile 
  (open-input-file 
   (vector-ref (current-command-line-arguments) 0)))

(define x (read infile))
(close-input-port infile)

(define (printl1 l1)
  (cond
    ((null? l1))
    ((null? (cdr l1)) (print (car l1)))
    (true (and (print (car l1))
               (printl1 (cdr l1))))))


;(edx += ecx)  =>  addl %ecx, %edx
;if we have 1 line of l1 in the form of a list, print out the associated x86asm instruction
(define (convertl1 l1)
  (cond
    ((= (length l1) 3)
     (let ([arg1 (first l1)]
           [arg2 (third l1)]
           [op (second l1)])
       (cond
         ((equal? '+= op) (eprintf (string-append "addl " (convertMem arg2) ", " (convertMem arg1) "\n")))
         ((equal? '-= op) (eprintf (string-append "subl " (convertMem arg2) ", " (convertMem arg1) "\n")))
         ((equal? '*= op) (eprintf (string-append "mull " (convertMem arg2) ", " (convertMem arg1) "\n")))
         ((equal? '&= op) (eprintf (string-append "andl " (convertMem arg2) ", " (convertMem arg1) "\n")))
         ((equal? '<<= op) (eprintf (string-append "sall %" (shiftOpLow8 arg2) ", %" (symbol->string arg1) "\n")))
         ((equal? '>>= op) (eprintf (string-append "shrl %" (shiftOpLow8 arg2) ", %" (symbol->string arg1) "\n")))
         ((equal? '<- op) 
          (cond
            ((= (length arg2) 1) (eprintf (string-append "movl " (convertMem arg2) ", " (convertMem arg1) "\n")))
            (true 
             (let ([arga (first arg2)])
               (cond
                 ((equal? arga 'print) (eprintf (string-append
                                                 "pushl " (convertMem (second arg2)) "\n"
                                                 "call print\n"
                                                 "addl $4, %esp\n")))
                 ((equal? arga 'allocate) (eprintf (string-append
                                                   "pushl " (convertMem (third arg2)) "\n"
                                                   "pushl " (convertMem (second arg2)) "\n"
                                                   "call allocate\n"
                                                   "addl $8, %esp\n")))
                 ((equal? arga 'array-error) (eprintf (string-append
                                                       "pushl " (convertMem (third arg2)) "\n"
                                                       "pushl " (convertMem (second arg2)) "\n"
                                                       "call print-error\n"
                                                       "addl $8, %esp\n"))))))))
                 
         (true (print "done")))))
    ((= (length l1) 6)
     (let ([op (third l1)]
           [label (fifth l1)]
           [arg2 (fourth l1)]
           [arg1 (second l1)])
       (cond
         ((equal? '< op) (eprintf (string-append 
                                   "cmpl " (convertMem arg2) ", %" (convertMem arg1) 
                                   "\njg " (symbol->string label) "\n")))
         ((equal? '<= op) (eprintf (string-append 
                                   "cmpl " (convertMem arg2) ", %" (convertMem arg1) 
                                   "\njge " (symbol->string label) "\n")))
         ((equal? '= op) (eprintf (string-append 
                                   "cmpl " (convertMem arg2) ", %" (convertMem arg1) 
                                   "\nje " (symbol->string label) "\n")))
         (true (print "done")))))
     ((= (length l1) 1)
      (let ([arg1 (first l1)])
        (let ([str (symbol->string arg1)])
          (cond
            ((equal? 'return arg1) (print ("ret")))
            (true (eprintf (string-append (substring str 1 (string-length string)) ":\n")))))))
     ((= (length l1) 2)
      (let ([arg1 (first l1)]
            [arg2 (second l1)])
        (cond
          ((equal? 'call arg1) (eprintf (string-append 
                                         "pushl $" (symbol->string arg2) "\n"
                                         "pushl %ebp\n"
                                         "movl %esp, %ebp\n"
                                         "jmp " (symbol->string arg2) "\n"
                                         (symbol->string arg2) ":\n")))
          ((equal? 'tail-call arg1) (eprintf (string-append
                                              "movl %ebp, %esp\n"
                                              "jmp " (symbol->string arg2) "\n")))
          ((equal? 'goto arg1) (eprintf (string-append
                                         "jmp " (symbol->string arg2) "\n"))))))
     ((= (length l1) 5)
      (let ([reg1 (first l1)]
            [reg2 (third l1)]
            [reg3 (fifth l1)])
        (eprintf (string-append 
                  "cmpl " (convertMem reg3) ", " (convertMem reg2) "\n"
                  "setl %" (shiftOpLow8 reg1) "\n"
                  "movzbl %" (shiftOpLow8 reg1) ", " (convertMem reg1) "\n"))))))
      
                  
                 

;to be used for shifting operations only
;given a symbol that is a register, returns the string for the lower 8 bit special register
(define (shiftOpLow8 reg)
  (cond
    ((equal? 'eax reg) "al")
    ((equal? 'ebx reg) "bl")
    ((equal? 'ecx reg) "cl")
    ((equal? 'edx reg) "dl")
    (true "NOOOOOOOOO shifting in illegal register with 8 bits")))


;to be used for saving comparison results 16-bit
;given a symbol that is a register, returns the string for the lower 16 bit special register
(define (shiftOpLow16 reg)
  (cond
    ((equal? 'eax reg) "ax")
    ((equal? 'ebx reg) "bx")
    ((equal? 'ecx reg) "cx")
    ((equal? 'edx reg) "dx")
    (true "NOOOOOOOOO shifting in illegal register with 16 bits")))

; eax <- (mem ebx 4)
;movl eax, 4(ebp)
;
;converting (mem ebx -4) to asm
;given a l1 symbol representing a memory location (a register or (mem reg n), returns the asm string
(define (convertMem l1)
  (cond
    ((= (length l1) 1) (string-append "%" (symbol->string (first l1))))
    (true (string-append (number->string (third l1)) "(" (symbol->string (second l1)) ")"))))

  



