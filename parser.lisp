(defun commandType (str)
  (cond
    ((some #'oddp
           (map 'list
                (lambda (x)
                  (if (search x str)
                      1
                      0))
                '("add" "sub" "neg" "eq" "gt" "lt" "and" "or" "not")))
                                 'C_ARITHMETIC) 
    ((search "push" str)         'C_PUSH)
    ((search "pop"  str)         'C_POP)) )

(defun arg1 (str)
  (let ((start (1+ (position #\  str)))
        (end   (position #\  str :from-end t)) )  
    (subseq str start end)))

(defun arg2 (str)
  (let ((start (1+ (position #\  str :from-end t))) )
    (subseq str start)))


(arg1 "push constant 7")
(arg2 "push constant 7")
(commandType "push")
(searchList '("add" "sub" "neg" "eq" "gt" "lt" "and") "kmai")


(defun test (str) 
 (some #'oddp         
       (map 'list
            (lambda (x)
              (if (search x str)
                  1
                  0))
            '("add" "sub" "neg" "eq" "gt" "lt" "and" "or" "not")) ))

(test "kami")

(defun searchList (lis str)
  (when lis
    (if (search (car lis) str)
        t
        (searchList (cdr lis) str)) ))
