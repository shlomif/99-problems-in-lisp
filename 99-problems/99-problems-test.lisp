;;; Load the testing framework
(load "cybertigger-test.lisp")
(import 'cybertiggyr-test:deftest)

;;; Load the library to be tested. 
; commented out.
; (load "99-problems.lisp")

(defun prefix-id (prefix id)
  (intern (concatenate 'string prefix "-" (string id))))

(defmacro with-test-name (prefix &rest body)
  `(let ((test-name (prefix-id ,prefix id)))
     ,@body))

(defmacro my-last-test (id the-list box)
  (with-test-name "test-my-last"
    `(deftest ,test-name ()
              (equal (my-last '(,@the-list)) '(,@box)))))

(defun my-last (the-list)
  (let ((tail (cdr the-list)))
    (if (null tail)
      the-list
      (my-last tail))))

(my-last-test A-B-C-D (a b c d) (d))
(my-last-test A-B-C-D-E (a b c d e) (e))
(my-last-test numbers1 (100 90 80 7 105) (105))
(my-last-test list-of-len-1 (hello) (hello))

(defmacro my-but-last-test (id the-list box)
  (with-test-name "test-my-but-last"
    `(deftest ,test-name ()
              (equal (my-but-last '(,@the-list)) '(,@box)))))

(defun my-but-last (the-list)
  (if (null (cddr the-list))
    the-list
    (my-but-last (cdr the-list))))

(my-but-last-test A-B-C-D (a b c d) (c d))
(my-but-last-test A-B-C-D-E (a b c d e) (d e))
(my-but-last-test test-nil () ())
(my-but-last-test one-elem-list (5) (5))

(defmacro element-at-test (id the-list pos elem)
  (with-test-name "test-element-at"
    `(deftest ,test-name ()
              (equal (element-at '(,@the-list) ,pos) ',elem))))

(defun element-at (the-list pos)
  (if (null the-list)
    nil
    (if (= pos 1)
      (car the-list)
      (element-at (cdr the-list) (1- pos)))))

(element-at-test simple (a b c d e) 3 c)
(element-at-test first-elem (a b c d e) 1 a)
(element-at-test mynull () 5 ())
(element-at-test pass-the-end (a b c) 100000000 ())

(defmacro list-len-test (id the-list result)
  (with-test-name "test-list-len"
    `(deftest ,test-name ()
              (equal (list-len '(,@the-list)) ,result))))

(defun list-len (the-list)
  (labels ((helper (myrest mycount)
                   (if (null myrest) 
                     mycount
                     (helper (cdr myrest) (1+ mycount)))))
    (helper the-list 0)))

(list-len-test empty () 0)
(list-len-test three-elems (a b c) 3)
(list-len-test nine-elems (1 2 3 4 5 6 7 8 9) 9)

(defmacro reverse-list-test (id the-list result)
  (with-test-name "test-reverse-list"
    `(deftest ,test-name ()
              (equal (reverse-list '(,@the-list)) '(,@result)))))

(defun reverse-list (mylist)
  (labels ((helper (in out)
                   (if (null in)
                     out
                     (helper (rest in) (cons (car in) out)))))
    (helper mylist ())))

(reverse-list-test empty () ())
(reverse-list-test one-elem (1) (1))
(reverse-list-test two-elem (a e) (e a))
(reverse-list-test three-elem (zod qod mod) (mod qod zod))
(reverse-list-test grand-finale (1 2 5 hello 100 placebo) 
                   (placebo 100 hello 5 2 1))

(defmacro list-eq-test (id list1 list2 result)
  (with-test-name "test-list-eq"
    `(deftest ,test-name ()
              (let ((verdict (list-eq '(,@list1) '(,@list2))))
                (if ,result verdict (not verdict))))))

(defun list-eq (l1 l2)
  (cond
    ((and (null l1) (null l2)) t)
    ((or (null l1) (null l2)) nil)
    ((not (eq (car l1) (car l2))) nil)
    (t (list-eq (cdr l1) (cdr l2)))))

(list-eq-test "null" () () t)
(list-eq-test "one-elem-same" (a) (a) t)
(list-eq-test "one-elem-different" (a) (b) nil)
(list-eq-test "two-elem-same" (a b) (a b) t)
(list-eq-test "two-elem-different" (a b) (a c) nil)
(list-eq-test "different-len-1" (a b c) (a b) nil)
(list-eq-test "different-len-2" (a b) (a b c d) nil)

(defmacro palindrome-test (id list1 result)
  (with-test-name "test-palindrome"
    `(deftest ,test-name ()
              (let ((verdict (is-palindrome '(,@list1))))
                (if ,result verdict (not verdict))))))

(defun is-palindrome (mylist)
  (list-eq mylist (reverse-list mylist)))

(palindrome-test "null" () t)
(palindrome-test "one-elem" (a) t)
(palindrome-test "two-elem" (a a) t)
(palindrome-test "three-elem" (a b a) t)
(palindrome-test "two-elem-fail" (a b) ())
(palindrome-test "grand-finale" (x a m a x) t)

(defun concat-symbols (&rest sym-list)
  (intern (apply #'concatenate (append '(string) (mapcar #'string sym-list)))))

(defmacro declare-result-func-test (func)
  (let ((macro-name (concat-symbols func '-test))
        (test-name-base (string (concat-symbols 'test- func))))
    `(defmacro ,macro-name (id the-list result)
       (with-test-name ,test-name-base
         `(deftest ,test-name ()
                  (equal (,',func '(,@the-list)) '(,@result)))))))

(defun my-flatten (the-list)
  (cond ((null the-list) nil)
        ((consp the-list) (append (my-flatten (car the-list))
                                  (my-flatten (cdr the-list))))
        (t (list the-list))))

(declare-result-func-test my-flatten)

(my-flatten-test "null" () ())
(my-flatten-test "one-elem" (a) (a))
(my-flatten-test "test3" (a b) (a b))
(my-flatten-test "test4" ((((a b)))) (a b))
(my-flatten-test "test5" ((a b) (d e)) (a b d e))
(my-flatten-test "test6" ((a b) (d e) ((g hello) h)) (a b d e g hello h))

(declare-result-func-test compress)

(defun compress (mylist)
  (if (null mylist)
    nil
    (let ((reclist (compress (rest mylist)))
          (item (first mylist)))
      (if (eq item (car reclist))
        reclist
        (cons item reclist)))))

(compress-test "null" () ())
(compress-test "single" (a) (a))
(compress-test "two-diff" (a b) (a b))
(compress-test "dup2" (a a) (a))
(compress-test "dup3" (a a a) (a))
(compress-test "aaab" (a a a b) (a b))
(compress-test "aaabba" (a a a b b a) (a b a))
(compress-test "grand-finale" (a a a a b c c a a d e e e e) (a b c a d e))

(declare-result-func-test pack)

(defun pack (mylist)
  (if (null mylist)
    nil
    (let ((reclist (pack (rest mylist)))
          (item (first mylist)))
       (if (eq item (caar reclist))
         (cons (cons item (car reclist)) (rest reclist))
         (cons (cons item nil) reclist)))))

(pack-test "null" () ())
(pack-test "single" (a) ((a)))
(pack-test "two-diff" (a b) ((a) (b)))
(pack-test "dup2" (a a) ((a a)))
(pack-test "dup3" (a a a) ((a a a)))
(pack-test "aaab" (a a a b) ((a a a) (b)))
(pack-test "aaabba" (a a a b b a) ((a a a) (b b) (a)))
(pack-test "grand-finale" (a a a a b c c a a d e e e e)
           ((a a a a) (b) (c c) (a a) (d) (e e e e)))

(declare-result-func-test encode)

(defun encode (mylist)
  (mapcar #'(lambda (sublist) (list (length sublist) (car sublist)))
          (pack mylist)))

(encode-test "null" () ())
(encode-test "single" (a) ((1 a)))
(encode-test "two-diff" (a b) ((1 a) (1 b)))
(encode-test "dup2" (a a) ((2 a)))
(encode-test "dup3" (a a a) ((3 a)))
(encode-test "aaab" (a a a b) ((3 a) (1 b)))
(encode-test "aaabba" (a a a b b a) ((3 a) (2 b) (1 a)))
(encode-test "grand-finale" (a a a a b c c a a d e e e e)
             ((4 a) (1 b) (2 c) (2 a) (1 d) (4 e)))

(declare-result-func-test encode-modified)

(defun encode-modified (mylist)
  (mapcar #'(lambda (pair) (if (= (car pair) 1) (cadr pair) pair))
          (encode mylist)))

(encode-modified-test "null" () ())
(encode-modified-test "single" (a) (a))
(encode-modified-test "two-diff" (a b) (a b))
(encode-modified-test "dup2" (a a) ((2 a)))
(encode-modified-test "dup3" (a a a) ((3 a)))
(encode-modified-test "aaab" (a a a b) ((3 a) b))
(encode-modified-test "aaabba" (a a a b b a) ((3 a) (2 b) a))
(encode-modified-test "grand-finale" (a a a a b c c a a d e e e e)
             ((4 a) b (2 c) (2 a) d (4 e)))

(declare-result-func-test decode-modified)

(defmacro reverse-decode-modified-test (name out in)
  `(decode-modified-test ,name ,in ,out))

(defun decode-modified (mylist)
  (labels ((get-item (pair)
                     (if (consp pair) (cadr pair) pair))
           (get-count (pair)
                      (if (consp pair) (car pair) 1))
           (helper (myrest item mycount)
                   (cond ((and (null myrest) (zerop mycount)) nil)
                         ((zerop mycount) (helper (rest myrest)
                                                  (get-item (first myrest))
                                                  (get-count (first myrest))))
                         (t (cons item (helper myrest item (1- mycount)))))))
        (helper mylist 0 0)))

(reverse-decode-modified-test "null" () ())
(reverse-decode-modified-test "single" (a) (a))
(reverse-decode-modified-test "two-diff" (a b) (a b))
(reverse-decode-modified-test "dup2" (a a) ((2 a)))
(reverse-decode-modified-test "dup3" (a a a) ((3 a)))
(reverse-decode-modified-test "aaab" (a a a b) ((3 a) b))
(reverse-decode-modified-test "aaabba" (a a a b b a) ((3 a) (2 b) a))
(reverse-decode-modified-test "grand-finale" (a a a a b c c a a d e e e e)
             ((4 a) b (2 c) (2 a) d (4 e)))


;;; P13
(declare-result-func-test encode-direct)

(defun encode-direct (mylist)
  (flet ((normalize-pair (pair)
                   (cond ((consp pair) pair)
                         (t (list 1 pair)))))
    (if (null mylist)
      nil
      (let ((reclist (encode-direct (rest mylist))))
        (if (null reclist)
          (cons (first mylist) nil)
          ; Else:
          (let ((pair (normalize-pair (first reclist)))
                (elem (first mylist)))
            (if (eq (cadr pair) elem)
              (cons (list (1+ (car pair)) elem) (rest reclist))
              (cons elem reclist))))))))

(encode-direct-test "null" () ())
(encode-direct-test "single" (a) (a))
(encode-direct-test "two-diff" (a b) (a b))
(encode-direct-test "dup2" (a a) ((2 a)))
(encode-direct-test "dup3" (a a a) ((3 a)))
(encode-direct-test "aaab" (a a a b) ((3 a) b))
(encode-direct-test "aaabba" (a a a b b a) ((3 a) (2 b) a))
(encode-direct-test "grand-finale" (a a a a b c c a a d e e e e)
             ((4 a) b (2 c) (2 a) d (4 e)))

;;; P14
(defun dupli (mylist)
  (if (null mylist)
    nil
    (cons (car mylist) (cons (car mylist) (dupli (cdr mylist))))))

(declare-result-func-test dupli)

(dupli-test "null" () ())
(dupli-test "one-elem" (a) (a a))
(dupli-test "double-elem" (a a) (a a a a))
(dupli-test "two-diff" (a b) (a a b b))
(dupli-test "three-elem" (a b c) (a a b b c c))
(dupli-test "four-with-two-same" (a b b c) (a a b b b b c c))

(defmacro declare-multi-result-func-test (func)
  (let ((macro-name (concat-symbols func '-test))
        (test-name-base (string (concat-symbols 'test- func))))
    `(defmacro ,macro-name (id input result)
       (with-test-name ,test-name-base
         `(deftest ,test-name ()
                  (equal (,',func ,@input) '(,@result)))))))

(defmacro list-proc-helper (args body)
  (let ((arg-names (mapcar #'car args))
        (arg-init-vals (mapcar #'cadr args)))
    `(labels ((helper (myrest ,@arg-names)
                      (if (null myrest)
                        ()
                        ,body)))
       (helper the-list ,@arg-init-vals))))

(defun repli (the-list times)
  (list-proc-helper ((mycount times)) 
                    (if (zerop mycount)
                      (helper (rest myrest) times)
                      (cons (first myrest) (helper myrest (1- mycount))))))

(declare-multi-result-func-test repli)

(repli-test "null1" (() 1) ())
(repli-test "null10" (() 10) ())
(repli-test "three-elem-time-1" ('(a b c) 1) (a b c))
(repli-test "three-elem-time-2" ('(a b c) 2) (a a b b c c))
(repli-test "four-elem-time-3" ('(a b 1 c) 3) (a a a b b b 1 1 1 c c c))

(defun drop (the-list times)
  (list-proc-helper ((c (1- times)))
                    (if (= c 0)
                       (helper (rest myrest) (1- times))
                       (cons (first myrest)
                             (helper (rest myrest) (1- c))))))

(declare-multi-result-func-test drop)

(drop-test "null1" (() 1) ())
(drop-test "null10" (() 10) ())
(drop-test "drop-more-than-exist" ('(a b c) 4) (a b c))
(drop-test "2-out-of-7" ('(1 2 3 4 5 6 7) 2) (1 3 5 7))
(drop-test "3-out-of-7" ('(1 2 3 4 5 6 7) 3) (1 2 4 5 7))

(declare-multi-result-func-test split)

(defun split (the-list len)
  (cond ((null the-list) (list () ()))
        ((zerop len) (list () the-list))
        (t (let ((ret (split (cdr the-list) (1- len))))
                (list (cons (car the-list) (car ret))
                      (cadr ret))))))

(split-test "null0" (() 0) (() ()))
(split-test "null5" (() 5) (() ()))
(split-test "3-2" ('(a b c) 2) ((a b) (c)))
(split-test "3-5" ('(a b c) 5) ((a b c) ()))
(split-test "a-k-3"  ('(a b c d e f g h i k) 3) ((a b c) ( d e f g h i k)))

(declare-multi-result-func-test slice)

(defun slice (the-list start end)
  (car (split (cadr (split the-list (1- start))) (1+ (- end start)))))

(slice-test "a-k-3-7" ('(a b c d e f g h i k) 3 7) (c d e f g))
(slice-test "a-k-1-7" ('(a b c d e f g h i k) 1 7) (a b c d e f g))

(defun rotate (the-list num-places)
  (let* ((l (length the-list))
         (place (mod num-places l))
         (splitted (split the-list place)))
    (append (cadr splitted) (car splitted))))

(declare-multi-result-func-test rotate)

(rotate-test "a-h-3" ('(a b c d e f g h) 3) (d e f g h a b c))
(rotate-test "a-h-minus-2" ('(a b c d e f g h) -2) (g h a b c d e f))

(cybertiggyr-test:run)
