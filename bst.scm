;Tom Parker
;Student Number a1608020



;STEP ONE
(define (insert x tree)
  ;x is an element that can be compared with the '<' operator
  ;tree is a binary search tree of elements with the same type as x
  ;will return a BST with x inserted into it
  (if (null? tree)       (list x (list) (list))         ;base case
  (if (equal? tree)      tree                           ;finds duplicates
  (if (null? (car tree)) (list x (list) (list))         ;another base case
  (if (> x (car tree))   (list (car tree) (cadr tree) (insert x (caddr tree)))  ;recursive calls
                         (list (car tree) (insert x (cadr tree)) (caddr tree)))
                         ))))
                         
                         
                         
(define (print-tree tree)
  ;tree is a binary tree of any type
  ;returns nothing but prints the tree
  ;NOTE: This is not purly applicative as begin is used
  (if (eqv? (null? tree) #f)      ;'if tree is not null'
  (begin
    (display " (")
    (display (car tree))
    (print-tree (cadr tree))
    (print-tree (caddr tree))
    (display ")")
  )
  )
)

;STEP TWO
;same as 'insert' except with 'order'
;order is a function with compares the elements of the tree
(define (insert2 order x tree)
  (if (null? tree)           (list x (list) (list))
  (if (null? (car tree))     (list x (list) (list))
  (if (equal? (car tree) x)  tree
  (if (order x (car tree))   (list (car tree) (cadr tree) (insert2 order x (caddr tree)))
                             (list (car tree) (insert2 order x (cadr tree)) (caddr tree)))
                             ))))
                            
;STEP THREE
;order is a function which compares elements of the tree
;entry is a list of elements which are compared by order
;mktree returns a BST with the elements in entry ordered by the function order
(define (mktree order entry)
  (define (maker unordered ordered)             ;recursive helper function. Takes elements from unordered and places them in ordered
    (if (null? unordered) ordered               ;base condition, exits when unordered is empty
    (maker (cdr unordered) (insert2 order (car unordered) ordered))))
  (maker entry (list))
  )


;STEP FOUR
;The traversals, bascially the same. Tree is a BST of any type. Will return a list of elements from the tree that are ordered.
(define (in-order tree)
  (if (null? tree) '() (append (in-order (cadr tree)) (list(car tree)) (in-order(caddr tree)))
  )
)

(define (pre-order tree)
  (if (null? tree) '() (append (list(car tree)) (pre-order (cadr tree)) (pre-order(caddr tree)))
  )
)

(define (post-order tree)
  (if (null? tree) '() (append (post-order (cadr tree)) (post-order(caddr tree)) (list(car tree)))
  )
)


;STEP FIVE
;;;;;;;;;;Tests Begin
(define elements (list 2 6 8 14 6 8 2 3 4 1 10 10 10 10))
(define large (list 37 14 5 29 26 47 45 42 17 31 50 9 23 2 39 35 16 36 46 30 28 34 43 25 4 48 20 41 11 6 19 49 10 15 3 24 27 21 22 44 1 33 32 38 8 18 7 40 12 13))
(define words (list "The" "quick" "fox" "jumped" "over" "the" "lazy" "dog"))
(define (ascending a b) (> a b))
(define (decending a b) (< a b))
(define (str a b) (< (string-length a) (string-length b)))



(display "The elements that are going to be in the tree: ")
(display elements)
(newline)
(newline)
(define t1 (mktree ascending elements))     ;testing putting duplicates in the tree
(display "The tree in ascending order: ")
(print-tree t1)                                ;displaying the tree
(newline)
(display "Printing the tree in-order: ")
(display (in-order t1))                     ;sorting the tree
(newline)
(display "Empty tree in-order: ")
(display (in-order (mktree ascending (list))))
(newline)
(display "Tree from before in post-order: ")
(display (post-order t1))
(newline)
(display "A tree made with the same elements but in decending order: ")
(print-tree (mktree decending elements))
(newline)
(display "That tree printed in-order: ")
(display (in-order (mktree decending elements)))
(newline)
(display "A very large binary search tree: ")
(print-tree (mktree ascending large))
(newline)
(display "The large tree printed in-order: ")
(display (in-order (mktree ascending large)))
(newline)
(display "The large tree printed post-order: ")
(display (post-order (mktree ascending large)))
(newline)
(newline)
(display "A list of words sorted by size")
(newline)
(display "The list: ")
(display words)
(newline)
(define t2 (mktree str words))
(display "In tree form: ")
(print-tree t2)
(newline)
(display "Printed in-order: ")
(display (in-order t2))
(newline)
(display "Printed pre-order: ")
(display (pre-order t2))
(newline)
(display "Printed post-order: ")
(display (post-order t2))

