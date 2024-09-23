#lang scheme
; ---------------------------------------------
; DO NOT REMOVE OR CHANGE ANYTHING UNTIL LINE 26
; ---------------------------------------------

; zipcodes.scm contains all the US zipcodes.
; This file must be in the same folder as hw2.scm file,
; and you should not modify it. Your code
; should work for other instances of this file.
(require "zipcodes.scm")

; Helper function
(define (mydisplay value)
	(display value)
	(newline)
)

; Helper function
(define (line func)
        (display "--------- ")
        (display func)
        (display " ------------")
        (newline)
)

; ================ Solve the following functions ===================
; Return a list with only the negatives items
(define (negatives lst)
  (if (null? lst) ; Checks if the list is empty
      '()  ; Return an empty list if lst is empty
      (if (>= (car lst) 0)  ; Check if the first element is non-negative
          (negatives (cdr lst))  ; Skip the element and process the rest
          (cons (car lst) (negatives (cdr lst)))  ; Otherwise, include it
      )
  )
)

(line "negatives")
(mydisplay (negatives '()))  ; -> ()
(mydisplay (negatives '(-1)))  ; -> (-1)
(mydisplay (negatives '(-1 1 2 3 4 -4 5)))  ; -> (-1 -4)
(mydisplay (negatives '(1 1 2 3 4 4 5)))  ; -> ()
(line "negatives")
; ---------------------------------------------

; Returns true if the two lists have identical structure
; in terms of how many elements and nested lists they have in the same order
(define (struct lst1 lst2)
  (cond
    ((and (null? lst1) (null? lst2)) #t)  ; Both lists are empty
    ((or (null? lst1) (null? lst2)) #f)   ; One list is empty and the other is not
    ((and (list? (car lst1)) (list? (car lst2))) ; Both heads are lists
     (and (struct (car lst1) (car lst2)) (struct (cdr lst1) (cdr lst2)))) ; Recursively check structure of heads and tails
    ((and (not (list? (car lst1))) (not (list? (car lst2)))) ; Both heads are not lists
     (struct (cdr lst1) (cdr lst2))) ; Continue checking tails
    (else #f)) ; If one is a list and the other isn't
)

(line "struct")
(mydisplay (struct '(a b c (c a b)) '(1 2 3 (a b c))))  ; -> #t
(mydisplay (struct '(a b c d (c a b)) '(1 2 3 (a b c))))  ; -> #f
(mydisplay (struct '(a b c (c a b)) '(1 2 3 (a b c) 0)))  ; -> #f
(line "struct")
; ---------------------------------------------

; Returns a list of two numeric values. The first is the smallest
; in the list and the second is the largest in the list. 
; lst -- contains numeric values, and length is >= 1.
(define (minAndMax lst)
  (if (= (length lst) 1)
      (list (car lst) (car lst))  ; Base case: single element, return it as both min and max
      (let* ((rest (minAndMax (cdr lst)))  ; Recursive call on the rest of the list
             (current-min (min (car lst) (car rest)))  ; Compare current element with min of rest
             (current-max (max (car lst) (cadr rest))))  ; Compare current element with max of rest
        (list current-min current-max)  ; Return list of min and max
	   )
	)
)


(line "minAndMax")
(mydisplay (minAndMax '(1 2 -3 4 2)))  ; -> (-3 4)
(mydisplay (minAndMax '(1)))  ; -> (1 1)
(line "minAndMax")
; ---------------------------------------------

; Returns a list identical to the first list, while having all elements
; that are inside nested loops taken out. So we want to flatten all elements and have
; them all in a single list. For example '(a (a a) a))) should become (a a a a)
(define (flatten lst)
  (cond
    ((null? lst) 
     '())  ; Base case: if the list is empty, return an empty list
    ((list? (car lst))  ; If the first element is a list, recursively flatten it
     (append (flatten (car lst))
             (flatten (cdr lst))))
    (else  ; Otherwise, add the first element to the result of flattening the rest
     (cons (car lst)
           (flatten (cdr lst))))
  )
)


(line "flatten")
(mydisplay (flatten '(a b c)))  ; -> (a b c)
(mydisplay (flatten '(a (a a) a)))  ; -> (a a a a)
(mydisplay (flatten '((a b) (c (d) e) f)))  ; -> (a b c d e f)
(line "flatten")
; ---------------------------------------------

; The paramters are two lists. The result should contain the cross product
; between the two lists: 
; The inputs '(1 2) and '(a b c) should return a single list:
; ((1 a) (1 b) (1 c) (2 a) (2 b) (2 c))
; lst1 & lst2 -- two flat lists.
(define (crossproduct lst1 lst2)
  (if (null? lst1)
      '()  ; Base case: if the first list is empty, return an empty list
      (append
        (map (lambda (x) (list (car lst1) x)) lst2)
        (crossproduct (cdr lst1) lst2)
      )
  )
)

(line "crossproduct")
(mydisplay (crossproduct '(1 2) '(a b c)))
(line "crossproduct")
; ---------------------------------------------

; Returns the first latitude and longitude of a particular zip code.
; if there are multiple latitude and longitude pairs for the same zip code,
; the function should only return the first pair. e.g. (53.3628 -167.5107)
; zipcode -- 5 digit integer
; zips -- the zipcode DB- You MUST pass the 'zipcodes' function
; from the 'zipcodes.scm' file for this. You can just call 'zipcodes' directly
; as shown in the sample example
(define (getLatLon zipcode zips)
  (cond ; iterate over zipcodes in zipcodes.scm
    ((null? zips) #f) ; check if no zips left
    ((equal? zipcode (car (car zips)))); check if zipcode matches with equal?
    (list (cadddr (car zips)) (car (cddddr (car zips)))); if zipcode is match, return first pair of zips for that zipcode
	  (else (getLatLon zipcode (cdr zips))) ; Recurse with the rest of the list
  )
)

(line "getLatLon")
(mydisplay (getLatLon 45056 zipcodes))
(line "getLatLon")
; ---------------------------------------------

; Returns a list of all the place names common to two states.
; placeName -- is the text corresponding to the name of the place
; zips -- the zipcode DB
(define (getCommonPlaces state1 state2 zips)
  (define (places-in-state state zips)
    (map (lambda (entry) (cadr entry))  ; extract place name (second element)
         (filter (lambda (entry) (equal? (caddr entry) state)) zips)))  ; filter by state (third element)

  ;; Find the common places between two states, and remove duplicates
  (remove-duplicates
   (filter (lambda (place)
             (member place (places-in-state state2 zips)))  ; filter to keep common places
           (places-in-state state1 zips))))  ; get the places from state1

(line "getCommonPlaces")
(mydisplay (getCommonPlaces "OH" "MI" zipcodes))
(line "getCommonPlaces")
; ---------------------------------------------

; Returns the number of zipcode entries for a particular state.
; state -- state
; zips -- zipcode DB
(define (zipCount state zips)
  (define (state-matches? entry)
    (equal? (caddr entry) state))  ; Check if the third element (state) matches the given state
  (length (filter state-matches? zips)))  ; Filter the entries and count them

(line "zipCount")
(mydisplay (zipCount "OH" zipcodes))
(line "zipCount")
; ---------------------------------------------

; Some sample predicates
(define (POS? x) (> x 0))
(define (NEG? x) (< x 0))
(define (LARGE? x) (>= (abs x) 10))
(define (SMALL? x) (not (LARGE? x)))

; Returns a list of items that satisfy a set of predicates.
; For example (filterList '(1 2 3 4 100) '(EVEN?)) should return the even numbers (2 4 100)
; (filterList '(1 2 3 4 100) '(EVEN? SMALL?)) should return (2 4)
; lst -- flat list of items
; filters -- list of predicates to apply to the individual elements

(define (filterList lst filters)
  (define (satisfies-all? x filters)
    (foldl (lambda (pred acc) (and (pred x) acc)) #t filters))  ; Check all predicates
  (filter (lambda (x) (satisfies-all? x filters)) lst))  ; Apply filters to the list

(line "filterList")
(mydisplay (filterList '(1 2 3 11 22 33 -1 -2 -3 -11 -22 -33) (list POS?)))
(mydisplay (filterList '(1 2 3 11 22 33 -1 -2 -3 -11 -22 -33) (list POS? even?)))
(mydisplay (filterList '(1 2 3 11 22 33 -1 -2 -3 -11 -22 -33) (list POS? even? LARGE?)))
(line "filterList")
; ---------------------------------------------

; #### Only for Graduate Students ####
; Returns a list of all the place names common to a set of states.
; states -- is list of state names
; zips -- the zipcode DB
(define (getCommonPlaces2 states zips)
  ;; Helper function to extract place names for a given state
  (define (places-in-state state zips)
    (map (lambda (entry) (cadr entry))  ; extract place name (second element)
         (filter (lambda (entry) (equal? (caddr entry) state)) zips)))  ; filter by state (third element)

  ;; Initialize with places from the first state
  (let loop ((remaining-states (cdr states))  ; Start with the rest of the states
             (common-places (places-in-state (car states) zips)))  ; Start with places from the first state
    (if (null? remaining-states)
        (remove-duplicates common-places)  ; Remove duplicates from the final result
        (loop (cdr remaining-states)  ; Continue with the remaining states
              (filter (lambda (place)  ; Update common-places with the intersection
                        (member place (places-in-state (car remaining-states) zips)))
                      common-places)))))

(line "getCommonPlaces2")
(mydisplay (getCommonPlaces2 '("OH" "MI" "PA") zipcodes))
(line "getCommonPlaces2")

; ---------------------------------------------

; #### Only for Graduate Students ####
; Returns the distance between two zip codes in "meters".
; Use lat/lon. Do some research to compute this.
; You can find some info here: https://www.movable-type.co.uk/scripts/latlong.html
; zip1 & zip2 -- the two zip codes in question.
; zips -- zipcode DB
(define (getDistanceBetweenZipCodes zip1 zip2 zips)
  (let* ((lat-lon1 (find-lat-lon zip1 zips))  ; Get lat/lon for zip1
         (lat-lon2 (find-lat-lon zip2 zips))  ; Get lat/lon for zip2
         (lat1 (deg-to-rad (car lat-lon1)))
         (lon1 (deg-to-rad (cadr lat-lon1)))
         (lat2 (deg-to-rad (car lat-lon2)))
         (lon2 (deg-to-rad (cadr lat-lon2)))
         (dlat (- lat2 lat1))  ; Difference in latitude
         (dlon (- lon2 lon1))  ; Difference in longitude
         (a (+ (expt (sin (/ dlat 2)) 2)
               (* (cos lat1) (cos lat2) (expt (sin (/ dlon 2)) 2))))
         (c (* 2 (atan (sqrt a) (sqrt (- 1 a))))))  ; Use atan directly
    (* earth-radius-meters c)))  ; Distance in meters

; helper code for getDistanceBetweenZipCodes
(define pi 3.141592653589793)
(define earth-radius-meters 6371000)  ; Earth's radius in meters

; Converts degrees to radians
(define (deg-to-rad deg)
  (* deg (/ pi 180)))

; Helper function to find the lat/lon of a given zip code
(define (find-lat-lon zip zips)
  (let ((entry (first (filter (lambda (z) (equal? (car z) zip)) zips))))
    (if entry
        (list (cadr (cdddr entry))  ; Latitude (5th element)
              (caddr (cdddr entry))) ; Longitude (6th element)
        (error "Zip code not found")))) ; zip doesn't exist in list

; Scheme doesn't include this function but makes this easier, written here to use normally
(define (atan2 y x)
  (cond
    ((> x 0) (atan (/ y x)))  ; Quadrant I
    ((and (< x 0) (> y 0)) (+ (atan (/ y x)) pi))  ; Quadrant II
    ((and (< x 0) (< y 0)) (- (atan (/ y x)) pi))  ; Quadrant III
    ((= x 0) (if (> y 0) (/ pi 2) (- (/ pi 2))) )  ; Quadrant IV
    (else 0)))  ; Point is (0,0)

(line "getDistanceBetweenZipCodes")
(mydisplay (getDistanceBetweenZipCodes 45056 48122 zipcodes))
(line "getDistanceBetweenZipCodes")
; ---------------------------------------------



