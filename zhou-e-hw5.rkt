;Eric Zhou, ezzhou
;Intermediate Student Custom

; Part One


; (1.)

(define-struct river (name pH DO-lvl tributaries))
; A river is a (make-river String Number Number ListOfRiver) where
; name is the name of the river
; pH is the pH level of the water
; DO-lvl is the dissolved oxygen levels in parts per million
; tributaries is a list of rivers that feed into the main river
; 
; A ListOfRiver is one of
; -empty
; -(cons River ListOfRiver)


; (2.)

(define YUKON (make-river "Yukon" 10 128 empty))
(define MISSOURI (make-river "Missouri" 4 9 empty))
(define MISSISSIPPI (make-river "Mississippi" 7 7 empty))
(define COLORADO (make-river "Colorado" 8.5 6 empty))
(define HUDSON (make-river "Hudson" 1 6.7 (list YUKON MISSOURI)))
(define CHARLES (make-river "Charles" 7.5 8 (list MISSISSIPPI HUDSON)))

; (3.)

; ;river-fnc: River -> ...
; (define (river-fcn a-river)
;   (... (river-name a-river)
;        (river-pH-lvl a-river)
;        (river-DO-lvl a-river)
;        (lor-fcn (river-tributaries a-river))))
; 
; ;lor-fcn: ListOfRiver ->
; (define (lor-fcn a-lor)
;   (cond
;     [(empty? a-lor) (...)]
;     [(cons? a-lor)  (... (river-fcn (first a-lor))
;                          (lor-fcn (rest a-lor)))]))


; (4.)

;Signature: River Number -> ListOfString
;Purpose: Returns a list of all the names of rivers whose pH level is less than a given level
(define (lower-ph-than a-river pH)
  (if (< (river-pH a-river) pH)
      (cons (river-name a-river) (lower-ph-than-list (river-tributaries a-river) pH))
      (lower-ph-than-list (river-tributaries a-river) pH)))

(check-expect (lower-ph-than CHARLES 12) (list "Charles" "Mississippi" "Hudson" "Yukon" "Missouri"))
(check-expect (lower-ph-than HUDSON 7) (list "Hudson" "Missouri"))
(check-expect (lower-ph-than YUKON 15) (list "Yukon"))
(check-expect (lower-ph-than MISSISSIPPI 0) empty)

;Signature: ListOfRiver Number -> ListOfString
;Purpose: Returns a list of all the names of rivers whose pH level is less than a given level
(define (lower-ph-than-list a-lor pH)
  (cond
    [(empty? a-lor) empty]
    [(cons? a-lor) (append (lower-ph-than (first a-lor) pH) (lower-ph-than-list (rest a-lor) pH))]))

(check-expect (lower-ph-than-list (river-tributaries CHARLES) 7) (list "Hudson" "Missouri"))
(check-expect (lower-ph-than-list (list CHARLES) 15) (list "Charles" "Mississippi" "Hudson" "Yukon" "Missouri"))

; (5.)

;Signature: River -> Boolean
;Purpose: Returns whether or not every river in the system has a pH between 6.5 and 8.5, and a DO of at least 6ppm
(define (healthy? a-river)
  (and (>= (river-pH a-river) 6.5) (<= (river-pH a-river) 8.5) (>= (river-DO-lvl a-river) 6) (healthy-list? (river-tributaries a-river))))

(check-expect (healthy? CHARLES) #false)
(check-expect (healthy? MISSISSIPPI) #true)
(check-expect (healthy? YUKON) #false)

;Signature: ListOfRiver -> Boolean
;Purpose: Returns whether every river in the system a pH between 6.5 and 8.5, and a DO of at least 6ppm
(define (healthy-list? a-lor)
  (cond
    [(empty? a-lor) #true]
    [(cons? a-lor) (and (healthy? (first a-lor)) (healthy-list? (rest a-lor)))]))

(check-expect (healthy-list? (river-tributaries CHARLES)) #false)
(check-expect (healthy-list? (list MISSISSIPPI COLORADO)) #true)

; (6.)

;Signature: River -> River
;Purpose: Returns a new River system where all the pH levels are reduced by 0.1
(define (lower-all-ph a-river)
  (make-river (river-name a-river) (- (river-pH a-river) 0.1) (river-DO-lvl a-river) (lower-all-ph-list (river-tributaries a-river))))

(check-expect (lower-all-ph YUKON) (make-river "Yukon" 9.9 128 empty))
(check-expect (lower-all-ph CHARLES) (make-river "Charles" 7.4 8 (list (make-river "Mississippi" 6.9 7 empty) (make-river "Hudson" 0.9 6.7 (list (make-river "Yukon" 9.9 128 empty) (make-river "Missouri" 3.9 9 empty))))))

;Signature: ListOfRiver -> River
;Purpose: Returns a new ListOfRiver where all the pH levels are reduced by 0.1
(define (lower-all-ph-list a-lor)
  (cond
    [(empty? a-lor) empty]
    [(cons? a-lor) (cons (lower-all-ph (first a-lor)) (lower-all-ph-list (rest a-lor)))]))

(check-expect (lower-all-ph-list (list MISSISSIPPI COLORADO)) (list (make-river "Mississippi" 6.9 7 empty) (make-river "Colorado" 8.4 6 empty)))

; Part Two


(define-struct volunteer-org (type name age consent? license? training hours languages))
; VolunteerOrg represents information about a volunteer organization, where:
;   type is the type of organization
;   name is the name of the organization
;   age is the minimum age required for volunteering
;   consent? is true if parental consent is needed for volunteers under 18
;   license? is true if a valid driver's license is required
;   training is the number of hours of training required prior to volunteering
;   hours is the minimum number of volunteer hours required per week
;   languages is a list of the languages spoken by clients of the organization
; 
; ListOfVolunteerOrg is one of:
;   -empty
;   -(cons VolunteerOrg ListOfVolunteerOrg)


(define org1 (make-volunteer-org "animal shelter" "Catdog." 12 #true #true 12 10 (list "English")))
(define org2 (make-volunteer-org "soup kitchen" "Mama Mias Delicious Ravioli" 13 #false #false 0 14 (list "Italian" "English" "French" "Spanish")))
(define org3 (make-volunteer-org "nursing home" "Boomer Residencies" 21 #false #true 30 40 (list "English" "Chinese" "Hindi" "Spanish" "Arabic")))
(define org4 (make-volunteer-org "nursing home" "The Falls at Cordingly Dam" 24 #false #true 120 40 (list "English" "Spanish")))

(define L1 (list org1 org2 org3 org4))

; (7.)

;Signature: ListOfVolunteerOrg Number -> ListOfVolunteerOrg
;Purpose: Returns a new ListOfVolunteerOrg where all Volunteer Orgs require a drivers license and require less than the specified training hours
(define (list-license-training a-lovo hours)
  (local
    [(define (list-license-training? a-volunteer) (and (volunteer-org-license? a-volunteer) (< (volunteer-org-training a-volunteer) hours)))]
    (filter list-license-training? a-lovo)))

(check-expect (list-license-training L1 15) (list org1))
(check-expect (list-license-training L1 100) (list org1 org3))
(check-expect (list-license-training L1 128) (list org1 org3 org4))

; (8.)

;Signature: ListOfVolunteerOrg Natural -> ListOfString
;Purpose: Returns a list of the names of all organizations that accept volunteers of a specified age
(define (names-by-age a-lovo age)
  (local
    [(define (valid-age? a-volunteer) (>= age (volunteer-org-age a-volunteer)))
     (define (org-name a-volunteer) (volunteer-org-name a-volunteer))]
    (map org-name (filter valid-age? a-lovo))))

(check-expect (names-by-age L1 69) (list "Catdog." "Mama Mias Delicious Ravioli" "Boomer Residencies" "The Falls at Cordingly Dam"))
(check-expect (names-by-age L1 18) (list "Catdog." "Mama Mias Delicious Ravioli"))
(check-expect (names-by-age L1 12) (list "Catdog."))

; (9.)

;Signature: ListOfLanguages -> Boolean
;Returns whether or not a organization has clients that speak Spanish
(define (contains-spanish? a-lol)
  (cond
    [(empty? a-lol) #false]
    [(cons? a-lol) (if (string-ci=? "Spanish" (first a-lol))
                       #true
                       (contains-spanish? (rest a-lol)))]))

(check-expect (contains-spanish? (volunteer-org-languages org1)) #false)
(check-expect (contains-spanish? (volunteer-org-languages org3)) #true)

;Signature: ListOfVolunteerOrg -> ListOfVolunteerOrg
;Returns a list of volunteer organizations who have Spanish as one of the languages spoken
(define (need-spanish-speakers a-lovo)
  (local
    [(define (speak-spanish? a-volunteer) (contains-spanish? (volunteer-org-languages a-volunteer)))]
    (filter speak-spanish? a-lovo)))

(check-expect (need-spanish-speakers L1) (list org2 org3 org4))
(check-expect (need-spanish-speakers (list org1)) empty)
