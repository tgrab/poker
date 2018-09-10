(defclass game()
  ((pot :initform 0 :accessor pot)
   (stake :initform 0 :accessor stake)
   (blind :initform 0 :accessor blind)
   (balance :initform (make-array 10 :initial-element 0) :accessor balance)
   (total-balance :initform (make-array 10 :initial-element 0) :accessor total-balance)
   (history :initform nil :accessor history)
   (player-history :initform nil :accessor player-history)

   (free-seat :initform 0 :accessor free-seat)
   (agent-name :initform "grabola" :accessor agent-name)
   (agent-pos :initform nil :accessor agent-pos)
   (name->seat :initform nil :accessor name->seat)
   (taken-seats :initform (make-array 10 :initial-element nil) :accessor taken-seats)
   (active-players :initform (make-array 10 :initial-element nil) :accessor active-players)

   (table-cards :initform nil :accessor table-cards)
   (cards :initform nil :accessor cards)
   ;-------- Logic properties:
   (predicates :initform nil :allocation :class :accessor predicates)
   ;levels of memory:
   (rules :initform (make-hash-table) :allocation :class :accessor rules)
   (memo :initform (make-hash-table) :accessor memo)
   (game-memo :initform (make-hash-table) :accessor game-memo)
   (round-memo :initform (make-hash-table) :accessor round-memo)
   (percept-memo :initform (make-hash-table) :accessor percept-memo)   ))


(defvar +game+ (make-instance 'game))

