;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-ALREADY-VOTED (err u101))
(define-constant ERR-INVALID-TIME (err u102))
(define-constant ERR-VOTING-ENDED (err u103))
(define-constant ERR-VOTING-NOT-STARTED (err u104))
(define-constant ERR-CANDIDATE-NOT-FOUND (err u105))
(define-constant ERR-MAX-CANDIDATES-REACHED (err u106))

;; Define list of candidates with their names and votes, and initialize it to an empty list
(define-data-var candidates (list 10 (tuple (name (buff 20)) (votes uint))) (list))

;; Define a map to store votes, with the voter's address as the key and a boolean value to indicate if they have voted
(define-map votes-map {voter: principal} bool)

;; Define the start and end height of the voting period
(define-data-var voting-start-height uint u0)
(define-data-var voting-end-height uint u0)

;; Flag to check if the voting period has been initialized
(define-data-var is-initialized bool false)

;; Define the contract owner
(define-constant CONTRACT-OWNER tx-sender)

;; Function to initialize the voting period
(define-public (initialize-voting-period (start-block uint) (duration uint))
    (begin
        ;; Check if caller is contract owner
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        ;; Check if voting period hasn't been initialized yet
        (asserts! (not (var-get is-initialized)) ERR-INVALID-TIME)
        ;; Check if start block is in the future
        (asserts! (> start-block (block-height)) ERR-INVALID-TIME)
        ;; Check if duration is greater than 0
        (asserts! (> duration u0) ERR-INVALID-TIME)
        
        ;; Initialize the voting period
        (var-set voting-start-height start-block)
        (var-set voting-end-height (+ start-block duration))
        (var-set is-initialized true)
        (ok true)))

;; Function to check if voting is active
(define-read-only (is-voting-active)
    (let (
        ;; Get current block height
        (current-height (block-height))
        ;; Get voting period start and end heights
        (start-height (var-get voting-start-height))
        (end-height (var-get voting-end-height))
    )
    (and 
        ;; Current height should be greater than or equal to start height
        ;; and less than or equal to end height
        (>= current-height start-height)
        (<= current-height end-height))))

;; Function to add a candidate to the list
(define-public (add-candidate (candidate-name (buff 20)))
    (begin
        ;; Check if caller is contract owner
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        ;; Check if voting hasn't started yet
        (asserts! (< (block-height) (var-get voting-start-height)) ERR-VOTING-STARTED)
        ;; Check if we haven't reached maximum candidates
        (asserts! (< (len (var-get candidates)) u10) ERR-MAX-CANDIDATES-REACHED)
        
        ;; Add the new candidate
        (var-set candidates (cons {name: candidate-name, votes: u0} (var-get candidates)))
        (ok candidate-name)))

;; Function to submit a vote for a specific candidate
(define-public (submit-vote (candidate-name (buff 20)))
    (begin
        ;; Check if voting is active
        (asserts! (is-voting-active) ERR-INVALID-TIME)
        ;; Check if voter hasn't voted before
        (asserts! (is-none (map-get? votes-map {voter: tx-sender})) ERR-ALREADY-VOTED)
        
        ;; Find and update candidate's vote count
        (let ((updated-candidates 
                (map (lambda (candidate)
                    ;; Check if candidate is the one we're looking for
                    (if (buff-eq? candidate-name (get name candidate))
                        ;; If yes, increment vote count
                        {name: (get name candidate), votes: (+ (get votes candidate) u1)}
                        candidate))
                    ;; Get the list of candidates
                    (var-get candidates))))
            
            ;; Update candidates list
            (var-set candidates updated-candidates)
            ;; Mark voter as having voted
            (map-set votes-map {voter: tx-sender} true)
            (ok true))))

;; Function to get the results of the voting
(define-read-only (get-results)
    (ok {
        candidates: (var-get candidates), ;; Get list of candidates
        voting-status: (is-voting-active), ;; Get voting status
    }))

;; Function to get voting period information
(define-read-only (get-voting-period-info)
    (ok {
        start-height: (var-get voting-start-height), ;; Get voting start height
        end-height: (var-get voting-end-height), ;; Get voting end height
        current-height: (block-height), ;; Get current block height
        is-active: (is-voting-active), ;; Get voting status
        time-remaining: (if (< (block-height) (var-get voting-end-height))
                           (- (var-get voting-end-height) (block-height))
                           u0) ;; Get time remaining in blocks
    }))

;; Function to check if a voter has already voted
(define-read-only (has-voted (address principal))
    ;; Check if address has voted by looking up the address in the votes map
    (ok (default-to false (map-get? votes-map {voter: address}))))

;; Function to get the number of candidates
(define-read-only (get-candidate-count)
    ;; Return the number of candidates in the list
    (ok (len (var-get candidates))))