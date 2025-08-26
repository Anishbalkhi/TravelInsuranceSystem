;; Travel Insurance System
;; A comprehensive travel insurance smart contract covering cancellations, medical emergencies, and lost luggage

;; Define the contract constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-policy-holder (err u101))
(define-constant err-insufficient-premium (err u102))
(define-constant err-invalid-policy (err u103))
(define-constant err-policy-expired (err u104))
(define-constant err-claim-already-processed (err u105))
(define-constant err-invalid-claim-type (err u106))

;; Policy types and coverage limits
(define-constant CANCELLATION-COVERAGE u500000) ;; 5000 STX in microSTX
(define-constant MEDICAL-COVERAGE u1000000)     ;; 10000 STX in microSTX  
(define-constant LUGGAGE-COVERAGE u100000)      ;; 1000 STX in microSTX

;; Premium rates (in microSTX)
(define-constant BASE-PREMIUM u50000)           ;; 500 STX base premium

;; Policy structure
(define-map policies 
  principal 
  {
    policy-id: uint,
    premium-paid: uint,
    coverage-start: uint,
    coverage-end: uint,
    cancellation-covered: bool,
    medical-covered: bool,
    luggage-covered: bool,
    active: bool
  })

;; Claims tracking
(define-map claims
  {policy-holder: principal, claim-id: uint}
  {
    claim-type: (string-ascii 20),
    amount: uint,
    processed: bool,
    approved: bool,
    claim-date: uint
  })

;; Contract state variables
(define-data-var total-policies uint u0)
(define-data-var total-claims uint u0)
(define-data-var total-premiums-collected uint u0)

;; Function 1: Purchase Travel Insurance Policy
(define-public (purchase-policy (coverage-duration uint) (cancellation bool) (medical bool) (luggage bool))
  (let 
    (
      (policy-id (+ (var-get total-policies) u1))
      (current-time stacks-block-height)
      (coverage-end (+ current-time coverage-duration))
      (premium-amount (calculate-premium cancellation medical luggage coverage-duration))
    )
    (begin
      ;; Validate inputs
      (asserts! (> coverage-duration u0) err-invalid-policy)
      (asserts! (or cancellation medical luggage) err-invalid-policy)
      
      ;; Transfer premium from user to contract
      (try! (stx-transfer? premium-amount tx-sender (as-contract tx-sender)))
      
      ;; Create policy record
      (map-set policies tx-sender
        {
          policy-id: policy-id,
          premium-paid: premium-amount,
          coverage-start: current-time,
          coverage-end: coverage-end,
          cancellation-covered: cancellation,
          medical-covered: medical,
          luggage-covered: luggage,
          active: true
        })
      
      ;; Update contract state
      (var-set total-policies policy-id)
      (var-set total-premiums-collected (+ (var-get total-premiums-collected) premium-amount))
      
      ;; Print policy details for events
      (print {
        action: "policy-purchased",
        policy-holder: tx-sender,
        policy-id: policy-id,
        premium: premium-amount,
        coverage-types: {cancellation: cancellation, medical: medical, luggage: luggage}
      })
      
      (ok policy-id)
    )))

;; Function 2: Submit Insurance Claim
(define-public (submit-claim (claim-type (string-ascii 20)) (claim-amount uint))
  (let 
    (
      (policy-data (unwrap! (map-get? policies tx-sender) err-invalid-policy))
      (claim-id (+ (var-get total-claims) u1))
      (current-time stacks-block-height)
      (max-coverage (get-max-coverage claim-type))
    )
    (begin
      ;; Validate policy exists and is active
      (asserts! (get active policy-data) err-invalid-policy)
      (asserts! (<= current-time (get coverage-end policy-data)) err-policy-expired)
      (asserts! (> claim-amount u0) err-invalid-claim-type)
      
      ;; Validate claim type is covered
      (asserts! (is-coverage-valid claim-type policy-data) err-invalid-claim-type)
      
      ;; Validate claim amount doesn't exceed coverage
      (asserts! (<= claim-amount max-coverage) err-invalid-claim-type)
      
      ;; Create claim record
      (map-set claims {policy-holder: tx-sender, claim-id: claim-id}
        {
          claim-type: claim-type,
          amount: claim-amount,
          processed: false,
          approved: false,
          claim-date: current-time
        })
      
      ;; Update total claims
      (var-set total-claims claim-id)
      
      ;; Print claim details for events
      (print {
        action: "claim-submitted",
        policy-holder: tx-sender,
        claim-id: claim-id,
        claim-type: claim-type,
        amount: claim-amount
      })
      
      (ok claim-id)
    )))

;; Helper function: Calculate premium based on coverage options
(define-private (calculate-premium (cancellation bool) (medical bool) (luggage bool) (duration uint))
  (let 
    (
      (base-cost BASE-PREMIUM)
      (cancellation-cost (if cancellation u20000 u0))  ;; 200 STX
      (medical-cost (if medical u30000 u0))            ;; 300 STX  
      (luggage-cost (if luggage u10000 u0))            ;; 100 STX
      (duration-multiplier (if (> duration u1000) u2 u1)) ;; Double cost for long trips
    )
    (* (+ base-cost cancellation-cost medical-cost luggage-cost) duration-multiplier)
  ))

;; Helper function: Get maximum coverage for claim type
(define-private (get-max-coverage (claim-type (string-ascii 20)))
  (if (is-eq claim-type "cancellation")
    CANCELLATION-COVERAGE
    (if (is-eq claim-type "medical")
      MEDICAL-COVERAGE  
      (if (is-eq claim-type "luggage")
        LUGGAGE-COVERAGE
        u0))))

;; Helper function: Validate if claim type is covered by policy
(define-private (is-coverage-valid (claim-type (string-ascii 20)) (policy {policy-id: uint, premium-paid: uint, coverage-start: uint, coverage-end: uint, cancellation-covered: bool, medical-covered: bool, luggage-covered: bool, active: bool}))
  (if (is-eq claim-type "cancellation")
    (get cancellation-covered policy)
    (if (is-eq claim-type "medical")
      (get medical-covered policy)
      (if (is-eq claim-type "luggage")
        (get luggage-covered policy)
        false))))

;; Read-only functions for policy and claim information
(define-read-only (get-policy (policy-holder principal))
  (map-get? policies policy-holder))

(define-read-only (get-claim (policy-holder principal) (claim-id uint))
  (map-get? claims {policy-holder: policy-holder, claim-id: claim-id}))

(define-read-only (get-contract-stats)
  {
    total-policies: (var-get total-policies),
    total-claims: (var-get total-claims),
    total-premiums: (var-get total-premiums-collected)
  })

;; Owner-only function to process claims (approve/deny)
(define-public (process-claim (policy-holder principal) (claim-id uint) (approved bool))
  (let 
    (
      (claim-key {policy-holder: policy-holder, claim-id: claim-id})
      (claim-data (unwrap! (map-get? claims claim-key) err-invalid-claim-type))
    )
    (begin
      (asserts! (is-eq tx-sender contract-owner) err-owner-only)
      (asserts! (not (get processed claim-data)) err-claim-already-processed)
      
      ;; Update claim status
      (map-set claims claim-key
        (merge claim-data {processed: true, approved: approved}))
      
      ;; If approved, transfer claim amount to policy holder
      (if approved
        (try! (as-contract (stx-transfer? (get amount claim-data) tx-sender policy-holder)))
        true)
      
      (print {
        action: "claim-processed",
        policy-holder: policy-holder,
        claim-id: claim-id,
        approved: approved,
        amount: (get amount claim-data)
      })
      
      (ok approved)
    )))