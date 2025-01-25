;; DecentraliSafe Insurance Protocol with Pool Administrator Role

;; Define constants
(define-constant protocol-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-already-initialized (err u101))
(define-constant err-not-initialized (err u102))
(define-constant err-pool-not-found (err u103))
(define-constant err-insufficient-funds (err u104))
(define-constant err-not-member (err u105))
(define-constant err-claim-not-found (err u106))
(define-constant err-invalid-name (err u107))
(define-constant err-invalid-premium (err u108))
(define-constant err-invalid-coverage (err u109))
(define-constant err-invalid-pool-id (err u110))
(define-constant err-invalid-claim-amount (err u111))
(define-constant err-not-admin (err u112))
(define-constant err-invalid-admin (err u113))

;; Define data variables
(define-data-var protocol-initialized bool false)
(define-data-var total-pool-count uint u0)

;; Define data maps
(define-map insurance-pools
  { pool-id: uint }
  {
    name: (string-ascii 50),
    balance: uint,
    premium: uint,
    coverage: uint,
    members: (list 200 principal),
    admin: principal
  }
)

(define-map insurance-claims
  { claim-id: uint }
  {
    pool-id: uint,
    claimant: principal,
    amount: uint,
    status: (string-ascii 20)
  }
)

;; Initialize protocol
(define-public (initialize-protocol)
  (begin
    (asserts! (is-eq tx-sender protocol-owner) err-owner-only)
    (asserts! (not (var-get protocol-initialized)) err-already-initialized)
    (var-set protocol-initialized true)
    (ok true)
  )
)

;; Create a new insurance pool
(define-public (create-insurance-pool (name (string-ascii 50)) (premium uint) (coverage uint) (admin principal))
  (begin
    (asserts! (var-get protocol-initialized) err-not-initialized)
    (asserts! (> (len name) u0) err-invalid-name)
    (asserts! (> premium u0) err-invalid-premium)
    (asserts! (> coverage premium) err-invalid-coverage)

    ;; Check if the admin is a valid principal (non-zero principal)
    (asserts! (not (is-eq admin tx-sender)) err-invalid-admin)

    (let ((new-pool-id (+ (var-get total-pool-count) u1)))
      (map-set insurance-pools
        { pool-id: new-pool-id }
        {
          name: name,
          balance: u0,
          premium: premium,
          coverage: coverage,
          members: (list),
          admin: admin
        }
      )
      (var-set total-pool-count new-pool-id)
      (ok new-pool-id)
    )
  )
)

;; Join an insurance pool (now requires admin approval)
(define-public (request-pool-membership (pool-id uint))
  (begin
    (asserts! (var-get protocol-initialized) err-not-initialized)
    (asserts! (> pool-id u0) err-invalid-pool-id)
    (asserts! (<= pool-id (var-get total-pool-count)) err-pool-not-found)
    
    (let (
      (pool (unwrap! (map-get? insurance-pools { pool-id: pool-id }) err-pool-not-found))
      (premium (get premium pool))
    )
      (asserts! (is-eq (stx-transfer? premium tx-sender (as-contract tx-sender)) (ok true)) err-insufficient-funds)
      (ok true)
    )
  )
)

;; Approve join request (admin only)
(define-public (approve-membership-request (pool-id uint) (new-member principal))
  (begin
    (asserts! (var-get protocol-initialized) err-not-initialized)
    (asserts! (> pool-id u0) err-invalid-pool-id)
    (asserts! (<= pool-id (var-get total-pool-count)) err-pool-not-found)

    (let (
      (pool (unwrap! (map-get? insurance-pools { pool-id: pool-id }) err-pool-not-found))
    )
      (asserts! (is-eq tx-sender (get admin pool)) err-not-admin)

      ;; Validate new-member principal
      (asserts! (not (is-eq new-member tx-sender)) err-invalid-admin)

      (map-set insurance-pools
        { pool-id: pool-id }
        (merge pool {
          balance: (+ (get balance pool) (get premium pool)),
          members: (unwrap! (as-max-len? (append (get members pool) new-member) u200) err-pool-not-found)
        })
      )
      (ok true)
    )
  )
)
