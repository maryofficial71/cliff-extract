;; CliffExtract - Milestone-Based Social Impact Funding Platform
;; A decentralized platform for community funding with milestone verification

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-invalid-milestone (err u103))
(define-constant err-insufficient-funds (err u104))
(define-constant err-already-exists (err u105))
(define-constant err-milestone-not-ready (err u106))
(define-constant err-already-verified (err u107))

;; Data Variables
(define-data-var project-nonce uint u0)
(define-data-var milestone-nonce uint u0)

;; Data Maps
(define-map projects
  { project-id: uint }
  {
    creator: principal,
    title: (string-ascii 100),
    total-funding: uint,
    withdrawn: uint,
    active: bool,
    impact-score: uint,
    created-at: uint
  }
)

(define-map milestones
  { milestone-id: uint }
  {
    project-id: uint,
    description: (string-ascii 200),
    funding-amount: uint,
    required-verifications: uint,
    verification-count: uint,
    verified: bool,
    released: bool
  }
)

(define-map project-funders
  { project-id: uint, funder: principal }
  { amount: uint }
)

(define-map milestone-verifiers
  { milestone-id: uint, verifier: principal }
  { verified: bool }
)

(define-map validators
  { validator: principal }
  { stake: uint, reputation: uint }
)

;; Read-only functions
(define-read-only (get-project (project-id uint))
  (map-get? projects { project-id: project-id })
)

(define-read-only (get-milestone (milestone-id uint))
  (map-get? milestones { milestone-id: milestone-id })
)

(define-read-only (get-validator-info (validator principal))
  (map-get? validators { validator: validator })
)

(define-read-only (get-project-funding (project-id uint) (funder principal))
  (map-get? project-funders { project-id: project-id, funder: funder })
)

;; Public functions

;; Create a new impact project
(define-public (create-project (title (string-ascii 100)))
  (let
    (
      (new-project-id (var-get project-nonce))
    )
    (map-set projects
      { project-id: new-project-id }
      {
        creator: tx-sender,
        title: title,
        total-funding: u0,
        withdrawn: u0,
        active: true,
        impact-score: u0,
        created-at: block-height
      }
    )
    (var-set project-nonce (+ new-project-id u1))
    (ok new-project-id)
  )
)

;; Fund a project
(define-public (fund-project (project-id uint) (amount uint))
  (let
    (
      (project (unwrap! (get-project project-id) err-not-found))
      (existing-funding (default-to { amount: u0 } 
        (get-project-funding project-id tx-sender)))
    )
    (asserts! (get active project) err-not-found)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (map-set project-funders
      { project-id: project-id, funder: tx-sender }
      { amount: (+ (get amount existing-funding) amount) }
    )
    (map-set projects
      { project-id: project-id }
      (merge project { total-funding: (+ (get total-funding project) amount) })
    )
    (ok true)
  )
)

;; Create a milestone for a project
(define-public (create-milestone 
    (project-id uint) 
    (description (string-ascii 200)) 
    (funding-amount uint)
    (required-verifications uint))
  (let
    (
      (project (unwrap! (get-project project-id) err-not-found))
      (new-milestone-id (var-get milestone-nonce))
    )
    (asserts! (is-eq tx-sender (get creator project)) err-unauthorized)
    (asserts! (get active project) err-not-found)
    (map-set milestones
      { milestone-id: new-milestone-id }
      {
        project-id: project-id,
        description: description,
        funding-amount: funding-amount,
        required-verifications: required-verifications,
        verification-count: u0,
        verified: false,
        released: false
      }
    )
    (var-set milestone-nonce (+ new-milestone-id u1))
    (ok new-milestone-id)
  )
)

;; Stake to become a validator
(define-public (stake-as-validator (stake-amount uint))
  (let
    (
      (existing-validator (default-to { stake: u0, reputation: u0 } 
        (get-validator-info tx-sender)))
    )
    (try! (stx-transfer? stake-amount tx-sender (as-contract tx-sender)))
    (map-set validators
      { validator: tx-sender }
      { 
        stake: (+ (get stake existing-validator) stake-amount),
        reputation: (get reputation existing-validator)
      }
    )
    (ok true)
  )
)

;; Verify a milestone
(define-public (verify-milestone (milestone-id uint))
  (let
    (
      (milestone (unwrap! (get-milestone milestone-id) err-not-found))
      (validator-info (unwrap! (get-validator-info tx-sender) err-unauthorized))
      (already-verified (default-to { verified: false }
        (map-get? milestone-verifiers 
          { milestone-id: milestone-id, verifier: tx-sender })))
    )
    (asserts! (> (get stake validator-info) u0) err-unauthorized)
    (asserts! (not (get verified already-verified)) err-already-verified)
    (asserts! (not (get verified milestone)) err-already-verified)
    
    (map-set milestone-verifiers
      { milestone-id: milestone-id, verifier: tx-sender }
      { verified: true }
    )
    
    (let
      (
        (new-verification-count (+ (get verification-count milestone) u1))
        (is-verified (>= new-verification-count (get required-verifications milestone)))
      )
      (map-set milestones
        { milestone-id: milestone-id }
        (merge milestone { 
          verification-count: new-verification-count,
          verified: is-verified
        })
      )
      
      ;; Reward validator
      (map-set validators
        { validator: tx-sender }
        (merge validator-info { reputation: (+ (get reputation validator-info) u1) })
      )
      
      (ok is-verified)
    )
  )
)

;; Release funds for verified milestone (cliff vesting)
(define-public (release-milestone-funds (milestone-id uint))
  (let
    (
      (milestone (unwrap! (get-milestone milestone-id) err-not-found))
      (project (unwrap! (get-project (get project-id milestone)) err-not-found))
    )
    (asserts! (is-eq tx-sender (get creator project)) err-unauthorized)
    (asserts! (get verified milestone) err-milestone-not-ready)
    (asserts! (not (get released milestone)) err-already-verified)
    
    (let
      (
        (available-funds (- (get total-funding project) (get withdrawn project)))
        (release-amount (get funding-amount milestone))
      )
      (asserts! (>= available-funds release-amount) err-insufficient-funds)
      
      (try! (as-contract (stx-transfer? release-amount tx-sender (get creator project))))
      
      (map-set milestones
        { milestone-id: milestone-id }
        (merge milestone { released: true })
      )
      
      (map-set projects
        { project-id: (get project-id milestone) }
        (merge project { 
          withdrawn: (+ (get withdrawn project) release-amount),
          impact-score: (+ (get impact-score project) u10)
        })
      )
      
      (ok true)
    )
  )
)

;; Extract unused funds from underperforming project
(define-public (extract-unused-funds (project-id uint))
  (let
    (
      (project (unwrap! (get-project project-id) err-not-found))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    
    (let
      (
        (unused-funds (- (get total-funding project) (get withdrawn project)))
      )
      (asserts! (> unused-funds u0) err-insufficient-funds)
      
      (try! (as-contract (stx-transfer? unused-funds tx-sender contract-owner)))
      
      (map-set projects
        { project-id: project-id }
        (merge project { active: false })
      )
      
      (ok unused-funds)
    )
  )
)

;; Unstake validator tokens
(define-public (unstake-validator (amount uint))
  (let
    (
      (validator-info (unwrap! (get-validator-info tx-sender) err-not-found))
    )
    (asserts! (>= (get stake validator-info) amount) err-insufficient-funds)
    
    (try! (as-contract (stx-transfer? amount tx-sender tx-sender)))
    
    (map-set validators
      { validator: tx-sender }
      (merge validator-info { stake: (- (get stake validator-info) amount) })
    )
    
    (ok true)
  )
)