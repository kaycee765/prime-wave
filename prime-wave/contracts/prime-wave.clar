;; PrimeWave - Zero-Knowledge Academic Credential Verification System
;; Knowledge Tokens (KNO) with Credential Verification Protocol

;; Error Constants
(define-constant ERR-UNAUTHORIZED (err u1000))
(define-constant ERR-CREDENTIAL-NOT-FOUND (err u1001))
(define-constant ERR-INSUFFICIENT-FUNDS (err u1002))
(define-constant ERR-INVALID-MILESTONE (err u1003))
(define-constant ERR-CREDENTIAL-INACTIVE (err u1004))
(define-constant ERR-VERIFIER-NOT-STAKED (err u1005))
(define-constant ERR-INSUFFICIENT-STAKE (err u1006))
(define-constant ERR-PROOF-DATA-INVALID (err u1007))
(define-constant ERR-THRESHOLD-NOT-MET (err u1008))
(define-constant ERR-VERIFICATION-EXPIRED (err u1009))
(define-constant ERR-ALREADY-VERIFIED (err u1010))
(define-constant ERR-CREDENTIAL-COMPLETED (err u1011))
(define-constant ERR-EMERGENCY-ACTIVE (err u1012))

;; Contract Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant MINIMUM-STAKE u1000000) ;; 1 KNO token minimum stake
(define-constant VERIFICATION-WINDOW u144) ;; ~24 hours in blocks
(define-constant MIN-VERIFIERS u3)
(define-constant ZK-VERIFICATION-THRESHOLD u75) ;; 75% accuracy threshold

;; Fungible Token Definition
(define-fungible-token knowledge-token)

;; Data Variables
(define-data-var total-credentials uint u0)
(define-data-var total-certificates-verified uint u0)
(define-data-var zk-protocol-active bool true)
(define-data-var emergency-pause bool false)
(define-data-var platform-fee-rate uint u250) ;; 2.5%

;; Data Maps
(define-map credentials
  uint
  {
    institution: principal,
    program-name: (string-ascii 100),
    field-of-study: (string-ascii 50),
    target-graduates: uint,
    enrolled-students: uint,
    current-milestone: uint,
    total-milestones: uint,
    academic-rigor: uint,
    curriculum-diversity: uint,
    employment-score: uint,
    active: bool,
    completed: bool,
    zk-verified: bool,
    proof-system: (string-ascii 64),
    creation-block: uint
  }
)

(define-map credential-milestones
  {credential-id: uint, milestone: uint}
  {
    description: (string-ascii 200),
    graduation-amount: uint,
    threshold-value: uint,
    assessment-type: (string-ascii 30),
    verification-proof: (string-ascii 500),
    achieved: bool,
    verification-block: uint,
    verifier-count: uint
  }
)

(define-map verifier-stakes
  principal
  {
    staked-amount: uint,
    active-verifications: uint,
    successful-verifications: uint,
    failed-verifications: uint,
    reputation-score: uint,
    last-verification-block: uint
  }
)

(define-map credential-verifications
  {verifier: principal, credential-id: uint, milestone: uint}
  {
    verification-result: bool,
    proof-data-hash: (buff 32),
    verification-block: uint,
    stake-amount: uint,
    processed: bool
  }
)

(define-map student-enrollments
  {student: principal, credential-id: uint}
  {
    total-invested: uint,
    token-balance: uint,
    academic-earned: uint,
    curriculum-earned: uint,
    employment-earned: uint,
    last-enrollment-block: uint
  }
)

(define-map proof-data-registry
  (buff 32)
  {
    credential-id: uint,
    milestone: uint,
    assessment-score: uint,
    timestamp: uint,
    institution-hash: (buff 32),
    verified: bool,
    verification-count: uint
  }
)

(define-map academic-network-connections
  {student1: principal, student2: principal}
  {
    shared-credentials: uint,
    connection-strength: uint,
    collaborative-learning: uint,
    last-interaction: uint
  }
)

;; Helper Functions
(define-private (calculate-kno-tokens (amount uint) (field-of-study (string-ascii 50)))
  ;; Simple token calculation - could be enhanced with field-based multipliers
  (/ (* amount u100) u1000000) ;; 0.01% conversion rate
)

;; Owner Functions
(define-public (set-platform-fee-rate (new-rate uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (asserts! (<= new-rate u1000) (err u2000)) ;; Max 10% fee
    (ok (var-set platform-fee-rate new-rate))
  )
)

(define-public (toggle-emergency-pause)
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (ok (var-set emergency-pause (not (var-get emergency-pause))))
  )
)

(define-public (set-zk-protocol-status (active bool))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (ok (var-set zk-protocol-active active))
  )
)

;; Public Functions
(define-public (create-credential 
  (program-name (string-ascii 100))
  (field-of-study (string-ascii 50))
  (target-graduates uint)
  (total-milestones uint)
  (proof-system (string-ascii 64))
)
  (let (
    (credential-id (+ (var-get total-credentials) u1))
  )
    (asserts! (not (var-get emergency-pause)) ERR-EMERGENCY-ACTIVE)
    (asserts! (> target-graduates u0) (err u2001))
    (asserts! (> total-milestones u0) (err u2002))
    (asserts! (<= total-milestones u10) (err u2003))
    
    (map-set credentials credential-id
      {
        institution: tx-sender,
        program-name: program-name,
        field-of-study: field-of-study,
        target-graduates: target-graduates,
        enrolled-students: u0,
        current-milestone: u1,
        total-milestones: total-milestones,
        academic-rigor: u0,
        curriculum-diversity: u0,
        employment-score: u0,
        active: true,
        completed: false,
        zk-verified: false,
        proof-system: proof-system,
        creation-block: block-height
      }
    )
    
    (var-set total-credentials credential-id)
    (ok credential-id)
  )
)

(define-public (enroll-in-credential (credential-id uint) (amount uint))
  (let (
    (credential (unwrap! (map-get? credentials credential-id) ERR-CREDENTIAL-NOT-FOUND))
    (current-enrollment (default-to 
      {total-invested: u0, token-balance: u0, academic-earned: u0, 
       curriculum-earned: u0, employment-earned: u0, last-enrollment-block: u0}
      (map-get? student-enrollments {student: tx-sender, credential-id: credential-id})
    ))
    (kno-tokens (calculate-kno-tokens amount (get field-of-study credential)))
  )
    (asserts! (not (var-get emergency-pause)) ERR-EMERGENCY-ACTIVE)
    (asserts! (get active credential) ERR-CREDENTIAL-INACTIVE)
    (asserts! (not (get completed credential)) ERR-CREDENTIAL-COMPLETED)
    (asserts! (> amount u0) ERR-INSUFFICIENT-FUNDS)
    
    ;; Transfer STX from student to contract
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    ;; Mint KNO tokens to student
    (try! (ft-mint? knowledge-token kno-tokens tx-sender))
    
    ;; Update credential
    (map-set credentials credential-id
      (merge credential {enrolled-students: (+ (get enrolled-students credential) amount)})
    )
    
    ;; Update student enrollment
    (map-set student-enrollments {student: tx-sender, credential-id: credential-id}
      (merge current-enrollment {
        total-invested: (+ (get total-invested current-enrollment) amount),
        token-balance: (+ (get token-balance current-enrollment) kno-tokens),
        last-enrollment-block: block-height
      })
    )
    
    (ok kno-tokens)
  )
)

(define-public (stake-for-verification (stake-amount uint))
  (let (
    (current-stake (default-to 
      {staked-amount: u0, active-verifications: u0, successful-verifications: u0,
       failed-verifications: u0, reputation-score: u100, last-verification-block: u0}
      (map-get? verifier-stakes tx-sender)
    ))
  )
    (asserts! (>= stake-amount MINIMUM-STAKE) ERR-INSUFFICIENT-STAKE)
    (asserts! (>= (ft-get-balance knowledge-token tx-sender) stake-amount) ERR-INSUFFICIENT-FUNDS)
    
    ;; Lock tokens for staking
    (try! (ft-transfer? knowledge-token stake-amount tx-sender (as-contract tx-sender)))
    
    (map-set verifier-stakes tx-sender
      (merge current-stake {
        staked-amount: (+ (get staked-amount current-stake) stake-amount)
      })
    )
    
    (ok true)
  )
)

(define-public (submit-proof-data 
  (credential-id uint) 
  (milestone uint) 
  (assessment-score uint) 
  (institution-hash (buff 32))
  (data-hash (buff 32))
)
  (let (
    (credential (unwrap! (map-get? credentials credential-id) ERR-CREDENTIAL-NOT-FOUND))
    (milestone-data (unwrap! (map-get? credential-milestones {credential-id: credential-id, milestone: milestone}) ERR-INVALID-MILESTONE))
  )
    (asserts! (not (var-get emergency-pause)) ERR-EMERGENCY-ACTIVE)
    (asserts! (get active credential) ERR-CREDENTIAL-INACTIVE)
    (asserts! (is-eq (get current-milestone credential) milestone) ERR-INVALID-MILESTONE)
    (asserts! (var-get zk-protocol-active) (err u2004))
    
    (map-set proof-data-registry data-hash
      {
        credential-id: credential-id,
        milestone: milestone,
        assessment-score: assessment-score,
        timestamp: block-height,
        institution-hash: institution-hash,
        verified: false,
        verification-count: u0
      }
    )
    
    (ok data-hash)
  )
)

(define-public (verify-milestone 
  (credential-id uint) 
  (milestone uint) 
  (verification-result bool) 
  (proof-data-hash (buff 32))
)
  (let (
    (verifier-stake (unwrap! (map-get? verifier-stakes tx-sender) ERR-VERIFIER-NOT-STAKED))
    (credential (unwrap! (map-get? credentials credential-id) ERR-CREDENTIAL-NOT-FOUND))
    (proof-data (unwrap! (map-get? proof-data-registry proof-data-hash) ERR-PROOF-DATA-INVALID))
    (verification-key {verifier: tx-sender, credential-id: credential-id, milestone: milestone})
    (existing-verification (map-get? credential-verifications verification-key))
  )
    (asserts! (> (get staked-amount verifier-stake) u0) ERR-INSUFFICIENT-STAKE)
    (asserts! (get active credential) ERR-CREDENTIAL-INACTIVE)
    (asserts! (is-eq (get current-milestone credential) milestone) ERR-INVALID-MILESTONE)
    (asserts! (is-none existing-verification) ERR-ALREADY-VERIFIED)
    (asserts! (< (- block-height (get timestamp proof-data)) VERIFICATION-WINDOW) ERR-VERIFICATION-EXPIRED)
    
    ;; Record verification
    (map-set credential-verifications verification-key
      {
        verification-result: verification-result,
        proof-data-hash: proof-data-hash,
        verification-block: block-height,
        stake-amount: (get staked-amount verifier-stake),
        processed: false
      }
    )
    
    ;; Update verifier stats
    (map-set verifier-stakes tx-sender
      (merge verifier-stake {
        active-verifications: (+ (get active-verifications verifier-stake) u1),
        last-verification-block: block-height
      })
    )
    
    ;; Update proof data verification count
    (map-set proof-data-registry proof-data-hash
      (merge proof-data {
        verification-count: (+ (get verification-count proof-data) u1)
      })
    )
    
    (ok true)
  )
)

;; Read-only Functions
(define-read-only (get-credential (credential-id uint))
  (map-get? credentials credential-id)
)

(define-read-only (get-credential-milestone (credential-id uint) (milestone uint))
  (map-get? credential-milestones {credential-id: credential-id, milestone: milestone})
)

(define-read-only (get-verifier-stake (verifier principal))
  (map-get? verifier-stakes verifier)
)

(define-read-only (get-student-enrollment (student principal) (credential-id uint))
  (map-get? student-enrollments {student: student, credential-id: credential-id})
)

(define-read-only (get-proof-data (data-hash (buff 32)))
  (map-get? proof-data-registry data-hash)
)

(define-read-only (get-total-credentials)
  (var-get total-credentials)
)

(define-read-only (get-platform-fee-rate)
  (var-get platform-fee-rate)
)

(define-read-only (is-emergency-paused)
  (var-get emergency-pause)
)

(define-read-only (get-token-balance (student principal))
  (ft-get-balance knowledge-token student)
)