;; PrimeWave - Enhanced Zero-Knowledge Academic Credential Verification System
;; Knowledge Tokens (KNO) with Advanced Credential Verification Protocol

;; ============================================================================
;; CONSTANTS & ERROR HANDLING
;; ============================================================================

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
(define-constant ERR-INVALID-PARAMETERS (err u1013))
(define-constant ERR-MILESTONE-ALREADY-VERIFIED (err u1014))
(define-constant ERR-INSUFFICIENT-VERIFICATIONS (err u1015))
(define-constant ERR-INVALID-PROOF-SYSTEM (err u1016))

;; Contract Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant DEPLOYER tx-sender)
(define-constant MINIMUM-STAKE u1000000) ;; 1 KNO token minimum stake
(define-constant VERIFICATION-WINDOW u1440) ;; ~10 days in blocks
(define-constant MIN-VERIFIERS u3)
(define-constant MAX-VERIFIERS u20)
(define-constant CONSENSUS-THRESHOLD u67) ;; 67% agreement required
(define-constant MAX-MILESTONES u20)
(define-constant MIN-ENROLLMENT u100000) ;; Minimum STX for enrollment
(define-constant TOKEN-CONVERSION-RATE u10000) ;; STX to KNO conversion
(define-constant REPUTATION-DECAY-BLOCKS u52560) ;; ~1 year in blocks

;; ============================================================================
;; TOKEN DEFINITION
;; ============================================================================

(define-fungible-token knowledge-token)

;; ============================================================================
;; DATA STORAGE
;; ============================================================================

;; Global State Variables
(define-data-var total-credentials uint u0)
(define-data-var total-verifications uint u0)
(define-data-var platform-fee-rate uint u250) ;; 2.5%
(define-data-var emergency-pause bool false)
(define-data-var zk-protocol-active bool true)
(define-data-var next-credential-id uint u1)

;; Institution Registry
(define-map institution-registry
  principal
  {
    name: (string-ascii 100),
    country: (string-ascii 50),
    accreditation-level: uint,
    verified: bool,
    registration-block: uint,
    total-credentials: uint,
    reputation-score: uint
  }
)

;; Enhanced Credential Structure
(define-map credentials
  uint
  {
    institution: principal,
    program-name: (string-ascii 100),
    field-of-study: (string-ascii 50),
    degree-level: (string-ascii 30), ;; bachelor, master, phd, certificate
    target-graduates: uint,
    enrolled-students: uint,
    graduated-students: uint,
    current-milestone: uint,
    total-milestones: uint,
    minimum-grade: uint, ;; Percentage 0-100
    active: bool,
    completed: bool,
    creation-block: uint,
    completion-block: uint,
    total-stx-locked: uint,
    verification-method: (string-ascii 50) ;; zk-snark, zk-stark, merkle-proof
  }
)

;; Milestone Management
(define-map credential-milestones
  {credential-id: uint, milestone: uint}
  {
    title: (string-ascii 100),
    description: (string-ascii 300),
    required-score: uint, ;; 0-100 percentage
    weight: uint, ;; Milestone weight in final grade
    assessment-type: (string-ascii 50), ;; exam, project, thesis, practicum
    deadline-block: uint,
    verification-reward: uint,
    completed: bool,
    completion-block: uint,
    average-score: uint,
    total-submissions: uint
  }
)

;; Advanced Verifier System
(define-map verifier-profiles
  principal
  {
    expertise-areas: (list 5 (string-ascii 50)),
    staked-amount: uint,
    locked-amount: uint, ;; Amount locked in active verifications
    total-verifications: uint,
    correct-verifications: uint,
    reputation-score: uint, ;; 0-1000
    last-activity-block: uint,
    slashing-count: uint,
    earnings: uint,
    active: bool
  }
)

;; Verification Records
(define-map milestone-verifications
  {credential-id: uint, milestone: uint, verifier: principal}
  {
    verification-result: bool,
    confidence-score: uint, ;; 0-100
    proof-hash: (buff 32),
    verification-block: uint,
    stake-locked: uint,
    processed: bool,
    reward-claimed: bool
  }
)

;; Student Enrollment & Progress
(define-map student-progress
  {student: principal, credential-id: uint}
  {
    enrollment-block: uint,
    total-invested: uint,
    kno-balance: uint,
    completed-milestones: uint,
    current-grade: uint, ;; Running average
    graduation-eligible: bool,
    graduated: bool,
    graduation-block: uint,
    final-grade: uint,
    certificate-hash: (buff 32)
  }
)

;; Student Milestone Scores
(define-map student-milestone-scores
  {student: principal, credential-id: uint, milestone: uint}
  {
    score: uint,
    submission-block: uint,
    verified: bool,
    verification-count: uint,
    proof-hash: (buff 32)
  }
)

;; ZK Proof Registry
(define-map zk-proof-registry
  (buff 32)
  {
    credential-id: uint,
    milestone: uint,
    student: principal,
    proof-type: (string-ascii 50),
    verification-count: uint,
    consensus-reached: bool,
    timestamp: uint
  }
)

;; ============================================================================
;; UTILITY FUNCTIONS
;; ============================================================================

(define-private (calculate-kno-reward (stx-amount uint) (field-multiplier uint))
  (/ (* stx-amount field-multiplier) TOKEN-CONVERSION-RATE)
)

(define-private (calculate-reputation-decay (last-block uint) (current-score uint))
  (let ((blocks-passed (- block-height last-block)))
    (if (> blocks-passed REPUTATION-DECAY-BLOCKS)
        (let ((halved-score (/ current-score u2)))
          (if (> halved-score u1) halved-score u1)) ;; Ensure minimum of 1
        current-score
    )
  )
)

(define-private (is-consensus-reached (positive-votes uint) (total-votes uint))
  (and 
    (>= total-votes MIN-VERIFIERS)
    (>= (/ (* positive-votes u100) total-votes) CONSENSUS-THRESHOLD)
  )
)

(define-private (calculate-weighted-grade (scores (list 20 uint)) (weights (list 20 uint)))
  (let (
    (total-weighted-score (fold + (map * scores weights) u0))
    (total-weights (fold + weights u0))
  )
    (if (> total-weights u0)
        (/ total-weighted-score total-weights)
        u0
    )
  )
)

;; ============================================================================
;; ADMIN FUNCTIONS
;; ============================================================================

(define-public (register-institution 
  (institution principal) 
  (name (string-ascii 100)) 
  (country (string-ascii 50))
  (accreditation-level uint)
)
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (asserts! (<= accreditation-level u5) ERR-INVALID-PARAMETERS)
    
    (map-set institution-registry institution
      {
        name: name,
        country: country,
        accreditation-level: accreditation-level,
        verified: true,
        registration-block: block-height,
        total-credentials: u0,
        reputation-score: u500 ;; Start with medium reputation
      }
    )
    (ok true)
  )
)

(define-public (update-platform-fee (new-fee-rate uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (asserts! (<= new-fee-rate u1000) ERR-INVALID-PARAMETERS) ;; Max 10%
    (var-set platform-fee-rate new-fee-rate)
    (ok true)
  )
)

(define-public (toggle-emergency-pause)
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (var-set emergency-pause (not (var-get emergency-pause)))
    (ok (var-get emergency-pause))
  )
)

;; ============================================================================
;; CREDENTIAL MANAGEMENT
;; ============================================================================

(define-public (create-credential
  (program-name (string-ascii 100))
  (field-of-study (string-ascii 50))
  (degree-level (string-ascii 30))
  (target-graduates uint)
  (total-milestones uint)
  (minimum-grade uint)
  (verification-method (string-ascii 50))
)
  (let (
    (credential-id (var-get next-credential-id))
    (institution-data (map-get? institution-registry tx-sender))
  )
    (asserts! (not (var-get emergency-pause)) ERR-EMERGENCY-ACTIVE)
    (asserts! (is-some institution-data) ERR-UNAUTHORIZED)
    (asserts! (and (> target-graduates u0) (<= target-graduates u10000)) ERR-INVALID-PARAMETERS)
    (asserts! (and (> total-milestones u0) (<= total-milestones MAX-MILESTONES)) ERR-INVALID-PARAMETERS)
    (asserts! (<= minimum-grade u100) ERR-INVALID-PARAMETERS)
    
    (map-set credentials credential-id
      {
        institution: tx-sender,
        program-name: program-name,
        field-of-study: field-of-study,
        degree-level: degree-level,
        target-graduates: target-graduates,
        enrolled-students: u0,
        graduated-students: u0,
        current-milestone: u1,
        total-milestones: total-milestones,
        minimum-grade: minimum-grade,
        active: true,
        completed: false,
        creation-block: block-height,
        completion-block: u0,
        total-stx-locked: u0,
        verification-method: verification-method
      }
    )
    
    ;; Update institution stats
    (match institution-data
      inst-data (map-set institution-registry tx-sender
        (merge inst-data {total-credentials: (+ (get total-credentials inst-data) u1)}))
      false
    )
    
    (var-set next-credential-id (+ credential-id u1))
    (var-set total-credentials (+ (var-get total-credentials) u1))
    
    (ok credential-id)
  )
)

(define-public (add-milestone
  (credential-id uint)
  (milestone uint)
  (title (string-ascii 100))
  (description (string-ascii 300))
  (required-score uint)
  (weight uint)
  (assessment-type (string-ascii 50))
  (deadline-block uint)
  (verification-reward uint)
)
  (let (
    (credential (unwrap! (map-get? credentials credential-id) ERR-CREDENTIAL-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender (get institution credential)) ERR-UNAUTHORIZED)
    (asserts! (get active credential) ERR-CREDENTIAL-INACTIVE)
    (asserts! (<= milestone (get total-milestones credential)) ERR-INVALID-MILESTONE)
    (asserts! (<= required-score u100) ERR-INVALID-PARAMETERS)
    (asserts! (> deadline-block block-height) ERR-INVALID-PARAMETERS)
    
    (map-set credential-milestones {credential-id: credential-id, milestone: milestone}
      {
        title: title,
        description: description,
        required-score: required-score,
        weight: weight,
        assessment-type: assessment-type,
        deadline-block: deadline-block,
        verification-reward: verification-reward,
        completed: false,
        completion-block: u0,
        average-score: u0,
        total-submissions: u0
      }
    )
    
    (ok true)
  )
)

;; ============================================================================
;; STUDENT ENROLLMENT & PROGRESS
;; ============================================================================

(define-public (enroll-in-credential (credential-id uint) (stx-amount uint))
  (let (
    (credential (unwrap! (map-get? credentials credential-id) ERR-CREDENTIAL-NOT-FOUND))
    (kno-tokens (calculate-kno-reward stx-amount u1))
    (platform-fee (/ (* stx-amount (var-get platform-fee-rate)) u10000))
    (net-amount (- stx-amount platform-fee))
  )
    (asserts! (not (var-get emergency-pause)) ERR-EMERGENCY-ACTIVE)
    (asserts! (get active credential) ERR-CREDENTIAL-INACTIVE)
    (asserts! (>= stx-amount MIN-ENROLLMENT) ERR-INSUFFICIENT-FUNDS)
    
    ;; Transfer STX (net amount goes to institution, fee to contract)
    (try! (stx-transfer? net-amount tx-sender (get institution credential)))
    (try! (stx-transfer? platform-fee tx-sender (as-contract tx-sender)))
    
    ;; Mint KNO tokens to student
    (try! (ft-mint? knowledge-token kno-tokens tx-sender))
    
    ;; Record student enrollment
    (map-set student-progress {student: tx-sender, credential-id: credential-id}
      {
        enrollment-block: block-height,
        total-invested: stx-amount,
        kno-balance: kno-tokens,
        completed-milestones: u0,
        current-grade: u0,
        graduation-eligible: false,
        graduated: false,
        graduation-block: u0,
        final-grade: u0,
        certificate-hash: 0x00
      }
    )
    
    ;; Update credential enrollment count
    (map-set credentials credential-id
      (merge credential {
        enrolled-students: (+ (get enrolled-students credential) u1),
        total-stx-locked: (+ (get total-stx-locked credential) net-amount)
      })
    )
    
    (ok kno-tokens)
  )
)

(define-public (submit-milestone-work
  (credential-id uint)
  (milestone uint)
  (proof-hash (buff 32))
  (self-assessed-score uint)
)
  (let (
    (credential (unwrap! (map-get? credentials credential-id) ERR-CREDENTIAL-NOT-FOUND))
    (milestone-data (unwrap! (map-get? credential-milestones {credential-id: credential-id, milestone: milestone}) ERR-INVALID-MILESTONE))
    (student-data (unwrap! (map-get? student-progress {student: tx-sender, credential-id: credential-id}) ERR-UNAUTHORIZED))
  )
    (asserts! (get active credential) ERR-CREDENTIAL-INACTIVE)
    (asserts! (<= self-assessed-score u100) ERR-INVALID-PARAMETERS)
    (asserts! (< block-height (get deadline-block milestone-data)) ERR-VERIFICATION-EXPIRED)
    
    ;; Record milestone submission
    (map-set student-milestone-scores {student: tx-sender, credential-id: credential-id, milestone: milestone}
      {
        score: self-assessed-score,
        submission-block: block-height,
        verified: false,
        verification-count: u0,
        proof-hash: proof-hash
      }
    )
    
    ;; Register ZK proof
    (map-set zk-proof-registry proof-hash
      {
        credential-id: credential-id,
        milestone: milestone,
        student: tx-sender,
        proof-type: (get verification-method credential),
        verification-count: u0,
        consensus-reached: false,
        timestamp: block-height
      }
    )
    
    (ok proof-hash)
  )
)

;; ============================================================================
;; VERIFIER SYSTEM
;; ============================================================================

(define-public (register-as-verifier 
  (expertise-areas (list 5 (string-ascii 50)))
  (initial-stake uint)
)
  (begin
    (asserts! (>= initial-stake MINIMUM-STAKE) ERR-INSUFFICIENT-STAKE)
    (asserts! (>= (ft-get-balance knowledge-token tx-sender) initial-stake) ERR-INSUFFICIENT-FUNDS)
    
    ;; Lock tokens for staking
    (try! (ft-transfer? knowledge-token initial-stake tx-sender (as-contract tx-sender)))
    
    (map-set verifier-profiles tx-sender
      {
        expertise-areas: expertise-areas,
        staked-amount: initial-stake,
        locked-amount: u0,
        total-verifications: u0,
        correct-verifications: u0,
        reputation-score: u500, ;; Start with medium reputation
        last-activity-block: block-height,
        slashing-count: u0,
        earnings: u0,
        active: true
      }
    )
    
    (ok true)
  )
)

(define-public (verify-student-milestone
  (credential-id uint)
  (milestone uint)
  (student principal)
  (verification-result bool)
  (confidence-score uint)
  (proof-hash (buff 32))
)
  (let (
    (verifier-profile (unwrap! (map-get? verifier-profiles tx-sender) ERR-VERIFIER-NOT-STAKED))
    (milestone-data (unwrap! (map-get? credential-milestones {credential-id: credential-id, milestone: milestone}) ERR-INVALID-MILESTONE))
    (student-submission (unwrap! (map-get? student-milestone-scores {student: student, credential-id: credential-id, milestone: milestone}) ERR-PROOF-DATA-INVALID))
    (verification-key {credential-id: credential-id, milestone: milestone, verifier: tx-sender})
    (stake-amount (/ (get staked-amount verifier-profile) u10)) ;; Lock 10% of stake
  )
    (asserts! (get active verifier-profile) ERR-VERIFIER-NOT-STAKED)
    (asserts! (<= confidence-score u100) ERR-INVALID-PARAMETERS)
    (asserts! (is-eq proof-hash (get proof-hash student-submission)) ERR-PROOF-DATA-INVALID)
    (asserts! (is-none (map-get? milestone-verifications verification-key)) ERR-ALREADY-VERIFIED)
    
    ;; Lock portion of verifier's stake
    (map-set verifier-profiles tx-sender
      (merge verifier-profile {
        locked-amount: (+ (get locked-amount verifier-profile) stake-amount),
        total-verifications: (+ (get total-verifications verifier-profile) u1),
        last-activity-block: block-height
      })
    )
    
    ;; Record verification
    (map-set milestone-verifications verification-key
      {
        verification-result: verification-result,
        confidence-score: confidence-score,
        proof-hash: proof-hash,
        verification-block: block-height,
        stake-locked: stake-amount,
        processed: false,
        reward-claimed: false
      }
    )
    
    ;; Update ZK proof verification count
    (match (map-get? zk-proof-registry proof-hash)
      proof-data (map-set zk-proof-registry proof-hash
        (merge proof-data {verification-count: (+ (get verification-count proof-data) u1)}))
      false
    )
    
    (var-set total-verifications (+ (var-get total-verifications) u1))
    (ok true)
  )
)

;; ============================================================================
;; READ-ONLY FUNCTIONS
;; ============================================================================

(define-read-only (get-credential-details (credential-id uint))
  (map-get? credentials credential-id)
)

(define-read-only (get-milestone-info (credential-id uint) (milestone uint))
  (map-get? credential-milestones {credential-id: credential-id, milestone: milestone})
)

(define-read-only (get-student-progress (student principal) (credential-id uint))
  (map-get? student-progress {student: student, credential-id: credential-id})
)

(define-read-only (get-verifier-profile (verifier principal))
  (map-get? verifier-profiles verifier)
)

(define-read-only (get-verification-record (credential-id uint) (milestone uint) (verifier principal))
  (map-get? milestone-verifications {credential-id: credential-id, milestone: milestone, verifier: verifier})
)

(define-read-only (get-institution-info (institution principal))
  (map-get? institution-registry institution)
)

(define-read-only (get-student-milestone-score (student principal) (credential-id uint) (milestone uint))
  (map-get? student-milestone-scores {student: student, credential-id: credential-id, milestone: milestone})
)

(define-read-only (get-zk-proof-info (proof-hash (buff 32)))
  (map-get? zk-proof-registry proof-hash)
)

(define-read-only (get-kno-balance (user principal))
  (ft-get-balance knowledge-token user)
)

(define-read-only (get-platform-stats)
  {
    total-credentials: (var-get total-credentials),
    total-verifications: (var-get total-verifications),
    platform-fee-rate: (var-get platform-fee-rate),
    emergency-paused: (var-get emergency-pause),
    zk-protocol-active: (var-get zk-protocol-active)
  }
)

;; ============================================================================
;; TOKEN INTERFACE COMPLIANCE
;; ============================================================================

(define-read-only (get-name)
  (ok "Knowledge Token")
)

(define-read-only (get-symbol)
  (ok "KNO")
)

(define-read-only (get-decimals)
  (ok u6)
)

(define-read-only (get-total-supply)
  (ok (ft-get-supply knowledge-token))
)