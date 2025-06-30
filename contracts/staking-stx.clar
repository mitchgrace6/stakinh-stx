
;; staking-stx
;; A smart contract for staking STX tokens to earn rewards

;; constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-invalid-amount (err u102))
(define-constant err-insufficient-balance (err u103))
(define-constant err-already-staked (err u104))
(define-constant err-not-staked (err u105))
(define-constant err-staking-period-not-over (err u106))
(define-constant err-paused (err u107))
(define-constant err-unauthorized (err u108))

;; Minimum stake amount (1 STX = 1,000,000 micro-STX)
(define-constant min-stake-amount u1000000)

;; Maximum stake amount (1,000,000 STX)
(define-constant max-stake-amount u1000000000000)

;; Staking periods in blocks (assuming ~10 minute block times)
(define-constant min-staking-period u1008) ;; ~1 week (7 * 24 * 6 blocks)
(define-constant standard-staking-period u4320) ;; ~1 month (30 * 24 * 6 blocks)
(define-constant long-staking-period u17280) ;; ~4 months (120 * 24 * 6 blocks)

;; Reward rates (basis points - 1 basis point = 0.01%)
(define-constant base-reward-rate u500) ;; 5% annual base rate
(define-constant bonus-rate-standard u200) ;; 2% bonus for standard period
(define-constant bonus-rate-long u500) ;; 5% bonus for long period

;; Pool and contract limits
(define-constant max-total-staked u100000000000000) ;; 100M STX max total staked
(define-constant reward-pool-reserve u10000000000) ;; 10K STX reserve for rewards

;; Time constants
(define-constant blocks-per-year u52560) ;; Approximate blocks per year (365 * 24 * 6)
(define-constant early-withdrawal-penalty u1000) ;; 10% penalty for early withdrawal

;; data maps and vars

;; Map to track individual user stakes
(define-map user-stakes
  { user: principal }
  {
    amount: uint,           ;; Amount staked in micro-STX
    start-block: uint,      ;; Block when staking started
    end-block: uint,        ;; Block when staking period ends
    staking-period: uint,   ;; Length of staking period in blocks
    reward-rate: uint,      ;; Total reward rate (base + bonus) in basis points
    last-claim-block: uint, ;; Last block when rewards were claimed
    is-active: bool         ;; Whether the stake is currently active
  }
)

;; Map to track user reward balances (unclaimed rewards)
(define-map user-rewards
  { user: principal }
  { 
    pending-rewards: uint,  ;; Accumulated rewards not yet claimed
    total-claimed: uint     ;; Total rewards claimed historically
  }
)

;; Map to track user staking history and statistics
(define-map user-stats
  { user: principal }
  {
    total-staked-ever: uint,    ;; Total amount ever staked by user
    total-rewards-earned: uint, ;; Total rewards earned by user
    stake-count: uint,          ;; Number of times user has staked
    first-stake-block: uint     ;; Block of user's first stake
  }
)

;; Map to track staking periods and their configurations
(define-map staking-period-config
  { period-blocks: uint }
  {
    reward-bonus: uint,     ;; Bonus reward rate for this period
    is-active: bool,        ;; Whether this staking period is available
    min-amount: uint,       ;; Minimum stake amount for this period
    max-amount: uint        ;; Maximum stake amount for this period
  }
)

;; Global contract state variables
(define-data-var contract-paused bool false)
(define-data-var total-staked uint u0)
(define-data-var total-rewards-distributed uint u0)
(define-data-var total-users uint u0)
(define-data-var reward-pool-balance uint u0)
(define-data-var emergency-shutdown bool false)

;; Admin and governance variables
(define-data-var contract-admin principal tx-sender)
(define-data-var pending-admin (optional principal) none)
(define-data-var fee-recipient principal tx-sender)

;; Reward calculation variables
(define-data-var base-reward-multiplier uint u10000) ;; 100.00% in basis points
(define-data-var reward-pool-last-update uint block-height)

;; Protocol fee settings (in basis points)
(define-data-var protocol-fee-rate uint u100) ;; 1% fee on rewards
(define-data-var early-withdrawal-fee-rate uint u1000) ;; 10% fee on early withdrawal

;; Staking limits and controls
(define-data-var max-stake-per-user uint max-stake-amount)
(define-data-var min-stake-global uint min-stake-amount)
(define-data-var staking-enabled bool true)

;; Reward distribution tracking
(define-data-var last-reward-distribution-block uint block-height)
(define-data-var reward-distribution-interval uint u4320) ;; ~30 days in blocks

;; Emergency and maintenance
(define-data-var maintenance-mode bool false)
(define-data-var upgrade-delay uint u17280) ;; ~4 months delay for upgrades

;; private functions
;;

;; public functions
;;
