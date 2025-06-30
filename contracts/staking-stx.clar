
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

;; Validation functions
(define-private (is-contract-admin (user principal))
  (is-eq user (var-get contract-admin))
)

(define-private (is-contract-owner (user principal))
  (is-eq user contract-owner)
)

(define-private (is-authorized-admin (user principal))
  (or (is-contract-owner user) (is-contract-admin user))
)

(define-private (is-contract-operational)
  (and 
    (not (var-get contract-paused))
    (not (var-get emergency-shutdown))
    (not (var-get maintenance-mode))
    (var-get staking-enabled)
  )
)

(define-private (validate-stake-amount (amount uint))
  (and 
    (>= amount (var-get min-stake-global))
    (<= amount (var-get max-stake-per-user))
    (> amount u0)
  )
)

(define-private (validate-staking-period (period uint))
  (or 
    (is-eq period min-staking-period)
    (is-eq period standard-staking-period)
    (is-eq period long-staking-period)
  )
)

(define-private (can-stake-more (user principal) (new-amount uint))
  (let (
    (current-stake (default-to { amount: u0, start-block: u0, end-block: u0, staking-period: u0, reward-rate: u0, last-claim-block: u0, is-active: false } 
                               (map-get? user-stakes { user: user })))
    (current-amount (get amount current-stake))
    (total-after-stake (+ current-amount new-amount))
  )
    (and 
      (<= total-after-stake (var-get max-stake-per-user))
      (<= (+ (var-get total-staked) new-amount) max-total-staked)
    )
  )
)

;; Reward calculation functions
(define-private (calculate-reward-rate (staking-period uint))
  (if (is-eq staking-period long-staking-period) 
    (+ base-reward-rate bonus-rate-long)
    (if (is-eq staking-period standard-staking-period)
      (+ base-reward-rate bonus-rate-standard)
      base-reward-rate
    )
  )
)

(define-private (calculate-time-based-rewards (amount uint) (reward-rate uint) (blocks-staked uint))
  (let (
    (annual-reward (/ (* amount reward-rate) u10000))
    (block-reward (/ annual-reward blocks-per-year))
    (total-reward (* block-reward blocks-staked))
  )
    total-reward
  )
)

(define-private (calculate-pending-rewards (user principal))
  (match (map-get? user-stakes { user: user })
    stake-info 
    (if (get is-active stake-info)
      (let (
        (blocks-since-last-claim (- block-height (get last-claim-block stake-info)))
        (time-based-reward (calculate-time-based-rewards 
                           (get amount stake-info)
                           (get reward-rate stake-info)
                           blocks-since-last-claim))
        (current-rewards (default-to { pending-rewards: u0, total-claimed: u0 } 
                                   (map-get? user-rewards { user: user })))
      )
        (+ (get pending-rewards current-rewards) time-based-reward)
      )
      u0
    )
    u0
  )
)

(define-private (apply-protocol-fee (reward-amount uint))
  (let (
    (fee-amount (/ (* reward-amount (var-get protocol-fee-rate)) u10000))
    (net-reward (- reward-amount fee-amount))
  )
    { fee: fee-amount, net-reward: net-reward }
  )
)

(define-private (calculate-early-withdrawal-penalty (amount uint))
  (/ (* amount (var-get early-withdrawal-fee-rate)) u10000)
)

;; State update functions
(define-private (update-user-stake (user principal) (amount uint) (staking-period uint))
  (let (
    (reward-rate (calculate-reward-rate staking-period))
    (end-block (+ block-height staking-period))
    (current-stats (default-to { total-staked-ever: u0, total-rewards-earned: u0, stake-count: u0, first-stake-block: u0 }
                               (map-get? user-stats { user: user })))
    (is-first-stake (is-eq (get stake-count current-stats) u0))
  )
    (begin
      ;; Update user stake
      (map-set user-stakes { user: user }
        {
          amount: amount,
          start-block: block-height,
          end-block: end-block,
          staking-period: staking-period,
          reward-rate: reward-rate,
          last-claim-block: block-height,
          is-active: true
        }
      )
      ;; Update user stats
      (map-set user-stats { user: user }
        {
          total-staked-ever: (+ (get total-staked-ever current-stats) amount),
          total-rewards-earned: (get total-rewards-earned current-stats),
          stake-count: (+ (get stake-count current-stats) u1),
          first-stake-block: (if is-first-stake block-height (get first-stake-block current-stats))
        }
      )
      true
    )
  )
)

(define-private (update-global-stats (amount uint) (is-new-user bool))
  (begin
    (var-set total-staked (+ (var-get total-staked) amount))
    (if is-new-user 
      (var-set total-users (+ (var-get total-users) u1))
      true
    )
    true
  )
)

(define-private (deactivate-user-stake (user principal))
  (match (map-get? user-stakes { user: user })
    stake-info
    (begin
      (map-set user-stakes { user: user }
        (merge stake-info { is-active: false })
      )
      (var-set total-staked (- (var-get total-staked) (get amount stake-info)))
      true
    )
    false
  )
)

;; Reward distribution functions
(define-private (distribute-rewards-to-user (user principal) (reward-amount uint))
  (let (
    (fee-calculation (apply-protocol-fee reward-amount))
    (net-reward (get net-reward fee-calculation))
    (fee-amount (get fee fee-calculation))
    (current-rewards (default-to { pending-rewards: u0, total-claimed: u0 }
                                 (map-get? user-rewards { user: user })))
  )
    (begin
      ;; Update user rewards
      (map-set user-rewards { user: user }
        {
          pending-rewards: u0,
          total-claimed: (+ (get total-claimed current-rewards) net-reward)
        }
      )
      ;; Update global stats
      (var-set total-rewards-distributed (+ (var-get total-rewards-distributed) net-reward))
      (var-set reward-pool-balance (- (var-get reward-pool-balance) reward-amount))
      ;; Return net reward amount
      net-reward
    )
  )
)

(define-private (update-last-claim-block (user principal))
  (match (map-get? user-stakes { user: user })
    stake-info
    (map-set user-stakes { user: user }
      (merge stake-info { last-claim-block: block-height })
    )
    false
  )
)

;; Helper functions
(define-private (is-staking-period-complete (user principal))
  (match (map-get? user-stakes { user: user })
    stake-info
    (>= block-height (get end-block stake-info))
    false
  )
)

(define-private (get-blocks-remaining (user principal))
  (match (map-get? user-stakes { user: user })
    stake-info
    (if (> (get end-block stake-info) block-height)
      (some (- (get end-block stake-info) block-height))
      (some u0)
    )
    none
  )
)

(define-private (has-sufficient-reward-pool (required-amount uint))
  (>= (var-get reward-pool-balance) required-amount)
)

;; public functions
;;
