
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

;; Core staking functions
(define-public (stake-stx (amount uint) (staking-period uint))
  (let (
    (user tx-sender)
    (current-stake (map-get? user-stakes { user: user }))
    (is-new-user (is-none current-stake))
  )
    (asserts! (is-contract-operational) err-paused)
    (asserts! (validate-stake-amount amount) err-invalid-amount)
    (asserts! (validate-staking-period staking-period) err-invalid-amount)
    (asserts! (can-stake-more user amount) err-invalid-amount)
    (asserts! (is-none current-stake) err-already-staked)
    
    (match (stx-transfer? amount user (as-contract tx-sender))
      success (begin
        (asserts! (update-user-stake user amount staking-period) err-unauthorized)
        (asserts! (update-global-stats amount is-new-user) err-unauthorized)
        (print { 
          event: "stake-created",
          user: user,
          amount: amount,
          staking-period: staking-period,
          end-block: (+ block-height staking-period)
        })
        (ok true)
      )
      error err-insufficient-balance
    )
  )
)

(define-public (unstake-stx)
  (let (
    (user tx-sender)
    (stake-info (unwrap! (map-get? user-stakes { user: user }) err-not-staked))
    (staked-amount (get amount stake-info))
    (is-period-complete (is-staking-period-complete user))
    (penalty-amount (if is-period-complete u0 (calculate-early-withdrawal-penalty staked-amount)))
    (withdrawal-amount (- staked-amount penalty-amount))
  )
    (asserts! (is-contract-operational) err-paused)
    (asserts! (get is-active stake-info) err-not-staked)
    
    ;; Calculate and distribute any pending rewards before unstaking
    (let ((pending-rewards (calculate-pending-rewards user)))
      (if (and (> pending-rewards u0) (has-sufficient-reward-pool pending-rewards))
        (begin
          (let ((net-reward (distribute-rewards-to-user user pending-rewards)))
            (asserts! (update-last-claim-block user) err-unauthorized)
            ;; Transfer rewards to user
            (match (as-contract (stx-transfer? net-reward tx-sender user))
              success true
              error false
            )
          )
        )
        true
      )
    )
    
    ;; Deactivate stake and update global stats
    (asserts! (deactivate-user-stake user) err-unauthorized)
    
    ;; Transfer tokens back to user
    (match (as-contract (stx-transfer? withdrawal-amount tx-sender user))
      success (begin
        ;; Handle penalty if applicable
        (if (> penalty-amount u0)
          (begin
            (match (as-contract (stx-transfer? penalty-amount tx-sender (var-get fee-recipient)))
              penalty-success true
              penalty-error false
            )
          )
          true
        )
        (print {
          event: "stake-withdrawn",
          user: user,
          amount: withdrawal-amount,
          penalty: penalty-amount,
          early-withdrawal: (not is-period-complete)
        })
        (ok withdrawal-amount)
      )
      error err-insufficient-balance
    )
  )
)

(define-public (claim-rewards)
  (let (
    (user tx-sender)
    (stake-info (unwrap! (map-get? user-stakes { user: user }) err-not-staked))
    (pending-rewards (calculate-pending-rewards user))
  )
    (asserts! (is-contract-operational) err-paused)
    (asserts! (get is-active stake-info) err-not-staked)
    (asserts! (> pending-rewards u0) err-not-found)
    (asserts! (has-sufficient-reward-pool pending-rewards) err-insufficient-balance)
    
    (let ((net-reward (distribute-rewards-to-user user pending-rewards)))
      (asserts! (update-last-claim-block user) err-unauthorized)
      
      ;; Transfer rewards to user
      (match (as-contract (stx-transfer? net-reward tx-sender user))
        success (begin
          (print {
            event: "rewards-claimed",
            user: user,
            gross-reward: pending-rewards,
            net-reward: net-reward,
            block-height: block-height
          })
          (ok net-reward)
        )
        error err-insufficient-balance
      )
    )
  )
)

(define-public (extend-staking-period (new-period uint))
  (let (
    (user tx-sender)
    (stake-info (unwrap! (map-get? user-stakes { user: user }) err-not-staked))
    (current-end-block (get end-block stake-info))
    (new-end-block (+ block-height new-period))
    (new-reward-rate (calculate-reward-rate new-period))
  )
    (asserts! (is-contract-operational) err-paused)
    (asserts! (get is-active stake-info) err-not-staked)
    (asserts! (validate-staking-period new-period) err-invalid-amount)
    (asserts! (> new-end-block current-end-block) err-invalid-amount)
    
    ;; Claim any pending rewards first
    (let ((pending-rewards (calculate-pending-rewards user)))
      (if (and (> pending-rewards u0) (has-sufficient-reward-pool pending-rewards))
        (begin
          (let ((net-reward (distribute-rewards-to-user user pending-rewards)))
            (asserts! (update-last-claim-block user) err-unauthorized)
            ;; Transfer rewards to user
            (match (as-contract (stx-transfer? net-reward tx-sender user))
              success true
              error false
            )
          )
        )
        true
      )
    )
    
    ;; Update stake with new period and reward rate
    (map-set user-stakes { user: user }
      (merge stake-info {
        end-block: new-end-block,
        staking-period: new-period,
        reward-rate: new-reward-rate,
        last-claim-block: block-height
      })
    )
    
    (print {
      event: "staking-period-extended",
      user: user,
      old-end-block: current-end-block,
      new-end-block: new-end-block,
      new-reward-rate: new-reward-rate
    })
    (ok true)
  )
)

;; Administrative functions
(define-public (pause-contract)
  (begin
    (asserts! (is-authorized-admin tx-sender) err-owner-only)
    (var-set contract-paused true)
    (print { event: "contract-paused", admin: tx-sender })
    (ok true)
  )
)

(define-public (unpause-contract)
  (begin
    (asserts! (is-authorized-admin tx-sender) err-owner-only)
    (var-set contract-paused false)
    (print { event: "contract-unpaused", admin: tx-sender })
    (ok true)
  )
)

(define-public (trigger-emergency-shutdown)
  (begin
    (asserts! (is-authorized-admin tx-sender) err-owner-only)
    (var-set emergency-shutdown true)
    (var-set contract-paused true)
    (print { event: "emergency-shutdown", admin: tx-sender })
    (ok true)
  )
)

(define-public (set-staking-enabled (enabled bool))
  (begin
    (asserts! (is-authorized-admin tx-sender) err-owner-only)
    (var-set staking-enabled enabled)
    (print { event: "staking-enabled-changed", enabled: enabled, admin: tx-sender })
    (ok true)
  )
)

(define-public (update-protocol-fee-rate (new-rate uint))
  (begin
    (asserts! (is-authorized-admin tx-sender) err-owner-only)
    (asserts! (<= new-rate u2000) err-invalid-amount) ;; Max 20% fee
    (var-set protocol-fee-rate new-rate)
    (print { event: "protocol-fee-updated", new-rate: new-rate, admin: tx-sender })
    (ok true)
  )
)

(define-public (update-staking-limits (min-amount uint) (max-amount uint))
  (begin
    (asserts! (is-authorized-admin tx-sender) err-owner-only)
    (asserts! (< min-amount max-amount) err-invalid-amount)
    (var-set min-stake-global min-amount)
    (var-set max-stake-per-user max-amount)
    (print { 
      event: "staking-limits-updated", 
      min-amount: min-amount, 
      max-amount: max-amount, 
      admin: tx-sender 
    })
    (ok true)
  )
)

(define-public (fund-reward-pool (amount uint))
  (begin
    (asserts! (is-authorized-admin tx-sender) err-owner-only)
    (match (stx-transfer? amount tx-sender (as-contract tx-sender))
      success (begin
        (var-set reward-pool-balance (+ (var-get reward-pool-balance) amount))
        (print { event: "reward-pool-funded", amount: amount, admin: tx-sender })
        (ok true)
      )
      error err-insufficient-balance
    )
  )
)

(define-public (set-pending-admin (new-admin principal))
  (begin
    (asserts! (is-contract-owner tx-sender) err-owner-only)
    (var-set pending-admin (some new-admin))
    (print { event: "pending-admin-set", new-admin: new-admin, owner: tx-sender })
    (ok true)
  )
)

(define-public (accept-admin)
  (let ((pending (unwrap! (var-get pending-admin) err-not-found)))
    (asserts! (is-eq tx-sender pending) err-unauthorized)
    (var-set contract-admin pending)
    (var-set pending-admin none)
    (print { event: "admin-transferred", new-admin: pending })
    (ok true)
  )
)

;; Read-only functions
(define-read-only (get-user-stake (user principal))
  (map-get? user-stakes { user: user })
)

(define-read-only (get-user-rewards (user principal))
  (map-get? user-rewards { user: user })
)

(define-read-only (get-user-stats (user principal))
  (map-get? user-stats { user: user })
)

(define-read-only (get-pending-rewards (user principal))
  (calculate-pending-rewards user)
)

(define-read-only (get-contract-stats)
  {
    total-staked: (var-get total-staked),
    total-users: (var-get total-users),
    total-rewards-distributed: (var-get total-rewards-distributed),
    reward-pool-balance: (var-get reward-pool-balance),
    contract-paused: (var-get contract-paused),
    emergency-shutdown: (var-get emergency-shutdown),
    staking-enabled: (var-get staking-enabled)
  }
)

(define-read-only (get-staking-info (user principal))
  (match (map-get? user-stakes { user: user })
    stake-info
    (some {
      stake: stake-info,
      pending-rewards: (calculate-pending-rewards user),
      blocks-remaining: (unwrap-panic (get-blocks-remaining user)),
      period-complete: (is-staking-period-complete user)
    })
    none
  )
)

(define-read-only (get-reward-rate-for-period (staking-period uint))
  (calculate-reward-rate staking-period)
)

(define-read-only (estimate-rewards (amount uint) (staking-period uint))
  (let (
    (reward-rate (calculate-reward-rate staking-period))
    (estimated-reward (calculate-time-based-rewards amount reward-rate staking-period))
    (fee-calc (apply-protocol-fee estimated-reward))
  )
    {
      gross-reward: estimated-reward,
      net-reward: (get net-reward fee-calc),
      fee: (get fee fee-calc),
      reward-rate: reward-rate
    }
  )
)

(define-read-only (is-user-staking (user principal))
  (match (map-get? user-stakes { user: user })
    stake-info (get is-active stake-info)
    false
  )
)

(define-read-only (get-contract-admin)
  (var-get contract-admin)
)

(define-read-only (get-pending-admin)
  (var-get pending-admin)
)
