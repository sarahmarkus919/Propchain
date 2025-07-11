(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_PROPERTY_NOT_FOUND (err u101))
(define-constant ERR_INSUFFICIENT_SHARES (err u102))
(define-constant ERR_PROPERTY_EXISTS (err u103))
(define-constant ERR_INVALID_AMOUNT (err u104))
(define-constant ERR_NOT_OWNER (err u105))
(define-constant ERR_TRANSFER_FAILED (err u106))
(define-constant ERR_INVALID_SHARES (err u107))
(define-constant ERR_NO_INCOME_AVAILABLE (err u108))
(define-constant ERR_ALREADY_CLAIMED (err u109))
(define-constant ERR_INVALID_PERIOD (err u110))
(define-constant ERR_RENTAL_PERIOD_ACTIVE (err u111))
(define-constant ERR_RENTAL_PERIOD_ENDED (err u112))

(define-data-var next-property-id uint u1)
(define-data-var next-rental-period-id uint u1)

(define-map properties
  { property-id: uint }
  {
    name: (string-ascii 100),
    description: (string-ascii 500),
    total-shares: uint,
    share-price: uint,
    owner: principal,
    created-at: uint
  }
)

(define-map property-shares
  { property-id: uint, owner: principal }
  { shares: uint }
)

(define-map property-shareholders
  { property-id: uint }
  { shareholders: (list 100 principal) }
)

(define-map user-properties
  { user: principal }
  { properties: (list 50 uint) }
)

(define-map rental-periods
  { rental-period-id: uint }
  {
    property-id: uint,
    total-income: uint,
    start-block: uint,
    end-block: uint,
    claimed-by: (list 100 principal),
    is-active: bool
  }
)

(define-map property-rental-periods
  { property-id: uint }
  { rental-periods: (list 100 uint) }
)

(define-map rental-income-claims
  { rental-period-id: uint, claimant: principal }
  { amount: uint, claimed-at: uint }
)

(define-map property-income-history
  { property-id: uint }
  { total-income: uint, last-distribution: uint }
)

(define-public (create-property (name (string-ascii 100)) (description (string-ascii 500)) (total-shares uint) (share-price uint))
  (let
    (
      (property-id (var-get next-property-id))
    )
    (asserts! (> total-shares u0) ERR_INVALID_SHARES)
    (asserts! (> share-price u0) ERR_INVALID_AMOUNT)
    (map-set properties
      { property-id: property-id }
      {
        name: name,
        description: description,
        total-shares: total-shares,
        share-price: share-price,
        owner: tx-sender,
        created-at: stacks-block-height
      }
    )
    (map-set property-shares
      { property-id: property-id, owner: tx-sender }
      { shares: total-shares }
    )
    (map-set property-shareholders
      { property-id: property-id }
      { shareholders: (list tx-sender) }
    )
    (map-set user-properties
      { user: tx-sender }
      { properties: (unwrap-panic (as-max-len? (append (get-user-properties tx-sender) property-id) u50)) }
    )
    (var-set next-property-id (+ property-id u1))
    (ok property-id)
  )
)

(define-public (buy-shares (property-id uint) (shares uint))
  (let
    (
      (property (unwrap! (map-get? properties { property-id: property-id }) ERR_PROPERTY_NOT_FOUND))
      (current-owner-shares (default-to u0 (get shares (map-get? property-shares { property-id: property-id, owner: (get owner property) }))))
      (buyer-current-shares (default-to u0 (get shares (map-get? property-shares { property-id: property-id, owner: tx-sender }))))
      (total-cost (* shares (get share-price property)))
    )
    (asserts! (> shares u0) ERR_INVALID_SHARES)
    (asserts! (>= current-owner-shares shares) ERR_INSUFFICIENT_SHARES)
    (try! (stx-transfer? total-cost tx-sender (get owner property)))
    (map-set property-shares
      { property-id: property-id, owner: (get owner property) }
      { shares: (- current-owner-shares shares) }
    )
    (map-set property-shares
      { property-id: property-id, owner: tx-sender }
      { shares: (+ buyer-current-shares shares) }
    )
    (if (is-eq buyer-current-shares u0)
      (begin
        (map-set property-shareholders
          { property-id: property-id }
          { shareholders: (unwrap-panic (as-max-len? (append (get-property-shareholders property-id) tx-sender) u100)) }
        )
        (map-set user-properties
          { user: tx-sender }
          { properties: (unwrap-panic (as-max-len? (append (get-user-properties tx-sender) property-id) u50)) }
        )
      )
      true
    )
    (ok shares)
  )
)

(define-public (transfer-shares (property-id uint) (recipient principal) (shares uint))
  (let
    (
      (sender-shares (default-to u0 (get shares (map-get? property-shares { property-id: property-id, owner: tx-sender }))))
      (recipient-shares (default-to u0 (get shares (map-get? property-shares { property-id: property-id, owner: recipient }))))
    )
    ;; (asserts! (map-get? properties { property-id: property-id }) ERR_PROPERTY_NOT_FOUND)
    (asserts! (> shares u0) ERR_INVALID_SHARES)
    (asserts! (>= sender-shares shares) ERR_INSUFFICIENT_SHARES)
    (map-set property-shares
      { property-id: property-id, owner: tx-sender }
      { shares: (- sender-shares shares) }
    )
    (map-set property-shares
      { property-id: property-id, owner: recipient }
      { shares: (+ recipient-shares shares) }
    )
    (if (is-eq recipient-shares u0)
      (begin
        (map-set property-shareholders
          { property-id: property-id }
          { shareholders: (unwrap-panic (as-max-len? (append (get-property-shareholders property-id) recipient) u100)) }
        )
        (map-set user-properties
          { user: recipient }
          { properties: (unwrap-panic (as-max-len? (append (get-user-properties recipient) property-id) u50)) }
        )
      )
      true
    )
    (ok shares)
  )
)

(define-public (update-share-price (property-id uint) (new-price uint))
  (let
    (
      (property (unwrap! (map-get? properties { property-id: property-id }) ERR_PROPERTY_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get owner property)) ERR_NOT_AUTHORIZED)
    (asserts! (> new-price u0) ERR_INVALID_AMOUNT)
    (map-set properties
      { property-id: property-id }
      (merge property { share-price: new-price })
    )
    (ok new-price)
  )
)

(define-read-only (get-property (property-id uint))
  (map-get? properties { property-id: property-id })
)

(define-read-only (get-user-shares (property-id uint) (user principal))
  (default-to u0 (get shares (map-get? property-shares { property-id: property-id, owner: user })))
)

(define-read-only (get-property-shareholders (property-id uint))
  (default-to (list) (get shareholders (map-get? property-shareholders { property-id: property-id })))
)

(define-read-only (get-user-properties (user principal))
  (default-to (list) (get properties (map-get? user-properties { user: user })))
)

(define-read-only (get-total-properties)
  (- (var-get next-property-id) u1)
)

(define-read-only (calculate-ownership-percentage (property-id uint) (user principal))
  (let
    (
      (property (unwrap! (map-get? properties { property-id: property-id }) (err u0)))
      (user-shares (get-user-shares property-id user))
      (total-shares (get total-shares property))
    )
    (if (is-eq total-shares u0)
      (ok u0)
      (ok (/ (* user-shares u10000) total-shares))
    )
  )
)

(define-read-only (get-property-value (property-id uint))
  (let
    (
      (property (unwrap! (map-get? properties { property-id: property-id }) (err u0)))
    )
    (ok (* (get total-shares property) (get share-price property)))
  )
)

(define-read-only (get-user-portfolio-value (user principal))
  (let
    (
      (user-props (get-user-properties user))
    )
    (ok (fold calculate-user-property-value user-props u0))
  )
)

(define-private (calculate-user-property-value (property-id uint) (acc uint))
  (let
    (
      (user-shares (get-user-shares property-id tx-sender))
      (property (unwrap-panic (map-get? properties { property-id: property-id })))
      (share-price (get share-price property))
    )
    (+ acc (* user-shares share-price))
  )
)

(define-public (create-rental-period (property-id uint) (duration-blocks uint))
  (let
    (
      (property (unwrap! (map-get? properties { property-id: property-id }) ERR_PROPERTY_NOT_FOUND))
      (rental-period-id (var-get next-rental-period-id))
      (current-block stacks-block-height)
      (end-block (+ current-block duration-blocks))
    )
    (asserts! (is-eq tx-sender (get owner property)) ERR_NOT_AUTHORIZED)
    (asserts! (> duration-blocks u0) ERR_INVALID_PERIOD)
    (map-set rental-periods
      { rental-period-id: rental-period-id }
      {
        property-id: property-id,
        total-income: u0,
        start-block: current-block,
        end-block: end-block,
        claimed-by: (list),
        is-active: true
      }
    )
    (map-set property-rental-periods
      { property-id: property-id }
      { rental-periods: (unwrap-panic (as-max-len? (append (get-property-rental-periods property-id) rental-period-id) u100)) }
    )
    (var-set next-rental-period-id (+ rental-period-id u1))
    (ok rental-period-id)
  )
)

(define-public (deposit-rental-income (rental-period-id uint) (amount uint))
  (let
    (
      (rental-period (unwrap! (map-get? rental-periods { rental-period-id: rental-period-id }) ERR_INVALID_PERIOD))
      (property (unwrap! (map-get? properties { property-id: (get property-id rental-period) }) ERR_PROPERTY_NOT_FOUND))
      (current-block stacks-block-height)
      (property-income (default-to { total-income: u0, last-distribution: u0 } (map-get? property-income-history { property-id: (get property-id rental-period) })))
    )
    (asserts! (is-eq tx-sender (get owner property)) ERR_NOT_AUTHORIZED)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (get is-active rental-period) ERR_RENTAL_PERIOD_ENDED)
    (asserts! (<= current-block (get end-block rental-period)) ERR_RENTAL_PERIOD_ENDED)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (map-set rental-periods
      { rental-period-id: rental-period-id }
      (merge rental-period { total-income: (+ (get total-income rental-period) amount) })
    )
    (map-set property-income-history
      { property-id: (get property-id rental-period) }
      { total-income: (+ (get total-income property-income) amount), last-distribution: current-block }
    )
    (ok amount)
  )
)

(define-public (finalize-rental-period (rental-period-id uint))
  (let
    (
      (rental-period (unwrap! (map-get? rental-periods { rental-period-id: rental-period-id }) ERR_INVALID_PERIOD))
      (property (unwrap! (map-get? properties { property-id: (get property-id rental-period) }) ERR_PROPERTY_NOT_FOUND))
      (current-block stacks-block-height)
    )
    (asserts! (is-eq tx-sender (get owner property)) ERR_NOT_AUTHORIZED)
    (asserts! (get is-active rental-period) ERR_RENTAL_PERIOD_ENDED)
    (asserts! (>= current-block (get end-block rental-period)) ERR_RENTAL_PERIOD_ACTIVE)
    (map-set rental-periods
      { rental-period-id: rental-period-id }
      (merge rental-period { is-active: false })
    )
    (ok rental-period-id)
  )
)

(define-public (claim-rental-income (rental-period-id uint))
  (let
    (
      (rental-period (unwrap! (map-get? rental-periods { rental-period-id: rental-period-id }) ERR_INVALID_PERIOD))
      (property (unwrap! (map-get? properties { property-id: (get property-id rental-period) }) ERR_PROPERTY_NOT_FOUND))
      (claimant-shares (get-user-shares (get property-id rental-period) tx-sender))
      (total-shares (get total-shares property))
      (total-income (get total-income rental-period))
      (claimant-income (/ (* total-income claimant-shares) total-shares))
      (claimed-by (get claimed-by rental-period))
      (existing-claim (map-get? rental-income-claims { rental-period-id: rental-period-id, claimant: tx-sender }))
    )
    (asserts! (not (get is-active rental-period)) ERR_RENTAL_PERIOD_ACTIVE)
    (asserts! (> claimant-shares u0) ERR_INSUFFICIENT_SHARES)
    (asserts! (> total-income u0) ERR_NO_INCOME_AVAILABLE)
    (asserts! (is-none existing-claim) ERR_ALREADY_CLAIMED)
    (asserts! (not (is-some (index-of claimed-by tx-sender))) ERR_ALREADY_CLAIMED)
    (try! (as-contract (stx-transfer? claimant-income tx-sender tx-sender)))
    (map-set rental-income-claims
      { rental-period-id: rental-period-id, claimant: tx-sender }
      { amount: claimant-income, claimed-at: stacks-block-height }
    )
    (map-set rental-periods
      { rental-period-id: rental-period-id }
      (merge rental-period { claimed-by: (unwrap-panic (as-max-len? (append claimed-by tx-sender) u100)) })
    )
    (ok claimant-income)
  )
)

(define-public (bulk-claim-rental-income (rental-period-ids (list 20 uint)))
  (let
    (
      (results (map claim-single-rental-income rental-period-ids))
    )
    (ok results)
  )
)

(define-private (claim-single-rental-income (rental-period-id uint))
  (match (claim-rental-income rental-period-id)
    success success
    error u0
  )
)

(define-read-only (get-rental-period (rental-period-id uint))
  (map-get? rental-periods { rental-period-id: rental-period-id })
)

(define-read-only (get-property-rental-periods (property-id uint))
  (default-to (list) (get rental-periods (map-get? property-rental-periods { property-id: property-id })))
)

(define-read-only (get-rental-income-claim (rental-period-id uint) (claimant principal))
  (map-get? rental-income-claims { rental-period-id: rental-period-id, claimant: claimant })
)

(define-read-only (get-property-income-history (property-id uint))
  (map-get? property-income-history { property-id: property-id })
)

(define-read-only (calculate-claimable-income (rental-period-id uint) (claimant principal))
  (let
    (
      (rental-period (unwrap! (map-get? rental-periods { rental-period-id: rental-period-id }) (err u0)))
      (property (unwrap! (map-get? properties { property-id: (get property-id rental-period) }) (err u0)))
      (claimant-shares (get-user-shares (get property-id rental-period) claimant))
      (total-shares (get total-shares property))
      (total-income (get total-income rental-period))
      (existing-claim (map-get? rental-income-claims { rental-period-id: rental-period-id, claimant: claimant }))
    )
    (if (is-some existing-claim)
      (ok u0)
      (if (is-eq total-shares u0)
        (ok u0)
        (ok (/ (* total-income claimant-shares) total-shares))
      )
    )
  )
)

(define-read-only (get-unclaimed-rental-periods (user principal))
  (let
    (
      (user-props (get-user-properties user))
    )
    (fold get-property-unclaimed-periods user-props (list))
  )
)

(define-private (get-property-unclaimed-periods (property-id uint) (acc (list 100 uint)))
  (let
    (
      (property-periods (get-property-rental-periods property-id))
      (unclaimed-periods (filter is-period-unclaimed-by-current-user property-periods))
    )
    (fold merge-period-lists unclaimed-periods acc)
  )
)

(define-private (is-period-unclaimed-by-current-user (rental-period-id uint))
  (let
    (
      (existing-claim (map-get? rental-income-claims { rental-period-id: rental-period-id, claimant: tx-sender }))
      (rental-period (map-get? rental-periods { rental-period-id: rental-period-id }))
    )
    (and 
      (is-none existing-claim)
      (is-some rental-period)
      (not (get is-active (unwrap-panic rental-period)))
    )
  )
)

(define-private (merge-period-lists (period-id uint) (acc (list 100 uint)))
  (unwrap-panic (as-max-len? (append acc period-id) u100))
)

(define-read-only (get-total-claimable-income (user principal))
  (let
    (
      (unclaimed-periods (get-unclaimed-rental-periods user))
    )
    (fold calculate-total-claimable unclaimed-periods u0)
  )
)

(define-private (calculate-total-claimable (rental-period-id uint) (acc uint))
  (let
    (
      (claimable-amount (unwrap-panic (calculate-claimable-income rental-period-id tx-sender)))
    )
    (+ acc claimable-amount)
  )
)