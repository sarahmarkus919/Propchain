(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_PROPERTY_NOT_FOUND (err u101))
(define-constant ERR_INSUFFICIENT_SHARES (err u102))
(define-constant ERR_PROPERTY_EXISTS (err u103))
(define-constant ERR_INVALID_AMOUNT (err u104))
(define-constant ERR_NOT_OWNER (err u105))
(define-constant ERR_TRANSFER_FAILED (err u106))
(define-constant ERR_INVALID_SHARES (err u107))

(define-data-var next-property-id uint u1)

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