;; oracle-registry.clar
;; ------------------------------------------------------------
;; Oracle Registry Contract for Stacks (STX)
;; - Oracles register by staking STX.
;; - Oracles submit value updates (key/value) which are stored on-chain.
;; - Admin can slash or deactivate misbehaving oracles.
;; - Consumers can query latest/historical values.
;; ------------------------------------------------------------

(define-constant ERR_NOT_ADMIN u100)
(define-constant ERR_ZERO_AMOUNT u101)
(define-constant ERR_ORACLE_NOT_FOUND u102)
(define-constant ERR_NOT_ORACLE_OWNER u103)
(define-constant ERR_ORACLE_INACTIVE u104)
(define-constant ERR_STAKE_TOO_LOW u105)
(define-constant ERR_INSUFFICIENT_FUNDS u106)
(define-constant ERR_ALREADY_REGISTERED u107)
(define-constant ERR_INVALID_PARAM u108)
(define-constant ERR_NO_DATA u109)
(define-constant ERR_NOT_ALLOWED u110)

;; Minimum stake required to register (microSTX). Admin can change.
(define-data-var admin principal tx-sender)
(define-data-var min-stake uint u1000000) ;; default 1 STX (1,000,000 microSTX)

;; Oracle id counter
(define-data-var next-oracle-id uint u1)

;; Oracles map:
;; key: { oracle-id: uint }
;; value: { owner: principal, name: (string-ascii 64), endpoint: (string-ascii 128),
;;          stake: uint, active: bool, version: uint }
(define-map oracles
  { oracle-id: uint }
  {
    owner: principal,
    name: (string-ascii 64),
    endpoint: (string-ascii 128),
    stake: uint,
    active: bool,
    version: uint
  })

;; Per-oracle next data index
(define-map next-data-index { oracle-id: uint } { index: uint })

;; Data points map:
;; key: { oracle-id: uint, index: uint }
;; value: { key-str: (string-ascii 64), value-str: (string-ascii 256), block: uint, tx-sender: principal }
(define-map data-points
  { oracle-id: uint, index: uint }
  {
    key-str: (string-ascii 64),
    value-str: (string-ascii 256),
    block: uint,
    sender: principal
  })

;; Slashed funds accumulator (admin can withdraw)
(define-data-var slashed-balance uint u0)

;; Events
(define-private (ev-oracle-registered (id uint) (owner principal) (name (string-ascii 64)) (endpoint (string-ascii 128)) (stake uint))
  (print { event: "oracle-registered", oracle_id: id, owner: owner, name: name, endpoint: endpoint, stake: stake }))

(define-private (ev-oracle-updated (id uint) (by principal) (field (string-ascii 32)) (version uint))
  (print { event: "oracle-updated", oracle_id: id, by: by, field: field, version: version }))

(define-private (ev-data-submitted (oracle-id uint) (index uint) (key (string-ascii 64)) (value (string-ascii 256)) (block uint) (by principal))
  (print { event: "oracle-data", oracle_id: oracle-id, index: index, key: key, value: value, block: block, sender: by }))

(define-private (ev-oracle-deactivated (id uint) (by principal))
  (print { event: "oracle-deactivated", oracle_id: id, by: by }))

(define-private (ev-oracle-reactivated (id uint) (by principal))
  (print { event: "oracle-reactivated", oracle_id: id, by: by }))

(define-private (ev-slashed (id uint) (amount uint) (to-admin principal))
  (print { event: "oracle-slashed", oracle_id: id, amount: amount, to: to-admin }))

(define-private (ev-stake-withdrawn (id uint) (owner principal) (amount uint))
  (print { event: "stake-withdrawn", oracle_id: id, owner: owner, amount: amount }))

(define-private (ev-stake-added (id uint) (owner principal) (amount uint))
  (print { event: "stake-added", oracle_id: id, owner: owner, amount: amount }))

;; -------------------------
;; Admin functions
;; -------------------------
(define-public (set-admin (p principal))
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) (err ERR_NOT_ADMIN))
    (var-set admin p)
    (ok true)))

(define-public (set-min-stake (amt uint))
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) (err ERR_NOT_ADMIN))
    (asserts! (> amt u0) (err ERR_INVALID_PARAM))
    (var-set min-stake amt)
    (ok amt)))

;; Slash an oracle's stake (admin action). amount moved to slashed-balance and oracle stake reduced.
(define-public (slash-oracle (oracle-id uint) (amount uint))
  (let ((o? (map-get? oracles { oracle-id: oracle-id })))
    (asserts! (is-some o?) (err ERR_ORACLE_NOT_FOUND))
    (let ((o (unwrap-panic o?)))
      (asserts! (is-eq tx-sender (var-get admin)) (err ERR_NOT_ADMIN))
      (let ((stake (get stake o)))
        (asserts! (>= stake amount) (err ERR_INSUFFICIENT_FUNDS))
        (begin
          (map-set oracles { oracle-id: oracle-id } (merge o { stake: (- stake amount), version: (+ (get version o) u1) }))
          (var-set slashed-balance (+ (var-get slashed-balance) amount))
          (ev-slashed oracle-id amount (var-get admin))
          (ok true))))))

(define-public (withdraw-slashed (to principal) (amount uint))
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) (err ERR_NOT_ADMIN))
    (asserts! (> amount u0) (err ERR_INVALID_PARAM))
    (asserts! (>= (var-get slashed-balance) amount) (err ERR_INSUFFICIENT_FUNDS))
    (var-set slashed-balance (- (var-get slashed-balance) amount))
    (try! (stx-transfer? amount tx-sender to))
    (ok true)))

;; Deactivate or reactivate an oracle (admin)
(define-public (deactivate-oracle (oracle-id uint))
  (let ((o? (map-get? oracles { oracle-id: oracle-id })))
    (asserts! (is-some o?) (err ERR_ORACLE_NOT_FOUND))
    (let ((o (unwrap-panic o?)))
      (asserts! (is-eq tx-sender (var-get admin)) (err ERR_NOT_ADMIN))
      (begin
        (map-set oracles { oracle-id: oracle-id } (merge o { active: false, version: (+ (get version o) u1) }))
        (ev-oracle-deactivated oracle-id tx-sender)
        (ok true)))))

(define-public (reactivate-oracle (oracle-id uint))
  (let ((o? (map-get? oracles { oracle-id: oracle-id })))
    (asserts! (is-some o?) (err ERR_ORACLE_NOT_FOUND))
    (let ((o (unwrap-panic o?)))
      (asserts! (is-eq tx-sender (var-get admin)) (err ERR_NOT_ADMIN))
      (begin
        (map-set oracles { oracle-id: oracle-id } (merge o { active: true, version: (+ (get version o) u1) }))
        (ev-oracle-reactivated oracle-id tx-sender)
        (ok true)))))

;; -------------------------
;; Oracle owner functions
;; -------------------------

;; Register a new oracle by staking `stake` microSTX; caller must send that STX in same call
(define-public (register-oracle (name (string-ascii 64)) (endpoint (string-ascii 128)) (stake uint))
  (begin
    (asserts! (> stake u0) (err ERR_ZERO_AMOUNT))
    (try! (stx-transfer? stake tx-sender tx-sender))
    (asserts! (>= stake (var-get min-stake)) (err ERR_STAKE_TOO_LOW))
    (let ((id (var-get next-oracle-id)))
      (begin
        (var-set next-oracle-id (+ id u1))
        (map-set oracles { oracle-id: id } { owner: tx-sender, name: name, endpoint: endpoint, stake: stake, active: true, version: u1 })
        (map-set next-data-index { oracle-id: id } { index: u0 })
        (ev-oracle-registered id tx-sender name endpoint stake)
        (ok id)))))

;; Add stake to an existing oracle
(define-public (add-stake (oracle-id uint) (amount uint))
  (let ((o? (map-get? oracles { oracle-id: oracle-id })))
    (asserts! (is-some o?) (err ERR_ORACLE_NOT_FOUND))
    (let ((o (unwrap-panic o?)))
      (asserts! (is-eq tx-sender (get owner o)) (err ERR_NOT_ORACLE_OWNER))
      (asserts! (> amount u0) (err ERR_ZERO_AMOUNT))
      (try! (stx-transfer? amount tx-sender tx-sender))
      (begin
        (map-set oracles { oracle-id: oracle-id } (merge o { stake: (+ (get stake o) amount), version: (+ (get version o) u1) }))
        (ev-stake-added oracle-id tx-sender amount)
        (ok true)))))

;; Withdraw stake (only allowed when oracle is deactivated). Owner may withdraw up to stake.
(define-public (withdraw-stake (oracle-id uint) (amount uint))
  (let ((o? (map-get? oracles { oracle-id: oracle-id })))
    (asserts! (is-some o?) (err ERR_ORACLE_NOT_FOUND))
    (let ((o (unwrap-panic o?)))
      (asserts! (is-eq tx-sender (get owner o)) (err ERR_NOT_ORACLE_OWNER))
      (asserts! (not (get active o)) (err ERR_ORACLE_INACTIVE))
      (let ((stake (get stake o)))
        (asserts! (>= stake amount) (err ERR_INSUFFICIENT_FUNDS))
        (begin
          (map-set oracles { oracle-id: oracle-id } (merge o { stake: (- stake amount), version: (+ (get version o) u1) }))
          (try! (stx-transfer? amount tx-sender tx-sender))
          (ev-stake-withdrawn oracle-id tx-sender amount)
          (ok true))))))

;; Update oracle metadata (owner)
(define-public (update-oracle (oracle-id uint) (name (string-ascii 64)) (endpoint (string-ascii 128)))
  (let ((o? (map-get? oracles { oracle-id: oracle-id })))
    (asserts! (is-some o?) (err ERR_ORACLE_NOT_FOUND))
    (let ((o (unwrap-panic o?)))
      (asserts! (is-eq tx-sender (get owner o)) (err ERR_NOT_ORACLE_OWNER))
      (begin
        (map-set oracles { oracle-id: oracle-id } (merge o { name: name, endpoint: endpoint, version: (+ (get version o) u1) }))
        (ev-oracle-updated oracle-id tx-sender "meta" (+ (get version o) u1))
        (ok true)))))

;; Transfer ownership of oracle
(define-public (transfer-oracle (oracle-id uint) (to principal))
  (let ((o? (map-get? oracles { oracle-id: oracle-id })))
    (asserts! (is-some o?) (err ERR_ORACLE_NOT_FOUND))
    (let ((o (unwrap-panic o?)))
      (asserts! (is-eq tx-sender (get owner o)) (err ERR_NOT_ORACLE_OWNER))
      (begin
        (map-set oracles { oracle-id: oracle-id } (merge o { owner: to, version: (+ (get version o) u1) }))
        (ev-oracle-updated oracle-id tx-sender "transfer" (+ (get version o) u1))
        (ok true)))))

;; -------------------------
;; Data submission by oracle
;; -------------------------
;; Oracles submit key/value (both strings). Caller must be oracle owner, oracle active.
;; Each submission increments the next-data-index and stores the data point.
(define-public (submit-data (oracle-id uint) (key (string-ascii 64)) (value (string-ascii 256)))
  (let ((o? (map-get? oracles { oracle-id: oracle-id })))
    (asserts! (is-some o?) (err ERR_ORACLE_NOT_FOUND))
    (let ((o (unwrap-panic o?)))
      (asserts! (is-eq tx-sender (get owner o)) (err ERR_NOT_ORACLE_OWNER))
      (asserts! (get active o) (err ERR_ORACLE_INACTIVE))
      (let ((ni (default-to u0 (get index (map-get? next-data-index { oracle-id: oracle-id })))))
        (begin
          (map-set data-points { oracle-id: oracle-id, index: ni } { key-str: key, value-str: value, block: burn-block-height, sender: tx-sender })
          (map-set next-data-index { oracle-id: oracle-id } { index: (+ ni u1) })
          (ev-data-submitted oracle-id ni key value burn-block-height tx-sender)
          (ok ni))))))

;; -------------------------
;; Views
;; -------------------------
(define-read-only (get-oracle (oracle-id uint))
  (ok (map-get? oracles { oracle-id: oracle-id })))

(define-read-only (get-next-oracle-id) (ok (var-get next-oracle-id)))

(define-read-only (get-next-data-index (oracle-id uint))
  (ok (default-to u0 (get index (map-get? next-data-index { oracle-id: oracle-id })))))

(define-read-only (get-data (oracle-id uint) (index uint))
  (let ((dp? (map-get? data-points { oracle-id: oracle-id, index: index })))
    (if (is-none dp?) (err ERR_NO_DATA) (ok (unwrap-panic dp?)))))

(define-read-only (get-latest (oracle-id uint))
  (let ((ni (default-to u0 (get index (map-get? next-data-index { oracle-id: oracle-id })))))
    (if (<= ni u0)
        (err ERR_NO_DATA)
        (ok (map-get? data-points { oracle-id: oracle-id, index: (- ni u1) })))))

(define-read-only (get-slashed-balance) (ok (var-get slashed-balance)))

(define-read-only (is-oracle-active (oracle-id uint))
  (let ((o? (map-get? oracles { oracle-id: oracle-id })))
    (if (is-none o?) (err ERR_ORACLE_NOT_FOUND) (ok (get active (unwrap-panic o?)))))
)
