;; Wedding Registry Core Contract
;; Handles basic gift management, claiming, and duplicate prevention

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-GIFT-NOT-FOUND (err u101))
(define-constant ERR-GIFT-ALREADY-CLAIMED (err u102))
(define-constant ERR-INVALID-AMOUNT (err u103))
(define-constant ERR-REGISTRY-NOT-FOUND (err u104))
(define-constant ERR-NOT-REGISTRY-OWNER (err u105))
(define-constant ERR-REGISTRY-LOCKED (err u106))

;; Data Variables
(define-data-var next-registry-id uint u1)
(define-data-var next-gift-id uint u1)

;; Data Maps
(define-map registries
    uint
    {
        couple-name: (string-ascii 100),
        owner: principal,
        wedding-date: uint,
        is-active: bool,
        created-at: uint
    }
)

(define-map gifts
    uint
    {
        registry-id: uint,
        name: (string-ascii 100),
        description: (string-ascii 500),
        price: uint,
        category: (string-ascii 50),
        claimed-by: (optional principal),
        claimed-at: (optional uint),
        is-pool-eligible: bool
    }
)

(define-map registry-gifts
    {registry-id: uint, gift-id: uint}
    bool
)

(define-map user-registries
    principal
    (list 50 uint)
)

;; Private Functions

(define-private (is-registry-owner (registry-id uint) (user principal))
    (match (map-get? registries registry-id)
        registry (is-eq (get owner registry) user)
        false
    )
)

(define-private (is-gift-in-registry (registry-id uint) (gift-id uint))
    (default-to false (map-get? registry-gifts {registry-id: registry-id, gift-id: gift-id}))
)

(define-private (add-gift-to-registry (registry-id uint) (gift-id uint))
    (map-set registry-gifts {registry-id: registry-id, gift-id: gift-id} true)
)

;; Public Functions

;; Create a new wedding registry
(define-public (create-registry (couple-name (string-ascii 100)) (wedding-date uint))
    (let
        (
            (registry-id (var-get next-registry-id))
            (current-height burn-block-height)
            (current-registries (default-to (list) (map-get? user-registries tx-sender)))
        )
        (asserts! (> (len couple-name) u0) ERR-INVALID-AMOUNT)
        (asserts! (> wedding-date current-height) ERR-INVALID-AMOUNT)

        (map-set registries
            registry-id
            {
                couple-name: couple-name,
                owner: tx-sender,
                wedding-date: wedding-date,
                is-active: true,
                created-at: current-height
            }
        )

        ;; Update user's registry list
        (map-set user-registries tx-sender (unwrap! (as-max-len? (append current-registries registry-id) u50) ERR-INVALID-AMOUNT))

        (var-set next-registry-id (+ registry-id u1))
        (ok registry-id)
    )
)

;; Add a gift to a registry
(define-public (add-gift
    (registry-id uint)
    (name (string-ascii 100))
    (description (string-ascii 500))
    (price uint)
    (category (string-ascii 50))
    (is-pool-eligible bool)
)
    (let
        (
            (gift-id (var-get next-gift-id))
            (registry (unwrap! (map-get? registries registry-id) ERR-REGISTRY-NOT-FOUND))
        )
        (asserts! (is-registry-owner registry-id tx-sender) ERR-NOT-REGISTRY-OWNER)
        (asserts! (get is-active registry) ERR-REGISTRY-LOCKED)
        (asserts! (> (len name) u0) ERR-INVALID-AMOUNT)
        (asserts! (> price u0) ERR-INVALID-AMOUNT)

        (map-set gifts
            gift-id
            {
                registry-id: registry-id,
                name: name,
                description: description,
                price: price,
                category: category,
                claimed-by: none,
                claimed-at: none,
                is-pool-eligible: is-pool-eligible
            }
        )

        (add-gift-to-registry registry-id gift-id)
        (var-set next-gift-id (+ gift-id u1))
        (ok gift-id)
    )
)

;; Claim a gift (prevents duplicates)
(define-public (claim-gift (gift-id uint))
    (let
        (
            (gift (unwrap! (map-get? gifts gift-id) ERR-GIFT-NOT-FOUND))
            (current-height burn-block-height)
        )
        (asserts! (is-none (get claimed-by gift)) ERR-GIFT-ALREADY-CLAIMED)
        (asserts! (not (get is-pool-eligible gift)) ERR-NOT-AUTHORIZED) ;; Pool gifts handled differently

        (map-set gifts
            gift-id
            (merge gift {
                claimed-by: (some tx-sender),
                claimed-at: (some current-height)
            })
        )
        (ok true)
    )
)

;; Release a claimed gift (for mistakes)
(define-public (release-gift (gift-id uint))
    (let
        (
            (gift (unwrap! (map-get? gifts gift-id) ERR-GIFT-NOT-FOUND))
        )
        (asserts! (is-some (get claimed-by gift)) ERR-GIFT-NOT-FOUND)
        (asserts! (is-eq (some tx-sender) (get claimed-by gift)) ERR-NOT-AUTHORIZED)

        (map-set gifts
            gift-id
            (merge gift {
                claimed-by: none,
                claimed-at: none
            })
        )
        (ok true)
    )
)

;; Update registry status
(define-public (toggle-registry-status (registry-id uint))
    (let
        (
            (registry (unwrap! (map-get? registries registry-id) ERR-REGISTRY-NOT-FOUND))
        )
        (asserts! (is-registry-owner registry-id tx-sender) ERR-NOT-REGISTRY-OWNER)

        (map-set registries
            registry-id
            (merge registry {is-active: (not (get is-active registry))})
        )
        (ok (not (get is-active registry)))
    )
)

;; Read-only functions

(define-read-only (get-registry (registry-id uint))
    (map-get? registries registry-id)
)

(define-read-only (get-gift (gift-id uint))
    (map-get? gifts gift-id)
)

(define-read-only (get-user-registries (user principal))
    (default-to (list) (map-get? user-registries user))
)

(define-read-only (is-gift-claimed (gift-id uint))
    (match (map-get? gifts gift-id)
        gift (is-some (get claimed-by gift))
        false
    )
)

(define-read-only (get-registry-gift-count (registry-id uint))
    ;; This is a simplified version - in production, you'd maintain a counter
    (ok u0) ;; Placeholder for gift counting logic
)

;; ===================================
;; CONTRACT 2: contracts/gift-pool.clar
;; ===================================

;; Gift Pool Contract
;; Handles contribution pooling for expensive items

;; Constants
(define-constant ERR-POOL-NOT-FOUND (err u200))
(define-constant ERR-POOL-ALREADY-EXISTS (err u201))
(define-constant ERR-CONTRIBUTION-TOO-LOW (err u202))
(define-constant ERR-POOL-ALREADY-FUNDED (err u203))
(define-constant ERR-NOT-POOL-GIFT (err u204))
(define-constant ERR-INVALID-WITHDRAWAL (err u205))
(define-constant ERR-POOL-NOT-FUNDED (err u206))

;; Data Variables
(define-data-var min-contribution uint u1000000) ;; 1 STX minimum

;; Data Maps
(define-map gift-pools
    uint ;; gift-id
    {
        target-amount: uint,
        current-amount: uint,
        contributor-count: uint,
        registry-id: uint,
        registry-owner: principal,
        is-funded: bool,
        created-at: uint,
        funded-at: (optional uint)
    }
)

(define-map pool-contributions
    {gift-id: uint, contributor: principal}
    uint
)

(define-map contributor-gifts
    principal
    (list 100 uint)
)

;; Private Functions

(define-private (add-contributor-gift (contributor principal) (gift-id uint))
    (let ((current-gifts (default-to (list) (map-get? contributor-gifts contributor))))
        (match (as-max-len? (append current-gifts gift-id) u100)
            new-list (begin
                (map-set contributor-gifts contributor new-list)
                true
            )
            false
        )
    )
)

;; Public Functions

;; Create a contribution pool for an expensive gift
(define-public (create-pool
    (gift-id uint)
    (target-amount uint)
    (registry-id uint)
    (registry-owner principal)
)
    (let
        (
            (current-height burn-block-height)
        )
        (asserts! (> target-amount u0) ERR-INVALID-WITHDRAWAL)
        (asserts! (is-none (map-get? gift-pools gift-id)) ERR-POOL-ALREADY-EXISTS)

        (map-set gift-pools
            gift-id
            {
                target-amount: target-amount,
                current-amount: u0,
                contributor-count: u0,
                registry-id: registry-id,
                registry-owner: registry-owner,
                is-funded: false,
                created-at: current-height,
                funded-at: none
            }
        )
        (ok gift-id)
    )
)

;; Contribute to a gift pool
(define-public (contribute-to-pool (gift-id uint) (amount uint))
    (let
        (
            (pool (unwrap! (map-get? gift-pools gift-id) ERR-POOL-NOT-FOUND))
            (current-contribution (default-to u0 (map-get? pool-contributions {gift-id: gift-id, contributor: tx-sender})))
            (current-height burn-block-height)
        )
        (asserts! (not (get is-funded pool)) ERR-POOL-ALREADY-FUNDED)
        (asserts! (>= amount (var-get min-contribution)) ERR-CONTRIBUTION-TOO-LOW)

        ;; Transfer STX to this contract
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))

        ;; Update contribution record
        (map-set pool-contributions
            {gift-id: gift-id, contributor: tx-sender}
            (+ current-contribution amount)
        )

        ;; Add to contributor's gift list if first contribution
        (if (is-eq current-contribution u0)
            (add-contributor-gift tx-sender gift-id)
            true
        )

        ;; Update pool
        (let
            (
                (new-amount (+ (get current-amount pool) amount))
                (new-contributor-count
                    (if (is-eq current-contribution u0)
                        (+ (get contributor-count pool) u1)
                        (get contributor-count pool)
                    )
                )
                (is-now-funded (>= new-amount (get target-amount pool)))
            )
            (map-set gift-pools
                gift-id
                (merge pool {
                    current-amount: new-amount,
                    contributor-count: new-contributor-count,
                    is-funded: is-now-funded,
                    funded-at: (if is-now-funded (some current-height) (get funded-at pool))
                })
            )

            (ok new-amount)
        )
    )
)

;; Withdraw funds when pool is complete (registry owner only)
(define-public (withdraw-pool-funds (gift-id uint))
    (let
        (
            (pool (unwrap! (map-get? gift-pools gift-id) ERR-POOL-NOT-FOUND))
        )
        (asserts! (get is-funded pool) ERR-POOL-NOT-FUNDED)
        (asserts! (is-eq tx-sender (get registry-owner pool)) ERR-NOT-AUTHORIZED)

        ;; Transfer funds to registry owner
        (try! (as-contract (stx-transfer? (get current-amount pool) tx-sender (get registry-owner pool))))

        ;; Mark pool as withdrawn by zeroing amount
        (map-set gift-pools
            gift-id
            (merge pool {current-amount: u0})
        )

        (ok (get current-amount pool))
    )
)

;; Refund contributor if pool fails or gift changes
(define-public (request-refund (gift-id uint))
    (let
        (
            (pool (unwrap! (map-get? gift-pools gift-id) ERR-POOL-NOT-FOUND))
            (contribution (unwrap! (map-get? pool-contributions {gift-id: gift-id, contributor: tx-sender}) ERR-INVALID-WITHDRAWAL))
        )
        (asserts! (not (get is-funded pool)) ERR-POOL-ALREADY-FUNDED)
        (asserts! (> contribution u0) ERR-INVALID-WITHDRAWAL)

        ;; Remove contribution
        (map-delete pool-contributions {gift-id: gift-id, contributor: tx-sender})

        ;; Update pool
        (map-set gift-pools
            gift-id
            (merge pool {
                current-amount: (- (get current-amount pool) contribution),
                contributor-count: (- (get contributor-count pool) u1)
            })
        )

        ;; Refund STX
        (try! (as-contract (stx-transfer? contribution tx-sender tx-sender)))

        (ok contribution)
    )
)

;; Read-only functions

(define-read-only (get-pool (gift-id uint))
    (map-get? gift-pools gift-id)
)

(define-read-only (get-contribution (gift-id uint) (contributor principal))
    (default-to u0 (map-get? pool-contributions {gift-id: gift-id, contributor: contributor}))
)

(define-read-only (get-contributor-gifts (contributor principal))
    (default-to (list) (map-get? contributor-gifts contributor))
)

(define-read-only (get-pool-progress (gift-id uint))
    (match (map-get? gift-pools gift-id)
        pool
        (ok {
            percentage: (/ (* (get current-amount pool) u100) (get target-amount pool)),
            remaining: (- (get target-amount pool) (get current-amount pool)),
            is-funded: (get is-funded pool)
        })
        ERR-POOL-NOT-FOUND
    )
)

(define-read-only (get-min-contribution)
    (var-get min-contribution)
)

;; Admin function to update minimum contribution
(define-public (set-min-contribution (new-min uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (var-set min-contribution new-min)
        (ok new-min)
    )
)
