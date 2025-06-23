(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_CAMPAIGN_NOT_FOUND (err u101))
(define-constant ERR_CAMPAIGN_EXPIRED (err u102))
(define-constant ERR_CAMPAIGN_NOT_ACTIVE (err u103))
(define-constant ERR_INSUFFICIENT_FUNDS (err u104))
(define-constant ERR_MINIMUM_NOT_MET (err u105))
(define-constant ERR_ALREADY_PARTICIPATED (err u106))
(define-constant ERR_INVALID_AMOUNT (err u107))
(define-constant ERR_CAMPAIGN_FINALIZED (err u108))
(define-constant ERR_REFUND_FAILED (err u109))
(define-constant ERR_INVALID_PARAMETERS (err u110))

(define-data-var campaign-counter uint u0)

(define-map campaigns
    uint
    {
        creator: principal,
        title: (string-ascii 100),
        target-amount: uint,
        min-participants: uint,
        price-per-unit: uint,
        discount-price: uint,
        end-block: uint,
        total-raised: uint,
        participant-count: uint,
        is-active: bool,
        is-finalized: bool,
    }
)

(define-map participants
    {
        campaign-id: uint,
        participant: principal,
    }
    {
        amount: uint,
        units: uint,
    }
)

(define-map user-campaigns
    principal
    (list 50 uint)
)

(define-read-only (get-campaign (campaign-id uint))
    (map-get? campaigns campaign-id)
)

(define-read-only (get-participation
        (campaign-id uint)
        (participant principal)
    )
    (map-get? participants {
        campaign-id: campaign-id,
        participant: participant,
    })
)

(define-read-only (get-user-campaigns (user principal))
    (default-to (list) (map-get? user-campaigns user))
)

(define-read-only (get-campaign-counter)
    (var-get campaign-counter)
)

(define-read-only (is-campaign-successful (campaign-id uint))
    (match (get-campaign campaign-id)
        campaign (>= (get total-raised campaign) (get target-amount campaign))
        false
    )
)

(define-read-only (calculate-discount-savings
        (campaign-id uint)
        (units uint)
    )
    (match (get-campaign campaign-id)
        campaign (let (
                (regular-cost (* units (get price-per-unit campaign)))
                (discount-cost (* units (get discount-price campaign)))
            )
            (if (> regular-cost discount-cost)
                (- regular-cost discount-cost)
                u0
            )
        )
        u0
    )
)

(define-private (add-to-user-campaigns
        (user principal)
        (campaign-id uint)
    )
    (let ((current-campaigns (get-user-campaigns user)))
        (map-set user-campaigns user
            (unwrap-panic (as-max-len? (append current-campaigns campaign-id) u50))
        )
    )
)

(define-private (validate-campaign-params
        (target-amount uint)
        (min-participants uint)
        (price-per-unit uint)
        (discount-price uint)
        (duration-blocks uint)
    )
    (and
        (> target-amount u0)
        (> min-participants u0)
        (> price-per-unit u0)
        (> discount-price u0)
        (< discount-price price-per-unit)
        (> duration-blocks u0)
        (<= duration-blocks u52560)
    )
)

(define-public (create-campaign
        (title (string-ascii 100))
        (target-amount uint)
        (min-participants uint)
        (price-per-unit uint)
        (discount-price uint)
        (duration-blocks uint)
    )
    (let (
            (campaign-id (+ (var-get campaign-counter) u1))
            (end-block (+ stacks-block-height duration-blocks))
        )
        (asserts!
            (validate-campaign-params target-amount min-participants
                price-per-unit discount-price duration-blocks
            )
            ERR_INVALID_PARAMETERS
        )
        (map-set campaigns campaign-id {
            creator: tx-sender,
            title: title,
            target-amount: target-amount,
            min-participants: min-participants,
            price-per-unit: price-per-unit,
            discount-price: discount-price,
            end-block: end-block,
            total-raised: u0,
            participant-count: u0,
            is-active: true,
            is-finalized: false,
        })
        (var-set campaign-counter campaign-id)
        (add-to-user-campaigns tx-sender campaign-id)
        (ok campaign-id)
    )
)

(define-public (participate
        (campaign-id uint)
        (units uint)
    )
    (let (
            (campaign (unwrap! (get-campaign campaign-id) ERR_CAMPAIGN_NOT_FOUND))
            (contribution-amount (* units (get discount-price campaign)))
            (existing-participation (get-participation campaign-id tx-sender))
        )
        (asserts! (> units u0) ERR_INVALID_AMOUNT)
        (asserts! (get is-active campaign) ERR_CAMPAIGN_NOT_ACTIVE)
        (asserts! (<= stacks-block-height (get end-block campaign))
            ERR_CAMPAIGN_EXPIRED
        )
        (asserts! (not (get is-finalized campaign)) ERR_CAMPAIGN_FINALIZED)
        (asserts! (is-none existing-participation) ERR_ALREADY_PARTICIPATED)
        (try! (stx-transfer? contribution-amount tx-sender (as-contract tx-sender)))
        (map-set participants {
            campaign-id: campaign-id,
            participant: tx-sender,
        } {
            amount: contribution-amount,
            units: units,
        })
        (map-set campaigns campaign-id
            (merge campaign {
                total-raised: (+ (get total-raised campaign) contribution-amount),
                participant-count: (+ (get participant-count campaign) u1),
            })
        )
        (add-to-user-campaigns tx-sender campaign-id)
        (ok true)
    )
)

(define-public (finalize-campaign (campaign-id uint))
    (let ((campaign (unwrap! (get-campaign campaign-id) ERR_CAMPAIGN_NOT_FOUND)))
        (asserts! (get is-active campaign) ERR_CAMPAIGN_NOT_ACTIVE)
        (asserts! (not (get is-finalized campaign)) ERR_CAMPAIGN_FINALIZED)
        (asserts!
            (or
                (> stacks-block-height (get end-block campaign))
                (>= (get total-raised campaign) (get target-amount campaign))
            )
            ERR_CAMPAIGN_NOT_ACTIVE
        )
        (map-set campaigns campaign-id
            (merge campaign {
                is-active: false,
                is-finalized: true,
            })
        )
        (if (and
                (>= (get total-raised campaign) (get target-amount campaign))
                (>= (get participant-count campaign)
                    (get min-participants campaign)
                )
            )
            (begin
                (try! (as-contract (stx-transfer? (get total-raised campaign) tx-sender
                    (get creator campaign)
                )))
                (ok "success")
            )
            (ok "refund-required")
        )
    )
)

(define-public (claim-refund (campaign-id uint))
    (let (
            (campaign (unwrap! (get-campaign campaign-id) ERR_CAMPAIGN_NOT_FOUND))
            (participation (unwrap! (get-participation campaign-id tx-sender) ERR_NOT_AUTHORIZED))
        )
        (asserts! (get is-finalized campaign) ERR_CAMPAIGN_NOT_ACTIVE)
        (asserts!
            (or
                (< (get total-raised campaign) (get target-amount campaign))
                (< (get participant-count campaign)
                    (get min-participants campaign)
                )
            )
            ERR_MINIMUM_NOT_MET
        )
        (map-delete participants {
            campaign-id: campaign-id,
            participant: tx-sender,
        })
        (try! (as-contract (stx-transfer? (get amount participation) tx-sender tx-sender)))
        (ok true)
    )
)

(define-public (emergency-refund (campaign-id uint))
    (let ((campaign (unwrap! (get-campaign campaign-id) ERR_CAMPAIGN_NOT_FOUND)))
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
        (asserts! (get is-active campaign) ERR_CAMPAIGN_NOT_ACTIVE)
        (map-set campaigns campaign-id
            (merge campaign {
                is-active: false,
                is-finalized: true,
            })
        )
        (ok true)
    )
)

(define-public (extend-campaign
        (campaign-id uint)
        (additional-blocks uint)
    )
    (let ((campaign (unwrap! (get-campaign campaign-id) ERR_CAMPAIGN_NOT_FOUND)))
        (asserts! (is-eq tx-sender (get creator campaign)) ERR_NOT_AUTHORIZED)
        (asserts! (get is-active campaign) ERR_CAMPAIGN_NOT_ACTIVE)
        (asserts! (<= stacks-block-height (get end-block campaign))
            ERR_CAMPAIGN_EXPIRED
        )
        (asserts! (> additional-blocks u0) ERR_INVALID_PARAMETERS)
        (asserts! (<= additional-blocks u17520) ERR_INVALID_PARAMETERS)
        (map-set campaigns campaign-id
            (merge campaign { end-block: (+ (get end-block campaign) additional-blocks) })
        )
        (ok true)
    )
)

(define-read-only (get-campaign-stats (campaign-id uint))
    (match (get-campaign campaign-id)
        campaign (ok {
            progress-percentage: (if (> (get target-amount campaign) u0)
                (/ (* (get total-raised campaign) u100)
                    (get target-amount campaign)
                )
                u0
            ),
            remaining-amount: (if (> (get target-amount campaign) (get total-raised campaign))
                (- (get target-amount campaign) (get total-raised campaign))
                u0
            ),
            blocks-remaining: (if (> (get end-block campaign) stacks-block-height)
                (- (get end-block campaign) stacks-block-height)
                u0
            ),
            is-successful: (and
                (>= (get total-raised campaign) (get target-amount campaign))
                (>= (get participant-count campaign)
                    (get min-participants campaign)
                )
            ),
            total-savings: (calculate-discount-savings campaign-id u1),
        })
        ERR_CAMPAIGN_NOT_FOUND
    )
)
