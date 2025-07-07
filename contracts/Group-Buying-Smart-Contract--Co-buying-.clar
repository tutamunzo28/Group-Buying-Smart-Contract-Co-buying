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
(define-map pricing-tiers
    {
        campaign-id: uint,
        tier-level: uint,
    }
    {
        min-participants: uint,
        discount-price: uint,
    }
)

(define-read-only (get-pricing-tier
        (campaign-id uint)
        (tier-level uint)
    )
    (map-get? pricing-tiers {
        campaign-id: campaign-id,
        tier-level: tier-level,
    })
)

(define-read-only (get-current-discount-price (campaign-id uint))
    (match (get-campaign campaign-id)
        campaign (let (
                (participant-count (get participant-count campaign))
                (base-discount (get discount-price campaign))
                (result (fold check-tier-discount (list u1 u2 u3 u4 u5) {
                    campaign-id: campaign-id,
                    participant-count: participant-count,
                    current-price: base-discount,
                }))
            )
            (get current-price result)
        )
        u0
    )
)

(define-private (check-tier-discount
        (tier-level uint)
        (context {
            campaign-id: uint,
            participant-count: uint,
            current-price: uint,
        })
    )
    (match (get-pricing-tier (get campaign-id context) tier-level)
        tier (if (>= (get participant-count context) (get min-participants tier))
            (merge context { current-price: (get discount-price tier) })
            context
        )
        context
    )
)

(define-public (add-pricing-tier
        (campaign-id uint)
        (tier-level uint)
        (min-participants uint)
        (discount-price uint)
    )
    (let ((campaign (unwrap! (get-campaign campaign-id) ERR_CAMPAIGN_NOT_FOUND)))
        (asserts! (is-eq tx-sender (get creator campaign)) ERR_NOT_AUTHORIZED)
        (asserts! (get is-active campaign) ERR_CAMPAIGN_NOT_ACTIVE)
        (asserts! (> min-participants u0) ERR_INVALID_PARAMETERS)
        (asserts! (> discount-price u0) ERR_INVALID_PARAMETERS)
        (asserts! (< discount-price (get price-per-unit campaign))
            ERR_INVALID_PARAMETERS
        )
        (asserts! (<= tier-level u5) ERR_INVALID_PARAMETERS)
        (map-set pricing-tiers {
            campaign-id: campaign-id,
            tier-level: tier-level,
        } {
            min-participants: min-participants,
            discount-price: discount-price,
        })
        (ok true)
    )
)

(define-read-only (get-all-pricing-tiers (campaign-id uint))
    (let (
            (tier1 (get-pricing-tier campaign-id u1))
            (tier2 (get-pricing-tier campaign-id u2))
            (tier3 (get-pricing-tier campaign-id u3))
            (tier4 (get-pricing-tier campaign-id u4))
            (tier5 (get-pricing-tier campaign-id u5))
        )
        (ok {
            tier-1: tier1,
            tier-2: tier2,
            tier-3: tier3,
            tier-4: tier4,
            tier-5: tier5,
        })
    )
)
(define-constant DEFAULT_REFERRAL_RATE u5)

(define-map referrals
    {
        campaign-id: uint,
        referred-user: principal,
    }
    {
        referrer: principal,
        reward-amount: uint,
        is-claimed: bool,
    }
)

(define-map campaign-referral-settings
    uint
    {
        referral-rate: uint,
        max-referral-reward: uint,
        is-enabled: bool,
    }
)

(define-map user-referral-stats
    principal
    {
        total-referrals: uint,
        total-rewards-earned: uint,
        total-rewards-claimed: uint,
    }
)

(define-read-only (get-referral-info
        (campaign-id uint)
        (referred-user principal)
    )
    (map-get? referrals {
        campaign-id: campaign-id,
        referred-user: referred-user,
    })
)

(define-read-only (get-campaign-referral-settings (campaign-id uint))
    (default-to {
        referral-rate: DEFAULT_REFERRAL_RATE,
        max-referral-reward: u1000000,
        is-enabled: false,
    }
        (map-get? campaign-referral-settings campaign-id)
    )
)

(define-read-only (get-user-referral-stats (user principal))
    (default-to {
        total-referrals: u0,
        total-rewards-earned: u0,
        total-rewards-claimed: u0,
    }
        (map-get? user-referral-stats user)
    )
)

(define-public (enable-referral-system
        (campaign-id uint)
        (referral-rate uint)
        (max-referral-reward uint)
    )
    (let ((campaign (unwrap! (get-campaign campaign-id) ERR_CAMPAIGN_NOT_FOUND)))
        (asserts! (is-eq tx-sender (get creator campaign)) ERR_NOT_AUTHORIZED)
        (asserts! (get is-active campaign) ERR_CAMPAIGN_NOT_ACTIVE)
        (asserts! (<= referral-rate u20) ERR_INVALID_PARAMETERS)
        (asserts! (> max-referral-reward u0) ERR_INVALID_PARAMETERS)
        (map-set campaign-referral-settings campaign-id {
            referral-rate: referral-rate,
            max-referral-reward: max-referral-reward,
            is-enabled: true,
        })
        (ok true)
    )
)

(define-public (participate-with-referral
        (campaign-id uint)
        (units uint)
        (referrer principal)
    )
    (let (
            (campaign (unwrap! (get-campaign campaign-id) ERR_CAMPAIGN_NOT_FOUND))
            (referral-settings (get-campaign-referral-settings campaign-id))
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
        (asserts! (not (is-eq tx-sender referrer)) ERR_INVALID_PARAMETERS)
        (asserts! (get is-enabled referral-settings) ERR_CAMPAIGN_NOT_ACTIVE)
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
        (let (
                (calculated-reward (/ (* contribution-amount (get referral-rate referral-settings))
                    u100
                ))
                (reward-amount (if (< calculated-reward
                        (get max-referral-reward referral-settings)
                    )
                    calculated-reward
                    (get max-referral-reward referral-settings)
                ))
            )
            (map-set referrals {
                campaign-id: campaign-id,
                referred-user: tx-sender,
            } {
                referrer: referrer,
                reward-amount: reward-amount,
                is-claimed: false,
            })
            (update-referrer-stats referrer reward-amount)
        )
        (add-to-user-campaigns tx-sender campaign-id)
        (ok true)
    )
)

(define-private (update-referrer-stats
        (referrer principal)
        (reward-amount uint)
    )
    (let ((current-stats (get-user-referral-stats referrer)))
        (map-set user-referral-stats referrer {
            total-referrals: (+ (get total-referrals current-stats) u1),
            total-rewards-earned: (+ (get total-rewards-earned current-stats) reward-amount),
            total-rewards-claimed: (get total-rewards-claimed current-stats),
        })
    )
)

(define-public (claim-referral-reward
        (campaign-id uint)
        (referred-user principal)
    )
    (let (
            (campaign (unwrap! (get-campaign campaign-id) ERR_CAMPAIGN_NOT_FOUND))
            (referral-info (unwrap! (get-referral-info campaign-id referred-user)
                ERR_NOT_AUTHORIZED
            ))
        )
        (asserts! (is-eq tx-sender (get referrer referral-info))
            ERR_NOT_AUTHORIZED
        )
        (asserts! (get is-finalized campaign) ERR_CAMPAIGN_NOT_ACTIVE)
        (asserts! (is-campaign-successful campaign-id) ERR_MINIMUM_NOT_MET)
        (asserts! (not (get is-claimed referral-info)) ERR_ALREADY_PARTICIPATED)
        (map-set referrals {
            campaign-id: campaign-id,
            referred-user: referred-user,
        }
            (merge referral-info { is-claimed: true })
        )
        (let ((current-stats (get-user-referral-stats tx-sender)))
            (map-set user-referral-stats tx-sender
                (merge current-stats { total-rewards-claimed: (+ (get total-rewards-claimed current-stats)
                    (get reward-amount referral-info)
                ) }
                ))
        )
        (try! (as-contract (stx-transfer? (get reward-amount referral-info) tx-sender tx-sender)))
        (ok true)
    )
)

(define-read-only (get-pending-referral-rewards (referrer principal))
    (let ((stats (get-user-referral-stats referrer)))
        (- (get total-rewards-earned stats) (get total-rewards-claimed stats))
    )
)
