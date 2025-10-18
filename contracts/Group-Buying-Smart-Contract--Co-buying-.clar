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
(define-constant ERR_INSUFFICIENT_INSURANCE (err u111))
(define-constant ERR_INSURANCE_ALREADY_CLAIMED (err u112))
(define-constant ERR_DELIVERY_NOT_FAILED (err u113))
(define-constant ERR_EARLY_BIRD_EXHAUSTED (err u114))

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
        insurance-amount: uint,
        delivery-deadline: uint,
        delivery-confirmed: bool,
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
            insurance-amount: u0,
            delivery-deadline: (+ end-block u8640),
            delivery-confirmed: false,
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
        (try! (check-and-trigger-milestones campaign-id))
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
        (try! (check-and-trigger-milestones campaign-id))
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

(define-map campaign-milestones
    {
        campaign-id: uint,
        milestone-id: uint,
    }
    {
        target-amount: uint,
        reward-percentage: uint,
        description: (string-ascii 100),
        is-achieved: bool,
        achievement-block: uint,
    }
)

(define-map milestone-participants
    {
        campaign-id: uint,
        milestone-id: uint,
        participant: principal,
    }
    {
        reward-amount: uint,
        is-claimed: bool,
    }
)

(define-read-only (get-milestone
        (campaign-id uint)
        (milestone-id uint)
    )
    (map-get? campaign-milestones {
        campaign-id: campaign-id,
        milestone-id: milestone-id,
    })
)

(define-read-only (get-milestone-participation
        (campaign-id uint)
        (milestone-id uint)
        (participant principal)
    )
    (map-get? milestone-participants {
        campaign-id: campaign-id,
        milestone-id: milestone-id,
        participant: participant,
    })
)

(define-read-only (get-achieved-milestones (campaign-id uint))
    (let (
            (milestone1 (get-milestone campaign-id u1))
            (milestone2 (get-milestone campaign-id u2))
            (milestone3 (get-milestone campaign-id u3))
            (milestone4 (get-milestone campaign-id u4))
            (milestone5 (get-milestone campaign-id u5))
        )
        (ok {
            milestone-1: (if (is-some milestone1)
                (get is-achieved (unwrap-panic milestone1))
                false
            ),
            milestone-2: (if (is-some milestone2)
                (get is-achieved (unwrap-panic milestone2))
                false
            ),
            milestone-3: (if (is-some milestone3)
                (get is-achieved (unwrap-panic milestone3))
                false
            ),
            milestone-4: (if (is-some milestone4)
                (get is-achieved (unwrap-panic milestone4))
                false
            ),
            milestone-5: (if (is-some milestone5)
                (get is-achieved (unwrap-panic milestone5))
                false
            ),
        })
    )
)

(define-read-only (calculate-milestone-reward
        (campaign-id uint)
        (milestone-id uint)
        (participant principal)
    )
    (match (get-milestone campaign-id milestone-id)
        milestone (match (get-participation campaign-id participant)
            participation (let (
                    (participant-contribution (get amount participation))
                    (reward-rate (get reward-percentage milestone))
                )
                (/ (* participant-contribution reward-rate) u100)
            )
            u0
        )
        u0
    )
)

(define-public (create-milestone
        (campaign-id uint)
        (milestone-id uint)
        (target-amount uint)
        (reward-percentage uint)
        (description (string-ascii 100))
    )
    (let ((campaign (unwrap! (get-campaign campaign-id) ERR_CAMPAIGN_NOT_FOUND)))
        (asserts! (is-eq tx-sender (get creator campaign)) ERR_NOT_AUTHORIZED)
        (asserts! (get is-active campaign) ERR_CAMPAIGN_NOT_ACTIVE)
        (asserts! (not (get is-finalized campaign)) ERR_CAMPAIGN_FINALIZED)
        (asserts! (> target-amount u0) ERR_INVALID_PARAMETERS)
        (asserts! (<= target-amount (get target-amount campaign))
            ERR_INVALID_PARAMETERS
        )
        (asserts! (<= reward-percentage u10) ERR_INVALID_PARAMETERS)
        (asserts! (<= milestone-id u5) ERR_INVALID_PARAMETERS)
        (map-set campaign-milestones {
            campaign-id: campaign-id,
            milestone-id: milestone-id,
        } {
            target-amount: target-amount,
            reward-percentage: reward-percentage,
            description: description,
            is-achieved: false,
            achievement-block: u0,
        })
        (ok true)
    )
)

(define-private (check-and-trigger-milestones (campaign-id uint))
    (let ((campaign (unwrap! (get-campaign campaign-id) ERR_CAMPAIGN_NOT_FOUND)))
        (fold check-milestone-achievement (list u1 u2 u3 u4 u5) {
            campaign-id: campaign-id,
            current-raised: (get total-raised campaign),
        })
        (ok true)
    )
)

(define-private (check-milestone-achievement
        (milestone-id uint)
        (context {
            campaign-id: uint,
            current-raised: uint,
        })
    )
    (match (get-milestone (get campaign-id context) milestone-id)
        milestone (if (and
                (not (get is-achieved milestone))
                (>= (get current-raised context) (get target-amount milestone))
            )
            (begin
                (map-set campaign-milestones {
                    campaign-id: (get campaign-id context),
                    milestone-id: milestone-id,
                }
                    (merge milestone {
                        is-achieved: true,
                        achievement-block: stacks-block-height,
                    })
                )
                context
            )
            context
        )
        context
    )
)

(define-public (claim-milestone-reward
        (campaign-id uint)
        (milestone-id uint)
    )
    (let (
            (campaign (unwrap! (get-campaign campaign-id) ERR_CAMPAIGN_NOT_FOUND))
            (milestone (unwrap! (get-milestone campaign-id milestone-id)
                ERR_CAMPAIGN_NOT_FOUND
            ))
            (existing-claim (get-milestone-participation campaign-id milestone-id tx-sender))
            (participation (unwrap! (get-participation campaign-id tx-sender) ERR_NOT_AUTHORIZED))
        )
        (asserts! (get is-finalized campaign) ERR_CAMPAIGN_NOT_ACTIVE)
        (asserts! (get is-achieved milestone) ERR_CAMPAIGN_NOT_ACTIVE)
        (asserts! (is-none existing-claim) ERR_ALREADY_PARTICIPATED)
        (let ((reward-amount (calculate-milestone-reward campaign-id milestone-id tx-sender)))
            (asserts! (> reward-amount u0) ERR_INVALID_AMOUNT)
            (map-set milestone-participants {
                campaign-id: campaign-id,
                milestone-id: milestone-id,
                participant: tx-sender,
            } {
                reward-amount: reward-amount,
                is-claimed: true,
            })
            (try! (as-contract (stx-transfer? reward-amount tx-sender tx-sender)))
            (ok reward-amount)
        )
    )
)

(define-map insurance-claims
    {
        campaign-id: uint,
        participant: principal,
    }
    {
        claim-amount: uint,
        is-claimed: bool,
    }
)

(define-read-only (get-insurance-claim
        (campaign-id uint)
        (participant principal)
    )
    (map-get? insurance-claims {
        campaign-id: campaign-id,
        participant: participant,
    })
)

(define-read-only (calculate-insurance-payout
        (campaign-id uint)
        (participant principal)
    )
    (match (get-campaign campaign-id)
        campaign (match (get-participation campaign-id participant)
            participation (let (
                    (total-insurance (get insurance-amount campaign))
                    (total-raised (get total-raised campaign))
                    (participant-contribution (get amount participation))
                )
                (if (and (> total-insurance u0) (> total-raised u0))
                    (/ (* total-insurance participant-contribution) total-raised)
                    u0
                )
            )
            u0
        )
        u0
    )
)

(define-read-only (is-delivery-failed (campaign-id uint))
    (match (get-campaign campaign-id)
        campaign (and
            (get is-finalized campaign)
            (not (get delivery-confirmed campaign))
            (> stacks-block-height (get delivery-deadline campaign))
        )
        false
    )
)

(define-public (deposit-insurance (campaign-id uint))
    (let (
            (campaign (unwrap! (get-campaign campaign-id) ERR_CAMPAIGN_NOT_FOUND))
            (insurance-amount (/ (get target-amount campaign) u10))
        )
        (asserts! (is-eq tx-sender (get creator campaign)) ERR_NOT_AUTHORIZED)
        (asserts! (get is-active campaign) ERR_CAMPAIGN_NOT_ACTIVE)
        (asserts! (not (get is-finalized campaign)) ERR_CAMPAIGN_FINALIZED)
        (asserts! (is-eq (get insurance-amount campaign) u0)
            ERR_ALREADY_PARTICIPATED
        )
        (try! (stx-transfer? insurance-amount tx-sender (as-contract tx-sender)))
        (map-set campaigns campaign-id
            (merge campaign { insurance-amount: insurance-amount })
        )
        (ok insurance-amount)
    )
)

(define-public (confirm-delivery (campaign-id uint))
    (let ((campaign (unwrap! (get-campaign campaign-id) ERR_CAMPAIGN_NOT_FOUND)))
        (asserts! (is-eq tx-sender (get creator campaign)) ERR_NOT_AUTHORIZED)
        (asserts! (get is-finalized campaign) ERR_CAMPAIGN_NOT_ACTIVE)
        (asserts! (is-campaign-successful campaign-id) ERR_MINIMUM_NOT_MET)
        (asserts! (not (get delivery-confirmed campaign))
            ERR_ALREADY_PARTICIPATED
        )
        (asserts! (<= stacks-block-height (get delivery-deadline campaign))
            ERR_CAMPAIGN_EXPIRED
        )
        (map-set campaigns campaign-id
            (merge campaign { delivery-confirmed: true })
        )
        (if (> (get insurance-amount campaign) u0)
            (begin
                (try! (as-contract (stx-transfer? (get insurance-amount campaign) tx-sender
                    (get creator campaign)
                )))
                (ok true)
            )
            (ok true)
        )
    )
)

(define-public (claim-insurance-payout (campaign-id uint))
    (let (
            (campaign (unwrap! (get-campaign campaign-id) ERR_CAMPAIGN_NOT_FOUND))
            (existing-claim (get-insurance-claim campaign-id tx-sender))
            (participation (unwrap! (get-participation campaign-id tx-sender) ERR_NOT_AUTHORIZED))
            (payout-amount (calculate-insurance-payout campaign-id tx-sender))
        )
        (asserts! (get is-finalized campaign) ERR_CAMPAIGN_NOT_ACTIVE)
        (asserts! (is-delivery-failed campaign-id) ERR_DELIVERY_NOT_FAILED)
        (asserts! (is-none existing-claim) ERR_INSURANCE_ALREADY_CLAIMED)
        (asserts! (> payout-amount u0) ERR_INSUFFICIENT_INSURANCE)
        (map-set insurance-claims {
            campaign-id: campaign-id,
            participant: tx-sender,
        } {
            claim-amount: payout-amount,
            is-claimed: true,
        })
        (try! (as-contract (stx-transfer? payout-amount tx-sender tx-sender)))
        (ok payout-amount)
    )
)

(define-public (extend-delivery-deadline
        (campaign-id uint)
        (additional-blocks uint)
    )
    (let ((campaign (unwrap! (get-campaign campaign-id) ERR_CAMPAIGN_NOT_FOUND)))
        (asserts! (is-eq tx-sender (get creator campaign)) ERR_NOT_AUTHORIZED)
        (asserts! (get is-finalized campaign) ERR_CAMPAIGN_NOT_ACTIVE)
        (asserts! (not (get delivery-confirmed campaign))
            ERR_ALREADY_PARTICIPATED
        )
        (asserts! (> additional-blocks u0) ERR_INVALID_PARAMETERS)
        (asserts! (<= additional-blocks u17520) ERR_INVALID_PARAMETERS)
        (map-set campaigns campaign-id
            (merge campaign { delivery-deadline: (+ (get delivery-deadline campaign) additional-blocks) })
        )
        (ok true)
    )
)

(define-read-only (get-campaign-insurance-info (campaign-id uint))
    (match (get-campaign campaign-id)
        campaign (ok {
            insurance-amount: (get insurance-amount campaign),
            delivery-deadline: (get delivery-deadline campaign),
            delivery-confirmed: (get delivery-confirmed campaign),
            delivery-failed: (is-delivery-failed campaign-id),
            blocks-until-deadline: (if (> (get delivery-deadline campaign) stacks-block-height)
                (- (get delivery-deadline campaign) stacks-block-height)
                u0
            ),
        })
        ERR_CAMPAIGN_NOT_FOUND
    )
)

(define-map early-bird-settings
    uint
    {
        max-slots: uint,
        slots-claimed: uint,
        bonus-percentage: uint,
        is-enabled: bool,
    }
)

(define-map early-bird-participants
    {
        campaign-id: uint,
        participant: principal,
    }
    {
        bonus-amount: uint,
        is-claimed: bool,
    }
)

(define-read-only (get-early-bird-settings (campaign-id uint))
    (map-get? early-bird-settings campaign-id)
)

(define-read-only (get-early-bird-participation
        (campaign-id uint)
        (participant principal)
    )
    (map-get? early-bird-participants {
        campaign-id: campaign-id,
        participant: participant,
    })
)

(define-read-only (is-early-bird-available (campaign-id uint))
    (match (get-early-bird-settings campaign-id)
        settings (and
            (get is-enabled settings)
            (< (get slots-claimed settings) (get max-slots settings))
        )
        false
    )
)

(define-read-only (calculate-early-bird-bonus
        (campaign-id uint)
        (participant principal)
    )
    (match (get-early-bird-settings campaign-id)
        settings (match (get-participation campaign-id participant)
            participation (let (
                    (contribution (get amount participation))
                    (bonus-rate (get bonus-percentage settings))
                )
                (/ (* contribution bonus-rate) u100)
            )
            u0
        )
        u0
    )
)

(define-public (enable-early-bird
        (campaign-id uint)
        (max-slots uint)
        (bonus-percentage uint)
    )
    (let ((campaign (unwrap! (get-campaign campaign-id) ERR_CAMPAIGN_NOT_FOUND)))
        (asserts! (is-eq tx-sender (get creator campaign)) ERR_NOT_AUTHORIZED)
        (asserts! (get is-active campaign) ERR_CAMPAIGN_NOT_ACTIVE)
        (asserts! (not (get is-finalized campaign)) ERR_CAMPAIGN_FINALIZED)
        (asserts! (> max-slots u0) ERR_INVALID_PARAMETERS)
        (asserts! (<= max-slots u50) ERR_INVALID_PARAMETERS)
        (asserts! (> bonus-percentage u0) ERR_INVALID_PARAMETERS)
        (asserts! (<= bonus-percentage u20) ERR_INVALID_PARAMETERS)
        (map-set early-bird-settings campaign-id {
            max-slots: max-slots,
            slots-claimed: u0,
            bonus-percentage: bonus-percentage,
            is-enabled: true,
        })
        (ok true)
    )
)

(define-private (register-early-bird-participant (campaign-id uint))
    (match (get-early-bird-settings campaign-id)
        settings (if (and
                (get is-enabled settings)
                (< (get slots-claimed settings) (get max-slots settings))
            )
            (begin
                (map-set early-bird-settings campaign-id
                    (merge settings { slots-claimed: (+ (get slots-claimed settings) u1) })
                )
                (ok true)
            )
            (ok false)
        )
        (ok false)
    )
)

(define-public (claim-early-bird-bonus (campaign-id uint))
    (let (
            (campaign (unwrap! (get-campaign campaign-id) ERR_CAMPAIGN_NOT_FOUND))
            (settings (unwrap! (get-early-bird-settings campaign-id) ERR_CAMPAIGN_NOT_FOUND))
            (existing-claim (get-early-bird-participation campaign-id tx-sender))
            (participation (unwrap! (get-participation campaign-id tx-sender) ERR_NOT_AUTHORIZED))
        )
        (asserts! (get is-finalized campaign) ERR_CAMPAIGN_NOT_ACTIVE)
        (asserts! (is-campaign-successful campaign-id) ERR_MINIMUM_NOT_MET)
        (asserts! (is-none existing-claim) ERR_ALREADY_PARTICIPATED)
        (let ((bonus-amount (calculate-early-bird-bonus campaign-id tx-sender)))
            (asserts! (> bonus-amount u0) ERR_INVALID_AMOUNT)
            (map-set early-bird-participants {
                campaign-id: campaign-id,
                participant: tx-sender,
            } {
                bonus-amount: bonus-amount,
                is-claimed: true,
            })
            (try! (as-contract (stx-transfer? bonus-amount tx-sender tx-sender)))
            (ok bonus-amount)
        )
    )
)

(define-read-only (get-early-bird-stats (campaign-id uint))
    (match (get-early-bird-settings campaign-id)
        settings (ok {
            max-slots: (get max-slots settings),
            slots-claimed: (get slots-claimed settings),
            slots-remaining: (- (get max-slots settings) (get slots-claimed settings)),
            bonus-percentage: (get bonus-percentage settings),
            is-enabled: (get is-enabled settings),
            is-available: (is-early-bird-available campaign-id),
        })
        ERR_CAMPAIGN_NOT_FOUND
    )
)

(define-public (participate-as-early-bird
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
        (asserts! (is-early-bird-available campaign-id) ERR_EARLY_BIRD_EXHAUSTED)
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
        (try! (check-and-trigger-milestones campaign-id))
        (unwrap-panic (register-early-bird-participant campaign-id))
        (add-to-user-campaigns tx-sender campaign-id)
        (ok true)
    )
)
