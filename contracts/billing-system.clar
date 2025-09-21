;; Billing System Smart Contract
;; Handles automated billing and payments with transparent pricing and account management
;; Enables dynamic pricing and automated payment processing

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u600))
(define-constant ERR-ACCOUNT-NOT-FOUND (err u601))
(define-constant ERR-INSUFFICIENT-FUNDS (err u602))
(define-constant ERR-INVALID-AMOUNT (err u603))
(define-constant ERR-BILL-NOT-FOUND (err u604))
(define-constant ERR-BILL-ALREADY-PAID (err u605))
(define-constant ERR-INVALID-RATE (err u606))

;; Billing Periods
(define-constant PERIOD-MONTHLY u1)
(define-constant PERIOD-QUARTERLY u2)
(define-constant PERIOD-ANNUALLY u3)

;; Payment Status
(define-constant PAYMENT-PENDING u1)
(define-constant PAYMENT-PAID u2)
(define-constant PAYMENT-OVERDUE u3)
(define-constant PAYMENT-CANCELLED u4)

;; Data Variables
(define-data-var next-account-id uint u1)
(define-data-var next-bill-id uint u1)
(define-data-var total-accounts uint u0)
(define-data-var total-revenue uint u0)

;; Data Maps
(define-map customer-accounts
    uint ;; account-id
    {
        customer-address: principal,
        account-balance: uint,
        deposit-amount: uint,
        billing-period: uint,
        auto-pay-enabled: bool,
        last-payment-date: uint,
        total-paid: uint,
        account-status: uint,
        credit-limit: uint
    }
)

(define-map utility-bills
    uint ;; bill-id
    {
        account-id: uint,
        billing-period: uint,
        consumption-amount: uint,
        base-rate: uint,
        total-cost: uint,
        due-date: uint,
        payment-status: uint,
        payment-date: uint,
        late-fees: uint
    }
)

(define-map pricing-tiers
    {utility-type: uint, tier: uint}
    {
        min-usage: uint,
        max-usage: uint,
        rate-per-unit: uint,
        description: (string-ascii 100)
    }
)

(define-map payment-history
    uint ;; account-id
    (list 100 {
        payment-date: uint,
        amount: uint,
        bill-id: uint,
        payment-method: (string-ascii 50)
    })
)

(define-map account-devices
    uint ;; account-id
    (list 10 uint) ;; device IDs
)

(define-map billing-cycles
    uint ;; cycle-id
    {
        start-date: uint,
        end-date: uint,
        total-bills: uint,
        total-revenue: uint,
        processed: bool
    }
)

;; Initialize pricing tiers
(map-set pricing-tiers {utility-type: u1, tier: u1} ;; Water - Basic
    {min-usage: u0, max-usage: u1000, rate-per-unit: u50, description: "Basic water usage"})
(map-set pricing-tiers {utility-type: u1, tier: u2} ;; Water - High
    {min-usage: u1001, max-usage: u999999, rate-per-unit: u75, description: "High water usage"})
(map-set pricing-tiers {utility-type: u2, tier: u1} ;; Electricity - Basic
    {min-usage: u0, max-usage: u500, rate-per-unit: u100, description: "Basic electricity usage"})
(map-set pricing-tiers {utility-type: u2, tier: u2} ;; Electricity - High
    {min-usage: u501, max-usage: u999999, rate-per-unit: u150, description: "High electricity usage"})

(define-data-var next-cycle-id uint u1)

;; Public Functions

;; Create customer account
(define-public (create-account
    (customer-address principal)
    (deposit-amount uint)
    (billing-period uint)
    (credit-limit uint)
)
    (let (
        (account-id (var-get next-account-id))
    )
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (asserts! (> deposit-amount u0) ERR-INVALID-AMOUNT)
        (asserts! (<= billing-period u3) ERR-INVALID-RATE)
        
        ;; Create account record
        (map-set customer-accounts account-id
            {
                customer-address: customer-address,
                account-balance: deposit-amount,
                deposit-amount: deposit-amount,
                billing-period: billing-period,
                auto-pay-enabled: false,
                last-payment-date: u0,
                total-paid: u0,
                account-status: u1,
                credit-limit: credit-limit
            }
        )
        
        ;; Initialize empty device list
        (map-set account-devices account-id (list))
        
        ;; Update counters
        (var-set next-account-id (+ account-id u1))
        (var-set total-accounts (+ (var-get total-accounts) u1))
        
        (ok account-id)
    )
)

;; Generate bill for account
(define-public (generate-bill
    (account-id uint)
    (consumption-amount uint)
    (utility-type uint)
    (billing-period uint)
)
    (let (
        (account (unwrap! (map-get? customer-accounts account-id) ERR-ACCOUNT-NOT-FOUND))
        (bill-id (var-get next-bill-id))
        (calculated-cost (calculate-bill-cost consumption-amount utility-type))
        (due-date (+ burn-block-height u1000)) ;; ~7 days
    )
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (asserts! (> consumption-amount u0) ERR-INVALID-AMOUNT)
        
        ;; Create bill record
        (map-set utility-bills bill-id
            {
                account-id: account-id,
                billing-period: billing-period,
                consumption-amount: consumption-amount,
                base-rate: (get-base-rate utility-type),
                total-cost: calculated-cost,
                due-date: due-date,
                payment-status: PAYMENT-PENDING,
                payment-date: u0,
                late-fees: u0
            }
        )
        
        ;; Update bill counter
        (var-set next-bill-id (+ bill-id u1))
        
        (ok bill-id)
    )
)

;; Pay bill
(define-public (pay-bill (bill-id uint) (payment-amount uint))
    (let (
        (bill (unwrap! (map-get? utility-bills bill-id) ERR-BILL-NOT-FOUND))
        (account (unwrap! (map-get? customer-accounts (get account-id bill)) ERR-ACCOUNT-NOT-FOUND))
        (total-due (+ (get total-cost bill) (get late-fees bill)))
        (payment-history-list (default-to (list) (map-get? payment-history (get account-id bill))))
    )
        (asserts! (is-eq tx-sender (get customer-address account)) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get payment-status bill) PAYMENT-PENDING) ERR-BILL-ALREADY-PAID)
        (asserts! (>= payment-amount total-due) ERR-INSUFFICIENT-FUNDS)
        
        ;; Transfer payment to contract
        (try! (stx-transfer? payment-amount tx-sender (as-contract tx-sender)))
        
        ;; Update bill status
        (map-set utility-bills bill-id
            (merge bill {
                payment-status: PAYMENT-PAID,
                payment-date: burn-block-height
            })
        )
        
        ;; Update account
        (map-set customer-accounts (get account-id bill)
            (merge account {
                last-payment-date: burn-block-height,
                total-paid: (+ (get total-paid account) payment-amount)
            })
        )
        
        ;; Add to payment history
        (let (
            (payment-record {
                payment-date: burn-block-height,
                amount: payment-amount,
                bill-id: bill-id,
                payment-method: "STX"
            })
        )
            (map-set payment-history (get account-id bill)
                (unwrap! (as-max-len? (append payment-history-list payment-record) u100) ERR-INVALID-AMOUNT)
            )
        )
        
        ;; Update total revenue
        (var-set total-revenue (+ (var-get total-revenue) payment-amount))
        
        (ok true)
    )
)

;; Add deposit to account
(define-public (add-deposit (account-id uint) (deposit-amount uint))
    (let (
        (account (unwrap! (map-get? customer-accounts account-id) ERR-ACCOUNT-NOT-FOUND))
    )
        (asserts! (is-eq tx-sender (get customer-address account)) ERR-NOT-AUTHORIZED)
        (asserts! (> deposit-amount u0) ERR-INVALID-AMOUNT)
        
        ;; Transfer deposit to contract
        (try! (stx-transfer? deposit-amount tx-sender (as-contract tx-sender)))
        
        ;; Update account balance
        (map-set customer-accounts account-id
            (merge account {
                account-balance: (+ (get account-balance account) deposit-amount)
            })
        )
        
        (ok true)
    )
)

;; Enable auto-pay
(define-public (enable-auto-pay (account-id uint))
    (let (
        (account (unwrap! (map-get? customer-accounts account-id) ERR-ACCOUNT-NOT-FOUND))
    )
        (asserts! (is-eq tx-sender (get customer-address account)) ERR-NOT-AUTHORIZED)
        
        (map-set customer-accounts account-id
            (merge account {auto-pay-enabled: true})
        )
        
        (ok true)
    )
)

;; Update pricing tier
(define-public (update-pricing-tier
    (utility-type uint)
    (tier uint)
    (min-usage uint)
    (max-usage uint)
    (rate-per-unit uint)
    (description (string-ascii 100))
)
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (asserts! (> rate-per-unit u0) ERR-INVALID-RATE)
        (asserts! (< min-usage max-usage) ERR-INVALID-AMOUNT)
        
        (map-set pricing-tiers {utility-type: utility-type, tier: tier}
            {
                min-usage: min-usage,
                max-usage: max-usage,
                rate-per-unit: rate-per-unit,
                description: description
            }
        )
        
        (ok true)
    )
)

;; Read-Only Functions

;; Get account details
(define-read-only (get-account (account-id uint))
    (map-get? customer-accounts account-id)
)

;; Get bill details
(define-read-only (get-bill (bill-id uint))
    (map-get? utility-bills bill-id)
)

;; Get payment history
(define-read-only (get-payment-history (account-id uint))
    (map-get? payment-history account-id)
)

;; Get pricing tier
(define-read-only (get-pricing-tier (utility-type uint) (tier uint))
    (map-get? pricing-tiers {utility-type: utility-type, tier: tier})
)

;; Get account devices
(define-read-only (get-account-devices (account-id uint))
    (map-get? account-devices account-id)
)

;; Calculate bill cost
(define-read-only (calculate-bill-cost (consumption uint) (utility-type uint))
    (let (
        (tier1 (unwrap-panic (map-get? pricing-tiers {utility-type: utility-type, tier: u1})))
        (tier2 (unwrap-panic (map-get? pricing-tiers {utility-type: utility-type, tier: u2})))
    )
        (if (<= consumption (get max-usage tier1))
            (* consumption (get rate-per-unit tier1))
            (+ (* (get max-usage tier1) (get rate-per-unit tier1))
               (* (- consumption (get max-usage tier1)) (get rate-per-unit tier2)))
        )
    )
)

;; Get base rate for utility type
(define-read-only (get-base-rate (utility-type uint))
    (let (
        (tier1 (unwrap-panic (map-get? pricing-tiers {utility-type: utility-type, tier: u1})))
    )
        (get rate-per-unit tier1)
    )
)

;; Get system statistics
(define-read-only (get-billing-stats)
    {
        total-accounts: (var-get total-accounts),
        total-revenue: (var-get total-revenue),
        next-account-id: (var-get next-account-id),
        next-bill-id: (var-get next-bill-id)
    }
)

