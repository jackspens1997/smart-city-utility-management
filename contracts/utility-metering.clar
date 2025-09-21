;; Utility Metering Smart Contract
;; Logs usage data securely from IoT devices with verification and fraud detection
;; Enables real-time monitoring and transparent consumption tracking

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u500))
(define-constant ERR-DEVICE-NOT-FOUND (err u501))
(define-constant ERR-INVALID-READING (err u502))
(define-constant ERR-DEVICE-INACTIVE (err u503))
(define-constant ERR-DUPLICATE-READING (err u504))
(define-constant ERR-INVALID-DEVICE (err u505))

;; Device Types
(define-constant DEVICE-WATER u1)
(define-constant DEVICE-ELECTRICITY u2)
(define-constant DEVICE-GAS u3)

;; Device Status
(define-constant STATUS-ACTIVE u1)
(define-constant STATUS-INACTIVE u2)
(define-constant STATUS-MAINTENANCE u3)

;; Data Variables
(define-data-var next-device-id uint u1)
(define-data-var next-reading-id uint u1)
(define-data-var total-devices uint u0)
(define-data-var total-readings uint u0)

;; Data Maps
(define-map registered-devices
    uint ;; device-id
    {
        device-address: principal,
        device-type: uint,
        location: (string-ascii 200),
        owner: principal,
        installation-date: uint,
        last-reading-date: uint,
        status: uint,
        total-consumption: uint,
        calibration-factor: uint
    }
)

(define-map device-readings
    uint ;; reading-id
    {
        device-id: uint,
        timestamp: uint,
        consumption-value: uint,
        meter-signature: (string-ascii 128),
        validator: principal,
        validated: bool,
        reading-type: (string-ascii 50)
    }
)

(define-map daily-consumption
    {device-id: uint, date: uint}
    {
        total-consumption: uint,
        reading-count: uint,
        average-rate: uint,
        peak-consumption: uint
    }
)

(define-map device-owners
    principal ;; owner address
    (list 50 uint) ;; list of device IDs
)

(define-map consumption-history
    uint ;; device-id
    (list 100 {
        date: uint,
        consumption: uint,
        cost: uint,
        reading-count: uint
    })
)

(define-map device-validators
    principal ;; validator address
    {
        authorized: bool,
        validation-count: uint,
        accuracy-rating: uint
    }
)

;; Public Functions

;; Register new IoT device
(define-public (register-device
    (device-address principal)
    (device-type uint)
    (location (string-ascii 200))
    (owner principal)
    (calibration-factor uint)
)
    (let (
        (device-id (var-get next-device-id))
        (owner-devices (default-to (list) (map-get? device-owners owner)))
    )
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (asserts! (<= device-type u3) ERR-INVALID-DEVICE)
        (asserts! (> calibration-factor u0) ERR-INVALID-READING)
        (asserts! (> (len location) u0) ERR-INVALID-DEVICE)
        
        ;; Create device record
        (map-set registered-devices device-id
            {
                device-address: device-address,
                device-type: device-type,
                location: location,
                owner: owner,
                installation-date: burn-block-height,
                last-reading-date: u0,
                status: STATUS-ACTIVE,
                total-consumption: u0,
                calibration-factor: calibration-factor
            }
        )
        
        ;; Update owner's device list
        (map-set device-owners owner
            (unwrap! (as-max-len? (append owner-devices device-id) u50) ERR-INVALID-DEVICE)
        )
        
        ;; Update counters
        (var-set next-device-id (+ device-id u1))
        (var-set total-devices (+ (var-get total-devices) u1))
        
        (ok device-id)
    )
)

;; Submit meter reading from IoT device
(define-public (submit-reading
    (device-id uint)
    (consumption-value uint)
    (meter-signature (string-ascii 128))
    (reading-type (string-ascii 50))
)
    (let (
        (device (unwrap! (map-get? registered-devices device-id) ERR-DEVICE-NOT-FOUND))
        (reading-id (var-get next-reading-id))
        (current-date (/ burn-block-height u144)) ;; Approximate days
    )
        (asserts! (is-eq tx-sender (get device-address device)) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get status device) STATUS-ACTIVE) ERR-DEVICE-INACTIVE)
        (asserts! (> consumption-value u0) ERR-INVALID-READING)
        (asserts! (> (len meter-signature) u0) ERR-INVALID-READING)
        
        ;; Create reading record
        (map-set device-readings reading-id
            {
                device-id: device-id,
                timestamp: burn-block-height,
                consumption-value: consumption-value,
                meter-signature: meter-signature,
                validator: tx-sender,
                validated: false,
                reading-type: reading-type
            }
        )
        
        ;; Update device total consumption
        (map-set registered-devices device-id
            (merge device {
                last-reading-date: burn-block-height,
                total-consumption: (+ (get total-consumption device) consumption-value)
            })
        )
        
        ;; Update daily consumption tracking
        (let (
            (daily-key {device-id: device-id, date: current-date})
            (current-daily (default-to 
                {total-consumption: u0, reading-count: u0, average-rate: u0, peak-consumption: u0}
                (map-get? daily-consumption daily-key)
            ))
        )
            (map-set daily-consumption daily-key
                {
                    total-consumption: (+ (get total-consumption current-daily) consumption-value),
                    reading-count: (+ (get reading-count current-daily) u1),
                    average-rate: (/ (+ (get total-consumption current-daily) consumption-value) 
                                    (+ (get reading-count current-daily) u1)),
                    peak-consumption: (if (> consumption-value (get peak-consumption current-daily))
                                        consumption-value
                                        (get peak-consumption current-daily))
                }
            )
        )
        
        ;; Update counters
        (var-set next-reading-id (+ reading-id u1))
        (var-set total-readings (+ (var-get total-readings) u1))
        
        (ok reading-id)
    )
)

;; Validate reading by authorized validator
(define-public (validate-reading (reading-id uint))
    (let (
        (reading (unwrap! (map-get? device-readings reading-id) ERR-INVALID-READING))
        (validator (default-to {authorized: false, validation-count: u0, accuracy-rating: u100}
                              (map-get? device-validators tx-sender)))
    )
        (asserts! (get authorized validator) ERR-NOT-AUTHORIZED)
        (asserts! (not (get validated reading)) ERR-DUPLICATE-READING)
        
        ;; Update reading validation
        (map-set device-readings reading-id
            (merge reading {
                validator: tx-sender,
                validated: true
            })
        )
        
        ;; Update validator statistics
        (map-set device-validators tx-sender
            (merge validator {
                validation-count: (+ (get validation-count validator) u1)
            })
        )
        
        (ok true)
    )
)

;; Update device status
(define-public (update-device-status (device-id uint) (new-status uint))
    (let (
        (device (unwrap! (map-get? registered-devices device-id) ERR-DEVICE-NOT-FOUND))
    )
        (asserts! (or 
            (is-eq tx-sender CONTRACT-OWNER)
            (is-eq tx-sender (get owner device))
        ) ERR-NOT-AUTHORIZED)
        (asserts! (<= new-status u3) ERR-INVALID-DEVICE)
        
        (map-set registered-devices device-id
            (merge device {status: new-status})
        )
        
        (ok true)
    )
)

;; Authorize validator
(define-public (authorize-validator (validator-address principal))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        
        (map-set device-validators validator-address
            {
                authorized: true,
                validation-count: u0,
                accuracy-rating: u100
            }
        )
        
        (ok true)
    )
)

;; Read-Only Functions

;; Get device information
(define-read-only (get-device (device-id uint))
    (map-get? registered-devices device-id)
)

;; Get reading details
(define-read-only (get-reading (reading-id uint))
    (map-get? device-readings reading-id)
)

;; Get daily consumption
(define-read-only (get-daily-consumption (device-id uint) (date uint))
    (map-get? daily-consumption {device-id: device-id, date: date})
)

;; Get owner's devices
(define-read-only (get-owner-devices (owner principal))
    (map-get? device-owners owner)
)

;; Get consumption history
(define-read-only (get-consumption-history (device-id uint))
    (map-get? consumption-history device-id)
)

;; Check if device is active
(define-read-only (is-device-active (device-id uint))
    (match (map-get? registered-devices device-id)
        device (is-eq (get status device) STATUS-ACTIVE)
        false
    )
)

;; Get system statistics
(define-read-only (get-system-stats)
    {
        total-devices: (var-get total-devices),
        total-readings: (var-get total-readings),
        next-device-id: (var-get next-device-id),
        next-reading-id: (var-get next-reading-id)
    }
)

;; Get validator status
(define-read-only (get-validator-status (validator principal))
    (map-get? device-validators validator)
)

