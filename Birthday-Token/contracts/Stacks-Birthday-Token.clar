;; Birthday Celebration Token Smart Contract
;; A social token system for celebrating birthdays with token gifts and personalized messages
;; Users can register birthdays, send celebratory tokens with messages, and claim birthday rewards

;;   TOKEN DEFINITION 
(define-fungible-token birthday-celebration-token)

;;   ERROR CONSTANTS 
(define-constant ERR-UNAUTHORIZED-ACCESS (err u100))
(define-constant ERR-INVALID-TOKEN-OWNER (err u101))
(define-constant ERR-INSUFFICIENT-TOKEN-BALANCE (err u102))
(define-constant ERR-INVALID-TOKEN-AMOUNT (err u103))
(define-constant ERR-BIRTHDAY-NOT-FOUND (err u104))
(define-constant ERR-BIRTHDAY-ALREADY-EXISTS (err u105))
(define-constant ERR-INVALID-BIRTHDAY-DATE (err u106))
(define-constant ERR-NOT-BIRTHDAY-TODAY (err u107))
(define-constant ERR-DAILY-GREETING-LIMIT-EXCEEDED (err u108))
(define-constant ERR-SELF-GREETING-NOT-ALLOWED (err u109))

;;   CONTRACT CONFIGURATION 
(define-constant contract-deployer tx-sender)
(define-constant celebration-token-name "Birthday Celebration Token")
(define-constant celebration-token-symbol "BCT")
(define-constant token-decimal-places u6)
(define-constant birthday-bonus-amount u1000000) ;; 1 token with 6 decimals
(define-constant blocks-per-day u144) ;; Approximate blocks in 24 hours
(define-constant months-in-year u12)
(define-constant days-in-month u30) ;; Simplified for demo

;;   STATE VARIABLES 
(define-data-var current-total-supply uint u0)
(define-data-var token-metadata-uri (optional (string-utf8 256)) none)

;;   DATA STORAGE MAPS 
;; User birthday information storage
(define-map user-birthday-registry 
  principal 
  {birth-month: uint, birth-day: uint})

;; Token balance tracking
(define-map account-token-balances principal uint)

;; Greeting interaction history
(define-map birthday-greeting-log 
  {greeting-sender: principal, greeting-recipient: principal} 
  uint) ;; stores block height of last interaction

;; Birthday message storage
(define-map celebration-messages 
  {message-sender: principal, message-recipient: principal, message-block: uint} 
  (string-ascii 500))

;;   UTILITY FUNCTIONS 
(define-private (validate-date-components (month-value uint) (day-value uint))
  (and 
    (>= month-value u1) 
    (<= month-value months-in-year)
    (>= day-value u1)
    (<= day-value u31)
    (or 
      ;; Months with 31 days
      (and (or (is-eq month-value u1) (is-eq month-value u3) (is-eq month-value u5) 
               (is-eq month-value u7) (is-eq month-value u8) (is-eq month-value u10) 
               (is-eq month-value u12)) (<= day-value u31))
      ;; Months with 30 days
      (and (or (is-eq month-value u4) (is-eq month-value u6) (is-eq month-value u9) 
               (is-eq month-value u11)) (<= day-value u30))
      ;; February (simplified - not accounting for leap years)
      (and (is-eq month-value u2) (<= day-value u29)))))

(define-private (validate-principal (principal-to-check principal))
  (not (is-eq principal-to-check 'SP000000000000000000002Q6VF78)))

(define-private (validate-message-content (message (string-ascii 500)))
  (and 
    (> (len message) u0)
    (<= (len message) u500)))

(define-private (validate-uri (uri (optional (string-utf8 256))))
  (match uri
    some-uri (and (> (len some-uri) u0) (<= (len some-uri) u256))
    true))

(define-private (calculate-current-month)
  (+ (mod (/ stacks-block-height blocks-per-day) months-in-year) u1))

(define-private (calculate-current-day)
  (+ (mod (/ stacks-block-height blocks-per-day) days-in-month) u1))

(define-private (check-if-birthday-today (user-principal principal))
  (match (map-get? user-birthday-registry user-principal)
    user-birthday-data
      (and 
        (is-eq (get birth-month user-birthday-data) (calculate-current-month))
        (is-eq (get birth-day user-birthday-data) (calculate-current-day)))
    false))

(define-private (check-daily-interaction-limit (sender-principal principal) (recipient-principal principal))
  (match (map-get? birthday-greeting-log {greeting-sender: sender-principal, greeting-recipient: recipient-principal})
    last-interaction-block (> (- stacks-block-height last-interaction-block) blocks-per-day)
    true))

;;   TOKEN METADATA READ-ONLY FUNCTIONS 
(define-read-only (get-token-name)
  (ok celebration-token-name))

(define-read-only (get-token-symbol)
  (ok celebration-token-symbol))

(define-read-only (get-token-decimals)
  (ok token-decimal-places))

(define-read-only (get-circulating-supply)
  (ok (var-get current-total-supply)))

(define-read-only (get-token-metadata-uri)
  (ok (var-get token-metadata-uri)))

;;   ACCOUNT BALANCE FUNCTIONS 
(define-read-only (get-account-balance (account-principal principal))
  (default-to u0 (map-get? account-token-balances account-principal)))

(define-read-only (get-user-birthday-info (user-principal principal))
  (map-get? user-birthday-registry user-principal))

(define-read-only (check-birthday-registration-status (user-principal principal))
  (is-some (map-get? user-birthday-registry user-principal)))

;;   BIRTHDAY MANAGEMENT FUNCTIONS 
(define-public (register-user-birthday (birth-month uint) (birth-day uint))
  (begin
    (asserts! (validate-date-components birth-month birth-day) ERR-INVALID-BIRTHDAY-DATE)
    (asserts! (is-none (map-get? user-birthday-registry tx-sender)) ERR-BIRTHDAY-ALREADY-EXISTS)
    (ok (map-set user-birthday-registry tx-sender {birth-month: birth-month, birth-day: birth-day}))))

(define-public (modify-registered-birthday (new-birth-month uint) (new-birth-day uint))
  (begin
    (asserts! (validate-date-components new-birth-month new-birth-day) ERR-INVALID-BIRTHDAY-DATE)
    (asserts! (is-some (map-get? user-birthday-registry tx-sender)) ERR-BIRTHDAY-NOT-FOUND)
    (ok (map-set user-birthday-registry tx-sender {birth-month: new-birth-month, birth-day: new-birth-day}))))

;;   TOKEN SUPPLY MANAGEMENT 
(define-public (mint-celebration-tokens (token-amount uint) (recipient-principal principal))
  (begin
    (asserts! (is-eq tx-sender contract-deployer) ERR-UNAUTHORIZED-ACCESS)
    (asserts! (> token-amount u0) ERR-INVALID-TOKEN-AMOUNT)
    (asserts! (validate-principal recipient-principal) ERR-INVALID-TOKEN-OWNER)
    (try! (ft-mint? birthday-celebration-token token-amount recipient-principal))
    (var-set current-total-supply (+ (var-get current-total-supply) token-amount))
    (ok true)))

(define-public (burn-celebration-tokens (token-amount uint))
  (begin
    (asserts! (> token-amount u0) ERR-INVALID-TOKEN-AMOUNT)
    (asserts! (>= (get-account-balance tx-sender) token-amount) ERR-INSUFFICIENT-TOKEN-BALANCE)
    (try! (ft-burn? birthday-celebration-token token-amount tx-sender))
    (var-set current-total-supply (- (var-get current-total-supply) token-amount))
    (ok true)))

;;   TOKEN TRANSFER FUNCTIONS 
(define-public (transfer-celebration-tokens (token-amount uint) (sender-principal principal) (recipient-principal principal) (transfer-memo (optional (buff 34))))
  (begin
    (asserts! (> token-amount u0) ERR-INVALID-TOKEN-AMOUNT)
    (asserts! (is-eq tx-sender sender-principal) ERR-INVALID-TOKEN-OWNER)
    (asserts! (validate-principal recipient-principal) ERR-INVALID-TOKEN-OWNER)
    (try! (ft-transfer? birthday-celebration-token token-amount sender-principal recipient-principal))
    (match transfer-memo memo-data (print memo-data) 0x)
    (ok true)))

;;   BIRTHDAY CELEBRATION FUNCTIONS 
(define-public (send-birthday-celebration (recipient-principal principal) (celebration-amount uint) (birthday-message (string-ascii 500)))
  (let ((current-block-height stacks-block-height))
    (begin
      ;; Input validation
      (asserts! (> celebration-amount u0) ERR-INVALID-TOKEN-AMOUNT)
      (asserts! (not (is-eq tx-sender recipient-principal)) ERR-SELF-GREETING-NOT-ALLOWED)
      (asserts! (validate-principal recipient-principal) ERR-INVALID-TOKEN-OWNER)
      (asserts! (validate-message-content birthday-message) ERR-INVALID-TOKEN-AMOUNT)
      (asserts! (is-some (map-get? user-birthday-registry recipient-principal)) ERR-BIRTHDAY-NOT-FOUND)
      (asserts! (check-if-birthday-today recipient-principal) ERR-NOT-BIRTHDAY-TODAY)
      (asserts! (check-daily-interaction-limit tx-sender recipient-principal) ERR-DAILY-GREETING-LIMIT-EXCEEDED)
      
      ;; Execute token transfer
      (try! (ft-transfer? birthday-celebration-token celebration-amount tx-sender recipient-principal))
      
      ;; Record interaction
      (map-set birthday-greeting-log {greeting-sender: tx-sender, greeting-recipient: recipient-principal} current-block-height)
      
      ;; Store birthday message
      (map-set celebration-messages 
        {message-sender: tx-sender, message-recipient: recipient-principal, message-block: current-block-height} 
        birthday-message)
      
      ;; Emit celebration event
      (print {
        event-type: "birthday-celebration-sent",
        celebration-sender: tx-sender,
        celebration-recipient: recipient-principal,
        token-amount: celebration-amount,
        celebration-message: birthday-message,
        block-timestamp: current-block-height
      })
      
      (ok true))))

(define-public (claim-birthday-reward-tokens)
  (let ((current-block-height stacks-block-height))
    (begin
      (asserts! (is-some (map-get? user-birthday-registry tx-sender)) ERR-BIRTHDAY-NOT-FOUND)
      (asserts! (check-if-birthday-today tx-sender) ERR-NOT-BIRTHDAY-TODAY)
      (asserts! (check-daily-interaction-limit tx-sender tx-sender) ERR-DAILY-GREETING-LIMIT-EXCEEDED)
      
      ;; Mint birthday bonus tokens
      (try! (ft-mint? birthday-celebration-token birthday-bonus-amount tx-sender))
      (var-set current-total-supply (+ (var-get current-total-supply) birthday-bonus-amount))
      
      ;; Record claim to prevent duplicate claims
      (map-set birthday-greeting-log {greeting-sender: tx-sender, greeting-recipient: tx-sender} current-block-height)
      
      ;; Emit reward claim event
      (print {
        event-type: "birthday-reward-claimed",
        reward-recipient: tx-sender,
        reward-amount: birthday-bonus-amount,
        claim-timestamp: current-block-height
      })
      
      (ok true))))

;;   QUERY FUNCTIONS 
(define-read-only (get-celebration-message (sender-principal principal) (recipient-principal principal) (message-block uint))
  (map-get? celebration-messages {message-sender: sender-principal, message-recipient: recipient-principal, message-block: message-block}))

(define-read-only (get-last-interaction-block (sender-principal principal) (recipient-principal principal))
  (map-get? birthday-greeting-log {greeting-sender: sender-principal, greeting-recipient: recipient-principal}))

(define-read-only (can-send-birthday-greeting (sender-principal principal) (recipient-principal principal))
  (and 
    (is-some (map-get? user-birthday-registry recipient-principal))
    (check-if-birthday-today recipient-principal)
    (not (is-eq sender-principal recipient-principal))
    (check-daily-interaction-limit sender-principal recipient-principal)))

(define-read-only (can-claim-birthday-reward (user-principal principal))
  (and 
    (is-some (map-get? user-birthday-registry user-principal))
    (check-if-birthday-today user-principal)
    (check-daily-interaction-limit user-principal user-principal)))

(define-read-only (get-birthday-status (user-principal principal))
  (let ((birthday-info (map-get? user-birthday-registry user-principal)))
    (match birthday-info
      birthday-data {
        birthday-registered: true,
        birth-month: (get birth-month birthday-data),
        birth-day: (get birth-day birthday-data),
        is-birthday-today: (check-if-birthday-today user-principal),
        can-claim-reward: (can-claim-birthday-reward user-principal)
      }
      {
        birthday-registered: false,
        birth-month: u0,
        birth-day: u0,
        is-birthday-today: false,
        can-claim-reward: false
      })))

;;  ADMIN FUNCTIONS
(define-public (update-token-metadata-uri (new-uri (optional (string-utf8 256))))
  (begin
    (asserts! (is-eq tx-sender contract-deployer) ERR-UNAUTHORIZED-ACCESS)
    (asserts! (validate-uri new-uri) ERR-INVALID-TOKEN-AMOUNT)
    (ok (var-set token-metadata-uri new-uri))))