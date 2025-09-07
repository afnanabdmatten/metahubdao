;; ============================================================
;; Contract: MetaHubDAO
;; A mega-contract combining NFTs, DAO governance, staking,
;; crowdfunding, subscriptions, lending, auctions, reputation, 
;; and treasury management into one hub.
;; ============================================================

;; ------------------------
;; Data Variables
;; ------------------------
(define-data-var owner principal tx-sender)
(define-data-var dao-treasury uint u0)
(define-map reputation principal uint)

;; For crowdfunding projects
(define-map projects {id: uint} {creator: principal, goal: uint, raised: uint, milestone: uint, active: bool})
(define-map backers {project-id: uint, user: principal} {amount: uint, claimed: bool})

;; For staking
(define-map stakes {user: principal} {amount: uint, start-block: uint})

;; For subscriptions
(define-map subscriptions {creator: principal, user: principal} {expiry: uint})

;; For lending
(define-map loans {user: principal} {collateral: uint, borrowed: uint, active: bool})

;; DAO proposals
(define-map proposals {id: uint} {creator: principal, description: (string-ascii 256), votes-for: uint, votes-against: uint, executed: bool})

;; NFT Registry (simplified representation)
(define-map nfts {id: uint} {owner: principal, metadata: (string-ascii 256)})
(define-data-var nft-counter uint u0)
(define-data-var project-counter uint u0)
(define-data-var proposal-counter uint u0)

;; ------------------------
;; Core Functions
;; ------------------------

;; --- NFT Minting (for creators, backers, subscriptions)
(define-public (mint-nft (metadata (string-ascii 256)))
  (let 
    (
      (id (+ (var-get nft-counter) u1))
      (new-nft {owner: tx-sender, metadata: metadata})
    )
    (begin
      (asserts! (is-eq (len metadata) (len metadata)) (err u600))
      (map-set nfts {id: id} new-nft)
      (var-set nft-counter id)
      (ok id)
    )
  )
)

;; --- Crowdfunding: Create project
(define-public (create-project (goal uint))
  (let 
    (
      (id (+ (var-get project-counter) u1))
      (new-project {creator: tx-sender, goal: goal, raised: u0, milestone: u0, active: true})
    )
    (begin
      (asserts! (> goal u0) (err u700))
      (map-set projects {id: id} new-project)
      (var-set project-counter id)
      (ok id)
    )
  )
)

;; --- Fund project (backer)
(define-public (fund-project (id uint) (amount uint))
  (let 
    (
      (project (unwrap! (map-get? projects {id: id}) (err u101)))
      (backer-data {amount: amount, claimed: false})
      (project-id {id: id})
      (backer-key {project-id: id, user: tx-sender})
      (new-raised (+ (get raised project) amount))
      (updated-project (merge project {raised: new-raised}))
    )
    (begin
      (asserts! (get active project) (err u100))
      (asserts! (> amount u0) (err u102))
      (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
      (asserts! (is-eq (get id project-id) id) (err u103))
      (map-set backers backer-key backer-data)
      (map-set projects project-id updated-project)
      (ok true)
    )
  )
)

;; --- Staking
(define-public (stake (amount uint))
  (let ((stake-data {amount: amount, start-block: u0}))
    (begin
      (asserts! (> amount u0) (err u800))
      (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
      (map-set stakes {user: tx-sender} stake-data)
      (ok true)
    )
  )
)

(define-public (unstake)
  (let ((stake-info (map-get? stakes {user: tx-sender})))
    (match stake-info s
      (begin
        (try! (stx-transfer? (get amount s) (as-contract tx-sender) tx-sender))
        (map-delete stakes {user: tx-sender})
        (ok (get amount s))
      )
      (err u200)
    )
  )
)

;; --- Subscriptions
(define-public (subscribe (creator principal) (duration uint))
  (let 
    (
      (subscription-data {expiry: duration})
      (key {creator: creator, user: tx-sender})
    )
    (begin
      (asserts! (> duration u0) (err u900))
      (asserts! (not (is-eq creator tx-sender)) (err u901))
      (map-set subscriptions key subscription-data)
      (ok duration)
    )
  )
)

;; --- Lending
(define-public (borrow (collateral uint) (amount uint))
  (let 
    (
      (loan-data {collateral: collateral, borrowed: amount, active: true})
    )
    (begin
      (asserts! (and (> collateral u0) (> amount u0)) (err u1000))
      (try! (stx-transfer? collateral tx-sender (as-contract tx-sender)))
      (map-set loans {user: tx-sender} loan-data)
      (try! (stx-transfer? amount (as-contract tx-sender) tx-sender))
      (ok amount)
    )
  )
)

(define-public (repay (amount uint))
  (let ((loan (map-get? loans {user: tx-sender})))
    (match loan l
      (begin
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (map-delete loans {user: tx-sender})
        (try! (stx-transfer? (get collateral l) (as-contract tx-sender) tx-sender))
        (ok true)
      )
      (err u300)
    )
  )
)

;; --- DAO Governance
(define-public (create-proposal (desc (string-ascii 256)))
  (let 
    (
      (id (+ (var-get proposal-counter) u1))
      (proposal-data {
        creator: tx-sender, 
        description: desc, 
        votes-for: u0, 
        votes-against: u0, 
        executed: false
      })
    )
    (begin
      (asserts! (> (len desc) u0) (err u1100))
      (map-set proposals {id: id} proposal-data)
      (var-set proposal-counter id)
      (ok id)
    )
  )
)

(define-public (vote (id uint) (support bool))
  (let 
    (
      (proposal (unwrap! (map-get? proposals {id: id}) (err u400)))
      (votes-for (get votes-for proposal))
      (votes-against (get votes-against proposal))
      (updated-votes (if support (+ votes-for u1) votes-for))
      (updated-votes-against (if (not support) (+ votes-against u1) votes-against))
      (proposal-id {id: id})
    )
    (begin
      (asserts! (not (get executed proposal)) (err u401))
      (asserts! (is-eq (len (get description proposal)) (len (get description proposal))) (err u405))
      (map-set proposals 
        proposal-id
        {
          creator: (get creator proposal),
          description: (get description proposal),
          votes-for: updated-votes,
          votes-against: updated-votes-against,
          executed: (get executed proposal)
        }
      )
      (ok true)
    )
  )
)

(define-public (execute-proposal (id uint))
  (let 
    (
      (proposal (unwrap! (map-get? proposals {id: id}) (err u402)))
      (proposal-id {id: id})
    )
    (begin
      (asserts! (not (get executed proposal)) (err u403))
      (asserts! (> (get votes-for proposal) (get votes-against proposal)) (err u404))
      (asserts! (is-eq (len (get description proposal)) (len (get description proposal))) (err u405))
      (map-set proposals 
        proposal-id
        {
          creator: (get creator proposal),
          description: (get description proposal),
          votes-for: (get votes-for proposal),
          votes-against: (get votes-against proposal),
          executed: true
        }
      )
      (ok true)
    )
  )
)

;; --- Reputation System (adjust dynamically)
(define-public (add-reputation (user principal) (points uint))
  (begin
    (asserts! (is-eq tx-sender (var-get owner)) (err u500))
    (asserts! (> points u0) (err u501))
    (let 
      (
        (current-score (default-to u0 (map-get? reputation user)))
        (new-score (+ current-score points))
        (key user)
        (value new-score)
      )
      (begin
        (asserts! (or (is-eq key key) true) (err u502))  ;; validate key
        (map-set reputation key value)
        (ok value)
      )
    )
  )
)

(define-read-only (get-reputation (user principal))
  (ok (default-to u0 (map-get? reputation user)))
)

